module Giraffe.Middleware

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.FileProviders
open Microsoft.AspNetCore.Mvc.Razor
open Giraffe.HttpHandlers
open Giraffe.ValueTask

/// ---------------------------
/// Logging helper functions
/// ---------------------------

let toTask (t:ValueTask<_>) = t.AsTask() :> Task
    
// let TaskToValueTask (t:Task) = 
//     t.GetAwaiter ValueTask<unit>(t)
    
let private getRequestInfo (ctx : HttpContext) =
    (ctx.Request.Protocol,
     ctx.Request.Method,
     ctx.Request.Path.ToString())
    |||> sprintf "%s %s %s"

/// ---------------------------
/// Default middleware
/// ---------------------------

type GiraffeMiddleware (next          : RequestDelegate,
                        handler       : HttpHandler,
                        loggerFactory : ILoggerFactory) =
    
    do if isNull next then raise (ArgumentNullException("next"))

    let logger = loggerFactory.CreateLogger<GiraffeMiddleware>()
    
    let printResult v ctx = sprintf "Giraffe returned %s for %s" v (getRequestInfo ctx) |> logger.LogDebug 

    let succ, fail =
        // debug case
        if logger.IsEnabled LogLevel.Debug then 
            (fun (ctx:HttpContext) -> task { 
                printResult "Succ/Some" ctx
                return ctx
            }),
            (fun (ctx:HttpContext) -> task { 
                printResult "Fail/None" ctx
                do! next.Invoke ctx
                return ctx
            })
        // production case
        else 
            (fun (ctx:HttpContext) -> task { return ctx }),
            (fun (ctx:HttpContext) -> 
                task { 
                    do! next.Invoke ctx
                    return ctx 
                })
    member __.Invoke (ctx : HttpContext) =
        task {
            return! handler succ fail ctx
        } |> toTask

/// ---------------------------
/// Error Handling middleware
/// ---------------------------

type GiraffeErrorHandlerMiddleware (next          : RequestDelegate,
                                    errorHandler  : ErrorHandler,
                                    loggerFactory : ILoggerFactory) =

    do if isNull next then raise (ArgumentNullException("next"))

    let succ, fail =
        (fun (ctx:HttpContext) -> task { return ctx }),
        (fun (ctx:HttpContext) -> 
            task { 
                do! next.Invoke ctx
                return ctx 
            })

    member __.Invoke (ctx : HttpContext) =
        task {
            let logger = loggerFactory.CreateLogger<GiraffeErrorHandlerMiddleware>()
            try
                do!
                    next.Invoke ctx
            with ex ->
                try
                    let! _ =
                        (errorHandler ex logger succ fail ctx)
                    return ()
                        
                with ex2 ->
                    logger.LogError(EventId(0), ex,  "An unhandled exception has occurred while executing the request.")
                    logger.LogError(EventId(0), ex2, "An exception was thrown attempting to handle the original exception.")
        } |> toTask

/// ---------------------------
/// Extension methods for convenience
/// ---------------------------

type IApplicationBuilder with
    member this.UseGiraffe (handler : HttpHandler) =
        this.UseMiddleware<GiraffeMiddleware>(handler)
        |> ignore

    member this.UseGiraffeErrorHandler (handler : ErrorHandler) =
        this.UseMiddleware<GiraffeErrorHandlerMiddleware>(handler)
        |> ignore

type IServiceCollection with
    member this.AddRazorEngine (viewsFolderPath : string) =
        this.Configure<RazorViewEngineOptions>(
            fun options ->
                options.FileProviders.Clear()
                options.FileProviders.Add(new PhysicalFileProvider(viewsFolderPath)))
            .AddMvc()
        |> ignore