module Giraffe.HttpHandlers

open System
open System.Text
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Primitives
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open FSharp.Core.Printf
open Giraffe.Common
open Giraffe.FormatExpressions
open Giraffe.HttpContextExtensions
open Giraffe.HtmlEngine

type HttpHandlerResult = Task<HttpContext option>

type HttpCont = HttpContext -> HttpHandlerResult

type HttpHandler = HttpCont -> HttpContext -> HttpHandlerResult

type ErrorHandler = exn -> ILogger -> HttpHandler

/// ---------------------------
/// Globally useful functions
/// ---------------------------

let inline warbler f a = f a a

/// ---------------------------
/// Sub route helper functions
/// ---------------------------

[<Literal>]
let private RouteKey = "giraffe_route"

let private getSavedSubPath (ctx : HttpContext) =
    if ctx.Items.ContainsKey RouteKey
    then ctx.Items.Item RouteKey |> string |> strOption
    else None

let private getPath =
    fun  (ctx : HttpContext) ->
        match getSavedSubPath ctx with
        | Some p -> ctx.Request.Path.ToString().[p.Length..]
        | None   -> ctx.Request.Path.ToString()

let private handlerWithRootedPath (path : string) (handler : HttpHandler) =
    fun (ctx : HttpContext) ->
        let savedSubPath = getSavedSubPath ctx
        try
            ctx.Items.Item RouteKey <- ((savedSubPath |> Option.defaultValue "") + path)
            handler ctx
        finally
            match savedSubPath with
            | Some savedSubPath -> ctx.Items.Item RouteKey <- savedSubPath
            | None              -> ctx.Items.Remove RouteKey |> ignore

/// ---------------------------
/// Default HttpHandlers
/// ---------------------------

/// Adapts a HttpHandler function to accept a HttpHandlerResult.
/// If the HttpHandlerResult returns Some HttpContext, then it will proceed
/// to the handler, otherwise short circuit and return None as the result.
/// If the response has already been written in the resulting HttpContext,
/// then it will skip the HttpHandler as well.
// let bind (handler : HttpHandler) =
//     fun (result : HttpHandlerResult) ->
//         task {
//             let! (ctxOpt : HttpContext option) = result
//             match ctxOpt with
//             | None     -> return None
//             | Some ctx ->
//                 match ctx.Response.HasStarted with
//                 | true  -> return  Some ctx
//                 | false -> return! handler ctx
//         }

/// Combines two HttpHandler functions into one.
let compose (handler : HttpHandler) (handler2 : HttpHandler) : HttpHandler =
    fun (next:HttpCont) ->
        // 1. next will ultimately be fn ctx -> Some ctx but allows to compose / short circuit with continuations
        // 2. We can apply next function at load so all handlers have thier next function refs and need only apply ctx on run 
        let child  = handler2 next
        let next = handler child
        fun (ctx : HttpContext) ->    
            match ctx.Response.HasStarted with
            | true  -> task { return Some ctx }
            | false -> next ctx                

/// Combines two HttpHandler functions into one.
/// See bind for more information.
let (>=>) = compose

/// Iterates through a list of HttpHandler functions and returns the
/// result of the first HttpHandler which outcome is Some HttpContext
let rec choose (handlers : HttpHandler list) : HttpHandler =    
    fun next ctx ->
        task {
            match handlers with
            | []              -> return None
            | handler :: tail ->
                let! result = handler next ctx
                match result with
                | Some c -> return Some c
                | None   -> return! choose tail next ctx
        }

/// Filters an incoming HTTP request based on the HTTP verb
let httpVerb (verb : string) : HttpHandler =
    fun _Some ctx ->
        if ctx.Request.Method.Equals verb
        then _Some ctx
        else task { return None }

let GET    : HttpHandler = httpVerb "GET"
let POST   : HttpHandler = httpVerb "POST"
let PUT    : HttpHandler = httpVerb "PUT"
let PATCH  : HttpHandler = httpVerb "PATCH"
let DELETE : HttpHandler = httpVerb "DELETE"

/// Filters an incoming HTTP request based on the accepted
/// mime types of the client.
let mustAccept (mimeTypes : string list) : HttpHandler =
    fun next ctx ->
        let headers = ctx.Request.GetTypedHeaders()
        headers.Accept
        |> Seq.map    (fun h -> h.ToString())
        |> Seq.exists (fun h -> mimeTypes |> Seq.contains h)
        |> function
            | true  -> next ctx
            | false -> None |> Task.FromResult
            
/// Challenges the client to authenticate with a given authentication scheme.
let challenge (authScheme : string) : HttpHandler =
    fun next ctx ->
        task {
            let auth = ctx.Authentication
            do! auth.ChallengeAsync authScheme
            return! next ctx
        }

/// Signs off the current user.
let signOff (authScheme : string) : HttpHandler =
    fun next ctx ->
        task {
            let auth = ctx.Authentication
            do! auth.SignOutAsync authScheme
            return! next ctx
        }

/// Validates if a user is authenticated.
/// If not it will proceed with the authFailedHandler.
let requiresAuthentication (authFailedHandler : HttpHandler) : HttpHandler =
    fun next ctx ->
        let user = ctx.User
        if isNotNull user && user.Identity.IsAuthenticated
        then Task.FromResult (Some ctx)
        else authFailedHandler ctx

/// Validates if a user is in a specific role.
/// If not it will proceed with the authFailedHandler.
let requiresRole (role : string) (authFailedHandler : HttpHandler) =
    fun (ctx : HttpContext) ->
        let user = ctx.User
        if user.IsInRole role
        then Task.FromResult (Some ctx)
        else authFailedHandler ctx

/// Validates if a user has at least one of the specified roles.
/// If not it will proceed with the authFailedHandler.
let requiresRoleOf (roles : string list) (authFailedHandler : HttpHandler) =
    fun (ctx : HttpContext) ->
        let user = ctx.User
        roles
        |> List.exists user.IsInRole
        |> function
            | true  -> Task.FromResult (Some ctx)
            | false -> authFailedHandler ctx

/// Attempts to clear the current HttpResponse object.
/// This can be useful inside an error handler when the response
/// needs to be overwritten in the case of a failure.
let clearResponse =
    fun (ctx : HttpContext) ->
        ctx.Response.Clear()
        Task.FromResult (Some ctx)

/// Filters an incoming HTTP request based on the request path (case sensitive).
let route (path : string) =
    fun (ctx : HttpContext) ->
        if (getPath ctx).Equals path
        then Some ctx
        else None
        |> Task.FromResult

/// Filters an incoming HTTP request based on the request path (case sensitive).
/// The arguments from the format string will be automatically resolved when the
/// route matches and subsequently passed into the supplied routeHandler.
let routef (path : StringFormat<_, 'T>) (routeHandler : 'T -> HttpHandler) =
    fun (ctx : HttpContext) ->
        tryMatchInput path (getPath ctx) false
        |> function
            | None      -> Task.FromResult None
            | Some args -> routeHandler args ctx

/// Filters an incoming HTTP request based on the request path (case insensitive).
let routeCi (path : string) =
    fun (ctx : HttpContext) ->
        if String.Equals(getPath ctx, path, StringComparison.CurrentCultureIgnoreCase)
        then Some ctx
        else None
        |> Task.FromResult

/// Filters an incoming HTTP request based on the request path (case insensitive).
/// The arguments from the format string will be automatically resolved when the
/// route matches and subsequently passed into the supplied routeHandler.
let routeCif (path : StringFormat<_, 'T>) (routeHandler : 'T -> HttpHandler) =
    fun (ctx : HttpContext) ->
        tryMatchInput path (getPath ctx) true
        |> function
            | None      -> Task.FromResult None
            | Some args -> routeHandler args ctx

/// Filters an incoming HTTP request based on the beginning of the request path (case sensitive).
let routeStartsWith (subPath : string) =
    fun (ctx : HttpContext) ->
        if (getPath ctx).StartsWith subPath
        then Some ctx
        else None
        |> Task.FromResult

/// Filters an incoming HTTP request based on the beginning of the request path (case insensitive).
let routeStartsWithCi (subPath : string) =
    fun (ctx : HttpContext) ->
        if (getPath ctx).StartsWith(subPath, StringComparison.CurrentCultureIgnoreCase)
        then Some ctx
        else None
        |> Task.FromResult

/// Filters an incoming HTTP request based on a part of the request path (case sensitive).
/// Subsequent route handlers inside the given handler function should omit the already validated path.
let subRoute (path : string) (handler : HttpHandler) =
    routeStartsWith path >=>
    handlerWithRootedPath path handler

/// Filters an incoming HTTP request based on a part of the request path (case insensitive).
/// Subsequent route handlers inside the given handler function should omit the already validated path.
let subRouteCi (path : string) (handler : HttpHandler) =
    routeStartsWithCi path >=>
    handlerWithRootedPath path handler

/// Sets the HTTP response status code.
let setStatusCode (statusCode : int) =
    fun (ctx : HttpContext) ->
        ctx.Response.StatusCode <- statusCode
        Some ctx |> Task.FromResult

/// Sets a HTTP header in the HTTP response.
let setHttpHeader (key : string) (value : obj) =
    fun (ctx : HttpContext) ->
        ctx.Response.Headers.[key] <- StringValues(value.ToString())
        Some ctx |> Task.FromResult

/// Writes to the body of the HTTP response and sets the HTTP header Content-Length accordingly.
let setBody (bytes : byte array) =
    fun (ctx : HttpContext) ->
        task {
            ctx.Response.Headers.["Content-Length"] <- StringValues(bytes.Length.ToString())
            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            return Some ctx
        }

/// Writes a string to the body of the HTTP response and sets the HTTP header Content-Length accordingly.
let setBodyAsString (str : string) =
    Encoding.UTF8.GetBytes str
    |> setBody

/// Writes a string to the body of the HTTP response.
/// It also sets the HTTP header Content-Type: text/plain and sets the Content-Length header accordingly.
let text (str : string) =
    setHttpHeader "Content-Type" "text/plain"
    >=> setBodyAsString str

/// Serializes an object to JSON and writes it to the body of the HTTP response.
/// It also sets the HTTP header Content-Type: application/json and sets the Content-Length header accordingly.
let json (dataObj : obj) =
    setHttpHeader "Content-Type" "application/json"
    >=> setBodyAsString (serializeJson dataObj)

/// Serializes an object to XML and writes it to the body of the HTTP response.
/// It also sets the HTTP header Content-Type: application/xml and sets the Content-Length header accordingly.
let xml (dataObj : obj) =
    setHttpHeader "Content-Type" "application/xml"
    >=> setBody (serializeXml dataObj)

/// Reads a HTML file from disk and writes its contents to the body of the HTTP response
/// with a Content-Type of text/html.
let htmlFile (relativeFilePath : string) =
    fun (ctx : HttpContext) ->
        task {
            let env = ctx.GetService<IHostingEnvironment>()
            let filePath = env.ContentRootPath + relativeFilePath
            let! html = readFileAsString filePath
            return!
                ctx
                |> (setHttpHeader "Content-Type" "text/html"
                >=> setBodyAsString html)
        }

/// Uses the Giraffe.HtmlEngine to compile and render a HTML Document from
/// a given HtmlNode. The HTTP response is of Content-Type text/html.
let renderHtml (document: HtmlNode) =
    fun (ctx : HttpContext) ->
        let htmlHandler =
            document
            |> renderHtmlDocument
            |> setBodyAsString
        ctx |> (setHttpHeader "Content-Type" "text/html" >=> htmlHandler)

/// Checks the HTTP Accept header of the request and determines the most appropriate
/// response type from a given set of negotiationRules. If the Accept header cannot be
/// matched with a supported media type then it will invoke the unacceptableHandler.
///
/// The negotiationRules must be a dictionary of supported media types with a matching
/// HttpHandler which can serve that particular media type.
///
/// Example:
/// let negotiationRules = dict [ "application/json", json; "application/xml" , xml ]
/// `json` and `xml` are both the respective default HttpHandler functions in this example.
let negotiateWith (negotiationRules    : IDictionary<string, obj -> HttpHandler>)
                  (unacceptableHandler : HttpHandler)
                  (responseObj         : obj) =
    fun (ctx : HttpContext) ->
        (ctx.Request.GetTypedHeaders()).Accept
        |> fun acceptedMimeTypes ->
            match isNull acceptedMimeTypes || acceptedMimeTypes.Count = 0 with
            | true  ->
                negotiationRules.Keys
                |> Seq.head
                |> fun mediaType -> negotiationRules.[mediaType]
                |> fun handler   -> handler responseObj ctx
            | false ->
                List.ofSeq acceptedMimeTypes
                |> List.filter (fun x -> negotiationRules.ContainsKey x.MediaType)
                |> fun mimeTypes ->
                    match mimeTypes.Length with
                    | 0 -> unacceptableHandler
                    | _ ->
                        mimeTypes
                        |> List.sortByDescending (fun x -> if x.Quality.HasValue then x.Quality.Value else 1.0)
                        |> List.head
                        |> fun mimeType -> negotiationRules.[mimeType.MediaType]
                        |> fun handler  -> handler responseObj
                <| ctx

/// Same as negotiateWith except that it specifies a default set of negotiation rules
/// and a default unacceptableHandler.
///
/// The supported media types are:
/// */*              -> serializes object to JSON
/// application/json -> serializes object to JSON
/// application/xml  -> serializes object to XML
/// text/xml         -> serializes object to XML
/// text/plain       -> returns object's ToString() result
let negotiate (responseObj : obj) =
    negotiateWith
        // Default negotiation rules
        (dict [
            "*/*"             , json
            "application/json", json
            "application/xml" , xml
            "text/xml"        , xml
            "text/plain"      , fun x -> x.ToString() |> text
        ])
        // Default unacceptable HttpHandler
        (fun (ctx : HttpContext) ->
            setStatusCode 406
            >=> ((ctx.Request.Headers.["Accept"]).ToString()
                |> sprintf "%s is unacceptable by the server."
                |> text)
            <| ctx)
        // response object
        responseObj

///Redirects to a different location with a 302 or 301 (when permanent) HTTP status code.
let redirectTo (permanent : bool) (location : string)  =
    fun (ctx:HttpContext) ->
        ctx.Response.Redirect(location, permanent)
        ctx |> Some |> Task.FromResult