module Giraffe.HttpContextExtensions

open System
open System.IO
open System.Reflection
open System.ComponentModel
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Microsoft.Extensions.Logging
open Microsoft.Net.Http.Headers
open Giraffe.Common
open Giraffe.ValueTask

type HttpContext with

    /// ---------------------------
    /// Dependency management
    /// ---------------------------

    member this.GetService<'T>() =
        this.RequestServices.GetService(typeof<'T>) :?> 'T

    member this.GetLogger<'T>() =
        this.GetService<ILogger<'T>>()

    /// ---------------------------
    /// Common helpers
    /// ---------------------------

    member this.TryGetRequestHeader (key : string) =
        let strValue = ref (StringValues())
        match this.Request.Headers.TryGetValue(key, strValue) with
        | true  -> Some (strValue.Value.ToString())
        | false -> None

    member this.GetRequestHeader (key : string) =
        let strValue = ref (StringValues())
        match this.Request.Headers.TryGetValue(key, strValue) with
        | true  -> Ok (strValue.Value.ToString())
        | false -> Error (sprintf "HTTP request header '%s' is missing." key)

    /// ---------------------------
    /// Model binding
    /// ---------------------------

    member this.ReadBodyFromRequest() =
        task {
            let body = this.Request.Body
            use reader = new StreamReader(body, true)
            return! reader.ReadToEndAsync()
        }

    member this.BindJson<'T>() =
        task {
            let! body = this.ReadBodyFromRequest()
            return deserializeJson<'T> body
        }

    member this.BindXml<'T>() =
        task {
            let! body = this.ReadBodyFromRequest()
            return deserializeXml<'T> body
        }

    member this.BindForm<'T>() =
        task {
            let! (form:IFormCollection) = this.Request.ReadFormAsync()
            let obj   = Activator.CreateInstance<'T>()
            let props = obj.GetType().GetProperties(BindingFlags.Instance ||| BindingFlags.Public)
            props
            |> Seq.iter (fun p ->
                //let strValue = ref (StringValues())
                match form.TryGetValue(p.Name) with
                | true , strValue ->  
                    let converter = TypeDescriptor.GetConverter p.PropertyType
                    let value = converter.ConvertFromInvariantString(strValue.ToString())
                    p.SetValue(obj, value, null)
                | false , _ -> ()
            )
            return obj
        }

    member this.BindQueryString<'T>() =
        task {
            let query = this.Request.Query
            let obj   = Activator.CreateInstance<'T>()
            let props = obj.GetType().GetProperties(BindingFlags.Instance ||| BindingFlags.Public)
            props
            |> Seq.iter (fun p ->
                let strValue = ref (StringValues())
                if query.TryGetValue(p.Name, strValue)
                then
                    let converter = TypeDescriptor.GetConverter p.PropertyType
                    let value = converter.ConvertFromInvariantString(strValue.Value.ToString())
                    p.SetValue(obj, value, null))
            return obj
        }

    member this.BindModel<'T>() =
        task {
            let method = this.Request.Method
            return!
                if method.Equals "POST" || method.Equals "PUT" then
                    let original = this.Request.ContentType
                    let parsed   = ref (MediaTypeHeaderValue("*/*"))
                    match MediaTypeHeaderValue.TryParse(original, parsed) with
                    | false -> failwithf "Could not parse Content-Type HTTP header value '%s'" original
                    | true  ->
                        match parsed.Value.MediaType with
                        | "application/json"                  -> this.BindJson<'T>()
                        | "application/xml"                   -> this.BindXml<'T>()
                        | "application/x-www-form-urlencoded" -> this.BindForm<'T>()
                        | _ -> failwithf "Cannot bind model from Content-Type '%s'" original
                else this.BindQueryString<'T>()
        }