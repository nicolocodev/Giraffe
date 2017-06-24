module Giraffe.Razor.HttpHandlers

open System.Text
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc.Razor
open Microsoft.AspNetCore.Mvc.ViewFeatures
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Primitives
open Giraffe.Razor.Engine
open Giraffe.Tasks

/// Reads a razor view from disk and compiles it with the given model and sets
/// the compiled output as the HTTP reponse with the given contentType.
let razorView (contentType : string) (viewName : string) (model : 'T) =
    fun (ctx : HttpContext) ->
        task {
            let engine = ctx.RequestServices.GetService<IRazorViewEngine>()
            let tempDataProvider = ctx.RequestServices.GetService<ITempDataProvider>()
            let! (result : Result<string, string>) = renderRazorView engine tempDataProvider ctx viewName model
            match result with
            | Error msg -> return (failwith msg)
            | Ok output ->
                let bytes = Encoding.UTF8.GetBytes output
                ctx.Response.Headers.["Content-Type"] <- StringValues contentType
                ctx.Response.Headers.["Content-Length"] <- bytes.Length |> string |> StringValues
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                return Some ctx
        }

/// Reads a razor view from disk and compiles it with the given model and sets
/// the compiled output as the HTTP reponse with a Content-Type of text/html.
let razorHtmlView (viewName : string) (model : 'T) =
    razorView "text/html" viewName model