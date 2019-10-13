namespace SuaveOnly

open Suave

module Controllers =
    open FSharp.Reflection.FSharpReflectionExtensions
    open System.Reflection
    open System.IO
    open Suave.Logging
    open SendGrid.Helpers.Mail
    open System.Net
    open System
    open Newtonsoft.Json
    open Core

    type Marker = interface end
    
    type Controller = (Map<string,string> * Suave.Http.HttpContext) -> obj
    
    let header1 = fun (funParams : Map<string,string>, req : HttpContext) ->
        {|
            title =                 funParams.["title"]
            subTitle =              funParams.["subTitle"]
            robotsMetaTag =         match env with | AzureLive -> "index" | _ -> "noindex"
            pageTitleClass =        if funParams.["page"] = "index" then "hide" else ""
            bannerClass =           if funParams.["page"] = "index" then "" else "hide"
            testModeStyle =         if Core.testMode then "" else "display: none;"
            testMode =              Core.testMode |> string |> String.toLowerInvariant
            gitCommitHash =         Core.gitCommitHash
            env =                   Core.env.ToString().ToLowerInvariant()
            customOrderJSON =       req.userState |> Map.tryFind "customOrder" |> function | Some id -> sprintf "\"%s\"" ((id :?> Guid).ToString()) | None -> "null"
            notificationClass =     if Core.indexNotification.IsSome then "" else "hide"
            notificationTitle =     match Core.indexNotification with | Some (s,_) -> s | None -> ""
            notificationMessage =   match Core.indexNotification with | Some (_,s) -> s | None -> ""
            nonceKey =              req.userState |> Map.tryFind "nonceKey" |> Option.map string |> Option.defaultValue ""
        |}

        
    let footer = fun (funParams : Map<string,string>, req : HttpContext) ->
        {|
            fullYear =              System.DateTime.Now.Year |> string
            //paymentScriptType =     if funParams |> Map.containsKey "includePaymentJS" then "text/javascript" else "application/json"
            //gitBranch =             Core.gitBranch
            gitCommitHashShort =    Core.gitCommitHashShort
            gitCommitHash =         Core.gitCommitHash
            //testModeStyle =         if Core.testMode then "" else "display: none;"
            //env =                   Core.env.ToString()
            //nonceKey =              req.userState |> Map.tryFind "nonceKey" |> Option.map string |> Option.defaultValue ""
        |}

        