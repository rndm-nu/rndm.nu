// Learn more about F# at http://fsharp.org
namespace SuaveOnly
open System
open Suave
open HtmlTemplate
open Newtonsoft.Json

module App =
    open Suave.Filters
    open System.IO
    open System.Reflection
    open System.Net
    open System.Diagnostics
    open Suave
    open GoogleMeasurementProtocol
    open GoogleMeasurementProtocol.Parameters.ContentInformation

    let functions =
        typeof<Functions.Marker>.DeclaringType.GetMethods()
        |> Array.map (fun methodInfo ->
            let func = (fun (ctx : HttpContext) -> methodInfo.Invoke(typeof<Controllers.Controller>, [|ctx :> obj|]) :?> string )
            methodInfo.Name, func
        )

    let redirects =
        [
            "support",      "https://tankstudios.ticksy.com"
            "userguide",    "https://tankstudios.ticksy.com/article/15292/"
            "faq",          "https://tankstudios.ticksy.com/article/15293/"
        ]
        |> Map.ofList
    

    let test (ctx : HttpContext) = 

        let reqPath =
            if ctx.request.path.StartsWith(sprintf "/%s" Core.gitCommitHashShort) then
                ctx.request.path.Substring(Core.gitCommitHashShort.Length + 1)
            else
                ctx.request.path
        
        let lang, path =
            match (Core.languages |> List.map(sprintf "/%s" ) |> List.tryFind (fun str -> reqPath.StartsWith str || reqPath = str)) with
            | Some str ->
                (str.Replace("/", "")), match (reqPath.Substring(str.Length)) with | "" -> "/" | s -> s
            | None ->
                Core.defaultLanguage, reqPath
            
        
        let pageName, extraURL =
            match path with
            | "/" -> "index"
            | "/update" -> "index"
            | "/pro" -> "index"
            | path -> path.Substring(1)
            |> fun str ->
                let splitStr = str.Split('/')
                let extraURL =
                    if splitStr.Length > 1 then 
                        splitStr |> Array.skip 1
                        |> fun strs -> String.Join('/', strs)
                    else
                        ""
                splitStr.[0], extraURL
            
        match Array.tryFind (fun (fn,_) -> fn = pageName) functions with
        | Some (_, func) -> 
            { 
                ctx with 
                    response = { 
                        ctx.response with 
                            status = HTTP_200.status; 
                            content = Bytes (UTF8.bytes (func ctx |> JsonConvert.SerializeObject)) 
                            headers = 
                                [
                                    ["Content-Type", "application/json"]
                                ]
                                |> List.concat
                    }        
            } |> succeed
     
        | None ->
            match Map.tryFind pageName redirects with
            | Some redirectURL -> 
                { ctx with
                    response = { 
                        ctx.response with
                            status = HTTP_307.status
                            headers = 
                                [
                                    ctx.response.headers
                                    [ "location", redirectURL ]
                                ]
                                |> List.concat
                    }
                }
                |> succeed
            | None ->

                match Array.tryFind (fun (p : Page) -> p.name = pageName) pages with
                | Some page -> 
                    let nonceKey = Guid.NewGuid().ToString("N")
                    let ctx =  
                        { ctx with 
                            userState = 
                                ctx.userState 
                                |> Map.add "nonceKey" (nonceKey :> obj) 
                                |>  match Guid.TryParse(extraURL) with
                                    | true, guid -> Map.add "customOrder" (guid :> obj)
                                    | _ ->          id
                                    
                        }
                    { 
                        ctx with 
                            response = { 
                                ctx.response with 
                                    status = HTTP_200.status
                                    content = createPageResponse(page.name, Map.empty, ctx) |> UTF8.bytes |> Bytes 
                                    headers = 
                                        [
                                            ctx.response.headers
                                            [
                                                "content-security-policy", (sprintf "script-src 'self' %s https://www.google-analytics.com https://*.stripe.com 'nonce-%s'" Core.CDNURL nonceKey)
                                                "strict-transport-security", "max-age=31536000; includeSubDomains"
                                                "x-frame-options", "SAMEORIGIN"
                                                "x-content-type-options", "nosniff"
                                                "referrer-policy", "no-referrer"
                                            ]
                                        ]
                                        |> List.concat
                            }
                    } |> succeed
     
                | None ->
                    let t = ctx.response.content
                        //Console.WriteLine("[SUAVE] {0}", Path.Combine(HtmlTemplate.rootDir, "Static"));
                    ctx
                    |> Files.browseFile (Path.Combine(Core.rootDir, "Static")) path
                    |>  Async.map(
                                fun o ->
                                match o with
                                | Some ctx -> 
                                    Some {
                                        ctx with 
                                            response = {
                                                ctx.response with 
                                                    headers = 
                                                        [
                                                            "Access-Control-Allow-Origin", "https://epicpenwebsitedev.azurewebsites.net"
                                                            "Cache-Control", "public, immutable, max-age=31536000"
                                                        ]
                                                        |> List.append ctx.response.headers
                                            }
                                    }
                                | None -> None
                    )
                    //try
                    //with
                    //| e ->
                    //let test = UTF8.bytes "TEST"
                    //
                    //{ ctx with response = { ctx.response with status = HTTP_200.status; content = Bytes test }} |> succeed
     
                    
                    //|> Async.map(
                    //            fun o ->
                    //            match o with
                    //            | Some ctx -> Some ctx
                    //            | None -> None
                    //)
            
        
    let helloWorld =
        fun (ctx : HttpContext) ->
            choose [test] ctx



    [<EntryPoint>]
    let main argv =
        let port =
            let arg = (match argv with | [||] -> "" | _ -> argv.[0])
            let value = ref 0;
            let success = Int32.TryParse(arg, value)
            match success with
            | true -> uint16 !value
            | false -> 80us
            

        //Process.Start(sprintf "http://localhost:%i" port) |> ignore
        Process.Start(new ProcessStartInfo("cmd", sprintf "/c start http://localhost:%i/" port)) |> ignore

        startWebServer {defaultConfig with bindings = [HttpBinding.create Protocol.HTTP IPAddress.Any port]} helloWorld 
        0 // return an integer exit code
