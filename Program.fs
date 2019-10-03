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

    let installers =
        Directory.EnumerateFiles(Path.Combine(Core.rootDir, "Installers"))
        |> Seq.map(fun fp ->
            let version =
                Path.GetFileNameWithoutExtension(fp).Replace("EpicPenSetup-v","").Split('.')
                |> Array.map(int)
            version, Path.GetFileName(fp)
        )
        |> Seq.toList
        |> List.sortBy(fun (num, _) -> 
            [
                num.[0] |> uint64 |> (*) (256uL*256uL*256uL)
                num.[1] |> uint64 |> (*) (256uL*256uL)
                num.[2] |> uint64 |> (*) (256uL)
                num.[3] |> uint64 
            ] |> List.sum
        )
        |> List.rev
        
    let latestInstallerString = installers |> List.item 0 |> fun (ints, _) -> sprintf "%i.%i.%i" ints.[0] ints.[1] ints.[2]
    let latestInstallerShortString = installers |> List.item 0 |> fun (ints, _) -> sprintf "%i.%i" ints.[0] ints.[1]

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
                    if reqPath.StartsWith("/download") then
                        ()
                        System.Diagnostics.Trace.TraceInformation(sprintf "length: %i" (reqPath.Split('/')).Length)
                        match reqPath.Split('/') with
                        | [|_;_;version|] ->
                                
                            let verInt = version.Split('.') |> Array.map int
                            let verString = (verInt |> Array.map string |> fun strs -> String.Join(".", strs))
                           

                            
                    
                            ctx
                            |> Files.browseFile (Path.Combine(Core.rootDir, "Installers")) (installers |> List.find (fun (a,b) -> a.[0] = verInt.[0] && a.[1] = verInt.[1] && a.[2] = verInt.[2] && a.[3] = verInt.[3]) |> snd)
                            |>  Async.map(
                                        fun o ->
                                        match o with
                                        | Some ctx -> 
                                            Some {
                                                ctx with 
                                                    response = {
                                                        ctx.response with 
                                                            headers = 
                                                                let installerVersion = (installers |> List.item 0 |> fst)
                                                                [
                                                                    "Content-Type",                 "application/vnd.microsoft.portable-executable"
                                                                    "content-disposition",          sprintf "attachment; filename=\"Epic Pen Setup v%s.exe\"" verString
                                                                    "Access-Control-Allow-Origin",  "https://epicpenwebsitedev.azurewebsites.net"
                                                                    "Cache-Control",                "public, immutable, max-age=31536000"
                                                                ]
                                                    }
                                            }
                                        | None -> None
                            )
                        | _ ->
                            
                            let installerVersion = (installers |> List.item 0 |> fst)
                            match ((ctx.request.headers |> Map.ofList) |> Map.tryFind "client-ip") with
                            | Some valu -> 
                                let ip = valu.Split(':').[0]
                                
                                let verString = (installerVersion |> Array.map string |> fun strs -> String.Join(".", strs))
                                let factory = GoogleMeasurementProtocol.GoogleAnalyticsRequestFactory("UA-68962371-5");
                    
                                let googleRequest = factory.CreateRequest(HitTypes.PageView, [GoogleMeasurementProtocol.Parameters.TrafficSources.DocumentReferrer(ctx.request.headers |> List.tryFind (fun (key,_) -> key.ToLowerInvariant() = "referer") |> (function | Some (_, str) -> str | None -> "")) ])
                    
                                googleRequest.Parameters.Add(DocumentHostName("epic-pen.com"));
                                googleRequest.Parameters.Add(DocumentPath(sprintf "/download/%s" verString));
                                googleRequest.Parameters.Add(DocumentTitle("Download"));
                    
                                googleRequest.Parameters.Add(new Parameters.Session.IpOverride(ip));
                                googleRequest.Parameters.Add(new Parameters.Session.UserAgentOverride(ctx.request.headers |> List.tryFind (fun (key,_) -> key.ToLowerInvariant() = "user-agent") |> (function | Some (_, str) -> str | None -> "")  ))
                    
                                let clientId = GoogleMeasurementProtocol.Parameters.User.ClientId(Guid.NewGuid());
                                googleRequest.PostAsync(clientId) |> Async.AwaitTask |> Async.Start
                            | None ->  ()
                            
                            { ctx with
                                response = { 
                                    ctx.response with
                                        status = HTTP_307.status
                                        headers = 
                                            [
                                                ctx.response.headers
                                                [ 
                                                    "Content-Type", "application/vnd.microsoft.portable-executable"
                                                    "location",     sprintf "%sdownload/%i.%i.%i.%i" Core.staticFileRoot installerVersion.[0] installerVersion.[1] installerVersion.[2] installerVersion.[3] 
                                                ]
                                            ]
                                            |> List.concat
                                }
                            }
                            |> succeed
                    else
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
