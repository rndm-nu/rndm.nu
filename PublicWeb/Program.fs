namespace PublicWeb

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Html
open Microsoft.Extensions.Primitives
open Microsoft.AspNetCore.StaticFiles
open System.Reflection
open FSharp.Control.Tasks.ContextSensitive
open System.Threading
open Newtonsoft.Json
open Microsoft.FSharp.Reflection

module PublicWeb =

    let rnd = new Random()
    
    let js = File.ReadAllText(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "tsout", "app.js"))
    let ujs = (NUglify.Uglify.Js js).Code
    

    let functionsMap = 
        let rand = 
            fun (startRand, endRand, seed, multiplier, requestId) -> 
                (Random(requestId)).Next(startRand, endRand + 1) * multiplier
                |> string

        [|
            "randMult",             rand :> obj
            "rand",                 (fun (startRand, endRand, seed, requestId) -> rand(startRand, endRand, seed, 1, requestId)) :> obj
            "timeString",           (fun (requestId : int) -> DateTime.UtcNow.AddHours(1.0).ToString("o").Replace(':','-').Substring(0, 22) + "Z") :> obj
            "import",               (fun (path : string, requestId : int) -> "") :> obj
            "gitCommitHashShort",   (fun (requestId : int) -> ThisAssembly.Git.Sha.Substring(0, 7)) :> obj
            "fullYear",             (fun (requestId : int) -> "2021") :> obj
        |]
        |> Map.ofArray

    
    let asmDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let htmlTemplateEngine = 
        let htmlFiles =
            Directory.EnumerateFiles(Path.Combine(asmDir, "pages"), "*.html", EnumerationOptions(RecurseSubdirectories = true))
            |> Seq.map(fun info -> Path.GetRelativePath(Path.Combine(asmDir, "pages"), info), info)
            |> Seq.toArray

        HtmlTemplateEngine.createHtmlTemplateEngine(
            htmlFiles |> Array.map(fun (path, fullPath) -> ((path.Replace('\\', '/')), ((File.ReadAllTextAsync(fullPath)) |> Async.AwaitTask)))

            //["api.html", ((File.ReadAllTextAsync(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "pages", "api.html"))) |> Async.AwaitTask)] 
            |> Map.ofArray, 
            functionsMap
        ) 
        |> Async.RunSynchronously

    let staticFiles =
        Directory.EnumerateFiles(Path.Combine(asmDir, "static"), "*.*", EnumerationOptions(RecurseSubdirectories = true))
        |> Seq.map(fun info -> Path.GetRelativePath(asmDir, info).Replace('\\', '/'), info)
        |> Map.ofSeq

    module Promise =
        let private base64chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789*!".ToCharArray()

        type Shard =
        | Shard0 = 0uy
        | Shard1 = 1uy
        | Shard2 = 2uy

        let createPrimiseId (shardId : Shard) =
            let shardByte = shardId |> byte

            let id =
                let bytes = Array.zeroCreate<byte>(8)
                Security.Cryptography.RandomNumberGenerator.Fill(bytes.AsSpan())
                BitConverter.ToUInt64(ReadOnlySpan(bytes))
                |> fun i -> i >>> 6
                |> fun i -> i + (shardByte |> uint64 <<< (64 - 6))

            let rec uint64toString(num : uint64, bitsLeft : int, acu : System.Text.StringBuilder) =
                match bitsLeft with
                | bitsLeft when bitsLeft <= 0 -> 
                    acu.ToString()
                | bitsLeft ->
                    let preparedInt = num >>> (64 - 6) |> uint8
                    let char = base64chars.[int preparedInt]

                    uint64toString(num <<< 6, bitsLeft - 6, acu.Append(char))

            shardId, (id <<< 6 >>> 6), uint64toString(id, 64, System.Text.StringBuilder())
        
            //do! context.Response.WriteAsync(sprintf "\r\nhttps://rndm.nu/p%s" str)

        let tryParsePromiseIdString (idString : string) =
            
            let rec stringToUint64(str : char array, acu : uint64, bitsLeft : int) =
                if bitsLeft = 0 then
                    Ok acu
                else
                    match str |> Array.tryItem 0 with
                    | Some char ->
                        match base64chars |> Array.tryFindIndex ((=) char) with
                        | Some index ->
                            let charIndex = byte index
                            let acu = (acu <<< Math.Min(6, bitsLeft)) + (uint64 charIndex >>> (6 - Math.Min(6, bitsLeft)))

                            stringToUint64(str |> Array.skip 1, acu, bitsLeft - Math.Min(bitsLeft, 6))
                        | None ->
                            Error "Invalid characters in promise id."
                    | None ->
                        Error "Promise id provided in invalid format."

            match stringToUint64(idString.ToCharArray(), 0UL, 64) with
            | Ok id ->
                let shardId = id >>> (64 - 6) |> byte |> Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<byte, Shard>
                Ok (shardId, (id <<< 6 >>> 6))
            | Error err -> Error err


        let tryParsePromise(path : string) =
            if path.StartsWith('p') then
                Some (path.Substring(1))
            else
                None
        

    let requestHandler (queueReaderSimulator : (unit -> Async<byte array>) option) (context : HttpContext) = 
        task {
            let stopWatch = new Diagnostics.Stopwatch()
            stopWatch.Start()

            let result =
                [|
                    "6"
                    "4-12"
                    "4-12x4"
                    "shuffle7"
                    "binary128"
                    "7x4"
                    "7x4.txt"
                    "7x4.json"
                    "7x4.xml"
                |]
                |> Array.map (fun path -> RequestParsing.tryParseRequest(path, [] |> Map.ofList))






            let path =
                match context.Request.Path.Value.TrimStart('/') with
                | "" -> "api.html"
                | path -> path
            let nonce =
                let rnd = Array.zeroCreate<byte> 16
                Security.Cryptography.RandomNumberGenerator.Fill(rnd.AsSpan())
                Convert.ToBase64String(rnd)
            
            context.Response.Headers.Add("Content-Security-Policy", StringValues(sprintf "script-src 'nonce-%s'" nonce))
            
            match! htmlTemplateEngine(path, context.Response.WriteAsync) with
            | Error () ->
                match staticFiles |> Map.tryFind path with
                | Some fullPath ->
                    let! bytes = File.ReadAllBytesAsync fullPath

                    let hashString = bytes |> System.Security.Cryptography.SHA1.HashData |> Convert.ToHexString

                    let mime =
                        match (new FileExtensionContentTypeProvider()).TryGetContentType(path) with
                        | (true, mime) ->  mime
                        | (false, _) ->     ""

                    context.Response.Headers.Add("E-Tag", StringValues(hashString))
                    context.Response.Headers.Add("Cache-Control", StringValues([|"public"; "max-age=36000"|]))
                    context.Response.Headers.Add("Content-Type", StringValues(mime))

                    let! r = context.Response.BodyWriter.WriteAsync(ReadOnlyMemory(bytes)).AsTask() |> Async.AwaitTask
                    ()
                | None -> 
                    match Promise.tryParsePromise(path) with
                    | Some promiseId ->
                        match Promise.tryParsePromiseIdString promiseId with
                        | Ok (shardId, idInt) ->
                            do! context.Response.WriteAsync(sprintf "\r\nhttps://rndm.nu/p%s\r\nshard: %s\r\nid: %i" promiseId (shardId.ToString()) idInt)
                        | Error err -> 
                            context.Response.StatusCode <- Http.StatusCodes.Status400BadRequest
                            do! context.Response.WriteAsync(err)
                    | None ->
                        match RequestParsing.tryParseRequest(path, ([] |> Map.ofList)) with
                        | Ok result -> 
                            let json =
                                let json = RequestParsing.requestToJsonString result
                                let r =  RequestParsing.jsonStringToRequest json
                                r |> RequestParsing.requestToJsonString
                            
                            stopWatch.Stop()
                            context.Response.Headers.Add("Server-Timing", StringValues(sprintf "total;dur=%f" stopWatch.Elapsed.TotalMilliseconds))

                            do! context.Response.WriteAsync(json)

                            let _, _, id = Promise.createPrimiseId Promise.Shard.Shard2 
                            match Promise.tryParsePromiseIdString id with
                            | Ok (shardId, idInt) ->
                                do! context.Response.WriteAsync(sprintf "\r\nhttps://rndm.nu/p%s" id)
                            | Error err -> 
                                context.Response.StatusCode <- Http.StatusCodes.Status400BadRequest
                                do! context.Response.WriteAsync(err)

                        | Error err -> 
                            context.Response.StatusCode <- Http.StatusCodes.Status400BadRequest
                            do! context.Response.WriteAsync(err)

            | Ok () -> ()
            ()
        }



module Program =
    
    type MiddleWare(next: RequestDelegate) = 
        member this.Invoke (context: HttpContext) =
            PublicWeb.requestHandler None context

    type Startup() =
    
        // This method gets called by the runtime. Use this method to add services to the container.
        // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
        member _.ConfigureServices(services: IServiceCollection) =
            ()
    
        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        member _.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
            if env.IsDevelopment() then
                app.UseDeveloperExceptionPage() |> ignore
    
            //app.Run(fun context -> PublicWeb.requestHandler None context) |> ignore
            app.UseMiddleware<MiddleWare>()  |> ignore


    let createHostBuilder args =
        Host.CreateDefaultBuilder(args)
            .ConfigureWebHostDefaults(fun webBuilder ->
                webBuilder.UseKestrel(fun (options) -> options.AddServerHeader <- false).UseStartup<Startup>() |> ignore
            )

    [<EntryPoint>]
    let main args =
        createHostBuilder(args).Build().Run()
        0 // Exit code
