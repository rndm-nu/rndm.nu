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
open FSharp.Control

module PublicWeb =
    module Database =
        type DataBasePromise = {
            uid : string
            pk : string
            timeStamp : DateTimeOffset
            promiseOriginIP : string
            gitSHA : string
            promise : RequestParsing.RandomNumberRequest
        }

        
        type DataBaseFulfillment = {
            uid : string
            pk : string
            timeStamp : DateTimeOffset
            gitSHA : string
        }

        type DatabaseEngine(read : ((string * string) -> Result<string option, string> Async), write : ((string * string * string) -> Result<unit, string> Async), readBinary : (string -> Result<Stream option, string> Async ), writeBinary : (string -> Async<Result<Stream, string>>)) =
            
            member __.SavePromise(id : string, shardId : string, requestIP : Net.IPAddress , request : RequestParsing.RandomNumberRequest) =
                {
                    uid = id
                    pk = shardId
                    timeStamp = DateTimeOffset.UtcNow
                    promiseOriginIP = requestIP.ToString()
                    gitSHA = ThisAssembly.Git.Sha
                    promise = request
                }
                |> JsonConvert.SerializeObject
                |> fun json -> write("promise", id, json)

            member __.TryLoadPromise(id : string) =
                async {
                    let! result = read ("promise", id)
                    match result with
                    | Ok json ->
                        return Ok (json |> Option.map JsonConvert.DeserializeObject<DataBasePromise> )
                    | Error err ->
                        return Error err
                }
            
            member __.SaveFulfillment(id : string, shardId : string, fulfillment : NumberRequestFulfilment.RequestFulfilment) =
                async {
                    let! result =
                        {
                            uid = id
                            pk = shardId
                            timeStamp = DateTimeOffset.UtcNow
                            gitSHA = ThisAssembly.Git.Sha
                        }
                        |> JsonConvert.SerializeObject
                        |> fun json -> write("fulfilment", id, json)
                    
                    match! writeBinary id with
                    | Ok stream -> 
                        match fulfillment with
                        | NumberRequestFulfilment.RequestFulfilment.Binary bytesSeq ->
                            do! 
                                bytesSeq 
                                |> AsyncSeq.iterAsync(fun bytes -> 
                                    stream.WriteAsync(bytes, 0, bytes.Length) |> Async.AwaitTask
                                )
                            stream.Close()
                            return Ok ()
                        | NumberRequestFulfilment.RequestFulfilment.ManyIntegers intSeq ->
                            do! 
                                intSeq 
                                |> AsyncSeq.indexed |> AsyncSeq.iterAsync(fun (index, ints) -> async {
                                    let json = 
                                        ints |> Seq.map string |> String.concat ", "
                                        |> fun numStr ->
                                            sprintf "%s%s" 
                                                (
                                                    if index = 0L then
                                                        "["
                                                    else 
                                                        ", "
                                                )
                                                numStr

                                    let bytes = Text.UTF8Encoding.UTF8.GetBytes(json)
                                    do! stream.WriteAsync(bytes, 0, bytes.Length) |> Async.AwaitTask
                                })
                            let bytes = Text.UTF8Encoding.UTF8.GetBytes("]")
                            do! stream.WriteAsync(bytes, 0, bytes.Length) |> Async.AwaitTask
                            stream.Close()
                            return Ok ()
                        | NumberRequestFulfilment.RequestFulfilment.OneInteger num ->
                            let! n = num
                            let bytes = Text.UTF8Encoding.UTF8.GetBytes(string n)
                            do! stream.WriteAsync(bytes, 0, bytes.Length) |> Async.AwaitTask
                            stream.Close()
                            return Ok ()
                    | Error err ->
                        return (Error err)
                }





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

    
    let tryParseTestMode (path : string) =
        if path.StartsWith("testmode/") then
            Ok (true, path.Substring("testmode/".Length))
        else
            Ok (false, path)
        
    let requestHandler (queueReaderSimulator : (unit -> Async<byte array>) option, databaseEngine : Database.DatabaseEngine Option) (context : HttpContext) = 
        
        let db = 
            databaseEngine
            |> Option.defaultValue(Database.DatabaseEngine(
                (fun _ -> async {return Error "not implemented"}), 
                (fun (_) -> async {return Error "not implemented"}), 
                (fun (_) -> async {return Error "not implemented"}), 
                (fun (_) -> async {return Error "not implemented"})
            ))
        
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






            let path = context.Request.Path.Value.TrimStart('/')
            let nonce =
                let rnd = Array.zeroCreate<byte> 16
                Security.Cryptography.RandomNumberGenerator.Fill(rnd.AsSpan())
                Convert.ToBase64String(rnd)
            
            context.Response.Headers.Add("Content-Security-Policy", StringValues(sprintf "script-src 'nonce-%s'" nonce))
            
            match tryParseTestMode path with
            | Ok (testMode, remainder) ->
                let remainder =
                    match remainder with
                    | "" -> "api.html"
                    | remainder -> remainder
                match! htmlTemplateEngine(remainder, context.Response.WriteAsync) with
                | Error () ->
                    match staticFiles |> Map.tryFind (match remainder with | "favicon.ico" -> "static/favicon.ico" | path -> path) with
                    | Some fullPath ->
                        let! bytes = File.ReadAllBytesAsync fullPath

                        let hashString = bytes |> System.Security.Cryptography.SHA1.HashData |> Convert.ToHexString

                        let mime =
                            match (new FileExtensionContentTypeProvider()).TryGetContentType(remainder) with
                            | (true, mime) ->  mime
                            | (false, _) ->     ""

                        context.Response.Headers.Add("E-Tag", StringValues(hashString))
                        context.Response.Headers.Add("Cache-Control", StringValues([|"public"; "max-age=36000"|]))
                        context.Response.Headers.Add("Content-Type", StringValues(mime))

                        let! r = context.Response.BodyWriter.WriteAsync(ReadOnlyMemory(bytes)).AsTask() |> Async.AwaitTask
                        ()
                    | None -> 
                        match Promise.tryParsePromise(remainder) with
                        | Some promiseId ->
                            match Promise.tryParsePromiseIdString promiseId with
                            | Ok (shardId, idInt) ->
                                do! context.Response.WriteAsync(sprintf "\r\nhttps://rndm.nu/p%s\r\nshard: %s\r\nid: %i" promiseId (shardId.ToString()) idInt)
                            | Error err -> 
                                context.Response.StatusCode <- Http.StatusCodes.Status400BadRequest
                                do! context.Response.WriteAsync(err)
                        | None ->
                            match RequestParsing.tryParseRequest(remainder, ([] |> Map.ofList)) with
                            | Ok request -> 
                                let json =
                                    let json = RequestParsing.requestToJsonString request
                                    let r =  RequestParsing.jsonStringToRequest json
                                    r |> RequestParsing.requestToJsonString
                            
                                stopWatch.Stop()
                                context.Response.Headers.Add("Server-Timing", StringValues(sprintf "total;dur=%f" stopWatch.Elapsed.TotalMilliseconds))

                                do! context.Response.WriteAsync(json)

                                do! context.Response.WriteAsync("\r\n\r\nfulfilment:\r\n")

                                let fullfilment = NumberRequestFulfilment.fulfillRequest request

                                match fullfilment with
                                | NumberRequestFulfilment.OneInteger i ->
                                    let! i = i
                                    do! context.Response.WriteAsync(sprintf "OneInteger: %i" i)
                                | NumberRequestFulfilment.ManyIntegers ints ->
                                    do! context.Response.WriteAsync("ManyIntegers:\r\n")
                                    do! ints |> AsyncSeq.indexed |> AsyncSeq.iterAsync (fun (index1, is) -> async {
                                        let prepend = if index1 = 0L then "" else ", "
                                        let str = is |> Array.map string |> String.concat ", "
                                        context.Response.WriteAsync(sprintf "%s%s" prepend str) |> ignore
                                    })
                                | NumberRequestFulfilment.Binary ints ->
                                    do! context.Response.WriteAsync("Binary:\r\n")
                                    do! ints |> AsyncSeq.iterAsync (fun b -> async {
                                        do! context.Response.WriteAsync(sprintf "%s\r\n---------\r\n" (Convert.ToHexString(b))) |> Async.AwaitTask
                                    })

                                
                                let fullfilment2 = NumberRequestFulfilment.fulfillRequest request
                                
                                let shardId, _, id = Promise.createPrimiseId Promise.Shard.Shard2 
                                match! db.SavePromise(id, shardId.ToString(), Net.IPAddress.Parse("192.168.1.1"), request) with
                                | Ok () -> ()
                                | Error err -> ()

                                
                                match! db.SaveFulfillment(id, shardId.ToString(), fullfilment2) with
                                | Ok () -> ()
                                | Error err -> ()

                                
                                do! context.Response.WriteAsync("\r\n")

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
            | Error () -> ()
            ()
        }



module Program =
    
    type MiddleWare(next: RequestDelegate) = 
        member this.Invoke (context: HttpContext) =
            PublicWeb.requestHandler (None, None) context

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
