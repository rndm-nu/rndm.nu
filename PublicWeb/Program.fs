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

    module RandomNumberRequestParser =
        type OptionConverter() =
            inherit JsonConverter()
        
            override x.CanConvert(t) = 
                t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
        
            override x.WriteJson(writer, value, serializer) =
                let value = 
                    if value = null then null
                    else 
                        let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                        fields.[0]  
                serializer.Serialize(writer, value)
        
            override x.ReadJson(reader, t, existingValue, serializer) =        
                let innerType = t.GetGenericArguments().[0]
                let innerType = 
                    if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
                    else innerType        
                let value = serializer.Deserialize(reader, innerType)
                let cases = FSharpType.GetUnionCases(t)
                if value = null then FSharpValue.MakeUnion(cases.[0], [||])
                else FSharpValue.MakeUnion(cases.[1], [|value|])

        
        type Format =
        | Html
        | Json
        | Xml
        | Text
        | Binary

        type IntRange = {
            min : int
            max : int
        }

        type RequestModeType =
        | Normal of IntRange * multiplier : int option
        | Shuffle of IntRange
        | Unique of IntRange * multiplier : int
        | Binary of numOfBytes : int

        type RequestModeTypeJsonConverter() =
            inherit JsonConverter<RequestModeType>() with
                override _.WriteJson(writer : JsonWriter, value : RequestModeType, serializer : JsonSerializer) =
                    writer.WriteStartObject()
                    writer.WritePropertyName("mode")
                    let caseInfo, _ = FSharp.Reflection.FSharpValue.GetUnionFields(value, typeof<RequestModeType>)
                    writer.WriteValue(caseInfo.Name.ToLowerInvariant())
                    writer.WritePropertyName("values")
                    writer.WriteStartObject()
                    
                    let writeRange (range : IntRange) =
                        writer.WritePropertyName("min")
                        writer.WriteValue(range.min)
                        writer.WritePropertyName("max")
                        writer.WriteValue(range.max)
                    
                    let writeMult (mult : int option) =
                        writer.WritePropertyName("multiplier")
                        match mult with
                        | Some m -> writer.WriteValue(m)
                        | None ->   writer.WriteNull()

                    match value with
                    | Normal (range, multiplier) ->
                        writeRange range
                        writeMult multiplier
                    | Shuffle (range) ->
                        writeRange range
                    | Unique (range, multiplier) ->
                        writeRange range
                        writer.WritePropertyName("multiplier")
                        writer.WriteValue(multiplier)
                    | Binary (numOfBytes) ->
                        writer.WritePropertyName("numOfBytes")
                        writer.WriteValue(numOfBytes)
                    writer.WriteEndObject()
                    writer.WriteEndObject()

                override _.ReadJson(reader : JsonReader, typeToConvert : Type, existinValue : RequestModeType, hasExistingValue : bool, serializer : JsonSerializer) : RequestModeType =
                    //reader.TokenType = JsonToken.StartObject
                    let item = Newtonsoft.Json.Linq.JObject.Load(reader)
                    let mode = item.["mode"].ToString()
                    let unionCase = FSharp.Reflection.FSharpType.GetUnionCases(typeof<RequestModeType>) |> Array.find(fun case -> case.Name.ToLowerInvariant() = mode)

                    let parameters =
                        let values = item.["values"] :?> Newtonsoft.Json.Linq.JObject
                        match mode with
                        | "normal" -> 
                            [|
                                {
                                    min = values.["min"].ToObject<int>()
                                    max = values.["max"].ToObject<int>()
                                } :> obj
                                (
                                    match values.["multiplier"].ToObject<obj>() with
                                    | null -> None
                                    | mult -> Some (mult :?> int64 |> int)
                                    :> obj
                                )
                            |]
                        | "shuffle" -> 
                            [|
                                {
                                    min = values.["min"].ToObject() |> int
                                    max = values.["max"].ToObject() |> int
                                } :> obj
                            |]
                        | "unique" -> 
                            [|
                                {
                                    min = values.["min"].ToObject() |> int
                                    max = values.["max"].ToObject() |> int
                                } :> obj
                                values.["multiplier"].ToObject<int>() :> obj
                            |]
                        | "binary" -> 
                            [|
                                values.["numOfBytes"].ToObject<int>() :> obj
                            |]



                    FSharp.Reflection.FSharpValue.MakeUnion(unionCase, parameters) :?> RequestModeType
                    
        
        type FormatJsonConverter() =
            inherit JsonConverter<Format>() with
                override _.WriteJson(writer : JsonWriter, value : Format, serializer : JsonSerializer) =
                    let caseInfo, _ = FSharp.Reflection.FSharpValue.GetUnionFields(value, typeof<Format>)
                    writer.WriteValue(caseInfo.Name.ToLowerInvariant())

                override _.ReadJson(reader : JsonReader, typeToConvert : Type, existinValue : Format, hasExistingValue : bool, serializer : JsonSerializer) : Format =
                    let value = reader.Value :?> string
                    FSharp.Reflection.FSharpType.GetUnionCases(typeof<Format>)
                    |> Array.find(fun case -> case.Name.ToLowerInvariant() = value)
                    |> fun case -> FSharp.Reflection.FSharpValue.MakeUnion(case, [||]) :?> Format

        
        type ModeParseType =
        | Normal 
        | Shuffle 
        | Unique 
        | Binary

        type RandomNumberRequest = {
            format : Format
            requestType : RequestModeType
            promiseBy : DateTimeOffset option
        }
        
        let formatFileExtesnions = [|
            "txt", Format.Text
            "bin", Format.Binary
            "html", Format.Html
            "json", Format.Json
            "xml", Format.Xml
        |]

        
        let formatMimeTypes = [|
            "text/html", Format.Html
            "text/plain", Format.Text
            "application/json", Format.Json
            "text/xml", Format.Xml
            "application/xml", Format.Xml
            "application/octet-stream", Format.Binary
        |]

        
        let modeNames =
            FSharp.Reflection.FSharpType.GetUnionCases(typeof<ModeParseType>)
            |> Array.map(fun info -> info.Name.ToLowerInvariant(), FSharp.Reflection.FSharpValue.MakeUnion(info, [||]) :?> ModeParseType)

        let tryParseRequest(path : string, header : Map<string,string>) =
            let parseFormat(path : string) =
                formatFileExtesnions 
                |> Array.tryFind (fun (extension, format) ->
                    path.EndsWith(sprintf ".%s" extension)
                )
                |> function
                | Some (extension, format) ->
                    ((Some format), (path.Substring(0, path.Length - extension.Length - 1))) |> Ok
                | None ->
                    header |> Map.tryFindKey (fun k v -> k.ToLowerInvariant() = "accept")
                    |> function
                    | Some acceptKey ->
                        header
                        |> Map.tryFind acceptKey
                        |> function
                        | Some accepts ->
                            formatMimeTypes 
                            |> Array.tryFind (fun (mime, format) -> accepts.ToLowerInvariant().Contains(mime.ToLowerInvariant()))
                            |> function
                            | Some (_, format) -> ((Some format), path) |> Ok
                            | None -> (None, path) |> Ok
                        | None -> (None, path) |> Ok
                    | None -> (None, path) |> Ok

            let parseMultiplier(path : string) =
                match path.Split('x') with
                | [|remainder; multiplier|] ->
                    match Int32.TryParse multiplier with
                    | (true, multiplier) -> ((Some multiplier), remainder) |> Ok
                    | (false, _) -> Error (sprintf "Could not parse multiplier '%s'." multiplier)
                | [|remainder|] -> (None, remainder) |> Ok
                | _ -> Error "Too many 'x' characters in path."

            

            let parseMode(path : string) =
                modeNames 
                |> Array.tryFind(fun (modeName, _) -> path.ToLowerInvariant().StartsWith(modeName))
                |> function
                | Some (name, mode) -> ((Some mode), (path.Substring(name.Length, path.Length - name.Length))) |> Ok
                | None -> (None, path) |> Ok

            
            let parseRange(path : string) =
                match path.Split('-') with
                | [|min; max|] ->
                    match Int32.TryParse min with
                    | (true, min) -> 
                        match Int32.TryParse max with
                        | (true, max) -> 
                            {min = min; max = max} |> Ok
                        | (false, _) -> Error (sprintf "Could not parse max '%s'." max)
                    | (false, _) -> Error (sprintf "Could not parse min '%s'." min)
                | [|max|] -> 
                    match Int32.TryParse max with
                    | (true, max) -> 
                        {min = 1; max = max} |> Ok
                    | (false, _) -> Error (sprintf "Could not parse max '%s'." max)
                | _ -> Error "Could not parse number range."


            parseFormat(path) 
            |> function
            | Ok (format, remainder) -> 
                let format = (format |> Option.defaultValue Format.Html)
                parseMultiplier(remainder)
                |> function
                | Ok (multiplier, remainder) ->
                    let impliedMultiplier = multiplier |> Option.defaultValue 1
                    parseMode(remainder)
                    |> function
                    | Ok (mode, remainder') ->
                        let mode = mode |> Option.defaultValue Normal
                        parseRange(remainder')
                        |> function
                        | Ok (range) ->
                            match mode, multiplier with
                            | Unique, None -> 
                                Error "You must provide a multiplier parameter in Unique mode."
                            | Unique, Some m when m > (range.max - range.min + 1) ->
                                Error "The mutliplier must be lower than the range of unique numbers."
                            | Shuffle, Some _ ->
                                Error "You cannot provide a multiplier value in suffle mode."
                            | _ ->
                                
                                {
                                    format = format
                                    requestType =
                                        match mode with
                                        | Normal -> RequestModeType.Normal(range, multiplier)
                                        | Shuffle -> RequestModeType.Shuffle(range)
                                        | Unique -> RequestModeType.Unique(range, multiplier.Value)
                                        | Binary -> RequestModeType.Binary(range.max)
                                    promiseBy = None
                                } |> Ok
                        | Error err -> err |> Error
                    | Error err -> err |> Error
                | Error err -> err |> Error
            | Error err -> err |> Error
            
        let requestToJsonString(req : RandomNumberRequest) =

            JsonConvert.SerializeObject(
                req, 
                [|
                    new RequestModeTypeJsonConverter() :> JsonConverter
                    new OptionConverter() :> JsonConverter
                    new FormatJsonConverter() :> JsonConverter
                |]
            )

        
        let jsonStringToRequest(json : string) =

            JsonConvert.DeserializeObject<RandomNumberRequest>(
                json, 
                [|
                    new RequestModeTypeJsonConverter() :> JsonConverter
                    new OptionConverter() :> JsonConverter
                    new FormatJsonConverter() :> JsonConverter
                |]
            )
                            








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

    let requestHandler (queueReaderSimulator : (unit -> Async<byte array>) option) (context : HttpContext) = 
        task {
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
                |> Array.map (fun path -> RandomNumberRequestParser.tryParseRequest(path, [] |> Map.ofList))






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

                    let hashString = Convert.ToHexString(System.Security.Cryptography.SHA1.HashData(bytes))

                    
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
                    match RandomNumberRequestParser.tryParseRequest(path, ([] |> Map.ofList)) with
                    | Ok result -> 
                        let json =
                            let json = RandomNumberRequestParser.requestToJsonString result
                            let r =  RandomNumberRequestParser.jsonStringToRequest json
                            
                            r |> RandomNumberRequestParser.requestToJsonString
                        

                        do! context.Response.WriteAsync(json)
                    | Error err -> 
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
