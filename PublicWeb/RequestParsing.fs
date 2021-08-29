namespace PublicWeb

open System
open System.Reflection
open FSharp.Control.Tasks.ContextSensitive
open System.Threading
open Newtonsoft.Json
open Microsoft.FSharp.Reflection

module RequestParsing =
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
        let parsePromiseTime(path : string) =
            match path.Split("at") with
            | [|remainder; timeStamp|] ->
                match timeStamp.Split('T') with
                | [|date; time|] -> 
                    match DateTimeOffset.TryParse(date + "T" + time.Replace('-', ':')) with
                    | (true, dateTime) ->
                        Ok (Some dateTime, remainder)
                    | _ -> Error "Could not parse promise time."
                | _ -> Error "Could not parse promise time."
            | _ -> 
                match path.Split("in") with
                | [||] -> Ok (None, path)
                | strings ->
                    let inTime = strings |> Seq.last
                    let remainder = strings |> Seq.rev |> Seq.skip 1 |> Seq.rev |> String.concat "in"
                    [
                        "seconds",  TimeSpan.FromSeconds
                        "minutes",  TimeSpan.FromMinutes
                        "hours",    TimeSpan.FromHours
                        "days",     TimeSpan.FromDays
                    ]
                    |> Seq.tryPick(fun (unit, intToTimeSpan) ->
                        if inTime.EndsWith(unit) then
                            let numOf = inTime.Substring(0, inTime.Length - unit.Length)
                            match Int32.TryParse(numOf) with
                            | (true, numOfUnit) -> 
                                let t = (DateTimeOffset.UtcNow + intToTimeSpan(float numOfUnit))
                                Some (Some t, remainder)
                            | _ ->
                                None
                        else
                            None
                    )
                    |> function
                    | Some result -> Ok result
                    | None -> Ok (None, path)
                


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

            parsePromiseTime(remainder)
            |> function
            | Ok (promiseTime, remainder) ->
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
                                match promiseTime with
                                | Some time when time < DateTimeOffset.UtcNow ->
                                    Error "Promise time must be in the future."
                                | _ ->
                                    {
                                        format = format
                                        requestType =
                                            match mode with
                                            | Normal -> RequestModeType.Normal(range, multiplier)
                                            | Shuffle -> RequestModeType.Shuffle(range)
                                            | Unique -> RequestModeType.Unique(range, multiplier.Value)
                                            | Binary -> RequestModeType.Binary(range.max)
                                        promiseBy = promiseTime
                                    } |> Ok
                        | Error err -> err |> Error
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
