namespace rndmnu

open Suave
open System
open System.Threading
open Suave.Filters
open Suave.Operators

open System.Security.Cryptography
open System.Collections.Generic
open Suave.Http

module private Option =
  let iff b x =
    if b then Some x else None


open System.IO

//params

type Format =
| HTML
| TXT
| JSON
| BIN

type Range =
| Single of uint64
| Range of uint64 * uint64

type Multiplier = int option

type Language =
| EN
| GA

type Params =
| Standard of Range * Multiplier * Format * Language
| Binary of int * Language
| Shuffle of int * Format * Language
| Lottery of int * int * Format * Language

type ParseResult<'T> =
| Success of 'T * string
| Failure of string * string

type ErrorResponse = {
    code : int
    friendlyName : string
    message : string
    langugae : string
}

module ErrorResponses =
    let parseError = {
        code = 0
        friendlyName = "Parse error"
        message = "There was an error parsing the parameters"
        langugae = "EN"
    }

type LocalThreadBuffer =
    private new () = {}
    [<ThreadStatic>] [<DefaultValue>] static val mutable private localThreadBuffer:LocalThreadBuffer


module Random =
    let getRandomBytes =
        //A function to get an arbritrary sized array of random bytes
        //It is designed to be thread safe and high performance
        //Calling this function has a high overhead so should ideally only be called once per http request
        
        let byteQueue = System.Collections.Concurrent.ConcurrentQueue<byte[]>()
        let fillByteQueueMailbox = MailboxProcessor<unit>.Start  (fun mb -> 
            async {
                let rnd = new System.Security.Cryptography.RNGCryptoServiceProvider()
                while (true) do
                    printfn "refill byte queue waiting"
                    let! _ = mb.Receive()
                    printfn "refill byte queue involked"
                    do! Async.Sleep 2
                    while (byteQueue.Count < 160) do
                        let byteArray = Array.zeroCreate (256 * 1024)
                        rnd.GetBytes byteArray
                        byteQueue.Enqueue byteArray
                        printfn "Byte queue count: %i %i" byteQueue.Count System.Threading.Thread.CurrentThread.ManagedThreadId
                    rnd.Dispose()
                    
                    printfn "refill byte queue done"
            }
        )

        fillByteQueueMailbox.Post()

        let dequeueByteArray() = async {
            let byteArray : byte array ref = ref null
            while (not (byteQueue.TryDequeue(byteArray))) do
                fillByteQueueMailbox.Post()
                do! Async.Sleep 200
            fillByteQueueMailbox.Post()
            return !byteArray
            }
        
        let tempArray = new ThreadLocal<_>(fun () -> 
            printfn "New tempArray, threadId: %i" System.Threading.Thread.CurrentThread.ManagedThreadId
            ref (dequeueByteArray() |> Async.RunSynchronously)
        )
        let tempArrayIndex = new ThreadLocal<_>(fun () -> ref 0)

        fun num ->
            async {
                let byteArray = Array.zeroCreate<byte> num
                let ms = new System.IO.MemoryStream(byteArray)
                while (ms.Position < ms.Length) do
                    if ((!tempArrayIndex.Value) = (!tempArray.Value).Length) then
                        tempArrayIndex.Value := 0
                        let! temp = dequeueByteArray()
                        tempArray.Value := temp

                    let lengthRemaining =
                        System.Math.Min((int ms.Position) + (256 * 1024), int ms.Length) - int ms.Position
                    
                    let tempArrayReadLength = System.Math.Min(lengthRemaining, (!tempArray.Value).Length - !tempArrayIndex.Value)

                    ms.Write(!tempArray.Value, !tempArrayIndex.Value, tempArrayReadLength)
                    tempArrayIndex.Value := !tempArrayIndex.Value + tempArrayReadLength
                ms.Dispose();
                return byteArray
            }

            
    let powOf2Array = [|1 .. 64|] |> Array.map(fun i -> Math.Pow(2.0, float i) |> uint64) 
    let getCeilPowerOf2 value =
        powOf2Array |> Array.find(fun i -> value < i)

    let getRandIntArray maxValue numOfItems maxValDecending =
        async {
            let getRandBytes() = Math.Max(8 * 8, 8 * 8 * numOfItems) |> getRandomBytes

            let! randBytesTemp = getRandBytes()
            let randBytes = ref randBytesTemp

            let index = ref 0
            let returnArrayIndex = ref 0
            let returnInts = Array.zeroCreate numOfItems
            
            for _ in Array.zeroCreate<uint64> numOfItems do
                let ceilPowerOf2 = getCeilPowerOf2 maxValue
                let randInt = ref (Option<uint64>.None)
                while ((!randInt).IsNone) do
                    if (!index = (!randBytes).Length) then
                        let! randBytesTemp' = getRandBytes()
                        randBytes := randBytesTemp'
                        index := 0
                    let randUint64 = BitConverter.ToUInt64(!randBytes, !index)
                    index := !index + 8
                    let randCapped = randUint64 % ceilPowerOf2
                    if (randCapped < maxValue + 1UL) then
                        randInt := Some randCapped
                returnInts.[!returnArrayIndex] <- (!randInt).Value
                returnArrayIndex := !returnArrayIndex + 1
            return returnInts
            
        }
        
    let getRandInts intCount start end'  =
        getRandIntArray (end' - start |> uint64) intCount false |> Async.map(Array.map ((+) start))
        
    let getRandInt start end' = getRandInts 1 start end' |> Async.map(Array.item 0)
        
    let shuffle count =  async {
        let array : uint64 array = Array.zeroCreate count |> Array.mapi (fun i _ -> uint64 (i + 1))
        let n = ref 0//array |> Array.length |> ref;  
        while (!n < array.Length) do
            let! k = getRandInt 0UL (uint64 !n)// |> int //rnd.Next(!n + 1); 
            let k' = int k
            let value = array.[k']
            array.[k'] <- array.[!n]
            array.[!n] <- value
            n := !n + 1
        return array
    }

module ParameterParse =
    open System.Reflection.Metadata.Ecma335
    open Suave.Http
    open System.Net

    let tryParseInt str =
        let result = ref 0UL;
        match UInt64.TryParse(str, result) with
        | true -> Some !result
        | false -> None

    let tryParseIntWhileValid (str : string) =
        let charAccListToResult chars remainder =
            let parseIntFromAcc = List.rev >> List.toArray >> String >> tryParseInt
            match parseIntFromAcc chars with
            | Some i ->
                Success (i, remainder |> List.toArray |> String)
            | None -> Failure ("Could not parse int", "")
        let rec parseInt (charAcc : char list) (chars : char list) =
            match chars with
            | h::t ->
                match ((tryParseInt (string h)).IsSome) with
                | true -> parseInt (h::charAcc) t
                | false -> charAccListToResult charAcc chars
            | [] -> charAccListToResult charAcc chars
        parseInt [] (str.ToCharArray() |> Array.toList)
    
    let tryParseParams (request : HttpRequest) =
        let path = request.path.Substring(1)
        let (|Prefix|_|) (p:string) (s:string) =
            if s.StartsWith(p) then
                Some(s.Substring(p.Length))
            else
                None
                
        let parseLangStr (str : string) =
            let rec parser (charAcc : char list) (chars : char list) =
                match chars with
                | h::t -> 
                    match h with
                    | '/' -> (charAcc |> List.rev |> List.toArray |> String), (t |> List.toArray |> String)
                    | h ->
                        match (tryParseInt (string h)).IsSome with
                        | true -> "", str
                        | false -> parser (h::charAcc) t
                | [] -> "", str
            match str.ToCharArray() |> Array.toList |> parser [] with
            | "" , remainder | "en", remainder -> Success (EN, remainder)
            | "ga", remainder  -> Success (GA, remainder)
            | _, str -> Failure ("Could not parse language parameter", "en/6")

        let parseModeStr (str : string) =
            let rec parser (charAcc : char list) (chars : char list) =
                match chars with
                | h::t -> 
                    match not (tryParseInt (string h)).IsSome with
                    | true -> parser (h::charAcc) t
                    | false -> (charAcc |> List.rev |> List.toArray |> String), chars |> List.toArray |> String
                | [] -> (charAcc |> List.rev |> List.toArray |> String), chars |> List.toArray |> String
            str.ToCharArray() |> Array.toList |> parser []
            |> Success

        let parseRange (str : string, divider : char) =
            let rec parser (num1CharsAcc : char list, num2CharsAcc : char list option)  (str' : char list)= 
                match str' with
                | h::t -> 
                    match h with
                    | num when (tryParseInt (string num)).IsSome -> 
                        match num2CharsAcc with
                        | None ->
                            parser ((h :: num1CharsAcc), None) t
                        | Some num2CharsAccVal ->
                            parser (num1CharsAcc, (Some (h :: num2CharsAccVal))) t
                    | h when h = divider ->
                            parser (num1CharsAcc, (Some [])) t
                    | _ -> (num1CharsAcc, num2CharsAcc), (str' |> List.toArray |> String)
                | [] -> (num1CharsAcc, num2CharsAcc), (str' |> List.toArray |> String)
                        
            let (num1, num2), remainder = str.ToCharArray() |> Array.toList |> parser ([], None)

            let charListToInt = List.rev >> List.toArray >> String >> tryParseInt

            let (num1Int, num2IntOption), remiander = (charListToInt num1 , match num2 with | Some charList -> Some (charListToInt charList) | None -> None), remainder

            match num1Int with
            | Some num1IntVal ->
                match num2IntOption with
                | Some io -> 
                    match io with
                    | Some i -> Success ((Range (num1IntVal, i)), remiander)
                    | None -> Failure ("Could not parse range parameter", "6")
                | None ->  Success ((Range.Single num1IntVal), remainder)
            | None -> Failure ("Could not parse range parameter", "6")
            
        let parseMultiplier (str : string) =
            match str with
            | Prefix "x" rest -> 
                match tryParseIntWhileValid rest with
                | Success (result, remainder) ->
                    Success (Some (int result), remainder)
                | Failure (_, _) -> Failure ("Could not parse multiplier parameter", "6x10")
            | _ -> Success (None, str)
            
        let parseFormat (str : string) =
            match str with
            | Prefix "." rest -> 
                match (rest.ToLowerInvariant()) with
                | "json" -> Success (JSON, "")
                | "txt" -> Success (TXT, "")
                | "bin" -> Success (BIN, "")
                | _ -> Failure ("Could not parse format parameter", "6.txt")
            | _ -> Success (HTML, str)

        //let bind f (parseResult : 'T ParseResult)  =
        //    match parseResult with
        //    | Success (value, remainder) -> f remainder value
        //    | Failure e -> Failure e

        match parseLangStr path with
        | Success (lang, remainder) -> 
            match parseModeStr remainder with
            | Success (result, remainder) ->
                match (result, remainder) with
                | "binary", rest | "b", rest -> 
                    match tryParseIntWhileValid rest with
                    | Success (byteCount, _) ->
                        Ok (Params.Binary (int byteCount, lang))
                    | Failure (m,t) ->
                        Error ("Could not parse byte count", "binary1024")
                | "shuffle", rest | "s", rest -> 
                    match tryParseIntWhileValid rest with
                    | Success (shuffleCount, remainder') ->
                        match parseFormat remainder' with
                        | Success (format, _) ->
                            Ok (Params.Shuffle (int shuffleCount, format, lang))
                        | Failure (m,t) -> 
                            Error ("Could not parse format", sprintf "shuffle%i.txt" shuffleCount)
                    | Failure (m,t) ->
                        Error ("Could not parse length", "shuffle26")
                | "lottery", rest | "l", rest -> 
                    match parseRange (rest, '-') with
                    | Success (range, remainder') ->
                        match range with
                        | Range (n1, n2) ->
                            if n1 <= uint64 0 then
                                Failure ("The range of numbers must be greater than 0", "lottery48-7")
                            else if n2 <= uint64 0 then
                                Failure ("The length parameter must be greater than 0", "lottery48-7")
                            else if n1 < n2 then
                                Failure ("The length parameter must be equal to or greater than the range", "lottery48-7")
                            else
                                Success ((n1,n2), remainder')
                        | Single _ -> Failure ("You must provide a value for count", "lottery48-7")
                        |> function
                        | Success ((count,range), remainder') ->
                            match parseFormat remainder' with
                            | Success (format, _) ->
                                Ok (Params.Lottery (int range, int count, format, lang))
                            | Failure (m,t) -> Error (m,t)
                        | Failure (m,t) -> Error (m,t)
                    | Failure (m,t) ->
                        Error ("Could not parse range", "lottery48-7")
                | "standard" as mode, rest 
                | mode, rest when mode = "" -> 
                    match parseRange (rest, '-') with
                    | Success (range, remainder) ->
                        let rangeStr =
                            match range with
                            | Range.Single n -> string n
                            | Range (n1,n2)-> sprintf "%i-%i" n1 n2
                        match range with
                        | Range (n1, n2) ->
                            if n1 < uint64 0 then
                                Failure ("Start of range value must be greater or equal to 0", "0-6")
                            else if n1 >= n2 then
                                Failure ("End of range must be greater than start of range", "0-6")
                            else
                                Success (range, remainder)
                        | Single n ->
                            if n < uint64 1 then
                                Failure ("Range value must be greater than 0", "6")
                            else
                                Success (range, remainder)
                        |> function
                        | Success (range, remainder) ->
                            match parseMultiplier remainder with
                            | Success (multiplier, remainder') ->
                                let multiplierStr =
                                    match multiplier with
                                    | Some n -> sprintf "x%i" n
                                    | None -> ""
                                match parseFormat remainder' with
                                | Success (format, _) ->
                                    Ok (Params.Standard (range, multiplier, format, lang))
                                | Failure (m,t) -> 
                                    Error ("Could not parse format", sprintf "%s%s.txt" rangeStr multiplierStr)
                            | Failure (m,t) ->
                                Error ("Could not parse multiplier parameter", sprintf "%sx6" rangeStr)
                        | Failure (m,t) -> Error (m,t)
                    | Failure (m,t) -> Error (m,t)
                | _ -> Error ("Could not parse the mode", "shuffle26")
            | _ -> Error ("Could not parse the mode", "shuffle26")
        | Failure (m,t) -> Error (m,t)
        |> function
            | Ok params' -> (params', request.host |> IPAddress.Parse) |> Ok
            | Error (m,t) -> Error (m,t)
        

module Qouta =
    open System.Collections.Concurrent
    open System.Net

    type QuotaFeildHeaderKey(header : String) = 
        inherit Attribute()
        member x.Header with get() = header
    type QuotaInfo = {
        [<QuotaFeildHeaderKey("X-Number-Qouta-Remaining")>]
        numberQoutaRemaining : int 
        [<QuotaFeildHeaderKey("X-Request-Qouta-Remaining")>]
        requestQoutaRemaining : int 
        [<QuotaFeildHeaderKey("X-Raw-Binary-Qouta-Remaining")>]
        rawBinaryQoutaRemaining : int 
        [<QuotaFeildHeaderKey("X-Qouta-Reset-Date")>]
        quoateResetDate : DateTime 
    }
    
    let defaultQoutaInfo() = {
        numberQoutaRemaining = 100000
        requestQoutaRemaining = 100000
        rawBinaryQoutaRemaining = 1000000
        quoateResetDate = DateTime.Now + TimeSpan.FromDays(1.0)
    }
    let private qoutaDict = ConcurrentDictionary<IPAddress, (QuotaInfo)>()
    
    let updateQoutaData f ip =
        qoutaDict.AddOrUpdate(ip, ignore >> defaultQoutaInfo >> f, (fun _ b -> f b))
        |> ignore

    let getQoutaData ip = 
        let qoutaData = qoutaDict.GetOrAdd(ip, ignore >> defaultQoutaInfo)
        if qoutaData.quoateResetDate < DateTime.Now then
            defaultQoutaInfo()
        else
            qoutaData
    
    let subtractNumbersRemaining i q =      {q with numberQoutaRemaining = Math.Max(q.numberQoutaRemaining - i, 0)}
    let subtractRequestsRemaining i q =     {q with requestQoutaRemaining = Math.Max(q.requestQoutaRemaining - i, 0)}
    let subtractRawBinaryRemaining i q =    {q with rawBinaryQoutaRemaining = Math.Max(q.rawBinaryQoutaRemaining - i, 0)}
    
module ViewModel =
    type ResponseType =
    | Binary of byte [] Async list
    | SingleNum of string * uint64 Async * Format * Language
    | ManyNum of string * uint64 [] Async list * Format * Language

    type Error = {
        errorCode: int
        languageCode: string
        friendlyName: string
        description: string
    }

    let createQoutaError (message, adviceUrl) =
        Error {
            errorCode = 0
            languageCode = "en"
            friendlyName = "Qouta error"
            description = sprintf "%s\n\ntry:\nrndm.nu/%s" message adviceUrl
        } 


    
    let paramsToViewModel (params') =
        match params' with
        | Ok (params', ip) ->
            let qoutaInfo = Qouta.getQoutaData ip
            Qouta.updateQoutaData (Qouta.subtractRequestsRemaining 1) ip
            match params' with
            | Params.Standard (range, multiplier, format, lang) ->
                let a, b =
                    match range with
                    | Single b -> uint64 1, b
                    | Range (a, b) -> a, b
                match multiplier with
                | None ->
                    SingleNum (sprintf "A random number between %i and %i" a b, Random.getRandInt a b, format, lang) |> Ok
                | Some mult ->
                    if mult > 128 && qoutaInfo.numberQoutaRemaining - mult < 0 then
                        createQoutaError("The remaining number qouta is too low to complete this request. You can still make requests for up to 128 numbers without affecting your number qouta.", "6x128")
                    else
                        if mult > 128 then
                            Qouta.updateQoutaData (Qouta.subtractNumbersRemaining mult) ip
                        let numsPerBlock = 1000
                        let list = [Array.create (mult / numsPerBlock) numsPerBlock; (if mult % numsPerBlock = 0 then [||] else [|mult % numsPerBlock|])] |> Array.concat |> List.ofArray
                        let t = list = list
                        //ManyNum (sprintf "%i random numbers between %i and %i" mult a b, list |> List.map(fun mult -> async { return Array.create mult range |> Array.map Random.rangeToRand }), format, lang)
                        ManyNum (sprintf "%i random numbers between %i and %i" mult a b, list |> List.map(fun mult -> Random.getRandInts mult a b), format, lang)
                        |> Ok
                        //ManyNum (sprintf "%i random numbers between %i and %i" mult a b, list |> List.map(fun mult -> async { return Array.zeroCreate<uint64> mult }), format, lang)
            | Params.Binary (byteCount, lang) ->
                if byteCount > 4096 then
                    Qouta.updateQoutaData (Qouta.subtractRawBinaryRemaining byteCount) ip
                Binary [Random.getRandomBytes byteCount]|> Ok
            | Params.Shuffle (count, format, lang) ->
                ManyNum (sprintf "A shuffled list of %i numbers" count, [Random.shuffle count], format, lang) |> Ok
            | Params.Lottery (count, ballNum, format, lang) ->
                ManyNum (sprintf "%i numbers picked from a lottery of %i numbers" count ballNum, [Random.shuffle ballNum |> Async.map(Array.take count)], format, lang )|> Ok
            |> function | Ok o -> (o, Qouta.getQoutaData ip) |> Ok | Error e -> Error e
        | Error (message, urlAdvice) -> 
            {
                errorCode = 0
                languageCode = "en"
                friendlyName = "Parse error"
                description = sprintf "%s\n\ntry:\nrndm.nu/%s" message urlAdvice
            } |> Error


 module Render =
    open Suave.Sockets.Control
    open Suave.Sockets
    open System.Diagnostics
    
    let bytes = fun (str : string) -> Text.UTF8Encoding.UTF8.GetBytes str
    let htmlTemplateString =
        use file =
            IO.Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "pages", "html_template.html")
            |> IO.File.OpenText
        file.ReadToEnd()
        
    let htmlErrorTemplateString =
        use file =
            IO.Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "pages", "html_error_template.html")
            |> IO.File.OpenText
        file.ReadToEnd()

        
    let stringToTemplate (template : string) =
        let keys = 
            template.Split("#{", StringSplitOptions.None) 
            |> Array.skip 1 
            |> Array.map (fun str -> str.Substring(0, str.IndexOf('}')))

        let strSections =
            template.Split("#{", StringSplitOptions.None) 
            |> Array.mapi(fun i str ->
                if i = 0 then
                    str
                else
                    let strIndex = str.IndexOf('}')
                    str.Substring(strIndex + 1, str.Length - strIndex - 1)
            )

        keys, strSections

    let templateToHtml (keys : string array, templateSections : string array) (data : Map<string, string Async seq>) =
        seq {
            for i in [0 .. keys.Length + templateSections.Length - 1] do
                if i % 2 = 0 then
                    yield templateSections |> Array.item (i / 2) |> bytes |> Async.result
                else
                    for item in data |> Map.find (keys |> Array.item ((i - 1) / 2)) do
                        yield item |> Async.map bytes
        }
        
    let htmlTemplate = stringToTemplate htmlTemplateString
    let htmlErrorTemplate = stringToTemplate htmlErrorTemplateString


    
    let viewModelToResponse (params' : Result<(ViewModel.ResponseType * Qouta.QuotaInfo), ViewModel.Error>) (ctx : HttpContext)  =
        let jsonMime = "application/json"
        let binMime = "application/octet-stream"
        let htmlMime = "text/html"
        let txtMime = "text/plain"


        
        let byteToSend, mime, httpCode, qoutaHeaders =
            match params' with
            | Ok (result, qoutaInfo) ->
                let qoutaHeaders =
                    FSharp.Reflection.FSharpType.GetRecordFields(typeof<Qouta.QuotaInfo>)
                    |> Array.map (fun feildInfo ->
                        let headerKeyInfo = feildInfo.GetCustomAttributes(typeof<Qouta.QuotaFeildHeaderKey>, false) |> Array.item 0 :?> Qouta.QuotaFeildHeaderKey
                        headerKeyInfo.Header, (feildInfo.GetValue(qoutaInfo) |> string)
                    ) |> List.ofArray

                match result with
                | ViewModel.ResponseType.SingleNum (title, number, format, lang) -> 
                    match format with
                    | HTML ->
                            [
                                "dataType", seq { yield ("single" |> Async.result )}
                                "data", seq { yield (number  |> Async.map(string))}
                                "title", seq { yield (title |> Async.result)}
                            ]
                            |> Map.ofList
                            |> templateToHtml (htmlTemplate)
                            , htmlMime
                    | TXT ->
                        seq {yield number |> Async.map(string >> bytes)}
                        , txtMime
                    | JSON ->
                        seq {yield number |> Async.map(string >> bytes)}
                        , jsonMime
                    | BIN ->
                        seq {yield number |> Async.map(BitConverter.GetBytes)}
                        , binMime
                | ViewModel.ResponseType.ManyNum (title, numbers, format, lang) -> 
                    match format with
                    | HTML ->
                            [
                                "dataType", seq { yield ("list" |> Async.result)}
                                "data", (numbers |> Seq.ofList |> Seq.mapi (fun i numbers -> 
                                                numbers |> Async.map (Array.map string) |> Async.map (fun strs -> (if i > 0 then ", " else "") + String.Join(", ", strs))
                                ))
                                "title", seq {yield (title |> Async.result)}
                            ]
                            |> Map.ofList
                            |> templateToHtml (htmlTemplate)
                            , htmlMime
                    | TXT ->
                        (numbers |> Seq.ofList |> Seq.mapi (fun i numbers -> 
                                                numbers |> Async.map (Array.map string) |> Async.map (fun strs -> (if i > 0 then ", " else "") + String.Join(", ", strs)) |> Async.map bytes
                                ))
                        , txtMime
                    | JSON ->
                        (numbers |> Seq.ofList |> Seq.mapi (fun i numbers' -> 
                                                numbers' |> Async.map (Array.map string) |> Async.map (fun strs -> (if i > 0 then ", " else "") + String.Join(", ", strs))
                                                |> Async.map (if i = 0 then sprintf "[%s" else id)
                                                |> Async.map (if i = numbers.Length - 1 then sprintf "%s]" else id)
                                                |> Async.map bytes

                                ))
                        , jsonMime
                    | BIN ->
                        (numbers |> Seq.ofList |> Seq.map (Async.map (fun numberArray -> 
                                            let byteArray = Array.zeroCreate<byte> (numberArray.Length * 8)
                                            Buffer.BlockCopy(numberArray, 0, byteArray, 0, byteArray.Length)
                                            byteArray
                        )))
                        , binMime
                | ViewModel.ResponseType.Binary (binary) -> 
                    binary |> Seq.ofList 
                    , binMime
                |> fun  (a, b) -> (a, b, HTTP_200, qoutaHeaders )
            | Error e ->
                [
                    "friendlyName", seq { yield (e.friendlyName |> Async.result )}
                    "description", seq { 
                        yield 
                            e.description.Split('\n')
                            |> Array.map(fun str ->
                                if str.StartsWith("rndm.nu") then
                                    sprintf """<a href="https://%s">%s</a>""" str str
                                else str
                            ) |> FSharp.Core.String.concat "<br>" |> Async.result
                    }
                    "errorCode", seq { yield (e.errorCode |> string |> Async.result)}
                ]
                |> Map.ofList
                |> templateToHtml (htmlErrorTemplate)
                , htmlMime, HTTP_400, []


               
        
        let write' (conn, _) = socket {
            printfn "write start"
            let sw = Stopwatch()
            sw.Start()
            let rec transferBytes (conn : Connection) (bytesToSend : byte [] Async IEnumerator) = socket {
                match bytesToSend.MoveNext() with
                | false -> 
                    sw.Stop()
                    printfn "sw: %i" sw.ElapsedMilliseconds
                    return conn 
                | true ->
                    let! bytes = bytesToSend.Current |> SocketOp.ofAsync
                    printfn "writeChunk %s" (bytes |> Text.UTF8Encoding.UTF8.GetChars |> String)
                    let! _, conn = HttpOutput.writeChunk bytes conn
                    return! transferBytes conn bytesToSend 
            }
            let! (_, conn) = asyncWriteLn "" conn
            let! conn = flush conn
            //let! conn = transferBytes conn (byteToSend |> Array.ofSeq |> Seq.ofArray |> fun o -> o.GetEnumerator())
            let! conn = transferBytes conn (byteToSend.GetEnumerator())
            //let! _, conn = HttpOutput.writeChunk [|22uy|] conn
            let! _, conn = HttpOutput.writeChunk [||] conn
            printfn "write end"
            return conn
        }

        { ctx with
            response =
                { ctx.response with
                    status = httpCode.status
                    headers =   [[
                                    "Content-type", mime
                                    "Transfer-Encoding", "chunked"
                                ]; qoutaHeaders] |> List.concat 
                    content = SocketTask write' 
                    writePreamble = true
                } 
        }
        |> succeed

module main =
    [<EntryPoint>]
    let main argv =
        do //Start the http server
            //let tt = tryParseParams >> ViewModel.paramsToViewModel >> Render.viewModelToResponse
    
            let app =
                choose [
                    Suave.Filters.GET >=> path "" >=> Files.file (Path.Combine("pages", "en", "index.html"))
                    request (ParameterParse.tryParseParams >> ViewModel.paramsToViewModel >> Render.viewModelToResponse)
                ]
            
            let conf = 
                let port =
                    match argv with
                    | [||] -> 8080
                    | a -> int a.[0]
                { 
                    defaultConfig with 
                        bindings = [ HttpBinding.createSimple HTTP (Net.IPAddress.Any.ToString()) port ]
                }
            
            startWebServer conf app
        0
    