namespace SuaveOnly
module HtmlTemplate =
    open FSharp.Reflection.FSharpReflectionExtensions
    open System.Reflection
    open System.IO
    open System
    open System.Reflection.Metadata.Ecma335
    open Suave.Http

    type TemplateItem =
    | HTML of string
    | Key of string

    type StringMapType =
    | Str of string
    | Template of string * obj
    
    let createTemplate (str: string) =
        //let strings = 
        //    Core.languages
        //    |> List.filter ((=) Core.defaultLanguage >> not)
        //    |> List.map(fun lang -> lang, (getStringMap ("index", lang)))
        //    |> Map.ofList

        str.Split("[@")
        |> Array.mapi (fun i str ->
            if i = 0 then
                [HTML str]
            else
                let endIndex = str.IndexOf("]")
                [
                    Key (str.Substring(0, endIndex))
                    HTML (str.Substring(endIndex + 1))
                ]
        )
        |> List.concat

        


    type Key =
    | LocalisedString of String
    | ProgrammaticString of String
    | Template of String * Map<string,string>

    type Page = {
        name : string
        HTMLSegments : string array
        keys : Key array
        controller : Controllers.Controller
    }

    let loadPage (pageName : string) = async {
        
        use file = 
            Directory.EnumerateFiles(Path.Combine(Core.rootDir, "Pages"), "*.html", EnumerationOptions(RecurseSubdirectories = true))
            |> Seq.find(fun fi -> Path.GetFileNameWithoutExtension(fi) = pageName)
            |> File.OpenText

        let! fileString = file.ReadToEndAsync() |> Async.AwaitTask
        let t  = fileString.Split("[@")

        let controller : Controllers.Controller = 
            let t = typeof<Controllers.Marker>
            let tt = t.DeclaringType
            tt.GetMethods()
            |> Array.tryFind (fun methodInfo ->
                methodInfo.Name = pageName
            )
            |> function
            | Some info -> fun (funParams, req) -> 
                let obj = info.Invoke(typeof<Controllers.Controller>, [|funParams; req|])
                obj
            | None -> fun (_,_) -> obj()


        return 
            fileString.Split("[@")
            |> Array.fold (fun (i, segs, keys) str ->
                if i = 0 then
                    (i + 1, str::segs, keys)
                else
                    let pn = pageName
                    let endIndex = str.IndexOf("]")
                    let key = str.Substring(0, endIndex)
                    let html = (str.Substring(endIndex + 1))
                    //let keyName = key.Substring(1)
                    let key =
                        match key.[0] with
                        | '@' -> LocalisedString (key.Substring(1))
                        | '$' -> 
                            match key.IndexOf("(") with
                            | -1 ->
                                ProgrammaticString (key.Substring(1))
                            | index ->
                                let varNames = 
                                    key.Substring(index + 1, key.Length - index - 2).Split(",", StringSplitOptions.RemoveEmptyEntries)
                                    |> Array.map(fun str -> 
                                        let pn = pageName
                                        let k = key
                                        let keyVal = str.Split(":")
                                        let keyVal =
                                            let ind = str.IndexOf(":")
                                            [|(str.Substring(0, ind)); (str.Substring(ind + 1))|]
                                        keyVal.[0].Trim(' '), keyVal.[1].Trim(' ').Trim('"'))
                                
                                Template (key.Substring(1, index - 1), varNames |> Map.ofArray)
                    (i + 1, html::segs, key::keys)
            ) (0, List.empty<string>, List.empty<Key>)
            |> (fun (_, segs, keys) -> 
                    {
                        name = pageName
                        HTMLSegments = segs |> List.rev |> Array.ofList
                        keys = keys |> List.rev |> Array.ofList
                        controller = controller
                    }
            )
    }

    let pages =
        Directory.EnumerateFiles(Path.Combine(Core.rootDir, "Pages"), "*.html", EnumerationOptions(RecurseSubdirectories = true))
        |> Seq.map(Path.GetFileNameWithoutExtension) 
        |> Seq.map (loadPage >> Async.RunSynchronously)
        |> Seq.toArray

    let globalParams = 
        [
            "staticRoot", Core.staticFileRoot
        ]
        |> Map.ofList

    let rec createPageResponse(pageName : string, funParams : Map<string,string>, ctx : Suave.Http.HttpContext) =
        let page = pages |> Array.find (fun page -> page.name = pageName)
        let controllerParams = page.controller(funParams, ctx)
        page.HTMLSegments
        |> Array.mapi(fun i html ->
            if i < page.keys.Length then
                Some page.keys.[i]
            else
                None
            |> function
            | Some key ->
                match key with
                | LocalisedString str -> str
                | ProgrammaticString str -> 
                    match globalParams |> Map.tryFind str with
                    | Some value -> value
                    | None ->       
                        FSharp.Reflection.FSharpType.GetRecordFields(controllerParams.GetType()) 
                        |> Array.find(fun info -> info.Name = str)
                        |> fun info -> info.GetValue(controllerParams) :?> string
                | Template (name, params') -> createPageResponse(name, params', ctx)
            | None -> ""
            |> sprintf "%s%s" html
        )
        |> String.concat ""
    
    let rr = createPageResponse("payment", Map.empty, HttpContext.empty)
    let t = ()