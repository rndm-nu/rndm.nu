namespace PublicWeb

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open System.Reflection
open FSharp.Control.Tasks.ContextSensitive
open System.Threading

module HtmlTemplateEngine =
    type HTMLTemplateSpan =
    | Function of string * (obj array)
    | HTML of string


    let createHtmlTemplateEngine(htmlPages : Map<string, (string Async)>, functionMap : Map<string, obj>) = async {
        let! pages = async {
            let! pageList =
                htmlPages
                |> Map.toSeq 
                |> Seq.map (
                    fun (name, content) -> async {
                        let! c = content
                        return (name, c)
                    }
                )
                |> Async.Parallel
            return pageList |> Map.ofArray
        }

        let templates =
            pages |> Map.toSeq |> Seq.map(fun (name, html) ->
                let htmlTemplate = html
                seq {
                    let index = ref (htmlTemplate.IndexOf("<!", 0))
                    while !index >= 0 do
                        yield htmlTemplate.Substring(!index)
                        index :=  htmlTemplate.IndexOf("<!", !index + 1)
                }
                |> Seq.map(fun str ->
                    let functionName = str.Substring(2, str.IndexOf('(') - 2)

                    let pars = 
                        str.Substring(str.IndexOf('(') + 1, str.IndexOf(')') - str.IndexOf('(') - 1).Split(',', StringSplitOptions.RemoveEmptyEntries)
                        |> Array.map(fun str -> str.Trim(' '))
                        |> Array.map(fun par ->
                            match Int32.TryParse par with
                            | (true, num) -> num :> obj
                            | (false, _) ->
                                if par.IndexOf('"') = 0 && par.LastIndexOf('"') = par.Length - 1 then
                                    par.Trim('"') :> obj
                                else
                                    raise (Exception "Could not parse parameter")
                        )

                    let remainder = 
                        let rem = str.Substring(str.IndexOf(")/>") + 3)
                        rem.Substring(0, match rem.IndexOf("<!") with | -1 -> 0 | n -> n )
                    
                    let variable = HTMLTemplateSpan.Function (functionName, pars)
                    if String.IsNullOrEmpty remainder then
                        [ variable ]
                    else
                        [
                            variable
                            HTMLTemplateSpan.HTML remainder
                        ]
                )
                |> Seq.concat
                |> fun seq -> 
                    match htmlTemplate.IndexOf("<!", 0) with
                    | -1 ->     [ HTMLTemplateSpan.HTML htmlTemplate ] |> List.toSeq
                    | 0 ->      Seq.empty
                    | index ->  [ HTMLTemplateSpan.HTML (htmlTemplate.Substring(0, index)) ] |> List.toSeq
                    |> fun s -> Seq.concat [ s; seq ]
                |> Seq.toArray
                |> fun a -> name, a
            )
            |> Map.ofSeq
        ()

        let rnd = new ThreadLocal<_>(fun () -> new Random())

        let rec handleRequest(path : string, writeResponse : string -> Task) : Task<Result<unit, unit>> = task {
            
            let requestId = rnd.Value.Next()
            match templates |> Map.tryFind(path) with
            | Some template ->
                for t in template do
                    match t with
                    | Function (name, pars) ->
                        match name with
                        | "import" -> 
                            match pars with
                            | [|path|] -> 
                                match path with 
                                | :? string as p -> 
                                    let! r = handleRequest(p, writeResponse)
                                    ()
                                    //return r
                                | _ -> raise (Exception "import function path parameter could not be parsed")
                            | _ -> raise (Exception "import function path parameter could not be parsed")
                        | _ ->
                            match functionMap |> Map.tryFind name with
                            | Some func ->
                                let tt = func.GetType()
                                let ttb = tt.BaseType.GetMethods() |> Array.map(fun info -> info.Name, info.GetParameters())
                                //let ttbm = ttb.GetMethods()
                            
                                let t1, _ = FSharp.Reflection.FSharpType.GetFunctionElements tt
                                let f1 = FSharp.Reflection.FSharpType.IsFunction tt


                                let invokeMethodInfo = 
                                    tt.GetMethods()
                                    |> Seq.filter (fun x -> x.Name = "Invoke")
                                    |> Seq.head
                                let requestIdNeeded = true //invokeMethodInfo.GetParameters() |> Array.exists(fun info -> info.Name = "requestId")
                            
                                let parst = 
                                    match pars with
                                    | [||] -> [| requestId :> obj |]
                                    | _ ->
                                        [| FSharp.Reflection.FSharpValue.MakeTuple((if requestIdNeeded then ([| pars; [| requestId :> obj |] |] |> Array.concat) else pars), t1) |]

                                let rrr = invokeMethodInfo.Invoke(func, parst) :?> string

                                do! writeResponse rrr
                            | None -> raise (Exception "could not find function")
                    | HTMLTemplateSpan.HTML html ->
                        do! writeResponse html

                return Ok()
            | None -> return Error ()


        }

        return handleRequest
    }
        

