namespace Simulator

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open PublicWeb.PublicWeb.Database

type CustomMemoryStream(closeCallback : CustomMemoryStream -> unit) =
    inherit IO.MemoryStream() with
        override this.Close() =
            base.Close()
            closeCallback(this)
type MiddleWare(next: RequestDelegate) = 

    
    let db =
        let promiseDict = new System.Collections.Concurrent.ConcurrentDictionary<string, string>()
        let fulfillmentDict = new System.Collections.Concurrent.ConcurrentDictionary<string, string>()
        let binaryDict = new System.Collections.Concurrent.ConcurrentDictionary<string, (string * byte[])>()

        DatabaseEngine(
            (fun (table, id) -> async {
                match (match table with | Promise -> promiseDict | Fulfilment -> fulfillmentDict).TryGetValue(id) with
                | (true, content) ->
                    return Ok(Some content)
                | _ -> 
                    return Ok None
            }), (fun (table, id, content) -> async {
                (match table with | Promise -> promiseDict | Fulfilment -> fulfillmentDict).TryAdd(id, content) |> ignore
                return Ok ()
            }), (fun (id) -> async {
                 match binaryDict.TryGetValue(id) with
                 | (true, (mime, content)) ->
                     return Ok (Some (mime, (new IO.MemoryStream(content)) :> IO.Stream))
                 | _ -> 
                     return Ok None
            }), (fun (id, mime) -> async {
                 let ms = 
                    new CustomMemoryStream(fun stream ->
                        let a,b = promiseDict, fulfillmentDict
                        let bytes = stream.ToArray()
                        let txt = Text.UTF8Encoding.UTF8.GetString(bytes)
                        let r = binaryDict.TryAdd(id, (mime, bytes)) 
                        ()
                    )
                 return Ok (ms :> IO.Stream)
            })
        )


    member this.Invoke (context: HttpContext) =
        PublicWeb.PublicWeb.requestHandler (None, Some db) context

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