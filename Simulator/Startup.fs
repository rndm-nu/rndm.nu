namespace Simulator

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting


type MiddleWare(next: RequestDelegate) = 
    member this.Invoke (context: HttpContext) =
        PublicWeb.PublicWeb.requestHandler None context

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