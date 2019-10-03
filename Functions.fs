namespace SuaveOnly

open Suave

module Functions =
    open FSharp.Reflection.FSharpReflectionExtensions
    open System.Reflection
    open System.IO
    open Suave.Logging
    open SendGrid.Helpers.Mail
    open System.Net
    open System
    open Newtonsoft.Json
    open HelpScout.Conversations.Models.Create
    open System.Collections
    open HelpScout
    open HelpScout.MailBoxes
    open HelpScout.Customers
    open HelpScout.Customers.Emails
    open HelpScout.Customers.Phones
    open HelpScout.Customers.Address

    type Marker = interface end
    type Function = (HttpContext) -> obj
    
    
    type IPCountryResponse = {
            ip : string
            continent_code : string
            continent_name : string
            country_code : string
            country_name : string
            region_code : string
            region_name : string
            city : string
            zip : string
            latitude : string
            longitude : string
            country_flag : string
            country_flag_emoji : string
            country_flag_emoji_unicode : string
            calling_code : string
            is_eu : Boolean
    }

    let ipcountry = fun (ctx : HttpContext) ->
        try
            let ip = ((ctx.request.headers |> Map.ofList) |> Map.find "client-ip").Split(':').[0]
            use wb = new WebClient()
            let jsonStr = wb.DownloadString(sprintf "http://api.ipstack.com/%s?access_key=3b1f00a818a6f6876c5219461bf100fc" ip) 
            Newtonsoft.Json.JsonConvert.DeserializeObject<IPCountryResponse>(jsonStr).country_code
        with
        | e -> "-"
        
    let contactform = fun (ctx : HttpContext) ->
    
        let countryInfo =
            try
                let ip = ((ctx.request.headers |> Map.ofList) |> Map.find "client-ip").Split(':').[0]
                use wb = new WebClient()
                let jsonStr = wb.DownloadString(sprintf "http://api.ipstack.com/%s?access_key=3b1f00a818a6f6876c5219461bf100fc" ip) 
                Newtonsoft.Json.JsonConvert.DeserializeObject<IPCountryResponse>(jsonStr) |> Some
            with
            | e -> None


        let form = ctx.request.form |> Map.ofList |> Map.map (fun _ value -> value.Value)

        let client= new HelpScoutApiClient("bbb90cd1356c49c0b2907e5585f744c0","8f7934e5ee874162a987ce8205b21ed0")

        let token = client.GetToken(true).Result

        let mailboxs = client.Mailboxes.List().Result.Items |> Seq.cast<MailboxDetail>
        
        let supportMailbox = mailboxs |> Seq.find(fun mbd -> mbd.Name.ToLowerInvariant().Contains("support"))
        
        let firstName, lastName =
            let words =
                [| form.["name"].Split(' '); [| "" |] |]
                |> Array.concat

            words |> Seq.item 0, (words |> Seq.skip 1 |> String.concat " ")

        let customerId =
            match client.Customers.List(Customers.CustomerSearchQuery(Query = form.["email"])).Result.Items |> Seq.cast<CustomerListItem> |> List.ofSeq  with
            | customer :: _ -> customer.Id
            | [] ->
                let customerId =
                    (client.Customers.Create(
                        CustomerCreateRequest(
                            FirstName = firstName,
                            LastName = lastName,
                            Emails = Generic.List<CustomerCreateRequest.Email>([
                                CustomerCreateRequest.Email(
                                    Type = "other",
                                    Value = form.["email"]
                                )
                            ]),
                            Location = 
                                match countryInfo with
                                | Some countryInfo -> sprintf "%s, %s" countryInfo.city countryInfo.country_name
                                | None -> ""
                        )
                    )).Result
                
                Customers.Phones.PhoneEndpoint(customerId, client).Create(
                    PhoneCreateRequest(
                        Type = PhoneType.Other,
                        Value = form.["phone"]
                    )
                ).Wait()

                
                
                customerId

        let isSpam = form |> Map.tryFind "email2" |> Option.map ((=) "" >> not) |> Option.defaultValue false

        //client.Customers.
        let req = 
            ConversationCreateRequest(
                MailboxId = supportMailbox.Id,
                Type = ConversationType.Email,
                Subject = "Epic Pen Support Query",
                Tags = Generic.List<string>([["contact-form"]; (if Core.testMode then ["test-mode"] else []); (if isSpam then ["spam-bot"] else [])] |> List.concat),
                Status = ConversationStatus.Active,
                Customer = new Customer(
                    Id =  Nullable<_>(customerId)
                ),
                Threads = Generic.List<ThreadCreateRequest>([
                    ThreadCreateRequest(
                        Type = ThreadType.Customer,
                        Text = form.["message"],
                        Customer = CreateConservationThreadCustomer(
                            Id =  Nullable<_>(customerId |> int)
                        )
                    )
                ])
            )
        if not isSpam then
            client.Conversations.Create(req).Result |> ignore
        "success"