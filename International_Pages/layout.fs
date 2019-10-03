namespace SuaveOnly.Pages.International.Strings

module layout =
    let strings = [
        "title", (fun () -> "")
        "langList", (fun () -> 
            SuaveOnly.Core.languages
            |> List.map (fun lang -> sprintf "<a>%s</a>" lang)
            |> String.concat ""
            |> sprintf "<div>%s</div>"
        )
    ]
