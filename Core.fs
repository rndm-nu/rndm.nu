namespace SuaveOnly
module Core =
    open System.IO
    open System.Reflection
    
    let rootDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    type EnviromentType =
    | DevMachine
    | AzureDev
    | AzureLive
    | Unknown

    let env =
        #if ENV_DEVMACHINE
            DevMachine
        #else
        #if ENV_AZUREDEV
            AzureDev
        #else
        #if ENV_AZURELIVE
            AzureLive
        #else
            Unknown
        #endif
        #endif
        #endif

    let gitBranch, gitCommitHash =
        try
            let headFileStr = File.ReadAllText(Path.Combine(rootDir, "git/HEAD")) |> String.trimc '\n'
            let branches =
                Directory.EnumerateFiles(Path.Combine(rootDir, "git/refs/remotes/origin")) |> Seq.map (
                    fun fileInfo ->
                        (Path.GetFileName(fileInfo)), (File.ReadAllText(fileInfo) |> String.trimc '\n')
                )

            branches |> Seq.find (fun (_, hash) -> hash = headFileStr)
        with
        | e ->
            "Unknown", "0000000000000000000000000000000000000000"

    let gitCommitHashShort = gitCommitHash.Substring(0, 7)
    
    //let CDNURL = "https://epicpenwebsitedev.azureedge.net"
    let CDNURL = 
        match env with
        | AzureLive -> "https://static.epic-pen.com"
        | AzureDev -> "https://static.dev.epic-pen.com"
        | _ -> ""
            
    let staticFileRoot =
        match env with
        //| AzureLive | AzureDev -> sprintf "%s/%s/" CDNURL gitCommitHashShort
        | _ -> sprintf "/%s/" gitCommitHashShort


    let testMode = 
        match env with
        | AzureDev -> true
        | AzureLive -> false
        | _ -> true

    let showBeta = false

    let latestBuild = 3, 7, 1

    let defaultLanguage = "en"
    
    let languages = [
        "en"
        "fr"
    ]

    let indexNotification = None// Some ("Welcome to the new Epic Pen website!","We've just launched our brand new website. If you experience any issues with it please let us know. We are working hard to get it working as smoothly as possible.")