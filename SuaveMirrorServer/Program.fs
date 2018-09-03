open System
open System.Threading
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Writers

let escapeJsonString str =
    str |> String.map (fun c -> match c with
                                | c when c = '"' -> '\''
                                | c              -> c)
        |> String.filter ((fun c -> (c = '\n' || c = '\r')) >> not)

type Message =
    {message: string}

    member x.ToJson () =
        sprintf "{\"message\":\"%s\"}" <| escapeJsonString x.message

[<EntryPoint>]
let main argv =

    let cts = new CancellationTokenSource ()
    let conf = {defaultConfig with cancellationToken = cts.Token}

    let setCorsHeaders =
        setHeader "Allow" "GET, POST"
        >=> setHeader "Access-Control-Allow-Methods" "GET, POST"
        >=> setHeader "Access-Control-Allow-Origin" "*"
        >=> setHeader "Access-Control-Allow-Headers" "Content-Type"

    let preflightReq =
        request (fun r ->
            printfn "[%s INF] Received OPTIONS request from %s at url %s. Details:\n%s\nProcessing..."
                (DateTime.Now.ToLongTimeString()) (r.host) (r.url.ToString()) (r.ToString())
            setCorsHeaders >=> OK "")

    let req200 =
        request (fun r ->
            printfn "[%s INF] Received %s request from %s at url %s. Details:\n%s\nProcessing..."
                (DateTime.Now.ToLongTimeString()) (r.method.ToString().ToUpper()) (r.host) (r.url.ToString()) (r.ToString())
            setMimeType "text/plain; charset=utf-8" >=> setCorsHeaders >=> ok (UTF8.bytes "Hello World!"))

    let req404 =
        request (fun r ->
            printfn "[%s WAR] Received request to unexisting or unrecognized path from %s at url %s. Details:\n%s\nReturning a 404 error..."
                (DateTime.Now.ToLongTimeString()) (r.host) (r.url.ToString()) (r.ToString())
            setMimeType "application/json; charset=utf-8" >=> setCorsHeaders >=> RequestErrors.not_found (UTF8.bytes <| {message = "Error: page not found"}.ToJson()))

    let req400 =
        request (fun r ->
            printfn "[%s WAR] Received a bad request from %s at url %s. Details:\n%s\nReturning a 400 error"
                (DateTime.Now.ToLongTimeString()) (r.host) (r.url.ToString()) (r.ToString())
            setMimeType "application/json; charset=utf-8" >=> setCorsHeaders >=> RequestErrors.bad_request (UTF8.bytes <| {message = "Error: Unrecognized method or request syntax"}.ToJson()))

    let mirrorReq =
        request (fun r ->
            printfn "[%s INF] Received OPTIONS request from %s at url %s. Details:\n%s\nProcessing..."
                (DateTime.Now.ToLongTimeString()) (r.host) (r.url.ToString()) (r.ToString())
            setMimeType <| snd (r.headers |> List.filter (fun (head, _) -> head = "content-type")).Head >=> setCorsHeaders >=> ok r.rawForm)

    let main : WebPart =
        choose
            [GET >=> choose
                [path "/" >=>
                    req200
                 req404]
             POST >=> choose
                [path "/" >=>
                    mirrorReq
                 req404]
             OPTIONS >=> preflightReq
             req400]

    let _, server = startWebServerAsync conf main
      
    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore
      
    cts.Cancel()
    
    0 // return an integer exit code
