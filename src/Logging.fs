module Logging

open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch

//Fable 2 transition 
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)

type SelfExplanationLogEntry082720 =
    {
        schema: string
        code: string
        explanation : string 
    } with
    static member Create code explanation = { schema = "se082720"; code = code; explanation=explanation }

type LogEntry = 
    {
        username: string
        json: string
    }

/// Where we are sending the data
let mutable logUrl : string option = None
 
let mutable idOption : string option = None

/// Log to server. Basically this is Express wrapping a database, but repo is not public as of 8/25/20
let LogToServer( logObject: obj ) = 
    match logUrl with
    | Some(url) ->
        promise {
            let username = 
                match idOption with
                | Some(id) -> id
                //In a JupyterHub situation, we can use the username embedded in the url
                | None -> Browser.Dom.window.location.href
            do! Fetch.post( url, { username=username; json=toJson(logObject) } ) //caseStrategy = SnakeCase
        } |> ignore
    | None -> ()

