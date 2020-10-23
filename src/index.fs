module App

// open FSharp.Core
open Fable.Core
// open Fable.Core.JS
open Fable.Core.JsInterop
// open Fable.Promise
open Browser.Types
open Browser.Dom
open JupyterlabServices.__kernel_messages.KernelMessage
open JupyterlabServices.__kernel_kernel.Kernel
open JupyterlabNotebook.Tokens

//tricky here: if we try to make collection of requires, F# complains they are different types unless we specify obj type
let mutable requires: obj array =
    [|  JupyterlabNotebook.Tokens.Types.INotebookTracker |]

/// id to self explanation map
let selfExplanationState = System.Collections.Generic.Dictionary<string,string>()

/// Log self explanation and save its state
let logSelfExplanation( text : string) ( id : string ) =
  Logging.LogToServer( Logging.JupyterLogEntry082720.Create "self-explanation" ( text |> Some ) ) 
  if not <| selfExplanationState.ContainsKey( id ) then selfExplanationState.Add( id, text)

/// Simplest way to connect javascript injected into code cell output to F#: make a global function in node
let [<Global>] ``global`` : obj = jsNative
``global``?logSelfExplanation <- logSelfExplanation

/// When the active cell changes, probe all cells
/// Probing necessary b/c we need to check for new code cells
/// We want to allow self-explanation before code is executed, so listening for kernel messages is insufficient
let onActiveCellChanged =
          PhosphorSignaling.Slot<INotebookTracker, Cell>(fun sender args ->  

            // check ALL code cell outputs every time a cell changes
            let cells = sender.currentWidget.Value.content.widgets
            for i = 0 to cells.length - 1 do
              let cell = cells.[i]
              if cell.model.``type`` = JupyterlabCoreutils.Nbformat.Nbformat.CellType.Code then
                // console.log ("I am a code cell")
                let codeCell = cell :?> JupyterlabCells.Widget.CodeCell

                //check if our self-explanation response box is already present; don't create duplicates!
                let hasResposeBox =
                  [| 0.0 .. codeCell.outputArea.model.length - 1.0 |] 
                  |> Array.exists( fun i ->
                    let model = codeCell.outputArea.model.get(i) 
                    let html = model.data.["text/html"] |> unbox<string>
                    html <> null && html.Contains("self-explanation") //below we enforce that <textarea> has an id containing the string "self-explanation"
                  )
                  
                if not <| hasResposeBox then
                  //conveniently we have a unique persistent id
                  let modelId = codeCell.model.id
                  //retrieve the stored self-explanation if it exists
                  let selfExplanation = 
                    match selfExplanationState.TryGetValue(modelId) with
                      | true,se -> se //we have a stored self explanation
                      | false,_ -> "" //nothing stored

                  //if self explanation exists, display it in black; else display an empty textarea with red font
                  let displayData = //
                    createObj 
                      [
                        "output_type" ==> "display_data"
                        //inject the modelId into the textarea id; insert the stored self explanation if it exists; creating a logging handler with this information
                        "data" ==> createObj [ "text/html" ==> """<p>Explain the code/output for this cell.</p><div style='display:inline-block;vertical-align: top;'><textarea id='self-explanation""" + modelId +  """' cols='60' rows='2'""" + (if selfExplanation = "" then " style='color:Tomato;'" else "") + ">" + selfExplanation  + """</textarea></div><div style='display:inline-block;vertical-align: top;'><button onclick="document.getElementById('self-explanation""" + modelId + """').style.color = 'black';logSelfExplanation(document.getElementById('self-explanation""" + modelId + """').value,'""" + modelId + """')">Save</button></div>""" ]
                      ] :?> JupyterlabCoreutils.Nbformat.Nbformat.IOutput
              
                  codeCell.outputArea.model.add( displayData ) |> ignore
                  // console.log("I added a self explanation a code cell's outputarea")
         
            true
           )

let extension =
    createObj
        [ "id" ==> "self_explanation_extension"
          "autoStart" ==> true
          "requires" ==> requires //
          //------------------------------------------------------------------------------------------------------------
          //NOTE: this **must** be wrapped in a Func, otherwise the arguments are tupled and Jupyter doesn't expect that
          //------------------------------------------------------------------------------------------------------------
          "activate" ==> System.Func<JupyterlabApplication.JupyterFrontEnd<JupyterlabApplication.LabShell>, JupyterlabNotebook.Tokens.INotebookTracker, unit>(fun app notebooks ->
                  let searchParams = Browser.Url.URLSearchParams.Create(  Browser.Dom.window.location.search )
                  match searchParams.get("se") with
                  //if query string has se=1, activate self-explanation extension
                  | Some(state) when state = "1" ->
                   console.log ("JupyterLab extension self_explanation_extension is activated!")

                   //register for cell change events
                   notebooks.activeCellChanged.connect( onActiveCellChanged, null ) |> ignore  

                   //decide if we should log
                   Logging.CheckShouldLog()

                  //deactivate self-explanation extension by default
                  | _ -> ()

                  //If query string has id=xxx, store this identifier as a participant id
                  match searchParams.get("id") with
                  | Some(id) -> Logging.idOption <- Some(id)
                  | _ -> ()

                ) //System.Func
        ]

exportDefault extension
