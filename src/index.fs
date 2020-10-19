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
    [| JupyterlabApputils.ICommandPalette; JupyterlabNotebook.Tokens.Types.INotebookTracker; JupyterlabApplication.ILayoutRestorer |]

/// id to self explanation map
let selfExplanationState = System.Collections.Generic.Dictionary<string,string>()

/// Log self explanation and save its state
let logSelfExplanation( text : string) ( id : string ) =
  Logging.LogToServer( Logging.JupyterLogEntry082720.Create "self-explanation" ( text |> Some ) ) 
  selfExplanationState.Add( id, text)

/// Simplest way to connect javascript injected into code cell output to F#: make a global function in node
let [<Global>] ``global`` : obj = jsNative
``global``?logSelfExplanation <- logSelfExplanation

//https://stackoverflow.com/questions/47640263/how-to-extend-a-js-class-in-fable
[<Import("Widget", from = "@phosphor/widgets")>]
[<AbstractClass>]
type Widget() =
    class
        // only implementing the minimal members needed
        member val node: HTMLElement = null with get, set
        /// Guess only fires once after attach to DOM. onAfterShow is called after every display (e.g., switching tabs)
        //abstract onAfterAttach: unit -> unit
        /// Using to resize blockly
        //abstract onResize: PhosphorWidgets.Widget.ResizeMessage -> unit
    end

/// Wrapping blockly in a widget helps with resizing and other GUI events that affect blockly
type SelfExplanationWidget(notebooks: JupyterlabNotebook.Tokens.INotebookTracker) as this =
    class
        inherit Widget()
        do
          //listen for cell changes
          notebooks.activeCellChanged.connect( this.onActiveCellChanged, this ) |> ignore  

          //decide if we should log
          Logging.CheckShouldLog()
        member val notHooked = true with get, set
        member this.Notebooks = notebooks
       
        member this.onActiveCellChanged =
          PhosphorSignaling.Slot<INotebookTracker, Cell>(fun sender args ->  

            //SELF EXPLANATION: probe all cells b/c listening for kernel messages requires waiting to attach (we could poll instead...)
            // check ALL code cell outputs every time a cell changes
            let cells = this.Notebooks.currentWidget.Value.content.widgets
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
    end

//TODO figure out autostart; lack of UI; injecting state from workspace

let extension =
    createObj
        [ "id" ==> "self_explanation_extension"
          "autoStart" ==> true
          "requires" ==> requires //
          //------------------------------------------------------------------------------------------------------------
          //NOTE: this **must** be wrapped in a Func, otherwise the arguments are tupled and Jupyter doesn't expect that
          //------------------------------------------------------------------------------------------------------------
          "activate" ==> System.Func<JupyterlabApplication.JupyterFrontEnd<JupyterlabApplication.LabShell>, JupyterlabApputils.ICommandPalette, JupyterlabNotebook.Tokens.INotebookTracker, JupyterlabApplication.ILayoutRestorer, unit>(fun app palette notebooks restorer ->
                             console.log ("JupyterLab extension self_explanation_extension is activated!")

                             //Create a blockly widget and place inside main area widget
                             let selfExplanationWidget = SelfExplanationWidget(notebooks)
                             let widget =
                                 JupyterlabApputils.Types.MainAreaWidget.Create
                                     (createObj [ "content" ==> selfExplanationWidget ])
                             widget.id <- "self-explanation-jupyterlab"
                             widget.title.label <- "Self-Explanation Extension"
                             widget.title.closable <- true

                            //TODO create tracker: https://github.com/jupyterlab/jupyterlab_apod/blob/2.0-06-prepare-to-publish/src/index.ts

                             // Add application command
                             let command = "self-explanation:open"
                             app.commands.addCommand
                                 (command,
                                  createObj
                                      [ "label" ==> "Self-Explanation Extension"
                                        "execute" ==> fun () ->
                                            if not <| widget.isAttached then
                                                match notebooks.currentWidget with
                                                | Some(c) ->
                                                    let options =
                                                        jsOptions<JupyterlabDocregistry.Registry.DocumentRegistry.IOpenOptions> (fun o ->
                                                            o.ref <- c.id |> Some
                                                            o.mode <-
                                                                PhosphorWidgets.DockLayout.InsertMode.SplitLeft |> Some)
                                                    c.context.addSibling (widget, options) |> ignore
                                                | None -> app.shell.add (widget, "main")
                                            app.shell.activateById (widget.id) ] :?> PhosphorCommands.CommandRegistry.ICommandOptions)
                             |> ignore
                             //Add command to palette
                             palette?addItem (createObj
                                                  [ "command" ==> command
                                                    "category" ==> "Self-Explanation" ])) ]

exportDefault extension
