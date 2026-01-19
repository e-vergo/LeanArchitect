/-
Copyright (c) 2025 LeanArchitect contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Batteries.Lean.NameMapAttribute
import SubVerso.Highlighting

/-!
# Highlighted Code Capture for Blueprint Declarations

This module provides functionality to capture SubVerso highlighted code for declarations.
It follows the pattern used by the Lean 4 reference manual (Verso) to re-elaborate source
code with proper info tree context, which is required for semantic syntax highlighting.
-/

open Lean Elab Command Term Meta Parser
open SubVerso.Highlighting

namespace Architect

/-- Option to enable/disable blueprint highlighting. -/
register_option blueprint.highlighting : Bool := {
  defValue := true
  descr := "Enables SubVerso syntax highlighting for blueprint declarations."
}

/-- Get names suppressed for highlighting (similar to SubVerso's approach). -/
def getSuppressedNamespaces : CoreM (List Name) := do
  return []

/--
Run a command elaboration action, capturing info trees with proper context.
This wraps the action in `withInfoTreeContext` to ensure SubVerso can process the trees.
-/
def runCommand (act : CommandElabM Unit) (stx : Syntax) (ctx : Command.Context)
    (state : Command.State) : IO Command.State := do
  let act' := withInfoTreeContext
    (mkInfoTree := pure ∘ InfoTree.node (.ofCommandInfo {elaborator := `Architect.highlight, stx}))
    act
  match ← EIO.toIO' <| (act' ctx).run state with
  | .ok ((), state') => return state'
  | .error e => throw <| IO.userError s!"Command elaboration failed: {← e.toMessageData.toString}"

/--
Run SubVerso highlighting in the appropriate monad context.
-/
private def runHighlighting (fileMap : FileMap) (cmds : Array Syntax)
    (trees : PersistentArray InfoTree) (messages : Array Message)
    (env : Environment) (opts : Options) : IO Highlighted := do
  let suppressedNS : List Name := []

  -- Create contexts for running TermElabM
  let coreCtx : Core.Context := {
    fileName := "<blueprint>"
    fileMap
    options := opts
    currNamespace := `main
    openDecls := []
  }
  let coreState : Core.State := { env }
  let metaCtx : Meta.Context := {}
  let metaState : Meta.State := {}
  let termCtx : Term.Context := {}
  let termState : Term.State := {}

  -- Run highlighting for each command and concatenate
  let act : TermElabM Highlighted := do
    let mut result := Highlighted.empty
    let mut lastPos : String.Pos.Raw := cmds[0]? >>= (·.getRange?.map (·.start)) |>.getD 0
    for cmd in cmds do
      if !isTerminalCommand cmd then
        let hl ← highlightIncludingUnparsed cmd messages trees suppressedNS (startPos? := lastPos)
        result := result ++ hl
        lastPos := (cmd.getTrailingTailPos?).getD lastPos
    return result

  match ← EIO.toIO' <| act.run termCtx termState |>.run metaCtx metaState |>.run coreCtx coreState with
  | .ok (((hl, _), _), _) => return hl
  | .error e => throw <| IO.userError s!"Highlighting failed: {← e.toMessageData.toString}"

/--
Highlights Lean source code by re-elaborating it with proper info tree context.

This follows the pattern used by Verso (Lean 4 reference manual) to get properly
contextualized info trees that SubVerso can process.

The source should be complete Lean code (with imports if needed).
Returns highlighted code and any messages from elaboration.
-/
def highlightSource (source : String) (env : Environment) (opts : Options := {})
    (fileName : String := "<blueprint>") : IO (Highlighted × MessageLog) := do
  let inputCtx := mkInputContext source fileName
  let commandCtx : Command.Context := {
    fileName
    fileMap := FileMap.ofString source
    snap? := none
    cancelTk? := none
  }

  let mut commandState : Command.State := Command.mkState env {} opts
  let mut parserState : ModuleParserState := {}
  let mut cmds : Array Syntax := #[]

  -- Parse and elaborate all commands
  repeat do
    let scope := commandState.scopes.head!
    let pmctx := {
      env := commandState.env
      options := scope.opts
      currNamespace := scope.currNamespace
      openDecls := scope.openDecls
    }
    let (cmd, ps', messages) := parseCommand inputCtx pmctx parserState commandState.messages
    cmds := cmds.push cmd
    parserState := ps'
    commandState := { commandState with messages }

    -- Elaborate with info tree context
    commandState ← runCommand (elabCommand cmd) cmd commandCtx commandState

    if isTerminalCommand cmd then break

  -- Now highlight using the captured info trees
  let trees := commandState.infoState.trees
  let nonSilentMsgs := commandState.messages.toArray.filter (!·.isSilent)

  -- Run highlighting in a TermElabM context
  let highlighted ← runHighlighting inputCtx.fileMap cmds trees nonSilentMsgs commandState.env opts

  return (highlighted, commandState.messages)

/--
Highlights a single declaration from source code.

This is the main entry point for blueprint highlighting.
Takes complete Lean source and returns highlighted code.
-/
def highlightDeclaration (source : String) (env : Environment) (opts : Options := {}) :
    IO Highlighted := do
  let (hl, _) ← highlightSource source env opts
  return hl

/--
Creates a minimal source file that imports necessary modules and contains the declaration.
-/
def mkMinimalSource (imports : Array Name) (declText : String) : String :=
  let importLines := imports.map (fun n => s!"import {n}") |>.toList |> "\n".intercalate
  s!"{importLines}\n\n{declText}"

/-! ## Environment Extension for Storing Highlighted Code

The hook mechanism captures highlighting during elaboration and stores it here.
-/

/-- Environment extension that stores highlighted code for blueprint declarations.
    This is populated by the Hook mechanism during command elaboration. -/
initialize highlightedCodeExt : NameMapExtension SubVerso.Highlighting.Highlighted ←
  registerNameMapExtension SubVerso.Highlighting.Highlighted

/-- Add highlighted code for a declaration to the environment. -/
def addHighlightedCode (name : Name) (hl : SubVerso.Highlighting.Highlighted) : CoreM Unit :=
  modifyEnv fun env => highlightedCodeExt.addEntry env (name, hl)

/-- Get highlighted code for a declaration from the environment. -/
def getHighlightedCode? (env : Environment) (name : Name) : Option SubVerso.Highlighting.Highlighted :=
  highlightedCodeExt.find? env name

/-- Capture highlighting for a declaration using current command state's info trees.
    Must be called from CommandElabM while info trees are still available.

    This is the preferred method for capturing highlighting because it uses the
    already-available info trees from elaboration, avoiding position mismatches
    that occur when re-elaborating source code later. -/
def captureHighlightingFromCommandState (name : Name) (stx : Syntax) : CommandElabM Unit := do
  try
    let trees := (← get).infoState.trees
    let messages := (← get).messages.toArray.filter (!·.isSilent)
    let suppressedNS : List Name := []
    trace[blueprint.debug] "Capturing highlighting for {name}: {trees.size} info trees, syntax kind: {stx.getKind}"
    -- Run SubVerso highlighting in TermElabM context
    let hl ← liftTermElabM <| highlightIncludingUnparsed stx messages trees suppressedNS
    trace[blueprint.debug] "Highlighting captured for {name}: {repr hl |>.pretty.length} chars"
    -- Store in extension
    modifyEnv fun env => highlightedCodeExt.addEntry env (name, hl)
  catch e =>
    -- SubVerso can crash on some declarations; log and continue gracefully
    logWarning m!"Failed to capture highlighting for {name}: {e.toMessageData}"

end Architect
