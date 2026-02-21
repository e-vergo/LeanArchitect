/-
Copyright (c) 2025 Side-by-Side Blueprint contributors.
Released under Apache 2.0 license.

Provides an RPC endpoint for the Lean 4 infoview to query blueprint metadata
at the cursor position. This enables the Blueprint panel in VS Code.
-/
import Lean
import Lean.Server.Rpc.RequestHandling
import Architect.Basic
import Architect.Attribute

open Lean Server Lsp

namespace Architect

/-- Blueprint information returned to the infoview panel via RPC. -/
structure BlueprintInfo where
  /-- The Lean constant name. -/
  name : String
  /-- The LaTeX label for the node. -/
  label : String
  /-- The 7-status model value: notReady/wip/sorry/proven/fullyProven/axiom/mathlibReady. -/
  status : String
  /-- The custom display title, if set. -/
  title : String := ""
  /-- The LaTeX statement text. -/
  statement : String := ""
  /-- Whether this is a key declaration. -/
  keyDeclaration : Bool := false
  /-- User message/notes. -/
  message : String := ""
  /-- The LaTeX proof text, if available. -/
  proof : String := ""
  /-- LaTeX content placed above this node. -/
  above : String := ""
  /-- LaTeX content placed below this node. -/
  below : String := ""
  deriving FromJson, ToJson, Inhabited

/-- Convert a `NodeStatus` to its string representation for the infoview. -/
private def nodeStatusToString : NodeStatus → String
  | .notReady => "notReady"
  | .wip => "wip"
  | .sorry => "sorry"
  | .proven => "proven"
  | .fullyProven => "fullyProven"
  | .axiom => "axiom"
  | .mathlibReady => "mathlibReady"

/-- Check whether a constant's value/proof expression references `sorryAx`. -/
private def hasSorryAx (env : Environment) (constName : Name) : Bool :=
  match env.find? constName with
  | some (.thmInfo v)    => v.value.getUsedConstants.any (· == ``sorryAx)
  | some (.defnInfo v)   => v.value.getUsedConstants.any (· == ``sorryAx)
  | some (.opaqueInfo v) => v.value.getUsedConstants.any (· == ``sorryAx)
  | _ => false

/-- Derive the effective status for a node based on the environment.

    Replicates the logic from `Dress.Graph.Builder.getStatus`:
    - mathlibReady / wip: manual flags, highest priority
    - proven: constant exists in env without sorryAx
    - sorry: constant exists in env but uses sorryAx
    - notReady: constant not in env (default)
    Note: `axiom` and `fullyProven` are computed downstream by Dress, not here. -/
private def deriveStatus (env : Environment) (node : Node) : NodeStatus :=
  match node.status with
  | .mathlibReady => .mathlibReady
  | .wip => .wip
  | _ =>
    if env.contains node.name then
      if hasSorryAx env node.name then .sorry else .proven
    else
      .notReady

/-- Convert a `Node` to `BlueprintInfo` for the infoview. -/
private def nodeToInfo (env : Environment) (node : Node) : BlueprintInfo where
  name := node.name.toString
  label := node.latexLabel
  status := nodeStatusToString (deriveStatus env node)
  title := node.title.getD ""
  statement := node.statement.text
  proof := (node.proof.map (·.text)).getD ""
  keyDeclaration := node.keyDeclaration
  message := node.message.getD ""
  above := node.above.getD ""
  below := node.below.getD ""

/-- Find the blueprint node whose declaration starts within the given snap's byte range.

Leverages the snap mechanism that `bindWaitFindSnap` already uses to identify which
command the cursor is in. Each snap corresponds to a single top-level command and has
a byte range (`stx.getPos?` .. `endPos`). We convert each blueprint declaration's
start position to a byte offset via `FileMap.ofPosition` and check containment.

This avoids fragile line-based matching and correctly handles multi-line declarations,
since the snap boundary is the authoritative source for "which command is the cursor in." -/
private def findBlueprintInSnap (snap : Snapshots.Snapshot) (text : FileMap)
    : Option Node := Id.run do
  let env := snap.cmdState.env
  let entries := blueprintExt.getEntries env
  let some snapBegin := snap.stx.getPos? | return none
  let snapEnd := snap.endPos
  for (declName, node) in entries do
    -- Skip imported declarations: their declRangeExt positions are file-local
    -- to their original source file, not the current file's byte space.
    if env.getModuleIdxFor? declName |>.isSome then continue
    if let some ranges := declRangeExt.find? env declName (level := .exported) <|>
        declRangeExt.find? env declName (level := .server) then
      let declBytePos := text.ofPosition ranges.range.pos
      if declBytePos >= snapBegin && declBytePos <= snapEnd then
        return some node
  return none

/-- RPC method that returns blueprint metadata for the declaration at the cursor position.

The infoview calls this method with the cursor position. `bindWaitFindSnap` locates the
snap (top-level command) containing the cursor. We then check whether any blueprint
declaration starts within that snap's byte range, using `FileMap` for position conversion.
This delegates cursor-to-command resolution entirely to the snap mechanism. -/
@[server_rpc_method]
def blueprintInfo (params : Lsp.TextDocumentPositionParams)
    : RequestM (RequestTask (Option BlueprintInfo)) := do
  let doc ← RequestM.readDoc
  let text := doc.meta.text
  let cursorPos := text.lspPosToUtf8Pos params.position
  RequestM.bindWaitFindSnap doc (notFoundX := RequestM.pureTask (pure none))
    (fun s => s.endPos >= cursorPos)
    fun snap => RequestM.pureTask do
      match findBlueprintInSnap snap text with
      | some node => return some (nodeToInfo snap.cmdState.env node)
      | none => return none

end Architect
