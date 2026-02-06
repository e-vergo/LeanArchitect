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
  /-- The 6-status model value: notReady/ready/sorry/proven/fullyProven/mathlibReady. -/
  status : String
  /-- The custom display title, if set. -/
  title : String := ""
  /-- The LaTeX statement text. -/
  statement : String := ""
  /-- Names of declared dependencies (from `uses` fields). -/
  dependencies : Array String := #[]
  /-- Whether this is a key declaration. -/
  keyDeclaration : Bool := false
  /-- User message/notes. -/
  message : String := ""
  deriving FromJson, ToJson, Inhabited

/-- Convert a `NodeStatus` to its string representation for the infoview. -/
private def nodeStatusToString : NodeStatus → String
  | .notReady => "notReady"
  | .ready => "ready"
  | .sorry => "sorry"
  | .proven => "proven"
  | .fullyProven => "fullyProven"
  | .mathlibReady => "mathlibReady"

/-- Convert a `Node` to `BlueprintInfo` for the infoview. -/
private def nodeToInfo (node : Node) : BlueprintInfo where
  name := node.name.toString
  label := node.latexLabel
  status := nodeStatusToString node.status
  title := node.title.getD ""
  statement := node.statement.text
  dependencies :=
    let stmtUses := node.statement.uses.map Name.toString
    let proofUses := match node.proof with
      | some p => p.uses.map Name.toString
      | none => #[]
    stmtUses ++ proofUses
  keyDeclaration := node.keyDeclaration
  message := node.message.getD ""

/-- Find the blueprint node whose declaration range contains the given cursor position.

Iterates over all blueprint-annotated declarations in the environment and checks
whether the cursor falls within their declaration range.

Note: `Lsp.Position.line` is 0-indexed while `Lean.Position.line` is 1-indexed.
We convert the LSP line to 1-indexed before comparison. -/
private def findBlueprintAtPos (env : Environment) (pos : Lsp.Position)
    : Option Node := Id.run do
  let blueprintState := (blueprintExt : SimplePersistentEnvExtension (Name × Node) (NameMap Node)).getState env
  -- Convert 0-indexed LSP line to 1-indexed Lean line
  let cursorLine := pos.line + 1
  let mut result : Option Node := none
  for (declName, node) in blueprintState do
    -- Use declRangeExt directly with the environment
    if let some ranges := declRangeExt.find? (level := .exported) env declName <|>
        declRangeExt.find? (level := .server) env declName then
      let declRange := ranges.range
      let startLine := declRange.pos.line
      let endLine := declRange.endPos.line
      if cursorLine >= startLine && cursorLine <= endLine then
        result := some node
  return result

/-- RPC method that returns blueprint metadata for the declaration at the cursor position.

The infoview calls this method with the cursor position. If the cursor is on a
`@[blueprint]` declaration, returns the blueprint metadata; otherwise returns `none`. -/
@[server_rpc_method]
def Architect.blueprintInfo (params : Lsp.TextDocumentPositionParams)
    : RequestM (RequestTask (Option BlueprintInfo)) := do
  let doc ← RequestM.readDoc
  let text := doc.meta.text
  let cursorPos := text.lspPosToUtf8Pos params.position
  RequestM.bindWaitFindSnap doc (notFoundX := RequestM.pureTask (pure none))
    (fun s => s.endPos >= cursorPos)
    fun snap => RequestM.pureTask do
      let env := snap.cmdState.env
      match findBlueprintAtPos env params.position with
      | some node => return some (nodeToInfo node)
      | none => return none

end Architect
