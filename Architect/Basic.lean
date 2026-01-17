import Lean
import Batteries.Lean.NameMapAttribute
import SubVerso.Highlighting
import Architect.Highlighting


open Lean Elab

namespace Architect

initialize registerTraceClass `blueprint
initialize registerTraceClass `blueprint.debug

/-- The statement or proof of a node. -/
structure NodePart where
  /-- The natural language description of this part. -/
  text : String
  /-- The specified set of nodes that this node depends on, in addition to inferred ones. -/
  uses : Array Name
  /-- The set of nodes to exclude from `uses`. -/
  excludes : Array Name
  /-- Additional LaTeX labels of nodes that this node depends on. -/
  usesLabels : Array String
  /-- The set of labels to exclude from `usesLabels`. -/
  excludesLabels : Array String
  /-- The LaTeX environment to use for this part. -/
  latexEnv : String
deriving Inhabited, Repr, FromJson, ToJson, ToExpr

/-- A theorem or definition in the blueprint graph. -/
structure Node where
  /-- The Lean name of the tagged constant. -/
  name : Name
  /-- The LaTeX label of the node. Multiple nodes can have the same label. -/
  latexLabel : String
  /-- The statement of this node. -/
  statement : NodePart
  /-- The proof of this node. -/
  proof : Option NodePart
  /-- The surrounding environment is not ready to be formalized, typically because it requires more blueprint work. -/
  notReady : Bool
  /-- A GitHub issue number where the surrounding definition or statement is discussed. -/
  discussion : Option Nat
  /-- The short title of the node in LaTeX. -/
  title : Option String
deriving Inhabited, Repr, FromJson, ToJson, ToExpr

structure NodeWithPos extends Node where
  /--
  Whether the node name is in the environment.
  This should always be true for nodes e.g. added by `@[blueprint]`.
  -/
  hasLean : Bool
  /-- The location (module & range) the node is defined in. -/
  location : Option DeclarationLocation
  /-- The proof body location (from end of signature to end of declaration). -/
  proofLocation : Option DeclarationRange := none
  /-- The file the node is defined in. -/
  file : Option System.FilePath
  /-- SubVerso highlighted code for the declaration. -/
  highlightedCode : Option SubVerso.Highlighting.Highlighted := none
  /-- SubVerso highlighted code for just the signature (up to and including `:=`). -/
  highlightedSignature : Option SubVerso.Highlighting.Highlighted := none
  /-- SubVerso highlighted code for just the proof body (after `:=`). -/
  highlightedProofBody : Option SubVerso.Highlighting.Highlighted := none
deriving Inhabited, Repr

/-- Environment extension that stores the nodes of the blueprint. -/
initialize blueprintExt : NameMapExtension Node ←
  registerNameMapExtension Node

namespace LatexLabelToLeanNames

abbrev State := SMap String (Array Name)
abbrev Entry := String × Name

private def addEntryFn (s : State) (e : Entry) : State :=
  match s.find? e.1 with
  | none => s.insert e.1 #[e.2]
  | some ns => s.insert e.1 (ns.push e.2)

end LatexLabelToLeanNames

open LatexLabelToLeanNames in
initialize latexLabelToLeanNamesExt : SimplePersistentEnvExtension Entry State ←
  registerSimplePersistentEnvExtension {
    addEntryFn := addEntryFn
    addImportedFn := fun es => mkStateFromImportedEntries addEntryFn {} es |>.switch
  }

def addLeanNameOfLatexLabel (env : Environment) (latexLabel : String) (name : Name) : Environment :=
  latexLabelToLeanNamesExt.addEntry env (latexLabel, name)

def getLeanNamesOfLatexLabel (env : Environment) (latexLabel : String) : Array Name :=
  latexLabelToLeanNamesExt.getState env |>.findD latexLabel #[]

/-! ## Highlighted Code Extension

This extension stores SubVerso highlighted code for declarations tagged with `@[blueprint]`.
The highlighted code is captured during command elaboration when info trees are available.
-/

/-- Environment extension that stores highlighted code for blueprint declarations. -/
initialize highlightedCodeExt : NameMapExtension SubVerso.Highlighting.Highlighted ←
  registerNameMapExtension SubVerso.Highlighting.Highlighted

/-- Add highlighted code for a declaration to the environment. -/
def addHighlightedCode (name : Name) (hl : SubVerso.Highlighting.Highlighted) : CoreM Unit :=
  modifyEnv fun env => highlightedCodeExt.addEntry env (name, hl)

/-- Get highlighted code for a declaration from the environment. -/
def getHighlightedCode? (env : Environment) (name : Name) : Option SubVerso.Highlighting.Highlighted :=
  highlightedCodeExt.find? env name

/--
Compute SubVerso highlighting for source code at a given range in a file.
This re-elaborates the source with proper info tree context.
-/
def computeHighlighting (file : System.FilePath) (range : DeclarationRange)
    (env : Environment) (opts : Options := {}) : IO (Option SubVerso.Highlighting.Highlighted) := do
  try
    -- Read the full file
    let contents ← IO.FS.readFile file

    -- Extract the relevant range (1-indexed lines, 0-indexed columns)
    let lines := contents.splitOn "\n"
    let startLine := range.pos.line - 1  -- Convert to 0-indexed
    let endLine := range.endPos.line - 1

    if startLine >= lines.length || endLine >= lines.length then
      return none

    -- Extract lines in range
    let mut extractedLines : Array String := #[]
    for i in [startLine:endLine + 1] do
      if h : i < lines.length then
        let line := lines[i]
        if i == startLine && i == endLine then
          -- Single line: extract substring
          extractedLines := extractedLines.push ((line.drop range.pos.column).take (range.endPos.column - range.pos.column)).toString
        else if i == startLine then
          -- First line: from column to end
          extractedLines := extractedLines.push (line.drop range.pos.column).toString
        else if i == endLine then
          -- Last line: from start to column
          extractedLines := extractedLines.push (line.take range.endPos.column).toString
        else
          -- Middle lines: full line
          extractedLines := extractedLines.push line

    let source := "\n".intercalate extractedLines.toList

    -- We need to create a self-contained file for highlighting.
    -- For now, just highlight the raw source without re-elaboration.
    -- Full re-elaboration would require reconstructing imports.
    let (hl, _) ← highlightSource source env opts file.toString
    return some hl
  catch _ =>
    return none

open SubVerso.Highlighting in
/-- Split highlighted code at a character position.
    Returns (before, after) where before contains characters [0, pos) and after contains [pos, end).
    This preserves all highlighting information by walking the tree and splitting at the exact position. -/
partial def splitHighlightedAtPosition (hl : Highlighted) (pos : Nat) : Highlighted × Highlighted := Id.run do
  -- We traverse the tree, accumulating characters until we reach `pos`
  let mut todo : List (Option Highlighted) := [some hl]
  let mut charCount : Nat := 0
  let mut before : Highlighted := .empty
  let mut after : Highlighted := .empty
  let mut inAfter := false

  while true do
    match todo with
    | [] => break
    | none :: rest =>
      todo := rest
    | some (.seq xs) :: rest =>
      todo := xs.toList.map some ++ rest
    | some (.span msgs content) :: rest =>
      -- For spans, we need to handle them specially - include the whole span in whichever part
      -- the majority falls into. For simplicity, check if we've passed pos.
      let contentStr := content.toString
      if inAfter then
        after := after ++ .span msgs content
        todo := rest
      else if charCount + contentStr.length <= pos then
        before := before ++ .span msgs content
        charCount := charCount + contentStr.length
        todo := rest
      else
        -- Span straddles the boundary - recursively split the content
        let (contentBefore, contentAfter) := splitHighlightedAtPosition content (pos - charCount)
        before := before ++ .span msgs contentBefore
        after := after ++ .span msgs contentAfter
        inAfter := true
        charCount := pos
        todo := rest
    | some (.tactics goals startPos endPos content) :: rest =>
      let contentStr := content.toString
      if inAfter then
        after := after ++ .tactics goals startPos endPos content
        todo := rest
      else if charCount + contentStr.length <= pos then
        before := before ++ .tactics goals startPos endPos content
        charCount := charCount + contentStr.length
        todo := rest
      else
        let (contentBefore, contentAfter) := splitHighlightedAtPosition content (pos - charCount)
        before := before ++ .tactics goals startPos endPos contentBefore
        after := after ++ .tactics goals startPos endPos contentAfter
        inAfter := true
        charCount := pos
        todo := rest
    | some node :: rest =>
      -- Handle text, token, point, unparsed
      let nodeStr := node.toString
      let nodeLen := nodeStr.length
      if inAfter then
        after := after ++ node
        todo := rest
      else if charCount + nodeLen <= pos then
        before := before ++ node
        charCount := charCount + nodeLen
        todo := rest
      else
        -- This node straddles the boundary - need to split it
        let splitPoint := pos - charCount
        match node with
        | .text s =>
          before := before ++ .text (s.take splitPoint).toString
          after := after ++ .text (s.drop splitPoint).toString
        | .token _ =>
          -- Tokens are atomic - put in before if any part is before pos
          before := before ++ node
        | .unparsed s =>
          before := before ++ .unparsed (s.take splitPoint).toString
          after := after ++ .unparsed (s.drop splitPoint).toString
        | .point .. =>
          -- Points have no length, keep in before
          before := before ++ node
        | _ => pure () -- seq, span, tactics handled above
        inAfter := true
        charCount := pos
        todo := rest

  return (before, after)

/-- Calculate the character offset from declaration start to selectionRange end.
    This accounts for multi-line ranges by reading the source file. -/
def calculateSignatureLength (file : System.FilePath) (declStart : Position) (sigEnd : Position) : IO Nat := do
  let contents ← IO.FS.readFile file
  let lines := contents.splitOn "\n"

  -- Convert Position to character offset
  let mut offset : Nat := 0

  -- Add characters from lines between declStart and sigEnd
  for lineIdx in [declStart.line - 1 : sigEnd.line] do
    if h : lineIdx < lines.length then
      let line := lines[lineIdx]
      if lineIdx == declStart.line - 1 && lineIdx == sigEnd.line - 1 then
        -- Same line: count from declStart.column to sigEnd.column
        offset := sigEnd.column - declStart.column
      else if lineIdx == declStart.line - 1 then
        -- First line: count from declStart.column to end of line (+ newline)
        offset := line.length - declStart.column + 1
      else if lineIdx == sigEnd.line - 1 then
        -- Last line: count from start to sigEnd.column
        offset := offset + sigEnd.column
      else
        -- Middle line: count full line + newline
        offset := offset + line.length + 1

  return offset

/-- Convert a Node to NodeWithPos, looking up position and highlighted code information. -/
def Node.toNodeWithPos (node : Node) (computeHighlight : Bool := false) : CoreM NodeWithPos := do
  let env ← getEnv
  if !env.contains node.name then
    return { node with hasLean := false, location := none, proofLocation := none, file := none }
  let module := match env.getModuleIdxFor? node.name with
    | some modIdx => env.allImportedModuleNames[modIdx]!
    | none => env.header.mainModule

  -- Get declaration ranges from Lean
  let ranges ← findDeclarationRanges? node.name
  let (location, proofLocation) := match ranges with
    | some r =>
      -- range = full declaration including proof
      -- selectionRange = signature only (name + type)
      let loc : DeclarationLocation := { module, range := r.range }
      -- Proof body is from end of selectionRange to end of range
      let selEnd := r.selectionRange.endPos
      let fullEnd := r.range.endPos
      let hasProofBody := selEnd.line < fullEnd.line ||
        (selEnd.line == fullEnd.line && selEnd.column < fullEnd.column)
      let proofLoc : Option DeclarationRange :=
        if hasProofBody then
          some {
            pos := r.selectionRange.endPos
            charUtf16 := r.selectionRange.charUtf16
            endPos := r.range.endPos
            endCharUtf16 := r.range.endCharUtf16
          }
        else
          none
      (some loc, proofLoc)
    | none => (none, none)

  let file ← (← getSrcSearchPath).findWithExt "lean" module

  -- Get or compute full highlighted code
  let mut highlightedCode := getHighlightedCode? env node.name
  if highlightedCode.isNone && computeHighlight then
    if let (some f, some r) := (file, ranges) then
      highlightedCode ← computeHighlighting f r.range env (← getOptions)

  -- Split highlighted code at signature boundary using position information
  let (highlightedSignature, highlightedProofBody) ← do
    match (highlightedCode, file, ranges) with
    | (some hl, some f, some r) =>
      -- Calculate the character offset where signature ends
      let sigLength ← calculateSignatureLength f r.range.pos r.selectionRange.endPos
      let (sig, body) := splitHighlightedAtPosition hl sigLength
      let bodyOpt := if body.isEmpty then none else some body
      pure (some sig, bodyOpt)
    | _ => pure (none, none)

  return { node with
    hasLean := true, location, proofLocation, file,
    highlightedCode, highlightedSignature, highlightedProofBody }

section ResolveConst

register_option blueprint.ignoreUnknownConstants : Bool := {
  defValue := false,
  descr := "Whether to ignore unknown constants in the `uses` and `proofUses` options of the `blueprint` attribute."
}

/--
Resolves an identifier using `realizeGlobalConstNoOverloadWithInfo`.
Ignores unknown constants if `blueprint.ignoreUnknownConstants` is true (default: false).
-/
def tryResolveConst (id : Ident) : CoreM Name := do
  try
    realizeGlobalConstNoOverloadWithInfo id
  catch ex =>
    if blueprint.ignoreUnknownConstants.get (← getOptions) then
      -- logNamedWarningAt id lean.unknownIdentifier ex.toMessageData
      return id.getId
    else
      throwNamedErrorAt id lean.unknownIdentifier
        "{ex.toMessageData}\n\nThis error can be disabled by `set_option blueprint.ignoreUnknownConstants true`."

end ResolveConst

end Architect
