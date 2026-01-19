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
Extract the import block from a Lean source file.
Returns lines from start of file up to and including the last import statement.
-/
def extractImports (contents : String) : String := Id.run do
  let lines := contents.splitOn "\n"
  let mut lastImportLine : Nat := 0
  for i in [:lines.length] do
    let line := lines[i]!.trimAsciiStart.toString
    -- Check for import, open, or set_option at file level (before declarations)
    if line.startsWith "import " || line.startsWith "open " || line.startsWith "set_option " then
      lastImportLine := i + 1
    -- Stop at first declaration keyword (def, theorem, lemma, etc.)
    else if line.startsWith "def " || line.startsWith "theorem " || line.startsWith "lemma " ||
            line.startsWith "structure " || line.startsWith "class " || line.startsWith "instance " ||
            line.startsWith "inductive " || line.startsWith "abbrev " || line.startsWith "@[" ||
            line.startsWith "namespace " || line.startsWith "section " ||
            line.startsWith "variable " || line.startsWith "/-" then
      break
  -- Include lines 0 to lastImportLine
  let importLines := lines.toArray[:lastImportLine].toArray.toList
  "\n".intercalate importLines

/--
Compute SubVerso highlighting for source code at a given range in a file.
This re-elaborates the source with proper info tree context.
Returns `none` if highlighting fails for any reason.
-/
def computeHighlighting (file : System.FilePath) (range : DeclarationRange)
    (env : Environment) (opts : Options := {}) : IO (Option SubVerso.Highlighting.Highlighted) := do
  -- Read the full file
  let contents ← IO.FS.readFile file

  -- Extract the relevant range (1-indexed lines, 0-indexed columns)
  let lines := contents.splitOn "\n"
  let startLine := range.pos.line - 1  -- Convert to 0-indexed
  let endLine := range.endPos.line - 1

  if startLine >= lines.length || endLine >= lines.length then
    return none

  -- Extract lines in range (the declaration)
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

  let declSource := "\n".intercalate extractedLines.toList

  -- Extract imports from file header and prepend to declaration
  -- This allows name resolution during re-elaboration
  let imports := extractImports contents
  let source := if imports.isEmpty then declSource else imports ++ "\n\n" ++ declSource

  -- Try highlighting with graceful fallback
  -- Note: Some declarations may cause SubVerso to panic on certain info tree patterns.
  -- In such cases, we return none and the caller should fall back to plain text.
  try
    let (hl, _) ← highlightSource source env opts file.toString
    return some hl
  catch e =>
    IO.eprintln s!"Warning: Highlighting failed for {file}: {e}"
    return none

open SubVerso.Highlighting in
/-- Split highlighted code at the definition's `:=` token.
    This finds the `:=` that separates the signature from the body by:
    1. Looking for def/theorem/lemma/abbrev keyword
    2. Finding the `:=` at bracket depth 0 after that keyword

    If `splitAtAssign` is true (default):
      Returns (signature, body) where signature includes up to and including `:=`.
    If `splitAtAssign` is false:
      Returns (full code from def keyword, none) - strips prefix but doesn't split at `:=`.

    Always strips any prefix before the def keyword (e.g., `@[blueprint ...]`). -/
def splitAtDefinitionAssign (hl : Highlighted) (splitAtAssign : Bool := true)
    : Highlighted × Option Highlighted := Id.run do
  -- Flatten to get all tokens in order
  let mut todo : List Highlighted := [hl]
  let mut tokens : Array (Highlighted × Bool) := #[]  -- (node, isDefKeyword)

  -- First pass: collect all leaf nodes
  while true do
    match todo with
    | [] => break
    | .seq xs :: rest => todo := xs.toList ++ rest
    | .span _ content :: rest => todo := content :: rest
    | .tactics _ _ _ content :: rest => todo := content :: rest
    | node :: rest =>
      let isDefKw := match node with
        | .token ⟨_, content⟩ => content ∈ ["def", "theorem", "lemma", "abbrev", "instance", "example", "structure", "class", "inductive"]
        | _ => false
      tokens := tokens.push (node, isDefKw)
      todo := rest

  -- Second pass: find the definition's `:=`
  -- Strategy: after seeing a def keyword, find `:=` at depth 0
  let mut sawDefKeyword := false
  let mut depth : Int := 0
  let mut splitIdx : Option Nat := none
  let mut defKeywordIdx : Option Nat := none

  for i in [:tokens.size] do
    let (node, isDefKw) := tokens[i]!
    if isDefKw then
      sawDefKeyword := true
      depth := 0
      defKeywordIdx := some i

    if sawDefKeyword then
      match node with
      | .token ⟨_, content⟩ =>
        -- Track bracket depth
        for c in content.toList do
          if c ∈ ['(', '[', '{', '⟨'] then depth := depth + 1
          else if c ∈ [')', ']', '}', '⟩'] then depth := depth - 1
        -- Check for `:=` at depth 0 (but don't break yet - check for `by`)
        if content == ":=" && depth == 0 && splitIdx.isNone then
          splitIdx := some i
        -- If we already found `:=`, check if current token is `by` to include it
        else if splitIdx.isSome && content == "by" then
          splitIdx := some i
          break
        -- If we found `:=` and this is a non-whitespace token that's not `by`, stop
        else if splitIdx.isSome then
          -- Don't break on whitespace-only text
          break
      | .text s =>
        for c in s.toList do
          if c ∈ ['(', '[', '{', '⟨'] then depth := depth + 1
          else if c ∈ [')', ']', '}', '⟩'] then depth := depth - 1
        -- If we already found `:=` and this text has non-whitespace, stop looking for `by`
        if splitIdx.isSome && s.toList.any (fun c => !c.isWhitespace) then
          break
      | _ => pure ()

  -- Always start from def keyword to strip any prefix (e.g., @[blueprint ...])
  let startIdx := defKeywordIdx.getD 0

  -- If no split point found or splitAtAssign is false, return code from def keyword with no body
  match splitIdx with
  | none =>
    -- No := found - return everything from def keyword
    let mut result : Highlighted := .empty
    for i in [startIdx:tokens.size] do
      let (node, _) := tokens[i]!
      result := result ++ node
    return (result, none)
  | some idx =>
    if !splitAtAssign then
      -- Don't split at := - return everything from def keyword
      let mut result : Highlighted := .empty
      for i in [startIdx:tokens.size] do
        let (node, _) := tokens[i]!
        result := result ++ node
      return (result, none)
    else
      -- Reconstruct signature (from def keyword to `:=` inclusive) and body (after `:=`)
      let mut signature : Highlighted := .empty
      let mut body : Highlighted := .empty
      for i in [startIdx:tokens.size] do
        let (node, _) := tokens[i]!
        if i <= idx then
          signature := signature ++ node
        else
          body := body ++ node
      return (signature, if body.isEmpty then none else some body)

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

  -- Split highlighted code at the definition's := using bracket-aware parsing
  -- Always strip @[blueprint ...] prefix; only split at := if there's a proof
  let (highlightedSignature, highlightedProofBody) :=
    match highlightedCode with
    | some hl =>
      let (sig, body) := splitAtDefinitionAssign hl (splitAtAssign := node.proof.isSome)
      (some sig, body)
    | none => (none, none)

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
