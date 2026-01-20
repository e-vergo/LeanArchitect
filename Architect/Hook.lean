/-
Copyright (c) 2025 LeanArchitect contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Batteries.Lean.NameMapAttribute
import SubVerso.Highlighting
import SubVerso.Module
import Architect.Highlighting
import Architect.HtmlRender
import Architect.HookState

/-!
# Elaboration-Time "Dressed" Artifact Generation

This module provides the core infrastructure for capturing rich metadata ("dressed" artifacts)
DURING Lean elaboration. Info trees (required for semantic highlighting) are ephemeral -
they exist in `commandState.infoState.trees` during elaboration but are discarded afterward.
This module captures this data while it's available.

## "Dressed" Artifacts

"Dressed" code = bare Lean source + rich metadata captured during elaboration:
- Semantic highlighting (SubVerso)
- Pre-rendered HTML (Verso)
- Type signatures
- Source positions
- Pre-computed base64 strings ready for TeX embedding

## Key Components

1. **Environment extension** (`dressedDeclExt`): Stores captured artifacts per declaration
2. **Core capture function** (`captureHighlightingFromInfoTrees`): Calls SubVerso's highlight function
3. **JSON serialization**: Export to `.lake/build/dressed/{Module/Path}.json`
4. **Automatic export**: When `blueprint.dress=true`, artifacts are exported after each `@[blueprint]` declaration

## Usage

Simply use `@[blueprint]` attributes on your declarations:

```lean
import Mathlib.Algebra.Group.Basic
import Architect

@[blueprint]
theorem my_theorem : ... := by
  ...
```

Run `lake build dress` to generate dressed artifacts. No explicit `#dress` command is needed -
export happens automatically when the `blueprint.dress` option is true.

**Files generated:** `.lake/build/dressed/{Module/Path}.json` containing:
- `html`: Pre-rendered HTML string
- `htmlBase64`: Base64-encoded HTML (for direct TeX embedding)
- `jsonBase64`: Base64-encoded SubVerso JSON (for backward compatibility)
-/

open Lean Elab Command Term Meta
open SubVerso.Highlighting
open SubVerso.Module

namespace Architect

/-! ## Blueprint Option for Dressing -/

/-- Option to enable dressing during `lake build dress`.
    When true, `#dress` will register a finalization hook to export dressed artifacts. -/
register_option blueprint.dress : Bool := {
  defValue := false
  descr := "Enable dressed artifact generation (set by `lake build dress`)"
}

/-! ## Environment Extension for Captured Artifacts -/

/-- Environment extension storing captured highlighting for blueprint declarations.
    Keyed by declaration name, stores the Highlighted value captured during elaboration. -/
initialize dressedDeclExt : NameMapExtension Highlighted ←
  registerNameMapExtension Highlighted

/-- Get all captured highlighting for the current environment. -/
def getModuleHighlighting (env : Environment) : NameMap Highlighted :=
  dressedDeclExt.getState env

/-- Add captured highlighting for a declaration to the environment. -/
def addHighlighting (env : Environment) (declName : Name) (hl : Highlighted) : Environment :=
  dressedDeclExt.addEntry env (declName, hl)

/-! ## Core Capture Function -/

/-- Capture SubVerso highlighting from info trees for a given syntax.

    This function:
    1. Takes the syntax, messages, and info trees from the current command state
    2. Calls SubVerso's `highlightIncludingUnparsed` function
    3. Returns the highlighted result or `none` on failure

    Must be called DURING elaboration while info trees are still available.

    Returns `none` if:
    - Info trees are empty
    - SubVerso highlighting fails
-/
def captureHighlightingFromInfoTrees
    (stx : Syntax)
    (messages : Array Message)
    (trees : PersistentArray InfoTree)
    (suppressedNamespaces : List Name := [])
    : TermElabM (Option Highlighted) := do
  if trees.isEmpty then
    return none
  try
    let hl ← highlightIncludingUnparsed stx messages trees suppressedNamespaces
    return some hl
  catch _ =>
    -- Silently fail - highlighting is an optional enhancement
    return none

/-- Capture highlighting for a declaration using current command state.

    This is the main entry point for the `@[blueprint]` attribute handler.
    It extracts info trees from the current command state and captures highlighting.

    Must be called DURING elaboration while info trees are still available.
    Silently does nothing if:
    - Called outside elaboration context
    - Info trees are not available
    - SubVerso highlighting fails
-/
def captureHighlighting (declName : Name) (stx : Syntax) : CommandElabM Unit := do
  -- Check if highlighting is enabled
  unless blueprint.highlighting.get (← getOptions) do return

  try
    -- Get current info trees from command state
    let trees := (← get).infoState.trees
    if trees.isEmpty then
      trace[blueprint.debug] "No info trees available for {declName}"
      return

    -- Get messages for this syntax range
    let allMessages := (← get).messages.toArray
    let fileMap := (← read).fileMap
    let messages := allMessages.filter fun msg =>
      !msg.isSilent &&
      stx.getRange?.any fun _ =>
        let msgStartPos := msg.pos
        -- Check if message position falls within syntax range
        match stx.getPos?, stx.getTailPos? with
        | some startPos, some endPos =>
          let stxStartLine := fileMap.toPosition startPos |>.line
          let stxEndLine := fileMap.toPosition endPos |>.line
          msgStartPos.line >= stxStartLine && msgStartPos.line <= stxEndLine
        | _, _ => false

    -- Run SubVerso highlighting in TermElabM
    let hl? ← liftTermElabM do
      captureHighlightingFromInfoTrees stx messages trees []

    match hl? with
    | some hl =>
      -- Store in environment extension
      modifyEnv fun env => addHighlighting env declName hl
      trace[blueprint] "Captured highlighting for {declName}"
    | none =>
      trace[blueprint.debug] "Highlighting capture returned none for {declName}"
  catch _ =>
    -- Silently fail - highlighting is optional enhancement
    trace[blueprint.debug] "Failed to capture highlighting for {declName}"

/-! ## Base64 Encoding -/

/-- Base64 encoding alphabet. -/
private def base64Chars : String :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

/-- Array of base64 characters for efficient indexing. -/
private def base64Array : Array Char := base64Chars.toList.toArray

/-- Encode a ByteArray to base64. -/
def encodeBase64 (data : ByteArray) : String := Id.run do
  let mut result := ""
  let mut i := 0
  while i < data.size do
    let b0 := data.get! i
    let b1 := if i + 1 < data.size then data.get! (i + 1) else 0
    let b2 := if i + 2 < data.size then data.get! (i + 2) else 0

    let c0 := (b0 >>> 2) &&& 0x3F
    let c1 := ((b0 &&& 0x03) <<< 4) ||| ((b1 >>> 4) &&& 0x0F)
    let c2 := ((b1 &&& 0x0F) <<< 2) ||| ((b2 >>> 6) &&& 0x03)
    let c3 := b2 &&& 0x3F

    result := result.push (base64Array[c0.toNat]!)
    result := result.push (base64Array[c1.toNat]!)
    if i + 1 < data.size then
      result := result.push (base64Array[c2.toNat]!)
    else
      result := result.push '='
    if i + 2 < data.size then
      result := result.push (base64Array[c3.toNat]!)
    else
      result := result.push '='
    i := i + 3
  return result

/-- Encode a String to base64 (UTF-8 encoded). -/
def stringToBase64 (s : String) : String :=
  encodeBase64 s.toUTF8

/-! ## JSON Serialization Helpers -/

/-- Serialize a Highlighted value to JSON using SubVerso's deduplicated export format.
    This produces a compact JSON representation suitable for storage. -/
def serializeHighlightedToJson (hl : Highlighted) : Json :=
  hl.exportCode.toJson

/-- Serialize a NameMap of Highlighted values to JSON in SubVerso Module format.
    This format is compatible with `subverso-extract-mod` output. -/
def serializeHighlightingMapToJson (highlighting : NameMap Highlighted) : Json :=
  let items : Array ModuleItem := highlighting.toList.foldl (init := #[]) fun acc (name, hl) =>
    acc.push {
      range := none  -- Range info not available at capture time
      kind := `blueprint
      defines := #[name]
      code := hl
    }
  let module : SubVerso.Module.Module := { items }
  module.toJson

/-- Get the output path for a module's dressed JSON file.
    Returns `.lake/build/dressed/{Module/Path}.json` -/
def getDressedOutputPath (buildDir : System.FilePath) (moduleName : Name) : System.FilePath :=
  let modulePath := moduleName.components.foldl (init := buildDir / "dressed")
    fun path component => path / component.toString
  modulePath.withExtension "json"

/-- Get the output path for a module's highlighting JSON file (legacy path).
    Returns `.lake/build/highlighted/{Module/Path}.json` -/
def getHighlightingOutputPath (buildDir : System.FilePath) (moduleName : Name) : System.FilePath :=
  let modulePath := moduleName.components.foldl (init := buildDir / "highlighted")
    fun path component => path / component.toString
  modulePath.withExtension "json"

/-- Write highlighted code to a JSON file atomically.
    Uses write-to-temp-then-rename for crash safety on POSIX systems. -/
def writeHighlightingJsonAtomic (path : System.FilePath) (json : Json) : IO Unit := do
  -- Ensure parent directory exists
  if let some parent := path.parent then
    IO.FS.createDirAll parent

  -- Write to temp file first
  let tmpPath := path.withExtension "json.tmp"
  IO.FS.writeFile tmpPath json.compress

  -- Atomic rename (on POSIX systems)
  IO.FS.rename tmpPath path

/-- Write all captured highlighting for a declaration to a JSON file.
    The file is written to `.lake/build/highlighted/{Module/Path}/{DeclName}.json`. -/
def writeHighlightingJson (buildDir : System.FilePath) (moduleName : Name)
    (declName : Name) (hl : Highlighted) : IO Unit := do
  let moduleDir := moduleName.components.foldl (init := buildDir / "highlighted")
    fun path component => path / component.toString
  let path := moduleDir / s!"{declName}.json"
  writeHighlightingJsonAtomic path (serializeHighlightedToJson hl)

/-- Write all captured module highlighting to a single JSON file.
    The file is written to `.lake/build/highlighted/{Module/Path}.json`. -/
def writeModuleHighlightingJson (buildDir : System.FilePath) (moduleName : Name)
    (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  let path := getHighlightingOutputPath buildDir moduleName
  writeHighlightingJsonAtomic path (serializeHighlightingMapToJson highlighting)

/-! ## HTML Serialization -/

/-- Get the output path for a module's highlighting HTML map file (legacy path).
    Returns `.lake/build/highlighted/{Module/Path}.html.json` -/
def getHighlightingHtmlOutputPath (buildDir : System.FilePath) (moduleName : Name) : System.FilePath :=
  let modulePath := moduleName.components.foldl (init := buildDir / "highlighted")
    fun path component => path / component.toString
  modulePath.withExtension "html.json"

/-- Serialize a NameMap of Highlighted values to a JSON map of declaration name → HTML string.
    This format allows Output.lean to directly embed pre-rendered HTML. -/
def serializeHighlightingMapToHtmlJson (highlighting : NameMap Highlighted) : Json :=
  let entries : List (String × Json) := highlighting.toList.map fun (name, hl) =>
    (name.toString, Json.str (HtmlRender.renderHighlightedToHtml hl))
  Json.mkObj entries

/-- Serialize a NameMap of Highlighted values to the full dressed artifact format.
    Includes HTML, base64-encoded HTML, and base64-encoded JSON for fast TeX generation.

    Format per declaration:
    ```json
    {
      "html": "<pre>...</pre>",
      "htmlBase64": "PHByZT4uLi48L3ByZT4=",
      "jsonBase64": "eyJoaWdobGlnaHRlZCI6Li4ufQ=="
    }
    ```
-/
def serializeDressedArtifacts (highlighting : NameMap Highlighted) : Json :=
  let entries : List (String × Json) := highlighting.toList.map fun (name, hl) =>
    let html := HtmlRender.renderHighlightedToHtml hl
    let jsonStr := (toJson hl).compress
    let artifact := Json.mkObj [
      ("html", Json.str html),
      ("htmlBase64", Json.str (stringToBase64 html)),
      ("jsonBase64", Json.str (stringToBase64 jsonStr))
    ]
    (name.toString, artifact)
  Json.mkObj entries

/-- Write all captured module highlighting as HTML to a JSON map file.
    The file is written to `.lake/build/highlighted/{Module/Path}.html.json` (legacy path). -/
def writeModuleHighlightingHtml (buildDir : System.FilePath) (moduleName : Name)
    (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  let path := getHighlightingHtmlOutputPath buildDir moduleName
  writeHighlightingJsonAtomic path (serializeHighlightingMapToHtmlJson highlighting)

/-- Write all captured module dressed artifacts to a JSON file.
    The file is written to `.lake/build/dressed/{Module/Path}.json`. -/
def writeModuleDressedArtifacts (buildDir : System.FilePath) (moduleName : Name)
    (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  let path := getDressedOutputPath buildDir moduleName
  writeHighlightingJsonAtomic path (serializeDressedArtifacts highlighting)

/-! ## Module Finalization -/

/-- Export all captured artifacts for the current module.
    Call this when module compilation completes.

    Writes to three locations:
    - `.lake/build/dressed/{Module/Path}.json` - Full dressed artifacts (new format)
    - `.lake/build/highlighted/{Module/Path}.json` - SubVerso JSON format (legacy)
    - `.lake/build/highlighted/{Module/Path}.html.json` - Pre-rendered HTML map (legacy) -/
def exportModuleHighlighting (buildDir : System.FilePath) : CommandElabM Unit := do
  let env ← getEnv
  let moduleName := env.header.mainModule
  let highlighting := getModuleHighlighting env

  if highlighting.isEmpty then
    trace[blueprint.debug] "No highlighting to export for {moduleName}"
    return

  trace[blueprint] "Exporting {highlighting.size} dressed declarations for {moduleName}"

  -- Write dressed format (new unified format with base64 strings)
  try
    writeModuleDressedArtifacts buildDir moduleName highlighting
    trace[blueprint] "Wrote dressed artifacts for {moduleName}"
  catch e =>
    let errMsg ← e.toMessageData.toString
    IO.eprintln s!"[blueprint] Failed to write dressed artifacts for {moduleName}: {errMsg}"
    trace[blueprint] "Failed to write dressed artifacts for {moduleName}: {e.toMessageData}"

  -- Write JSON format (backward compatibility with subverso-extract-mod)
  try
    writeModuleHighlightingJson buildDir moduleName highlighting
    trace[blueprint] "Wrote highlighting JSON for {moduleName}"
  catch e =>
    trace[blueprint] "Failed to write highlighting JSON for {moduleName}: {e.toMessageData}"

  -- Write HTML format (backward compatibility)
  try
    writeModuleHighlightingHtml buildDir moduleName highlighting
    trace[blueprint] "Wrote highlighting HTML for {moduleName}"
  catch e =>
    let errMsg ← e.toMessageData.toString
    IO.eprintln s!"[blueprint] Failed to write highlighting HTML for {moduleName}: {errMsg}"
    trace[blueprint] "Failed to write highlighting HTML for {moduleName}: {e.toMessageData}"

/-! ## Dress Command -/

/-- Optional manual trigger for dressing.
    With `blueprint.dress=true`, export happens automatically after each `@[blueprint]` declaration,
    so this command is typically not needed. -/
syntax (name := dress) "#dress" : command

/-- IO.Ref to track if #dress has been called in this module. -/
initialize dressEnabledRef : IO.Ref Bool ← IO.mkRef false

@[command_elab dress]
def elabDress : CommandElab := fun _stx => do
  -- Skip if dress mode is not enabled (regular `lake build`)
  unless blueprint.dress.get (← getOptions) do return
  -- Also skip if highlighting is disabled
  unless blueprint.highlighting.get (← getOptions) do return

  -- Mark this module as requiring dressing
  dressEnabledRef.set true
  trace[blueprint] "#dress: Module marked for dressing"

/-- Check if dressing is enabled for the current module.
    Returns true if `#dress` was called AND `blueprint.dress` option is true. -/
def isDressEnabled : IO Bool := do
  dressEnabledRef.get

/-- Export dressed artifacts if dressing is enabled.
    Call this after the last declaration in the module. -/
def exportIfDressEnabled : CommandElabM Unit := do
  unless (← dressEnabledRef.get) do return
  let buildDir : System.FilePath := ".lake" / "build"
  exportModuleHighlighting buildDir

/-- Export all captured blueprint highlighting for the current module.
    Writes to `.lake/build/dressed/`, `.lake/build/highlighted/`, and `.lake/build/highlighted/*.html.json`. -/
syntax (name := exportBlueprintHighlighting) "#export_blueprint_highlighting" : command

@[command_elab exportBlueprintHighlighting]
def elabExportBlueprintHighlighting : CommandElab := fun _stx => do
  -- Skip if highlighting is disabled
  unless blueprint.highlighting.get (← getOptions) do return

  -- Get build directory from Lake workspace (default to .lake/build)
  let buildDir : System.FilePath := ".lake" / "build"
  exportModuleHighlighting buildDir

/-! ## Loading Captured Highlighting -/

/-- Load highlighted code from a JSON file.
    Returns empty map if file doesn't exist or parsing fails. -/
def loadHighlightingFromJson (path : System.FilePath) : IO (NameMap Highlighted) := do
  if !(← path.pathExists) then
    return {}

  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return {}
  | .ok json =>
    match SubVerso.Module.Module.fromJson? json with
    | .error _ => return {}
    | .ok mod =>
      return mod.items.foldl (init := {}) fun acc item =>
        item.defines.foldl (init := acc) fun acc' name =>
          acc'.insert name item.code

/-- Load highlighting for a specific module from the build cache.
    Looks for `.lake/build/highlighted/{Module/Path}.json`. -/
def loadModuleHighlighting (buildDir : System.FilePath) (moduleName : Name)
    : IO (NameMap Highlighted) := do
  let path := getHighlightingOutputPath buildDir moduleName
  loadHighlightingFromJson path

/-- Load pre-rendered HTML highlighting from a JSON map file.
    Returns a NameMap of declaration name → HTML string.
    Returns empty map if file doesn't exist or parsing fails. -/
def loadHighlightingHtmlFromJson (path : System.FilePath) : IO (NameMap String) := do
  if !(← path.pathExists) then
    return {}

  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return {}
  | .ok json =>
    match json with
    | .obj kvs =>
      return kvs.toList.foldl (init := {}) fun acc (key, val) =>
        match val with
        | .str html => acc.insert key.toName html
        | _ => acc
    | _ => return {}

/-- Load pre-rendered HTML highlighting for a specific module from the build cache.
    Looks for `.lake/build/highlighted/{Module/Path}.html.json`. -/
def loadModuleHighlightingHtml (buildDir : System.FilePath) (moduleName : Name)
    : IO (NameMap String) := do
  let path := getHighlightingHtmlOutputPath buildDir moduleName
  loadHighlightingHtmlFromJson path

/-! ## Declaration Interception via elab_rules

We use `elab_rules` to intercept declarations that have `@[blueprint]`.
After standard elaboration completes (but while info trees still exist), we capture
the highlighting and store it in the environment extension.

The key insight is that `elab_rules` run before the standard elaborators, and we can
call the standard elaborator explicitly, then capture highlighting afterward.
-/

/-- Check if declModifiers syntax contains a `@[blueprint ...]` attribute.

    The syntax structure is:
    - declModifiers = docComment? attributes? visibility? ...
    - attributes = @[ attrInstance,* ]
    - attrInstance for blueprint has kind `Architect.blueprint`
-/
def hasBlueprintAttr (mods : Syntax) : Bool :=
  -- declModifiers[1] is the optional attributes
  let attrs? := mods[1]?
  match attrs? with
  | none => false
  | some attrs =>
    if attrs.isNone then false
    else
      -- attrs is `Lean.Parser.Term.attributes` node: @[ attrInstance,* ]
      -- The actual content is in attrs[0] which is the @[...] node
      let attrsNode := attrs[0]!
      -- attrsNode[1] is the SepArray of attrInstance
      let attrInstances := attrsNode[1]!
      attrInstances.getArgs.any fun attrInst =>
        -- attrInst structure:
        --   attrInst[0] = attrKind (scoped/local/global)
        --   attrInst[1] = the actual attribute (kind = Architect.blueprint for @[blueprint ...])
        -- Check attrInst[1].kind for the attribute name
        (attrInst[1]?.map (·.getKind == `Architect.blueprint) |>.getD false) ||
        (attrInst[1]?.map (·.getKind == `blueprint) |>.getD false)

/-- Extract declaration name from a declId syntax node. -/
def getDeclNameFromDeclId (declId : Syntax) : Option Name :=
  if declId.getKind == ``Lean.Parser.Command.declId then
    declId[0]?.map (·.getId)
  else if declId.isIdent then
    some declId.getId
  else
    none

/-- Check if we're currently inside the capture hook. -/
def inCaptureHook : CommandElabM Bool := do
  blueprintCaptureHookRef.get

/-- Run an action with the capture hook flag set. -/
def withCaptureHookFlag (act : CommandElabM α) : CommandElabM α := do
  blueprintCaptureHookRef.set true
  try
    act
  finally
    blueprintCaptureHookRef.set false

/-- Elaborate a declaration command and capture highlighting for blueprint declarations.
    This helper is called by the elab_rules below.

    When `blueprint.dress=true`, automatically exports dressed artifacts after each capture.
    This allows `lake build dress` to work without requiring `#dress` in each source file. -/
def elabDeclAndCaptureHighlighting (stx : Syntax) (declId : Syntax) : CommandElabM Unit := do
  -- Run standard command elaboration with the flag set to prevent recursion
  withCaptureHookFlag do
    elabCommandTopLevel stx
    -- Capture highlighting immediately after elaboration, while info trees are still available
    if let some name := getDeclNameFromDeclId declId then
      -- Resolve the name with current namespace
      let ns ← getCurrNamespace
      let fullName := if ns.isAnonymous then name else ns ++ name
      let env ← getEnv
      let resolvedName := if env.contains fullName then fullName else name
      if env.contains resolvedName then
        captureHighlighting resolvedName stx

        -- Auto-export when dress mode is enabled (no #dress command needed)
        if blueprint.dress.get (← getOptions) then
          let buildDir : System.FilePath := ".lake" / "build"
          exportModuleHighlighting buildDir

/-- Elaboration rules for declarations with @[blueprint] attribute.
    These intercept declarations and capture highlighting after elaboration.

    We use scoped rules so they only apply when Architect is imported.
    The rules check the `inCaptureHook` flag to prevent infinite recursion.
-/

-- Theorem declarations with @[blueprint]
-- Use high priority to run before built-in elaborators
@[command_elab Lean.Parser.Command.declaration]
def elabBlueprintDeclaration : CommandElab := fun stx => do
  -- Only handle declarations
  unless stx.getKind == ``Lean.Parser.Command.declaration do
    throwUnsupportedSyntax
  let decl := stx[1]
  let declKind := decl.getKind
  -- Handle theorem/lemma (both use Command.theorem kind)
  unless declKind == ``Lean.Parser.Command.theorem do
    throwUnsupportedSyntax
  if (← inCaptureHook) then
    throwUnsupportedSyntax
  let mods := stx[0]
  let hasBlueprint := hasBlueprintAttr mods
  trace[blueprint.debug] "elabBlueprintDeclaration: hasBlueprint={hasBlueprint}"
  if hasBlueprint then
    let declId := decl[1]
    let declName := getDeclNameFromDeclId declId |>.getD `unknown
    IO.println s!"[Hook] Capturing highlighting for @[blueprint] theorem: {declName}"
    elabDeclAndCaptureHighlighting stx declId
  else
    throwUnsupportedSyntax

-- Definition declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers def $declId:declId $_sig:optDeclSig $_val:declVal) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    let hasBlueprint := hasBlueprintAttr mods
    trace[blueprint.debug] "elab_rules def: hasBlueprint={hasBlueprint}"
    if hasBlueprint then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Abbreviation declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers abbrev $declId:declId $_sig:optDeclSig $_val:declVal) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Structure declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers structure $declId:declId $_*) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Class declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers class $declId:declId $_*) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Inductive declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers inductive $declId:declId $_*) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Instance declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers instance $[$_prio:namedPrio]? $declId:declId $_sig:declSig $_val:declVal) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

end Architect
