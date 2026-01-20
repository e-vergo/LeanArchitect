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
# Elaboration-Time Highlighting Capture Infrastructure

This module provides the core infrastructure for capturing SubVerso syntax highlighting
DURING Lean elaboration. Info trees (required for semantic highlighting) are ephemeral -
they exist in `commandState.infoState.trees` during elaboration but are discarded afterward.
This module captures highlighting while these trees are available.

## Key Components

1. **Environment extension** (`highlightedDeclExt`): Stores captured highlighting per declaration
2. **Core capture function** (`captureHighlightingFromInfoTrees`): Calls SubVerso's highlight function
3. **JSON serialization** (`serializeHighlightedToJson`, `writeHighlightingJson`): Export to JSON files
4. **Module finalization hook**: Writes accumulated highlighting when module compilation completes

## Usage

The `@[blueprint]` attribute handler calls `captureHighlighting` after registering a node.
When the module finishes compilation, all captured highlighting is exported to
`.lake/build/highlighted/{Module/Path}.json`.
-/

open Lean Elab Command Term Meta
open SubVerso.Highlighting
open SubVerso.Module

namespace Architect

/-! ## Environment Extension for Captured Highlighting -/

/-- Environment extension storing captured highlighting for blueprint declarations.
    Keyed by declaration name, stores the Highlighted value captured during elaboration. -/
initialize highlightedDeclExt : NameMapExtension Highlighted ←
  registerNameMapExtension Highlighted

/-- Get all captured highlighting for the current environment. -/
def getModuleHighlighting (env : Environment) : NameMap Highlighted :=
  highlightedDeclExt.getState env

/-- Add captured highlighting for a declaration to the environment. -/
def addHighlighting (env : Environment) (declName : Name) (hl : Highlighted) : Environment :=
  highlightedDeclExt.addEntry env (declName, hl)

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

/-- Get the output path for a module's highlighting JSON file.
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

/-- Get the output path for a module's highlighting HTML map file.
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

/-- Write all captured module highlighting as HTML to a JSON map file.
    The file is written to `.lake/build/highlighted/{Module/Path}.html.json`. -/
def writeModuleHighlightingHtml (buildDir : System.FilePath) (moduleName : Name)
    (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  let path := getHighlightingHtmlOutputPath buildDir moduleName
  writeHighlightingJsonAtomic path (serializeHighlightingMapToHtmlJson highlighting)

/-! ## Module Finalization -/

/-- Export all captured highlighting for the current module to JSON and HTML.
    Call this when module compilation completes.

    Writes both:
    - `.lake/build/highlighted/{Module/Path}.json` - SubVerso JSON format (backward compat)
    - `.lake/build/highlighted/{Module/Path}.html.json` - Pre-rendered HTML map -/
def exportModuleHighlighting (buildDir : System.FilePath) : CommandElabM Unit := do
  let env ← getEnv
  let moduleName := env.header.mainModule
  let highlighting := getModuleHighlighting env

  if highlighting.isEmpty then
    trace[blueprint.debug] "No highlighting to export for {moduleName}"
    return

  trace[blueprint] "Exporting {highlighting.size} highlighted declarations for {moduleName}"

  -- Write JSON format (backward compatibility with subverso-extract-mod)
  try
    writeModuleHighlightingJson buildDir moduleName highlighting
    trace[blueprint] "Wrote highlighting JSON for {moduleName}"
  catch e =>
    trace[blueprint] "Failed to write highlighting JSON for {moduleName}: {e.toMessageData}"

  -- Write HTML format (pre-rendered for fast plasTeX processing)
  try
    writeModuleHighlightingHtml buildDir moduleName highlighting
    trace[blueprint] "Wrote highlighting HTML for {moduleName}"
  catch e =>
    -- Use IO.eprintln for visible errors during build
    let errMsg ← e.toMessageData.toString
    IO.eprintln s!"[blueprint] Failed to write highlighting HTML for {moduleName}: {errMsg}"
    trace[blueprint] "Failed to write highlighting HTML for {moduleName}: {e.toMessageData}"

/-! ## Export Command -/

/--
Export captured blueprint highlighting to JSON.

This command exports all highlighting captured via `@[blueprint]` attributes
in the current module to a JSON file at `.lake/build/highlighted/{Module/Path}.json`.

Usage: Add this at the end of a file with `@[blueprint]` declarations:
```
#export_blueprint_highlighting
```

The command is a no-op if:
- No highlighting has been captured in the current module
- The `blueprint.highlighting` option is disabled
-/
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

open Parser in
/-- Check if a syntax contains an attribute with the given name (iterative to avoid termination issues). -/
partial def hasAttrNamed (attrName : Name) (stx : Syntax) : Bool :=
  let rec go (worklist : List Syntax) : Bool :=
    match worklist with
    | [] => false
    | s :: rest =>
      match s with
      | .node _ kind args =>
        if kind == ``Lean.Parser.Term.attrInstance then
          if args.any (fun arg => arg.getId == attrName) then
            true
          else
            go (args.toList ++ rest)
        else
          go (args.toList ++ rest)
      | .ident _ _ id _ => id == attrName || go rest
      | _ => go rest
  go [stx]

/-- Check if a command syntax has `@[blueprint ...]` attribute in its declModifiers. -/
def hasBlueprintAttr (stx : Syntax) : Bool :=
  hasAttrNamed `blueprint stx

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
    This helper is called by the elab_rules below. -/
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

/-- Elaboration rules for declarations with @[blueprint] attribute.
    These intercept declarations and capture highlighting after elaboration.

    We use scoped rules so they only apply when Architect is imported.
    The rules check the `inCaptureHook` flag to prevent infinite recursion.
-/

-- Theorem declarations with @[blueprint]
-- Use high priority to run before built-in elaborators
@[command_elab Lean.Parser.Command.declaration]
def elabBlueprintTheorem : CommandElab := fun stx => do
  trace[blueprint.debug] "elabBlueprintTheorem called"
  -- Only handle theorem declarations
  unless stx.getKind == ``Lean.Parser.Command.declaration do
    throwUnsupportedSyntax
  let decl := stx[1]
  unless decl.getKind == ``Lean.Parser.Command.theorem do
    throwUnsupportedSyntax
  if (← inCaptureHook) then
    throwUnsupportedSyntax
  let mods := stx[0]
  let hasBlueprint := hasBlueprintAttr mods
  trace[blueprint.debug] "  hasBlueprint={hasBlueprint}"
  if hasBlueprint then
    let declId := decl[1]
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
