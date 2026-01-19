# Elaboration-Time Highlighting Capture Design

## Executive Summary

This document describes a robust architecture for capturing SubVerso syntax highlighting DURING Lean elaboration, writing JSON files to `.lake/build/highlighted/{Module/Path}.json`, so that the blueprint facet can read pre-computed highlighting without re-elaboration.

The key insight is that info trees (required for semantic highlighting) are ephemeral - they exist in `commandState.infoState.trees` during elaboration but are discarded afterward. We must capture highlighting while these trees are available.

## Architecture Overview

```
                              ELABORATION PHASE
+------------------------------------------------------------------+
|                                                                  |
|  [Lean Compiler]                                                 |
|       |                                                          |
|       v                                                          |
|  [Command Elaboration]                                           |
|       |                                                          |
|       +---> [@[blueprint] attribute handler]                     |
|       |            |                                             |
|       |            v                                             |
|       |     [HighlightCapture.captureDeclaration]                |
|       |            |                                             |
|       |            +---> Read info trees from commandState       |
|       |            |                                             |
|       |            +---> Call SubVerso highlightIncludingUnparsed|
|       |            |                                             |
|       |            +---> Store in highlightedDeclExt             |
|       |                                                          |
|       v                                                          |
|  [Module Compilation Complete]                                   |
|       |                                                          |
|       +---> [module_data hook] Write JSON to build/highlighted/  |
|                                                                  |
+------------------------------------------------------------------+

                              BUILD PHASE
+------------------------------------------------------------------+
|                                                                  |
|  [Lake `highlighted` facet]                                      |
|       |                                                          |
|       +---> Depends on module olean                              |
|       |                                                          |
|       +---> Runs `subverso-extract-mod` for FULL module JSON     |
|             (captures non-blueprint declarations too)            |
|                                                                  |
|  [Lake `blueprint` facet]                                        |
|       |                                                          |
|       +---> Reads JSON from build/highlighted/                   |
|       |                                                          |
|       +---> Builds NameMap Highlighted for blueprint nodes       |
|                                                                  |
+------------------------------------------------------------------+
```

## Module Structure

### File Organization

```
Architect/
  Basic.lean           -- Core data types (Node, NodeWithPos, etc.) [existing]
  Command.lean         -- blueprint_comment command [existing]
  Content.lean         -- BlueprintContent extraction [existing]
  Highlighting.lean    -- highlightSource, highlightDeclaration [existing]
  SubVersoExtract.lean -- CLI extraction utilities [existing]

  HighlightCapture.lean   -- NEW: Elaboration-time capture logic
  HighlightExport.lean    -- NEW: Module data export to JSON
```

### Module Responsibilities

1. **HighlightCapture.lean**: Core capturing logic
   - Environment extension to store captured highlighting per declaration
   - Attribute handler that captures highlighting when `@[blueprint]` is processed
   - Internal helper functions for interacting with SubVerso

2. **HighlightExport.lean**: Serialization and file I/O
   - Module data hooks to export highlighting when module completes
   - JSON serialization using SubVerso.Module format
   - File path utilities

## Key Data Types and Functions

### Environment Extension for Captured Highlighting

```lean
-- In HighlightCapture.lean

/-- Environment extension storing captured highlighting for blueprint declarations.
    Keyed by declaration name, stores the Highlighted value captured during elaboration. -/
initialize highlightedDeclExt : NameMapExtension SubVerso.Highlighting.Highlighted <-
  registerNameMapExtension SubVerso.Highlighting.Highlighted

/-- Get all captured highlighting for the current module. -/
def getModuleHighlighting (env : Environment) : NameMap Highlighted :=
  highlightedDeclExt.getState env
```

### Attribute Handler

```lean
-- In HighlightCapture.lean

/-- Internal flag to track if we're currently capturing highlighting.
    This prevents re-entrant calls during attribute processing. -/
structure CaptureContext where
  isCapturing : Bool := false
deriving Inhabited

/-- Reader context for capture operations. -/
abbrev CaptureM := ReaderT CaptureContext CommandElabM

/-- Check if currently in a capture operation. -/
def isCapturing : CaptureM Bool := do
  return (← read).isCapturing

/-- Run an action with capture flag set. -/
def withCapturing (act : CaptureM α) : CaptureM α :=
  withReader (fun ctx => { ctx with isCapturing := true }) act
```

### Core Capture Function

```lean
-- In HighlightCapture.lean

/-- Capture SubVerso highlighting for a declaration using current info trees.

    This function:
    1. Retrieves info trees from the current command state
    2. Calls SubVerso's highlightIncludingUnparsed
    3. Stores the result in the environment extension

    Must be called DURING elaboration while info trees are still available.
    Silently does nothing if:
    - Called outside elaboration context
    - Info trees are not available
    - SubVerso highlighting fails
-/
def captureHighlighting (declName : Name) (stx : Syntax) : CommandElabM Unit := do
  try
    -- Get current info trees from command state
    let trees := (← get).infoState.trees
    if trees.isEmpty then
      trace[blueprint.debug] "No info trees available for {declName}"
      return

    -- Get messages for this syntax range
    let messages := (← get).messages.toArray.filter fun msg =>
      !msg.isSilent &&
      stx.getRange?.any fun range =>
        msg.pos.line >= range.start.line &&
        (msg.endPos.map (·.line <= range.stop.line)).getD true

    -- Run SubVerso highlighting in TermElabM
    let hl <- liftTermElabM do
      highlightIncludingUnparsed stx messages trees []

    -- Store in environment extension
    modifyEnv fun env => highlightedDeclExt.addEntry env (declName, hl)

    trace[blueprint] "Captured highlighting for {declName}"
  catch e =>
    -- Silently fail - highlighting is optional enhancement
    trace[blueprint.debug] "Failed to capture highlighting for {declName}: {e.toMessageData}"
```

### Attribute Integration

The key design decision is WHERE to capture highlighting. Two viable approaches:

**Option A: Custom attribute handler (Recommended)**

```lean
-- Modify the existing @[blueprint] attribute processing
-- After the attribute adds the Node to blueprintExt, capture highlighting

/-- Callback invoked after @[blueprint] attribute is fully processed. -/
def onBlueprintAttrApplied (declName : Name) (stx : Syntax) : CommandElabM Unit := do
  -- Only capture if option enabled
  unless blueprint.highlighting.get (← getOptions) do return

  -- Capture highlighting using current command state
  captureHighlighting declName stx
```

**Option B: Post-declaration hook**

```lean
-- Use Lean's declaration hooks to capture after any declaration
-- Filter to only blueprint-tagged declarations

register_post_decl_hook fun declName => do
  let env <- getEnv
  if blueprintExt.getState env |>.contains declName then
    captureHighlighting declName ...
```

Option A is preferred because:
- We have access to the exact syntax that was elaborated
- We know immediately that `@[blueprint]` was applied
- The info trees are guaranteed to be in the correct state

### Module Export Hook

```lean
-- In HighlightExport.lean

/-- Write all captured highlighting to JSON file.
    Called when module compilation completes. -/
def exportModuleHighlighting : IO Unit := do
  let env <- getEnv
  let moduleName := env.header.mainModule

  -- Get captured highlighting
  let highlighting := getModuleHighlighting env
  if highlighting.isEmpty then return

  -- Build output path: .lake/build/highlighted/Module/Path.json
  let buildDir <- getBuildDir
  let modulePath := moduleName.components.foldl (· / ·.toString) ""
  let outputPath := buildDir / "highlighted" / modulePath |>.withExtension "json"

  -- Convert to SubVerso.Module format for compatibility
  let items : Array SubVerso.Module.ModuleItem := highlighting.fold (init := #[]) fun acc name hl =>
    acc.push {
      range := none  -- Could extract from DeclarationRanges if needed
      kind := `blueprint
      defines := #[name]
      code := hl
    }

  let module : SubVerso.Module.Module := { items }

  -- Write JSON
  IO.FS.createDirAll outputPath.parent.get!
  IO.FS.writeFile outputPath (toString module.toJson)
```

## Recursion Prevention Strategy

The previous implementation used a global `IO.Ref Bool` which had initialization order issues. The new design uses a **context-based approach**:

### Why Context-Based is Better

1. **No module initialization issues**: No `initialize` declarations needed
2. **Thread-safe**: Each elaboration thread has its own context
3. **Scope-safe**: Flag is automatically cleared when scope exits
4. **Composable**: Works correctly with nested elaboration contexts

### Implementation

```lean
/-- Context extension for tracking capture state. -/
structure HighlightCaptureCtx where
  /-- True if we're currently inside a capture operation. -/
  inCapture : Bool := false
deriving Inhabited

/-- Monad transformer adding capture context. -/
abbrev HighlightCaptureT (m : Type -> Type) := ReaderT HighlightCaptureCtx m

/-- Check if currently capturing (to prevent recursion). -/
def inCapture [MonadReaderOf HighlightCaptureCtx m] : m Bool :=
  return (← read).inCapture

/-- Run action with capture flag set. Automatically cleared on exit. -/
def withCaptureFlag [MonadWithReaderOf HighlightCaptureCtx m] (act : m α) : m α :=
  withReader (fun ctx => { ctx with inCapture := true }) act
```

### Alternative: Command State Extension

If ReaderT composition is problematic, use Lean's elaboration context:

```lean
/-- Marker stored in elaboration context to prevent recursion. -/
initialize highlightCaptureMarker : Unit <- pure ()

def isCapturing : CommandElabM Bool := do
  -- Check if marker is in current scope
  return (← getScope).hasAttribute `highlightCapture

def withCapturing (act : CommandElabM α) : CommandElabM α := do
  if <- isCapturing then act  -- Already capturing, just run
  else
    -- Add marker to current scope
    withScope (fun s => s.addAttribute `highlightCapture) act
```

## JSON File Format and Paths

### Output Path Convention

```
{project_root}/.lake/build/highlighted/{Module/Path}.json
```

Examples:
- `MyProject.Theorems.Basic` -> `.lake/build/highlighted/MyProject/Theorems/Basic.json`
- `Mathlib.Algebra.Group.Basic` -> `.lake/build/highlighted/Mathlib/Algebra/Group/Basic.json`

### JSON Format

Use SubVerso's existing `Module` format for compatibility:

```json
{
  "data": {
    // SubVerso Export deduplication data
    "tokens": [...],
    "messages": [...],
    // etc.
  },
  "items": [
    {
      "range": {"start": {"line": 10, "column": 1}, "end": {"line": 15, "column": 4}},
      "kind": "Lean.Parser.Command.theorem",
      "defines": ["MyTheorem"],
      "code": {
        // Exported Highlighted structure (deduplicated via Export)
      }
    },
    // ... more items
  ]
}
```

### Writing Timing

Two options for when to write the JSON:

**Option A: On-demand during module finalization (Recommended)**

```lean
-- Register a module_data export that writes JSON when module is serialized
initialize registerModuleExportHook fun modName => do
  let highlighting := getModuleHighlighting (← getEnv)
  unless highlighting.isEmpty do
    exportHighlightingToFile modName highlighting
```

**Option B: At end of each declaration**

```lean
-- Write immediately after each capture
-- Pro: More incremental
-- Con: Many small writes, potential for incomplete files on crash
```

Option A is preferred for atomicity and efficiency.

## Blueprint Facet Integration

### Facet Definition Update

```lean
-- In lakefile.lean

/-- Facet that extracts highlighted JSON for a module.
    Prefers reading from elaboration-time capture, falls back to subverso-extract-mod. -/
module_facet highlighted (mod : Module) : FilePath := do
  let ws <- getWorkspace
  let buildDir := ws.root.buildDir
  let hlFile := mod.filePath (buildDir / "highlighted") "json"

  -- If elaboration-time capture file exists and is newer than olean, use it
  -- Otherwise, run subverso-extract-mod as fallback

  let modJob <- mod.olean.fetch
  modJob.mapM fun _oleanFile => do
    -- Check if elaboration-time capture exists
    if <- hlFile.pathExists then
      let hlMTime <- IO.FS.getModificationTime hlFile
      let oleanMTime <- IO.FS.getModificationTime _oleanFile
      if hlMTime >= oleanMTime then
        return hlFile  -- Use cached

    -- Fallback to subverso-extract-mod
    let some extract <- findLeanExe? `«subverso-extract-mod»
      | error "subverso-extract-mod executable not found"

    buildFileUnlessUpToDate' hlFile do
      proc { cmd := extract.toString, args := #[mod.name.toString, hlFile.toString] }

    pure hlFile
```

### Reading from Blueprint Facet

```lean
-- In buildModuleBlueprint (lakefile.lean)

def buildModuleBlueprint (mod : Module) (ext : String) (extractArgs : Array String) : FetchM (Job Unit) := do
  let hlJob <- fetch <| mod.facet `highlighted

  hlJob.bindM fun hlFile => do
    -- hlFile now contains either:
    -- 1. Elaboration-time captured JSON (if @[blueprint] was used)
    -- 2. Full module highlighting from subverso-extract-mod

    -- Both use the same JSON format, so the blueprint extractor handles them uniformly
    ...
```

## Edge Cases and Error Handling

### 1. Missing Info Trees

**Scenario**: `captureHighlighting` called when info trees are empty or unavailable.

**Handling**:
```lean
if trees.isEmpty then
  trace[blueprint.debug] "No info trees available"
  return  -- Silent no-op
```

### 2. SubVerso Highlighting Failure

**Scenario**: SubVerso throws an exception during highlighting (malformed syntax, internal error).

**Handling**:
```lean
try
  let hl <- highlightIncludingUnparsed ...
  modifyEnv fun env => highlightedDeclExt.addEntry env (declName, hl)
catch e =>
  -- Log at debug level, do not fail compilation
  trace[blueprint.debug] "Highlighting failed for {declName}: {e.toMessageData}"
  -- Blueprint will work, just without highlighting for this declaration
```

### 3. Duplicate Captures

**Scenario**: Multiple `@[blueprint]` attributes on the same declaration, or attribute applied multiple times.

**Handling**: Use `NameMap` semantics - last write wins:
```lean
-- NameMapExtension.addEntry overwrites existing entries
modifyEnv fun env => highlightedDeclExt.addEntry env (declName, hl)
```

### 4. Instance Declarations Without Explicit Names

**Scenario**: `instance : Foo Bar` generates a synthetic name.

**Handling**: Use the resolved declaration name from the environment:
```lean
-- Get the actual name that was registered
let declName := getDeclNameFromEnv env stx
```

### 5. Mutual/Nested Declarations

**Scenario**: `mutual ... end` blocks or nested `where` clauses.

**Handling**: Capture each individual declaration separately. The syntax range should be specific to each declaration within the mutual block.

### 6. Macro-Generated Declarations

**Scenario**: Macros that expand to multiple declarations.

**Handling**: Only the top-level syntax that has `@[blueprint]` is captured. Macro-generated declarations without explicit `@[blueprint]` are not captured (this is the expected behavior).

### 7. Incremental Compilation

**Scenario**: Only some modules are recompiled.

**Handling**: JSON files are written per-module. Lake's dependency tracking ensures:
- If module olean is rebuilt, JSON is regenerated
- If module is unchanged, existing JSON is used

### 8. Build Interruption

**Scenario**: Build is killed mid-way through JSON writing.

**Handling**: Use atomic file writes:
```lean
def writeJsonAtomically (path : FilePath) (content : String) : IO Unit := do
  let tmpPath := path.withExtension "json.tmp"
  IO.FS.writeFile tmpPath content
  IO.FS.rename tmpPath path  -- Atomic on POSIX
```

## Declaration Type Coverage

The system must handle all declaration types that can have `@[blueprint]`:

| Declaration Type | Syntax Kind | Notes |
|-----------------|-------------|-------|
| `theorem` | `Lean.Parser.Command.theorem` | Primary use case |
| `def` | `Lean.Parser.Command.definition` | Definitions |
| `lemma` | `Lean.Parser.Command.theorem` | Alias for theorem |
| `abbrev` | `Lean.Parser.Command.abbrev` | Abbreviations |
| `instance` | `Lean.Parser.Command.instance` | May have auto-generated names |
| `structure` | `Lean.Parser.Command.structure` | Includes fields |
| `class` | `Lean.Parser.Command.classInductive` | Type classes |
| `inductive` | `Lean.Parser.Command.inductive` | Inductive types |
| `example` | `Lean.Parser.Command.example` | Anonymous, captured by range |

All of these are handled uniformly by:
1. The attribute handler receiving the full declaration syntax
2. SubVerso's `highlightIncludingUnparsed` working on any valid Lean syntax

## Performance Considerations

### Memory Usage

- `Highlighted` structures can be large for complex proofs
- Stored in environment extension only for current module
- Serialized to JSON and released after module compilation

### CPU Usage

- SubVerso highlighting is computationally expensive
- Runs once per `@[blueprint]` declaration during compilation
- No additional runtime cost after compilation

### Build Time Impact

- Adds ~100-500ms per decorated declaration (rough estimate)
- Parallelizes naturally with module compilation
- Only runs for `@[blueprint]` declarations, not all code

## Testing Strategy

1. **Unit tests**: Test `captureHighlighting` with mock command state
2. **Integration tests**: Full build with `@[blueprint]` declarations
3. **Edge case tests**: Each edge case listed above
4. **Regression tests**: Ensure existing blueprint functionality unchanged

## Migration Path

1. Implement `HighlightCapture.lean` and `HighlightExport.lean`
2. Modify `@[blueprint]` attribute to call capture after registration
3. Update lakefile facet to prefer elaboration-time capture
4. Test with existing projects
5. Remove dependency on `subverso-extract-mod` for blueprint declarations (keep as fallback for non-blueprint code)
