import Architect.Output
import Architect.SubVersoExtract
import Architect.Hook


namespace Architect

/-!
Loading the analysis result of a module.
-/

open Lean

/-- This is copied from `DocGen4.envOfImports`. -/
def envOfImports (imports : Array Name) : IO Environment := do
  -- needed for modules which use syntax registered with `initialize add_parser_alias ..`
  unsafe Lean.enableInitializersExecution
  importModules (imports.map (Import.mk · false true false)) Options.empty (leakEnv := true) (loadExts := true)

/-- This is copied from `DocGen4.load`, except for separate handling of `options`. -/
def runEnvOfImports (imports : Array Name) (options : Options) (x : CoreM α) : IO α := do
  initSearchPath (← findSysroot)
  let env ← envOfImports imports
  let config := {
    maxHeartbeats := 100000000,
    options := options
      |>.set `debug.skipKernelTC true
      |>.set `Elab.async false,
    fileName := default,
    fileMap := default,
  }

  Prod.fst <$> x.toIO config { env }

/-- Outputs the blueprint of a module.
    If `highlightedJsonPath?` is provided, loads cached highlighting from that path.
    Otherwise, tries to load from `.lake/build/dressed/{Module/Path}.json` (Hook.lean output),
    falling back to calling subverso-extract-mod directly (slower).
    Pre-rendered HTML is loaded from `.lake/build/dressed/{Module/Path}.html.json` if available. -/
def latexOutputOfImportModule (module : Name) (options : Options)
    (highlightedJsonPath? : Option String := none) : IO LatexOutput := do
  let buildDir : System.FilePath := ".lake" / "build"
  let highlightingMap ← match highlightedJsonPath? with
    | some path => SubVersoExtract.loadHighlightingFromFile path
    | none =>
      -- Try to load from standard Hook.lean location, fall back to subverso-extract-mod
      SubVersoExtract.loadHighlightingWithFallback module buildDir
  -- Load pre-rendered HTML map from .html.json files (created by Hook.lean)
  let htmlMap ← loadModuleHighlightingHtml buildDir module
  runEnvOfImports #[module] options (moduleToLatexOutput module highlightingMap htmlMap)

/-- Outputs the JSON data for the blueprint of a module.
    If `highlightedJsonPath?` is provided, loads cached highlighting from that path.
    Otherwise, tries to load from `.lake/build/dressed/{Module/Path}.json` (Hook.lean output),
    falling back to calling subverso-extract-mod directly (slower). -/
def jsonOfImportModule (module : Name) (options : Options)
    (highlightedJsonPath? : Option String := none) : IO Json := do
  let highlightingMap ← match highlightedJsonPath? with
    | some path => SubVersoExtract.loadHighlightingFromFile path
    | none =>
      -- Try to load from standard Hook.lean location, fall back to subverso-extract-mod
      let buildDir : System.FilePath := ".lake" / "build"
      SubVersoExtract.loadHighlightingWithFallback module buildDir
  runEnvOfImports #[module] options (moduleToJson module highlightingMap)

end Architect
