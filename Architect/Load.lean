import Architect.Output
import Architect.SubVersoExtract


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
    Highlighted code is obtained via subverso-extract-mod, with fallback to
    the Hook mechanism during elaboration. -/
def latexOutputOfImportModule (module : Name) (options : Options) : IO LatexOutput := do
  -- Extract highlighting map using subverso-extract-mod (returns empty map on failure)
  let highlightingMap ← SubVersoExtract.extractHighlightingMap module
  runEnvOfImports #[module] options (moduleToLatexOutput module highlightingMap)

/-- Outputs the JSON data for the blueprint of a module.
    Highlighted code is obtained via subverso-extract-mod, with fallback to
    the Hook mechanism during elaboration. -/
def jsonOfImportModule (module : Name) (options : Options) : IO Json := do
  -- Extract highlighting map using subverso-extract-mod (returns empty map on failure)
  let highlightingMap ← SubVersoExtract.extractHighlightingMap module
  runEnvOfImports #[module] options (moduleToJson module highlightingMap)

end Architect
