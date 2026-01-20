import Lake
open System Lake DSL

package LeanArchitect where
  testDriver := "ArchitectTest"

@[default_target]
lean_lib Architect

lean_lib ArchitectTest where
  globs := #[.submodules `ArchitectTest]

@[default_target]
lean_exe extract_blueprint where
  root := `Main
  supportInterpreter := true

/-- Utility script used for converting from existing blueprint format. -/
@[default_target]
lean_exe add_position_info where
  root := `scripts.convert.add_position_info
  supportInterpreter := true

require batteries from git
  "https://github.com/leanprover-community/batteries" @ "v4.27.0-rc1"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "v4.27.0-rc1"

require subverso from git
  "https://github.com/leanprover/subverso"

require verso from git
  "https://github.com/leanprover/verso.git" @ "main"

/-- Facet that provides dressed JSON for a module.

    **New behavior (preferred):** If the module imports `Architect`, highlighting is captured
    during elaboration via Hook.lean and written to `.lake/build/dressed/{Module/Path}.json`.
    This facet simply waits for the olean to be built and returns the JSON path.

    **Fallback behavior:** If the JSON file doesn't exist after olean compilation (module doesn't
    import Architect, or `#export_blueprint_highlighting` wasn't called), falls back to running
    `subverso-extract-mod` to generate the highlighting.

    Cached by Lake - only rebuilds when module's olean changes. -/
module_facet dressed (mod : Module) : FilePath := do
  let ws ← getWorkspace
  let modJob ← mod.olean.fetch

  let buildDir := ws.root.buildDir
  -- Use full module name path to match Hook.lean's getHighlightingOutputPath
  let hlFile := (mod.name.components.foldl (init := buildDir / "dressed")
    fun path component => path / component.toString).withExtension "json"

  modJob.mapM fun _oleanFile => do
    -- Check if JSON was written during elaboration (by Hook.lean)
    if ← hlFile.pathExists then
      -- JSON exists from elaboration-time capture - use it directly
      pure hlFile
    else
      -- Fallback: run subverso-extract-mod for modules that don't use Hook.lean
      let some extract ← findLeanExe? `«subverso-extract-mod»
        | error "subverso-extract-mod executable not found"
      let exeFile ← extract.exe.fetch >>= (·.await)
      buildFileUnlessUpToDate' hlFile do
        IO.FS.createDirAll (buildDir / "dressed")
        proc {
          cmd := exeFile.toString
          args := #[mod.name.toString, hlFile.toString]
          env := ← getAugmentedEnv
        }
      pure hlFile

def buildModuleBlueprint (mod : Module) (ext : String) (extractArgs : Array String) : FetchM (Job Unit) := do
  let modJob ← mod.leanArts.fetch
  let buildDir := (← getRootPackage).buildDir
  let mainFile := mod.filePath (buildDir / "blueprint" / "module") ext
  -- Skip if .tex file already exists (generated during dressing with BLUEPRINT_DRESS=1)
  -- This makes `lake build :blueprint` fast when dressing has already run
  if ← mainFile.pathExists then
    modJob.mapM fun _ => pure ()
  else
    -- Fall back to extract_blueprint for non-dressed builds
    let exeJob ← extract_blueprint.fetch
    let hlJob ← fetch <| mod.facet `dressed  -- Get cached dressed JSON
    let leanOptions := Lean.toJson mod.leanOptions |>.compress
    exeJob.bindM fun exeFile => do
      hlJob.bindM fun hlFile => do  -- Thread through highlighted JSON path
        modJob.mapM fun _ => do
          -- The output is a main file plus a list of auxiliary files
          buildFileUnlessUpToDate' mainFile do
            proc {
              cmd := exeFile.toString
              args := #["single", "--build", buildDir.toString,
                        "--highlightedJson", hlFile.toString,
                        "--options", leanOptions, mod.name.toString] ++ extractArgs
              env := ← getAugmentedEnv
            }

/-- Build module blueprint with highlighting, falling back to plain on crash. -/
def buildModuleBlueprintSafe (mod : Module) (ext : String) : FetchM (Job Unit) := do
  let modJob ← mod.leanArts.fetch
  let buildDir := (← getRootPackage).buildDir
  let mainFile := mod.filePath (buildDir / "blueprint" / "module") ext
  -- Skip if .tex file already exists (generated during dressing with BLUEPRINT_DRESS=1)
  if ← mainFile.pathExists then
    modJob.mapM fun _ => pure ()
  else
    let exeJob ← extract_blueprint.fetch
    let hlJob ← fetch <| mod.facet `dressed  -- Get cached dressed JSON
    let leanOptions := Lean.toJson mod.leanOptions |>.compress
    exeJob.bindM fun exeFile => do
      hlJob.bindM fun hlFile => do
        modJob.mapM fun _ => do
          buildFileUnlessUpToDate' mainFile do
            let env ← getAugmentedEnv
            let baseArgs := #["single", "--build", buildDir.toString,
                              "--highlightedJson", hlFile.toString,
                              "--options", leanOptions, mod.name.toString]
            -- Highlighting is now pre-computed via facet, just run extraction
            proc {
              cmd := exeFile.toString
              args := baseArgs
              env := env
            }

/-- A facet to extract the blueprint for a module (with syntax highlighting via cached facet). -/
module_facet blueprint (mod : Module) : Unit := do
  buildModuleBlueprint mod "tex" #[]

/-- A facet to extract the blueprint for a module (without syntax highlighting).
    Use this if SubVerso highlighting causes panics on certain code patterns. -/
module_facet blueprintPlain (mod : Module) : Unit := do
  buildModuleBlueprint mod "tex" #[]

/-- A facet to extract JSON data of blueprint for a module. -/
module_facet blueprintJson (mod : Module) : Unit := do
  buildModuleBlueprint mod "json" #["--json"]

/-- A facet to extract the blueprint for a module with highlighting, falling back to plain on crash. -/
module_facet blueprintSafe (mod : Module) : Unit := do
  buildModuleBlueprintSafe mod "tex"

def buildLibraryBlueprint (lib : LeanLib) (moduleFacet : Lean.Name) (ext : String) (extractArgs : Array String) : FetchM (Job Unit) := do
  let mods ← (← lib.modules.fetch).await
  let moduleJobs := Job.collectArray <| ← mods.mapM (fetch <| ·.facet moduleFacet)
  let exeJob ← extract_blueprint.fetch
  let buildDir := (← getRootPackage).buildDir
  let outputFile := buildDir / "blueprint" / "library" / lib.name.toString |>.addExtension ext
  exeJob.bindM fun exeFile => do
    moduleJobs.mapM fun _ => do
      buildFileUnlessUpToDate' outputFile do
        logInfo "Blueprint indexing"
        proc {
          cmd := exeFile.toString
          args := #["index", "--build", buildDir.toString, lib.name.toString, ",".intercalate (mods.map (·.name.toString)).toList] ++ extractArgs
          env := ← getAugmentedEnv
        }

/-- A facet to extract the blueprint for a library (with syntax highlighting). -/
library_facet blueprint (lib : LeanLib) : Unit := do
  buildLibraryBlueprint lib `blueprint "tex" #[]

/-- A facet to extract the blueprint for a library (without syntax highlighting). -/
library_facet blueprintPlain (lib : LeanLib) : Unit := do
  buildLibraryBlueprint lib `blueprintPlain "tex" #[]

/-- A facet to extract the JSON data for the blueprint for a library. -/
library_facet blueprintJson (lib : LeanLib) : Unit := do
  buildLibraryBlueprint lib `blueprintJson "json" #["--json"]

/-- A facet to extract the blueprint for a library with highlighting, falling back to plain per-module. -/
library_facet blueprintSafe (lib : LeanLib) : Unit := do
  buildLibraryBlueprint lib `blueprintSafe "tex" #[]

/-- A facet to extract the blueprint for each library in a package (with syntax highlighting). -/
package_facet blueprint (pkg : Package) : Unit := do
  let libJobs := Job.collectArray <| ← pkg.leanLibs.mapM (fetch <| ·.facet `blueprint)
  let _ ← libJobs.await
  return .nil

/-- A facet to extract the blueprint for each library in a package (without syntax highlighting). -/
package_facet blueprintPlain (pkg : Package) : Unit := do
  let libJobs := Job.collectArray <| ← pkg.leanLibs.mapM (fetch <| ·.facet `blueprintPlain)
  let _ ← libJobs.await
  return .nil

/-- A facet to extract the blueprint JSON data for each library in a package. -/
package_facet blueprintJson (pkg : Package) : Unit := do
  let libJobs := Job.collectArray <| ← pkg.leanLibs.mapM (fetch <| ·.facet `blueprintJson)
  let _ ← libJobs.await
  return .nil

/-- A facet to extract the blueprint for each library with highlighting, falling back to plain per-module. -/
package_facet blueprintSafe (pkg : Package) : Unit := do
  let libJobs := Job.collectArray <| ← pkg.leanLibs.mapM (fetch <| ·.facet `blueprintSafe)
  let _ ← libJobs.await
  return .nil

open IO.Process in
/-- Run a command, print all outputs, and throw an error if it fails. -/
private def runCmd (cmd : String) (args : Array String) : ScriptM Unit := do
  let child ← spawn { cmd, args, stdout := .inherit, stderr := .inherit, stdin := .null }
  let exitCode ← child.wait
  if exitCode != 0 then
    throw <| IO.userError s!"Error running command {cmd} {args.toList}"

/-- Build the project with dressed artifact generation enabled.

    This runs `lake build` with `-Dblueprint.dress=true`, which causes Hook.lean to
    automatically export dressed artifacts (highlighting, HTML, base64) for all
    `@[blueprint]` declarations.

    Usage: `lake run dress` or `lake run dress MyLib`

    The dressed artifacts are written to `.lake/build/dressed/{Module/Path}.json`. -/
script dress (args : List String) do
  let lake ← getLake
  let buildArgs := if args.isEmpty
    then #["-Dblueprint.dress=true"]
    else #["-Dblueprint.dress=true"] ++ args.toArray
  runCmd lake.toString (#["build"] ++ buildArgs)
  return 0

/-- A script to convert an existing blueprint to LeanArchitect format,
modifying the Lean and LaTeX source files in place. -/
script blueprintConvert (args : List String) do
  let architect ← Architect.get
  let convertScript := architect.srcDir / "scripts" / "convert" / "main.py"
  let libs := (← getRootPackage).leanLibs
  let rootMods := libs.flatMap (·.rootModules)
  if h : rootMods.size = 0 then
    IO.eprintln "No root modules found for any library"
    return 1
  else  -- this else is needed for rootMods[0] to work
  for lib in libs do
    runCmd (← getLake).toString #["build", lib.name.toString]
  let leanOptions := Lean.toJson (← getRootPackage).leanOptions |>.compress
  runCmd "python3" <|
    #[convertScript.toString] ++
    #["--libraries"] ++ libs.map (·.name.toString) ++
    #["--modules"] ++ rootMods.map (·.name.toString) ++
    #["--root_file", rootMods[0].leanFile.toString] ++
    #["--options", leanOptions] ++
    args
  return 0
