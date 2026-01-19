import Architect
import Lean
import Cli

/-!
This executable extracts the blueprint data from a module, or
collates the blueprint data from multiple modules into a LaTeX file.
-/

open Lean Cli Architect

def outputBaseDir (buildDir : System.FilePath) : System.FilePath :=
  buildDir / "blueprint"

def runSingleCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let baseDir := outputBaseDir buildDir
  let module := p.positionalArg! "module" |>.as! String |>.toName
  let isJson := p.hasFlag "json"
  -- Get path to pre-computed highlighted JSON from Lake facet (if provided)
  let highlightedJsonPath := p.flag? "highlightedJson" |>.map (·.as! String)
  let options : LeanOptions ← match p.flag? "options" with
    | some o => IO.ofExcept (Json.parse (o.as! String) >>= fromJson?)
    | none => pure (∅ : LeanOptions)

  if isJson then
    let json ← jsonOfImportModule module options.toOptions highlightedJsonPath
    outputJsonResults baseDir module json
  else
    let latexOutput ← latexOutputOfImportModule module options.toOptions highlightedJsonPath
    discard <| outputLatexResults baseDir module latexOutput
  return 0

def runIndexCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let baseDir := outputBaseDir buildDir
  let library := p.positionalArg! "library" |>.as! String |>.toName
  let modules := p.positionalArg! "modules" |>.as! (Array String) |>.map (·.toName)
  let isJson := p.hasFlag "json"
  if isJson then
    outputLibraryJson baseDir library modules
  else
    outputLibraryLatex baseDir library modules
  return 0

def singleCmd := `[Cli|
  single VIA runSingleCmd;
  "Only extract the blueprint for the module it was given, might contain broken \\input{}s unless all blueprint files are extracted."

  FLAGS:
    j, json; "Output JSON instead of LaTeX."
    h, highlight; "No-op (kept for backward compatibility)."
    b, build : String; "Build directory."
    o, options : String; "LeanOptions in JSON to pass to running the module."
    highlightedJson : String; "Path to pre-computed highlighted JSON from Lake facet."

  ARGS:
    module : String; "The module to extract the blueprint for."
]

def indexCmd := `[Cli|
  index VIA runIndexCmd;
  "Collates the LaTeX outputs of modules in a library from `single` into a LaTeX file with \\input{}s pointing to the modules."

  FLAGS:
    j, json; "Output JSON instead of LaTeX."
    b, build : String; "Build directory."

  ARGS:
    library : String; "The library to index."
    modules : Array String; "The modules in the library."
]

def blueprintCmd : Cmd := `[Cli|
  "LeanArchitect" NOOP;
  "A blueprint generator for Lean 4."

  SUBCOMMANDS:
    singleCmd;
    indexCmd
]

def main (args : List String) : IO UInt32 :=
  blueprintCmd.validate args
