import Lean
import Batteries.Lean.NameMapAttribute


open Lean Elab

namespace Architect

initialize registerTraceClass `blueprint
initialize registerTraceClass `blueprint.debug

/-- Status of a blueprint node for visualization.

    There are 8 possible statuses, determined as follows:
    - `notReady`: Manual flag via `@[blueprint (notReady)]`
    - `stated`: Default (in blueprint but no Lean decl yet, or no manual status)
    - `ready`: Manual flag via `@[blueprint (ready)]`
    - `sorry`: Derived - has `sorryAx` in proof
    - `proven`: Derived - formalized without sorry
    - `fullyProven`: Auto-computed from graph (this + all deps proven)
    - `mathlibReady`: Manual flag via `@[blueprint (mathlibReady)]`
    - `inMathlib`: Derived (module prefix) OR manual override

    The `sorry` and `fullyProven` statuses only apply to theorems/lemmas (ellipse shape),
    not definitions (box shape). -/
inductive NodeStatus where
  | notReady     -- Manual: not ready to formalize
  | stated       -- Default: statement exists in blueprint
  | ready        -- Manual: ready to formalize
  | sorry        -- Derived: has sorryAx in proof
  | proven       -- Derived: formalized without sorry
  | fullyProven  -- Auto-computed: this + all ancestors proven
  | mathlibReady -- Manual: ready to upstream to Mathlib
  | inMathlib    -- Derived or manual: already in Mathlib
  deriving Repr, Inhabited, BEq, DecidableEq

instance : Inhabited NodeStatus where
  default := .stated

instance : ToJson NodeStatus where
  toJson
    | .notReady => "notReady"
    | .stated => "stated"
    | .ready => "ready"
    | .sorry => "sorry"
    | .proven => "proven"
    | .fullyProven => "fullyProven"
    | .mathlibReady => "mathlibReady"
    | .inMathlib => "inMathlib"

instance : FromJson NodeStatus where
  fromJson? json := do
    let s ← json.getStr?
    match s with
    | "notReady" => pure .notReady
    | "stated" => pure .stated
    | "ready" => pure .ready
    | "sorry" => pure .sorry
    | "proven" => pure .proven
    | "fullyProven" => pure .fullyProven
    | "mathlibReady" => pure .mathlibReady
    | "inMathlib" => pure .inMathlib
    | _ => throw s!"unknown NodeStatus: {s}"

instance : ToExpr NodeStatus where
  toTypeExpr := mkConst ``NodeStatus
  toExpr
    | .notReady => mkConst ``NodeStatus.notReady
    | .stated => mkConst ``NodeStatus.stated
    | .ready => mkConst ``NodeStatus.ready
    | .sorry => mkConst ``NodeStatus.sorry
    | .proven => mkConst ``NodeStatus.proven
    | .fullyProven => mkConst ``NodeStatus.fullyProven
    | .mathlibReady => mkConst ``NodeStatus.mathlibReady
    | .inMathlib => mkConst ``NodeStatus.inMathlib

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
  /-- The manually-set status of the node from the @[blueprint] attribute.
      This is the "input" status that may be overridden by derived statuses. -/
  status : NodeStatus := .stated
  /-- A GitHub issue number where the surrounding definition or statement is discussed. -/
  discussion : Option Nat
  /-- The short title of the node in LaTeX. -/
  title : Option String
deriving Inhabited, Repr, FromJson, ToJson, ToExpr

/-- Backwards compatibility: check if a node is marked as not ready. -/
def Node.notReady (n : Node) : Bool := n.status == .notReady

structure NodeWithPos extends Node where
  /--
  Whether the node name is in the environment.
  This should always be true for nodes e.g. added by `@[blueprint]`.
  -/
  hasLean : Bool
  /-- The location (module & range) the node is defined in. -/
  location : Option DeclarationLocation
  /-- The file the node is defined in. -/
  file : Option System.FilePath
deriving Inhabited, Repr

def Node.toNodeWithPos (node : Node) : CoreM NodeWithPos := do
  let env ← getEnv
  if !env.contains node.name then
    return { node with hasLean := false, location := none, file := none }
  let module := match env.getModuleIdxFor? node.name with
    | some modIdx => env.allImportedModuleNames[modIdx]!
    | none => env.header.mainModule
  let location := match ← findDeclarationRanges? node.name with
    | some ranges => some { module, range := ranges.range }
    | none => none
  let file ← (← getSrcSearchPath).findWithExt "lean" module
  return { node with hasLean := true, location, file }

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
