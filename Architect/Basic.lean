import Lean
import Batteries.Lean.NameMapAttribute


open Lean Elab

/-- Forward-port of lean4#12469: `Thunk α` is `Inhabited` when `α` is.
    Required by newer batteries (≥ v4.28.0) which wraps `NameMapExtension` state in a `Thunk`. -/
instance [Inhabited α] : Inhabited (Thunk α) := ⟨.pure default⟩

namespace Architect

initialize registerTraceClass `blueprint
initialize registerTraceClass `blueprint.debug
initialize registerTraceClass `blueprint.timing

/-- Status of a blueprint node for visualization.

    There are 6 possible statuses:
    - `notReady`: Default + Manual - not ready/not formalized (blueprint only, no Lean decl)
    - `ready`: Manual flag via `@[blueprint (ready)]` - ready to formalize
    - `sorry`: Derived - has `sorryAx` in proof
    - `proven`: Derived - formalized without sorry
    - `fullyProven`: Auto-computed - this node AND all ancestors are proven/fullyProven
    - `mathlibReady`: Manual flag via `@[blueprint (mathlibReady)]` - highest priority, ready for/in mathlib

    Status determination order (highest to lowest priority):
    1. `mathlibReady` - if manually set
    2. `fullyProven` - auto-computed if this + all deps proven
    3. `sorry` - if proof contains sorryAx
    4. `proven` - if formalized without sorry
    5. `ready` - if manually set
    6. `notReady` - default -/
inductive NodeStatus where
  | notReady     -- Default + Manual: not ready/not formalized
  | ready        -- Manual: ready to formalize
  | sorry        -- Derived: has sorryAx in proof
  | proven       -- Derived: formalized without sorry
  | fullyProven  -- Auto-computed: this + all ancestors proven/fullyProven
  | mathlibReady -- Manual: highest priority, ready for/in mathlib
  deriving Repr, Inhabited, BEq, DecidableEq

instance : Inhabited NodeStatus where
  default := .notReady

instance : ToJson NodeStatus where
  toJson
    | .notReady => "notReady"
    | .ready => "ready"
    | .sorry => "sorry"
    | .proven => "proven"
    | .fullyProven => "fullyProven"
    | .mathlibReady => "mathlibReady"

instance : FromJson NodeStatus where
  fromJson? json := do
    let s ← json.getStr?
    match s with
    | "notReady" => pure .notReady
    | "ready" => pure .ready
    | "sorry" => pure .sorry
    | "proven" => pure .proven
    | "fullyProven" => pure .fullyProven
    | "mathlibReady" => pure .mathlibReady
    -- Backwards compatibility: map old statuses to new ones
    | "stated" => pure .notReady
    | "inMathlib" => pure .mathlibReady
    | _ => throw s!"unknown NodeStatus: {s}"

instance : ToExpr NodeStatus where
  toTypeExpr := mkConst ``NodeStatus
  toExpr
    | .notReady => mkConst ``NodeStatus.notReady
    | .ready => mkConst ``NodeStatus.ready
    | .sorry => mkConst ``NodeStatus.sorry
    | .proven => mkConst ``NodeStatus.proven
    | .fullyProven => mkConst ``NodeStatus.fullyProven
    | .mathlibReady => mkConst ``NodeStatus.mathlibReady

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
  /-- LaTeX content placed above this node in the blueprint/paper. -/
  above : Option String := none
  /-- LaTeX content placed below this node in the blueprint/paper. -/
  below : Option String := none
  /-- The manually-set status of the node from the @[blueprint] attribute.
      This is the "input" status that may be overridden by derived statuses. -/
  status : NodeStatus := .notReady
  /-- Whether the status was explicitly set by the user (vs. being the default).
      When true, the status takes priority over auto-derived statuses. -/
  statusExplicit : Bool := false
  /-- A GitHub issue number where the surrounding definition or statement is discussed. -/
  discussion : Option Nat
  /-- The short title of the node in LaTeX. Also used as custom display name for dependency graph if set. -/
  title : Option String
  /-- Mark as a key declaration (highlighted in dashboard) -/
  keyDeclaration : Bool := false
  /-- User message/notes about this node -/
  message : Option String := none
  /-- Priority item for dashboard display -/
  priorityItem : Bool := false
  /-- Reason the node is blocked -/
  blocked : Option String := none
  /-- Known potential issues -/
  potentialIssue : Option String := none
  /-- Technical debt notes -/
  technicalDebt : Option String := none
  /-- Miscellaneous notes -/
  misc : Option String := none
deriving Inhabited, Repr, FromJson, ToJson

/-- Manual ToExpr instance for Node to ensure all fields including status are serialized.
    The derived instance has issues with fields that have default values. -/
instance : ToExpr Node where
  toTypeExpr := mkConst ``Node
  toExpr n := Lean.mkAppN (mkConst ``Node.mk) #[
    toExpr n.name,
    toExpr n.latexLabel,
    toExpr n.statement,
    toExpr n.proof,
    toExpr n.above,
    toExpr n.below,
    toExpr n.status,  -- Explicitly include status
    toExpr n.statusExplicit,
    toExpr n.discussion,
    toExpr n.title,
    toExpr n.keyDeclaration,
    toExpr n.message,
    toExpr n.priorityItem,
    toExpr n.blocked,
    toExpr n.potentialIssue,
    toExpr n.technicalDebt,
    toExpr n.misc
  ]

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
