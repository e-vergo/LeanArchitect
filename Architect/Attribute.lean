import Lean
import Architect.Basic

open Lean Meta Elab

namespace Architect

/-- `Config` is the type of arguments that can be provided to `blueprint`. -/
structure Config where
  /-- The statement of the node in text. -/
  statement : Option String := none
  /-- By default, only theorems have separate proof parts. This option overrides this behavior. -/
  hasProof : Option Bool := none
  /-- The proof of the node in text. Uses proof docstrings if not present. -/
  proof : Option String := none
  /-- The set of nodes that this node depends on. Infers from the constant if not present. -/
  uses : Array Name := #[]
  /-- The set of nodes to exclude from `uses`. -/
  excludes : Array Name := #[]
  /-- Additional LaTeX labels of nodes that this node depends on. -/
  usesLabels : Array String := #[]
  /-- The set of labels to exclude from `usesLabels`. -/
  excludesLabels : Array String := #[]
  /-- The set of nodes that the proof of this node depends on. Infers from the constant's value if not present. -/
  proofUses : Array Name := #[]
  /-- The set of nodes to exclude from `proofUses`. -/
  proofExcludes : Array Name := #[]
  /-- Additional LaTeX labels of nodes that the proof of this node depends on. -/
  proofUsesLabels : Array String := #[]
  /-- The set of labels to exclude from `proofUsesLabels`. -/
  proofExcludesLabels : Array String := #[]
  /-- The manually-set status of the node. Defaults to `.stated`. -/
  status : NodeStatus := .stated
  /-- A GitHub issue number where the surrounding definition or statement is discussed. -/
  discussion : Option Nat := none
  /-- The short title of the node in LaTeX. -/
  title : Option String := none
  /-- The LaTeX environment to use for the node. -/
  latexEnv : Option String := none
  /-- The LaTeX label to use for the node. -/
  latexLabel : Option String := none
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
  /-- Enable debugging. -/
  trace : Bool := false
deriving Repr

/-- Backwards compatibility accessor. -/
def Config.notReady (c : Config) : Bool := c.status == .notReady

syntax blueprintSingleUses := "-"? (ident <|> str)
syntax blueprintUses := "[" blueprintSingleUses,* "]"

/-- Returns array of (used names, excluded names, used labels, excluded labels). -/
def elabBlueprintUses : TSyntax ``blueprintUses →
    CoreM (Array Name × Array Name × Array String × Array String)
  | `(blueprintUses| [$[$usesStx:blueprintSingleUses],*]) => do
    let uses ← usesStx.filterMapM fun
      | `(blueprintSingleUses| $id:ident) => some <$> tryResolveConst id
      | _ => pure none
    let excludes ← usesStx.filterMapM fun
      | `(blueprintSingleUses| -$id:ident) => some <$> tryResolveConst id
      | _ => pure none
    let usesLabels := usesStx.filterMap fun
      | `(blueprintSingleUses| $str:str) => some str.getString
      | _ => none
    let excludesLabels := usesStx.filterMap fun
      | `(blueprintSingleUses| -$str:str) => some str.getString
      | _ => none
    return (uses, excludes, usesLabels, excludesLabels)
  | _ => throwUnsupportedSyntax

syntax blueprintStatementOption := &"statement" " := " plainDocComment
syntax blueprintHasProofOption := &"hasProof" " := " (&"true" <|> &"false")
syntax blueprintProofOption := &"proof" " := " plainDocComment
syntax blueprintUsesOption := &"uses" " := " blueprintUses
syntax blueprintProofUsesOption := &"proofUses" " := " blueprintUses
syntax blueprintTitleOption := &"title" " := " (plainDocComment <|> str)
-- Status options (only 3 manual flags: notReady, ready, mathlibReady)
-- fullyProven is auto-computed via graph traversal, inMathlib was removed
syntax blueprintNotReadyOption := &"notReady" " := " (&"true" <|> &"false")
syntax blueprintReadyOption := &"ready" " := " (&"true" <|> &"false")
syntax blueprintMathlibReadyOption := &"mathlibReady" " := " (&"true" <|> &"false")
syntax blueprintDiscussionOption := &"discussion" " := " num
syntax blueprintLatexEnvOption := &"latexEnv" " := " str
syntax blueprintLatexLabelOption := &"latexLabel" " := " str
-- Dashboard-related options
syntax blueprintKeyDeclarationOption := &"keyDeclaration" " := " (&"true" <|> &"false")
syntax blueprintMessageOption := &"message" " := " str
syntax blueprintPriorityItemOption := &"priorityItem" " := " (&"true" <|> &"false")
syntax blueprintBlockedOption := &"blocked" " := " str
syntax blueprintPotentialIssueOption := &"potentialIssue" " := " str
syntax blueprintTechnicalDebtOption := &"technicalDebt" " := " str
syntax blueprintMiscOption := &"misc" " := " str

syntax blueprintOption := "("
  blueprintStatementOption <|>
  blueprintHasProofOption <|> blueprintProofOption <|>
  blueprintUsesOption <|> blueprintProofUsesOption <|>
  blueprintTitleOption <|>
  blueprintNotReadyOption <|> blueprintReadyOption <|>
  blueprintMathlibReadyOption <|>
  blueprintDiscussionOption <|>
  blueprintLatexEnvOption <|> blueprintLatexLabelOption <|>
  blueprintKeyDeclarationOption <|> blueprintMessageOption <|>
  blueprintPriorityItemOption <|> blueprintBlockedOption <|>
  blueprintPotentialIssueOption <|> blueprintTechnicalDebtOption <|>
  blueprintMiscOption ")"
syntax blueprintOptions := (ppSpace str)? (ppSpace blueprintOption)*

/--
The `blueprint` attribute tags a constant to add to the blueprint.

You may optionally add:
- `"latex-label"`: The LaTeX label to use for the node (default: the Lean name).
- `statement := /-- ... -/`: The statement of the node in LaTeX.
- `hasProof := true`: If the node has a proof part (default: true if the node is a theorem).
- `proof := /-- ... -/`: The proof of the node in LaTeX (default: the docstrings in proof tactics).
- `uses := [a, "b"]`: The dependencies of the node, as Lean constants or LaTeX labels (default: inferred from the used constants).
- `proofUses := [a, "b"]`: The dependencies of the proof of the node, as Lean constants or LaTeX labels (default: inferred from the used constants).
- `title := /-- Title -/`: The title of the node in LaTeX.
- Status options (3 manual flags):
  - `notReady := true`: The node is not ready to formalize (needs more blueprint work).
  - `ready := true`: The node is ready to formalize.
  - `mathlibReady := true`: The node is ready to upstream to Mathlib.
  Note: `fullyProven` status is auto-computed via graph traversal (not a manual flag).
- `discussion := 123`: The discussion issue number of the node.
- `latexEnv := "lemma"`: The LaTeX environment to use for the node (default: "theorem" or "definition").
- Dashboard/metadata options:
  - `keyDeclaration := true`: Mark as a key declaration (highlighted in dashboard).
  - `message := "note"`: User message/notes about this node.
  - `priorityItem := true|false`: Priority item for dashboard display.
  - `blocked := "reason"`: Reason the node is blocked.
  - `potentialIssue := "description"`: Known potential issues.
  - `technicalDebt := "description"`: Technical debt notes.
  - `misc := "notes"`: Miscellaneous notes.

For more information, see [LeanArchitect](https://github.com/hanwenzhu/LeanArchitect).

Use `blueprint?` to show the raw data of the added node.
-/
syntax (name := blueprint) "blueprint" "?"? blueprintOptions : attr

@[inherit_doc blueprint]
macro "blueprint?" opts:blueprintOptions : attr => `(attr| blueprint ? $opts)

/-- Elaborates the configuration options for `blueprint`. -/
def elabBlueprintConfig : Syntax → CoreM Config
  | `(attr| blueprint%$_tk $[?%$trace?]? $[$label?:str]? $[$opts:blueprintOption]*) => do
    let mut config : Config := { trace := trace?.isSome }
    if let some latexLabel := label? then config := { config with latexLabel := latexLabel.getString }
    for stx in opts do
      match stx with
      | `(blueprintOption| (statement := $doc)) =>
        let statement := (← getDocStringText doc).trimAscii.copy
        config := { config with statement }
      | `(blueprintOption| (hasProof := true)) =>
        config := { config with hasProof := some true }
      | `(blueprintOption| (hasProof := false)) =>
        config := { config with hasProof := some false }
      | `(blueprintOption| (proof := $doc)) =>
        let proof := (← getDocStringText doc).trimAscii.copy
        config := { config with proof }
      | `(blueprintOption| (uses := $uses)) =>
        let (uses, excludes, usesLabels, excludesLabels) ← elabBlueprintUses uses
        config := { config with
          uses := config.uses ++ uses, excludes := config.excludes ++ excludes,
          usesLabels := config.usesLabels ++ usesLabels, excludesLabels := config.excludesLabels ++ excludesLabels }
      | `(blueprintOption| (proofUses := $uses)) =>
        let (uses, excludes, usesLabels, excludesLabels) ← elabBlueprintUses uses
        config := { config with
          proofUses := config.proofUses ++ uses, proofExcludes := config.proofExcludes ++ excludes,
          proofUsesLabels := config.proofUsesLabels ++ usesLabels, proofExcludesLabels := config.proofExcludesLabels ++ excludesLabels }
      | `(blueprintOption| (title := $str:str)) =>
        config := { config with title := str.getString }
      | `(blueprintOption| (title := $doc:docComment)) =>
        config := { config with title := (← getDocStringText doc).trimAscii.copy }
      | `(blueprintOption| (notReady := true)) =>
        config := { config with status := .notReady }
      | `(blueprintOption| (notReady := false)) =>
        pure () -- no-op, stays at default .stated
      | `(blueprintOption| (ready := true)) =>
        config := { config with status := .ready }
      | `(blueprintOption| (ready := false)) =>
        pure () -- no-op
      | `(blueprintOption| (mathlibReady := true)) =>
        config := { config with status := .mathlibReady }
      | `(blueprintOption| (mathlibReady := false)) =>
        pure () -- no-op
      | `(blueprintOption| (discussion := $n)) =>
        config := { config with discussion := n.getNat }
      | `(blueprintOption| (latexEnv := $str)) =>
        config := { config with latexEnv := str.getString }
      | `(blueprintOption| (latexLabel := $str)) =>
        config := { config with latexLabel := str.getString }
      | `(blueprintOption| (keyDeclaration := true)) =>
        config := { config with keyDeclaration := true }
      | `(blueprintOption| (keyDeclaration := false)) =>
        config := { config with keyDeclaration := false }
      | `(blueprintOption| (message := $s:str)) =>
        config := { config with message := some s.getString }
      | `(blueprintOption| (priorityItem := true)) =>
        config := { config with priorityItem := true }
      | `(blueprintOption| (priorityItem := false)) =>
        config := { config with priorityItem := false }
      | `(blueprintOption| (blocked := $s:str)) =>
        config := { config with blocked := some s.getString }
      | `(blueprintOption| (potentialIssue := $s:str)) =>
        config := { config with potentialIssue := some s.getString }
      | `(blueprintOption| (technicalDebt := $s:str)) =>
        config := { config with technicalDebt := some s.getString }
      | `(blueprintOption| (misc := $s:str)) =>
        config := { config with misc := some s.getString }
      | _ => throwUnsupportedSyntax
    return config
  | _ => throwUnsupportedSyntax

/-- Whether a node has a proof part. -/
def hasProof (name : Name) (cfg : Config) : CoreM Bool := do
  return cfg.hasProof.getD (cfg.proof.isSome || wasOriginallyTheorem (← getEnv) name)

def mkStatementPart (_name : Name) (cfg : Config) (hasProof : Bool) : CoreM NodePart := do
  return {
    text := cfg.statement.getD "",
    uses := cfg.uses, excludes := cfg.excludes,
    usesLabels := cfg.usesLabels, excludesLabels := cfg.excludesLabels,
    latexEnv := cfg.latexEnv.getD (if hasProof then "theorem" else "definition")
  }

def mkProofPart (_name : Name) (cfg : Config) : CoreM NodePart := do
  return {
    text := cfg.proof.getD "",
    uses := cfg.proofUses, excludes := cfg.proofExcludes,
    usesLabels := cfg.proofUsesLabels, excludesLabels := cfg.proofExcludesLabels,
    latexEnv := "proof"
  }

def mkNode (name : Name) (cfg : Config) : CoreM Node := do
  trace[blueprint.debug] "mkNode {.ofConstName name} {repr cfg}"
  let latexLabel := cfg.latexLabel.getD name.toString
  if ← hasProof name cfg then
    let statement ← mkStatementPart name cfg true
    let proof ← mkProofPart name cfg
    return { name, latexLabel, statement, proof, status := cfg.status, discussion := cfg.discussion,
             title := cfg.title,
             keyDeclaration := cfg.keyDeclaration, message := cfg.message, priorityItem := cfg.priorityItem,
             blocked := cfg.blocked, potentialIssue := cfg.potentialIssue,
             technicalDebt := cfg.technicalDebt, misc := cfg.misc }
  else
    let statement ← mkStatementPart name cfg false
    return { name, latexLabel, statement, proof := none, status := cfg.status, discussion := cfg.discussion,
             title := cfg.title,
             keyDeclaration := cfg.keyDeclaration, message := cfg.message, priorityItem := cfg.priorityItem,
             blocked := cfg.blocked, potentialIssue := cfg.potentialIssue,
             technicalDebt := cfg.technicalDebt, misc := cfg.misc }

-- register_option blueprint.checkCyclicUses : Bool := {
--   defValue := true,
--   descr := "Whether to check for cyclic dependencies in the blueprint."
-- }

-- TODO: remove
-- /--
-- Raises an error if `newLabel` occurs in the (irreflexive transitive) dependencies of `label`.
-- If ignored, this would create a cycle and then an error during `leanblueprint web`.
-- -/
-- partial def checkCyclicUses {m} [Monad m] [MonadEnv m] [MonadError m]
--     (newLabel : String) (label : String)
--     (visited : Std.HashSet String := ∅) (path : Array String := #[]) : m Unit := do
--   let path' := path.push label
--   if visited.contains label then
--     if path.contains label then
--       throwError "cyclic dependency in blueprint:\n  {" uses ".intercalate (path'.toList.map toString)}"
--     else
--       return
--   let visited' := visited.insert label

--   for name in getLeanNamesOfLatexLabel (← getEnv) label do
--     if let some node := blueprintExt.find? (← getEnv) name then
--       for used in node.statement.uses ++ (node.proof.map (·.uses) |>.getD #[]) do
--         checkCyclicUses newLabel used visited' path'
--     else
--       throwError "unknown constant {name} in blueprint"

initialize registerBuiltinAttribute {
    name := `blueprint
    descr := "Adds a node to the blueprint"
    applicationTime := .afterCompilation
    add := fun name stx kind => do
      unless kind == AttributeKind.global do throwError "invalid attribute 'blueprint', must be global"
      let cfg ← elabBlueprintConfig stx
      withOptions (·.updateBool `trace.blueprint (cfg.trace || ·)) do

      let node ← mkNode name cfg
      blueprintExt.add name node
      modifyEnv fun env => addLeanNameOfLatexLabel env node.latexLabel name
      trace[blueprint] "Blueprint node added:\n{repr node}"

      -- pushInfoLeaf <| .ofTermInfo {
      --   elaborator := .anonymous, lctx := {}, expectedType? := none,
      --   stx, expr := toExpr node }
  }

end Architect
