import Lean
import Batteries.Lean.NameMapAttribute
import Architect.Basic


open Lean Elab Tactic Meta

namespace Architect

namespace ProofDocString

/-! Here we implement docstrings but for proofs. -/

/-- The environment extension that stores the proof docstrings.
Note that this is not persistent, so the data is not available to other modules. -/
initialize proofDocStringExt : EnvExtension (NameMap (Array String)) ←
  registerEnvExtension (pure ∅) (asyncMode := .async .asyncEnv)

end ProofDocString

open ProofDocString

def addProofDocString (env : Environment) (name : Name) (doc : String) : Environment :=
  proofDocStringExt.modifyState (asyncDecl := name) env fun s =>
    match s.find? name with
    | none => s.insert name #[doc]
    | some es => s.insert name (es.push doc)

def getProofDocString (env : Environment) (name : Name) : String :=
  "\n\n".intercalate <|
    proofDocStringExt.getState (asyncDecl := name) env |>.find? name |>.getD #[] |>.toList

elab (name := tacticDocComment) docComment:plainDocComment t:tactic : tactic => do
  let some name ← Term.getDeclName? | throwError "could not get declaration name"
  let doc := (← getDocStringText ⟨docComment⟩).trimAscii.copy
  modifyEnv fun env => addProofDocString env name doc
  -- NOTE: an alternative approach is to remove `t:tactic` and `evalTactic t`.
  -- This would also work for our purpose, but we require a following `t:tactic` and then immediately
  -- evaluate it because this would avoid the unusedTactic linter in Mathlib to flag the docComment
  -- (and we do not currently import Mathlib and hence cannot modify to ignore `tacticDocComment`).
  evalTactic t

/-! We implement the `blueprint_using` and `sorry_using` tactics that declares used constants. -/

-- **TODO**: support `sorry_using ["label"]` (which should accumulate to an environment extension similar to `proofDocStringExt`).

/--
`blueprint_using [a, b]` adds `a` and `b` as dependencies for the blueprint metadata.

It is basically the same as `let := a; let := b`.
-/
elab "blueprint_using" " [" ids:ident,* "]" : tactic => do
  for id in ids.getElems do
    let used ← realizeGlobalConstNoOverloadWithInfo id
    let info ← getConstInfo used
    -- Instantiate universe level parameters with 0, to avoid errors
    let lvls := List.replicate info.numLevelParams levelZero
    let ty := info.instantiateTypeLevelParams lvls
    let const := mkConst used lvls
    liftMetaTactic1 fun g => do
      let g' ← g.define (← mkFreshBinderNameForTactic `blueprint_using) ty const
      let (_, g'') ← g'.intro1P
      return g''

/--
`sorry_using [a, b]` is the same as `sorry`, but adds `a` and `b` as dependencies for the blueprint metadata.

It is basically similar to `let := a; let := b; sorry`.
-/
elab (name := tacticSorryUsing) "sorry_using" " [" ids:ident,* "]" : tactic => do
  evalTactic (← `(tactic| blueprint_using [$[$ids],*]))
  liftMetaTactic1 fun g => do
    let mut g := g
    -- We touch every local hypothesis to avoid unused variable linter
    -- This is not an elegant solution, but it works
    for h in ← getLocalHyps do
      g ← g.define (← mkFreshBinderNameForTactic `sorry_using) (← inferType h) h
      (_, g) ← g.intro1P
    -- A non-synthetic sorry because the `sorry_using` tactic is explicitly written by the user
    g.admit (synthetic := false)
    return g

@[inherit_doc tacticSorryUsing]
macro (name := termSorryUsing) "sorry_using" " [" ids:ident,* "]" : term =>
  `(term| by sorry_using [$[$ids],*])

end Architect
