/-
Copyright (c) 2025 LeanArchitect contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Architect.Basic
import SubVerso.Highlighting
import Batteries.Data.String.Matcher

/-!
# Highlighted Code Capture for Blueprint Declarations

This module provides a linter that captures SubVerso highlighted code for declarations
tagged with `@[blueprint]`. The highlighted code is stored in an environment extension
and can be retrieved later when generating documentation.

The highlighting is captured during the linting phase when info trees are still
available, then stored for later retrieval.
-/

open Lean Elab Command Term Meta Linter

namespace Architect

/-- Option to enable/disable the blueprint highlighting linter. -/
register_option linter.blueprintHighlighting : Bool := {
  defValue := true
  descr := "Enables the blueprint highlighting linter that captures SubVerso highlighted code."
}

/-- Gets the value of the `linter.blueprintHighlighting` option. -/
private def getLinterBlueprintHighlighting (o : LinterOptions) : Bool :=
  getLinterValue linter.blueprintHighlighting o

/-- Get names suppressed for highlighting (similar to SubVerso's approach). -/
private def getSuppressedNamespaces : CoreM (List Name) := do
  -- Could be made configurable via an option
  return []

/--
Finds all syntax nodes matching a predicate.
-/
private partial def findAllSyntax (stx : Syntax) (p : Syntax → Bool) : Array Syntax :=
  let found := if p stx then #[stx] else #[]
  match stx with
  | .node _ _ args => found ++ args.flatMap (findAllSyntax · p)
  | _ => found

/--
Extracts the declaration name from a declId syntax.
-/
private def getDeclName? (stx : Syntax) : Option Ident :=
  if stx.getKind == ``Lean.Parser.Command.declId then
    let id := stx[0]
    if id.isIdent then some ⟨id⟩ else none
  else if stx.isIdent then
    some ⟨stx⟩
  else
    none

/--
Check if a syntax tree contains a `@[blueprint]` attribute.
This checks the syntax directly rather than the environment extension,
which is necessary because the `@[blueprint]` attribute has
`applicationTime := .afterCompilation` and isn't available during elaboration.
-/
private def hasBlueprintAttribute (stx : Syntax) : Bool :=
  -- Find all declModifiers in the syntax
  let modifiers := findAllSyntax stx fun s =>
    s.getKind == ``Lean.Parser.Command.declModifiers
  -- Check if any modifier contains "blueprint" in its text representation
  modifiers.any fun mods =>
    let str := mods.reprint.getD ""
    str.containsSubstr "blueprint"

/--
Process a single declaration syntax, extracting highlighted code if it's in the blueprint.
-/
private def processDeclaration (declStx : Syntax) : CommandElabM Unit := do
  -- Check if this declaration has the @[blueprint] attribute in its syntax.
  -- We check the syntax directly rather than the extension because the attribute
  -- has applicationTime := .afterCompilation, so the extension isn't populated yet.
  unless hasBlueprintAttribute declStx do return

  -- Try to extract the declaration name from the syntax
  let declIds := findAllSyntax declStx fun s =>
    s.getKind == ``Lean.Parser.Command.declId

  for declIdStx in declIds.toList do
    let some nameId := getDeclName? declIdStx | continue

    -- Resolve the name
    let name ← try
      liftCoreM <| realizeGlobalConstNoOverloadWithInfo nameId
    catch _ =>
      continue  -- Name resolution failed, skip

    let env ← getEnv

    -- Check if we already have highlighted code for this declaration
    if getHighlightedCode? env name |>.isSome then continue

    -- Get info trees while they're still available
    let trees ← getInfoTrees
    let msgs := (← get).messages.toArray
    let suppressedNS ← liftCoreM getSuppressedNamespaces

    -- Generate highlighted code using SubVerso
    try
      let hl ← liftTermElabM <| SubVerso.Highlighting.highlight declStx msgs trees suppressedNS
      -- Store the highlighted code in the extension
      liftCoreM <| addHighlightedCode name hl
      trace[blueprint.debug] "Captured highlighted code for {name}"
    catch e =>
      trace[blueprint.debug] "Failed to capture highlighting for {name}: {e.toMessageData}"

/--
A linter that captures SubVerso highlighted code for blueprint declarations.

This linter runs after each declaration is elaborated. If the declaration is
tagged with `@[blueprint]`, it captures the highlighted code and stores it
in the `highlightedCodeExt` environment extension.
-/
def blueprintHighlightingLinter : Linter where
  run := withSetOptionIn fun stx => do
    -- Skip if linting is disabled
    unless getLinterBlueprintHighlighting (← getLinterOptions) do return

    -- Find declarations in the syntax
    let decls := findAllSyntax stx fun s =>
      s.getKind == ``Lean.Parser.Command.declaration ||
      s.getKind == ``Lean.Parser.Command.theorem ||
      s.getKind == ``Lean.Parser.Command.definition ||
      s.getKind == ``Lean.Parser.Command.abbrev

    for declStx in decls.toList do
      processDeclaration declStx

  name := `blueprintHighlighting

/-- Register the blueprint highlighting linter. -/
initialize addLinter blueprintHighlightingLinter

end Architect
