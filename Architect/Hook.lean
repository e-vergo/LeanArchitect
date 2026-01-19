/-
Copyright (c) 2025 LeanArchitect contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Architect.Highlighting
import Architect.HookState

/-!
# Elaboration Hook for Blueprint Declarations

This module provides a mechanism to automatically capture SubVerso highlighted code
when a declaration with `@[blueprint]` attribute is elaborated.

## The Challenge

The `@[blueprint]` attribute handler runs at `.afterCompilation` when info trees are gone.
We need to capture highlighting DURING command elaboration when info trees exist.

## Solution

We use `elab_rules` with higher priority to intercept declarations that have `@[blueprint]`.
After standard elaboration completes (but while info trees still exist), we capture
the highlighting and store it in the environment extension.

The key insight is that `elab_rules` run before the standard elaborators, and we can
call the standard elaborator explicitly, then capture highlighting afterward.
-/

open Lean Elab Command Meta Parser

namespace Architect

/-- Check if a syntax contains an attribute with the given name (iterative to avoid termination issues). -/
partial def hasAttrNamed (attrName : Name) (stx : Syntax) : Bool :=
  -- Use a worklist to avoid recursion issues
  let rec go (worklist : List Syntax) : Bool :=
    match worklist with
    | [] => false
    | s :: rest =>
      match s with
      | .node _ kind args =>
        if kind == ``Lean.Parser.Term.attrInstance then
          -- Check if any arg is or contains the attribute name
          if args.any (fun arg => arg.getId == attrName) then
            true
          else
            go (args.toList ++ rest)
        else
          go (args.toList ++ rest)
      | .ident _ _ id _ => id == attrName || go rest
      | _ => go rest
  go [stx]

/-- Check if a command syntax has `@[blueprint ...]` attribute in its declModifiers. -/
def hasBlueprintAttr (stx : Syntax) : Bool :=
  hasAttrNamed `blueprint stx

/-- Extract declaration name from a declId syntax node. -/
def getDeclNameFromDeclId (declId : Syntax) : Option Name :=
  -- declId is: ident (optional universe params)
  if declId.getKind == ``Lean.Parser.Command.declId then
    declId[0]?.map (·.getId)
  else if declId.isIdent then
    some declId.getId
  else
    none

/-- Check if we're currently inside the capture hook. -/
def inCaptureHook : CommandElabM Bool := do
  blueprintCaptureHookRef.get

/-- Run an action with the capture hook flag set. -/
def withCaptureHookFlag (act : CommandElabM α) : CommandElabM α := do
  blueprintCaptureHookRef.set true
  try
    act
  finally
    blueprintCaptureHookRef.set false

/-- Elaborate a declaration command and capture highlighting for blueprint declarations.
    This helper is called by the elab_rules below. -/
def elabDeclAndCaptureHighlighting (stx : Syntax) (declId : Syntax) : CommandElabM Unit := do
  -- Run standard command elaboration with the flag set to prevent recursion
  withCaptureHookFlag do
    elabCommandTopLevel stx

  -- Now capture highlighting while info trees are available
  if let some name := getDeclNameFromDeclId declId then
    -- Resolve the name with current namespace
    let ns ← getCurrNamespace
    let fullName := if ns.isAnonymous then name else ns ++ name
    let env ← getEnv
    let resolvedName := if env.contains fullName then fullName else name
    if env.contains resolvedName then
      -- Capture highlighting using the combined info trees
      captureHighlightingFromCommandState resolvedName stx

/-- Elaboration rules for declarations with @[blueprint] attribute.
    These intercept declarations and capture highlighting after elaboration.

    We use scoped rules so they only apply when Architect is imported.
    The rules check the `inCaptureHook` flag to prevent infinite recursion.
-/

-- Theorem declarations with @[blueprint]
scoped elab_rules : command
  | `($mods:declModifiers theorem $declId:declId $_sig:declSig $_val:declVal) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Definition declarations with @[blueprint]
scoped elab_rules : command
  | `($mods:declModifiers def $declId:declId $_sig:optDeclSig $_val:declVal) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Abbreviation declarations with @[blueprint]
scoped elab_rules : command
  | `($mods:declModifiers abbrev $declId:declId $_sig:optDeclSig $_val:declVal) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Structure declarations with @[blueprint]
scoped elab_rules : command
  | `($mods:declModifiers structure $declId:declId $_*) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Class declarations with @[blueprint]
scoped elab_rules : command
  | `($mods:declModifiers class $declId:declId $_*) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Inductive declarations with @[blueprint]
scoped elab_rules : command
  | `($mods:declModifiers inductive $declId:declId $_*) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

-- Instance declarations with @[blueprint]
-- Instance syntax: declModifiers attrKind "instance" optNamedPrio? declId? declSig declVal
-- We use a broader match pattern since instance syntax is complex
scoped elab_rules : command
  | `($mods:declModifiers instance $[$_prio:namedPrio]? $declId:declId $_sig:declSig $_val:declVal) => do
    if (← inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId
    else
      throwUnsupportedSyntax

end Architect
