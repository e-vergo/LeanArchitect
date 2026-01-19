# Hook-Based Highlighting Architecture Analysis

## Overview

This document analyzes the hook-based approach for capturing SubVerso highlighting during elaboration, implemented in commit `a04919a` and subsequently reverted in commit `5d80bd1`.

## Files Analyzed

1. `Architect/Hook.lean` - Elaboration interception rules
2. `Architect/HookState.lean` - Recursion prevention state
3. `Architect/Highlighting.lean` - Core highlighting capture logic

---

## Architecture

### Core Concept

The hook approach intercepted declarations during elaboration using `scoped elab_rules` with the goal of capturing SubVerso highlighting while info trees were still available in the command state. This was motivated by the fact that info trees are discarded after command elaboration completes.

### Component 1: HookState.lean

```lean
initialize blueprintCaptureHookRef : IO.Ref Bool <- IO.mkRef false
```

**Purpose:** Prevent infinite recursion when the hook's `elab_rules` call `elabCommandTopLevel`, which would otherwise re-trigger the same hook.

**Design Decision:** Using `IO.Ref` rather than thread-local state or a command state extension. This was placed in a separate file to avoid the "cannot evaluate init in same module" error that occurs when `initialize` declarations are used in the same module that references them.

### Component 2: Hook.lean

The hook implemented `scoped elab_rules` for multiple declaration types:

```lean
-- Theorem declarations with @[blueprint]
scoped elab_rules : command
  | `($mods:declModifiers theorem $declId:declId $_sig:declSig $_val:declVal) => do
    if (<- inCaptureHook) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (<- getRef) declId
    else
      throwUnsupportedSyntax
```

Similar rules existed for: `def`, `abbrev`, `structure`, `class`, `inductive`, `instance`.

**Key functions:**

1. `hasAttrNamed` / `hasBlueprintAttr` - Syntax inspection to detect `@[blueprint]` attribute
2. `inCaptureHook` - Check recursion flag
3. `withCaptureHookFlag` - Set flag during elaboration to prevent recursion
4. `elabDeclAndCaptureHighlighting` - Core logic:
   ```lean
   def elabDeclAndCaptureHighlighting (stx : Syntax) (declId : Syntax) : CommandElabM Unit := do
     withCaptureHookFlag do
       elabCommandTopLevel stx
       -- Capture highlighting immediately after elaboration
       if let some name := getDeclNameFromDeclId declId then
         let ns <- getCurrNamespace
         let fullName := if ns.isAnonymous then name else ns ++ name
         let env <- getEnv
         let resolvedName := if env.contains fullName then fullName else name
         if env.contains resolvedName then
           captureHighlightingFromCommandState resolvedName stx
   ```

### Component 3: Highlighting.lean Addition

```lean
def captureHighlightingFromCommandState (name : Name) (stx : Syntax) : CommandElabM Unit := do
  try
    let trees := (<- get).infoState.trees
    let messages := (<- get).messages.toArray.filter (!*.isSilent)
    let suppressedNS : List Name := []
    let hl <- liftTermElabM <| highlightIncludingUnparsed stx messages trees suppressedNS
    modifyEnv fun env => highlightedCodeExt.addEntry env (name, hl)
  catch e =>
    logWarning m!"Failed to capture highlighting for {name}: {e.toMessageData}"
```

---

## Identified Issues

### Issue 1: Module Initialization Order

**Problem:** The `IO.Ref` in `HookState.lean` is initialized when the module is loaded. If `Hook.lean` imports `HookState.lean`, and `Hook.lean` is imported by a file that also transitively imports `HookState.lean` through a different path, there can be initialization ordering issues.

**Symptom:** "Cannot evaluate init in same module" errors or inconsistent state behavior.

**Assessment:** Partially mitigated by putting `initialize` in a separate file, but fundamental initialization order issues in Lean's module system remain.

### Issue 2: Recursion Prevention Complexity

**Problem:** The `IO.Ref Bool` approach is fragile:
- It's global mutable state shared across all elaboration contexts
- If an exception occurs inside `withCaptureHookFlag`, the `finally` block resets the flag, but nested elaboration might have unexpected interactions
- The flag doesn't account for parallel elaboration scenarios

**Symptom:** Potential for infinite recursion if the flag isn't properly set, or missed captures if it's incorrectly left set.

**Assessment:** Fixable implementation bug. A more robust approach would use a stack-based counter or thread-local storage.

### Issue 3: Scoped Elaboration Rule Priority

**Problem:** `scoped elab_rules` compete with the standard declaration elaborators. The hook must either:
1. Successfully match and handle the syntax, OR
2. Call `throwUnsupportedSyntax` to delegate

When the blueprint attribute is NOT present, the hook throws `throwUnsupportedSyntax`, which is correct. But the priority ordering between these scoped rules and Lean's built-in elaborators isn't guaranteed to be stable across Lean versions.

**Symptom:** Declarations might be elaborated by the wrong elaborator, or elaboration might fail entirely if priority ordering changes.

**Assessment:** Fundamental design concern. Lean's elaboration priority system is not designed for this kind of interception pattern.

### Issue 4: Info Tree Availability Timing

**Problem:** The code captures highlighting inside `withCaptureHookFlag` after `elabCommandTopLevel` returns. However, the exact state of info trees at this point depends on internal Lean elaboration behavior. The info trees might:
- Be cleared by subsequent elaboration phases
- Not contain complete semantic information if elaboration is still in progress
- Contain duplicate or incomplete entries

**Symptom:** Empty or incomplete highlighting, or SubVerso crashes on malformed info trees.

**Assessment:** Timing-dependent behavior that's difficult to diagnose and fix without deep Lean internals knowledge.

### Issue 5: Attribute Detection via Syntax Inspection

**Problem:** The `hasAttrNamed` function does syntax-level inspection to detect `@[blueprint]`:

```lean
partial def hasAttrNamed (attrName : Name) (stx : Syntax) : Bool :=
  let rec go (worklist : List Syntax) : Bool :=
    match worklist with
    | [] => false
    | s :: rest =>
      match s with
      | .node _ kind args =>
        if kind == ``Lean.Parser.Term.attrInstance then
          if args.any (fun arg => arg.getId == attrName) then true
          else go (args.toList ++ rest)
        else go (args.toList ++ rest)
      | .ident _ _ id _ => id == attrName || go rest
      | _ => go rest
  go [stx]
```

This is fragile because:
- Syntax structure can change between Lean versions
- Complex attribute syntax (e.g., `@[blueprint "label"]`) might not be detected correctly
- The check happens before elaboration, so macros haven't expanded yet

**Assessment:** Fixable implementation bug. A better approach would query the environment for the attribute after elaboration.

### Issue 6: Name Resolution Edge Cases

**Problem:** The code attempts to resolve declaration names:

```lean
let ns <- getCurrNamespace
let fullName := if ns.isAnonymous then name else ns ++ name
let env <- getEnv
let resolvedName := if env.contains fullName then fullName else name
```

This doesn't handle:
- Private declarations
- Protected declarations
- Declarations in nested namespaces opened with `open`
- Ambiguous names

**Assessment:** Fixable implementation bug. Should use the environment's name resolution APIs.

---

## Why the Approach Was Abandoned

Based on the git history, commit `5d80bd1` ("replicate lean docs architecture") replaced the hook approach with `SubVersoExtract.lean`, which uses the external `subverso-extract-mod` CLI tool.

The replacement approach:
1. Runs `lake exe subverso-extract-mod <module>` as a subprocess
2. Parses the JSON output containing pre-computed highlighting
3. Builds a `NameMap` from declaration names to highlighted code

**Advantages of CLI approach:**
- No need to intercept elaboration
- Highlighting computed by SubVerso's maintained tooling
- Works post-build when all information is available
- No recursion or timing issues

**Disadvantages:**
- Requires external tool invocation
- Slower (subprocess overhead)
- Highlighting not available during elaboration
- Depends on build system integration

---

## Recommendations for New Implementation

### If Pursuing Hook-Based Approach

1. **Use `registerBuiltinAttribute`** instead of syntax inspection
   - Register a callback that fires when `@[blueprint]` is applied
   - The callback runs at a well-defined point with access to the declaration info

2. **Store highlighting in a separate extension**
   - Don't modify the environment during elaboration
   - Use `IO.Ref (NameMap Highlighted)` and flush to extension at module end

3. **Handle recursion via elaboration context**
   - Pass a flag through the elaboration context rather than global state
   - Or use `withReader` to add a marker to the context

4. **Defer highlighting capture**
   - Use `modifyGetEnv` with a callback that runs after full elaboration
   - Or hook into `afterCompilation` phase with saved info tree reference

5. **Test extensively**
   - Private declarations
   - Nested namespaces
   - Mutual recursion blocks
   - Instance declarations without explicit names
   - Macro-generated declarations

### If Pursuing Hybrid Approach

1. **Attribute callback + CLI fallback**
   - Attribute callback stores declaration metadata (name, range, module)
   - Highlighting fetched lazily via CLI when needed

2. **Lake facet integration**
   - Define a Lake facet that runs highlighting
   - Store results in build cache
   - Load from cache at runtime

---

## Conclusion

The hook-based approach in commit `a04919a` was architecturally sound in concept but had implementation issues related to:

1. **Module initialization ordering** - Fundamental Lean limitation, requires careful file organization
2. **Recursion prevention** - Implementation bug, fixable with better state management
3. **Elaboration timing** - Requires deeper understanding of Lean internals
4. **Syntax inspection fragility** - Implementation bug, should use attribute callbacks

The issues were likely a combination of fundamental design challenges and fixable implementation bugs. The decision to switch to the CLI-based approach (`subverso-extract-mod`) was pragmatic - it avoids all the elaboration-time complexity at the cost of requiring external tooling and build system integration.

A future implementation could potentially combine both approaches:
- Use attribute callbacks to mark declarations for highlighting
- Defer actual highlighting to a well-defined phase (end of module or build facet)
- Cache results persistently to avoid re-computation
