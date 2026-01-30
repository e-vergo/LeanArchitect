# LeanArchitect

The `@[blueprint]` attribute for formalization documentation.

LeanArchitect is a lightweight Lean 4 library that marks declarations for inclusion in mathematical formalization blueprints. It stores metadata about theorems, definitions, and their dependencies without any artifact generation.

> **Fork Status**: This is a fork of [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) with significant architectural changes. See [Fork Changes](#fork-changes) for details.

## Usage

### Basic Attribute

```lean
import Architect  -- or `import Dress` for full artifact generation

/-- The sum of two even numbers is even. -/
@[blueprint "lem:even-sum"]
theorem even_add_even (a b : Nat) (ha : Even a) (hb : Even b) : Even (a + b) := by
  obtain ⟨k, hk⟩ := ha
  obtain ⟨l, hl⟩ := hb
  exact ⟨k + l, by omega⟩
```

### Metadata Options (8)

| Option | Type | Purpose |
|--------|------|---------|
| `title` | `String` | Custom node label in dependency graph |
| `keyDeclaration` | `Bool` | Highlight in dashboard Key Theorems section |
| `message` | `String` | User notes displayed in Messages panel |
| `priorityItem` | `Bool` | Flag for dashboard Attention column |
| `blocked` | `String` | Reason the node is blocked |
| `potentialIssue` | `String` | Known concerns or issues |
| `technicalDebt` | `String` | Technical debt / cleanup notes |
| `misc` | `String` | Catch-all miscellaneous notes |

**Examples:**

```lean
-- Key theorem highlighted in dashboard
@[blueprint "thm:main" (keyDeclaration := true)]
theorem mainTheorem : ... := ...

-- With user message
@[blueprint "lem:helper" (message := "Used in main proof chain")]
lemma helperLemma : ... := ...

-- Custom title for cleaner graph labels
@[blueprint "thm:sqrt2" (title := "Sqrt 2 Irrational")]
theorem sqrt2_irrational : ... := ...

-- Blocked work with reason
@[blueprint "thm:blocked" (blocked := "Waiting for mathlib PR #12345")]
theorem blockedTheorem : ... := sorry

-- Priority item for attention
@[blueprint "lem:urgent" (priorityItem := true, potentialIssue := "Edge case not handled")]
lemma urgentFix : ... := ...

-- Technical debt tracking
@[blueprint "thm:needs-cleanup" (technicalDebt := "Refactor to use new API")]
theorem needsCleanup : ... := ...
```

### Status Flags (3)

| Option | Type | Sets Status To |
|--------|------|----------------|
| `notReady` | `Bool` | `notReady` - not ready to formalize (sandy brown) |
| `ready` | `Bool` | `ready` - ready to formalize (light sea green) |
| `mathlibReady` | `Bool` | `mathlibReady` - ready for mathlib (light blue) |

**Note**: `fullyProven` is auto-computed (proven + all ancestors proven/fullyProven), not a manual flag.

**Examples:**

```lean
-- Not ready to formalize yet (default status)
@[blueprint "thm:future" (notReady := true)]
theorem futureWork : ... := sorry

-- Ready to be proven
@[blueprint "lem:ready" (ready := true)]
lemma readyToProve : ... := sorry

-- Ready for mathlib contribution
@[blueprint "thm:upstream" (mathlibReady := true)]
theorem readyForMathlib : ... := ...
```

### Node Status Types (6)

The dependency graph displays 6 possible statuses with distinct colors:

| Status | Color | Hex | Source |
|--------|-------|-----|--------|
| `notReady` | Sandy Brown | #F4A460 | Default + Manual: `(notReady := true)` |
| `ready` | Light Sea Green | #20B2AA | Manual: `(ready := true)` |
| `sorry` | Dark Red | #8B0000 | Auto: proof contains `sorryAx` |
| `proven` | Light Green | #90EE90 | Auto: complete proof without sorry |
| `fullyProven` | Forest Green | #228B22 | Auto-computed: proven + all ancestors proven/fullyProven |
| `mathlibReady` | Light Blue | #87CEEB | Manual: `(mathlibReady := true)` |

The `sorry` and `proven` statuses are automatically derived by analyzing the proof term. The `fullyProven` status is auto-computed when a node is proven and all its ancestors are also proven or fullyProven. Manual flags take precedence.

### Dependency Options

```lean
@[blueprint "thm:main"
  (title := "Main Theorem")
  (statement := /-- Custom statement text for LaTeX -/)
  (proof := /-- Custom proof explanation -/)
  (uses := [lem_helper, thm_base])           -- Statement dependencies
  (excludes := [-lem_internal])              -- Exclude from inference
  (proofUses := [lem_technical])             -- Proof-only dependencies
  (proofExcludes := [-lem_auto])             -- Exclude from proof inference
  (status := .stated)                        -- Manual status override
  (discussion := 42)                         -- GitHub issue number
  (latexEnv := "theorem")]                   -- LaTeX environment
theorem mainTheorem : ... := ...
```

### Label-Based Dependencies

Dependencies can reference other nodes by LaTeX label instead of Lean name:

```lean
@[blueprint "thm:c"
  (usesLabels := ["thm:a", "thm:b"])
  (proofUsesLabels := ["lem:helper"])]
theorem thmC : ... := ...
```

### Proof-Specific Dependencies

Distinguish dependencies that appear only in proofs vs. statements:

| Field | Purpose |
|-------|---------|
| `proofUses` | Dependencies used only in the proof (solid edges in graph) |
| `proofExcludes` | Exclude inferred proof dependencies |
| `proofUsesLabels` | Same as `proofUses`, by LaTeX label |
| `proofExcludesLabels` | Same as `proofExcludes`, by LaTeX label |

This enables dashed edges (statement dependencies) vs. solid edges (proof dependencies) in the dependency graph.

### Dependency Inference

LeanArchitect automatically infers dependencies by analyzing which constants a declaration uses:

```lean
@[blueprint "lem:a"]
lemma lemA : ... := ...

@[blueprint "lem:b"]
lemma lemB : ... := lemA  -- Automatically depends on lem:a

@[blueprint "thm:c"]
theorem thmC : ... := by
  apply lemB              -- Proof depends on lem:b (solid edge)
```

Use `excludes` to remove inferred dependencies, or `uses` to add explicit ones.

## Role in Dependency Chain

```
SubVerso -> LeanArchitect -> Dress -> Runway
```

LeanArchitect is the **metadata layer** in the blueprint toolchain:

```
+---------------------------------------------------------------------+
|                     YOUR LEAN PROJECT                               |
|                                                                     |
|  @[blueprint "thm:foo"]                                             |
|  theorem foo : P -> Q := ...                                        |
+---------------------------------------------------------------------+
           |                              |
           | Metadata only                | + Artifact generation
           v                              v
+---------------------+    +---------------------------------------------+
|   LEANARCHITECT     |    |                DRESS                        |
|   (this library)    |    |   (recommended for most projects)           |
+---------------------+    +---------------------------------------------+
| - @[blueprint] attr |    | - Re-exports LeanArchitect                  |
| - Node/NodePart     |    | - SubVerso syntax highlighting              |
| - Dependency infer  |    | - Verso HTML rendering                      |
| - Dashboard fields  |    | - LaTeX with embedded hover data            |
| - NO SubVerso/Verso |    | - Dressed artifacts for Runway              |
| - Fast compilation  |    | - Lake facets (blueprint, blueprintJson)    |
+---------------------+    +---------------------------------------------+
                                          |
                                          v
                           +---------------------------------------------+
                           |               RUNWAY                        |
                           |  Consumes Dress artifacts to produce       |
                           |  interactive website + dashboard + PDF      |
                           +---------------------------------------------+
```

## Installation

**Recommended:** For full blueprint functionality with syntax highlighting, import Dress (which re-exports LeanArchitect):

```toml
[[require]]
name = "Dress"
git = "https://github.com/e-vergo/Dress"
rev = "main"
```

If you only need the `@[blueprint]` attribute without artifact generation:

```toml
[[require]]
name = "LeanArchitect"
git = "https://github.com/e-vergo/LeanArchitect"
rev = "main"
```

## Additional Features

### Blueprint Comments

Add standalone comments that appear in the blueprint:

```lean
blueprint_comment "sec:intro" /--
  This section introduces the main concepts...
-/
```

### Tactics

```lean
-- Specify dependencies for a sorry
theorem foo : P := by
  sorry_using [lem_needed, thm_assumed]

-- Specify dependencies mid-proof
theorem bar : P := by
  blueprint_using [helper_lemma]
  apply helper_lemma
  trivial
```

### Multiple Declarations per Label

Multiple Lean declarations can share a label:

```lean
@[blueprint "thm:a"] theorem a_part_one : ...
@[blueprint "thm:a"] theorem a_part_two : ...
```

Works with `to_additive`:

```lean
@[to_additive (attr := blueprint "thm:b")] theorem b_mul : ...
```

## Core Data Structures

### Node

Represents a single blueprint declaration:

```lean
structure Node where
  name : Name                    -- Lean name (e.g., `MyModule.myTheorem`)
  latexLabel : String            -- LaTeX label (e.g., "thm:my-theorem")
  statement : NodePart           -- Statement with text and dependencies
  proof : Option NodePart        -- Optional proof part
  status : NodeStatus            -- notReady, stated, ready, sorry, proven, etc.
  discussion : Option Nat        -- GitHub issue number
  title : Option String          -- Short title for LaTeX / custom graph label
  -- Dashboard metadata:
  keyDeclaration : Bool          -- Highlight in Key Theorems
  message : Option String        -- User notes
  priorityItem : Bool            -- Attention column flag
  blocked : Option String        -- Blockage reason
  potentialIssue : Option String -- Known concerns
  technicalDebt : Option String  -- Cleanup notes
  misc : Option String           -- Miscellaneous notes
```

### NodePart

Represents the statement or proof of a node:

```lean
structure NodePart where
  text : String                 -- Natural language description
  uses : Array Name             -- Declared dependencies
  excludes : Array Name         -- Excluded from inference
  usesLabels : Array String     -- Dependencies by LaTeX label
  excludesLabels : Array String
  latexEnv : String             -- LaTeX environment name
```

### NodeStatus

```lean
inductive NodeStatus where
  | notReady     -- Not ready to formalize (default)
  | ready        -- Ready to be proven
  | sorry        -- Contains sorryAx
  | proven       -- Proof complete
  | fullyProven  -- Auto-computed: proven + all ancestors proven/fullyProven
  | mathlibReady -- Ready for mathlib contribution
```

### Environment Extensions

- `blueprintExt : NameMapExtension Node` - All registered blueprint nodes
- `latexLabelToLeanNamesExt` - Reverse mapping from LaTeX labels to Lean names

## Modules

| Module | Purpose |
|--------|---------|
| `Architect.Basic` | `Node`, `NodePart`, `NodeStatus`, environment extensions |
| `Architect.Attribute` | `@[blueprint]` attribute syntax and elaboration |
| `Architect.CollectUsed` | Dependency inference from constant values |
| `Architect.Command` | `blueprint_comment` command |
| `Architect.Tactic` | `sorry_using`, `blueprint_using` tactics |

## Fork Changes

This fork ([e-vergo/LeanArchitect](https://github.com/e-vergo/LeanArchitect)) diverges from [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) with two major changes:

### 1. Metadata-Only Architecture

Artifact generation has been **moved to [Dress](https://github.com/e-vergo/Dress)**. This fork removes:

| Removed Component | Purpose | New Location |
|-------------------|---------|--------------|
| `Main.lean` | CLI executable (`extract_blueprint`) | Dress |
| `Cli` dependency | Command-line argument parsing | Dress |
| Lake facets | `blueprint`, `blueprintJson` extraction | Dress |
| `Architect.Content` | Rendering infrastructure | Dress (via SubVerso/Verso) |
| `blueprintConvert` script | Migration from legacy format | Dress |

**Benefits**:
- Projects needing only `@[blueprint]` metadata avoid SubVerso/Verso compilation overhead
- Single dependency on `batteries` (no Cli, no SubVerso/Verso transitives)
- Faster compilation for projects not generating artifacts
- Cleaner separation of concerns: metadata collection vs. artifact generation

**Dependency comparison**:

| Dependency | Upstream | This Fork |
|------------|----------|-----------|
| `batteries` | Yes | Yes |
| `Cli` | Yes | **No** |
| SubVerso/Verso | Transitive | **No** (moved to Dress) |

### 2. New Metadata Fields

Eight metadata fields and three status flags added to support dashboard and project management features, as documented above.

## Related Projects

| Project | Purpose |
|---------|---------|
| [Dress](https://github.com/e-vergo/Dress) | Artifact generator (highlighting, HTML, LaTeX) |
| [Runway](https://github.com/e-vergo/Runway) | Website/dashboard/PDF generator |
| [SubVerso](https://github.com/e-vergo/subverso) | Syntax highlighting extraction |
| [SBS-Test](https://github.com/e-vergo/SBS-Test) | Minimal test project |
| [dress-blueprint-action](https://github.com/e-vergo/dress-blueprint-action) | GitHub Actions CI/CD |
| [Original LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) | Upstream project |

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.
