# LeanArchitect

> **Fork Status**: This is a fork of [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) with significant architectural changes. See [Fork Changes](#fork-changes) for details.

---

**LeanArchitect** is a lightweight Lean 4 library that provides the `@[blueprint]` attribute for marking declarations in mathematical formalization projects. It stores metadata about theorems, definitions, and their dependencies without any artifact generation.

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

Eight new fields added to support dashboard and project management features:

| Field | Type | Purpose |
|-------|------|---------|
| `displayName` | `Option String` | Custom node label in dependency graph (defaults to qualified name) |
| `keyDeclaration` | `Bool` | Highlight in dashboard Key Theorems section |
| `message` | `Option String` | User notes displayed in Messages panel |
| `priorityItem` | `Bool` | Flag for dashboard Attention column |
| `blocked` | `Option String` | Reason the node is blocked |
| `potentialIssue` | `Option String` | Known concerns or issues |
| `technicalDebt` | `Option String` | Technical debt / cleanup notes |
| `misc` | `Option String` | Catch-all miscellaneous notes |

### 3. Proof-Specific Dependencies

New fields distinguish dependencies that appear only in proofs vs. statements:

| Field | Purpose |
|-------|---------|
| `proofUses` | Dependencies used only in the proof (solid edges in graph) |
| `proofExcludes` | Exclude inferred proof dependencies |
| `proofUsesLabels` | Same as `proofUses`, by LaTeX label |
| `proofExcludesLabels` | Same as `proofExcludes`, by LaTeX label |

This enables dashed edges (statement dependencies) vs. solid edges (proof dependencies) in the dependency graph.

---

## Architecture

LeanArchitect is the **metadata layer** in the blueprint toolchain:

```
┌─────────────────────────────────────────────────────────────────────┐
│                     YOUR LEAN PROJECT                               │
│                                                                     │
│  @[blueprint "thm:foo"]                                             │
│  theorem foo : P → Q := ...                                         │
└─────────────────────────────────────────────────────────────────────┘
           │                              │
           │ Metadata only                │ + Artifact generation
           ▼                              ▼
┌─────────────────────┐    ┌─────────────────────────────────────────┐
│   LEANARCHITECT     │    │                DRESS                    │
│   (this library)    │    │   (recommended for most projects)       │
├─────────────────────┤    ├─────────────────────────────────────────┤
│ • @[blueprint] attr │    │ • Re-exports LeanArchitect              │
│ • Node/NodePart     │    │ • SubVerso syntax highlighting          │
│ • Dependency infer  │    │ • Verso HTML rendering                  │
│ • Dashboard fields  │    │ • LaTeX with embedded hover data        │
│ • NO SubVerso/Verso │    │ • Dressed artifacts for Runway          │
│ • Fast compilation  │    │ • Lake facets (blueprint, blueprintJson)│
└─────────────────────┘    └─────────────────────────────────────────┘
                                          │
                                          ▼
                           ┌─────────────────────────────────────────┐
                           │               RUNWAY                    │
                           │  Consumes Dress artifacts to produce   │
                           │  interactive website + dashboard + PDF  │
                           └─────────────────────────────────────────┘
```

**For most projects, import [Dress](https://github.com/e-vergo/Dress) instead of LeanArchitect directly.** Dress re-exports all of LeanArchitect's functionality and adds artifact generation.

## Installation

If you only need the `@[blueprint]` attribute without artifact generation:

```toml
[[require]]
name = "LeanArchitect"
git = "https://github.com/e-vergo/LeanArchitect"
rev = "main"
```

**Recommended:** For full blueprint functionality with syntax highlighting:

```toml
[[require]]
name = "Dress"
git = "https://github.com/e-vergo/Dress"
rev = "main"
```

## Usage

### Basic `@[blueprint]` Attribute

```lean
import Architect  -- or `import Dress` for artifact generation

/-- The sum of two even numbers is even. -/
@[blueprint "lem:even-sum"]
theorem even_add_even (a b : ℕ) (ha : Even a) (hb : Even b) : Even (a + b) := by
  obtain ⟨k, hk⟩ := ha
  obtain ⟨l, hl⟩ := hb
  exact ⟨k + l, by omega⟩
```

### Dashboard Metadata

```lean
-- Key theorem highlighted in dashboard
@[blueprint "thm:main" (keyDeclaration := true)]
theorem mainTheorem : ... := ...

-- With user message
@[blueprint "lem:helper" (message := "Used in main proof chain")]
lemma helperLemma : ... := ...

-- Blocked work with reason
@[blueprint "thm:blocked" (blocked := "Waiting for mathlib PR #12345")]
theorem blockedTheorem : ... := sorry

-- Custom display name for cleaner graph labels
@[blueprint "thm:sqrt2" (displayName := "√2 Irrational")]
theorem sqrt2_irrational : ... := ...

-- Priority item for attention
@[blueprint "lem:urgent" (priorityItem := true, potentialIssue := "Edge case not handled")]
lemma urgentFix : ... := ...

-- Technical debt tracking
@[blueprint "thm:needs-cleanup" (technicalDebt := "Refactor to use new API")]
theorem needsCleanup : ... := ...
```

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
  title : Option String          -- Short title for LaTeX
  -- Dashboard metadata (new in this fork):
  displayName : Option String    -- Custom graph label
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
  | notReady     -- Work in progress, not ready for use
  | stated       -- Statement only, no proof
  | ready        -- Ready to be proven
  | sorry        -- Contains sorry
  | proven       -- Proof complete
  | fullyProven  -- Proof complete with all dependencies proven
  | mathlibReady -- Ready for mathlib contribution
  | inMathlib    -- Already in mathlib
```

### Environment Extensions

- `blueprintExt : NameMapExtension Node` — All registered blueprint nodes
- `latexLabelToLeanNamesExt` — Reverse mapping from LaTeX labels to Lean names

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

## Modules

| Module | Purpose |
|--------|---------|
| `Architect.Basic` | `Node`, `NodePart`, `NodeStatus`, environment extensions |
| `Architect.Attribute` | `@[blueprint]` attribute syntax and elaboration |
| `Architect.CollectUsed` | Dependency inference from constant values |
| `Architect.Command` | `blueprint_comment` command |
| `Architect.Tactic` | `sorry_using`, `blueprint_using` tactics |

## Design Philosophy

LeanArchitect is intentionally minimal:

- **No heavy dependencies** — Only requires `batteries`
- **No artifact generation** — Leaves highlighting/rendering to Dress
- **Fast compilation** — Doesn't slow down your build
- **Separation of concerns** — Metadata collection vs. artifact generation

This allows projects to use `@[blueprint]` without SubVerso/Verso compilation overhead when rich output isn't needed.

## Related Projects

- **[Dress](https://github.com/e-vergo/Dress)** — Artifact generator (highlighting, HTML, LaTeX)
- **[Runway](https://github.com/e-vergo/Runway)** — Website/dashboard/PDF generator consuming Dress artifacts
- **[Original LeanArchitect](https://github.com/hanwenzhu/LeanArchitect)** — Upstream project this is forked from

## License

Apache 2.0 — see [LICENSE](LICENSE) for details.
