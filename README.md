# LeanArchitect

> **Experience Prototype**: This is an experimental fork of [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) exploring enhanced features for Lean blueprint generation. Key additions include improved dependency inference, proof-specific dependency tracking, and integration with [Dress](https://github.com/e-vergo/Dress) for artifact generation. Changes may be upstreamed once stable.

---

**LeanArchitect** is a lightweight Lean 4 library that provides the `@[blueprint]` attribute for marking declarations in mathematical formalization projects. It stores metadata about theorems, definitions, and their dependencies without any artifact generation.

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
│ • NO SubVerso/Verso │    │ • LaTeX with embedded hover data        │
│ • Fast compilation  │    │ • Dressed artifacts for leanblueprint   │
└─────────────────────┘    └─────────────────────────────────────────┘
                                          │
                                          ▼
                           ┌─────────────────────────────────────────┐
                           │            LEANBLUEPRINT                │
                           │  Consumes Dress artifacts to produce   │
                           │  interactive website + PDF              │
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

### Attribute Options

```lean
@[blueprint "thm:main"
  (title := "Main Theorem")
  (statement := /-- Custom statement text for LaTeX -/)
  (proof := /-- Custom proof explanation -/)
  (uses := [lem_helper, thm_base])           -- Explicit dependencies
  (proofUses := [lem_technical])             -- Proof-only dependencies
  (notReady := true)                          -- Mark as work-in-progress
  (discussion := 42)                          -- GitHub issue number
  (latexEnv := "theorem")]                    -- LaTeX environment
theorem mainTheorem : ... := ...
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
  apply lemB              -- Proof depends on lem:b
```

Use `(excludes := [-name])` to remove inferred dependencies, or `(uses := [name])` to add explicit ones.

## Core Data Structures

### Node

Represents a single blueprint declaration:

```lean
structure Node where
  name : Name           -- Lean name (e.g., `MyModule.myTheorem`)
  latexLabel : String   -- LaTeX label (e.g., "thm:my-theorem")
  statement : NodePart  -- Statement with text and dependencies
  proof : Option NodePart
  notReady : Bool
  discussion : Option Nat
  title : Option String
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
  latexEnv : String            -- LaTeX environment name
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
| `Architect.Basic` | `Node`, `NodePart`, environment extensions |
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
- **[leanblueprint](https://github.com/e-vergo/leanblueprint)** — Website/PDF generator consuming Dress artifacts
- **[Original LeanArchitect](https://github.com/hanwenzhu/LeanArchitect)** — Upstream project this is forked from

## Fork Changes

This fork ([e-vergo/LeanArchitect](https://github.com/e-vergo/LeanArchitect)) diverges from [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) with a **metadata-only architecture**: artifact generation (LaTeX, JSON, syntax highlighting) has been moved to [Dress](https://github.com/e-vergo/Dress).

### Diff Summary

```text
 8 files changed, 172 insertions(+), 584 deletions(-)
```

### Removed Components

| Component | Purpose | New Location |
|-----------|---------|--------------|
| `Main.lean` | CLI executable (`extract_blueprint`) | Dress |
| `Cli` dependency | Command-line argument parsing | Dress |
| Lake facets (`blueprint`, `blueprintJson`) | Module/library/package extraction | Dress |
| `blueprintConvert` script | Migration from legacy format | Dress |
| `Architect.Content` import | Rendering infrastructure | Dress (via SubVerso/Verso) |

### Modified Files

| File | Change |
|------|--------|
| `lakefile.lean` | Removed `extract_blueprint` executable, Lake facets, and `Cli` dependency. Now exports only `Architect` library. |
| `Architect/Attribute.lean` | Changed import from `Architect.Content` to `Architect.Basic` (content rendering moved to Dress). |
| `lake-manifest.json` | Removed `Cli` package. Single dependency on `batteries`. |
| `lean-toolchain` | Updated to current Mathlib-compatible toolchain. |

### Architectural Rationale

Upstream LeanArchitect bundles metadata collection with artifact generation. This fork separates concerns:

1. **LeanArchitect** — `@[blueprint]` attribute, `Node`/`NodePart` structures, dependency inference, environment extensions
2. **Dress** — SubVerso syntax highlighting, Verso HTML rendering, LaTeX output, Lake build facets

Benefits:

- Projects needing only `@[blueprint]` metadata avoid SubVerso/Verso compilation overhead
- Dress can evolve rendering independently of the core attribute semantics
- Cleaner dependency graph for downstream consumers

### Dependency Comparison

| | Upstream | This Fork |
|-|----------|-----------|
| `batteries` | Yes | Yes |
| `Cli` | Yes | No |
| SubVerso/Verso | Transitive | No (moved to Dress) |

## License

Apache 2.0 — see [LICENSE](LICENSE) for details.
