# LeanArchitect

The `@[blueprint]` attribute for Lean 4 formalization documentation.

LeanArchitect marks declarations for inclusion in mathematical formalization blueprints. It stores metadata about theorems, definitions, and their dependencies without generating artifacts.

**Fork**: This is a fork of [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) with architectural changes (metadata-only design, additional dashboard fields). See [Fork Changes](#fork-changes) for details.

## Installation

For full blueprint functionality with syntax highlighting, use [Dress](https://github.com/e-vergo/Dress) (which re-exports LeanArchitect):

```toml
[[require]]
name = "Dress"
git = "https://github.com/e-vergo/Dress"
rev = "main"
```

For metadata-only usage without artifact generation:

```toml
[[require]]
name = "LeanArchitect"
git = "https://github.com/e-vergo/LeanArchitect"
rev = "main"
```

## Basic Usage

```lean
import Architect

/-- The sum of two even numbers is even. -/
@[blueprint "lem:even-sum"]
theorem even_add_even (a b : Nat) (ha : Even a) (hb : Even b) : Even (a + b) := by
  obtain ⟨k, hk⟩ := ha
  obtain ⟨l, hl⟩ := hb
  exact ⟨k + l, by omega⟩
```

The string argument (`"lem:even-sum"`) is the LaTeX label used in the blueprint document.

## Attribute Options

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

```lean
-- Key theorem highlighted in dashboard
@[blueprint "thm:main" (keyDeclaration := true)]
theorem mainTheorem : ... := ...

-- With user message
@[blueprint "lem:helper" (message := "Used in main proof chain")]
lemma helperLemma : ... := ...

-- Custom title for graph display
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

### Manual Status Flags (3)

| Option | Type | Sets Status To |
|--------|------|----------------|
| `notReady` | `Bool` | `notReady` - not ready to formalize (sandy brown) |
| `ready` | `Bool` | `ready` - ready to formalize (light sea green) |
| `mathlibReady` | `Bool` | `mathlibReady` - ready for mathlib (light blue) |

```lean
-- Not ready to formalize yet
@[blueprint "thm:future" (notReady := true)]
theorem futureWork : ... := sorry

-- Ready to be proven
@[blueprint "lem:ready" (ready := true)]
lemma readyToProve : ... := sorry

-- Ready for mathlib contribution
@[blueprint "thm:upstream" (mathlibReady := true)]
theorem readyForMathlib : ... := ...
```

### Node Status Model (6 States)

The dependency graph displays 6 possible statuses:

| Status | Color | Hex | Source |
|--------|-------|-----|--------|
| `notReady` | Sandy Brown | #F4A460 | Default or manual `(notReady := true)` |
| `ready` | Light Sea Green | #20B2AA | Manual `(ready := true)` |
| `sorry` | Dark Red | #8B0000 | Auto-derived: proof contains `sorryAx` |
| `proven` | Light Green | #90EE90 | Auto-derived: complete proof without sorry |
| `fullyProven` | Forest Green | #228B22 | Auto-computed: proven and all ancestors proven/fullyProven |
| `mathlibReady` | Light Blue | #87CEEB | Manual `(mathlibReady := true)` |

**Status priority** (highest to lowest):
1. `mathlibReady` - if manually set
2. `fullyProven` - auto-computed if this node and all dependencies are proven
3. `sorry` - if proof contains sorryAx
4. `proven` - if formalized without sorry
5. `ready` - if manually set
6. `notReady` - default

The `fullyProven` status is computed via graph traversal during artifact generation. It cannot be set manually.

### Dependency Options

```lean
@[blueprint "thm:main"
  (uses := [lem_helper, thm_base])           -- Statement dependencies
  (excludes := [-lem_internal])              -- Exclude from inference
  (proofUses := [lem_technical])             -- Proof-only dependencies
  (proofExcludes := [-lem_auto])]            -- Exclude from proof inference
theorem mainTheorem : ... := ...
```

Dependencies can reference other nodes by LaTeX label:

```lean
@[blueprint "thm:c"
  (usesLabels := ["thm:a", "thm:b"])
  (proofUsesLabels := ["lem:helper"])]
theorem thmC : ... := ...
```

| Field | Purpose |
|-------|---------|
| `uses` | Statement dependencies (dashed edges in graph) |
| `excludes` | Exclude inferred statement dependencies |
| `proofUses` | Proof-only dependencies (solid edges in graph) |
| `proofExcludes` | Exclude inferred proof dependencies |
| `usesLabels` | Same as `uses`, by LaTeX label |
| `proofUsesLabels` | Same as `proofUses`, by LaTeX label |
| `excludesLabels` | Same as `excludes`, by LaTeX label |
| `proofExcludesLabels` | Same as `proofExcludes`, by LaTeX label |

### Content Options

```lean
@[blueprint "thm:main"
  (statement := /-- Custom statement text for LaTeX -/)
  (proof := /-- Custom proof explanation -/)
  (hasProof := true)                          -- Force proof part (default: true for theorems)
  (latexEnv := "lemma")                       -- LaTeX environment
  (latexLabel := "thm:custom-label")          -- Override label
  (discussion := 42)]                         -- GitHub issue number
theorem mainTheorem : ... := ...
```

### Debugging

Use `blueprint?` to display the raw node data:

```lean
@[blueprint? "thm:debug"]
theorem debugExample : ... := ...
```

## Dependency Inference

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

## Data Structures

### Node

```lean
structure Node where
  name : Name                    -- Lean name (e.g., `MyModule.myTheorem`)
  latexLabel : String            -- LaTeX label (e.g., "thm:my-theorem")
  statement : NodePart           -- Statement with text and dependencies
  proof : Option NodePart        -- Optional proof part
  status : NodeStatus            -- notReady, ready, sorry, proven, fullyProven, mathlibReady
  discussion : Option Nat        -- GitHub issue number
  title : Option String          -- Custom title / graph label
  keyDeclaration : Bool          -- Highlight in Key Theorems
  message : Option String        -- User notes
  priorityItem : Bool            -- Attention column flag
  blocked : Option String        -- Blockage reason
  potentialIssue : Option String -- Known concerns
  technicalDebt : Option String  -- Cleanup notes
  misc : Option String           -- Miscellaneous notes
```

### NodePart

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
  | notReady     -- Default: not ready to formalize
  | ready        -- Manual: ready to be proven
  | sorry        -- Derived: contains sorryAx
  | proven       -- Derived: proof complete
  | fullyProven  -- Auto-computed: proven + all ancestors proven/fullyProven
  | mathlibReady -- Manual: ready for mathlib contribution
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
| `Architect.Content` | `BlueprintContent` type for module contents |
| `Architect.Load` | Loading nodes from environment |
| `Architect.Tactic` | `sorry_using`, `blueprint_using` tactics |

## Role in Toolchain

```
SubVerso -> LeanArchitect -> Dress -> Runway
```

LeanArchitect is the metadata layer:

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

## Fork Changes

This fork ([e-vergo/LeanArchitect](https://github.com/e-vergo/LeanArchitect)) diverges from [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect):

### 1. Metadata-Only Architecture

Artifact generation moved to [Dress](https://github.com/e-vergo/Dress). Removed components:

| Component | Purpose | New Location |
|-----------|---------|--------------|
| `Main.lean` | CLI executable | Dress |
| `Cli` dependency | Command-line parsing | Dress |
| Lake facets | `blueprint`, `blueprintJson` | Dress |
| Rendering code | HTML/LaTeX generation | Dress (via SubVerso/Verso) |

**Dependency comparison**:

| Dependency | Upstream | This Fork |
|------------|----------|-----------|
| `batteries` | Yes | Yes |
| `Cli` | Yes | No |
| SubVerso/Verso | Transitive | No (moved to Dress) |

### 2. Dashboard Metadata Fields

Eight metadata fields and three manual status flags added for dashboard and project management:
- `keyDeclaration`, `message`, `priorityItem`, `blocked`, `potentialIssue`, `technicalDebt`, `misc`
- `notReady`, `ready`, `mathlibReady` status flags

### 3. Manual ToExpr Instance

The `Node` structure uses a manual `ToExpr` instance. Lean's derived `ToExpr` for structures with default field values does not correctly serialize all fields through environment extensions:

```lean
instance : ToExpr Node where
  toTypeExpr := mkConst ``Node
  toExpr n := Lean.mkAppN (mkConst ``Node.mk) #[
    toExpr n.name,
    toExpr n.latexLabel,
    toExpr n.statement,
    toExpr n.proof,
    toExpr n.status,
    -- ... all 14 fields explicitly serialized
  ]
```

## Related Projects

| Project | Purpose |
|---------|---------|
| [Dress](https://github.com/e-vergo/Dress) | Artifact generator (highlighting, HTML, LaTeX, graph layout) |
| [Runway](https://github.com/e-vergo/Runway) | Website/dashboard/PDF generator |
| [SubVerso](https://github.com/e-vergo/subverso) | Syntax highlighting extraction |
| [SBS-Test](https://github.com/e-vergo/SBS-Test) | Minimal test project (16 nodes, all 6 status colors) |
| [dress-blueprint-action](https://github.com/e-vergo/dress-blueprint-action) | GitHub Actions CI/CD |
| [Original LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) | Upstream project |

## License

Apache 2.0 - see [LICENSE](LICENSE).
