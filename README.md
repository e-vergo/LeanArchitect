# LeanArchitect

The `@[blueprint]` attribute for Lean 4 formalization documentation.

LeanArchitect marks declarations for inclusion in mathematical formalization blueprints. It stores metadata about theorems, definitions, and their dependencies, and provides dependency inference by analyzing which constants a declaration uses.

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

The string argument (`"lem:even-sum"`) is the LaTeX label used in the blueprint document. If omitted, the Lean declaration name is used as the label.

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

**Status determination** (evaluated in order):
1. `mathlibReady` - if manually set, takes highest priority
2. `ready` - if manually set
3. If Lean code exists:
   - `sorry` - if proof contains `sorryAx`
   - `proven` - if proof is complete
4. `notReady` - default (no Lean code or `notReady := true`)
5. `fullyProven` - computed post-graph: upgrades `proven` nodes where all ancestors are also proven/fullyProven

The `fullyProven` status is computed via O(V+E) graph traversal during artifact generation. It cannot be set manually.

### Dependency Options

```lean
@[blueprint "thm:main"
  (uses := [lem_helper, thm_base])           -- Statement dependencies
  (proofUses := [lem_technical])]            -- Proof-only dependencies
theorem mainTheorem : ... := ...
```

Dependencies can reference other nodes by LaTeX label:

```lean
@[blueprint "thm:c"
  (uses := ["thm:a", "thm:b"])
  (proofUses := ["lem:helper"])]
theorem thmC : ... := ...
```

Exclude inferred dependencies with `-` prefix:

```lean
@[blueprint "thm:d"
  (uses := [lemA, -lemB])]          -- Include lemA, exclude lemB
theorem thmD : ... := ...
```

| Field | Purpose |
|-------|---------|
| `uses` | Statement dependencies (dashed edges in graph) |
| `proofUses` | Proof-only dependencies (solid edges in graph) |

Both options accept Lean constants or LaTeX labels (strings). Prefix with `-` to exclude from inferred dependencies.

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

LeanArchitect automatically infers dependencies by analyzing which constants a declaration uses. The `collectUsed` function traverses the expression tree of a declaration's type and value, collecting all referenced constants that have `@[blueprint]` attributes.

```lean
@[blueprint "lem:a"]
lemma lemA : ... := ...

@[blueprint "lem:b"]
lemma lemB : ... := lemA  -- Automatically depends on lem:a

@[blueprint "thm:c"]
theorem thmC : ... := by
  apply lemB              -- Proof depends on lem:b (solid edge)
```

Dependencies are split into two categories:
- **Statement dependencies**: Constants used in the type signature (dashed edges)
- **Proof dependencies**: Constants used in the proof value but not the type (solid edges)

Use `uses` to add explicit dependencies or prefix with `-` to exclude inferred ones.

## Additional Features

### Blueprint Comments

Add standalone comments that appear in the blueprint module output:

```lean
blueprint_comment /--
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

### Proof Docstrings

Docstrings within proofs are captured and included in the LaTeX output:

```lean
@[blueprint "thm:example"]
theorem example_thm : P := by
  /-- First we establish the base case. -/
  apply base_case
  /-- Then we proceed by induction. -/
  induction n with
  | zero => trivial
  | succ => assumption
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

### Commands

```lean
-- Show LaTeX output of current module
#show_blueprint

-- Show LaTeX output of a specific declaration
#show_blueprint myTheorem
#show_blueprint "thm:label"

-- Show JSON output
#show_blueprint_json
#show_blueprint_json myTheorem
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
  excludesLabels : Array String -- Excluded labels
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
| `Architect.Output` | LaTeX and JSON generation |
| `Architect.Tactic` | `sorry_using`, `blueprint_using` tactics, proof docstrings |

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

## Options

| Option | Default | Purpose |
|--------|---------|---------|
| `blueprint.ignoreUnknownConstants` | `false` | Whether to ignore unknown constants in `uses` and `proofUses` |

Set via `set_option blueprint.ignoreUnknownConstants true` to suppress errors for unresolved constants.

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

### 4. Backwards Compatibility

JSON parsing handles legacy status values:
- `"stated"` maps to `.notReady`
- `"inMathlib"` maps to `.mathlibReady`

## Related Projects

| Project | Purpose |
|---------|---------|
| [Dress](https://github.com/e-vergo/Dress) | Artifact generator (highlighting, HTML, LaTeX, graph layout) |
| [Runway](https://github.com/e-vergo/Runway) | Website/dashboard/PDF generator |
| [SubVerso](https://github.com/e-vergo/subverso) | Syntax highlighting extraction |
| [SBS-Test](https://github.com/e-vergo/SBS-Test) | Minimal test project (~35 nodes, all 6 status colors) |
| [dress-blueprint-action](https://github.com/e-vergo/dress-blueprint-action) | GitHub Actions CI/CD |
| [Original LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) | Upstream project |

## License

Apache 2.0 - see [LICENSE](LICENSE).
