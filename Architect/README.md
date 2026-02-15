# Architect Module

The `Architect` module implements the `@[blueprint]` attribute for marking Lean declarations as nodes in a Side-by-Side Blueprint dependency graph. It defines the core data types, attribute syntax, dependency inference, LaTeX generation, validation, and an RPC endpoint for IDE integration.

## The `@[blueprint]` Attribute

The attribute tags a Lean declaration (theorem, definition, lemma, etc.) for inclusion in the blueprint. It accepts an optional LaTeX label and a set of configuration options in parenthesized groups.

### Basic Syntax

```lean
-- Minimal: just a label
@[blueprint "thm:main"]
theorem mainTheorem : P := by ...

-- No label (defaults to the fully qualified Lean name)
@[blueprint]
def helperDef : Nat := 42

-- With options
@[blueprint "lem:key"
  (keyDeclaration := true)
  (message := "Central lemma for the main argument")]
lemma keyLemma : Q := by ...
```

### Configuration Options

Every option is specified as `(optionName := value)` after the optional label string. All options have sensible defaults and are independently optional.

#### Content Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `statement` | docstring | `""` | LaTeX text for the statement. Written as `(statement := /-- LaTeX here -/)`. |
| `proof` | docstring | `""` | LaTeX text for the proof. Falls back to proof docstrings if not provided. |
| `above` | docstring | `none` | LaTeX content placed above the node in the rendered blueprint/paper. |
| `below` | docstring | `none` | LaTeX content placed below the node in the rendered blueprint/paper. |
| `hasProof` | `Bool` | `true` for theorems | Override whether this node has a separate proof part. |
| `title` | `String` or docstring | `none` | Short title used as the display label in the dependency graph. |
| `latexEnv` | `String` | `"theorem"` or `"definition"` | LaTeX environment name (e.g., `"lemma"`, `"proposition"`, `"corollary"`). |
| `latexLabel` | `String` | the label argument | Override the LaTeX label (rarely needed; prefer the positional argument). |
| `discussion` | `Nat` | `none` | GitHub issue number for discussion about this node. |

#### Dependency Options

Dependencies can reference Lean constant names or LaTeX labels (as strings). Prefix with `-` to exclude.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `uses` | `[a, "b", -c]` | inferred | Statement dependencies. Lean names or LaTeX labels. |
| `proofUses` | `[a, "b"]` | inferred | Proof dependencies. |

By default, dependencies are inferred automatically from the expression tree via `collectUsed` (see [Dependency Inference](#dependency-inference)). Manual `uses`/`proofUses` add to or override inferred dependencies.

#### Metadata Options (8 fields)

These fields appear in the Runway-generated dashboard and project management views.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `keyDeclaration` | `Bool` | `false` | Highlight in the dashboard Key Theorems section. |
| `message` | `String` | `none` | User notes displayed in the Messages panel. |
| `priorityItem` | `Bool` | `false` | Flag for the dashboard Attention column. |
| `blocked` | `String` | `none` | Reason the node is blocked. |
| `potentialIssue` | `String` | `none` | Known concerns or issues. |
| `technicalDebt` | `String` | `none` | Technical debt / cleanup notes. |
| `misc` | `String` | `none` | Catch-all miscellaneous notes. |
| `title` | `String` | `none` | Custom display name in the dependency graph (also listed under Content). |

#### Manual Status Flags (3 options)

| Option | Sets Status To | Graph Color |
|--------|----------------|-------------|
| `notReady := true` | notReady | Sandy Brown (#F4A460) |
| `ready := true` | ready | Light Sea Green (#20B2AA) |
| `mathlibReady := true` | mathlibReady | Light Blue (#87CEEB) |

Setting `false` is a no-op (the status stays at default).

#### Validation Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `skipValidation` | `Bool` | `false` | Skip LaTeX brace/math-delimiter validation for this node. |
| `skipCrossRef` | `Bool` | `false` | Skip cross-reference checking between statement text and Lean signature. |

## Node Status Model

Every node has one of 6 statuses, determined by a priority system that combines manual flags with automatic derivation.

| Status | How Determined | Color |
|--------|---------------|-------|
| `notReady` | Default, or set manually | Sandy Brown (#F4A460) |
| `ready` | Manual flag | Light Sea Green (#20B2AA) |
| `sorry` | Auto: proof contains `sorryAx` | Dark Red (#8B0000) |
| `proven` | Auto: formalized without sorry | Light Green (#90EE90) |
| `fullyProven` | Auto-computed: node AND all ancestors proven | Forest Green (#228B22) |
| `mathlibReady` | Manual flag (highest priority) | Light Blue (#87CEEB) |

**Priority order** (highest to lowest): mathlibReady > ready > notReady (explicit) > fullyProven > sorry > proven > notReady (default).

The `sorry` and `proven` statuses are derived by checking whether the constant's value references `sorryAx`. The `fullyProven` status is computed downstream by Dress via graph traversal -- it is not a manual flag.

## Dependency Inference

`CollectUsed.lean` provides `collectUsed`, which traverses the expression tree of a constant to find all transitively referenced blueprint nodes and axioms. It returns two disjoint sets:

- **Statement dependencies** (from the type signature) -- rendered as dashed edges in the graph
- **Proof dependencies** (from the proof/value body) -- rendered as solid edges in the graph

The traversal stops at blueprint nodes (they become leaf dependencies) but recurses through non-blueprint constants to find deeper references.

## Validation

Two validation systems run during elaboration (warnings only, never errors):

1. **Statement validation** (`Validation.lean`): Checks LaTeX statement text for empty content, unbalanced braces, and unbalanced math delimiters (`$`, `\[...\]`).

2. **Cross-reference checking** (`CrossRef.lean`): Heuristic matching between LaTeX statement text and the Lean type signature. Checks for missing math structure names, quantifier count mismatches, and declaration name relevance.

Both can be disabled per-node via `skipValidation` and `skipCrossRef`.

## Proof Docstrings and Tactics

- **Proof docstrings**: Write `/-- LaTeX -/` before a tactic inside a proof to add LaTeX text to the proof part. These accumulate via `tacticDocComment` and are used as fallback proof text when no explicit `proof` option is given.
- **`blueprint_using [a, b]`**: Tactic that declares `a` and `b` as proof dependencies without affecting the goal.
- **`sorry_using [a, b]`**: Like `sorry`, but records `a` and `b` as dependencies. Useful for stubbing proofs while preserving the dependency graph.
- **`blueprint_comment /-- ... -/`**: Command (not a tactic) that adds free-form LaTeX text to the module's blueprint output, interleaved with nodes by source position.

## Data Flow

```
@[blueprint] attribute (LeanArchitect)
    |
    v
Dress elab_rules capture artifacts + SubVerso highlighting
    |
    v
Dress graph builder: inferUses, status computation, Sugiyama layout
    |
    v
manifest.json (nodes, edges, layout coordinates, status counts)
    |
    v
Runway site generator: dashboard, chapters, dependency graph, paper
```

## Key Types

| Type | File | Description |
|------|------|-------------|
| `Node` | `Basic.lean` | Core node data: name, label, statement, proof, status, all metadata fields |
| `NodePart` | `Basic.lean` | A statement or proof part with LaTeX text, uses, excludes, and environment |
| `NodeStatus` | `Basic.lean` | The 6-status enum: notReady, ready, sorry, proven, fullyProven, mathlibReady |
| `Config` | `Attribute.lean` | Parsed configuration from `@[blueprint]` syntax (all options) |
| `BlueprintContent` | `Content.lean` | Union type: either a node or a module docstring, ordered by source position |
| `BlueprintInfo` | `RPC.lean` | Subset of node data returned to the VS Code infoview via RPC |

## Usage Examples

```lean
-- Main theorem with full metadata
@[blueprint "thm:main_result"
  (statement := /-- For every finite group $G$, the order of $G$ divides $|G|!$. -/)
  (keyDeclaration := true)
  (message := "Main theorem of the formalization")]
theorem main_result (G : Type*) [Group G] [Fintype G] : ... := by
  /-- We proceed by induction on the order of $G$. -/
  induction ...

-- Blocked lemma with sorry
@[blueprint "lem:helper"
  (priorityItem := true)
  (blocked := "Waiting for mathlib PR #12345")]
lemma helper_lemma : P := by
  sorry_using [some_dependency]

-- Definition with custom LaTeX environment
@[blueprint "def:widget"
  (latexEnv := "definition")
  (title := /-- Widget Construction -/)]
def widgetConstruction : Widget := ...

-- Ready for mathlib
@[blueprint "thm:upstream" (mathlibReady := true)]
theorem ready_for_mathlib : Q := by ...

-- Manual dependency specification
@[blueprint "thm:combined"
  (uses := [lemma_a, "lem:external", -irrelevant_lemma])
  (proofUses := [technical_lemma])]
theorem combined_result : R := by ...
```

## Commands

- **`#show_blueprint`**: Display the LaTeX output for the current module.
- **`#show_blueprint name`**: Display the LaTeX for a specific declaration (by Lean name or LaTeX label).
- **`#show_blueprint_json`**: Display the JSON representation of blueprint contents.
- **`blueprint?`**: Variant of `@[blueprint]` that traces the raw node data during elaboration.

## Options

- `set_option blueprint.ignoreUnknownConstants true`: Suppress errors for unresolved constants in `uses`/`proofUses` (useful during development when dependencies are not yet defined).
