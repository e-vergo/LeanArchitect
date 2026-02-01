# LeanArchitect

Fork of [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect) extended into a general-purpose tagging system for the [Side-by-Side Blueprint](https://github.com/e-vergo/Side-By-Side-Blueprint) toolchain.

## Fork Extensions

This fork adds **8 metadata options** and **3 manual status flags** to the `@[blueprint]` attribute for dashboard and project management.

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

### Manual Status Flags (3)

| Option | Sets Status To | Color |
|--------|----------------|-------|
| `notReady` | notReady | Sandy Brown (#F4A460) |
| `ready` | ready | Light Sea Green (#20B2AA) |
| `mathlibReady` | mathlibReady | Light Blue (#87CEEB) |

### Example

```lean
@[blueprint "thm:main"
  (keyDeclaration := true)
  (message := "Main theorem of the formalization")]
theorem mainTheorem : ... := ...

@[blueprint "lem:helper"
  (priorityItem := true)
  (blocked := "Waiting for mathlib PR #12345")]
lemma helperLemma : ... := sorry

@[blueprint "thm:upstream" (mathlibReady := true)]
theorem readyForMathlib : ... := ...
```

## 6-Status Color Model

| Status | Color | Hex | Source |
|--------|-------|-----|--------|
| `notReady` | Sandy Brown | #F4A460 | Default or manual `(notReady := true)` |
| `ready` | Light Sea Green | #20B2AA | Manual `(ready := true)` |
| `sorry` | Dark Red | #8B0000 | Auto-detected: proof contains `sorryAx` |
| `proven` | Light Green | #90EE90 | Auto-detected: complete proof |
| `fullyProven` | Forest Green | #228B22 | Auto-computed: proven + all ancestors proven |
| `mathlibReady` | Light Blue | #87CEEB | Manual `(mathlibReady := true)` |

**Priority order** (manual always wins):
1. `mathlibReady` (manual) - highest
2. `ready` (manual)
3. `notReady` (manual, if explicitly set)
4. `fullyProven` (auto-computed from graph)
5. `sorry` (auto-detected via `sorryAx`)
6. `proven` (auto-detected)
7. `notReady` (default)

The `fullyProven` status is computed via O(V+E) graph traversal during artifact generation and cannot be set manually.

## Key Files

| File | Purpose |
|------|---------|
| `Architect/Basic.lean` | `Node`, `NodePart`, `NodeStatus` types with manual `ToExpr` instance |
| `Architect/Attribute.lean` | `@[blueprint]` attribute syntax and elaboration |
| `Architect/CollectUsed.lean` | Dependency inference from expression trees |

## Technical Notes

### Manual `ToExpr` Instance

The `Node` structure uses a manual `ToExpr` instance because Lean's derived instance for structures with default field values does not correctly serialize all fields through environment extensions:

```lean
instance : ToExpr Node where
  toTypeExpr := mkConst ``Node
  toExpr n := Lean.mkAppN (mkConst ``Node.mk) #[
    toExpr n.name,
    toExpr n.latexLabel,
    toExpr n.statement,
    toExpr n.proof,
    toExpr n.status,
    -- all 14 fields explicitly serialized
  ]
```

### Dependency Inference

`CollectUsed.lean` provides `collectUsed` which traverses expression trees to infer dependencies:
- **Statement dependencies**: Constants used in type signature (dashed edges in graph)
- **Proof dependencies**: Constants used in proof body (solid edges in graph)

```lean
@[blueprint "lem:a"]
lemma lemA : ... := ...

@[blueprint "lem:b"]
lemma lemB : ... := lemA  -- Automatically depends on lem:a
```

### Backwards Compatibility

JSON parsing handles legacy status values:
- `"stated"` maps to `.notReady`
- `"inMathlib"` maps to `.mathlibReady`

## Installation

For full blueprint functionality, use [Dress](https://github.com/e-vergo/Dress) (which re-exports LeanArchitect):

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

## Architectural Changes from Upstream

This fork uses a **metadata-only architecture**. Artifact generation has been moved to [Dress](https://github.com/e-vergo/Dress):

| Removed from LeanArchitect | Now in Dress |
|---------------------------|--------------|
| `Main.lean` CLI executable | `Main.lean` |
| Lake facets (`blueprint`, `blueprintJson`) | Dress facets |
| HTML/LaTeX rendering | SubVerso/Verso integration |
| `Cli` dependency | Dress dependency |

**Dependency chain:**
```
SubVerso -> LeanArchitect -> Dress -> Runway
```

LeanArchitect only depends on `batteries`. Fast compilation, no SubVerso/Verso overhead.

## Upstream

For original LeanArchitect functionality and documentation, see [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect).

## License

Apache 2.0 - see [LICENSE](LICENSE).
