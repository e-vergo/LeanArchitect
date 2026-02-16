# LeanArchitect

> **Fork of [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect)** for the Side-by-Side Blueprint toolchain.

This fork extends the `@[blueprint]` attribute with metadata options and manual status flags to support dashboard features and project management in the [Side-by-Side Blueprint](https://github.com/e-vergo/SLS-Strange-Loop-Station) toolchain.

**For full upstream documentation, see [hanwenzhu/LeanArchitect](https://github.com/hanwenzhu/LeanArchitect).**

## What Changed

| Upstream (hanwenzhu) | This Fork |
|---------------------|-----------|
| Basic `@[blueprint "label"]` syntax | Extended with 8 metadata + 2 manual status flags |
| Includes CLI executable and Lake facets | Metadata-only (CLI/facets moved to Dress) |
| Depends on Cli, SubVerso | Depends only on batteries |
| Self-contained tool | Component in toolchain: SubVerso -> LeanArchitect -> Dress -> Runway |

## SBS Extensions

### 8 Metadata Options

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

### 2 Manual Status Flags

| Option | Sets Status To | Color |
|--------|----------------|-------|
| `wip` | wip | Deep Teal (#0097A7) |
| `mathlibReady` | mathlibReady | Vivid Blue (#42A5F5) |

**Note:** `notReady` is the default status (no flag needed). `axiom` status is auto-detected for Lean `axiom` declarations and cannot be set manually.

### Usage Example

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

@[blueprint "lem:active" (wip := true)]
lemma activelyWorking : ... := sorry
```

## Key Files

| File | Purpose |
|------|---------|
| `Architect/Basic.lean` | `Node`, `NodePart`, `NodeStatus` types with manual `ToExpr` instance |
| `Architect/Attribute.lean` | `@[blueprint]` attribute syntax and elaboration with all options |
| `Architect/CollectUsed.lean` | Dependency inference from expression trees |

## Technical Notes

### Manual `ToExpr` Instance

The `Node` structure uses a manual `ToExpr` instance because Lean's derived instance for structures with default field values does not correctly serialize all fields through environment extensions.

### Dependency Inference

`CollectUsed.lean` provides `collectUsed` which traverses expression trees to extract dependencies. Downstream, Dress uses this via `Node.inferUses` to distinguish:

- **Statement dependencies**: Constants used in type signature (dashed edges in graph)
- **Proof dependencies**: Constants used in proof body (solid edges in graph)

### Backwards Compatibility

JSON parsing handles legacy status values:
- `"stated"` maps to `.notReady`
- `"inMathlib"` maps to `.mathlibReady`

## Integration

LeanArchitect is one component in the Side-by-Side Blueprint toolchain:

```
SubVerso -> LeanArchitect -> Dress -> Runway
```

For full documentation of the complete toolchain, see the [project README](https://github.com/e-vergo/SLS-Strange-Loop-Station).

## Installation

For full blueprint functionality, use [Dress](https://github.com/e-vergo/Dress) which re-exports LeanArchitect:

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

## License

Apache 2.0 - see [LICENSE](LICENSE).
