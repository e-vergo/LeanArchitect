/-
Copyright (c) 2025 LeanArchitect contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Blueprint Hook State

This module contains the state initialization for the blueprint elaboration hook.
It's in a separate module to avoid the "cannot evaluate init in same module" issue.
-/

namespace Architect

/-- IO.Ref to track if we're inside the blueprint capture hook to prevent recursion.
    Using IO.Ref to avoid infinite recursion when our elab_rules call elabCommandTopLevel. -/
initialize blueprintCaptureHookRef : IO.Ref Bool ← IO.mkRef false

/-- Track declarations processed for .tex generation in this module.
    Stores `(name, latexLabel)` pairs for later module header generation.
    Reset at module boundaries by the elaboration hook. -/
initialize texGeneratedDeclsRef : IO.Ref (Array (Lean.Name × String)) ← IO.mkRef #[]

end Architect
