import Architect

@[blueprint (statement := /-- Natural numbers. -/)]
inductive MyNat : Type where
  | zero : MyNat
  | succ : MyNat → MyNat

set_option warn.sorry false

namespace MyNat

@[blueprint
  -- You may manually specify a \label
  "def:nat-add"
  (statement := /-- Natural number addition. -/)]
def add (a b : MyNat) : MyNat :=
  match b with
  | zero => a
  | succ b => succ (add a b)

@[simp, blueprint
  (statement := /-- For any natural number $a$, $0 + a = a$, where $+$ is \cref{def:nat-add}. -/)]
theorem zero_add (a : MyNat) : add zero a = a := by
  /-- The proof follows by induction. -/
  induction a <;> simp [*, add]

@[blueprint
  (statement := /-- For any natural numbers $a, b$, $(a + 1) + b = (a + b) + 1$. -/)]
theorem succ_add (a b : MyNat) : add (succ a) b = succ (add a b) := by
  /-- Proof by induction on $b$. -/
  -- If the proof contains sorry, the `\leanok` command will not be added
  sorry

@[blueprint
  (statement := /-- For any natural numbers $a, b$, $a + b = b + a$. -/)]
theorem add_comm (a b : MyNat) : add a b = add b a := by
  induction b with
  | zero =>
    have := trivial
    /-- The base case follows from \cref{MyNat.zero_add}. -/
    simp [add]
  | succ b ih =>
    /-- The inductive case follows from \cref{MyNat.succ_add}. -/
    sorry_using [succ_add]  -- the `sorry_using` tactic declares that the proof uses succ_add

/-! ## Multiplication -/

@[blueprint
  (statement := /-- Natural number multiplication. -/)]
def mul (a b : MyNat) : MyNat := sorry

@[blueprint
  (statement := /-- For any natural numbers $a, b$, $a * b = b * a$. -/)]
theorem mul_comm (a b : MyNat) : mul a b = mul b a := by sorry

/-! ## Fermat's Last Theorem -/

@[blueprint "thm:flt"
  (statement := /-- Fermat's last theorem. -/)
  (title := "Taylor-Wiles")
  (proof := /-- See \cite{Wiles1995, Taylor-Wiles1995}. -/)
  (notReady := true) (discussion := 1)]
theorem flt : (sorry : Prop) := sorry

end MyNat

-- Test that blueprint nodes are stored in the environment extension
open Lean Architect in
run_meta do
  let env ← getEnv
  -- Check that all the blueprint nodes exist
  assert! (blueprintExt.find? env ``MyNat).isSome
  assert! (blueprintExt.find? env ``MyNat.add).isSome
  assert! (blueprintExt.find? env ``MyNat.zero_add).isSome
  assert! (blueprintExt.find? env ``MyNat.succ_add).isSome
  assert! (blueprintExt.find? env ``MyNat.add_comm).isSome
  assert! (blueprintExt.find? env ``MyNat.mul).isSome
  assert! (blueprintExt.find? env ``MyNat.mul_comm).isSome
  assert! (blueprintExt.find? env ``MyNat.flt).isSome
  -- Check some specific node properties
  let flt := (blueprintExt.find? env ``MyNat.flt).get!
  assert! flt.latexLabel == "thm:flt"
  assert! flt.title == some "Taylor-Wiles"
  assert! flt.status == .notReady
  assert! flt.statusExplicit == true
  assert! flt.discussion == some 1
  assert! flt.proof.isSome
