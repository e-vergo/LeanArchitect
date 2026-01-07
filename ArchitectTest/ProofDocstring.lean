import Architect
import ArchitectTest.MyNat

-- Test regular proof docstring

@[blueprint] theorem test : True := by
  /-- my test -/
  trivial

/-- info: my test -/
#guard_msgs in
open Lean Architect in
run_meta do
  let doc := getProofDocString (← getEnv) ``test
  logInfo doc

-- Test imported proof docstring (the presistence of proofDocStringExt)

/-- info: The base case follows from \cref{MyNat.zero_add}.

The inductive case follows from \cref{MyNat.succ_add}. -/
#guard_msgs in
open Lean Architect in
run_meta do
  let doc := getProofDocString (← getEnv) ``MyNat.add_comm
  logInfo doc
