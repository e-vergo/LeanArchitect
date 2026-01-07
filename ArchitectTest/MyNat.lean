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
  (uses := [add]) -- Manually added dependency
  (statement := /-- Natural number multiplication. -/)]
def mul (a b : MyNat) : MyNat := sorry

@[blueprint
  (statement := /-- For any natural numbers $a, b$, $a * b = b * a$. -/)]
theorem mul_comm (a b : MyNat) : mul a b = mul b a := by sorry

/-! ## Fermat's Last Theorem -/

@[blueprint "thm:flt"
  (statement := /-- Fermat's last theorem. -/)
  (title := "Taylor-Wiles")
  -- You may override the inferred statement dependencies by `uses`.
  (uses := [mul])
  -- Alternatively to docstring tactics and `using` tactics, proof metadata can be specified
  -- by `proof` and `proofUses`.
  (proof := /-- See \cite{Wiles1995, Taylor-Wiles1995}. -/) (proofUses := [mul_comm])
  (notReady := true) (discussion := 1)]
theorem flt : (sorry : Prop) := sorry

end MyNat

open Lean Architect in
run_meta
  let json ← mainModuleToJson
  let currFile ← (← getSrcSearchPath).findWithExt "lean" (← getEnv).header.mainModule
  -- NB: some locations are `null` in v4.22.0 (why?)
  assert! json == json%
[{"type": "node",
  "data":
  {"title": null,
   "statement":
   {"usesLabels": [],
    "uses": [],
    "text": "Natural numbers.",
    "latexEnv": "definition",
    "excludesLabels": [],
    "excludes": []},
   "proof": null,
   "notReady": false,
   "name": "MyNat",
   "location":
   {"range": {"pos": {"line": 3, "column": 0}, "endPos": {"line": 6, "column": 24}}, "module": "ArchitectTest.MyNat"},
   "latexLabel": "MyNat",
   "hasLean": true,
   "file": $(currFile),
   "discussion": null}},
 {"type": "node",
  "data":
  {"title": null,
   "statement":
   {"usesLabels": [],
    "uses": [],
    "text": "Natural number addition.",
    "latexEnv": "definition",
    "excludesLabels": [],
    "excludes": []},
   "proof": null,
   "notReady": false,
   "name": "MyNat.add",
   "location":
   {"range": {"pos": {"line": 12, "column": 0}, "endPos": {"line": 19, "column": 28}}, "module": "ArchitectTest.MyNat"},
   "latexLabel": "def:nat-add",
   "hasLean": true,
   "file": $(currFile),
   "discussion": null}},
 {"type": "node",
  "data":
  {"title": null,
   "statement":
   {"usesLabels": [],
    "uses": ["MyNat.add"],
    "text": "Natural number multiplication.",
    "latexEnv": "definition",
    "excludesLabels": [],
    "excludes": []},
   "proof": null,
   "notReady": false,
   "name": "MyNat.mul",
   "location":
   {"range": {"pos": {"line": 48, "column": 0}, "endPos": {"line": 51, "column": 38}}, "module": "ArchitectTest.MyNat"},
   "latexLabel": "MyNat.mul",
   "hasLean": true,
   "file": $(currFile),
   "discussion": null}},
 {"type": "node",
  "data":
  {"title": "Taylor-Wiles",
   "statement":
   {"usesLabels": [],
    "uses": ["MyNat.mul"],
    "text": "Fermat's last theorem.",
    "latexEnv": "theorem",
    "excludesLabels": [],
    "excludes": []},
   "proof":
   {"usesLabels": [],
    "uses": ["MyNat.mul_comm"],
    "text": "See \\cite{Wiles1995, Taylor-Wiles1995}.",
    "latexEnv": "proof",
    "excludesLabels": [],
    "excludes": []},
   "notReady": true,
   "name": "MyNat.flt",
   "location": null,
   "latexLabel": "thm:flt",
   "hasLean": true,
   "file": $(currFile),
   "discussion": 1}},
 {"type": "node",
  "data":
  {"title": null,
   "statement":
   {"usesLabels": [],
    "uses": [],
    "text": "For any natural numbers $a, b$, $a + b = b + a$.",
    "latexEnv": "theorem",
    "excludesLabels": [],
    "excludes": []},
   "proof": {"usesLabels": [], "uses": [], "text": "", "latexEnv": "proof", "excludesLabels": [], "excludes": []},
   "notReady": false,
   "name": "MyNat.add_comm",
   "location": null,
   "latexLabel": "MyNat.add_comm",
   "hasLean": true,
   "file": $(currFile),
   "discussion": null}},
 {"type": "node",
  "data":
  {"title": null,
   "statement":
   {"usesLabels": [],
    "uses": [],
    "text": "For any natural numbers $a, b$, $(a + 1) + b = (a + b) + 1$.",
    "latexEnv": "theorem",
    "excludesLabels": [],
    "excludes": []},
   "proof": {"usesLabels": [], "uses": [], "text": "", "latexEnv": "proof", "excludesLabels": [], "excludes": []},
   "notReady": false,
   "name": "MyNat.succ_add",
   "location": null,
   "latexLabel": "MyNat.succ_add",
   "hasLean": true,
   "file": $(currFile),
   "discussion": null}},
 {"type": "node",
  "data":
  {"title": null,
   "statement":
   {"usesLabels": [],
    "uses": [],
    "text": "For any natural number $a$, $0 + a = a$, where $+$ is \\cref{def:nat-add}.",
    "latexEnv": "theorem",
    "excludesLabels": [],
    "excludes": []},
   "proof": {"usesLabels": [], "uses": [], "text": "", "latexEnv": "proof", "excludesLabels": [], "excludes": []},
   "notReady": false,
   "name": "MyNat.zero_add",
   "location": null,
   "latexLabel": "MyNat.zero_add",
   "hasLean": true,
   "file": $(currFile),
   "discussion": null}},
 {"type": "node",
  "data":
  {"title": null,
   "statement":
   {"usesLabels": [],
    "uses": [],
    "text": "For any natural numbers $a, b$, $a * b = b * a$.",
    "latexEnv": "theorem",
    "excludesLabels": [],
    "excludes": []},
   "proof": {"usesLabels": [], "uses": [], "text": "", "latexEnv": "proof", "excludesLabels": [], "excludes": []},
   "notReady": false,
   "name": "MyNat.mul_comm",
   "location": null,
   "latexLabel": "MyNat.mul_comm",
   "hasLean": true,
   "file": $(currFile),
   "discussion": null}}]
