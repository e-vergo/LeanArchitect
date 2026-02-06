import Lean

open Lean

namespace Architect

/-! # Statement Validation

Lightweight LaTeX validation for `@[blueprint]` statement text.
Fires during elaboration and emits warnings (never errors) so builds are not broken.
-/

/-- A single validation diagnostic. -/
structure ValidationDiag where
  message : String
deriving Repr

/-- Check that a statement string is non-empty after trimming. -/
def validateStatementCompleteness (text : String) : Option ValidationDiag :=
  if text.trimAscii.isEmpty then
    some { message := "blueprint statement is empty" }
  else
    none

/-- Count unbalanced `{` / `}` in LaTeX text, skipping escaped `\{` and `\}`. -/
def validateBraceBalance (text : String) : Option ValidationDiag :=
  let chars := text.toList
  let rec go : List Char → Int → Int
    | [], depth => depth
    -- Skip escaped braces: \{ and \}
    | '\\' :: '{' :: rest, depth => go rest depth
    | '\\' :: '}' :: rest, depth => go rest depth
    -- Skip escaped backslash followed by brace
    | '\\' :: '\\' :: rest, depth => go rest depth
    | '{' :: rest, depth => go rest (depth + 1)
    | '}' :: rest, depth => go rest (depth - 1)
    | _ :: rest, depth => go rest depth
  let balance := go chars 0
  if balance == 0 then none
  else if balance > 0 then
    some { message := s!"blueprint statement has {balance} unmatched opening brace(s)" }
  else
    some { message := s!"blueprint statement has {-balance} unmatched closing brace(s)" }

/-- Check that inline math delimiters `$...$` are balanced.
    Skips escaped `\$`. Does not validate display math `$$...$$`
    (which is rare in blueprint statements). -/
def validateMathDelimiters (text : String) : Array ValidationDiag := Id.run do
  let mut diags : Array ValidationDiag := #[]
  -- Check inline $ delimiters (skip \$)
  let chars := text.toList
  let dollarCount := countUnescapedDollars chars
  if dollarCount % 2 != 0 then
    diags := diags.push { message := s!"blueprint statement has unbalanced '$' math delimiter ({dollarCount} unescaped '$' found)" }
  -- Check \[ ... \] display math
  let openCount := countDisplayOpen text.toList
  let closeCount := countDisplayClose text.toList
  if openCount != closeCount then
    diags := diags.push { message := s!"blueprint statement has unbalanced display math delimiters ({openCount} open vs {closeCount} close)" }
  return diags
where
  /-- Count `$` characters that are not preceded by `\`. -/
  countUnescapedDollars : List Char → Nat
    | [] => 0
    | '\\' :: '$' :: rest => countUnescapedDollars rest
    | '$' :: rest => 1 + countUnescapedDollars rest
    | _ :: rest => countUnescapedDollars rest
  /-- Count `\[` (display math open) occurrences. -/
  countDisplayOpen : List Char → Nat
    | [] => 0
    | '\\' :: '[' :: rest => 1 + countDisplayOpen rest
    | _ :: rest => countDisplayOpen rest
  /-- Count `\]` (display math close) occurrences. -/
  countDisplayClose : List Char → Nat
    | [] => 0
    | '\\' :: ']' :: rest => 1 + countDisplayClose rest
    | _ :: rest => countDisplayClose rest

/-- Run all validation checks on a statement string.
    Returns the collected diagnostics (empty = all good). -/
def validateStatement (text : String) : Array ValidationDiag := Id.run do
  let mut diags : Array ValidationDiag := #[]
  if let some d := validateStatementCompleteness text then
    diags := diags.push d
    -- If empty, skip further checks (they'd be vacuously noisy)
    return diags
  if let some d := validateBraceBalance text then
    diags := diags.push d
  diags := diags ++ validateMathDelimiters text
  return diags

end Architect
