import Lean
import Architect.Basic

/-! # Statement-Signature Cross-Referencing

Heuristic matching between LaTeX statement text and Lean type signatures.
Surfaces warnings for obvious drift between the informal statement and the formal definition.

All checks are conservative -- better to miss real drift than to spam false warnings.
-/

open Lean

namespace Architect

/-! ## String Utilities -/

/-- Lowercase a string (ASCII only). -/
private def toLowerAscii (s : String) : String :=
  String.ofList (s.toList.map fun c =>
    if 'A' ≤ c && c ≤ 'Z' then Char.ofNat (c.toNat - 'A'.toNat + 'a'.toNat) else c)

/-- Check if `needle` is a substring of `haystack`. -/
private def strContains (haystack : String) (needle : String) : Bool :=
  if needle.isEmpty then true
  else if needle.length > haystack.length then false
  else Id.run do
    let haystackChars := haystack.toList
    let needleChars := needle.toList
    let limit := haystackChars.length - needleChars.length
    for i in [:limit + 1] do
      let slice := haystackChars.drop i |>.take needleChars.length
      if slice == needleChars then return true
    return false

/-- Split a string into words (ASCII alphanumeric sequences, lowercased). -/
private def splitWords (s : String) : Array String := Id.run do
  let mut result : Array String := #[]
  let mut current : String := ""
  for c in s.toList do
    if c.isAlphanum then
      current := current.push c
    else
      if current.length > 0 then
        result := result.push (toLowerAscii current)
        current := ""
  if current.length > 0 then
    result := result.push (toLowerAscii current)
  return result

/-! ## Signature Components (from Lean ConstantInfo) -/

/-- Components extracted from a Lean constant's type signature. -/
structure SignatureComponents where
  /-- Quantified variable names with their types (from forall telescope). -/
  quantifiedVars : Array (Name × Expr)
  /-- Hypotheses: propositions in the forall telescope. -/
  hypotheses : Array (Name × Expr)
  /-- The conclusion type (after all forall binders). -/
  conclusion : Expr
  /-- Key identifiers appearing in the type (const names from the signature). -/
  keyIdentifiers : Array Name
deriving Inhabited

/-- Known mathematical structure names that are worth cross-referencing.
    Only includes common top-level structures to avoid noise. -/
private def mathStructureNames : Array String :=
  -- Only algebraic structures whose presence in a type signature strongly implies
  -- the statement should mention them. Excludes:
  -- - Basic types (Nat, Int, Bool, List, Array, Set, Finset, Multiset)
  -- - Implementation types (Matrix, Polynomial) often used implicitly
  -- - Types that appear as notation (Real = ℝ, Complex = ℂ)
  #["Group", "Ring", "Field", "Module", "Monoid", "Semiring",
    "CommGroup", "CommRing", "CommMonoid", "Algebra",
    "TopologicalSpace", "MetricSpace", "NormedSpace",
    "LinearMap", "RingHom", "MonoidHom",
    "Subgroup", "Ideal", "Submodule"]

/-- Extract the last component of a Lean name as a string. -/
private def nameLastComponent (n : Name) : String :=
  match n with
  | .str _ s => s
  | .num _ n => toString n
  | .anonymous => ""

/-- Check if a name corresponds to a known mathematical structure. -/
private def isMathStructure (n : Name) : Bool :=
  let last := nameLastComponent n
  mathStructureNames.any (· == last)

/-- Collect all constant names used in an expression (iterative worklist). -/
private partial def collectExprConsts (e : Expr) : Array Name := Id.run do
  let mut result : Array Name := #[]
  let mut stack : Array Expr := #[e]
  let mut visited : Lean.NameSet := {}
  while h : stack.size > 0 do
    let expr := stack[stack.size - 1]
    stack := stack.pop
    match expr with
    | .const n _ =>
      unless visited.contains n do
        visited := visited.insert n
        result := result.push n
    | .app f a =>
      stack := stack.push f
      stack := stack.push a
    | .lam _ t b _ =>
      stack := stack.push t
      stack := stack.push b
    | .forallE _ t b _ =>
      stack := stack.push t
      stack := stack.push b
    | .letE _ t v b _ =>
      stack := stack.push t
      stack := stack.push v
      stack := stack.push b
    | .mdata _ e =>
      stack := stack.push e
    | .proj _ _ e =>
      stack := stack.push e
    | _ => pure ()
  return result

/-- Extract signature components from a Lean constant's type.
    Walks the forall telescope and collects binders, hypotheses, conclusion, and key identifiers. -/
def extractSignatureComponents (type : Expr) : SignatureComponents := Id.run do
  let mut quantifiedVars : Array (Name × Expr) := #[]
  let mut hypotheses : Array (Name × Expr) := #[]
  let mut keyIds : Lean.NameSet := {}
  let mut currentType := type

  -- Walk the forall telescope
  while currentType.isForall do
    match currentType with
    | .forallE name binderType body _bi =>
      -- Collect const names from the binder type
      for c in collectExprConsts binderType do
        keyIds := keyIds.insert c
      -- All binders count as quantified variables
      quantifiedVars := quantifiedVars.push (name, binderType)
      -- If the name starts with 'h' or 'H', it's likely a hypothesis
      let nameStr := name.toString
      if nameStr.startsWith "h" || nameStr.startsWith "H" || name.isAnonymous then
        hypotheses := hypotheses.push (name, binderType)
      currentType := body
    | _ => break

  -- Collect consts from the conclusion
  for c in collectExprConsts currentType do
    keyIds := keyIds.insert c

  -- Filter to only math-relevant identifiers using foldl
  let mathKeys := keyIds.foldl (init := #[]) fun acc n =>
    if isMathStructure n then acc.push n else acc

  return {
    quantifiedVars
    hypotheses
    conclusion := currentType
    keyIdentifiers := mathKeys
  }

/-! ## Statement Components (from LaTeX text) -/

/-- Components extracted from a LaTeX statement string. -/
structure StatementComponents where
  /-- Mathematical identifiers found in $...$ or \[...\] math mode. -/
  mathIdentifiers : Array String
  /-- Approximate count of quantifier-like phrases. -/
  quantifierCount : Nat
  /-- Whether the statement contains implication language. -/
  hasImplication : Bool
  /-- Whether the statement contains conjunction language. -/
  hasConjunction : Bool
  /-- Whether the statement contains existential language. -/
  hasExistential : Bool
  /-- All words from the statement (lowercased), for general matching. -/
  allWords : Array String
deriving Inhabited

/-- Extract text between inline math delimiters $...$, skipping \$. -/
private def extractInlineMath (text : String) : Array String := Id.run do
  let chars := text.toList
  let mut result : Array String := #[]
  let mut inMath := false
  let mut current : List Char := []
  let mut prevBackslash := false
  for c in chars do
    if prevBackslash then
      if inMath then current := c :: current
      prevBackslash := false
    else if c == '\\' then
      prevBackslash := true
      if inMath then current := c :: current
    else if c == '$' then
      if inMath then
        -- Closing math
        let mathText := String.ofList current.reverse
        let trimmed := mathText.trimAscii.toString
        if !trimmed.isEmpty then
          result := result.push trimmed
        current := []
        inMath := false
      else
        -- Opening math
        inMath := true
        current := []
    else
      if inMath then current := c :: current
  return result

/-- Extract individual identifiers from a math mode string.
    Looks for sequences of alphabetic characters that look like variable or type names. -/
private def extractMathIdentifiers (mathText : String) : Array String := Id.run do
  let mut result : Array String := #[]
  let mut current : String := ""
  for c in mathText.toList do
    if c.isAlpha then
      current := current.push c
    else
      if current.length > 0 then
        result := result.push current
        current := ""
  if current.length > 0 then
    result := result.push current
  return result

/-- Check if a word indicates implication. -/
private def isImplicationWord (w : String) : Bool :=
  w == "implies" || w == "then" || w == "therefore" || w == "hence" ||
  w == "thus" || w == "consequently"

/-- Check if a word indicates conjunction. -/
private def isConjunctionWord (w : String) : Bool :=
  w == "and" || w == "both" || w == "moreover" || w == "furthermore"

/-- Check if a word indicates existence. -/
private def isExistentialWord (w : String) : Bool :=
  w == "exists" || w == "there" || w == "some" || w == "unique"

/-- Extract statement components from a LaTeX statement text. -/
def extractStatementComponents (text : String) : StatementComponents := Id.run do
  -- Extract math-mode content
  let mathBlocks := extractInlineMath text
  let mut mathIds : Array String := #[]
  for block in mathBlocks do
    mathIds := mathIds ++ extractMathIdentifiers block

  -- Tokenize the full text
  let words := splitWords text

  -- Count quantifiers
  -- Look for "for all", "for every", etc. and unicode quantifiers
  let mut quantifierCount : Nat := 0
  for i in [:words.size] do
    let w := words[i]!
    if w == "forall" || w == "foreach" then
      quantifierCount := quantifierCount + 1
    else if w == "for" && i + 1 < words.size then
      let next := words[i + 1]!
      if next == "all" || next == "every" || next == "any" || next == "each" then
        quantifierCount := quantifierCount + 1
  -- Also check for unicode quantifiers in original text
  for c in text.toList do
    if c == '∀' then quantifierCount := quantifierCount + 1
    if c == '∃' then quantifierCount := quantifierCount + 1

  -- Check for implication language
  let hasImplication := words.any isImplicationWord ||
    strContains text "\\implies" || strContains text "\\Rightarrow" ||
    strContains text "if " || strContains text "\\iff" ||
    strContains text "↔" || strContains text "→"

  -- Check for conjunction
  let hasConjunction := words.any isConjunctionWord ||
    strContains text "\\land" || strContains text "\\wedge" ||
    strContains text "∧"

  -- Check for existential
  let hasExistential := words.any isExistentialWord ||
    strContains text "\\exists" || strContains text "∃"

  return {
    mathIdentifiers := mathIds
    quantifierCount
    hasImplication
    hasConjunction
    hasExistential
    allWords := words
  }

/-! ## Cross-Reference Diagnostics -/

/-- A cross-reference diagnostic. -/
structure CrossRefDiag where
  message : String
deriving Repr

/-- Check if any word in the statement plausibly references a math structure name.
    Uses case-insensitive substring matching. -/
private def statementMentionsStructure (words : Array String) (structName : String) : Bool :=
  let lower := toLowerAscii structName
  words.any fun w => w == lower || strContains w lower

/-- Rule 1: If Lean signature has key math structures, check statement mentions them. -/
private def checkMathStructureMatch
    (sig : SignatureComponents) (stmt : StatementComponents) : Array CrossRefDiag := Id.run do
  let mut diags : Array CrossRefDiag := #[]
  for name in sig.keyIdentifiers do
    let structName := nameLastComponent name
    unless statementMentionsStructure stmt.allWords structName do
      -- Also check math-mode identifiers
      let inMath := stmt.mathIdentifiers.any fun id =>
        toLowerAscii id == toLowerAscii structName ||
        strContains (toLowerAscii id) (toLowerAscii structName)
      unless inMath do
        diags := diags.push {
          message := s!"cross-reference: Lean signature mentions '{structName}' but statement does not reference it"
        }
  return diags

/-- Rule 2: Quantifier count mismatch.
    Only flags when the statement has zero quantifiers but Lean has 3+.
    This catches obvious omissions while avoiding noise. -/
private def checkQuantifierBalance
    (sig : SignatureComponents) (stmt : StatementComponents) : Option CrossRefDiag :=
  let leanQuantifiers := sig.quantifiedVars.size
  let stmtQuantifiers := stmt.quantifierCount
  -- Only flag for very large mismatches (5+ binders with zero statement quantifiers)
  if leanQuantifiers ≥ 5 && stmtQuantifiers == 0 then
    some { message := s!"cross-reference: Lean signature has {leanQuantifiers} quantified variables but statement has no quantifier language" }
  else
    none

/-- Rule 3: Declaration name check.
    If the Lean declaration name (lowercased, split by underscores) has meaningful tokens
    that don't appear anywhere in the statement, flag it. -/
private def checkNameRelevance
    (declName : Name) (stmt : StatementComponents) : Option CrossRefDiag :=
  let nameStr := nameLastComponent declName
  -- Split by underscores and filter short/common tokens
  let tokens := (nameStr.splitOn "_").filter fun t =>
    t.length > 3 && t != "theorem" && t != "lemma" && t != "main" && t != "aux" &&
    t != "left" && t != "right" && t != "this" && t != "that" && t != "have" &&
    t != "show" && t != "proof" && t != "demo" && t != "test" && t != "chain" &&
    t != "node" && t != "true" && t != "false"
  -- Only flag when NONE of the meaningful tokens match, and there are at least 2
  if tokens.length == 0 then none
  else
    let matchCount := tokens.filter (fun t =>
      stmt.allWords.any fun w => strContains w (toLowerAscii t)) |>.length
    if tokens.length ≥ 2 && matchCount == 0 then
      some { message := s!"cross-reference: declaration name '{nameStr}' tokens [{", ".intercalate tokens}] not found in statement text" }
    else
      none

/-- Run all cross-reference heuristics.
    Returns an array of diagnostics (empty = no issues detected). -/
def crossReferenceCheck
    (declName : Name) (sig : SignatureComponents) (stmt : StatementComponents)
    : Array CrossRefDiag := Id.run do
  let mut diags : Array CrossRefDiag := #[]

  -- Skip if statement has very few words (probably just a placeholder)
  if stmt.allWords.size < 3 then return diags

  -- Rule 1: Math structure matching
  diags := diags ++ checkMathStructureMatch sig stmt

  -- Rule 2: Quantifier balance
  if let some d := checkQuantifierBalance sig stmt then
    diags := diags.push d

  -- Rule 3: Declaration name relevance
  if let some d := checkNameRelevance declName stmt then
    diags := diags.push d

  return diags

end Architect
