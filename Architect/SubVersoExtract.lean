/-
Copyright (c) 2025 LeanArchitect contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Module
import SubVerso.Highlighting

/-!
# SubVerso Module Extraction

This module provides functionality to call `subverso-extract-mod` and parse its output
to extract highlighted code for Lean modules.

The `subverso-extract-mod` CLI tool outputs JSON with module items containing:
- Source range information
- Syntax kind
- Names defined by the item
- Highlighted code (using SubVerso's deduplicated export format)
-/

open Lean System
open SubVerso.Module
open SubVerso.Highlighting

namespace Architect.SubVersoExtract

/-- Default relative path to the subverso-extract-mod executable from project root. -/
def defaultExtractModPath : FilePath :=
  ".lake" / "packages" / "subverso" / ".lake" / "build" / "bin" / "subverso-extract-mod"

/-- Find the subverso-extract-mod executable.
    Searches in order:
    1. The provided explicit path (if any)
    2. Default location relative to current working directory
    3. PATH environment variable
    Returns the path if found and executable, or none otherwise. -/
def findExtractModExecutable (explicitPath? : Option FilePath := none) : IO (Option FilePath) := do
  -- Try explicit path first
  if let some path := explicitPath? then
    if ← path.pathExists then
      return some path

  -- Try default location relative to cwd
  let cwd ← IO.currentDir
  let defaultPath := cwd / defaultExtractModPath
  if ← defaultPath.pathExists then
    return some defaultPath

  -- Try searching in LAKE_HOME if set
  if let some lakeHome := (← IO.getEnv "LAKE_HOME") then
    let lakePath : FilePath := lakeHome
    let subversoPath := lakePath / "packages" / "subverso" / ".lake" / "build" / "bin" / "subverso-extract-mod"
    if ← subversoPath.pathExists then
      return some subversoPath

  -- Not found
  return none

/-- Run subverso-extract-mod on a module and return the raw JSON output.
    Uses `lake exe` to ensure the correct toolchain and environment.
    Returns none if the command fails. -/
def runExtractMod (moduleName : Name) (projectRoot? : Option FilePath := none)
    : IO (Option String) := do
  -- Use `lake exe subverso-extract-mod` to ensure correct toolchain
  let proc : IO.Process.SpawnArgs := {
    cmd := "lake"
    args := #["exe", "subverso-extract-mod", moduleName.toString]
    cwd := projectRoot?
    stdin := .null
    stdout := .piped
    stderr := .piped
  }

  try
    let child ← IO.Process.spawn proc
    let stdout ← child.stdout.readToEnd
    let stderr ← child.stderr.readToEnd
    let exitCode ← child.wait

    if exitCode != 0 then
      IO.eprintln s!"Warning: subverso-extract-mod failed for {moduleName}: {stderr}"
      return none

    return some stdout
  catch e =>
    IO.eprintln s!"Warning: Failed to run subverso-extract-mod: {e}"
    return none

/-- Extract module highlighting by calling subverso-extract-mod and parsing the JSON output.
    Returns the array of ModuleItems, or none on failure. -/
def extractModuleHighlighting (moduleName : Name) (projectRoot? : Option FilePath := none)
    : IO (Option (Array ModuleItem)) := do
  let some jsonStr ← runExtractMod moduleName projectRoot?
    | return none

  -- Parse JSON
  let json := Json.parse jsonStr
  match json with
  | .error e =>
    IO.eprintln s!"Warning: Failed to parse JSON from subverso-extract-mod: {e}"
    return none
  | .ok json =>
    -- Parse as Module using SubVerso's FromJson instance
    match Module.fromJson? json with
    | .error e =>
      IO.eprintln s!"Warning: Failed to parse Module from JSON: {e}"
      return none
    | .ok mod =>
      return some mod.items

/-- Build a NameMap from declaration names to their highlighted code.
    For each ModuleItem, maps each name in `defines` to the item's `code`. -/
def buildHighlightingMap (items : Array ModuleItem) : NameMap Highlighted :=
  items.foldl (init := {}) fun acc item =>
    item.defines.foldl (init := acc) fun acc' name =>
      acc'.insert name item.code

/-- Load highlighting from a pre-computed JSON file (from Lake facet cache or Hook elaboration).
    Handles both the old `subverso-extract-mod` format and the new Hook.lean format.
    Both formats use SubVerso.Module.Module JSON structure.
    This is the fast path when highlighting has already been captured. -/
def loadHighlightingFromFile (path : String) : IO (NameMap Highlighted) := do
  let filePath : FilePath := path
  if !(← filePath.pathExists) then
    return {}
  let contents ← IO.FS.readFile filePath
  match Json.parse contents with
  | .error e =>
    IO.eprintln s!"Warning: Failed to parse highlighted JSON from {path}: {e}"
    return {}
  | .ok json =>
    match Module.fromJson? json with
    | .error e =>
      IO.eprintln s!"Warning: Failed to parse Module from JSON: {e}"
      return {}
    | .ok mod =>
      return buildHighlightingMap mod.items

/-- Extract module highlighting and build a NameMap in one step.
    This is the main entry point for getting highlighted code for a module's declarations.

    Returns an empty map on failure (logs warnings to stderr). -/
def extractHighlightingMap (moduleName : Name) (projectRoot? : Option FilePath := none)
    : IO (NameMap Highlighted) := do
  let some items ← extractModuleHighlighting moduleName projectRoot?
    | return {}
  return buildHighlightingMap items

/-- Compute the path for a module's highlighting JSON file.
    Returns `.lake/build/highlighted/{Module/Path}.json`. -/
def getHighlightingPath (buildDir : FilePath) (moduleName : Name) : FilePath :=
  let modulePath := moduleName.components.foldl (init := buildDir / "highlighted")
    fun path component => path / component.toString
  modulePath.withExtension "json"

/-- Load highlighting for a module, preferring elaboration-time capture over subverso-extract-mod.
    1. First checks for JSON from Hook.lean (written during `lake build`)
    2. Falls back to running `subverso-extract-mod` if no cached JSON exists

    The `buildDir` should be the Lake build directory (e.g., `.lake/build`). -/
def loadHighlightingWithFallback (moduleName : Name) (buildDir : FilePath)
    (projectRoot? : Option FilePath := none) : IO (NameMap Highlighted) := do
  -- Try loading from cached JSON file first (from Hook.lean elaboration-time capture)
  let hlPath := getHighlightingPath buildDir moduleName
  if ← hlPath.pathExists then
    return ← loadHighlightingFromFile hlPath.toString

  -- Fallback to running subverso-extract-mod
  extractHighlightingMap moduleName projectRoot?

/-- Get highlighted code for a specific declaration from a module.
    This extracts the full module and looks up the specific name. -/
def getDeclarationHighlighting (moduleName : Name) (declName : Name)
    (projectRoot? : Option FilePath := none) : IO (Option Highlighted) := do
  let hlMap ← extractHighlightingMap moduleName projectRoot?
  return hlMap.find? declName

/-- Get the source range for a specific declaration from module items. -/
def getDeclarationRange (items : Array ModuleItem) (declName : Name)
    : Option (Position × Position) :=
  items.findSome? fun item =>
    if item.defines.contains declName then item.range else none

/-- Get all declarations defined in a module along with their highlighted code. -/
def getAllDeclarations (items : Array ModuleItem) : Array (Name × Highlighted) :=
  items.foldl (init := #[]) fun acc item =>
    item.defines.foldl (init := acc) fun acc' name =>
      acc'.push (name, item.code)

end Architect.SubVersoExtract
