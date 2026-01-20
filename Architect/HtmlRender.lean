/-
Copyright (c) 2025 LeanArchitect contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Verso.Code.Highlighted
import Verso.Doc

/-!
# HTML Rendering for Blueprint Highlighting

This module provides a thin wrapper around Verso's `Highlighted.toHtml` to render
SubVerso highlighted code to HTML strings.

Uses `Genre.none` and minimal configuration to avoid dependencies on Verso's
full document infrastructure.
-/

open Verso.Code
open Verso.Doc
open Verso.Output

namespace Architect.HtmlRender

/-- Render highlighted code to HTML string using Verso's production-quality renderer.

Uses `Genre.none` with empty `LinkTargets` (no hyperlinks) and default options.
This provides syntax highlighting without interactive features like hovers or links.
-/
def renderHighlightedToHtml (hl : SubVerso.Highlighting.Highlighted) : String :=
  let linkTargets : LinkTargets Unit := {}
  let context : HighlightHtmlM.Context Genre.none := {
    linkTargets := linkTargets
    traverseContext := ()
    definitionIds := {}
    options := {}
  }
  let initialState : Hover.State Html := .empty
  let (html, _finalState) := (hl.toHtml).run context |>.run initialState
  html.asString (breakLines := false)

/-- Render highlighted code wrapped in a code element with appropriate CSS classes.

The output includes `class="hl lean block"` which Verso uses for styling.
-/
def renderHighlightedBlock (hl : SubVerso.Highlighting.Highlighted) : String :=
  let inner := renderHighlightedToHtml hl
  s!"<code class=\"hl lean block\">{inner}</code>"

/-- Render highlighted code as an inline element.

Uses `class="hl lean inline"` for inline code styling.
-/
def renderHighlightedInline (hl : SubVerso.Highlighting.Highlighted) : String :=
  let inner := renderHighlightedToHtml hl
  s!"<code class=\"hl lean inline\">{inner}</code>"

end Architect.HtmlRender
