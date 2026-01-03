import subprocess
import re
import json
import sys
from typing import Optional, Literal

from loguru import logger

from pydantic import BaseModel, ConfigDict
from pydantic.alias_generators import to_camel


def _quote(s: str) -> str:
    """Quotes a string in double quotes."""
    return json.dumps(s, ensure_ascii=False)


class BaseSchema(BaseModel):
    """A Pydantic base model with camelCase aliases."""
    model_config = ConfigDict(
        alias_generator=to_camel,
        populate_by_name=True,
    )

# These classes are ported from Architect/Basic.lean

class NodePart(BaseSchema):
    text: str
    # NB: `uses`, `excludes`, `excludes_labels` are NOT used during conversion script,
    # and they should remain as empty sets.
    uses: set[str]
    excludes: set[str]
    uses_labels: set[str]
    excludes_labels: set[str]
    latex_env: str


class FormattingConfig(BaseModel):
    docstring_indent: int
    docstring_style: Literal["hanging", "compact"]
    max_columns: int


class Node(BaseSchema):
    name: str  # Lean identifier (unique)
    latex_label: str
    statement: NodePart
    proof: Optional[NodePart]
    not_ready: bool
    discussion: Optional[int]
    title: Optional[str]

    @property
    def uses_labels(self) -> set[str]:
        return self.statement.uses_labels | (self.proof.uses_labels if self.proof is not None else set())

    def to_lean_attribute(
        self,
        config: FormattingConfig,
        add_statement_text: bool = True, add_uses: bool = True,
        add_proof_text: bool = True, add_proof_uses: bool = True
    ) -> str:
        options = []
        # See Architect/Attribute.lean for the options
        if self.latex_label != self.name:
            options.append(_quote(self.latex_label))
        if self.title:
            options.append(f"(title := {_quote(self.title)})")
        if add_statement_text and self.statement.text.strip():
            options.append(f"(statement := {make_docstring(self.statement.text, config, start_column=len("  (statement := "))})")
        if add_uses and self.statement.uses_labels:
            options.append(f"(uses := [{_wrap_list([_quote(use) for use in self.statement.uses_labels], indent=4, start_column=len('  (uses := ['), max_columns=config.max_columns)}])")
        if self.proof is not None:
            if add_proof_text and self.proof.text.strip():
                options.append(f"(proof := {make_docstring(self.proof.text, config, start_column=len("  (proof := "))})")
            if add_proof_uses and self.proof.uses_labels:
                options.append(f"(proofUses := [{_wrap_list([_quote(use) for use in self.proof.uses_labels], indent=4, start_column=len('  (proofUses := ['), max_columns=config.max_columns)}])")
        if self.not_ready:
            options.append("(notReady := true)")
        if self.discussion:
            options.append(f"(discussion := {self.discussion})")
        if self.proof is None and self.statement.latex_env != "definition" or self.proof is not None and self.statement.latex_env != "theorem":
            options.append(f"(latexEnv := {_quote(self.statement.latex_env)})")
        blueprint_options = "".join(f"\n  {o}" for o in options)
        return f"blueprint{blueprint_options}"

class Position(BaseSchema):
    line: int
    column: int

class DeclarationRange(BaseSchema):
    pos: Position
    end_pos: Position

class DeclarationLocation(BaseSchema):
    module: str
    range: DeclarationRange

class NodeWithPos(Node):
    has_lean: bool
    location: Optional[DeclarationLocation]
    file: Optional[str]


def _indent(lines: list[str], indent: int) -> list[str]:
    if not lines:
        return []
    common_indent = min(len(line) - len(line.lstrip()) for line in lines if line.strip())
    dedented = [line[common_indent:] for line in lines]
    return [f"{' ' * indent}{line}" for line in dedented]

def _wrap(line: str, indent: Optional[int], start_column: int, indent_first_line: bool, latex_mode: bool, max_columns: int) -> list[str]:
    """Wrap a single line of text to a list of indented lines.
    If indent is not provided, infer from the number of leading spaces.
    Respects LaTeX comments if latex_mode is True.
    """
    if indent is None:
        indent = len(line) - len(line.lstrip())
    is_comment = False
    words = line.lstrip().split(" ")
    if not words:
        return [""]
    res = []
    cur = (" " * indent if indent_first_line else "") + words[0]
    for word in words[1:]:
        if (start_column if not res else 0) + len(cur) + 1 + len(word) > max_columns:
            res.append(cur)
            # If unescaped % is in the current line, then switch to is_comment
            if latex_mode and re.search(r"[^\\]%", cur):
                is_comment = True
            if is_comment:
                cur = " " * indent + "% " + word
            else:
                cur = " " * indent + word
        else:
            cur += " " + word
    res.append(cur)
    return res

def _wrap_list(items: list[str], indent: int, start_column: int, max_columns: int) -> str:
    text = ", ".join(items)
    return "\n".join(_wrap(text, indent, start_column, indent_first_line=True, latex_mode=False, max_columns=max_columns)).strip()

def make_docstring(text: str, config: FormattingConfig, start_column: int = 0) -> str:
    # If fits in one line, then use /-- {text} -/
    if "\n" not in text.strip() and start_column + len(f"/-- {text.strip()} -/") <= config.max_columns:
        return f"/-- {text.strip()} -/"

    # Remove common indentation
    lines = [line.rstrip() for line in text.splitlines()]
    lines = _indent(lines, config.docstring_indent)
    if config.docstring_style == "hanging":
        lines = [subline for line in lines for subline in _wrap(line, None, 0, indent_first_line=True, latex_mode=True, max_columns=config.max_columns)]
    else:
        lines[-1] += " -/"
        lines = [
            subline
            for i, line in enumerate(lines)
            for subline in (
                _wrap(line, None, start_column + len("/-- "), indent_first_line=False, latex_mode=True, max_columns=config.max_columns)
                if i == 0 else
                _wrap(line, None, 0, indent_first_line=True, latex_mode=True, max_columns=config.max_columns)
            )
        ]
    text = "\n".join(lines)
    if config.docstring_style == "hanging":
        return f"/--\n{text}\n{' ' * config.docstring_indent}-/"
    else:
        return f"/-- {text}"
