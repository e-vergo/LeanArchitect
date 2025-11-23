import re
from pathlib import Path

from loguru import logger

from common import Node, NodeWithPos

def write_latex_source(
    nodes_with_pos: list[NodeWithPos],
    latex_label_to_raw_sources: dict[str, list[str]],
    blueprint_root: Path,
    convert_informal: bool,
    libraries: list[str]
):
    for node in nodes_with_pos:
        # If not convert_informal, skip writing \inputleannode for nodes that are not in Lean
        if not convert_informal and not node.has_lean:
            continue
        first_source, *rest_sources = latex_label_to_raw_sources[node.latex_label]
        for file in blueprint_root.glob("**/*.tex"):
            file_content = file.read_text()
            file_content = file_content.replace(first_source, f"\\inputleannode{{{node.latex_label}}}")
            for s in rest_sources:
                file_content = file_content.replace(s, "")
            file.write_text(file_content)

    # Add import to macros file
    macros_file = blueprint_root / "macros" / "common.tex"
    new_macros = "\n".join(f"\\input{{../../.lake/build/blueprint/library/{library}}}" for library in libraries)
    if macros_file.exists():
        macros_file.write_text(macros_file.read_text() + "\n" + new_macros + "\n")
    else:
        logger.warning(f"{macros_file} not found; please add the following to anywhere in the start of LaTeX blueprint:\n{new_macros}")

    macros_file_print = blueprint_root / "macros" / "print.tex"
    print_macros = "\\usepackage{fvextra}"
    if macros_file_print.exists():
        macros_file_print.write_text(macros_file_print.read_text() + "\n" + print_macros + "\n")
    else:
        logger.warning(f"{macros_file_print} not found; please add the following to the macros file for print.tex:\n{print_macros}")

    macros_file_web = blueprint_root / "macros" / "web.tex"
    # NB: \Verb is not defined in plasTeX
    web_macros = "\\providecommand{\\Verb}{\\verb}"
    if macros_file_web.exists():
        macros_file_web.write_text(macros_file_web.read_text() + "\n" + web_macros + "\n")
    else:
        logger.warning(f"{macros_file_web} not found; please add the following to the macros file for web.tex:\n{web_macros}")
