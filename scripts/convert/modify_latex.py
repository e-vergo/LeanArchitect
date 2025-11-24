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
    macros_files_common = [blueprint_root / "macros" / "common.tex", blueprint_root / "preamble" / "common.tex"]
    common_macros = "\n".join(f"\\input{{../../.lake/build/blueprint/library/{library}}}" for library in libraries)
    for file in macros_files_common:
        if file.exists():
            file.write_text(file.read_text() + "\n" + common_macros + "\n")
            break
    else:
        logger.warning(f"{macros_files_common[0]} not found; please add the following to anywhere in the start of LaTeX blueprint:\n{common_macros}")

    macros_files_print = [blueprint_root / "macros" / "print.tex", blueprint_root / "preamble" / "print.tex"]
    print_macros = "\\usepackage{fvextra}"
    for file in macros_files_print:
        if file.exists():
            file.write_text(file.read_text() + "\n" + print_macros + "\n")
            break
    else:
        logger.warning(f"{macros_files_print[0]} not found; please add the following to the macros file for print.tex:\n{print_macros}")

    macros_files_web = [blueprint_root / "macros" / "web.tex", blueprint_root / "preamble" / "web.tex"]
    # NB: \Verb is not defined in plasTeX
    web_macros = "\\providecommand{\\Verb}{\\verb}"
    for file in macros_files_web:
        if file.exists():
            file.write_text(file.read_text() + "\n" + web_macros + "\n")
            break
    else:
        logger.warning(f"{macros_files_web[0]} not found; please add the following to the macros file for web.tex:\n{web_macros}")
