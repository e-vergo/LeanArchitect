import Lake
open Lake DSL

package LeanArchitect where
  testDriver := "ArchitectTest"

@[default_target]
lean_lib Architect

lean_lib ArchitectTest where
  globs := #[.submodules `ArchitectTest]

require batteries from git
  "https://github.com/leanprover-community/batteries" @ "v4.27.0"
