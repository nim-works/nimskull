discard """
  description: "Test import statements are properly analysed"
  output: '''
StmtList
  ImportStmt
    Sym "mfoo"
  ImportStmt
    Sym "mbar"
    Sym "mbaz"
  FromStmt
    Sym "mqux1"
    Sym "bar1"
  FromStmt
    Sym "mqux2"
    Sym "bar2"
    Sym "baz2"
  ImportExceptStmt
    Sym "mqux3"
  ImportExceptStmt
    Sym "mqux4"
  ImportStmt
    Sym "mqux6"
'''
"""

import std/macros

macro outputTyped(n: typed) =
  let output = treeRepr n
  quote:
    echo `output`

outputTyped:
  import mfoo
  import mbar, mbaz
  from mqux1 import bar1
  from mqux2 import bar2, baz2
  # import except output has issues, see below
  import mqux3 except bar3
  import mqux4 except bar4, baz4
  # import as output might have issues, see below
  import mqux5 as mqux6

when false: # knownIssue: `import except` doesn't preserve exclusion info
  # test should check for output:
  #   ImportExceptStmt
  #     Sym "mqux3"
  #     Sym "bar3"
  #   ImportExceptStmt
  #     Sym "mqux4"
  #     Sym "bar4"
  #     Sym "baz4"
  outputTyped:
    import mqux3 except bar3
    import mqux4 except bar4, baz4

when false: # knownIssue: `Import as` doesn't preserve original identifier info
  # not quite sure what the test should be yet, current behaviour might be
  # acceptable, because the symbol decl can be introspected?
  outputTyped:
    import mqux5 as mqux6