discard """
  description: "Test import statements are properly analysed"
  knownissue: "`import except` doesn't preserve exclusion info"
  output: '''
StmtList
  ImportExceptStmt
    Sym "mqux3"
    Sym "bar3"
  ImportExceptStmt
    Sym "mqux4"
    Sym "bar4"
    Sym "baz4"
'''
"""

import std/macros

macro outputTyped(n: typed) =
  let output = treeRepr n
  quote:
    echo `output`

outputTyped:
  import mqux3 except bar3
  import mqux4 except bar4, baz4

when false: # knownIssue: `Import as` doesn't preserve original identifier info
  # not quite sure what the test should be yet, current behaviour might be
  # acceptable, because the symbol decl can be introspected?
  outputTyped:
    import mqux5 as mqux6