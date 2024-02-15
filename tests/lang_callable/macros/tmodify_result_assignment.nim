discard """
  description: '''
    Partially typed ``nkReturnStmt`` AST is properly re-analyzed after macro
    invocation
  '''
  errmsg: "got <string> but expected 'int'"
  line: 26
"""

import std/macros

macro modify(input: typed) =
  let input = input[0]
  input.expectKind nnkProcDef
  input.body.expectKind nnkReturnStmt
  input.body[0].expectKind nnkAsgn

  result = copyNimTree input
  # replace the right side of the assignment with an expression of different,
  # incompatible type
  result.body[0][1] = newStrLitNode("") # wrong type: int vs. string
  result.body.copyLineInfo(input.body[0][1])

modify:
  proc p(): int =
    return 0
