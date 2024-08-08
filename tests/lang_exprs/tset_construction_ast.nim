discard """
  description: '''
    Ensure that valid `nkRange` AST in a set construction emitted by a macro
    is correctly semantically anaylsed
  '''
  targets: native
"""

import std/macros

macro m(): untyped =
  result = nnkCurly.newTree(nnkRange.newTree(newLit(1'u8), newLit(3'u8)))

doAssert m() == {1'u8, 2'u8, 3'u8}
