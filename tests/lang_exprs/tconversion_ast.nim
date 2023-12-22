discard """
  description: '''
    Ensure that valid conversion AST emitted by a macro is correctly
    semantically anaylsed
  '''
  targets: native
"""

import std/macros

macro m(): untyped =
  result = nnkConv.newTree(ident"int", newLit(1.1))

doAssert m() == 1