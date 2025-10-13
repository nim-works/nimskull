discard """
  description: '''
    Ensure that the shape of literal array construction expresions is
    preserved in typed AST
  '''
  action: compile
"""

import std/macros

macro m(x: typed) =
  doAssert treeRepr(x) == """Bracket
  ExprColonExpr
    IntLit 1
    StrLit "a"
  StrLit "b"
  ExprColonExpr
    IntLit 3
    StrLit "c""""

m([1: "a", "b", 3: "c"])
