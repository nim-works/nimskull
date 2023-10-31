discard """
  description: '''
    Regression test for invalid macro-generated dereference expression
    crashing the compiler
  '''
  errormsg: "the built-in dereference operator is only available for 'ptr' and 'ref' types"
  line: 16
"""

import std/macros

macro gen(x: untyped): untyped =
  nnkDerefExpr.newTree(x)

var val = 1 # not something that has a pointer-like type
discard gen(val)