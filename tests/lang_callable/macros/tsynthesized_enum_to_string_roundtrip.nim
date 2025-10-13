discard """
  description: '''
    Regression test where returning an AST containing a call to a compiler-
    generated enum-to-string procedure crashed the compiler.
  '''
  action: compile
"""

macro passthrough(x: typed): untyped = x

type Enum = enum
  enumA

var e = enumA
# the compiler crashed when processing the macro output
passthrough:
  discard $(e)
