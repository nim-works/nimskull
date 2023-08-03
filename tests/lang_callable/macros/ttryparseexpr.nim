discard """
  target: "!vm"
  outputsub: '''Error: expression expected, but found '[EOF]' 45'''
"""

# disabled for VM until we support `getCurrentExceptionMsg` (knownIssue)

# feature request #1473
import std/macros

macro test(text: string): untyped =
  try:
    result = parseExpr(text.strVal)
  except ValueError:
    result = newLit getCurrentExceptionMsg()

const
  valid = 45
  a = test("foo&&")
  b = test("valid")
  c = test("\"") # bug #2504

echo a, " ", b
