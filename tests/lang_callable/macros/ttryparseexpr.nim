discard """
  description: "Ensure that `parseExpr` raises a catchable exception on error"
  action: compile
  targets: native
"""

# feature request #1473
import std/macros

macro test(text: string): untyped =
  try:
    result = parseExpr(text.strVal)
  except ValueError:
    result = newLit getCurrentExceptionMsg()

const valid = 45

static:
  doAssert test("foo&&") == "Error: expression expected, but found '[EOF]'"
  doAssert test("valid") == 45
  doAssert test("\"")    == "Error: closing \" expected" # bug #2504

  const error = "Error: expected expression, but got multiple statements"
  # test with empty string
  doAssert test("") == error
  # test with multiple declarative statements
  doAssert test("type A = int\ntype A = int") == error
