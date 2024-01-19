discard """
  description: "Ensure that ill-formed AST generates a well formed error"
  errmsg: "identifier expected, but found '123'"
"""

# This test covers a regression where an error with the symbol outside a pragma
# expr was not correctly wrapped in an error leading to a compiler bug.

template makeAFor(x: untyped) =
  for x in [1, 2]:
    discard

makeAFor(123)