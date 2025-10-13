discard """
  description: "Fail lambda inference due to error within the body."
  errormsg: "undeclared identifier: 'i'"
  line: 13
"""

# This is a regression test, ensure we report errors inside lambdas during
# inference

proc test(p: proc(x: int)) =
  discard

test proc(e: auto) = i