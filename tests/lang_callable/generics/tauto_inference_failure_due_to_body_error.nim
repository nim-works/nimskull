discard """
  description: "Fail lambda inference due to error without the body."
  errormsg: "undeclared identifier: 'i'"
  line: 11
"""

# This is a regression test, ensure we report errors inside lambdas during
# inference

var x: seq[proc(i: int)]
x.add proc(e: auto) = echo i