discard """
  description: "Test `varargs[typed]` params cannot be followed with defaulted param"
  knownIssue: "Implement me: not checked for and enforced"
  errormsg: "'varargs[typed]' parameter must not be followed by a parameter with default value"
  line: 9
"""


macro foo(a: varargs[typed], b = "hi") = discard
# error in definition: only non-defaulted parameters allowed after a
# `varargs[typed]` parameter