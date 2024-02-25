discard """
  description: "Test `varargs[typed]` params cannot be followed with varargs param"
  knownIssue: "Implement me: not checked for and enforced"
  errormsg: "'varargs[typed]' parameter must not be followed by a varargs parameter"
  line: 9
"""


macro foo(a: varargs[typed], b: varargs[string]) = discard
# error in definition: only non-varargs parameters allowed after a
# `varargs[typed]` parameter