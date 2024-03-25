discard """
  description: "Test `varargs[typed]` params only one per routine"
  knownIssue: "Implement me: not checked for and enforced"
  errormsg: "only one trailing 'varargs[typed]' parameter allowed per routine"
  line: 9
"""


macro foo(a: varargs[typed], b: varargs[typed]) = discard
# error in definition: only one varargs[typed] allowed per routine

## the reason is `foo = bar` could be a named param or a valid
## assignment statement that can be typed and passed in as a NimNode
## as one of the many variable arguments.