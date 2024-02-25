discard """
  description: "Test `varargs[typed]` params require compile time context"
  knownIssue: "Implement me: not checked for and enforced"
  errormsg: "routine with 'varargs[typed]' parameter is implicitly compile-time only"
  line: 10
"""


proc foo(a: varargs[typed]) = discard
foo(1)
# error: `varargs[typed]` requires a compile time context, cannot be
#        called at runtime.