discard """
  description: "Test `varargs[untyped]` params disallow conversion params"
  knownIssue: "Implement me: not checked for and enforced"
  errormsg: "routine with 'varargs[untyped]' parameter is implicitly compile-time only"
  line: 10
"""


proc foo(a: varargs[untyped]) = discard
foo(1)
# error: `varargs[untyped` requires a compile time context, cannot be
#        called at runtime.