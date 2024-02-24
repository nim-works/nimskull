discard """
  description: "Test `varargs[untyped]` params disallow conversion params"
  knownIssue: "Implement me: not checked for and enforced"
  errormsg: "only one trailing 'varargs[untyped]' parameter allowed per routine"
  line: 9
"""


template foo(a, b: varargs[untyped]) = discard
# error `foo`'s declaration: only one trailing `varargs[untyped]`
# parameter is allowed