discard """
  description: "Test to ensure `varargs[untyped]` is only trailing"
  errormsg: "'varargs[untyped]' parameter argument cannot be made non-trailing via named param"
  line: 10
  knownIssue: "Implement me: not checked for and enforced"
"""


template foo(a: int, b: varargs[untyped]) = discard
foo(b = whatever, a = 1)
# error, `a` is never provided and everything after `b =` is the
# `untyped` argument, including `a = 1`