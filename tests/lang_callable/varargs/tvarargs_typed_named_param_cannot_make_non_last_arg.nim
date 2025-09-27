discard """
  description: "Test to ensure `varargs[typed]` is only trailing"
  errormsg: "'varargs[typed]' parameter argument cannot be made non-trailing via named param"
  line: 10
  knownIssue: "Implement me: not checked for and enforced"
"""


template foo(a: int, b: varargs[typed]) = discard
foo(b = string, a = 1)
# error, `a` is never provided and everything after `b =` is part of
# the `varargs[typed]` argument, including `a = 1`