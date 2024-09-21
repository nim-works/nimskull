discard """
  description: "Test to ensure `varargs[untyped]` is only trailing"
  errormsg: "at most one trailing 'varargs[untyped]' parameter per routine allowed"
  line: 9
  knownIssue: "Implement me: not checked for and enforced"
"""


template foo(a: varargs[untyped], b: int) = discard
# error as `varargs[untyped]` may only appear as the last parameter