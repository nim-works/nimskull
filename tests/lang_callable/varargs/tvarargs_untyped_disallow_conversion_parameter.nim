discard """
  description: "Test `varargs[untyped]` params disallow conversion params"
  knownIssue: "Implement me: not checked for and enforced"
  errormsg: "'varargs[untyped]' parameters do not support conversions"
  line: 10
"""


template bar(a: untyped): untyped = a
template foo(a: varargs[untyped, bar]) = discard
# error `foo`'s declaration: conversions are not allowed on untyped
# vararg parameters; this has an open question, so it might change