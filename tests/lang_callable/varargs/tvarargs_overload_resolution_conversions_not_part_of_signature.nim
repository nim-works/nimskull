discard """
  description: "Test varargs conversions do not differentiate signatures"
  errormsg: "redefinition of 'foo'; previous declaration here"
  line: 12
"""

block conversions_do_not_differentiate_signatures:
  ## all this should result in ambiguity errors
  ## N.B.: this must be an error because there is no way to unambiguously call
  ##       the non-conversion `foo`
  proc foo(a: varargs[string]) = "no conversion"
  proc foo(a: varargs[string, `$`]) = "conversion"

  foo("")
  foo(a = "")