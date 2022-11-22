discard """
  errormsg: "'untyped' is only allowed in templates and macros or magic procs"
  knownIssue: "https://github.com/nim-lang/Nim/issues/18113"
  knownIssue: "https://github.com/nim-lang/Nim/issues/18124"
"""

template something(op: proc (v: untyped): void): void =
  discard

