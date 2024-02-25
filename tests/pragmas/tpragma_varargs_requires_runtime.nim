discard """
  description: "Test to ensure `{.varargs.}` ensures runtime context"
  errormsg: "'{.varargs.}' pragma may only apply to a runtime procedure"
  line: 10
  knownIssue: "Implement me: not checked for and enforced"
"""

# TODO: figure out how to test string -> cstring conversion

proc foo() {.varargs, compiletime.} = discard
# error in definition: varargs pragma cannot be used in a compile time
# context