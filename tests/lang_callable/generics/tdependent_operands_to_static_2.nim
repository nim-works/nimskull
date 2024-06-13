discard """
  description: '''
    Operands with a type not known upfront cannot be used as arguments to
    complex static
  '''
  errormsg: "cannot instantiate Type"
  line: 21
  knownIssue: '''
    `Type`s generic parameter is not detected as containing a `static`, thus
    full analysis is not disabled (which subsequently fails)
  '''
"""

type
  Type[T: string | static float] = object ## must be an int or static float

proc eval[T](x: T): T {.compileTime.} = x

# the operand's type is not known and no concrete type can be derived from
# the constraint -> reject early
proc p[T](): Type[eval(default(T))] = discard
