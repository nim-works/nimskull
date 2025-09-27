discard """
  description: "Disallow setting a generic param to varargs"
  errormsg: "it is illegal to set a type parameter to varargs"
  line: 12
  knownIssue: "compiler doesn't guard against this yet"
"""


block cannot_set_varargs_as_type_parameter:
  proc foo[T](a: T) =
    discard

  foo[varargs[int]](1, 2, 3)