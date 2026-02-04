discard """
  errormsg: "undeclared identifier: 'Enum'"
  line: 14
"""

# TODO: This is not an ideal error message, it should be improved and the
#       test updated accordingly. Alternatively, and likely better we should
#       defer the current type semantic analysis, and perform it after the
#       `Enum` type has been declared, and if we can't defer any more we should
#       report an error.

type
  Object = object
    case x: Enum
    of a, b:
      discard
  Enum = enum a, b
