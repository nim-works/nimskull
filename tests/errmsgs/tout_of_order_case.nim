discard """
  action: reject
  matrix: "--errorMax:2"
"""

# TODO: This is not an ideal error message, it should be improved and the
#       test updated accordingly. Alternatively, and likely better we should
#       defer the current type semantic analysis, and perform it after the
#       `Enum` type has been declared, and if we can't defer any more we should
#       report an error.

type
  Object = object
    case x: Enum #[tt.Error
         ^ undeclared identifier: 'Enum' ]#
    of a, b: #[tt.Error
       ^ undeclared identifier: 'a' ]#
      discard
  Enum = enum a, b
