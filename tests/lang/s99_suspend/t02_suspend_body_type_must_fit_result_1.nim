discard """
  description: '''
    If an expression, the suspend block's type must fit the enclosing routine's
    return type.
  '''
"""

proc test(): int =
  suspend void, cont: 2'i8

doAssert test() == 2
