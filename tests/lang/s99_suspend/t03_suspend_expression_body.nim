discard """
  description: '''
    When the 'suspend' is part of a non-void routine, the suspend block may
    be an expression.
  '''
"""

proc test(): int =
  suspend void, cont, 1

doAssert test() == 1
