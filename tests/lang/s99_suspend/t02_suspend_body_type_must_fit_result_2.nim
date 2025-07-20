discard """
  description: '''
    If an expression, the suspend block's type must fit the enclosing routine's
    return type.
  '''
  action: reject
"""

proc test(): int =
  suspend void, cont: 1.5
