discard """
  description: '''
    When the 'suspend' is part of a non-void routine, the suspend block may
    be a statement.
  '''
"""

proc test(): int =
  suspend void, cont:
    result = 1

echo test()
