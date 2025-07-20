discard """
  description: '''
    When the 'suspend' is part of a void routine, the suspend block must
    be a statement.
  '''
"""

proc test() =
  suspend void, cont:
    discard
