discard """
  description: '''
    When the 'suspend' is part of a void routine, the suspend block must
    be a statement.
  '''
  action: reject
"""

proc test() =
  suspend void, cont, 1
