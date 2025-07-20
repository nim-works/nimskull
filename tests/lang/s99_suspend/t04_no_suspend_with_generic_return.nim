discard """
  description: '''
    Using 'suspend' where the current routine's return type is still
    generic/unresolved is disallowed.
  '''
  action: reject
"""

proc test(): auto =
  suspend void, cont:
    discard
  return 1
