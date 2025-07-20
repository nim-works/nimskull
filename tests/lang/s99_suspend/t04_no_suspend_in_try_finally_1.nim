discard """
  description: '''
    It's not allowed to suspend in a 'try' with a 'finally' clause.
  '''
  action: reject
"""

proc test() =
  try:
    suspend void, cont:
      discard
  finally:
    discard
