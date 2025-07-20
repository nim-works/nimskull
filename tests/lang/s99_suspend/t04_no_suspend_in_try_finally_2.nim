discard """
  description: '''
    It's not allowed to suspend in an implicit 'try' with a 'finally' clause.
  '''
  action: reject
  knownIssue: "'defer' is not lowered by the time sempass2 runs"
"""

proc test() =
  defer: discard
  suspend void, cont:
    discard
