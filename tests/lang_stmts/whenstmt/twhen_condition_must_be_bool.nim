discard """
  description: '''
    Ensure that an error is reported for a non-boolean expression used as the
    condition of a `when`
  '''
  errormsg: "type mismatch: got <int literal(2)> but expected 'bool'"
  line: 10
"""

when 1 + 1:
  discard
