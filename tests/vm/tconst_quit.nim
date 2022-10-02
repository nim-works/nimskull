discard """
  description: '''
    Calling `quit` when inside the computation of a const value is an
    error.
  '''

  target: native
  action: reject
  errormsg: "`quit` called with exit-code: 2"
"""

proc compute(): int =
  quit(2)

const Value = compute()