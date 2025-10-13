discard """
  description: '''
    Ensure that disabling overflow checks works for 64-bit integer negations
  '''
  targets: "c js vm"
  matrix: "--overflowChecks:off"
"""

var x = low(int64)
discard -x
