discard """
  description: '''
    Ensure that disabling overflow checks works for 32-bit integer negations
  '''
  targets: "c js vm"
  matrix: "--overflowChecks:off"
"""

var x = low(int32)
discard -x