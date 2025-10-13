discard """
  targets: "c js vm"
  description: "Ensure that overflow checks work for 64-bit integer negations"
  exitcode: 1
  outputsub: "over- or underflow"
  knownIssue.js: '''
    The lowest 64-bit signed integer value isn't properly represented in JS
  '''
"""

var x = low(int64)
discard -x
