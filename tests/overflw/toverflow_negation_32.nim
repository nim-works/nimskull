discard """
  targets: "c js vm"
  description: "Ensure that overflow checks work for 32-bit integer negations"
  exitcode: 1
  outputsub: "over- or underflow"
  knownIssue.vm: "Error message does not match"
"""

var x = low(int32)
discard -x
