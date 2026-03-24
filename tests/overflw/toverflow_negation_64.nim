discard """
  targets: "c js vm"
  description: "Ensure that overflow checks work for 64-bit integer negations"
  exitcode: 1
  outputsub: "over- or underflow"
"""

var x = low(int64)
discard -x
