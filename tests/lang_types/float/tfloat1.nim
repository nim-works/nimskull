discard """
  targets: "c js vm"
  outputsub: "FPU operation caused an overflow"
  exitcode: "1"
  knownIssue.js vm: '''
    Overflow checks for floating-point arithmetic are not implemented
  '''
"""
# Test new floating point exceptions

{.floatChecks: on.}

var x = 0.8
var y = 0.0

echo x / y #OUT Error: unhandled exception: FPU operation caused an overflow
