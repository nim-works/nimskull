discard """
  targets: "native"
  outputsub: "Error: unhandled exception: FPU operation caused a NaN result [FloatInvalidOpDefect]"
  exitcode: "1"
"""
# Test new floating point exceptions

# xxx: what should js behaviour be?

{.floatChecks: on.}

var x = 0.0
var y = 0.0

echo x / y #OUT Error: unhandled exception: FPU operation caused a NaN result
