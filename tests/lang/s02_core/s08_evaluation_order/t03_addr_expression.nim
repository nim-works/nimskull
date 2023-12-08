discard """
  description: "Describe the evaluation behaviour of the `addr` operation"
"""

# the operand expression of an `addr` operation is not fully evaluated, only
# all sub-expressions and sub-statements are evaluated (according to the
# general evaluation order rules)
var x = 0

var p = addr (inc x; x)
doAssert p[] == 1
doAssert x   == 1