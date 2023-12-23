discard """
  description: "Describe the evaluation order of the assignment operands"
"""

# assignments have the same left-to-right, inside-out evaluation behaviour
# as is used everywhere else. The left operand (i.e., the destination) is
# evaluated *as if* it were wrapped in an `addr` operation.

# mutation effects of the LHS are observable by the RHS
var a = [1, 2]
(a[1] = 3; a[0]) = a[1]
# the assignment to `a[1]` is computed first
doAssert a == [3, 3]

var i = 0
proc get(): int =
  inc i
  result = i

# mutation effects of sub-expressions on left are observable by sub-
# expressions on the right
a = [1, 2] # reset
a[(inc i; 0)] = a[i]
doAssert a == [2, 2]

# but mutation effects of sub-expressions on the right are not observable on
# the left
a = [1, 2]
i = 0
a[i] = a[get()]
doAssert a == [2, 2]