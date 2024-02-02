discard """
  description: '''
    Describe the evaluation behaviour of arguments to immutable parameters of
    scalar type
  '''
"""

# arguments to procedures are evaluated at the callsite from left to right,
# inside to outside.

proc test(a, b, v1, v2: int) =
  doAssert a == v1
  doAssert b == v2

var x = 0
# the first argument is evaluated first
test(x, (inc x; x), 0, 1)
# the second argument observes all mutation effects of the first one
test((inc x; x), x, 2, 2)

# indirect mutations are also observed
proc get(): int =
  inc x
  result = x

test(x, get(), 2, 3)
test(get(), x, 4, 4)

# effects of an argument's sub-expression are observable by all following
# arguments, but not in the other direction
var
  a = [1, 2]
  i = 0

test(a[i], a[(inc i; i)], 1, 2)
i = 0
test(a[(inc i; i)], a[i], 2, 2)