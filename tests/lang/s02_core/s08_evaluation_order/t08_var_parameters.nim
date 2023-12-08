discard """
  description: "Describe how arguments to `var` parameters are evaluated"
"""

# for `var` parameters (except `var openArray`), the procedure recieves the
# location of the argument rather than its value. Only the sub-expressions
# of the argument are evaluated

proc test(a, b: var int, v1, v2: int) =
  doAssert a == v1
  doAssert b == v2
  inc a
  inc b

var
  x = 2
  y = 4

test((inc y; x), (inc x; y), 3, 5)
# the call observes all modifications of `x` that happen before the call
# is entered
doAssert x == 4
doAssert y == 6

var
  arr = [1, 2]
  i   = 0

# evaluation order for the sub-expressions is still left-to-right, inside-to-
# outside
test(arr[i], arr[(inc i; i)], 1, 2)
doAssert i == 1
doAssert arr == [2, 3]