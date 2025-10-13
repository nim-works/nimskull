discard """
  description: '''
    `var` parameters work with the same restrictions as by-ref parameters.
  '''
"""

proc tail1(a: var int) {.tailcall.} =
  a = 3

proc test1(a: var int) {.tailcall.} =
  tail1(a)

var x = 0
test1(x)
doAssert x == 3

# passing a location derived from a parameter to a .tailcall call also works:

proc tail2(a: var (int, int)) {.tailcall.} =
  tail1(a[1])

var y = (1, 2)
tail2(y)
doAssert y == (1, 3)
