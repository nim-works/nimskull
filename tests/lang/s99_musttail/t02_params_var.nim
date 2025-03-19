discard """
  description: '''
    `var` parameters work with the same restrictions as by-ref parameters.
  '''
"""

proc tail1(a: var int) {.musttail.} =
  a = 3

proc test1(a: var int) =
  tail1(a)

var x = 0
test1(x)
doAssert x == 3

# passing a location derived from a parameter to a .musttail call also works:

proc tail2(a: var (int, int)) {.musttail.} =
  tail1(a[1])

proc test2(a: var (int, int)) =
  tail2(a)

var y = (1, 2)
test2(y)
doAssert y == (1, 3)
