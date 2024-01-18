discard """
  description: '''
    Ensure that taking the address of an aggregate constant's element works
  '''
  targets: "c js vm"
  knownIssue: '''
    The element access is folded into a literal integer, later causing an
    internal compiler error, since taking the address of a literal integer
    is illegal.
  '''
"""

# XXX: once the test succeeds, merge it back into ``taddr.nim``

proc test[T](x: ptr T, expect: T) {.noinline.} =
  # prevent the addr + deref from being optimized away by using a .noinline
  # procedure
  doAssert x[] == expect

type Object = object
  x: int

const
  obj = Object(x: 1)
  tup = (1, 2, 3)
  arr = [1, 2, 3]

test(addr o.x, 1)
doAssert not compiles(test(addr tup[1], 2))
test(addr arr[1], 2)
