discard """
  description: '''
    Regression test for an alias-analysis bug that caused expressions involving
    array-subscript expressions to erroneously be eliminated in some cases
  '''
  targets: c js vm
"""

type
  Object = object
    val: int
  Type = array[2, array[2, Object]]

# important: the bug only affected code in procedures that work with destructor-
# having types
proc `=destroy`(x: var Object) =
  discard

proc test(x, y: int) {.noinline.} =
  var a = [[Object(val: 1), Object(val: 2)], [Object(val: 3), Object(val: 4)]]
  # whether the two expressions refer to the same location is unknown; the
  # assignment must not be removed
  a[x][1] = a[y][1]
  # note: the common trailing [1] sub-expressions are important for this test

  doAssert a[0][1].val == 4

test(0, 1)
