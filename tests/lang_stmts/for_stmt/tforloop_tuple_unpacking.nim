discard """
  description: "Tests for tuple unpacking done by for-loops"
  targets: "c js vm"
"""

type
  Tup = tuple
    a, b: int

  Copyable = object
    valid: bool

var numCopies = 0

proc `=copy`(a: var Copyable, b: Copyable) =
  if b.valid:
    inc numCopies
    a.valid = true

iterator single[T](item: T): T =
  yield item

iterator singleVar[T](item: var T): var T =
  yield item

iterator nestedVar[T](item: var T): (int, var T) =
  yield (0, item)

iterator nestedLent[T](item: T): (int, lent T) =
  yield (0, item)

iterator singleLent[T](item: T): lent T =
  yield item

block not_mutable:
  # without the iterator returning the tuple as a ``var``, the unpacked
  # elements are not mutable
  var item: Tup = (1, 2)

  for x, y in single(item):
    doAssert not compiles(x = 1)
    doAssert not compiles(y = 1)
    doAssert x == 1
    doAssert y == 2

  for (x, y) in single(item):
    doAssert not compiles(x = 1)
    doAssert not compiles(y = 1)
    doAssert x == 1
    doAssert y == 2

block var_access:
  # if an unpacked tuple is of ``var`` type, so are its fields
  var item: Tup = (1, 2)
  for x, y in singleVar(item):
    x = 3
    y = 4

  doAssert item.a == 3
  doAssert item.b == 4

  for (x, y) in singleVar(item):
    x = 1
    y = 2

  doAssert item.a == 1
  doAssert item.b == 2

  # this is also true for nested unpacking
  for _, (x, y) in nestedVar(item):
    x = 3
    y = 4

  doAssert item.a == 3
  doAssert item.b == 4

block no_copy_lent:
  # unpacking a ``lent`` tuple doesn't copy its elements
  numCopies = 0
  var item = (1, Copyable(valid: true))

  for x, y in singleLent(item):
    doAssert x == 1
    doAssert y.valid == true

  for (x, y) in singleLent(item):
    doAssert x == 1
    doAssert y.valid == true

  # this is also true for nested unpacking
  for _, (x, y) in nestedLent(item):
    doAssert x == 1
    doAssert y.valid == true

  doAssert numCopies == 0

block copy_no_lent:
  # if it's safe to borrow, the copy *may* be elided -- otherwise it is
  # required for when the item is not yielded as ``lent``
  numCopies = 0
  var item = (1, Copyable(valid: true))

  for x, y in single(item):
    item[1].valid = false # it's not safe to borrow (or to use a cursor)
    doAssert y.valid

  doAssert numCopies == 1
  item[1].valid = true

  for (x, y) in single(item):
    item[1].valid = false
    doAssert y.valid

{.experimental: "views".}

# the below doesn't work due to an unrelated view bug that creates invalid
# code for the ``(1, x)`` expression
when false: #block nested_var_in_lent:
  # NOTE: the behaviour of this test is questionable

  proc make(x: var int): (int, var int) =
    ## A helper required to create a tuple with a ``var`` element
    (1, x)

  var a = 2
  let item = make(a)

  for x, y in singleLent(item):
    doAssert y == 2
    y = 3

  doAssert a == 3

  for (x, y) in singleLent(item):
    doAssert y == 3
    y = 2

  doAssert a == 2

  # the same also applies to nested unpacking

  for _, (x, y) in nestedLent(item):
    doAssert y == 2
    y = 3

  doAssert a == 3