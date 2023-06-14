discard """
  targets: "vm"
"""

# https://github.com/nim-lang/nim/issues/2946

proc testSwap(): int =
  type T = object
    data: seq[int]
  var x: T
  x.data = @[10]
  var y = @[11]
  swap(y, x.data)
  doAssert x.data == @[11]
  doAssert y == @[10]
  result = 99

discard testSwap()

# https://github.com/nim-lang/nim/issues/15463
block:
  static:
    var s = @[1, 2, 3]
    swap(s[0], s[2])

    doAssert s == [3, 2, 1]

block arguments_with_side_effect:
  # both argument expressions must only be evaluated once
  var
    a = 1
    b = 2
    counter = 0

  swap((inc counter; a), (inc counter; b))
  doAssert a == 2
  doAssert b == 1
  doAssert counter == 2

block primitive_type:
  # make sure that ``swap`` works for operands of primitive type
  proc swapLocals() =
    # both operands are locals
    var
      a = 1
      b = 2
    swap(a, b)
    doAssert a == 2
    doAssert b == 1

  swapLocals()

  proc swapFieldAndLocal() =
    # one operand is a local, the other one a field
    type Obj = object
      field: int

    var
      a = 1
      b = Obj(field: 2)
    swap(a, b.field)

    doAssert a == 2
    doAssert b.field == 1

  swapFieldAndLocal()