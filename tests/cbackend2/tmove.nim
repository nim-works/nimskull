discard """
  target: c
"""

type Complex = object
  x: seq[int]
  y: ref int

template makeTemp(name: untyped): untyped =
  var name = Complex(x: @[1, 2], y: new int)

block discard_move:
  makeTemp(a)
  discard move(a)

block tuple_assign:
  var ab = ("a", "b")
  let (x, y) = move ab
  doAssert x == "a"
  doAssert y == "b"

block call:
  proc p(c: Complex) =
    doAssert c.x == [1, 2]
    doAssert c.y[] == 0

  makeTemp(a)
  p(move a)

block obj_constr:
  type Obj = object
    c: Complex

  makeTemp(a)
  let o = Obj(c: move a)

block tuple_constr:
  makeTemp(a)
  let t = (x: move a)

block array_constr:
  makeTemp(a)
  let arr = [move a]