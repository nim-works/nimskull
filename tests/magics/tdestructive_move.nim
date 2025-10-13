discard """
  description: "Ensure that a `move` resets the source location"
  targets: "c js vm"
"""

type Object = object
  a, b: int

# test with primitive type:
var a = 1
let b = move(a)
doAssert b == 1
doAssert a == 0

# test with aggregate type:
var c = Object(a: 1, b: 2)
let d = move(c)
doAssert d.a == 1 and d.b == 2
doAssert c.a == 0 and c.b == 0
