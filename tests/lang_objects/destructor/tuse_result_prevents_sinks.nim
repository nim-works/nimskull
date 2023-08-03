discard """
  output: ""
  targets: "c js vm"
"""

# bug #9594

type
  Foo = object
    i: int

proc `=`(self: var Foo; other: Foo) =
  self.i = other.i + 1

proc `=sink`(self: var Foo; other: Foo) =
  self.i = other.i

proc `=destroy`(self: var Foo) = discard

template preventCursorInference(x) =
  let p = addr(x)

proc test(): Foo =
  result = Foo()
  let temp = result
  preventCursorInference temp
  doAssert temp.i > 0
  return result

proc testB(): Foo =
  result = Foo()
  let temp = result
  preventCursorInference temp
  doAssert temp.i > 0

discard test()
discard testB()
