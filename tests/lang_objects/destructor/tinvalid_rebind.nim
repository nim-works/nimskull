discard """
errormsg: "cannot bind another '=destroy' to: Foo; previous declaration was constructed here implicitly: tinvalid_rebind.nim(10, 7)"
line: 12
"""

type
  Foo[T] = object

proc main =
  var f: Foo[int]

proc `=destroy`[T](f: var Foo[T]) =
  discard
