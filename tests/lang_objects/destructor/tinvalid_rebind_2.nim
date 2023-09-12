discard """
errormsg: "cannot bind another '=destroy' to: Foo; previous declaration was constructed here implicitly: tinvalid_rebind_2.nim(14, 7)"
line: 16
"""

# compared to ``tinvalid_rebind.nim``, the ``Foo`` type here uses the generic
# parameter in its body

type
  Foo[T] = object
    x: T

proc main =
  var f: Foo[int]

proc `=destroy`[T](f: var Foo[T]) =
  discard
