discard """
  targets: "c js vm"
  description: '''
    Regression test for phantom object types that that use generic hooks
  '''
"""

type
  Phantom[T] = object

var trace: seq[string]

proc `=copy`[T](x: var Phantom[T], y: Phantom[T]) =
  trace.add("copy " & $T)

proc `=sink`[T](x: var Phantom[T], y: Phantom[T]) =
  trace.add("sink " & $T)

proc `=destroy`[T](x: var Phantom[T]) =
  trace.add("destroy " & $T)

proc test[T]() =
  var
    a = Phantom[T]()
    b = Phantom[T]()

  a = b # copies
  discard b # use 'b' in order to prevent a move above
  b = a # sinks

  # only 'b' still stores a value, meaning that only a single destructor is
  # called

test[int]()
test[float]()

# previously, both ``Phantom[int]`` and ``Phantom[float]`` were treated as the
# same type when lifting the hook procedures, meaning that both got the `int`
# hook
doAssert trace == ["copy int", "sink int", "destroy int",
                   "copy float", "sink float", "destroy float"],
         $trace