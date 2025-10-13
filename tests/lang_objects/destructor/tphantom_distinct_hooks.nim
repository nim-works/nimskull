discard """
  description: '''
    Ensure that phantom distinct types can use generic hooks, and that every
    instance gets its own instantiations thereof
  '''
  targets: "c js vm"
  matrix: "--cursorInference:off"
"""

type
  Phantom[T] = distinct int

var trace: seq[string]

proc `=copy`[T](x: var Phantom[T], y: Phantom[T]) =
  trace.add("copy " & $T)

proc `=sink`[T](x: var Phantom[T], y: Phantom[T]) =
  trace.add("sink " & $T)

proc `=destroy`[T](x: var Phantom[T]) =
  trace.add("destroy " & $T)

proc test[T](a: sink Phantom[T] = default(Phantom[T])) =
  var x: Phantom[T]
  x = a # copies
  x = a # sinks
  # `x` is destroyed

test[int]()
test[float]()

# make sure that both types got their own hook instantiations
doAssert trace == ["copy int", "sink int", "destroy int",
                   "copy float", "sink float", "destroy float"],
         $trace
