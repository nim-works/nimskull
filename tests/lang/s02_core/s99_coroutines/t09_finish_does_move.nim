
## Calling ``finish`` moves the result value out of the instance.

type Object = object
  value: int

# copying is disallowed
proc `=copy`(x: var Object, y: Object) {.error.}

proc coro(): Object {.coroutine.} =
  result = Object(value: 1)

var instance = launch coro()
resume(instance)

# the value is moved out of the instance, no copy is made
let o = finish(instance)
doAssert o.value == 1
