discard """
  output: "1\nhere"
"""

## Same as with normal routines, coroutines can be forward declared.

proc coroInt(x: int): int {.coroutine.}
proc coroVoid() {.coroutine.}

var instance = coroInt(1)
resume(instance)

var instance2 = coroVoid()
resume(instance2)

## Their body must be specified before the module is closed.

proc coroInt(x: int): int {.coroutine.} =
  echo x

proc coroVoid() {.coroutine.} =
  echo "here"
