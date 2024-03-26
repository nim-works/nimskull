
## A coroutine can be an inner procedure that closes over locals of the outer
## procedure.

proc outer() =
  var x = 0

  proc coro(): int {.closure, coroutine.} =
    x = 1
    suspend()
    result = x

  var instance = coro()

  doAssert x == 0
  resume(instance)
  doAssert x == 1

  # change the variable from the outer procedure
  x = 2
  resume(instance)
  doAssert finish(instance) == 2

outer()
