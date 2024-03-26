## When suspending via ``suspend``, it is possible to specify the instance
## that the originating-from ``resume`` call should return.

proc other() {.coroutine.} =
  discard

var otherInst = other()

proc coro() {.coroutine.} =
  suspend(otherInst)

var instance = coro()
# hold on to the instance:
let original = instance

doAssert resume(instance) == otherInst
doAssert instance.status == csSuspended
doAssert original.status == csSuspended
