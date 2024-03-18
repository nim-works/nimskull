## It is valid to pass `nil` to a ``suspend`` call, in which case ``resume``
## will return `nil`.

proc coro() {.coroutine.} =
  suspend(nil)

var instance = launch coro()
# hold on to the instance:
var original = instance

doAssert resume(instance) == nil
doAssert original.status == csSuspended

# resume the instance. On reaching the body's end, the instance itself is
# returned
doAssert resume(original) == original
doAssert original.status == csPending
