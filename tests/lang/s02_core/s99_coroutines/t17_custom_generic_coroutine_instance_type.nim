
## The custom type to use for the coroutine instance can be generic too.

type Custom[T] = ref object of Coroutine[void]
  value: T

proc coro[T]() {.coroutine: Custom[T].} =
  discard

var instance = coro[int]()
doAssert instance is Custom[int]
