
## The custom type to use for the coroutine can be generic too.

type Custom[T] = ref object of Coroutine[void]
  value: T

proc coro[T]() {.coroutine: Custom[T].} =
  discard

var instance = launch coro[int]()
doAssert instance is Custom[int]
