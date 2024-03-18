discard """
  output: "1"
"""

## A coroutine can be a generic routine.

proc coro[T](x: T) {.coroutine.} =
  echo x

var instance = launch coro(1)
doAssert instance is Coroutine[int]
resume(instance)
