discard """
"""

## Normal `proc`s and `func`s become coroutines by applying the `.coroutine`
## pragma to their definition.

proc coro1() {.coroutine.} =
  discard

func coro2() {.coroutine.} =
  discard