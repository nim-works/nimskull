
## It is possible to invoke a coroutine without using ``launch``; such call
## expands to ``trampoline(launch(call))``. The ``trampoline`` symbol
## is resolved in the context of the call.

proc trampoline(c: sink Coroutine[int]): int =
  # an example implementation
  var c = c
  while c.status == csSuspended:
    c = c.resume()
  result = c.finish()

proc coro(x: int): int {.coroutine.} =
  result = x

doAssert coro(2) == 2
