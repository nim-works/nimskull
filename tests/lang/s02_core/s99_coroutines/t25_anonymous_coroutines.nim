## An anonymous routine be turned into a coroutine too.

let coro = proc (x: int): int {.coroutine.} =
  result = x

var instance = coro(1)
resume(instance)
doAssert finish(instance) == 1
