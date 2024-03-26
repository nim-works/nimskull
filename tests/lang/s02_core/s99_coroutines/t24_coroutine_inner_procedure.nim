## Coroutines can have inner procedures that close over the coroutines'
## locals.

type Instance = ref object of Coroutine[int]
  callback: proc(): int

proc coro(): int {.coroutine: Instance.} =
  var x = 1
  proc inner(): int =
    result = x
    inc x

  self.callback = inner
  inc x
  suspend(self)

  result = x

## For coroutines, the created closure uses the instance type as it's
## environment, meaning that no extra allocation happens, and that the created
## closure keeps the coroutine instance alive.

var instance = coro()
resume(instance)
doAssert instance.callback() == 2

resume(instance)
doAssert finish(instance) == 3
