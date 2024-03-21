## Much like normal procedures, a coroutine can yield a result. The `result`
## variable is stored as part of the coroutine instance object. To extract,
## the result value, the built-in ``finish`` procedure needs to be called
## on a "pending" instance. An instance enters the "pending" state when
## the end of the coroutine's body is reached.

proc coro(): int {.coroutine.} =
  result = 1

let instance = coro()
resume(instance) # run to completion
doAssert instance.status == csPending

doAssert finish(instance) == 1

## A successful call to ``finish`` moves the instance into the "finished"
## state.
doAssert instance.status == csFinished

## An instance also enters the "pending" state when the coroutine exits due to
## a ``return`` being executed.

proc coro2(early: bool): int {.coroutine.} =
  if early:
    return 2
  return 1

var instance2 = coro2(true)

resume(instance2)
doAssert instance2.status == csPending

doAssert finish(instance2) == 2
doAssert instance2.status == csFinished

## "finished" is the terminal state. There's no other state that the instance
## can change to from it.
