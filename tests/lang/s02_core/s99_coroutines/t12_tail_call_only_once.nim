## A coroutine instance can only be tail-called once.

proc first() {.coroutine.} =
  suspend()

proc coro(other: Coroutine[void]) {.coroutine.} =
  tail(other)

var f = first()
resume(coro(f))
doAssert f.status == csSuspended

var instance = resume(coro(f))
doAssert instance.status == csAborted
doAssert unwrap(instance).msg == "cannot tail-call again"
