
## If a coroutine has no result (i.e., the return type is ``void``), then
## ``finish`` returns nothing and only changes the status from "pending" to
## "finished".

proc coro() {.coroutine.} =
  discard

var instance = coro()

resume(instance)
doAssert instance.status == csPending

finish(instance)
doAssert instance.status == csFinished
