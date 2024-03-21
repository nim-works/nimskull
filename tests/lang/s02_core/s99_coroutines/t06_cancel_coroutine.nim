
## A coroutine can be cancelled, by raising an exception and letting it
## propagate to the edge of the coroutine. Upon cancellation, the
## ``resume`` call that started execution changes the state of the instance
## to "aborted" and returns the instance.

proc coro(cancel: bool) {.coroutine.}
  if cancel:
    raise CatchableError.newException("cancel")

let instance = coro(true)
doAssert instance.status == csSuspended

resume(instance)
doAssert instance.status == csAborted

## If a coroutine instance is cancelled, the exception that caused
## cancellation is stored in the coroutine instance object. It can be
## extracted by using the built-in ``unwrap``.

# XXX: unwrap needs a better name

let error = unwrap(instance)
doAssert error of CatchableError
doAssert error.msg == "fail"

## The call to ``unwrap`` moves the instance into the "finished" state.
doAssert instance.status == csFinished
