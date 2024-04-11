discard """
  output: "caught: a"
"""

# TODO: fix the test order

## Exceptions propagate across tail call boundaries into the calling coroutine.

proc a() {.coroutine.} =
  suspend()
  raise CatchableError.newException("a")

proc b(catch: bool) {.coroutine.} =
  if catch:
    try:
      tail a()
    except CatchableError as e:
      echo "caught: ", e.msg
  else:
    tail a()

var instance = b(false)
var c = resume(instance)

doAssert resume(c) == instance
## Upon exception propagation leaving the tail call, the raising tail-called
## coroutine instance moves into the "finished" state.
doAssert c.status == csFinished
## If not caught within the calling coroutine, the raised exception aborts the
## latter.
doAssert instance.status == csAborted
doAssert unwrap(instance).msg == "a"

instance = b(true)
resume(resume(instance))
## Catching the exception stops error propagation, as usual.
doAssert instance.status == csPending