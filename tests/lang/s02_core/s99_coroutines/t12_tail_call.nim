discard """
  output: "1\n2\n3\n4\n"
"""

## The built-in ``tail`` routine is used for passing control to a coroutine
## from within another coroutine, without yielding control back to the
## ``resume`` call. A ``suspend`` within a tail-called coroutine then yields
## control to the original ``resume``.
##
## Only coroutine instances where the return type is known can be passed to
## ``tail``.

proc a() {.coroutine.} =
  echo "2"
  suspend()
  echo "3"

proc b() {.coroutine.} =
  echo "1"
  tail a()
  echo "4"

var instance = b()
var c = resume(instance)

## The originally resumed coroutine instance is still running in this case.
doAssert c != instance
doAssert instance.status == csRunning
doAssert c.status == csSuspended

doAssert resume(c) == instance

## Upon returning from tail-called instance, it is automatically finished.
doAssert c.status == csFinished
