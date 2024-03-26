
## What operations are valid on a coroutine instance depends on its state.
## If performing an operation on a coroutine while in a state where the
## operation is not applicable, a ``CoroutineError`` is raised. The coroutine
## instance is not modified in this case.

# XXX: a catchable error could be annoying in `.raises: []` routines where
#      it's guaranteed that the operation is valid. Perhaps it should
#      be a defect?

proc coro() {.coroutine.} =
  discard

let instance = coro()
doAssert instance.status == csSuspended

var wasError = false
# unwrap only legal when instance was aborted
try:
  discard unwrap(instance)
except CoroutineError as e:
  wasError = true

doAssert wasError
