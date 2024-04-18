
## A coroutine (not a coroutine *instance*) has a type itself.

proc coro() {.coroutine.} =
  discard

doAssert coro is (proc() {.nimcall, coroutine.})
doAssert coro isnot (proc(x: int) {.nimcall, coroutine.})
doAssert coro isnot (proc(): int {.nimcall, coroutine.})

## Same as with non-coroutine procedural types, a coroutine procedural type
## without an explicit calling convention is a closure, by default.

doAssert coro isnot (proc() {.coroutine.})
doAssert (proc() {.coroutine.}) is (proc() {.closure, coroutine.})

## If the coroutine doesn't close over outer locals, it's not a closure
## coroutine.
