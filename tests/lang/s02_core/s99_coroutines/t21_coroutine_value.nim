
# XXX: maybe coroutine values should be named "first-class coroutine symbol"?

proc coro() {.coroutine.} =
  discard

## Coroutines can be assigned to locals...

proc p() =
  var local = coro

## ... globals ...

var global = coro

## ... stored in aggregates ...

var aggregate = (coro, 1)

## ..., and passed as parameters

proc test(param: proc() {.coroutine.}) =
  discard

test(coro)
