discard """
  output: "custom"
"""

## The ``suspend`` built-in procedure is not special-cased with regards to
## lookup. It can be overloaded like any other routine.

template suspend() =
  echo "custom"
  system.suspend()

# XXX: this could be a problem. A module could export a routine such as the
#      above and thus override all suspend calls in the importing module!

proc coro() {.coroutine.} =
  suspend()

var instance = coro()
resume(instance)
