discard """
  output: "here"
"""

## To resume a coroutine instance (that is, run it until the next suspension
## point is reached), the built-in nullary ``resume`` routine is used. The
## ``resume`` procedure returns a coroutine instance, which, by default, is
## the same one that was resumed. Only an instance that is in the "suspended"
## state can be resumed.
##
## Before passing control to the coroutine, ``resume`` sets the instance's
## status to "running"

proc coro() {.coroutine.}
  echo "here"

let instance = launch coro()
discard resume(instance) # the echo will be executed

## When a suspension point is reached, ``resume`` returns. If there's no more
## code to run within the coroutine, prior to returning from ``resume``, the
## state is set to "pending", otherwise it's set to "suspendend".
