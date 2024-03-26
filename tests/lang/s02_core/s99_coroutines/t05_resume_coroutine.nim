discard """
  output: "here"
"""

## To resume a coroutine instance (that is, run it until the next suspension
## point is reached), the built-in nullary ``resume`` procedure is used. The
## ``resume`` procedure returns the coroutine instance that the coroutine
## suspended with. Only an instance that is in the "suspended" state can be
## resumed.
##
## Before passing control to the coroutine, ``resume`` sets the instance's
## status to "running"

proc coro() {.coroutine.} =
  echo "here"

let instance = coro()
discard resume(instance) # the echo will be executed

## When a suspension point is reached, ``resume`` returns. If there's no more
## code to run within the coroutine, prior to returning from ``resume``, the
## status is set to "pending", otherwise it's set to "suspendend".
