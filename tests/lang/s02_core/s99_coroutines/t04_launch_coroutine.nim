
## To launch a coroutine, the built-in ``launch`` routine is used. As its
## single parameter, it expects the invocation of a coroutine.
##
## What happens is, in the following order:
## 1. a managed heap cell of the coroutine's internal environment type is
##    allocated
## 2. the provided arguments are captured in the internal environment, either
##    by copying or moving, depending on the parameter
## 3. the status of the couroutine instance is set to the "suspended" state
##
## The instantiated coroutine is returned as the built-in ``Coroutine[T]``
## type (where `T` is the return type of the coroutine), which is a
## polymorphic ``ref`` type. A coroutine's internal environment type is always
## derived from the ``Coroutine[T]`` type.
##
## The body of the coroutine is not executed yet.

proc coro(x: int) {.coroutine.} =
  discard

# every routine invocation syntax is valid for ``launch``
let instance = launch coro(1)
doAssert instance is Coroutine[void]
doAssert instance.state == csSuspended

## It is legal to do nothing with an instantiated coroutine.
