
## To launch a coroutine, the syntax for routine invocation is used. Unlike a
## normal procedure, this doesn't call the coroutine; instead, it constructs
## a coroutine instance. The expression is referred to as a *coroutine
## construction* (coroutine instantiation?).
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

let instance = coro(1)
doAssert instance is Coroutine[void]
doAssert instance.status == csSuspended

## It is legal to do nothing with an instantiated coroutine.
