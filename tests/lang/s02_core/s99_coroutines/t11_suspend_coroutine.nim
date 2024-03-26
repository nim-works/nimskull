discard """
  output: '''
outside: 1
coro: 1
outside: 2
coro: 2
outside: 3
coro: 3'''
"""

## A coroutine can be suspended without returning or raising an exception.
## This is achieved by calling the built-in ``suspend`` routine. On calling
## ``suspend``, execution of the coroutine halts, and control is given back to
## the callsite of the ``resume`` call that previously resumed execution of
## the instance.
##
## When suspended this way, the ``resume`` that control is given back to
## returns the instance it was invoked with.

# IDEA: instead of the ``suspend`` routine, the ``yield`` keyword could be
#       re-used, which could be much more convenient to use. It'd also be
#       a bit easier to implement

proc coro() {.coroutine.} =
  echo "coro: 1"
  suspend()
  echo "coro: 2"
  suspend()
  echo "coro: 3"

var instance = coro()

echo "outside: 1"
doAssert resume(instance) == instance

## After suspending, the coroutine is in the "suspended" state.
doAssert instance.status == csSuspended

echo "outside: 2"
doAssert resume(instance) == instance
echo "outside: 3"
doAssert resume(instance) == instance

doAssert instance.status == csPending
