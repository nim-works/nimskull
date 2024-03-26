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
## This is achieved by calling the built-in ``suspend`` routine, which expects
## a ``CoroutineBase`` as the parameter -- the instance passed to ``suspend``
## is what ``resume`` will return. It is legal pass the currently running
## coroutine (i.e., `self`) to ``suspend``.

# IDEA: instead of the ``suspend`` routine, the ``yield`` keyword could be
#       re-used, which could be much more convenient to use. It'd also be
#       a bit easier to implement

proc coro() {.coroutine.} =
  echo "coro: 1"
  suspend(self)
  echo "coro: 2"
  suspend(self)
  echo "coro: 3"

var instance = coro()
let original = instance

echo "outside: 1"
instance = Coroutine[void](resume(instance))

## After suspending, the coroutine is the "suspended" state.
doAssert instance.status == csSuspended

echo "outside: 2"
instance = Coroutine[void](resume(instance))
echo "outside: 3"
instance = Coroutine[void](resume(instance))

doAssert instance.status == csPending
doAssert instance == original
