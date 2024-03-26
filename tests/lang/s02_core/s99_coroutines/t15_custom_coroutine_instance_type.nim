discard """
  output: "0"
"""

## The ``.coroutine`` pragma can be supplied with a type, which the internal
## coroutine instance object then derives from. The type must be a non-
## final ``ref`` sub-type of ``Coroutine[T]``, where `T` is the return type
## of the coroutine.

# TODO: a negative test is missing

type Custom = ref object of Coroutine[void]
  value: int

proc coro() {.coroutine: Custom.} =
  # the hidden `self` parameter is also of type ``Custom``
  echo self.value

## The constructed instance is of the provided custom instance type.
var instance = coro()
resume(instance)
doAssert typeof(instance) is Custom
doAssert instance.value == 0
