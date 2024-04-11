discard """
  output: "1\n2\n3\n4\n"
"""

# TODO: fix the test order

## If the coroutine that control is passed to returns a value, the ``tail``
## call will return it.

proc a(): int {.coroutine.} =
  echo "2"
  return 3

proc b() {.coroutine.} =
  echo "1"
  echo tail(a())
  echo "4"

var instance = resume b()
doAssert instance.status == csPending
