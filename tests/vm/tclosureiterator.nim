discard """
  target: native
"""

iterator iter*(): int {.closure.} =
  yield 3

static:
  var x = iter
  doAssert x() == 3
  discard x()
  doAssert finished(x)
