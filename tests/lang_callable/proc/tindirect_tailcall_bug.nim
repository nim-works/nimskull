discard """
  targets: "c js vm"
"""

block:
  # parameters had garbage values when the first parameter of the indirectly-
  # called tailcall routine is a pass-by-reference parameter. This only
  # affected the C backend
  type Large = object
    ## something that's passed by reference
    pad: array[256, int]

  proc test(x: Large, y: int) {.tailcall.} =
    doAssert y == 128

  var p = test
  p(Large(), 128)

block:
  # parameters had garbage values when the first parameter of the indirectly-
  # called tailcall routine is a an openArray. This affected both the C and JS
  # backend
  proc test(x: openArray[int], y: int) {.tailcall.} =
    doAssert x[1] == 128
    doAssert y == 256

  var p = test
  p([1, 128, 2], 256)
