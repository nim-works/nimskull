discard """
  description: "Tests that views are correctly treated as not owning"
  matrix: "--gc:arc --cursorInference:off"
  targets: "c !js !vm"
"""

import mhelper

template testCase(name: untyped, pairs: array, code: untyped) {.dirty.} =
  block:
    numCopies = 0
    numDestroy = 0

    # wrap the code in a procedure so that variables defined in it are not
    # globals (which would interfere with the test)
    proc inner() =
      code

    inner()

    doAssert (numCopies, numDestroy) in pairs

testCase "openarray", [(0, 2), (1, 3)]:
  let
    x = [initValue("a"), initValue("b")]
    y = toOpenArray(x, 0, 1)[0] # may copy

testCase "openarray", [(1, 3)]:
  var
    x = [initValue("a"), initValue("b")]
    y = toOpenArray(x, 0, 1)[0] # must copy

  # modify the source location so that a copy is required:
  x[0].content = "c"
  doAssert y.content == "a"