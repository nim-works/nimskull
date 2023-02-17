discard """
  description: '''
    Tests to make sure that the left-to-right evaluation order is respected for
    assignments involving types with destructors
  '''
  target: "c js !vm"
  matrix: "--cursorInference:off"
"""

## knownIssue: no destructor injection is performed for the VM target

import mhelper

proc testBlockExpr() =
  var test = 0
  var x: Value[int]
  (test = 1; x) = block:
    doAssert test == 1
    initValue(1)

  doAssert x.content == 1

proc testIfExpr() =
  var test = 0
  var x: Value[int]
  (test = 1; x) =
    if test == 1:
      initValue(1)
    else:
      initValue(2)

  doAssert x.content == 1

proc testCaseExpr() =
  var test: range[0..2] = 0
  var x: Value[int]
  (test = 1; x) =
    case test
    of 0, 2: doAssert false, "evaluation order violation"; initValue(2)
    of 1: initValue(1)

  doAssert x.content == 1

proc testTryExpr() =
  var test = 0
  var x: Value[int] ## something that has a destructor

  # check that the effects of the lhs are evaluated before the ``except``
  # clause:
  (test = 1; x) =
    try:
      if true:
        raise CatchableError.newException("")

      # we can't use the raise directly, as the ``try``-clause needs a type
      initValue(2)
    except:
      doAssert test == 1
      initValue(1)

  doAssert x.content == 1

  # check that the effects of the lhs are evaluated before the ``try`` clause:
  test = 0

  (test = 1; x) =
    try:
      if test != 1:
        raise CatchableError.newException("")

      initValue(1)
    except:
      # if we reach here, the assertion above failed
      doAssert false, "evaluation order violation"
      initValue(2)

  doAssert x.content == 1

testBlockExpr()
testIfExpr()
testCaseExpr()
testTryExpr()