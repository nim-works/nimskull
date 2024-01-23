discard """
  description: '''
    Ensure that lifted locals declared inside loops are destroyed when control-
    flow reaches the statement again
  '''
  targets: "c js vm"
  knownIssue.js vm: "Heap cells (the closure environment) is not destroyed"
"""

import mhelper

# all tests are wrapped into procedures, otherwise the inner procedures would
# not use closures

proc test_no_value() =
  ## Test local definition with no initial value.
  var i = 0
  while i < 2:
    let o = initValue(1) # can be moved
    var o2: Value[int] # must destroy the previous value
    doAssert o2.content == 0 # make sure the location was reset
    o2 = o
    proc inner() =
      doAssert o2.content == 1
    inner()
    inc i

test_no_value()
doAssert numDestroy == 2
numDestroy = 0

proc test_with_value() =
  ## Test local definition with initial value.
  var i = 0
  while i < 2:
    let o = initValue(1) # can be moved
    let o2 = o # must destroy the previous value
    proc inner() =
      doAssert o2.content == 1
    inner()
    inc i

test_with_value()
doAssert numDestroy == 2
numDestroy = 0

proc test_tuple_unpacking() =
  ## Test local definition using tuple constructor destructuring syntax.
  var i = 0
  while i < 2:
    let o = initValue(1) # can be moved
    let (o2, other) = (o, 1) # must destroy the previous value
    proc inner() =
      doAssert o2.content == 1
    inner()
    inc i

test_tuple_unpacking()
doAssert numDestroy == 2
numDestroy = 0

proc get(): (Value[int], int) =
  (initValue(1), 1)

proc test_tuple_unpacking2() =
  ## Test local definition using tuple destructuring syntax.
  var i = 0
  while i < 2:
    let (o2, other) = get()
    proc inner() =
      doAssert o2.content == 1
    inner()
    inc i

test_tuple_unpacking2()
doAssert numDestroy == 2
