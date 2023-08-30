discard """
  targets: "c js vm"
  description: '''
    Regression test for making sure ``.cursor`` locals work in closure
    iterators
  '''
"""

type Value = distinct int

var numDestroy = 0

proc `=destroy`(x: var Value) =
  inc numDestroy

iterator iter(s: seq[Value]): int {.closure.} =
  # because it is used across yields, `s2` is lifted into the iterator's
  # environment. Since non-ref cursors in object didn't have their hooks
  # disabled inside the environments lifted hooks, this led to double
  # frees
  var s2 {.cursor.} = s
  var i = 0
  let L = s2.len
  while i < L:
    yield s2[i].int
    inc i

proc test() =
  var s = @[Value(1), Value(2)]
  let cl = iter
  # make sure resuming the iterator works:
  doAssert cl(s) == 1
  doAssert cl(s) == 2
  doAssert cl(s) == 0

test()
doAssert numDestroy == 2