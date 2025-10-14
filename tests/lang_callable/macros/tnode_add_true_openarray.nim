discard """
  description: '''
    Regression test for passing a real `openArray` value to
    `add(NimNode, varargs[NimNode])` not working.
  '''
  targets: native
  action: compile
"""

import std/macros

proc test() =
  let arr = [newIntLitNode(1), newIntLitNode(2), newIntLitNode(3)]
  var n = nnkStmtList.newNimNode()
  n.add(arr.toOpenArray(1, 2))
  doAssert n.len == 2
  doAssert n[0].intVal == 2
  doAssert n[1].intVal == 3

static: test()
