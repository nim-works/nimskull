discard """
  description: '''
    Ensure that a NimNode can be passed as a static parameter to a macro.
  '''
  action: compile
"""

import std/macros

macro m(n: static NimNode) =
  doAssert n.kind == nnkStmtList
  doAssert n.len == 0

m(newStmtList())
