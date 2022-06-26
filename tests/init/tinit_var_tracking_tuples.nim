discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/8314
    Incorrect 'Cannot prove that 'y' is initialized' caused by tuple assignment
  . If a normal assignment is used instead, the warning disappears.
  . Fixed by https://github.com/nim-lang/Nim/pull/8321
'''
"""


proc foo(x: range[1..10]) : bool =
  block:
    var (y,) = (x,)
    doAssert y == x
  block:
    var (_,y) = (1,x)
    doAssert y == x
  block:
    var (y,_,) = (x,1,)
    doAssert y == x
  return true

doAssert foo(1) == true

