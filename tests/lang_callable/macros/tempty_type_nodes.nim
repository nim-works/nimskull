discard """
  description: '''
    Ensure that untyped `nnkEmpty` nodes used in type positions
    result in a proper error
  '''
  target: native
  matrix: "--errorMax:100"
  action: reject
"""

import std/macros

macro test1(): untyped =
  # test case #1: untyped ``nnkEmpty`` node in type position
  nnkConv.newTree(newNimNode(nnkEmpty)): #[tt.Error
                            ^ type expected, but expression has no type]#
    newNimNode(nnkEmpty)

test1()
