discard """
ccodecheck: "__restrict__"
joinable: false
labels: "codegen iterator pragma"
description: '''
  . From https://github.com/nim-lang/Nim/issues/6497
    Pragma codegenDecl doesn't work inside iterators
  . I need to use __restrict__ C keyword inside a iterator,
    but when inside an iterator codegenDecl does not work.
  . The issue is at transformVarSection in tranf.nim
  . See also https://github.com/nim-lang/Nim/pull/16027
'''
"""

iterator myitems(s: seq[int]): int =
  var data {.codegenDecl: "$# __restrict__ $#".} : ptr int = nil
  yield 1

for i in @[1].myitems:
  doAssert i == 1