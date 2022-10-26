discard """
  nimout: '''1
2
3
[1, 2, 3]'''
  output: '''1
2
3
[1, 2, 3]'''
description: '''
  . From https://github.com/nim-lang/Nim/issues/12640
    C-Backend link failure for let with { .global, compileTime.} pragma
  . Current Output
    ... undefined reference to `bug__maw0iw4sXYmMR4fqohWCPA'
'''
"""


proc doIt(a: openArray[int]) =
 echo a

proc foo() = 
  var bug {.global, compiletime.}: seq[int]
  bug = @[1, 2 ,3]
  for i in 0 .. high(bug): echo bug[i]
  doIt(bug)

static:
  foo()
foo()