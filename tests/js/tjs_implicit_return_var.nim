discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/11354
    compiler error related to implicit return and return var type
  . Current Output
    foob.nim(8, 3) Error: internal error: genAddr: nkStmtListExpr
'''
"""

type
  TrackySeq[T] = object
    s: seq[T]
    pos: int

proc foobar(ls: var TrackySeq[seq[int]], i: int): var seq[int] =
  echo ls.pos # removing this, or making the return explicit works
  doAssert ls.pos == 0
  ls.s[i]

var foo: TrackySeq[seq[int]]
foo.s.add(@[0])
foo.foobar(0).add(1)
doAssert foo.s ==  @[@[0, 1]]
