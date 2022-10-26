discard """
cmd: '''nim c --gc:arc $file'''
description: '''
 . From https://github.com/nim-lang/Nim/issues/12037
   Newruntime: seq copy invalidates original seq

 . From https://github.com/nim-lang/Nim/issues/12820
   @[] is a problem for --gc:destructors

 . From https://github.com/nim-lang/Nim/issues/12989
   ARC: Unpacking tuple with seq causes segfault
   '''
"""

proc test() =
  var sq1 = @[42]
  doAssert sq1 is seq[int]
  doAssert sq1.len == 1
  doAssert cast[int](sq1[0].addr) != 0

  var sq2 = sq1 # copy of original

  doAssert sq2.len == sq1.len
  doAssert sq2 == sq1
  doAssert cast[int](sq2[0].addr) != 0
  doAssert cast[int](sq1[0].addr) != 0

test()


#############################################
### bug 12820
import tables
var t = initTable[string, seq[ptr int]]()
discard t.hasKeyOrPut("f1", @[])


#############################################
### bug #12989
proc bug(start: (seq[int], int)) =
  let (s, i) = start

let input = @[0]
bug((input, 0))