discard """
labels: "char codegen conversion int"
description: '''
  . From https://github.com/nim-lang/Nim/issues/10128
    char to int conversion in string lookup leads to index out of bounds
  . A signed char (in C) wraps around at 128 to -128
    The conversion from -128 to NU (uint) then produces 18446744073709551488
    because the signed byte is filled up with 0xff bytes and then reinterpreted
    as unsigned.
'''
"""

let data = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
var seq2 = newSeq[char](data.len)
for i in 0..<data.len:
  seq2[i] = data[i]

let c = '\128'

block case1:
  doAssert data[c.int] == 'y'
  doAssert seq2[c.int] == 'y'


block case2:
  proc play(x: openArray[char]) =
    doAssert x[c.int] == 'y'

    play(data)
    play(seq2)