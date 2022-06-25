discard """
description: '''
Basics of sequences:
- dynamically sized list of a single type
- literals
- indexing
- length
'''
"""

block literal:
  let
    a: seq[int] = @[]
    b = @[1, 2, 3]
  doAssert a == @[],           "two empty seqs are equal"
  doAssert b == @[1, 2, 3],    "seq elements are compared by element pair"
  doAssert b != @[1, 2, 3, 4], "seq's don't partially match"

block seq_len:
  var
    a: seq[int]
    b = @[1, 2, 3, 4, 5, 6, 7, 8 , 9, 10]
  doAssert a.len == 0,  "an empty seq has len 0"
  doAssert b.len == 10, "seq len is the count of elements"

block seq_indexing:
  var a = @[1, 2]

  doAssert a[0] == 1, "seqs by default are zero based indexed"
  a[0] = 4
  doAssert a[0] == 4, "seqs update in place"

block seq_different_types:
  # seqs can have more than just the `int` type
  var
    a = @["foo", "bar", "bar"]
    b = @[@[1], @[2, 3], @[4, 5, 6]]
  doAssert a[2] == "bar"
  a[2] = "baz"
  doAssert a[2] == "baz", "element was updated"
  doAssert b[1][1] == 3,  "chain index look ups work"
