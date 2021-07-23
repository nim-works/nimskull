discard """
description: '''
Describe the basics of arrays:
- fixed size list of a single type
- literals
- indexing
- length
'''
target: "c cpp js"
"""

block literal:
  let
    a: array[0, int] = []
    b = [1, 2, 3]
  doAssert a == [],           "two empty arrays are equal"
  doAssert b == [1, 2, 3],    "array elements are compared by element pair"
  doAssert b != [1, 2, 3, 4], "array's don't partially match"

block array_len:
  var
    a: array[0, int]
    b: array[10, int]
  doAssert a.len == 0,  "an empty array has len 0"
  doAssert b.len == 10, "arrays reserve the space upfront"

block array_indexing:
  var a = [1, 2]

  doAssert a[0] == 1, "arrays by default are zero based indexed"
  a[0] = 4
  doAssert a[0] == 4, "arrays update in place"

block array_different_types:
  # arrays can have more than just the `int` type
  var
    a = ["foo", "bar", "bar"]
    b = [[1], [2], [3]]
  doAssert a[2] == "bar"
  a[2] = "baz"
  doAssert a[2] == "baz", "element was updated"
  doAssert b[1][0] == 2,  "chain index look ups work"
