discard """
description: '''
Describe the basics of arrays:
- fixed size list of a single type
- literals
- indexing
- length
'''
"""

block literal:
  ## It is possible to construct an empty array literal
  let a: array[0, int] = []

  let b = [1, 2, 3]
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

block array_indexing_with_offset:
  var index: array[2 .. 10, int] = [0, 1, 2, 3, 4, 5, 6, 7, 8]

  doAssert len(index) == 10 - 1
  doAssert high(index) == 10
  doAssert low(index) == 2

  doAssert index[2] == 0
  doAssert index[10] == 8

block curly_literal:
  block single_pairs:
    let curly = {
      "key1": "value1",
      "key2": "value2"
    }

    doAssert curly is array[2, (string, string)], "{} is an array literal as well"
    doAssert curly[0] == ("key1", "value1")
    doAssert curly[1] == ("key2", "value2")

  block multi_key:
    var used = 0
    when not defined(js) or defined(tryBrokenSpecification):
      # FIXME this code behaves completely differently on the JS backend - it
      # results in `[("key1", 3), ("key2", 3), ("key3", 3)]`
      let curly = {
        "key1", "key2": (inc used; used),
        "key3": (inc used; used)
      }


      doAssert used == 3, "Expression is evaluated for each key"
      doAssert curly == [
        ("key1", 1),
        ("key2", 2),
        ("key3", 3)
      ]

block key_value_bracket:
  let bracket = [
    0: "val0",
    1: "val1",
    2: "val2"
  ]

  doAssert bracket is array[3, string]
  doAssert bracket == ["val0", "val1", "val2"]

block array_different_types:
  # arrays can have more than just the `int` type
  var
    a = ["foo", "bar", "bar"]
    b = [[1], [2], [3]]
  doAssert a[2] == "bar"
  a[2] = "baz"
  doAssert a[2] == "baz", "element was updated"
  doAssert b[1][0] == 2,  "chain index look ups work"
