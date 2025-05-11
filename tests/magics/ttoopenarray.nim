discard """
  description: '''
    Specify the behaviour of the ``toOpenArray`` built-in procedure
  '''
  targets: "c js vm"
"""

template test(a, b: untyped) =
  ## Temporary helper template for a comparison that is expected to fail with
  ## the JS backend.
  {.line.}:
    when defined(js):
      doAssert a != b
    else:
      doAssert a == b

proc toSeq(a: openArray[int]): seq[int] {.noinline.} =
  ## Intended to prevent comparisons being folded away by the compiler.
  for x in a.items:
    result.add x

block from_array_construction:
  # test with an array construction as the array operand
  doAssert toSeq(toOpenArray([1, 2, 3], 0, 0)) == [1]
  doAssert toSeq(toOpenArray([1, 2, 3], 0, 2)) == [1, 2, 3]

block from_postive_range_based_array:
  var arr: array[8..12, int] = [11, 12, 13, 14, 15]

  # knownIssue: the bounds are not properly offset
  test toSeq(toOpenArray(arr, 8, 12)), [11, 12, 13, 14, 15]

block from_negative_range_based_array:
  var arr: array[-3 .. -1, int] = [1, 2, 3]
  # knownIssue: the bounds are not properly offset
  test toSeq(toOpenArray(arr, -3, -1)), [1, 2, 3]
  doAssert toSeq(toOpenArray(arr, 0, -1)) == []
  doAssert toSeq(toOpenArray(arr, -3, -4)) == []

block from_seq:
  var s = @[1, 2, 3, 4, 5]
  doAssert toSeq(toOpenArray(s, 1, 3)) == [2, 3, 4]

  # test creating an empty openArray
  # https://github.com/nim-lang/nim/issues/7904
  doAssert toSeq(toOpenArray(s, 0, -1)) == []
  doAssert toSeq(toOpenArray(s, 1, 0)) == []

  doAssert toSeq(toOpenArray(s, 9, 8)) == []
  doAssert toSeq(toOpenArray(s, 0, -1)) == []
  doAssert toSeq(toOpenArray(s, 1, 0)) == []

proc first(a: openArray[int]): int =
  toSeq(toOpenArray(a, 0, 0))[0]

# test openArray of openArray
block from_openArray:
  var s = @[1, 2, 3]

  proc testEmpty(a: openArray[int]) =
    doAssert toSeq(toOpenArray(a, 0, -1)) == []

  # create empty openArray from empty openArray
  testEmpty(toOpenArray(s, 0, -1))
  testEmpty(toOpenArray(s, 1, 0))
  # create empty openArray from non-empty openArray
  testEmpty(toOpenArray(s, 1, 2))
  doAssert first(toOpenArray(s, 1, s.len-1)) == 2
