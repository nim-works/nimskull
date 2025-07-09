discard """
  description: '''
    Ensure that the bound checks for `toOpenArray` work correctly.
  '''
  targets: "c js vm"
  knownIssue.vm: "Bound checks for `toOpenArray` are not implemented"
"""

block from_postive_range_based_array:
  var arr: array[8..12, int] = [11, 12, 13, 14, 15]
  doAssertRaises(IndexDefect):
    discard toOpenArray(arr, 10, 8)

block from_negative_range_based_array:
  var arr: array[-3 .. -1, int] = [1, 2, 3]
  doAssertRaises(IndexDefect):
    discard toOpenArray(arr, -4, -1)
  doAssertRaises(IndexDefect):
    discard toOpenArray(arr, -1, 0)
  doAssertRaises(IndexDefect):
    discard toOpenArray(arr, -1, -3)

block from_seq:
  var s = @[1, 2, 3, 4, 5]
  doAssertRaises(IndexDefect):
    discard toOpenArray(s, 0, -2)
