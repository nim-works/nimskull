discard """
  description: '''
    Ensure that the bound checks for `toOpenArray` work correctly.
  '''
  targets: "c js vm"
  knownIssue.vm: "Bound checks for `toOpenArray` are not implemented"
"""

block from_array:
  var arr: array['a'..'b', int]
  let lo = 'c'
  let hi = 'd'
  doAssertRaises(RangeDefect):
    # range defect, not an index defect, as `lo` and `hi` are outside
    # the index type's range
    discard toOpenArray(arr, lo, hi)

block from_non_zero_based_array:
  var arr: array[4..8, int]
  let lo = 0
  let hi = 1
  doAssertRaises(RangeDefect):
    # range defect, not an index defect, as `lo` and `hi` are outside
    # the index type's range
    discard toOpenArray(arr, lo, hi)

block from_seq:
  var s = @[1, 2, 3, 4, 5]
  doAssertRaises(IndexDefect):
    discard toOpenArray(s, 0, -2)
