discard """
  description: '''
    Ensure that accessing an array works when the index operand's type cannot
    be safely converted to the array's index type
  '''
  knownIssue.vm: '''
    Arrays with a start index outside of -128..127 crash the code generator
  '''
"""

proc test1(index: int): int =
  # try with an index range that overlaps with the `int` range
  var arr: array[uint(high(int))..(uint(high(int)) + 2), int] = [1, 2, 3]
  # not all valid array indices can be represented by `int`
  result = arr[index]

# the index is valid, no index error must be raised
doAssert test1(high(int)) == 1

proc test2(index: uint): int =
  var arr: array[-1..1, int] = [1, 2, 3]
  # not all valid array indices can be represented by `uint`
  result = arr[index]

doAssert test2(1) == 3

# regression test: make sure that an index that is valid after conversion to
# uint gets caught by the index check
when not defined(vm): # catching defects is not supported by the VM
  doAssertRaises IndexDefect:
    discard test1(low(int))
