discard """
  description: '''
    Regression test for ensuring that type-class constraints work when applied
    to multiple generic type parameters at the same time
  '''
"""

type
  Object1 = object
  Object2 = object

  Enum1 = enum a
  Enum2 = enum b

  Distinct1 = distinct int
  Distinct2 = distinct int

var
  arr1 = [1]
  arr2 = [""]

# for this test, it's important that the ``a, b: Z`` syntax is used, and not
# the ``a: Z, b: Z`` one

proc test[A, B: distinct](a: A, b: B): int = 1
proc test[A, B: enum](a: A, b: B): int = 2
proc test1[A, B: Ordinal](a: A, b: B): int = 3
proc test[A, B: array](a: A, b: B): int = 4
proc test[A, B: tuple](a: A, b: B): int = 5
proc test[A, B: object](a: A, b: B): int = 6
proc test[A, B: set](a: A, b: B): int = 7
proc test[A, B: range](a: A, b: B): int = 8
proc test[A, B: ptr](a: A, b: B): int = 9
proc test[A, B: ref](a: A, b: B): int = 10
proc test[A, B: seq](a: A, b: B): int = 11
proc test[A, B: proc](a: A, b: B): int = 12
proc test1[A, B: openArray](a: A, b: B): int = 13
proc test1[A, B: UncheckedArray](a: ptr A, b: ptr B): int = 14

doAssert test(Distinct1(1), Distinct2(1)) == 1
doAssert test(a, b) == 2
doAssert test1(1, 'c') == 3
doAssert test(arr1, arr2) == 4
doAssert test((1,), ("",)) == 5
doAssert test(Object1(), Object2()) == 6
doAssert test({1}, {'2'}) == 7
doAssert test(range[0..1](0), range[3..4](3)) == 8
doAssert test((ptr int)(nil), (ptr float)(nil)) == 9
doAssert test((ref int)(nil), (ref float)(nil)) == 10
doAssert test(@[1], @[""]) == 11
doAssert test((proc(a: int))(nil), (proc(a: float))(nil)) == 12
doAssert test1(arr1, arr2) == 13
doAssert test1((ptr UncheckedArray[int])(nil), (ptr UncheckedArray[float])(nil)) == 14