discard """
  description: '''
    Type classes of built-in generic types can be specified as constraints on
    generic type parameters
  '''
"""

type
  Distinct1 = distinct int
  Enum1 = enum a
  Object1 = object

proc test[T: object](x: T): int = 1
proc test[T: distinct](x: T): int = 2
proc test[T: enum](x: T): int = 3
proc test1[T: Ordinal](x: T): int = 4
proc test[T: array](x: T): int = 5
proc test[T: tuple](x: T): int = 6
proc test[T: set](x: T): int = 7
proc test[T: range](x: T): int = 8
proc test[T: ptr](x: T): int = 9
proc test[T: ref](x: T): int = 10
proc test[T: seq](x: T): int = 11
proc test[T: proc](x: T): int = 12
proc test[T: openArray](x: T): int = 13
proc test1[T: varargs](x: T): int = 14
proc test[T: UncheckedArray](x: ptr T): int = 15

doAssert test(Object1()) == 1
doAssert test(Distinct1(1)) == 2
doAssert test(a) == 3
doAssert test1(1) == 4
doAssert test([1]) == 5
doAssert test((1,)) == 6
doAssert test({1}) == 7
doAssert test(range[1..2](1)) == 8
doAssert test((ptr int)(nil)) == 9
doAssert test((ref int)(nil)) == 10
doAssert test(@[1]) == 11
doAssert test((proc (): int)(nil)) == 12
doAssert test(toOpenArray([1], 0, 0)) == 13
doAssert test1([1]) == 14
doAssert test((ptr UncheckedArray[int])(nil)) == 15
