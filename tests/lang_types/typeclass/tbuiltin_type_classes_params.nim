discard """
  description: '''
    Type classes of built-in generic types can be used as the type of routine
    parameters and return types
  '''
"""

type
  Distinct1 = distinct int
  Enum1 = enum a
  Object1 = object

proc test(x: object): object = x
proc test(x: distinct): distinct = x
proc test(x: enum): enum = x
proc test1(x: Ordinal): Ordinal = x
proc test(x: array): array = x
proc test(x: tuple): tuple = x
proc test(x: set): set = x
proc test(x: range): range = x
proc test(x: ptr): ptr = x
proc test(x: ref): ref = x
proc test(x: seq): seq = x
proc test(x: proc): proc = x
proc test(x: ptr UncheckedArray): ptr UncheckedArray = x

# openArray and varargs are not supported as return types
proc test(x: openArray): int = 1
proc test1(x: varargs): int = 2

doAssert test(Object1()) == Object1()
doAssert test(Distinct1(1)).int == 1
doAssert test(a) == a
doAssert test1(1) == 1
doAssert test([1]) == [1]
doAssert test((1,)) == (1,)
doAssert test({1}) == {1}
doAssert test(range[1..2](1)) == 1
doAssert test((ptr int)(nil)) == nil
doAssert test((ref int)(nil)) == nil
doAssert test(@[1]) == @[1]
doAssert test((proc (): int)(nil)) == nil
doAssert test(toOpenArray([1], 0, 0)) == 1
doAssert test1([1]) == 2
doAssert test((ptr UncheckedArray[int])(nil)) == nil