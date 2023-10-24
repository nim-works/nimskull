discard """
  description: '''
    It is possible to create aliases for type classes of built-in generic
    types. Since they have a different name, they are lifted into separate
    generic parameter.
  '''
"""

type
  Distinct1 = distinct int
  Distinct2 = distinct int
  Enum1 = enum a
  Enum2 = enum b

  TcDistinct = distinct
  TcOrdinal = Ordinal
  TcArray = array
  TcSet = set
  TcRange = range
  TcPtr = ptr
  TcRef = ref
  TcSeq = seq
  TcProc = proc
  TcOpenArray = openArray
  TcVarargs = varargs
  TcUncheckedArray = UncheckedArray

# note: no aliases of built-in tuple, enum, and object type-class can be created

proc test(x: distinct, y: TcDistinct): int = 1
proc test1(x: Ordinal, y: TcOrdinal): int = 2
proc test(x: array, y: TcArray): int = 3
proc test(x: set, y: TcSet): int = 4
proc test(x: range, y: TcRange): int = 5
proc test(x: ptr, y: TcPtr): int = 6
proc test(x: ref, y: TcRef): int = 7
proc test(x: seq, y: TcSeq): int = 8
proc test(x: proc, y: TcProc): int = 9
proc test(x: openArray, y: TcOpenArray): int = 10
proc test1(x: varargs, y: TcVarargs): int = 11
proc test(x: ptr UncheckedArray, y: ptr TcUncheckedArray): int = 12

doAssert test(Distinct1(1), Distinct2(2)) == 1
doAssert test1(1, a) == 2
doAssert test([1], [""]) == 3
doAssert test({1}, {' '}) == 4
doAssert test(range[1..2](1), range[4..5](4)) == 5
doAssert test((ptr int)(nil), (ptr float)(nil)) == 6
doAssert test((ref int)(nil), (ref float)(nil)) == 7
doAssert test(@[1], @[""]) == 8
doAssert test((proc(): int)(nil), (proc(): float)(nil)) == 9
doAssert test(toOpenArray([1], 0, 0), toOpenArray([""], 0, 0)) == 10
doAssert test1(x=[1], y=[""]) == 11
doAssert test((ptr UncheckedArray[int])(nil),
              (ptr UncheckedArray[float])(nil)) == 12