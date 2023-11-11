discard """
  description: '''
    All built-in type classes are *named*, and they thus have bind-once
    behaviour. ``typedesc`` is an exception.
  '''
  action: compile
"""

# the used syntax is a deliberate choice, as it highlights
# that multiple uses of the type name still all use the same
# lifted generic parameter

type
  Object1 = object
  Object2 = object
  Distinct1 = distinct int
  Distinct2 = distinct int
  Enum1 = enum a
  Enum2 = enum b

proc test(x: object, y: object) = discard
proc test(x: distinct, y: distinct) = discard
proc test(x: enum, y: enum) = discard
proc test1(x: Ordinal, y: Ordinal) = discard
proc test(x: array, y: array) = discard
proc test(x: tuple, y: tuple) = discard
proc test(x: set, y: set) = discard
proc test(x: range, y: range) = discard
proc test(x: ptr, y: ptr) = discard
proc test(x: ref, y: ref) = discard
proc test(x: seq, y: seq) = discard
proc test(x: proc, y: proc) = discard
proc test(x: openArray, y: openArray) = discard
proc test1(x: varargs, y: varargs) = discard
proc test(x: ptr UncheckedArray, y: ptr UncheckedArray) = discard

template fail(x: untyped) =
  doAssert not compiles(x)

static:
  fail test(Object1(), Object2())
  fail test(Distinct1(1), Distinct2(2))
  fail test(a, b)
  fail test1(1, a)
  fail test([1], [""])
  fail test((1,), ("",))
  fail test({1}, {' '})
  fail test(range[1..2](1), range[4..5](4))
  fail test((ptr int)(nil), (ptr float)(nil))
  fail test((ref int)(nil), (ref float)(nil))
  fail test(@[1], @[""])
  fail test((proc(): int)(nil), (proc(): float)(nil))
  fail test(toOpenArray([1], 0, 0), toOpenArray([""], 0, 0))
  fail test1(x=[1], y=[""])
  fail test((ptr UncheckedArray[int])(nil), (ptr UncheckedArray[float])(nil))
