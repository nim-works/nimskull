discard """
  output: "2"
"""


block:
  const a2 = $(int)
  const a3 = $int
  doAssert a2 == "int"
  doAssert a3 == "int"

  proc fun[T: typedesc](t: T) =
    const a2 = $(t)
    const a3 = $t
    doAssert a2 == "int"
    doAssert a3 == "int"
  fun(int)

# check high/low implementations
doAssert high(int) > low(int)
doAssert high(int8) > low(int8)
doAssert high(int16) > low(int16)
doAssert high(int32) > low(int32)
doAssert high(int64) > low(int64)
# doAssert high(uint) > low(uint) # reconsider depending on issue #6620
doAssert high(uint8) > low(uint8)
doAssert high(uint16) > low(uint16)
doAssert high(uint32) > low(uint32)
# doAssert high(uint64) > low(uint64) # reconsider depending on issue #6620
doAssert high(float) > low(float)
doAssert high(float32) > low(float32)
doAssert high(float64) > low(float64)

doAssertRaises(Exception):
  raise newException(Exception, "foo")

block:
  var didThrow = false
  try:
    doAssertRaises(IndexDefect): # should fail since it's wrong exception
      raise newException(FieldDefect, "foo")
  except AssertionDefect:
    # ok, throwing was correct behavior
    didThrow = true
  doAssert didThrow

template boundedOpenArray[T](x: seq[T], first, last: int): openarray[T] =
  toOpenarray(x, max(0, first), min(x.high, last))

# bug #9281

proc foo[T](x: openarray[T]) =
  echo x.len

let a = @[1, 2, 3]

# a.boundedOpenArray(1, 2).foo()  # Works
echo a.boundedOpenArray(1, 2).len # Internal compiler error

block: # `$`*[T: tuple|object](x: T)
  doAssert $(foo1:0, bar1:"a") == """(foo1: 0, bar1: "a")"""
  doAssert $(foo1:0, ) == """(foo1: 0)"""
  doAssert $(0, "a") == """(0, "a")"""
  doAssert $(0, ) == "(0,)"
  type Foo = object
    x:int
    x2:float
  doAssert $Foo(x:2) == "(x: 2, x2: 0.0)"
  doAssert $() == "()"

# this is a call indirection to prevent `toInt` to be resolved at compile time.
proc testToInt(arg: float64, a: int, b: BiggestInt) =
  doAssert toInt(arg) == a
  doAssert toBiggestInt(arg) == b

testToInt(0.45, 0, 0)    # should round towards 0
testToInt(-0.45, 0, 0)   # should round towards 0
testToInt(0.5, 1, 1)     # should round away from 0
testToInt(-0.5, -1, -1)  # should round away from 0
testToInt(13.37, 13, 13)    # should round towards 0
testToInt(-13.37, -13, -13) # should round towards 0
testToInt(7.8, 8, 8)     # should round away from 0
testToInt(-7.8, -8, -8)  # should round away from 0

# test min/max for correct NaN handling

proc testMinMax(a,b: float32) =
  doAssert max(float32(a),float32(b)) == 0'f32
  doAssert min(float32(a),float32(b)) == 0'f32
  doAssert max(float64(a),float64(b)) == 0'f64
  doAssert min(float64(a),float64(b)) == 0'f64

testMinMax(0.0, NaN)
testMinMax(NaN, 0.0)


block:
  type Foo = enum
    k1, k2
  var
    a = {k1}
    b = {k1,k2}
  doAssert a < b


block: # Ordinal
  doAssert int is Ordinal
  doAssert uint is Ordinal
  doAssert int64 is Ordinal
  doAssert uint64 is Ordinal
  doAssert char is Ordinal
  type Foo = enum k1, k2
  doAssert Foo is Ordinal
  doAssert Foo is SomeOrdinal
  doAssert enum is SomeOrdinal

  # these fail:
  # doAssert enum is Ordinal # fails
  # doAssert Ordinal is SomeOrdinal
  # doAssert SomeOrdinal is Ordinal

block:
  proc p() = discard

  doAssert not compiles(echo p.rawProc.repr)
  doAssert not compiles(echo p.rawEnv.repr)
  doAssert not compiles(echo p.finished)
