discard """
  description: "Unary plus operator tests"
  targets: "c js vm"
"""

block static_plus:
  discard +1

block int_plus:
  var i = 1
  discard +i

block int8_plus:
  var i8: int8 = 1
  discard +i8

block int16_plus:
  var i16: int16 = 1
  discard +i16

block int32_plus:
  var i32: int32 = 1
  discard +i32

block int64_plus:
  var i64: int64 = 1
  discard +i64

block static_float_plus:
  discard +1.0

block float_plus:
  var f = 1.0
  discard +f

block float32_plus:
  var f32: float32 = 1.0
  discard +f32

block float64_plus:
  var f64: float64 = 1.0
  discard +f64
