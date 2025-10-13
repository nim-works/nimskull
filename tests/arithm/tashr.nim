discard """
  labels: "arithmetic bitwise shift int"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/6255
      shr operator should keep the sign bit on signed types
    . The right shift operator is useful with both, keeping the sign bit, and
      not keeping the sign bit. But for not keeping the sign bit,
      that is what unsigend integers are for. Therefore I recommend that on
      signed integers the sign is kept.
    . The arithmetic right shift operator is very important for writing
      optimized routines for scientific computing.
    . Added system.ashr arithmetic right shift.
'''
"""

var x1 = -123'i8
var x2 = -123'i16
var x3 = -123'i32
var x4 = -123'i64
var x5 = -123

block codegen_test:
  doAssert ashr(x1, 1) == -62
  doAssert ashr(x2, 1) == -62
  doAssert ashr(x3, 1) == -62
  doAssert ashr(x4, 1) == -62
  doAssert ashr(x5, 1) == -62

block semfold_test:
  doAssert ashr(-123'i8 , 1) == -62
  doAssert ashr(-123'i16, 1) == -62
  doAssert ashr(-123'i32, 1) == -62
  doAssert ashr(-123'i64, 1) == -62
  doAssert ashr(-123    , 1) == -62
