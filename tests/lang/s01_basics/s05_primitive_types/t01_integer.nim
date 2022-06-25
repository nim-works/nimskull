discard """
description: '''
Pre-defined integer types, the listing is under numeric literals, this covers
additional operations and conversion.
'''
"""

# Conversions types:
# - narrowing: result data type loses information (fewer bits)
# - widening:  result data type does not lose information (same or more bits)

# Automatic or Implied Conversions:
# - widening conversions can be implicit
# - narrowing conversions for `int` literals (compile time) are compile time
#   checked, and if they fit are done implicitly

block type_conversion_to_8:
  let
    a: int8 = 1
    min: int8 = -128
    max: int8 = 127
  doAssert typeOf(a) is int8,   "auto-narrow: literal `int`->`int8`"
  doAssert typeOf(min) is int8, "auto-narrow: literal `int`->`int8` - min"
  doAssert typeOf(max) is int8, "auto-narrow: literal `int`->`int8` - max"
  # xxx: cover the over under flows in separate tests

block type_conversion_to_unsigned_8:
  let
    a: uint8 = 1
    min: uint8 = 0
    max: uint8 = 255
  doAssert typeOf(a) is uint8,   "auto-narrow: literal `int`->`uint8`"
  doAssert typeOf(min) is uint8, "auto-narrow: literal `int`->`uint8` - min"
  doAssert typeOf(max) is uint8, "auto-narrow: literal `int`->`uint8` - max"
  # xxx: cover the over under flows in separate tests

block type_conversion_to_16:
  let
    a: int16 = 1
    min: int16 = -32_768
    max: int16 = 32_767
  doAssert typeOf(a) is int16,   "auto-narrow: literal `int`->`int16`"
  doAssert typeOf(min) is int16, "auto-narrow: literal `int`->`int16` - min"
  doAssert typeOf(max) is int16, "auto-narrow: literal `int`->`int16` - max"
  # xxx: cover the over under flows in separate tests

block type_conversion_to_unsigned_16:
  let
    a: uint16 = 1
    min: uint16 = 0
    max: uint16 = 65_535
  doAssert typeOf(a) is uint16,   "auto-narrow: literal `int`->`uint16`"
  doAssert typeOf(min) is uint16, "auto-narrow: literal `int`->`uint16` - min"
  doAssert typeOf(max) is uint16, "auto-narrow: literal `int`->`uint16` - max"
  # xxx: cover the over under flows in separate tests

block type_conversion_to_32:
  let
    a: int32 = 1
    min: int32 = -2_147_483_648
    max: int32 = 2_147_483_647
  doAssert typeOf(a) is int32,   "auto-narrow: literal `int`->`int32`"
  doAssert typeOf(min) is int32, "auto-narrow: literal `int`->`int32` - min"
  doAssert typeOf(max) is int32, "auto-narrow: literal `int`->`int32` - max"
  # xxx: cover the over under flows in separate tests

# at this point, it starts depending upon the platform whether it's a narrowing
# or widening of the type

block type_conversion_to_unsigned_32:
  let
    a: uint32 = 1
    min: uint32 = 0
    max: uint32 = 4_294_967_295'u32
  doAssert typeOf(a) is uint32,   "auto-conv: literal `int`->`uint32`"
  doAssert typeOf(min) is uint32, "auto-conv: literal `int`->`uint32` - min"
  doAssert typeOf(max) is uint32, "auto-conv: literal `int`->`uint32` - max"
  # xxx: cover the over under flows in separate tests

block type_conversion_to_64:
  let
    a: int64 = 1
    min: int64 = -9_223_372_036_854_775_808
    max: int64 = 9_223_372_036_854_775_807
  doAssert typeOf(a) is int64,   "auto-conv: literal `int`->`int64`"
  doAssert typeOf(min) is int64, "auto-conv: literal `int`->`int64` - min"
  doAssert typeOf(max) is int64, "auto-conv: literal `int`->`int64` - max"
  # xxx: cover the over under flows in separate tests

# automatic/implied widening and contrast with narrowing
# xxx: need to add more coverage

block type_conversion_to_unsigned_64:
  let
    a = 6'i8
    b: int = 10
    narrow = a + 10
    widened = a + b
  doAssert typeof(narrow) is int8, "integer operations with literals narrow"
  doAssert typeof(widened) is int, "runtime integer operations widened types"
  doAssert narrow == widened,      "the values are comparable and equal"

block negative_literals:
  doAssert -1 == -(1)

  doAssert -1'i8 == -(1'i8)
  doAssert -1'i8 == -(1'i8)
  doAssert -1'i16 == -(1'i16)
  doAssert -1'i16 == -(1'i16)
  doAssert -1'i32 == -(1'i32)
  doAssert -1'i32 == -(1'i32)
  doAssert -1'i64 == -(1'i64)
  doAssert -1'i64 == -(1'i64)


block literal_prefixes:
  ## Integer literals may be given in decimal (no prefix), binary (prefix `0b`), octal
  ## (prefix `0o`), and hexadecimal (prefix `0x`) notation.
  block binary_literals:
    doAssert 0b10 == 2
    doAssert 0b00 == 0

  block octal_literals:
    doAssert 0o1 == 1
    doAssert 0o10 == 8

  block hexadecimal_literals:
    doAssert 0x1 == 1
    doAssert 0xA == 10