discard """
description: '''
Covers numeric literals, in particular defaults and how suffixes are handled
'''
targets: "c cpp js"
"""

# basic int literals
block:
  let
    a = 0
    b = 1
    c = -1
  doAssert typeof(a) is int, "by default integers are `int`, 0"
  doAssert typeof(b) is int, "by default integers are `int`, 1"
  doAssert typeof(c) is int, "by default integers are `int`, -1"

# underscores between literal numbers
block:
  let a = 1_1
  doAssert typeof(a) is int, "allow underscores between numbers"

# basic float literals
block:
  let
    a = 0.0
    b = 1.0
    c = -1.0
    d = 2e1
  doAssert typeof(a) is float, "ending in `.`number makes it a `float`, 0.0"
  doAssert typeof(b) is float, "ending in `.`number makes it a `float`, 1.0"
  doAssert typeof(c) is float, "ending in `.`number makes it a `float`, -1.0"
  doAssert typeof(d) is float, "ending in `e`exponent makes it a `float`"

# hex notation -- decimal is default
block:
  let
    a = 0x0
    b = 0x1
    c = -0x1
    d = 0X0
  doAssert a == 0,  "hexidecimal 0 is 0x0 int"
  doAssert b == 1,  "hexidecimal 1 is 0x1 int"
  doAssert c == -1, "hexidecimal -1 is -0x1 int"
  doAssert d == 0,  "hexidecimal can use a capital `X` instead of lowercase"

# octal notation -- decimal is default
block:
  let
    a = 0o0
    b = 0o1
    c = -0o1
  doAssert a == 0,  "octal 0 is 0o0 int"
  doAssert b == 1,  "octal 1 is 0o1 int"
  doAssert c == -1, "octal -1 is -0o1 int"
  # FYI: upper case 'O' (that's not a zero) isn't supported

# binary notation -- decimal is default
block:
  let
    a = 0b0
    b = 0b1
    c = -0b1
    d = 0B0
  doAssert a == 0,  "binary 0 is 0b0 int"
  doAssert b == 1,  "binary 1 is 0b1 int"
  doAssert c == -1, "binary -1 is -0b1 int"
  doAssert d == 0,  "binary can use a capital `B` instead of lowercase"

# suffix to specify literal type
block:
  let
    a = 0'i8
    b = 1'i8
    c = -1'i8
  doAssert typeof(a) is int8, "0 int 8"
  doAssert typeof(b) is int8, "1 int 8"
  doAssert typeof(c) is int8, "-1 int 8"

block:
  let
    a = 0'i16
    b = 1'i16
    c = -1'i16
  doAssert typeof(a) is int16, "0 int 16"
  doAssert typeof(b) is int16, "1 int 16"
  doAssert typeof(c) is int16, "-1 int 16"

block:
  let
    a = 0'i32
    b = 1'i32
    c = -1'i32
  doAssert typeof(a) is int32, "0 int 32"
  doAssert typeof(b) is int32, "1 int 32"
  doAssert typeof(c) is int32, "-1 int 32"

block:
  let
    a = 0'i64
    b = 1'i64
    c = -1'i64
  doAssert typeof(a) is int64, "0 int 64"
  doAssert typeof(b) is int64, "1 int 64"
  doAssert typeof(c) is int64, "-1 int 64"

block:
  let
    a = 0'u
    b = 1'u
  doAssert typeof(a) is uint, "0 unsigned int"
  doAssert typeof(b) is uint, "1 unsigned int"

block:
  let
    a = 0'u8
    b = 1'u8
  doAssert typeof(a) is uint8, "0 unsigned int 8"
  doAssert typeof(b) is uint8, "1 unsigned int 8"

block:
  let
    a = 0'u16
    b = 1'u16
  doAssert typeof(a) is uint16, "0 unsigned int 16"
  doAssert typeof(b) is uint16, "1 unsigned int 16"

block:
  let
    a = 0'u32
    b = 1'u32
  doAssert typeof(a) is uint32, "0 unsigned int 32"
  doAssert typeof(b) is uint32, "1 unsigned int 32"

block:
  let
    a = 0'u64
    b = 1'u64
  doAssert typeof(a) is uint64, "0 usigned int 64"
  doAssert typeof(b) is uint64, "1 usigned int 64"

block:
  let
    a = 0'f
    b = 1'f
    c = -1'f
  doAssert typeof(a) is float32, "0 float 32"
  doAssert typeof(b) is float32, "1 float 32"
  doAssert typeof(c) is float32, "-1 float 32"

block:
  let
    a = 0'd
    b = 1'd
    c = -1'd
  doAssert typeof(a) is float64, "0 float 64"
  doAssert typeof(b) is float64, "1 float 64"
  doAssert typeof(c) is float64, "-1 float 64"

block:
  let
    a = 0'f32
    b = 1'f32
    c = -1'f32
  doAssert typeof(a) is float32, "0 float 32"
  doAssert typeof(b) is float32, "1 float 32"
  doAssert typeof(c) is float32, "-1 float 32"

block:
  let
    a = 0'f64
    b = 1'f64
    c = -1'f64
  doAssert typeof(a) is float64, "0 float 64"
  doAssert typeof(b) is float64, "1 float 64"
  doAssert typeof(c) is float64, "-1 float 64"

# low and high on various platforms

block:
  let
    a = 0x80'i8
    b = 0x7F'i8
  doAssert a == -128, "int 8 min"
  doAssert b == 127,  "int 8 max"

block:
  let
    a = 0x80_00'i16
    b = 0x7F_FF'i16
  doAssert a == -32_768, "int 16 min"
  doAssert b == 32_767,  "int 16 max"

block:
  let
    a = 0x80_00_00_00'i32
    b = 0x7F_FF_FF_FF'i32
  doAssert a == -2_147_483_648, "int 32 min"
  doAssert b == 2_147_483_647,  "int 32 max"

block:
  let
    a = 0xFF'u8
    b = 0xFF_FF'u16
  doAssert a == 255, "unsigned int 8 max"
  doAssert b == 65_535, "unsigned int 16 max"

block:
  let a = 0xFFFF_FFFF'u32
  doAssert a == uint32 4_294_967_295, "unsigned int 32 max"

block:
  let a = 0x8000_0000_0000_0000'i64
  doAssert a == int64 -9_223_372_036_854_775_808, "int 64 min"

block:
  let a = 0x7FFF_FFFF_FFFF_FFFF'i64
  doAssert a == int64 9_223_372_036_854_775_807, "int 64 max"

block:
  let a = 0xFFFF_FFFF_FFFF_FFFF'u64
  doAssert a == 18_446_744_073_709_551_615'u64, "unsigned int 64 max"

# xxx: revisit floating point related checks