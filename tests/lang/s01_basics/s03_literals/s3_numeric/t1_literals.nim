discard """
description: '''
Covers numeric literals, in particular defaults and how suffixes are handled
'''
targets: "c cpp js"
"""

# basic int literals
doAssert typeof(0)  is int, "by default integers are `int`, 0"
doAssert typeof(1)  is int, "by default integers are `int`, 1"
doAssert typeof(-1) is int, "by default integers are `int`, -1"

# underscores between literal numbers
doAssert typeof(1_1) is int, "allow underscores between numbers"

# basic float literals
doAssert typeof(0.0)  is float, "ending in `.`number makes it a `float`, 0.0"
doAssert typeof(1.0)  is float, "ending in `.`number makes it a `float`, 1.0"
doAssert typeof(-1.0) is float, "ending in `.`number makes it a `float`, -1.0"
doAssert typeof(2e1)  is float, "ending in `e`exponent makes it a `float`"

# hex notation -- decimal is default
doAssert 0x0  == 0,  "hexidecimal 0 is 0x0 int"
doAssert 0x1  == 1,  "hexidecimal 1 is 0x1 int"
doAssert -0x1 == -1, "hexidecimal -1 is -0x1 int"
doAssert 0X0  == 0,  "hexidecimal can use a capital `X` instead of lowercase"

# octal notation -- decimal is default
doAssert 0o0  == 0,  "octal 0 is 0o0 int"
doAssert 0o1  == 1,  "octal 1 is 0o1 int"
doAssert -0o1 == -1, "octal -1 is -0o1 int"
# upper case 'O' (that's not a zero) isn't supported

# binary notation -- decimal is default
doAssert 0b0  == 0,  "binary 0 is 0b0 int"
doAssert 0b1  == 1,  "binary 1 is 0b1 int"
doAssert -0b1 == -1, "binary -1 is -0b1 int"
doAssert 0B0  == 0,  "binary can use a capital `B` instead of lowercase"

# suffix to specify literal type

doAssert typeof(0'i8)  is int8, "0 int 8"
doAssert typeof(1'i8)  is int8, "1 int 8"
doAssert typeof(-1'i8) is int8, "-1 int 8"

doAssert typeof(0'i16)  is int16, "0 int 16"
doAssert typeof(1'i16)  is int16, "1 int 16"
doAssert typeof(-1'i16) is int16, "-1 int 16"

doAssert typeof(0'i32)  is int32, "0 int 32"
doAssert typeof(1'i32)  is int32, "1 int 32"
doAssert typeof(-1'i32) is int32, "-1 int 32"

doAssert typeof(0'i64)  is int64, "0 int 64"
doAssert typeof(1'i64)  is int64, "1 int 64"
doAssert typeof(-1'i64) is int64, "-1 int 64"

doAssert typeof(0'u)  is uint, "0 unsigned int"
doAssert typeof(1'u)  is uint, "1 unsigned int"
# doAssert typeof(-1'u) is uint, "-1 unsigned int"

doAssert typeof(0'u8)  is uint8, "0 unsigned int 8"
doAssert typeof(1'u8)  is uint8, "1 unsigned int 8"
# doAssert typeof(-1'u8) is uint8, "-1 unsigned int 8"

doAssert typeof(0'u16)  is uint16, "0 unsigned int 16"
doAssert typeof(1'u16)  is uint16, "1 unsigned int 16"
# doAssert typeof(-1'u16) is uint16, "-1 unsigned int 16"

doAssert typeof(0'u32)  is uint32, "0 unsigned int 32"
doAssert typeof(1'u32)  is uint32, "1 unsigned int 32"
# doAssert typeof(-1'u32) is uint32, "-1 unsigned int 32"

doAssert typeof(0'u64)  is uint64, "0 usigned int 64"
doAssert typeof(1'u64)  is uint64, "1 usigned int 64"
# doAssert typeof(-1'u64) is uint64, "-1 usigned int 64"

doAssert typeof(0'f)  is float32, "0 float 32"
doAssert typeof(1'f)  is float32, "1 float 32"
doAssert typeof(-1'f) is float32, "-1 float 32"

doAssert typeof(0'd)  is float64, "0 float 64"
doAssert typeof(1'd)  is float64, "1 float 64"
doAssert typeof(-1'd) is float64, "-1 float 64"

doAssert typeof(0'f32)  is float32, "0 float 32"
doAssert typeof(1'f32)  is float32, "1 float 32"
doAssert typeof(-1'f32) is float32, "-1 float 32"

doAssert typeof(0'f64)  is float64, "0 float 64"
doAssert typeof(1'f64)  is float64, "1 float 64"
doAssert typeof(-1'f64) is float64, "-1 float 64"

# low and high on various platforms

doAssert 0x80'i8 == -128, "int 8 min"
doAssert 0x7F'i8 == 127,  "int 8 max"

doAssert 0x80_00'i16 == -32_768, "int 16 min"
doAssert 0x7F_FF'i16 == 32_767,  "int 16 max"

doAssert 0x80_00_00_00'i32 == -2_147_483_648, "int 32 min"
doAssert 0x7F_FF_FF_FF'i32 == 2_147_483_647,  "int 32 max"

doAssert 0xFF'u8 == 255, "unsigned int 8 max"
doAssert 0xFF_FF'u16 == 65_535, "unsigned int 16 max"

const uint32Max = uint32 4_294_967_295
doAssert 0xFFFF_FFFF'u32 == uint32Max, "unsigned int 32 max"

const int64Min = int64 -9_223_372_036_854_775_808
doAssert 0x8000_0000_0000_0000'i64 == int64Min, "int 64 min"

const int64Max = int64 9_223_372_036_854_775_807
doAssert 0x7FFF_FFFF_FFFF_FFFF'i64 == int64Max, "int 64 max"

const uint64Max = 18_446_744_073_709_551_615'u64
doAssert 0xFFFF_FFFF_FFFF_FFFF'u64 == uint64Max, "unsigned int 64 max"

# xxx: revisit floating point related checks