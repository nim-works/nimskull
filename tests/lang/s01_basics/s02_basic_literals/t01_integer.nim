discard """
description: "Basic integer literals"
"""

# basic int literals
block:
  doAssert typeof(0) is int, "by default integers are `int`, 0"
  doAssert typeof(1) is int, "by default integers are `int`, 1"
  doAssert typeof(-1) is int, "by default integers are `int`, -1"

# underscores between literal numbers
block:
  doAssert 11 == 1_1,          "allow underscores between numbers"
  doAssert typeof(1_1) is int, "underscore numbers default to `int`"

# hex notation -- decimal is default
block:
  doAssert 0x0 == 0,   "hexidecimal 0 is 0x0 int"
  doAssert 0x1 == 1,   "hexidecimal 1 is 0x1 int"
  doAssert -0x1 == -1, "hexidecimal -1 is -0x1 int"
  doAssert 0X0 == 0,   "hexidecimal can use a capital `X` instead of lowercase"

# octal notation -- decimal is default
block:
  doAssert 0o0 == 0,   "octal 0 is 0o0 int"
  doAssert 0o1 == 1,   "octal 1 is 0o1 int"
  doAssert -0o1 == -1, "octal -1 is -0o1 int"
  # FYI: upper case 'O' (that's not a zero) isn't supported

# binary notation -- decimal is default
block:
  doAssert 0b0 == 0,   "binary 0 is 0b0 int"
  doAssert 0b1 == 1,   "binary 1 is 0b1 int"
  doAssert -0b1 == -1, "binary -1 is -0b1 int"
  doAssert 0B0 == 0,   "binary can use a capital `B` instead of lowercase"

# suffix to specify literal type
block:
  doAssert typeof(0'i8) is int8, "0 int 8"
  doAssert typeof(1'i8) is int8, "1 int 8"
  doAssert typeof(-1'i8) is int8, "-1 int 8"

block:
  doAssert typeof(0'i16) is int16, "0 int 16"
  doAssert typeof(1'i16) is int16, "1 int 16"
  doAssert typeof(-1'i16) is int16, "-1 int 16"

block:
  doAssert typeof(0'i32) is int32, "0 int 32"
  doAssert typeof(1'i32) is int32, "1 int 32"
  doAssert typeof(-1'i32) is int32, "-1 int 32"

block:
  doAssert typeof(0'i64) is int64, "0 int 64"
  doAssert typeof(1'i64) is int64, "1 int 64"
  doAssert typeof(-1'i64) is int64, "-1 int 64"

block:
  doAssert typeof(0'u) is uint, "0 unsigned int"
  doAssert typeof(1'u) is uint, "1 unsigned int"

block:
  doAssert typeof(0'u8) is uint8, "0 unsigned int 8"
  doAssert typeof(1'u8) is uint8, "1 unsigned int 8"

block:
  doAssert typeof(0'u16) is uint16, "0 unsigned int 16"
  doAssert typeof(1'u16) is uint16, "1 unsigned int 16"

block:
  doAssert typeof(0'u32) is uint32, "0 unsigned int 32"
  doAssert typeof(1'u32) is uint32, "1 unsigned int 32"

block:
  doAssert typeof(0'u64) is uint64, "0 usigned int 64"
  doAssert typeof(1'u64) is uint64, "1 usigned int 64"

# low and high on various platforms

# TODO: move vm/comptime checks into more advanced stages

block:
  const
    vmInt8Min = 0x80'i8
    vmInt8Max = 0x7F'i8
  let
    rtInt8Min = vmInt8Min
    rtInt8Max = vmInt8Max
  doAssert rtInt8Min == -128, "int 8 min"
  doAssert rtInt8Max == 127,  "int 8 max"
  const
    sMin = $vmInt8Min
    sMax = $vmInt8Max
  doAssert $rtInt8Min == sMin, "string compare int8 min VM vs runtime"
  doAssert $rtInt8Max == sMax, "string compare int8 max VM vs runtime"

block:
  const
    vmInt16Min = 0x80_00'i16
    vmInt16Max = 0x7F_FF'i16
  let
    rtInt16Min = vmInt16Min
    rtInt16Max = vmInt16Max
  doAssert rtInt16Min == -32_768, "int 16 min"
  doAssert rtInt16Max == 32_767,  "int 16 max"
  const
    sMin = $vmInt16Min
    sMax = $vmInt16Max
  doAssert $rtInt16Min == sMin, "string compare int16 min VM vs runtime"
  doAssert $rtInt16Max == sMax, "string compare int16 max VM vs runtime"

block:
  const
    vmInt32Min = 0x80_00_00_00'i32
    vmInt32Max = 0x7F_FF_FF_FF'i32
  let
    rtInt32Min = vmInt32Min
    rtInt32Max = vmInt32Max
  doAssert rtInt32Min == -2_147_483_648, "int 32 min"
  doAssert rtInt32Max == 2_147_483_647,  "int 32 max"
  const
    sMin = $vmInt32Min
    sMax = $vmInt32Max
  doAssert $rtInt32Min == sMin, "string compare int32 min VM vs runtime"
  doAssert $rtInt32Max == sMax, "string compare int32 max VM vs runtime"

block:
  const
    vmUint8Max  = 0xFF'u8
    vmUint16Max = 0xFF_FF'u16
  let
    rtUint8Max  = vmUint8Max
    rtUint16Max = vmUint16Max
  doAssert rtUint8Max == 255,     "unsigned int 8 max"
  doAssert rtUint16Max == 65_535, "unsigned int 16 max"
  const
    s8 = $vmUint8Max
    s16 = $vmUint16Max
  doAssert $rtUint8Max  == s8,  "string compare uint8 max VM vs runtime"
  doAssert $rtUint16Max == s16, "string compare uint16 max VM vs runtime"

block:
  const vmUint32Max = 0xFFFF_FFFF'u32
  let rtUint32Max = vmUint32Max
  doAssert rtUint32Max == uint32 4_294_967_295, "unsigned int 32 max"
  const sMax = $vmUint32Max
  doAssert $rtUint32Max == sMax, "string compare uint32 max VM vs runtime"

block:
  const vmInt64Min = 0x8000_0000_0000_0000'i64
  let rtInt64Min = vmInt64Min
  doAssert rtInt64Min == int64 -9_223_372_036_854_775_808, "int 64 min"
  const sMin = $vmInt64Min
  when defined(js):
    # JS codegen has a bug and Nimskull emits the raw literal into JS which
    # then gets approximated as a double, here JS and the VM disagree
    if $rtInt64Min == sMin:
      doAssert false, "JS coddegen fixed for int64, congrats, renable this test"
    else:
      discard "Javascript code gen is still broken"
  else:
    doAssert $rtInt64Min == sMin, "string compare int64 min VM vs runtime"

block:
  const vmInt64Max = 0x7FFF_FFFF_FFFF_FFFF'i64
  let rtInt64Max = vmInt64Max
  doAssert rtInt64Max == int64 9_223_372_036_854_775_807, "int 64 max"
  const sMax = $vmInt64Max
  when defined(js):
    # JS codegen has a bug and Nimskull emits the raw literal into JS which
    # then gets approximated as a double, here JS and the VM disagree
    if $rtInt64Max == sMax:
      doAssert false, "JS coddegen fixed for int64, congrats, renable this test"
    else:
      discard "Javascript code gen is still broken"
  else:
    doAssert $rtInt64Max == sMax, "string compare int64 max VM vs runtime"

block:
  const vmUint64Max = 0xFFFF_FFFF_FFFF_FFFF'u64
  let rtUint64Max = vmUint64Max
  doAssert rtUint64Max == 18_446_744_073_709_551_615'u64, "unsigned int 64 max"
  const sMax = $vmUint64Max
  when defined(js):
    # JS codegen has a bug and Nimskull emits the raw literal into JS which
    # then gets approximated as a double, here JS and the VM disagree
    if $rtUint64max == sMax:
      doAssert false, "JS coddegen fixed for uint64, congrats, renable this test"
    else:
      discard "Javascript code gen is still broken"
  else:
    doAssert $rtUint64Max == sMax, "string compare uint64 max VM vs runtime"