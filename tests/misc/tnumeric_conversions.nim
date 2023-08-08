discard """
  targets: "c js vm"
  description: "Tests for conversion between the primitive numeric types"
  knownIssue.js: "full-range integers aren't supported yet"
"""

block negative_float64_to_uint:
  # has the same result as converting to a signed int of the same width first,
  # and then to the unsigned one
  var f = -1.5
  doAssert uint64(f) == uint64(int64(f))
  doAssert uint32(f) == uint32(int32(f))
  doAssert uint16(f) == uint16(int16(f))
  doAssert uint8(f) == uint8(int8(f))

block negative_float32_to_uint:
  # has the same result as converting to a signed int of the same width first,
  # and then to the unsigned one
  var f = -1.5'f32
  doAssert uint64(f) == uint64(int64(f))
  doAssert uint32(f) == uint32(int32(f))
  doAssert uint16(f) == uint16(int16(f))
  doAssert uint8(f) == uint8(int8(f))

block bool_to_float:
  var b = true
  doAssert float(b) == 1.0
  b = false
  doAssert float(b) == 0.0

block unsigned_to_float:
  var u = 0xFFFF'u16
  doAssert float32(u) == 65535

  # edge case: all 64 bits set
  var
    full = high(uint64)
    f32 = float32(full)
    f64 = float64(full)
  doAssert f32 == 1.8446744073709552e+19
  doAssert f64 == 1.8446744073709552e+19

block to_bool:
  # test the integer/float-to-bool conversion. Only zero maps to `false` --
  # everything non-zero maps to `true`
  var f = 0.0
  doAssert bool(f) == false
  f = 0.01
  doAssert bool(f) == true
  f = -1
  doAssert bool(f) == true

  var u = high(uint64)
  doAssert bool(u) == true

  # test that the bool really is stored as `1`, and is not kept as the
  # ``uint64`` value internally
  var b = bool(u)
  doAssert ord(b) == 1

  u = 0
  doAssert bool(u) == false

  var i = low(int) + 1
  doAssert bool(i) == true

block truncate:
  # unsigned integer conversion to a smaller width type truncate
  var a = 0x8070605040302010'u64 # a value that can't be represented with 32 bits
  var b = uint32(a)
  doAssert b == 0x40302010'u32
  var c = uint16(a)
  doAssert c == 0x2010'u16
  var d = uint8(a)
  doAssert d == 0x10'u8

block signed_to_unsigned:
  # signed-to-unsigned conversion to also truncate, but otherwise keep the bit
  # representation intact
  var x = 0xFFFFFFFF'i32
  doAssert uint32(x) == 0xFFFFFFFF'u32
  doAssert uint16(x) == 0xFFFF'u16

  x = 0x0000FFFF'i32
  doAssert uint32(x) == 0x0000FFFF'u32
  doAssert uint16(x) == 0xFFFF'u16

block signed_to_larger_unsigned:
  # bit pattern reinterpretation + truncation (in that order)
  # XXX: it's unclear whether this is the correct / wanted behaviour
  var x = 0xFFFF'i16 # -1
  doAssert uint16(x) == high(uint16)
  doAssert uint32(x) == high(uint32)
  doAssert uint64(x) == high(uint64)

  x = 0xFF00'i16
  doAssert uint32(x) == 0xFFFFFF00'u32