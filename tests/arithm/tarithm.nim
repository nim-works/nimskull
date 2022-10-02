discard """
labels: "arithmetic bitwise conversion int range system"
description: '''
  . From https://github.com/nim-lang/Nim/issues/5216
    Wrong result type when using bitwise and

  . From https://github.com/nim-lang/Nim/issues/5854
    Range type inference leads to counter-intuitive behvaiour
  . Nim modifies range types under the hood when you make arithmetic operations
    on them.
  . For example if you add 2 to the type range[0..127], you get range[2..129].
  . While being useful in some cases, in others this leads to problems that can
    not be seen, until the program encounters some values that cause range errors
    in runtime.

  . From https://github.com/nim-lang/Nim/issues/12177
    system.nim unsigned operators use different rules than signed operators
  . When using basic operators (+,-,*,div) on integers of different lengths
    (eg uint16 and uint32), implicit conversion only works when the longest type
    is first.
  . It is expected that the order of operands on these operators should not
    affect the operation.
'''
"""

import typetraits

block tcast:
  template crossCheck(ty: untyped, exp: untyped) =
    let rt = ty(exp)
    const ct = ty(exp)
    doAssert $rt == $ct, astToStr(exp) & "\nGot " & $ct & "\nExpected " & $rt

  template add1(x: uint8): untyped = x + 1
  template add1(x: uint16): untyped = x + 1
  template add1(x: uint32): untyped = x + 1

  template sub1(x: uint8): untyped = x - 1
  template sub1(x: uint16): untyped = x - 1
  template sub1(x: uint32): untyped = x - 1

  crossCheck(int8, 0'i16 - 5'i16)
  crossCheck(int16, 0'i32 - 5'i32)
  crossCheck(int32, 0'i64 - 5'i64)

  crossCheck(uint8, 0'u8 - 5'u8)
  crossCheck(uint16, 0'u16 - 5'u16)
  crossCheck(uint32, 0'u32 - 5'u32)
  crossCheck(uint64, 0'u64 - 5'u64)

  crossCheck(uint8, uint8.high + 5'u8)
  crossCheck(uint16, uint16.high + 5'u16)
  crossCheck(uint32, uint32.high + 5'u32)
  crossCheck(uint64, 0xFFFFFFFFFFFFFFFF'u64 + 5'u64)
  crossCheck(uint64, uint64.high + 5'u64)

  doAssert $sub1(0'u8) == "255"
  doAssert $sub1(0'u16) == "65535"
  doAssert $sub1(0'u32) == "4294967295"

  doAssert $add1(255'u8) == "0"
  doAssert $add1(65535'u16) == "0"
  doAssert $add1(4294967295'u32) == "0"

  crossCheck(int32, high(int32))
  crossCheck(int32, high(int32).int32)
  crossCheck(int32, low(int32))
  crossCheck(int32, low(int32).int32)
  crossCheck(int64, high(int8).int16.int32.int64)
  crossCheck(int64, low(int8).int16.int32.int64)

  doAssert not compiles(echo int64(0xFFFFFFFFFFFFFFFF'u64))
  doAssert not compiles(echo int32(0xFFFFFFFFFFFFFFFF'u64))
  doAssert not compiles(echo int16(0xFFFFFFFFFFFFFFFF'u64))
  doAssert not compiles(echo  int8(0xFFFFFFFFFFFFFFFF'u64))

block tnot:
  # Signed types
  block:
    const t0: int8  = not 4
    const t1: int16 = not 4
    const t2: int32 = not 4
    const t3: int64 = not 4
    const t4: int8  = not -5
    const t5: int16 = not -5
    const t6: int32 = not -5
    const t7: int64 = not -5
    doAssert t0 == -5
    doAssert t1 == -5
    doAssert t2 == -5
    doAssert t3 == -5
    doAssert t4 == 4
    doAssert t5 == 4
    doAssert t6 == 4
    doAssert t7 == 4

  # Unsigned types
  block:
    const t0: uint8  = not 4'u8
    const t1: uint16 = not 4'u16
    const t2: uint32 = not 4'u32
    const t3: uint64 = not 4'u64
    const t4: uint8  = not 251'u8
    const t5: uint16 = not 65531'u16
    const t6: uint32 = not 4294967291'u32
    const t7: uint64 = not 18446744073709551611'u64
    doAssert t0 == 251
    doAssert t1 == 65531
    doAssert t2 == 4294967291'u32
    doAssert t3 == 18446744073709551611'u64
    doAssert t4 == 4
    doAssert t5 == 4
    doAssert t6 == 4
    doAssert t7 == 4

block tshr:
  proc T() =
    # let VI = -8
    let VI64 = -8'i64
    let VI32 = -8'i32
    let VI16 = -8'i16
    let VI8 = -8'i8
    # doAssert( (VI shr 1) == 9_223_372_036_854_775_804, "Actual: " & $(VI shr 1))
    doAssert( (VI64 shr 1) == -4, "Actual: " & $(VI64 shr 1))
    doAssert( (VI32 shr 1) == -4, "Actual: " & $(VI32 shr 1))
    doAssert( (VI16 shr 1) == -4, "Actual: " & $(VI16 shr 1))
    doAssert( (VI8 shr 1) == -4, "Actual: " & $(VI8 shr 1))

  T()
  static:
    T()

block tUnsignedOps:
  proc testUnsignedOps(): bool =
    let
      a: int8 = -128
      b: int8 = 127

    doAssert b +% 1 == -128
    doAssert b -% -1 == -128
    doAssert b *% 2 == -2
    doAssert a /% 4 == 32
    doAssert a %% 7 == 2

    result = true

  doAssert testUnsignedOps()
  static:
    doAssert testUnsignedOps()

block issue_5216_bitwise_and:
  doAssert (name typeof((0x0A'i8 and 0x7F'i32) shl 7'i32)) == "int32"

  let i8 = 0x0A'i8
  doAssert (name typeof((i8 and 0x7F'i32) shl 7'i32)) == "int32"

  doAssert ((0x0A'i8 and 0x7F'i32) shl 7'i32) == 1280

  let ii8 = 0x0A'i8
  doAssert ((ii8 and 0x7F'i32) shl 7'i32) == 1280

block issue_5854_subrange:

  type
    n16 = range[0'i16..high(int16)]

  var level: n16 = 1
  let maxLevel: n16 = 1

  level = min(level + 2, maxLevel).n16
  doAssert level == 1

block issue_12177_conversion:
  var a: uint16 = 1
  var b: uint32 = 2

  doAssert b + a == 3
  doAssert b - a == 1
  doAssert b * a == 2
  doAssert b div a == 2

  doAssert a + b == 3
  doAssert a - b == 4294967295'u32
  doAssert a * b == 2
  doAssert a div b == 0
