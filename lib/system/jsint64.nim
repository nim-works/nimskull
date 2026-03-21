## Implement the various operations (e.g., basic arithmetic, bit operations,
## conversion, etc.) for 64-bit integers for the JavaScript target.

type
  Int64 = object
    ## The internal representation of a sign-less 64-bit integer. How the bit
    ## pattern is interpreted depends on the operation.
    lo, hi: uint32

{.push stackTrace: off, checks: off.}

proc negInt64(a: Int64): Int64 {.compilerproc.}

# MARK: conversion operations

proc doubleToUInt64(val: float64): Int64 {.compilerproc, asmNoStackFrame.} =
  # needs to use the JavaScript logical right shift operator, hence the asm
  asm """
    if (`val` > 0.0) {
      if (`val` >= Math.pow(2, 64)) {
        return {lo: 0xFFFFFFFF, hi: 0xFFFFFFFF};
      } else {
        return {lo: `val`>>>0, hi: (`val` / Math.pow(2, 32)) >>> 0};
      }
    } else {
      return {lo: 0, hi: 0};
    }
  """

proc doubleToInt64(val: float64): Int64 {.compilerproc, asmNoStackFrame.} =
  # needs to use the JavaScript logical right shift operator, hence the asm
  asm """
    if (`val` > 0.0) {
      if (`val` >= Math.pow(2, 63)) {
        return {lo: 0xFFFFFFFF, hi: 0x7FFFFFFF};
      } else {
        return {lo: `val`>>>0, hi: (`val` / Math.pow(2, 32)) >>> 0};
      }
    } else if (`val` < 0.0) {
      if (`val` < -Math.pow(2, 63)) {
        return {lo: 0, hi: 0x8000_0000};
      } else {
        return `negInt64`({lo: (-`val`) >>> 0,
                           hi: (-`val` / Math.pow(2, 32)) >>> 0});
      }
    } else {
      return {lo: 0, hi: 0};
    }
  """

proc uint64ToDouble(a: Int64): float64 {.compilerproc.} =
  float64(a.lo) + float64(a.hi) * float64(1'i64 shl 32)

proc int64ToDouble(a: Int64): float64 {.compilerproc.} =
  if a.hi >= 0x8000_0000'u32:
    -uint64ToDouble(negInt64(a))
  else:
    uint64ToDouble(a)

proc zextInt64(val: uint32): Int64 {.compilerproc.} =
  Int64(lo: cast[uint32](val), hi: 0)

proc sextInt64(val: int32): Int64 {.compilerproc.} =
  Int64(lo: cast[uint32](val), hi: cast[uint32](val shr 31))

proc truncInt64(a: Int64): uint32 {.compilerproc.} =
  a.lo

# MARK: comparison operations

proc ltInt64(a, b: Int64): bool {.compilerproc.} =
  if cast[int32](a.hi) < cast[int32](b.hi):
    true
  else:
    a.hi == b.hi and a.lo < b.lo

proc eqInt64(a, b: Int64): bool {.compilerproc.} =
  a.hi == b.hi and a.lo == b.lo

proc leInt64(a, b: Int64): bool {.compilerproc.} =
  if cast[int32](a.hi) < cast[int32](b.hi):
    true
  else:
    a.hi == b.hi and a.lo <= b.lo

proc leUInt64(a, b: Int64): bool {.compilerproc.} =
  if a.hi < b.hi:
    true
  else:
    a.hi == b.hi and a.lo <= b.lo

proc ltUInt64(a, b: Int64): bool {.compilerproc.} =
  if a.hi < b.hi:
    true
  else:
    a.hi == b.hi and a.lo < b.lo

# MARK: bit operations

proc shlInt64(a: Int64, shift: int): Int64 {.compilerproc.} =
  if shift == 0:
    a
  elif shift < 32:
    Int64(lo: a.lo shl shift, hi: (a.hi shl shift) or (a.lo shr (32 - shift)))
  else:
    Int64(lo: 0'u32, hi: (a.lo shl (shift - 32)))

proc shrInt64(a: Int64, shift: int): Int64 {.compilerproc.} =
  if shift == 0:
    a
  elif shift < 32:
    Int64(lo: (a.lo shr shift) or (a.hi shl (32 - shift)), hi: a.hi shr shift)
  else:
    Int64(lo: (a.hi shr (shift - 32)), hi: 0)

proc ashrInt64(a: Int64, shift: int): Int64 {.compilerproc.} =
  proc ashr(x: uint32, by: int): uint32 =
    cast[uint32](cast[int32](x) shr by)

  if shift == 0:
    a
  elif shift < 32:
    Int64(lo: (a.lo shr shift) or (a.hi shl (32 - shift)),
          hi: ashr(a.hi, shift))
  else:
    Int64(lo: ashr(a.hi, shift - 32), hi: ashr(a.hi, 31))

proc bitAndInt64(a, b: Int64): Int64 {.compilerproc.} =
  Int64(lo: a.lo and b.lo, hi: a.hi and b.hi)

proc bitOrInt64(a, b: Int64): Int64 {.compilerproc.} =
  Int64(lo: a.lo or b.lo, hi: a.hi or b.hi)

proc bitXorInt64(a, b: Int64): Int64 {.compilerproc.} =
  Int64(lo: a.lo xor b.lo, hi: a.hi xor b.hi)

proc bitNotInt64(a: Int64): Int64 {.compilerproc.} =
  Int64(lo: not a.lo, hi: not a.hi)

# MARK: arithmetic operations

proc negInt64(a: Int64): Int64 {.compilerproc.} =
  # negation is the same as `0 - a`
  result.lo = not(a.lo) + 1
  # `a.lo == 0` is the only value for which the above addition can overflow
  result.hi = not(a.hi) + uint32(ord(a.lo == 0))

proc addInt64(a, b: Int64): Int64 {.compilerproc.} =
  result.lo = a.lo + b.lo
  result.hi = a.hi + b.hi + uint32(ord(result.lo < a.lo))

proc subInt64(a, b: Int64): Int64 {.compilerproc.} =
  # subtracting `b` is the same as adding the two's complement of `b`
  result.lo = a.lo + not(b.lo) + 1
  result.hi = a.hi + not(b.hi) + uint32(ord(result.lo <= not(b.lo)))

proc mulInt32(a, b: uint32): Int64 =
  ## Multiplies two 32-bit integers, returning a 64-bit result.
  let aLo = a and 0xFFFF
  let aHi = a shr 16
  let bLo = b and 0xFFFF
  let bHi = b shr 16
  let part1 = aLo * bLo
  let part2 = aHi * bLo
  let part3 = aLo * bHi
  let part4 = aHi * bHi

  let tmp = part2 + part3
  result.lo = part1 + (tmp shl 16)
  result.hi = part4 + (tmp shr 16) + (if tmp < part2: 1'u32 shl 16 else: 0) +
              uint32(ord(result.lo < part1))

proc mulInt64(a, b: Int64): Int64 {.compilerproc.} =
  # integer multiplication works the same regardless of sign
  # a * b = (aLo + aHi*shift) * (bLo + bHi*shift)
  #       = aLo*bLo + aHi*bLo*shift + aLo*bHi*shift + aHi*bHi*shift*shift
  let part1 = mulInt32(a.lo, b.lo)
  let part2 = mulInt32(a.hi, b.lo)
  let part3 = mulInt32(a.lo, b.hi)
  # a.hi * b.hi is always outside the valid range and can thus be ignored
  result.lo = part1.lo
  result.hi = part1.hi + part2.lo + part3.lo

proc udiv(dividend, divisor: Int64): Int64 =
  ## Treats both operands as unsigned and performs an integer division.
  if ltUInt64(dividend, divisor):
    return Int64()

  # convert both operands to double (lossy) and use fp division. The
  # result will be close (or equal) to the real result
  let approximate = uint64ToDouble(dividend) / uint64ToDouble(divisor)
  result = doubleToUInt64(approximate)

  # shrink the approximate quotient until q * divisor <= dividend * divisor
  var got = mulInt64(divisor, result)
  while ltUInt64(got, result) or ltUInt64(dividend, got):
    result = shrInt64(result, 1)
    got = mulInt64(divisor, result)

  # `(a + b) / c` is the same as `(a/c) + (a/b)` (including when the
  # intermediate results are truncated)
  if not eqInt64(dividend, got):
    # note: with the V8 engine, this check and branch (even though not
    # necessary for correctness) have shown to be beneficial for performance
    let tmp = udiv(subInt64(dividend, got), divisor)
    result = addInt64(result, tmp)

proc idiv(dividend, divisor: Int64): Int64 =
  ## Treats both operands as signed integers and performs a division.
  let
    ndividend = dividend.hi >= 0x8000_0000'u32
    ndivisor  = divisor.hi >= 0x8000_0000'u32
    a = if ndividend: negInt64(dividend) else: dividend
    b = if ndivisor:  negInt64(divisor)  else: divisor
  result = udiv(a, b)
  if ndivisor xor ndividend:
    # the quotient must be negative
    result = negInt64(result)

proc divInt64(a, b: Int64): Int64 {.compilerproc.} =
  idiv(a, b)

proc divUInt64(a, b: Int64): Int64 {.compilerproc.} =
  udiv(a, b)

proc modInt64(a, b: Int64): Int64 {.compilerproc.} =
  subInt64(a, mulInt64(idiv(a, b), b))

proc modUInt64(a, b: Int64): Int64 {.compilerproc.} =
  subInt64(a, mulInt64(udiv(a, b), b))

proc absInt64(a: Int64): Int64 {.compilerproc.} =
  if a.hi >= 0x8000_0000'u32:
    negInt64(a)
  else:
    a

proc nimMin64(a, b: Int64): Int64 {.compilerproc.} =
  if ltInt64(a, b): a else: b

proc nimMax64(a, b: Int64): Int64 {.compilerproc.} =
  if ltInt64(a, b): b else: a

# MARK: checked operations

proc checkedNegInt64(a: Int64): Int64 {.compilerproc.} =
  # if a == low(int64), the result of the negation will overflow
  if a.lo == 0 and a.hi == 0x8000_0000'u32:
    raiseOverflow()
  else:
    result = negInt64(a)

proc checkedAddInt64(a, b: Int64): Int64 {.compilerproc.} =
  const HiBit = 0x8000_0000'u32
  result = addInt64(a, b)
  # if adding two negative numbers results in a positive one, or adding two
  # positive numbers in a negative one, the operation overflowed
  if (result.hi xor a.hi) >= HiBit and (result.hi xor b.hi) >= HiBit:
    raiseOverflow()

proc checkedSubInt64(a, b: Int64): Int64 {.compilerproc.} =
  const HiBit = 0x8000_0000'u32
  result = subInt64(a, b)
  if (result.hi xor a.hi) >= HiBit and (result.hi xor not b.hi) >= HiBit:
    raiseOverflow()

proc checkedMulInt64(x, y: Int64): Int64 {.compilerproc.} =
  var a = x
  var b = y
  # perform an unsigned multiplication, by using the absolute of both operands
  if a.hi >= 0x8000_0000'u32:
    a = negInt64(a)
  if b.hi >= 0x8000_0000'u32:
    b = negInt64(b)

  if a.hi != 0 and b.hi != 0:
    raiseOverflow()

  let part1 = mulInt32(a.lo, b.lo)
  let part2 = mulInt32(a.hi, b.lo)
  let part3 = mulInt32(a.lo, b.hi)
  var tmp = addInt64(addInt64(part2, part3), Int64(lo: part1.hi))
  # tmp will be shifted left by 32, so the high part being != 0 means that
  # an overflow will occur
  if tmp.hi != 0:
    raiseOverflow()

  result = Int64(lo: part1.lo, hi: tmp.lo)
  # restore the sign
  if (x.hi xor y.hi) >= 0x8000_0000'u32:
    # the result needs to be negative
    result = negInt64(result)
  elif result.hi >= 0x8000_0000'u32:
    # both operands were positive, but the result is negative -> overflow
    raiseOverflow()

proc checkedDivInt64(a, b: Int64): Int64 {.compilerproc.} =
  # check for division by zero and overflow
  if (b.lo == 0 and b.hi == 0):
    raiseDivByZero()
  elif (b.lo == high(uint32) and b.hi == high(uint32) and
        a.hi == 0x8000_0000'u32 and a.lo == 0):
    raiseOverflow()
  else:
    divInt64(a, b)

proc checkedModInt64(a, b: Int64): Int64 {.compilerproc.} =
  if (b.lo == 0 and b.hi == 0):
    raiseDivByZero()
  else:
    modInt64(a, b)

# MARK: cast operations

proc castFloatToInt64(val: float32): Int64 {.compilerproc, asmNoStackFrame.} =
  asm """
    var buf = new ArrayBuffer(4);
    (new Float32Array(buf))[0] = `val`;
    var view = new Uint32Array(buf);
    `result`.lo = view[0];
    `result`.hi = 0;
  """

proc castDoubleToInt64(val: float64): Int64 {.compilerproc, asmNoStackFrame.} =
  asm """
    var buf = new ArrayBuffer(8);
    (new Float64Array(buf))[0] = `val`;
    var view = new Uint32Array(buf);
    `result`.lo = view[0];
    `result`.hi = view[1];
  """

proc castInt64ToDouble(val: Int64): float64 {.compilerproc, asmNoStackFrame.} =
  asm """
    var buf = new ArrayBuffer(8);
    var view = new Uint32Array(buf);
    view[0] = `val`.lo;
    view[1] = `val`.hi;
    `result` = (new Float64Array(buf))[0];
  """

proc castInt64ToFloat(val: Int64): float32 {.compilerproc, asmNoStackFrame.} =
  asm """
    var buf = new ArrayBuffer(4);
    var view = new Uint32Array(buf);
    view[0] = `val`.lo;
    `result` = (new Float32Array(buf))[0];
  """

{.pop.}
