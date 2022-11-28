discard """
  targets: "c js"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/14127
      cast[int8] treated as cast[uint8] with nim js plus related issues with
      cast to signed in nim js
    . cast[int8] is treated as cast[uint8] with nim js and gives wrong results,
      likewise with int16. These are within the range of js's Number type so
      should work
  '''
"""
block:
  template int2uint(T) =
    var a = -1
    let b = cast[T](a)
    doAssert b < 0
    let c = b + 1
    doAssert c is T
    doAssert c == 0

  int2uint(int8)
  int2uint(int16)
  int2uint(int32)
  int2uint(int64)

block: # maybe related
  template uint2int(T) =
    var a = 3
    let b = cast[T](a)
    doAssert b > 0
    let c = b - 1
    doAssert c is T
    doAssert c == 2

  uint2int(uint8)
  uint2int(uint16)
  uint2int(uint32)
  uint2int(uint64)

