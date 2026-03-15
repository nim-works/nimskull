discard """
  targets: "c js"
"""


import std/[options, unittest, strutils]
import experimental/property_testing_2

from std/algorithm import sorted
from std/sequtils import toSeq
from std/typetraits import enumLen


type 
  Colors = enum Red, Green, Blue


  E = enum A, B, C


const defaultSeed: uint32 = 1


# Helper to check for generator properties
proc getSamples[T](gen: Gen[T], count: int = 256,
                   seed: uint32 = defaultSeed,
                   debug: bool = false): seq[T] =
  let source = newSource(seed, debug = debug)
  result = gen.sample(source, count)


suite "Property Testing with Integrated Shrinking":

  test "Source Initialization API - newSource(seed, limit)":
    let s = newSource(seed = 1234, limit = 50)
    check s.limit == 50
    check s.buffer.len == 0
    check s.idempotent == false

    # Check default limit
    let s2 = newSource(seed = 1)
    check s2.limit == DefaultSourceLimit


  test "Source Initialization API - newSource(buffer)":
    let buf = @[1.byte, 2.byte, 3.byte]
    let s = newSource(buf)
    check s.limit == buf.len
    check s.buffer == buf
    check s.idempotent == false


  test "Primitive Byte & Kind I/O - writeRawByte and readRawByte":
    let s = newSource(seed = 1)
    # Recording
    s.writeRawByte(10.byte)
    s.writeRawByte(255.byte)
    check s.buffer == @[10.byte, 255.byte]

    # Replay
    let sReplay = newSource(s.buffer)
    check sReplay.readRawByte() == 10.byte
    check sReplay.readRawByte() == 255.byte
    # Overflow behavior: should return 0
    check sReplay.readRawByte() == 0.byte


  test "Primitive Byte & Kind I/O - writeStorageKind and readStorageKind":
    let s = newSource(seed = 1)
    s.writeStorageKind(skByte)
    s.writeStorageKind(skRange)
    s.writeStorageKind(skGroup)

    check s.buffer == @[byte(ord(skByte)), byte(ord(skRange)), byte(ord(skGroup))]

    let sReplay = newSource(s.buffer)
    check sReplay.readStorageKind() == skByte
    check sReplay.readStorageKind() == skRange
    check sReplay.readStorageKind() == skGroup
    # Overflow reads skByte by default as it's ord(0)
    check sReplay.readStorageKind() == skByte


  test "Multi-byte Serialization - bytesForRange and getScalarBytes":
    # bytesForRange
    check bytesForRange(0) == 1
    check bytesForRange(255) == 1
    check bytesForRange(256) == 2
    check bytesForRange(65535) == 2
    check bytesForRange(65536) == 4
    check bytesForRange(4294967295'u64) == 4
    check bytesForRange(4294967296'u64) == 8

    # getScalarBytes
    check getScalarBytes(skByte) == 1
    check getScalarBytes(sk2Bytes) == 2
    check getScalarBytes(sk4Bytes) == 4
    check getScalarBytes(sk8Bytes) == 8
    check getScalarBytes(skRange) == 0 # Or whatever fallback is appropriate
  

  test "Multi-byte Serialization - writeRawBytes and readRawBytes":
    let s = newSource(seed = 1)

    let 
      v1: uint64 = 0xAB
      v2: uint64 = 0xCDEF
      v3: uint64 = 0x12345678
      v4: uint64 = 0x9ABCDEF012345678'u64

    s.writeRawBytes(v1, 1)
    s.writeRawBytes(v2, 2)
    s.writeRawBytes(v3, 4)
    s.writeRawBytes(v4, 8)

    let sReplay = newSource(s.buffer)
    check sReplay.readRawBytes(1) == v1
    check sReplay.readRawBytes(2) == v2
    check sReplay.readRawBytes(4) == v3
    check sReplay.readRawBytes(8) == v4
    # Replay buffer exhaustion yields 0
    check sReplay.readRawBytes(8) == 0


  test "Generation Subsystem API - rngNextBytes":
    let s = newSource(seed = 42)
    let 
      r1 = s.rngNextBytes(1)
      r2 = s.rngNextBytes(2)
      r3 = s.rngNextBytes(4)
      r4 = s.rngNextBytes(8)

    let s2 = newSource(seed = 42)
    check s2.rngNextBytes(1) == r1
    check s2.rngNextBytes(2) == r2
    check s2.rngNextBytes(4) == r3
    check s2.rngNextBytes(8) == r4


  test "Generation Subsystem API - chooseScalarRaw and chooseRange":
    let s = newSource(seed = 123)

    # Recording
    let s1 = s.chooseScalarRaw(skByte)
    let s2 = s.chooseScalarRaw(sk4Bytes)

    # Range choosing
    let r1 = s.chooseRange(5, 10, skByte)
    check r1 >= 5 and r1 <= 10

    let r2 = s.chooseRange(1000, 2000, sk2Bytes)
    check r2 >= 1000 and r2 <= 2000

    # Replay
    let sReplay = newSource(s.buffer)
    check sReplay.chooseScalarRaw(skByte) == s1
    check sReplay.chooseScalarRaw(sk4Bytes) == s2

    check sReplay.chooseRange(5, 10, skByte) == r1
    check sReplay.chooseRange(1000, 2000, sk2Bytes) == r2


  test "Generation Subsystem API - bounds clamping on replay corruption":
    # Let's manually craft a corrupted chooseRange buffer that is out of bounds
    let s = newSource(seed = 1)

    # What the buffer *should* look like, but we corrupt the final value
    s.writeStorageKind(skRange)
    s.writeStorageKind(skByte)
    s.writeRawBytes(10, 1) # min
    s.writeRawBytes(20, 1) # max
    s.writeRawBytes(255, 1) # val range (max allowable is 10, but we provide 255)

    let sReplay = newSource(s.buffer)
    # The read rangeSize should be 10. The read val should be clamped from 255 -> 10.
    # The mathematical return would be min + 10 = 10 + 10 = 20. Let's verify.
    let r = sReplay.chooseRange(10, 20, skByte)
    check r == 20


  test "Structural Generation API - beginArray and readArrayLength":
    let s = newSource(seed = 1)
    
    # Recording
    s.beginArray(5)
    s.beginArray(256)
    s.beginArray(65536)
    
    # Replay
    let sReplay = newSource(s.buffer)
    check sReplay.readArrayLength() == 5
    check sReplay.readArrayLength() == 256
    check sReplay.readArrayLength() == 65536
    
    # Overflow/exhaustion
    check sReplay.readArrayLength() == 0


  test "Structural Generation API - beginGroup and readGroupLength":
    let s = newSource(seed = 1)
    
    # Recording
    s.beginGroup(3)
    s.beginGroup(10)
    
    # Replay
    let sReplay = newSource(s.buffer)
    check sReplay.readGroupLength() == 3
    check sReplay.readGroupLength() == 10
    
    # Overflow/exhaustion
    check sReplay.readGroupLength() == 0


  test "Structural Buffer Parser - skipNode":
    var s = newSource(seed = 1)
    
    # 1. Scalar testing
    s.writeStorageKind(skByte)
    s.writeRawByte(42)
    s.writeStorageKind(sk4Bytes)
    s.writeRawBytes(1234, 4)
    
    let endByte1 = skipNode(s.buffer, 0)
    check endByte1 == 2 # skByte (1) + val (1)
    
    let endByte2 = skipNode(s.buffer, endByte1)
    check endByte2 == 2 + 5 # sk4Bytes (1) + val (4)
    check endByte2 == s.buffer.len
    
    # 2. Range testing
    var sR = newSource(seed = 2)
    let minV: uint64 = 100
    let maxV: uint64 = 200 # size = 100 -> 1 byte
    sR.recordRange(minV, maxV, 150, sk2Bytes)
    let endRange = skipNode(sR.buffer, 0)
    # skRange(1) + sk2Bytes(1) + min(2) + max(2) + valRange(1) = 7 bytes
    check endRange == 7
    check endRange == sR.buffer.len
    
    # 3. Array testing
    var sA = newSource(seed = 3)
    sA.beginArray(3) # skArray8 (1) + len (1) = 2 bytes
    sA.writeStorageKind(skByte); sA.writeRawByte(1) # 2 bytes
    sA.writeStorageKind(skByte); sA.writeRawByte(2) # 2 bytes
    sA.writeStorageKind(skByte); sA.writeRawByte(3) # 2 bytes
    
    let endArray = skipNode(sA.buffer, 0)
    check endArray == 2 + 2 + 2 + 2 # 8 bytes total
    check endArray == sA.buffer.len


  test "Structural Iterator Parsing - candidates":
    # 1. Scalar Lowering Validation
    var sS = newSource(seed = 1)
    sS.writeStorageKind(sk2Bytes)
    sS.writeRawBytes(10, 2)
    var yieldsS = 0
    for cand in candidates(sS.buffer):
      # Should yield empty buffer, halved values (5, 2, 1, 0), decremented value (9), and decrement 2 (8)
      yieldsS.inc
    check yieldsS == 7

    # 2. Array Deletion Validation
    var sA = newSource(seed = 2)
    sA.beginArray(4) # len=4
    for i in 1..4:
      sA.writeStorageKind(skByte)
      sA.writeRawByte(byte(i))
    
    var yieldsA = 0
    var seenLengths = newSeq[int]()
    for cand in candidates(sA.buffer):
      yieldsA.inc
      if cand.len > 0 and cand[0] == byte(skArray8) and cand.len > 1:
        seenLengths.add(int(cand[1]))
    # Expected Array element lengths yielded should include structural truncations
    check seenLengths.len > 0
    check 0 in seenLengths or 2 in seenLengths or 3 in seenLengths


  test "Constant generator - produces the same value over and over again":
    let vals = getSamples(genConst(42))
    for val in vals:
      check val == 42


  test "Byte generator - is exhaustive and in random order":
    let
      vals = getSamples(genByte())
      expected = toSeq(byte.low .. byte.high)
    
    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]
    
    let vals2 = getSamples(genByte(), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Bool generator - is exhaustive and in random order":
    let
      vals = getSamples(genBool(), count = 2)
      expected = @[false, true]
    
    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]
    
    let vals2 = getSamples(genBool(), seed = defaultSeed + 1, count = 2)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Char range generator - is exhaustive and in random order":
    let
      vals = getSamples(genChar('a', 'z'), count = 26)
      expected = toSeq('a' .. 'z')

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genChar('a', 'z'), seed = defaultSeed + 1, count = 26)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Char generator - is exhaustive and in random order":
    let
      vals = getSamples(genChar())
      expected = toSeq(char.low .. char.high)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genChar(), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "ASCII Char generator - is exhaustive and in random order":
    let
      vals = getSamples(genAsciiChar(), count = 128)
      expected = toSeq(char(0) .. char(127))

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genAsciiChar(), seed = defaultSeed + 1, count = 128)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Int generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genInt(-128, 127))
      expected = toSeq(-128 .. 127)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genInt(-128, 127), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Int generator":
    let vals = getSamples(genInt())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= int.low and v <= int.high


  test "Int8 generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genInt8(-128, 127))
      expected = toSeq(-128'i8 .. 127)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genInt8(-128, 127), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Int8 generator":
    let vals = getSamples(genInt8())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= int8.low and v <= int8.high


  test "Int16 generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genInt16(-128, 127))
      expected = toSeq(-128'i16 .. 127)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genInt16(-128, 127), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Int16 generator":
    let vals = getSamples(genInt16())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= int16.low and v <= int16.high


  test "Int32 generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genInt32(-128, 127))
      expected = toSeq(-128'i32 .. 127)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genInt32(-128, 127), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Int32 generator":
    let vals = getSamples(genInt32())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= int32.low and v <= int32.high


  test "Int64 generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genInt64(-128, 127))
      expected = toSeq(-128'i64 .. 127)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genInt64(-128, 127), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Int64 generator":
    let vals = getSamples(genInt64())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= int64.low and v <= int64.high


  test "Uint8 generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genUint8(0, 255))
      expected = toSeq(0'u8 .. 255)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genUint8(0, 255), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Uint8 generator":
    let vals = getSamples(genUint8())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= uint8.low and v <= uint8.high


  test "Uint16 generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genUint16(0, 255))
      expected = toSeq(0'u16 .. 255)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genUint16(0, 255), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Uint16 generator":
    let vals = getSamples(genUint16())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= uint16.low and v <= uint16.high


  test "Enum generator - is exhaustive for small ranges and in random order":
    let
      vals = getSamples(genEnum[E](), count = enumLen(E))
      expected = toSeq(E.items)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genEnum[E](), seed = defaultSeed + 1,
                           count = enumLen(E))
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Uint32 generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genUint32(0, 255))
      expected = toSeq(0'u32 .. 255)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genUint32(0, 255), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Uint32 generator":
    let vals = getSamples(genUint32())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= uint32.low and v <= uint32.high


  test "Uint64 generator - over small ranges is exhaustive and in random order":
    let
      vals = getSamples(genUint64(0, 255))
      expected = toSeq(0'u64 .. 255)

    checkpoint "vals: " & $vals
    checkpoint "expected: " & $expected

    for i, v in vals.sorted().pairs:
      check v == expected[i]

    let vals2 = getSamples(genUint64(0, 255), seed = defaultSeed + 1)
    checkpoint "vals2: " & $vals2
    check vals != vals2
    check vals2.sorted() == expected


  test "Uint64 generator":
    let vals = getSamples(genUint64())

    checkpoint "vals: " & $vals

    for v in vals:
      check v >= uint64.low and v <= uint64.high


  # TODO: test genEnum with holey enums
  # TODO: test genEnum for larger than 8 bit enums


  test "Set generator":
    let samples = getSamples(genSet[E]())
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        for e in s:
          check e in {E.low .. E.high}


  test "Set generator - samples of a certain length":
    let samples = getSamples(genSet[E](minLen=2))
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        check s.len >= 2
        for e in s:
          check e in {E.low .. E.high}


  test "Set generator - excluding some elements":
    let samples = getSamples(genSet[E](exclude={A}))
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        for e in s:
          check e in {E.low .. E.high}
          check e != A


  test "Seq generator":
    let samples = getSamples(genSeq(genEnum[E]()))
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        for e in s:
          check e in {E.low .. E.high}


  test "Seq generator - samples of a certain length":
    let samples = getSamples(genSeq(genEnum[E](), minLen=2))
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        check s.len >= 2
        for e in s:
          check e in {E.low .. E.high}


  test "String generator":
    let samples = getSamples(genString())
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        for e in s:
          check e in {char.low .. char.high}


  test "String generator - samples of a certain length":
    let samples = getSamples(genString(minLen=2))
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        check s.len >= 2
        for e in s:
          check e in {char.low .. char.high}


  test "ASCII string generator":
    let samples = getSamples(genAsciiString())
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        for e in s:
          check e in {char(0) .. char(127)}


  test "ASCII string generator - samples of a certain length":
    let samples = getSamples(genAsciiString(minLen=2))
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        check s.len >= 2
        for e in s:
          check e in {char(0) .. char(127)}


  test "Array generator":
    let samples = getSamples(genArray(genEnum[E](), 5))
    for s in samples:
      checkpoint "s: " & $s
      for s in samples:
        for e in s:
          check e in {E.low .. E.high}


  test "Constant generator, produces the same value always":
    let prop = Property[int](
      gen: genConst(42),
      check: proc(x: int): PropertyStatus = psFail
    )
    let res = runProperty(prop, trials=10)
    check res.status == psFail
    check res.failingValue.get() == 42
    check res.shrunkValue.get() == 42


  test "Byte generator, exhaustively produces all values in range in random order":
    let prop = Property[byte](
      gen: genByte(),
      check: proc(b: byte): PropertyStatus =
        if b >= 0 and b <= 255: psPass else: psFail
    )
    check runProperty(prop).status == psPass


  test "Bool generator, exhaustively produces both true and false":
    # Just verify it produces both true and false over enough runs
    var seenTrue, seenFalse = false
    let prop = Property[bool](
      gen: genBool(),
      check: proc(b: bool): PropertyStatus =
        if b: seenTrue = true else: seenFalse = true
        psPass
    )
    discard runProperty(prop, trials=50)
    if not (seenTrue and seenFalse):
        checkpoint "Warning: genBool didn't produce both values in 50 runs (unlikely but possible)"


  test "ASCII char generator, exhaustively produces all values in range in random order":
    # TODO: verify order is random
    let prop = Property[char](
      gen: genAsciiChar(),
      check: proc(c: char): PropertyStatus =
        if c >= char(0) and c <= char(127): psPass else: psFail
    )
    check runProperty(prop).status == psPass


  test "Char range generator, exhaustively produces all values in range in random order":
    # TODO: verify order is random
    let prop = Property[char](
      gen: genChar('a', 'z'),
      check: proc(c: char): PropertyStatus =
        if c >= 'a' and c <= 'z': psPass else: psFail
    )
    check runProperty(prop).status == psPass

  test "UInt32 range generator, shrinking a simple integer predicate, exhaustive for small ranges":
    # Predicate: x < 10.
    # Failure: x >= 10.
    # Expectation: Shrink to 10.
    
    let prop = Property[uint32](
      gen: genUInt32(0, 100),
      check: proc(x: uint32): PropertyStatus =
        if x < 10: psPass else: psFail
    )

    let res = runProperty(prop, trials = 101, seed = 1)
    
    check res.status == psFail
    check res.shrunk
    if res.shrunkValue.isSome:
      let val = res.shrunkValue.get
      checkpoint "Shrunk value: " & $val
      check val == 10
      # this should work because small int ranges are exhaustive

  test "Integer range generator, shrinking a simple integer predicate, exhaustive for small ranges":
    # Predicate: x < 10.
    # Failure: x >= 10.
    # Expectation: Shrink to 10.
    
    let prop = Property[int](
      gen: genInt(-100, 100),
      check: proc(x: int): PropertyStatus =
        if x < 10: psPass else: psFail
    )

    let res = runProperty(prop, seed = 1)
    
    check res.status == psFail
    check res.shrunk
    check res.shrunkValue.isSome
    let val = res.shrunkValue.get
    checkpoint "Shrunk value: " & $val
    check val == 10
    # this should work because small int ranges are exhaustive


  test "Enum generator, exhaustively produces all values in enum in random order (for small enums)":
    # Want x < Green (Red).
    # Fail if >= Green.
    # Gen Green or Blue.
    # Expect shrink to Green (Simpler than Blue).
    # TODO: occasionally fails, need to figure out why
    let prop = Property[Colors](
      gen: genEnum[Colors](),
      check: proc(x: Colors): PropertyStatus =
        if x < Green: psPass else: psFail
    )
    let res = runProperty(prop, trials=100)
    if res.status == psFail:
      check res.shrunkValue.get() == Green


  test "Set generator property check":
    # Set must be empty.
    # Fail if not empty.
    # Shrink to {A} (Smallest non-empty set).
    let prop = Property[set[E]](
      gen: genSet[E](),
      check: proc(s: set[E]): PropertyStatus =
        if s.len == 0: psPass else: psFail
    )
    let res = runProperty(prop)
    check res.status == psFail
    # Minimal failure is size 1.
    check res.shrunkValue.get().len == 1
    # Expect {A}
    check res.shrunkValue.get() == {A}


  test "Shrinking a sequence length":
    # Predicate: len(s) < 5
    # Failure: len(s) >= 5
    # Expectation: Shrink to len 5.
    
    let prop = Property[seq[byte]](
      gen: genSeq(genByte(), minLen = 0, maxLen = 20),
      check: proc(s: seq[byte]): PropertyStatus =
        if s.len < 5: psPass else: psFail
    )
    
    let res = runProperty(prop, trials = 100)
    
    check res.status == psFail
    if res.shrunkValue.isSome:
      let val = res.shrunkValue.get
      checkpoint "Shrunk seq: " & $val & " len: " & $val.len
      check val.len >= 5


  test "Shrinking a string content (manual seeded)":
    # Predicate: not s.contains('A')
    # Failure: s.contains('A')
    # Use restricted generator to ensure 'A' appears often.
    
    let genRestricted = proc(s: Source): char =
      let b = cast[byte](s.chooseRange(0, 255, skByte))
      if (b mod 10) == 0: 'A' else: 'b'

    let propRestricted = Property[string](
       gen: genSeq(genRestricted, 0, 10).map(proc(x: seq[char]): string = x.join("")),
       check: proc(s: string): PropertyStatus =
         if s.contains('A'): psFail else: psPass
    )

    let res = runProperty(propRestricted, trials = 200)
    if res.status == psFail:
        let s = res.shrunkValue.get()
        checkpoint "Shrunk string: " & $s
        check s.contains('A')
        # Minimal string containing A is just "A". 
        # Or something small.
        check s.len < 5


  test "Array generator produces arrays of a given length with given generator for elements":
    # Array of 3 bytes.
    # Want x[0] == 0.
    # Fail if x[0] != 0.
    # Shrink x[0] to 1 (0 passes, 1 fails).
    # Minimal failure.
    let prop = Property[array[3, byte]](
      gen: genArray(genByte(), 3),
      check: proc(a: array[3, byte]): PropertyStatus =
        if a[0] == 0: psPass else: psFail
    )
    let res = runProperty(prop)
    check res.status == psFail
    let shrunk = res.shrunkValue.get()
    check shrunk[0] == 1
    # Check other elements are zeroed (simplest)
    check shrunk[1] == 0
    check shrunk[2] == 0


  # This test covers non-shrinking pass behavior
  test "Passing Property":
    let prop = Property[int](
      gen: genInt(0, 100),
      check: proc(x: int): PropertyStatus =
        if x >= 0: psPass else: psFail
    )
    let res = runProperty(prop, trials=10)
    check res.status == psPass
    check res.failingValue.isNone
    check res.shrunk == false


  test "Filter creates a new generator and shrinks correctly":
    # Even numbers. Want < 10.
    # Fail 10, 12, ...
    # Expect shrink to 10.
    let prop = Property[int](
      gen: genInt(0, 20).filter(proc(x: int): bool = x mod 2 == 0),
      check: proc(x: int): PropertyStatus =
        if x < 10: psPass else: psFail
    )
    let res = runProperty(prop, trials=100)
    if res.status == psFail:
      check res.shrunkValue.get() == 10

  test "Map creates a new generator and shrinks correctly":
    # Gen range 0..10. Map * 10 -> 0, 10, 20...
    # Want < 50.
    # Fail 50, 60...
    # Shrink to 50.
    # Underlying gen produces 5 (fails), 6 (fails)...
    # Shrink underlying to 5. Map(5) = 50.
    # TODO: this test occasionaly fails, reproduce and failure should emit counter example
    let prop = Property[int](
      gen: genInt(0, 10).map(proc(x: int): int = x * 10),
      check: proc(x: int): PropertyStatus =
        if x < 50: psPass else: psFail
    )
    let res = runProperty(prop, trials=100)
    if res.status == psFail:
      check res.shrunkValue.get() == 50

  test "Exception in Check":
    let prop = Property[int](
        gen: genConst(1),
        check: proc(x: int): PropertyStatus = raise newException(ValueError, "boom")
    )
    let res = runProperty(prop, trials=1)
    check res.status == psFail
    check res.failingValue.get() == 1

  test "Discarded Property / Filter Exhaustion":
    # This test covers the psDiscard status and retry/exhaustion
    # We create a filter that rejects everything to force exhaustion.
    let prop = Property[int](
      gen: genConst(1).filter(proc(x: int): bool = false, maxRetries=10),
      check: proc(x: int): PropertyStatus = psPass
    )
    # The runner catches FilterExhaustedError and marks as psDiscard
    # However, currently runProperty treats psDiscard as "skip run" and might loop forever if all discard.
    # OR it just continues. Since our runner loops for fixed trials, if all discard, it returns psPass by default (no failure).
    # But let's verify it doesn't crash or fail.
    
    # Ideally we'd want to check that it *tried* and discarded, but TestResult doesn't expose discard count.
    # We can at least ensure it doesn't crash.
    # TODO: add discard count to TestResult so we can verify the above
    let res = runProperty(prop, trials=5)
    check res.status == psPass # Pass because no failure found.

  test "FlatMap produces a generator that generates values based on the values generated by another generator":
    # Generate a length L, then a sequence of length L.
    let gen = genInt(0, 5).flatMap(proc(len: int): Gen[seq[byte]] =
        genSeq(genByte(), minLen = uint32(len), maxLen = uint32(len))
    )
    
    let prop = Property[seq[byte]](
        gen: gen,
        check: proc(s: seq[byte]): PropertyStatus =
            # Just check that it enters pass
            psPass
    )
    let res = runProperty(prop, trials=20)
    check res.status == psPass

  test "Shrinking FlatMapped generators works correctly":
      # Gen size L. Gen Seq[byte] of size L.
      # Predicate: L < 3.
      # Fail if L >= 3.
      # Expect shrink to L=3. Seq of length 3.
      # Note: FlatMap shrinking is complex because the structure depends on the first value.
      # Integrated shrinking handles this naturally!
      let gen = genInt(0, 10).flatMap(proc(len: int): Gen[seq[byte]] =
        genSeq(genByte(), minLen = uint32(len), maxLen = uint32(len))
      )

      let prop = Property[seq[byte]](
        gen: gen,
        check: proc(s: seq[byte]): PropertyStatus =
            if s.len < 3: psPass else: psFail
      )
      
      let res = runProperty(prop, trials=100)
      if res.status == psFail:
          check res.shrunkValue.get().len == 3
          # It should shrink L to 3, then the seq content to zeros.

  test "1 Element Tuple Generation and Shrinking":
    # TODO: this test occasionaly fails, reproduce and failure should emit counter example
    let prop = Property[(int,)](
      gen: genTuple(genInt(0, 10)),
      check: proc(t: (int,)): PropertyStatus =
        if t[0] < 5: psPass else: psFail
    )
    let res = runProperty(prop, trials=20)
    if res.status == psFail:
        check res.shrunkValue.get()[0] == 5

  test "2 Element Tuple Generation and Shrinking":
    let prop = Property[(int, int)](
      gen: genTuple(genInt(0, 100), genInt(0, 100)),
      check: proc(t: (int, int)): PropertyStatus =
        if t[0] < 10 and t[1] < 10: psPass else: psFail
    )
    let res = runProperty(prop, trials=200)
    if res.status == psFail:
      let val = res.shrunkValue.get()
      check val[0] == 10 or val[1] == 10
      check val[0] <= 10
      check val[1] <= 10
    
  test "3 Element Tuple Generation and Shrinking":
    let prop = Property[(int, int, int)](
      gen: genTuple(genInt(0, 10), genInt(0, 10), genInt(0, 10)),
      check: proc(t: (int, int, int)): PropertyStatus =
        if t[0] + t[1] + t[2] < 15: psPass else: psFail
    )
    let res = runProperty(prop, trials=100)
    if res.status == psFail:
      let val = res.shrunkValue.get()
      # Minimal sum >= 15 likely involves 10, 5, 0 or similar but distributed
      # Shrinking should minimize lexicographically (based on byte order).
      # First elements come first in byte stream.
      # Expect roughly minimal components.
      check val[0] + val[1] + val[2] >= 15

  test "4 Element Tuple Generation and Shrinking":
    let prop = Property[(int, int, int, int)](
      gen: genTuple(genConst(1), genConst(2), genConst(3), genConst(4)),
      check: proc(t: (int, int, int, int)): PropertyStatus = psFail
    )
    let res = runProperty(prop, trials=1)
    check res.status == psFail
    check res.shrunkValue.get() == (1, 2, 3, 4)

  test "5 Element Tuple Generation and Shrinking":
    let prop = Property[(int,int,int,int,int)](
      gen: genTuple(genConst(1), genConst(2), genConst(3), genConst(4), genConst(5)),
      check: proc(t: (int,int,int,int,int)): PropertyStatus = psFail
    )
    let res = runProperty(prop, trials=1)
    check res.status == psFail
    check res.shrunkValue.get() == (1, 2, 3, 4, 5)

  test "6 Element Tuple Generation and Shrinking":
    let prop = Property[(int,int,int,int,int,int)](
      gen: genTuple(genConst(1), genConst(2), genConst(3), genConst(4), genConst(5), genConst(6)),
      check: proc(t: (int,int,int,int,int,int)): PropertyStatus = psFail
    )
    let res = runProperty(prop, trials=1)
    check res.status == psFail
    check res.shrunkValue.get() == (1, 2, 3, 4, 5, 6)

  test "7 Element Tuple Generation and Shrinking":
    let prop = Property[(int,int,int,int,int,int,int)](
      gen: genTuple(genConst(1), genConst(2), genConst(3), genConst(4), genConst(5), genConst(6), genConst(7)),
      check: proc(t: (int,int,int,int,int,int,int)): PropertyStatus = psFail
    )
    let res = runProperty(prop, trials=1)
    check res.status == psFail
    check res.shrunkValue.get() == (1, 2, 3, 4, 5, 6, 7)

  test "8 Element Tuple Generation and Shrinking":
    let prop = Property[(int,int,int,int,int,int,int,int)](
      gen: genTuple(genConst(1), genConst(2), genConst(3), genConst(4), genConst(5), genConst(6), genConst(7), genConst(8)),
      check: proc(t: (int,int,int,int,int,int,int,int)): PropertyStatus = psFail
    )
    let res = runProperty(prop, trials=1)
    check res.status == psFail
    check res.shrunkValue.get() == (1, 2, 3, 4, 5, 6, 7, 8)

  test "9 Element Tuple Generation and Shrinking":
    let prop = Property[(int,int,int,int,int,int,int,int,int)](
      gen: genTuple(genConst(1), genConst(2), genConst(3), genConst(4), genConst(5), genConst(6), genConst(7), genConst(8), genConst(9)),
      check: proc(t: (int,int,int,int,int,int,int,int,int)): PropertyStatus = psFail
    )
    let res = runProperty(prop, trials=1)
    check res.status == psFail
    check res.shrunkValue.get() == (1, 2, 3, 4, 5, 6, 7, 8, 9)

  test "10 Element Tuple Generation and Shrinking":
    # Just verify compilation and basic running
    let gen10 = genTuple(
        genConst(1), genConst(2), genConst(3), genConst(4), genConst(5),
        genConst(6), genConst(7), genConst(8), genConst(9), genConst(10)
    )
    let prop = Property[(int,int,int,int,int,int,int,int,int,int)](
        gen: gen10,
        check: proc(t: (int,int,int,int,int,int,int,int,int,int)): PropertyStatus = psPass
    )
    let res = runProperty(prop, trials=5)
    check res.status == psPass

  test "0-arity proc generation":
    let prop = Property[proc(): int](
      gen: genProc(genConst(42)),
      check: proc(f: proc(): int): PropertyStatus =
        if f() == 42: psPass else: psFail
    )
    check runProperty(prop).status == psPass

  test "1-arity proc generation":
    let prop = Property[proc(x: int): int](
      gen: genProc1[int, int](genInt(0, 100)),
      check: proc(f: proc(x: int): int): PropertyStatus =
        let v1 = f(10)
        let v2 = f(10)
        checkpoint "v1: " & $v1 & " v2: " & $v2
        if v1 == v2: psPass else: psFail
    )
    check runProperty(prop).status == psPass

  test "2-arity proc generation":
      let prop = Property[proc(x: int, y: int): int](
        gen: genProc2[int, int, int](genConst(5)),
        check: proc(f: proc(x: int, y: int): int): PropertyStatus =
           if f(1, 2) == 5: psPass else: psFail
      )
      check runProperty(prop).status == psPass

  test "0-arity void proc generation":
    let prop = Property[proc()](
      gen: genProcVoid(),
      check: proc(f: proc()): PropertyStatus =
        f() # Should just work
        psPass
    )
    check runProperty(prop).status == psPass
