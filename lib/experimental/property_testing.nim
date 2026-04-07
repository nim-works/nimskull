## Property Testing library, which allows for the specification and testing
## of properties of code.
##
## Property-based testing is a methodology where you define general characteristics
## (properties) that your code should satisfy across a wide range of inputs, rather
## than asserting specific outputs for hardcoded inputs. This library automatically
## generates random inputs to test these properties, and if a failure occurs, it
## aggressively "shrinks" the input to find the minimal, simplest example that
## reproduces the bug.
##
## ### Motivating Example
##
## Imagine an e-commerce function that applies a coupon discount to a shopping cart
## total. A naive unit test might check `applyDiscount(100, 20) == 80`.
## A property test instead asserts universal truths about the function:
##
runnableExamples:
  # import experimental/property_testing # required in other files

  proc applyDiscount(total, discount: int): int =
    max(0, total - discount)

  let result = runProperty:
    forAll (total: genInt(0, 1000), discount: genInt(0, 100)):
      let discounted = applyDiscount(total, discount)
      # Property 1: The discounted total is never greater than the original total
      if discounted > total: return psFail
      # Property 2: The discounted total is never negative
      if discounted < 0: return psFail
      return psPass

  assert result.status == psPass
##
## ### Core Concepts
##
## The library revolves around a few key types and concepts:
## - **Properties (`forAll`)**: The idiomatic way to define a test is using the `forAll`
##   procedures, which pair generators with a predicate function that returns a `PropertyStatus`
##   (`psPass`, `psFail`, or `psDiscard`).
## - **Generators (`Gen[T]`)**: Procedures that consume a `Source` of randomness to
##   produce values of type `T`.
## - **Shrinking**: An automatic process that simplifies failing test cases.
##
## ### Writing Property Tests
##
## Tests are typically constructed using `forAll` and executed with `runProperty`.
## `runProperty` runs the scenario numerous times (default 256) with different seeds.
## If a failure (`psFail`) is encountered, the library automatically begins shrinking
## the generated inputs to find the most minimal reproducing case, which is then
## available in the `TestResult`.
##
## Tests can return `psDiscard` if the generated inputs do not meet certain
## preconditions, effectively skipping that run without failing the test. For example,
## validating that a division function works correctly when the denominator is not zero.
##
## ### Generators and Sources
##
## To generate data, you build or compose `Gen[T]` procedures. The standard library provides
## many built-in generators:
## - **Primitives**: `genInt`, `genBool`, `genByte`, `genChar`, `genString`.
## - **Collections**: `genSeq`, `genSet`, `genArray`.
## - **Ranges**: `genInt(min, max)`, `genEnum`.
##
## You can compose and modify generators using combinators:
## - `map`: Transforms the output of a generator (e.g., generating even numbers by mapping `x => x * 2`).
## - `filter`: Discards values that don't meet a predicate. (Use sparingly, as too many retries raise `FilterExhaustedError`).
## - `flatMap`: Chains generators dependently.
##
## Underlying all generation is the `Source` object. It provides the entropy for generators
## and records the sequence of choices made. This recording is what enables the library's
## powerful, integrated shrinking capabilities.
##
## ### Integrated Shrinking
##
## This library uses **integrated shrinking** (inspired by Hypothesis).
## Unlike traditional type-directed shrinking, this library shrinks the *underlying byte stream*
## (the `Source` buffer) that produced the values, rather than shrinking the typed values themselves.
##
## This approach has several massive advantages:
## - You do not need to write custom `shrink` functions for your custom types.
## - Filtering and `flatMap` work perfectly and maintain invariants during shrinking,
##   because the shrinking happens on the raw entropy before the combinators run.
## - It aggressively finds minimal examples using structural heuristics like sequence deletion,
##   binary search on numeric ranges, and unbounded scalar lowering.
##
## See the following posts on the hows and whys of integrated shrinking:
## - https://hypothesis.works/articles/integrated-shrinking/
## - https://hypothesis.works/articles/compositional-shrinking/


# MARK: Future Development TODOs:
# - increase default number of scenario runs to 1000
# - rename to `property_testing`
# - separate core and api modules
# - allow pluggable random number generators
# - integrate with unittest runner
# - improve performance for JS by easing up on the 64-bit math


import std/[
    macros,   # sigh
    options,
    random,
  ]

from std/hashes import hash
from std/sequtils import delete, mapIt, toSeq
from std/sugar import `=>`
from std/times import getTime, toUnix
from std/typetraits import enumLen
from std/sets import incl, contains, initHashSet
from std/strutils import repeat

# MARK: Core Types

type
  StorageKind* = enum
    skByte         ## 1 byte
    sk2Bytes       ## 2 bytes
    sk4Bytes       ## 4 bytes
    sk8Bytes       ## 8 bytes
    skNBytes       ## N bytes
    skRange        ## Range of values
    skArray        ## Array of bytes, with size stored as a range in the
                   ## immediately following bytes and elements thereafter
    skGroup        ## Group of values, with size stored in the next byte

  ScalarBytes* = range[skByte..sk8Bytes] # xxx: should this include skNBytes?

  Source* = ref object
    rng: Rand
    buffer*: seq[byte]
    pos: int
    recording: bool
    limit*: int       ## Max bytes to generate before stopping/erroring
    idempotent*: bool ## whether the generator consuming this source should
                      ## be able to produce the same value given the same
                      ## source state, i.e.: disabling exhausitiveness
    debug*: bool      ## used for debugging

  Gen*[T] = proc(s: Source): T

  # Helper for exhaustive generation state
  ExhaustiveState[T] = ref object
    vals: seq[T]
    indices: seq[int]
    pos: int

  PropertyStatus* = enum
    psPass,
    psFail,
    psDiscard # Discard is for preconditions not met

  Property*[T] = object
    gen*: Gen[T]
    check*: proc(x: T): PropertyStatus

  SourceLimitExceededError* = object of CatchableError
  FilterExhaustedError* = object of CatchableError


# MARK: Source Implementation

const DefaultSourceLimit* = 100_000 # Reasonable default limit


proc newSource*(seed: uint32, limit: int = DefaultSourceLimit,
                idempotent: bool = false, debug: bool = false): Source =
  new(result)
  result.rng = initRand(int64(seed))
  result.buffer = @[]
  result.pos = 0
  result.recording = true
  result.limit = limit
  result.idempotent = idempotent
  result.debug = debug


proc newSource*(buffer: seq[byte]): Source =
  ## Create a source for replaying/shrinking with a fixed buffer
  new(result)
  result.rng = initRand(0)
  result.buffer = buffer # Not used when not recording
  result.pos = 0
  result.recording = false
  result.limit = buffer.len
  result.idempotent = false


proc writeRawByte*(s: Source, b: byte) =
  if s.recording:
    if s.buffer.len >= s.limit:
      raise newException(SourceLimitExceededError, "Source limit exceeded")
    s.buffer.add(b)
    s.pos.inc


proc readRawByte*(s: Source): byte =
  if s.pos < s.buffer.len:
    result = s.buffer[s.pos]
  else:
    # Start generating zeroes if we run out of buffer.
    # This is crucial for shrinking where we might cut the buffer short.
    result = 0
  s.pos.inc


proc writeStorageKind*(s: Source, kind: StorageKind) =
  s.writeRawByte(byte(ord(kind)))


proc readStorageKind*(s: Source): StorageKind =
  cast[StorageKind](s.readRawByte())


func bytesForRange*(rangeSize: uint64): int =
  if rangeSize <= 0xFF'u64: 1
  elif rangeSize <= 0xFFFF'u64: 2
  elif rangeSize <= 0xFFFFFFFF'u64: 4
  else: 8


proc writeRawBytes*(s: Source, val: uint64, bytes: int) =
  for i in 0 ..< bytes:
    s.writeRawByte(byte((val shr (i * 8)) and 0xFF))


proc readRawBytes*(s: Source, bytes: int): uint64 =
  for i in 0 ..< bytes:
    result = result or (uint64(s.readRawByte()) shl (i * 8))


proc getScalarBytes*(kind: StorageKind): int =
  case kind
  of skByte:   1
  of sk2Bytes: 2
  of sk4Bytes: 4
  of sk8Bytes: 8
  else:        unreachable("getScalarBytes: invalid StorageKind got: " & $kind)


proc rngNextBytes*(s: Source, bytes: int): uint64 =
  var val: uint64 = s.rng.next()
  if bytes < 8:
    let mask = (1'u64 shl (bytes * 8)) - 1
    val = val and mask
  return val


proc rngNextUInt32(s: Source): uint32 =
  uint32(s.rng.next() and 0xFFFFFFFF'u64)


proc chooseScalarRaw*(s: Source, kind: StorageKind): uint64 =
  let bytes = getScalarBytes(kind)
  if s.recording:
    result = s.rngNextBytes(bytes)
    s.writeStorageKind(kind)
    s.writeRawBytes(result, bytes)
  else:
    let readK = s.readStorageKind()
    if readK == kind:
      result = s.readRawBytes(bytes)
    else:
      discard s.readRawBytes(bytes)
      result = 0


proc readRangeValue*(s: Source): uint64 =
  let k = s.readStorageKind()
  assert k == skRange or (not s.recording and k == skByte),
         "Expected skRange, got " & $k
  let
    tgtKind = s.readStorageKind()
    sBytes = getScalarBytes(tgtKind)
    rMin = s.readRawBytes(sBytes)
    rMax = s.readRawBytes(sBytes)
    rangeSize = rMax - rMin
    valRange = s.readRawBytes(bytesForRange(rangeSize))
  assert rMax >= rMin, "rMax must be >= rMin, got: " & $rMax & " >= " & $rMin
  result = rMin + valRange


proc recordRange*(s: Source, min, max, offset: uint64, scalarKind: StorageKind) =
  s.writeStorageKind(skRange)
  s.writeStorageKind(scalarKind)
  let sBytes = getScalarBytes(scalarKind)
  s.writeRawBytes(min, sBytes)
  s.writeRawBytes(max, sBytes)
  s.writeRawBytes(offset, bytesForRange(max - min))


proc chooseRange*(s: Source, min, max: uint64, scalarKind: StorageKind): uint64 =
  if s.recording:
    let
      rangeSize = max - min
      valRange =
        if rangeSize == 0: 0'u64
        elif rangeSize == 0xFFFFFFFFFFFFFFFF'u64: s.rngNextBytes(8)
        else: s.rngNextBytes(bytesForRange(rangeSize)) mod (rangeSize + 1)

    result = min + valRange
    s.recordRange(min, max, valRange, scalarKind)
  else:
    let readK = s.readStorageKind()
    if readK == skRange:
      let
        tgtKind = s.readStorageKind()
        sBytes = getScalarBytes(tgtKind)
        rMin = s.readRawBytes(sBytes)
        rMax = s.readRawBytes(sBytes)
        rangeSize = rMax - rMin
        rVal = s.readRawBytes(bytesForRange(rangeSize))
        safeVal = if rVal > rangeSize: rangeSize else: rVal
      result = rMin + safeVal
    else:
      result = min


template renumerateInt64ToUint64(x: int64): uint64 =
  ## Maps int64 to uint64 values, preserving the order of values, the reverse
  ## of `renumerateUint64ToInt64`. This treats `(1u64 shl 63)` as the midpoint
  ## value, meaning `0i64` becomes `(1u64 shl 63)`, `1i64` becomes
  ## `(1u64 shl 63) + 1`, etc.
  uint64(x) + (1u64 shl 63)


template renumerateUint64ToInt64(x: uint64): int64 =
  ## Maps uint64 to int64 values, preserving the order of values, the reverse
  ## of `renumerateInt64ToUint64`. This treats `(1u64 shl 63)` as the midpoint
  ## value, meaning `(1u64 shl 63)` becomes `0i64`, `(1u64 shl 63) + 1` becomes
  ## `1i64`, etc.
  cast[int64](x - (1u64 shl 63))


proc chooseRange*(s: Source, min, max: int64, scalarKind: StorageKind): int64 =
  let
    uMin = renumerateInt64ToUint64(min)
    uMax = renumerateInt64ToUint64(max)
  return renumerateUint64ToInt64(chooseRange(s, uMin, uMax, scalarKind))


proc readArrayLength*(s: Source): uint32 =
  ## Parses an array marker and returns its exact length
  let k = s.readStorageKind()
  assert k == skArray or (not s.recording and k == skByte),
         "Expected skArray, got " & $k
  return uint32(s.readRangeValue())


proc beginArray*(s: Source, min, max: uint32): uint32 =
  ## Marks the beginning of an array of length in the range [min, max] of
  ## homogeneous elements in the stream, returning the chosen length.
  assert min <= max
  if s.recording:
    let
      lenRange = max - min
      lenBytesNeeded = bytesForRange(lenRange)
      tgtKind =
        case bytesForRange(max)
        of 1: skByte
        of 2: sk2Bytes
        of 4: sk4Bytes
        else: unreachable("array length too big: " & $lenRange)
      bytesNeeded = 1 #[skArray]# + 1 #[skRange]# + 1 #[scalarKind]# +
                    (getScalarBytes(tgtKind) * 2) #[min, max]# +
                    lenBytesNeeded
      bufLenBefore = s.buffer.len
    s.writeStorageKind(skArray)
    result = cast[uint32](s.chooseRange(cast[uint64](min), cast[uint64](max), tgtKind))
    assert s.buffer.len - bufLenBefore == bytesNeeded,
           "need: " & $bytesNeeded & " got: " & $(s.buffer.len - bufLenBefore) &
           " min: " & $min & " max: " & $max & " val: " & $result &
           " tgtKind: " & $tgtKind
  else:
    result = readArrayLength(s)


proc beginFixedArray*(s: Source, len: uint32) =
  ## Marks the beginning of a fixed-size array of length `len` of homogeneous
  ## elements in the stream.
  discard beginArray(s, len, len)


proc readGroupLength*(s: Source): uint8 =
  ## Parses a group marker and returns the number of fields
  let k = s.readStorageKind()
  assert k == skGroup or (not s.recording and k == skByte),
         "Expected skGroup, got " & $k
  result = uint8(s.readRawBytes(1))


proc beginGroup*(s: Source, numElements: uint8): uint8 =
  ## Marks the beginning of a heterogeneous group (tuple, object) with `numElements`
  if s.recording:
    s.writeStorageKind(skGroup)
    s.writeRawBytes(numElements, 1)
    result = numElements
  else:
    result = readGroupLength(s)


proc nextFloat64*(s: Source): float64 =
  cast[float64](s.chooseScalarRaw(sk8Bytes))


# MARK: Combinators ---

proc map*[T, U](g: Gen[T], f: proc(x: T): U): Gen[U] =
  return proc(s: Source): U = f(g(s))


proc filter*[T](g: Gen[T], pred: proc(x: T): bool, maxRetries: int = 100): Gen[T] =
  return proc(s: Source): T =
    # This loop requires care to avoid infinite loops.
    # We should probably limit retries.

    # Try first attempt
    result = g(s)
    if pred(result): return result

    for _ in 0 ..< maxRetries:
      result = g(s)
      if pred(result): return result

    # If exhausted, we must inform the caller that generation failed.
    # Checks using this generator should likely discard the run.
    raise newException(FilterExhaustedError, "Filter retries exhausted")


proc flatMap*[T, U](g: Gen[T], f: proc(x: T): Gen[U]): Gen[U] =
  ## Takes the initial generator, and a factory function that creates a new generator
  ## based on the value generated by the initial generator.
  ##
  ## This is useful for creating a generator that generates values based on the
  ## values generated by another generator.
  return proc(s: Source): U =
    let t = g(s)
    f(t)(s)


proc sample*[T](g: Gen[T], source: Source, count: int): seq[T] =
  result = newSeq[T](count)
  for i in 0 ..< count:
    result[i] = g(source)


# MARK: Generators

proc swapAccess[T](s: var openArray[T], a, b: int): T =
  ## Swap the value at position `a` for position `b`, then return the new value
  ## at position `a`. Used for exhaustive arbitrary traversal.
  result = s[b]

  if a != b:      # only need to swap if they're different
    s[b] = s[a]
    s[a] = result


type
  ExhaustiveRangeState*[T] = ref object
    min*: T
    rangeSize*: uint64
    indices*: seq[int]
    pos*: int


proc sequenceFromRange*[T](min, max: T): seq[T] =
  result = newSeq[T]()
  var curr = min
  while true:
    result.add(curr)
    if curr == max: break
    inc curr


proc genExhaustive*[T](vals: seq[T]): Gen[T] =
  ## Creates a generator that exhaustively iterates through `vals` before switching
  ## to random generation. It shuffles `vals` as it goes to ensure randomness
  ## within the exhaustive phase too (Simulated via swap access logic or just iteration).
  ##
  ## It keeps a `pos` index. It swaps `vals[pos]` with `vals[random index >= pos]`.
  ## This effectively shuffles the "remaining" items and picks one.
  ## Once `pos` reaches end, it switches to pure random

  let
    indices = toSeq(0 ..< vals.len)
    state = ExhaustiveState[T](vals: vals, indices: indices, pos: 0)

  return proc(s: Source): T =
    let tgtK = if state.vals.len <= 256: skByte
               elif state.vals.len <= 65536: sk2Bytes
               else: sk4Bytes

    var chosenIdx: int

    if not s.recording:
      chosenIdx = int(s.chooseRange(0, cast[uint64](state.vals.len - 1), tgtK))
    else:
      let randValOrig = s.rngNextUInt32()
      if state.pos < state.indices.len and not s.idempotent:
        let
          remaining = state.indices.len - state.pos
          offset = int(randValOrig mod uint32(remaining))
        chosenIdx = state.indices.swapAccess(state.pos, state.pos + offset)
        state.pos.inc
      else:
        chosenIdx = int(randValOrig mod uint32(state.vals.len))

      # Record it formally as a range so shrinking works predictably!
      s.recordRange(0, cast[uint64](state.vals.len - 1), cast[uint64](chosenIdx), tgtK)

    result = state.vals[chosenIdx]


proc genExhaustiveRange*[T](min: T, rangeSize: uint64): Gen[T] =
  let
    len = int(rangeSize + 1)
    indices = toSeq(0 ..< len)
    state = ExhaustiveRangeState[T](min: min, rangeSize: rangeSize, indices: indices, pos: 0)

  return proc(s: Source): T =
    let tgtK = if len <= 256: skByte
               elif len <= 65536: sk2Bytes
               else: sk4Bytes

    var chosenIdx: int

    if not s.recording:
      chosenIdx = int(s.chooseRange(0, rangeSize, tgtK))
    else:
      let randValOrig = s.rngNextUInt32()
      if state.pos < state.indices.len and not s.idempotent:
        let
          remaining = state.indices.len - state.pos
          offset = int(randValOrig mod uint32(remaining))
        chosenIdx = state.indices.swapAccess(state.pos, state.pos + offset)
        state.pos.inc
      else:
        chosenIdx = int(randValOrig mod uint32(len))

      # Record it formally as a range so shrinking works predictably!
      s.recordRange(0, rangeSize, cast[uint64](chosenIdx), tgtK)

    # Reconstruct the value by addition
    when T is enum:
      result = cast[T](cast[uint64](ord(state.min)) + cast[uint64](chosenIdx))
    else:
      result = cast[T](cast[uint64](state.min) + cast[uint64](chosenIdx))


proc genConst*[T](v: T): Gen[T] =
  ## Create a generator that always returns `v`.
  return proc(s: Source): T = v


proc genByte*(): Gen[byte] =
  ## Create a byte generator.
  return genExhaustiveRange(byte.low, 255'u64)


proc genBool*(): Gen[bool] =
  ## Create a boolean generator.
  return genExhaustive(@[false, true])


proc genChar*(min, max: char): Gen[char] =
  ## create a char arbitrary for the range [min, max].
  let rangeSize = cast[uint64](ord(max)) - cast[uint64](ord(min))
  return genExhaustiveRange(min, rangeSize)


proc genChar*(): Gen[char] =
  ## Create a char arbitrary for the full character range,
  ## see: `genAsciiChar` for the ASCII range.
  genChar(char.low, char.high)


proc genAsciiChar*(): Gen[char] =
  ## Create a char arbitrary for the ASCII range.
  genChar(char(0), char(127))


proc genInt*(min, max: int): Gen[int] =
  ## Create an integer arbitrary for the range [min, max].
  assert max >= min
  let
    uMin = renumerateInt64ToUint64(min)
    uMax = renumerateInt64ToUint64(max)
    rangeSize = uMax - uMin
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int =
      cast[int](s.chooseRange(uMin, uMax, sk8Bytes))


proc genInt*(): Gen[int] =
  ## Generate an int for the full range of int.
  genInt(low(int), high(int))


proc genInt8*(min, max: int8): Gen[int8] =
  ## Create an int8 generator for the range [min, max].
  assert max >= min
  let
    uMin = renumerateInt64ToUint64(min)
    uMax = renumerateInt64ToUint64(max)
    rangeSize = uMax - uMin
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int8 =
      cast[int8](s.chooseRange(uMin, uMax, skByte))


proc genInt8*(): Gen[int8] = genInt8(low(int8), high(int8))


proc genInt16*(min, max: int16): Gen[int16] =
  assert max >= min
  let
    uMin = renumerateInt64ToUint64(min)
    uMax = renumerateInt64ToUint64(max)
    rangeSize = uMax - uMin
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int16 =
      cast[int16](s.chooseRange(uMin, uMax, sk2Bytes))


proc genInt16*(): Gen[int16] = genInt16(low(int16), high(int16))


proc genInt32*(min, max: int32): Gen[int32] =
  assert max >= min
  let
    uMin = renumerateInt64ToUint64(min)
    uMax = renumerateInt64ToUint64(max)
    rangeSize = uMax - uMin
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int32 =
      cast[int32](s.chooseRange(uMin, uMax, sk4Bytes))


proc genInt32*(): Gen[int32] = genInt32(low(int32), high(int32))


proc genInt64*(min, max: int64): Gen[int64] =
  assert max >= min
  let
    uMin = renumerateInt64ToUint64(min)
    uMax = renumerateInt64ToUint64(max)
    rangeSize = uMax - uMin
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int64 =
      cast[int64](s.chooseRange(uMin, uMax, sk8Bytes))


proc genInt64*(): Gen[int64] = genInt64(low(int64), high(int64))


proc genUint8*(min, max: uint8): Gen[uint8] =
  assert max >= min
  let rangeSize = cast[uint64](max) - cast[uint64](min)
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): uint8 =
      cast[uint8](s.chooseRange(cast[uint64](min), cast[uint64](max), skByte))


proc genUint8*(): Gen[uint8] = genUint8(low(uint8), high(uint8))


proc genUint16*(min, max: uint16): Gen[uint16] =
  assert max >= min
  let rangeSize = cast[uint64](max) - cast[uint64](min)
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): uint16 =
      cast[uint16](s.chooseRange(cast[uint64](min), cast[uint64](max), sk2Bytes))


proc genUint16*(): Gen[uint16] = genUint16(low(uint16), high(uint16))


proc genUint32*(min, max: uint32): Gen[uint32] =
  assert max >= min
  let rangeSize = cast[uint64](max) - cast[uint64](min)
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): uint32 =
      cast[uint32](s.chooseRange(cast[uint64](min), cast[uint64](max), sk4Bytes))


proc genUint32*(): Gen[uint32] = genUint32(low(uint32), high(uint32))


proc genUint64*(min, max: uint64): Gen[uint64] =
  assert max >= min
  let rangeSize = max - min
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): uint64 =
      cast[uint64](s.chooseRange(min, max, sk8Bytes))


proc genUint64*(): Gen[uint64] = genUint64(low(uint64), high(uint64))


proc genEnum*[T: enum](min, max: T): Gen[T] =
  assert max >= min
  let rangeSize = cast[uint64](ord(max)) - cast[uint64](ord(min))
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    let rangeK = if enumLen(T) <= 256: skByte
                 elif enumLen(T) <= 65536: sk2Bytes
                 else: sk4Bytes
    return proc(s: Source): T =
      cast[T](s.chooseRange(cast[uint64](min), cast[uint64](max), rangeK))


proc genEnum*[T: enum](): Gen[T] =
  assert enumLen(T) < int(uint16.high), "oversized enum"
  let vals = toSeq(T.items)
  if enumLen(T) <= 256:
    return genExhaustive(vals)
  else:
    let rangeK = if enumLen(T) <= 256: skByte
                 elif enumLen(T) <= 65536: sk2Bytes
                 else: sk4Bytes
    let
      minIdx = 0
      maxIdx = vals.len - 1
    return proc(s: Source): T =
      let idx = int(s.chooseRange(cast[uint64](minIdx), cast[uint64](maxIdx), rangeK))
      result = vals[idx]


proc genSet*[T: enum](minLen: uint16 = 0, exclude: set[T] = {}): Gen[set[T]] =
  ## create a set generator for the enum type `T` excluding the values in
  ## `exclude`.
  let maxLen = enumLen(T) - exclude.len
  assert minLen <= uint16(maxLen), "minLen (" & $minLen & ") must be <= maxLen (" & $maxLen & ")"

  # TODO: rework this so we generate enum values the same way we generate
  #       exhaustive enums, that way we don't use up too much recorded entropy
  #       when generating shrunken sets.

  let g =
    if exclude.len == 0: genEnum[T]()
    else: genEnum[T]().filter((e) => e notin exclude)

  return proc(s: Source): set[T] =
    let
      len = s.beginArray(minLen, cast[uint32](maxLen))
      upperLimit = maxLen * 15
    var i = 0
    while result.len < int(len) and i < upperLimit:
      result.incl g(s)
      inc i


proc genSeq*[T](g: Gen[T], minLen: uint32 = 0, maxLen: uint32 = 100): Gen[seq[T]] =
  ## create a sequence generator with element type `T` and length in the range
  ## [minLen, maxLen].
  assert maxLen >= minLen
  return proc(s: Source): seq[T] =
    let len = s.beginArray(minLen, maxLen)

    result = newSeq[T](int(len))
    for i in 0 ..< int(len):
      result[i] = g(s)


proc genString*(minLen: uint32 = 0, maxLen: uint32 = 100,
                charGen: Gen[char] = genChar()): Gen[string] =
  ## create a string generator for the range [minLen, maxLen] using the given
  ## char generator.
  assert maxLen >= minLen
  let g = genSeq(charGen, minLen, maxLen)
  return proc(s: Source): string =
    let sSeq = g(s)
    result = newString(sSeq.len)
    for i, c in sSeq:
      result[i] = c


proc genAsciiString*(minLen: uint32 = 0, maxLen: uint32 = 100): Gen[string] =
  ## create an ASCII string generator.
  genString(minLen, maxLen, genAsciiChar())


proc genArray*[T](g: Gen[T], size: static uint32): Gen[array[size, T]] =
  return proc(s: Source): array[size, T] =
    s.beginFixedArray(size)
    var arr: array[size, T]
    for i in 0 ..< size:
      arr[i] = g(s)
    return arr


# MARK: Shrinking Strategies ---

proc decodeUint64*(buffer: seq[byte], pos: int, bytes: int): uint64 =
  for i in 0 ..< bytes:
    assert pos + i < buffer.len, "decodeUint64 buffer ran out before end of scalar"
    result = result or (uint64(buffer[pos + i]) shl (i * 8))


template skipToRangeOffsetAndGetSizeAndMin(buffer: seq[byte], pos: var int): (uint64, uint64) =
  ## Returns the range size (max - min) and min value for a range stored in the
  ## buffer at the given position where the skRange byte has already been
  ## traversed. Advances `pos` past the min and max values.
  doAssert pos < buffer.len, "skipNode buffer ran out before kind for range"
  let tgtKind = cast[StorageKind](buffer[pos])
  inc pos
  let sBytes = getScalarBytes(tgtKind)
  doAssert pos + 2 * sBytes <= buffer.len, "skipNode buffer ran out before min and max for range"
  let rMin = decodeUint64(buffer, pos, sBytes)
  pos += sBytes
  let rMax = decodeUint64(buffer, pos, sBytes)
  pos += sBytes
  doAssert rMax >= rMin, "rMax must be >= rMin, got: " & $rMax & " >= " & $rMin
  let rangeSize = rMax - rMin
  (rangeSize, rMin)


proc skipNode*(buffer: seq[byte], startPos: int): int =
  ## Parses the structural `StorageKind` at `startPos` and returns the index
  ## immediately *after* the fully encoded node (including all nested children).
  doAssert startPos < buffer.len, "skipNode startPos out of bounds"

  let kind = cast[StorageKind](buffer[startPos])
  var pos = startPos + 1

  case kind
  of skByte: pos += 1
  of sk2Bytes: pos += 2
  of sk4Bytes: pos += 4
  of sk8Bytes: pos += 8
  of skNBytes:
    doAssert pos < buffer.len, "skipNode buffer ran out before n for nbytes for skNBytes"
    let n = int(buffer[pos])
    pos += 1 + n
  of skRange:
    let (rangeSize, _) = skipToRangeOffsetAndGetSizeAndMin(buffer, pos)
    pos += bytesForRange(rangeSize)
  of skArray:
    doAssert pos < buffer.len, "skipNode buffer ran out before n for array"
    assert buffer[pos] == byte(skRange),
           "skipNode: Expected skRange, got " & $cast[StorageKind](buffer[pos])
    inc pos # skip skRange
    let
      (rangeSize, rangeMin) = skipToRangeOffsetAndGetSizeAndMin(buffer, pos)
      rangeBytes = bytesForRange(rangeSize)
      offset = int(decodeUint64(buffer, pos, rangeBytes))
      n = int(rangeMin) + offset
    pos += rangeBytes
    doAssert pos + n <= buffer.len, "skipNode buffer ran out before children for array"
    for _ in 0 ..< n: pos = skipNode(buffer, pos)
  of skGroup:
    doAssert pos < buffer.len, "skipNode buffer ran out before n for group"
    let n = int(buffer[pos])
    pos += 1
    doAssert pos + n <= buffer.len, "skipNode buffer ran out before children for group"
    for _ in 0 ..< n: pos = skipNode(buffer, pos)

  doAssert pos <= buffer.len, "skipNode pos out of bounds"
  return pos


proc writeUint64(buffer: var seq[byte], pos: int, bytes: int, val: uint64) =
  for i in 0 ..< bytes:
    if pos + i < buffer.len:
      buffer[pos + i] = byte((val shr (i * 8)) and 0xFF)


proc collectNodes(nodes: var seq[(int, StorageKind)], buf: seq[byte], pos: int) =
  assert pos < buf.len, "collectNodes pos is out of bounds, pos: " & $pos &
                        " buf.len: " & $buf.len
  let kind = cast[StorageKind](buf[pos])
  nodes.add((pos, kind))

  var p = pos + 1
  case kind
  of skArray:
    assert buf[p] == byte(skRange),
           "skipNode: Expected skRange, got " & $cast[StorageKind](buf[pos])
    inc p
    let
      (rangeSize, rangeMin) = skipToRangeOffsetAndGetSizeAndMin(buf, p)
      rangeBytes = bytesForRange(rangeSize)
      offset = int(decodeUint64(buf, p, rangeBytes))
      n = int(rangeMin) + offset
    p += rangeBytes
    doAssert p + n <= buf.len, "skipNode buffer ran out before children for array"
    for _ in 0 ..< n:
      collectNodes(nodes, buf, p)
      p = skipNode(buf, p)
  of skGroup:
    let lenBytes = 1
    doAssert(lenBytes == 0 or (p + lenBytes - 1) < buf.len, "buffer is too short, buf.len: " & $buf.len & " p: " & $p & " lenBytes: " & $lenBytes)

    if lenBytes > 0 and (p + lenBytes - 1) < buf.len:
      let n = int(decodeUint64(buf, p, lenBytes))

      doAssert(lenBytes > 0 or n == 0, "lenBytes was 0 and n was > 0")

      p += lenBytes
      doAssert(p < buf.len or n == 0, "buffer is too short after reading length, buf.len: " & $buf.len & " p: " & $p & " lenBytes: " & $lenBytes & " n: " & $n)
      let currentlyCollectedNodeCount = nodes.len
      for _ in 0 ..< n:
        doAssert(lenBytes > 0, "len bytes was 0")
        collectNodes(nodes, buf, p)
        p = skipNode(buf, p)

      doAssert(nodes.len >= currentlyCollectedNodeCount + n,
                "nodes.len was not greater than or equal to currentlyCollectedNodeCount + n, nodes.len: " & $nodes.len &
                " currentlyCollectedNodeCount: " & $currentlyCollectedNodeCount &
                " n: " & $n)
  else:
    # make this explicit/exhaustive
    discard

iterator candidates*(buffer: seq[byte]): seq[byte] =
  if buffer.len > 0:
    yield @[]

  var nodes: seq[(int, StorageKind)] = @[]

  if buffer.len > 0:
    var p = 0
    while p < buffer.len:
      collectNodes(nodes, buffer, p)
      p = skipNode(buffer, p)

  # Strategy 1: Array Element Deletion
  for i, (pos, kind) in nodes.pairs:
    if kind == skArray:
      let
        tgtKind = cast[StorageKind](buffer[pos + 2]) # skip skArray and skRange
        sBytes = getScalarBytes(tgtKind)
        minElem = decodeUint64(buffer, pos + 3, sBytes)
        maxElem = decodeUint64(buffer, pos + 3 + sBytes, sBytes)
        rangeSize = maxElem - minElem
        offsetBytes = bytesForRange(rangeSize)
        offsetPos = pos + 3 + 2 * sBytes

      doAssert offsetPos + offsetBytes <= buffer.len
      let numElems = int(minElem + decodeUint64(buffer, offsetPos, offsetBytes))
      if numElems > 0:
        var
          elemStarts: seq[int] = @[]
          p = offsetPos + offsetBytes
        for _ in 0 ..< numElems:
          elemStarts.add(p)
          p = skipNode(buffer, p)
        elemStarts.add(p)

        # element deletion must not result in less than `minElem` elements
        var k = numElems
        while k > 0:
          if numElems - k >= int(minElem):
            var i = 0
            while i <= numElems - k:
              var copy = buffer
              let
                newElems = numElems - k
                newOffset = cast[uint64](newElems - int(minElem))
              writeUint64(copy, offsetPos, offsetBytes, newOffset)
              let
                delStart = elemStarts[i]
                delEnd   = elemStarts[i + k] - 1
              if delStart <= delEnd:
                copy.delete(delStart .. delEnd)
              yield copy
              i.inc
          k = k div 2

  template numberShrinker(val: uint64, p: int, vBytes: int, buffer: seq[byte]) =
    var tryVal = val
    while tryVal > 0:
      tryVal = tryVal div 2
      var copy = buffer
      writeUint64(copy, p, vBytes, tryVal)
      yield copy
    if val >= 3: # val < 3 is covered by prior while loop and val - 1
      var copy = buffer
      writeUint64(copy, p, vBytes, val - 1)
      yield copy
    if val >= 4: # val < 4 is covered by prior while loop and val - 1
      var copy = buffer
      writeUint64(copy, p, vBytes, val - 2)
      yield copy

  # TODO: implement skNBytes shrinking
  #       it's basically an arbitrary precision bigint

  # Strategy 2: Range Binary Search
  for (pos, kind) in nodes:
    if kind == skRange:
      doAssert pos + 1 < buffer.len
      let
        tgtKind = cast[StorageKind](buffer[pos + 1])
        sBytes = getScalarBytes(tgtKind)
      doAssert pos + 2 + 2 * sBytes <= buffer.len
      let
        rMin = decodeUint64(buffer, pos + 2, sBytes)
        rMax = decodeUint64(buffer, pos + 2 + sBytes, sBytes)
        rangeSize = rMax - rMin
        vBytes = bytesForRange(rangeSize)
      # skip min & max bytes
      let p = pos + 2 + 2 * sBytes
      doAssert p + vBytes <= buffer.len
      let offset = decodeUint64(buffer, p, vBytes)
      numberShrinker(offset, p, vBytes, buffer)

  # Strategy 3: Unbounded Scalar Lowering
  for (pos, kind) in nodes:
    if kind in {skByte, sk2Bytes, sk4Bytes, sk8Bytes}:
      let sBytes = getScalarBytes(kind)
      doAssert pos + 1 + sBytes <= buffer.len
      let val = decodeUint64(buffer, pos + 1, sBytes)
      numberShrinker(val, pos + 1, sBytes, buffer)


# MARK: Buffer Tree Tools

proc treeRepr*(buffer: seq[byte]): string =
  ## Returns a string representation of the buffer tree, stored in `buffer`,
  ## mostly used for debugging and exploration.
  var
    nodes: seq[(int, StorageKind)] = @[]
    p = 0
    indent = 0

  if buffer.len > 0:
    while p < buffer.len:
      collectNodes(nodes, buffer, p)
      p = skipNode(buffer, p)

  var
    counters: seq[int] = @[]

  result = "collectedNodes: " & $nodes & "\n"

  for (pos, kind) in nodes:
    while counters.len > 0 and counters[^1] == 0:
      discard counters.pop()
      indent.dec
      result &= "  ".repeat(indent) & "}\n"

    if counters.len > 0:
      counters[^1].dec

    p = pos + 1
    case kind:
      of skArray:
        inc p # skip skRange
        let
          (rangeSize, rangeMin) = skipToRangeOffsetAndGetSizeAndMin(buffer, p)
          lenBytes = bytesForRange(rangeSize)

        if p + lenBytes <= buffer.len:
          let
            nOffset = decodeUint64(buffer, p, lenBytes)
            n = int(nOffset + rangeMin)
          result &= "  ".repeat(indent) & $kind & " (size: " & $n & ") {"
          if n > 0:
            indent.inc
            counters.add(n)
          else:
            result &= "}"
      of skRange:
        let tgtKind = cast[StorageKind](buffer[p])
        inc p
        let sBytes = getScalarBytes(tgtKind)
        let minVal = decodeUint64(buffer, p, sBytes)
        inc p, sBytes
        let maxVal = decodeUint64(buffer, p, sBytes)
        inc p, sBytes
        let
          vBytes = bytesForRange(maxVal - minVal)
          offset = decodeUint64(buffer, p, vBytes)
        result &= "  ".repeat(indent) & $kind & " (kind: " & $tgtKind &
                  ", min: " & $minVal & ", max: " & $maxVal & ", offset: " &
                  $offset & ", value: " & $(minVal + offset) & ")"
      of skGroup:
        let n = int(decodeUint64(buffer, p, 1))
        result &= "  ".repeat(indent) & $kind & " (size: " & $n & ") {"
        if n > 0:
          indent.inc
          counters.add(n)
        else:
          result &= "}"
      of skByte, sk2Bytes, sk4Bytes, sk8Bytes:
        let val = decodeUint64(buffer, p, getScalarBytes(kind))
        result &= "  ".repeat(indent) & $kind & " (value: " & $val & ")"
      of skNBytes:
        let n = int(decodeUint64(buffer, p, 1))
        result &= "  ".repeat(indent) & $kind & " (size: " & $n & ")"
    result &= "\n"

  while counters.len > 0:
    if counters[^1] != 0:
      result &= "  ".repeat(indent - 1) & "# ERROR: counter was " & $counters[^1] & "\n"
    discard counters.pop()
    indent.dec
    result &= "  ".repeat(indent) & "}\n"

  return result


# MARK: Runner

type
  TestResult*[T] = object
    status*: PropertyStatus
    runCount*: int
    seed*: uint32
    failingValue*: Option[T]
    failingBuffer*: seq[byte]
    shrunk*: bool
    shrunkValue*: Option[T]
    shrunkBuffer*: seq[byte]
    debugBuffer*: seq[byte]


const defaultTrials* = 1024 ## number of trials to run per property


proc runProperty*[T](p: Property[T], trials: int = defaultTrials,
                     seed: uint32 = 0, debug: bool = false): TestResult[T] =
  ## Runs the property `p` for `trials` iterations, using `seed` as the base
  ## seed. Returns a `TestResult` containing the status of the test, the number
  ## of trials run, the seed used, and the failing value and buffer if a failure
  ## was found. If `seed` is 0, the current time is used as the seed base.
  ## If `debug` is true, the property will record the first failing buffer in
  ## `p.debugBuffer`.

  let mainSeed = if seed == 0: uint32(getTime().toUnix() and 0xFFFFFFFF)
                 else:         seed

  var rng = initRand(int64(mainSeed))

  result.status = psPass
  result.runCount = 0

  for i in 1..trials:
    result.runCount = i
    let runSeed = uint32(rng.next() and 0xFFFFFFFF'u64)
    result.seed = runSeed

    var
      s = newSource(runSeed)
      val: T
      status: PropertyStatus

    try:
      val = p.gen(s)
      status = p.check(val)
    except FilterExhaustedError:
      # Filter failed too many times, discard this run
      status = psDiscard
    except SourceLimitExceededError:
      # General generation error (limit exceeded perhaps)
      status = psDiscard # Or failure? Typically discard if valid input couldn't be formed
    except:
      # Exception during gen (other than known ones) or check counts as failure
      status = psFail
      # We could capture exception msg here

    if status == psDiscard:
      # It might be tempting to not count this trial, but we could be stuck in a
      # loop of discarding. A better approach would be to limit the number of
      # discards, but for now we'll just count it as a pass.
      continue

    if status == psFail:
      # Found failure!
      result.status = psFail
      result.failingValue = some(val)
      result.failingBuffer = s.buffer
      if debug:
        result.debugBuffer = s.buffer

      # Start shrinking
      var
        bestBuffer = s.buffer
        bestVal = val

      # Shrink loop
      var improved = true
      while improved:
        improved = false
        for cand in candidates(bestBuffer):
          if cand.len >= bestBuffer.len and cand == bestBuffer:
            # Skip if same
            continue

          # Try candidate
          var
            sCand = newSource(cand)
            cVal: T
            cStatus: PropertyStatus

          try:
            cVal = p.gen(sCand)
            cStatus = p.check(cVal)
          except SourceLimitExceededError:
            # TODO: this is for debugging, it shouldn't happen here
            unreachable("SourceLimitExceededError during shrinking shouldn't be possible")
          except CatchableError as e:
            # TODO: capture the exception and add it to the result
            raise
            cStatus = psFail

          if cStatus == psFail:
            # Our candidates iterator uses AST heuristics to generate strictly
            # smaller or simpler candidate buffers.
            # We accept the first one that fails:
            bestBuffer = cand
            bestVal = cVal
            improved = true
            result.shrunk = true
            break # Restart candidates iterator with new bestBuffer

      result.shrunkBuffer = bestBuffer
      result.shrunkValue = some(bestVal)
      return # Return immediately on failure+shrink

  # If loop finishes without returning, it's a pass


# MARK: Tuple Generators

proc genTuple*[T](g: Gen[T]): Gen[(T,)] =
  ## Generates a tuple of a single element.
  return proc(s: Source): (T,) =
    discard s.beginGroup(1)
    (g(s),)

proc genTuple*[T1, T2](g1: Gen[T1], g2: Gen[T2]): Gen[(T1, T2)] =
  ## Generates a tuple of two elements.
  return proc(s: Source): (T1, T2) =
    discard s.beginGroup(2)
    (g1(s), g2(s))

proc genTuple*[T1, T2, T3](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3]): Gen[(T1, T2, T3)] =
  ## Generates a tuple of three elements.
  return proc(s: Source): (T1, T2, T3) =
    discard s.beginGroup(3)
    (g1(s), g2(s), g3(s))

proc genTuple*[T1, T2, T3, T4](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4]): Gen[(T1, T2, T3, T4)] =
  ## Generates a tuple of four elements.
  return proc(s: Source): (T1, T2, T3, T4) =
    discard s.beginGroup(4)
    (g1(s), g2(s), g3(s), g4(s))

proc genTuple*[T1, T2, T3, T4, T5](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5]): Gen[(T1, T2, T3, T4, T5)] =
  ## Generates a tuple of five elements.
  return proc(s: Source): (T1, T2, T3, T4, T5) =
    discard s.beginGroup(5)
    (g1(s), g2(s), g3(s), g4(s), g5(s))

proc genTuple*[T1, T2, T3, T4, T5, T6](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6]): Gen[(T1, T2, T3, T4, T5, T6)] =
  ## Generates a tuple of six elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6) =
    discard s.beginGroup(6)
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s))

proc genTuple*[T1, T2, T3, T4, T5, T6, T7](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7]): Gen[(T1, T2, T3, T4, T5, T6, T7)] =
  ## Generates a tuple of seven elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6, T7) =
    discard s.beginGroup(7)
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s), g7(s))

proc genTuple*[T1, T2, T3, T4, T5, T6, T7, T8](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8]): Gen[(T1, T2, T3, T4, T5, T6, T7, T8)] =
  ## Generates a tuple of eight elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6, T7, T8) =
    discard s.beginGroup(8)
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s), g7(s), g8(s))

proc genTuple*[T1, T2, T3, T4, T5, T6, T7, T8, T9](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8], g9: Gen[T9]): Gen[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
  ## Generates a tuple of nine elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6, T7, T8, T9) =
    discard s.beginGroup(9)
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s), g7(s), g8(s), g9(s))

proc genTuple*[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8], g9: Gen[T9], g10: Gen[T10]): Gen[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
  ## Generates a tuple of ten elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) =
    discard s.beginGroup(10)
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s), g7(s), g8(s), g9(s), g10(s))


# MARK: Function Generators

# Function generation is tricky. A generated function needs to be deterministic
# based on its inputs and the source. Since we can't easily "embed" the source
# into the function pointer in a way that preserves purity or state correctly
# without closures, we use closures.
#
# Strategy for functions:
# The generated function, when called, uses its arguments to perturb
# a seed, and then generates a return value from that perturbed seed.
# This ensures that f(x) always returns the same y for the same x.
#
# Strategy for procedures:
# The generated procedure, when called, uses its arguments to perturb
# a seed, and then generates return values from a new source with the
# perturbed seed. Ensuring the values are not deterministic.
#
# TODO: redo the above, we need idempotent and non-idempotent routine
#       support

proc hashCombine(seed: var uint32, val: uint32) =
  # Simple hash combination from boost
  seed = seed xor (val + 0x9e3779b9'u32 + (seed shl 6) + (seed shr 2))


proc hashArg[T](x: T): uint32 =
  # We need a way to hash arbitrary arguments to seed the RNG.
  # For now, let's hope standard `hash` and cast to uint32 works.
  cast[uint32](hash(x))


proc genProc*[R](retGen: Gen[R]): Gen[proc(): R] =
  return proc(s: Source): proc(): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(): R =
      var callSeed = funcSeed
      # No args to hash
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc1*[T1, R](retGen: Gen[R]): Gen[proc(a: T1): R] =
  return proc(s: Source): proc(a: T1): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc2*[T1, T2, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2): R] =
  return proc(s: Source): proc(a: T1, b: T2): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      var src = newSource(callSeed, idempotent=true )
      return retGen(src)

proc genProc3*[T1, T2, T3, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc4*[T1, T2, T3, T4, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c))
      hashCombine(callSeed, hashArg(d))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc5*[T1, T2, T3, T4, T5, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c))
      hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc6*[T1, T2, T3, T4, T5, T6, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc7*[T1, T2, T3, T4, T5, T6, T7, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc8*[T1, T2, T3, T4, T5, T6, T7, T8, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g)); hashCombine(callSeed, hashArg(h))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc9*[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g)); hashCombine(callSeed, hashArg(h))
      hashCombine(callSeed, hashArg(i))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)

proc genProc10*[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g)); hashCombine(callSeed, hashArg(h))
      hashCombine(callSeed, hashArg(i)); hashCombine(callSeed, hashArg(j))
      var src = newSource(callSeed, idempotent=true)
      return retGen(src)


# Void return variants

proc genVoidProc*(): Gen[proc()] =
  return proc(s: Source): proc() =
    # Function with no return value and no args doesn't need to do anything
    # other than exist.
    return proc() = discard

# For void procs with args, they just consume args but return nothing.
# They don't need to be deterministic for return value since there is none.
# They are essentially sinks.

macro genVoidProcN*(T: varargs[typedesc]): untyped =
  var args = @[bindSym"void"]

  let names = toSeq('a'..'z').mapIt($it)
  if args.len > names.len:
    error("genVoidProcN: too many arguments, max: " & $names.len)

  for i, t in T.pairs:
    args.add(nnkIdentDefs.newTree(genSym(names[i]), t, newEmptyNode()))

  let prc = newProc(params = args, pragmas = nnkPragma.newTree(ident"closure"))

  result = quote do:
    genConst(`prc`)


# MARK: Property Helpers

macro forAll*(gens: untyped, check: untyped): untyped =
  ## `forAll` takes a tuple of generators and a check procedure body, and
  ## returns a Property[T] where T is a tuple type of all the generators.

  if gens.kind notin {nnkTupleConstr, nnkPar}:
    error("Generators must be provided as a tuple, e.g., (g1: genInt(), g2: genString())", gens)

  var
    genExprs = newSeq[NimNode]()
    typeInferences = newSeq[NimNode]()
    paramNames = newSeq[NimNode]()
    paramNamesSet = initHashSet[string]()

  for g in gens:
    expectKind(g, nnkExprColonExpr)
    let
      paramName = g[0]
      genExpr   = g[1]
      genExprCopy = copyNimTree(genExpr)
      typeInference = quote do:
        typeof((`genExprCopy`)(default(Source)))

    if paramNamesSet.contains(paramName.strVal):
      error("Duplicate parameter name: " & paramName.strVal, paramName)

    paramNamesSet.incl(paramName.strVal)
    paramNames.add(paramName)
    genExprs.add(copyNimTree(genExpr))
    typeInferences.add(typeInference)

  let propType = if typeInferences.len == 1:
                   copyNimTree(typeInferences[0])
                 else:
                   let ty = newNimNode(nnkTupleConstr)
                   for t in typeInferences: ty.add(copyNimTree(t))
                   ty

  var
    procArgs = @[ident("PropertyStatus")]
    procBody = newStmtList()

  if genExprs.len == 1:
    procArgs.add(newIdentDefs(paramNames[0], copyNimTree(propType)))
    procBody.add(check)
  else:
    let argsName = genSym("args")
    procArgs.add(newIdentDefs(argsName, copyNimTree(propType)))
    for i, name in paramNames:
      procBody.add(newTree(nnkLetSection,
        newIdentDefs(name, newEmptyNode(), newTree(nnkBracketExpr, argsName, newIntLitNode(i)))
      ))
    procBody.add(check)

  let checkProc = newProc(
    params = procArgs,
    pragmas = newTree(nnkPragma, ident("closure")),
    body = procBody
  )

  let genArg = if genExprs.len == 1:
                 genExprs[0]
               else:
                 let call = newCall(ident("genTuple"))
                 for e in genExprs: call.add(e)
                 call

  result = newTree(nnkObjConstr,
    newTree(nnkBracketExpr, ident("Property"), propType),
    newTree(nnkExprColonExpr, ident("gen"), genArg),
    newTree(nnkExprColonExpr, ident("check"), checkProc)
  )
