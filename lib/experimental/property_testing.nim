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
    math,
    options,
    random,
  ]

from std/hashes import hash
from std/sequtils import delete, mapIt, toSeq
from std/sugar import `=>`
from std/times import getTime, toUnix
from std/typetraits import enumLen, OrdinalEnum, HoleyEnum
from std/enumutils import items
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


proc lexLess(a, b: seq[byte]): bool =
  ## Lexicographical comparison from last byte to first (big-endian order).
  ## Matches complexity of little-endian integers (higher bits at the end).
  let L = min(a.len, b.len)
  for i in countdown(L - 1, 0):
    if a[i] < b[i]: return true
    if a[i] > b[i]: return false
  return a.len < b.len


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


proc recordRangeData(s: Source, rangeSize, offset: uint64, scalarKind: StorageKind) =
  ## Writes a naked range (no StorageKind tag) to the source.
  s.writeStorageKind(scalarKind)
  let sBytes = getScalarBytes(scalarKind)
  s.writeRawBytes(rangeSize, sBytes)
  s.writeRawBytes(offset, bytesForRange(rangeSize))


proc readRangeData(s: Source, currentRangeSize: uint64): uint64 =
  ## Reads a naked range (no StorageKind tag) from the source.
  let
    tgtKind = s.readStorageKind()
    sBytes = getScalarBytes(tgtKind)
    recordedRangeSize = s.readRawBytes(sBytes)
    rVal = s.readRawBytes(bytesForRange(recordedRangeSize))
  result = if rVal > currentRangeSize: currentRangeSize else: rVal


proc recordRange*(s: Source, rangeSize, offset: uint64, scalarKind: StorageKind) =
  s.writeStorageKind(skRange)
  s.recordRangeData(rangeSize, offset, scalarKind)


proc chooseRange*(s: Source, min, max: uint64, scalarKind: StorageKind): uint64 =
  let rangeSize = max - min
  if s.recording:
    let
      valRange =
        if rangeSize == 0: 0'u64
        elif rangeSize == 0xFFFFFFFFFFFFFFFF'u64: s.rngNextBytes(8)
        else: s.rngNextBytes(bytesForRange(rangeSize)) mod (rangeSize + 1)

    result = min + valRange
    s.recordRange(rangeSize, valRange, scalarKind)
  else:
    let readK = s.readStorageKind()
    if readK == skRange:
      result = min + s.readRangeData(rangeSize)
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


proc decodeUint64*(buffer: seq[byte], pos: int, bytes: int): uint64 =
  for i in 0 ..< bytes:
    assert pos + i < buffer.len, "decodeUint64 buffer ran out before end of scalar"
    result = result or (uint64(buffer[pos + i]) shl (i * 8))


template skipToRangeOffsetAndGetSize(buffer: seq[byte], pos: var int): uint64 =
  ## Returns the range size for a range stored in the
  ## buffer at the given position where the skRange byte has already been
  ## traversed. Advances `pos` past the rangeSize.
  doAssert pos < buffer.len, "skipNode buffer ran out before kind for range"
  let tgtKind = cast[StorageKind](buffer[pos])
  inc pos
  let sBytes = getScalarBytes(tgtKind)
  doAssert pos + sBytes <= buffer.len, "skipNode buffer ran out before rangeSize for range"
  let rangeSize = decodeUint64(buffer, pos, sBytes)
  pos += sBytes
  rangeSize


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
    let rangeSize = skipToRangeOffsetAndGetSize(buffer, pos)
    pos += bytesForRange(rangeSize)
  of skArray:
    let rangeSize = skipToRangeOffsetAndGetSize(buffer, pos)
    pos += bytesForRange(rangeSize) # skip the offset
    doAssert pos + 4 <= buffer.len, "skipNode buffer ran out before actualLen for array"
    let n = int(decodeUint64(buffer, pos, 4))
    pos += 4
    for _ in 0 ..< n: pos = skipNode(buffer, pos)
  of skGroup:
    doAssert pos < buffer.len, "skipNode buffer ran out before n for group"
    let n = int(buffer[pos])
    pos += 1
    doAssert pos + n <= buffer.len, "skipNode buffer ran out before children for group"
    for _ in 0 ..< n: pos = skipNode(buffer, pos)

  doAssert pos <= buffer.len, "skipNode pos out of bounds"
  return pos


proc skipNodes*(s: Source, count: int) =
  ## Advances the source position past `count` nodes in the stream.
  for _ in 0 ..< count:
    s.pos = skipNode(s.buffer, s.pos)


proc writeRawBytesAt(s: Source, pos: int, val: uint64, bytes: int) =
  ## Internal helper to patch the buffer at a specific position.
  if not s.recording: return
  for i in 0 ..< bytes:
    if pos + i < s.buffer.len:
      s.buffer[pos + i] = byte((val shr (i * 8)) and 0xFF)


proc beginArray*(s: Source, min, max: uint32): (uint32, uint32, int) =
  ## Marks the beginning of an array of length in the range [min, max] of
  ## homogeneous elements in the stream, returning the chosen length, the
  ## absolute length from the source, and the position of the absolute length
  ## scalar in the buffer (for patching).
  assert min <= max
  let
    rangeSize = uint64(max - min)
    tgtKind = if max <= 255: skByte elif max <= 65535: sk2Bytes else: sk4Bytes

  if s.recording:
    s.writeStorageKind(skArray)
    let valRange =
        if rangeSize == 0: 0'u64
        elif rangeSize == 0xFFFFFFFFFFFFFFFF'u64: s.rngNextBytes(8)
        else: s.rngNextBytes(bytesForRange(rangeSize)) mod (rangeSize + 1)

    s.recordRangeData(rangeSize, valRange, tgtKind)
    let chosenLen = uint32(uint64(min) + valRange)
    let actualLenPos = s.buffer.len
    s.writeRawBytes(uint64(chosenLen), 4)
    result = (chosenLen, chosenLen, actualLenPos)
  else:
    let k = s.readStorageKind()
    assert k == skArray or (not s.recording and k == skByte),
           "Expected skArray, got " & $k
    let
      offset = s.readRangeData(rangeSize)
      newLen = uint32(uint64(min) + offset)
      actualLen = uint32(s.readRawBytes(4))
    result = (newLen, actualLen, -1)


proc beginFixedArray*(s: Source, len: uint32): (uint32, uint32, int) =
  ## Marks the beginning of a fixed-size array of length `len` of homogeneous
  ## elements in the stream.
  beginArray(s, len, len)


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


proc float64ToOrdinal*(f: float64): int64 =
  ## Maps float64 to monotonic int64 ordinal. +0.0 maps to 0.
  let bits = cast[uint64](f)
  if (bits and 0x8000000000000000'u64) == 0:
    result = cast[int64](bits)
  else:
    result = -cast[int64](bits and 0x7FFFFFFFFFFFFFFF'u64)


proc ordinalToFloat64*(o: int64): float64 =
  if o >= 0: result = cast[float64](cast[uint64](o))
  else: result = cast[float64](cast[uint64](-o) or 0x8000000000000000'u64)


proc float32ToOrdinal*(f: float32): int32 =
  let bits = cast[uint32](f)
  if (bits and 0x80000000'u32) == 0:
    result = cast[int32](bits)
  else:
    result = -cast[int32](bits and 0x7FFFFFFF'u32)


proc ordinalToFloat32*(o: int32): float32 =
  if o >= 0: result = cast[float32](cast[uint32](o))
  else: result = cast[float32](cast[uint32](-o) or 0x80000000'u32)


proc ordinalToRank*(o: int64): uint64 =
  if o >= 0: result = uint64(o) shl 1
  else: result = (uint64(-o) shl 1) - 1


proc rankToOrdinal*(r: uint64, minOrd, maxOrd, simplestOrd: int64): int64 =
  ## Maps a non-negative rank back to an int64 ordinal within [minOrd, maxOrd],
  ## centering on simplestOrd.
  let
    uMin = cast[uint64](minOrd)
    uMax = cast[uint64](maxOrd)
    uSimp = cast[uint64](simplestOrd)
    numBelow = uSimp - uMin
    numAbove = uMax - uSimp
    common = min(numBelow, numAbove)

  var resU: uint64
  if r <= 2 * common:
    if (r and 1) != 0: resU = uSimp - ((r + 1) shr 1)
    else: resU = uSimp + (r shr 1)
  elif numBelow > numAbove:
    resU = uSimp - (r - common)
  else:
    resU = uSimp + (r - common)

  return cast[int64](resU)


# MARK: Combinators ---

proc map*[T, U](g: sink Gen[T], f: sink proc(x: T): U): Gen[U] =
  ## Create a new generator based on `g`, using `f` to map values of the base
  ## generator.
  return proc(s: Source): U = f(g(s))


proc filter*[T](g: sink Gen[T], pred: sink proc(x: T): bool, maxRetries: int = 100): Gen[T] =
  ## Create a new generator based on `g`, that filters output based on the
  ## predicate procedure (`proc`), with `maxRetries` per filter attempt,
  ## raising a `FilterExhaustedError` if exceeded.
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


proc flatMap*[T, U](g: sink Gen[T], f: sink proc(x: T): Gen[U]): Gen[U] =
  ## Takes the initial generator, and a factory function that creates a new generator
  ## based on the value generated by the initial generator.
  ##
  ## This is useful for creating a generator that generates values based on the
  ## values generated by another generator.
  return proc(s: Source): U =
    let t = g(s)
    f(t)(s)


proc sample*[T](g: Gen[T], source: Source, count: int): seq[T] =
  ## Sample a generator, `g`, with a given `source` for a `count` number of
  ## elements, returning a sequence of that length.
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


proc getSimplest[T: Ordinal](min, max: T): T =
  let zero = T(0)
  if min > zero: min
  elif max < zero: max
  else: zero


proc genExhaustive[T](num: uint64, gen: sink proc(val: uint64): T): Gen[T] =
  ## Creates a generator that delegates producing the actual value to `gen`,
  ## the latter which is invoked with values in the range [0, num).
  var
    indices = toSeq(0 ..< num)
    pos = 0

  return proc(s: Source): T =
    let kind =
      if   num <= high(uint8):  skByte
      elif num <= high(uint16): sk2Bytes
      elif num <= high(uint32): sk4Bytes
      else:                     sk8Bytes
    var index: uint64
    if not s.recording:
      index = s.chooseRange(0'u64, num - 1, kind)
    else:
      let rv = s.rngNextUInt32()
      if pos < indices.len and not s.idempotent:
        let
          remaining = indices.len - pos
          offset = int(rv mod uint32(remaining))
        index = uint64(indices.swapAccess(pos, pos + offset))
        inc pos
      else:
        index = uint64(rv mod uint32(num))
      s.recordRange(num - 1, index, kind)

    result = gen(index)


proc genExhaustiveRanked[T: Ordinal](min, max, simplest: T): Gen[T] =
  let
    minO = when T is SomeInteger: cast[int64](min) else: cast[int64](ord(min))
    maxO = when T is SomeInteger: cast[int64](max) else: cast[int64](ord(max))
    simplestO = when T is SomeInteger: cast[int64](simplest) else: cast[int64](ord(simplest))
    rangeSize = cast[uint64](maxO) - cast[uint64](minO)

  genExhaustive(rangeSize + 1,
                rank => (let o = rankToOrdinal(rank, minO, maxO, simplestO);
                         when T is SomeInteger: cast[T](o) else: cast[T](int(o))))


proc chooseRanked[T: Ordinal](s: Source, min, max, simplest: T, kind: StorageKind): T =
  let
    minO = when T is SomeInteger: cast[int64](min) else: cast[int64](ord(min))
    maxO = when T is SomeInteger: cast[int64](max) else: cast[int64](ord(max))
    simplestO = when T is SomeInteger: cast[int64](simplest) else: cast[int64](ord(simplest))
    rangeSize = cast[uint64](maxO) - cast[uint64](minO)

  if rangeSize == 0: return simplest

  let rank = s.chooseRange(0'u64, rangeSize, kind)
  let o = rankToOrdinal(rank, minO, maxO, simplestO)
  return when T is SomeInteger: cast[T](o) else: cast[T](int(o))


proc genFromList*[T](vals: sink seq[T], simplestIdx: int = 0): Gen[T] =
  ## Create a generator that draws from a fixed list of values, `vals`, with
  ## ranked shrinking targeting the `simplestIdx`.
  if vals.len == 0: return proc(s: Source): T = default(T)

  let len = vals.len
  if len <= 256:
    let g = genExhaustiveRanked(0, len - 1, simplestIdx)
    return proc(s: Source): T = vals[int(g(s))]

  let rangeK = if len <= 65536: sk2Bytes else: sk4Bytes
  return proc(s: Source): T =
    let idx = s.chooseRanked(0, len - 1, simplestIdx, rangeK)
    vals[int(idx)]


proc getEnumMembers[T: enum](min, max: T, exclude: set[T] = {}): seq[T] =
  ## Internal helper to collect valid enum members in a range with an exclusion set.
  for v in T.items:
    if v >= min and v <= max and v notin exclude:
      result.add(v)


proc genEnumImpl[T: enum](vals: sink seq[T]): Gen[T] =
  ## Internal helper to handle the common logic for enum generators.
  var
    minDist = uint64.high
    simplestIdx = 0
  for i, v in vals:
    let d = abs(cast[int64](ord(v)))
    if uint64(d) < minDist:
      minDist = uint64(d)
      simplestIdx = i

  genFromList(vals, simplestIdx)


proc genExhaustive*[T](vals: sink seq[T]): Gen[T] =
  ## Creates a generator producing values that are part of `vals`.
  genExhaustive(uint64(vals.len), (idx) => vals[int(idx)])


proc genScalar[T: Ordinal](min, max: T): Gen[T] =
  ## Creates a generator for a contiguous scalar value in the range [min, max].
  assert min <= max
  let
    minO = when T is SomeInteger: cast[int64](min) else: cast[int64](ord(min))
    maxO = when T is SomeInteger: cast[int64](max) else: cast[int64](ord(max))
    uMin = cast[uint64](minO)
    uMax = cast[uint64](maxO)
    rangeSize = uMax - uMin
    simplest = getSimplest(min, max)
    tgtKind = case sizeof(T)
              of 1: skByte
              of 2: sk2Bytes
              of 4: sk4Bytes
              else: sk8Bytes

  if rangeSize < 256:
    return genExhaustiveRanked(min, max, simplest)

  return proc(s: Source): T = s.chooseRanked(min, max, simplest, tgtKind)


proc genConst*[T](v: T): Gen[T] =
  ## Create a generator that always returns `v`.
  return proc(s: Source): T = v


proc genByte*(): Gen[byte] =
  ## Create a byte generator.
  genScalar(byte.low, byte.high)


proc genBool*(): Gen[bool] =
  ## Create a boolean generator.
  return genScalar(false, true)


proc genChar*(min, max: char): Gen[char] =
  ## create a char arbitrary for the range [min, max].
  genScalar(min, max)


proc genChar*(): Gen[char] =
  ## Create a char arbitrary for the full character range,
  ## see: `genAsciiChar` for the ASCII range.
  genChar(char.low, char.high)


proc genAsciiChar*(): Gen[char] =
  ## Create a char arbitrary for the ASCII range.
  genChar(char(0), char(127))


proc genInt*(min, max: int): Gen[int] =
  ## Create an integer arbitrary for the range [min, max].
  genScalar(min, max)


proc genInt*(): Gen[int] =
  ## Generate an int for the full range of int.
  genInt(low(int), high(int))


proc genInt8*(min, max: int8): Gen[int8] =
  genScalar(min, max)


proc genInt8*(): Gen[int8] = genInt8(low(int8), high(int8))


proc genInt16*(min, max: int16): Gen[int16] =
  genScalar(min, max)


proc genInt16*(): Gen[int16] = genInt16(low(int16), high(int16))


proc genInt32*(min, max: int32): Gen[int32] =
  genScalar(min, max)


proc genInt32*(): Gen[int32] = genInt32(low(int32), high(int32))


proc genInt64*(min, max: int64): Gen[int64] =
  genScalar(min, max)


proc genInt64*(): Gen[int64] = genInt64(low(int64), high(int64))


proc genUint8*(min, max: uint8): Gen[uint8] =
  genScalar(min, max)


proc genUint8*(): Gen[uint8] = genUint8(low(uint8), high(uint8))


proc genUint16*(min, max: uint16): Gen[uint16] =
  genScalar(min, max)


proc genUint16*(): Gen[uint16] = genUint16(low(uint16), high(uint16))


proc genUint32*(min, max: uint32): Gen[uint32] =
  genScalar(min, max)


proc genUint32*(): Gen[uint32] = genUint32(low(uint32), high(uint32))


proc genUint64*(min, max: uint64): Gen[uint64] =
  genScalar(min, max)


proc genUint64*(): Gen[uint64] = genUint64(low(uint64), high(uint64))


proc genEnum*[T: enum](min, max: T): Gen[T] =
  assert max >= min
  when T is OrdinalEnum:
    return genScalar(min, max)
  else:
    genEnumImpl(getEnumMembers(min, max))


proc genEnum*[T: enum](): Gen[T] =
  genEnum[T](T.low, T.high)


proc genSet*[T: enum](minLen: uint16 = 0, exclude: set[T] = {}): Gen[set[T]] =
  ## Create a set generator for the enum type `T` excluding the values in
  ## `exclude`.
  let vals = getEnumMembers(T.low, T.high, exclude)
  let maxLen = vals.len
  assert minLen <= uint16(maxLen), "minLen (" & $minLen & ") must be <= maxLen (" & $maxLen & ")"

  let g = genEnumImpl(vals)

  return proc(s: Source): set[T] =
    let (len, oldLen, actualLenPos) = s.beginArray(uint32(minLen), uint32(maxLen))
    let upperLimit = int(maxLen) * 15
    var draws = 0

    if s.recording:
      while result.len < int(len) and draws < upperLimit:
        result.incl g(s)
        draws.inc
      s.writeRawBytesAt(actualLenPos, uint64(draws), 4)
    else:
      while result.len < int(len) and draws < int(oldLen):
        result.incl g(s)
        draws.inc
      s.skipNodes(int(oldLen) - draws)


proc genSeq*[T](g: sink Gen[T], minLen: uint32 = 0, maxLen: uint32 = 100): Gen[seq[T]] =
  ## Create a sequence generator with element type `T` and length in the range
  ## [minLen, maxLen].
  assert maxLen >= minLen
  return proc(s: Source): seq[T] =
    let (len, oldLen, _) = s.beginArray(minLen, maxLen)

    result = newSeq[T](int(len))
    for i in 0 ..< int(len):
      result[i] = g(s)
    s.skipNodes(int(oldLen) - int(len))


proc genString*(minLen: uint32 = 0, maxLen: uint32 = 100,
                charGen: sink Gen[char] = genChar()): Gen[string] =
  ## Create a string generator for the range [minLen, maxLen] using the given
  ## char generator.
  assert maxLen >= minLen
  return proc(s: Source): string =
    let (len, oldLen, _) = s.beginArray(minLen, maxLen)
    result = newString(int(len))
    for i in 0 ..< int(len):
      result[i] = charGen(s)
    s.skipNodes(int(oldLen) - int(len))


proc genAsciiString*(minLen: uint32 = 0, maxLen: uint32 = 100): Gen[string] =
  ## Create an ASCII string generator.
  genString(minLen, maxLen, genAsciiChar())


proc genArray*[T](g: sink Gen[T], size: static uint32): Gen[array[size, T]] =
  ## Create an array generator with element type `T` and static size `size`.
  return proc(s: Source): array[size, T] =
    let (_, oldLen, _) = s.beginArray(size, size)
    for i in 0'u32 ..< size:
      result[i] = g(s)
    s.skipNodes(int(oldLen) - int(size))


proc genFloatScalar[T: SomeFloat](min, max: T,
                                  allowNaN, allowInf,
                                  allowSubnormal: bool): Gen[T] =
  return proc(s: Source): T =
    # Classes: 0:normal, 1:-0.0, 2:Inf, 3:-Inf, 4:NaN
    var classes: seq[int] = @[0, 1]
    if allowInf:
      classes.add(2)
      classes.add(3)
    if allowNaN: classes.add(4)

    let choice = s.chooseRange(0'u64, 100'u64, skByte)
    var cls = 0
    if choice > 90 and classes.len > 1:
      cls = classes[int((choice - 91) mod uint64(classes.len - 1)) + 1]

    case cls
    of 1: return T(-0.0)
    of 2: return T(Inf)
    of 3: return T(-Inf)
    of 4: return T(NaN)
    else:
      let
        fHigh = when T is float64: 1.7976931348623157e+308 else: 3.4028235e+38f
        actualMin = if classify(min) == fcNaN: (if allowInf: T(-Inf) else: T(-fHigh)) else: min
        actualMax = if classify(max) == fcNaN: (if allowInf: T(Inf) else: T(fHigh)) else: max
        minOrd = when T is float64: float64ToOrdinal(actualMin) else: int64(float32ToOrdinal(actualMin))
        maxOrd = when T is float64: float64ToOrdinal(actualMax) else: int64(float32ToOrdinal(actualMax))
        simplest = if actualMin > 0: actualMin elif actualMax < 0: actualMax else: T(0.0)
        simplestOrd = when T is float64: float64ToOrdinal(simplest) else: int64(float32ToOrdinal(simplest))

        numBelow = cast[uint64](simplestOrd) - cast[uint64](minOrd)
        numAbove = cast[uint64](maxOrd) - cast[uint64](simplestOrd)
        rangeSize = numBelow + numAbove
        tgtKind = when T is float64: sk8Bytes else: sk4Bytes

      let rank = s.chooseRange(0'u64, rangeSize, tgtKind)
      let o = rankToOrdinal(rank, minOrd, maxOrd, simplestOrd)

      let res = when T is float64: ordinalToFloat64(o) else: T(ordinalToFloat32(int32(o)))
      if not allowSubnormal and classify(res) == fcSubnormal: return T(0.0)
      return res


proc genFloat64*(min, max: float64 = NaN,
                 allowNaN: bool = false, allowInf: bool = true,
                 allowSubnormal: bool = true): Gen[float64] =
  genFloatScalar(min, max, allowNaN, allowInf, allowSubnormal)


proc genFloat32*(min, max: float32 = NaN,
                 allowNaN: bool = false, allowInf: bool = true,
                 allowSubnormal: bool = true): Gen[float32] =
  genFloatScalar(min, max, allowNaN, allowInf, allowSubnormal)


proc genFloat*(min, max: float = NaN,
               allowNaN: bool = false, allowInf: bool = true,
               allowSubnormal: bool = true): Gen[float] =
  genFloatScalar(min, max, allowNaN, allowInf, allowSubnormal)


# MARK: Shrinking Strategies ---

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
    let
      rangeSize = skipToRangeOffsetAndGetSize(buf, p)

    # After skipToRangeOffsetAndGetSize, p is at the offset.
    # We skip the offset too.
    p += bytesForRange(rangeSize)

    # Now p is at actualLen scalar (4 bytes)
    let n = int(decodeUint64(buf, p, 4))
    p += 4

    # Now p is at elements
    doAssert p + n <= buf.len, "collectNodes buffer ran out before children for array"
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
      var p = pos + 1 # skip skArray tag (1). Now at naked range.
      let
        rangeTgtKind = cast[StorageKind](buffer[p])
      inc p
      let
        sBytes = getScalarBytes(rangeTgtKind)
        recordedRangeSize = decodeUint64(buffer, p, sBytes)
      p += sBytes
      let
        vBytes = bytesForRange(recordedRangeSize)
        offsetPos = p
      p += vBytes
      let
        actualLenPos = p
        numElems = int(decodeUint64(buffer, p, 4))
      p += 4

      if numElems > 0:
        var
          elemStarts: seq[int] = @[]
          curr = p
        for _ in 0 ..< numElems:
          elemStarts.add(curr)
          curr = skipNode(buffer, curr)
        elemStarts.add(curr)

        var k = numElems
        while k > 0:
          var i = 0
          while i <= numElems - k:
            var copy = buffer
            let
              newElems = numElems - k
              oldOffset = decodeUint64(buffer, offsetPos, vBytes)
              newOffset = if oldOffset >= uint64(k): oldOffset - uint64(k) else: 0'u64
            writeUint64(copy, actualLenPos, 4, uint64(newElems))
            writeUint64(copy, offsetPos, vBytes, newOffset)
            let
              delStart = elemStarts[i]
              delEnd   = elemStarts[i + k] - 1
            if delStart <= delEnd:
              copy.delete(delStart .. delEnd)
            yield copy
            i.inc
          k = k div 2

  template numberShrinker(val: uint64, p: int, vBytes: int, buffer: seq[byte]) =
    if val != 0:
      var copy = buffer
      writeUint64(copy, p, vBytes, 0)
      yield copy

    # Delta shrinking (Hits thresholds precisely)
    var step = 1'u64 shl (uint64(vBytes) * 8 - 1)
    while step > 0:
      if step <= val:
        let cand = val - step
        if cand != 0:
          var copy = buffer
          writeUint64(copy, p, vBytes, cand)
          yield copy
      step = step shr 1

    # Binary shrinking (coarse scale)
    var tryVal = val
    while tryVal > 0:
      tryVal = tryVal div 2
      if tryVal != 0:
        var copy = buffer
        writeUint64(copy, p, vBytes, tryVal)
        yield copy

    # Bit clearing
    for i in countdown(63, 0):
      let mask = 1'u64 shl i
      if (val and mask) != 0:
        let candidate = val xor mask
        if candidate != 0:
          var copy = buffer
          writeUint64(copy, p, vBytes, candidate)
          yield copy

    if val > 0 and val < 100:
      var copy = buffer
      writeUint64(copy, p, vBytes, val - 1)
      yield copy

  # TODO: implement skNBytes shrinking
  #       it's basically an arbitrary precision bigint

  # Strategy 2: Range Binary Search
  for (pos, kind) in nodes:
    if kind == skRange:
      var p = pos + 1
      let
        rangeSize = skipToRangeOffsetAndGetSize(buffer, p)
        vBytes = bytesForRange(rangeSize)
      # p is now at the offset
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
        let rangeSize = skipToRangeOffsetAndGetSize(buffer, p)
        # p is now at offset
        p += bytesForRange(rangeSize)
        # p is now at actualLen
        let n = int(decodeUint64(buffer, p, 4))
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
        let rangeSize = decodeUint64(buffer, p, sBytes)
        p += sBytes
        let
          vBytes = bytesForRange(rangeSize)
          offset = decodeUint64(buffer, p, vBytes)
        result &= "  ".repeat(indent) & $kind & " (kind: " & $tgtKind &
                  ", rangeSize: " & $rangeSize & ", offset: " &
                  $offset & ")"
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
    errorMsg*: Option[string]
    shrunk*: bool
    shrunkValue*: Option[T]
    shrunkBuffer*: seq[byte]
    debugBuffer*: seq[byte]


const defaultTrials* = 1024 ## number of trials to run per property


proc runProperty*[T](p: Property[T], trials: int = defaultTrials,
                     seed: uint32 = 0, maxDiscards: int = -1,
                     debug: bool = false): TestResult[T] =
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

  let actualMaxDiscards = if maxDiscards == -1: trials * 10 else: maxDiscards
  var discardCount = 0

  while result.runCount < trials:
    if discardCount >= actualMaxDiscards:
      result.status = psFail
      result.errorMsg = some("Gave up: too many discards (" & $discardCount & ")")
      return

    let runSeed = uint32(rng.next() and 0xFFFFFFFF'u64)
    result.seed = runSeed

    var
      s = newSource(runSeed)
      val: T
      status: PropertyStatus
      valErrorMsg: Option[string]

    try:
      val = p.gen(s)
      status = p.check(val)
    except FilterExhaustedError:
      # Filter failed too many times, discard this run
      status = psDiscard
    except SourceLimitExceededError:
      # General generation error (limit exceeded perhaps)
      status = psDiscard
    except CatchableError as e:
      status = psFail
      valErrorMsg = some(e.msg)
    except Exception as e:
      status = psFail
      valErrorMsg = some(e.msg)

    if status == psDiscard:
      discardCount.inc
      continue

    result.runCount.inc

    if status == psFail:
      # Found failure!
      result.status = psFail
      result.failingValue = some(val)
      result.failingBuffer = s.buffer
      result.errorMsg = valErrorMsg
      if debug:
        result.debugBuffer = s.buffer

      # Start shrinking
      var
        bestBuffer = s.buffer
        bestVal = val
        bestErrorMsg = valErrorMsg
        attempts = 0

      # Shrink loop
      var improved = true
      while improved:
        improved = false
        for cand in candidates(bestBuffer):
          attempts.inc
          if attempts > 100000: break # Safety break

          # Shortlex ordering: strictly shorter or same length but lexicographically smaller
          if cand.len > bestBuffer.len: continue
          if cand.len == bestBuffer.len and not lexLess(cand, bestBuffer): continue

          # Try candidate
          var
            sCand = newSource(cand)
            cVal: T
            cStatus: PropertyStatus
            cErrorMsg: Option[string]

          try:
            cVal = p.gen(sCand)
            cStatus = p.check(cVal)
          except SourceLimitExceededError:
            cStatus = psDiscard
          except CatchableError as e:
            cStatus = psFail
            cErrorMsg = some(e.msg)
          except:
            # Defect or other weirdness.
            # In shrinking, this usually means we corrupted the stream.
            # Don't accept this as an improvement.
            continue

          if cStatus == psFail:
            # Our candidates iterator uses AST heuristics to generate strictly
            # smaller or simpler candidate buffers.
            # We accept the first one that fails:
            bestBuffer = cand
            bestVal = cVal
            bestErrorMsg = cErrorMsg
            improved = true
            result.shrunk = true
            break # Restart candidates iterator with new bestBuffer

        if attempts > 100000: break

      result.shrunkBuffer = bestBuffer
      result.shrunkValue = some(bestVal)
      result.errorMsg = bestErrorMsg
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
