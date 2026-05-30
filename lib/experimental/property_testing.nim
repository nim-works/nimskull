## Property Testing library, which allows for the specification and testing
## of properties of code.
##
## Property-based testing is a methodology where you define general
## characteristics (properties) that your code should satisfy across a wide
## range of inputs, rather than asserting specific outputs for hardcoded
## inputs. This library automatically generates random inputs to test these
## properties, and if a failure occurs, it aggressively "shrinks" the input to
## find the minimal, simplest example that reproduces the bug.
##
## ### Motivating Example
##
## Imagine an e-commerce function that applies a coupon discount to a shopping
## cart total. A naive unit test might check `applyDiscount(100, 20) == 80`.
## A property test instead asserts universal truths about the function:
##
runnableExamples:
  # import experimental/property_testing # required in other files

  proc applyDiscount(total, discount: int): int =
    max(0, total - discount)

  let result = runProperty:
    forAll (total: genInt(0, 1000), discount: genInt(0, 100)):
      let discounted = applyDiscount(total, discount)
      # Property 1: The discounted total is never greater than the original
      if discounted > total: return psFail
      # Property 2: The discounted total is never negative
      if discounted < 0: return psFail
      return psPass

  assert result.status == psPass
##
## ### Core Concepts
##
## The library revolves around a few key types and concepts:
## - **Properties (`forAll`)**: The idiomatic way to define a test is using the
##   `forAll` procedures, which pair generators with a predicate function that
##   returns a `PropertyStatus` (`psPass`, `psFail`, or `psDiscard`).
## - **Generators (`Gen[T]`)**: Procedures that consume a `Source` of
##   randomness to produce values of type `T`.
## - **Shrinking**: An automatic process that simplifies failing test cases.
##
## ### Writing Property Tests
##
## Tests are typically constructed using `forAll` and executed with
## `runProperty`. `runProperty` runs the scenario numerous times (default 256)
## with different seeds. If a failure (`psFail`) is encountered, the library
## automatically begins shrinking the generated inputs to find the most minimal
## reproducing case, which is then available in the `TestResult`.
##
## Tests can return `psDiscard` if the generated inputs do not meet certain
## preconditions, effectively skipping that run without failing the test. For
## example, validating that a division function works correctly when the
## denominator is not zero.
##
## ### Generators and Sources
##
## To generate data, you build or compose `Gen[T]` procedures. The standard
## library provides many built-in generators:
## - **Primitives**: `genInt`, `genBool`, `genByte`, `genChar`, `genString`.
## - **Collections**: `genSeq`, `genSet`, `genArray`.
## - **Ranges**: `genInt(min, max)`, `genEnum`.
##
## You can compose and modify generators using combinators:
## - `map`: Transforms the output of a generator (e.g. generating even numbers
##   by mapping `x => x * 2`).
## - `filter`: Discards values that don't meet a predicate. (Use sparingly, as
##   too many retries raise `FilterExhaustedError`).
## - `flatMap`: Chains generators dependently.
##
## Underlying all generation is the `Source` object. It provides the entropy
## for generators and records the sequence of choices made. This recording is
## what enables the library's powerful, integrated shrinking capabilities.
##
## ### Integrated Shrinking
##
## This library uses **integrated shrinking** (inspired by Hypothesis).
## Unlike traditional type-directed shrinking, this library shrinks the
## *underlying byte stream* (the `Source` buffer) that produced the values,
## rather than shrinking the typed values themselves.
##
## This approach has several massive advantages:
## - You do not need to write custom `shrink` functions for your custom types.
## - Filtering and `flatMap` work perfectly and maintain invariants during
##   shrinking, because the shrinking happens on the raw entropy before the
##   combinators run.
## - It aggressively finds minimal examples using structural heuristics like
##   sequence deletion, binary search on numeric ranges, and unbounded scalar
##   lowering.
##
## See the following posts on the hows and whys of integrated shrinking:
## - https://hypothesis.works/articles/integrated-shrinking/
## - https://hypothesis.works/articles/compositional-shrinking/


# MARK: Future Development TODOs:
# - separate core and api modules
# - allow pluggable random number generators
# - integrate with unittest runner
# - implement skNBytes


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
    ## Defines the different types of data storage in the source buffer.
    skByte         ## 1 byte
    sk2Bytes       ## 2 bytes
    sk4Bytes       ## 4 bytes
    sk8Bytes       ## 8 bytes
    skNBytes       ## N bytes
    skRange        ## Range of values
    skArray        ## Array of bytes, with size stored as a range in the
                   ## immediately following bytes and elements thereafter
    skGroup        ## Group of values, with size stored in the next byte

  ScalarStorageKind = range[skByte..sk8Bytes]

  ScalarBytes* = range[1..8] ## The number of bytes used to store a scalar value.

  Source* = ref object
    ## A source of randomness and a record of choices made by generators.
    rng: Rand
    buffer*: seq[byte]
    pos: int
    recording: bool
    limit*: int       ## Max bytes to generate before stopping/erroring
    idempotent*: bool ## whether the generator consuming this source should
                      ## be able to produce the same value given the same
                      ## source state, i.e.: disabling exhaustiveness
    debug*: bool      ## used for debugging

  Gen*[T] = proc(s: Source): T

  PropertyStatus* = enum
    ## Represents the result of a single property test execution.
    psPass,
    psFail,
    psDiscard, # Discard is for preconditions not met
    psError    # Error is for unexpected generator crashes

  Property*[T] = object
    ## Represents a property to be tested, consisting of a generator and a
    ## predicate function.
    gen*: Gen[T]
    check*: proc(x: T): PropertyStatus

  SourceLimitExceededError* = object of CatchableError
    ## Error raised when a source exceeds its allocated byte limit.
  FilterExhaustedError* = object of CatchableError
    ## Error raised when a filter fails to find a valid value after its maximum
    ## number of retries.


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
  ## Creates a source for recording.
  Source(
    rng: initRand(int64(seed)),
    buffer: @[],
    pos: 0,
    recording: true,
    limit: limit,
    idempotent: idempotent,
    debug: debug
  )


proc newSource*(buffer: sink seq[byte]): Source =
  ## Creates a source for replaying/shrinking with a fixed buffer.
  Source(
    rng: initRand(0),
    limit: buffer.len,
    buffer: buffer, # Not used when not recording
    pos: 0,
    recording: false,
    idempotent: false
  )


proc writeRawByte*(s: Source, b: byte) =
  ## When recording writes the byte `b` to the buffer, otherwise ignores it.
  if s.recording:
    if s.buffer.len >= s.limit:
      raise newException(SourceLimitExceededError, "Source limit exceeded")
    s.buffer.add(b)
    s.pos.inc


proc readRawByte*(s: Source): byte =
  ## Read the current byte and advances to the next byte, if there are no more
  ## recorded buffer given the position, it returns zero.
  if s.pos < s.buffer.len:
    result = s.buffer[s.pos]
  else:
    # Start generating zeroes if we run out of buffer.
    # This is crucial for shrinking where we might cut the buffer short.
    result = 0
  s.pos.inc


proc writeStorageKind*(s: Source, kind: StorageKind) =
  ## Writes the storage kind, `kind`, to the buffer and advances to the next
  ## byte.
  s.writeRawByte(byte(ord(kind)))


proc readStorageKind*(s: Source, expected: StorageKind): StorageKind =
  ## Reads the current byte as a `StorageKind` and advances to the next byte.
  ##
  ## Structural Resilience:
  ## If the buffer is not exhausted, it asserts that the read kind matches
  ## `expected`. If the buffer is exhausted, it returns `expected` without
  ## asserting, allowing the caller to return a safe default value.
  result = cast[StorageKind](s.readRawByte())
  if s.pos <= s.buffer.len:
    assert result == expected, "Expected " & $expected & ", got " & $result


func bytesForRange*(rangeSize: uint64): ScalarBytes =
  ## Determines the number of bytes required to store a range of size
  ## `rangeSize`.
  if rangeSize <= 0xFF'u64: 1
  elif rangeSize <= 0xFFFF'u64: 2
  elif rangeSize <= 0xFFFFFFFF'u64: 4
  else: 8


proc writeRawBytes*(s: Source, val: uint64, bytes: ScalarBytes) =
  ## Writes upto `bytes` worth of bytes from `val`.
  for i in 0 ..< bytes:
    s.writeRawByte(byte((val shr (i * 8)) and 0xFF))


proc readRawBytes*(s: Source, bytes: ScalarBytes): uint64 =
  ## Read upto `bytes` worth of bytes and returns them.
  for i in 0 ..< bytes:
    result = result or (uint64(s.readRawByte()) shl (i * 8))


proc getScalarBytes*(kind: ScalarStorageKind): ScalarBytes =
  ## Determines the number of bytes that `kind` requires.
  case kind
  of skByte:   1
  of sk2Bytes: 2
  of sk4Bytes: 4
  of sk8Bytes: 8


proc rngNextBytes*(s: Source, bytes: ScalarBytes): uint64 =
  ## Generate and return `bytes` worth RNG bytes.
  result = s.rng.next()
  if bytes < 8:
    let mask = (1'u64 shl (bytes * 8)) - 1
    result = result and mask


proc rngNextUInt32(s: Source): uint32 =
  uint32(s.rng.next() and 0xFFFFFFFF'u64)


proc chooseScalarRaw*(s: Source, kind: ScalarStorageKind): uint64 =
  ## Produces a random scalar of `kind` when recording, otherwise reads a
  ## scalar of `kind` from the buffer.
  ##
  ## Structural Verification:
  ## During replay, this uses `readStorageKind(kind)` to ensure the buffer
  ## is structurally compatible. If the buffer is exhausted, it returns a
  ## safe default (0) without crashing.
  let bytes = getScalarBytes(kind)
  if s.recording:
    result = s.rngNextBytes(bytes)
    s.writeStorageKind(kind)
    s.writeRawBytes(result, bytes)
  else:
    discard s.readStorageKind(kind)
    result = s.readRawBytes(bytes)


proc recordRangeData(s: Source, rangeSize, offset: uint64,
                     kind: ScalarStorageKind) =
  ## Writes a naked range (no `skRange` tag) to the source, using `kind` to
  ## size the rangeSize field.
  s.writeStorageKind(kind)
  s.writeRawBytes(rangeSize, getScalarBytes(kind))
  s.writeRawBytes(offset, bytesForRange(rangeSize))


proc readRangeData(s: Source, currentRangeSize: uint64,
                   kind: ScalarStorageKind): uint64 =
  ## Reads a naked range (no `skRange` tag) from the source, expecting `kind`
  ## sized storage for the rangeSize.
  ##
  ## Structural Resilience:
  ## If the buffer is exhausted, it returns a safe default offset (0).
  let
    tgtKind = ScalarStorageKind(s.readStorageKind(kind))
    recordedRangeSize = s.readRawBytes(getScalarBytes(tgtKind))
    rVal = s.readRawBytes(bytesForRange(recordedRangeSize))
  result = if rVal > currentRangeSize: currentRangeSize else: rVal


proc recordRange*(s: Source, rangeSize, offset: uint64,
                  kind: ScalarStorageKind) =
  ## Records a structural range marker (`skRange`) followed by its metadata.
  s.writeStorageKind(skRange)
  s.recordRangeData(rangeSize, offset, kind)


proc chooseRange*(s: Source, min, max: uint64,
                  kind: ScalarStorageKind): uint64 =
  ## Produces a random uint64 between `min` and `max` when recording,
  ## otherwise reads it from the buffer.
  ##
  ## Structural Verification:
  ## Replay-phase reads verify structural compatibility via `skRange` tags.
  let rangeSize = max - min
  if s.recording:
    let
      valRange =
        if rangeSize == 0: 0'u64
        elif rangeSize == 0xFFFFFFFFFFFFFFFF'u64: s.rngNextBytes(8)
        else: s.rngNextBytes(bytesForRange(rangeSize)) mod (rangeSize + 1)

    result = min + valRange
    s.recordRange(rangeSize, valRange, kind)
  else:
    discard s.readStorageKind(skRange)
    result = min + s.readRangeData(rangeSize, kind)


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


proc chooseRange*(s: Source, min, max: int64,
                  scalarKind: ScalarStorageKind): int64 =
  ## Produces a random int64 between `min` and `max` when recording,
  ## otherwise reads it from the buffer.
  ##
  ## Mapping:
  ## Uses monotonic renumeration to map the signed range to an unsigned space
  ## centering 0.0 for high-quality shrinking.
  let
    uMin = renumerateInt64ToUint64(min)
    uMax = renumerateInt64ToUint64(max)
  renumerateUint64ToInt64(chooseRange(s, uMin, uMax, scalarKind))


proc decodeUint64*(buffer: seq[byte], pos: int, bytes: ScalarBytes): uint64 =
  ## Reads `bytes` worth of bytes from `buffer` at position `pos` as a uint64.
  for i in 0 ..< bytes:
    assert pos + i < buffer.len,
           "decodeUint64 buffer ran out before end of scalar"
    result = result or (uint64(buffer[pos + i]) shl (i * 8))


proc skipToRangeOffsetAndGetSize(buffer: seq[byte], pos: var int): uint64 =
  ## Returns the range size for a range stored in the
  ## buffer at the given position where the skRange byte has already been
  ## traversed. Advances `pos` past the rangeSize.
  doAssert pos < buffer.len, "skipNode buffer ran out before kind for range"
  let tgtKind = ScalarStorageKind(buffer[pos])
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
  pos

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
    discard s.readStorageKind(skArray)
    let
      offset = s.readRangeData(rangeSize, tgtKind)
      newLen = uint32(uint64(min) + offset)
      actualLen = uint32(s.readRawBytes(4))
    result = (newLen, actualLen, -1)


proc beginFixedArray*(s: Source, len: uint32): (uint32, uint32, int) =
  ## Marks the beginning of a fixed-size array of length `len` of homogeneous
  ## elements in the stream.
  beginArray(s, len, len)


proc readGroupLength*(s: Source): uint8 =
  ## Parses a group marker and returns the number of fields
  discard s.readStorageKind(skGroup)
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
  ## Converts ordinal value `o` to a float64.
  if o >= 0: result = cast[float64](cast[uint64](o))
  else: result = cast[float64](cast[uint64](-o) or 0x8000000000000000'u64)


proc float32ToOrdinal*(f: float32): int32 =
  ## Converts float `f` to an `int32` ordinal.
  let bits = cast[uint32](f)
  if (bits and 0x80000000'u32) == 0:
    result = cast[int32](bits)
  else:
    result = -cast[int32](bits and 0x7FFFFFFF'u32)


proc ordinalToFloat32*(o: int32): float32 =
  ## Converts ordinal value `o` to a float32.
  if o >= 0: result = cast[float32](cast[uint32](o))
  else: result = cast[float32](cast[uint32](-o) or 0x80000000'u32)


proc ordinalToRank*(o: int64): uint64 =
  ## Converts an ordinal `o` to an uint64.
  if o >= 0: result = uint64(o) shl 1
  else: result = (uint64(-o) shl 1) - 1


proc rankToOrdinal*(r: uint64, uMin, uMax, uSimp: uint64): uint64 =
  ## Maps a non-negative rank back to a uint64 value within [uMin, uMax],
  ## centering on uSimp.
  let
    numBelow = uSimp - uMin
    numAbove = uMax - uSimp
    common = min(numBelow, numAbove)

  if r <= 2 * common:
    if (r and 1) != 0: result = uSimp - ((r + 1) shr 1)
    else: result = uSimp + (r shr 1)
  elif numBelow > numAbove:
    result = uSimp - (r - common)
  else:
    result = uSimp + (r - common)


# MARK: Combinators

proc map*[T, U](g: sink Gen[T], f: sink proc(x: T): U): Gen[U] =
  ## Create a new generator based on `g`, using `f` to map values of the base
  ## generator.
  result = proc(s: Source): U = f(g(s))


proc filter*[T](g: sink Gen[T], pred: sink proc(x: T): bool, maxRetries: int = 100): Gen[T] =
  ## Create a new generator based on `g`, that filters output based on the
  ## predicate procedure (`pred`), with `maxRetries` per filter attempt,
  ## raising a `FilterExhaustedError` if exceeded.
  result = proc(s: Source): T =
    # This loop requires care to avoid infinite loops.

    # Try first attempt
    result = g(s)
    if not pred(result):
      var found = false
      for _ in 0 ..< maxRetries:
        result = g(s)
        if pred(result):
          found = true
          break

      if not found:
        # If exhausted, we must inform the caller that generation failed.
        # Checks using this generator should likely discard the run.
        raise newException(FilterExhaustedError, "Filter retries exhausted")


proc flatMap*[T, U](g: sink Gen[T], f: sink proc(x: T): Gen[U]): Gen[U] =
  ## Takes the initial generator, and a factory function that creates a new generator
  ## based on the value generated by the initial generator.
  ##
  ## This is useful for creating a generator that generates values based on the
  ## values generated by another generator.
  result = proc(s: Source): U = f(g(s))(s)


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

  result = proc(s: Source): T =
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
    uMin = renumerateInt64ToUint64(minO)
    uMax = renumerateInt64ToUint64(maxO)
    uSimp = renumerateInt64ToUint64(simplestO)
    rangeSize = uMax - uMin

  genExhaustive(rangeSize + 1,
                rank => (let uRes = rankToOrdinal(rank, uMin, uMax, uSimp);
                         let o = renumerateUint64ToInt64(uRes);
                         when T is SomeInteger: cast[T](o) else: cast[T](int(o))))


proc chooseRanked[T: Ordinal](s: Source, min, max, simplest: T,
                              kind: ScalarStorageKind): T =
  let
    minO = cast[int64](min)
    maxO = cast[int64](max)
    simplestO = cast[int64](simplest)
    uMin = renumerateInt64ToUint64(minO)
    uMax = renumerateInt64ToUint64(maxO)
    uSimp = renumerateInt64ToUint64(simplestO)
    rangeSize = uMax - uMin

  if rangeSize == 0: return simplest

  let rank = s.chooseRange(0'u64, rangeSize, kind)
  let uRes = rankToOrdinal(rank, uMin, uMax, uSimp)
  let o = renumerateUint64ToInt64(uRes)
  cast[T](o)


proc genFromList*[T](vals: sink seq[T], simplestIdx: int = 0): Gen[T] =
  ## Create a generator that draws from a fixed list of values, `vals`, with
  ## ranked shrinking targeting the `simplestIdx`.
  let len = vals.len
  if len == 0:
    result = proc(s: Source): T = default(T)
  elif len <= 256:
    let g = genExhaustiveRanked(0, len - 1, simplestIdx)
    result = proc(s: Source): T = vals[g(s)]
  else:
    let rangeK = if len <= 65536: sk2Bytes else: sk4Bytes
    result = proc(s: Source): T =
      vals[s.chooseRanked(0, len - 1, simplestIdx, rangeK)]


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
  genExhaustive(uint64(vals.len), (idx) => vals[idx])


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

  result =
    if rangeSize < 256:
      genExhaustiveRanked(min, max, simplest)
    else:
      (proc(s: Source): T = s.chooseRanked(min, max, simplest, tgtKind))


proc genConst*[T](v: T): Gen[T] =
  ## Create a generator that always returns `v`.
  return proc(s: Source): T = v


proc genByte*(): Gen[byte] =
  ## Create a byte generator.
  genScalar(byte.low, byte.high)


proc genBool*(): Gen[bool] =
  ## Create a boolean generator.
  genScalar(false, true)


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
  ## Generate an int8 arbitrary for the range [min, max].
  genScalar(min, max)


proc genInt8*(): Gen[int8] = genInt8(low(int8), high(int8))
  ## Create an int8 arbitrary for the full int8 range.


proc genInt16*(min, max: int16): Gen[int16] =
  ## Generate an int16 arbitrary for the range [min, max].
  genScalar(min, max)


proc genInt16*(): Gen[int16] = genInt16(low(int16), high(int16))
  ## Create an int16 arbitrary for the full int16 range.


proc genInt32*(min, max: int32): Gen[int32] =
  ## Generate an int32 arbitrary for the range [min, max].
  genScalar(min, max)


proc genInt32*(): Gen[int32] = genInt32(low(int32), high(int32))
  ## Create an int32 arbitrary for the full int32 range.


proc genInt64*(min, max: int64): Gen[int64] =
  ## Generate an int64 arbitrary for the range [min, max].
  genScalar(min, max)


proc genInt64*(): Gen[int64] = genInt64(low(int64), high(int64))
  ## Create an int64 arbitrary for the full int64 range.


proc genUint8*(min, max: uint8): Gen[uint8] =
  ## Generate an uint8 arbitrary for the range [min, max].
  genScalar(min, max)


proc genUint8*(): Gen[uint8] = genUint8(low(uint8), high(uint8))
  ## Create an uint8 arbitrary for the full uint8 range.


proc genUint16*(min, max: uint16): Gen[uint16] =
  ## Generate an uint16 arbitrary for the range [min, max].
  genScalar(min, max)


proc genUint16*(): Gen[uint16] = genUint16(low(uint16), high(uint16))
  ## Create an uint16 arbitrary for the full uint16 range.


proc genUint32*(min, max: uint32): Gen[uint32] =
  ## Generate an uint32 arbitrary for the range [min, max].
  genScalar(min, max)


proc genUint32*(): Gen[uint32] = genUint32(low(uint32), high(uint32))
  ## Create an uint32 arbitrary for the full uint32 range.


proc genUint64*(min, max: uint64): Gen[uint64] =
  ## Generate an uint64 arbitrary for the range [min, max].
  genScalar(min, max)


proc genUint64*(): Gen[uint64] = genUint64(low(uint64), high(uint64))
  ## Create an uint64 arbitrary for the full uint64 range.


proc genEnum*[T: enum](min, max: T): Gen[T] =
  ## Generate an enum arbitrary of type `T` for the range [min, max].
  assert min <= max
  when T is OrdinalEnum:
    genScalar(min, max)
  else:
    genEnumImpl(getEnumMembers(min, max))


proc genEnum*[T: enum](): Gen[T] =
  ## Create an enum arbitrary of type `T` for the full uint64 range.
  genEnum[T](T.low, T.high)


proc genSet*[T: enum](minLen: uint16 = 0, exclude: set[T] = {}): Gen[set[T]] =
  ## Create a set generator for the enum type `T` excluding the values in
  ## `exclude`.
  let
    vals = getEnumMembers(T.low, T.high, exclude)
    maxLen = uint16(vals.len)
  assert minLen <= maxLen,
         "minLen (" & $minLen & ") must be <= maxLen (" & $maxLen & ")"

  result = proc(s: Source): set[T] =
    # represent the set as a fixed-size list of bit-packed scalars
    let
      numFull = int(maxLen) div 64
      remBits = int(maxLen) mod 64
      numChunks = numFull + (if remBits > 0: 1 else: 0)

    discard s.beginFixedArray(uint32(numChunks))

    var idx = 0
    for _ in 0 ..< numFull:
      let bits = s.chooseScalarRaw(sk8Bytes)
      for j in 0 ..< 64:
        if (bits and (1'u64 shl j)) != 0:
          result.incl vals[idx]
        idx.inc

    if remBits > 0:
      let
        kind = if   remBits <= 8:  skByte
               elif remBits <= 16: sk2Bytes
               elif remBits <= 32: sk4Bytes
               else:               sk8Bytes
        bits = s.chooseScalarRaw(kind)
      for j in 0 ..< remBits:
        if (bits and (1'u64 shl j)) != 0:
          result.incl vals[idx]
        idx.inc

    # make sure the set always has at least `minLen` elements
    if uint16(result.len) < minLen:
      var pos = int(s.chooseRange(0'u64, uint64(maxLen - 1), sk2Bytes))
      for i in uint16(result.len) ..< minLen:
        while vals[pos] in result:
          pos = (pos + 1) mod int(maxLen)
        result.incl vals[pos]


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
  # Classes: 0:normal, 1:-0.0, 2:Inf, 3:-Inf, 4:NaN
  var classes: seq[int] = @[0, 1]
  if allowInf:
    classes.add(2)
    classes.add(3)
  if allowNaN:
    classes.add(4)

  return proc(s: Source): T =

    let choice = s.chooseRange(0'u64, 100'u64, skByte)
    var cls = 0
    if choice > 90 and classes.len > 1:
      cls = classes[int((choice - 91) mod uint64(classes.len - 1)) + 1]

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

    # always pick a rank, so that the same number of bytes written/read
    # is the same every time
    let rank = s.chooseRange(0'u64, rangeSize, tgtKind)

    result = case cls
      of 1: T(-0.0)
      of 2: T(Inf)
      of 3: T(-Inf)
      of 4: T(NaN)
      else:
        let
          uRes = rankToOrdinal(rank, renumerateInt64ToUint64(minOrd),
                                     renumerateInt64ToUint64(maxOrd),
                                     renumerateInt64ToUint64(simplestOrd))
          o = renumerateUint64ToInt64(uRes)
          res = when T is float64: ordinalToFloat64(o) else: T(ordinalToFloat32(int32(o)))
        if not allowSubnormal and classify(res) == fcSubnormal: T(0.0)
        else: res


proc genFloat64*(min, max: float64 = NaN,
                 allowNaN: bool = false, allowInf: bool = true,
                 allowSubnormal: bool = true): Gen[float64] =
  ## Generate an float64 arbitrary for the range [min, max], with flags to
  ## control whether NaN, Inf, and Subnormal values are allowed.
  genFloatScalar(min, max, allowNaN, allowInf, allowSubnormal)


proc genFloat32*(min, max: float32 = NaN,
                 allowNaN: bool = false, allowInf: bool = true,
                 allowSubnormal: bool = true): Gen[float32] =
  ## Generate an float32 arbitrary for the range [min, max], with flags to
  ## control whether NaN, Inf, and Subnormal values are allowed.
  genFloatScalar(min, max, allowNaN, allowInf, allowSubnormal)


proc genFloat*(min, max: float = NaN,
               allowNaN: bool = false, allowInf: bool = true,
               allowSubnormal: bool = true): Gen[float] =
  ## Generate an float arbitrary for the range [min, max], with flags to
  ## control whether NaN, Inf, and Subnormal values are allowed.
  genFloatScalar(min, max, allowNaN, allowInf, allowSubnormal)


# MARK: Shrinking Strategies

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
    let rangeSize = skipToRangeOffsetAndGetSize(buf, p)

    # After skipToRangeOffsetAndGetSize, p is at the offset.
    # We skip the offset too.
    p += bytesForRange(rangeSize)

    # Now p is at actualLen scalar (4 bytes)
    let n = int(decodeUint64(buf, p, 4))
    p += 4

    # Now p is at elements
    doAssert p + n <= buf.len,
             "collectNodes buffer ran out before children for array"
    for _ in 0 ..< n:
      collectNodes(nodes, buf, p)
      p = skipNode(buf, p)
  of skGroup:
    let lenBytes = 1
    doAssert lenBytes == 0 or (p + lenBytes - 1) < buf.len,
             "buffer is too short, buf.len: " & $buf.len & " p: " & $p &
             " lenBytes: " & $lenBytes

    if lenBytes > 0 and (p + lenBytes - 1) < buf.len:
      let n = int(decodeUint64(buf, p, lenBytes))

      doAssert(lenBytes > 0 or n == 0, "lenBytes was 0 and n was > 0")

      p += lenBytes
      doAssert p < buf.len or n == 0,
               "buffer is too short after reading length, buf.len: " &
               $buf.len & " p: " & $p & " lenBytes: " & $lenBytes & " n: " & $n
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
  ## Take the `buffer` and produces a set of candidate buffers that are shrinks
  ## of the original.
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
      let rangeTgtKind = ScalarStorageKind(buffer[p])
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
          k.dec

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
        let tgtKind = ScalarStorageKind(buffer[p])
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
    ## The results of running a property test, including failure details and
    ## shrinking results if a failure was found.
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


proc evaluate[T](p: Property[T], s: Source): (PropertyStatus, T, Option[string]) =
  ## Internal helper to run a generator and check its property status.
  ##
  ## Error Handling:
  ## Distinguishes between generator crashes (`psError`) and property
  ## violations (`psFail`). This separation is critical for the shrinker
  ## to avoid pursuing corrupted buffer states.
  try:
    result[1] = p.gen(s)
  except FilterExhaustedError:
    return (psDiscard, default(T), none(string))
  except SourceLimitExceededError:
    return (psDiscard, default(T), none(string))
  except:
    let e = getCurrentException()
    return (psError, default(T), some(if e != nil: e.msg
                                      else: "Unknown error during generation"))

  try:
    result[0] = p.check(result[1])
  except:
    let e = getCurrentException()
    result[0] = psFail
    result[2] = some(if e != nil: e.msg else: "Unknown error during check")


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

    var s = newSource(runSeed)
    let (status, val, valErrorMsg) = p.evaluate(s)

    if status == psDiscard:
      discardCount.inc
      continue

    result.runCount.inc

    if status == psError:
      result.status = psFail
      result.failingValue = some(val)
      result.failingBuffer = s.buffer
      result.errorMsg = valErrorMsg
      return

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
        validProbes = false

      # Shrink loop
      var improved = true
      while improved:
        improved = false
        for cand in candidates(bestBuffer):
          attempts.inc
          if attempts > 100000: break # Safety break

          # Shortlex ordering: strictly shorter or same length but lexicographical smaller
          if cand.len > bestBuffer.len: continue
          if cand.len == bestBuffer.len and not lexLess(cand, bestBuffer): continue

          # Try candidate
          var sCand = newSource(cand)
          let (cStatus, cVal, cErrorMsg) = p.evaluate(sCand)

          if cStatus != psError:
            validProbes = true

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

      if validProbes:
        result.shrunk = true

      result.shrunkBuffer = bestBuffer
      result.shrunkValue = some(bestVal)
      result.errorMsg = bestErrorMsg
      return # Return immediately on failure+shrink

  # If loop finishes without returning, it's a pass


# MARK: Tuple Generators

macro genTupleProc*(N: static uint): untyped =
  ## Generates a `genTuple` procedure for the given arity `N`.
  let n = int(N)
  let procName = ident("genTuple")

  # 1. Generic Parameters: [T1, T2, ..., TN]
  var genericParams = newNimNode(nnkGenericParams)
  for i in 1..n:
    genericParams.add(newIdentDefs(ident("T" & $i), newEmptyNode()))

  # 2. Return Tuple Type: (T1, ..., TN)
  # Using nnkPar for tuple types in this context.
  var tupleTy = newNimNode(nnkTupleConstr)
  for i in 1..n:
    tupleTy.add(ident("T" & $i))

  # 3. Formal Parameters: (g1: sink Gen[T1], ..., gN: sink Gen[TN]): Gen[(T1, ..., TN)]
  var formalParams = newNimNode(nnkFormalParams)
  formalParams.add(newTree(nnkBracketExpr, bindSym("Gen"), tupleTy))
  for i in 1..n:
    formalParams.add(newIdentDefs(ident("g" & $i),
      newTree(nnkCommand, ident("sink"),
              newTree(nnkBracketExpr, bindSym("Gen"), ident("T" & $i)))))

  # 4. Closure Body
  var tupleConstr = newNimNode(nnkTupleConstr)
  for i in 1..n:
    tupleConstr.add(newCall(ident("g" & $i), ident("s")))

  var closureBody = newStmtList()
  closureBody.add(newTree(nnkDiscardStmt,
    newCall(newDotExpr(ident("s"), ident("beginGroup")), newLit(uint8(n)))))
  closureBody.add(tupleConstr)

  # 5. The Lambda Closure: proc(s: Source): (T1, ..., TN) = ...
  let closure = newTree(nnkLambda,
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode(),
    newTree(nnkFormalParams, copyNimTree(tupleTy), newIdentDefs(ident("s"), bindSym("Source"))),
    newEmptyNode(),
    newEmptyNode(),
    closureBody
  )

  # 6. The Final Procedure: proc genTupleN*[...] = result = closure
  result = newTree(nnkProcDef,
    newTree(nnkPostfix, ident("*"), procName),
    newEmptyNode(),
    genericParams,
    formalParams,
    newTree(nnkPragma, ident("inline")),
    newEmptyNode(),
    newTree(nnkStmtList, newAssignment(ident("result"), closure))
  )


genTupleProc(1)
genTupleProc(2)
genTupleProc(3)
genTupleProc(4)
genTupleProc(5)
genTupleProc(6)
genTupleProc(7)
genTupleProc(8)
genTupleProc(9)
genTupleProc(10)


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


proc genProc*[R](retGen: sink Gen[R]): Gen[proc(): R] =
  ## Create a generator for a procedure that takes no arguments and returns a
  ## value of type R produced by retGen.
  return proc(s: Source): proc(): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(): R =
      var callSeed = funcSeed
      # No args to hash
      var src = newSource(callSeed, idempotent=true)
      retGen(src)


proc genProc1*[T1, R](retGen: sink Gen[R]): Gen[proc(a: T1): R] =
  ## Create a generator for a procedure that takes one argument of type T1 and
  ## returns a value of type R produced by retGen.
  return proc(s: Source): proc(a: T1): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      var src = newSource(callSeed, idempotent=true)
      retGen(src)


proc genProc2*[T1, T2, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2): R] =
  ## Create a generator for a procedure that takes two arguments of types T1
  ## and T2, and returns a value of type R produced by retGen.
  return proc(s: Source): proc(a: T1, b: T2): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      var src = newSource(callSeed, idempotent=true )
      retGen(src)


proc genProc3*[T1, T2, T3, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2, c: T3): R] =
  ## Create a generator for a procedure that takes three arguments of types T1,
  ## T2, and T3, and returns a value of type R produced by retGen.
  return proc(s: Source): proc(a: T1, b: T2, c: T3): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c))
      var src = newSource(callSeed, idempotent=true)
      retGen(src)


proc genProc4*[T1, T2, T3, T4, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4): R] =
  ## Create a generator for a procedure that takes four arguments of types T1,
  ## T2, T3, and T4, and returns a value of type R produced by retGen.
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c))
      hashCombine(callSeed, hashArg(d))
      var src = newSource(callSeed, idempotent=true)
      retGen(src)


proc genProc5*[T1, T2, T3, T4, T5, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5): R] =
  ## Create a generator for a procedure that takes five arguments of types T1,
  ## T2, T3, T4, and T5, and returns a value of type R produced by retGen.
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
      retGen(src)


proc genProc6*[T1, T2, T3, T4, T5, T6, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R] =
  ## Create a generator for a procedure that takes six arguments of types T1
  ## through T6, and returns a value of type R produced by retGen.
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      var src = newSource(callSeed, idempotent=true)
      retGen(src)


proc genProc7*[T1, T2, T3, T4, T5, T6, T7, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R] =
  ## Create a generator for a procedure that takes seven arguments of types T1
  ## through T7, and returns a value of type R produced by retGen.
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g))
      var src = newSource(callSeed, idempotent=true)
      retGen(src)


proc genProc8*[T1, T2, T3, T4, T5, T6, T7, T8, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R] =
  ## Create a generator for a procedure that takes eight arguments of types T1
  ## through T8, and returns a value of type R produced by retGen.
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R =
    let funcSeed = cast[uint32](s.chooseRange(0, cast[uint64](uint32.high), sk4Bytes))
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g)); hashCombine(callSeed, hashArg(h))
      var src = newSource(callSeed, idempotent=true)
      retGen(src)


proc genProc9*[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9): R] =
  ## Create a generator for a procedure that takes nine arguments of types T1
  ## through T9, and returns a value of type R produced by retGen.
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
      retGen(src)


proc genProc10*[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](retGen: sink Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10): R] =
  ## Create a generator for a procedure that takes ten arguments of types T1
  ## through T10, and returns a value of type R produced by retGen.
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
      retGen(src)



# Void return variants

proc genVoidProc*(): Gen[proc()] =
  ## Create a generator for a procedure that takes no arguments and returns
  ## nothing.
  return proc(s: Source): proc() = (proc() = discard)

# For void procs with args, they just consume args but return nothing.
# They don't need to be deterministic for return value since there is none.
# They are essentially sinks.

macro genVoidProcN*(T: varargs[typedesc]): untyped =
  ## Create a generator for a procedure that takes arguments of types specified
  ## by T and returns nothing.
  var args = @[bindSym"void"]

  let names = toSeq('a'..'z').mapIt($it)
  if args.len > names.len:
    error("genVoidProcN: too many arguments, max: " & $names.len)

  for i, t in T.pairs:
    args.add(nnkIdentDefs.newTree(ident(names[i]), t, newEmptyNode()))

  let
    prc = newProc(params = args)
    typ = nnkProcTy.newTree(copyNimTree(prc.params), newEmptyNode())

  result = quote do:
    genConst(`typ`(`prc`))


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

  let checkProc = newProc(params = procArgs, body = procBody)

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
