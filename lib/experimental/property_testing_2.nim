## Property Testing library, which allows for the specification and testing
## of properties of code.

# This is inspired by hypothesis (https://hypothesis.works/), where shrinking
# is an integral part of the testing process. See the following post on the
# hows and whys of integrated shrinking:
# https://hypothesis.works/articles/integrated-shrinking/ and the follow-on
# about compositional shrinking:
# https://hypothesis.works/articles/compositional-shrinking/

import std/[
    mersenne,
    options,
  ]

from std/algorithm import sort
from std/hashes import hash
from std/sequtils import delete, toSeq
from std/sugar import `=>`
from std/times import getTime, toUnix
from std/typetraits import enumLen

# MARK: Core Types

type
  StorageKind* = enum
    skByte         ## 1 byte
    sk2Bytes       ## 2 bytes
    sk4Bytes       ## 4 bytes
    sk8Bytes       ## 8 bytes
    skNBytes       ## N bytes
    skRange        ## Range of values
    skBoolString8  ## Boolean string, with size stored in the next byte
    skBoolString16 ## Boolean string, with size stored in the next 2 bytes
    skBoolString32 ## Boolean string, with size stored in the next 4 bytes
    skArray8       ## Array of bytes, with size stored in the next byte
    skArray16      ## Array of 2 bytes, with size stored in the next 2 bytes
    skArray32      ## Array of 4 bytes, with size stored in the next 4 bytes
    skGroup        ## Group of values, with size stored in the next byte

  Source* = ref object
    rng: MersenneTwister
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
  ExhaustiveState*[T] = ref object
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
  result.rng = newMersenneTwister(seed)
  result.buffer = @[]
  result.pos = 0
  result.recording = true
  result.limit = limit
  result.idempotent = idempotent
  result.debug = debug


proc newSource*(buffer: seq[byte]): Source =
  ## Create a source for replaying/shrinking with a fixed buffer
  new(result)
  result.rng = newMersenneTwister(0)
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
  of skByte: 1
  of sk2Bytes: 2
  of sk4Bytes: 4
  of sk8Bytes: 8
  else: 0


proc rngNextBytes*(s: Source, bytes: int): uint64 =
  var val: uint64 = 0
  if bytes <= 4:
    val = uint64(s.rng.getNum())
  else:
    val = uint64(s.rng.getNum()) or (uint64(s.rng.getNum()) shl 32)
  if bytes < 8:
    let mask = (1'u64 shl (bytes * 8)) - 1
    val = val and mask
  return val


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


proc recordRange*(s: Source, min, max, val: uint64, scalarKind: StorageKind) =
  s.writeStorageKind(skRange)
  s.writeStorageKind(scalarKind)
  let sBytes = getScalarBytes(scalarKind)
  s.writeRawBytes(min, sBytes)
  s.writeRawBytes(max, sBytes)
  s.writeRawBytes(val - min, bytesForRange(max - min))


proc chooseRange*(s: Source, min, max: uint64, scalarKind: StorageKind): uint64 =
  if s.recording:
    let
      rangeSize = max - min
      valRange = 
        if rangeSize == 0: 0'u64
        elif rangeSize == 0xFFFFFFFFFFFFFFFF'u64: s.rngNextBytes(8)
        else: s.rngNextBytes(8) mod (rangeSize + 1)
      
    result = min + valRange
    s.recordRange(min, max, result, scalarKind)
  else:
    let readK = s.readStorageKind()
    if readK == skRange:
      let
        tgtKind = s.readStorageKind()
        sBytes = getScalarBytes(tgtKind)
        rMin = s.readRawBytes(sBytes)
        rMax = s.readRawBytes(sBytes)
        rangeSize = if rMax > rMin: rMax - rMin else: 0'u64
        rVal = s.readRawBytes(bytesForRange(rangeSize))
        safeVal = if rVal > rangeSize: rangeSize else: rVal
      result = rMin + safeVal
      if result > max: result = max
      if result < min: result = min
    else:
      result = min


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
      if state.pos < state.indices.len and not s.idempotent:
        let
          randValOrig = s.rng.getNum()
          remaining = state.indices.len - state.pos
          offset = int(randValOrig mod uint32(remaining))
        chosenIdx = state.indices.swapAccess(state.pos, state.pos + offset)
        state.pos.inc
      else:
        let randValOrig = s.rng.getNum()
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
      if state.pos < state.indices.len and not s.idempotent:
        let
          randValOrig = s.rng.getNum()
          remaining = state.indices.len - state.pos
          offset = int(randValOrig mod uint32(remaining))
        chosenIdx = state.indices.swapAccess(state.pos, state.pos + offset)
        state.pos.inc
      else:
        let randValOrig = s.rng.getNum()
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
  let rangeSize = cast[uint64](max) - cast[uint64](min)
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int =
      cast[int](s.chooseRange(cast[uint64](min), cast[uint64](max), sk8Bytes))

proc genInt*(): Gen[int] =
  ## Generate an int for the full range of int.
  genInt(low(int), high(int))


proc genInt8*(min, max: int8): Gen[int8] =
  ## Create an int8 generator for the range [min, max].
  assert max >= min
  let rangeSize = cast[uint64](max) - cast[uint64](min)
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int8 =
      cast[int8](s.chooseRange(cast[uint64](min), cast[uint64](max), skByte))


proc genInt8*(): Gen[int8] = genInt8(low(int8), high(int8))


proc genInt16*(min, max: int16): Gen[int16] =
  assert max >= min
  let rangeSize = cast[uint64](max) - cast[uint64](min)
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int16 =
      cast[int16](s.chooseRange(cast[uint64](min), cast[uint64](max), sk2Bytes))


proc genInt16*(): Gen[int16] = genInt16(low(int16), high(int16))


proc genInt32*(min, max: int32): Gen[int32] =
  assert max >= min
  let rangeSize = cast[uint64](max) - cast[uint64](min)
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int32 =
      cast[int32](s.chooseRange(cast[uint64](min), cast[uint64](max), sk4Bytes))

proc genInt32*(): Gen[int32] = genInt32(low(int32), high(int32))


proc genInt64*(min, max: int64): Gen[int64] =
  assert max >= min
  let rangeSize = cast[uint64](max) - cast[uint64](min)
  if rangeSize <= 255'u64:
    return genExhaustiveRange(min, rangeSize)
  else:
    return proc(s: Source): int64 =
      cast[int64](s.chooseRange(cast[uint64](min), cast[uint64](max), sk8Bytes))


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


proc genSet*[T: enum](minLen = 0, exclude: set[T] = {}): Gen[set[T]] =
  ## create a set generator for the enum type `T` excluding the values in
  ## `exclude`.
  let maxLen = enumLen(T) - exclude.len
  assert minLen <= maxLen, "minLen (" & $minLen & ") must be <= maxLen (" & $maxLen & ")"

  # TODO: rework this so we generate enum values the same way we generate
  #       exhaustive enums, that way we don't use up too much recorded entropy
  #       when generating shrunken sets.

  let g =
    if exclude.len == 0: genEnum[T]()
    else: genEnum[T]().filter((e) => e notin exclude)

  return proc(s: Source): set[T] =
    let
      # Use chooseRange avoiding module arithmetic
      len = s.chooseRange(cast[uint64](minLen), cast[uint64](maxLen), sk4Bytes)
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
    let len: uint32 =
      if maxLen == minLen: minLen
      else: cast[uint32](s.chooseRange(cast[uint64](minLen), cast[uint64](maxLen), sk4Bytes))
    result = newSeq[T](len)
    for i in 0 ..< len:
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


proc genArray*[T](g: Gen[T], size: static int): Gen[array[size, T]] = 
  return proc(s: Source): array[size, T] = 
    var arr: array[size, T]
    for i in 0 ..< size:
      arr[i] = g(s)
    return arr


# MARK: Shrinking Strategies ---

iterator candidates(buffer: seq[byte]): seq[byte] =
  # Strategy 1: Remove chunks
  # We iterate over chunk sizes: 1, 2, 4, 8 ...
  # And for each chunk size, we try removing chunks at various offsets.
  
  if buffer.len > 0:
    # Try removing the whole thing first?
    yield @[] 

  var k = 8 # Initial chunk size, can be tuned
  while k > 0:
    var i = 0
    while i <= buffer.len - k:
      # Remove buffer[i ..< i+k]
      var copy = buffer
      copy.delete(i..(i + k - 1))
      yield copy
      i.inc # Try all offsets? Or step by k? Stepping by k is faster.
    k = k div 2

  # Strategy 2: Zeroing chunks
  if buffer.len > 0:
    var k = 8
    while k > 0:
      var i = 0
      while i <= buffer.len - k:
        # Check if range has non-zeros
        var hasNonZero = false
        for j in 0 ..< k:
          if buffer[i+j] != 0:
            hasNonZero = true
            break
        
        if hasNonZero:
          var copy = buffer
          for j in 0 ..< k:
            copy[i+j] = 0
          yield copy
        
        i.inc
      k = k div 2
       
  # Strategy 3: Sort chunks
  # This strategy assumes that the order of bytes might not matter for some properties (e.g. set equality)
  # or that sorted inputs are "simpler".
  # We can try to sort chunks of the buffer.
  if buffer.len > 1:
      # Try sorting the whole buffer first
      var copy = buffer
      copy.sort()
      if copy != buffer:
          yield copy

      # Try sorting smaller chunks?
      # Maybe just sorting pairs?
      var i = 0
      while i < buffer.len - 1:
          if buffer[i] > buffer[i+1]:
              var swapCopy = buffer
              swap(swapCopy[i], swapCopy[i+1])
              yield swapCopy
          i.inc

  # Strategy 4: Byte Lowering
  # Try to decrement individual bytes to minimize values
  if buffer.len > 0:
    for i in 0 ..< buffer.len:
      let original = buffer[i]
      if original > 0:
        var copy = buffer
        
        # Try halving (binary search step)
        copy[i] = original div 2
        yield copy

        # Try decrementing by 1
        copy[i] = original - 1
        yield copy

        # Try decrementing by 2 (helps bridge gaps from filters)
        if original > 1:
          copy[i] = original - 2
          yield copy


func `<`(x, y: seq[byte]): bool =
  ## Lexicographical comparison for sequences of bytes, used for ordering
  ## shrunk values. `x < y` if `x` is lexicographically smaller than `y`,
  ## meaning `x` is shorter or has smaller bytes at the first differing
  ## position.
  result = x.len < y.len
  if not result and x.len == y.len:
    for i in 0 ..< x.len:
      if x[i] == y[i]:
        continue
      else:
        result = x[i] < y[i]
        break

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


proc runProperty*[T](p: Property[T], trials: int = 256, seed: uint32 = 0): TestResult[T] =
  # Uses time as seed base if not provided 
  var masterSeed = seed
  if masterSeed == 0:
      masterSeed = uint32(getTime().toUnix() and 0xFFFFFFFF)
      
  var rng = newMersenneTwister(masterSeed)
  
  result.status = psPass
  result.runCount = 0

  for i in 1..trials:
    result.runCount = i
    let runSeed = rng.getNum()
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
        continue # Skip this run, verify if we should counting it against trials?
                 # Usually discards shouldn't count towards success, 
                 # but we accept it for simplicity here to avoid infinite loops.

    if status == psFail:
      # Found failure!
      result.status = psFail
      result.failingValue = some(val)
      result.failingBuffer = s.buffer
      
      # Start shrinking
      var bestBuffer = s.buffer
      var bestVal = val

      # Shrink loop
      var improved = true
      while improved:
        improved = false
        for cand in candidates(bestBuffer):
          if cand.len >= bestBuffer.len and cand == bestBuffer: continue # Skip if same
          
          # Try candidate
          var sCand = newSource(cand)
          var cVal: T
          var cStatus: PropertyStatus
          
          try:
            cVal = p.gen(sCand)
            cStatus = p.check(cVal)
          except:
            cStatus = psFail # Exception is failure too
            
          if cStatus == psFail:
            # We found a smaller/simpler failure!
            # We accept it if it is strictly smaller/simpler by some metric.
            # Our candidates iterator usually yields "smaller" things first or structurally simpler things.
            # We'll just take it.
            if cand < bestBuffer:
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
    (g(s),)

proc genTuple*[T1, T2](g1: Gen[T1], g2: Gen[T2]): Gen[(T1, T2)] =
  ## Generates a tuple of two elements.
  return proc(s: Source): (T1, T2) =
    (g1(s), g2(s))

proc genTuple*[T1, T2, T3](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3]): Gen[(T1, T2, T3)] =
  ## Generates a tuple of three elements.
  return proc(s: Source): (T1, T2, T3) =
    (g1(s), g2(s), g3(s))

proc genTuple*[T1, T2, T3, T4](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4]): Gen[(T1, T2, T3, T4)] =
  ## Generates a tuple of four elements.
  return proc(s: Source): (T1, T2, T3, T4) =
    (g1(s), g2(s), g3(s), g4(s))

proc genTuple*[T1, T2, T3, T4, T5](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5]): Gen[(T1, T2, T3, T4, T5)] =
  ## Generates a tuple of five elements.
  return proc(s: Source): (T1, T2, T3, T4, T5) =
    (g1(s), g2(s), g3(s), g4(s), g5(s))

proc genTuple*[T1, T2, T3, T4, T5, T6](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6]): Gen[(T1, T2, T3, T4, T5, T6)] =
  ## Generates a tuple of six elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6) =
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s))

proc genTuple*[T1, T2, T3, T4, T5, T6, T7](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7]): Gen[(T1, T2, T3, T4, T5, T6, T7)] =
  ## Generates a tuple of seven elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6, T7) =
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s), g7(s))

proc genTuple*[T1, T2, T3, T4, T5, T6, T7, T8](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8]): Gen[(T1, T2, T3, T4, T5, T6, T7, T8)] =
  ## Generates a tuple of eight elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6, T7, T8) =
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s), g7(s), g8(s))

proc genTuple*[T1, T2, T3, T4, T5, T6, T7, T8, T9](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8], g9: Gen[T9]): Gen[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
  ## Generates a tuple of nine elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6, T7, T8, T9) =
    (g1(s), g2(s), g3(s), g4(s), g5(s), g6(s), g7(s), g8(s), g9(s))

proc genTuple*[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8], g9: Gen[T9], g10: Gen[T10]): Gen[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
  ## Generates a tuple of ten elements.
  return proc(s: Source): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) =
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

proc genProcVoid*(): Gen[proc()] =
  return proc(s: Source): proc() =
    # Function with no return value and no args doesn't need to do anything 
    # other than exist. 
    return proc() = discard

# For void procs with args, they just consume args but return nothing.
# They don't need to be deterministic for return value since there is none.
# They are essentially sinks.
proc genProcVoid1*[T1](): Gen[proc(a: T1)] =
  return proc(s: Source): proc(a: T1) = (proc(a: T1) = discard)

proc genProcVoid2*[T1, T2](): Gen[proc(a: T1, b: T2)] =
  return proc(s: Source): proc(a: T1, b: T2) = (proc(a: T1, b: T2) = discard)

proc genProcVoid3*[T1, T2, T3](): Gen[proc(a: T1, b: T2, c: T3)] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3) = 
    (proc(a: T1, b: T2, c: T3) = discard)

proc genProcVoid4*[T1, T2, T3, T4](): Gen[proc(a: T1, b: T2, c: T3, d: T4)] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4) = 
    (proc(a: T1, b: T2, c: T3, d: T4) = discard)

proc genProcVoid5*[T1, T2, T3, T4, T5](): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5)] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5) = 
    (proc(a: T1, b: T2, c: T3, d: T4, e: T5) = discard)

proc genProcVoid6*[T1, T2, T3, T4, T5, T6](): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6)] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6) = 
    (proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6) = discard)

proc genProcVoid7*[T1, T2, T3, T4, T5, T6, T7](): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7)] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7) = 
    (proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7) = discard)

proc genProcVoid8*[T1, T2, T3, T4, T5, T6, T7, T8](): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8)] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8) = 
    (proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8) = discard)

proc genProcVoid9*[T1, T2, T3, T4, T5, T6, T7, T8, T9](): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9)] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9) = 
    (proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9) = discard)

proc genProcVoid10*[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10)] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10) = 
    (proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10) = discard)


# MARK: Property Helpers

# TODO: implement these like `execProperty` and `forAll` from `property_testing`

proc forAll*[T](gen: Gen[T], check: proc(x: T): PropertyStatus): Property[T] = 
  return Property[T](gen: gen, check: check)

proc forAll*[T1, T2](g1: Gen[T1], g2: Gen[T2], check: proc(x: T1, y: T2): PropertyStatus): Property[(T1, T2)] = 
  return Property[(T1, T2)](gen: genTuple(g1, g2), check: check)

proc forAll*[T1, T2, T3](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], check: proc(x: T1, y: T2, z: T3): PropertyStatus): Property[(T1, T2, T3)] = 
  return Property[(T1, T2, T3)](gen: genTuple(g1, g2, g3), check: check)

proc forAll*[T1, T2, T3, T4](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], check: proc(x: T1, y: T2, z: T3, w: T4): PropertyStatus): Property[(T1, T2, T3, T4)] = 
  return Property[(T1, T2, T3, T4)](gen: genTuple(g1, g2, g3, g4), check: check)

proc forAll*[T1, T2, T3, T4, T5](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], check: proc(x: T1, y: T2, z: T3, w: T4, v: T5): PropertyStatus): Property[(T1, T2, T3, T4, T5)] = 
  return Property[(T1, T2, T3, T4, T5)](gen: genTuple(g1, g2, g3, g4, g5), check: check)

proc forAll*[T1, T2, T3, T4, T5, T6](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], check: proc(x: T1, y: T2, z: T3, w: T4, v: T5, u: T6): PropertyStatus): Property[(T1, T2, T3, T4, T5, T6)] = 
  return Property[(T1, T2, T3, T4, T5, T6)](gen: genTuple(g1, g2, g3, g4, g5, g6), check: check)

proc forAll*[T1, T2, T3, T4, T5, T6, T7](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], check: proc(x: T1, y: T2, z: T3, w: T4, v: T5, u: T6, t: T7): PropertyStatus): Property[(T1, T2, T3, T4, T5, T6, T7)] = 
  return Property[(T1, T2, T3, T4, T5, T6, T7)](gen: genTuple(g1, g2, g3, g4, g5, g6, g7), check: check)

proc forAll*[T1, T2, T3, T4, T5, T6, T7, T8](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8], check: proc(x: T1, y: T2, z: T3, w: T4, v: T5, u: T6, t: T7, s: T8): PropertyStatus): Property[(T1, T2, T3, T4, T5, T6, T7, T8)] = 
  return Property[(T1, T2, T3, T4, T5, T6, T7, T8)](gen: genTuple(g1, g2, g3, g4, g5, g6, g7, g8), check: check)

proc forAll*[T1, T2, T3, T4, T5, T6, T7, T8, T9](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8], g9: Gen[T9], check: proc(x: T1, y: T2, z: T3, w: T4, v: T5, u: T6, t: T7, s: T8, r: T9): PropertyStatus): Property[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = 
  return Property[(T1, T2, T3, T4, T5, T6, T7, T8, T9)](gen: genTuple(g1, g2, g3, g4, g5, g6, g7, g8, g9), check: check)

proc forAll*[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8], g9: Gen[T9], g10: Gen[T10], check: proc(x: T1, y: T2, z: T3, w: T4, v: T5, u: T6, t: T7, s: T8, r: T9, q: T10): PropertyStatus): Property[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = 
  return Property[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)](gen: genTuple(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10), check: check)
