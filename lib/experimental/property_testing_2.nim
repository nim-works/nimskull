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
  Source* = ref object
    rng: MersenneTwister
    buffer*: seq[byte]
    pos: int
    recording: bool
    limit*: int # Max bytes to generate before stopping/erroring

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

const DefaultSourceLimit = 10_000 # Reasonable default limit

proc newSource*(seed: uint32, limit: int = DefaultSourceLimit): Source =
  new(result)
  result.rng = newMersenneTwister(seed)
  result.buffer = @[]
  result.pos = 0
  result.recording = true
  result.limit = limit

proc newSource*(buffer: seq[byte]): Source =
  ## Create a source for replaying/shrinking with a fixed buffer
  new(result)
  result.rng = newMersenneTwister(0)
  result.buffer = buffer # Not used when not recording
  result.pos = 0
  result.recording = false
  result.limit = buffer.len

proc nextByte*(s: Source): byte =
  if s.recording:
    if s.buffer.len >= s.limit:
      raise newException(SourceLimitExceededError, "Source limit exceeded")
      
    result = byte(s.rng.getNum() and 0xFF)
    s.buffer.add(result)
    s.pos.inc
  else:
    if s.pos < s.buffer.len:
      result = s.buffer[s.pos]
      s.pos.inc
    else:
      # Start generating zeroes if we run out of buffer.
      # This is crucial for shrinking where we might cut the buffer short.
      result = 0

proc nextBytes*(s: Source, count: int): seq[byte] =
  result = newSeq[byte](count)
  for i in 0 ..< count:
    result[i] = s.nextByte()

proc nextUint32*(s: Source): uint32 =
  # We consume 4 bytes
  let b = s.nextBytes(4)
  result = b[0].uint32 or (b[1].uint32 shl 8) or (b[2].uint32 shl 16) or (b[3].uint32 shl 24)

proc nextUint64*(s: Source): uint64 =
  let b = s.nextBytes(8)
  result = b[0].uint64 or (b[1].uint64 shl 8) or (b[2].uint64 shl 16) or (b[3].uint64 shl 24) or
           (b[4].uint64 shl 32) or (b[5].uint64 shl 40) or (b[6].uint64 shl 48) or (b[7].uint64 shl 56)

proc nextFloat64*(s: Source): float64 =
  cast[float64](s.nextUint64()) # Simple cast, might produce NaN etc.


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
  return proc(s: Source): U =
    let t = g(s)
    f(t)(s)


# MARK: Generators

proc swapAccess[T](s: var openArray[T], a, b: int): T =
  ## swap the value at position `a` for position `b`, then return the new value
  ## at position `a`. Used for exhaustive arbitrary traversal.
  result = s[b]

  if a != b:      # only need to swap if they're different
    s[b] = s[a]
    s[a] = result


proc genExhaustive*[T](vals: seq[T]): Gen[T] =
  ## Creates a generator that exhaustively iterates through `vals` before switching
  ## to random generation. It shuffles `vals` as it goes to ensure randomness
  ## within the exhaustive phase too (Simulated via swap access logic or just iteration).
  ##
  ## It keeps a `pos` index. It swaps `vals[pos]` with `vals[random index >= pos]`.
  ## This effectively shuffles the "remaining" items and picks one.
  ## Once `pos` reaches end, it switches to pure random

  var indices = newSeq[int](vals.len)
  for i in 0 ..< vals.len: indices[i] = i
  let state = ExhaustiveState[T](vals: vals, indices: indices, pos: 0)

  return proc(s: Source): T =
    if not s.recording:
      # Replay/Shrink mode:
      # We ignore the state.indices (which might be shuffled) and sample directly
      # from state.vals using the input source. This is equivalent to resetting
      # the indices to their original sorted state [0, 1, 2...] and sampling.
      let randVal = s.nextUint32()
      let idx = int(randVal mod uint32(state.vals.len))
      return state.vals[idx]

    if state.pos < state.indices.len:
      # Exhaustive phase
      # We pick an index `j` such that `state.pos <= j < state.indices.len`
      # We use the source RNG to pick `j`.
      
      let
        remaining = state.indices.len - state.pos
        randVal = s.nextUint32()
        offset = int(randVal mod uint32(remaining))
        swapPos = state.pos + offset

      let chosenIdx = state.indices.swapAccess(state.pos, swapPos)
      result = state.vals[chosenIdx]
      state.pos.inc
    else:
      # Random phase after exhaustion
      let randVal = s.nextUint32()
      let idx = int(randVal mod uint32(state.vals.len))
      result = state.vals[idx]


proc genByte*(): Gen[byte] =
  ## create a byte generator.
  return proc(s: Source): byte = s.nextByte()


proc genUint32*(): Gen[uint32] =
  ## create a uint32 generator.
  return proc(s: Source): uint32 = s.nextUint32()


proc genInt*(): Gen[int] =
  # Assuming 64-bit int for now or system int
  return proc(s: Source): int = cast[int](s.nextUint64()) 


proc genIntRange*(min, max: int): Gen[int] =
  ## create an integer generator for the range [min, max].
  assert max >= min
  let rangeSize = (max - min)
  if rangeSize <= int(uint8.high):
    let vals = toSeq(min..max)
    return genExhaustive(vals)
  else:
    return proc(s: Source): int =
      let rangeSize = (max - min)
      if rangeSize == 0: return min
      let val = abs(cast[int](s.nextUint64()))
      return min + (val mod rangeSize)


proc genChar*(min, max: char): Gen[char] =
  ## create a char arbitrary for the range [min, max].
  let vals = toSeq(min..max)
  return genExhaustive(vals)


proc genAsciiChar*(): Gen[char] =
  ## create a char arbitrary for the ASCII range.
  genChar(char(0), char(127))


proc genChar*(): Gen[char] =
  ## create a char arbitrary for the full character range, see: `genAsciiChar`
  genChar(char.low, char.high)


proc genBool*(): Gen[bool] =
  ## create a boolean generator.
  return genExhaustive(@[false, true])


proc genEnum*[T: enum](): Gen[T] =
  var vals = newSeq[T]()
  for e in T.items:
    vals.add(e)
  if enumLen(T) < int(high(uint8)):
    return genExhaustive(vals)
  else:
    return proc(s: Source): T =
      # TODO: because of holey enums we need to do this inefficiently to avoid
      # generating invalid enum values regardless of the specific enum, but
      # we should do some compile time logic to only do this for the holey
      # variety via: `typetraits.isHoleyEnum`
      let idx = s.nextUint32() mod uint32(vals.len)
      result = vals[idx]


proc genSet*[T: enum](minLen = 0, maxLen = enumLen(T), exclude: set[T] = {}): Gen[set[T]] =
  ## create a set generator for the enum type `T` excluding the values in
  ## `exclude`.
  let g =
    if exclude.len == 0: genEnum[T]()
    else: genEnum[T]().filter((e) => e notin exclude)

  return proc(s: Source): set[T] =
    let len = s.nextUint32() mod uint32(maxLen - minLen + 1) + uint32(minLen)
    for i in 0 ..< len:
      result.incl g(s)


proc genConst*[T](v: T): Gen[T] =
  ## create a generator that always returns `v`.
  return proc(s: Source): T = v


proc genSeq*[T](g: Gen[T], minLen: uint32 = 0, maxLen: uint32 = 100): Gen[seq[T]] =
  ## create a sequence generator with element type `T` and length in the range
  ## [minLen, maxLen].
  return proc(s: Source): seq[T] =
    let len: uint32 =
      if maxLen == minLen: minLen
      else:                s.nextUint32() mod uint32(maxLen - minLen + 1) + uint32(minLen)
    result = newSeq[T](len)
    for i in 0 ..< len:
      result[i] = g(s)


proc genString*(minLen: uint32 = 0, maxLen: uint32 = 100,
                charGen: Gen[char] = genChar()): Gen[string] =
  ## create a string generator for the range [minLen, maxLen] using the given
  ## char generator.
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

proc runProperty*[T](p: Property[T], trials: int = 100, seed: uint32 = 0): TestResult[T] =
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
    
    var s = newSource(runSeed)
    var val: T
    var status: PropertyStatus
    
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
    let funcSeed = s.nextUint32()
    return proc(): R =
      var callSeed = funcSeed
      # No args to hash
      var src = newSource(callSeed)
      return retGen(src)

proc genProc1*[T1, R](retGen: Gen[R]): Gen[proc(a: T1): R] =
  return proc(s: Source): proc(a: T1): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc2*[T1, T2, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2): R] =
  return proc(s: Source): proc(a: T1, b: T2): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc3*[T1, T2, T3, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2, c: T3): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc4*[T1, T2, T3, T4, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2, c: T3, d: T4): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c))
      hashCombine(callSeed, hashArg(d))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc5*[T1, T2, T3, T4, T5, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a))
      hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c))
      hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc6*[T1, T2, T3, T4, T5, T6, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc7*[T1, T2, T3, T4, T5, T6, T7, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc8*[T1, T2, T3, T4, T5, T6, T7, T8, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g)); hashCombine(callSeed, hashArg(h))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc9*[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g)); hashCombine(callSeed, hashArg(h))
      hashCombine(callSeed, hashArg(i))
      var src = newSource(callSeed)
      return retGen(src)

proc genProc10*[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](retGen: Gen[R]): Gen[proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10): R] =
  return proc(s: Source): proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10): R =
    let funcSeed = s.nextUint32()
    return proc(a: T1, b: T2, c: T3, d: T4, e: T5, f: T6, g: T7, h: T8, i: T9, j: T10): R =
      var callSeed = funcSeed
      hashCombine(callSeed, hashArg(a)); hashCombine(callSeed, hashArg(b))
      hashCombine(callSeed, hashArg(c)); hashCombine(callSeed, hashArg(d))
      hashCombine(callSeed, hashArg(e)); hashCombine(callSeed, hashArg(f))
      hashCombine(callSeed, hashArg(g)); hashCombine(callSeed, hashArg(h))
      hashCombine(callSeed, hashArg(i)); hashCombine(callSeed, hashArg(j))
      var src = newSource(callSeed)
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

# TODO move into the CLI interface once created

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
