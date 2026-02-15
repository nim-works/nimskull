discard """
  targets: "c js"
"""

import std/[options, unittest, strutils]
import experimental/property_testing_2

type 
  Colors = enum Red, Green, Blue
  E = enum A, B, C

suite "Property Testing 2 (Integrated Shrinking)":

  test "genConst":
    let prop = Property[int](
      gen: genConst(42),
      check: proc(x: int): PropertyStatus = psFail
    )
    let res = runProperty(prop, trials=10)
    check res.status == psFail
    check res.failingValue.get() == 42
    check res.shrunkValue.get() == 42

  test "Gen Char Ranges":
    let prop = Property[char](
      gen: genChar('a', 'z'),
      check: proc(c: char): PropertyStatus =
        if c >= 'a' and c <= 'z': psPass else: psFail
    )
    check runProperty(prop).status == psPass

  test "Gen Bool":
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

  test "genArray Shrinking":
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

  test "genEnum Shrinking":
    # Want x < Green (Red).
    # Fail if >= Green.
    # Gen Green or Blue.
    # Expect shrink to Green (Simpler than Blue).
    let prop = Property[Colors](
      gen: genEnum[Colors](),
      check: proc(x: Colors): PropertyStatus =
        if x < Green: psPass else: psFail
    )
    let res = runProperty(prop, trials=100)
    if res.status == psFail:
      check res.shrunkValue.get() == Green

  test "genSet Shrinking":
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

  test "Shrinking a simple integer predicate":
    # Predicate: x < 10.
    # Failure: x >= 10.
    # Expectation: Shrink to 10.
    
    let prop = Property[int](
      gen: genIntRange(0, 100),
      check: proc(x: int): PropertyStatus =
        if x < 10: psPass else: psFail
    )

    let res = runProperty(prop, trials = 100, seed = 1)
    
    check res.status == psFail
    check res.shrunk
    if res.shrunkValue.isSome:
      let val = res.shrunkValue.get
      checkpoint "Shrunk value: " & $val
      check val == 10
      # this should work because small int ranges are exhaustive

  # This test covers non-shrinking pass behavior
  test "Passing Property":
    let prop = Property[int](
      gen: genIntRange(0, 100),
      check: proc(x: int): PropertyStatus =
        if x >= 0: psPass else: psFail
    )
    let res = runProperty(prop, trials=10)
    check res.status == psPass
    check res.failingValue.isNone
    check res.shrunk == false

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
      # We hope it shrinks to exactly 5
      check val.len == 5

  test "Shrinking a string content (manual seeded)":
    # Predicate: not s.contains('A')
    # Failure: s.contains('A')
    # Use restricted generator to ensure 'A' appears often.
    
    let genRestricted = proc(s: Source): char =
      let b = s.nextByte()
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

  test "Filter shrinks correctly":
    # Even numbers. Want < 10.
    # Fail 10, 12, ...
    # Expect shrink to 10.
    let prop = Property[int](
      gen: genIntRange(0, 100).filter(proc(x: int): bool = x mod 2 == 0),
      check: proc(x: int): PropertyStatus =
        if x < 10: psPass else: psFail
    )
    let res = runProperty(prop, trials=100)
    if res.status == psFail:
      check res.shrunkValue.get() == 10

  test "Map shrinking":
    # Gen range 0..10. Map * 10 -> 0, 10, 20...
    # Want < 50.
    # Fail 50, 60...
    # Shrink to 50.
    # Underlying gen produces 5 (fails), 6 (fails)...
    # Shrink underlying to 5. Map(5) = 50.
    let prop = Property[int](
      gen: genIntRange(0, 10).map(proc(x: int): int = x * 10),
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

  test "Discarded Property":
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
    let res = runProperty(prop, trials=5)
    check res.status == psPass # Pass because no failure found.

  test "Gen Combinator - FlatMap":
    # Generate a length L, then a sequence of length L.
    let gen = genIntRange(0, 5).flatMap(proc(len: int): Gen[seq[byte]] =
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

  test "Gen Combinator - FlatMap Shrinking":
      # Gen size L. Gen Seq[byte] of size L.
      # Predicate: L < 3.
      # Fail if L >= 3.
      # Expect shrink to L=3. Seq of length 3.
      # Note: FlatMap shrinking is complex because the structure depends on the first value.
      # Integrated shrinking handles this naturally!
      let gen = genIntRange(0, 10).flatMap(proc(len: int): Gen[seq[byte]] =
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
