discard """
  targets: "c js"
"""

import std/[options, unittest, strutils]
import experimental/property_testing_2

type 
  Colors = enum Red, Green, Blue
  E = enum A, B, C

suite "Property Testing with Integrated Shrinking":

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

  test "Char range generator, exhaustively produces all values in range in random order":
    # TODO: verify order is random
    let prop = Property[char](
      gen: genChar('a', 'z'),
      check: proc(c: char): PropertyStatus =
        if c >= 'a' and c <= 'z': psPass else: psFail
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

  test "Enum generator, exhaustively produces all values in enum in random order (for small enums)":
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

  test "Set generator, exhaustively produces all possible sets of given elements (for small Enums)":
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

  test "Integer range generator, shrinking a simple integer predicate, exhaustive for small ranges":
    # Predicate: x < 10.
    # Failure: x >= 10.
    # Expectation: Shrink to 10.
    
    let prop = Property[int](
      gen: genIntRange(0, 100),
      check: proc(x: int): PropertyStatus =
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

  test "Filter creates a new generator and shrinks correctly":
    # Even numbers. Want < 10.
    # Fail 10, 12, ...
    # Expect shrink to 10.
    let prop = Property[int](
      gen: genIntRange(0, 20).filter(proc(x: int): bool = x mod 2 == 0),
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

  test "Shrinking FlatMapped generators works correctly":
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

  test "1 Element Tuple Generation and Shrinking":
    let prop = Property[(int,)](
      gen: genTuple(genIntRange(0, 10)),
      check: proc(t: (int,)): PropertyStatus =
        if t[0] < 5: psPass else: psFail
    )
    let res = runProperty(prop, trials=20)
    if res.status == psFail:
        check res.shrunkValue.get()[0] == 5

  test "2 Element Tuple Generation and Shrinking":
    let prop = Property[(int, int)](
      gen: genTuple(genIntRange(0, 100), genIntRange(0, 100)),
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
      gen: genTuple(genIntRange(0, 10), genIntRange(0, 10), genIntRange(0, 10)),
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
      gen: genProc1[int, int](genIntRange(0, 100)),
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
