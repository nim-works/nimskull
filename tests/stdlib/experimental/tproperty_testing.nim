discard """
  targets: "c js vm"
"""

import experimental/property_testing
import std/options
import std/sequtils
import std/sugar

# Helper to check reproducibility
proc getSamples[T](arb: Arbitrary[T], seed: uint32, count: uint): seq[T] =
  var rng = newRandom(seed)
  result = arb.sample(count, rng).mapIt(it.value)

block basic_random:
  var rng1 = newRandom(42)
  var rng2 = newRandom(42)
  doAssert rng1.getCallCount() == 0
  doAssert rng2.getCallCount() == 0

  let v1 = rng1.nextUint32()
  let v2 = rng2.nextUint32()
  doAssert v1 == v2
  doAssert rng1.getCallCount() == 1

block const_arb:
  let arb = constArb(10)
  let samples = arb.getSamples(123, 5)
  doAssert samples == @[10, 10, 10, 10, 10]

block int_arb:
  let arb = intArb(0, 10)
  let samples = arb.getSamples(1, 100)
  for s in samples:
    doAssert s >= 0 and s <= 10

block char_arb:
  let arb = charArb('a', 'z')
  let samples = arb.getSamples(2, 50)
  for s in samples:
    doAssert s >= 'a' and s <= 'z'

block string_arb:
  let arb = stringArb(0, 5, charArb('a', 'b'))
  let samples = arb.getSamples(3, 20)
  for s in samples:
    doAssert s.len >= 0 and s.len <= 5
    for c in s:
      doAssert c == 'a' or c == 'b'

block map:
  let arb = intArb(1, 10).map(x => x * 2)
  let samples = arb.getSamples(4, 20)
  for s in samples:
    doAssert s mod 2 == 0
    doAssert s >= 2 and s <= 20

block filter:
  let arb = intArb(0, 100).filter(x => x mod 2 == 0)
  let samples = arb.getSamples(5, 50)
  for s in samples:
    doAssert s mod 2 == 0

block flat_map:
  # Generate a length, then a string of that length
  let arb = uint32Arb(1, 5).flatMap(len => stringArb(len, len, charArb('a', 'z')))
  let samples = arb.getSamples(6, 20)
  for s in samples:
    doAssert s.len >= 1 and s.len <= 5

block tuple_arb:
  let arb = tupleArb(constArb(1), constArb(2))
  let samples = arb.getSamples(7, 5)
  for s in samples:
    doAssert s == (1, 2)

block seq_arb:
  let arb = seqArbOf(intArb(0, 9), 2, 4)
  let samples = arb.getSamples(8, 20)
  for s in samples:
    doAssert s.len >= 2 and s.len <= 4
    for item in s:
      doAssert item >= 0 and item <= 9

block enum_arb:
  type MyEnum = enum A, B, C
  let arb = enumArb[MyEnum]()
  let samples = arb.getSamples(9, 20)
  for s in samples:
    doAssert s in {A, B, C}
    
block set_arb:
  type MyEnum = enum A, B, C
  let arb = setArb[MyEnum]()
  let samples = arb.getSamples(10, 20)
  for s in samples:
    for e in s:
      doAssert e in {A, B, C}

block reproducibility:
  let arb = intArb()
  let s1 = arb.getSamples(999, 10)
  let s2 = arb.getSamples(999, 10)
  doAssert s1 == s2
