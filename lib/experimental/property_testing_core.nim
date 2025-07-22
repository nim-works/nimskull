# these modules are used extensively
import std/macros
import std/mersenne
import std/options # need this for counter examples

# these modules have limited use, so be selective
from std/strutils import join, repeat
from std/sugar import `=>` # XXX: maybe a bust because inference can't keep up
from std/sequtils import toSeq, apply
from std/typetraits import enumLen

# XXX: Once this is mature enough (repeatability, shrinking, and API) move out
#      of experimental.

## :Author: Saem Ghani
## :License: MIT License
##
## Current Development:
## ====================
## This module implements property based testing facilities. Like most nice
## things there are two major parts of this module:
## 1. core concepts of structured data generation, operations, & property tests
## 2. the API by which the former is expressed
## It's important not to conflate the two, the current way tests are executed
## and reported upon is not relevant to the core as an example of this. Nor is
## the composition of arbitraries, predicates, and random number generators.
##
## API Evolution:
## --------------
## The API needs evolving, in a few areas:
## 1. definition of generators
## 2. expression of properties
## 
## Generators: at the very least `chain` or `fmap` is likely required along
## with a number of other combinators that allow for rapid definition of
## generators. This is likely the most important on the road to being able to
## generate AST allowing for rapidly testing languages features working in
## combinations with each other. This should allow for exacting documentation
## of a spec.
##
## Properties: something that provides some simple combinators that allow for:
## `"some property is true" given arb1 & arb2 & arb3 when somePredicate(a,b,c)`
## Where we have a nice textual description of a spec, declaration of
## assumptions, and the predicate. More complex expressions such as given a
## circumstance (textual + given), many properties predicates can be checked
## including introducing further givens specific to the branch of properties.
## To provide a cleaner API in most circumstances, an API that can take a
## subset of `typedesc`s with associated default arbitraries would remove a lot
## of noise with good defaults.
## 
## Core Evolution:
## ---------------
## Evolving the core has a number of areas, the most pressing:
## 1. establishing a strong base of default arbitraries
## 2. shrinking support
## 3. replay failed path in run
## 
## Default Arbitraries: a strong base is required to feed NimNode generation so
## valid ASTs can be generated quickly.
## 
## Shrinking: automatically generated programs will often contain a lot of
## noise, shrinking can do much to provide small failure demonstrating
## scenarios.
## 
## Replay: when you can generate tests, test suites start taking longer very
## quickly. This is of course a good thing, it's a relfection of the lowered
## cost of rapidly exploring large areas of the input space. Being able to
## re-run only a single failing run that otherwise only shows up towards the
## end of a test battery quickly becomes important.
## 
## Heavily inspired by the excellent
## [Fast Check library](https://github.com/dubzzz/fast-check).
## 
## Concepts:
## * predicate - a function which given a value indicates true or false
## * arbitrary - generator of arbitrary value for some set of values
## * property - a condition a value must hold to, given a predicate
## * run - test a single value against a property
## 
## Future directions:
## * properties with predefined examples -- not purely random
## * before and after run hooks for properties
## * support for multiple random number generators
## * optimise arbitraries for Map/Filter/etc via variants, but allow extension
## * distribution control
## * model based checking
## * async testing
## * shrinking

type
  PTStatus* = enum
    ## the result of a single run/predicate check
    ## XXX: likely to be changed to a variant to support, true/false/error+data
    ptPreCondFail,
    ptFail,
    ptPass

  RunId* = range[1..high(int)]
    ## sequential id of the run, starts from 1
  
  PossibleRunId* = int
    ## separate from `RunId` to support 0 value, indicating non-specified
  
  PropCheck*[T] = proc(s: T): PTStatus {.noSideEffect.}
    ## test function to see if a property holds
  
  Random* = object
    ## random number generator, allows abstraction over algorithm
    seed: uint32
    rng: MersenneTwister # TODO: this should not be hard coded
    calls: uint          ## number of calls
  
  ArbitraryKind = enum
    akLarge,     ## infeasilbe to generate all possible values (most cases)
    akExhaustive ## possible to generate all values (bool, enums, 8 bit ints)

  Arbitrary*[T] = object
    ## arbitrary value generator for some type T
    ## XXX: eventually migrate to concepts once they're more stable, but
    ##      language stability is the big reason for making this whole property
    ##      based testing framework. :D
    mgenerate: proc(a: Arbitrary[T], mrng: var Random): Shrinkable[T]
    case kind: ArbitraryKind # XXX: setup support for exhaustive kinds
    of akExhaustive:
      size: uint8
    of akLarge:
      discard

  Shrinkable*[T] = object
    ## future support for shrinking
    # xxx: maybe we should get rid of `Shrinkable`, make it part of the
    #      generator, but that might not work when map/filtering/etc
    value*: T

  Property*[T] = object
    ## a condition that must hold for an arbitrary as specified by a predicate
    arb: Arbitrary[T]
    predicate: PropCheck[T]

#-- Run Id

const noRunId* = 0.PossibleRunId

proc isUnspecified*(r: PossibleRunId): bool =
  ## used for default param handling
  result = r.uint == 0

proc newRun*(): RunId = 1.RunId

proc startRun(r: var RunId): RunId {.discardable, inline.} =
  ## marks the current run as complete and returns the preivous RunId
  result = r
  inc r

proc startRun*(r: var PossibleRunId) {.inline.} =
  inc r

#-- Random

func getCallCount*(r: Random): uint {.inline.} =
  r.calls

#-- Shrinkable

# These seem redundant with Arbitraries, this is mostly for convenience. The
# main reason is that these represent map/filter/etc over a singular shrinkable
# valid value -- which might need particular care. The convenience is when we
# actually implement shrinking and distinguishing specific valid instance vs
# intermediate values an Arbitrary might generate along the way to generating a
# valid value are not the same thing.

proc map[T, U](s: Shrinkable[T], mapper: proc(t: T): U): Shrinkable[U] =
  result = Shrinkable[U](value: mapper(s.value))

proc filter[T](s: Shrinkable[T], predicate: proc(t: T): bool): Option[T] =
  result = if predicate(s.value): some(s.value) else: none[T]()

proc shrinkableOf[T](v: T): Shrinkable[T] =
  result = Shrinkable[T](value: v)

proc shrinkableOf[T](v: var T): var Shrinkable[T] =
  result = Shrinkable[T](value: v)

#-- Arbitrary

proc generate*[T](a: Arbitrary[T], mrng: var Random): Shrinkable[T] =
  ## calls the internal implementation
  a.mgenerate(a, mrng)

proc map*[T,U](o: Arbitrary[T], mapper: proc(t: T): U): Arbitrary[U] =
  ## creates a new Arbitrary with mapped values
  var orig = o
  let
    mgenerate = proc(a: Arbitrary[U], mrng: var Random): Shrinkable[U] =
                  result = orig.generate(mrng).map(mapper)

  return Arbitrary[U](mgenerate: mgenerate)

proc filter*[T](o: Arbitrary[T], predicate: proc(t: T): bool): Arbitrary[T] =
  ## creates a new Arbitrary with filtered values, aggressive filters can lead
  ## to exhausted values.
  var orig = o
  let
    mgenerate = proc(a: Arbitrary[T], mrng: var Random): Shrinkable[T] =
                  var g = orig.generate(mrng)
                  while g.filter(predicate).isNone:
                    g = a.generate(mrng)
                  result = g

  return Arbitrary[T](mgenerate: mgenerate)

proc flatMap[T, U](s: Arbitrary[T],
                   fmapper: proc(t: T): Arbitrary[U]): Arbitrary[U] =
  ## creates a new Arbitrary for every value produced by `s`. For when you want
  ## to make the value of an Arbitrary depend upon the value of another.
  var orig = s
  let
    mgenerate = proc(a: Arbitrary[U], mrng: var Random): Shrinkable[U] =
                  result = fmapper(orig.generate(mrng).value).generate(mrng)
  return Arbitrary[U](mgenerate: mgenerate)

proc take*[T](a: Arbitrary[T], n: uint, mrng: var Random): Shrinkable[seq[T]] =
  ## generates a sequence of values meant to be used collectively
  var rng = mrng
  result = shrinkableOf(newSeqOfCap[T](n))
  for i in 0..<n:
    result.value.add a.generate(rng).value
  mrng = rng

proc sample*[T](a: Arbitrary[T], n: uint, mrng: var Random): seq[Shrinkable[T]] =
  ## generate a sequence of values meant to be used individually
  var rng = mrng
  result = newSeqOfCap[Shrinkable[T]](n)
  for i in 0..<n:
    result.add a.generate(rng)
  mrng = rng

#-- Random Number Generation
# XXX: the trick with rngs is that the number of calls to them matter, so we'll
#      have to start tracking number of calls in between arbitrary generation
#      and other such things (well beyond just the seed) in order to quickly
#      reproduce a failure. Additionally, different psuedo random number
#      generation schemes are required because they have various distribution
#      and performance characteristics which quickly become relevant at scale.
proc newRandom*(seed: uint32 = 0): Random =
  Random(seed: seed, rng: newMersenneTwister(seed))

proc nextUint32(r: var Random): uint32 =
  inc r.calls
  result = r.rng.getNum()

proc nextInt(r: var Random): int =
  inc r.calls
  result = cast[int32](r.rng.getNum())

proc nextUint32(r: var Random; min, max: uint32): uint32 =
  assert min < max, "max must be greater than min"
  let size = max - min
  result = min + (r.nextUint32() mod size)

proc nextChar(r: var Random; min, max: char): char =
  assert min < max, "max must be greater than min"
  let size = uint8(max) - uint8(min)
  result = char(uint8(min) + uint8(r.nextUint32() mod size))

proc nextInt(r: var Random; min, max: int): int =
  assert min < max, "max must be greater than min"
  let size = abs(max - min)
  result = min + abs(r.nextInt() mod size)

#-- Property

converter toPTStatus*(b: bool): PTStatus =
  ## yes, converters are evil, but in this case they're incredibly helpful
  ## XXX: does this need to be exported?
  if b: ptPass else: ptFail

proc newProperty*[T](arb: Arbitrary[T], p: PropCheck): Property[T] =
  result = Property[T](arb: arb, predicate: p)

proc toss(mrng: var Random) {.inline.} =
  ## skips 42 numbers to introduce noise between generate calls, think toss as
  ## in tossing dice
  for _ in 0..41:
    discard mrng.nextInt()

proc generateAux[T](p: var Property[T], rng: Random,
                    r: PossibleRunId): Shrinkable[T] =
  var mrng = rng
  toss(mrng)
  result = p.arb.generate(mrng)

proc generate*[T](p: var Property[T], mrng: Random, runId: RunId): Shrinkable[T] =
  return generateAux(p, mrng, runId)

proc generate*[T](p: Property[T], mrng: Random): Shrinkable[T] =
  return generateAux(p, mrng, noRunId)

proc run*[T](p: Property[T], v: T): PTStatus =
  try:
    result = p.predicate(v)
  except:
    # XXX: do some exception related checking here, for now pass through
    raise getCurrentException()
  finally:
    # XXX: for hooks
    discard

#-- Basic Arbitraries
# these are so you can actually test a thing

proc tupleArb*[A](a1: Arbitrary[A]): Arbitrary[(A,)] =
  ## Arbitrary of single-value tuple
  result = Arbitrary[(A,)](
    mgenerate: proc(arb: Arbitrary[(A,)], rng: var Random): Shrinkable[(A,)] =
                  shrinkableOf((a1.generate(rng).value,))
  )

proc tupleArb*[A,B](a1: Arbitrary[A], a2: Arbitrary[B]): Arbitrary[(A,B)] =
  ## Arbitrary of pair tuple
  var
    o1 = a1
    o2 = a2
  result = Arbitrary[(A,B)](
    mgenerate: proc(a: Arbitrary[(A,B)], rng: var Random): Shrinkable[(A,B)] =
                  shrinkableOf(
                    (o1.generate(rng).value, o2.generate(rng).value)
                  )
  )

proc intArb*(): Arbitrary[int] =
  result = Arbitrary[int](
    mgenerate: proc(arb: Arbitrary[int], rng: var Random): Shrinkable[int] =
                  shrinkableOf(rng.nextInt())
  )

proc intArb*(min, max: int): Arbitrary[int] =
  ## create a int arbitrary with values in the range of min and max which are
  ## inclusive.
  result = Arbitrary[int](
    mgenerate: proc(arb: Arbitrary[int], rng: var Random): Shrinkable[int] =
                  shrinkableOf(rng.nextInt(min, max))
  )

proc uint32Arb*(): Arbitrary[uint32] =
  result = Arbitrary[uint32](
    mgenerate: proc(arb: Arbitrary[uint32], rng: var Random): Shrinkable[uint32] =
                  shrinkableOf(rng.nextUint32())
  )

proc uint32Arb*(min, max: uint32): Arbitrary[uint32] =
  ## create a uint32 arbitrary with values in the range of min and max which
  ## are inclusive.
  result = Arbitrary[uint32](
    mgenerate: proc(arb: Arbitrary[uint32], rng: var Random): Shrinkable[uint32] =
                  shrinkableOf(rng.nextUint32(min, max))
  )

proc swapAccess[T](s: var openArray[T], a, b: int): T =
  ## swap the value at position `a` for position `b`, then return the new value
  ## at position `a`. Used for exhaustive arbitrary traversal.
  result = s[b]

  if a != b:      # only need to swap if they're different
    s[b] = s[a]
    s[a] = result

proc charArb*(min, max: char): Arbitrary[char] =
  ## create a char arbitrary for a given range
  var
    vals = toSeq(min..max)
    pos: int = 0
  result = Arbitrary[char](
    kind: akExhaustive,
    size: high(uint8),
    mgenerate: proc(arb: Arbitrary[char], rng: var Random): Shrinkable[char] =
                  let endPos = vals.len - 1
                  if pos < endPos:
                    let
                      atEnd = pos == endPos
                      swapPos = if atEnd: endPos
                                else: rng.nextInt(pos, endPos)
                    result = shrinkableOf(vals.swapAccess(pos, swapPos))
                    inc pos
                  else:
                    result = shrinkableOf(rng.nextChar(min, max))
  )

proc charArb*(): Arbitrary[char] {.inline.} =
  ## create a char arbitrary for the full character range, see: `charAsciiArb`
  charArb(char.low, char.high)

proc charAsciiArb*(): Arbitrary[char] {.inline.} =
  ## create an ascii char arbitrary
  charArb(char.low, chr(127))

proc seqArbOf*[T](a: Arbitrary[T], min: uint32 = 0, max: uint32 = 100): Arbitrary[seq[T]] =
  ## create a sequence of varying size of some type
  assert min <= max
  result = uint32Arb(min, max).map((i) => a.take(i))

proc stringArb*(min: uint32 = 0, max: uint32 = 1000, charArb = charArb()): Arbitrary[string] =
  ## create strings using the full character range with len of `min` to `max`
  ## see: `stringAsciiArb`
  result = Arbitrary[string](
    mgenerate: proc(a: Arbitrary[string], mrng: var Random): Shrinkable[string] =
                 let size = mrng.nextUint32(min, max)
                 charArb.take(size, mrng).map((cs) => cs.join())
  )

proc stringAsciiArb*(min: uint32 = 0, max: uint32 = 1000): Arbitrary[string] {.inline.} =
  ## create strings using the ascii character range with len of `min` to `max`
  stringArb(min, max, charAsciiArb())

proc enumArb*[T: enum](): Arbitrary[T] =
  # XXX: use a uint32 arb to get a value between the current pos and end of seq, then swap access over that
  var
    vals = toSeq(T.low..T.high)
    pos: int = 0
  let length = enumLen(T)
  if length < int(high(uint8)):
    result = Arbitrary[T](
      kind: akExhaustive,
      size: uint8(length)
    )
  else:
    result = Arbitrary[T](
      kind: akLarge,
    )
  result.mgenerate = 
    proc(arb: Arbitrary[T], rng: var Random): Shrinkable[T] =
      let
        endPos = max(0, vals.len - 1)
        atEnd = pos == endPos
        swapPos = if atEnd: endPos
                  else: rng.nextInt(pos, endPos)
      result = shrinkableOf(vals.swapAccess(pos, swapPos))
      inc pos
      if pos == endPos:
        pos = 0

proc setArb*[T: enum](exclude: set[T] = {}): Arbitrary[set[T]] =
  result = Arbitrary[set[T]](
    kind: akLarge,
    mgenerate: proc(arb: Arbitrary[set[T]], rng: var Random): Shrinkable[set[T]] =
                  let size = rng.nextUint32(0, enumLen(T))
                  enumArb[T]().filter(i => i notin exclude)
                              .take(size, rng)
                              .map(es => (var s: set[T]; for e in es: incl(s, e); s))
  )

proc constArb*[T: not void](v: T): Arbitrary[T] =
  ## creates an arbitrary that produces the same value over and over again
  result = Arbitrary[T](
    kind: akExhaustive,
    size: 1,
    mgenerate: proc(arb: Arbitrary[T], rng: var Random): Shrinkable[T] =
                  result = shrinkableOf(v)
  )

proc nimNodeArb*(): Arbitrary[NimNode] =
  # XXX: what is even going on?
  result = enumArb[NimNodeKind]()
            .filter(k => k notin {nnkError, nnkSym, nnkType, nnkIdent})
            .map(k => newNimNode(k))