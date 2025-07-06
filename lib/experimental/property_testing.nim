# these modules are used extensively
import std/macros
import std/mersenne
import std/options # need this for counter examples

# these modules have limited use, so be selective
from std/math import floor, log10, clamp
from std/strformat import fmt
from std/strutils import join, repeat
from std/sugar import `=>` # XXX: maybe a bust because inference can't keep up
from std/sequtils import toSeq, apply
from std/times import toUnix, getTime

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
## noise, a shrinking can do much to provide small failure demonstrating
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
    kind: ArbitraryKind # XXX: setup support for exhaustive kinds

  Shrinkable*[T] = object
    ## future support for shrinking
    value: T

  Property*[T] = object
    ## a condition that must hold for an arbitrary as specified by a predicate
    arb: Arbitrary[T]
    predicate: PropCheck[T]
  
  Frequency* = int
    ## future use to allow specification of biased generation

#-- Run Id

const noRunId = 0.PossibleRunId

proc isUnspecified*(r: PossibleRunId): bool =
  ## used for default param handling
  result = r.uint == 0

proc newRun(): RunId = 1.RunId

proc startRun(r: var RunId): RunId {.discardable, inline.} =
  ## marks the current run as complete and returns the preivous RunId
  result = r
  inc r

proc startRun(r: var PossibleRunId) {.inline.} =
  inc r

proc runIdToFrequency(r: RunId): int =
  2 + toInt(floor(log10(r.int.float)))

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
                  var g = a.generate(mrng)
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
proc newRandom(seed: uint32 = 0): Random =
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

proc nextInt(r: var Random; min, max: int): int =
  assert min < max, "max must be greater than min"
  let size = abs(max - min)
  result = min + abs(r.nextInt() mod size)

#-- Property

converter toPTStatus(b: bool): PTStatus =
  ## yes, converters are evil, but in this case they're incredibly helpful
  ## XXX: does this need to be exported?
  if b: ptPass else: ptFail

proc newProperty*[T](arb: Arbitrary[T], p: PropCheck): Property[T] =
  result = Property[T](arb: arb, predicate: p)

proc withBias[T](arb: var Arbitrary[T], f: Frequency): var Arbitrary[T] =
  ## create an arbitrary with bias
  ## XXX: implement biasing
  return arb

proc toss(mrng: var Random) {.inline.} =
  ## skips 42 numbers to introduce noise between generate calls, think toss as
  ## in tossing dice
  for _ in 0..41:
    discard mrng.nextInt()

proc generateAux[T](p: var Property[T], rng: Random,
                    r: PossibleRunId): Shrinkable[T] =
  var mrng = rng
  toss(mrng)
  result =
    if r.isUnspecified():
      p.arb.generate(mrng)
    else:
      p.arb.withBias(runIdToFrequency(r)).generate(mrng)

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
    mgenerate: proc(arb: Arbitrary[char], rng: var Random): Shrinkable[char] =
                  let
                    endPos = vals.len - 1
                    atEnd = pos == endPos
                    swapPos = if atEnd: endPos
                              else: rng.nextInt(pos, endPos)
                  result = shrinkableOf(vals.swapAccess(pos, swapPos))
                  inc pos
                  if pos == endPos:
                    pos = 0
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
  result = Arbitrary[T](
    kind: akExhaustive,
    mgenerate: proc(arb: Arbitrary[T], rng: var Random): Shrinkable[T] =
                  let
                    endPos = max(0, vals.len - 1)
                    atEnd = pos == endPos
                    swapPos = if atEnd: endPos
                              else: rng.nextInt(pos, endPos)
                  result = shrinkableOf(vals.swapAccess(pos, swapPos))
                  inc pos
                  if pos == endPos:
                    pos = 0
  )

proc constArb*[T](v: T): Arbitrary[T] =
  ## creates an arbitrary that produces the same value over and over again
  result = Arbitrary[T](
    kind: akExhaustive,
    mgenerate: proc(arb: Arbitrary[T], rng: var Random): Shrinkable[T] =
                  result = shrinkableOf(v)
  )

proc nimNodeArb*(): Arbitrary[NimNode] =
  # XXX: what is even going on?
  result = enumArb[NimNodeKind]()
            .filter(k => k notin {nnkError, nnkSym, nnkType, nnkIdent})
            .map(k => newNimNode(k))

#-- Assert Property Reporting

type
  RunExecution[T] = object
    ## result of run execution
    # XXX: move to using this rather than open state in `execProperty` procs
    # XXX: lots to do to finish this:
    #      * save necessary state for quick reproduction (path, etc)
    #      * support async and streaming (iterator? CPS? magical other thing?)
    runId: uint32
    failureOn: PossibleRunId
    seed: uint32
    counterExample: Option[T]

  GlobalContext* = object
    hasFailure: bool
    specNames: seq[string]
    # compileTime: bool      ## are we executing the property at compile time
    # ctOutput: string       ## the output generated

  AssertReport*[T] = object
    ## result of a property assertion, with all runs information
    # XXX: don't need counter example and generic param here once
    #      `RunExecution` is being used.
    name: string
    runId: PossibleRunId
    failures: uint32
    firstFailure: PossibleRunId
    failureType: PTStatus
    seed: uint32
    counterExample: Option[T]

proc startRun[T](r: var AssertReport[T]) {.inline.} =
  r.runId.startRun()

proc recordFailure*[T](r: var AssertReport[T], example: T,
                      ft: PTStatus) =
  ## records the failure in the report, and notes first failure and associated
  ## counter-example as necessary
  assert ft in {ptFail, ptPreCondFail}, fmt"invalid failure status: {ft}"
  if r.firstFailure.isUnspecified():
    r.firstFailure = r.runId
    r.counterExample = some(example)
  inc r.failures
  when defined(debug):
    let exampleStr = $example.get() # XXX: handle non-stringable stuff
    echo fmt"Fail({r.runId}): {ft} - {exampleStr}"

proc hasFailure*(r: AssertReport): bool =
  result = not r.firstFailure.isUnspecified()

proc isSuccessful*(r: AssertReport): bool =
  result = r.firstFailure.isUnspecified()

proc `$`*[T](r: AssertReport[T]): string =
  # XXX: make this less ugly
  let status =
    if r.hasFailure:
      fmt"failures: {r.failures}, firstFailure: {r.firstFailure}, firstFailureType: {r.failureType}, counter-example: {r.counterExample}, seed: {r.seed}"
    else:
      "status: success"

  result = fmt"{r.name} - {status}, totalRuns: {r.runId.int}"

proc startReport[T](name: string, seed: uint32): AssertReport[T] =
  ## start a new report
  result = AssertReport[T](name: name, runId: noRunId, failures: 0, seed: seed,
                        firstFailure: noRunId, counterExample: none[T]())

#-- Assert Properties

type
  AssertParams* = object
    ## parameters for asserting properties
    # XXX: add more params to control tests, eg:
    #      * `examples` as a seq[T], for default values
    seed*: uint32
    random*: Random
    runsBeforeSuccess*: range[1..high(int)]

proc timeToUint32(): uint32 {.inline.} =
  when nimvm:
    # XXX: can't access time in the VM, figure out another way
    0
  else:
    cast[uint32](clamp(toUnix(getTime()), 0'i64, uint32.high.int64))
  

proc defAssertPropParams(): AssertParams =
  ## default params used for an `execProperty`
  let seed: uint32 = timeToUint32()
  result = AssertParams(seed: seed, random: newRandom(seed),
                        runsBeforeSuccess: 1000)

proc indent(ctx: GlobalContext): string =
  '\t'.repeat(max(ctx.specNames.len - 2, 0))

proc ctxEcho(ctx: GlobalContext, msg: string) =
  echo ctx.indent, msg

proc reportSuccess(ctx: GlobalContext, msg: string) =
  ## XXX: do better reporting
  ctx.ctxEcho "- " & msg

proc reportFailure(ctx: var GlobalContext, msg: string) =
  ## XXX: do better reporting
  ctx.hasFailure = true
  ctx.ctxEcho "- " & msg

proc execProperty*[A](
  ctx: var GlobalContext,
  name: string,
  arb: Arbitrary[A],
  pred: PropCheck[A],
  params: AssertParams = defAssertPropParams()): AssertReport[A] =

  result = startReport[A](name, params.seed)
  var
    rng = params.random # XXX: need a var version
    p = newProperty(arb, pred)

  while(result.runId < params.runsBeforeSuccess):
    result.startRun()
    let
      s: Shrinkable[A] = p.generate(rng, result.runId)
      r = p.run(s.value)
      didSucceed = r notin {ptFail, ptPreCondFail}
    
    if not didSucceed:
      result.recordFailure(s.value, r)
  
  if result.hasFailure:
    ctx.reportFailure($result)
  else:
    ctx.reportSuccess($result)

proc execProperty*[A, B](
  ctx: var GlobalContext,
  name: string,
  arb1: Arbitrary[A], arb2: Arbitrary[B],
  pred: PropCheck[(A, B)],
  params: AssertParams = defAssertPropParams()): AssertReport[(A,B)] =

  result = startReport[(A, B)](name, params.seed)
  var
    rng = params.random # XXX: need a var version
    arb = tupleArb[A,B](arb1, arb2)
    p = newProperty(arb, pred)

  while(result.runId < params.runsBeforeSuccess):
    result.startRun()
    let
      s: Shrinkable[(A,B)] = p.generate(rng, result.runId)
      r = p.run(s.value)
      didSucceed = r notin {ptFail, ptPreCondFail}
    
    if not didSucceed:
      result.recordFailure(s.value, r)
  
  if result.hasFailure:
    ctx.reportFailure($result)
  else:
    ctx.reportSuccess($result)

proc execProperty*[A, B, C](
  ctx: var GlobalContext,
  name: string,
  arb1: Arbitrary[A], arb2: Arbitrary[B], arb3: Arbitrary[C],
  pred: PropCheck[(A, B, C)],
  params: AssertParams = defAssertPropParams()): AssertReport[(A,B,C)] =

  result = startReport[(A, B, C)](name, params.seed)
  var
    rng = params.random # XXX: need a var version
    arb = tupleArb[A,B,C](arb1, arb2, arb3)
    p = newProperty(arb, pred)

  while(result.runId < params.runsBeforeSuccess):
    result.startRun()
    let
      s: Shrinkable[(A,B,C)] = p.generate(rng, result.runId)
      r = p.run(s.value)
      didSucceed = r notin {ptFail, ptPreCondFail}
    
    if not didSucceed:
      result.recordFailure(s.value, r)
  
  if result.hasFailure:
    ctx.reportFailure($result)
  else:
    ctx.reportSuccess($result)

#-- API

proc name(ctx: GlobalContext): string =
  if ctx.specNames.len > 0: ctx.specNames[0] else: ""

proc startInnerSpec(ctx: var GlobalContext, name: string) =
  ctx.specNames.add(name)
  ctx.ctxEcho name

proc stopInnerSpec(ctx: var GlobalContext) =
  discard ctx.specNames.pop
  echo "" # empty line to break up the spec

template specAux(globalCtx: var GlobalContext, body: untyped): untyped =
  block:
    {.push hint[XDeclaredButNotUsed]: off.}

    template forAll[A](
        name: string = "",
        arb1: Arbitrary[A],
        pred: PropCheck[A] # XXX: move the predicate decl inline
        ) =
      discard execProperty(globalCtx, name, arb1, pred, defAssertPropParams())
    
    template forAll[A,B](
        name: string = "",
        arb1: Arbitrary[A], arb2: Arbitrary[B],
        pred: PropCheck[(A, B)] # XXX: move the predicate decl inline
        ) =
      discard execProperty(globalCtx, name, arb1, arb2, pred,
                           defAssertPropParams())
    
    template forAll[A,B,C](
        name: string = "",
        arb1: Arbitrary[A], arb2: Arbitrary[B], arb3: Arbitrary[C],
        pred: PropCheck[(A, B, C)] # XXX: move the predicate decl inline
        ) =
      discard execProperty(globalCtx, name, arb1, arb2, arb3, pred,
                           defAssertPropParams())

    {.pop.}

    template spec(name: string = "", b: untyped): untyped =
      globalCtx.startInnerSpec(name)
      block:
        b
      globalCtx.stopInnerSpec()

    if globalCtx.specNames.len > 0:
      echo globalCtx.name, "\n"

    body
    globalCtx

template spec*(n: string = "", body: untyped): untyped =
  var globalCtx = GlobalContext(hasFailure: false,
                                specNames: if n.len > 0: @[n] else: @[])
  discard specAux(globalCtx, body)

  if globalCtx.hasFailure:
    echo "Failed"
    quit(QuitFailure)
  else:
    echo "Success"
    quit(QuitSuccess)

#-- Hackish Tests

when isMainModule:
  from macros import NimNodeKind

  spec "nim":
    spec "uint32":
      forAll("are >= 0, yes it's silly ", uint32Arb(),
             proc(i: uint32): PTStatus = i >= 0)

      const
        min: uint32 = 100000000
        max = high(uint32)
      forAll(fmt"within the range[{min}, {max}]", uint32Arb(min, max),
             func(i: uint32): PTStatus = i >= min and i <= max)

    spec "enums":
      forAll("are typically ordinals", enumArb[NimNodeKind](),
             func(n: NimNodeKind): PTStatus =
               n > NimNodeKind.low  or n == NimNodeKind.low or
               n < NimNodeKind.high or n == NimNodeKind.high
            )

    spec "characters":
      spec "are ordinals":
        forAll("forming a bijection with int values between 0..255 (inclusive)",
               charArb(),
               func(c: char): PTStatus =
                 c == chr(ord(c)) and ord(c) >= 0 and ord(c) <= 255)

        block:
          let gen = proc(c: char): (char, char, char) =
            let
              prev = if c == low(char): c else: pred(c)
              curr = c
              next = if c == high(char): c else: succ(c)
            (prev, curr, next)
          forAll("have successors and predecessors or are at the end range",
                 charArb().map(gen),
                 func(cs: (char, char, char)): PTStatus =
                   let (a, b, c) = cs
                   (a < b and b < c) or (a <= b and b < c) or (a < b and b <= c))
      forAll("ascii - are from 0 to 127",
             charAsciiArb(),
             func(c: char): PTStatus =
               c.ord >= 0 or c.ord <= 127)

    spec "strings":
      forAll("concatenation - len is >= the sum of the len of the parts",
             stringArb(), stringArb(),
             func(ss: (string, string)): PTStatus =
               let (a, b) = ss
               a.len + b.len <= (a & b).len)

  # block:
    # XXX: this tests the failure branch but isn't running right now
    # test failure at the end because the assert exits early
    # let foo = func(t: ((uint32, uint32))): PTStatus =
    #             let (a, b) = t
    #             case a + b > a
    #             of true: ptPass
    #             of false: ptFail
    # forAll("classic math assumption should fail", uint32Arb(), uint32Arb(), foo)

#-- Macro approach, need to revisit

when false:
  # XXX: need to make these work, they move into the library part
  proc initArbitrary[T: tuple]: Arbitrary[T] =
    # Temporary procedure we need to figure out how to make for *all* types
    let size = 100u32
    result = Arbitrary[T](
      mgenerate: proc(arb: Arbitrary[T], rng: var Random): Shrinkable[T] =
        var a = default T
        for field in a.fields:
          field = type(field)(rng.nextUint32() mod size)
    )

  macro execProperty*(name: string, values: varargs[typed],
                        params = defAssertPropParams(), body: untyped): untyped =
    ## Generates and runs a property. Currently this auto-generates parameter
    ## names from a to z based on the tuple width -- 26 parameters is good enough
    ## for now.
    # XXX: do we want to make the parameter naming explicit?
    var tupleTyp = nnkTupleConstr.newTree()
    let
      isTuple = values.kind == nnkBracket and values[0].kind == nnkTupleConstr
      values = if isTuple: values[0] else: values
      possibleIdents = {'a'..'z'}.toSeq
      idents = block: # Generate the tuple, and the name unpack varaibles
        var
          idents: seq[NimNode]
        for i, x in values:
          let retT = x[0].getImpl[3][0][1]
          idents.add ident($possibleIdents[i])
          tupleTyp.add retT
        idents

    # make the `let (a, b ...) = input`
    let unpackNode = nnkLetSection.newTree(nnkVarTuple.newTree(idents))  
    unpackNode[0].add newEmptyNode(), ident"input"
    
    body.insert 0, unpackNode # add unpacking to the first step

    result = newStmtList()
    result.add newProc(ident"test",
                      [
                        ident"PTStatus",
                        newIdentDefs(ident"input", tupleTyp, newEmptyNode())
                      ],
                      body) # Emit the proc
    result.add quote do:
      var
        arb = initArbitrary[`tupleTyp`]()
        report = startReport[`tupleTyp`](`name`)
        rng = `params`.random
        p = newProperty(arb, test)
      while report.runId < `params`.runsBeforeSuccess:
        report.startRun()
        let
          s: Shrinkable[`tupleTyp`] = p.generate(rng, report.runId)
          r: PTStatus = p.run(s.value)
          didSucceed = r notin {ptFail, ptPreCondFail}
        
        if not didSucceed:
          report.recordFailure(s.value, r)
    
      # XXX: useful for debugging the macro code
      # echo result.repr

      echo report
      if report.hasFailure:
        doAssert report.isSuccessful, $report