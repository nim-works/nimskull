# these modules are used extensively
import std/macros
import std/options # need this for counter examples

import ./cmdline
import ./cmdline/parsers
from std/sugar import `=>`

# these modules have limited use, so be selective
from std/strformat import fmt
from std/strutils import join, repeat
from std/sequtils import toSeq, apply, mapIt
from std/times import toUnix, getTime
from std/typetraits import enumLen

import ./property_testing_core

#-- Assert Property Reporting

type
  RunMode = enum
    rmTest = "test"
    rmOutline = "outline"

  Args = object
    runMode: RunMode

  GlobalContext* = object
    hasFailure: bool
    specNames: seq[string]
    specCounters: seq[uint]
    propCounter: uint32
    # compileTime: bool      ## are we executing the property at compile time
    # ctOutput: string       ## the output generated

  AssertReport*[T] = object
    ## result of a property assertion, with all runs information
    id: uint32
    name: string
    runId: PossibleRunId
    failures: uint32
    firstFailure: PossibleRunId
    failureType: PTStatus
    seed: uint32
    failStartRngCallCount: uint
    counterExample: Option[T]

proc startRun[T](r: var AssertReport[T], rng: Random) {.inline.} =
  r.runId.startRun()
  if r.firstFailure == noRunId:
    r.failStartRngCallCount = rng.getCallCount()


proc recordFailure*[T](r: var AssertReport[T], example: T,
                      ft: PTStatus) =
  ## records the failure in the report, and notes first failure and associated
  ## counter-example as necessary
  assert ft in {ptFail, ptPreCondFail}, fmt"invalid failure status: {ft}"
  r.failureType = ft
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
      fmt"failures: {r.failures}, firstFailure: {r.firstFailure}, firstFailureType: {r.failureType}, counter-example: {r.counterExample}, seed: {r.seed}, rng-skip: {r.failStartRngCallCount}"
    else:
      "status: success"

  result = fmt"{r.name} (id: {r.id}) - {status}, totalRuns: {r.runId.int}"

proc startReport[T](id: uint32, name: string, seed: uint32): AssertReport[T] =
  ## start a new report
  result = AssertReport[T](id: id, name: name, runId: noRunId,
                        failures: 0, seed: seed, failureType: ptPass,
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

proc ctxEcho(ctx: GlobalContext, msg: varargs[string]) =
  echo ctx.indent, msg.join(" ")

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
  propCheck: PropCheck[A],
  params: AssertParams = defAssertPropParams()): AssertReport[A] =

  inc ctx.propCounter
  result = startReport[A](ctx.propCounter, name, params.seed)
  var
    rng = params.random # XXX: need a var version
    p = newProperty(arb, propCheck)

  while(result.runId < params.runsBeforeSuccess):
    result.startRun(rng)
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
    a: Arbitrary[A], b: Arbitrary[B],
    propCheck: proc(a: A, b: B): PTStatus {.noSideEffect.},
    params: AssertParams = defAssertPropParams()): AssertReport[(A, B)] =
  execProperty(ctx, name, tupleArb[A, B](a, b),
    proc(t: (A, B)): PTStatus =
      propCheck(t[0], t[1])
    , params)

proc execProperty*[A, B, C](
    ctx: var GlobalContext,
    name: string,
    a: Arbitrary[A], b: Arbitrary[B], c: Arbitrary[C],
    propCheck: proc(a: A, b: B, c: C): PTStatus {.noSideEffect.},
    params: AssertParams = defAssertPropParams()): AssertReport[(A, B, C)] =
  execProperty(ctx, name, tupleArb[A, B, C](a, b, c),
    proc(t: (A, B, C)): PTStatus =
      propCheck(t[0], t[1], t[2])
    , params)

proc execProperty*[A, B, C, D](
    ctx: var GlobalContext,
    name: string,
    a: Arbitrary[A], b: Arbitrary[B], c: Arbitrary[C], d: Arbitrary[D],
    propCheck: proc(a: A, b: B, c: C, d: D): PTStatus {.noSideEffect.},
    params: AssertParams = defAssertPropParams()): AssertReport[(A, B, C, D)] =
  execProperty(ctx, name, tupleArb[A, B, C, D](a, b, c, d),
    proc(t: (A, B, C, D)): PTStatus =
      propCheck(t[0], t[1], t[2], t[3])
    , params)

proc execProperty*[A, B, C, D, E](
    ctx: var GlobalContext,
    name: string,
    a: Arbitrary[A], b: Arbitrary[B], c: Arbitrary[C], d: Arbitrary[D], e: Arbitrary[E],
    propCheck: proc(a: A, b: B, c: C, d: D, e: E): PTStatus {.noSideEffect.},
    params: AssertParams = defAssertPropParams()): AssertReport[(A, B, C, D, E)] =
  execProperty(ctx, name, tupleArb[A, B, C, D, E](a, b, c, d, e),
    proc(t: (A, B, C, D, E)): PTStatus =
      propCheck(t[0], t[1], t[2], t[3], t[4])
    , params)

#-- API

proc name(ctx: GlobalContext): string =
  if ctx.specNames.len > 0: ctx.specNames[0] else: ""

proc startInnerSpec(ctx: var GlobalContext, name: string) =
  ctx.specNames.add(name)
  if ctx.specNames.len == ctx.specCounters.len:
    inc ctx.specCounters[^1]
  elif ctx.specNames.len > ctx.specCounters.len:
    ctx.specCounters.add(1)
  else:
    while ctx.specCounters.len != ctx.specNames.len:
      # shrink down to the same length
      discard ctx.specCounters.pop
    inc ctx.specCounters[^1]
  ctx.ctxEcho ctx.specCounters.mapIt($it).join("."), name

proc stopInnerSpec(ctx: var GlobalContext) =
  discard ctx.specNames.pop
  echo "" # empty line to break up the spec

template specAux(globalCtx: var GlobalContext, body: untyped): untyped =
  block:
    {.push hint[XDeclaredButNotUsed]: off.}

    template forAll[A](
        name: string = "",
        arb1: Arbitrary[A],
        propCheck: proc(a: A): PTStatus {.noSideEffect.}
        ) =
      discard execProperty(globalCtx, name, arb1, propCheck, defAssertPropParams())
    
    template forAll[A,B](
        name: string = "",
        arb1: Arbitrary[A], arb2: Arbitrary[B],
        propCheck: proc(a: A, b: B): PTStatus {.noSideEffect.}
        ) =
      discard execProperty(globalCtx, name, arb1, arb2, propCheck,
                           defAssertPropParams())
    
    template forAll[A,B,C](
        name: string = "",
        arb1: Arbitrary[A], arb2: Arbitrary[B], arb3: Arbitrary[C],
        propCheck: proc(a: A, b: B, c: C): PTStatus {.noSideEffect.}
        ) =
      discard execProperty(globalCtx, name, arb1, arb2, arb3, propCheck,
                           defAssertPropParams())
    
    template forAll[A,B,C,D](
        name: string = "",
        arb1: Arbitrary[A], arb2: Arbitrary[B], arb3: Arbitrary[C], arb4: Arbitrary[D],
        propCheck: proc(a: A, b: B, c: C, d: D): PTStatus {.noSideEffect.}
        ) =
      discard execProperty(globalCtx, name, arb1, arb2, arb3, arb4, propCheck,
                           defAssertPropParams())
    
    template forAll[A,B,C,D,E](
        name: string = "",
        arb1: Arbitrary[A], arb2: Arbitrary[B], arb3: Arbitrary[C], arb4: Arbitrary[D], arb5: Arbitrary[E],
        propCheck: proc(a: A, b: B, c: C, d: D, e: E): PTStatus {.noSideEffect.}
        ) =
      discard execProperty(globalCtx, name, arb1, arb2, arb3, arb4, arb5, propCheck,
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

#-- CLI

var cli = commandBuilder(Args)
  .name("pbt") # todo auto detect based on test
  .initCli()
cli.addHelpFlag()
cli.flagBuilder()
  .name("mode")
  .parser(RunMode, (opt, v, var args) => (args.runMode = v))
  .describe("whether to run the tests or outline them")
  .addTo(cli)
let args = cli.run(defaults = Args(runMode: rmTest))

#-- Hackish Tests

when isMainModule:
  spec "nim":
    spec "uint32":
      forAll("are >= 0", uint32Arb(),
             proc(i: uint32): PTStatus = i >= 0) # a silly check

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
              # xxx: pred/succ should be used here; they trigger a compiler bug
              prev = if c == low(char): c else: char(uint8(c) - 1'u8)
              curr = c
              next = if c == high(char): c else: char(uint8(c) + 1'u8)
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
      forAll("concatenation - len is == the sum of the len of the parts",
             stringArb(), stringArb(),
             func(a: string, b: string): PTStatus =
                a.len + b.len == (a & b).len)

    spec "sets":
      type EnumA = enum ea, eb, ec
      forAll("cannot contain more items than the enum itself",
             constArb({ea, eb, ec}),
             enumArb[EnumA](),
             func(s: set[EnumA], c: EnumA): PTStatus =
                let e = s + c
                enumLen(EnumA) == e.len)

      spec "union":
        forAll("a union of sets contain all elements of each",
              setArb[EnumA](), setArb[EnumA](),
              func(a: set[EnumA], b: set[EnumA]): PTStatus =
                  let c = a + b
                  a <= c and b <= c)

        forAll("union is commutative",
               setArb[EnumA](), setArb[EnumA](),
               func(a: set[EnumA], b: set[EnumA]): PTStatus =
                  let
                    c = a + b
                    d = b + a
                  c == d)

      spec "intersection":
        forAll("an intersection is a subset of both operands",
               setArb[EnumA](), setArb[EnumA](),
               func(a: set[EnumA], b: set[EnumA]): PTStatus =
                  let c = a * b
                  c <= a and c <= b)

        forAll("intersection is commutative",
               setArb[EnumA](), setArb[EnumA](),
               func(a: set[EnumA], b: set[EnumA]): PTStatus =
                  let
                    c = a * b
                    d = b * a
                  c == d)

      spec "difference (or relative complement)":
        forAll("a difference has no overlap with the second operand",
               setArb[EnumA](), setArb[EnumA](),
               func(a: set[EnumA], b: set[EnumA]): PTStatus =
                  let c = a - b
                  c * b == {})

      # XXX: this tests the failure branch but isn't running right now
      # test failure at the end because the assert exits early
      # let foo = func(a: uint32, b: uint32): PTStatus =
      #             case a + b > a
      #             of true: ptPass
      #             of false: ptFail
      # forAll("classic math assumption should fail",
      #       uint32Arb(),
      #       uint32Arb(),
      #       foo)
