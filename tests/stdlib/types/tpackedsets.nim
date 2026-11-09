import std/packedsets
import std/sets

import std/sequtils
import std/algorithm

import std/typetraits

import experimental/property_testing

proc genOrdSet[T: Ordinal](
  g: sink Gen[T],
  minLen = 0.Natural,
  maxLen = 100.Natural,
  exclude: HashSet[T] = HashSet[T](),
  maxRetries = 100
): Gen[HashSet[T]] =
  # Cap maximum length to number of unique elements
  let maxLen =
    when sizeof(T) < sizeof(int):
      min(maxLen, len(low(T)..high(T)) - len(exclude))
    else:
      maxLen

  assert minLen <= maxLen

  result = proc (s: Source): HashSet[T] =
    let (len, oldLen, _) = s.beginArray(minLen.uint32, maxLen.uint32)

    var output = initHashSet[T](len.int)
    let
      pred = proc (x: T): bool =
        x notin output and x notin exclude
      filtered = filter(g, pred, maxRetries)
    for _ in 0 ..< len:
      output.incl filtered(s)

    s.skipNodes(oldLen.int - len.int)

    output

proc toPackedSet[T: Ordinal](hs: HashSet[T]): PackedSet[T] =
  for i in hs.items:
    result.incl i

proc genPackedOrdSet[T: Ordinal](
  g: sink Gen[T],
  minLen = 0.Natural,
  maxLen = 100.Natural,
  exclude: HashSet[T] = HashSet[T](),
  maxRetries = 100
): Gen[PackedSet[T]] =
  map(
    genOrdSet(g, minLen, maxLen, exclude, maxRetries),
    proc (s: HashSet[T]): PackedSet[T] = toPackedSet(s)
  )

proc genUniqueOrdSeq[T: Ordinal](
  g: sink Gen[T],
  minLen = 0.Natural,
  maxLen = 100.Natural,
  exclude: HashSet[T] = HashSet[T](),
  maxRetries = 100
): Gen[seq[T]] =
  genOrdSet(g, minLen, maxLen, exclude, maxRetries).map do (s: HashSet[T]) -> seq[T]:
    result = newSeqOfCap[T](s.len)
    for elem in s.items:
      result.add elem

type
  SeqPartitions[T] = tuple
    a: Slice[int]
    b: Slice[int]
    pool: seq[T]

proc genUniqueOrdSeqPartitions[T: Ordinal](
  g: sink Gen[T],
  minPoolLen = 0.Natural,
  maxPoolLen = 200.Natural,
  exclude: HashSet[T] = HashSet[T](),
  maxRetries = 100
): Gen[SeqPartitions[T]] =
  genUniqueOrdSeq(g, minPoolLen, maxPoolLen, exclude, maxRetries).flatMap do (pool: seq[T]) -> Gen[SeqPartitions[T]]:
    let poolEnd = pool.len - 1
    genTuple: (
      a: genInt(min(0, poolEnd), poolEnd).map(proc (send: auto): auto = 0..send),
      b: genInt(0, max(0, poolEnd)).map(proc (start: auto): auto = start..poolEnd),
      pool: genConst(pool)
    )

type
  Operation {.pure.} = enum
    Incl
    Excl
    Clear

  TestValue[T] = object
    case isSequence: bool
    of true: seq: seq[T]
    of false: value: T

proc genOrdSetOps[T: Ordinal](
  g: sink Gen[T],
  minOpsLen = 0.Natural,
  maxOpsLen = 100.Natural,
  minPoolLen = 0.Natural,
  maxPoolLen = 200.Natural,
  maxRetries = 100
): Gen[seq[(Operation, TestValue[T])]] =
  genUniqueOrdSeq(g, minPoolLen, maxPoolLen, maxRetries = maxRetries).flatMap do (pool: seq[T]) -> auto:
    let genValue = genBool().flatMap do (isSequence: bool) -> Gen[TestValue[T]]:
      if isSequence:
        genUniqueOrdSeq(genExhaustive(pool), maxLen = pool.len).map do (s: seq[T]) -> TestValue[T]:
          TestValue[T](isSequence: true, seq: s)
      else:
        genExhaustive(pool).map do (x: T) -> TestValue[T]:
          TestValue[T](isSequence: false, value: x)

    let genOp =
      if pool != []:
        genEnum[Operation]().flatMap do (op: Operation) -> Gen[(Operation, TestValue[T])]:
          genTuple: (
            operation: genConst(op),
            value: if op == Clear: genConst(default(TestValue[T])) else: genValue
          )
      else:
        genConst: (
          operation: Clear,
          value: default(TestValue[T])
        )

    genSeq(genOp, minOpsLen.uint32, maxOpsLen.uint32)

proc `==`[T](ps: PackedSet[T], hs: HashSet[T]): bool =
  if ps.len != hs.len:
    return false

  for i in hs.items:
    if i notin ps:
      return false

  return true

template property(name, gens, check: untyped): untyped =
  block name:
    {.line: instantiationInfo().}:
      let result = runProperty:
        forAll gens: check

    if result.status in {psFail, psError}:
      {.line: instantiationInfo().}:
        raise newException(AssertionDefect, "Test: `" & astToStr(name) & "` failed: " & $result)

proc testProperties[T](g: sink Gen[T]) =
  property toPackedSetDedups, (
    input: genUniqueOrdSeq(g).flatMap(
      proc (s: seq[T]): auto =
        genTuple: (
          maxElems: genConst(s.len),
          samples: genSeq(genExhaustive(s), minLen = s.len.uint32, maxLen = s.len.uint32 * 2)
        )
    )
  ):
    let (maxElems, samples) = input
    let ps = toPackedSet(samples)

    if ps.len > maxElems:
      return psFail

    let hs = toHashSet(samples)
    if ps == hs:
      return psPass
    else:
      return psFail

  property iterateAllMembers, (hs: genOrdSet(g)):
    let
      ps = toPackedSet(hs)
      hsElems = toSeq(hs.items).sorted()
      psElems = toSeq(ps.items).sorted()

    if ps == hs:
      psPass
    else:
      psFail

  property containsOrIncl, (
    input: g.flatMap(
      proc (testElem: T): auto =
        genTuple: (
          testElem: genConst(testElem),
          set: genPackedOrdSet(g, exclude = toHashSet([testElem]))
        )
    )
  ):
    let (testElem, set) = input
    var ps = set

    # Add once, should not be in the set
    if ps.containsOrIncl(testElem):
      return psFail

    # Should now be in the set
    if testElem notin ps or not ps.containsOrIncl(testElem):
      return psFail

    return psPass

  property missingOrExcl, (
    input: genUniqueOrdSeq(g, minLen = 1).flatMap(
      proc (s: seq[T]): auto =
        genTuple: (
          testElem: genExhaustive(s),
          set: genConst(toPackedSet(s))
        )
    )
  ):
    let (testElem, set) = input
    var ps = set

    # Should be in the set, will be removed
    if ps.missingOrExcl(testElem):
      return psFail

    # Should not be in the set
    if testElem in ps or not ps.missingOrExcl(testElem):
      return psFail

    return psPass

  property subset, (
    input: genUniqueOrdSeq(g).flatMap(
      proc (s: seq[T]): auto =
        genTuple: (
          set: genConst(toPackedSet(s)),
          subset: genPackedOrdSet(genExhaustive(s), maxLen = s.len)
        )
    )
  ):
    let (set, subset) = input

    if not (subset <= set and set >= subset):
      return psFail

    if subset > set or set < subset:
      return psFail

    if subset.len < set.len:
      if not (subset < set and set > subset):
        return psFail

      if set == subset:
        return psFail

    return psPass

  property equivalence, (
    input: genPackedOrdSet(g)
  ):
    let cloned = input
    if input != cloned:
      return psFail

    return psPass

  property union, (
    input: genUniqueOrdSeqPartitions(g)
  ):
    let
      (firstSpan, secondSpan, pool) = input
      reference = toPackedSet(pool[firstSpan] & pool[secondSpan])
      a = toPackedSet(pool[firstSpan])
      b = toPackedSet(pool[secondSpan])

    if a + b != reference or b + a != reference:
      return psFail

    var mutIncl = a
    mutIncl.incl b
    if mutIncl != reference:
      return psFail

    mutIncl = b
    mutIncl.incl a
    if mutIncl != reference:
      return psFail

    return psPass

  property difference, (
    input: genUniqueOrdSeqPartitions(g)
  ):
    let
      (firstSpan, secondSpan, pool) = input
      aOnlySpan = 0..min(firstSpan.b, secondSpan.a - 1)
      bOnlySpan = max(firstSpan.b + 1, secondSpan.a)..pool.high
      a = toPackedSet(pool[firstSpan])
      b = toPackedSet(pool[secondSpan])
      aOnly = toPackedSet(pool[aOnlySpan])
      bOnly = toPackedSet(pool[bOnlySpan])

    if a - b != aOnly:
      return psFail

    if b - a != bOnly:
      return psFail

    return psPass

  property disjoint, (
    input: genUniqueOrdSeqPartitions(g)
  ):
    let
      (firstSpan, secondSpan, pool) = input
      overlapSpan = max(0, secondSpan.a)..min(firstSpan.b, secondSpan.b)
      a = toPackedSet(pool[firstSpan])
      b = toPackedSet(pool[secondSpan])

    if overlapSpan.len > 0:
      if a.disjoint(b) or b.disjoint(a):
        psFail
      else:
        psPass
    else:
      if not a.disjoint(b) or not b.disjoint(a):
        psFail
      else:
        psPass

  property intersection, (
    input: genUniqueOrdSeqPartitions(g)
  ):
    let
      (firstSpan, secondSpan, pool) = input
      overlapSpan = max(0, secondSpan.a)..min(firstSpan.b, secondSpan.b)
      overlap = toPackedSet(pool[overlapSpan])
      a = toPackedSet(pool[firstSpan])
      b = toPackedSet(pool[secondSpan])

    if a * b != overlap or b * a != overlap:
      psFail
    else:
      psPass

  property mutation, (
    input: genOrdSetOps(g)
  ):
    type Element = typeof(input[0][1].value)
    var
      reference = initHashSet[Element]()
      subject = initPackedSet[Element]()

    for (operation, value) in input.items:
      case operation
      of Incl:
        if value.isSequence:
          reference.incl toHashSet(value.seq)
          subject.incl toPackedSet(value.seq)
        else:
          reference.incl value.value
          subject.incl value.value
      of Excl:
        if value.isSequence:
          reference.excl toHashSet(value.seq)
          subject.excl toPackedSet(value.seq)
        else:
          reference.excl value.value
          subject.excl value.value
      of Clear:
        reference.clear()
        subject.clear()

      if subject != reference:
        return psFail

    if subject != reference:
      return psFail
    return psPass

block ints:
  testProperties(genInt())
  testProperties(genInt8())
  testProperties(genInt16())
  testProperties(genInt32())
  testProperties(genInt64())

  testProperties(genUint8())
  testProperties(genUint16())
  testProperties(genUint32())
  testProperties(genUint64())

block enums:
  type SmallEnum = enum A, B, C, D
  testProperties(genBool())
  testProperties(genChar())
  testProperties(genEnum[SmallEnum]())

proc sortedPairs[T](t: T): auto = toSeq(t.pairs).sorted
template sortedItems(t: untyped): untyped = sorted(toSeq(t))

type Id = distinct int
proc `$`(x: Id): string {.borrow.}
proc cmp(a: Id, b: Id): int {.borrow.}
proc `==`(a: Id, b: Id): bool {.borrow.}
proc `<`(a: Id, b: Id): bool {.borrow.}

block special:
  testProperties(genInt().map(proc (x: int): Id = Id(x)))

block typeSafetyTest:
  # mixing sets of different types shouldn't compile
  doAssert not compiles( union(initPackedSet[Id](), initPackedSet[int]()) )
  doAssert     compiles( union(initPackedSet[Id](), initPackedSet[Id]()))

  var ids: PackedSet[Id]
  doAssert not compiles( ids.incl(3) )
  doAssert     compiles( ids.incl(Id(3)) )

  type NonOrdinal = string
  doAssert not compiles( initPackedSet[NonOrdinal]() )

block printTest:
  var a = initPackedSet[EnumABCD]()
  a.incl A
  a.incl C
  doAssert $a == "{A, C}" or $a == "{C, A}"
