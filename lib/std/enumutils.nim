#
#
#            Nim's Runtime Library
#        (c) Copyright 2020 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import std/macros
from std/typetraits import OrdinalEnum, HoleyEnum

macro enumFullRange(a: typed): untyped =
  newNimNode(nnkCurly).add(a.getType[1][1..^1])

# xxx `genEnumCaseStmt` needs tests and runnableExamples

macro genEnumCaseStmtAux(argSym, normalizer, default, list: typed): untyped =
  ## Invoked by ``genEnumCaseStmt`` to produce the actual case
  ## statement. `argSym`, `normalizer`, and `default` are passed through,
  ## while `list` is a list of tuples with the enum values + associated string
  ## values.
  expectKind list, nnkBracket

  var seen: seq[string]
    ## already added fields. Used for detecting duplicate names.

  result = nnkCaseStmt.newTree(newCall(normalizer, argSym))
  for it in list.items:
    expectKind it, nnkTupleConstr

    let fStr = it[1].strVal
    if fStr notin seen:
      result.add nnkOfBranch.newTree(it[1], it[0])
      seen.add fStr
    else:
      error("Ambiguous enums cannot be parsed, field " & $fStr &
            " appears multiple times!", it[1])

  # finally, add a branch for the default case:
  if default == nil:
    # no default value is provided -> raise
    let raiseStmt = quote do:
      raise newException(ValueError, "Invalid enum value: " & $`argSym`)
    result.add nnkElse.newTree(raiseStmt)
  else:
    expectKind default, nnkSym
    result.add nnkElse.newTree(default)

  result = nnkStmtListExpr.newTree(result)

template genEnumCaseStmt*(typ: typedesc[enum], arg: typed, default: typed,
                          userMin, userMax: static[int],
                          normalizer: typed): untyped =
  ## Generates a case statement yielding the enum value corresponding the
  ## normalized value of `arg`. String normalization is done using passed
  ## normalizer. Only enum values in the provide range are considered.
  proc collect(): seq[(typ, string)] =
    # go over all enum values in the provided range and fetch their string
    # value
    for it in enumFullRange(typ):
      if ord(it) in userMin..userMax:
        result.add (it, normalizer($it))

  # the macro needs the collapsed value expression, which is achieved
  # by using a ``static`` expression
  genEnumCaseStmtAux(arg, normalizer, default, static(collect()))

macro enumNames(a: typed): untyped =
  # this could be exported too; in particular this could be useful for enum with holes.
  result = newNimNode(nnkBracket)
  for ai in a.getType[1][1..^1]:
    assert ai.kind == nnkSym
    result.add newLit ai.strVal

iterator items*[T: HoleyEnum](E: typedesc[T]): T =
  ## Iterates over an enum with holes.
  runnableExamples:
    type
      A = enum
        a0 = 2
        a1 = 4
        a2
      B[T] = enum
        b0 = 2
        b1 = 4
    from std/sequtils import toSeq
    assert A.toSeq == [a0, a1, a2]
    assert B[float].toSeq == [B[float].b0, B[float].b1]
  for a in enumFullRange(E): yield a

func span(T: typedesc[HoleyEnum]): int =
  (T.high.ord - T.low.ord) + 1

const invalidSlot = uint8.high

proc genLookup[T: typedesc[HoleyEnum]](_: T): auto =
  const n = span(T)
  var ret: array[n, uint8]
  var i = 0
  assert n <= invalidSlot.int
  for ai in mitems(ret): ai = invalidSlot
  for ai in items(T):
    ret[ai.ord - T.low.ord] = uint8(i)
    inc(i)
  return ret

func symbolRankImpl[T](a: T): int {.inline.} =
  const n = T.span
  const thres = 255 # must be <= `invalidSlot`, but this should be tuned.
  when n <= thres:
    const lookup = genLookup(T)
    let lookup2 {.global.} = lookup # xxx improve pending https://github.com/timotheecour/Nim/issues/553
    #[
    This could be optimized using a hash adapted to `T` (possible since it's known at CT)
    to get better key distribution before indexing into the lookup table table.
    ]#
    {.noSideEffect.}: # because it's immutable
      let ret = lookup2[ord(a) - T.low.ord]
    if ret != invalidSlot: return ret.int
  else:
    var i = 0
    # we could also generate a case statement as optimization
    for ai in items(T):
      if ai == a: return i
      inc(i)
  raise newException(IndexDefect, $ord(a) & " invalid for " & $T)

template symbolRank*[T: enum](a: T): int =
  ## Returns the index in which `a` is listed in `T`.
  ##
  ## The cost for a `HoleyEnum` is implementation defined, currently optimized
  ## for small enums, otherwise is `O(T.enumLen)`.
  runnableExamples:
    type
      A = enum # HoleyEnum
        a0 = -3
        a1 = 10
        a2
        a3 = (20, "f3Alt")
      B = enum # OrdinalEnum
        b0
        b1
        b2
      C = enum # OrdinalEnum
        c0 = 10
        c1
        c2
    assert a2.symbolRank == 2
    assert b2.symbolRank == 2
    assert c2.symbolRank == 2
    assert c2.ord == 12
    assert a2.ord == 11
  when T is Ordinal: ord(a) - T.low.ord.static
  else: symbolRankImpl(a)

func symbolName*[T: enum](a: T): string =
  ## Returns the symbol name of an enum.
  ##
  ## This uses `symbolRank`.
  runnableExamples:
    type B = enum
      b0 = (10, "kb0")
      b1 = "kb1"
      b2
    let b = B.low
    assert b.symbolName == "b0"
    assert $b == "kb0"
    static: assert B.high.symbolName == "b2"
    type C = enum # HoleyEnum
      c0 = -3
      c1 = 4
      c2 = 20
    assert c1.symbolName == "c1"
  const names = enumNames(T)
  names[a.symbolRank]
