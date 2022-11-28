discard """
  description: "Test the utils/bitsets compiler module"
  targets: "c js"
"""

import compiler/utils/bitsets

block incl_excl:
  var a: TBitSet
  bitSetInit(a, 1)

  doAssert not bitSetIn(a, 0)
  doAssert not bitSetIn(a, 7)
  doAssertRaises(IndexDefect):
    discard bitSetIn(a, 8)

  # Excluding an element not in the set is valid
  bitSetExcl(a, 2)

  bitSetIncl(a, 3)
  doAssert bitSetIn(a, 3)
  bitSetExcl(a, 3)
  doAssert not bitSetIn(a, 3)


block incl_range:
  var a: TBitSet
  bitSetInit(a, 3)

  template testCase(slice) =
    let s = slice
    # reset set
    for x in a.mitems:
      x = 0

    bitSetInclRange(a, BiggestInt(slice.a)..BiggestInt(slice.b))
    {.line.}:
      for x {.inject.} in s:
        doAssert bitSetIn(a, x), $x

      doAssert bitSetCard(a) == s.len

  # Empty slice
  testCase(2..1)
  # Empty negative slice (must not fail)
  testCase(-8..(-9))
  # single bit
  testCase(1..1)
  # slice modifies only a single bit set element
  testCase(4..(ElemSize-1))
  # test `lastBit mod ElemSize < firstBit mod ElemSize`
  testCase(4..ElemSize)
  # test `lastBit mod ElemSize == firstBit mod ElemSize`
  testCase(4..(ElemSize+4))
  # test `lastBit mod ElemSize > firstBit mod ElemSize`
  testCase(4..(ElemSize+6))
  # more than 2 bit set elements are modified
  testCase(4..(ElemSize * 2 + 4))


template test(ac, bc: int, equal, bInA, aInB) =
  {.line.}:
    doAssert bitSetCard(a) == ac
    doAssert bitSetCard(b) == bc
    let eqA = bitSetEquals(a, b)
    let eqB = bitSetEquals(b, a)
    doAssert eqA == equal
    doAssert eqA == eqB
    doAssert bitSetContains(a, b) == bInA
    doAssert bitSetContains(b, a) == aInB


block comparison:
  var a, b: TBitSet
  bitSetInit(a, 10)
  bitSetInit(b, 10)

  # empty sets
  test(ac = 0, bc = 0, equal = true, bInA = true, aInB = true)


  # same elements, not empty
  bitSetIncl(a, 10)
  bitSetIncl(a, 15)
  bitSetIncl(b, 10)
  bitSetIncl(b, 15)

  test(ac = 2, bc = 2, equal = true, bInA = true, aInB = true)


  # same elements, full possible range
  for i in 0..79:
    bitSetIncl(a, i)
    bitSetIncl(b, i)

  test(ac = 80, bc = 80, equal = true, bInA = true, aInB = true)


  # a < b (proper subset)
  bitSetExcl(a, 30)

  test(ac = 79, bc = 80, equal = false, bInA = false, aInB = true)


  # a != b
  bitSetExcl(b, 10)

  test(ac = 79, bc = 79, equal = false, bInA = false, aInB = false)



# Utility functions

func asgn[T](x: var TBitSet, s: set[T]) =
  assert x.len == sizeof(s)
  for v in x.mitems:
    v = 0
  for e in s.items:
    bitSetIncl(x, int(e))

func `==`[T](x: TBitSet, s: set[T]): bool =
  var tmp: TBitSet
  tmp.bitSetInit(x.len)
  tmp.asgn(s)
  result = bitSetEquals(x, tmp)


# The following tests treat Nim's `set` implementation as the
# source of truth

template test(bitSetOp, setOp, sa, sb) =
  a.asgn(sa)
  b.asgn(sb)

  c = a
  bitSetOp(c, b)
  {.line.}: doAssert c == setOp(sa, sb)

  c = b
  bitSetOp(c, a)
  {.line.}: doAssert c == setOp(sb, sa)

template test(bitSetOp, setOp) =
  bitSetInit(a, 32)
  bitSetInit(b, 32)

  # disjoint sets
  test(bitSetOp, setOp, {10'u8..20'u8}, {30'u8..40'u8})
  # overlapping sets
  test(bitSetOp, setOp, {10'u8..20'u8}, {15'u8..25'u8})
  # same sets
  test(bitSetOp, setOp, {10'u8..20'u8}, {10'u8..20'u8})

block set_operations:
  var a, b, c: TBitSet

  test(bitSetDiff, `-`)
  test(bitSetUnion, `+`)
  test(bitSetIntersect, `*`)

  func symDiff[T](a, b: set[T]): set[T] =
    (a - b) + (b - a)

  test(bitSetSymDiff, symDiff)


block mismatching_length:
  var a, b: TBitSet
  bitSetInit(a, 2)
  bitSetInit(b, 1)

  template test(code) =
    ## Test if the statement or expression `code`
    ## raises an IndexDefect
    {.line.}:
      doAssertRaises(IndexDefect):
        when typeof(code) is void:
          code
        else:
          discard code

  test bitSetEquals(a, b)
  test bitSetContains(a, b)
  test bitSetUnion(a, b)
  test bitSetSymDiff(a, b)
  test bitSetDiff(a, b)
  test bitSetIntersect(a, b)