#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# this unit handles Nim sets; it implements bit sets
# the code here should be reused in the Nim standard library

type
  ElemType = byte
  TBitSet* = seq[ElemType]    # we use byte here to avoid issues with
                              # cross-compiling; uint would be more efficient
                              # however
  TBitSetView* = openArray[ElemType] ## A view into a bit set. This doesn't
                                     ## necessarily have to be a `TBitSet`,
                                     ## but can be any byte sequence storing
                                     ## representing a bit set

const
  ElemSize* = 8
  One = ElemType(1)
  Zero = ElemType(0)
  AllOne = ElemType(0xFF)

template modElemSize(arg: untyped): untyped = arg and 7
template divElemSize(arg: untyped): untyped = arg shr 3

func bitSetIn*(x: TBitSetView, e: BiggestInt): bool =
  ## Tests if `e` is an element of `x`
  result = (x[int(e.divElemSize)] and (One shl e.modElemSize)) != Zero

func bitSetIncl*(x: var TBitSetView, elem: BiggestInt) =
  ## If `elem` is not in set `x` yet, includes the element
  assert(elem >= 0)
  x[int(elem.divElemSize)] = x[int(elem.divElemSize)] or
      (One shl elem.modElemSize)

func bitSetExcl*(x: var TBitSetView, elem: BiggestInt) =
  ## If `elem` is in set `x`, removes it, otherwise does nothing
  x[int(elem.divElemSize)] = x[int(elem.divElemSize)] and
      not(One shl elem.modElemSize)

func bitSetInclRange*(x: var TBitSetView, s: Slice[BiggestInt]) =
  ## Includes elements in the slice `r` that aren't part of `x` yet into `x`
  if unlikely(s.a > s.b): return # Do nothing for empty slices

  # This functions aims to be more efficient than a for loop with `bitSetIncl`

  let start = int(s.a.divElemSize)
  let last = int(s.b.divElemSize)

  let firstBit = s.a.modElemSize
  let endBit = s.b.modElemSize + 1 # lastBit + 1

  # The position of the last bit + 1 in the start element
  let startElemEndBit =
    if start < last: ElemSize
    else: int(endBit)

  # Calculate the intersection between the bit ranges `0..lastBit` and `firstBit..high`
  let startElemBits = ((One shl startElemEndBit) - 1) and (AllOne shl firstBit)

  x[start] = x[start] or startElemBits

  for i in (start+1)..<last:
    x[i] = AllOne

  if start < last:
    x[last] = x[last] or ((One shl endBit) - 1)

func bitSetInit*(b: var TBitSet, length: int) =
  ## Creates a new bitset in `b` with a size-in-bytes of `length * ElemSize` .
  ## The resulting bitset allows for elements in the
  ## range `0..<(length * ElemSize * 8)`
  newSeq(b, length)

func bitSetUnion*(x: var TBitSetView, y: TBitSetView) =
  ## Calculate the union between sets `x` and `y`
  for i in 0..high(x): x[i] = x[i] or y[i]

func bitSetDiff*(x: var TBitSetView, y: TBitSetView) =
  ## Calculates the difference between sets `x` and `y` and stores the result
  ## in `x`
  for i in 0..high(x): x[i] = x[i] and not y[i]

func bitSetSymDiff*(x: var TBitSetView, y: TBitSetView) =
  ## Calculates the symmetric set difference between `x` and `y` and store the
  ## result in `x`
  for i in 0..high(x): x[i] = x[i] xor y[i]

func bitSetIntersect*(x: var TBitSetView, y: TBitSetView) =
  ## Calculates the set intersection between `x` and `y` and store the result
  ## in `x`
  for i in 0..high(x): x[i] = x[i] and y[i]

func bitSetEquals*(x, y: TBitSetView): bool =
  ## Set equality test. Evaluates to `true` ff all elements (bits) in `x` are
  ## also in `y` and both sets have the same number of elements
  for i in 0..high(x):
    if x[i] != y[i]:
      return false
  result = true

func bitSetContains*(x, y: TBitSetView): bool =
  ## Tests if `y` is a subset `x`
  for i in 0..high(x):
    if (not x[i] and y[i]) != Zero:
      return false
  result = true

# Number of set bits for all values of int8
const populationCount: array[uint8, uint8] = block:
    var arr: array[uint8, uint8]

    func countSetBits(x: uint8): uint8 =
      return
        ( x and 0b00000001'u8) +
        ((x and 0b00000010'u8) shr 1) +
        ((x and 0b00000100'u8) shr 2) +
        ((x and 0b00001000'u8) shr 3) +
        ((x and 0b00010000'u8) shr 4) +
        ((x and 0b00100000'u8) shr 5) +
        ((x and 0b01000000'u8) shr 6) +
        ((x and 0b10000000'u8) shr 7)


    for it in low(uint8)..high(uint8):
      arr[it] = countSetBits(cast[uint8](it))

    arr

func bitSetCard*(x: TBitSetView): BiggestInt =
  ## Calculates the number of elements in the `x`
  for it in x:
    result.inc int(populationCount[it])
