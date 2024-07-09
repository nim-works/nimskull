#
#
#                 NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/bitops

## Implements an uncompressed bitmap for uint16 values

const BitmapSize = when defined(js): 2048 else: 1024
  ## The number of words required to store all uint16 values

type
  Word = (when defined(js): uint32 else: uint64)
    ## Size of a word in the container.
    ##
    ## For JS uint32 is used since that's the largest native size

  BitmapContainer* {.requiresInit.} = object
    ## A bitmap.
    ##
    ## This is a value type.
    len: int ## The number of set bits
    bits: ref array[BitmapSize, Word]

const WordSize = sizeof(Word) * 8

proc `=copy`(dst: var BitmapContainer, src: BitmapContainer) =
  ## Implements copying for bitmaps
  if dst.bits != src.bits:
    dst.len = src.len
    new(dst.bits)
    dst.bits[] = src.bits[]

proc newBitmapContainer*(): BitmapContainer {.inline.} =
  ## Creates a new empty bitmap.
  BitmapContainer(len: 0, bits: new(BitmapContainer.bits))

proc `==`*(a, b: BitmapContainer): bool {.inline.} =
  ## Test two bitmaps for equality.
  a.len == b.len and a.bits[] == b.bits[]

proc contains*(b: BitmapContainer, value: uint16): bool {.inline.} =
  ## Check if `value` is set within `b`.
  b.bits[][value div WordSize].testBit(value mod WordSize)

proc len*(b: BitmapContainer): int {.inline.} = b.len
  ## Returns the number of elements set in `b`.

proc containsOrIncl*(b: var BitmapContainer, value: uint16): bool =
  ## Add `value` to `b`.
  ##
  ## Returns whether the value was already set.
  result = value in b
  b.bits[][value div WordSize].setBit(value mod WordSize)
  inc b.len, ord(not result)

proc missingOrExcl*(b: var BitmapContainer, value: uint16): bool =
  ## Remove `value` from `b`.
  ##
  ## Returns whether the value was already unset.
  result = value notin b
  b.bits[][value div WordSize].clearBit(value mod WordSize)
  dec b.len, ord(not result)

proc incl*(b: var BitmapContainer, value: uint16) {.inline.} =
  ## Add `value` to `b`.
  discard b.containsOrIncl(value)

proc excl*(b: var BitmapContainer, value: uint16) {.inline.} =
  ## Remove `value` from `b`.
  discard b.missingOrExcl(value)

proc incl*(b: var BitmapContainer, other: BitmapContainer) =
  ## Add values from `other` to `b`.
  b.len = 0
  for idx in 0..<BitmapSize:
    b.bits[][idx] = b.bits[][idx] or other.bits[][idx]
    b.len.inc b.bits[][idx].countSetBits()

proc excl*(b: var BitmapContainer, other: BitmapContainer) =
  ## Remove values in `other` from `b`.
  b.len = 0
  for idx in 0..<BitmapSize:
    b.bits[][idx] = b.bits[][idx] and not other.bits[][idx]
    b.len.inc b.bits[][idx].countSetBits()

proc `+`*(a, b: BitmapContainer): BitmapContainer =
  ## Returns the union of `a` and `b`.
  result = newBitmapContainer()
  for idx in 0..<BitmapSize:
    result.bits[][idx] = a.bits[][idx] or b.bits[][idx]
    result.len.inc result.bits[][idx].countSetBits()

iterator differenceBits(a, b: BitmapContainer): tuple[idx: int, bits: Word] =
  ## Yields the set difference between each words in `a` and `b`
  for idx in 0..<BitmapSize:
    yield (idx, a.bits[][idx] and not b.bits[][idx])

iterator symmetricDifferenceBits(a, b: BitmapContainer): tuple[idx: int, bits: Word] =
  ## Yields the set symmetric difference between each words in `a` and `b`
  for idx in 0..<BitmapSize:
    yield (idx, a.bits[][idx] xor b.bits[][idx])

iterator intersectionBits(a, b: BitmapContainer): tuple[idx: int, bits: Word] =
  ## Yields the set intersection between each words in `a` and `b`
  for idx in 0..<BitmapSize:
    yield (idx, a.bits[][idx] and b.bits[][idx])

proc `-`*(a, b: BitmapContainer): BitmapContainer =
  ## Returns the difference between `a` and `b`.
  result = newBitmapContainer()
  for idx, bits in differenceBits(a, b):
    result.bits[][idx] = bits
    result.len.inc bits.countSetBits()

proc `-+-`*(a, b: BitmapContainer): BitmapContainer =
  ## Returns the symmetric difference between `a` and `b`.
  result = newBitmapContainer()
  for idx, bits in symmetricDifferenceBits(a, b):
    result.bits[][idx] = bits
    result.len.inc bits.countSetBits()

proc `*`*(a, b: BitmapContainer): BitmapContainer =
  ## Returns the intersection of `a` and `b`.
  result = newBitmapContainer()
  for idx, bits in intersectionBits(a, b):
    result.bits[][idx] = bits
    result.len.inc bits.countSetBits()

proc `<=`*(a, b: BitmapContainer): bool =
  ## Returns whether `a` is a subset of `b`.
  if a.len <= b.len:
    for idx in 0..<BitmapSize:
      let intersect = a.bits[][idx] and b.bits[][idx]
      if a.bits[][idx] != intersect:
        return false

    true
  else:
    false

proc `<`*(a, b: BitmapContainer): bool {.inline.} =
  ## Returns whether `a` is a proper subset of `b`.
  a.len < b.len and a <= b

proc intersectionLen*(a, b: BitmapContainer): int {.inline.} =
  ## Returns the number of elements within the intersection of `a` and `b`.
  for _, bits in intersectionBits(a, b):
    result.inc countSetBits(bits)

proc differenceLen*(a, b: BitmapContainer): int {.inline.} =
  ## Returns the number of elements within the difference of `a` and `b`.
  for _, bits in differenceBits(a, b):
    result.inc countSetBits(bits)

proc symmetricDifferenceLen*(a, b: BitmapContainer): int {.inline.} =
  ## Returns the number of elements within the symmetric difference of `a` and `b`.
  for _, bits in symmetricDifferenceBits(a, b):
    result.inc countSetBits(bits)

proc `-%`(x: uint64): uint64 =
  ## Returns the 2-complement negation of `x`.
  (not x) + 1

proc `-%`(x: uint32): uint32 =
  ## Returns the 2-complement negation of `x`.
  (not x) + 1

iterator items(w: Word): uint16 =
  ## Yields the index of each bit set within `w`.
  var word = w
  while word != 0:
    let temp = word and -%word
    yield countSetBits(temp - 1).uint16
    word = word and (word - 1)

iterator items*(b: BitmapContainer): uint16 =
  ## Yields the values set within `b`.
  ##
  ## Values are always yielded in increasing order.
  var wordoff = 0u16
  for word in b.bits[].items:
    for bitoff in word.items:
      yield bitoff + wordoff
    wordoff.inc WordSize

iterator difference*(a, b: BitmapContainer): uint16 =
  ## Yields the values within `a - b`.
  ##
  ## This is faster than `items(a - b)` since no temporary containers are
  ## created.
  var wordoff = 0u16
  for _, word in differenceBits(a, b):
    for bitoff in word.items:
      yield bitoff + wordoff
    wordoff.inc WordSize

iterator symmetricDifference*(a, b: BitmapContainer): uint16 =
  ## Yields the values within `a -+- b`.
  ##
  ## This is faster than `items(a -+- b)` since no temporary containers are
  ## created.
  var wordoff = 0u16
  for _, word in symmetricDifferenceBits(a, b):
    for bitoff in word.items:
      yield bitoff + wordoff
    wordoff.inc WordSize

iterator intersection*(a, b: BitmapContainer): uint16 =
  ## Yields the values within `a * b`.
  ##
  ## This is faster than `items(a * b)` since no temporary containers are
  ## created.
  var wordoff = 0u16
  for _, word in intersectionBits(a, b):
    for bitoff in word.items:
      yield bitoff + wordoff
    wordoff.inc WordSize

proc `$`*(b: BitmapContainer): string =
  ## Stringify `b`. This is meant for debugging purposes only.
  result.add '{'
  for value in b.items:
    if result.len > 1:
      result.add ", "
    result.add $value
  result.add '}'
