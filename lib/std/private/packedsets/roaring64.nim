#
#
#                 NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/algorithm
import roaring

## Implements the 64-bit extension for Roaring Bitmap

type
  Roaring64Bitmap* = object
    ## A compressed bitmap supporting 64-bit values
    key: seq[uint32]
    bitmap: seq[RoaringBitmap]

proc initRoaring64Bitmap(cap: Natural): Roaring64Bitmap =
  ## Creates a roaring bitmap with the specified capacity
  Roaring64Bitmap(
    key: newSeqOfCap[uint32](cap),
    bitmap: newSeqOfCap[RoaringBitmap](cap),
  )

proc split(x: uint64): tuple[high: uint32, low: uint32] =
  ## Split an `uint32` into high and low parts
  (uint32(x shr 32), uint32(x))

proc join(high, low: uint32): uint64 =
  ## Join high and low parts into one `uint64`
  uint64(high) shl 32 or uint64(low)

proc clear*(r: var Roaring64Bitmap) =
  ## Clear the bitmap `r`
  r.key.setLen(0)
  r.bitmap.setLen(0)

proc len*(r: Roaring64Bitmap): int =
  ## Returns the cardinality of the bitmap
  for bitmap in r.bitmap.items:
    result.inc bitmap.len

proc contains*(r: Roaring64Bitmap, value: uint64): bool =
  ## Returns whether `value` is in `r`
  let (high, low) = value.split()
  let idx = r.key.binarySearch(high)

  idx >= 0 and low in r.bitmap[idx]

proc containsOrIncl*(r: var Roaring64Bitmap, value: uint64): bool =
  ## Add `value` to `r`.
  ##
  ## Returns `true` if `value` is in `r` prior to calling this function.
  let (high, low) = value.split()
  let insertAt = r.key.lowerBound(high)

  if insertAt >= r.key.len or r.key[insertAt] != high:
    r.key.insert(high, insertAt)
    r.bitmap.insert(default(RoaringBitmap), insertAt)

  r.bitmap[insertAt].containsOrIncl(low)

proc missingOrExcl*(r: var Roaring64Bitmap, value: uint64): bool =
  ## Remove `value` from `r`.
  ##
  ## Returns `true` if `value` is not in `r` prior to calling this function.
  let (high, low) = value.split()
  let idx = r.key.binarySearch(high)

  result = idx < 0 or r.bitmap[idx].missingOrExcl(low)
  if not result and r.bitmap[idx].len == 0:
    r.key.delete(idx)
    r.bitmap.delete(idx)

proc incl*(r: var Roaring64Bitmap, other: Roaring64Bitmap) =
  ## Add all values in `other` to `r`.
  var rIdx, otherIdx: int
  while rIdx < r.key.len and otherIdx < other.key.len:
    let
      rKey = r.key[rIdx]
      otherKey = other.key[otherIdx]

    # If destination key is larger than the other bitmap, then we are missing
    # a bitmap existed only in the other bitmap
    if rKey > otherKey:
      # Copy the other roaring bitmap
      r.key.insert(otherKey, rIdx)
      r.bitmap.insert(other.bitmap[otherIdx], rIdx)

    elif rKey == otherKey:
      # Merge the two bitmaps if they are both present
      r.bitmap[rIdx].incl other.bitmap[otherIdx]

    else:
      discard "Nothing to do"

    inc rIdx, ord(rKey <= otherKey)
    inc otherIdx, ord(rKey >= otherKey)

  # If there are extra elements in other bitmap, copy the remainders.
  while otherIdx < other.key.len:
    r.key.add other.key[otherIdx]
    r.bitmap.add other.bitmap[otherIdx]
    inc otherIdx

proc excl*(r: var Roaring64Bitmap, other: Roaring64Bitmap) =
  ## Remove all values in `other` from `r`.
  var rIdx, otherIdx: int
  while rIdx < r.key.len and otherIdx < other.key.len:
    let
      rKey = r.key[rIdx]
      otherKey = other.key[otherIdx]

    if rKey == otherKey:
      r.bitmap[rIdx].excl other.bitmap[otherIdx]
      if r.bitmap[rIdx].len == 0:
        r.key.delete(rIdx)
        r.bitmap.delete(rIdx)
      else:
        inc rIdx

    inc rIdx, ord(rKey < otherKey)
    inc otherIdx, ord(rKey >= otherKey)

iterator merge(a, b: Roaring64Bitmap,
                exhaustive: bool = true): tuple[keyCmp: int, idx: tuple[a, b: int]] =
  ## Iterates through `a` and `b` keys in a similar fashion to mergesort.
  ## The bitmap with smaller key currently will have its index advanced in the
  ## next iteration.
  ##
  ## Yields the result of `cmp` between the current key of `a` and `b`, and
  ## their indices.
  ##
  ## When exhaustive is true, iteration will continue after either one of the
  ## bitmap has been exhausted. `idx` of the exhausted bitmap will be `-1`, and
  ## `keyCmp` will be `-1` for `a` and `1` for `b`. This mimics the typical
  ## selection criteria during merge (ie. select the smaller key).
  var aIdx, bIdx: int
  while aIdx < a.key.len and bIdx < b.key.len:
    let keyCmp = cmp(a.key[aIdx], b.key[bIdx])
    yield (keyCmp, (aIdx, bIdx))

    inc aIdx, ord(keyCmp <= 0)
    inc bIdx, ord(keyCmp >= 0)

  if exhaustive:
    while aIdx < a.key.len:
      yield (-1, (aIdx, -1))
      inc aIdx

    while bIdx < b.key.len:
      yield (1, (-1, bIdx))
      inc bIdx

proc `+`*(a, b: Roaring64Bitmap): Roaring64Bitmap =
  ## Returns the union of `a` and `b`.
  result = initRoaring64Bitmap(a.key.len + b.key.len)
  for cmp, idx in merge(a, b):
    if cmp > 0:
      result.key.add b.key[idx.b]
      result.bitmap.add b.bitmap[idx.b]
    elif cmp < 0:
      result.key.add a.key[idx.a]
      result.bitmap.add a.bitmap[idx.a]
    else:
      result.key.add a.key[idx.a]
      result.bitmap.add: a.bitmap[idx.a] + b.bitmap[idx.b]

proc `-`*(a, b: Roaring64Bitmap): Roaring64Bitmap =
  ## Returns the difference between `a` and `b`.
  result = initRoaring64Bitmap(a.key.len)
  for cmp, idx in merge(a, b, exhaustive = a.key.len > b.key.len):
    if cmp < 0:
      result.key.add a.key[idx.a]
      result.bitmap.add a.bitmap[idx.a]
    elif cmp == 0:
      let bitmap = a.bitmap[idx.a] - b.bitmap[idx.b]
      if bitmap.len > 0:
        result.key.add a.key[idx.a]
        result.bitmap.add bitmap

proc `-+-`*(a, b: Roaring64Bitmap): Roaring64Bitmap =
  ## Returns the symmetric difference between `a` and `b`.
  result = initRoaring64Bitmap(a.key.len + b.key.len)
  for cmp, idx in merge(a, b):
    if cmp > 0:
      result.key.add b.key[idx.b]
      result.bitmap.add b.bitmap[idx.b]
    elif cmp < 0:
      result.key.add a.key[idx.a]
      result.bitmap.add a.bitmap[idx.a]
    else:
      let bitmap = a.bitmap[idx.a] -+- b.bitmap[idx.b]
      if bitmap.len > 0:
        result.key.add a.key[idx.a]
        result.bitmap.add bitmap

proc `*`*(a, b: Roaring64Bitmap): Roaring64Bitmap =
  ## Returns the intersection between `a` and `b`.
  result = initRoaring64Bitmap(min(a.key.len, b.key.len))
  for cmp, idx in merge(a, b, exhaustive = false):
    if cmp == 0:
      let bitmap = a.bitmap[idx.a] * b.bitmap[idx.b]
      if bitmap.len > 0:
        result.key.add a.key[idx.a]
        result.bitmap.add bitmap

proc intersectionLen*(a, b: Roaring64Bitmap): int =
  ## Returns the number of elements within the intersection between `a` and `b`.
  for cmp, idx in merge(a, b, exhaustive = false):
    if cmp == 0:
      result.inc a.bitmap[idx.a].intersectionLen(b.bitmap[idx.b])

proc `<=`*(a, b: Roaring64Bitmap): bool =
  ## Returns whether `a` is a subset of `b`.
  if a.key.len <= b.key.len:
    for cmp, idx in merge(a, b, exhaustive = false):
      if cmp > 0:
        return false

      if cmp == 0 and not (a.bitmap[idx.a] <= b.bitmap[idx.b]):
        return false

    true
  else:
    false

proc `<`*(a, b: Roaring64Bitmap): bool =
  ## Returns whether `a` is a proper subset of `b`.
  if a.key.len <= b.key.len:
    for cmp, idx in merge(a, b, exhaustive = false):
      if cmp > 0:
        return false

      if cmp == 0 and not (a.bitmap[idx.a] < b.bitmap[idx.b]):
        return false

    true
  else:
    false

proc `==`*(a, b: Roaring64Bitmap): bool {.inline.} =
  ## Returns whether `a` has the same contents as `b`.
  a.key == b.key and a.bitmap == b.bitmap

iterator items*(r: Roaring64Bitmap): uint64 =
  ## Yields elements included in `r`.
  for idx, high in r.key.pairs:
    for low in r.bitmap[idx].items:
      yield join(high, low)

proc `$`*(r: Roaring64Bitmap): string =
  ## Returns the string representation of `r`.
  result.add '{'
  for value in r.items:
    if result.len > 1:
      result.add ", "
    result.add $value
  result.add '}'
