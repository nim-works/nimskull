#
#
#                 NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/algorithm
import containers

## Implements the [Roaring Bitmap][1] data structure and algorithms
##
## [1]: https://roaringbitmap.org

type
  RoaringBitmap* = object
    ## A compressed bitmap
    key: seq[uint16]
    container: seq[Container]

proc initRoaringBitmap(cap: Natural): RoaringBitmap =
  ## Creates a roaring bitmap with the specified capacity
  RoaringBitmap(
    key: newSeqOfCap[uint16](cap),
    container: newSeqOfCap[Container](cap),
  )

proc split(x: uint32): tuple[high: uint16, low: uint16] =
  ## Split an `uint32` into high and low parts
  (uint16(x shr 16), uint16(x))

proc join(high, low: uint16): uint32 =
  ## Join high and low parts into one `uint32`
  uint32(high) shl 16 or uint32(low)

proc clear*(r: var RoaringBitmap) =
  ## Clear the bitmap `r`
  r.key.setLen(0)
  r.container.setLen(0)

proc len*(r: RoaringBitmap): int =
  ## Returns the cardinality of the bitmap
  for container in r.container.items:
    result.inc container.len

proc contains*(r: RoaringBitmap, value: uint32): bool =
  ## Returns whether `value` is in `r`
  let (high, low) = value.split()
  let idx = r.key.binarySearch(high)

  idx >= 0 and low in r.container[idx]

proc containsOrIncl*(r: var RoaringBitmap, value: uint32): bool =
  ## Add `value` to `r`.
  ##
  ## Returns `true` if `value` is in `r` prior to calling this function.
  let (high, low) = value.split()
  let insertAt = r.key.lowerBound(high)

  if insertAt >= r.key.len or r.key[insertAt] != high:
    r.key.insert(high, insertAt)
    r.container.insert(initContainer(), insertAt)

  r.container[insertAt].containsOrIncl(low)

proc missingOrExcl*(r: var RoaringBitmap, value: uint32): bool =
  ## Remove `value` from `r`.
  ##
  ## Returns `true` if `value` is not in `r` prior to calling this function.
  let (high, low) = value.split()
  let idx = r.key.binarySearch(high)

  result = idx < 0 or r.container[idx].missingOrExcl(low)
  if not result and r.container[idx].len == 0:
    r.key.delete(idx)
    r.container.delete(idx)

proc incl*(r: var RoaringBitmap, other: RoaringBitmap) =
  ## Add all values in `other` to `r`.
  var rIdx, otherIdx: int
  while rIdx < r.key.len and otherIdx < other.key.len:
    let
      rKey = r.key[rIdx]
      otherKey = other.key[otherIdx]

    # If destination key is larger than the other bitmap, then we are missing
    # a container existed only in the other bitmap
    if rKey > otherKey:
      # Copy the other bitmap container
      r.key.insert(otherKey, rIdx)
      r.container.insert(other.container[otherIdx], rIdx)

    elif rKey == otherKey:
      # Merge the two containers if they are both present
      r.container[rIdx].incl other.container[otherIdx]

    else:
      discard "Nothing to do"

    inc rIdx, ord(rKey <= otherKey)
    inc otherIdx, ord(rKey >= otherKey)

  # If there are extra elements in other bitmap, copy the remaining containers.
  while otherIdx < other.key.len:
    r.key.add other.key[otherIdx]
    r.container.add other.container[otherIdx]
    inc otherIdx

proc excl*(r: var RoaringBitmap, other: RoaringBitmap) =
  ## Remove all values in `other` from `r`.
  var rIdx, otherIdx: int
  while rIdx < r.key.len and otherIdx < other.key.len:
    let
      rKey = r.key[rIdx]
      otherKey = other.key[otherIdx]

    if rKey == otherKey:
      r.container[rIdx].excl other.container[otherIdx]
      if r.container[rIdx].len == 0:
        r.key.delete(rIdx)
        r.container.delete(rIdx)
      else:
        inc rIdx

    inc rIdx, ord(rKey < otherKey)
    inc otherIdx, ord(rKey >= otherKey)

iterator merge(a, b: RoaringBitmap,
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

proc `+`*(a, b: RoaringBitmap): RoaringBitmap =
  ## Returns the union of `a` and `b`.
  result = initRoaringBitmap(a.key.len + b.key.len)
  for cmp, idx in merge(a, b):
    if cmp > 0:
      result.key.add b.key[idx.b]
      result.container.add b.container[idx.b]
    elif cmp < 0:
      result.key.add a.key[idx.a]
      result.container.add a.container[idx.a]
    else:
      result.key.add a.key[idx.a]
      result.container.add: a.container[idx.a] + b.container[idx.b]

proc `-`*(a, b: RoaringBitmap): RoaringBitmap =
  ## Returns the difference between `a` and `b`.
  result = initRoaringBitmap(a.key.len)
  for cmp, idx in merge(a, b, exhaustive = a.key.len > b.key.len):
    if cmp < 0:
      result.key.add a.key[idx.a]
      result.container.add a.container[idx.a]
    elif cmp == 0:
      let container = a.container[idx.a] - b.container[idx.b]
      if container.len > 0:
        result.key.add a.key[idx.a]
        result.container.add container

proc `-+-`*(a, b: RoaringBitmap): RoaringBitmap =
  ## Returns the symmetric difference between `a` and `b`.
  result = initRoaringBitmap(a.key.len + b.key.len)
  for cmp, idx in merge(a, b):
    if cmp > 0:
      result.key.add b.key[idx.b]
      result.container.add b.container[idx.b]
    elif cmp < 0:
      result.key.add a.key[idx.a]
      result.container.add a.container[idx.a]
    else:
      let container = a.container[idx.a] -+- b.container[idx.b]
      if container.len > 0:
        result.key.add a.key[idx.a]
        result.container.add container

proc `*`*(a, b: RoaringBitmap): RoaringBitmap =
  ## Returns the intersection between `a` and `b`.
  result = initRoaringBitmap(min(a.key.len, b.key.len))
  for cmp, idx in merge(a, b, exhaustive = false):
    if cmp == 0:
      let container = a.container[idx.a] * b.container[idx.b]
      if container.len > 0:
        result.key.add a.key[idx.a]
        result.container.add container

proc intersectionLen*(a, b: RoaringBitmap): int =
  ## Returns the number of elements within the intersection between `a` and `b`.
  for cmp, idx in merge(a, b, exhaustive = false):
    if cmp == 0:
      result.inc a.container[idx.a].intersectionLen(b.container[idx.b])

proc `<=`*(a, b: RoaringBitmap): bool =
  ## Returns whether `a` is a subset of `b`.
  if a.key.len <= b.key.len:
    for cmp, idx in merge(a, b, exhaustive = false):
      if cmp > 0:
        return false

      if cmp == 0 and not (a.container[idx.a] <= b.container[idx.b]):
        return false

    true
  else:
    false

proc `<`*(a, b: RoaringBitmap): bool =
  ## Returns whether `a` is a proper subset of `b`.
  if a.key.len <= b.key.len:
    for cmp, idx in merge(a, b, exhaustive = false):
      if cmp > 0:
        return false

      if cmp == 0 and not (a.container[idx.a] < b.container[idx.b]):
        return false

    true
  else:
    false

proc `==`*(a, b: RoaringBitmap): bool {.inline.} =
  ## Returns whether `a` has the same contents as `b`.
  a.key == b.key and a.container == b.container

iterator items*(r: RoaringBitmap): uint32 =
  ## Yields elements included in `r`.
  for idx, high in r.key.pairs:
    for low in r.container[idx].items:
      yield join(high, low)

proc `$`*(r: RoaringBitmap): string =
  ## Returns the string representation of `r`.
  result.add '{'
  for value in r.items:
    if result.len > 1:
      result.add ", "
    result.add $value
  result.add '}'
