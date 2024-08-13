#
#
#                 NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/algorithm
import std/sequtils
import std/typetraits

import arrays, bitmaps, miniarrays

## Implements set operations between `ArrayContainer` and `BitmapContainer`.

proc toBitmap*(a: ArrayContainer): BitmapContainer =
  ## Convert an `ArrayContainer` to a `BitmapContainer`.
  result = newBitmapContainer()
  for value in a.items:
    discard result.containsOrIncl(value)

proc toArray*(b: BitmapContainer): ArrayContainer =
  ## Convert a `BitmapContainer` to an `ArrayContainer`.
  result = newArrayContainer(b.len)
  for value in b.items:
    result.uncheckedAdd value

proc toArray*(a: MiniArrayContainer): ArrayContainer =
  result = newArrayContainer(a.len + 1)
  for value in a.items:
    result.uncheckedAdd value

proc incl*(a: var ArrayContainer, b: MiniArrayContainer) =
  for value in b.items:
    discard a.containsOrIncl(value)

proc incl*(b: var BitmapContainer, a: MiniArrayContainer | ArrayContainer) =
  ## Add all values from `a` into `b`.
  for value in a.items:
    discard b.containsOrIncl(value)

proc `+`*(a: ArrayContainer, b: MiniArrayContainer): ArrayContainer =
  result = newArrayContainer(a.len + b.len)
  var aIdx: int

  for value in b.items:
    while aIdx < a.len:
      let aval = a.distinctBase[aIdx]

      if aval == value:
        inc aIdx
        break

      if aval > value:
        break

      result.uncheckedAdd aval
      inc aIdx

    result.uncheckedAdd value

  while aIdx < a.len:
    result.uncheckedAdd a.distinctBase[aIdx]

proc `+`*(a: BitmapContainer, b: MiniArrayContainer | ArrayContainer): BitmapContainer =
  ## Returns the union of `a` and `b`.
  result = a
  result.incl b

proc excl*(b: var BitmapContainer, a: MiniArrayContainer | ArrayContainer) =
  ## Removes values in `a` from `b`.
  for value in a.items:
    discard b.missingOrExcl(value)

proc excl*(a: var ArrayContainer, b: BitmapContainer) =
  ## Removes values in `b` from `a`.
  a.distinctBase.keepItIf it notin b

proc `-`*(b: BitmapContainer, a: MiniArrayContainer | ArrayContainer): BitmapContainer {.inline.} =
  ## Returns the difference between `b` and `a`.
  result = b
  result.excl a

proc `-`*(a: ArrayContainer, b: BitmapContainer): ArrayContainer {.inline.} =
  ## Returns the difference between `a` and `b`.
  result = newArrayContainer(a.len)
  for value in a.items:
    if value notin b:
      result.uncheckedAdd value

proc `-`*(a: MiniArrayContainer, b: BitmapContainer): MiniArrayContainer {.inline.} =
  for value in a.items:
    if value notin b:
      discard result.uncheckedAdd value

proc `-`*(a: MiniArrayContainer, b: ArrayContainer): MiniArrayContainer {.inline.} =
  var bIdx: int

  for value in a.items:
    if bIdx > b.len:
      break

    bIdx += b.distinctBase.toOpenArray(bIdx, b.len - 1).lowerBound(value)
    if bIdx >= b.len or value != b.distinctBase[bIdx]:
      discard result.uncheckedAdd value

proc excl*(a: var MiniArrayContainer, b: BitmapContainer) {.inline.} =
  ## Removes values in `b` from `a`.
  a = a - b

proc `*`*(a: MiniArrayContainer, b: ArrayContainer): MiniArrayContainer {.inline.} =
  var bIdx: int

  for value in a.items:
    if bIdx > b.len:
      break

    bIdx += b.distinctBase.toOpenArray(bIdx, b.len - 1).lowerBound(value)
    if bIdx < b.len and value == b.distinctBase[bIdx]:
      discard result.uncheckedAdd value

proc `*`*(a: MiniArrayContainer, b: BitmapContainer): MiniArrayContainer {.inline.} =
  ## Returns the intersection between `a` and `b`.
  for value in a.items:
    if value in b:
      discard result.uncheckedAdd value

proc `*`*(a: ArrayContainer, b: BitmapContainer): ArrayContainer {.inline.} =
  ## Returns the intersection between `a` and `b`.
  result = newArrayContainer(a.len)
  for value in a.items:
    if value in b:
      result.uncheckedAdd value

proc intersectionLen*(a: MiniArrayContainer, b: ArrayContainer | BitmapContainer): int {.inline.} =
  ## Returns the number of elements in the intersection between `a` and `b`.
  len(a * b)

proc intersectionLen*(a: ArrayContainer, b: BitmapContainer): int {.inline.} =
  ## Returns the number of elements in the intersection between `a` and `b`.
  for value in a.items:
    inc result, ord(value in b)

proc `-+-`*(b: BitmapContainer, a: ArrayContainer): BitmapContainer {.inline.} =
  ## Returns the symmetric difference between `a` and `b`.
  result = b
  for value in a.items:
    if value in result:
      result.excl value
    else:
      result.incl value

proc `-+-`*(b: BitmapContainer, a: MiniArrayContainer): BitmapContainer {.inline.} =
  ## Returns the symmetric difference between `a` and `b`.
  result = b
  for value in a.items:
    if value in result:
      result.excl value
    else:
      result.incl value

proc `-+-`*(a: ArrayContainer, b: MiniArrayContainer): ArrayContainer {.inline.} =
  ## Returns the symmetric difference between `a` and `b`.
  result = newArrayContainer(a.len + b.len)
  var aIdx: int

  for value in b.items:
    while aIdx < a.len:
      let aval = a.distinctBase[aIdx]

      if aval == value:
        inc aIdx
        break

      if aval > value:
        break

      result.uncheckedAdd aval
      inc aIdx

    if aIdx >= a.len or a.distinctBase[aIdx] != value:
      result.uncheckedAdd value

  while aIdx < a.len:
    result.uncheckedAdd a.distinctBase[aIdx]

proc `<`*(a: ArrayContainer, b: BitmapContainer): bool {.inline.} =
  ## Returns whether `a` is a proper subset of `b`.
  assert a.len < b.len
  result = true
  for value in a.items:
    if value notin b:
      return false
