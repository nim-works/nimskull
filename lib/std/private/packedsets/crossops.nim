#
#
#                 NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/sequtils
import std/typetraits

import arrays, bitmaps

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

proc incl*(b: var BitmapContainer, a: ArrayContainer) =
  ## Add all values from `a` into `b`.
  for value in a.items:
    discard b.containsOrIncl(value)

proc `+`*(a: BitmapContainer, b: ArrayContainer): BitmapContainer =
  ## Returns the union of `a` and `b`.
  result = a
  result.incl b

proc excl*(b: var BitmapContainer, a: ArrayContainer) =
  ## Removes values in `a` from `b`.
  for value in a.items:
    discard b.missingOrExcl(value)

proc excl*(a: var ArrayContainer, b: BitmapContainer) =
  ## Removes values in `b` from `a`.
  a.distinctBase.keepItIf it notin b

proc `-`*(b: BitmapContainer, a: ArrayContainer): BitmapContainer {.inline.} =
  ## Returns the difference between `b` and `a`.
  result = b
  result.excl a

proc `-`*(a: ArrayContainer, b: BitmapContainer): ArrayContainer {.inline.} =
  ## Returns the difference between `a` and `b`.
  result = newArrayContainer(a.len)
  for value in a.items:
    if value notin b:
      result.uncheckedAdd value

proc `*`*(a: ArrayContainer, b: BitmapContainer): ArrayContainer {.inline.} =
  ## Returns the intersection between `a` and `b`.
  result = newArrayContainer(a.len)
  for value in a.items:
    if value in b:
      result.uncheckedAdd value

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

proc `<`*(a: ArrayContainer, b: BitmapContainer): bool {.inline.} =
  ## Returns whether `a` is a proper subset of `b`.
  assert a.len < b.len
  result = true
  for value in a.items:
    if value notin b:
      return false
