#
#
#                 NimSkull's runtime library
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/algorithm
import std/typetraits

## Implements a seq-based set implementation. This is done via a sorted seq of
## numbers.

type
  ArrayContainer* = distinct seq[uint16]
    ## An array-based set.

  ValueSource {.pure.} = enum
    ## Source of the value
    RHS
    LHS
    Equal

proc newArrayContainer*(): ArrayContainer {.inline.} = discard
  ## Creates a new empty `ArrayContainer`. Same as default initialization.

proc newArrayContainer*(cap: Natural): ArrayContainer {.inline.} =
  ## Creates a new empty `ArrayContainer` with `cap` preallocated slots.
  ArrayContainer newSeqOfCap[uint16](cap)

proc contains*(s: ArrayContainer, value: uint16): bool {.inline.} =
  ## Returns whether `value` can be found within `s`.
  s.distinctBase.binarySearch(value) >= 0

proc len*(s: ArrayContainer): int {.inline.} = s.distinctBase.len
  ## Returns the cardinality of the set `s`.

template `[]`(s: ArrayContainer, idx: int): untyped =
  ## Private `[]` access operator
  s.distinctBase[idx]

template `==`*(a, b: ArrayContainer): bool =
  ## Returns whether `a` and `b` have the same contents.
  a.distinctBase == b.distinctBase

iterator items*(s: ArrayContainer): lent uint16 =
  ## Yields all values within `s`, in increasing order.
  for i in s.distinctBase.items:
    yield i

proc `$`*(s: ArrayContainer): string =
  ## Returns the string representation of `s`. For debugging only.
  result.add '{'
  for i in s.items:
    if result.len > 1:
      result.add ", "
    result.add $i
  result.add '}'

proc uncheckedAdd*(s: var ArrayContainer, value: uint16) {.inline.} =
  ## Append `value` to `s` without checking if it already exist or is
  ## the largest value.
  ##
  ## This is used for manual set construction.
  s.distinctBase.add(value)

proc containsOrIncl*(s: var ArrayContainer, value: uint16): bool =
  ## Add `value` to `s`.
  ##
  ## Returns whether the value was already set.
  let insertAt = s.distinctBase.lowerBound(value)
  if insertAt >= s.len or s[insertAt] != value:
    s.distinctBase.insert(value, insertAt)
    false
  else:
    true

proc missingOrExcl*(s: var ArrayContainer, value: uint16): bool =
  ## Remove `value` from `s`.
  ##
  ## Returns whether the value was already unset.
  let idx = s.distinctBase.binarySearch(value)
  if idx >= 0:
    s.distinctBase.delete(idx)
    false
  else:
    true

proc incl*(s: var ArrayContainer, value: uint16) {.inline.} =
  ## Add `value` to `s`.
  discard s.containsOrIncl(value)

proc excl*(s: var ArrayContainer, value: uint16) {.inline.} =
  ## Remove `value` from `s`.
  discard s.missingOrExcl(value)

iterator merge(a, b: ArrayContainer, exhaustive: bool = true): tuple[src: ValueSource, value: uint16] =
  ## Iterate through the merged `a` and `b` sets. If `exhaustive` is false,
  ## iteration is terminated when either sets runs out of value.
  var aIdx, bIdx: int
  while aIdx < a.len and bIdx < b.len:
    let a = a[aIdx]
    let b = b[bIdx]

    let src = if a > b: RHS elif a < b: LHS else: Equal
    let value = if a > b: b else: a
    yield (src, value)

    aIdx.inc ord(a <= b)
    bIdx.inc ord(a >= b)

  if exhaustive:
    while aIdx < a.len:
      yield (LHS, a[aIdx])
      inc aIdx

    while bIdx < b.len:
      yield (RHS, b[bIdx])
      inc bIdx

proc `+`*(a, b: ArrayContainer): ArrayContainer =
  ## Returns the union of `a` and `b`.
  result = newArrayContainer(a.len + b.len)
  for _, value in merge(a, b):
    result.uncheckedAdd value

proc `-`*(a, b: ArrayContainer): ArrayContainer =
  ## Returns the difference between `a` and `b`.
  result = newArrayContainer(min(a.len, b.len))
  for src, value in merge(a, b, exhaustive = a.len > b.len):
    if src == LHS:
      result.uncheckedAdd value

proc incl*(s: var ArrayContainer, other: ArrayContainer) {.inline.} =
  ## Add values from `other` to `s`.
  s = s + other

proc excl*(s: var ArrayContainer, other: ArrayContainer) {.inline.} =
  ## Remove values in `other` from `s`.
  s = s - other

proc `-+-`*(a, b: ArrayContainer): ArrayContainer =
  ## Returns the symmetric difference between `a` and `b`.
  result = newArrayContainer(min(a.len, b.len))
  for src, value in merge(a, b):
    if src in {LHS, RHS}:
      result.uncheckedAdd value

iterator gallopingIntersection(short, long: ArrayContainer): uint16 =
  ## Yields the values within the intersection between `short` and `long`, using
  ## binary search.
  ##
  ## `short` must have a smaller length than `long`.
  # The algorithm still works even if this assertion is wrong, it will just be
  # very slow
  assert short.len <= long.len

  var shortIdx = 0
  var longIdx = 0

  while shortIdx < short.len and longIdx < long.len:
    longIdx += long.distinctBase.toOpenArray(longIdx, long.len - 1).lowerBound(short[shortIdx])
    if short[shortIdx] == long[longIdx]:
      yield short[shortIdx]

    inc shortIdx

proc `*`*(a, b: ArrayContainer): ArrayContainer =
  ## Returns the intersection of `a` and `b`.
  const factor = 64
    ## The difference factor before gallopingIntersection is used.
    ##
    ## This value is provided by the Roaring Bitmap paper.

  result = newArrayContainer(min(a.len, b.len))
  if a.len * factor < b.len:
    for value in gallopingIntersection(short = a, long = b):
      result.uncheckedAdd value
  elif b.len * factor < a.len:
    for value in gallopingIntersection(short = b, long = a):
      result.uncheckedAdd value
  else:
    for src, value in merge(a, b, exhaustive = false):
      if src == Equal:
        result.uncheckedAdd value

proc intersectionLen*(a, b: ArrayContainer): int {.inline.} =
  ## Returns the number of elements within the intersection of `a` and `b`.
  const factor = 64
    ## The difference factor before gallopingIntersectionLen is used.
    ##
    ## This value is provided by the Roaring Bitmap paper.

  if a.len * factor < b.len:
    for value in gallopingIntersection(short = a, long = b):
      inc result
  elif b.len * factor < a.len:
    for value in gallopingIntersection(short = b, long = a):
      inc result
  else:
    for src, value in merge(a, b, exhaustive = false):
      inc result, ord(src == Equal)

proc `<=`*(a, b: ArrayContainer): bool =
  ## Returns whether `a` is a subset of `b`.
  if a.len <= b.len:
    for src, value in merge(a, b, exhaustive = false):
      if src == LHS:
        return false

    true
  else:
    false

proc `<`*(a, b: ArrayContainer): bool {.inline.} =
  ## Returns whether `a` is a proper subset of `b`.
  a.len < b.len and a <= b
