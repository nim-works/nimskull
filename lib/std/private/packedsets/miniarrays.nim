import std/options

const
  MaxMiniSize = sizeof(seq[uint16]) div sizeof(uint16) - 1

type
  MiniArrayContainer* = object
    size: uint16
    data: array[MaxMiniSize, uint16]

  InclResult* {.pure.} = enum
    NotFound
    Found
    CantAdd

  ValueSource {.pure.} = enum
    ## Source of the value
    RHS
    LHS
    Equal

proc len*(a: MiniArrayContainer): int {.inline.} =
  a.size.int

proc insert(a: var MiniArrayContainer, value: uint16, pos: Natural): bool {.inline.} =
  if a.len < a.data.len:
    let oldlen = a.len
    when nimvm:
      for idx in countdown(oldlen, pos + 1):
        a.data[idx] = a.data[idx - 1]
    else:
      if pos < oldlen:
        moveMem(addr a.data[pos + 1], addr a.data[pos], (oldlen - pos) * sizeof(uint16))

    if pos > a.len:
      raise newException(IndexDefect, "index " & $pos & " not in " & $(0..<a.len))

    inc a.size
    a.data[pos] = value
    result = true

proc uncheckedAdd*(a: var MiniArrayContainer, value: uint16): bool {.inline.} =
  a.insert(value, a.len)

proc delete(a: var MiniArrayContainer, pos: Natural) {.inline.} =
  if pos >= a.len:
    raise newException(IndexDefect, "index " & $pos & " not in " & $(0..<a.len))

  when nimvm:
    for idx in pos + 1 ..< a.len:
      a.data[idx - 1] = a.data[idx]
  else:
    if pos + 1 < a.len:
      moveMem(addr a.data[pos], addr a.data[pos + 1], (a.len - pos - 1) * sizeof(uint16))

  dec a.size

proc contains*(a: MiniArrayContainer, value: uint16): bool {.inline.} =
  for idx in 0..<a.len:
    if a.data[idx] == value:
      return true

iterator items*(a: MiniArrayContainer): lent uint16 =
  ## Yields all values within `s`, in increasing order.
  for idx in 0..<a.len:
    yield a.data[idx]

proc containsOrIncl*(a: var MiniArrayContainer, value: uint16): InclResult {.inline.} =
  for idx in 0..<a.len:
    let cmp = cmp(value, a.data[idx])
    if cmp < 0:
      if not a.insert(value, idx):
        return CantAdd
      return NotFound
    if cmp == 0:
      return Found

  if not a.uncheckedAdd(value):
    CantAdd
  else:
    NotFound

proc missingOrExcl*(a: var MiniArrayContainer, value: uint16): bool {.inline.} =
  result = true
  for idx in 0 ..< a.len:
    let cmp = cmp(value, a.data[idx])
    if cmp < 0:
      return true
    if cmp == 0:
      a.delete(idx)
      return false

proc incl*(a: var MiniArrayContainer, value: uint16) {.inline.} =
  ## Add `value` to `s`.
  discard a.containsOrIncl(value)

proc excl*(a: var MiniArrayContainer, value: uint16) {.inline.} =
  ## Remove `value` from `s`.
  discard a.missingOrExcl(value)

iterator merge(a, b: MiniArrayContainer, exhaustive: bool = true): tuple[src: ValueSource, value: uint16] =
  ## Iterate through the merged `a` and `b` sets. If `exhaustive` is false,
  ## iteration is terminated when either sets runs out of value.
  var aIdx, bIdx: int
  while aIdx < a.len and bIdx < b.len:
    let a = a.data[aIdx]
    let b = b.data[bIdx]

    let src = if a > b: RHS elif a < b: LHS else: Equal
    let value = if a > b: b else: a
    yield (src, value)

    aIdx.inc ord(a <= b)
    bIdx.inc ord(a >= b)

  if exhaustive:
    while aIdx < a.len:
      yield (LHS, a.data[aIdx])
      inc aIdx

    while bIdx < b.len:
      yield (RHS, b.data[bIdx])
      inc bIdx

iterator union*(a, b: MiniArrayContainer): uint16 =
  for _, value in merge(a, b):
    yield value

proc `+`*(a, b: MiniArrayContainer): Option[MiniArrayContainer] =
  var r: MiniArrayContainer
  for value in union(a, b):
    if not r.uncheckedAdd(value):
      return

  some(r)

proc `-`*(a, b: MiniArrayContainer): MiniArrayContainer =
  for src, value in merge(a, b, exhaustive = a.len > b.len):
    if src == LHS:
      discard result.uncheckedAdd(value)

proc incl*(a: var MiniArrayContainer, other: MiniArrayContainer): bool {.inline.} =
  ## Add values from `other` to `a`.
  let new = a + other
  if new.isSome:
    a = new.get()
    true
  else:
    false

proc excl*(a: var MiniArrayContainer, other: MiniArrayContainer): bool {.inline.} =
  ## Remove values in `other` from `a`.
  a = a - other

iterator symmetricDifference*(a, b: MiniArrayContainer): uint16 =
  for src, value in merge(a, b):
    if src in {LHS, RHS}:
      yield value

proc `-+-`*(a, b: MiniArrayContainer): Option[MiniArrayContainer] =
  ## Returns the symmetric difference between `a` and `b`.
  var r: MiniArrayContainer
  for value in symmetricDifference(a, b):
    if not r.insert(value, r.len):
      return

  some(r)

proc `*`*(a, b: MiniArrayContainer): MiniArrayContainer {.inline.} =
  for src, value in merge(a, b, exhaustive = false):
    if src == Equal:
      discard result.insert(value, result.len)

proc `<=`*(a, b: MiniArrayContainer): bool =
  ## Returns whether `a` is a subset of `b`.
  if a.len <= b.len:
    for src, value in merge(a, b, exhaustive = false):
      if src == LHS:
        return false

    true
  else:
    false

proc `<`*(a, b: MiniArrayContainer): bool {.inline.} =
  ## Returns whether `a` is a proper subset of `b`.
  a.len < b.len and a <= b
