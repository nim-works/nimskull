import arrays, bitmaps, crossops, miniarrays
import std/options

## Implements a polymorphic set container which automatically selects the
## smallest representation.

const
  BitmapThreshold = 4097
    ## The minimum number of elements for a BitmapContainer to be used.

type
  ContainerKind* {.pure.} = enum
    ## The kind of the container in use.
    Mini
    Array
    Bitmap

  Container* = object
    ## A polymorphic container. The representation in use is chosen to have the
    ## least memory footprint.
    case kind: ContainerKind
    of Mini:
      mini: MiniArrayContainer
    of Array:
      array: ArrayContainer
    of Bitmap:
      bitmap: BitmapContainer

static: doAssert sizeof(Container) == sizeof(ArrayContainer) + sizeof(int): $sizeof(Container)

proc initContainer(b: sink BitmapContainer): Container {.inline.} =
  ## Consume a `BitmapContainer` to create a new `Container`.
  ##
  ## The resulting container might be an `Array` container if `b` has less
  ## elements than the threshold.
  if b.len < BitmapThreshold:
    Container(kind: Array, array: b.toArray())
  else:
    Container(kind: Bitmap, bitmap: b)

proc initContainer(a: sink ArrayContainer): Container {.inline.} =
  ## Consume an `ArrayContainer` to create a new `Container`.
  ##
  ## The resulting container might be a `Bitmap` container if `b` has more or
  ## equal elements compared to the threshold.
  if a.len >= BitmapThreshold:
    Container(kind: Bitmap, bitmap: a.toBitmap())
  else:
    Container(kind: Array, array: a)

proc initContainer(a: sink MiniArrayContainer): Container {.inline.} =
  Container(kind: Mini, mini: a)

proc initContainer*(): Container {.inline.} =
  ## Create a new empty `Container`
  Container(kind: Mini)

proc `==`*(a, b: Container): bool {.inline.} =
  if a.kind == b.kind:
    case a.kind
    of Mini:
      a.mini == b.mini
    of Array:
      a.array == b.array
    of Bitmap:
      a.bitmap == b.bitmap
  else:
    false

proc contains*(c: Container, value: uint16): bool {.inline.} =
  ## Returns if `value` is in `c`.
  case c.kind
  of Mini:
    value in c.mini
  of Array:
    value in c.array
  of Bitmap:
    value in c.bitmap

proc len*(c: Container): int {.inline.} =
  ## Returns the number of elements in `c`.
  case c.kind
  of Mini:
    c.mini.len
  of Array:
    c.array.len
  of Bitmap:
    c.bitmap.len

proc containsOrIncl*(c: var Container, value: uint16): bool {.inline.} =
  ## Add `value` to `c` and returns whether it was already set.
  case c.kind
  of Mini:
    case c.mini.containsOrIncl(value)
    of NotFound:
      result = false
    of Found:
      result = true
    of CantAdd:
      c = initContainer(c.mini.toArray())
      result = c.array.containsOrIncl(value)
  of Array:
    result = c.array.containsOrIncl(value)
    if c.array.len >= BitmapThreshold:
      c = initContainer(c.array.toBitmap())
  of Bitmap:
    result = c.bitmap.containsOrIncl(value)

proc missingOrExcl*(c: var Container, value: uint16): bool {.inline.} =
  ## Remove `value` from `c` and returns whether it was already unset.
  case c.kind
  of Mini:
    result = c.mini.missingOrExcl(value)
  of Array:
    result = c.array.missingOrExcl(value)
  of Bitmap:
    result = c.bitmap.missingOrExcl(value)
    c = initContainer(c.bitmap)

proc incl*(c: var Container, other: BitmapContainer) =
  ## Add elements from `other` to `c`.
  case c.kind
  of Mini:
    c = initContainer(other + c.mini)
  of Array:
    c = initContainer(other + c.array)
  of Bitmap:
    c.bitmap.incl other

proc incl*(c: var Container, other: ArrayContainer) =
  ## Add elements from `other` to `c`.
  case c.kind
  of Mini:
    c = initContainer(other + c.mini)
  of Array:
    # Optimistically allocate a big container if the total length meets
    # the threshold.
    #
    # We will reallocate a smaller container if this assumption was false.
    if c.array.len + other.len >= BitmapThreshold:
      var newbitmap = newBitmapContainer()
      newbitmap.incl c.array
      newbitmap.incl other

      c = initContainer(newbitmap)

    else:
      c.array.incl other

  of Bitmap:
    c.bitmap.incl other

proc incl*(c: var Container, other: MiniArrayContainer) =
  ## Add elements from `other` to `c`.
  case c.kind
  of Mini:
    var new = c.mini + other
    if new.isNone:
      var newarray = newArrayContainer(c.mini.len + other.len)
      for value in union(c.mini, other):
        newarray.uncheckedAdd value

      c = initContainer(newarray)
    else:
      c = initContainer(new.get())
  of Array:
    c.array.incl other
    if c.array.len >= BitmapThreshold:
      c = initContainer(c.array.toBitmap())
  of Bitmap:
    c.bitmap.incl other

proc incl*(c: var Container, other: Container) {.inline.} =
  ## Add elements from `other` to `c`.
  case other.kind
  of Mini:
    c.incl other.mini
  of Array:
    c.incl other.array
  of Bitmap:
    c.incl other.bitmap

proc excl*(c: var Container, other: BitmapContainer) =
  ## Remove elements in `other` from `c`.
  case c.kind
  of Mini:
    doAssert false
  of Array:
    c.array.excl other

  of Bitmap:
    c.bitmap.excl other
    # Reallocate if the bitmap is smaller than the threshold
    c = initContainer(c.bitmap)

proc excl*(c: var Container, other: ArrayContainer) =
  ## Remove elements in `other` from `c`.
  case c.kind
  of Mini:
    doAssert false
  of Array:
    c.array.excl other
  of Bitmap:
    c.bitmap.excl other
    # Reallocate if the bitmap is smaller than the threshold
    c = initContainer(c.bitmap)

proc excl*(c: var Container, other: Container) {.inline.} =
  ## Remove elements in `other` from `c`.
  case other.kind
  of Mini:
    doAssert false
  of Array:
    c.excl other.array
  of Bitmap:
    c.excl other.bitmap

proc `+`*(a: Container, b: BitmapContainer): Container =
  ## Returns the union of `a` and `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    initContainer(b + a.array)
  of Bitmap:
    initContainer(a.bitmap + b)

proc `+`*(a: Container, b: ArrayContainer): Container =
  ## Returns the union of `a` and `b`.
  case a.kind
  of Mini:
    doAssert false

    raise getCurrentException()
  of Array:
    # Optimistically allocate a big container if the total length meets
    # the threshold.
    #
    # We will reallocate a smaller container if this assumption was false.
    if a.array.len + b.len >= BitmapThreshold:
      var newbitmap = newBitmapContainer()
      newbitmap.incl a.array
      newbitmap.incl b

      initContainer(newbitmap)
    else:
      initContainer(a.array + b)
  of Bitmap:
    initContainer(a.bitmap + b)

proc `+`*(a: Container, b: Container): Container {.inline.} =
  ## Returns the union of `a` and `b`.
  case b.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a + b.array
  of Bitmap:
    a + b.bitmap

proc `-`*(a: Container, b: BitmapContainer): Container {.inline.} =
  ## Returns the difference between `a` and `b`.
  case a.kind
  of Mini:
    initContainer(a.mini - b)
  of Array:
    initContainer(a.array - b)
  of Bitmap:
    # Preemptively prepare for size reduction. According to Roaring Bitmap
    # paper, the reallocation overhead is worth the precompute.
    let differenceLen = a.bitmap.differenceLen(b)
    if differenceLen < BitmapThreshold:
      var newarray = newArrayContainer(differenceLen)
      for value in difference(a.bitmap, b):
        newarray.uncheckedAdd(value)

      initContainer(newarray)
    else:
      initContainer(a.bitmap - b)

proc `-`*(a: Container, b: ArrayContainer): Container {.inline.} =
  ## Returns the difference between `a` and `b`.
  case a.kind
  of Mini:
    initContainer(a.mini - b)
  of Array:
    initContainer(a.array - b)
  of Bitmap:
    # Be optimistic that the result will still be larger than the threshold
    initContainer(a.bitmap - b)

proc `-`*(a: Container, b: MiniArrayContainer): Container {.inline.} =
  case a.kind
  of Mini:
    initContainer(a.mini - b)
  of Array:
    initContainer(a.array - b.toArray)
  of Bitmap:
    # Be optimistic that the result will still be larger than the threshold
    initContainer(a.bitmap - b)

proc `-`*(a: Container, b: Container): Container {.inline.} =
  ## Returns the difference between `a` and `b`.
  case b.kind
  of Mini:
    a - b.mini
  of Array:
    a - b.array
  of Bitmap:
    a - b.bitmap

proc `-+-`*(a: Container, b: BitmapContainer): Container {.inline.} =
  ## Returns the symmetric difference bteween `a` and `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    initContainer(b -+- a.array)
  of Bitmap:
    initContainer(a.bitmap -+- b)

proc `-+-`*(a: Container, b: ArrayContainer): Container {.inline.} =
  ## Returns the symmetric difference bteween `a` and `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    initContainer(a.array -+- b)
  of Bitmap:
    initContainer(a.bitmap -+- b)

proc `-+-`*(a: Container, b: Container): Container {.inline.} =
  ## Returns the symmetric difference bteween `a` and `b`.
  case b.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a -+- b.array
  of Bitmap:
    a -+- b.bitmap

proc intersectionLen*(a: Container, b: BitmapContainer): int {.inline.} =
  ## Returns the number of elements in the intersection between `a` and `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a.array.intersectionLen(b)
  of Bitmap:
    a.bitmap.intersectionLen(b)

proc `*`*(a: Container, b: BitmapContainer): Container {.inline.} =
  ## Returns the intersection between `a` and `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    initContainer(a.array * b)
  of Bitmap:
    # Preemptively prepare for size reduction. According to Roaring Bitmap
    # paper, the reallocation overhead is worth the precompute.
    let intersectionLen = a.bitmap.intersectionLen(b)
    if intersectionLen < BitmapThreshold:
      var newarray = newArrayContainer(intersectionLen)
      for value in intersection(a.bitmap, b):
        newarray.uncheckedAdd value
      initContainer(newarray)
    else:
      initContainer(a.bitmap * b)

proc intersectionLen*(a: Container, b: ArrayContainer): int {.inline.} =
  ## Returns the number of elements in the intersection between `a` and `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a.array.intersectionLen(b)
  of Bitmap:
    b.intersectionLen(a.bitmap)

proc `*`*(a: Container, b: ArrayContainer): Container {.inline.} =
  ## Returns the intersection between `a` and `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    initContainer(a.array * b)
  of Bitmap:
    initContainer(b * a.bitmap)

proc intersectionLen*(a: Container, b: Container): int {.inline.} =
  ## Returns the number of elements in the intersection between `a` and `b`.
  case b.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a.intersectionLen(b.array)
  of Bitmap:
    a.intersectionLen(b.bitmap)

proc `*`*(a: Container, b: Container): Container {.inline.} =
  ## Returns the intersection between `a` and `b`.
  case b.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a * b.array
  of Bitmap:
    a * b.bitmap

proc `<=`*(a: Container, b: BitmapContainer): bool {.inline.} =
  ## Returns whether `a` is a subset of `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a.array < b
  of Bitmap:
    a.bitmap <= b

proc `<=`*(a: Container, b: ArrayContainer): bool {.inline.} =
  ## Returns whether `a` is a subset of `b`.
  case a.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a.array <= b
  of Bitmap:
    # Bitmap always have more elements than array
    false

proc `<=`*(a: Container, b: Container): bool {.inline.} =
  ## Returns whether `a` is a subset of `b`.
  case b.kind
  of Mini:
    doAssert false
    raise getCurrentException()
  of Array:
    a <= b.array
  of Bitmap:
    a <= b.bitmap

proc `<`*(a: Container, b: Container): bool {.inline.} =
  ## Returns whether `a` is a proper subset of `b`.
  a.len < b.len and a <= b

iterator items*(c: Container): uint16 =
  ## Yields elements included in `c`.
  case c.kind
  of Mini:
    for value in c.mini.items:
      yield value
  of Array:
    for value in c.array.items:
      yield value
  of Bitmap:
    for value in c.bitmap.items:
      yield value
