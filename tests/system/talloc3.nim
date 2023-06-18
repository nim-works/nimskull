discard """
  targets: "c"
"""

type
  # Imported C Types as Opaques
  M128* {.importc: "__m128", header: "xmmintrin.h".} = object
  M128i* {.importc: "__m128i", header: "emmintrin.h".} = object
  M128d* {.importc: "__m128d", header: "emmintrin.h".} = object
  # Dummy Objects
  NaivePixel = object
    r, g, b: int
  SIMDPixel = object
    r: M128
    g: M128i
    b: M128d
  TinyPixel = object
    r {.bitsize: 4.}: cint
    g {.bitsize: 4.}: cint
    b {.bitsize: 4.}: cint
  MixedPixel = object
    r {.bitsize: 4.}: cint
    g: int
    b: M128i
    a: M128
  MapNaive = UncheckedArray[NaivePixel]
  MapSIMD = UncheckedArray[SIMDPixel]
  MapTiny = UncheckedArray[TinyPixel]
  MapMixed = UncheckedArray[MixedPixel]

block create_simple:
  let
    a = create(NaivePixel, 2)
    b = create(SIMDPixel, 2)
    c = create(TinyPixel, 2)
    d = create(MixedPixel, 2)
  # Allocated Successfully?
  doAssert a != nil
  doAssert b != nil
  doAssert c != nil
  doAssert d != nil
  # Dealloc Pixels
  dealloc(a)
  dealloc(b)
  dealloc(c)
  dealloc(d)

block create_shared:
  let
    a = createShared(NaivePixel, 2)
    b = createShared(SIMDPixel, 2)
    c = createShared(TinyPixel, 2)
    d = createShared(MixedPixel, 2)
  # Allocated Successfully?
  doAssert a != nil
  doAssert b != nil
  doAssert c != nil
  doAssert d != nil
  # Dealloc Pixels
  dealloc(a)
  dealloc(b)
  dealloc(c)
  dealloc(d)

block create_unchecked:
  doAssert not compiles(create(MapNaive))
  doAssert not compiles(createU(MapSIMD))
  doAssert not compiles(createShared(MapTiny))
  doAssert not compiles(createSharedU(MapMixed))
  # ptr and ref UncheckedArray Should Compile
  doAssert compiles(create(ref MapMixed))
  doAssert compiles(createU(ptr MapSIMD))
  doAssert compiles(createShared(ptr MapTiny))
  doAssert compiles(createSharedU(ref MapMixed))
