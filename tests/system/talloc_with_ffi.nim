discard """
  targets: "c"
  description: '''
    Using the higher-level allocation procedures with types that have a size not
    known at the NimSkull side must work using backend sizeof
  '''
"""

type FFIObject {.exportc.} = object
  c, y, o: cint
# Avoid DCE Optimize Out
var dummy {.used.}: FFIObject

type 
  # Imported FFI Type
  Imported {.importc: "FFIObject", nodecl.} = object
  # Dummy Objects
  Native = object
    a, b: int
  WithImported = object
    a, b: int
    ffi: Imported
  WithBitField = object
    a {.bitsize: 4.}: cint
    b {.bitsize: 4.}: cint
  Mixed = object
    a {.bitsize: 4.}: cint
    b: int
    c: Imported
  ArrayNative       = UncheckedArray[Native]
  ArrayWithImported = UncheckedArray[WithImported]
  ArrayWithBitField = UncheckedArray[WithBitField]
  ArrayMixed        = UncheckedArray[Mixed]

block create_simple:
  let
    a = create(Imported, 2)
    b = create(Native, 2)
    c = create(WithImported, 2)
    d = create(WithBitField, 2)
    e = create(Mixed, 2)
  # Allocated Successfully?
  doAssert a != nil
  doAssert b != nil
  doAssert c != nil
  doAssert d != nil
  doAssert e != nil
  # Dealloc Pixels
  dealloc(a)
  dealloc(b)
  dealloc(c)
  dealloc(d)
  dealloc(e)

block create_shared:
  let
    a = createShared(Imported, 2)
    b = createShared(Native, 2)
    c = createShared(WithImported, 2)
    d = createShared(WithBitField, 2)
    e = createShared(Mixed, 2)
  # Allocated Successfully?
  doAssert a != nil
  doAssert b != nil
  doAssert c != nil
  doAssert d != nil
  doAssert e != nil
  # Dealloc Pixels
  dealloc(a)
  dealloc(b)
  dealloc(c)
  dealloc(d)
  dealloc(e)

block create_unchecked:
  doAssert not compiles(create(ArrayNative))
  doAssert not compiles(createU(ArrayWithImported))
  doAssert not compiles(createShared(ArrayWithBitField))
  doAssert not compiles(createSharedU(ArrayMixed))
  # ptr and ref UncheckedArray Should Compile
  doAssert compiles(create(ref ArrayNative))
  doAssert compiles(createU(ptr ArrayWithImported))
  doAssert compiles(createShared(ptr ArrayWithBitField))
  doAssert compiles(createSharedU(ref ArrayMixed))
