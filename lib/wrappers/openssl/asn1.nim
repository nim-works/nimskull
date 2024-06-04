type
  ASN1_ITEM* {.pure.} = object

template DECLARE_ASN1_ALLOC_FUNCTIONS*(T: typedesc): untyped {.dirty.} =
  proc `T _ new`*(): ptr T {.importc, cdecl.}
  proc `T _ free`*(a: ptr T) {.importc, cdecl.}

template DECLARE_ASN1_ITEM*(name: untyped): untyped {.dirty.} =
  bind ASN1_ITEM
  proc `name _ it`*(): ptr ASN1_ITEM {.importc, cdecl.}

template DECLARE_ASN1_ENCODE_FUNCTIONS*(T: typedesc): untyped {.dirty.} =
  bind DECLARE_ASN1_ITEM
  DECLARE_ASN1_ITEM(T)
  proc `d2i _ T`*(a: ptr ptr T, `in`: ptr ptr UncheckedArray[cuchar], len: clong): ptr T {.importc, cdecl.}
  proc `i2d _ T`*(a: ptr T, `out`: ptr ptr UncheckedArray[cuchar]): cint {.importc, cdecl.}

template DECLARE_ASN1_FUNCTIONS*(T: typedesc): untyped =
  bind DECLARE_ASN1_ALLOC_FUNCTIONS
  bind DECLARE_ASN1_ENCODE_FUNCTIONS
  DECLARE_ASN1_ALLOC_FUNCTIONS(T)
  DECLARE_ASN1_ENCODE_FUNCTIONS(T)
