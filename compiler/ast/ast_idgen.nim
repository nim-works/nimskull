## id generator for modules, symbols, and types.

import
  compiler/ast/[
    ast_types, # Main ast type definitions
  ]

type
  IdGenerator* = ref object # unfortunately, we really need the 'shared mutable' aspect here.
    module*: int32
    symId*: int32
    typeId*: int32
    sealed*: bool

const
  PackageModuleId* = -3'i32

proc idGeneratorFromModule*(m: PSym): IdGenerator =
  assert m.kind == skModule
  result = IdGenerator(
    module: m.itemId.module, symId: m.itemId.item, typeId: 0)

proc nextSymId*(x: IdGenerator): ItemId {.inline.} =
  assert(not x.sealed)
  inc x.symId
  result = ItemId(module: x.module, item: x.symId)

proc nextTypeId*(x: IdGenerator): ItemId {.inline.} =
  assert(not x.sealed)
  inc x.typeId
  result = ItemId(module: x.module, item: x.typeId)

when false:
  proc nextId*(x: IdGenerator): ItemId {.inline.} =
    inc x.item
    result = x[]

when false:
  proc storeBack*(dest: var IdGenerator; src: IdGenerator) {.inline.} =
    assert dest.ItemId.module == src.ItemId.module
    if dest.ItemId.item > src.ItemId.item:
      echo dest.ItemId.item, " ", src.ItemId.item, " ", src.ItemId.module
    assert dest.ItemId.item <= src.ItemId.item
    dest = src