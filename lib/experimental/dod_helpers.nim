import std/[macros, strutils, options]

template declareIdType*(
    Name: untyped,
    addHash: static[bool] = false,
    BaseType: typed = uint64,
    zeroIsEmpty: static[bool] = true
  ): untyped =

  when zeroIsEmpty:
    type `Name Id`* {.requiresinit, inject.} = distinct BaseType
    const `Empty Name Id`* {.inject.} = `Name Id`(0)
    const idxOffset = 1

  else:
    type `Name Id`* {.inject.} = distinct BaseType
    const `Empty Name Id`* {.inject.} = `Name Id`(-1)
    const idxOffset = 0

  func `==`*(i1, i2: `Name Id`): bool {.inject.} = i1.int == i2.int
  func isNil*(i: `Name Id`): bool {.inject.} = i == `Empty Name Id`

  func `$`*(id: `Name Id`): string {.inject.} =
    if id == `Empty Name Id`:
      result = "<empty-(" & $id.int & ")-" & astToStr(Name) & "Id>"

    else:
      result = "<" & astToStr(Name) & "-" & $id.int & ">"

  func toIndex*(id: `Name Id`): int {.inject.} =
    assert not isNil(id), $id
    result = int(BaseType(id) - idxOffset)

  func `to Name Id`*(idx: int): `Name Id` {.inject.} =
    result = `Name Id`(idx + idxOffset)

  when addHash:
    func hash(id: `Name Id`): Hash {.inject.} = Hash(id.int)

import std/bitops

proc bits[T](t: T): string =
  let v = cast[uint64](t)
  for bit in countdown(high(BitsRange[T]), 0):
    if testBit(v, bit.int):
      result.add "1"
    else:
      result.add "0"


template declareHighMasking*(
    Name: untyped,
    highMaskRange: static[range[0..64]] = 0,
    BaseType: typed = uint64
  ): untyped =
    const Max = sizeof(BaseType) * 8
    const shift = Max - highMaskRange
    const highMask =
      block:
        var mask: BaseType
        for idx in countdown(Max - 1, Max - highMaskRange):
          mask.setBit(idx)

        mask

    type
      `Name Id Mask`* {.inject.} = distinct BaseType

    func `==`*(m1, m2: `Name Id Mask`): bool {.inject.} =
      BaseType(m1) == BaseType(m2)

    func getMask*(id: `Name Id`): `Name Id Mask` {.inject.} =
      `Name Id Mask`((BaseType(id) and highMask) shr shift)

    func popMask*(id: `Name Id`): `Name Id` {.inject.} =
      `Name Id`((BaseType(id) and not highMask))

    func setMask*(id: var `Name Id`, mask: `Name Id Mask`) {.inject.} =
      id = `Name Id`(BaseType(id) or (mask.BaseType shl shift))


template declareStoreType*(
    ValueName, StoreName, IdName: untyped): untyped {.dirty.} =

  type
    StoreName* = object
      data*: seq[ValueName]

  func len*(store: StoreName): int = store.data.len

  func clear*(store: var StoreName) =
    {.warning[UnsafeSetLen]: off.}
    store.data.setLen(0)
    {.warning[UnsafeSetLen]: on.}

  func `init StoreName`(values: openarray[ValueName]): StoreName =
    {.warning[ProveInit]: off.}
    result = StoreName(data: @values)
    {.warning[ProveInit]: on.}

  func add*(store: var StoreName, item: ValueName): IdName {.discardable.} =
    result = `to IdName`(store.data.len)
    store.data.add item

  func `[]`*(store: var StoreName, index: IdName): var ValueName =
    store.data[toIndex(index)]

  func `[]`*(store: StoreName, index: IdName): ValueName =
    store.data[toIndex(index)]

  iterator items*(store: StoreName): ValueName =
    for item in items(store.data):
      yield item

  iterator mitems*(store: var StoreName): var ValueName =
    for item in mitems(store.data):
      yield item

  iterator pairs*(store: StoreName): (IdName, ValueName) =
    for idx, item in pairs(store.data):
      yield (`to IdName`(idx), item)

  iterator mpairs*(store: var StoreName): (IdName, var ValueName) =
    for idx, item in mpairs(store.data):
      yield (`to IdName`(idx), item)

template declareStoreField*(Type, field, Name: untyped): untyped {.dirty.} =
  func add*(main: var Type, typ: `Name`): `Name Id` = main.field.add typ
  func `[]`*(main: Type, id: `Name Id`): Name = main.field[id]
  func `[]`*(main: var Type, id: `Name Id`): var Name = main.field[id]


macro declareStoredTableField*(Db, field, Name, Typ: untyped): untyped =
  let name = field.strVal().capitalizeAscii()
  let
    (hasName, getName, setName) = (
      ident("has" & name), ident("get" & name), ident("set" & name))

    nameId = ident(Name.strVal() & "Id")

  result = quote do:
    func `hasName`*(main: `Db`, id: `nameId`): bool = id in main.`field`
    proc `getName`*(main: `Db`, id: `nameId`): `Typ` = main.`field`[id]
    func `setName`*(main: var `Db`, id: `nameId`, val: `Typ`) =
      main.`field`[id] = val

    func `setName`*(main: var `Db`, id: `nameId`, val: Option[`Typ`]) =
      if isSome(val):
        main.`field`[id] = get(val)


macro declareStoredTableSeqField*(Db, field, Name, Typ: untyped): untyped =
  let name = field.strVal().capitalizeAscii()
  let
    hasName = ident("has" & name)
    getName = ident("get" & name)
    setName = ident("set" & name)
    addName = ident("add" & name)
    lenName = ident("len" & name)
    nameId = ident(Name.strVal() & "Id")

  result = quote do:
    func `hasName`*(main: `Db`, id: `nameId`): bool = id in main.`field`
    proc `getName`*(main: `Db`, id: `nameId`): seq[`Typ`] = main.`field`[id]
    func `setName`*(main: var `Db`, id: `nameId`, val: seq[`Typ`]) =
      main.`field`[id] = val

    func `addName`*(main: var `Db`, id: `nameId`, val: `Typ`) =
      main.`field`.mgetOrPut(id).add val

    func `lenName`*(main: `Db`, id: `nameId`): int =
      if id in main.`field`:
        return len(main.`field`)



  # echo result.repr()


import std/[macros]

when isMainModule:

  type
    Value = object
      name: string

  declareIdType(Value, addHash = false)
  declareStoreType(Value)
  declareHighMasking(Value, 4)

  var store: ValueStore
  let id = store.add Value()

  store[id].name = "test"

  echo store

  block:
    var id = ValueId(614091)
    echo "value>  ", bits(id)
    id.setMask(ValueIdMask(2))
    echo "masked> ", bits(id)
    echo "mask>   ", bits id.getMask()
    echo "value>  ", bits(id.popMask().int)
