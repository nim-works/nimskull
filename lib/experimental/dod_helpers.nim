import
  std/[
    macros,
    options,
    strutils,
    typetraits
  ]

type
  OptIndex[T] = distinct uint32
    ## The nilable version of an index-like type. Concepts should be used to
    ## restrict for which types ``OptIndex`` is usable, but because of their
    ## not-yet stable nature, they aren't. Instead, the type-operator ``opt``
    ## is used to construct the type.
    ##
    ## The zero-representation of ``OptIndex`` represents the 'nil' state --
    ## this is achieved by shifting the index by 1. This allows for
    ## ``OptIndex`` to have no size overhead compared to just using the
    ## underlying type, but also means that not the whole range can be
    ## used for the index values.
    ##
    ## Because of multiple compiler bugs, ``OptIndex`` right now only works for
    ## index-like types that use ``uint32`` as the underlying type

# XXX: instead of making ``OptIndex`` only available for ``uint32``-based
#      types, the idea was to make ``OptIndex`` a ``distinct Underlying[T]``
#      where ``Underlying`` is a type operator that is defined for each
#      index-like type. Example:
#
#        type Underlying[T: NameOfIndexType] = uint32
#
#      Because of a severe compiler bug, that is currently not possible, as
#      defining a generic alias of a primitive type results in the size
#      information of the primitive type to become unknown. Using a template
#      for the operator also fails to compile

# ----------------- OptIndex implementation -----------------

template opt*[T](t: typedesc[T]): untyped =
  ## A type operator that returns the nilable version of the index-like type
  ## `T`. To indicate that `T` represents an index-like, a tag routine
  ## of the form ``indexLike(typedesc[T]): void`` has to be visible
  mixin indexLike
  when compiles(indexLike(t)):
    when distinctBase(t) is uint32:
      OptIndex[T]
    else:
      {.error: name(t) & " is not ``uint32``-based".}
  else:
    {.error: name(t) & " is not an index-like -- the tag routine is missing"}

template `==`*[T](a, b: OptIndex[T]): bool =
  ## Compares `a` and `b` for equality. If both are 'nil', they're are also
  ## treated as equal
  ord(a) == ord(b)

func isSome*[T](i: OptIndex[T]): bool {.inline.} =
  ## Tests if `i` stores a valid index-like value
  ord(i) != 0

func isNone*[T](i: OptIndex[T]): bool {.inline.} =
  ## Tests if `i` is empty, i.e. doesn't store a valid index-like
  ord(i) == 0

template val[T](x: OptIndex[T]): untyped =
  #(Underlying[T])(x)
  uint32(x)

func `[]`*[T](i: OptIndex[T]): T {.inline.} =
  ## Returns the underlying index value. `i` is required to store a valid
  ## value
  bind val
  assert val(i) != 0, "`i` is empty"
  T(val(i) - 1)

template noneOpt*[T](t: typedesc[T]): untyped =
  ## Returns the value representing 'none' (or 'nil') for the index-like `T`
  # XXX: the template can't be named ``none``, as that would create ambiguities
  #      with the ``none`` routine for ``Option``
  default(opt(t))

func someOpt*[T](i: T): auto {.inline.} =
  ## Returns the index-like value `i` as the corresponding nilable type
  type Opt = opt(T)
  #Opt((Underlying[T])(i) + 1)
  Opt(uint32(i) + 1)

template declareIdType*(
    Name: untyped,
    addHash: static[bool] = false,
    BaseType: typed = uint64
  ): untyped {.dirty.} =

  type
    `Name Id`* = distinct BaseType

  const `Empty Name Id`* = `Name Id`(0)

  func `==`*(i1, i2: `Name Id`): bool = i1.int == i2.int
  func isNil*(i: `Name Id`): bool = i == `Empty Name Id`

  func `$`*(id: `Name Id`): string =
    if id == `Empty Name Id`:
      result = "<empty- " & astToStr(Name) & "Id>"

    else:
      result = "<" & astToStr(Name) & "-" & $id.int & ">"

  func toIndex*(id: `Name Id`): int =
    assert not isNil(id), $id
    result = int(BaseType(id) - 1)

  func `to Name Id`*(idx: int): `Name Id` =
    result = `Name Id`(idx + 1)

  when addHash:
    func hash(id: `Name Id`): Hash = Hash(id.int)

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


template declareStoreType*(Name: untyped): untyped {.dirty.} =
  type
    `Name Store`* = object
      data: seq[Name]

  func len*(store: `Name Store`): int = store.data.len

  func add*(store: var `Name Store`, item: `Name`): `Name Id` =
    result = `to Name Id`(store.data.len)
    store.data.add item

  template `[]`*(store: var `Name Store`, index: `Name Id`): Name =
    store.data[toIndex(index)]

  iterator items*(store: `Name Store`): Name =
    for item in items(store.data):
      yield item

  iterator mitems*(store: var `Name Store`): var Name =
    for item in mitems(store.data):
      yield item

  iterator pairs*(store: `Name Store`): (`Name Id`, Name) =
    for idx, item in pairs(store.data):
      yield (`to Name Id`(idx), item)

  iterator mpairs*(store: var `Name Store`): (`Name Id`, var Name) =
    for idx, item in mpairs(store.data):
      yield (`to Name Id`(idx), item)

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
