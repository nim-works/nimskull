## The definitions for the type representation used by the compiler back-end (mid-end?) IR.

import
  std/[
    algorithm,
    hashes,
    intsets,
    packedsets,
    tables
  ],
  compiler/front/[
    options, msgs
  ],
  compiler/ast/[
    ast_types,
    ast,
    idents,
    types
  ],
  compiler/utils/[
    ropes
  ]

import std/options as stdoptions
from compiler/vm/vmdef import unreachable

const useGenTraces {.booldefine.} = false

type RecordNodeKind* = enum
  rnkEmpty # meant to be used by the garbage collector to fill cleaned slots
  rnkList
  rnkFields
  rnkCase
  rnkBranch

type RecordNode* = object
  kind*: RecordNodeKind
  len*: uint32 ## the number of items
  a*: uint32   ##
  b*: uint32

type RecordNodeIndex* = distinct uint32
type TypeNodeIndex = uint32
type CanonTypeId* = uint32

type RecordId* = distinct uint32

type TypeId* = distinct uint32

type SymId* = distinct uint32

type TypeNodeKind* = enum
  tnkEmpty

  tnkVoid

  tnkBool
  tnkChar

  tnkInt
  tnkUInt
  tnkFloat

  tnkRef
  tnkPtr
  tnkVar
  tnkLent

  tnkSeq
  tnkOpenArray
  tnkString
  tnkCString
  tnkUncheckedArray
  #tnkSink # XXX: ?

  tnkSet

  tnkRecord # tuples and objects
  tnkArray

  tnkProc ## the type of procedure value
  tnkClosure ## the type of a closure object (procedure + environment
             ## reference)

  tnkTypeDesc ## only relevant for compile-time function evaluation

  tnkName # a reference to named type

  #tnkAlias

type FieldDesc* = object
  sym*: SymId # may be empty
  typ: TypeNodeIndex
  # XXX: bitsize should likely be stored as part of FieldDesc

type FieldId* = distinct uint32

type TypeNode* = object
  kind: TypeNodeKind
  a: uint32
  b: uint32

# XXX: there actually exist two kinds of types of which the backend cares
#      about the distinction:
#      * "declared type": the entity that is defined in a 'type' section in the source code. Not relevant at the IR-level, only for the code-generators. Represents information such as the type's name, if it's imported, which C-header it depends on, etc.
#      * type (haven't found a good name for this one yet): the raw type information used by the IR


type Type* = object
  kind*: TypeNodeKind
  base: TypeId
  a*: uint32
  b*: uint32
  c*: uint32 # for records, a ``RecordNodeIndex``
  sig: seq[TypeNodeIndex] # for procedures

# XXX: obsolete, but the idea was good
#[
type
  DeclType* = object
    ## A `DeclType`  is a direct translation from ``PType``. It corresponds to
    ## the types defined in the source-code.
    ##
    ## .. code-block:: nim
    ##
    ##   type
    ##     A = int        # Both `A` and `int`
    ##     B = object     # `B`
    ##       x: int       # `int`
    ##   var x: seq[int]  # `seq[int]`
    ##   var y: var int   # `var int`
    ##
    ## `TypeDecl` links a canonical type with an "interface" and extra data.
    ## A canonical type is the type representation used at the IR-level.
    ## The "interface" has the information necessary for the code-generator to
    ## handle imported and exported types (e.g. the imported name).
    ## The extra data are things that aren't needed for the code-generator or
    ## the IR processing to work (e.g. the name), but would, for example, help
    ## with generating easier to read code

    # TODO: maybe rename to `DeclaredType`?

    canonical: TypeNodeIndex

    # XXX: the "interface" and extra part are both combined into a `PSym` for
    #      now. This makes the first implementation simpler and also helps
    #      with reducing memory usage
    decl: PSym
]#


type TypeEnv* = object
  ## Holds the data for all types
  # XXX: in general, a `seq[seq]` could be used for `records`, `fields`, and
  #      `types`. This would make access a bit simpler; require less copying
  #      on resize; and make garbge collection easier. It would also increase
  #      memory fragmentation and reduce cache locality
  records: seq[RecordNode] ## the bodies for all record-like types (objects and tuples) in one contiguous seq
  fields: seq[FieldDesc] ## all fields
  types: seq[Type] ## all types in one contiguous seq

  attachMap: Table[TypeId, int32]
  attachments: seq[tuple[name: PIdent, forceName: bool]] # TODO: should use ``DeclarationV2``
  ifaces: Table[TypeId, PSym]

  # XXX: currently maps a unique structural represnetation of a type to it's
  #      ID. What we actually want is a `seq[(Hash, TypeNodeIndex)]` which is
  #      basically one half of a ``BiTable`` but that would require yet
  #      another duplicate of the low-level ``Table`` implementation
  structMap: Table[seq[int], TypeNodeIndex]

  proxies: seq[TypeId] ## redirection for each type. If the slot has a value of
    ## ``NoneType``, the corrsponding type is not a proxy. Proxies are
    ## created/needed when replacing types, since the replaced type might have
    ## other data attached to it's ID.

  typdescs: Table[ItemId, PType] # type-id -> a `tyTypeDesc` type

type TypeLookup* = object
  ## Data needed for mapping ``PType`` to the ``TypeId``

type
  DeferredTypeGen* = object
    map: Table[ItemId, TypeId] # type-id -> ``TypeId``
    list: seq[PType] ## the list of deferred types in the order they were requested

    data: seq[TypeId]

    voidType*: PType ## a ``PType`` of kind ``tyVoid``. Requesting a nil type
                     ## is remapped to a request using this type
    charType*: PType

    marker: IntSet
    cache: Table[ItemId, TypeNodeIndex] # caches which canonical type a `PType` maps to

    when useGenTraces:
      traceMap: seq[int] ## stores the corresponding trace index for each
                         ## entry in `list`

      trace: int
      traces: seq[seq[StackTraceEntry]]
      isInGen: bool

    nextTypeId: uint32

  Declaration* = object
    name*: string # the name to used for the declaration in the output of the
                 # code-generators. If `forceName` is false, the name may be
                 # escaped if deemed necessary. `name` is allowed be empty.
    forceName*: bool
    header*: string # only needed for the C-backend. # TODO: this need to be handled differently. Will likely use the attachment strategy

  DeclarationV2* = object
    # TODO: maybe use a different name? E.g. CodeGenEntity, CodeGenDesc?

    name*: PIdent # same as for ``Declaration.name``
    forceName*: bool
    omit*: bool # don't emit any code for to the entity this declaration is attached to. XXX: this doesn't really fit here

    format*: string # formatting string. May be empty (in case special
                    # formatting is to be applied). Only relevant for
                    # source-based code-generators.

  SymInterfaceDef* = object
    header*: string

  Symbol* = object
    ## The symbol representation used by the backend

    # XXX: using one type (i.e. `Symbol` to describe procedures, locals,
    #      globals and constants) might be the wrong approach for the backend.
    #      They all require different kinds of information and while using an
    #      opaque handle and attaching data to it separately works, it's
    #      probably a better idea to put them in fully separate namespaces.
    #      This would also allow eaiser dependency scanning without requiring
    #      and indirection (at the cost of more enum values in `IrNodeKind3`)

    kind*: TSymKind
    typ*: TypeId

    flags*: TSymFlags

    magic*: TMagic
    position*: int # inherited from `TSym`, might be removed/replaced

    # TODO: store the declarations separately
    decl*: DeclarationV2

  SymbolEnv* = object
    # TODO: maybe rename?

    symbols*: seq[Symbol]

    # TODO: maybe split off `map` into a separate `SymbolLookup` type?
    map: Table[ItemId, SymId] # ``PSym``-id -> ``SymId``

    # XXX: `orig` will likely be removed/replaced later on
    orig*: Table[SymId, PSym] # stores the associated ``PSym`` for a symbol. Currently meant to be used by the code-generators.

  ProcId* = distinct uint32

  ProcParam = tuple
    name: PIdent
    typ: TypeId

  ProcHeader* = object
    ## At the IR-level, there is no distinction done between ``func``s,
    ## ``proc``s, ``iterator``s, and ``method``s. They are all treated as a
    ## "procedure" and work the same.

    params*: seq[ProcParam]
    returnType*: TypeId
    envType*: TypeId ## the ``ref`` type of the captured environment, or
      ##``NoneType`` if the procedure doesn't capture an environment

    callConv*: TCallingConvention
    magic*: TMagic

    flags*: TSymFlags # XXX: uses `TSymFlags` for now, but this will be changed to something else later on

    # XXX: each procedure requires a ``Declaration``, so it's stored as part of
    #      the type in order to avoid indirections via a ``DeclId`` (or similar).
    #      Since a ``Declaration`` object is quite large, this does mean that
    #      less ``ProcHeader``s fit into a cache-line. The declration is only
    #      needed by code-generators so it likely makes sense to move the
    #      declaration into a separate seq in ``ProcedureEnv``
    decl*: DeclarationV2
    iface*: SymInterfaceDef

  ProcedureEnv* = object
    # TODO: maybe rename
    procs: seq[ProcHeader]
    map: Table[PSym, ProcId]
    orig*: Table[ProcId, PSym]

const NoneType* = TypeId(0)
const NoneSymbol* = SymId(0)

const ProcedureLike = {tnkProc, tnkClosure}

# XXX: copied from `ccgtypes`, might need some adjustments
# note: we DO care for ``tyDistinct``s, since they act as nominal types and
#       type-bound operations can be attached to them
const
  irrelevantForBackend = {tyGenericBody, tyGenericInst, tyGenericInvocation,
                          tyRange, tyStatic, tyAlias, tySink, tyInferred,
                          tyOwned}

func `==`*(a, b: TypeId): bool {.borrow.}
func `==`*(a, b: SymId): bool {.borrow.}
func `==`*(a, b: ProcId): bool {.borrow.}

func `inc`*(a: var RecordNodeIndex, val: int = 1) {.borrow.}

type SomeId = TypeId | SymId | RecordId | FieldId | ProcId

template toIndex*(id: SomeId): uint32 =
  id.uint32 - 1

template toId[T: SomeId](index: Natural, id: typedesc[T]): T =
  T(index + 1)

func `[]`*(e: SymbolEnv, s: SymId): lent Symbol =
  e.symbols[s.int - 1]

const TypeIdKindBit = 1'u32 shl 31
const TypeIdMask = 0x7FFFFFFF

template isDecl(id: TypeId): bool =
  (id.uint32 and TypeIdKindBit) == 0

template isNode(id: TypeId): bool =
  (id.uint32 and TypeIdKindBit) != 0

template maskedId[T](id: TypeId, typ: typedesc[T]): T =

  # if the kind bit is set (i.e. the type-id is for a node) the value after
  # applying the ``TypeIdKindBit`` mask is equal to the mask, otherwise it's
  # '0'. Subtracting 1 will in both cases result in a bitmask that will
  # eliminate the kind-bit
  let idVal = id.uint32
  cast[T](idVal and ((idVal and TypeIdKindBit) - 1))

template toId(idx: TypeNodeIndex): TypeId =
  TypeId(idx + 1)#discard TypeId(idx or TypeIdKindBit)

func nodeId(e: TypeEnv, id: TypeId): TypeNodeIndex {.inline.} =
  assert id != NoneType
  #if isNode(id):
  toIndex(id)
  #TypeNodeIndex(id.uint32 and TypeIdMask)
  #else:
  #  e.decls[toIndex(id)].canonical

func `[]`*(e: TypeEnv, t: TypeId): lent Type =
  e.types[t.toIndex]#nodeId(e, t)]

func `[]`*(e: TypeEnv, f: FieldId): lent FieldDesc =
  e.fields[f.int - 1]

func field*(e: TypeEnv, f: Natural): lent FieldDesc =
  e.fields[f]

func `[]`*(e: TypeEnv, i: RecordNodeIndex): lent RecordNode =
  e.records[i.int]

func `[]`*(e: TypeEnv, i: RecordId): lent RecordNode =
  e.records[toIndex(i)]

func `[]`*(e: ProcedureEnv, i: ProcId): lent ProcHeader {.inline.} =
  e.procs[toIndex(i)]

func getReturnType*(e: TypeEnv, t: TypeId): TypeId =
  ## Returns the return type of the given procedure type `t`
  assert e[t].kind in ProcedureLike, $e[t].kind
  e[t].sig[0].toId

func elemType*(e: TypeEnv, t: TypeId): TypeId =
  e[t].base

func baseType*(e: TypeEnv, t: TypeId): TypeId =
  e[t].base

func numFields*(n: RecordNode): int =
  assert n.kind == rnkList
  n.a.int

func base*(t: Type): TypeId =
  t.base

func kind*(t: Type): TypeNodeKind {.inline.} =
  t.kind

func fieldStart*(t: Type): int {.inline.} =
  ## Returns the *index* (not the ID) of the first field for record types
  assert t.kind == tnkRecord
  t.a.int

func base*(e: TypeEnv, id: TypeId): TypeId =
  e[id].base

func iface*(e: TypeEnv, id: TypeId): PSym {.inline.} =
  e.ifaces.getOrDefault(id, nil)

func kind*(e: TypeEnv, id: TypeId): TypeNodeKind {.inline.} =
  e.types[nodeId(e, id)].kind

func typ*(f: FieldDesc): TypeId {.inline.} =
  f.typ.toId

func record*(t: Type): RecordId =
  assert t.kind == tnkRecord
  t.c.RecordId

type Fields* = distinct Slice[uint32]

iterator pairs*(f: Fields): (int, FieldId) =
  let
    o = Slice[uint32](f)
    L = o.len
  var i = 0
  while i < L:
    yield (i, toId(o.a + i.uint32, FieldId))
    inc i

func fields*(env: TypeEnv, id: TypeId): Fields =
  let t = env[id]
  assert t.kind == tnkRecord
  Fields((t.a + 1) .. (t.a + env[t.record].a - 1))

func `[]`*(f: Fields, x: Natural): FieldId {.inline.} =
  let o = Slice[uint32](f)
  assert x <= o.b.int
  toId(o.a.int + x, FieldId)

func numFields*(env: TypeEnv, t: TypeId): int =
  assert env[t].kind == tnkRecord
  env[env[t].record].a.int

func skipVarOrLent*(env: TypeEnv, t: TypeId): TypeId =
  if env[t].kind in {tnkVar, tnkLent}:
    # a ``var var T`` (same with ``lent``) is not possible so no need to use
    # a while loop
    env[t].base
  else:
    t


func combine(lo, hi: uint32): uint64 {.inline.}

# TODO: use ``BiggestUInt`` here
func length*(e: TypeEnv, t: TypeId): uint =
  assert e[t].kind in {tnkArray, tnkSet}
  combine(e[t].a, e[t].b).uint

func callConv*(e: TypeEnv, t: TypeId): TCallingConvention =
  assert e[t].kind == tnkProc
  e[t].a.TCallingConvention

func callConv*(t: Type): TCallingConvention =
  assert t.kind == tnkProc
  t.a.TCallingConvention

func param*(e: TypeEnv, t: TypeId, i: BackwardsIndex): TypeId =
  assert e[t].kind in ProcedureLike
  e[t].sig[i].toId

func param*(e: TypeEnv, t: TypeId, i: Natural): TypeId =
  assert e[t].kind in ProcedureLike
  e[t].sig[i].toId

func size*(t: Type): uint =
  assert t.kind in {tnkFloat, tnkInt, tnkUInt}
  t.a.uint

func length*(t: Type): uint =
  assert t.kind in {tnkSet, tnkArray}
  t.a.uint

iterator params*(e: TypeEnv, t: TypeId): TypeId =
  let typ = e[t]
  assert typ.kind in ProcedureLike
  for i in 1..<typ.sig.len:
    yield typ.sig[i].toId

func numParams*(e: TypeEnv, t: TypeId): int =
  assert e[t].kind in ProcedureLike
  e[t].sig.len - 1

func getSize*(e: TypeEnv, t: TypeId): uint =
  ## the size in bytes
  assert e[t].kind == tnkSet
  e[t].a.uint

func nthField*(e: TypeEnv, id: TypeId, i: Natural): FieldId =
  var t = nodeId(e, id)
  while true:
    let typ = e.types[t]
    if typ.b.int <= i:
      let i = i - typ.b.int
      assert e[typ.c.RecordId].kind == rnkList
      assert i < e[typ.c.RecordId].numFields, $e[typ.c.RecordId].numFields
      return FieldId(typ.a.int + i + 1)
    elif typ.base != NoneType:
      t = typ.base.toIndex
    else:
      return FieldId(0) # TODO: use a `NoneField`

func findField*(e: TypeEnv, t: TypeId, i: Natural): tuple[id: FieldId, steps: int] =
  ## Searches for the field with index `i` in the given record type `t`.
  ## If found, returns the field's ID together with the number of base types
  ## that were traversed (e.g. '0' means the field is in the given record, '1'
  ## that it's in the record's base type, etc.).
  ##
  ## If not found, returns a 'none' field ID.
  var t = nodeId(e, t)

  while true:
    let typ = e.types[t]
    if typ.b.int <= i:
      let i = i - typ.b.int
      assert e[typ.c.RecordId].kind == rnkList
      assert i < e[typ.c.RecordId].numFields
      result.id = toId(typ.a.int + i, FieldId)
      return
    elif typ.base != NoneType:
      t = typ.base.toIndex
      inc result.steps
    else:
      return (FieldId(0), 0) # TODO: use a `NoneField`

func skipTypesConsiderImported(t: PType, kinds: TTypeKinds): tuple[imported: bool, t: PType] =
  result.t = t
  while result.t.kind in kinds:
    result.imported = t.sym != nil and sfImportc in t.sym.flags
    if result.imported:
      return
    result.t = lastSon(result.t)

when useGenTraces:
  func requestTrace(gen: var DeferredTypeGen): int =
    if gen.isInGen:
      gen.trace
    else:
      {.noSideEffect.}:
        gen.traces.add getStackTraceEntries()
      gen.traces.high

template overlaps(a: set, b: set): bool =
  a * b != {}

func requestType*(gen: var DeferredTypeGen, t: PType): TypeId =
  var t = t
  if t == nil:
    t = gen.voidType

  # skip all types that we are not interested in on the IR level. This does
  # however mean that the original type name is lost and the code-generators
  # can't make use of it.
  # For the version this comment was written against the skipping reduced the
  # number of genrated types by 5%, the record nodes by 50%, and the fields
  # by 30%
  while true:
    if t.sym != nil and sfImportc in t.sym.flags:
      # don't skip types that have an external interface attached
      break

    if t.kind in irrelevantForBackend:
      t = t.lastSon
    else:
      break

  #[
  if gen.sysTypes != nil and (t.sym == nil or not t.sym.flags.overlaps({sfImportc, sfExportc})):
    result = gen.sysTypes[][t.kind]
    if result != NoneType:
      return
      ]#
  let next = TypeId((gen.nextTypeId + 1) or TypeIdKindBit)
  result = gen.map.mgetOrPut(t.itemId, next)
  if result == next:
    gen.list.add(t)
    when useGenTraces:
      gen.traceMap.add requestTrace(gen)

    inc gen.nextTypeId

func lookupType*(gen: DeferredTypeGen, t: ItemId): TypeId =
  gen.map.getOrDefault(t, NoneType)

func requestSym*(e: var SymbolEnv, s: PSym): SymId =
  # TODO: a deferred approach is probably a better idea here. That is, just
  #       reserve a slot here, remember the `PSym` for the slot, and return
  #       the slot's id. The callsite of `requestSym` doesn't need
  #       to have access to everything required for creating a ``Symbol`` from
  #       a `PSym` then.
  assert s.kind notin routineKinds

  let next = SymId(e.symbols.len + 1) # +1 since ID '0' is reserved for indicating 'none'

  result = e.map.mgetOrPut(s.itemId, next)
  if result == next:
    # a not yet translated symbol
    e.symbols.add(Symbol(kind: s.kind, position: s.position, magic: s.magic, flags: s.flags))
    e.symbols[^1].decl.name = s.name
    e.orig[result] = s

type TypeGen = object
  syms: SymbolEnv

  cache: Table[ItemId, TypeNodeIndex] # caches which canonical type a `PType` maps to

  def: ptr DeferredTypeGen

func collectDeps(list: var seq[PType], marker: var IntSet, t: PType)

func collectDeps(list: var seq[PType], marker: var IntSet, n: PNode) =
  case n.kind
  of nkSym:
    collectDeps(list, marker, n.sym.typ)
  of nkRecList:
    for it in n:
      collectDeps(list, marker, it)

  of nkRecCase:
    collectDeps(list, marker, n[0])
    for i in 1..<n.len:
      collectDeps(list, marker, lastSon(n[i]))

  else:
    unreachable(n.kind)

const NominalTypes = {tyObject} ## ``PType``-kinds that are considered to be
  ## nominal types in the context of type translation

func collectDeps(list: var seq[PType], marker: var IntSet, t: PType) =
  let t = t.skipTypes(irrelevantForBackend)

  # TODO: imported types must not be skipped here.

  if t.kind == tyObject and marker.containsOrIncl(t.id):
    return

  if t.kind == tyObject and t.n != nil:
    collectDeps(list, marker, t.n)

  for it in t.sons:
    if it != nil:
      collectDeps(list, marker, it)

  case t.kind
  of tyObject: list.add t
  of tyTyped:
    # because of ``varargs[typed]`` (used by ``system.echo``) we have to guard
    # against ``typed`` here
    discard
  else:
    if not marker.containsOrIncl(t.id):
      list.add t

func requestType(gen: TypeGen, t: PType): TypeNodeIndex =
  result = gen.cache[t.skipTypes(irrelevantForBackend).itemId]

type SOmeErr = object of CatchableError
  info: TLineInfo

func addField(dest: var TypeEnv, g: var TypeGen, s: PSym,
              numFields: var int): tuple[fields, entries: int] =
  if dest.records[^1].kind == rnkFields:
    # append to the active field section
    inc dest.records[^1].b
  else:
    # a new field section
    dest.records.add RecordNode(kind: rnkFields, a: numFields.uint32, b: numFields.uint32)
    result.entries = 1

  # the order in which types are translated is such that each dependency was
  # already translated before types referencing it, so we can directly look up
  # the ID in the cache.
  dest.fields.add(FieldDesc(sym: g.syms.requestSym(s),
                            typ: g.requestType(s.typ)))

  inc numFields
  result.fields = 1

func translate(dest: var TypeEnv, g: var TypeGen, n: PNode, numFields: var int): tuple[fields, entries: int] =
  func `+=`(a: var (int, int), b: (int, int)) {.inline.} =
    a[0] += b[0]
    a[1] += b[1]

  case n.kind
  of nkSym:
    result += addField(dest, g, n.sym, numFields)
  of nkRecList:
    let start = dest.records.len
    dest.records.add RecordNode(kind: rnkList)

    for it in n.sons:
      result += translate(dest, g, it, numFields)

    dest.records[start].len = result.entries.uint32
    dest.records[start].a = numFields.uint32#result.fields.uint32

    result.entries = 1
  of nkRecCase:
    dest.records.add RecordNode(kind: rnkCase, len: n.len.uint32)
    result.entries = 1

    discard addField(dest, g, n[0].sym, numFields) # discriminator

    for i in 1..<n.len:
      let start = dest.records.len
      dest.records.add RecordNode(kind: rnkBranch)

      let r = translate(dest, g, lastSon(n[i]), numFields)
      dest.records[start].len = r.entries.uint32

  else:
    unreachable(n.kind)

func split(x: uint64): tuple[lo, hi: uint32] {.inline.} =
  result.lo = uint32(x and 0xFFFFFFFF'u64)
  result.hi = uint32(x shr 32)

func combine(lo, hi: uint32): uint64 {.inline.} =
  result = lo.uint64 or (hi.uint64 shl 32)

proc translate(dest: var TypeEnv, gen: var TypeGen, conf: ConfigRef, pos: Natural, t: PType) =
  # XXX: translate needs to be a `proc` since ``getSize`` possibly mutates `t`

  assert t != nil

  case t.kind
  of tyVoid:
    dest.types[pos] = Type(kind: tnkVoid)
  of tyBool:
    dest.types[pos] = Type(kind: tnkBool)
  of tyChar:
    dest.types[pos] = Type(kind: tnkChar)
  of tyFloat..tyFloat128:
    dest.types[pos] = Type(kind: tnkFloat, a: getSize(conf, t).uint32 * 8)
  of tyUInt..tyUInt64:
    dest.types[pos] = Type(kind: tnkUInt, a: getSize(conf, t).uint32 * 8)
  of tyInt..tyInt64:
    dest.types[pos] = Type(kind: tnkInt, a: getSize(conf, t).uint32 * 8)

  of tyEnum:
    # enums are lowered to integers already
    if firstOrd(conf, t) < 0:
      # if the first possible value for an enum is negative, the enum type maps
      # to an int32. This is compatible with what the old C
      # code-generator did.
      dest.types[pos] = Type(kind: tnkInt, a: getSize(conf, t).uint32 * 8)
    else:
      dest.types[pos] = Type(kind: tnkUInt, a: getSize(conf, t).uint32 * 8)

  of tySet:
    # sets are not yet lowered, as some code-generators have dedicated
    # handling for sets
    let (lo, hi) = lengthOrd(conf, t).toUInt64().split()

    dest.types[pos] = Type(kind: tnkSet, a: lo, b: hi)

  of tyObject:
    dest.types[pos] = Type(kind: tnkRecord, a: dest.fields.len.uint32, b: 0,
                           c: RecordId(dest.records.len + 1).uint32)
    var tmp: int
    discard translate(dest, gen, t.n, tmp)

    if t[0] != nil:
      let base = gen.cache[t[0].skipTypes(skipPtrs).itemId]
      dest.types[pos].base = base.toId

      # fill in the information about the first field's position
      let bt = dest.types[base]
      dest.types[pos].b = bt.b + dest[bt.record].numFields.uint32

  of tyTuple:
    dest.types[pos] = Type(kind: tnkRecord, a: dest.fields.len.uint32, b: 0,
                           c: RecordId(dest.records.len + 1).uint32)

    block:
      dest.records.add RecordNode(kind: rnkList, len: 1, a: t.len.uint32)
      dest.records.add RecordNode(kind: rnkFields, a: 0, b: uint32(t.len - 1))

      let start = dest.fields.len
      dest.fields.setLen(start + t.len)
      for i in 0..<t.len:
        dest.fields[start + i].typ = gen.requestType(t[i])


  of tyArray:
    let (lo, hi) = lengthOrd(conf, t).toUInt64().split()
    dest.types[pos] = Type(kind: tnkArray, base: gen.requestType(t.elemType).toId, a: lo, b: hi)

  of tyString:
    # in order to simplify the IR passes, the string type uses ``char`` as the
    # base type. This makes it possible to use ``elemType`` on the string type
    dest.types[pos] = Type(kind: tnkString, base: gen.requestType(gen.def.charType).toId)

  of tyCstring:
    dest.types[pos] = Type(kind: tnkCString, base: gen.requestType(gen.def.charType).toId)

  of tyRef, tyPtr, tyVar, tyLent, tySequence, tyOpenArray, tyUncheckedArray:
    const Map = {tyRef: tnkRef, tyPtr: tnkPtr, tyVar: tnkVar, tyLent: tnkLent, tySequence: tnkSeq, tyOpenArray: tnkOpenArray, tyUncheckedArray: tnkUncheckedArray}.toTable
    dest.types[pos] = Type(kind: Map[t.kind], base: gen.requestType(t.lastSon).toId)

  of tyVarargs:
    # HACK: `echo` uses `varargs[typed]` as the parameter and we have to work
    #        around that here
    let elem =
      if t.lastSon.kind == tyTyped: gen.def.voidType
      else: t.lastSon

    dest.types[pos] = Type(kind: tnkOpenArray, base: gen.requestType(elem).toId)

  of tyNil, tyPointer:
    # treat an untyped pointer as a `ptr void`
    dest.types[pos] = Type(kind: tnkPtr, base: gen.requestType(gen.def.voidType).toId)

  of tyProc:
    let kind =
      if t.callConv == ccClosure: tnkClosure
      else: tnkProc

    var r = Type(kind: kind, a: t.callConv.ord.uint32)
    r.sig.newSeq(t.len)

    r.sig[0] = gen.requestType:
      if t[0] != nil: t[0]
      else:           gen.def.voidType

    for i in 1..<t.len:
      r.sig[i] = gen.requestType(t[i])

    dest.types[pos] = move r

  of tyDistinct:
    translate(dest, gen, conf, pos, t.skipTypes(irrelevantForBackend + {tyDistinct}))

  of tyTypeDesc:
    let itId = t.itemId
    dest.typdescs[itId] = t
    dest.types[pos] = Type(kind: tnkTypeDesc, a: cast[uint32](itId.module), b: cast[uint32](itId.item))

  else:
    if t.sym != nil:
      debugEcho conf.toFileLineCol(t.sym.info)
    unreachable(t.kind)

func hash(e: TypeEnv, hcode: var Hash, se: var seq[int], typ: Type)

iterator sliceIt[T](x: seq[T], first, last: int): (int, lent T) =
  var i = 0
  let L = last
  while i < L:
    yield (i, x[i])
    inc i

proc addPrimitiveType*(env: var TypeEnv, gen: var DeferredTypeGen, conf: ConfigRef, t: PType): TypeId =
  # XXX: might be a good idea to merge ``marker`` with ``cache``
  gen.marker.incl t.id

  var ctx: TypeGen
  ctx.def = addr gen
  swap(gen.cache, ctx.cache)

  env.types.setLen(env.types.len + 1)
  translate(env, ctx, conf, env.types.high, t)

  swap(gen.cache, ctx.cache)

  var hcode: Hash
  var se: seq[int]
  hash(env, hcode, se, env.types[^1])

  let
    idx = env.types.high.TypeNodeIndex
    r = env.structMap.mgetOrPut(se, idx)

  # we also do deduplication here in order to make ``int``/``float`` handling
  # simpler
  if r != idx:
    # already exists --> rewind
    env.types.setLen(env.types.len - 1)

  result = toId(r, TypeId)
  gen.cache[t.itemId] = r

proc flush*(gen: var DeferredTypeGen, env: var TypeEnv, symEnv: var SymbolEnv, conf: ConfigRef) =
  ## Generates all the types that were deferred. `types` may be
  ## re-used after calling this procedure.

  var ctx: TypeGen
  ctx.def = addr gen

  swap(symEnv, ctx.syms)
  defer: swap(symEnv, ctx.syms)

  when useGenTraces:
    gen.isInGen = true

  var total: seq[PType]

  swap(gen.cache, ctx.cache)

  for t in gen.list.items:
    collectDeps(total, gen.marker, t)

  for t in total.items:
    if t.kind == tyObject:
      ctx.cache[t.itemId] = env.types.len.TypeNodeIndex
      env.types.add default(Type)

  for i, t in total.pairs:
    let
      pt = env.types.len
      pr = env.records.len
      pf = env.fields.len

    var canonicalize = false

    var idx: int
    if t.kind == tyObject:
      idx = ctx.cache[t.itemId].int

      # create an attached declaration
      env.attachMap[idx.TypeNodeIndex.toId] = env.attachments.len.int32
      env.attachments.add (t.sym.name, sfExportc in t.sym.flags)

    else:
      idx = env.types.len
      env.types.add default(Type)

      # do not deduplicate imported types
      canonicalize = t.sym == nil or sfImportc notin t.sym.flags

    if t.sym != nil and sfImportc in t.sym.flags:
      env.ifaces[idx.TypeNodeIndex.toId] = t.sym

    translate(env, ctx, conf, idx, t)

    var id = idx.TypeNodeIndex

    # don't commit duplicate structural types
    if canonicalize:
      # XXX: enum types are nominal types too, but they're already lowered to
      #      ints. Maybe the latter is not a good idea?
      var hcode: Hash
      var se: seq[int]
      hash(env, hcode, se, env.types[^1])
      let prevId = env.structMap.mgetOrPut(se, id)

      if prevId != id:
        # a duplicate --> undo the changes
        assert env.types.len == pt + 1
        env.types.setLen(pt)
        env.records.setLen(pr)
        env.fields.setLen(pf)

        id = prevId

    if t.kind != tyObject:
      ctx.cache[t.itemId] = id

  let start = gen.data.len
  gen.data.setLen(start + gen.list.len)

  for i, t in gen.list.pairs:
    gen.data[start + i] = ctx.requestType(t).toId

  assert gen.nextTypeId.int == gen.data.len

  swap(gen.cache, ctx.cache)

  when false:
    debugEcho "after flush: "
    debugEcho "  ", env.types.len
    debugEcho "  ", env.records.len
    debugEcho "  ", env.fields.len

  when useGenTraces:
    gen.isInGen = false
    gen.traceMap.setLen(0)
    gen.traces.setLen(0)

  # support re-using
  gen.list.setLen(0)


func getAttachmentIndex*(e: TypeEnv, id: TypeId): Option[int] =
  let i = e.attachMap.getOrDefault(id, -1)
  if i != -1: some(i.int)
  else:       none(int)

func getAttachment*(e: TypeEnv, i: Natural): auto {.inline.} =
  e.attachments[i]

func addSym*(e: var SymbolEnv, kind: TSymKind, typ: TypeId, name: PIdent, flags: TSymFlags = {}): SymId =
  # XXX: temporary helper
  e.symbols.add(Symbol(kind: kind, typ: typ, flags: flags, decl: DeclarationV2(name: name)))
  result = e.symbols.len.SymId

func addMagic*(e: var ProcedureEnv, typ: TypeId, name: PIdent, m: TMagic): ProcId =
  # XXX: temporary helper
  e.procs.add ProcHeader(returnType: typ, magic: m, decl: DeclarationV2(name: name))
  result = e.procs.len.ProcId

iterator items*(e: SymbolEnv): SymId =
  var i = 0
  let L = e.symbols.len
  while i < L:
    yield toId(i, SymId)
    inc i

iterator msymbols*(e: var SymbolEnv): (SymId, var Symbol) =
  var i = 0
  let L = e.symbols.len
  while i < L:
    yield (SymId(i + 1), e.symbols[i])
    inc i

iterator mtypes*(e: var TypeEnv): (TypeId, var Type) =
  var i = 0
  let L = e.types.len
  while i < L:
    yield (toId(i, TypeId), e.types[i])
    inc i


iterator types*(e: TypeEnv): TypeId =
  for i in 0..<e.types.len:
    yield toId(i, TypeId)

iterator fields*(e: TypeEnv, t: Type): lent FieldDesc =
  assert t.kind == tnkRecord

func containsOrIncl(e: var TypeEnv, t: Type, idx: Natural): (bool, TypeNodeIndex) =
  var se: seq[int]
  var hcode: Hash
  hash(e, hcode, se, t)

  let idx = idx.TypeNodeIndex
  result[1] = e.structMap.mgetOrPut(se, idx)
  # if the retrieved index is not the same as the one passed to us, the type
  # is already present in `e`
  result[0] = result[1] != idx

func add(e: var TypeEnv, t: sink Type): TypeNodeIndex {.inline.} =
  result = e.types.len.TypeNodeIndex
  e.types.add t

func getOrPut(e: var TypeEnv, t: sink Type): TypeId =
  ## If the given structural type already exists, the ID of the existing instance is returned. Otherwise, `t` is added to the environment and a new ID is returned.
  let (exists, idx) = containsOrIncl(e, t, e.types.len)
  if not exists:
    e.types.add t

  result = idx.toId

func genRecordType*(e: var TypeEnv, base: TypeId, fields: varargs[(SymId, TypeId)]): Type =
  result.kind = tnkRecord
  result.a = e.fields.len.uint32

  if base != NoneType:
    result.base = base
    result.b = e[base].b + e.numFields(base).uint32
  else:
    discard "that's a problem"
    #result.base = #

  result.c = toId(e.records.len, RecordId).uint32

  # TODO: use `setLen` + []
  for s, t in fields.items:
    e.fields.add FieldDesc(sym: s, typ: nodeId(e, t))

  e.records.add RecordNode(kind: rnkList, len: 1, a: fields.len.uint32)
  e.records.add RecordNode(kind: rnkFields, a: 0, b: fields.high.uint32)

# TODO: genX is the wrong terminology here
func genArrayType*(e: var TypeEnv, len: BiggestUInt, elem: TypeId): Type =
  let s = split(len.uint64)
  Type(kind: tnkArray, base: elem, a: s.lo, b: s.hi)

func requestArrayType*(e: var TypeEnv, len: BiggestUInt, elem: TypeId): TypeId =
  getOrPut(e, genArrayType(e, len, elem))

func requestRecordType*(e: var TypeEnv, base: TypeId, fields: varargs[(SymId, TypeId)]): TypeId =
  # TODO: instead of inserting the type first, calculating the hash, and then
  #       rewinding if it's a duplicate, calculate the hash first and only
  #       then call ``genRecordType``
  let
    pr = e.records.len
    pf = e.fields.len
    t = genRecordType(e, base, fields)

  let (exists, idx) = containsOrIncl(e, t, e.types.len)
  if exists:
    # already exists -> rewind
    e.records.setLen(pr)
    e.fields.setLen(pf)
  else:
    e.types.add t

  result = toId(idx)

func genGenericType*(e: TypeEnv, kind: TypeNodeKind, elem: TypeId): Type =
  assert kind in {tnkUncheckedArray, tnkSeq, tnkVar, tnkLent, tnkPtr, tnkRef, tnkOpenArray}
  Type(kind: kind, base: elem)

func requestGenericType*(e: var TypeEnv, kind: TypeNodeKind, elem: TypeId): TypeId =
  getOrPut(e, Type(kind: kind, base: elem))

func requestProcType*(e: var TypeEnv, inherit: TypeId, cc: TCallingConvention, params: varargs[TypeId]): TypeId =
  ## `inherit` is a procedure type to inherit the signature from
  assert inherit != NoneType
  assert e[inherit].kind in ProcedureLike

  var sig = e[inherit].sig
  for p in params:
    sig.add nodeId(e, p)

  getOrPut(e, Type(kind: tnkProc, a: cc.uint32, sig: sig))

func translateProc*(s: PSym, types: var DeferredTypeGen, ic: IdentCache, dest: var ProcHeader) =
  assert s != nil

  dest.magic = s.magic
  dest.flags = s.flags

  # fill in the declaration info
  if {sfImportc, sfInfixCall} * s.flags == {sfImportc} or
     sfExportc in s.flags:
    # either an imported procedure that doesn't use a pattern or an exported
    # one --> the value in ``loc.r`` is an identifier
    dest.decl.name = ic.getIdent($s.loc.r)
    dest.decl.forceName = true
  else:
    dest.decl.name = s.name
    dest.decl.forceName = false

  dest.decl.omit = lfNoDecl in s.loc.flags
  dest.decl.format = if s.constraint != nil: getStr(s.constraint) else: ""

  if lfHeader in s.loc.flags:
    dest.iface.header = getStr(s.annex.path)

  # XXX: temporary workaround for manually created magic syms (e.g. via
  #      ``createMagic``), as these have no type information. Those shouldn't
  #      be passed to ``requestProc`` however and instead be handled differently
  if s.typ == nil:
    return

  # type information
  let t = s.typ

  dest.returnType = types.requestType(t[0])
  if tfCapturesEnv in s.typ.flags:
    dest.envType = types.requestType(s.ast[paramsPos].lastSon.typ)

  dest.callConv = s.typ.callConv

  # an existing hidden environment parameter is **not** added to the parameter
  # list here
  dest.params.setLen(t.n.len - 1) # skip the first node
  var j = 0
  for i in 1..<t.n.len:
    let n = t.n[i]
    # don't add e.g. ``static`` or ``typeDesc`` parameters to the list
    if not n.typ.isCompileTimeOnly():
      dest.params[j] = (n.sym.name, types.requestType(n.typ))
      inc j

  # shrink to the number of used parameters
  dest.params.setLen(j)

func hash(s: PSym): Hash =
  hash(s.itemId)

func requestProc*(e: var ProcedureEnv, s: PSym): ProcId =
  ## Requests the ID for the given procedure-like `s`. The ID is cached, so
  ## multiple calls to ``requestProc`` with the same symbol will all yield the
  ## same one.
  assert s.kind in {skProc, skFunc, skMethod, skIterator, skConverter}, $s.kind

  let next = toId(e.procs.len, ProcId)
  result = e.map.mgetOrPut(s, next)
  if result == next:
    # the actual translation is deferred
    e.procs.setLen(e.procs.len + 1)
    e.orig[next] = s

func finish*(e: var ProcedureEnv, types: var DeferredTypeGen, ic: IdentCache) =
  ## Performs the translation for all collected symbols and cleans up
  ## accumulated mappings from ``PSym`` to ``ProcId`` in order to reduce
  ## memory usage. After calling this procedure, ``requestProc`` must
  ## not be called again.

  for sym, id in e.map.pairs:
    translateProc(sym, types, ic, e.procs[toIndex(id)])


  # TODO: use something like a ``DeferredProcGen`` instead (same as it works for types)
  reset(e.map)

func getReturnType*(e: ProcedureEnv, p: ProcId): TypeId =
  e[p].returnType

func param*(e: ProcedureEnv, p: ProcId, i: Natural): lent ProcParam {.inline.} =
  e[p].params[i]

func numParams*(e: ProcedureEnv, p: ProcId): int =
  e[p].params.len

iterator params*(e: ProcedureEnv, p: ProcId): ProcParam =
  for it in e[p].params.items:
    yield it

iterator items*(e: ProcedureEnv): ProcId =
  var i = 0
  let L = e.procs.len
  while i < L:
    yield toId(i, ProcId)
    inc i

# TODO: used for modifying procedure arguments, but this needs to be done
#       differently
func mget*(e: var ProcedureEnv, p: ProcId): var ProcHeader =
  e.procs[toIndex(p)]


func hashV2(e: TypeEnv, hcode: var Hash, se: var seq[int], id: TypeNodeIndex) =
  # since each type is unique, we can simply hash the id
  hcode = hcode !& id.int
  se.add(id.int)

func hashV2(e: TypeEnv, hcode: var Hash, se: var seq[int], id: TypeId) =
  # since each type is unique, we can simply hash the id
  hcode = hcode !& id.int
  se.add(id.int)

func hashField(e: TypeEnv, hcode: var Hash, se: var seq[int], f: FieldId) =
  # we take the field's symbol into account so that
  # TODO: this is wrong! The symbol's name is what's relevant here; not the ID
  # XXX: taking the name into account prevents object types from being
  #      collapsed into one, but it also prevents named tuples from being
  #      collapsed (which is problematic for the code-generators)
  hcode = hcode !& e[f].sym.int
  se.add(e[f].sym.int)
  hashV2(e, hcode, se, e[f].typ)

func hashRecord(e: TypeEnv, hcode: var Hash, se: var seq[int], ri: var int, fstart: int) =
  let n = e.records[ri]
  inc ri
  hcode = hcode !& n.kind.ord !& n.len.int
  se.add(n.kind.ord)
  se.add(n.len.int)

  case n.kind
  of rnkEmpty:
    discard
  of rnkList, rnkBranch, rnkCase:
    if n.kind == rnkList:
      hcode = hcode !& n.a.int
      se.add(n.a.int)

    for _ in 0..<n.len:
      hashRecord(e, hcode, se, ri, fstart)

  of rnkFields:
    for i in n.a..n.b:
      hashField(e, hcode, se, toId(fstart + i.int, FieldId))

func hash(e: TypeEnv, hcode: var Hash, se: var seq[int], typ: Type) =
  hcode = hcode !& typ.kind.ord
  se.add(typ.kind.ord)
  hashV2(e, hcode, se, typ.base)

  case typ.kind
  of tnkEmpty, tnkVoid, tnkBool, tnkChar:
    discard

  of tnkInt, tnkUInt, tnkFloat:
    hcode = hcode !& typ.a.int
    se.add(typ.a.int)

  of tnkRef, tnkPtr, tnkVar, tnkLent, tnkSeq, tnkOpenArray, tnkString, tnkCString, tnkUncheckedArray:
    # only the `base` field is relevant
    discard
  of tnkRecord:
    var ri = toIndex(typ.c.RecordId).int
    hashRecord(e, hcode, se, ri, typ.a.int)

  of tnkArray, tnkSet, tnkTypeDesc:
    hcode = hcode !& typ.a.int !& typ.b.int
    se.add(typ.a.int)
    se.add(typ.b.int)

  of tnkProc, tnkClosure:
    hcode = hcode !& typ.a.int !& typ.sig.len
    se.add(typ.a.int)
    se.add(typ.sig.len)
    for it in typ.sig.items:
      hashV2(e, hcode, se, it)

  of tnkName:
    # TODO: remove
    discard


iterator items*(x: TypeEnv): (TypeId, lent Type) =
  ## Supports adding types while iterating, but the added types are not included
  # XXX: yielding a view to the type is unsafe in the case that the callsite
  #      is adding new types, since the `types` list might get reallocated,
  #      turning the view into a dangling pointer
  var i = 0
  let L = x.types.len
  while i < L:
    yield (toId(i.TypeNodeIndex), x.types[i])
    inc i

func map*(gen: DeferredTypeGen, id: TypeId): TypeId {.inline.} =
  assert id != NoneType
  result = gen.data[(id.uint32 and TypeIdMask) - 1]

func maybeMap*(gen: DeferredTypeGen, id: TypeId): TypeId {.inline.} =
  assert id != NoneType
  result =
    if id.uint32 >= TypeIdKindBit:
      gen.data[(id.uint32 and TypeIdMask) - 1]
    else:
      id

iterator proxies*(e: TypeEnv): tuple[orig, target: TypeId] =
  ## Iterates over all proxy types, i.e. types that were replaced with
  ## others, and yields the original and target type ID.
  var i = 0
  let L = e.types.len
  while i < L:
    if e.proxies[i] != NoneType:
      yield (toId(i.TypeNodeIndex), e.proxies[i])
    inc i

func resolve*(e: TypeEnv, id: TypeId): TypeId =
  ## Skips to the underlying type if the given `id` is that of a proxy
  let i = id.toIndex
  result =
    if e.proxies[i] == NoneType:
      id
    else:
      e.proxies[i]

  assert e.proxies[result.toIndex] == NoneType

func commit*(e: var TypeEnv, remap: Table[TypeId, TypeId]) =
  # XXX: skip fields and types that were added during type modification passes?

  #debugEcho "remap: ", remap.len

  e.proxies.setLen(e.types.len)

  # we need to copy the type nodes from the new position to the old ones
  # since the original slots have external references that would also require
  # patching. While this isn't impossible (it's done once after IR
  # generation), we don't use this approach here, since commiting type changes
  # may happen multiple times and patching type references does take some time
  # (only a very small amount however)
  # XXX: copying the nodes is a problem if the target ID is that of an already
  #      existing canonical type. If that's the case, we're introducing a
  #      duplicate here. Either a ``tnkForward`` or something like
  #      lookup-level redirection is needed in this case. **edit:** somewhat
  #      solved with "proxy"-types
  for k, v in remap.pairs:
    e.types[k.toIndex] = e.types[v.toIndex]

    # we keep track of the remapping for the code-generators (they need types
    # to be unique)
    # XXX: this solves the problem for now - it's a bit awkward however
    e.proxies[k.toIndex] = v

  # XXX: currently doesn't work. Maybe it's not worth the hassle?
  when false:
    # replace the committed type nodes with an empty slot. This helps with detecting
    # bugs and a future garbage collector could also make use of this
    for v in remap.values:
      e.types[v.toIndex] = Type(kind: tnkEmpty)

  when false:
    for it in e.types.mitems:
      let nid = remap.getOrDefault(it.base.toId, NoneType)
      if nid != NoneType:
        it.base = nodeId(e, nid)

      for it2 in it.sig.mitems:
        let nid = remap.getOrDefault(it2.toId, NoneType)
        if nid != NoneType:
          it2 = nodeId(e, nid)

    for it in e.fields.mitems:
      let nid = remap.getOrDefault(it.typ.toId, NoneType)
      if nid != NoneType:
        it.typ = nodeId(e, nid)

func mapTypes*(e: var ProcedureEnv, g: DeferredTypeGen) =
  for it in e.procs.mitems:
    for pa in it.params.mitems:
      pa.typ = map(g, pa.typ)

    # magics not created during sem don't have type information
    if it.returnType != NoneType:
      it.returnType = map(g, it.returnType)

    if it.envType != NoneType:
      it.envType = map(g, it.envType)

func mapTypes*(e: var SymbolEnv, g: DeferredTypeGen) =
  for it in e.symbols.mitems:
    # XXX: not all symbols have type information - why?
    if it.typ != NoneType:
      it.typ = map(g, it.typ)
