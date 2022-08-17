## The definitions for the type representation used by the compiler back-end (mid-end?) IR.

import
  std/[
    hashes,
    tables
  ],
  compiler/front/[
    options, msgs
  ],
  compiler/ast/[
    ast_types,
    ast,
    types
  ]

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

  tnkProc

  tnkTypeDesc ## only relevant for compile-time function evaluation

  tnkName # a reference to named type

  #tnkAlias

type FieldDesc* = object
  sym*: SymId # may be empty
  typ*: TypeId
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
  sig: seq[TypeId] # for procedures

  # XXX: even though ``DeclType`` exists, `Type` is used to store the
  #      interface information for now
  iface*: PSym

type
  DeclTypeId* = distinct uint32

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

    canonical: TypeId

    # XXX: the "interface" and extra part are both combined into a `PSym` for
    #      now. This makes the first implementation simpler and also helps
    #      with reducing memory usage
    decl: PSym

type TypeEnv* = object
  ## Holds the data for all types
  # XXX: in general, a `seq[seq]` could be used for `records`, `fields`, and
  #      `types`. This would make access a bit simpler; require less copying
  #      on resize; and make garbge collection easier. It would also increase
  #      memory fragmentation and reduce cache locality
  records: seq[RecordNode] ## the bodies for all record-like types (objects and tuples) in one contiguous seq
  fields: seq[FieldDesc] ## all fields
  types: seq[Type] ## all types in one contiguous seq

  decls: seq[DeclType] ## all declared types

  # XXX: maybe a redirection table for `tnkName` makes sense? Alternatively,
  #      indirections to another tnkName could be allowed
  typdescs: Table[ItemId, PType] # type-id -> a `tyTypeDesc` type

type TypeLookup* = object
  ## Data needed for mapping ``PType`` to the ``TypeId``

type
  DeferredTypeGen* = object
    env*: ptr TypeEnv # XXX: a `lent` should be used instead of a pointer. It
                     #      would also make sure that the borrowed from
                     #      `TypeEnv` is sealed for the lifetime of the `DeferredTypeGen`
    map: Table[ItemId, TypeId] # type-id -> ``TypeId``
    list: seq[PType] ## the list of deferred types in the order they were requested

    voidType*: PType ## a ``PType`` of kind ``tyVoid``. Requesting a nil type
                     ## is remapped to a request using this type
    charType*: PType

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

    decl*: Declaration

  SymbolEnv* = object
    # TODO: maybe rename?

    symbols*: seq[Symbol]

    # TODO: maybe split off `map` into a separate `SymbolLookup` type?
    map: Table[ItemId, SymId] # ``PSym``-id -> ``SymId``

    # XXX: `orig` will likely be removed/replaced later on
    orig*: Table[SymId, PSym] # stores the associated ``PSym`` for a symbol. Currently meant to be used by the code-generators.

  ProcId* = distinct uint32

  ProcHeader* = object
    ## At the IR-level, there is no distinction done between ``func``s,
    ## ``proc``s, ``iterator``s, and ``method``s. They are all treated as a
    ## "procedure" and work the same.

    params*: seq[tuple[name: string, typ: TypeId]]
    returnType*: TypeId

    magic*: TMagic

    flags*: TSymFlags # XXX: uses `TSymFlags` for now, but this will be changed to something else later on

    # XXX: each procedure requires a ``Declaration``, so it's stored as part of
    #      the type in order to avoid indirections via a ``DeclId`` (or similar).
    #      Since a ``Declaration`` object is quite large, this does mean that
    #      less ``ProcHeader``s fit into a cache-line. The declration is only
    #      needed by code-generators so it likely makes sense to move the
    #      declaration into a separate seq in ``ProcedureEnv``
    decl*: Declaration

  ProcedureEnv* = object
    # TODO: maybe rename
    procs: seq[ProcHeader]
    map: Table[PSym, ProcId]
    orig*: Table[ProcId, PSym]

const NoneType* = TypeId(0)
const NoneSymbol* = SymId(0)
const NoneDType* = DeclTypeId(0)

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

type SomeId = TypeId | SymId | RecordId | FieldId | ProcId | DeclTypeId

template toIndex*(id: SomeId): uint32 =
  id.uint32 - 1

template toId[T: SomeId](index: Natural, id: typedesc[T]): T =
  T(index + 1)

func `[]`*(e: SymbolEnv, s: SymId): lent Symbol =
  e.symbols[s.int - 1]

func `[]`*(e: TypeEnv, t: TypeId): lent Type =
  e.types[toIndex(t)]

func `[]`*(e: TypeEnv, t: DeclTypeId): lent Type =
  ## For the convenience of the IR processing steps, this procedure returns a
  ## ``Type`` instead of a ``DeclType``
  e.types[e.decls[toIndex(t)].canonical.toIndex]

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
  assert e[t].kind == tnkProc, $e[t].kind
  e[t].sig[0]

func elemType*(e: TypeEnv, t: TypeId): TypeId =
  e[t].base

func baseType*(e: TypeEnv, t: TypeId): TypeId =
  e[t].base

func numFields*(n: RecordNode): int =
  assert n.kind == rnkList
  n.a.int

func base*(t: Type): TypeId =
  t.base


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
  assert e[t].kind == tnkProc
  e[t].sig[i]

func param*(e: TypeEnv, t: TypeId, i: Natural): TypeId =
  assert e[t].kind == tnkProc
  e[t].sig[i]

func size*(t: Type): uint =
  assert t.kind in {tnkFloat, tnkInt, tnkUInt}
  t.a.uint

func length*(t: Type): uint =
  assert t.kind in {tnkSet, tnkArray}
  t.a.uint

iterator params*(e: TypeEnv, t: TypeId): TypeId =
  let typ = e[t]
  assert typ.kind == tnkProc
  for i in 1..<typ.sig.len:
    yield typ.sig[i]


func numParams*(e: TypeEnv, t: TypeId): int =
  assert e[t].kind == tnkProc
  e[t].sig.len - 1

func getSize*(e: TypeEnv, t: TypeId): uint =
  ## the size in bytes
  assert e[t].kind == tnkSet
  e[t].a.uint

func nthField*(e: TypeEnv, t: TypeId, i: Natural): FieldId =
  var t = toIndex(t)
  while true:
    let typ = e.types[t]
    if typ.b.int <= i:
      let i = i - typ.b.int
      assert e[typ.c.RecordId].kind == rnkList
      assert i < e[typ.c.RecordId].numFields, $e[typ.c.RecordId].numFields
      return FieldId(typ.a.int + i + 1)
    elif typ.base != NoneType:
      t = toIndex(typ.base)
    else:
      return FieldId(0) # TODO: use a `NoneField`

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
    if t.sym != nil and t.sym.flags.overlaps({sfImportc, sfExportc}):
      # don't skip types that have an external interface attached
      break

    if t.kind in irrelevantForBackend:
      t = t.lastSon
    else:
      break

  #[
  if t.kind in {tyTyped, tyUntyped, tyGenericParam}:
    debugEcho gen.traces[trace]
  ]#

  let next = TypeId(gen.nextTypeId + 1)
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
    e.symbols[^1].decl.name = s.name.s
    e.orig[result] = s

type TypeGen = object
  syms: SymbolEnv

  def: ptr DeferredTypeGen


func addField(dest: var TypeEnv, g: var TypeGen, s: PSym,
              numFields: var int): tuple[fields, entries: int] =
  if dest.records[^1].kind == rnkFields:
    # append to the active field section
    inc dest.records[^1].b
  else:
    # a new field section
    dest.records.add RecordNode(kind: rnkFields, a: numFields.uint32, b: numFields.uint32)
    result.entries = 1

  dest.fields.add(FieldDesc(sym: g.syms.requestSym(s),
                            typ: g.def[].requestType(s.typ)))

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

#[
func canonical(dest: var TypeEnv, t: PType): seq[TypeNode] =
  case t.kind


  of tyObject:
    result.add TypeNode(kind: tnkRecord, a: (t.kind.ord - tyInt8))

  of tyTuple:

  of tyGenericBody, tyGenericInst, tyGenericInvocation, tyDistinct, tyRange,
     tyStatic, tyAlias, tySink, tyInferred, tyOwned, tyDistinct:
    result.add canonical(dest, t.lastSon)

  else:
    unreachable(t.kind)

func toCanon(dest: var TypeEnv, t: PType) =
  let temp = canonical(dest, t)
  hash(temp)

  dest
]#

func split(x: uint64): tuple[lo, hi: uint32] {.inline.} =
  result.lo = uint32(x and 0xFFFFFFFF'u64)
  result.hi = uint32(x shr 32)

func combine(lo, hi: uint32): uint64 {.inline.} =
  result = lo.uint64 or (hi.uint64 shl 32)

proc translate(dest: var TypeEnv, gen: var TypeGen, conf: ConfigRef, pos: int, t: PType) =
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
      # the base type might be a ``ref object``, but that's ok. A separate
      # pass takes care of removing the indirection
      dest.types[pos].base = gen.def[].requestType(t[0])

  of tyTuple:
    dest.types[pos] = Type(kind: tnkRecord, a: dest.fields.len.uint32, b: 0,
                           c: RecordId(dest.records.len + 1).uint32)

    if t.n != nil:
      var tmp: int
      discard translate(dest, gen, t.n, tmp)
    else:
      dest.records.add RecordNode(kind: rnkList, len: 1, a: t.len.uint32)
      dest.records.add RecordNode(kind: rnkFields, a: 0, b: uint32(t.len - 1))

      let start = dest.fields.len
      dest.fields.setLen(start + t.len)
      for i in 0..<t.len:
        dest.fields[start + i].typ = gen.def[].requestType(t[i])


  of tyArray:
    let (lo, hi) = lengthOrd(conf, t).toUInt64().split()
    dest.types[pos] = Type(kind: tnkArray, base: gen.def[].requestType(t.elemType), a: lo, b: hi)

  of tyString:
    # in order to simplify the IR passes, the string type uses ``char`` as the
    # base type. This makes it possible to use ``elemType`` on the string type
    dest.types[pos] = Type(kind: tnkString, base: gen.def[].requestType(gen.def.charType))

  of tyCstring:
    dest.types[pos] = Type(kind: tnkCString, base: gen.def[].requestType(gen.def.charType))

  of tyRef, tyPtr, tyVar, tyLent, tySequence, tyOpenArray, tyUncheckedArray:
    const Map = {tyRef: tnkRef, tyPtr: tnkPtr, tyVar: tnkVar, tyLent: tnkLent, tySequence: tnkSeq, tyOpenArray: tnkOpenArray, tyUncheckedArray: tnkUncheckedArray}.toTable
    dest.types[pos] = Type(kind: Map[t.kind], base: gen.def[].requestType(t.lastSon))

  of tyVarargs:
    # HACK: `echo` uses `varargs[typed]` as the parameter and we have to work
    #        around that here
    let elem =
      if t.lastSon.kind == tyTyped: gen.def.voidType
      else: t.lastSon

    dest.types[pos] = Type(kind: tnkOpenArray, base: gen.def[].requestType(elem))

  of tyGenericBody, tyGenericInst, tyGenericInvocation, tyDistinct, tyRange,
     tyStatic, tyAlias, tySink, tyInferred, tyOwned:

    translate(dest, gen, conf, pos, t.lastSon)

  of tyNil, tyPointer:
    # treat an untyped pointer as a `ptr void`
    dest.types[pos] = Type(kind: tnkPtr, base: gen.def[].requestType(gen.def.voidType))

  of tyProc:
    var r = Type(kind: tnkProc, a: t.callConv.ord.uint32)
    r.sig.newSeq(t.len)

    for i in 0..<t.len:
      r.sig[i] = gen.def[].requestType(t[i])

    dest.types[pos] = move r

  of tyTypeDesc:
    let itId = t.itemId
    dest.typdescs[itId] = t
    dest.types[pos] = Type(kind: tnkTypeDesc, a: cast[uint32](itId.module), b: cast[uint32](itId.item))

  else:
    if t.sym != nil:
      debugEcho conf.toFileLineCol(t.sym.info)
    unreachable(t.kind)

proc flush*(gen: var DeferredTypeGen, symEnv: var SymbolEnv, conf: ConfigRef) =
  ## Generates all the types that were deferred. `types` may be
  ## re-used after calling this procedure.

  var ctx: TypeGen
  ctx.def = addr gen

  swap(symEnv, ctx.syms)
  defer: swap(symEnv, ctx.syms)

  when useGenTraces:
    gen.isInGen = true

  let start = gen.env.types.len
  var i = 0
  while i < gen.list.len:
    gen.env.types.add(default(Type))
    when useGenTraces:
      gen.trace = gen.list[i][1]

    translate(gen.env[], ctx, conf, start + i, gen.list[i])
    gen.env.types[start + i].iface = gen.list[i].sym
    inc i

  # fix up pass. Remove ``tnkRef`` indirections when used as the base type of objects and also set the relative field offset.
  # Since objects at a higher inheritance depth come _before_ their bases in
  # the list we have to iterate in reverse in order to propagate the offset correctly
  for j in countdown(gen.env.types.high, start):
    let t = addr gen.env.types[j]
    case t.kind
    of tnkRecord:
      if t.base != NoneType:
        if gen.env[][t.base].kind == tnkRef:
          t.base = gen.env[][t.base].base

        let bt = gen.env[][t.base]
        t.b = bt.b + gen.env[][bt.record].numFields.uint32
    else:
      discard

  when useGenTraces:
    gen.isInGen = false
    gen.traceMap.setLen(0)
    gen.traces.setLen(0)

  # support re-using
  gen.list.setLen(0)

func addSym*(e: var SymbolEnv, kind: TSymKind, typ: TypeId, name: string, flags: TSymFlags = {}): SymId =
  # XXX: temporary helper
  e.symbols.add(Symbol(kind: kind, typ: typ, flags: flags, decl: Declaration(name: name)))
  result = e.symbols.len.SymId

func addMagic*(e: var ProcedureEnv, typ: TypeId, name: string, m: TMagic): ProcId =
  # XXX: temporary helper
  e.procs.add ProcHeader(returnType: typ, magic: m, decl: Declaration(name: name))
  result = e.procs.len.ProcId

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

func genRecordType*(e: var TypeEnv, base: TypeId, fields: varargs[(SymId, TypeId)]): Type =
  result.kind = tnkRecord
  result.a = e.fields.len.uint32

  if base != NoneType:
    result.base = base
    result.b = e[base].b + e.numFields(base).uint32

  result.c = toId(e.records.len, RecordId).uint32

  # TODO: use `setLen` + []
  for s, t in fields.items:
    e.fields.add FieldDesc(sym: s, typ: t)

  e.records.add RecordNode(kind: rnkList, len: 1, a: fields.len.uint32)
  e.records.add RecordNode(kind: rnkFields, a: 0, b: fields.high.uint32)

func genArrayType*(e: var TypeEnv, len: BiggestUInt, elem: TypeId): Type =
  let s = split(len.uint64)
  Type(kind: tnkArray, base: elem, a: s.lo, b: s.hi)

func requestArrayType*(e: var TypeEnv, len: BiggestUInt, elem: TypeId): TypeId =
  # TODO: don't add duplicate types
  e.types.add(genArrayType(e, len, elem))
  result = toId(e.types.high, TypeId)

func requestRecordType*(e: var TypeEnv, base: TypeId, fields: varargs[(SymId, TypeId)]): TypeId =
  let t = genRecordType(e, base, fields)
  e.types.add(t)
  result = toId(e.types.high, TypeId)

func genGenericType*(kind: TypeNodeKind, elem: TypeId): Type =
  assert kind in {tnkUncheckedArray, tnkSeq, tnkVar, tnkLent, tnkPtr, tnkRef, tnkOpenArray}
  Type(kind: kind, base: elem)

func requestGenericType*(e: var TypeEnv, kind: TypeNodeKind, elem: TypeId): TypeId =
  # TODO: first check if the type exists already
  e.types.add(Type(kind: kind, base: elem))
  result = toId(e.types.high, TypeId)

func translateProc*(s: PSym, types: var DeferredTypeGen, dest: var ProcHeader) =
  assert s != nil

  dest.magic = s.magic
  dest.flags = s.flags

  # fill in the declaration info
  dest.decl.name = s.name.s

  # XXX: temporary workaround for manually created magic syms (e.g. via
  #      ``createMagic``), as these have no type information. Those shouldn't
  #      be passed to ``requestProc`` however and instead be handled differently
  if s.typ == nil:
    return

  # type information
  let t = s.typ

  dest.returnType = types.requestType(t[0])

  # an existing hidden environment parameter is **not** added to the parameter
  # list here
  dest.params.setLen(t.n.len - 1) # skip the first node
  var j = 0
  for i in 1..<t.n.len:
    let n = t.n[i]
    # don't add e.g. ``static`` or ``typeDesc`` parameters to the list
    if not n.typ.isCompileTimeOnly():
      dest.params[j] = (n.sym.name.s, types.requestType(n.typ))
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

func finish*(e: var ProcedureEnv, types: var DeferredTypeGen) =
  ## Performs the translation for all collected symbols and cleans up
  ## accumulated mappings from ``PSym`` to ``ProcId`` in order to reduce
  ## memory usage. After calling this procedure, ``requestProc`` must
  ## not be called again.

  for sym, id in e.map.pairs:
    translateProc(sym, types, e.procs[toIndex(id)])


  # TODO: use something like a ``DeferredProcGen`` instead (same as it works for types)
  reset(e.map)

func getReturnType*(e: ProcedureEnv, p: ProcId): TypeId =
  e[p].returnType

func param*(e: ProcedureEnv, p: ProcId, i: Natural): auto =
  # XXX: costly string copy since ``lent`` is not used here
  e[p].params[i]

func numParams*(e: ProcedureEnv, p: ProcId): int =
  e[p].params.len

iterator params*(e: ProcedureEnv, p: ProcId): tuple[name: lent string, typ: TypeId] =
  for n, t in e[p].params.items:
    yield (n, t)

iterator items*(e: ProcedureEnv): ProcId =
  var i = 0
  let L = e.procs.len
  while i < L:
    yield toId(i, ProcId)
    inc i
