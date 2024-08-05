## Implements the type IR for the MIR phase (not yet), plus the
## `TypeEnv <#TypeEnv>`_, which stores the data for all types.
##
## All types are addressed via ``TypeId``, with the built-in types using
## static IDs. Every ``TypeId`` is associated with a `TypeSym <#TypeSym>`_,
## linking together the type's various representations. If multiple equal
## structural types are added to the environment, the one added first is
## designated as the "canonical" one.
##
## At the MIR level, a type representation has three different levels
## (`Level <#Level>`_). On the `Original` level, all type references are
## exactly as they appear in the ``PType`` form, the `Canonical` level only
## uses references to canonical types, and the `Lowered` level is the type as
## the code generator sees it.

import
  std/[
    hashes,
    tables
  ],
  compiler/ast/[
    ast,
    ast_types,
    lineinfos,
    idents,
    types
  ],
  compiler/front/[
    options
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/mir/[
    mirtrees,
    typemaps
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/utils/[
    containers,
    idioms
  ]

# XXX: sighashes are currently needed for merging generic instantiations, but
#      this should ultimately happen earlier - in sem - already
from compiler/sem/sighashes import hashType, ConsiderFlag

type
  TypeKind* = enum
    tkVoid
    tkBool, tkChar
    tkInt, tkUInt, tkFloat

    tkPointer
    tkPtr, tkRef, tkVar, tkLent

    tkProc
    tkClosure ## proc + environment

    tkArray
    tkUncheckedArray
    tkOpenArray
    tkSet
    tkSeq
    tkString
    tkCstring

    tkImported
    tkIndirect # not a real type

    # record-like types:
    tkRecord
    tkUnion
    tkTaggedUnion

  ParamFlag* = enum
    pfByRef

  IntVal = distinct uint32
    ## If the MSB is not set, the lower 31 bit represent an unsigned integer
    ## value. Otherwise, they represent a ``LitId``.

  FieldId* = distinct uint32
  HeaderId* = uint32

  RecField* = object
    ## Record field description.
    ident: LitId
    offset: IntVal
    align*: int16
    extra: uint16
    typ*: TypeId

  TypeHeader* = object
    ## Type description header.
    kind*: TypeKind
    align*: int16   ## required alignment
    size: IntVal    ## size in bytes
    a: uint32       ## meaning depends on the type
    b: uint32       ## meaning depends on the type

  Level* = enum
    Original  ## contains the original type symbols
    Canonical ## same as `Original`, but with only canonical types symbols
    Lowered   ## fully lowered version of the type

  TypeSym* = object
    ## A *type symbol* links together the various representations of a type.
    inst*: PType
      ## the original type instance
    canon*: TypeId
      ## if a type symbol is the canonical one, canon points to itself
    desc*: array[Level, HeaderId]

  TypeEnv* = object
    ## Stores the data associated with types. Has no valid default value, and
    ## must be explicitly initialized first.
    map: TypeTable[TypeId]
    symbols {.requiresInit.}: Store[TypeId, TypeSym]

    headers: Store[HeaderId, TypeHeader]
      ## all type headers
    fields: seq[RecField]
      ## all record fields referenced from `headers`
    params: seq[tuple[x: uint32, typ: TypeId]]
      ## all parameters referenced from `headers`

    structs: seq[uint32]
      ## an open-addressing using hash table. Indexed by ``hash(desc)``. The
      ## items are indices (biased by 1) into the `headers` sequence
    numStructs: int
      ## the number of occupied slots in `structs`

    canon: Table[HeaderId, TypeId]
      ## maps headers of canonical type descriptions to their type symbol

    instances: Table[SigHash, TypeId]
      ## associates the sighash of a generic type instance with a type symbol.
      ## This is used for eliminating same-shaped instantiations of a generic
      ## object type

    idents: BiTable[string]
    numbers: BiTable[BiggestInt]

    config: ConfigRef
    graph: ModuleGraph

    sizeType: TypeId
      ## the target-dependent integer type to use for size values
    usizeType: TypeId
      ## the target-dependent unsigned integer type to use for size values

  RecordBuilder = object
    header: TypeHeader
    start: int
    fields: seq[RecField]

  ProcBuilder* = object
    header: TypeHeader
    params: seq[tuple[x: uint32, typ: TypeId]]

const
  VoidType*    = TypeId 0
  BoolType*    = TypeId 1
  CharType*    = TypeId 2
  Int8Type*    = TypeId 3
  Int16Type*   = TypeId 4
  Int32Type*   = TypeId 5
  Int64Type*   = TypeId 6
  UInt8Type*   = TypeId 7
  UInt16Type*  = TypeId 8
  UInt32Type*  = TypeId 9
  UInt64Type*  = TypeId 10
  Float32Type* = TypeId 11
  Float64Type* = TypeId 12
  StringType*  = TypeId 13
  CstringType* = TypeId 14
  PointerType* = TypeId 15

  Skip = {tyAlias, tyDistinct, tySink, tyGenericInst, tyEnum, tyOrdinal,
          tyRange} + tyUserTypeClasses
    ## types not relevant to the MIR type description

  MangleFlag = 0x4000'u16
  NoAliasFlag = 0x8000'u16

  VarargsFlag = 0x8000_0000'u32

func `==`*(a, b: FieldId): bool {.borrow, inline.}
func `==`(a, b: IntVal): bool {.borrow, inline.}

func hash(env: TypeEnv, t: TypeHeader): Hash =
  ## Computes the structural hash for the type `t`.
  result = hash(t.kind)
  case t.kind
  of tkVoid, tkBool, tkChar, tkPointer, tkString, tkCstring:
    discard "no additional content to hash"
  of tkInt, tkUInt, tkFloat, tkPtr, tkRef, tkVar, tkLent, tkSeq,
     tkUncheckedArray, tkOpenArray, tkSet, tkIndirect, tkImported:
    result = result !& hash(t.a)
  of tkArray:
    result = result !& hash(t.a) !& hash(t.b)
  of tkRecord, tkUnion, tkTaggedUnion:
    # size and alignment doesn't need to be part of the hash. Two structural
    # types with the same content cannot have different size or alignment,
    # and two nominal types are always distinct
    for it in t.a..<t.b:
      result = result !& hash(env.fields[it])
  of tkProc, tkClosure:
    for it in t.a..<t.b:
      result = result !& hash(env.params[it])

  result = !$(result)

func isEqual(env: TypeEnv, a, b: TypeHeader): bool =
  ## Compares `a` and `b` for structural equality.
  if a.kind != b.kind:
    return false

  func isEqual(s: seq, a, b, len: uint32): bool {.nimcall.} =
    for i in 0..<len:
      if s[a + i] != s[b + i]:
        return false
    result = true

  template fieldCount(t: TypeHeader): uint32 =
    t.b - t.a # also valid for procedure types

  case a.kind
  of tkVoid, tkBool, tkChar, tkPointer, tkString, tkCstring:
    true
  of tkInt, tkUInt, tkFloat, tkPtr, tkRef, tkVar, tkLent, tkSeq,
     tkUncheckedArray, tkOpenArray, tkSet, tkIndirect, tkImported:
    a.a == b.a
  of tkArray:
    a.a == b.a and a.b == b.b
  of tkRecord, tkUnion, tkTaggedUnion:
    if fieldCount(a) == fieldCount(b): # same number of fields?
      isEqual(env.fields, a.a, b.a, fieldCount(a))
    else:
      false
  of tkProc, tkClosure:
    if fieldCount(a) == fieldCount(b): # same number of params?
      isEqual(env.params, a.a, b.a, fieldCount(a))
    else:
      false

# -------------------------
# Hash-table implementation

template maxHash(t: seq): int =
  t.high

template isFilled(m: uint32): bool =
  m != 0

template nextTry(i, max: int): int =
  (i + 1) and max

func enlarge(env: var TypeEnv) =
  ## Grows and re-hashes the `env.structs` hash-table.
  template tbl: untyped = env.structs
  var n = newSeq[uint32](tbl.len * 2)
  swap(n, tbl)
  for old in n.items:
    if old.isFilled:
      var j = hash(env, env.headers[old-1]) and maxHash(tbl)
      while tbl[j].isFilled:
        j = nextTry(j, maxHash(tbl))
      tbl[j] = old

func deduplicate(env: var TypeEnv, header: TypeHeader): (bool, HeaderId) =
  ## Adds `header` to env, but only if the type wasn't added already. Only
  ## types previously passed to `deduplicate` are considered. Returns the
  ## ID to address the added header with and whether it existed already.
  let hash = hash(env, header) ## hash of the tree
  var i = hash and maxHash(env.structs)
  # note: the items in the table are offset by 1, so that '0' means
  # "empty slot"
  if env.structs.len > 0:
    while (let slot = env.structs[i]; slot.isFilled):
      let h = uint32(slot - 1) # header index
      if isEqual(env, header, env.headers[h]): # same types?
        return (true, h) # already exists
      i = nextTry(i, maxHash(env.structs))

    # not a duplicate. Before adding a new entry, first enlarge the table, if
    # necessary
    template mustRehash(len, counter: int): bool =
      (len * 2 < counter * 3) or (len - counter < 4)

    if mustRehash(env.structs.len, env.numStructs):
      enlarge(env)
      i = hash and maxHash(env.structs)
      # find the first empty slot:
      while isFilled(env.structs[i]):
        i = nextTry(i, maxHash(env.structs))

  else:
    # the table is empty, initialize it
    env.structs.setLen(16) # must to be a power-of-two
    i = hash and maxHash(env.structs)

  let id = env.headers.add header
  # remember the description in the list:
  env.structs[i] = id + 1
  inc env.numStructs

  result = (false, id)

proc toIntVal(env: var TypeEnv, val: BiggestInt): IntVal =
  ## Turns `val` into an ``IntVal``.
  if val in 0..0x7FFF_FFFF:
    IntVal(val)
  else:
    IntVal(uint32(env.numbers.getOrIncl(val)) or 0x8000_0000'u32)

proc getInt(env: TypeEnv, val: IntVal): BiggestInt =
  ## Turns `val` back into an integer value.
  if (val.uint32 and 0x8000_0000'u32) == 0:
    BiggestInt(val.uint32 and 0x7FFF_FFFF'u32)
  else:
    env.numbers[LitId(val.uint32 and 0x7FFF_FFFF'u32)]

# Type inspection/query routines
# ------------------------------

# the querie routines are simple enough to warrant inlining
{.push inline.}

func headerFor*(env: TypeEnv, id: TypeId, phase: Level): lent TypeHeader =
  env.headers[env.symbols[id].desc[phase]]

func `[]`*(env: TypeEnv, id: HeaderId): lent TypeHeader {.inline.} =
  env.headers[id]

func size*(desc: TypeHeader, env: TypeEnv): BiggestInt {.inline.} =
  ## Returns the size-in-bytes for the given type.
  env.getInt(desc.size)

proc elem*(desc: TypeHeader): TypeId {.inline.} =
  ## Returns the element type for `h`.
  assert desc.kind in {tkArray, tkSeq, tkUncheckedArray, tkIndirect,
                       tkImported, tkOpenArray, tkLent, tkVar, tkPtr, tkRef}
  desc.a.TypeId

proc count*(desc: TypeHeader): uint32 {.inline.} =
  ## Returns the number of elements the ``set`` type can contain.
  assert desc.kind == tkSet
  desc.a

proc arrayLen*(desc: TypeHeader, env: TypeEnv): BiggestInt {.inline.} =
  assert desc.kind == tkArray
  env.getInt(IntVal desc.b)

func discr*(desc: TypeHeader, env: TypeEnv): FieldId =
  ## Returns the discriminator field for the given tagged union.
  assert desc.kind == tkTaggedUnion
  FieldId desc.a

func numParams*(desc: TypeHeader): int =
  int(desc.b - desc.a) - 1

func callConv*(desc: TypeHeader, env: TypeEnv): TCallingConvention =
  # mask away the varargs flag
  TCallingConvention(env.params[desc.a].x and not(VarargsFlag))

func hasVarargs*(desc: TypeHeader, env: TypeEnv): bool =
  (env.params[desc.a].x and VarargsFlag) != 0

func retType*(desc: TypeHeader, env: TypeEnv): TypeId =
  assert desc.kind in {tkProc, tkClosure}
  env.params[desc.a].typ

iterator params*(env: TypeEnv, desc: TypeHeader
                ): tuple[i: int, typ: TypeId, flags: set[ParamFlag]] =
  ## Returns the typ and flags for all parameters of type t
  for i in (desc.a + 1)..<desc.b:
    yield (int(i - desc.a - 1), env.params[i].typ,
           cast[set[ParamFlag]](env.params[i].x))

func base*(desc: TypeHeader, env: TypeEnv): TypeId =
  ## Returns the node storing the base type (i.e., the parent type) for a
  ## record type.
  assert desc.kind == tkRecord
  env.fields[desc.a].typ

func fieldOffset*(desc: TypeHeader, env: TypeEnv): int32 =
  ## Returns the first field's position in the object.
  assert desc.kind == tkRecord
  env.fields[desc.a].align.int32

func isPacked*(desc: TypeHeader, env: TypeEnv): bool =
  ## Whether the record type is
  assert desc.kind == tkRecord
  env.fields[desc.a].extra == 1

func numFields*(desc: TypeHeader): int =
  ## Returns the number of fields in the record-like type, ignoring parent
  ## types.
  int(desc.b - desc.a) - ord(desc.kind == tkRecord)

func isNamed*(f: RecField): bool =
  f.ident != LitId(0)

func name*(env: TypeEnv, f: RecField): lent string =
  assert f.ident != LitId(0)
  result = env.idents[f.ident]

func isNoMangle*(f: RecField): bool =
  ## Whether the field's name must not be mangled.
  (f.extra and MangleFlag) == 0

func isNoAlias*(f: RecField): bool =
  (f.extra and NoAliasFlag) != 0

func bitsize*(f: RecField): int =
  int(f.extra and 0x00FF)

{.pop.} # inline

iterator fields*(env: TypeEnv, desc: TypeHeader;
                 offset = 0): (FieldId, RecField) =
  ## Returns all fields directly part of `desc`. Super types are not
  ## considered.
  assert desc.kind in {tkRecord, tkUnion, tkTaggedUnion}
  # note: the field storing the super type is not included
  let offset = ord(desc.kind == tkRecord) + offset
  for it in (desc.a + uint32(offset))..<desc.b:
    yield (FieldId(it), env.fields[it])

proc computeDepth*(env: TypeEnv, desc: TypeHeader, pos: int32): int =
  ## Computes the depth at which the field with position `pos` is located.
  ## 0 means it's part of `desc`, 1 means it's part of the first parent type,
  ## etc.
  case desc.kind
  of tkRecord:
    result = 0
    var h {.cursor.} = desc
    while h.fieldOffset(env) > pos:
      h = env.headerFor(h.base(env), Lowered)
      inc result
  of tkUnion:
    result = 0
  of tkImported, tkIndirect:
    result = computeDepth(env, env.headerFor(desc.elem, Lowered), pos)
  else:
    unreachable(desc.kind)

proc canonical*(env: TypeEnv, typ: TypeId): TypeId =
  ## Returns the canonical symbol for `typ`. All indirections are skipped.
  result = env.symbols[typ].canon
  # skip indirections:
  while env.headerFor(result, Canonical).kind == tkIndirect:
    result = env.headerFor(result, Canonical).elem

proc isEmbedded*(env: TypeEnv, typ: TypeId): bool =
  ## Whether the `typ` is a record that's directly embedded where it's used.
  env.symbols[typ].inst.isNil and
    env.headerFor(typ, Lowered).kind in {tkRecord, tkTaggedUnion}

proc lookupField*(env: TypeEnv, typ: TypeId, pos: int32): FieldId =
  ## Returns the ID of the field with position `pos`. Said field *must* exist
  ## in record-like type `typ`. Imported types and indirection are skipped.

  # skip indirections and imported types:
  var typ = env.symbols[typ].canon
  while env.headerFor(typ, Canonical).kind in {tkIndirect, tkImported}:
    typ = env.headerFor(typ, Canonical).elem

  var curr = 0'i32
  # seek to the type in the inheritance hierarchy that contains the field
  if env.headerFor(typ, Lowered).kind == tkRecord:
    curr = env.headerFor(typ, Lowered).fieldOffset(env)
    while curr > pos: # part of the current record?
      # it's not, try the parent type
      typ = env.headerFor(typ, Lowered).base(env)
      curr = env.headerFor(typ, Lowered).fieldOffset(env)

    assert typ != VoidType, "field not in record"

  proc searchRecord(env: TypeEnv, desc: TypeHeader, pos: int32,
                    curr: var int32): (bool, FieldId) =
    # look for the field whose position matches `pos`. Anonymous record-like
    # types are always embedded at the moment, so they are transparently
    # recursed into
    for (id, it) in fields(env, desc):
      if isEmbedded(env, it.typ):
        result = searchRecord(env, env.headerFor(it.typ, Lowered), pos, curr)
        if result[0]:
          return
      elif curr == pos:
        return (true, id)
      else:
        inc curr

    result = (false, default(FieldId))

  let r = searchRecord(env, env.headerFor(typ, Lowered), pos, curr)
  assert r[0], "field not in type"
  result = r[1]

# Record/proc builder API
# -----------------------

proc newType*(env: var TypeEnv, desc: HeaderId): TypeId =
  ## If none exists already, creates a new type symbol for `desc`.
  result = env.canon.mgetOrPut(desc, env.symbols.nextId())
  if result == env.symbols.nextId():
    # no type symbol exists yet
    result = env.symbols.add(TypeSym(canon: result, desc: [desc, desc, desc]))

proc openRecord(size: IntVal, align: int16; offset = 0;
                base = VoidType): RecordBuilder =
  result.header = TypeHeader(kind: tkRecord, size: size, align: align, b: 1)
  result.fields.add RecField(typ: base, align: offset.int16)

proc open(kind: TypeKind; size: IntVal, align: int16): RecordBuilder =
  assert kind in {tkUnion, tkTaggedUnion}
  result.header = TypeHeader(kind: kind, size: size, align: align)

proc openProc(env: TypeEnv, kind: TypeKind, conv: TCallingConvention,
              ret: TypeId, isVarargs: bool): ProcBuilder =
  ## Opens a builder for a procedure-like type.
  assert kind in {tkProc, tkClosure}
  result.header = TypeHeader(kind: kind, b: 1,
                             align: env.config.target.ptrSize.int16)
  if kind == tkProc:
    result.header.size = IntVal(env.config.target.ptrSize)
  else:
    # a closure is two pointers
    result.header.size = IntVal(env.config.target.ptrSize * 2)
  result.params.add (uint32(conv) or (uint32(ord(isVarargs)) shl 31), ret)

proc openRecord(b: var RecordBuilder): RecordBuilder =
  result = RecordBuilder(start: b.fields.len)
  swap(result.fields, b.fields) # temporarily take over the buffer
  result.header = TypeHeader(kind: tkRecord, b: 1)
  result.fields.add RecField(typ: VoidType, align: 0)

proc open(b: var RecordBuilder, kind: TypeKind): RecordBuilder =
  assert kind in {tkUnion, tkTaggedUnion}
  result = RecordBuilder(start: b.fields.len)
  swap(result.fields, b.fields) # temporarily take over the buffer
  result.header = TypeHeader(kind: kind)

proc addField(b: var RecordBuilder, env: var TypeEnv, offset: IntVal,
              typ: TypeId; name = ""; mangle = true) =
  ## Adds a field declaration. `typ` is the type, `name` the name, and `mangle`
  ## indicates whether the name should be mangled.
  inc b.header.b
  if name.len > 0:
    b.fields.add RecField(typ: typ, offset: offset,
                          ident: env.idents.getOrIncl(name),
                          extra: (if mangle: MangleFlag else: 0))
  else:
    b.fields.add RecField(typ: typ, offset: offset)

proc addField(b: var RecordBuilder, offset: IntVal, typ: TypeId) =
  inc b.header.b
  b.fields.add RecField(typ: typ, offset: offset)

proc addField(b: var RecordBuilder, env: var TypeEnv, s: PSym, typ: TypeId) =
  var field = RecField(typ: typ, offset: env.toIntVal(s.offset),
                       align: s.alignment.int16)
  if {sfImportc, sfExportc} * s.flags == {}:
    field.ident = env.idents.getOrIncl(s.name.s)
    field.extra = MangleFlag
  else:
    # use the external name and disable mangling
    field.ident = env.idents.getOrIncl(s.extname)

  if sfNoalias in s.flags:
    field.extra = field.extra or NoAliasFlag

  # the bitsize value can only be in the range 0..64, so it fits well into the
  # lower 8 bit of `extra`
  field.extra = field.extra or uint16(s.bitsize)

  inc b.header.b
  b.fields.add field

proc addParam*(b: var ProcBuilder, s: set[ParamFlag], typ: TypeId) =
  ## Adds a parameter to the proc type.
  inc b.header.b
  b.params.add (cast[uint32](s), typ)

proc close(prev: var RecordBuilder, env: var TypeEnv,
           other: sink RecordBuilder): HeaderId =
  ## Closes `other`, commiting the type description to `env`. `prev` must be
  ## the builder `other` was previously spawned from.
  var header = other.header
  header.a += env.fields.len.uint32
  header.b += env.fields.len.uint32
  # move the fields to the environment:
  env.fields.add other.fields.toOpenArray(other.start, other.fields.high)
  other.fields.setLen(other.start)
  # hand the buffer back to the parent builder:
  swap(prev.fields, other.fields)

  # nested records are currently always anonymous and never de-duplicated:
  result = env.headers.add header

proc close(b: sink RecordBuilder, env: var TypeEnv; unique = false): HeaderId =
  ## Finalizes the record description and commits it to `env`. De-duplication
  ## is only performed if `unique` is false.
  var header = b.header
  header.a += env.fields.len.uint32
  header.b += env.fields.len.uint32
  var start = env.fields.len
  env.fields.add b.fields

  if unique:
    result = env.headers.add(header)
  else:
    # register the type description in the deduplication table
    var existed: bool
    (existed, result) = deduplicate(env, header)
    if existed:
      env.fields.setLen(start)

proc close(env: var TypeEnv, b: sink ProcBuilder): uint32 =
  ## Finished the type description, and returns its root node. If the type
  ## description already existed previously, no new one is added.
  var header = b.header
  header.a += env.params.len.uint32
  header.b += env.params.len.uint32
  var start = env.params.len
  env.params.add b.params

  # register the type description in the deduplication table
  var existed: bool
  (existed, result) = deduplicate(env, header)
  if existed:
    env.params.setLen(start)

# Type translation/lowering
# -------------------------

proc add*(env: var TypeEnv, t: PType): TypeId

proc recordToMir(env: var TypeEnv, rec: var RecordBuilder, n: PNode,
                 packed, canon: bool) =
  ## Translates record node/AST `n` to the corresponding MIR type description.
  template recurse(rec: var RecordBuilder, n: PNode) =
    recordToMir(env, rec, n, packed, canon)

  case n.kind
  of nkSym:
    var t = env.add(n.sym.typ)
    if canon:
      t = canonical(env, t)

    rec.addField(env, n.sym, t)
  of nkRecList:
    for it in n.items:
      recurse(rec, it)
  of nkRecCase:
    # at the moment, tagged union description are directly embedded into
    # their parent record
    var tu = rec.open(tkTaggedUnion)
    recurse(tu, n[0]) # discriminator
    for i in 1..<n.len:
      let child = n[i][^1]
      if child.kind == nkSym:
        recurse(tu, child)
      else:
        # start a new record
        var sub = tu.openRecord()
        if packed:
          sub.fields[^1].extra = 1 # mark as packed
        recurse(sub, child)
        let x = tu.close(env, sub)
        # add as field to the tagged union:
        tu.addField(IntVal(0), env.newType(x))

    let x = rec.close(env, tu)
    rec.addField(env.toIntVal(n[0].sym.offset), env.newType(x))
  else:
    unreachable(n.kind)

proc nextFieldPosition(t: PType): int =
  ## Computes the position of the would-be next field added to object-type `t`.
  let t = t.skipTypes(skipPtrs)
  if t.n.len > 0:
    var n = t.n
    # seek to the record's very last field:
    while n.kind != nkSym:
      n = n[^1]
    result = n.sym.position + 1
  elif t[0].isNil:
    # no super type and no body
    result = 0
  else:
    # no body, but there's a super type to inspect
    result = nextFieldPosition(t[0])

proc skipIrrelevant(t: sink PType): PType =
  ## Skips all types that don't contribute to the MIR type description.
  while t.kind in Skip and (t.sym.isNil or sfImportc notin t.sym.flags):
    t = t.lastSon
  result = t

proc add(env: var TypeEnv, desc: sink TypeHeader): HeaderId =
  ## Adds `desc` to `env`, but only if it doesn't exist there already.
  var existed: bool
  (existed, result) = deduplicate(env, desc)

proc objectBase(t: PType): PType =
  # skip ref/ptr types, but make sure to not skip instance types
  # wrapping the immediate object type
  result = t.base
  while result.kind notin {tyRef, tyPtr, tyObject}:
    result = result.lastSon

  case result.kind
  of tyRef, tyPtr:
    result = result.lastSon
  else:
    result = t.base # use the original base type

proc makeDesc(kind: TypeKind, size: IntVal, align: int16,
              typ: TypeId; other = 0'u32): TypeHeader {.inline.} =
  TypeHeader(kind: kind, size: size, align: align, a: typ.uint32, b: other)

proc typeToMir(env: var TypeEnv, t: PType; canon = false, unique=true): HeaderId =
  ## Translates `t` to its MIR representation. All structural types are
  ## deduplicated, meaning that two structural types with the same structure
  ## will result in the same ``HeaderId``. For ``tyObject`` types,
  ## deduplication only happens if `unique` is false.
  template typeref(typ: PType): TypeId =
    let t = env.add(typ)
    if canon: canonical(env, t)
    else:     t

  template single(k: TypeKind, elem: PType): HeaderId =
    env.add makeDesc(k, env.toIntVal(t.size), t.align, typeref elem)

  template simple(id: TypeId): HeaderId = env.symbols[id].desc[Original]

  case t.kind
  of tyVoid:    simple(VoidType)
  of tyBool:    simple(BoolType)
  of tyChar:    simple(CharType)
  of tyInt8:    simple(Int8Type)
  of tyInt16:   simple(Int16Type)
  of tyInt32:   simple(Int32Type)
  of tyInt64:   simple(Int64Type)
  of tyInt:     simple(env.sizeType)
  of tyUInt8:   simple(UInt8Type)
  of tyUInt16:  simple(UInt16Type)
  of tyUInt32:  simple(UInt32Type)
  of tyUInt64:  simple(UInt64Type)
  of tyUInt:    simple(env.usizeType)
  of tyFloat32: simple(Float32Type)
  of tyFloat:   simple(Float64Type)
  of tyFloat64: simple(Float64Type)
  of tyString:  simple(StringType)
  of tyCstring: simple(CstringType)
  of tyPointer, tyNil: simple(PointerType)
  of tyTuple:
    var tup = openRecord(env.toIntVal(t.size), t.align)
    if t.len == 0:
      tup.addField(IntVal 0, CharType)
    elif t.size < 0:
      # the size contains some incomplete imported types; no offsets can be
      # computed
      for i in 0..<t.len:
        tup.addField(env, IntVal 0, typeref t[i])
    else:
      var offset: BiggestInt = 0
      for i in 0..<t.len:
        let mask = t[i].align - 1
        offset = (offset + mask) and not(mask) # align the offset
        tup.addField(env, env.toIntVal(offset), typeref t[i])
        offset += t[i].size

    tup.close(env)
  of tyObject:
    let
      size = env.toIntVal(t.size)
    var
      rec: RecordBuilder
      isEmpty = false

    if tfUnion in t.flags:
      rec = open(tkUnion, size, t.align)
      isEmpty = t.n.len == 0
    elif t[0] != nil:
      # object has a super type
      let b = objectBase(t)
      rec = openRecord(size, t.align, b.nextFieldPosition, typeref(b))
    elif lacksMTypeField(t):
      # no super type and no type header
      rec = openRecord(size, t.align)
      isEmpty = t.n.len == 0
    elif (let rtti = env.graph.getCompilerProc("TNimTypeV2"); rtti != nil):
      # the object has a field for the RTTI
      let ptrTyp = env.newType(single(tkPtr, rtti.typ))
      # the type field is at position -1
      rec = openRecord(size, t.align, -1)
      rec.addField(env, IntVal 0, ptrTyp, "m_type")
    else:
      # legacy support for backends not yet using RTTI fields
      rec = openRecord(size, t.align)

    if rec.header.kind == tkRecord and tfPacked in t.flags:
      rec.fields[0].extra = 1 # mark as packed

    recordToMir(env, rec, t.n, tfPacked in t.flags, canon)

    if isEmpty:
      # record-like types must always have at least *one* field
      rec.addField(IntVal 0, CharType)

    # object/union types are not de-duplicated
    rec.close(env, unique)
  of tyProc:
    var prc: ProcBuilder
    let ret = if t[0].isNil: VoidType else: typeref(t[0])
    if t.callConv == ccClosure:
      prc = env.openProc(tkClosure, t.callConv, ret, tfVarargs in t.flags)
    else:
      prc = env.openProc(tkProc, t.callConv, ret, tfVarargs in t.flags)

    # future direction: static parameters need to be filtered out here.
    # Typedesc parameters only need to be removed in non-compile-time
    # execution contexts
    for i in 1..<t.len:
      var s: set[ParamFlag]
      if isPassByRef(env.config, t.n[i].sym, t[0]):
        s.incl pfByRef

      prc.addParam(s, typeref t[i])

    env.close(prc)
  of tyVar:
    # a ``var openArray`` is just an ``openArray``
    if classifyBackendView(t) == bvcSequence:
      typeToMir(env, t[0], canon)
    else:
      single(tkVar, t[0])
  of tyLent:
    if classifyBackendView(t) == bvcSequence:
      typeToMir(env, t[0], canon)
    else:
      single(tkLent, t[0])
  of tyRef:
    single(tkRef, t[^1])
  of tyPtr:
    single(tkPtr, t[^1])
  of tyOpenArray, tyVarargs:
    single(tkOpenArray, t[0])
  of tyArray:
    # in terms of in-memory representation, array always have a length of at
    # least 1
    let len = max(lengthOrd(env.config, t), One)
    env.add makeDesc(tkArray, env.toIntVal(t.size), t.align,
                     typeref elemType(t), uint32 env.toIntVal(toInt len))
  of tySequence:
    single(tkSeq, t[0])
  of tySet:
    # sets always have a length <= 2^16
    let len = toUInt32 lengthOrd(env.config, t)
    env.add TypeHeader(kind: tkSet, size: env.toIntVal(t.size), align: t.align,
                       a: len)
  of tyUncheckedArray:
    single(tkUncheckedArray, t[0])
  of tyTypeDesc, tyStatic, tyUntyped, tyTyped:
    # have no relevance in the MIR's type syste, beyond taking up slots
    # XXX: untyped/typed shouldn't reach here, but currently they do
    simple(VoidType)
  of tyEnum, tyOrdinal, tyRange:
    # the underlying type is usually a simple, single-node type, so
    # translate it directly
    typeToMir(env, t.lastSon, canon)
  of tyUserTypeClasses, tyGenericInst, tyInferred, tySink, tyAlias, tyDistinct:
    # use a type-reference instead of in-place translation. This prevents
    # unnecessary de-duplication work for, e.g., object types
    single(tkIndirect, t.lastSon)
  else:
    unreachable(t.kind)

proc initTypeEnv*(graph: ModuleGraph): TypeEnv =
  ## Returns a fully initialized type environment instance.
  result = TypeEnv(symbols: default(Store[TypeId, TypeSym]))
  result.graph = graph
  result.config = graph.config

  template add(ttk: TTypeKind, expect: TypeId, tk: TypeKind) =
    block:
      let
        typ = graph.getSysType(unknownLineInfo, ttk)
        desc = result.headers.add:
          TypeHeader(kind: tk, size: IntVal(typ.size), align: typ.align)
        id  = result.symbols.add:
          TypeSym(inst: typ, canon: expect, desc: [desc, desc, desc])

      assert id == expect
      # register a mapping:
      result.map[typ] = id
      result.canon[desc] = id

  # setup the built-in types:
  add tyVoid,    VoidType,    tkVoid
  add tyBool,    BoolType,    tkBool
  add tyChar,    CharType,    tkChar
  add tyInt8,    Int8Type,    tkInt
  add tyInt16,   Int16Type,   tkInt
  add tyInt32,   Int32Type,   tkInt
  add tyInt64,   Int64Type,   tkInt
  add tyUInt8,   UInt8Type,   tkUInt
  add tyUInt16,  UInt16Type,  tkUInt
  add tyUInt32,  UInt32Type,  tkUInt
  add tyUInt64,  UInt64Type,  tkUInt
  add tyFloat32, Float32Type, tkFloat
  add tyFloat64, Float64Type, tkFloat
  add tyString,  StringType,  tkString
  add tyCstring, CstringType, tkCstring
  add tyPointer, PointerType, tkPointer

  (result.sizeType, result.usizeType) =
    case graph.config.target.intSize
    of 1, 2, 4: (Int32Type, UInt32Type)
    of 8:       (Int64Type, UInt64Type)
    else:       unreachable()

  # also register the built-in unspecified-width types. This prevents int/float
  # literal types from being added to the environment
  discard result.add(graph.getSysType(unknownLineInfo, tyInt))
  discard result.add(graph.getSysType(unknownLineInfo, tyFloat))

  # setup the lowered representation of the string type
  let str = graph.getCompilerProc("NimStringV2")
  if str != nil: # not all code generators use the lowered type yet
    let base = result.add(str.typ)
    # redirect NimStringV2 in a way such that the code generators will
    # treat it as being the same as ``string``
    # XXX: this is an interim solution. Ultimately, the definition of
    #      ``string`` in the system module should express this directly
    result.symbols[StringType].desc[Lowered] =
      result.symbols[base].desc[Lowered]
    result.symbols[base].canon = StringType

proc newPtrTy(env: var TypeEnv, elem: TypeId): TypeId =
  # inherit size and alignment information from the base pointer type
  var h = env.headerFor(PointerType, Original)
  h.kind = tkPtr
  h.a = elem.uint32
  env.newType(env.add h)

proc newUncheckedArrayTy(env: var TypeEnv, elem: TypeId): TypeId =
  let desc = makeDesc(tkUncheckedArray, IntVal(0),
                      env.headerFor(elem, Original).align, elem)
  env.newType(env.add desc)

template buildProc*(env: var TypeEnv, kind: TypeKind, conv: TCallingConvention,
                    ret: TypeId, builder, body: untyped): TypeId =
  ## Convenience template for a structured way of producing type descriptions.
  block:
    var builder = openProc(env, kind, conv, ret, false)
    body
    env.newType(env.close(builder))

template buildRecord(env: var TypeEnv, size: IntVal, align: int16,
                     builder, body: untyped): HeaderId =
  block:
    var builder = openRecord(size, align)
    body
    builder.close(env)

proc lowerType(env: var TypeEnv, graph: ModuleGraph, id: HeaderId): HeaderId =
  let h = env.headers[id]
  case h.kind
  of tkSet:
    # either an array or integer, depending on the number of elements
    case env.getInt(h.size)
    of 1: env.symbols[UInt8Type].desc[Lowered]
    of 2: env.symbols[UInt16Type].desc[Lowered]
    of 4: env.symbols[UInt32Type].desc[Lowered]
    of 8: env.symbols[UInt64Type].desc[Lowered]
    else:
      # -> array[size, uint8]
      env.add makeDesc(tkArray, h.size, h.align, UInt8Type, h.size.uint32)
  of tkClosure:
    # -> (ClP_0: proc, ClE_0: pointer)
    let prc = env.buildProc(tkProc, ccClosure, h.retType(env), bu):
      for _, typ, flags in params(env, h):
        bu.addParam(flags, typ)

    env.buildRecord(h.size, h.align, bu):
      bu.addField(env, IntVal 0, prc, "ClP_0", mangle=false)
      # XXX: the type of the environment pointer should be a ``RootRef``
      bu.addField(env, IntVal graph.config.target.ptrSize,
                  PointerType, "ClE_0", mangle=false)
  of tkOpenArray:
    # -> (ptr UncheckedArray[T], int)
    let ptrTyp = env.newPtrTy(env.newUncheckedArrayTy(h.elem))

    env.buildRecord(h.size, h.align, bu):
      bu.addField(env, IntVal 0, ptrTyp)
      bu.addField(env, IntVal graph.config.target.ptrSize, env.sizeType)
  of tkSeq:
    # -> (cap: int, data: ptr (int, UncheckedArray[T]))
    let
      dataType = env.newUncheckedArrayTy(h.elem)
      # the payload type's name is inferred from the body
      payload = env.buildRecord(h.size, h.align, bu):
        bu.addField(env, IntVal 0, env.sizeType, "cap")
        bu.addField(env, IntVal graph.config.target.intSize, dataType, "data")
      ppTyp = env.newPtrTy(env.newType(payload))

    env.buildRecord(h.size, h.align, bu):
      bu.addField(env, IntVal 0, env.sizeType, "len")
      bu.addField(env, IntVal graph.config.target.intSize, ppTyp, "p")
  else:
    id

proc typeSymToMir(env: var TypeEnv, t: PType): TypeId =
  discard getSize(env.config, t) # compute size, alignment, and field offsets

  if t.kind == tyObject:
    if sfCompilerProc in t.sym.flags:
      # compilerproc types can be defined in multiple modules (see
      # ``TNimType``). Only create a type symbol for the instance that's
      # registered in the compilerproc table
      # XXX: this is workaround. Compilerproc types should only be defined
      #      a single time within a project
      let real = env.graph.getCompilerProc(t.sym.name.s).typ
      if t != real:
        result = env.add(real)
        env.map[t] = result
        return

    # register the type symbol *first*. This prevents infinite recursion for
    # cyclic types
    result = env.symbols.add TypeSym(inst: t, canon: env.symbols.nextId())
    env.map[t] = result

    let
      orig  = typeToMir(env, t, canon=false)
      canon = typeToMir(env, t, canon=true, unique=(tfFromGeneric notin t.flags))

    # there's nothing to lower for object types
    env.symbols[result].desc = [orig, canon, canon]

    # generic types support covariance for tuples. Pick an instance as the
    # "canonical" one, so that - for example - ``Generic[(int,)]`` and
    # ``Generic[tuple[x: int]]`` map to the same MIR type in the end. In order
    # to support cyclic types, ``sighashes`` has to be used
    if tfFromGeneric in t.flags and
       (let c = env.instances.mgetOrPut(hashType(t, {CoType, CoDistinct}),
                                        result);
        c != result):
      env.symbols[result].canon = c
  else:
    # create the type description preserving the original type symbols:
    let
      orig  = typeToMir(env, t, canon=false)
      canon = typeToMir(env, t, canon=true)
    var lowered: HeaderId

    var prev = env.canon.getOrDefault(canon, env.symbols.nextId())
    if prev == env.symbols.nextId():
      # the new type symbol is the *canonical* one. Create the lowered
      # representation
      lowered = lowerType(env, env.graph, canon)
      # lowering could have added new type symbols
      prev = env.symbols.nextId()
      env.canon[canon] = prev
    else:
      # a canonical type symbol already exists. Inherit its lowered
      # representation
      lowered = env.symbols[prev].desc[Lowered]

    # now add the symbol and mapping:
    result = env.symbols.add TypeSym(inst: t, canon: prev,
                                     desc: [orig, canon, lowered])
    env.map[t] = result

proc handleImported(env: var TypeEnv, t: PType): TypeId =
  if t.sym != nil and sfImportc in t.sym.flags:
    # an imported type. It's wrapped in a ``tkImported``, referencing the
    # underlying type
    let base = typeSymToMir(env):
      if t.kind in Skip:
        t.lastSon.skipIrrelevant()
      else:
        t

    discard getSize(env.config, t) # compute the sizes, alignments, and offsets

    let
      size  = env.toIntVal(t.size)
      orig  = env.add makeDesc(tkImported, size, t.align, base)
      canon = env.add makeDesc(tkImported, size, t.align,
                               env.canonical(base))
    result = env.symbols.add TypeSym(inst: t, canon: env.symbols.nextId(),
                                     desc: [orig, canon, canon])

    # doesn't matter if a symbol mapping already exists (happens when
    # `base` == `t`); override it
    env.map[t] = result
  else:
    result = typeSymToMir(env, t)

proc add*(env: var TypeEnv, t: PType): TypeId =
  ## If not registered yet, adds `t` to `env` and returns the ID to later
  ## look it up with.
  result = env.map.getOrDefault(t, env.symbols.nextId())
  if result == env.symbols.nextId(): # not seen yet?
    result = handleImported(env, t)
    # translation of the type registered the mapping for us

func get*(env: TypeEnv, id: TypeId): lent TypeSym =
  ## Returns the symbol for `id`.
  env.symbols[id]

template `[]`*(env: TypeEnv, id: FieldId): RecField =
  env.fields[ord(id)]

template `[]`*(env: TypeEnv, t: PType): TypeId =
  env.map[t]

func `[]`*(env: TypeEnv, id: TypeId): lent PType {.inline.} =
  # XXX: this procedure needs to eventually start returning the ``TypeSym``
  #      instead
  env.symbols[id].inst

func sizeType*(env: TypeEnv): TypeId {.inline.} =
  ## Returns the type to use for values representing some size. This is a
  ## signed integer type of target-dependent bit-width.
  env.sizeType

func usizeType*(env: TypeEnv): TypeId {.inline.} =
  ## Returns the type to use for values representing some size. This is an
  ## unsigned integer type of target-dependent bit-width.
  env.usizeType
