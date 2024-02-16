## This module contains the type definitions for a packed representation of
## all the state required for loading and running a standalone VM instance.
##
## In addition, procedures for storing and loading the original objects to and
## from the packed environment object are also located here.
##
## For reading and writing `PackedEnv` to files, a ``rodfile``-based solution
## is provided.
##
## First collecting/packing into a data structure that is then written to disk
## in a separate is not as resource efficient (both in terms of memory
## consumption and time) as doing both steps in a combined manner. The first
## (and current) approach is less complex however, which is why it's chosen
## over the latter.

import
  std/[
    hashes,
    tables
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    lineinfos,
    types
  ],
  compiler/front/[
    options
  ],
  compiler/ic/[
    bitabs,
    rodfiles
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    idioms,
    int128,
    pathutils # for `AbsoluteFile`
  ],
  compiler/vm/[
    vmdef,
  ]

from compiler/vm/vmaux import findRecCase, findMatchingBranch
from compiler/vm/vmobjects import `$`, packDiscr
from compiler/vm/vmtypes import alignedSize

export RodFileError

type
  PackedDataKind* = enum
    pdkInt
    pdkIntLit ## an embedded literal
    pdkFloat
    pdkString
    pdkPtr # also includes ref
    pdkObj
    pdkArray
    pdkSet
    pdkField

  # XXX: `PackedDataNode` is memory-usage wise quite inefficient. Not only at
  #      the layout level (3 out of it's 8 bytes are wasted for padding), but
  #      also for representing data in general. For example: `array[256, byte]`
  #      requires 2048 bytes!
  #      A possible different approach: store the data in it's
  #      raw-byte-serialized representation, with additional metadata
  #      describing where strings and seqs (i.e. values with non-automatic
  #      storage) should be placed. Maybe also only store the non-zero parts
  PackedDataNode* = object
    ## A single node in a depth-first linear tree that is meant for storing
    ## complex data. If a node has children, they follow directly after their
    ## parent node. `PackedDataNode` can be seen as a specialized
    ## version of `PackedNodeLite`
    kind*: PackedDataKind
    pos*: uint32 ## for {pdkInt, pdkFloat, pdkString}: the respective `LitId`
                 ## for {pdkObj, pdkArray, pdkSet}: the number of children
                 ## for pdkPtr: `0` if nil, `1` otherwise
                 ## for `pdkField`: the field's position
                 ## for `pdkIntLit`: a direct literal that fits into 4 byte

  # PackedSymLite, PackedNodeLite, etc. are similiar to the types in
  # ``packed_ast.nim``, with the difference that the types here only store the
  # data necessary for their use-case (they're used to provide type
  # information for `repr`) and are thus more compact.

  SymId = distinct int32
  TypeId = distinct int32
  NodeId = distinct int32

  PackedSymLite = object
    kind: TSymKind
    magic: TMagic
    name: LitId
    typ: TypeId
    ast: NodeId
    flags: TSymFlags
    position: int
    offset: int

  PackedTypeLite = object
    kind: TTypeKind
    callConv: TCallingConvention
    flags: TTypeFlags
    types: seq[TypeId]
    n: NodeId
    sym: SymId
    #typeInst*: TypeId

  PackedNodeLite = object
    kind: TNodeKind
    flags: TNodeFlags
    operand: int32  # for kind in {nkSym}: SymId
                    # for kind in {nkStrLit, nkIdent, nkNumberLit}: LitId
                    # for all others: the number of children
    typeId: TypeId

  SliceListType* = BiggestInt|BiggestFloat|ConstantId

  PackedVmType* = object
    # XXX: inefficient storage. For statically sized atoms, `size` and `align`
    #      are redundant, wasting space. `VmType` has the same problem
    size*: uint32
    align*: uint8
    kind*: AtomKind
    numFields*: uint16

  # XXX: the rodfile interface for `BiTable`s stores both `keys` and `values`,
  #      even though `keys` can be reconstructed during loading (at least in
  #      our use-case here). This makes the resulting file larger than
  #      necessary

  PackedEnv* = object
    strings*: BiTable[string]
    numbers*: BiTable[BiggestInt] # also includes floats

    files*: seq[string]
    infos*: BiTable[TLineInfo]
    dbgSyms*: seq[tuple[name, info: LitId]] # for debugging only

    nodes*: seq[PackedDataNode] ## complex data. Currently used for storing
                                ## complex constants
    consts*: seq[(ConstantKind, uint32)]
    cconsts*: seq[tuple[typ: VmTypeId, packedId: uint32]] ##
      ## Packed `TCtx.complexConsts`. The constants type together with an
      ## index referencing a sub-tree in `nodes`

    tfields*: seq[tuple[offset: uint32, typId: uint32]]
    tbranches*: seq[BranchListEntry]
    types*: seq[PackedVmType]

    globals*: seq[VmTypeId] ## All globals. Only their types are stored
    functions*: seq[tuple[sym: uint32, sig: RoutineSigId, t1: VmTypeId,
                          isClosure: bool, kind: CallableKind, a, b: uint32]]
    callbacks*: seq[string]

    code*: seq[TInstr]
    debug*: seq[uint32] # Packed version of `TCtx.debug`. Indices into `infos`
    ehTable*: seq[HandlerTableEntry]
    ehCode*: seq[EhInstr]

    # rtti related data:
    nimNodes: seq[PackedNodeLite]
    nimSyms: seq[PackedSymLite]
    nimTypes: seq[PackedTypeLite]
    typeInfos: seq[tuple[nt: TypeId, t: VmTypeId]] ##
      ## Packed version of `TCtx.rtti`

    entryPoint*: FunctionIndex

  TypeInfoDecoder = object
    types: seq[PType]
    syms: seq[PSym]
    nodes: seq[PNode]

  TypeInfoEncoder = object
    types: Table[ItemId, TypeId]
    syms: Table[ItemId, SymId]

    pendingTypes: seq[(PType, TypeId)]
    pendingSyms: seq[(PSym, SymId)]

  PackedEncoder* = object
    typeInfoEnc: TypeInfoEncoder

    # XXX: `typeMap` is only necessary until `PVmType` gets replaced
    #      by `VmTypeId`
    typeMap*: Table[PVmType, VmTypeId] ## Maps each type instance to it's ID

  DataEncoder* = object
    ## Contextual state needed for turning data `PNode`-trees into
    ## `PackedDataNode` trees and storing them into the packed environment
    config*: ConfigRef
    i: int ## the index in `PackedEnv.nodes` where the next item is to be stored

const
  EmbeddedUInts = {nkCharLit, nkUInt8Lit..nkUInt32Lit}
  EmbeddedInts = {nkInt8Lit..nkInt32Lit} # these also fit into a `uint32`
  ExternalInts = {nkIntLit, nkInt64Lit, nkUIntLit, nkUInt64Lit}

  NilSymId = -1.SymId
  NilTypeId = -1.TypeId


# -------- general utilities ---------------------------------------------

func growBy[T](x: var seq[T], n: Natural) {.inline.} =
  x.setLen(x.len + n)

iterator lpairs[T](x: openArray[T]): tuple[key: int, val: lent T] =
  ## Custom `pairs` iterator that uses `lent`. As of this comment, the stdlib
  ## one doesn't
  var i = 0
  let L = x.len
  while i < L:
    yield (i, x[i])
    inc i

template mapList*[D, S](d: seq[D], s: openArray[S], it: untyped, code) =
  ## `s` and `d` get evaluated multiple times, so beware
  bind lpairs
  d.newSeq(s.len)
  for i, it {.inject.} in lpairs(s):
    d[i] = code

# -------- accessors and shortcuts ---------------------------------------

func hash(x: PVmType): int {.inline.} =
  # Don't use `x` directly, as the lower 2-3 bits are always empty (due to
  # the 4 or 8 byte alignment of `VmType`)
  hash(cast[int](x))

func `==`(a, b: TypeId): bool {.borrow.}

func `==`(a, b: SymId): bool {.borrow.}

template genAccessors(t: type, f: untyped, idTyp: type) {.dirty.} =
  template `[]`(self: t, id: idTyp): untyped =
    self.f[id.int]

  template `[]=`(self: var t, id: idTyp, v: untyped) =
    self.f[id.int] = v

genAccessors(PackedEnv, nimSyms, SymId)
genAccessors(PackedEnv, nimTypes, TypeId)

genAccessors(TypeInfoDecoder, syms, SymId)
genAccessors(TypeInfoDecoder, types, TypeId)


func getLitId(e: var PackedEnv, x: string): LitId {.inline.} =
  e.strings.getOrIncl(x)

func getLitId(e: var PackedEnv, x: BiggestInt): LitId {.inline.} =
  e.numbers.getOrIncl(x)

func getLitId(e: var PackedEnv, x: BiggestFloat): LitId {.inline.} =
  e.numbers.getOrIncl(cast[BiggestInt](x))

# -------- data storing --------------------------------------------------

func startEncoding*(enc: var DataEncoder, e: PackedEnv) {.inline.} =
  enc.i = e.nodes.len

func put(enc: var DataEncoder, e: var PackedEnv,
         d: sink PackedDataNode) {.inline.} =
  e.nodes[enc.i] = d
  inc enc.i

func putLater(enc: var DataEncoder): int {.inline.} =
  result = enc.i
  inc enc.i

func setAt(enc: var DataEncoder, e: var PackedEnv, i: int,
           d: sink PackedDataNode) {.inline.} =
  e.nodes[i] = d

func storeDataNode(enc: var DataEncoder, e: var PackedEnv,
                   t: MirTree, n: NodePosition)
  ## Stores in `e.nodes` the data represented by the MIR constant expression
  ## `t`. The caller is responsible for making sure that there's a slot
  ## allocated in `e.nodes` for the top data-node. Space allocation for the
  ## sub-data-nodes is handled by ``storeData``.

func storeDiscrData(enc: var DataEncoder, e: var PackedEnv, s: PSym, v: PNode) =
  let
    recCase = findRecCase(s.owner.typ, s)
    b = findMatchingBranch(recCase, getInt(v))
  assert b != -1
  # We don't have access to vm type information here, so 32 is always
  # used for `numBits`. This is safe, since the both `value` and `index`
  # can only take up a maximum of 16 bits.  The loading logic takes care
  # of repacking the discriminator with the correct `numBits`
  # XXX: 16 could be safely used for `numBits` and then the resulting
  #      value could be stored as a `pdkIntLit`
  let val = packDiscr(v.intVal, b, numBits = 32)
  enc.put e, PackedDataNode(kind: pdkInt, pos: e.getLitId(val).uint32)

proc storeFieldsData(enc: var DataEncoder, e: var PackedEnv,
                     t: MirTree, n: NodePosition) =
  let count = t[n].len
  enc.put e, PackedDataNode(kind: pdkObj, pos: count.uint32)
  e.nodes.growBy(count * 2) # make space for the content

  # iterate over all fields in the construction and pack and store them:
  var n = n + 1
  for _ in 0..<count:
    let s = t[n].field ## the field symbol
    inc n # move the cursor to the field's data

    enc.put e, PackedDataNode(kind: pdkField, pos: s.position.uint32)

    if sfDiscriminant notin s.flags:
      enc.storeDataNode(e, t, n+1)
    else:
      enc.storeDiscrData(e, s, t[n+1].lit)

    n = t.sibling(n) # move the cursor to the next field

proc storeTupleData(enc: var DataEncoder, e: var PackedEnv,
                    t: MirTree, n: NodePosition) =
  let count = t[n].len
  enc.put e, PackedDataNode(kind: pdkObj, pos: count.uint32)
  e.nodes.growBy(count * 2) # make space for the content

  # pack and store all elements:
  var n = n + 1
  for i in 0..<count:
    enc.put e, PackedDataNode(kind: pdkField, pos: i.uint32)
    enc.storeDataNode(e, t, n+1)
    n = t.sibling(n)

proc storeArrayData(enc: var DataEncoder, e: var PackedEnv,
                    t: MirTree, n: NodePosition) =
  let count = t[n].len
  enc.put e, PackedDataNode(kind: pdkArray, pos: count.uint32)
  e.nodes.growBy(count) # make space for the content

  # encode all elements:
  var n = n + 1
  for _ in 0..<count:
    enc.storeDataNode(e, t, n+1)
    n = t.sibling(n)

proc storeSetData(enc: var DataEncoder, e: var PackedEnv,
                  t: MirTree, n: NodePosition) =
  let
    count = t[n].len
    typ = t[n].typ
  enc.put e, PackedDataNode(kind: pdkSet, pos: count.uint32 * 2)
  e.nodes.growBy(count * 2) # make space for the content

  proc adjusted(enc: DataEncoder, n: PNode, typ: PType): uint32 =
    # make the range start at zero
    toUInt32(getInt(n) - firstOrd(enc.config, typ))

  var n = n + 1
  # bitsets only store values in the range 0..high(uint16), so the values can
  # be stored directly
  for _ in 0..<count:
    let x = t[n+1].lit
    if x.kind == nkRange:
      enc.put e, PackedDataNode(kind: pdkIntLit, pos: adjusted(enc, x[0], typ))
      enc.put e, PackedDataNode(kind: pdkIntLit, pos: adjusted(enc, x[1], typ))
    else:
      let d = PackedDataNode(kind: pdkIntLit, pos: adjusted(enc, x, typ))
      enc.put e, d
      enc.put e, d

    n = t.sibling(n)

func storeLiteral(enc: var DataEncoder, e: var PackedEnv, n: PNode) =
  let dstIdx = enc.putLater()
  let (kind, item) =
    case n.kind
    of EmbeddedUInts: (pdkIntLit, n.intVal.uint32)
    of EmbeddedInts:  (pdkIntLit, cast[uint32](n.intVal))
    of ExternalInts:  (pdkInt,    e.getLitId(n.intVal).uint32)
    of nkFloatKinds:  (pdkFloat,  e.getLitId(n.floatVal).uint32)
    of nkStrKinds:    (pdkString, e.getLitId(n.strVal).uint32)
    of nkNilLit:
      if n.typ.skipTypes(abstractInst).callConv == ccClosure:
        # XXX: some unexpanded `nil` closure literals reach here, so we have
        #      to expand them here. This needs to happen earlier
        e.nodes.growBy(4)
        enc.put e, PackedDataNode(kind: pdkField, pos: 0)
        enc.put e, PackedDataNode(kind: pdkPtr, pos: 0)
        enc.put e, PackedDataNode(kind: pdkField, pos: 0)
        enc.put e, PackedDataNode(kind: pdkPtr, pos: 0)
        (pdkObj, 2'u32)
      else:
        (pdkPtr, 0'u32)
    else:             unreachable(n.kind)
  enc.setAt e, dstIdx, PackedDataNode(kind: kind, pos: item)

func storeDataNode(enc: var DataEncoder, e: var PackedEnv,
                   t: MirTree, n: NodePosition) =
  case t[n].kind
  of mnkLiteral:
    storeLiteral(enc, e, t[n].lit)
  of mnkProc:
    # the ID is stable, it can be packed directly
    enc.put e, PackedDataNode(kind: pdkIntLit, pos: t[n].prc.uint32)
  of mnkConstr:
    case t[n].typ.skipTypes(abstractInst).kind
    of tySequence, tyArray, tyOpenArray:
      enc.storeArrayData(e, t, n)
    of tyTuple, tyProc:
      enc.storeTupleData(e, t, n)
    of tySet:
      enc.storeSetData(e, t, n)
    else:
      unreachable(t[n].kind)
  of mnkObjConstr:
    enc.storeFieldsData(e, t, n)
  else:
    unreachable(t[n].kind)

func storeData*(enc: var DataEncoder, e: var PackedEnv, tree: MirTree): int =
  ## Packs the MIR constant expression `tree` and puts it into `e`. Returns
  ## the index of the top data node.
  result = enc.i
  e.nodes.growBy(1)
  storeDataNode(enc, e, tree, NodePosition 0)

func getIntVal*(pe: PackedEnv, n: PackedDataNode): BiggestInt {.inline.} =
  case n.kind
  of pdkInt:    pe.numbers[n.pos.LitId]
  of pdkIntLit: n.pos.BiggestInt
  else:         unreachable(n.kind)

func getFloatVal*(pe: PackedEnv, n: PackedDataNode): BiggestFloat {.inline.} =
  assert n.kind == pdkFloat
  cast[BiggestFloat](pe.numbers[n.pos.LitId])

proc storeSliceList[T: SliceListType](e: var PackedEnv,
                                      sl: seq[Slice[T]]): uint32 =
  const elemKind =
    when T is BiggestInt: pdkInt
    elif T is BiggestFloat: pdkFloat
    elif T is ConstantId: pdkIntLit

  template toId(a): uint32 =
    when T is ConstantId: a.uint32
    else: e.getLitId(a).uint32

  result = e.nodes.len.uint32
  e.nodes.growBy(sl.len * 2 + 1)
  e.nodes[result] = PackedDataNode(kind: pdkSet, pos: sl.len.uint32)

  let start = result.int + 1
  for i, it in sl.pairs:
    let
      i1 = start + i * 2
      i2 = i1 + 1

    if it.a == it.b:
      let elem = PackedDataNode(kind: elemKind, pos: toId(it.a))
      e.nodes[i1] = elem
      e.nodes[i2] = elem
    else:
      e.nodes[i1] = PackedDataNode(kind: elemKind, pos: toId(it.a))
      e.nodes[i2] = PackedDataNode(kind: elemKind, pos: toId(it.b))

proc loadSliceList*[T: SliceListType](p: PackedEnv, id: uint32): seq[Slice[T]] =
  let top = p.nodes[id]
  assert top.kind == pdkSet

  let
    L = top.pos.int
    start = id.int + 1

  template get(i): untyped =
    let n = p.nodes[i]
    when T is BiggestInt:
      assert n.kind == pdkInt
      p.numbers[n.pos.LitId]
    elif T is BiggestFloat:
      p.getFloatVal(n)
    elif T is ConstantId:
      assert n.kind == pdkIntLit
      n.pos.ConstantId

  result.newSeq(L)
  for i in 0..<L:
    let
      nI = start + i * 2
      a = get(nI + 0)
      b = get(nI + 1)

    result[i] = a..b

# -------- `VmType` store and load ---------------------------------------

func numFields(t: PVmType): int =
  case t.kind
  of akInt, akFloat, akPNode: 0
  of akPtr, akRef, akSeq, akString, akSet, akDiscriminator, akCallable: 1
  of akArray: 1
  of akObject: 1 + t.objFields.len

func storeVmType(enc: var PackedEncoder, dst: var PackedEnv, t: PVmType): PackedVmType =
  let
    nf = numFields(t)
    fieldStart = dst.tfields.len

  result = PackedVmType(size: t.sizeInBytes.uint32,
                        align: t.alignment,
                        kind: t.kind,
                        numFields: nf.uint16)

  if nf == 0:
    return

  dst.tfields.growBy(nf)
  dst.tfields[fieldStart] =
    case t.kind
    of akPtr, akRef:          (0'u32, enc.typeMap[t.targetType])
    of akSeq, akString:
      # it's not strictly necessary to store the stride (since it can
      # currently be computed from the element type)
      (t.seqElemStride.uint32, enc.typeMap[t.seqElemType])

    of akSet:                 (t.setLength.uint32, 0'u32)
    of akDiscriminator:       (t.numBits.uint32, 0'u32)
    of akCallable:            (t.routineSig.uint32, 0'u32)
    of akArray:               (t.elementCount.uint32, enc.typeMap[t.elementType])
    of akObject:              (t.relFieldStart, t.branches.len.uint32)
    of akInt, akFloat, akPNode:
      unreachable()

  if t.kind == akObject:
    for i, f in t.objFields.pairs:
      dst.tfields[fieldStart + 1 + i] = (f.offset.uint32, enc.typeMap[f.typ])

    let bStart = dst.tbranches.len
    dst.tbranches.growBy(t.branches.len)

    for i, b in t.branches.pairs:
      dst.tbranches[bStart + i] = b

func loadVmType(s: PackedEnv, types: seq[PVmType],
                t: var VmType, id: uint32,
                fstart, bstart: int): tuple[numFields, numBranches: int] =
  let
    pt = s.types[id]
    numFields = pt.numFields

  t = VmType(kind: pt.kind, sizeInBytes: pt.size, alignment: pt.align)

  if pt.kind in {akInt, akFloat, akPNode}:
    return (0, 0)

  assert numFields > 0
  result = (numFields.int, 0) # valid for all atom kinds except `akObject`

  let firstField = s.tfields[fstart]
  case pt.kind
  of akInt, akFloat, akPNode:
    unreachable()
  of akPtr, akRef:
    t.targetType = types[firstField.typId]
  of akSeq, akString:
    t.seqElemStride = firstField.offset.int
    t.seqElemType = types[firstField.typId]
  of akSet:
    t.setLength = firstField.offset.int
  of akDiscriminator:
    t.numBits = firstField.offset.int
  of akArray:
    t.elementCount = firstField.offset.int
    t.elementType = types[firstField.typId]
    t.elementStride = alignedSize(t.elementType).int
  of akCallable:
    t.routineSig = firstField.offset.RoutineSigId
  of akObject:
    t.relFieldStart = firstField.offset
    # The `typId` is used to store the number of branch list entries:
    let branchListLen = firstField.typId.int

    {.cast(noSideEffect).}: # faulty side-effect-analysis workaround
      t.objFields.newSeq(numFields - 1)
      for i in 1..<numFields.int:
        let f = s.tfields[fstart + i]
        t.objFields[i - 1] = (f.offset.int, types[f.typId])

      t.branches.newSeq(branchListLen)
      for i in 0..<branchListLen:
        t.branches[i] = s.tbranches[bstart + i]

    result.numBranches = branchListLen

func loadVmTypes(src: PackedEnv): seq[PVmType] =
  var
    fstart = 0
    bstart = 0

  result.newSeq(src.types.len + 1) # +1 since the invalid type is not
                                      # included

  # types can reference other types that have higher ID values. Loading all
  # types in a single pass thus won't work, since the referenced type might
  # still be a nil `PVmType`. So instead, we first create a `PVmType` for
  # each type and then fill them in a separate second step

  for id in 0..<src.types.len:
    result[id+1] = new(VmType)

  for id in 0..<src.types.len:
    let
      t = result[id+1]
      (fl, bl) = loadVmType(src, result, t[], id.uint32, fstart, bstart)
    fstart += fl
    bstart += bl


func storeDbgSym(dst: var PackedEnv, sym: PSym): uint32 =
  let
    info = dst.infos.getOrIncl(sym.info)
    name = dst.strings.getOrIncl(sym.name.s)

  result = dst.dbgSyms.len.uint32
  dst.dbgSyms.add((name: name, info: info))

# -------- auxilliary RTTI data store/load -------------------------------

func storeType(enc: var TypeInfoEncoder, ps: var PackedEnv, t: PType): TypeId
func storeSym(enc: var TypeInfoEncoder, ps: var PackedEnv, s: PSym): SymId

func storeSymLater(enc: var TypeInfoEncoder, ps: var PackedEnv, s: PSym): SymId =
  if s == nil:
    return NilSymId

  let next = ps.nimSyms.len.SymId

  result = enc.syms.mgetOrPut(s.itemId, next)
  if result == next:
    ps.nimSyms.growBy(1)
    enc.pendingSyms.add((s, next))

func storeTypeLater(enc: var TypeInfoEncoder, ps: var PackedEnv, t: PType): TypeId =
  if t == nil:
    return NilTypeId

  let next = ps.nimTypes.len.TypeId

  result = enc.types.mgetOrPut(t.itemId, next)
  if result == next:
    ps.nimTypes.growBy(1)
    enc.pendingTypes.add((t, next))

func storeNode(enc: var TypeInfoEncoder, ps: var PackedEnv, n: PNode): NodeId =
  if n == nil:
    return NodeId(-1)

  var hasSons = false
  let item =
    case n.kind
    of nkEmpty, nkType, nkNilLit, nkCommentStmt: 0'i32 # zero children
    of nkIdent: ps.getLitId(n.ident.s).int32
    of nkSym:   enc.storeSymLater(ps, n.sym).int32
    of nkIntKinds:   ps.getLitId(n.intVal).int32
    of nkFloatKinds: ps.getLitId(n.floatVal).int32
    of nkStrKinds:   ps.getLitId(n.strVal).int32
    of nkWithSons:
      hasSons = true
      n.sons.len.int32
    of nkError, nkNone:
      unreachable("errors and invalid nodes must not reach here")

  result = ps.nimNodes.len.NodeId
  ps.nimNodes.add(PackedNodeLite(kind: n.kind, flags: n.flags,
                                 operand: item,
                                 typeId: storeTypeLater(enc, ps, n.typ)))

  if hasSons:
    for s in n.sons.items:
      discard storeNode(enc, ps, s)

func storeSymAt(enc: var TypeInfoEncoder, ps: var PackedEnv, s: PSym, id: SymId) =
  let p = PackedSymLite(kind: s.kind, magic: s.magic,
                        name: ps.getLitId(s.name.s),
                        typ: storeType(enc, ps, s.typ),
                        ast: storeNode(enc, ps, s.ast),
                        flags: s.flags,
                        position: s.position, offset: s.offset)
  ps[id] = p

func storeTypeAt(enc: var TypeInfoEncoder, ps: var PackedEnv, t: PType, id: TypeId) =
  var p = PackedTypeLite(kind: t.kind, callConv: t.callConv, flags: t.flags,
                         n: storeNode(enc, ps, t.n),
                         sym: storeSym(enc, ps, t.sym))

  mapList(p.types, t.sons, s): storeType(enc, ps, s)

  ps[id] = p

func storeSym(enc: var TypeInfoEncoder, ps: var PackedEnv, s: PSym): SymId =
  if s == nil:
    return NilSymId

  let next = ps.nimSyms.len.SymId
  result = enc.syms.mgetOrPut(s.itemId, next)
  if result == next:
    ps.nimSyms.growBy(1)
    storeSymAt(enc, ps, s, next)

func storeType(enc: var TypeInfoEncoder, ps: var PackedEnv, t: PType): TypeId =
  if t == nil:
    return NilTypeId

  let next = ps.nimTypes.len.TypeId

  result = enc.types.mgetOrPut(t.itemId, next)
  if result == next:
    ps.nimTypes.growBy(1)
    storeTypeAt(enc, ps, t, next)


proc loadSym(dec: var TypeInfoDecoder, ps: PackedEnv, id: SymId): PSym
proc loadType(dec: var TypeInfoDecoder, ps: PackedEnv, id: TypeId): PType

proc loadNode(dec: var TypeInfoDecoder, ps: PackedEnv, id: NodeId): (PNode, int32) =
  if id.int32 == -1:
    return (nil, 0'i32)

  let n = ps.nimNodes[id.int]
  var r = PNode(kind: n.kind, flags: n.flags,
                typ: loadType(dec, ps, n.typeId))

  case n.kind
  of nkEmpty, nkType, nkNilLit, nkCommentStmt:
    discard "do nothing"
  of nkCharLit..nkUInt64Lit:
    r.intVal = ps.numbers[n.operand.LitId]
  of nkFloatLit..nkFloat64Lit:
    # use a `cast` to preserve the bit representation:
    r.floatVal = cast[BiggestFloat](ps.numbers[n.operand.LitId])
  of nkStrLit..nkTripleStrLit:
    r.strVal = ps.strings[n.operand.LitId]
  of nkSym:
    r.sym = dec.loadSym(ps, n.operand.SymId)
  of nkIdent:
    r.ident = PIdent(s: ps.strings[n.operand.LitId])
  of nkWithSons:
    r.sons.newSeq(n.operand)
    var nextId = id.int32 + 1
    for i in 0..<n.operand:
      let (node, skip) = loadNode(dec, ps, nextId.NodeId)
      r.sons[i] = node
      nextId += skip

    return (r, nextId - id.int32)
  of nkNone, nkError:
    # should have not been stored in the first place
    unreachable()

  result = (r, 1'i32)

proc loadSym(dec: var TypeInfoDecoder, ps: PackedEnv, id: SymId): PSym =
  if id == NilSymId:
    return nil

  if dec[id] != nil:
    result = dec[id]
  else:
    let p = ps[id]
    result = PSym(kind: p.kind, name: PIdent(s: ps.strings[p.name]),
                  flags: p.flags, magic: p.magic,
                  position: p.position, offset: p.offset)
    # It's important to put the symbol into the lookup table _before_ calling
    # `loadType` and `loadNode`, in order to prevent infinite recursion
    dec[id] = result
    result.typ = loadType(dec, ps, p.typ)
    result.ast = loadNode(dec, ps, p.ast)[0]

proc loadType(dec: var TypeInfoDecoder, ps: PackedEnv, id: TypeId): PType =
  if id == NilTypeId:
    return nil

  if dec[id] != nil:
    result = dec[id]
  else:
    let p = ps[id]
    result = PType(kind: p.kind, callConv: p.callConv, flags: p.flags)
    dec[id] = result

    mapList(result.sons, p.types, it): loadType(dec, ps, it)

    result.sym = loadSym(dec, ps, p.sym)
    result.n = loadNode(dec, ps, p.n)[0]

# -------- RTTI store/load -----------------------------------------------

func storeTypeInfos(penc: var PackedEncoder, p: var PackedEnv, list: openArray[VmTypeInfo]) =
  let enc = addr penc.typeInfoEnc

  mapList(p.typeInfos, list, x):
    (nt: storeType(enc[], p, x.nimType),
     t:  penc.typeMap[x.internal])

  # Process remaining types and symbols until none is left.
  # note: each call to `storeXAt` might add new pending symbols or types
  while true:
    if enc.pendingTypes.len > 0:
      let (t, id) = enc.pendingTypes.pop()
      storeTypeAt(enc[], p, t, id)
    elif enc.pendingSyms.len > 0:
      let (s, id) = enc.pendingSyms.pop()
      storeSymAt(enc[], p, s, id)
    else:
      break

proc loadTypeInfos*(p: PackedEnv, types: seq[PVmType]): seq[VmTypeInfo] =
  var dec: TypeInfoDecoder
  dec.types.newSeq(p.nimTypes.len)
  dec.syms.newSeq(p.nimSyms.len)

  mapList(result, p.typeInfos, x):
    VmTypeInfo(internal: types[x.t],
               nimType:  dec.loadType(p, x.nt))

proc loadEnv*(dst: var TCtx, src: PackedEnv) =
  ## Loads all data from `src` into `dst` for which no further/extra
  ## processing is required. Things that are not loaded are: complex
  ## constants, globals, `code`, and the callback list.

  # further loading requires a filled `types` list, so `loadVmTypes` has to
  # happen first:
  types(dst) = loadVmTypes(src)

  mapList(dst.constants, src.consts, x):
    var co = VmConstant(kind: x[0])
    let id = x[1]

    case x[0]
    of cnstInt:     co.intVal = src.numbers[id.LitId]
    of cnstFloat:   co.floatVal = cast[BiggestFloat](src.numbers[id.LitId])

    of cnstSliceListInt:   co.intSlices = loadSliceList[BiggestInt](src, id)
    of cnstSliceListFloat: co.floatSlices = loadSliceList[BiggestFloat](src, id)
    of cnstNode: unreachable()

    co

  mapList(dst.functions, src.functions, x):
    let
      sym = src.dbgSyms[x.sym]
      nimSym = PSym(name: PIdent(s: src.strings[sym.name]),
                    info: src.infos[sym.info])
    # In standalone mode, `sym` is just used to provide line information and
    # stack trace entries
    var f = FuncTableEntry(sym: nimSym, sig: x.sig,
                           retValDesc: dst.types[x.t1],
                           isClosure: x.isClosure,
                           kind: x.kind)
    case x.kind
    of ckDefault:
      f.start = x.a.int
      f.regCount = x.b.uint16
    of ckCallback:
      f.cbOffset = x.a.int

    f

  mapList(dst.debug, src.debug, x): src.infos[x.LitId]

  dst.rtti = loadTypeInfos(src, dst.types)

  dst.config.m.fileInfos.newSeq(src.files.len)
  for i, f in src.files.pairs:
    dst.config.m.fileInfos[i].fullPath = f.AbsoluteFile

func init*(enc: var PackedEncoder, types: seq[PVmType]) =
  ## Initializes the encoder. The sequence pass to `types` should not change
  ## past this point. Else, encoding error may happen

  # set up the `PVmType` -> `VmTypeId` mappings
  # XXX: this will become unnecessary once `PVmType` is replaced by `VmTypeId`
  for i in 0..<types.len:
    let t = types[i]
    enc.typeMap[t] = i.uint32

  assert enc.typeMap.len == types.len

func storeEnv*(enc: var PackedEncoder, dst: var PackedEnv, c: TCtx) =
  ## Stores all relevant data provided by `c` into `dst`. Previously stored
  ## data (except `nodes`, `numbers`, and `strings`) is thrown away. The only
  ## parts of `dst` not touched by `storeEnv` are: `cconsts`, `globals`, and
  ## `entryPoint`. These have to be filled in separately.
  # store all types:
  dst.types.newSeq(c.types.len - 1) # -1 since the invalid type is not included
  for i in 1..<c.types.len:
    let t = c.types[i]
    dst.types[i - 1] = storeVmType(enc, dst, t)

  mapList(dst.consts, c.constants, c):
    let item =
      case c.kind
      of cnstInt:     dst.getLitId(c.intVal).uint32
      of cnstFloat:   dst.getLitId(c.floatVal).uint32

      of cnstSliceListInt:   dst.storeSliceList(c.intSlices)
      of cnstSliceListFloat: dst.storeSliceList(c.floatSlices)
      of cnstNode:
        # node constants are not created in a non-compiletime context
        unreachable()

    (c.kind, item)

  mapList(dst.functions, c.functions, x):
    let
      d = dst.storeDbgSym(x.sym)
      t1 = enc.typeMap[x.retValDesc]

    let (a, b) =
      case x.kind
      of ckCallback: (x.cbOffset.uint32, 0'u32)
      of ckDefault:  (x.start.uint32, x.regCount.uint32)

    (d, x.sig, t1, x.isClosure, x.kind, a, b)

  dst.code = c.code

  mapList(dst.debug, c.debug, d):
    dst.infos.getOrIncl(d).uint32

  dst.ehTable = c.ehTable
  dst.ehCode = c.ehCode

  mapList(dst.files, c.config.m.fileInfos, fi):
    fi.fullPath.string

  storeTypeInfos(enc, dst, c.rtti)

proc writeToFile*(p: PackedEnv, file: AbsoluteFile): RodFileError =
  var f = rodfiles.create(file.string)
  f.storeHeader()

  # All changes here need to be reflected in `readFromFile`

  # The order in which the sections are stored doesn't match the data's
  # dependencies, but the rodfile format writer/parser dictates it this way.
  # Sorting the data into different sections is only done for stylistic
  # reasons, it has no semantic meaning.

  f.storeSection stringsSection
  f.store p.strings

  f.storeSection numbersSection
  f.store p.numbers

  f.storeSection topLevelSection
  f.storeSeq p.nodes
  f.storeSeq p.consts
  f.storeSeq p.cconsts
  f.storeSeq p.globals

  f.storeSection bodiesSection
  f.storePrim p.entryPoint
  f.storeSeq p.code
  f.storeSeq p.debug
  f.storeSeq p.ehTable
  f.storeSeq p.ehCode

  f.storeSection symsSection
  f.store p.infos
  f.storeSeq p.files
  f.storeSeq p.dbgSyms
  f.storeSeq p.functions
  f.storeSeq p.callbacks

  f.storeSection typesSection
  f.storeSeq p.tfields
  f.storeSeq p.tbranches
  f.storeSeq p.types

  f.storeSeq p.nimNodes
  f.storeSeq p.nimSyms
  f.storeSeq p.nimTypes
  f.storeSeq p.typeInfos

  f.close()

  result = f.err

proc readFromFile*(p: var PackedEnv, file: AbsoluteFile): RodFileError =
  var f = rodfiles.open(file.string)
  f.loadHeader()

  f.loadSection stringsSection
  f.load p.strings

  f.loadSection numbersSection
  f.load p.numbers

  f.loadSection topLevelSection
  f.loadSeq p.nodes
  f.loadSeq p.consts
  f.loadSeq p.cconsts
  f.loadSeq p.globals

  f.loadSection bodiesSection
  f.loadPrim p.entryPoint
  f.loadSeq p.code
  f.loadSeq p.debug
  f.loadSeq p.ehTable
  f.loadSeq p.ehCode

  f.loadSection symsSection
  f.load p.infos
  f.loadSeq p.files
  f.loadSeq p.dbgSyms
  f.loadSeq p.functions
  f.loadSeq p.callbacks

  f.loadSection typesSection
  f.loadSeq p.tfields
  f.loadSeq p.tbranches
  f.loadSeq p.types
  f.loadSeq p.nimNodes
  f.loadSeq p.nimSyms
  f.loadSeq p.nimTypes
  f.loadSeq p.typeInfos

  f.close()

  result = f.err
