## This module implements the various parts needed for ``TNimType``-based
## RTTI support:
## * a lifting pass operating on the back-end code IR that collects and lifts
##   all accessed RTTI into globals
## * procedures for: collecting the transitive RTTI-object dependencies for
##   RTTI-objects; generating the setup code for the required run-time
##   structures
## * a procedure for optionally generating the type-name setup code
##
## ``TNimType``-based RTTI is used by the C-family targets as well as the JS
## target.

# TODO: module needs a better name

import
  std/[
    packedsets,
    tables
  ],
  compiler/ast/[
    ast_types,
    idents
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/vm/[
    bitsetutils,
    irpasses,
    pass_helpers,
    vmir
  ]

from compiler/ast/astalgo import lookupInRecord
from compiler/modules/magicsys import getCompilerProc
from compiler/vm/vmdef import unreachable

# XXX: ``typeToStr`` is only meant for debugging purposes - it should be used
#      to provide the name used for the run-time types
from compiler/vm/irdbg import typeToStr

type
  # TODO: rename
  TempCtx = object
    # immutable state:
    nodeArr: IRIndex
    nodePtrArr: IRIndex
    self: TypeId

    # inherited state:
    data: LiteralData # mutated

    # the fields below are mutated during ``genRttiRecordInit``
    numNodes: int    ## the number of elements in the ``TNimNode`` array
    numNodePtrs: int ## the number of elements in the ``ptr TNimNode`` array

  InitRttiCtx = object
    szField: int
    alField: int
    kindField: int
    flagsField: int
    baseField: int
    nodeField: int
    finalizerField: int
    markerField: int
    deepcopyField: int

    nkindField: int
    offsetField: int
    ntypField: int
    lenField: int
    sonsField: int

    nodeArr, nodePtrArr: SymId

    # TODO: rename to `pe` or `passEnv`
    graph: PassEnv

# these need to match the integer values of the ``hti.TNimNodeKind`` enum
const
  NimNodeKindSlot = 1
  NimNodeKindList = 2
  NimNodeKindCase = 3

# XXX: only exported for the fake-closure-type
proc liftRttiGlobal*(c: var LiftPassCtx, id: TypeId): SymId =
  result = c.typeInfoMarker.getOrDefault(id)
  if result == NoneSymbol:
    # TODO: use a stack-allocated array for formatting the string into
    let name = "NTI" & $(id.int) & "_" # XXX: too many short-lived and
                                       #      unnecessary allocations

    # TODO: cache the `TNimType` type
    let nimType = c.graph.getCompilerType("TNimType")
    # the symbol is owned by the module the type is owned by
    result = c.addGlobal(nimType, name)

    c.typeInfoMarker[id] = result

proc liftTypeInfoV1(c: var LiftPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Turns all ``mGetTypeInfo`` calls into globals and collects each mapping
  ## between a type and it's corresponding global to ``c.typeInfoMarker``
  # XXX: can this really be considered lifting?
  case n.kind
  of ntkCall:
    if getMagic(ir, c.env[], n) == mGetTypeInfo:
      cr.replace()

      let
        typ = ir.getLit(ir.at(ir.argAt(cr, 0))).typ

      assert typ != NoneType
      discard cr.insertSym(liftRttiGlobal(c, typ))

  else:
    discard

const typeV1Pass* = LinearPass2[LiftPassCtx](visit: liftTypeInfoV1)

func collectType(env: TypeEnv, id: TypeId, dest: var PackedSet[TypeId])

func collectDeps(env: TypeEnv, id: TypeId, dest: var PackedSet[TypeId]) =
  ## Recursively walks the type with the given `id` and collects all visited
  ## types into `dest`. The types part of a procedural type's signature are
  ## *not* included in both the traversal and collection
  let t = env[id]

  if t.base != NoneType:
    collectType(env, t.base, dest)

  case t.kind
  of tnkRecord:
    # iterate over all fields and collect the types
    for f in fields(env, t):
      collectType(env, f.typ, dest)

  else:
    # has no fields (or none we're interested in)
    discard

func collectType(env: TypeEnv, id: TypeId, dest: var PackedSet[TypeId]) =
  if dest.containsOrIncl(id):
    return

  collectDeps(env, id, dest)

proc init(c: var InitRttiCtx, g: ModuleGraph, p: PassEnv) =
  let
    ntType = g.getCompilerProc("TNimType").typ
    nnType = g.getCompilerProc("TNimNode").typ

  template posInRecord(t: PType, name: string): int =
    lookupInRecord(t.n, g.cache.getIdent(name)).position

  c.graph = p

  c.szField = posInRecord(ntType, "size")
  c.alField = posInRecord(ntType, "align")
  c.kindField = posInRecord(ntType, "kind")
  c.flagsField = posInRecord(ntType, "flags")
  c.baseField = posInRecord(ntType, "base")
  c.nodeField = posInRecord(ntType, "node")
  c.finalizerField = posInRecord(ntType, "finalizer")
  c.markerField = posInRecord(ntType, "marker")
  c.deepcopyField = posInRecord(ntType, "deepcopy")

  c.nkindField = posInRecord(nnType, "kind")
  c.offsetField = posInRecord(nnType, "offset")
  c.ntypField = posInRecord(nnType, "typ")
  c.lenField = posInRecord(nnType, "len")
  c.sonsField = posInRecord(nnType, "sons")


func getNimTypeKind(env: TypeEnv, tfInfo: TypeFieldInfo, id: TypeId): TTypeKind =
  ## Maps a back-end IR type to the ``TTypeKind`` used for ``TNimType``
  case env.kind(id)
  of tnkVoid: tyVoid
  of tnkBool: tyBool
  of tnkChar: tyChar
  of tnkInt:
    case env[id].size
    of 8: tyInt8
    of 16: tyInt16
    of 32: tyInt32
    of 64: tyInt64
    else: unreachable() # not representable
  of tnkUInt:
    case env[id].size
    of 8: tyUInt8
    of 16: tyUInt16
    of 32: tyUInt32
    of 64: tyUInt64
    else: unreachable()
  of tnkFloat:
    case env[id].size
    of 32: tyFloat32
    of 64: tyFloat64
    of 128: tyFloat128
    else: unreachable()
  of tnkPtr: tyPtr
  of tnkRef: tyRef
  of tnkVar: tyVar
  of tnkLent: tyLent
  of tnkProc: tyProc
  of tnkCString: tyCstring
  of tnkSeq: tySequence
  of tnkString: tyString
  of tnkOpenArray: tyOpenArray
  of tnkRecord:
    if tfsHeader in tfInfo[id.toIndex]:
      # only records that have a type field in the header are treated as
      # non-pure objects
      # XXX: this has the side-effect that ``genericAssign`` will not copy the
      #      contents of any base types if the base type for an object is
      #      marked as `.pure` (prevents the creation of a type-header)
      tyObject
    else:
      # if the record has no type-header, it's treated as a pure object
      tyTuple

  of tnkClosure:
    # closure objects are represented as pure objects (``tyTuple``)
    tyTuple
  of tnkArray: tyArray
  of tnkUncheckedArray: tyUncheckedArray
  else:
    unreachable(env.kind(id))

# XXX: why are sons for ``TNimNode`` stored as an array of pointers? In
#      seemingly all cases, each node is only ever referenced once. Storing
#      them as ``ptr array[..., TNimNode]`` would simplify the logic here (no
#      need for ``numNodePtrs`` and ``nodePtrArr``) and should also improve
#      run-time efficiency. **EDIT**: the reason are variant objects. The way
#      branches are represented more or less necessitates that an array of
#      pointers is used

# XXX: all nodes and node-pointers are each stored in a contiguous array.
#      This is a problem for hot-code-reloading, since it makes it impossible
#      to add or remove RTTI nodes dynamically (at least with the approach used
#      by the current HCR implementation)

func countRequiredNodes(iter: RecordIter, env: TypeEnv): int =
  ## Counts the number of ``TNimNode`` slots that are required to hold all
  ## *direct* children (not children of children) for the record node `iter`
  ## is pointing to
  assert env[iter.getId()].kind in {rnkBranch, rnkList}

  for id in children(iter, env):
    let n = env[id]
    case n.kind
    of rnkCase, rnkList:
      result += 1 # both record-node kinds are represented by a single
                  # ``TNimNode``
    of rnkFields:
      result += n.slice.len
    of rnkBranch, rnkEmpty:
      # neither ``rnkBranch`` nor ``rnkList`` can have ``rnkBranch`` as a
      # child
      unreachable(n.kind)


func genNode(c: var TempCtx, cr: var IrCursor): IRIndex =
  result = cr.insertPathArr(c.nodeArr, cr.insertLit(c.data, c.numNodes))
  inc c.numNodes

func insertRttiRef(cr: var IrCursor, map: Table[TypeId, SymId], id: TypeId): IRIndex =
  cr.insertAddr(cr.insertSym(map[id]))

func offsetExpr(cr: var IrCursor, data: var LiteralData, pe: PassEnv, id: TypeId, pos: int32): IRIndex =
  # TODO: the second parameter to the mOffsetOf magic should be an immediate
  #       instead of a literal
  cr.insertMagicCall(pe, mOffsetOf, tyInt, cr.insertTypeLit(id), cr.insertLit(data, pos))

func genRttiRecordInit(c: InitRttiCtx, cr: var IrCursor, iter: var RecordIter, map: Table[TypeId, SymId], tc: var TempCtx, env: TypeEnv): int =
  ## Recursively traverses the record and emits the initialization logic for
  ## the ``TNimNode``-tree representing the record at run-time

  template setField(acc: IRIndex, name: untyped, val: IRIndex) =
    ## Emits an assignment to the given field
    cr.insertAsgn(askInit, cr.insertPathObj(acc, c.name.int16), val)

  template insertNodeAcc(index: int): IRIndex =
    cr.insertPathArr(tc.nodeArr, cr.insertLit(tc.data, index))

  template insertNodePtrAcc(index: int): IRIndex =
    cr.insertPathArr(tc.nodePtrArr, cr.insertLit(tc.data, index))

  let
    nId = nextId(env, iter)
    n = env[nId]

  case n.kind
  of rnkFields:
    # generate a ``TNimNode`` for each field
    for f in n.slice.items:
      let node = genNode(tc, cr)
      setField(node, nkindField, cr.insertLit(tc.data, NimNodeKindSlot))
      setField(node, offsetField, cr.offsetExpr(tc.data, c.graph, tc.self, iter.fieldPos(f)))
      setField(node, ntypField, cr.insertRttiRef(map, env[iter.field(f)].typ))

    result = n.slice.len

  of rnkList:
    let len = countRequiredNodes(iter, env)

    if len == 1:
      # flatten single-child list nodes.
      # note: it's important to look at the number of *physical* nodes
      #       (i.e. ``TNimNode``), not at the number of *logical* ones
      #       (i.e. ``RecordNode``)
      discard genRttiRecordInit(c, cr, iter, map, tc, env)

    elif len > 0:
      # reserve the required number of node-pointer slots:
      let start = tc.numNodePtrs
      tc.numNodePtrs += len

      var pos = start
      for _ in 0..<n.len:
        let num = genRttiRecordInit(c, cr, iter, map, tc, env)

        # initialize the corresponding node-pointer slots:
        for i in (tc.numNodes-num)..<tc.numNodes:
          cr.insertAsgn(askInit,
                        insertNodePtrAcc(pos),
                        cr.insertAddr(insertNodeAcc(i)))
          inc pos

      assert pos - start == len

      # fill in the relevant fields
      let p = genNode(tc, cr)
      setField(p, nkindField, cr.insertLit(tc.data, NimNodeKindList))
      setField(p, lenField, cr.insertLit(tc.data, len))
      setField(p, sonsField, cr.insertAddr(insertNodePtrAcc(start)))

    else:
      # generate an empty ``nkRecList`` node:
      let p = genNode(tc, cr)
      setField(p, nkindField, cr.insertLit(tc.data, NimNodeKindList))

    result = 1

  of rnkCase:
    let dn = next(env, iter)

    # the RTTI (i.e. ``TNimNode``) for a record case is stored as follows:
    # - an ``nkCase`` has N+1 sons, where N is the number of possible values
    #   for the discriminator, e.g. for ``case discr: range[0..9]``, N would
    #   be 10
    # - the ``ptr TNimNode`` for each *branch* that is not an 'else' is
    #   assigned to every ``sons[x]`` where ``x`` is a value that the branch
    #   covers
    # - the ``ptr TNimNode`` for the 'else' branch (if one exists) is assigned
    #   to ``sons[N]``
    # - to get the currently active branch, ``ord(discr)`` is used as an index
    #   into ``sons``
    #
    # Note that this design forces ``ord(low(discr))`` to always be 0, since
    # ``ord(discr)`` is directly used to index into the 0-based ``sons`` array
    let
      start = tc.numNodePtrs
      L = env.discrLength(nId)

    # reserve a pointer slot for each possible value
    tc.numNodePtrs += L + 1 # +1 for the 'else' slot

    # first, create the nodes for the discriminator value table
    for _ in 1..<n.len:
      let bId = peekId(env, iter) # a ``rnkBranch`` record-node

      discard genRttiRecordInit(c, cr, iter, map, tc, env)

      let node = cr.insertAddr insertNodeAcc(tc.numNodes-1)
      # the logic for ``rnkBranch`` makes sure that we always get a single
      # node

      if env.rawBranch(bId) == NoneLit:
        # an else branch -> assign to the highest slot
        cr.insertAsgn(askInit, insertNodePtrAcc(start + L), node)
      else:
        # assign the node's ptr to the slots corresponding to each value the
        # branch covers
        for v in branchValues(tc.data, env.rawBranch(bId)):
          cr.insertAsgn(askInit, insertNodePtrAcc(start + v), node)

    let p = genNode(tc, cr)
    setField(p, nkindField, cr.insertLit(tc.data, NimNodeKindCase))

    # the RTTI node for the record-case uses the offset and type of the
    # discriminator field:
    setField(p, offsetField, cr.offsetExpr(tc.data, c.graph, tc.self, iter.fieldPos(dn.slice.a)))
    setField(p, ntypField, cr.insertRttiRef(map, env[iter.field(dn.slice.a)].typ))

    setField(p, lenField, cr.insertLit(tc.data, L))
    setField(p, sonsField, cr.insertAddr insertNodePtrAcc(start))
    result = 1

  of rnkBranch:
    let L = genRttiRecordInit(c, cr, iter, map, tc, env)
    case L
    of 0:
      # a branch is expected to always provide a node, so we create an
      # empty one
      let p = genNode(tc, cr)
      setField(p, nkindField, cr.insertLit(tc.data, NimNodeKindList))
      setField(p, lenField, cr.insertLit(tc.data, 0))

    of 1: discard
    else: unreachable()

    result = 1

  of rnkEmpty:
    unreachable()

func genRttiInit(c: InitRttiCtx, cr: var IrCursor, data: var LiteralData,
                 env: TypeEnv, gcLookup: BitSet[TypeId], tfInfo: TypeFieldInfo,
                 map: Table[TypeId, SymId], markers: Table[TypeId, ProcId],
                 nodes: Table[TypeId, int], sym: SymId, id: TypeId) =
  ## Generates the code for initializing the RTTI global corresponding to the
  ## type with the given `id`
  let
    base = env.base(id)
    kind = env.kind(id)
    s = cr.insertSym(sym)

  template setField(f: int, val: IRIndex) =
    cr.insertAsgn(askCopy, cr.insertPathObj(s, f.int16), val)

  setField(c.kindField, cr.insertLit(data, ord(getNimTypeKind(env, tfInfo, id))))

  if base != NoneType:
    setField(c.baseField, cr.insertAddr cr.insertSym(map[base]))

  let (sizeExpr, alignSrcType) =
    case kind
    of tnkUncheckedArray:
      # treat it as having a size of '0' and an alignment equal to that of
      # it's element type. This matches the behaviour of
      # ``sizealignoffsetimpl.computeSizeAlign``
      (cr.insertLit(data, 0), base)
    else:
      (cr.insertCallExpr(mSizeOf, c.graph.sysTypes[tyInt],
                         cr.insertTypeLit(id)), id)

  setField(c.szField, sizeExpr)
  setField(c.alField, cr.insertCallExpr(mAlignOf, c.graph.sysTypes[tyInt],
                                        cr.insertTypeLit(alignSrcType)))

  if id notin gcLookup:
    # set the ``ntfNoRefs`` flag. There are also the ``ntfAcyclic`` and
    # ``ntfEnumHole`` flags, but:
    # - ``ntfAcyclic`` is ignored by the garbage collectors using the RTTI
    # - enums were lowered to integer types and don't exist here anymore
    setField(c.flagsField, cr.insertLit(data, 1)) # ``ntfNoRefs`` == 1

  if (let prc = c.graph.attachedOps[attachedDeepCopy].getOrDefault(id); prc != NoneProc):
    # the ``=deepCopy`` attached op has a different but compatible signature
    # compared to the ``TNimType.deepcopy`` field - we need to convert first
    setField(c.deepcopyField,
             cr.insertConv(env[env.nthField(id, c.deepcopyField)].typ,
                           cr.insertProcSym(prc)))

  if kind == tnkRef:
    # if the base type has a destructor, use that as the finalizer for the ref
    # XXX: a finalizer only needs to be set (and included in code-generation)
    #      if a `ref` is actually created (via ``new``). This information
    #      could be collected during the hook lifting/insertion pass since it's
    #      somewhat related
    if (let op = c.graph.attachedOps[attachedDestructor].getOrDefault(base); op != NoneProc):
      # XXX: ``cgen`` verifies that the calling convention is ``.nimcall`` and
      #      reports an error otherwise. We can't easily report an error here,
      #      but the verification should happen during sem anyways.
      setField(c.finalizerField,
               cr.insertCast(c.graph.sysTypes[tyPointer], cr.insertProcSym(op)))

  if (let prc = markers.getOrDefault(id); prc != NoneProc):
    setField(c.markerField, cr.insertProcSym(prc))

  if kind == tnkRecord:
    setField(c.nodeField, cr.insertAddr(cr.insertPathArr(cr.insertSym(c.nodeArr),
             cr.insertLit(data, nodes[id]))))


func genNodes(c: InitRttiCtx, map: Table[TypeId, SymId], types: var TypeEnv,
              data: var LiteralData, syms: var SymbolEnv, cr: var IrCursor
             ): Table[TypeId, int] =
  ## Generates the code for setting up the ``TNimNode``-trees for all record
  ## types in `map`. Also sets up the types of the auxiliary globals used as
  ## the backing storage of the trees.
  ## Returns a ``Table`` mapping record types to an index into the flat tree
  let
    nnType = c.graph.getCompilerType("TNimNode")

  var tc: TempCtx
  tc.nodeArr = cr.insertSym(c.nodeArr)
  tc.nodePtrArr = cr.insertSym(c.nodePtrArr)

  swap(tc.data, data)

  template isImported(id: TypeId): bool =
    (let iface = types.iface(id); iface != nil and sfImportc in iface.flags)

  for id in map.keys:
    case types.kind(id)
    of tnkRecord:
      # only record-types have a valid `node` RTTI field
      if not isImported(id):
        # a non-imported type
        tc.self = id

        var iter = initRecordIter(types, id)
        let num = genRttiRecordInit(c, cr, iter, map, tc, types)
        assert num == 1
      else:
        # leave the RTTI node empty for imported record types
        inc tc.numNodes

      result[id] = tc.numNodes-1

    else:
      discard

  swap(tc.data, data)

  # TODO: try to move initializing the types of the globals out of this
  #       procedure. That would remove the need for a mutable symbol and type
  #       environment
  # now we know the length of both arrays
  syms.setType(c.nodeArr,    types.requestArrayType(tc.numNodes.uint, nnType))
  syms.setType(c.nodePtrArr, types.requestArrayType(tc.numNodePtrs.uint, types.requestGenericType(tnkPtr, nnType)))

proc genFakeClosureType*(types: var TypeEnv, pe: PassEnv): TypeId =
  ## Generates a type equivalent to ``tuple[pointer, ref tuple[]]`, adds it
  ## to the environment, and returns it's ID
  let
    tup    = types.requestRecordType(base=NoneType)
    refTyp = types.requestGenericType(tnkRef, tup)

  result = types.requestRecordType(base=NoneType):
    [(NoneDecl, pe.sysTypes[tyPointer]), (NoneDecl, refTyp)]

proc generateTypeNames*(typeInfos: Table[TypeId, SymId], env: TypeEnv,
                        data: var LiteralData, pe: PassEnv, g: ModuleGraph,
                        typeListRoot: SymId, cr: var IrCursor) =
  ## Generates the code for both setting the type name of each RTTI object and
  ## for setting up the global linked-list storing all RTTI objects.
  ## `typeListRoot` is the symbol of the global to use as the list's head pointer.
  assert typeListRoot != NoneSymbol
  let
    ntType = g.getCompilerProc("TNimType").typ
    nameField = lookupInRecord(ntType.n, g.cache.getIdent("name")).position.int16
    nextField = lookupInRecord(ntType.n, g.cache.getIdent("nextType")).position.int16

    rootAcc = cr.insertSym(typeListRoot)

  var prev = rootAcc
  for t, s in typeInfos.pairs:
    let rtti = cr.insertSym(s)
    # TODO: explcitly set the type of the literal to be a cstring
    cr.insertAsgn(askInit, cr.insertPathObj(rtti, nameField),
                  cr.insertLit(data, typeToStr(env, t), pe.sysTypes[tyCstring]))

    # `typeListRoot` is the head of a linked-list. We add the type-infos to the
    # linked-list by prepending them - that is, we set their ``nextType`` field
    # to the current head and change the head to point to them:
    #
    # .. code-block:: nim
    #
    #   rttiObj1.next = head
    #   rttiObj2.next = rttiObj1
    #   ...
    #   head = rttiObjX
    cr.insertAsgn(askCopy, cr.insertPathObj(rtti, nextField), prev)
    prev = cr.insertAddr(rtti)

  cr.insertAsgn(askCopy, rootAcc, prev)

func generateDependencies*(typeInfos: var Table[TypeId, SymId],
                           syms: var SymbolEnv, ic: var IdentCache,
                           pe: PassEnv, types: TypeEnv) =
  ## Generates RTTI globals for all types referenced by the types in
  ## `typeInfos`. The created symbols together with their associated type
  ## are added to `typeInfos`

  # `typeInfos` contains all "roots". Now we need to collect the types
  # referenced by them and create RTTI globals for those

  var marker: PackedSet[TypeId]
  # mark the roots as already visited
  for id in typeInfos.keys:
    marker.incl id

  # deep-scan each root for dependencies
  for id in typeInfos.keys:
    collectDeps(types, id, marker)

  # create a global for each collected type
  let ntType = pe.getCompilerType("TNimType")
  for id in marker.items:
    # `marker` also contains the roots, so we have to guard against
    # creating another global for them
    if id notin typeInfos:
      typeInfos[id] = syms.addSym(skVar, ntType, ic.getIdent("NTI_" & $id.uint32))

# TODO: might need a better name
proc generateRttiInit*(g: ModuleGraph, pe: PassEnv, gcLookup: BitSet[TypeId],
                       tfInfo: TypeFieldInfo, fakeClosure: TypeId,
                       typeInfos: Table[TypeId, SymId],
                       markers: Table[TypeId, ProcId], data: var LiteralData,
                       syms: var SymbolEnv, ic: var IdentCache,
                       types: var TypeEnv, cr: var IrCursor): (SymId, SymId) =
  ## Generates the code for setting up all RTTI globals listed by `typeInfos`.
  ## Returns the IDs of two created globals that store auxiliary data required
  ## by the RTTI.
  ##
  ## `fakeClosure` is the type that provides the layout of closure types in
  ## the context of RTTI.
  ##
  ## `markers` provides the names of the marker procedures - the table may
  ## be empty.
  var c: InitRttiCtx
  c.init(g, pe)

  # we only know the type of both arrays *after* all nodes were generated
  c.nodeArr = syms.addSym(skLet, NoneType, ic.getIdent("nimNodes"), {sfGlobal})
  c.nodePtrArr = syms.addSym(skLet, NoneType, ic.getIdent("nimNodePtrs"), {sfGlobal})

  let nodes = genNodes(c, typeInfos, types, data, syms, cr)

  for id in typeInfos.keys:
    let
      sym = typeInfos[id]
      id =
        if types.kind(id) != tnkClosure:
          id
        else:
          fakeClosure

    genRttiInit(c, cr, data, types, gcLookup, tfInfo, typeInfos, markers,
                nodes, sym, id)

  result = (c.nodeArr, c.nodePtrArr)