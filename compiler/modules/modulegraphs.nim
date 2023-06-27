#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the module graph data structure. The module graph
## represents a complete Nim project. Single modules can either be kept in RAM
## or stored in a rod-file.

import
  std/[
    intsets,
    tables,
    hashes,
    md5
  ],
  compiler/front/[
    options,
    msgs,
  ],
  compiler/ast/[
    ast,
    astalgo,
    lineinfos,
    idents,
  ],
  compiler/utils/[
    pathutils,
    btrees,
    ropes,
  ],
  compiler/ic/[
    packed_ast,
    ic
  ]

# xxx: all this to output '.' on the command line... it's not even sem
from compiler/ast/reports_sem import SemReport
from compiler/ast/report_enums import ReportKind

type
  SigHash* = distinct MD5Digest

  LazySym* = object
    ## represents a symbol that maybe in a module that may be loaded or not
    ## yet fully defined. This is handy when we want to declare some symbols
    ## who's definitions refer to each other first and then process the
    ## definitions with lazy symbol resolution -- as in type sections.
    id*: FullId
    sym*: PSym

  Iface* = object
    ## data we don't want to store directly in the ast.PSym type for
    ## `s.kind == skModule`
    module*: PSym             ## module this "Iface" belongs to
    converters*: seq[LazySym]
    patterns*: seq[LazySym]   ## patterns for term rewriting macros -- ick
    pureEnums*: seq[LazySym]
    interf: TStrTable
    interfHidden: TStrTable   ## xxx: unexported or internal interface?
    uniqueName*: Rope

  Operators* = object
    opNot*, opContains*, opLe*, opLt*, opAnd*, opOr*, opIsNil*, opEq*: PSym
    opAdd*, opSub*, opMul*, opDiv*, opLen*: PSym

  FullId* = object
    module*: int
    packed*: PackedItemId

  LazyType* = object
    ## represents a type that maybe in a module that may be loaded or not
    ## yet fully defined. This is handy when we want to declare some symbols
    ## who's definitions refer to each other first and then process the
    ## definitions with lazy type resolution -- as in type sections.
    id*: FullId
    typ*: PType

  LazyInstantiation* = object
    module*: int
    sym*: FullId
    concreteTypes*: seq[FullId]
    inst*: PInstantiation

  ModuleGraph* {.acyclic.} = ref object
    ifaces*: seq[Iface]  ## indexed by int32 fileIdx
    packed*: PackedModuleGraph
    encoders*: seq[PackedEncoder]

    typeInstCache*: Table[ItemId, seq[LazyType]] # A symbol's ItemId.
    procInstCache*: Table[ItemId, seq[LazyInstantiation]] # A symbol's ItemId.
    attachedOps*: array[TTypeAttachedOp, Table[ItemId, LazySym]] # Type ID, destructors, etc.
    methodsPerType*: Table[ItemId, seq[(int, LazySym)]] # Type ID, attached methods
    enumToStringProcs*: Table[ItemId, LazySym]
    emittedTypeInfo*: Table[string, FileIndex]

    startupPackedConfig*: PackedConfig
    packageSyms*: TStrTable
    deps*: IntSet # the dependency graph or potentially its transitive closure.
    suggestMode*: bool # whether we are in nimsuggest mode or not.
    invalidTransitiveClosure: bool
    inclToMod*: Table[FileIndex, FileIndex] # mapping of include file to the
                                            # first module that included it
    importStack*: seq[FileIndex]  # The current import stack. Used for detecting recursive
                                  # module dependencies.
    backend*: RootRef # XXX: having this field is a hack, but it's still
                      #      required by the current ``passes`` design. Remove
                      #      once the ``passes`` design is phased out
    config*: ConfigRef
    cache*: IdentCache
    vm*: RootRef # unfortunately the 'vm' state is shared project-wise, this will
                 # be clarified in later compiler implementations.
    doStopCompile*: proc(): bool {.closure.}
    usageSym*: PSym # for nimsuggest
    owners*: seq[PSym]
    methods*: seq[tuple[methods: seq[PSym], dispatcher: PSym]] # needs serialization!
    systemModule*: PSym
    sysTypes*: array[TTypeKind, PType]
    compilerprocs*: TStrTable
    exposed*: TStrTable
    packageTypes*: TStrTable
    emptyNode*: PNode
    canonTypes*: Table[SigHash, PType]
    symBodyHashes*: Table[int, SigHash] # symId to digest mapping
    importModuleCallback*: proc (graph: ModuleGraph; m: PSym, fileIdx: FileIndex): PSym {.nimcall.}
    includeFileCallback*: proc (graph: ModuleGraph; m: PSym, fileIdx: FileIndex): PNode {.nimcall.}
    cacheSeqs*: Table[string, PNode] # state that is shared to support the 'macrocache' API; IC: implemented
    cacheCounters*: Table[string, BiggestInt] # IC: implemented
    cacheTables*: Table[string, BTree[string, PNode]] # IC: implemented
    passes*: seq[TPass]
    strongSemCheck*: proc (graph: ModuleGraph; owner: PSym; body: PNode) {.nimcall.}
    compatibleProps*: proc (graph: ModuleGraph; formal, actual: PType): bool {.nimcall.}
    idgen*: IdGenerator
    operators*: Operators
    when defined(nimsuggest):
      onMarkUsed*: SuggestCallback
        ## callback decouples regular compiler code `markUsed` from suggest
    onSymImport*: SuggestCallback
        ## callback decouples regular compiler code `importer` from suggest

  TPassContext* = object of RootObj # the pass's context
    idgen*: IdGenerator
  PPassContext* = ref TPassContext

  TPassOpen* = proc (graph: ModuleGraph; module: PSym; idgen: IdGenerator): PPassContext {.nimcall.}
  TPassClose* = proc (graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.}
  TPassProcess* = proc (p: PPassContext, topLevelStmt: PNode): PNode {.nimcall.}

  TPass* = tuple[open: TPassOpen,
                 process: TPassProcess,
                 close: TPassClose,
                 isFrontend: bool]
  
  SuggestCallback* = proc (graph: ModuleGraph, info: TLineInfo, s: PSym,
                          usageSym: var PSym, isDecl: bool)
    ## callback is used to decouple regular compiler code from suggest tool

proc resetForBackend*(g: ModuleGraph) =
  initStrTable(g.compilerprocs)
  g.typeInstCache.clear()
  g.procInstCache.clear()
  for a in mitems(g.attachedOps):
    a.clear()
  g.methodsPerType.clear()
  g.enumToStringProcs.clear()

const
  cb64 = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
    "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9a",
    "9b", "9c"]

proc toBase64a(s: cstring, len: int): string =
  ## encodes `s` into base64 representation.
  result = newStringOfCap(((len + 2) div 3) * 4)
  result.add "__"
  var i = 0
  while i < len - 2:
    let a = ord(s[i])
    let b = ord(s[i+1])
    let c = ord(s[i+2])
    result.add cb64[a shr 2]
    result.add cb64[((a and 3) shl 4) or ((b and 0xF0) shr 4)]
    result.add cb64[((b and 0x0F) shl 2) or ((c and 0xC0) shr 6)]
    result.add cb64[c and 0x3F]
    inc(i, 3)
  if i < len-1:
    let a = ord(s[i])
    let b = ord(s[i+1])
    result.add cb64[a shr 2]
    result.add cb64[((a and 3) shl 4) or ((b and 0xF0) shr 4)]
    result.add cb64[((b and 0x0F) shl 2)]
  elif i < len:
    let a = ord(s[i])
    result.add cb64[a shr 2]
    result.add cb64[(a and 3) shl 4]

template interfSelect(iface: Iface, importHidden: bool): TStrTable =
  var ret = iface.interf.addr # without intermediate ptr, it creates a copy and compiler becomes 15x slower!
  if importHidden: ret = iface.interfHidden.addr
  ret[]

template semtab(g: ModuleGraph, m: PSym): TStrTable =
  g.ifaces[m.position].interf

template semtabAll*(g: ModuleGraph, m: PSym): TStrTable =
  g.ifaces[m.position].interfHidden

proc initStrTables*(g: ModuleGraph, m: PSym) =
  initStrTable(semtab(g, m))
  initStrTable(semtabAll(g, m))

proc strTableAdds*(g: ModuleGraph, m: PSym, s: PSym) =
  strTableAdd(semtab(g, m), s)
  strTableAdd(semtabAll(g, m), s)

proc isCachedModule(g: ModuleGraph; module: int): bool {.inline.} =
  result = module < g.packed.len and g.packed[module].status == loaded

proc isCachedModule(g: ModuleGraph; m: PSym): bool {.inline.} =
  isCachedModule(g, m.position)

proc simulateCachedModule*(g: ModuleGraph; moduleSym: PSym; m: PackedModule) =
  when false:
    echo "simulating ", moduleSym.name.s, " ", moduleSym.position
  simulateLoadedModule(g.packed, g.config, g.cache, moduleSym, m)

proc initEncoder*(g: ModuleGraph; module: PSym) =
  let id = module.position
  if id >= g.encoders.len:
    setLen g.encoders, id+1
  ic.initEncoder(g.encoders[id],
    g.packed[id].fromDisk, module, g.config, g.startupPackedConfig)

type
  ModuleIter* = object
    fromRod: bool
    modIndex: int
    ti: TIdentIter
    rodIt: RodIter
    importHidden: bool

proc initModuleIter*(mi: var ModuleIter; g: ModuleGraph; m: PSym; name: PIdent): PSym =
  assert m.kind == skModule
  mi.modIndex = m.position
  mi.fromRod = isCachedModule(g, mi.modIndex)
  mi.importHidden = optImportHidden in m.options
  if mi.fromRod:
    result = initRodIter(mi.rodIt, g.config, g.cache, g.packed, FileIndex mi.modIndex, name, mi.importHidden)
  else:
    result = initIdentIter(mi.ti, g.ifaces[mi.modIndex].interfSelect(mi.importHidden), name)

proc nextModuleIter*(mi: var ModuleIter; g: ModuleGraph): PSym =
  if mi.fromRod:
    result = nextRodIter(mi.rodIt, g.packed)
  else:
    result = nextIdentIter(mi.ti, g.ifaces[mi.modIndex].interfSelect(mi.importHidden))

iterator allSyms*(g: ModuleGraph; m: PSym): PSym =
  let importHidden = optImportHidden in m.options
  if isCachedModule(g, m):
    var rodIt: RodIter
    var r = initRodIterAllSyms(rodIt, g.config, g.cache, g.packed, FileIndex m.position, importHidden)
    while r != nil:
      yield r
      r = nextRodIter(rodIt, g.packed)
  else:
    for s in g.ifaces[m.position].interfSelect(importHidden).data:
      if s != nil:
        yield s

proc someSym*(g: ModuleGraph; m: PSym; name: PIdent): PSym =
  let importHidden = optImportHidden in m.options
  if isCachedModule(g, m):
    result = interfaceSymbol(g.config, g.cache, g.packed, FileIndex(m.position), name, importHidden)
  else:
    result = strTableGet(g.ifaces[m.position].interfSelect(importHidden), name)

proc systemModuleSym*(g: ModuleGraph; name: PIdent): PSym =
  result = someSym(g, g.systemModule, name)

iterator systemModuleSyms*(g: ModuleGraph; name: PIdent): PSym =
  var mi: ModuleIter
  var r = initModuleIter(mi, g, g.systemModule, name)
  while r != nil:
    yield r
    r = nextModuleIter(mi, g)

proc resolveType(g: ModuleGraph; t: var LazyType): PType =
  result = t.typ
  if result == nil and isCachedModule(g, t.id.module):
    result = loadTypeFromId(g.config, g.cache, g.packed, t.id.module, t.id.packed)
    t.typ = result
  assert result != nil

proc resolveSym(g: ModuleGraph; t: var LazySym): PSym =
  result = t.sym
  if result == nil and isCachedModule(g, t.id.module):
    result = loadSymFromId(g.config, g.cache, g.packed, t.id.module, t.id.packed)
    t.sym = result
  assert result != nil

proc resolveInst(g: ModuleGraph; t: var LazyInstantiation): PInstantiation =
  result = t.inst
  if result == nil and isCachedModule(g, t.module):
    result = PInstantiation(sym: loadSymFromId(g.config, g.cache, g.packed, t.sym.module, t.sym.packed))
    result.concreteTypes = newSeq[PType](t.concreteTypes.len)
    for i in 0..high(result.concreteTypes):
      result.concreteTypes[i] = loadTypeFromId(g.config, g.cache, g.packed,
          t.concreteTypes[i].module, t.concreteTypes[i].packed)
    t.inst = result
  assert result != nil

iterator typeInstCacheItems*(g: ModuleGraph; s: PSym): PType =
  if g.typeInstCache.contains(s.itemId):
    let x = addr(g.typeInstCache[s.itemId])
    for t in mitems(x[]):
      yield resolveType(g, t)

iterator procInstCacheItems*(g: ModuleGraph; s: PSym): PInstantiation =
  if g.procInstCache.contains(s.itemId):
    let x = addr(g.procInstCache[s.itemId])
    for t in mitems(x[]):
      yield resolveInst(g, t)

proc resolveBackendSym(g: ModuleGraph, t: var LazySym): PSym =
  ## Returns the concrete symbol for `t`, loading it from the packed
  ## module-graph first if necessary. `t` is updated with the loaded symbol.
  ##
  ## Compared to ``resolveSym``, this procedure also support being called from
  ## the backend.
  result = t.sym
  if result == nil:
    # XXX: ``storing``, ``stored``, and ``outdated`` are also valid here, as
    #      the backend currently throws away and reloads all type-bound
    #      operator attachment information
    assert g.packed[t.id.module].status in {loaded, storing, stored, outdated}
    result = loadSymFromId(g.config, g.cache, g.packed,
                           t.id.module, t.id.packed)
    t.sym = result

  assert result != nil

proc getAttachedOp*(g: ModuleGraph; t: PType; op: TTypeAttachedOp): PSym =
  ## returns the requested attached operation for type `t`. Can return nil
  ## if no such operation exists.
  withValue(g.attachedOps[op], t.itemId, value):
    result = resolveBackendSym(g, value[])

proc setAttachedOp*(g: ModuleGraph; module: int; t: PType; op: TTypeAttachedOp; value: PSym) =
  g.attachedOps[op][t.itemId] = LazySym(sym: value)
  if g.config.symbolFiles != disabledSf:
    storeAttachedOp(g.encoders[module], g.packed[module].fromDisk, op, t, value)

proc setAttachedOpPartial*(g: ModuleGraph; module: int; t: PType; op: TTypeAttachedOp; value: PSym) =
  g.attachedOps[op][t.itemId] = LazySym(sym: value)
  # the effect is recored once the op is completed

proc completePartialOp*(g: ModuleGraph; module: int; t: PType; op: TTypeAttachedOp; value: PSym) =
  if g.config.symbolFiles != disabledSf:
    assert module < g.encoders.len
    assert isActive(g.encoders[module])
    toPackedGeneratedProcDef(value, g.encoders[module], g.packed[module].fromDisk)
    storeAttachedOp(g.encoders[module], g.packed[module].fromDisk, op, t, value)

proc getToStringProc*(g: ModuleGraph; t: PType): PSym =
  result = resolveSym(g, g.enumToStringProcs[t.itemId])
  assert result != nil

proc setToStringProc*(g: ModuleGraph; t: PType; value: PSym) =
  g.enumToStringProcs[t.itemId] = LazySym(sym: value)

iterator methodsForGeneric*(g: ModuleGraph; t: PType): (int, PSym) =
  if g.methodsPerType.contains(t.itemId):
    for it in mitems g.methodsPerType[t.itemId]:
      yield (it[0], resolveSym(g, it[1]))

proc addMethodToGeneric*(g: ModuleGraph; module: int; t: PType; col: int; m: PSym) =
  g.methodsPerType.mgetOrPut(t.itemId, @[]).add (col, LazySym(sym: m))

proc hasDisabledAsgn*(g: ModuleGraph; t: PType): bool =
  let op = getAttachedOp(g, t, attachedAsgn)
  result = op != nil and sfError in op.flags

proc copyTypeProps*(g: ModuleGraph; module: int; dest, src: PType) =
  for k in low(TTypeAttachedOp)..high(TTypeAttachedOp):
    let op = getAttachedOp(g, src, k)
    if op != nil:
      setAttachedOp(g, module, dest, k, op)

proc loadCompilerProc*(g: ModuleGraph; name: string): PSym =
  if g.config.symbolFiles == disabledSf: return nil

  # slow, linear search, but the results are cached:
  for module in 0..high(g.packed):
    #if isCachedModule(g, module):
    let x = searchForCompilerproc(g.packed[module], name)
    if x >= 0:
      result = loadSymFromId(g.config, g.cache, g.packed, module, toPackedItemId(x))
      if result != nil:
        strTableAdd(g.compilerprocs, result)
      return result

proc loadPackedSym*(g: ModuleGraph; s: var LazySym) =
  if s.sym == nil:
    s.sym = loadSymFromId(g.config, g.cache, g.packed, s.id.module, s.id.packed)

proc `$`*(u: SigHash): string =
  toBase64a(cast[cstring](unsafeAddr u), sizeof(u))

proc `==`*(a, b: SigHash): bool =
  result = equalMem(unsafeAddr a, unsafeAddr b, sizeof(a))

proc hash*(u: SigHash): Hash =
  result = 0
  for x in 0..3:
    result = (result shl 8) or u.MD5Digest[x].int

proc hash*(x: FileIndex): Hash {.borrow.}

proc stopCompile*(g: ModuleGraph): bool {.inline.} =
  result = g.doStopCompile != nil and g.doStopCompile()

proc createMagic*(g: ModuleGraph; idgen: IdGenerator; name: string, m: TMagic): PSym =
  result = newSym(skProc, getIdent(g.cache, name), nextSymId(idgen), nil, unknownLineInfo, {})
  result.magic = m
  result.flags = {sfNeverRaises}

proc createMagic(g: ModuleGraph; name: string, m: TMagic): PSym =
  result = createMagic(g, g.idgen, name, m)

proc registerModule*(g: ModuleGraph; m: PSym) =
  assert m != nil
  assert m.kind == skModule

  if m.position >= g.ifaces.len:
    setLen(g.ifaces, m.position + 1)

  if m.position >= g.packed.len:
    setLen(g.packed, m.position + 1)

  g.ifaces[m.position] = Iface(module: m, converters: @[], patterns: @[],
                               uniqueName: rope(uniqueModuleName(g.config, FileIndex(m.position))))

  initStrTables(g, m)

proc registerModuleById*(g: ModuleGraph; m: FileIndex) =
  registerModule(g, g.packed[int m].module)

proc initOperators(g: ModuleGraph): Operators =
  # These are safe for IC.
  result.opLe = createMagic(g, "<=", mLeI)
  result.opLt = createMagic(g, "<", mLtI)
  result.opAnd = createMagic(g, "and", mAnd)
  result.opOr = createMagic(g, "or", mOr)
  result.opIsNil = createMagic(g, "isnil", mIsNil)
  result.opEq = createMagic(g, "==", mEqI)
  result.opAdd = createMagic(g, "+", mAddI)
  result.opSub = createMagic(g, "-", mSubI)
  result.opMul = createMagic(g, "*", mMulI)
  result.opDiv = createMagic(g, "div", mDivI)
  result.opLen = createMagic(g, "len", mLengthSeq)
  result.opNot = createMagic(g, "not", mNot)
  result.opContains = createMagic(g, "contains", mInSet)

proc newModuleGraph*(cache: IdentCache; config: ConfigRef): ModuleGraph =
  result = ModuleGraph()
  # A module ID of -1 means that the symbol is not attached to a module at all,
  # but to the module graph:
  result.idgen = IdGenerator(module: -1'i32, symId: 0'i32, typeId: 0'i32)
  initStrTable(result.packageSyms)
  result.deps = initIntSet()
  result.ifaces = @[]
  result.importStack = @[]
  result.inclToMod = initTable[FileIndex, FileIndex]()
  result.config = config
  result.cache = cache
  result.owners = @[]
  result.methods = @[]
  initStrTable(result.compilerprocs)
  initStrTable(result.exposed)
  initStrTable(result.packageTypes)
  result.emptyNode = newNode(nkEmpty)
  result.cacheSeqs = initTable[string, PNode]()
  result.cacheCounters = initTable[string, BiggestInt]()
  result.cacheTables = initTable[string, BTree[string, PNode]]()
  result.canonTypes = initTable[SigHash, PType]()
  result.symBodyHashes = initTable[int, SigHash]()
  result.operators = initOperators(result)
  result.emittedTypeInfo = initTable[string, FileIndex]()

proc resetAllModules*(g: ModuleGraph) =
  initStrTable(g.packageSyms)
  g.deps = initIntSet()
  g.ifaces = @[]
  g.importStack = @[]
  g.inclToMod = initTable[FileIndex, FileIndex]()
  g.usageSym = nil
  g.owners = @[]
  g.methods = @[]
  initStrTable(g.compilerprocs)
  initStrTable(g.exposed)

proc getModule*(g: ModuleGraph; fileIdx: FileIndex): PSym =
  if fileIdx.int32 >= 0:
    if isCachedModule(g, fileIdx.int32):
      result = g.packed[fileIdx.int32].module
    elif fileIdx.int32 < g.ifaces.len:
      result = g.ifaces[fileIdx.int32].module

proc moduleOpenForCodegen*(g: ModuleGraph; m: FileIndex): bool {.inline.} =
  if g.config.symbolFiles == disabledSf:
    result = true
  else:
    result = g.packed[m.int32].status notin {undefined, stored, loaded}

proc rememberEmittedTypeInfo*(g: ModuleGraph; m: FileIndex; ti: string) =
  #assert(not isCachedModule(g, m.int32))
  if g.config.symbolFiles != disabledSf:
    #assert g.encoders[m.int32].isActive
    assert g.packed[m.int32].status != stored
    g.packed[m.int32].fromDisk.emittedTypeInfo.add ti
    #echo "added typeinfo ", m.int32, " ", ti, " suspicious ", not g.encoders[m.int32].isActive

proc rememberFlag*(g: ModuleGraph; m: PSym; flag: ModuleBackendFlag) =
  if g.config.symbolFiles != disabledSf:
    #assert g.encoders[m.int32].isActive
    assert g.packed[m.position].status != stored
    g.packed[m.position].fromDisk.backendFlags.incl flag

proc closeRodFile*(g: ModuleGraph; m: PSym) =
  if g.config.symbolFiles in {readOnlySf, v2Sf}:
    # For stress testing we seek to reload the symbols from memory. This
    # way much of the logic is tested but the test is reproducible as it does
    # not depend on the hard disk contents!
    let mint = m.position
    saveRodFile(toRodFile(g.config, AbsoluteFile toFullPath(g.config, FileIndex(mint))),
                g.encoders[mint], g.packed[mint].fromDisk)
    g.packed[mint].status = stored

  elif g.config.symbolFiles == stressTest:
    # debug code, but maybe a good idea for production? Could reduce the compiler's
    # memory consumption considerably at the cost of more loads from disk.
    let mint = m.position
    simulateCachedModule(g, m, g.packed[mint].fromDisk)
    g.packed[mint].status = loaded

proc dependsOn(a, b: int): int {.inline.} = (a shl 15) + b

proc addDep*(g: ModuleGraph; m: PSym, dep: FileIndex) =
  assert m.position == m.info.fileIndex.int32
  if g.suggestMode:
    g.deps.incl m.position.dependsOn(dep.int)
    # we compute the transitive closure later when querying the graph lazily.
    # this improves efficiency quite a lot:
    #invalidTransitiveClosure = true

proc addIncludeDep*(g: ModuleGraph; module, includeFile: FileIndex) =
  discard hasKeyOrPut(g.inclToMod, includeFile, module)

proc parentModule*(g: ModuleGraph; fileIdx: FileIndex): FileIndex =
  ## returns 'fileIdx' if the file belonging to this index is
  ## directly used as a module or else the module that first
  ## references this include file.
  if fileIdx.int32 >= 0 and fileIdx.int32 < g.ifaces.len and g.ifaces[fileIdx.int32].module != nil:
    result = fileIdx
  else:
    result = g.inclToMod.getOrDefault(fileIdx)

proc transitiveClosure(g: var IntSet; n: int) =
  # warshall's algorithm
  for k in 0..<n:
    for i in 0..<n:
      for j in 0..<n:
        if i != j and not g.contains(i.dependsOn(j)):
          if g.contains(i.dependsOn(k)) and g.contains(k.dependsOn(j)):
            g.incl i.dependsOn(j)

proc markDirty*(g: ModuleGraph; fileIdx: FileIndex) =
  let m = g.getModule fileIdx
  if m != nil: incl m.flags, sfDirty

proc markClientsDirty*(g: ModuleGraph; fileIdx: FileIndex) =
  # we need to mark its dependent modules D as dirty right away because after
  # nimsuggest is done with this module, the module's dirty flag will be
  # cleared but D still needs to be remembered as 'dirty'.
  if g.invalidTransitiveClosure:
    g.invalidTransitiveClosure = false
    transitiveClosure(g.deps, g.ifaces.len)

  # every module that *depends* on this file is also dirty:
  for i in 0i32..<g.ifaces.len.int32:
    let m = g.ifaces[i].module
    if m != nil and g.deps.contains(i.dependsOn(fileIdx.int)):
      incl m.flags, sfDirty

proc isDirty*(g: ModuleGraph; m: PSym): bool =
  result = g.suggestMode and sfDirty in m.flags

proc getBody*(g: ModuleGraph; s: PSym): PNode {.inline.} =
  if s.kind == skError:
    result = s.ast
    assert result != nil and result.kind == nkError,
      "assume we've populated the nkError here"
  else:
    result = s.ast[bodyPos]
    if result == nil and g.config.symbolFiles in {readOnlySf, v2Sf, stressTest}:
      result = loadProcBody(g.config, g.cache, g.packed, s)
      s.ast[bodyPos] = result
  assert result != nil

proc moduleFromRodFile*(g: ModuleGraph; fileIdx: FileIndex;
                        cachedModules: var seq[FileIndex]): PSym =
  ## Returns 'nil' if the module needs to be recompiled.
  if g.config.symbolFiles in {readOnlySf, v2Sf, stressTest}:
    result = ic.moduleFromRodFile(g.packed, g.config, g.cache, fileIdx, cachedModules)

proc configComplete*(g: ModuleGraph) =
  rememberStartupConfig(g.startupPackedConfig, g.config)

proc onProcessing*(graph: ModuleGraph, fileIdx: FileIndex, moduleStatus: string, fromModule: PSym) =
  let conf = graph.config
  let isNimscript = conf.isDefined("nimscript")
  if (not isNimscript) or rsemProcessing in conf.cmdlineNotes:
    conf.localReport SemReport(
      kind: rsemProcessing,
      sym: fromModule,
      processing: (
        isNimscript: isNimscript,
        importStackLen: graph.importStack.len,
        moduleStatus: moduleStatus,
        fileIdx: fileIdx))
