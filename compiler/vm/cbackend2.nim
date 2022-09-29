import
  std/[
    intsets,
    tables
  ],
  compiler/ast/[
    ast,
    ast_types,
    astalgo, # for `getModule`,
    idents,
    lineinfos,
    reports
  ],
  compiler/backend/[
    extccomp
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/sem/[
    passes,
    transf
  ],
  compiler/utils/[
    pathutils
  ],
  compiler/vm/[
    cpasses,
    irgen,
    irtypes,
    vmir,
    cgen2,
    irpasses,
    irdbg,
    typeinfogen,
    typeprocessing
  ],
  experimental/[
    results
  ]

from compiler/vm/vmdef import unreachable

import std/options as stdoptions

type
  CodeFragment = object
    ## The state required for generating code in multiple steps.
    ## `CodeFragment` helps when generating code for multiple procedures in
    ## an interleaved manner.
    prc: PProc
    irs: IrStore3

  Module = object
    stmts: seq[PNode] ## top level statements in the order they were parsed
    sym: PSym ## module symbol

  ModuleDataExtra = object
    ## Extra data associated with a module
    # XXX: since this is data that's somewhat relevant to all targets, it
    #      might be a good idea to merge ``ModuleDataExtra`` with
    #      ``ModuleData``. The way in which the data for modules is organized
    #      needs an overhaul in general.
    fileIdx: FileIndex
    flags: TSymFlags

    initProc: ProcId ## the 'init' procedure for the module. May be unset
    dataInitProc: ProcId ##
      ## the procedure responsible for:
      ## * intializing RTTI related to the module
      ## * running dynamic-library loading logic

    # XXX: ``dataInitProc`` is not relevant to the VM target. Maybe the field
    #      should be split off?

  # TODO: turn into a ``distinct uint32``
  ModuleId = int ## The ID of a module in the back-end

  ModuleListRef = ref ModuleList
  ModuleList = object of RootObj
    modules: seq[Module] ## ``ModuleId`` -> data collected via the ``passes``
                         ## interface

    # XXX: ``modulesClosed`` and ``moduleMap`` should be split off into a
    #      separate object. They're relevant until the end of ``generateCode``,
    #      while ``modules`` gets processed at the start and is then discarded
    modulesClosed: seq[ModuleId] ## the modules in the order they were closed.
                                 ## The first closed module comes first, then
                                 ## the next, etc.
    moduleMap: Table[int32, ModuleId] ## maps the module IDs used by semantic
                                      ## analysis to the ones used in the
                                      ## back-end
    # TODO: use a ``seq`` instead of a ``Table``

  ModuleRef = ref object of TPassContext
    ## The pass context for the VM backend. Represents a reference to a
    ## module in the module list
    list: ModuleListRef
    index: int

func growBy[T](x: var seq[T], n: Natural) {.inline.} =
  x.setLen(x.len + n)

iterator cpairs[T](s: seq[T]): (int, lent T) =
  ## Continous pair iterator. Supports `s` growing during iteration
  var i = 0
  while i < s.len:
    yield (i, s[i])
    inc i

func collect(list: var seq[PSym], s: sink PSym, marker: var IntSet) {.inline.} =
  ## If `s.id` is not present in `marker`, adds `s` to `list` and remember it
  ## in `markers`
  if not marker.containsOrIncl(s.id):
    list.add s

func collectRoutineSyms(ast: PNode, syms: var seq[PSym], marker: var IntSet) =
  ## Traverses the `ast` and collects all referenced symbols of routine kind
  ## to `syms` and `marker`
  if ast.kind == nkSym:
    let s = ast.sym
    if s.kind in routineKinds:
      collect(syms, s, marker)

    return

  for i in 0..<ast.safeLen:
    collectRoutineSyms(ast[i], syms, marker)

func earlyTransformConst(ast: PNode, procs: var ProcedureEnv): PNode =
  ## Traverses the `ast` and replaces all routine symbol nodes with the
  ## representation further processing expects. Returns the transformed node
  ## (or `ast` if no transformation happened)
  result = ast
  if ast.kind == nkSym:
    let s = ast.sym
    if s.kind in routineKinds:
      # encode references to procedures as a ``nkProcTy`` with an
      # ``nkIntLit`` child holding the ID
      result = PNode(kind: nkProcTy)
      # XXX: ``ProcedureEnv`` doesn't allowe for mutable lookup (it should!)
      #      so we - we have to use ``requestProc``
      result.sons.add:
        PNode(kind: nkIntLit, intVal: procs.requestProc(s).toIndex.BiggestInt)
      return

  for i in 0..<ast.safeLen:
    ast[i] = earlyTransformConst(ast[i], procs)

proc generateTopLevelStmts*(module: Module, c: var TCtx,
                            config: ConfigRef): Option[IrStore3] =
  ## Generates code for all collected top-level statements of `module` and
  ## compiles the fragments into a single procedure. If the resulting
  ## procedure is empty, a 'none' is returned.
  let n = newNodeI(nkEmpty, module.sym.info) # for line information

  c.prc = PProc(sym: module.sym)
  c.irs.reset()

  c.startProc()

  let ast =
    if module.stmts.len > 1: newTree(nkStmtList, module.stmts)
    elif module.stmts.len == 1: module.stmts[0]
    else: newNode(nkEmpty)

  let tn = transformStmt(c.graph, c.idgen, c.module, ast)
  let r = c.genStmt(tn)

  if unlikely(r.isErr):
    config.localReport(r.takeErr)

  c.endProc()

  # if the resulting procedure is empty, `irs` still has two items: the
  # 'join's at the end
  # TODO: use a more forward-compatible approach for testing if there's no
  #       code
  result =
    if c.irs.len > 2: some(move c.irs)
    else:             none(IrStore3)

proc generateCodeForProc(c: var TCtx, s: PSym): IrGenResult =
  assert s != nil
  #debugEcho s.name.s, "(", s.kind, "): ", c.config.toFileLineCol(s.info)
  let body = transformBody(c.graph, c.idgen, s, cache = false)
  c.irs.reset()
  result = genProc(c, s, body)

proc unwrap[T](c: TCtx, r: Result[T, SemReport]): T =
  if r.isErr:
    c.config.localReport(r.takeErr)
  else:
    result = r.unsafeGet

proc generateGlobalInit(c: var TCtx, f: var CodeFragment, defs: openArray[PNode]) =
  ## Generates and emits code for the given `{.global.}` initialization
  ## statements (`nkIdentDefs` in this case) into `f`
  template swapState() =
    #swap(c.code, f.code)
    #swap(c.debug, f.debug)
    swap(c.prc, f.prc)

  # In order to generate code into the fragment, the fragment's state is
  # swapped with the `TCtx`'s one
  swapState()

  for def in defs.items:
    assert def.kind == nkIdentDefs
    for i in 0..<def.len-2:
      # note: don't transform the expressions here; they already were, during
      # transformation of their owning procs
      let
        a = c.genExpr(def[i])
        b = c.genExpr(def[^1])
        r = c.irs.irAsgn(askInit, c.unwrap a, c.unwrap b)

  # Swap back once done
  swapState()

proc generateEntryProc(g: PassEnv, mlist: ModuleList,
                       modules: seq[ModuleDataExtra]): IrStore3 =
  ## Generates and returns the code for the entry procedure. The entry
  ## procedure is responsible for calling the data-init and init procedures
  ## of each module. On exit, the entry procedure returns the value of the
  ## ``programResult`` global

  # first, emit calls to the modules' data-init procedure
  for id in mlist.modulesClosed.items:
    let m {.cursor.} = modules[id]
    if m.dataInitProc != NoneProc:
      discard result.irCall(result.irProc(m.dataInitProc))

    if sfSystemModule in m.flags:
      # the 'init' procedure for the ``system`` module must be called right
      # after the data init procedure
      # XXX: that's what ``cgen`` does, but what's the reason behind this
      #      requirement?
      discard result.irCall(result.irProc(m.initProc))

  # then call the init procedures
  for id in mlist.modulesClosed.items:
    let m {.cursor.} = modules[id]
    # a call to the ``system`` module's init procedure was generated already
    if m.initProc != NoneProc and sfSystemModule notin m.flags:
      discard result.irCall(result.irProc(m.initProc))

  # TODO: setting the stack-bottom is missing

  # use the ``programResult`` global as the return value
  let resultVar = result.addLocal Local(kind: lkVar, typ: g.getSysType(tyInt))
  discard result.irAsgn(askInit, result.irLocal(resultVar),
                        result.irSym(g.compilerglobals["programResult"]))

func collectRoutineSyms(s: IrStore3, env: ProcedureEnv, list: var seq[PSym], known: var IntSet) =
  for n in s.nodes:
    case n.kind
    of ntkProc:
      let sym = env.orig[n.procId] # XXX: inefficient
      collect(list, sym, known)

    else: discard

func moduleId(o: PIdObj): int32 {.inline.} =
  ## Returns the ID of the module `o` is *attached* to. Do note that in the
  ## case generic instantiations, this is not the necessarily the same module
  ## as the one returned by ``getModule(o)``
  o.itemId.module

# XXX: copied from `cgen.nim` and adjusted
proc getCFile(config: ConfigRef, filename: AbsoluteFile): AbsoluteFile =
  ## `filename` is the file path without the file extension
  let ext = ".nim.c"
  result = changeFileExt(
    completeCfilePath(config, withPackageName(config, filename)), ext)


proc getSysMagic2(g: ModuleGraph, name: string, m: TMagic): PSym =
  ## Same as ``magicsys.getSysMagic``, except that:
  ## * it doesn't use ``localReport``.
  ## * procedures returning int don't have higher precedence
  ## * `nil` is returned if no matching magic is found
  ## * no line info is required
  let id = getIdent(g.cache, name)
  for r in systemModuleSyms(g, id):
    if r.magic == m:
      result = r

proc newPassEnv(g: ModuleGraph, tgen: var DeferredTypeGen, syms: var SymbolEnv,
                procs: var ProcedureEnv): PassEnv =
  new(result)

proc initCompilerProcs(p: PassEnv, g: ModuleGraph, tgen: var DeferredTypeGen,
                       procs: var ProcedureEnv, syms: var DeferredSymbols,
                       data: var LiteralData) =
  ## Registers all ``.compilerproc`` symbols with `p`
  # TODO: rename
  for sym in g.compilerprocs.items:
    case sym.kind
    of routineKinds:
      p.compilerprocs[sym.name.s] = procs.requestProc(sym)
    of skType:
      p.compilertypes[sym.name.s] = tgen.requestType(sym.typ)
    of skVar, skLet:
      p.compilerglobals[sym.name.s] = syms.requestSym(sym)
    of skConst:
      p.compilerconsts[sym.name.s] = data.add(sym.astdef)
    else:
      unreachable(sym.kind)

# TODO: needs a different name:
proc resolveTypeBoundOps(p: PassEnv, g: ModuleGraph, tgen: DeferredTypeGen, procs: var ProcedureEnv) =
  for op, tbl in p.attachedOps.mpairs:
    for k, v in g.attachedOps[op].pairs:
      let t = tgen.lookupType(k)
      if t != NoneType:
        tbl[t] = procs.requestProc(v)
      else:
        # XXX: is this case even possible
        discard#echo "missing type for type-bound operation"

proc initSysTypes(p: PassEnv, g: ModuleGraph, types: var TypeEnv, tgen: var DeferredTypeGen) =
  template addPrim(tk: TTypeKind) =
    p.sysTypes[tk] = types.addPrimitiveType(tgen, g.config, g.getSysType(unknownLineInfo, tk))

  # it's important that ``tyChar`` and ``tyVoid`` are added first, since
  # other primitive types depend on them already existing
  addPrim(tyVoid)
  addPrim(tyChar)

  for t in {tyBool, tyPointer, tyInt..tyFloat64, tyUInt..tyUInt64, tyString, tyCstring}.items:
    addPrim(t)

proc logError(conf: ConfigRef, ir: IrStore3, prc: ProcId, env: IrEnv, pos: (bool, int)) =
  let sym = env.procs.orig.getOrDefault(prc)
  if sym != nil:
    echo conf.toFileLineCol(sym.info)
  else:
    echo "???"

  if pos[0]:
    echo "Node added at:"
    echoTrace(ir, pos[1])

  if pos[0]:
    echo "Node position: ", pos[1]

  echo "IR:"
  printIr(ir, env, calcStmt(ir))

  printTypes(ir, env)

template logError(ir: IrStore3, env: IrEnv, prc: ProcId, code: untyped) =
  try:
    code
  except PassError as e:
    logError(conf, ir, prc, env, (true, e.n))
    raise
  except:
    logError(conf, ir, prc, env, (false, 0))
    raise

func finishTypes*(g: PassEnv, types: DeferredTypeGen,
                  procs: var openArray[IrStore3],
                  penv: var ProcedureEnv, syms: var SymbolEnv) =
  ## Replaces all used placeholder type IDs generated during IR creation with the
  ## correct ones

  for ir in procs.mitems:
    mapTypes(ir, types)

  mapTypes(penv, types)
  mapTypes(syms, types)

  for it in g.compilertypes.mvalues:
    it = types.map(it)

# TODO: needs a better name
func processObjects(g: PassEnv, ic: var IdentCache, types: var TypeEnv, syms: var SymbolEnv,
                    objects: Table[TypeId, PType]) =
  ## Adds the ``m_type`` field to ``RootObj`` and rebases all non-final object
  ## types that don't have a base type onto ``RootObj``. `objects` holds the
  ## mappings from each object type to it's respective source type (i.e. the
  ## ``PType`` it was translated from).
  let rootType = g.getCompilerType("RootObj")

  # XXX: we're depending on a ``ptr TNimType`` type existing here. The
  #      ``.compilerproc`` global ``hti.nimTypeRoot`` is of that type,
  #      so it's currently always available.
  let fieldType = types.lookupGenericType(tnkPtr, g.getCompilerType("TNimType"))

  # insert a field of type ``PNimType`` into ``RootObj``. In order to not make
  # type processing more complex (by rewriting each record type inheriting
  # from ``RootObj`` plus all ``ntkPathObj`` nodes), the type is changed to
  # have a negative field offset
  types.setFieldOffset(rootType, -1)
  types.replaceRecord(rootType):
    [(syms.addDecl(ic.getIdent("m_type")), fieldType)]

  for id, t in objects.pairs:
    if id != rootType and types.base(id) == NoneType and
       tfFinal notin t.flags and sfPure notin t.sym.flags:
      # if an object has no ancestor, can be inherited from, and is not marked
      # as pure, it needs a type field. Since inserting a new field into
      # records is effectively not possible with how field access works (i.e.
      # lookup by position), we resort to setting the type's base to
      # ``RootObj`` (which provides the type field)
      # XXX: the way this is implemented here differs from how it works in the
      #      old back-end. There, a type field (a header) was directly
      #      embedded into the object
      types.setBase(id, rootType)

func computeTypeFieldStatus(pe: PassEnv, types: TypeEnv,
                            objects: Table[TypeId, PType]): TypeFieldInfo =
  ## Computes and returns the type-field status for each type in `types`. This
  ## procedure must be called after ``processObjects`` as it depends on the
  ## type transformations done by the latter
  # TODO: move this procedure somewhere else
  let rootType = pe.getCompilerType("RootObj")

  template `[]`(x: TypeFieldInfo, i: TypeId): untyped =
    x[toIndex(i)]
  template `[]=`(x: TypeFieldInfo, i: TypeId, v: TypeFieldStatus) =
    x[toIndex(i)] = v

  func hasEmbedded(types: TypeEnv, id: TypeId, tf: TypeFieldInfo): bool =
    for f in types.fields(types[id]):
      if tf[f.typ] in {tfsHeader, tfsEmbedded}:
        return true

    result = false

  sync(result, types)

  # we depend on the following ``TypeEnv`` properties present after
  # translation here:
  # 1. *structural* types are visited in such an order as that each direct
  #   dependency is visited before the types dependent on it
  # 2. nominal record types are are visited after all their base types
  #
  # Since ``processObjects`` already took place, 2. is only partially
  # fulfilled

  # set the type-field state for ``RootObj`` to ``header``. All objects
  # inheriting from ``RootObj`` will start off in the ``header`` state then
  result[toIndex(rootType)] = tfsHeader

  # first iteration: propagate the 'header' status (originating from
  # ``RootObj``). Each ``object`` type is visited *after* it's base type(s)
  for id, t in types.items:
    if t.kind == tnkRecord and t.base != NoneType:
      # the type-header status is inherited from the the base type. That is,
      # if the base has a type-header, the inheriting object is treated as
      # having one too
      assert t.base == rootType or t.base.toIndex < id.toIndex # sanity check
      result[id] = result[t.base]

  # second iteration: compute the 'embedded' status for structural types and
  # propagate it
  for id, t in types.items:
    case t.kind
    of tnkArray:
      result[id] =
        case result[t.base]
        of tfsNone: tfsNone
        of tfsHeader, tfsEmbedded: tfsEmbedded

    of tnkRecord:
      # don't iterate over nominal record types (``object``) yet, they're not
      # ordered before their dependencies
      if id notin objects and hasEmbedded(types, id, result):
        result[id] = tfsEmbedded

    else:
      # an atom -- has no type-header nor embedded objects
      discard

  # third iteration: compute the 'embedded' status for nominal record types
  # and propagate it
  # XXX: if `objects` would be an ``OrderedTable``, we could iterate it
  #      directly
  for id, t in types.items:
    if id in objects and
      ((t.base != NoneType and result[t.base] == tfsEmbedded) or
       hasEmbedded(types, id, result)):
      result[id] = tfsEmbedded


proc drain(c: var TCtx, conf: ConfigRef, env: var IrEnv, code: var seq[IrStore3], a, b: var seq[PSym], seenProcs: var IntSet) =
  # TODO: rename
  assert b.len == 0

  while a.len > 0:
    # make sure that there's a slot for each known procedure in `code`
    sync(code, c.procs)

    for sym in a.items:
      let
        id = c.procs.requestProc(sym) # TODO: use non-mutating lookup
        idx = id.toIndex

      if sfImportc in sym.flags:
        # a quick fix to not run `irgen` for 'importc'ed procs
        code[idx].owner = id
        continue

      let ir = c.unwrap generateCodeForProc(c, sym)
      collectRoutineSyms(ir, c.procs, b, seenProcs)

      code[idx] = ir
      code[idx].owner = id

    block:
      let start = b.len

      # scan the collected constants for referenced routines. The list only
      # contains not-yet-scanned constants
      for sym in c.collectedConsts.items:
        collectRoutineSyms(astdef(sym), b, seenProcs)

      # clear the list so that the loop invariant mentioned above holds
      c.collectedConsts.setLen(0)

      # register the routines with the environment
      for i in start..<b.len:
        discard c.procs.requestProc(b[i])

    # flush deferred types already to reduce memory usage a bit
    c.types.flush(env.types, c.defSyms, conf)

    a.setLen(0)
    swap(a, b)

func addProcedure(codeList: var seq[IrStore3], id: ProcId, code: sink IrStore3) =
  let idx = id.toIndex
  codeList.setLen(idx + 1)
  codeList[idx] = move code

proc generateCode*(g: ModuleGraph) =
  ## The backend's entry point. Orchestrates code generation and linking. If
  ## all went well, the resulting binary is written to the project's output
  ## file
  let
    mlist = g.backend.ModuleListRef
    conf = g.config

  echo "starting codgen"

  var procImpls: seq[IrStore3] ## proc-id -> IR representation
  var modules: seq[ModuleData]
  var modulesExtra: seq[ModuleDataExtra]
  # XXX: since both of them associate data with a module ID, and also share the
  #      same lifetime, merge them into a single sequence?
  modules.newSeq(mlist.modules.len)
  modulesExtra.newSeq(mlist.modules.len)

  var env = IrEnv()

  var seenProcs: IntSet
  var nextProcs, nextProcs2: seq[PSym]

  var c = TCtx(config: g.config, graph: g, idgen: g.idgen)
  c.magicPredicate = proc(m: TMagic): bool = m in CallMagics
  swap(c.procs, env.procs)
  swap(c.data, env.data)
  c.types.voidType = g.getSysType(unknownLineInfo, tyVoid)
  c.types.charType = g.getSysType(unknownLineInfo, tyChar)

  # setup a ``PassEnv``
  let passEnv = PassEnv()
  passEnv.initSysTypes(g, env.types, c.types)
  passEnv.initCompilerProcs(g, c.types, c.procs, c.defSyms, c.data)

  c.passEnv = passEnv

  # mark all compilerprocs as seen so that they don't get collected during
  # the following dependency scanning
  for it in g.compilerprocs.items:
    if it.kind in routineKinds:
      seenProcs.incl it.id

  # the range of definitely alive procedures starts after the compilerprocs
  sync(procImpls, c.procs)
  var aliveRange = procImpls.len..0

  # generate all module init procs (i.e. code for the top-level statements):
  for i, m in mlist.modules.cpairs:
    # TODO: use ``lpairs`` (or ``pairs`` once it uses ``lent``) instead of
    #       ``cpairs``
    modulesExtra[i] =
      ModuleDataExtra(fileIdx: m.sym.position.FileIndex, flags: m.sym.flags)

    c.module = m.sym
    c.idgen = g.idgen

    var code = generateTopLevelStmts(m, c, g.config)
    if code.isSome:
      # the module needs an 'init' procedure
      let id = c.procs.add(c.types.requestType(c.types.voidType),
                           g.cache.getIdent("init"),
                           keepName=false)

      # the logic for registering procedures with modules uses the source
      # symbol tracked by ``ProcedureEnv.orig`` as the entity providing which
      # module the procedure is owned by/attached to. We just pass the
      # symbol of the module
      # XXX: ``orig`` needs to be phased out. It's only reamining purpose is
      #      to keep hacks working. To provide the "attached-to" information,
      #      either store the procedure's associated ``ModuleId`` in
      #      ``ProcHeader`` or attach it via a side-channel
      c.procs.orig[id] = m.sym

      modulesExtra[i].initProc = id
      # XXX: ``Option`` is missing a ``take`` procedure
      addProcedure(procImpls, id, move code.get())

  # we no longer need the ``mlist.modules`` list, so we reset it so that the
  # the memory used by the collected top-level nodes can be reclaimed already
  reset(mlist.modules)

  assert nextProcs.len == 0
  for it in modulesExtra.items:
    if it.initProc != NoneProc:
      collectRoutineSyms(procImpls[it.initProc.toIndex],
                        c.procs, nextProcs, seenProcs)

  # run the dependency collection/``irgen`` loop using the procedures
  # referenced by top-level statements as the starting set
  drain(c, g.config, env, procImpls, nextProcs, nextProcs2, seenProcs)

  aliveRange.b = procImpls.high
  # `aliveRange` is now the slice of all definitely alive procedures

  block:
    # compilerprocs are a bit tricky to handle. We don't know if one of them can
    # be considered 'alive' (used) until after all IR transformation took place.
    # But at that point, it's too late for scanning/processing the used
    # compilerprocs since all processing is already done. To solve this, we
    # scan and process all compilerprocs and their dependencies upfront, but
    # only queue them for code-generation after a separate scanning (run after
    # all IR transformation took place) yields that they're part of the alive
    # graph

    # XXX: the current approach has the downside that code which may end up
    #      not being part of the alive graph is still put through all
    #      processing. A different approach would be to run all processing
    #      (dependency collection/irgen and the IR passes)

    # run ``irgen`` for compilerprocs and their (not yet seen) dependencies
    for it in g.compilerprocs.items:
      if it.kind in routineKinds:
        nextProcs.add it

    drain(c, g.config, env, procImpls, nextProcs, nextProcs2, seenProcs)

  # generate all deferred symbols and declarations. ``c.defSyms`` should not
  # be used past this point
  flush(c.defSyms, g.cache, env.syms)

  # XXX: it would be better to also set the type during
  #      ``DeferredSymbols.flush``. But then the procedure needs access to a
  #      ``DeferredTypeGen`` object - which is a bit awkward, since
  #      ``DeferredTypeGen.flush`` requires a ``DeferredSymbol`` object
  for id, s in env.syms.msymbols:
    if (let orig = env.syms.orig.getOrDefault(id); orig != nil):
      s.typ = c.types.requestType(orig.typ)

  # XXX: this is a problem. Resolving the type-bounds operation requires
  #      all alive types being present, but that isn't the case yet since
  #      the final flush hasn't happenend, but for that we need to call
  #      ``finish``, but we can't since we're still adding procedures
  resolveTypeBoundOps(passEnv, g, c.types, c.procs)

  block:
    # translate the literal data from their ``PNode``-based representation
    for id, data in c.constData.pairs:
      assert env.syms[id].kind == skConst
      env.syms.setData(id): add(c.data, c.procs, g.config, data)

    reset c.constData # no longer needed

  c.procs.finish(c.types, g.cache)

  block:
    # flush all remaining deferred types. ``c.symEnv`` was consumed when
    # flushing it, so we create a temporary new ``DeferredSymbol`` object here
    var defSyms = initDeferredSyms(env.syms)
    c.types.flush(env.types, defSyms, g.config)

    flush(defSyms, g.cache, env.syms)

  # replace all placeholder type IDs
  finishTypes(passEnv, c.types, procImpls, c.procs, env.syms)

  processObjects(passEnv, g.cache, env.types, env.syms, c.types.objects)

  swap(c.procs, env.procs)
  swap(c.data, env.data)

  let objects = move c.types.objects

  # TODO: ``generateCode`` needs to be split up. Freeing the ``TCtx`` should
  #       happen automatically as part of stack-frame clean up
  reset(c) # we no longer need the irgen context

  block:
    # all alive globals are collected by now - register them with their
    # owning module

    for id in env.syms.items:
      let s = env.syms[id]
      case s.kind
      of skVar, skLet, skForVar:
        # TODO: remove the guard once locals are not stored in the symbol
        #       table anymore
        if sfGlobal in s.flags:
          let mIdx = mlist.moduleMap[env.syms.orig[id].getModule().moduleId]
          modules[mIdx].syms.add(id)

      else:
        discard

  # XXX: mutable because it needs to swapped in and out of the ``RefcPassCtx``.
  #      The way immutable data is passed to passses needs an overhaul in
  #      general. In the case of ``tfInfo``, ``shallowCopy`` could be used, but
  #      it's only available for refc. For ARC/ORC ``.cursor`` would have to be
  #      used instead
  var
    tfInfo = computeTypeFieldStatus(passEnv, env.types, objects)
    gcInfo = computeGcLookup(env.types)

  var lpCtx = LiftPassCtx(graph: passEnv, idgen: g.idgen, cache: g.cache)
  lpCtx.env = addr env
  var ttc = TypeTransformCtx(graph: passEnv, ic: g.cache)
  var upc = initUntypedCtx(passEnv, addr env) # XXX: not mutated - should be ``let``

  block:
    # the lowering of ``echo`` for the C-targets has to happen *before*
    # transforming ``openArray``s because the pass inserts code that needs to
    # also be transformed by the latter
    for s, ir in mpairsId(procImpls, ProcId):
        logError(ir, env, s):
          runPass(ir, upc, lowerEchoPass)

  block:
    # the ``openArray`` lowering has to happen separately
    # TODO: explain why
    # TODO: since the ``lowerOpenArrayPass`` doesn't mutate the procedures'
    #       parameters anymore, it should be possible to run it in a batch
    #       with others
    var ctx: LowerOACtx
    ctx.init(passEnv)

    for s, irs in mpairsId(procImpls, ProcId):
      logError(irs, env, s):
        runPass(irs, initTypeContext(irs, env), env, ctx, lowerOpenArrayPass)

  lowerOpenArrayTypes(ttc, env.types, env.syms)

  let destructorSeq = optSeqDestructors in conf.globalOptions

  block:
    # if a non-v2 garbage collector (e.g. ``refc``, ``markAndSweep``,
    # everything else that uses ``unsureAsgnRef``) is active, the sequence
    # operation lowering has to happen before GC transform pass
    let seqPass =
      if destructorSeq:
        seqV1Pass #seqV2Pass
      else:
        seqV1Pass

    for s, irs in mpairsId(procImpls, ProcId):
      logError(irs, env, s):
        # the error-handling pass inserts new nodes (instead of replacing
        # them), which might cause conflicts with other changes if performed
        # concurrently. To be on the safe side, the changes are applied separately.
        lowerTestError(irs, passEnv, g.cache, env.types, env.procs, env.syms)

        block:
          var diff = initChanges(irs)
          let typeCtx = initTypeContext(irs, env)
          # TODO: resue the ``TypeContext`` object across the batches (and maybe
          #       the loop)
          # hooks need to be resolved before injecting the garbage collector
          # related logic
          runPass2(irs, typeCtx, env, passEnv, diff, hookPass)

          # the changes done by this pass need to be visible to further
          # passes, so it can't be run in the same batch
          runPass2(irs, typeCtx, env, passEnv, diff, lowerMatchPass)

          apply(irs, diff)

        block:
          # TODO: reuse the ``TypeContext`` object across the loop
          # TODO: resue the ``Changes`` object
          var diff = initChanges(irs)
          runPass2(irs, initTypeContext(irs, env), env, passEnv, diff, seqPass)
          runPass2(irs, diff, lpCtx, seqConstV1Pass)

          apply(irs, diff)

  if destructorSeq:
    discard #lowerSeqTypesV2(ttc, env.types, env.syms)
  else:
    lowerSeqTypesV1(ttc, env.types, env.syms)

  # don't commit the type changes yet. The following passes still need access
  # to the original type
  # XXX: ideally they shouldn't. If sequence types were lowered to ``ref``
  #      types, it wouldn't be necessary
  var remap: TypeMap
  swap(remap, ttc.remap)

  for s, irs in mpairsId(procImpls, ProcId):
      logError(irs, env, s):
        block:
          # the following passes all modify/replace different nodes and don't
          # depend on each others changes, so they're run concurrently
          var diff = initChanges(irs)
          let typeCtx = initTypeContext(irs, env, remap)

          # TODO: don't swap the seqs in and out inside the loop -- do it once
          #       outside the loop
          var rpCtx: RefcPassCtx
          rpCtx.setupRefcPass(passEnv, typeCtx, env, irs)
          template swapState() =
            swap(rpCtx.tfInfo, tfInfo)
            swap(rpCtx.gcLookup, gcInfo)
          swapState()

          runPass2(irs, typeCtx, env, passEnv, diff, lowerRangeCheckPass)
          runPass2(irs, typeCtx, env, passEnv, diff, lowerSetsPass)

          runPass2(irs, typeCtx, env, rpCtx, diff, refcPass)

          runPass2(irs, diff, upc, ofV1Pass)

          runPass2(irs, diff, lpCtx, setConstPass)
          runPass2(irs, diff, lpCtx, arrayConstPass)

          apply(irs, diff)

          swapState()

        # TODO: lifting the type info needs to happen after the alive
        #       procedure detection. Otherwise, we're creating RTTI that isn't
        #       actually used
        runPass(irs, lpCtx, typeV1Pass)


  block:
    # perform the C-target specific constant-data transformations. This has to
    # happen *before* lowering types, as we need the original types

    # first, lift ``set|string|seq`` literals that are part of other constants
    # into their own constants
    liftSetConsts(env.syms, env.data, lpCtx.constCache, g.cache.getIdent("setConst"), env.types)
    liftSeqConstsV1(env.syms, env.data, lpCtx.constCache, g.cache.getIdent("seqConst"), env.types)

    # TODO: merge the changes from both passes before applying them
    transformSetConsts(passEnv, env.syms, env.data, env.types)
    if optSeqDestructors in conf.globalOptions:
      discard
    else:
      transformSeqConstsV1(passEnv, env.syms, env.data, env.types)

  block:
    # we don't know the owning module of the types corresponding to the
    # lifted RTTI globals, so we simply add the globals to the system
    # module
    # XXX: this is different to what the current code-generator does
    # XXX: instead, all RTTI globals and their initialization logic
    #      could be registered to a dedicated module (.c file)
    for id in lpCtx.typeInfoMarker.values:
      modules[mlist.moduleMap[g.systemModule.moduleId]].syms.add(id)

  # now commit the lowered sequence-types
  commit(env.types, remap)

  lowerSetTypes(ttc, env.types, env.syms)

  block:
    # apply transformations meant for the C-like targets
    var cenv: CTransformEnv
    applyCTypeTransforms(cenv, passEnv, env.types, env.syms)

    let
      ctx = CTransformCtx(graph: passEnv, transEnv: addr cenv)
      paramName = g.cache.getIdent("ClE")
      envName = env.syms.addDecl(g.cache.getIdent(":env"))

    for s, irs in mpairsId(procImpls, ProcId):
      logError(irs, env, s):
        block:
          let typeCtx = initTypeContext(irs, env)
          var diff = initChanges(irs)

          runPass2(irs, typeCtx, env, ctx, diff, ctransformPass)
          runPass2(irs, typeCtx, env, ctx, diff, lowerClosuresPass)

          apply(irs, diff)

        transformClosureProc(passEnv, paramName, envName, s, env.procs, irs)

    finish(cenv, env.types)

  var mainProc: ProcId
  block:
    # generate and register the 'main' procedure

    # the code-generator must not mangle the name
    mainProc = env.procs.add(passEnv.sysTypes[tyInt],
                             g.cache.getIdent("main"), keepName=true)

    # TODO: the 'argc', 'args', and 'env' parameters are missing, as well as
    #       the logic for assigning them to the corresponding globals

    addProcedure(procImpls, mainProc):
      generateEntryProc(passEnv, mlist[], modulesExtra)

    env.procs.orig[mainProc] = g.ifaces[g.config.projectMainIdx2.int32].module

  template register(items: iterable[int]) =
    ## Registers all procedures stored by index in `x` to the owning module
    for it in items:
      let
        id = ProcId(it + 1) # TODO: not acceptable
        sym = env.procs.orig[id]
        mIdx = mlist.moduleMap[sym.getModule().moduleId]

      modules[mIdx].procs.add id

  # TODO: move this logic into a separate proc
  block:
    # `procImpls` is partitioned in the following way:
    # +---------------+-----------------+------------+
    # | compilerprocs | explicitly used | additional |
    # +---------------+-----------------+------------+
    #
    # To figure out which procedures from the "compilerprocs" and "additional"
    # partitions are actually used, an iterative dependency collection using
    # the procedures in the "B" partition as the starting set is run
    var collected: IntSet
    var a, b: IntSet

    template markImpl(extra: untyped) {.dirty.} =
      for n in code.nodes:
        case n.kind
        of ntkProc:
          let elem = toIndex(n.procId).int
          if elem notin ignore and extra:
            next.incl elem

        else:
          discard

    func mark(code: IrStore3, ignore: Slice[int], next: var IntSet) =
      markImpl():
        true

    func mark2(code: IrStore3, ignore: Slice[int], alive: IntSet, next: var IntSet) =
      markImpl():
        elem notin alive

    # calculate the starting set
    for i in aliveRange.items:
      mark(procImpls[i], aliveRange, a)

    # also include the main procedure in order for the data-init procs to be
    # marked as alive
    a.incl mainProc.toIndex.int

    # run the main collection
    while a.len > 0:
      collected.incl(a)
      for it in a.items:
        mark2(procImpls[it], aliveRange, collected, b)

      a.clear()
      swap(a, b)

    # `collected` now stores all indirectly used procedures - register them
    register(collected.items)

  # register all explicitly used procedures
  register(aliveRange.items)

  var gCtx: GlobalGenCtx
  initGlobalContext(gCtx, env)

  for i, m in modulesExtra.pairs:
    if modules[i].syms.len == 0 and modules[i].procs.len == 0:
      # don't generate anything for modules that have no alive content
      continue

    let
      cname = getCFile(conf, AbsoluteFile toFullPath(conf, m.fileIdx))
      cf = Cfile(nimname: g.ifaces[m.fileIdx.int].module.name.s, cname: cname,
                 obj: completeCfilePath(conf, toObjFile(conf, cname)),
                 flags: {})

    emitModuleToFile(conf, cname, gCtx, env, procImpls, modules[i])

    addFileToCompile(conf, cf)


  # code generation is finished


# Below is the `passes` interface implementation

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  if graph.backend == nil:
    graph.backend = ModuleListRef()

  let
    mlist = ModuleListRef(graph.backend)
    next = mlist.modules.len
    id = module.itemId.module

  assert id >= 0 and id == module.position # sanity check

  # append an empty module to the list
  mlist.modules.growBy(1)
  mlist.modules[next] = Module(sym: module)
  mlist.moduleMap[id] = next

  result = ModuleRef(list: mlist, index: next)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n
  let m = ModuleRef(b)

  if n.kind == nkStmtList:
    m.list.modules[m.index].stmts.add(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)

  let m = ModuleRef(b)
  m.list.modulesClosed.add(m.index)

const cgen2Pass* = makePass(myOpen, myProcess, myClose)