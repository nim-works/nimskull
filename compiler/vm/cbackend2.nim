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
    irgen,
    irtypes,
    vmir,
    cgen2,
    irpasses,
    irdbg
  ],
  experimental/[
    results
  ]

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

    initGlobalsCode: CodeFragment ## the bytecode of `initGlobalsProc`. Each
      ## encountered `{.global.}`'s init statement gets code-gen'ed into the
      ## `initGlobalCode` of the module that owns it
    initGlobalsProc: (SymId, IrStore3) ## the proc that initializes `{.global.}`
      ## variables
    initProc: (SymId, IrStore3) ## the module init proc (top-level statements)

  ModuleListRef = ref ModuleList
  ModuleList = object of RootObj
    modules: seq[Module]
    modulesClosed: seq[int] ## indices into `modules` in the order the modules
                            ## were closed. The first closed module comes
                            ## first, then the next, etc.
    moduleMap: Table[int, int] ## module sym-id -> index into `modules`

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

func collectRoutineSyms(ast: PNode, syms: var seq[PSym]) =
  ## Traverses the `ast`, collects all symbols that are of routine kind and
  ## appends them to `syms`
  if ast.kind == nkSym:
    if ast.sym.kind in routineKinds:
      syms.add(ast.sym)

    return

  for i in 0..<ast.safeLen:
    collectRoutineSyms(ast[i], syms)

proc generateTopLevelStmts*(module: var Module, c: var TCtx,
                            config: ConfigRef) =
  ## Generates code for all collected top-level statements of `module` and
  ## compiles the fragments into a single function. The resulting code is
  ## stored in `module.initProc`
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

  # the `initProc` symbol is missing a valid `ast` field
  module.initProc[0] = c.symEnv.addSym(skProc, NoneType, "init") # TODO: non-obvious mutation, move this somewhere else
  module.initProc[1] = c.irs

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

proc genInitProcCall(c: var IrStore3, m: Module) =
  discard c.irCall(c.irSym(m.initProc[0]))

proc generateEntryProc(c: var TCtx, g: PassEnv, mlist: ModuleList): IrStore3 =
  ## Generates the entry function and returns it's function table index.
  ## The entry function simply calls all given `initProcs` (ordered from low
  ## to high by their function table index) and then returns the value of the
  ## ``programResult`` global

  # setup code-gen state. One register for the return value and one as a
  # temporary to hold the init procs
  c.prc = PProc()
  c.irs.reset()

  let resultVar = c.irs.genLocal(lkVar, g.getSysType(tyInt))

  var systemIdx, mainIdx: int
  # XXX: can't use `pairs` since it copies
  for i in 0..<mlist.modules.len:
    let sym = mlist.modules[i].sym
    if sfMainModule     in sym.flags: mainIdx = i
    elif sfSystemModule in sym.flags: systemIdx = i

  # Call the init procs int the right order. That is, module-closed-order with
  # special handling for the main and system module
  genInitProcCall(c.irs, mlist.modules[systemIdx])
  for mI in mlist.modulesClosed.items:
    let m = mlist.modules[mI]
    if {sfMainModule, sfSystemModule} * m.sym.flags == {}:
      genInitProcCall(c.irs, m)

  genInitProcCall(c.irs, mlist.modules[mainIdx])

  # write ``programResult`` into the result variable

  # XXX: ``programResult`` is not a compiler*proc*
  #[
  let prSym = g.getCompilerProc("programResult")
  c.irs.irAsgn(askInit, c.irs.irLocal(resultVar), c.irs.irProc(prSym))
  ]#

  # refc-compatible "move"
  swap(result, c.irs)

proc generateMain(c: var TCtx, g: PassEnv,
                  mlist: ModuleList): IrStore3 =
  ## Generates and links in the main procedure (the entry point) along with
  ## setting up the required state.

  # lastly, generate the actual code:
  result = generateEntryProc(c, g, mlist)

func collectRoutineSyms(s: IrStore3, env: ProcedureEnv, list: var seq[PSym], known: var IntSet) =
  for n in s.nodes:
    case n.kind
    of ntkProc:
      let sym = env.orig.getOrDefault(n.procId) # XXX: inefficient
      # XXX: excluding all magics is wrong. Depending on which back-end is
      #      used, some magics are treated like any other routine
      if sym != nil and
         sym.magic == mNone and
         sym.id notin known:
        known.incl(sym.id)
        list.add(sym)
    else: discard

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
  for sym in g.compilerprocs.items:
    case sym.kind
    of routineKinds:
      result.compilerprocs[sym.name.s] = procs.requestProc(sym)
    of skType:
      result.compilertypes[sym.name.s] = tgen.requestType(sym.typ)
    else:
      # TODO: the rest (e.g. globals) also need to be handled
      discard

  # XXX: a magic is not necessarily a procedure - it can also be a type
  # create a symbol for each magic to be used by the IR transformations
  for m in low(TMagic)..high(TMagic):
    # fetch the name from a "real" symbol
    let sym = g.getSysMagic2("", m)

    let name =
      if sym.isNil():
        # not every magic has symbol defined in ``system.nim`` (e.g. procs and
        # types only used in the backend)
        $m
      else:
        sym.name.s

    if sym != nil and sym.kind notin routineKinds:
      # we don't care about magic types here
      continue

    result.magics[m] = procs.addMagic(NoneType, name, m)

  for op, tbl in result.attachedOps.mpairs:
    for k, v in g.attachedOps[op].pairs:
      let t = tgen.lookupType(k)
      if t != NoneType:
        tbl[t] = procs.requestProc(v)
      else:
        # XXX: is this case even possible
        discard#echo "missing type for type-bound operation"

  for t in { tyVoid, tyInt..tyFloat64, tyBool, tyChar, tyString, tyCstring, tyPointer }.items:
    result.sysTypes[t] = tgen.requestType(g.getSysType(unknownLineInfo, t))

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

proc generateCode*(g: ModuleGraph) =
  ## The backend's entry point. Orchestrates code generation and linking. If
  ## all went well, the resulting binary is written to the project's output
  ## file
  let
    mlist = g.backend.ModuleListRef
    conf = g.config

  echo "starting codgen"

  var moduleProcs: seq[seq[(ProcId, IrStore3)]]
  moduleProcs.newSeq(mlist.modules.len)

  var env = IrEnv()

  var c = TCtx(config: g.config, graph: g, idgen: g.idgen)
  swap(c.symEnv, env.syms)
  swap(c.procs, env.procs)
  c.types.env = addr env.types
  c.types.voidType = g.getSysType(unknownLineInfo, tyVoid)
  c.types.charType = g.getSysType(unknownLineInfo, tyChar)

  # setup a ``PassEnv``
  let passEnv = newPassEnv(g, c.types, c.symEnv, c.procs)
  c.passEnv = passEnv

  # generate all module init procs (i.e. code for the top-level statements):
  for m in mlist.modules.mitems:
    c.module = m.sym
    c.idgen = g.idgen
    generateTopLevelStmts(m, c, g.config)

    # combine module list iteration with initialiazing `initGlobalsCode`:
    m.initGlobalsCode.prc = PProc()

  var nextProcs: seq[PSym]
  var seenProcs: IntSet

  for it in mlist.modules.items:
    collectRoutineSyms(it.initProc[1], c.procs, nextProcs, seenProcs)

  var nextProcs2: seq[PSym]
  while nextProcs.len > 0:
    for it in nextProcs.items:
      let mIdx = it.itemId.module
      let realIdx = mlist.moduleMap[it.getModule().id]
      let sId = c.procs.requestProc(it)

      if g.getBody(it).kind == nkEmpty:
        # a quick fix to not run `irgen` for 'importc'ed procs
        moduleProcs[realIdx].add((sId, IrStore3(owner: sId)))
        continue

      let ir = generateCodeForProc(c, it)
      collectRoutineSyms(c.unwrap ir, c.procs, nextProcs2, seenProcs)

      #doAssert mIdx == realIdx
      moduleProcs[realIdx].add((sId, c.unwrap ir))
      moduleProcs[realIdx][^1][1].owner = sId

    # flush deferred types already to reduce memory usage a bit
    c.types.flush(c.symEnv, g.config)

    nextProcs.setLen(0)
    swap(nextProcs, nextProcs2)

  for id, s in c.symEnv.msymbols:
    if (let orig = c.symEnv.orig.getOrDefault(id); orig != nil):
      s.typ = c.types.requestType(orig.typ)

  c.procs.finish(c.types)

  c.types.flush(c.symEnv, g.config)

  let entryPoint =
    generateMain(c, passEnv, mlist[])

  swap(env.syms, c.symEnv)
  swap(c.procs, env.procs)

  var lpCtx = LiftPassCtx(graph: passEnv, idgen: g.idgen, cache: g.cache)
  lpCtx.env = addr env

  for i in 0..<mlist.modules.len:
    for s, irs in moduleProcs[i].mitems:
      let orig = irs
      logError(irs, env, s):
        runPass(irs, initHookCtx(passEnv, irs, env), hookPass)

        lowerTestError(irs, passEnv, env.types, env.procs, env.syms)
        var rpCtx: RefcPassCtx
        rpCtx.setupRefcPass(passEnv, addr env, g, g.idgen, irs)
        runPass(irs, rpCtx, lowerSetsPass)

        rpCtx.setupRefcPass(passEnv, addr env, g, g.idgen, irs)
        runPass(irs, rpCtx, lowerRangeCheckPass)

        rpCtx.setupRefcPass(passEnv, addr env, g, g.idgen, irs)
        #runV2(irs, rpCtx, refcPass)
        runPass(irs, rpCtx, refcPass)
        if optSeqDestructors in conf.globalOptions:
          rpCtx.setupRefcPass(passEnv, addr env, g, g.idgen, irs)
        else:
          rpCtx.setupRefcPass(passEnv, addr env, g, g.idgen, irs)
          runPass(irs, rpCtx, seqV1Pass)

        runPass(irs, lpCtx, typeV1Pass)


  # type lowering passes
  var ttc = TypeTransformCtx(graph: passEnv)
  if optSeqDestructors in conf.globalOptions:
    discard
  else:
    lowerSeqTypesV1(ttc, env.types, env.syms)

  var gCtx: GlobalGenCtx
  initGlobalContext(gCtx, env)

  for i, m in mlist.modules.pairs:
    let cfile = getCFile(conf, AbsoluteFile toFullPath(conf, m.sym.position.FileIndex))
    var cf = Cfile(nimname: m.sym.name.s, cname: cfile,
                   obj: completeCfilePath(conf, toObjFile(conf, cfile)), flags: {})

    emitModuleToFile(conf, cfile, gCtx, env, moduleProcs[i])

    addFileToCompile(conf, cf)


  # code generation is finished


# Below is the `passes` interface implementation

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  if graph.backend == nil:
    graph.backend = ModuleListRef()

  let
    mlist = ModuleListRef(graph.backend)
    next = mlist.modules.len

  # append an empty module to the list
  mlist.modules.growBy(1)
  mlist.modules[next] = Module(sym: module)
  mlist.moduleMap[module.id] = next

  result = ModuleRef(list: mlist, index: next)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n
  let m = ModuleRef(b)

  const declarativeKinds = routineDefs + {nkTypeSection, nkPragma,
    nkExportStmt, nkExportExceptStmt, nkFromStmt, nkImportStmt,
    nkImportExceptStmt}

  if n.kind notin declarativeKinds:
    m.list.modules[m.index].stmts.add(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)

  let m = ModuleRef(b)
  m.list.modulesClosed.add(m.index)

const cgen2Pass* = makePass(myOpen, myProcess, myClose)