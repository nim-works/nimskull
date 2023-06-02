## The backend for the VM. The core code-generation is done by `vmgen`; the
## linking bits and artifact creation are implemented here.
##
## Executable generation happens in roughly the following steps:
## 1. Generate all module init procedures (i.e. code for all top-level
##  statements)
## 2. Iteratively generate code for all alive routines (excluding `method`s)
## 3. Generate the main procedure
## 4. Pack up all required data into `PackedEnv` and write it to the output
##  file
##
## Similiar to the C and JS backend, dead-code-elimination (DCE) happens as a
## side-effect of how the routines are processed

import
  std/[
    tables
  ],
  compiler/ast/[
    ast,
    ast_types,
    lineinfos
  ],
  compiler/backend/[
    backends
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    collectors,
    transf
  ],
  compiler/mir/[
    mirbridge
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/utils/[
    containers
  ],
  compiler/vm/[
    packed_env,
    vmaux,
    vmdef,
    vmgen,
    vmlegacy,
    vmobjects,
    vmops,
    vmtypegen
  ],
  experimental/[
    results
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_backend import BackendReport
from compiler/ast/report_enums import ReportKind

import std/options as stdoptions

type
  CodeFragment = object
    ## The state required for generating code in multiple steps.
    ## `CodeFragment` helps when generating code for multiple procedures in
    ## an interleaved manner.
    prc: PProc
    code: seq[TInstr]
    debug: seq[TLineInfo]

  BModule = object
    initGlobalsCode: CodeFragment
      ## the bytecode for the module's pre-init procedure

  ModuleId = distinct uint32
    ## The ID of a ``BModule`` instance.

  BModuleList = object
    orig: ModuleList ## the list with the modules as produced by the
                     ## "collector" pass

    modules: Store[ModuleId, BModule]
    moduleMap: Table[int, ModuleId]
      ## maps a module's position to the ID of the module's ``BModule``
      ## instance

func growBy[T](x: var seq[T], n: Natural) {.inline.} =
  x.setLen(x.len + n)

iterator cpairs[T](s: seq[T]): (int, lent T) =
  ## Continous pair iterator. Supports `s` growing during iteration
  var i = 0
  while i < s.len:
    yield (i, s[i])
    inc i


proc registerCallbacks(c: var TCtx) =
  ## Registers callbacks for various functions, so that no code is
  ## generated for them and that they can (must) be overridden by the runner

  # the callbacks registered here need to be manually kept in sync with the
  # callbacks registered in ``vmrunner.registerCallbacks``. The runner will
  # complain if there's a mismatch

  template cb(key: string) =
    c.callbackKeys.add(IdentPattern(key))

  registerBasicOps(c)
  registerDebugOps(c)
  registerIoReadOps(c)
  registerIoWriteOps(c)
  registerOsOps(c)
  registerOs2Ops(c)

  # Used by some tests
  cb "stdlib.system.getOccupiedMem"

func initFuncTblEntry(sym: PSym, sig: RoutineSigId, info: CodeInfo): FuncTableEntry =
  FuncTableEntry(sym: sym, sig: sig,
                 kind: ckDefault,
                 start: info.start, regCount: info.regCount.uint16)

proc appendCode(c: var TCtx, f: CodeFragment) =
  ## Copies the code from the fragment to the end of the global code buffer
  c.code.add(f.code)
  c.debug.add(f.debug)

func collectRoutineSyms(ast: PNode, syms: var seq[PSym]) =
  ## Traverses the `ast`, collects all symbols that are of routine kind and
  ## appends them to `syms`
  if ast.kind == nkSym:
    if ast.sym.kind in routineKinds:
      syms.add(ast.sym)

    return

  for i in 0..<ast.safeLen:
    collectRoutineSyms(ast[i], syms)

proc queueProcedure(c: var TCtx, prc: PSym) =
  ## Queues the procedure `prc` for later code generation if it wasn't
  ## already.
  registerLinkItem(c.symToIndexTbl, c.linkState.newProcs, prc,
                   c.linkState.nextProc)

proc refresh(c: var TCtx, m: Module) =
  ## Prepares the code-generator state of `c` for being used to generate code
  ## belonging to the module `m`.
  assert m.idgen != nil
  c.refresh(m.sym, m.idgen)

proc genStmt(c: var TCtx, n: PNode): auto =
  ## Wrapper around ``vmgen.genStmt`` that canonicalizes the input AST first
  let n = canonicalizeWithInject(c.graph, c.idgen, c.module, n, {goIsNimvm})
  c.gatherDependencies(n, withGlobals=true)
  vmgen.genStmt(c, n)

proc generateCodeForProc(c: var TCtx, s: PSym,
                         globals: var seq[PNode]): VmGenResult =
  ## Generates and emits the bytecode for the procedure `s`. The globals
  ## defined in it are extracted from the body and their identdefs appended
  ## to `globals`.
  var body = transformBody(c.graph, c.idgen, s, cache = false)
  extractGlobals(body, globals, isNimVm = true)
  body = canonicalizeWithInject(c.graph, c.idgen, s, body, {goIsNimvm})
  c.gatherDependencies(body, withGlobals=true)
  result = genProc(c, s, body)

proc generateGlobalInit(c: var TCtx, f: var CodeFragment, defs: openArray[PNode]) =
  ## Generates and emits code for the given `{.global.}` initialization
  ## statements (`nkIdentDefs` in this case) into `f`
  template swapState() =
    swap(c.code, f.code)
    swap(c.debug, f.debug)
    swap(c.prc, f.prc)

  # In order to generate code into the fragment, the fragment's state is
  # swapped with the `TCtx''s one
  swapState()

  for def in defs.items:
    assert def.kind == nkIdentDefs
    for i in 0..<def.len-2:
      if def[^1].kind == nkEmpty:
        # do nothing for globals without initializer expressions
        continue

      # note: don't transform the expressions here; they already were, during
      # transformation of their owning procs
      let
        asgn = newTreeI(nkAsgn, def[i].info, def[i], def[^1])
        r = genStmt(c, asgn)

      if unlikely(r.isErr):
        c.config.localReport(vmGenDiagToLegacyReport(r.takeErr))

  # Swap back once done
  swapState()

proc generateAliveProcs(c: var TCtx, mlist: var BModuleList) =
  ## Runs code generation for all routines (except methods) directly used
  ## by the routines in `c.linkState.newProcs`, including the routines in
  ## the list itself.
  ##
  ## This function can be called multiple times, but `c.linkState.newProcs`
  ## must only contain not-yet-code-gen'ed routines each time.
  ##
  ## An alive routine is a routine who's symbol is used in: a top-level
  ## statement; another alive routine's body; a ``const``s value

  let start = c.functions.len
  # adjust the function table size. Since `generateAllProcs` may be called
  # multiple times, `setLen` has to be used instead of `newSeq`
  c.functions.setLen(c.linkState.nextProc)

  var globals: seq[PNode]
    ## the identdefs of global defined inside procedures. Reused across loop
    ## iterations for efficiency

  # `newProcs` can grow during iteration, so `citems` has to be used
  for ri, sym in c.linkState.newProcs.cpairs:
    c.config.internalAssert(sym.kind notin {skMacro, skTemplate}):
      "unexpanded macro or template"

    let i = start + ri
    c.functions[i] = c.initProcEntry(sym)

    # - don't generate code for a procedure that is overridden with a
    #   callback
    # - ``lfNoDecl`` is used internally by the backend to signal that code
    #   generation is handled elsewhere
    if c.functions[i].kind == ckCallback or lfNoDecl in sym.loc.flags:
      continue

    c.refresh(mlist.orig[sym.itemId.module.FileIndex])

    # code-gen' the routine. This might add new entries to the `newProcs` list
    let r = generateCodeForProc(c, sym, globals)
    if r.isOk:
      fillProcEntry(c.functions[i], r.unsafeGet)
    else:
      c.config.localReport(vmGenDiagToLegacyReport(r.takeErr))

    # generate and emit the code for `{.global.}` initialization here, as the
    # initializer expression might depend on otherwise unused procedures (which
    # might define further globals...)
    if globals.len > 0:
      let mI = mlist.moduleMap[c.module.position]
      generateGlobalInit(c, mlist.modules[mI].initGlobalsCode,
                         globals)

      # prepare for reuse:
      globals.setLen(0)

    # code-gen might've found new functions, so adjust the function table:
    c.functions.setLen(c.linkState.nextProc)

proc generateCodeForMain(c: var TCtx, modules: var BModuleList): FunctionIndex =
  ## Generate, emits, and links in the main procedure (the entry point).
  let prc = generateMainProcedure(c.graph, mainModule(modules.orig).idgen, modules.orig)
  queueProcedure(c, prc)

  # generate the code for `prc` and all its dependencies. New pure globals
  # discovered beyond this point will be ignored: no initialization nor
  # de-initialization logic will be generated for them.
  generateAliveProcs(c, modules)

  result = c.symToIndexTbl[prc.id].FunctionIndex

func storeExtra(enc: var PackedEncoder, dst: var PackedEnv,
                routineSymLookup: sink Table[int, LinkIndex],
                consts: seq[(PVmType, PNode)], globals: seq[PVmType]) =
  ## Stores the previously gathered complex constants and globals into `dst`

  var denc: DataEncoder
  denc.startEncoding(dst)
  # XXX: `sink` is used for `routineSymLookup` in order to get around a
  #      deep copy
  denc.routineSymLookup = routineSymLookup

  # complex constants (i.e. non-literals):
  mapList(dst.cconsts, consts, it):
    let id = dst.nodes.len
    dst.nodes.growBy(1)
    denc.storeData(dst, it[1])
    (enc.typeMap[it[0]], id.uint32)

  # for globals, only their types are stored. All initialization is
  # done in VM bytecode
  mapList(dst.globals, globals, it):
    enc.typeMap[it]

proc patchInitProcedure(prc: PSym, preInit: PSym) =
  ## Patches the `prc` with a call to the `preInit` procedure. This is a
  ## temporary solution for making sure that pure globals (i.e., those defined
  ## via the ``.global`` pragma) get initialized.
  let call = newTree(nkCall, newSymNode(preInit))
  if prc.ast[bodyPos].kind == nkEmpty:
    prc.ast[bodyPos] = call
  else:
    # prepend to the other statements
    let n = prc.ast[bodyPos]
    assert n.kind == nkStmtList
    n.sons.insert(call, 0)

proc initModuleList(g: ModuleGraph, c: var TCtx,
                    mlist: sink ModuleList): BModuleList =
  ## Creates and sets up the list storing backend data associated with each
  ## module.
  result = BModuleList()

  for it in mlist.modules.values:
    var m = BModule()
    m.initGlobalsCode.prc = PProc(sym: it.preInit)

    result.moduleMap[it.sym.position] = result.modules.add(m)

  result.orig = mlist

proc generateCode*(g: ModuleGraph, mlist: sink ModuleList) =
  ## The backend's entry point. Orchestrates code generation and linking. If
  ## all went well, the resulting binary is written to the project's output
  ## file
  let
    conf = g.config

  var c = TCtx(config: g.config, cache: g.cache, graph: g, idgen: g.idgen,
               mode: emStandalone)

  c.typeInfoCache.init()

  # register the extra ops so that code generation isn't performed for the
  # corresponding procs:
  registerCallbacks(c)

  var mlist = initModuleList(g, c, mlist)

  # queue the module-init procedures for code generation and register the
  # contents of the modules' structs:
  for m in mlist.orig.modules.values:
    # we're manually generating code for the pre-init procedure, so prevent
    # normal code generation from interfering:
    m.preInit.loc.flags.incl lfNoDecl

    # patch in the call to the pre-init procedure:
    patchInitProcedure(m.init, m.preInit)
    queueProcedure(c, m.init)

    template declareGlobal(sym: PSym) =
      # we silently ignore imported globals here and let ``vmgen`` raise an
      # error when one is accessed
      if lfNoDecl notin sym.loc.flags and sfImportc notin sym.flags:
        # make sure the type is generated and register the global in the
        # link table
        discard getOrCreate(c, sym.typ)
        registerLinkItem(c.symToIndexTbl, c.linkState.newGlobals, sym,
                         c.linkState.nextGlobal)

    for s in m.structs.globals.items:
      declareGlobal(s)

    for s in m.structs.globals2.items:
      declareGlobal(s)

    for s in m.structs.threadvars.items:
      declareGlobal(s)

  # generate code for all alive routines
  generateAliveProcs(c, mlist)
  reset(c.linkState.newProcs) # free the occupied memory already

  # XXX: generation of method dispatchers would go here. Note that `method`
  #      support will require adjustments to DCE handling
  #generateMethods(c)

  # create procs from the global initializer code fragments
  for m in mlist.modules.mitems:
    template frag: untyped = m.initGlobalsCode

    let
      start = c.code.len
      rc = frag.prc.regInfo.len

    c.appendCode(frag)
    c.gABC(g.emptyNode, opcRet)

    # XXX: always add an entry for now, even if the procedure is empty.
    #      It's easier to properly implement this once the processing for
    #      procedure-level globals is unified across the backends
    fillProcEntry(c.functions[c.symToIndexTbl[frag.prc.sym.id]]): (start: start, regCount: rc)

    # the fragment isn't used beyond this point anymore, so it can be freed
    # already
    reset(frag)


  let entryPoint = generateCodeForMain(c, mlist)

  c.gABC(g.emptyNode, opcEof)

  # code generation is finished

  # collect globals and `const`s:
  # XXX: these two steps could be combined with storing into `PackedEnv`.
  #      Pros: no need for the `globals` and `consts` seqs
  #      Cons: (probably) higher I-cache pressure, slightly more complex logic

  var globals = newSeq[PVmType](c.linkState.newGlobals.len)
  for i, sym in c.linkState.newGlobals.pairs:
    let typ = c.typeInfoCache.lookup(conf, sym.typ)
    # the type was already created during vmgen
    globals[i] = typ.unsafeGet

  var consts = newSeq[(PVmType, PNode)](c.linkState.newConsts.len)
  for i, sym in c.linkState.newConsts.pairs:
    let typ = c.typeInfoCache.lookup(conf, sym.typ)
    consts[i] = (typ.unsafeGet, sym.ast)

  # pack the data and write it to the ouput file:
  var
    enc: PackedEncoder
    env: PackedEnv

  enc.init(c.types)
  storeEnv(enc, env, c)
  storeExtra(enc, env, c.symToIndexTbl, consts, globals)
  env.code = move c.code
  env.entryPoint = entryPoint

  let err = writeToFile(env, prepareToWriteOutput(conf))
  if err != RodFileError.ok:
    let rep = BackendReport(kind: rbackVmFileWriteFailed,
                            outFilename: conf.absOutFile.string,
                            failureMsg: $err)
    conf.globalReport(rep)
