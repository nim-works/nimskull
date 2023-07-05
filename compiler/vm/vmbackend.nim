## The code generation orchestrator for the VM backend. It takes the
## semantically analysed AST of the whole program as input and produces
## a``.nimbc`` executable file for it.
##
## Generating the actual code for procedures and statements is delegated to
## ``vmgen``, with the orchestrator assembling the bytecode fragments and
## additional data into the final executable.

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
    modulelowering,
  ],
  compiler/mir/[
    mirgen
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

  PartialTbl = Table[int, CodeFragment]
    ## Maps the symbol ID of a partial procedure to the in-progress fragment

func growBy[T](x: var seq[T], n: Natural) {.inline.} =
  x.setLen(x.len + n)

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

proc appendCode(c: var TCtx, f: CodeFragment) =
  ## Copies the code from the fragment to the end of the global code buffer
  c.code.add(f.code)
  c.debug.add(f.debug)

proc refresh(c: var TCtx, m: Module) =
  ## Prepares the code-generator state of `c` for processing AST
  ## belonging to the module `m`.
  assert m.idgen != nil
  c.refresh(m.sym, m.idgen)

proc generateCodeForProc(c: var TCtx, s: PSym, body: sink MirFragment): CodeInfo =
  ## Generates and the bytecode for the procedure `s` with body `body`. The
  ## resulting bytecode is emitted into the global bytecode section.
  let
    body = generateAST(c.graph, c.idgen, s, body)
    r    = genProc(c, s, body)

  if r.isOk:
    result = r.unsafeGet
  else:
    c.config.localReport(vmGenDiagToLegacyReport(r.takeErr))

proc genStmt(c: var TCtx, f: var CodeFragment, stmt: PNode) =
  ## Generates and emits the code for a statement into the fragment `f`.
  template swapState() =
    swap(c.code, f.code)
    swap(c.debug, f.debug)
    swap(c.prc, f.prc)

  # in order to generate code into the fragment, the fragment's state is
  # swapped with `c`'s
  swapState()
  let r = genStmt(c, stmt)
  swapState() # swap back

  if unlikely(r.isErr):
    c.config.localReport(vmGenDiagToLegacyReport(r.takeErr))

proc declareGlobal(c: var TCtx, sym: PSym) =
  # we silently ignore imported globals here and let ``vmgen`` raise an
  # error when one is accessed
  if lfNoDecl notin sym.loc.flags and sfImportc notin sym.flags:
    # make sure the type is generated and register the global in the
    # link table
    discard getOrCreate(c, sym.typ)
    registerLinkItem(c.symToIndexTbl, c.linkState.newGlobals, sym,
                      c.linkState.nextGlobal)

proc prepare(c: var TCtx, data: var DiscoveryData) =
  ## Registers with the link table all procedures, constants, globals,
  ## and threadvars discovered as part of producing the currently
  ## processed event.

  c.functions.setLen(data.procedures.len)
  for i, it in peek(data.procedures):
    c.functions[i] = c.initProcEntry(it)
    c.symToIndexTbl[it.id] = LinkIndex(i)

    # if a procedure's implementation is overridden with a VM callback, we
    # don't want any processing to happen for it, which we signal to the
    # event producer via ``lfNoDecl``
    if c.functions[i].kind == ckCallback:
      it.loc.flags.incl lfNoDecl

  # register the constants with the link table:
  for i, s in visit(data.constants):
    c.symToIndexTbl[s.id] = LinkIndex(i)

  for _, s in visit(data.globals):
    declareGlobal(c, s)

  for _, s in visit(data.threadvars):
    declareGlobal(c, s)

proc processEvent(c: var TCtx, mlist: ModuleList, discovery: var DiscoveryData,
                  partial: var PartialTbl, evt: sink BackendEvent) =
  ## The orchestrator's event processor.
  prepare(c, discovery)

  c.refresh(mlist[evt.module])

  case evt.kind
  of bekModule:
    discard "nothing to do"
  of bekPartial:
    let p = addr mgetOrPut(partial, evt.sym.id, CodeFragment())
    if p.prc == nil:
      # it's a fragment that was just started
      p.prc = PProc(sym: evt.sym)

    let stmt = generateAST(c.graph, c.idgen, evt.sym, evt.body)
    genStmt(c, p[], stmt)
  of bekProcedure:
    # a complete procedure became available
    let r = generateCodeForProc(c, evt.sym, evt.body)
    fillProcEntry(c.functions[c.symToIndexTbl[evt.sym.id]], r)

proc generateAliveProcs(c: var TCtx, config: BackendConfig,
                        discovery: var DiscoveryData, mlist: var ModuleList) =
  ## Generates and emits the bytecode for all alive procedure (excluding the
  ## entry point).
  var
    partial: PartialTbl

  for evt in process(c.graph, mlist, discovery, MagicsToKeep, config):
    processEvent(c, mlist, discovery, partial, evt)

  # finish the partial procedures:
  for s, frag in partial.mpairs:
    let
      start = c.code.len
      rc = frag.prc.regInfo.len

    c.appendCode(frag)
    c.gABC(c.graph.emptyNode, opcRet)

    let id = registerProc(c, frag.prc.sym)
    fillProcEntry(c.functions[id.int]): (start: start, regCount: rc)

    frag.prc.sym.ast[bodyPos] = newNode(nkStmtList)

    # the fragment isn't used beyond this point anymore, so it can be freed
    # already
    reset(frag)

proc generateCodeForMain(c: var TCtx, config: BackendConfig,
                         modules: var ModuleList): FunctionIndex =
  ## Generate, emits, and links in the main procedure (the entry point).
  let prc = generateMainProcedure(c.graph, mainModule(modules).idgen, modules)
  var p = preprocess(config, prc, c.graph, c.idgen)
  process(p, c.graph, c.idgen)

  result = registerProc(c, prc)

  let r = generateCodeForProc(c, prc, p.body)
  fillProcEntry(c.functions[result.int], r)

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

proc generateCode*(g: ModuleGraph, mlist: sink ModuleList) =
  ## The backend's entry point. Orchestrates code generation and linking. If
  ## all went well, the resulting binary is written to the project's output
  ## file
  let
    conf = g.config
    bconf = BackendConfig(noImported: true, options: {goIsNimvm})

  var c = TCtx(config: g.config, cache: g.cache, graph: g, idgen: g.idgen,
               mode: emStandalone)

  c.typeInfoCache.init()

  # register the extra ops so that code generation isn't performed for the
  # corresponding procs:
  registerCallbacks(c)

  # generate code for all alive routines:
  var discovery: DiscoveryData
  generateAliveProcs(c, bconf, discovery, mlist)

  let entryPoint = generateCodeForMain(c, bconf, mlist)

  c.gABC(g.emptyNode, opcEof)

  # ----- code generation is finished

  # collect globals and `const`s:
  # XXX: these two steps could be combined with storing into `PackedEnv`.
  #      Pros: no need for the `globals` and `consts` seqs
  #      Cons: (probably) higher I-cache pressure, slightly more complex logic

  var globals = newSeq[PVmType](c.linkState.newGlobals.len)
  for i, sym in c.linkState.newGlobals.pairs:
    let typ = c.typeInfoCache.lookup(conf, sym.typ)
    # the type was already created during vmgen
    globals[i] = typ.unsafeGet

  var consts = newSeq[(PVmType, PNode)](discovery.constants.len)
  for i, sym in all(discovery.constants):
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
