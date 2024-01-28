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
    backends,
    cgir
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    modulelowering,
  ],
  compiler/mir/[
    datatables,
    mirbodies,
    mirenv,
    mirgen,
    mirtrees
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/utils/[
    containers,
    idioms
  ],
  compiler/vm/[
    packed_env,
    vmaux,
    vmdef,
    vmgen,
    vmlegacy,
    vmlinker,
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
  PartialProc = object
    ## The in-progress body of a procedure.
    sym: PSym
    body: Body

  PartialTbl = Table[ProcedureId, PartialProc]
    ## Maps the ID of a partial procedure to its in-progress body

  GenCtx = object
    ## State of the orchestrator.
    graph: ModuleGraph

    # link tables:
    globals: OrdinalSeq[GlobalId, PVmType]
    functions: OrdinalSeq[ProcedureId, FuncTableEntry]
    # no extra data is needed for constants

    gen: CodeGenCtx ## code generator state

func growBy[T](x: var seq[T], n: Natural) {.inline.} =
  x.setLen(x.len + n)

proc registerCallbacks(linking: var LinkerData) =
  ## Registers callbacks for various functions, so that no code is
  ## generated for them and that they can (must) be overridden by the runner

  # the callbacks registered here need to be manually kept in sync with the
  # callbacks registered in ``vmrunner.registerCallbacks``. The runner will
  # complain if there's a mismatch

  template cb(key: string) =
    linking.callbackKeys.add(IdentPattern(key))

  template register(iter: untyped) =
    for it in iter:
      cb(it.pattern)

  register: basicOps()
  register: debugOps()
  register: ioReadOps()
  register: ioWriteOps()
  register: osOps()
  register: os2Ops()

  # Used by some tests
  cb "stdlib.system.getOccupiedMem"

func setLinkIndex(c: var GenCtx, s: PSym, i: LinkIndex) =
  assert s.id notin c.gen.linking.symToIndexTbl
  c.gen.linking.symToIndexTbl[s.id] = i

proc initProcEntry(c: var GenCtx, prc: PSym): FuncTableEntry {.inline.} =
  initProcEntry(c.gen.linking, c.graph.config, c.gen.typeInfoCache, prc)

proc registerProc(c: var GenCtx, prc: ProcedureId) =
  ## Adds an empty function-table entry for `prc` and registers `prc` in the
  ## link table.
  let idx = int prc
  # make space for table entry:
  if idx >= c.functions.len:
    c.functions.setLen(idx + 1)

  let sym = c.gen.env[prc]
  c.functions[prc] = c.initProcEntry(sym)
  setLinkIndex(c, sym, LinkIndex idx)

proc generateCodeForProc(c: var CodeGenCtx, idgen: IdGenerator, s: PSym,
                         body: sink MirBody): CodeInfo =
  ## Generates and the bytecode for the procedure `s` with body `body`. The
  ## resulting bytecode is emitted into the global bytecode section.
  let
    body = generateIR(c.graph, idgen, c.env, s, body)
    r    = genProc(c, s, body)

  if r.isOk:
    result = r.unsafeGet
  else:
    c.config.localReport(vmGenDiagToLegacyReport(r.takeErr))

proc declareGlobal(c: var GenCtx, sym: PSym) =
  if exfNoDecl notin sym.extFlags and sfImportc notin sym.flags:
    # make sure the type is generated and register the global in the
    # link table
    discard c.globals.add(getOrCreate(c.gen, sym.typ))
  else:
    # we silently ignore imported globals here and let ``vmgen`` raise an
    # error when one is accessed. A global must still be registered,
    # otherwise `GlobalId` would go out of sync
    discard c.globals.add(c.gen.typeInfoCache.emptyType)

proc prepare(c: var GenCtx, n: MirNode) =
  ## Responds to the discovery of entity `n`. This means registering it with
  ## the link table, if necessary.
  case n.kind
  of mnkProc:
    let it = c.gen.env[n.prc]
    registerProc(c, n.prc)

    # if a procedure's implementation is overridden with a VM callback, we
    # don't want any processing to happen for it, which we signal to the
    # event producer via ``exfNoDecl``
    if c.functions[n.prc].kind == ckCallback:
      it.extFlags.incl exfNoDecl

  of mnkConst:
    discard "nothing to do"
  of mnkGlobal:
    # global or threadvar
    declareGlobal(c, c.gen.env[n.global])
  else:
    unreachable(n.kind)

proc processEvent(c: var GenCtx, mlist: ModuleList, discovery: var DiscoveryData,
                  partial: var PartialTbl, evt: sink BackendEvent) =
  ## The orchestrator's event processor.
  let idgen = mlist[evt.module].idgen
  c.gen.module = mlist[evt.module].sym

  case evt.kind
  of bekDiscovered:
    prepare(c, evt.entity)
  of bekModule, bekConstant:
    discard "nothing to do"
  of bekPartial:
    let p = addr mgetOrPut(partial, evt.id, PartialProc(sym: evt.sym))
    discard merge(p.body):
      generateIR(c.graph, idgen, c.gen.env, evt.sym, evt.body)
  of bekProcedure:
    # a complete procedure became available
    let r = generateCodeForProc(c.gen, idgen, evt.sym, evt.body)
    fillProcEntry(c.functions[evt.id], r)
  of bekImported:
    # not supported at the moment; ``vmgen`` is going to raise an
    # error when generating a call to a dynlib procedure
    discard "ignore"

proc generateAliveProcs(c: var GenCtx, config: BackendConfig,
                        discovery: var DiscoveryData, mlist: var ModuleList) =
  ## Generates and emits the bytecode for all alive procedure (excluding the
  ## entry point).
  var
    partial: PartialTbl

  for evt in process(c.graph, mlist, c.gen.env, discovery, config):
    processEvent(c, mlist, discovery, partial, evt)

  # generate the bytecode for the partial procedures:
  for id, p in partial.mpairs:
    let r = genProc(c.gen, p.sym, move p.body)
    if r.isOk:
      fillProcEntry(c.functions[id]): r.unsafeGet
    else:
      c.gen.config.localReport(vmGenDiagToLegacyReport(r.takeErr))

    # mark as non-empty:
    p.sym.ast[bodyPos] = newNode(nkStmtList)

    # the fragment isn't used beyond this point anymore, so it can be freed
    # already
    reset(p)

proc generateCodeForMain(c: var GenCtx, config: BackendConfig,
                         modules: var ModuleList): ProcedureId =
  ## Generate, emits, and links in the main procedure (the entry point).
  let
    idgen = mainModule(modules).idgen
    prc = generateMainProcedure(c.graph, idgen, modules)
    id = c.gen.env.procedures.add(prc)

  # transform, generate code, and register in the function table:
  let body = translate(id, prc.ast[bodyPos], c.graph, config, idgen, c.gen.env)
  registerProc(c, id)
  fillProcEntry(c.functions[id], generateCodeForProc(c.gen, idgen, prc, body))

  result = id

proc storeExtra(enc: var PackedEncoder, dst: var PackedEnv,
                linking: sink LinkerData, config: ConfigRef,
                consts: seq[(PVmType, PNode)], globals: seq[PVmType]) =
  ## Stores the previously gathered complex constants and globals into `dst`

  var denc = DataEncoder(config: config)
  denc.startEncoding(dst)
  denc.routineSymLookup = move linking.symToIndexTbl

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

  # TODO: add support for either `distinct string` or custom `storePrim`
  #       overloads (or both) to `rodfiles`. Due to the lack of both, we
  #       have to perform a manual copy instead of a move here
  mapList(dst.callbacks, linking.callbackKeys, c):
    c.string

proc generateCode*(g: ModuleGraph, mlist: sink ModuleList) =
  ## The backend's entry point. Orchestrates code generation and linking. If
  ## all went well, the resulting binary is written to the project's output
  ## file
  let
    conf = g.config
    bconf = BackendConfig(noImported: true, tconfig:
              TranslationConfig(options: {goIsNimvm},
                                magicsToKeep: MagicsToKeep))

  var c =
    GenCtx(graph: g,
           gen: CodeGenCtx(config: g.config, graph: g, mode: emStandalone))

  c.gen.typeInfoCache.init()
  c.gen.typeInfoCache.initRootRef(g.config, g.getCompilerProc("RootObj").typ)

  # register the extra ops so that code generation isn't performed for the
  # corresponding procs:
  registerCallbacks(c.gen.linking)

  # generate code for all alive routines:
  var discovery: DiscoveryData
  generateAliveProcs(c, bconf, discovery, mlist)

  let entryPoint = generateCodeForMain(c, bconf, mlist)

  c.gen.gABC(unknownLineInfo, opcEof)

  # ----- code generation is finished

  # set up a VM execution environment and fill it with the artifacts produced
  # by the of code generator:
  var env: TCtx
  env.config = c.gen.config # currently needed by the packer
  env.code = move c.gen.code
  env.debug = move c.gen.debug
  env.functions = move base(c.functions)
  env.constants = move c.gen.constants
  env.rtti = move c.gen.rtti

  # produce a list with the type of each constant:
  var consts = newSeq[(PVmType, PNode)](c.gen.env.data.len)
  for i, data in c.gen.env.data.pairs:
    let typ = c.gen.typeInfoCache.lookup(conf, data.typ)
    consts[ord(i)] = (get(typ), data)

  env.typeInfoCache = move c.gen.typeInfoCache

  # pack the data and write it to the ouput file:
  var
    enc: PackedEncoder
    penv: PackedEnv

  enc.init(env.types)
  storeEnv(enc, penv, env)
  storeExtra(enc, penv, c.gen.linking, conf, consts, base(c.globals))
  penv.entryPoint = FunctionIndex(entryPoint)

  let err = writeToFile(penv, prepareToWriteOutput(conf))
  if err != RodFileError.ok:
    let rep = BackendReport(kind: rbackVmFileWriteFailed,
                            outFilename: conf.absOutFile.string,
                            failureMsg: $err)
    conf.globalReport(rep)
