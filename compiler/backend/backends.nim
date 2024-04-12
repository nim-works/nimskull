## Shared processing logic used by all backends.

import
  std/[
    deques,
    dynlib, # for computing possible candidate names
    strtabs,
    tables
  ],
  compiler/ast/[
    ast,
    idents,
    lineinfos
  ],
  compiler/backend/[
    cgmeth,
    cgir,
    cgirgen
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/mir/[
    datatables,
    injecthooks,
    mirbodies,
    mirbridge,
    mirconstr,
    mirenv,
    mirgen,
    mirpasses,
    mirtrees,
    sourcemaps,
    utils
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/sem/[
    injectdestructors,
    modulelowering,
    transf,
    varpartitions
  ],
  compiler/utils/[
    containers,
    idioms
  ]

export TranslationConfig

type
  BackendConfig* = object
    ## Configuration state altering the operation of the ``process``
    ## iterator.
    tconfig*: TranslationConfig
      ## passed on to ``mirgen``. See ``mirgen.generateCode`` for more
      ## details
    noImported*: bool
      ## if ``true``, indicates that a procedure with a body should not be
      ## treated as imported, even if it's marked as such

  DiscoveryData* = object
    ## Bundles all data needed during the disovery of alive, backend-relevant
    ## entities.
    progress: EnvCheckpoint
      ## tracks how much of the environment was already processed (e.g.,
      ## translated, scanned, etc.)
    libs*: seq[LibId]
      ## all dynamic libraries that the alive graph depends on

    overrides: Table[ProcedureId, FileIndex]
      ## maps a procedure to the module it needs to be queued with.
      ## If not overriden, a procedure is queued with the module it's
      ## discovered from.

  BackendEventKind* = enum
    bekDiscovered## a new entity was discovered during MIR processing. This
                 ## event is meant to be used for pre-processing of symbols
                 ## and registration with the code generators -- the
                 ## environment must not be modified during processing of
                 ## this event
    bekModule    ## the initial set of alive entities for a module was
                 ## discovered
    bekConstant  ## a complete constant was processed and transformed
    bekPartial   ## a fragment of a procedure that's generated incrementally
                 ## became available
    bekProcedure ## a complete procedure was processed and transformed
    bekImported  ## an alive runtime-imported procedure finished processing

  BackendEvent* = object
    ## Progress event returned by the ``process`` iterator.
    module*: FileIndex
      ## the module in whose context the processing happens. For actions
      ## related to .inline procedures, this is not necessarily the module
      ## the symbol is attached to

    case kind*: BackendEventKind
    of bekDiscovered:
      entity*: MirNode
        ## a reference to the discovered entity
    of bekModule:
      discard
    of bekConstant:
      cnst*: ConstId
        ## the ID of the constant
    of bekPartial, bekProcedure, bekImported:
      id*: ProcedureId
        ## the ID of the procedure
      sym*: PSym
        ## the symbol of the procedure the event is about
        ## XXX: only here for convenience, remove it once feasible
      body*: MirBody

  WorkItemKind = enum
    wikPreprocess
      ## run the internal pre-processing for a procedure
    wikProcess
      ## translate a procedure to the MIR, apply all passes, and report the
      ## result
    wikProcessConst
      ## scan and translate a constant to its MIR representation
    wikProcessGlobals
      ## produce the init and deinit code for globals lifted from procedures
    wikImported
      ## report an imported procedure
    wikReport
      ## report that some fully-processed MIR fragment became available
    wikReportConst
      ## report that a fully-processed constant became available

  WorkItem = object
    ## For simpler processing and scheduling, much of the backend's pre
    ## processing is split into discrete steps, which are added to a work
    ## queue. `WorkItem` describes a step.
    case kind: WorkItemKind
    of wikPreprocess:
      raw: ProcedureId
    of wikProcess:
      prc: ProcedureId
      body: PNode
    of wikProcessConst, wikReportConst:
      cnst: ConstId
    of wikProcessGlobals:
      globals: seq[PNode]
        ## the unprocessed identdefs of globals lifted from a procedure's
        ## body. Due to how ``transf`` handles inlining, this list can
        ## contain duplicates
    of wikImported:
      imported: ProcedureId
    of wikReport:
      evt: range[bekPartial..bekImported]
      fragId: ProcedureId
      frag: MirBody

  WorkQueue = object
    items: Deque[tuple[item: WorkItem, fr: FileIndex]]
      ## the queued steps to run. Each item is associated with a module:
      ## it indicate used for events reported to the caller
    config: BackendConfig

func prepend(queue: var WorkQueue, m: FileIndex,
             item: sink WorkItem) {.inline.} =
  ## Adds `item` to the beginning of the queue.
  queue.items.addFirst (item, m)

func append(queue: var WorkQueue, m: FileIndex,
            item: sink WorkItem) {.inline.} =
  ## Adds `item` to the end of the queue.
  queue.items.addLast (item, m)

func moduleId*(o: PIdObj): int32 {.inline.} =
  ## Returns the ID of the module `o` is *attached* to. Do note that in the
  ## case of generic instantiations, this is not the necessarily the same
  ## module as the one indicated via the owner.
  o.itemId.module

# ---- main procedure generation -----

proc emitOpCall(graph: ModuleGraph, op: PSym, dest: PNode) =
  ## Emits a call to the provided operator `op`, but only if the operator is
  ## a non-empty procedure.
  if getBody(graph, op).kind != nkEmpty:
    dest.add newTree(nkCall, newSymNode(op))

proc generateMain*(graph: ModuleGraph, modules: ModuleList, result: PNode) =
  ## Generates the program initialization code and emits it to `result`. The
  ## initialization logic is the code that invokes each module's init
  ## procedures.

  # XXX: why not fully initialize the ``system`` module first?
  # first initialize the additional data associated with each module:
  for it in closed(modules):
    emitOpCall(graph, it.dataInit, result)
    emitOpCall(graph, it.dynlibInit, result)
    # the system module is special cased: its fully initialized during the
    # data-init phase
    if sfSystemModule in it.sym.flags:
      emitOpCall(graph, it.preInit, result)
      emitOpCall(graph, it.init, result)

  # then the modules are initialized and their module-level code executed
  for it in closed(modules):
    if sfSystemModule notin it.sym.flags:
      emitOpCall(graph, it.preInit, result)
      emitOpCall(graph, it.init, result)

proc generateTeardown*(graph: ModuleGraph, modules: ModuleList, result: PNode) =
  ## Generates the code for de-initializing the program, and emits it to
  ## `result`.
  # tearing down the modules has to happen in the reverse order they were
  # initialized in, but with ``system`` always coming last
  for it in rclosed(modules):
    if sfSystemModule notin it.sym.flags:
      emitOpCall(graph, it.destructor, result)
      emitOpCall(graph, it.postDestructor, result)

  emitOpCall(graph, systemModule(modules).destructor, result)
  emitOpCall(graph, systemModule(modules).postDestructor, result)

proc generateMainProcedure*(graph: ModuleGraph, idgen: IdGenerator,
                            modules: ModuleList): PSym =
  ## Generates the procedure for initializing, running, and de-initializing
  ## the full program (`modules`). The procedure returns the value of the
  ## internal ``programResult`` global.
  let
    owner = mainModule(modules).sym
    programRes = getCompilerProc(graph, "programResult")

  # setup the symbol:
  result = newSym(skProc, getIdent(graph.cache, "NimMain"), nextSymId idgen,
                  owner, unknownLineInfo, {})
  result.typ = newProcType(unknownLineInfo, nextTypeId idgen, owner)

  let resSym = newSym(skResult, getIdent(graph.cache, "result"),
                      nextSymId idgen, result, unknownLineInfo, {})
  resSym.typ = programRes.typ
  result.typ[0] = resSym.typ # set the correct return type

  var body = newNode(nkStmtList)
  generateMain(graph, modules, body)
  generateTeardown(graph, modules, body)
  # generate the result assignment:
  body.add newTree(nkAsgn, [newSymNode(resSym), newSymNode(programRes)])

  result.ast = newProcNode(nkProcDef, owner.info, body,
                           params        = newTree(nkFormalParams, [newNodeIT(nkType, owner.info, resSym.typ)]),
                           name          = newSymNode(result),
                           pattern       = graph.emptyNode,
                           genericParams = graph.emptyNode,
                           pragmas       = graph.emptyNode,
                           exceptions    = graph.emptyNode)
  # attach the result symbol:
  result.ast.sons.setLen(resultPos + 1)
  result.ast[resultPos] = newSymNode(resSym)

# ----- general queries about MIR fragments and trees -----

func isEmpty*(tree: MirTree): bool =
  ## Returns whether `tree` contains either no nodes or only nodes that have
  ## no meaning by themselves.
  for n in tree.items:
    if n.kind notin {mnkScope, mnkStmtList, mnkEnd}:
      return false

  result = true

func isEmpty*(f: MirBody): bool {.inline.} =
  isEmpty(f.code)

iterator deps*(tree: MirTree): lent MirNode =
  ## Returns all external entities (procedures, globals, etc.) that `tree`
  ## references *directly*, in an unspecified order.
  var i = NodePosition(0)
  while i < NodePosition(tree.len):
    let n {.cursor.} = tree[i]
    case n.kind
    of mnkDef:
      # skip over the name slot:
      i = NodePosition tree.operand(i, 1)
      continue
    of mnkProc, mnkProcVal:
      yield tree[i]
    of mnkGlobal:
      yield tree[i]
    else:
      discard "nothing to do"

    inc i

# ----- procedure lowering and transformation -----

proc preprocess*(queue: var WorkQueue, graph: ModuleGraph, idgen: IdGenerator,
                 env: MirEnv, id: ProcedureId, module: FileIndex) =
  ## Runs the ``transf`` pass on the body of `prc` and queues the steps
  ## needed for fully processing the procedure. `module` is the module the
  ## step was queued from: it's used as the module the next processing is
  ## queued from.
  let prc = env[id]
  if exfDynamicLib in prc.extFlags:
    # a procedure imported at runtime, it has no body
    queue.prepend(module, WorkItem(kind: wikImported, imported: id))
    return

  var body = transformBodyWithCache(graph, idgen, prc)

  # extract the identdefs of lifted globals (which is the first step towards
  # actually lifting them into proper globals) and store them with the
  # result. Do note that this step modifies the potentially cached body.
  var globals: seq[PNode]
  extractGlobals(body, globals,
                 isNimVm = goIsNimvm in queue.config.tconfig.options)

  queue.prepend(module, WorkItem(kind: wikProcess, prc: id, body: body))

  if globals.len > 0:
    # processing the lifted globals has to happen *before* processing the
    # procedure's body. In addition, the step is queued from the module
    # the procedure is *attached* to, not the one it's queued from
    queue.prepend(moduleId(prc).FileIndex):
      WorkItem(kind: wikProcessGlobals, globals: move globals)

proc process(body: var MirBody, prc: PSym, graph: ModuleGraph,
             idgen: IdGenerator, env: var MirEnv) =
  ## Applies all applicable MIR passes to the `body`. `prc` is the enclosing
  ## procedure.
  if shouldInjectDestructorCalls(prc):
    injectDestructorCalls(graph, idgen, env, prc, body)
    injectHooks(body, graph, env, prc)

    if graph.config.arcToExpand.hasKey(prc.name.s):
      graph.config.msgWrite("--expandArc: " & prc.name.s & "\n")
      graph.config.msgWrite(render(body.code, addr env, addr body))
      graph.config.msgWrite("\n-- end of expandArc ------------------------\n")

  let target =
    case graph.config.backend
    of backendC:       targetC
    of backendJs:      targetJs
    of backendNimVm:   targetVm
    of backendInvalid: unreachable()

  applyPasses(body, prc, env, graph, target)

proc translate*(id: ProcedureId, body: PNode, graph: ModuleGraph,
                config: BackendConfig, idgen: IdGenerator,
                env: var MirEnv): MirBody =
  ## Translates `body` to MIR code, applies all applicable MIR passes, and
  ## returns the result. `id` is the ID of the procedure the fragment belongs
  ## to.
  let prc = env[id]
  if optCursorInference in graph.config.options and
      shouldInjectDestructorCalls(prc):
    # TODO: turn cursor inference into a MIR pass and remove this part
    computeCursors(prc, body, graph)

  echoInput(graph.config, prc, body)
  result = generateCode(graph, env, prc, config.tconfig, body)
  echoMir(graph.config, prc, result)

  # now apply the passes:
  process(result, prc, graph, idgen, env)

proc generateIR*(graph: ModuleGraph, idgen: IdGenerator, env: MirEnv,
                 owner: PSym, body: sink MirBody): Body =
  ## Translates the MIR code provided by `code` into ``CgNode`` IR and,
  ## if enabled, echoes the result.
  result = cgirgen.generateIR(graph, idgen, env, owner, body)
  echoOutput(graph.config, owner, result)

# ------- handling of lifted globals ---------

proc produceFragmentsForGlobals(
    env: var MirEnv, identdefs: seq[PNode], graph: ModuleGraph,
    config: TranslationConfig): tuple[init, deinit: MirBody] =
  ## Given a list of identdefs of lifted globals, produces the MIR code for
  ## initialzing and deinitializing the globals. All not-yet-seen globals and
  ## threadvars are added to `env`.

  func prepare(bu: var MirBuilder, m: var SourceMap, n: PNode) {.nimcall.} =
    # the fragments need to be wrapped in scopes; some MIR passes depend
    # on this
    if bu.front.len == 0:
      discard bu.addLocal(Local()) # empty result slot
      bu.add(m.add(n)): MirNode(kind: mnkScope)

  func finish(bu: sink MirBuilder, m: var SourceMap, n: PNode
             ): auto {.nimcall.} =
    if bu.front.len > 0:
      bu.setSource(m.add(n))
      bu.add endNode(mnkScope)
    # we're creating a body here, so there is no list of locals yet
    result = finish(bu, default(Store[LocalId, Local]))

  var init, deinit: MirBuilder

  # lifted globals can appear re-appear in the identdefs list for two reasons:
  # - the definition appears in the body of a for-loop using an inline iterator
  #   with multiple yields
  # - the definition appears in the body of an inline iterator
  #
  # in both cases, all encountered definitions of a global are equal, but we
  # only want to generate code for the first definition we encounter
  for it in identdefs.items:
    let s = it[0].sym
    # threadvars don't support initialization nor destruction, so they're
    # skipped
    if sfThread in s.flags:
      discard env.globals.add(s)
    elif s notin env.globals: # cull duplicates
      let global = env.globals.add(s)
      # generate the MIR code for an initializing assignment:
      prepare(init, result.init.source, graph.emptyNode)
      generateAssignment(graph, env, config, it, init, result.init.source)

      # if the global requires one, emit a destructor call into the deinit
      # fragment:
      if hasDestructor(s.typ):
        prepare(deinit, result.deinit.source, graph.emptyNode)
        deinit.setSource(result.deinit.source.add(it[0]))
        genDestroy(deinit, graph, env, toValue(global, s.typ))

  (result.init.code, result.init.locals) =
    finish(init, result.init.source, graph.emptyNode)
  (result.deinit.code, result.deinit.locals) =
    finish(deinit, result.deinit.source, graph.emptyNode)

# ----- dynlib handling -----

proc genLoadLib(bu: var MirBuilder, graph: ModuleGraph, env: var MirEnv,
                loc, name: Value): Value =
  ## Emits the MIR code for ``loc = nimLoadLibrary(name); loc.isNil``.
  let loadLib = graph.getCompilerProc("nimLoadLibrary")

  bu.subTree MirNode(kind: mnkAsgn):
    bu.use loc
    bu.buildCall env.procedures.add(loadLib), loadLib.typ[0]:
      bu.emitByVal name

  bu.wrapTemp(graph.getSysType(unknownLineInfo, tyBool)):
    bu.buildMagicCall mIsNil, graph.getSysType(unknownLineInfo, tyBool):
      bu.emitByVal loc

proc genLibSetup(graph: ModuleGraph, env: var MirEnv, conf: BackendConfig,
                 libVar: GlobalId, path: PNode,
                 bu: var MirBuilder, source: var SourceMap) =
  ## Emits the MIR code for loading a dynamic library to `dest`, with `name`
  ## being the symbol of the location that stores the handle and `path` the
  ## expression used with the ``.dynlib`` pragma.
  let
    errorProc = graph.getCompilerProc("nimLoadLibraryError")
    voidTyp   = graph.getSysType(path.info, tyVoid)
    val       = toValue(libVar, env[libVar].typ)

  if path.kind in nkStrKinds:
    # the library name is known at compile-time
    var candidates: seq[string]
    libCandidates(path.strVal, candidates)

    let outer = LabelId(1) # labels are 1-based

    # generate an 'or' chain that tries every candidate until one is found
    # for which loading succeeds
    bu.subTree MirNode(kind: mnkBlock, label: outer):
      bu.add MirNode(kind: mnkStmtList) # manual, for less visual nesting
      for candidate in candidates.items:
        var tmp = genLoadLib(bu, graph, env, val):
          literal(env.getOrIncl(candidate),
                  graph.getSysType(path.info, tyString))

        tmp = bu.wrapTemp(graph.getSysType(path.info, tyBool)):
          bu.buildMagicCall mNot, graph.getSysType(path.info, tyBool):
            bu.emitByVal tmp

        bu.subTree mnkIf:
          bu.use tmp
          bu.add MirNode(kind: mnkBreak, label: outer)

      # if none of the candidates worked, a run-time error is reported:
      bu.subTree mnkVoid:
        bu.buildCall env.procedures.add(errorProc), voidTyp:
          bu.emitByVal literal(env.getOrIncl(path.strVal), path.typ)
      bu.add endNode(mnkStmtList)
  else:
    # the name of the dynamic library to load the procedure from is only known
    # at run-time
    let
      strType = graph.getSysType(path.info, tyString)

    let nameTemp = bu.allocTemp(strType)
    bu.buildStmt mnkDef:
      bu.use nameTemp
      generateCode(graph, env, conf.tconfig, path, bu, source)

    let cond = genLoadLib(bu, graph, env, val, nameTemp)
    bu.subTree mnkIf:
      bu.use cond
      bu.subTree mnkVoid:
        bu.buildCall env.procedures.add(errorProc), voidTyp:
          bu.emitByVal nameTemp

proc produceLoader(graph: ModuleGraph, m: Module, data: var DiscoveryData,
                   env: var MirEnv, conf: BackendConfig, sym: PSym): MirBody =
  ## Produces a MIR fragment with the load-at-run-time logic for procedure/
  ## variable `sym`. If not generated already, the loading logic for the
  ## necessary dynamic library is emitted into the fragment and the global
  ## storing the library handle is registered with `env`.
  let
    lib      = graph.getLib(sym.annex)
    loadProc = graph.getCompilerProc("nimGetProcAddr")
    path     = transformExpr(graph, m.idgen, m.sym, lib.path)
    extname  = newStrNode(nkStrLit, sym.extname)
    voidTyp  = graph.getSysType(path.info, tyVoid)

  extname.typ = graph.getSysType(lib.path.info, tyCstring)

  var bu = initBuilder(result.source.add(path))
  discard bu.addLocal(Local()) # empty result slot

  let dest =
    if sym.kind in routineKinds:
      toValue(env.procedures[sym], sym.typ)
    else:
      toValue(env.globals[sym], sym.typ)

  # the scope makes sure that locals are destroyed once loading the
  # procedure has finished
  bu.add MirNode(kind: mnkScope)

  if path.kind in nkCallKinds and path.typ != nil and
     path.typ.kind in {tyPointer, tyProc}:
    # a call expression for loading the procedure
    path[^1] = extname # update to the correct name
    # XXX: ^^ maybe sem should do this instead...

    let tmp = bu.allocTemp(dest.typ)
    bu.buildStmt mnkDef:
      bu.use tmp
      generateCode(graph, env, conf.tconfig, path, bu, result.source)
    bu.subTree mnkVoid:
      bu.buildMagicCall mAsgnDynlibVar, voidTyp:
        bu.emitByName(dest, ekReassign)
        bu.emitByVal(tmp)
  else:
    # the imported procedure is identified by the symbol's external name and
    # the built-in proc loading logic is to be used
    let
      isNew = lib.name in env.globals
      libVar = env.globals.add(lib.name)

    if not isNew:
      # the library hasn't been loaded yet
      genLibSetup(graph, env, conf, libVar, path, bu, result.source)
      if path.kind in nkStrKinds: # only register statically-known dependencies
        data.libs.add sym.annex

    # generate the code for ``sym = cast[typ](nimGetProcAddr(lib, extname))``
    let tmp = bu.wrapTemp(loadProc.typ[0]):
      bu.buildCall env.procedures.add(loadProc), loadProc.typ[0]:
        bu.emitByVal toValue(libVar, lib.name.typ)
        bu.emitByVal literal(env.getOrIncl(extname.strVal), extname.typ)

    bu.subTree mnkVoid:
      bu.buildMagicCall mAsgnDynlibVar, voidTyp:
        bu.emitByName(dest, ekReassign)
        bu.emitByVal tmp

  bu.add endNode(mnkScope)
  (result.code, result.locals) = finish(bu, result.locals)

# ----- discovery and queueing logic -----

func discoverFrom*(env: var MirEnv, decl: PNode) =
  ## Updates `env` with all not-yet-seen entities from the list of declarative
  ## statements (`decl`).
  if decl.kind == nkEmpty:
    return # nothing to do

  assert decl.kind == nkStmtList
  for n in decl.items:
    case n.kind
    of nkProcDef, nkFuncDef, nkConverterDef, nkMethodDef:
      let prc = n[namePos].sym
      if {sfExportc, sfCompilerProc} * prc.flags == {sfExportc} or
         (sfExportc in prc.flags and exfExportLib in prc.extFlags):
        # an exported routine. It must always have code generated for it. Note
        # that compilerprocs, while exported, still only have code generated
        # for them when used
        discard env.procedures.add(prc)
    of nkIteratorDef:
      discard "ignore; cannot be exported"
    of nkConstSection:
      # scan the section for exported constants:
      for it in n.items:
        if it.kind == nkConstDef:
          let s = it[0].sym
          if {sfExportc, sfCompilerProc} * s.flags == {sfExportc}:
            discard env.constants.add(s)

    of nkTypeSection:
      discard
    else:
      unreachable(n.kind)

func queue(queue: var WorkQueue, id: ProcedureId, prc: PSym, m: FileIndex) =
  ## If eligible for processing and code generation, adds `prc` to
  ## `queue`'s queue.
  assert prc.kind in routineKinds
  if sfForward notin prc.flags and
     exfNoDecl notin prc.extFlags and
     (sfImportc notin prc.flags or
      exfDynamicLib in prc.extFlags or (queue.config.noImported and
                                        prc.ast[bodyPos].kind != nkEmpty)):
    queue.append(m, WorkItem(kind: wikPreprocess, raw: id))

iterator flush(queue: var WorkQueue, env: var MirEnv,
               data: var DiscoveryData, origin: FileIndex): BackendEvent =
  ## Commits to all unprocessed entities in `env`. This means:
  ## * for procedures to queue them for translation/code-generation
  ## * for named constants to queue them for translation/code-generation
  ##
  ## In addition, a ``bekDiscovered`` event is raised (i.e., returned) for
  ## every commited-to entity.
  let next = checkpoint(env)

  template event(e): untyped =
    BackendEvent(module: origin, kind: bekDiscovered, entity: e)

  for id, it in since(env.procedures, data.progress.procs):
    # report the procedure before queuing it
    yield event(MirNode(kind: mnkProc, prc: id))
    queue(queue, id, it, origin)

  for id, _ in since(env.constants, data.progress.consts):
    yield event(MirNode(kind: mnkConst, cnst: id))
    # constants are translated and reported *before* the finished procedure
    # they were reported as part of is
    queue.prepend(origin, WorkItem(kind: wikProcessConst, cnst: id))

  for id, _ in since(env.globals, data.progress.globals):
    yield event(MirNode(kind: mnkGlobal, global: id))

  assert next == checkpoint(env), "the environment was modified"
  # move the progress cursor:
  data.progress = next

# ----- ``process`` iterator implementation -----

proc isTrivialProc(graph: ModuleGraph, prc: PSym): bool {.inline.} =
  getBody(graph, prc).kind == nkEmpty

proc translateConst(env: var MirEnv, id: ConstId, c: PSym) =
  ## Translates the content of the const `c` to its MIR form and links
  ## the result with `id` in the `env`ironment.
  let tree = constDataToMir(env, astdef(c))
  env.setData(id, env.data.getOrPut(tree))

proc pushProgress(queue: var WorkQueue, env: var MirEnv, graph: ModuleGraph,
                  idgen: IdGenerator, prc: PSym, frag: sink MirBody,
                  m: FileIndex) =
  ## Runs `frag` through MIR processing and, if `frag` is not empty, queues
  ## the step for reporting the progress.
  if not isEmpty(frag):
    let id = env.procedures.add(prc)
    process(frag, prc, graph, idgen, env)
    # get the fragment out as soon as possible (hence ``prepend``):
    queue.prepend(m, WorkItem(kind: wikReport, evt: bekPartial,
                              fragId: id, frag: frag))

    # mark the procedure as non-empty:
    if prc.ast[bodyPos].kind == nkEmpty:
      prc.ast[bodyPos] = newNode(nkStmtList)

func postActions(queue: var WorkQueue, discovery: var DiscoveryData,
                 env: var MirEnv, m: FileIndex) =
  ## Queues the procedures registered with `env` by the event handler. These
  ## are generaly referred to as "late dependencies".
  for id, it in since(env.procedures, discovery.progress.procs):
    let m = discovery.overrides.getOrDefault(id, m)
    queue(queue, id, env.procedures[id], m)

  # no need to keep the overrides around, all procedures they applied to are
  # now queued
  discovery.overrides.clear()
  discovery.progress = checkpoint(env)

iterator process*(graph: ModuleGraph, modules: var ModuleList,
                  env: var MirEnv, discovery: var DiscoveryData,
                  conf: BackendConfig): BackendEvent =
  ## Implements discovery of alive entities (procedures, globals, constants,
  ## etc.) and applying the various transformations and MIR passes to
  ## the alive procedures. Progress is reported by returning an event (refer
  ## to `BackendEventKind <#BackendEventKind>`_ for more informations about the
  ## events).
  ##
  ## The iterator is complex and contains multiple yield statements, so it's
  ## advised to implement ``BackendEvent`` processing with a dedicated
  ## procedure.
  ##
  ## At the callsite, no new entities must be registered with `discovery`
  ## during processing of a ``bekDiscovered`` event.
  var
    queue = WorkQueue(config: conf)

  # future direction: both the registered-from tracking and the support
  # for late dependencies are fundamentally workarounds. They can and
  # should be removed once:
  # 1. procedure inlining is managed at the |NimSkull| side
  # 2. the code generators do not raise new dependencies

  # first, generate the method dispatchers, so that we can treat the
  # dispatchers as normal procedures:
  generateMethodDispatchers(graph)

  # mark all procedures that require incremental code generation as forwarded,
  # so that they're not queued for normal code generation
  for _, m in modules.modules.pairs:
    for it in [m.preInit, m.postDestructor, m.dynlibInit]:
      it.flags.incl sfForward

  discovery.progress = checkpoint(env)
  # remember where the globals start, it's needed for producing the dynlib
  # loaders later:
  let start = discovery.progress.globals

  # discover and register the initial entities of each module:
  for id, m in modules.modules.pairs:
    discoverFrom(env, m.decls)

    if not isTrivialProc(graph, m.init):
      discard env.procedures.add(m.init)

    if not isTrivialProc(graph, m.destructor):
      discard env.procedures.add(m.destructor)

    # register the globals and threadvars:
    for s in m.structs.globals.items:
      discard env.globals.add(s)

    for s in m.structs.nestedGlobals.items:
      discard env.globals.add(s)

    for s in m.structs.threadvars.items:
      # threadvars are part of the globals:
      discard env.globals.add(s)

    # inform the caller that the initial set of alive entities became
    # available:
    for evt in flush(queue, env, discovery, id):
      yield evt
    yield BackendEvent(module: id, kind: bekModule)
    postActions(queue, discovery, env, id)

  template reportBody(prc: ProcedureId, m: FileIndex, evt: BackendEventKind,
                      frag: MirBody) =
    ## Reports a procedure-related event (by yielding it).
    yield BackendEvent(module: m, kind: evt, id: prc, sym: env[prc],
                       body: frag)
    postActions(queue, discovery, env, m)

  template pushProgress(prc: PSym, frag: MirBody, m: FileIndex) =
    pushProgress(queue, env, graph, modules[m].idgen, prc, frag, m)

  # generate the importing logic for all known dynlib globals:
  for _, it in since(env.globals, start):
    # XXX: `env.globals` is mutated during the loop. While not a problem at
    #      the moment, this should eventually be fixed
    if exfDynamicLib in it.extFlags:
      let module = moduleId(it).FileIndex
      var frag = produceLoader(graph, modules[module], discovery, env, conf,
                               it)
      pushProgress(modules[module].dynlibInit, frag, module)

  # let the entities discovered while producing the loaders "bleed" over
  # XXX: neither clean nor correct (the registered-from module will be wrong),
  #      but it's the most simple solution. Dynlib loader fragments don't
  #      reference inline procedures, so this is also only technically wrong

  # drain the queue until there's nothing left to do. This makes up the main
  # processing
  while queue.items.len > 0:
    var (item, module) = queue.items.popFirst()

    case item.kind
    of wikPreprocess:
      let id = item.raw
      preprocess(queue, graph, modules[moduleId(env[id]).FileIndex].idgen,
                 env, id, module)
      continue # the environment was not modified, skip the scan
    of wikProcess:
      let
        origin = moduleId(env[item.prc]).FileIndex # the attched-to module
        frag = translate(item.prc, item.body, graph, conf,
                         modules[origin].idgen, env)

      if env[item.prc].typ.callConv != ccInline:
        # non-inline procedure are registered as coming from the module
        # they're attached to
        module = origin

      # save resources: if there are no new entities to report, report the body
      # directly
      if discovery.progress == checkpoint(env):
        reportBody(item.prc, module, bekProcedure, frag)
      else:
        queue.prepend(module, WorkItem(kind: wikReport, evt: bekProcedure,
                                       fragId: item.prc, frag: frag))
    of wikProcessConst:
      module = moduleId(env[item.cnst]).FileIndex
      translateConst(env, item.cnst, env[item.cnst])
      # we cannot report (i.e., yield) right away, the discovered dependencies
      # have to be reported first
      queue.prepend(module, WorkItem(kind: wikReportConst, cnst: item.cnst))
    of wikProcessGlobals:
      # produce the init/de-init code for the lifted globals:
      let (init, deinit) =
        produceFragmentsForGlobals(env, item.globals, graph, conf.tconfig)

      pushProgress(modules[module].preInit, init, module)
      pushProgress(modules[module].postDestructor, deinit, module)
    of wikImported:
      let id = item.imported
      # the procedure is always reported from the module its attached to
      module = moduleId(env[id]).FileIndex
      # first report that an imported procedure became available...
      yield BackendEvent(module: module, kind: bekImported, id: id,
                         sym: env[id])
      postActions(queue, discovery, env, module)

      # ... then produce the loader code
      let frag = produceLoader(graph, modules[module], discovery, env, conf,
                               env[id])
      pushProgress(modules[module].dynlibInit, frag, module)
    of wikReport:
      reportBody(item.fragId, module, item.evt, item.frag)
    of wikReportConst:
      yield BackendEvent(module: module, kind: bekConstant, cnst: item.cnst)
      postActions(queue, discovery, env, module)

    # report and queue all discovered dependencies:
    for evt in flush(queue, env, discovery, module):
      yield evt

  # unmark all completed incremental procedures:
  for _, m in modules.modules.pairs:
    for it in [m.preInit, m.postDestructor, m.dynlibInit]:
      if not isTrivialProc(graph, it):
        it.flags.excl sfForward

# ----- API for interacting with ``DiscoveryData`` -----

func setModuleOverride*(discovery: var DiscoveryData, id: ProcedureId,
                        module: FileIndex) =
  ## Overrides which module the procedure identified by `id` will be reported
  ## as having been first seen with. This only works with procedures that
  ## haven't been queued for code generation yet. It's also fundamentally a
  ## workaround, try to use it as little as possible.
  discovery.overrides[id] = module

# ----- routines for manual implementation of the backend processing -----

iterator discover*(env: var MirEnv, progress: EnvCheckpoint
                  ): tuple[s: PSym, n: MirNode] =
  ## Returns all entities - in an unspecified order - added to `env` since the
  ## `progress` checkpoint was created. Procedures referenced from constants
  ## also added to the environment and returned.
  ##
  ## This iterator is meant for a manual implementation of the backend
  ## processing -- don't use it in conjunction with the ``process`` iterator.
  for id, it in since(env.constants, progress.consts):
    # first discover and report the procedures referenced by the constant's
    # data. This ensures that the callsite can rely on all the constant's
    # dependencies existing in the environment
    translateConst(env, id, it)
    yield (it, MirNode(kind: mnkConst, cnst: id))

  for id, it in since(env.procedures, progress.procs):
    yield (it, MirNode(kind: mnkProc, prc: id))

  for id, it in since(env.globals, progress.globals):
    yield (it, MirNode(kind: mnkGlobal, global: id))