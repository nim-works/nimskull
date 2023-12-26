## Shared processing logic used by all backends.

import
  std/[
    deques,
    dynlib, # for computing possible candidate names
    intsets
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
    options
  ],
  compiler/mir/[
    mirbridge,
    mirconstr,
    mirgen,
    mirpasses,
    mirtrees,
    sourcemaps
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

type
  MirFragment* = object
    tree*: MirTree
    source*: SourceMap

  Procedure* = object
    sym*: PSym
    case isImported*: bool ## wether it's a .dynlib procedure
    of false:
      body*: MirFragment
        ## the procedure's body

      globals*: seq[PNode]
        ## the unprocessed identdefs of globals defined as part of the
        ## procedure's body. Due to how ``transf`` handles inlining, this
        ## list can contain duplicates
    of true:
      discard

  BackendConfig* = object
    ## Configuration state altering the operation of the ``process``
    ## iterator.
    options*: set[GenOption]
      ## passed on to ``mirgen``. See ``mirgen.generateCode`` for more
      ## details
    noImported*: bool
      ## if ``true``, indicates that a procedure with a body should not be
      ## treated as imported, even if it's marked as such

  ProcedureIter = object
    queued: Deque[tuple[prc: PSym, fr: FileIndex]]
      ## procedures that are queued for code-generation
    config: BackendConfig

  Queue*[T] = object
    ## Combines a sequence of items with a "read" cursor.
    data: seq[T]
    progress: int
      ## remembers the position until which the items have already
      ## been processed

  DiscoveryData* = object
    ## Bundles all data needed during the disovery of alive, backend-relevant
    ## entities.
    # XXX: this type does too much. It acts as both a symbol table and
    #      communication interface. Eventually, it should be split up.
    seen: IntSet
      ## remembers the symbol ID of all discovered entities

    procedures*: Queue[PSym]
    constants*: Queue[PSym]
    globals*: Queue[PSym]
    threadvars*: Queue[PSym]

    libs*: seq[LibId]
      ## all dynamic libraries that the alive graph depends on

    additional: seq[tuple[m: FileIndex, prc: PSym]]
      # HACK: see documentation of the procedure that appends to this list

  BackendEventKind* = enum
    bekDiscovered## new entities were discovered during MIR processing. This
                 ## event is meant to be used for pre-processing of symbols
                 ## and registration with the code generators, but no new
                 ## entities must be raised during processing of this event
    bekModule    ## the initial set of alive entities for a module was
                 ## discovered
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
    of bekDiscovered, bekModule:
      discard
    of bekPartial, bekProcedure, bekImported:
      sym*: PSym
        ## the symbol of the procedure the event is about
      body*: MirFragment

template add[T](x: Deque[T], elem: T) =
  ## Convenience template for appending to a deque.
  x.addLast elem

func append*(dest: var MirFragment, src: sink MirFragment) =
  ## Appends the code from `src` to `dest`. No check whether the resulting
  ## code is semantically valid is performed
  dest.source.merge(src.tree, src.source)
  dest.tree.add src.tree

# ----- private and public API for ``Queue`` -----

iterator visit*[T](q: var Queue[T]): (int, lent T) =
  ## Returns all unread items from `q` together with their index, and marks
  ## them the items as read (or processed).
  while q.progress < q.data.len:
    yield (q.progress, q.data[q.progress])
    inc q.progress

iterator peek*[T](q: Queue[T]): (int, lent T) =
  ## Returns all unread items from `q` together with their index, but doesn't
  ## mark them as read.
  for i in q.progress..<q.data.len:
    yield (i, q.data[i])

iterator all*[T](q: Queue[T]): (int, lent T) =
  ## Returns *all* items (regardless of whether their read or unread) from
  ## `q` together with their index.
  for i in 0..<q.data.len:
    yield (i, q.data[i])

func len*[T](q: Queue[T]): int =
  q.data.len

func isProcessed*[T](q: Queue[T]): bool =
  q.progress == q.data.len

func add[T](q: var Queue[T], item: sink T) =
  q.data.add item

func addProcessed[T](q: var Queue[T], item: sink T) =
  assert q.progress == q.data.len
  q.data.add item
  inc q.progress

func markProcessed[T](q: var Queue[T]): int =
  q.progress = q.data.len
  result = q.progress

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

func isEmpty*(f: MirFragment): bool {.inline.} =
  isEmpty(f.tree)

iterator deps*(tree: MirTree; includeMagics: set[TMagic]): PSym =
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
    of mnkProc:
      # XXX: `includeMagics` is a workaround. Magics should either lowered
      #      already or encoded as ``mnkMagic`` nodes when reaching here
      if n.sym.magic == mNone or n.sym.magic in includeMagics:
        yield n.sym
    of mnkConst, mnkGlobal:
      yield n.sym
    else:
      discard "nothing to do"

    inc i

# ----- procedure lowering and transformation -----

proc preprocess*(conf: BackendConfig, prc: PSym, graph: ModuleGraph,
                 idgen: IdGenerator): Procedure =
  ## Transforms the body of the given procedure and translates it to MIR code.
  ## No MIR passes are applied yet
  if exfDynamicLib in prc.extFlags:
    # a procedure imported at runtime, it has no body
    return Procedure(sym: prc, isImported: true)

  result = Procedure(sym: prc, isImported: false)

  var body = transformBodyWithCache(graph, idgen, prc)

  # extract the identdefs of lifted globals (which is the first step towards
  # actually lifting them into proper globals) and store them with the
  # result. Do note that this step modifies the potentially cached body.
  extractGlobals(body, result.globals, isNimVm = goIsNimvm in conf.options)

  if optCursorInference in graph.config.options and
      shouldInjectDestructorCalls(prc):
    # TODO: turn cursor inference into a MIR pass and remove this part
    computeCursors(prc, body, graph)

  echoInput(graph.config, prc, body)

  (result.body.tree, result.body.source) =
    generateCode(graph, prc, conf.options, body)

  echoMir(graph.config, prc, result.body.tree)

proc process*(prc: var Procedure, graph: ModuleGraph, idgen: IdGenerator) =
  ## Applies all applicable MIR passes to the procedure `prc`.
  rewriteGlobalDefs(prc.body.tree, prc.body.source)

  if shouldInjectDestructorCalls(prc.sym):
    injectDestructorCalls(graph, idgen, prc.sym,
                          prc.body.tree, prc.body.source)

  let target =
    case graph.config.backend
    of backendC:       targetC
    of backendJs:      targetJs
    of backendNimVm:   targetVm
    of backendInvalid: unreachable()

  applyPasses(prc.body.tree, prc.body.source, prc.sym, graph.config, target)

proc process(body: var MirFragment, ctx: PSym, graph: ModuleGraph,
             idgen: IdGenerator) =
  ## Applies all applicable MIR passes to the fragment `body`. `ctx`
  ## represents the procedure in whose context the processing happens, and
  ## is used for the purpose of error reporting and debug tracing.
  injectDestructorCalls(graph, idgen, ctx, body.tree, body.source)

proc generateIR*(graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                  code: sink MirFragment): Body =
  ## Translates the MIR code provided by `code` into ``CgNode`` IR and,
  ## if enabled, echoes the result.
  result = generateIR(graph, idgen, owner, code.tree, code.source)
  echoOutput(graph.config, owner, result)

# ------- handling of lifted globals ---------

proc produceFragmentsForGlobals(data: var DiscoveryData, identdefs: seq[PNode],
                                graph: ModuleGraph,
                                options: set[GenOption]): tuple[init, deinit: MirFragment] =
  ## Given a list of identdefs of lifted globals, produces the MIR code for
  ## initialzing and deinitializing the globals. `data` is updated with
  ## not-yet-seen globals, and is at the same time used for discarding
  ## the identdefs for globals that were already processed.

  func prepare(bu: var MirBuilder, m: var SourceMap, n: PNode) {.nimcall.} =
    # the fragments need to be wrapped in scopes; some MIR passes depend
    # on this
    if bu.front.len == 0:
      bu.add(m.add(n)): MirNode(kind: mnkScope)

  func finish(bu: sink MirBuilder, m: var SourceMap, n: PNode
             ): MirTree {.nimcall.} =
    if bu.front.len > 0:
      bu.setSource(m.add(n))
      bu.add endNode(mnkScope)
    result = finish(bu)

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
    if not containsOrIncl(data.seen, s.id): # have we seen it yet?
      if sfThread in s.flags:
        data.threadvars.add(s)
      else:
        data.globals.add(s)

      if sfThread in s.flags:
        # threadvars don't support initialization nor destruction, so skip the
        # logic ahead
        continue

      # generate the MIR code for an initializing assignment:
      prepare(init, result.init.source, graph.emptyNode)
      init.setSource(result.init.source.add(it))
      init.buildStmt mnkInit:
        init.setSource(result.init.source.add(it[0]))
        init.use symbol(mnkGlobal, s)
        init.setSource(result.init.source.add(it[2]))
        if it[2].kind == nkEmpty:
          # no explicit initializer expression means that the default value
          # should be used
          # XXX: ^^ it'd make sense to instead let semantic analysis ensure
          #      this (i.e. by placing a ``default(T)`` in the initializer
          #      slot)
          init.buildMagicCall mDefault, s.typ:
            discard
        else:
          generateCode(graph, options, it[2], init, result.init.source)

      # if the global requires one, emit a destructor call into the deinit
      # fragment:
      if hasDestructor(s.typ):
        prepare(deinit, result.deinit.source, graph.emptyNode)
        deinit.setSource(result.deinit.source.add(it[0]))
        genDestroy(deinit, graph, symbol(mnkGlobal, s))

  result.init.tree = finish(init, result.init.source, graph.emptyNode)
  result.deinit.tree = finish(deinit, result.deinit.source, graph.emptyNode)

# ----- dynlib handling -----

proc genLoadLib(graph: ModuleGraph, bu: var MirBuilder,
                loc, name: Value): Value =
  ## Emits the MIR code for ``loc = nimLoadLibrary(name); loc.isNil``.
  let loadLib = graph.getCompilerProc("nimLoadLibrary")

  bu.subTree MirNode(kind: mnkAsgn):
    bu.use loc
    bu.buildCall loadLib, loadLib.typ[0]:
      bu.emitByVal name

  bu.wrapTemp(graph.getSysType(unknownLineInfo, tyBool)):
    bu.buildMagicCall mIsNil, graph.getSysType(unknownLineInfo, tyBool):
      bu.emitByVal loc

proc genLibSetup(graph: ModuleGraph, conf: BackendConfig,
                 name: PSym, path: PNode,
                 bu: var MirBuilder, source: var SourceMap) =
  ## Emits the MIR code for loading a dynamic library to `dest`, with `name`
  ## being the symbol of the location that stores the handle and `path` the
  ## expression used with the ``.dynlib`` pragma.
  let
    errorProc = graph.getCompilerProc("nimLoadLibraryError")
    voidTyp   = graph.getSysType(path.info, tyVoid)
    nameNode  = symbol(mnkGlobal, name)

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
        var tmp = genLoadLib(graph, bu, nameNode):
          literal(newStrNode(nkStrLit, candidate))

        tmp = bu.wrapTemp(graph.getSysType(path.info, tyBool)):
          bu.buildMagicCall mNot, graph.getSysType(path.info, tyBool):
            bu.emitByVal tmp

        bu.subTree mnkIf:
          bu.use tmp
          bu.add MirNode(kind: mnkBreak, label: outer)

      # if none of the candidates worked, a run-time error is reported:
      bu.subTree mnkVoid:
        bu.buildCall errorProc, voidTyp:
          bu.emitByVal literal(path)
      bu.add endNode(mnkStmtList)
  else:
    # the name of the dynamic library to load the procedure from is only known
    # at run-time
    let
      strType = graph.getSysType(path.info, tyString)

    let nameTemp = bu.allocTemp(strType)
    bu.buildStmt mnkDef:
      bu.use nameTemp
      generateCode(graph, conf.options, path, bu, source)

    let cond = genLoadLib(graph, bu, nameNode, nameTemp)
    bu.subTree mnkIf:
      bu.use cond
      bu.subTree mnkVoid:
        bu.buildCall errorProc, voidTyp:
          bu.emitByVal nameTemp

proc produceLoader(graph: ModuleGraph, m: Module, data: var DiscoveryData,
                   conf: BackendConfig, sym: PSym): MirFragment =
  ## Produces a MIR fragment with the load-at-run-time logic for procedure/
  ## variable `sym`. If not generated already, the loading logic for the
  ## necessary dynamic library is emitted into the fragment and the global
  ## storing the library handle registered with `data`.
  let
    lib      = graph.getLib(sym.annex)
    loadProc = graph.getCompilerProc("nimGetProcAddr")
    path     = transformExpr(graph, m.idgen, m.sym, lib.path)
    extname  = newStrNode(nkStrLit, sym.extname)
    voidTyp  = graph.getSysType(path.info, tyVoid)

  extname.typ = graph.getSysType(lib.path.info, tyCstring)

  var bu = initBuilder(result.source.add(path))

  let dest =
    if sym.kind in routineKinds:
      procLit(sym)
    else:
      symbol(mnkGlobal, sym)

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
      generateCode(graph, conf.options, path, bu, result.source)
    bu.subTree mnkVoid:
      bu.buildMagicCall mAsgnDynlibVar, voidTyp:
        bu.emitByName(dest, ekReassign)
        bu.emitByVal(tmp)
  else:
    # the imported procedure is identified by the symbol's external name and
    # the built-in proc loading logic is to be used

    if not data.seen.containsOrIncl(lib.name.id):
      # the library hasn't been loaded yet
      genLibSetup(graph, conf, lib.name, path, bu, result.source)
      if path.kind in nkStrKinds: # only register statically-known dependencies
        data.libs.add sym.annex
      data.globals.add lib.name # register the global

    # generate the code for ``sym = cast[typ](nimGetProcAddr(lib, extname))``
    let tmp = bu.wrapTemp(loadProc.typ[0]):
      bu.buildCall loadProc, loadProc.typ[0]:
        bu.emitByVal symbol(mnkGlobal, lib.name)
        bu.emitByVal literal(extname)

    bu.subTree mnkVoid:
      bu.buildMagicCall mAsgnDynlibVar, voidTyp:
        bu.emitByName(dest, ekReassign)
        bu.emitByVal tmp

  bu.add endNode(mnkScope)
  result.tree = finish(bu)

# ----- discovery and queueing logic -----

func includeIfUnseen(q: var Queue[PSym], marker: var IntSet, sym: PSym) =
  if not marker.containsOrIncl(sym.id):
    q.data.add(sym)

template register(data: var DiscoveryData, queue: untyped, s: PSym) =
  data.queue.includeIfUnseen(data.seen, s)

func discoverFrom*(data: var DiscoveryData, noMagics: set[TMagic], body: MirTree) =
  ## Updates `data` with all not-yet-seen entities (except for globals) that
  ## `body` references.
  for dep in deps(body, noMagics):
    case dep.kind
    of routineKinds:
      register(data, procedures, dep)
    of skConst:
      register(data, constants, dep)
    of skVar, skLet, skForVar:
      discard "a global; ignore"
    else:
      unreachable()

func discoverFrom*(data: var DiscoveryData, decl: PNode) =
  ## Updates `data` with all not-yet-seen entities from the declarative
  ## statement list `decl`.
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
        # that compilerprocs, while exported, are still only have code generated
        # for them when used
        register(data, procedures, prc)
    of nkIteratorDef:
      discard "ignore; cannot be exported"
    of nkConstSection:
      # scan the section for exported constants:
      for it in n.items:
        if it.kind == nkConstDef:
          let s = it[0].sym
          if {sfExportc, sfCompilerProc} * s.flags == {sfExportc}:
            register(data, constants, s)

    of nkTypeSection:
      discard
    else:
      unreachable(n.kind)

func discoverFromValueAst(data: var DiscoveryData, ast: PNode) =
  ## Discover new routines from `ast`, which is an AST representing a value
  ## construction expression.
  case ast.kind
  of nkSym:
    let s = ast.sym
    if s.kind in routineKinds:
      register(data, procedures, s)
  of nkWithSons:
    for n in ast.items:
      discoverFromValueAst(data, n)
  of nkWithoutSons - {nkSym}:
    discard "nothing to do"

func queue(iter: var ProcedureIter, prc: PSym, m: FileIndex) =
  ## If eligible for processing and code generation, adds `prc` to
  ## `iter`'s queue.
  assert prc.kind in routineKinds
  if exfNoDecl notin prc.extFlags and
     (sfImportc notin prc.flags or
      exfDynamicLib in prc.extFlags or (iter.config.noImported and
                                        prc.ast[bodyPos].kind != nkEmpty)):
    iter.queued.add (prc, m)

iterator queueAll(iter: var ProcedureIter, data: var DiscoveryData,
                  origin: FileIndex): FileIndex =
  ## Queues all newly discovered procedures and marks them as processed/read.
  ## When a new batch of procedures becomes available, yields the ID of the
  ## module the procedures were added from, allowing the callsite to pre-
  ## preocess the symbols.
  let start = data.constants.progress
  # notify about all new entities (if there are any)
  yield origin

  # queue the procedures that the callsite was just notified about
  for _, it in visit(data.procedures):
    queue(iter, it, origin)

  # support for the iterator's to not mark constants as processed
  let fin = markProcessed(data.constants)

  # then scan all constants, notify the callsite about new procedures, and
  # queue all new procedures
  for i in start..<fin:
    let
      c = data.constants.data[i]
      m = moduleId(c).FileIndex
    discoverFromValueAst(data, astdef(c))

    if not isProcessed(data.procedures):
      # only yield when there's something to do
      yield m

    # queue the procedures that the callsite was just notified about
    for _, it in visit(data.procedures):
      queue(iter, it, m)

# ----- ``process`` iterator implementation -----

proc isTrivialProc(graph: ModuleGraph, prc: PSym): bool {.inline.} =
  getBody(graph, prc).kind == nkEmpty

proc next(iter: var ProcedureIter, graph: ModuleGraph,
          modules: ModuleList): tuple[origin: FileIndex, prc: Procedure] =
  ## Retrieves and transforms the procedure that is next in the queue.
  let
    (sym, origin) = iter.queued.popFirst()
    idgen         = modules[moduleId(sym).FileIndex].idgen

  result.prc = preprocess(iter.config, sym, graph, idgen)
  result.origin = origin

  if not result.prc.isImported:
    # apply all MIR passes:
    process(result.prc, graph, idgen)

func processAdditional(iter: var ProcedureIter, data: var DiscoveryData) =
  ## Queues all extra dependencies registered with `data`.
  for m, s in data.additional.items:
    if not data.seen.containsOrIncl(s.id):
      data.procedures.addProcessed(s)
      queue(iter, s, m)

  data.additional.setLen(0) # we've processed everything; reset

func postActions(iter: var ProcedureIter, discovery: var DiscoveryData,
                 m: FileIndex) =
  ## Queues for processing all procedures that were discovered during event
  ## processing (i.e., by the iterator's callsite).
  for _, it in visit(discovery.procedures):
    queue(iter, it, m)
  processAdditional(iter, discovery)

iterator process*(graph: ModuleGraph, modules: var ModuleList,
                  discovery: var DiscoveryData, noMagics: set[TMagic],
                  conf: BackendConfig): BackendEvent =
  ## Implements discovery of alive entities (procedures, globals, constants,
  ## etc.) and applying the various transformations and MIR passes to
  ## the alive procedures. Progress is reported by returning an event (refer
  ## to `BackendEventKind <#BackendEventKind>`_ for more informations about the
  ## events).
  ##
  ## `noMagics` is the set of magics that need to be treated as normal
  ## procedure during discovery of alive procedures, and `conf` is additional
  ## configuration that modifies some aspects of the processing.
  ##
  ## The iterator is complex and contains multiple yield statements, so it's
  ## advised to implement ``BackendEvent`` processing with a dedicated
  ## procedure.
  ##
  ## At the callsite, no new entities must be registered with `discovery`
  ## during processing of a ``bekDiscovered`` event.
  var
    iter = ProcedureIter(config: conf)

  # future direction: both the registered-from tracking and the support
  # for late dependencies are fundamentally workarounds. They can and
  # should be removed once:
  # 1. procedure inlining is managed at the |NimSkull| side
  # 2. the code generators do not raise new dependencies

  # first, generate the method dispatchers, so that we can treat the
  # dispatchers as normal procedures:
  generateMethodDispatchers(graph)

  # queue the init procedures:
  for id, m in modules.modules.pairs:
    discoverFrom(discovery, m.decls)

    if not isTrivialProc(graph, m.init):
      register(discovery, procedures, m.init)

    if not isTrivialProc(graph, m.destructor):
      register(discovery, procedures, m.destructor)

    # register the globals and threadvars:
    for s in m.structs.globals.items:
      register(discovery, globals, s)

    for s in m.structs.nestedGlobals.items:
      register(discovery, globals, s)

    for s in m.structs.threadvars.items:
      register(discovery, threadvars, s)

    # inform the caller that the initial set of alive entities became
    # available:
    for m in queueAll(iter, discovery, id):
      yield BackendEvent(module: m, kind: bekDiscovered)
    yield BackendEvent(module: id, kind: bekModule)
    postActions(iter, discovery, id)

  template reportBody(prc: PSym, m: FileIndex, evt: BackendEventKind,
                      frag: MirFragment) =
    ## Reports (i.e., yields an event) a procedure-related event.
    discoverFrom(discovery, noMagics, frag.tree)

    # fire 'discovered' events for new procedures and also queue them for
    # processing
    for m2 in queueAll(iter, discovery, m):
      yield BackendEvent(module: m2, kind: bekDiscovered)

    yield BackendEvent(module: m, kind: evt, sym: prc, body: frag)
    postActions(iter, discovery, m)

  template reportProgress(prc: PSym, frag: MirFragment) =
    ## Applies the relevant passes to the fragment and notifies the caller
    ## about it.
    if not isEmpty(frag):
      process(frag, prc, graph, modules[module].idgen)
      reportBody(prc, module, bekPartial, frag)

      # mark the procedure as non-empty:
      if prc.ast[bodyPos].kind == nkEmpty:
        prc.ast[bodyPos] = newNode(nkStmtList)

  # generate the importing logic for all known dynlib globals:
  for _, it in all(discovery.globals):
    if exfDynamicLib in it.extFlags:
      let module = moduleId(it).FileIndex
      var frag = produceLoader(graph, modules[module], discovery, conf, it)
      reportProgress(modules[module].dynlibInit, frag)

  # process queued procedures until there are none left:
  while iter.queued.len > 0:
    let
      (origin, prc) = next(iter, graph, modules)
      module = moduleId(prc.sym).FileIndex
        ## the module the procedure is *attached* to

    case prc.isImported
    of false:
      block:
        # produce the init/de-init code for the lifted globals:
        var (init, deinit) =
          produceFragmentsForGlobals(discovery, prc.globals, graph,
                                     conf.options)

        reportProgress(modules[module].preInit, init)
        reportProgress(modules[module].postDestructor, deinit)

      # for inline procedures, use the context of the module the procedure
      # was first seen in
      let id =
        if prc.sym.typ.callConv == ccInline: origin
        else:                                module

      reportBody(prc.sym, id, bekProcedure, prc.body)
    of true:
      # a procedure imported at run-time (i.e., a dynlib procedure)
      # first announce the procedure...
      yield BackendEvent(module: module, kind: bekImported, sym: prc.sym)
      postActions(iter, discovery, module)

      # ... then produce and announce the loader fragment:
      var frag = produceLoader(graph, modules[module], discovery, conf, prc.sym)
      reportProgress(modules[module].dynlibInit, frag)

# ----- API for interacting with ``DiscoveryData`` -----

func register*(data: var DiscoveryData, prc: PSym) =
  ## If not already know to `data`, adds the procedure `prc` to the list
  ## of known procedures.
  register(data, procedures, prc)

func registerGlobal*(data: var DiscoveryData, sym: PSym) {.inline.} =
  ## If not already known to `data`, adds the global `sym` to the list of
  ## known globals.
  register(data, globals, sym)

func registerLate*(discovery: var DiscoveryData, prc: PSym, module: FileIndex) =
  ## Registers a late late-dependency with `data`. These are dependencies
  ## that were raised while processing some code fragment, but that are not
  ## directly related to said fragment. They should be kept to a minimum, and
  ## `register <#register,DiscoveryData,PSym>`_ should be preferred whenever
  ## possible.
  discovery.additional.add (module, prc)

func rewind*(data: var DiscoveryData) =
  ## Un-discovers all not-yet-processed procedures, globals, threadvars,
  ## and constants. After the call to ``rewind``, it will appear as if the
  ## dropped entities were never registered with `data`.
  template rewindQueue(q: Queue[PSym]) =
    for _, it in peek(q):
      data.seen.excl(it.id)

    q.data.setLen(q.progress) # remove all unprocessed items

  rewindQueue(data.procedures)
  rewindQueue(data.constants)
  rewindQueue(data.globals)
  rewindQueue(data.threadvars)