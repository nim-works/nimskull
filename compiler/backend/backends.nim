## Shared processing logic used by all backends.

import
  std/[
    deques,
    intsets
  ],
  compiler/ast/[
    ast,
    idents,
    lineinfos
  ],
  compiler/backend/[
    cgmeth
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    astgen,
    mirbridge,
    mirgen,
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

    additional: seq[tuple[m: FileIndex, prc: PSym]]
      # HACK: see documentation of the procedure that appends to this list

  EventContext = object
    ## The data that guides the post-event-processing actions.
    start: int
      ## the procedure progress indicator at the start of the current cycle
    preIndirect: int
      ## the procedure progress indicator before indirect dependencies were
      ## discovered
    fin: int
      ## the procedure progress indicator prior to event processing

    indirect: seq[tuple[m: FileIndex, index: int]]
      ## the list of all indirect procedure dependencies that need to be
      ## queued (stored as indices into the procedure list)

  BackendEventKind* = enum
    bekModule    ## the initial set of alive entities for a module was
                 ## discovered
    bekPartial   ## a fragment of a procedure that's generated incrementally
                 ## became available
    bekProcedure ## a complete procedure was processed and transformed

  BackendEvent* = object
    ## Progress event returned by the ``process`` iterator.
    module*: FileIndex
      ## the module in whose context the processing happens. For actions
      ## related to .inline procedures, this is not necessarily the module
      ## the symbol is attached to

    case kind*: BackendEventKind
    of bekModule:
      discard
    of bekPartial, bekProcedure:
      sym*: PSym
        ## the symbol of the procedure the event is about
      body*: MirFragment

template add[T](x: Deque[T], elem: T) =
  ## Convenience template for appending to a deque.
  x.addLast elem

func append*(dest: var MirFragment, src: sink MirFragment) =
  ## Appends the code from `src` to `dest`. No check whether the resulting
  ## code is semantically valid is performed
  dest.tree.add src.tree
  dest.source.append src.source

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

func rewind[T](q: var Queue[T], pos: int): int =
  assert pos <= q.progress
  result = q.progress
  q.progress = pos

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
      # make sure to not process the entity inside a 'def'
      i = sibling(tree, i)
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
  rewriteGlobalDefs(prc.body.tree, prc.body.source, outermost = true)

  if shouldInjectDestructorCalls(prc.sym):
    injectDestructorCalls(graph, idgen, prc.sym,
                          prc.body.tree, prc.body.source)

  rewriteGlobalDefs(prc.body.tree, prc.body.source, outermost = false)
  # XXX: ^^ this is a hack. See the documentation of the routine for more
  #      details

proc process(body: var MirFragment, ctx: PSym, graph: ModuleGraph,
             idgen: IdGenerator) =
  ## Applies all applicable MIR passes to the fragment `body`. `ctx`
  ## represents the procedure in whose context the processing happens, and
  ## is used for the purpose of error reporting and debug tracing.
  injectDestructorCalls(graph, idgen, ctx, body.tree, body.source)

proc generateAST*(graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                  code: sink MirFragment): PNode =
  ## Translates the MIR code provided by `code` into ``PNode`` AST and,
  ## if enabled, echoes the result.
  result = generateAST(graph, idgen, owner, code.tree, code.source)
  echoOutput(graph.config, owner, result)

# ------- handling of lifted globals ---------

func add(frag: var MirFragment, origin: PNode, node: sink MirNode) =
  assert frag.tree.len == frag.source.map.len
  frag.source.map.add(frag.source.source.add(origin))
  frag.tree.add node

proc updateWithSource(f: var MirFragment, origin: PNode) =
  ## Sets `origin` as the source node of all MIR nodes that have no source
  ## information associated yet.
  # TODO: replace this with using the routines from ``mirgen``, once they're
  #       sufficiently generalized
  let
    id = f.source.source.add(origin)
    start = f.source.map.len
  f.source.map.setLen(f.tree.len)
  for i in start..<f.tree.len:
    f.source.map[i] = id

proc produceFragmentsForGlobals(data: var DiscoveryData, identdefs: seq[PNode],
                                graph: ModuleGraph,
                                options: set[GenOption]): tuple[init, deinit: MirFragment] =
  ## Given a list of identdefs of lifted globals, produces the MIR code for
  ## initialzing and deinitializing the globals. `data` is updated with
  ## not-yet-seen globals, and is at the same time used for discarding
  ## the identdefs for globals that were already processed.

  func prepare(f: var MirFragment, n: PNode) {.nimcall.} =
    # the fragments need to be wrapped in scopes; some MIR passes depend
    # on this
    if f.tree.len == 0:
      f.add(n): MirNode(kind: mnkScope)

  func finish(f: var MirFragment, n: PNode) {.nimcall.} =
    if f.tree.len > 0:
      f.add(n): MirNode(kind: mnkEnd, start: mnkScope)

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
      block:
        template r: MirFragment = result.init

        prepare(r, graph.emptyNode)
        r.add(it): MirNode(kind: mnkArgBlock)
        r.add(it[0]): MirNode(kind: mnkGlobal, sym: s, typ: s.typ)
        r.add(it[0]): MirNode(kind: mnkTag, typ: s.typ)
        r.add(it[0]): MirNode(kind: mnkName, typ: s.typ)
        if it[2].kind == nkEmpty:
          # no explicit initializer expression means that the default value
          # should be used
          # XXX: ^^ it'd make sense to instead let semantic analysis ensure this
          #      (i.e. by placing a ``default(T)`` in the initializer slot)
          r.add(it[2]): MirNode(kind: mnkArgBlock)
          r.add(it[2]): MirNode(kind: mnkEnd, start: mnkArgBlock)
          r.add(it[2]): MirNode(kind: mnkMagic, magic: mDefault, typ: s.typ)
        else:
          generateCode(graph, options, it[2], r.tree, r.source)

        r.add(it[2]): MirNode(kind: mnkConsume, typ: s.typ)
        r.add(it): MirNode(kind: mnkEnd, start: mnkArgBlock)
        r.add(it): MirNode(kind: mnkInit)

      # if the global requires one, emit a destructor call into the deinit
      # fragment:
      if hasDestructor(s.typ):
        prepare(result.deinit, graph.emptyNode)
        genDestroy(result.deinit.tree, graph, s.typ):
          MirNode(kind: mnkGlobal, sym: s, typ: s.typ)
        updateWithSource(result.deinit, it[0])

  finish(result.init, graph.emptyNode)
  finish(result.deinit, graph.emptyNode)

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
     (sfImportc notin prc.flags or (iter.config.noImported and
                                    prc.ast[bodyPos].kind != nkEmpty)):
    iter.queued.add (prc, m)

func queueAll(iter: var ProcedureIter, data: var DiscoveryData,
              origin: FileIndex) =
  ## Queues all newly discovered procedures and marks the now queued ones as
  ## processed/read.
  for _, it in visit(data.procedures):
    queue(iter, it, origin)

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

proc preprocessDynlib(graph: ModuleGraph, idgen: IdGenerator,
                      sym: PSym, deps: var seq[PSym]) =
  # HACK: so that procedures and unexpanded constants used in the
  #       expressions passed to `.dylib` pragmas are discovered, we
  #       translate the expression to MIR code, scan it, and then
  #       translate it back to AST and update the symbol with it. This is
  #       horrendous, but fortunately, this hack (`preprocessDynlib``) can
  #       be removed once handling of dynlib procedures and globals is fully
  #       implemented in the ``process`` iterator
  if exfDynamicLib in sym.extFlags:
    let lib = addr graph.getLib(sym.annex)
    if lib.path.kind in nkStrKinds:
      # it's a string, no need to transform nor scan it
      discard
    else:
      # XXX: the logic here ignores a large amount of things
      #      (options, proper owner symbol, etc.)...
      var
        t: MirTree
        m: SourceMap

      generateCode(graph, {}, lib.path, t, m)
      for dep in deps(t, {}): # just ignore magics here
        if dep.kind in routineKinds + {skConst}:
          deps.add dep

      lib.path = generateAST(graph, idgen, sym.owner, t, m)

proc preprocessDynlib(graph: ModuleGraph, mlist: ModuleList,
                      data: var DiscoveryData) =
  # XXX: remove this procedure once ``process`` is fully responsible for
  #      .dynlib handling
  var deps: seq[PSym]
  for _, it in peek(data.procedures):
    preprocessDynlib(graph, mlist[moduleId(it).FileIndex].idgen, it, deps)

  for it in deps.items:
    if it.kind in routineKinds:
      register(data, procedures, it)
    else:
      register(data, constants, it)

func processConstants(data: var DiscoveryData): seq[(FileIndex, int)] =
  ## Registers with `data` the procedures used by all un-processed constants
  ## and marks the constants as processed.
  assert data.procedures.isProcessed
  for _, s in peek(data.constants):
    discoverFromValueAst(data, astdef(s))
    # the procedure needs to be queued from the context of the module `s` is
    # attached to:
    let m = moduleId(s).FileIndex
    for i, _ in visit(data.procedures):
      result.add (m, i)

func processAdditional(iter: var ProcedureIter, data: var DiscoveryData) =
  ## Queues all extra dependencies registered with `data`.
  for m, s in data.additional.items:
    if not data.seen.containsOrIncl(s.id):
      data.procedures.addProcessed(s)
      queue(iter, s, m)

  data.additional.setLen(0) # we've processed everything; reset

func preActions(discovery: var DiscoveryData): EventContext =
  ## Performs the actions required before emitting a ``BackendEvent``. New
  ## entities need to be discovered, but they must not be queued for
  ## processing yet -- the caller might still want to intercept / pre-process.
  result.start = discovery.procedures.progress
  result.preIndirect = discovery.procedures.markProcessed()
  # discover the procedures referenced by the new constants:
  result.indirect = processConstants(discovery)

  # rewind the progress indicator so that all procedures marked as
  # processed as part of this cycle appear as unprocessed again:
  result.fin = discovery.procedures.rewind(result.start)

func postActions(iter: var ProcedureIter, discovery: var DiscoveryData,
                 m: FileIndex, ctx: sink EventContext) =
  ## Queues for processing all procedures that were discovered (by both
  ## ``progress`` and its caller) during the current event cycle.

  # make sure that all new constants discovered as part of this cycle are
  # marked as processed:
  discard markProcessed(discovery.constants)

  # now comes the complex part: all new procedures discovered as part of this
  # cycle need to be queued for processing

  # first come the direct procedure dependencies. They're associated with the
  # provided module:
  for i in ctx.start..<ctx.preIndirect:
    queue(iter, discovery.procedures.data[i], m)

  # then come the indirect procedure dependencies:
  for moduleId, i in ctx.indirect.items:
    queue(iter, discovery.procedures.data[i], moduleId)

  # set the progress indicator to where it was prior to event processing:
  discovery.procedures.progress = ctx.fin

  # finally, queue all late dependencies raised by the event processor:
  queueAll(iter, discovery, m)
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
  ## When control is passed to the caller, the `discovery.procedures` and
  ## `discovery.constants` queues contain all new procedures and constants
  ## that were discovered directly or indirectly for the entity that the
  ## returned event is about. The `globals` and `threadvars` queues, while
  ## possibly filled with new content, are not "drained" yet, and have the
  ## same "read" position that the caller left them with.
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

    preprocessDynlib(graph, modules, discovery)

    let ctx = preActions(discovery)
    # inform the caller that the initial set of alive entities became
    # available:
    yield BackendEvent(module: id, kind: bekModule)
    postActions(iter, discovery, id, ctx)

  template reportBody(prc: PSym, m: FileIndex, evt: BackendEventKind,
                      frag: MirFragment) =
    ## Reports (i.e., yields an event) a procedure-related event.
    discoverFrom(discovery, noMagics, frag.tree)

    preprocessDynlib(graph, modules, discovery)

    let work = preActions(discovery)
    yield BackendEvent(module: m, kind: evt, sym: prc, body: frag)
    postActions(iter, discovery, m, work)

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

        template reportProgress(prc: PSym, frag: MirFragment) =
          ## Applies the relevant passes, and notifies the caller about the
          ## fragments.
          if not isEmpty(frag):
            process(frag, prc, graph, modules[module].idgen)
            reportBody(prc, module, bekPartial, frag)

            # mark the procedure as non-empty:
            if prc.ast[bodyPos].kind == nkEmpty:
              prc.ast[bodyPos] = newNode(nkStmtList)

        reportProgress(modules[module].preInit, init)
        reportProgress(modules[module].postDestructor, deinit)

      # for inline procedures, use the context of the module the procedure
      # was first seen in
      let id =
        if prc.sym.typ.callConv == ccInline: origin
        else:                                module

      reportBody(prc.sym, id, bekProcedure, prc.body)
    of true:
      # a procedure imported at run-time (i.e. a .dynlib procedure)
      discard "still managed by the code generator"

# ----- API for interacting with ``DiscoveryData`` -----

func register*(data: var DiscoveryData, prc: PSym) =
  ## If not already know to `data`, adds the procedure `prc` to the list
  ## of known procedures.
  register(data, procedures, prc)

func registerLate*(discovery: var DiscoveryData, prc: PSym, module: FileIndex) =
  ## Registers a late late-dependency with `data`. These are dependencies
  ## that were raised while processing some code fragment, but that are not
  ## directly related to said fragment. They should be kept to a minimum, and
  ## `register <#register,DiscoveryData,PSym>`_ should be preferred whenever
  ## possible.
  discovery.additional.add (module, prc)
