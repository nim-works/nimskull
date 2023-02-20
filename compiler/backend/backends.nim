import
  std/[
    deques,
    intsets,
    tables,
  ],
  compiler/ast/[
    ast,
    ast_types,
    astalgo, # for `getModule`,
    idents,
    lineinfos
  ],
  compiler/backend/[
    cgmeth
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/mir/[
    mirchangesets,
    mirgen,
    mirtrees,
    sourcemaps,
    mirbridge # for the echoX procedures
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    injectdestructors,
    passes,
    transf,
    varpartitions
  ],
  compiler/utils/[
    containers,
    idioms
  ]

type
  CodeFragment = object
    tree: MirTree
    sourceMap: SourceMap

  Module* = object
    stmts*: PNode ## the top level statements in the order they were parsed
    sym*: PSym ## module symbol
    idgen*: IdGenerator

    preInitFragment: CodeFragment
      ## the in-flight code fragment of the module's pre-initialization procedure

  ModuleDataExtra = object
    ## Extra data associated with a module
    # XXX: since this is data that's somewhat relevant to all targets, it
    #      might be a good idea to merge ``ModuleDataExtra`` with
    #      ``ModuleData``. The way in which the data for modules is organized
    #      needs an overhaul in general.
    fileIdx: FileIndex
    flags: TSymFlags

  ModuleId* = distinct uint32 ## The ID of a module in the back-end

  ModuleListRef* = ref ModuleList
  ModuleList* = object of RootObj
    modules*: Store[ModuleId, Module]

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
    id: ModuleId

  Procedure* = object
    sym*: PSym
    case isImported*: bool
    of false:
      tree*: MirTree # TODO: rename to `body`
      source*: SourceMap
    of true:
      discard

  ProcessOption* = enum
    ## The processing options are only meant as a temporary solution until the
    ## code generators work similar enough for the options to no longer be
    ## necessary
    poLiftGlobals ## extract the globals defined inside inside procedures

  ProcedureIter* = object
    # general state:
    seen: IntSet
      ## remembers the IDs of procedures that were queued at one point already
    queued: Deque[PSym]
      ## procedures that are queued for code-generation

    queuedInner: seq[Procedure]
      ## pre-processed inner procedures. They take precedence before other
      ## queued procedures, that is, ``queuedInner`` is first drained before
      ## new procedures are processed
    nextInner: int

    generatedDispatchers: bool

    noMagics: set[TMagic] # warning: very large

    # TODO: restructure
    globalDestructors: Changeset

    # configuration state:
    options: set[GenOption]
    processOptions: set[ProcessOption]
    noImported*: bool
      ## if ``true``, indicates that a procedure with a body should not be
      ## treated as imported, even if it's marked as such

proc get(m: ModuleRef): var Module =
  m.list.modules[m.id]

template add*[T](x: Deque[T], elem: T) =
  x.addLast elem

func collect[T](list: var T, s: sink PSym, marker: var IntSet) {.inline.} =
  ## If `s.id` is not present in `marker`, adds `s` to `list` and remember it
  ## in `markers`
  if not marker.containsOrIncl(s.id):
    list.add s

func moduleId(o: PIdObj): int32 {.inline.} =
  ## Returns the ID of the module `o` is *attached* to. Do note that in the
  ## case of generic instantiations, this is not the necessarily the same
  ## module as the one returned by ``getModule(o)``
  o.itemId.module

func lookupModule*(mlist: ModuleList, s: PSym): ModuleId =
  mlist.moduleMap[getModule(s).moduleId]

iterator modulesClosed*(m: ModuleList): ModuleId =
  for it in m.modulesClosed.items:
    yield it

func queue*(iter: var ProcedureIter, prc: PSym): bool {.discardable.} =
  ## If the procedure `prc` is not queued already, adds it to the iterators
  ## processing queue. Assuming that two procedures queued this way weren't
  ## queued already, they are returned from `next<#next; ProcedureIter>`_ in
  ## the same order in which they were queued
  assert prc.kind in routineKinds
  # don't queue
  if not containsOrIncl(iter.seen, prc.id) and sfDispatcher notin prc.flags:
    iter.queued.add prc
    true
  else:
    false

func queueProcedureSyms*(iter: var ProcedureIter, ast: PNode) =
  ## Traverses the `ast` and collects all referenced symbols of procedure kind
  ## to `syms` and `marker`
  case ast.kind
  of nkSym:
    let s = ast.sym
    if s.kind in routineKinds:
      queue(iter, s)
  of nkWithSons:
    for n in ast.items:
      queueProcedureSyms(iter, n)
  of nkWithoutSons - {nkSym}:
    discard "nothing to do"

iterator deps*(tree: MirTree; includeMagics: set[TMagic] = {}): PSym {.noSideEffect.} =
  ## Returns all external entities (procedures, globals, etc.) that `tree`
  ## references *directly* in an unspecified order
  var i = NodePosition(0)
  while i < NodePosition(tree.len):
    let n {.cursor.} = tree[i]
    case n.kind
    of mnkDef:
      # make sure to not process the entity inside a 'def'
      i = sibling(tree, i)
      continue
    of mnkProc:
      # don't treat magics as dependencies. They're (in most but not all) cases
      # no "real" procedures
      # TODO: this is a workaround. Magics should be lowered or encoded as
      #       ``mnkMagic`` nodes when reaching here
      if n.sym.magic == mNone or n.sym.magic in includeMagics:
        yield n.sym
    of mnkConst, mnkGlobal:
      yield n.sym
    else:
      discard "nothing to do"

    inc i

iterator procDefs*(tree: MirTree): PSym =
  var i = NodePosition(0)
  while i < NodePosition(tree.len):
    let n {.cursor.} = tree[i]
    case n.kind
    of mnkDef:
      inc i
      if tree[i].kind == mnkProc:
        yield tree[i].sym
      # make sure to not process the entity inside a 'def'
      i = parentEnd(tree, i)
    else:
      discard "nothing to do"

    inc i

func takeInner*(iter: var ProcedureIter): seq[Procedure] =
  ## Removes all queued closure procedures from `iter` and returns them
  result = move iter.queuedInner

proc hasNext*(iter: ProcedureIter): bool =
  result = iter.queued.len > 0 or iter.nextInner < iter.queuedInner.len


type InnerProc = PSym

## The (not yet implemented) new lambda-lifting pass needs access to the MIR
## bodies of multiple procedures at once. We first acquire the MIR code for the
## entry procedure (i.e. the one from which the pass starts) and then
## iteratively collect all transitive relevant inner procedures. Once done, we
## run the lambda-lifting pass

proc extractGlobals(iter: var ProcedureIter, m: var Module, graph: ModuleGraph, body: PNode) =
    ## Extract globals defined in `body` into the pre-init procedure of `m`.
    ## Also registers destructor calls for them, if necessary. `body` is
    ## mutated.
    var globals: seq[PNode]
    extractGlobals(body, globals, isNimVm = false)
    # note: we're modifying the procedure's cached transformed body above,
    # meaning that globals defiend inside ``inline`` procedures are also only
    # extracted once

    # first pass: register the destructors
    for it in globals.items:
      let sym = it[0].sym
      # NOTE: thread-local variables are currently never destroyed
      if sfThread notin sym.flags and hasDestructor(sym.typ):
        iter.globalDestructors.insert(NodeInstance 0, buf):
          genDestroy(buf, graph, sym.typ, MirNode(kind: mnkGlobal, sym: sym, typ: sym.typ))

    # second pass: generate the initialization code. This is done here already,
    # as it might depend on other procedures. Deferring this to ``genInitCode``
    # is not possible, because then it's too late to raise further dependencies.
    # Also, generate the code in the pre-init procedure of the module where the
    # procedure is *defined*, not where it's first *used* (this is only relevant
    # for ``inline`` procedures, as they're generated multiple times)
    for it in globals.items:
      # since the identdefs are extracted from the transformed AST, the
      # initializer expression isn't canonicalized yet
      if it[2].kind != nkEmpty:
        let n = newTreeI(nkFastAsgn, it.info, it[0], it[2])

        # generate the MIR code for the assignment and append it to the
        # pre-init fragment
        generateCode(graph, iter.options, n, m.preInitFragment.tree, m.preInitFragment.sourceMap)

        # TODO: run the destructor injection pass for the pre-init fragment

        # the expression of the initial value might reference procedures
        # TODO: don't scan the whole fragment each time
        for dep in deps(m.preInitFragment.tree, iter.noMagics):
          if dep.kind in routineKinds:
            queue(iter, dep)

proc preprocess(iter: var ProcedureIter, prc: PSym, graph: ModuleGraph, m: var Module): Procedure =
  ## Transforms the body of the given procedure and translates it to MIR code.
  ## No MIR passes are applied yet
  var body = transformBody(graph, m.idgen, prc, cache = false)

  if poLiftGlobals in iter.processOptions:
    extractGlobals(iter, m, graph, body)

  # TODO: turn cursor inference into a MIR pass and remove the logic below
  if optCursorInference in graph.config.options and
     shouldInjectDestructorCalls(prc):
    computeCursors(prc, body, graph)

  #echo prc, " at ", graph.config.toFileLineCol(prc.info)
  if sfImportc in prc.flags and not (iter.noImported and getBody(graph, prc).kind == nkEmpty):
    # the procedure is imported
    result = Procedure(sym: prc, isImported: true)
  else:
    echoInput(graph.config, prc, body)

    result = Procedure(sym: prc, isImported: false)
    (result.tree, result.source) = generateCode(graph, prc, iter.options, body)

    echoMir(graph.config, prc, result.tree)

proc collectInner(iter: var ProcedureIter, prc: PSym, body: MirTree, inner: var seq[InnerProc], marker: var IntSet) =
  ## Collects all transitive inner routines of `prc` that might capture something
  for dep in deps(body):
    if dep.kind in routineKinds and dep.skipGenericOwner.kind in routineKinds and dep.typ.callConv == ccClosure and not containsOrIncl(marker, dep.id):
      # it's an inner procedure; remember it
      inner.add dep

proc processInner(iter: var ProcedureIter, prc: Procedure, graph: ModuleGraph, m: var Module) =
  var list: seq[InnerProc]
  var marker: IntSet
  # first, scan the procedure for inner routines that might capture
  # something:
  collectInner(iter, prc.sym, prc.tree, list, marker)

  # then, pre-process the collected inner routines (if any) and collect the
  # inner routines referenced by them. This is repeated until we've discovered
  # all of them
  var i = 0
  while i < list.len:
    let
      sym = list[i]
      inner = preprocess(iter, sym, graph, m)

    collectInner(iter, sym, inner.tree, list, marker)

    # queue the pre-processed routine and remember that we've seen it:
    iter.seen.incl inner.sym.id
    iter.queuedInner.add inner

    inc i

proc process(iter: var ProcedureIter, prc: var Procedure, graph: ModuleGraph, m: Module) =
  # apply all applicable MIR passes:
  if shouldInjectDestructorCalls(prc.sym):
    injectDestructorCalls(graph, m.idgen, prc.sym, prc.tree, prc.source)

proc processTopLevel*(module: Module, tree: var MirTree, source: var SourceMap, graph: ModuleGraph) =
  # apply all applicable MIR passes:
  if shouldInjectDestructorCalls(module.sym):
    injectDestructorCalls(graph, module.idgen, module.sym, tree, source)

proc next*(iter: var ProcedureIter, graph: ModuleGraph, mlist: var ModuleList): Procedure =
  # TODO: don't require the whole `mlist` to be mutable, we only to modify the
  #       data of the module the next procedure belongs to
  assert hasNext(iter)
  if iter.nextInner < iter.queuedInner.len:
    # there are some pre-processed routines
    result = move iter.queuedInner[iter.nextInner]
    inc iter.nextInner
  elif iter.queued.len > 0:
    let sym = iter.queued.popFirst()
    let m = addr mlist.modules[mlist.lookupModule(sym)]
    #echo "process: ", sym.name.s
    result = preprocess(iter, sym, graph, m[])
    if not result.isImported:
      processInner(iter, result, graph, m[])
  elif not iter.generatedDispatchers:
    # no more queued routines and method dispatchers haven't been generated yet.
    # We assume that this means that all normal routines have been processed
    # and that we should now process all methods

    # XXX: instead of processing and returning *all* methods, we could collect
    #      all used buckets and then only process these. Methods only used
    #      inside other methods would be a small problem, however
    for disp in generateMethodDispatchers2(graph):
      iter.queued.add disp

    iter.generatedDispatchers = true

    # pick the first dispatcher:
    let sym = iter.queued.popFirst()
    let m = addr mlist.modules[mlist.lookupModule(sym)]
    result = preprocess(iter, sym, graph, m[])
    processInner(iter, result, graph, m[])
  else:
    unreachable("hasNext == false")

  if not result.isImported:
    # now apply all MIR passes:
    process(iter, result, graph, mlist.modules[mlist.lookupModule(result.sym)])

# Below is the `passes` interface implementation

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  if graph.backend == nil:
    graph.backend = ModuleListRef()

  let
    mlist = ModuleListRef(graph.backend)
    id = module.itemId.module

  assert id >= 0 and id == module.position # sanity check

  # create an entry in the store and add an ID mapping:
  let mId =
    mlist.modules.add Module(sym: module, stmts: newNode(nkStmtList),
                             idgen: idgen)
  mlist.moduleMap[id] = mId

  result = ModuleRef(list: mlist, id: mId)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n
  let m = ModuleRef(b)

  if n.kind == nkStmtList:
    # append all statements to the top-level list:
    for it in n.items:
      m.get.stmts.add(it)
  else:
    m.get.stmts.add(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)

  let m = ModuleRef(b)
  # prevent empty statement lists from reaching the code-generators
  if m.get.stmts.len == 0:
    m.get.stmts = newNode(nkEmpty)

  # remember the order relative to the other modules in which the current one
  # is closed
  m.list.modulesClosed.add(m.id)

const collectPass* = makePass(myOpen, myProcess, myClose)