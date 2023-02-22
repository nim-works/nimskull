## This module implements the code-generation orchestrator for the C backend.

import
  std/[
    intsets,
    tables,
    sets
  ],
  compiler/ast/[
    ast,
    ast_types,
    lineinfos
  ],
  compiler/backend/[
    backends,
    cgen,
    cgendata
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    mirgen,
    mirbridge,
    mirtrees,
    astgen
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/sem/[
    transf
  ],
  compiler/utils/[
    containers,
    ropes,
    platform
  ]

from compiler/sem/injectdestructors import genDestroy

export collectPass # TODO: remove

type
  InlineProc = object
    info: TLineInfo
      ## where the procedure was defined. Used by error reporting
    sym: PSym
    body: PNode
      ## the fully processed body of the procedure

    firstSeen: ModuleId ## the module in which the procedure was first seen
      # TODO: make this work for inline procedure called from inside other inline procedures

    deps: IntSet

  GlobalData = object
    ## Code-generator-related data shared across all modules
    inlineProcs: Store[uint32, InlineProc]
      ## stores the additional information plus the generated code for each
      ## alive inline procedure
    inlineMap: Table[int, uint32]
      ## maps a procedure ID to the associated InlineProc ID

  ModuleStruct* = object
    ## Stores information about a module struct. A module struct is the
    ## aggregate type that stores all module-level data, which, for now, are
    ## only globals and threadvars. A global can be viewed as a field of the
    ## module struct corresponding to the module it belongs
    globals*: seq[PSym]
    privateFields*: seq[PSym] ## procedure-level globals of procedures attached
      ## to the module

    threadvars*: seq[PSym] ## thread-local variables of the module

  LocalModuleData = object
    bmod: BModule

    inlined: OrderedSet[uint32]
      ## all procedures that need to be inlined into the module

    struct: ModuleStruct

  GCodegenCtx*[T] = object
    # TODO: move it to a module that is shared between all backends
    list*: ModuleListRef
    modules*: SeqMap[ModuleId, T]

    # TODO: implement this differently
    noMagics*: set[TMagic]

    # TODO: this too
    seen*: IntSet

  CodegenCtx = GCodegenCtx[LocalModuleData]

iterator ritems[T](x: openArray[T]): lent T =
  # TODO: move this routine somewhere common
  var i = x.high
  while i >= 0:
    yield x[i]
    dec i

func isFilled(x: LocalModuleData): bool =
  x.bmod != nil

proc registerInline(g: var GlobalData, prc: PSym): uint32 =
  ## If not already registered, registers the inline procedure `prc` with the
  ## global code-generator context. This only sets up an ``InlineProc`` entry
  ## stub -- the entry is not populated yet
  if prc.id in g.inlineMap:
    # XXX: ``getOrDefault`` can't be used to get around the double
    #      table-lookup, as there doesn't exist a known unused symbol ID.
    #      ``mgetOrPut`` could be used
    result = g.inlineMap[prc.id]
  else:
    result = g.inlineProcs.add(InlineProc(sym: prc))
    g.inlineMap[prc.id] = result

proc getModuleOf(c: CodegenCtx, s: PSym): BModule =
  ## Returns the module `prc` is attached to. Note that this is not
  ## necessarily the symbol's owner
  c.modules[c.list[].lookupModule(s)].bmod

proc queueSingle(ctx: var CodegenCtx, iter: var ProcedureIter, g: var GlobalData, m: var LocalModuleData, mId: ModuleId, dep: PSym) =
  let isInline = dep.typ.callConv == ccInline

  if queue(iter, dep):
    # a new procedure; fill in the `loc`. Pass the module the procedure is
    # attached to, not the the one it's used from
    fillProcLoc(ctx.getModuleOf(dep), dep.ast[namePos])

    if isInline:
      # remember the module we've first seen the procedure used in:
      let id = registerInline(g, dep)
      g.inlineProcs[id].firstSeen = mId

  elif isInline:
    # remember the dependency on an inline procedure
    m.inlined.incl registerInline(g, dep)

const CBackendNoMagics =
  {mNewString, mNewStringOfCap, mExit, mParseBiggestFloat, mDotDot, mEqCString,
   mIsolate}
  ## Magic procedures that have use no dedicated code-generation logic but are instead normal calls

const CBackendNimv2NoMagics =
  {mNewSeq, mSetLengthSeq, mAppendSeqElem}

proc queueAll(ctx: var CodegenCtx, iter: var ProcedureIter, g: var GlobalData, m: var LocalModuleData, mId: ModuleId, tree: MirTree) =
  ## Processes the direct dependencies of the given `tree`.
  for dep in deps(tree, ctx.noMagics):
    case dep.kind
    of routineKinds:
      queueSingle(ctx, iter, g, m, mId, dep)
    of skConst:
      if not containsOrIncl(ctx.seen, dep.id):
        # a complex constant might reference the symbols of further procedures --
        # they too are alive
        # TODO: this needs to work differently. inline procedures are not handled
        #       properly
        queueProcedureSyms(iter, astdef(dep))
    else:
      discard "a global; ignore"

func selectedModule(ctx: CodegenCtx, gstate: var GlobalData, prc: Procedure): ModuleId =
  # TODO: don't require a mutable gstate
  let id = ctx.list[].lookupModule(prc.sym)
  let isInline = prc.sym.typ.callConv == ccInline

  if isInline and not prc.isImported:
    # inline procedures might have late dependencies (i.e. those raised
    # only during by ``cgen``) so, in order to know about them without
    # requiring a nested loop, we generate and emit code for them in the
    # module where they were first seen. This way, we can already collect
    # the late dependencies during the main loop
    gstate.inlineProcs[registerInline(gstate, prc.sym)].firstSeen
  else:
    id

proc dependOnCompilerProc(ctx: var CodegenCtx, iter: var ProcedureIter,
                          g: var GlobalData, m: var LocalModuleData,
                          mId: ModuleId, graph: ModuleGraph, name: string) =
  let sym = getCompilerProc(graph, name)
  # guard against nil so that the procedure can be used in contexts where not
  # all compilerprocs have been processed yet
  if sym != nil:
    # a compilerproc can also be marked as ``inline``, hence the usage of
    # ``queueSingle`` and not just ``queue``
    queueSingle(ctx, iter, g, m, mId, sym)

proc generateModuleDestructor(m: ModuleStruct, graph: ModuleGraph, dest: PNode) =
  ## Generates the code for cleaning up a module's data, and appends it to
  ## `dest`.

  # as their destruction might access the non-private module fields, first
  # destroy the private fields in the reverse order they were defined
  for it in m.privateFields.ritems:
    if hasDestructor(it.typ):
      dest.add genDestroy(graph, newSymNode(it))

  # then destroy the non-private fields (also in the reverse order)
  for it in m.globals.ritems:
    if hasDestructor(it.typ):
      dest.add genDestroy(graph, newSymNode(it))

proc generateProgramDestructor*[T](ctx: GCodegenCtx[T], graph: ModuleGraph): PNode =
  ## Generates the code for cleaning up all data owned by the modules the
  ## program is made up of.
  result = newNode(nkStmtList)
  # the modules are destroyed in reverse order. That is, the module that was
  # closed last is cleaned up first, then the second last, etc.
  for id in ctx.list.modulesClosed.ritems:
    generateModuleDestructor(ctx.modules[id].struct, graph, result)

proc queueDestructor*(iter: var ProcedureIter, graph: ModuleGraph, global: PSym) =
  ## If it requires destruction and has a destructor, queue the destructor for
  ## `global` with `iter`
  if sfThread notin global.flags and hasDestructor(global.typ):
    # assume that destructors can't be inline
    queue(iter, getAttachedOp(graph, global.typ, attachedDestructor))

proc generateCodeC*(graph: ModuleGraph) =
  echo "codegen"

  var ctx = CodegenCtx(list: ModuleListRef(graph.backend))
  var clist = newModuleList(graph)
  var iter: ProcedureIter
  var gstate: GlobalData

  reserve(ctx.modules, ctx.list.modules)

  ctx.noMagics = CBackendNoMagics
  if optSeqDestructors in graph.config.globalOptions:
    ctx.noMagics.incl CBackendNimv2NoMagics

  # first create a ``BModule`` instance for all modules that we know about:
  for i, m in ctx.list.modules.pairs:
    var local = LocalModuleData(bmod: newModule(clist, m.sym, graph.config))
    local.bmod.idgen = m.idgen
    ctx.modules[i] = local

  # process the top-level statements. Since dependency processing might
  # requires access to modules other than the one for which the top-level
  # statements are code-gen'ed, it can't happen as part of the above loop
  for i, m in ctx.list.modules.pairs:
    let top = transformStmt(graph, m.idgen, m.sym, m.stmts)
    var (tree, source) = generateCode(graph, m.sym, {}, top)

    # exported procedures always need to be code-gen'ed, irrespective of
    # whether they're actually used
    for def in procDefs(tree):
      # don't queue compilerprocs here. They're only included in code
      # generation if they're used
      if {sfExportc, sfCompilerProc} * def.flags == {sfExportc}:
        queueSingle(ctx, iter, gstate, ctx.modules[i], i, def)

    # add the defined globals and threadvars to the module's struct:
    for def in globalDefs(tree):
      if sfThread in def.flags:
        ctx.modules[i].struct.threadvars.add def
      else:
        ctx.modules[i].struct.globals.add def
        # if the global has a destructor, we need to queue the procedure
        # already
        queueDestructor(iter, graph, def)

    processTopLevel(m, tree, source, graph)

    # process and queue the dependencies:
    queueAll(ctx, iter, gstate, ctx.modules[i], i, tree)

    let stmts = generateAST(graph, m.idgen, m.sym, tree, source)
    echoOutput(graph.config, m.sym, stmts)

    genTopLevelStmt2(ctx.modules[i].bmod, stmts) # might raise late dependencies

    for it in ctx.modules[i].bmod.extra.items:
      queueSingle(ctx, iter, gstate, ctx.modules[i], i, it)

    # we processed/consumed all elements
    ctx.modules[i].bmod.extra.setLen(0)

    # there are some dependencies raised during ``finalCodegenActions``, but
    # at that point, the iterator can now longer be notified about them. As a
    # workaround, we raise these dependencies here
    # work around
    # XXX: in the future, code generator should no longer be be allowed to
    #      introduce new dependencies
    if sfMainModule in m.sym.flags:
      if graph.config.exc == excGoto:
        dependOnCompilerProc(ctx, iter, gstate, ctx.modules[i], i, graph, "nimTestErrorFlag")

      if graph.config.target.targetOS != osStandalone:
        if graph.config.selectedGC != gcNone:
          dependOnCompilerProc(ctx, iter, gstate, ctx.modules[i], i, graph, "initStackBottomWith")

        if emulatedThreadVars(graph.config):
          dependOnCompilerProc(ctx, iter, gstate, ctx.modules[i], i, graph, "initThreadVarsEmulation")

  # discover and generate code for all alive procedures
  while hasNext(iter):
    let prc = next(iter, graph, ctx.list[])
    let id = selectedModule(ctx, gstate, prc)
    let m {.cursor.} = ctx.list.modules[id]
    let bmod = ctx.modules[id].bmod
    let isInline = prc.sym.typ.callConv == ccInline
    case prc.isImported
    of false:
      if isInline:
        # FIXME: this is inefficient. The dependencies are already iterated in
        #        ``queueAll``. Find a way to merge both traversals
        let iid = registerInline(gstate, prc.sym)
        for dep in deps(prc.tree, ctx.noMagics):
          if dep.kind in routineKinds and dep.typ.callConv == ccInline:
            let other = registerInline(gstate, dep)
            gstate.inlineProcs[iid].deps.incl other.int

      block:
        # queue all procedures used by the initialization logic for the
        # procedure-level globals. They need to be queued from the module to
        # which the procedure is *attached*, not from the one it's *first used*
        # from
        let mId = ctx.list[].lookupModule(prc.sym)
        queueAll(ctx, iter, gstate, ctx.modules[mId], mId, prc.extra.tree)

        for it in prc.globals.items:
          ctx.modules[mId].struct.privateFields.add it
          # also queue the used destructors here -- doing so when generating the
          # calls would be too late
          queueDestructor(iter, graph, it)

      # FIXME: argument aliasing rule violation
      queueAll(ctx, iter, gstate, ctx.modules[id], id, prc.tree)

      let body = generateAST(graph, m.idgen, prc.sym, prc.tree, prc.source)
      echoOutput(graph.config, m.sym, body)

      genProcPrototype(bmod, prc.sym)
      let r = genProcAux(bmod, prc.sym, body)
      bmod.declaredThings.incl(prc.sym.id)

      if isInline:
        #echo "emit ", prc.sym.name.s, " in ", bmod.module.name.s
        # remember the generated body:
        gstate.inlineProcs[registerInline(gstate, prc.sym)].body = body

      # add the C function to the procedure section of the module it is
      # attached to, or, in the case of inline procedures, we're it's first
      # used
      bmod.s[cfsProcs].add(r)

    of true:
      # XXX: code generation for dynamically imported procedures should also
      #      be managed here. Due to the fact that code generation for them has
      #      to happen *before* their name is first accessed (otherwise the
      #      original name would be used, and not the internal one, e.g.
      #      ``Dl_XXX``) we still let ``cgen`` take care of that for now
      if lfDynamicLib notin prc.sym.loc.flags:
        genProcNoForward(bmod, prc.sym)

    # calls to procedures part of the compiler's runtime (i.e.
    # ``.compilerproc``s) are still emitted by the code-generators for now.
    # So that we know about which ones are used, the code-generator collects
    # them to a list that we process here
    for it in bmod.extra.items:
      queueSingle(ctx, iter, gstate, ctx.modules[id], id, it)
      if isInline and it.typ.callConv == ccInline:
        let other = registerInline(gstate, it)
        gstate.inlineProcs[registerInline(gstate, prc.sym)].deps.incl other.int

    # we processed/consumed all elements
    bmod.extra.setLen(0)

  echo "pass 1 done"

  # pass 2: emit all inlined procedures. Due to how ``cgen`` currently works,
  # this means generating code for them again
  for i, m in ctx.modules.pairs:
    proc emit(m: BModule, map: Store[uint32, InlineProc], prc: InlineProc, mId: ModuleId, r: var Rope) =
      if m.declaredThings.containsOrIncl(prc.sym.id):
        return

      for dep in prc.deps.items:
        emit(m, map, map[dep.uint32], mId, r)

      genProcPrototype(m, prc.sym)
      r.add(genProcAux(m, prc.sym, prc.body))

    var r: Rope
    for it in m.inlined.items:
      emit(m.bmod, gstate.inlineProcs, gstate.inlineProcs[it], i, r)

    m.bmod.s[cfsProcs].add(r)

  echo "pass 2 done"

  # the main part of code generation is done. We now know about the content of
  # all module structs, so we can generate the cleanup logic
  let programFinalizer =
    if {optGenStaticLib, optGenDynLib, optNoMain} * graph.config.globalOptions == {}:
      generateProgramDestructor(ctx, graph)
    else:
      newNode(nkStmtList)

  # generate the code for the init procedures and close the modules. This has
  # to happen in the order the modules were closed:
  for m in ctx.list[].modulesClosed:
    let extra =
      if sfMainModule in ctx.list.modules[m].sym.flags:
        programFinalizer
      else:
        newNode(nkStmtList)

    finalCodegenActions(graph, ctx.modules[m].bmod, extra)

    #echo "flags: ", whichInitProcs(it.bmod), " in ", it.bmod.module.name.s
    #registerInitProcs(clist, it.bmod.module, whichInitProcs(it.bmod))

  echo "pass 3 done"

  cgenWriteModules(clist, graph.config)
