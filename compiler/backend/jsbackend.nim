
import
  std/[
    intsets,
    tables,
    json
  ],
  compiler/ast/[
    ast,
    ast_types,
  ],
  compiler/backend/[
    backends,
    jsgen,
    backend2
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    mirgen,
    mirtrees,
    astgen,
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    transf,
    sourcemap,
  ],
  compiler/utils/[
    containers,
    ropes,
  ]

type
  LocalModuleData = object
    bmod: BModule
    init: PProc

    struct: ModuleStruct

  CodegenCtx = GCodegenCtx[LocalModuleData]

proc queueAll(ctx: var CodegenCtx, iter: var ProcedureIter, tree: MirTree) =
  ## Processes the direct dependencies of the given `tree`.
  for dep in deps(tree, ctx.noMagics):
    case dep.kind
    of routineKinds:
      queue(iter, dep)
    of skConst:
      if not containsOrIncl(ctx.seen, dep.id):
        # a complex constant might reference the symbols of further procedures --
        # they too are alive
        queueProcedureSyms(iter, astdef(dep))
    else:
      discard "a global; ignore"

proc processModule(ctx: var CodegenCtx, m: Module, graph: ModuleGraph,
                   g: PGlobals, data: var LocalModuleData,
                   iter: var ProcedureIter) =
    let top = transformStmt(graph, m.idgen, m.sym, m.stmts)
    var (tree, source) = generateCode(graph, m.sym, {}, top)

    # exported procedures always need to be code-gen'ed, irrespective of
    # whether they're actually used
    for def in procDefs(tree):
      # don't queue compilerprocs here. They're only included in code
      # generation if they're used
      if {sfExportc, sfCompilerProc} * def.flags == {sfExportc}:
        queue(iter, def)

    # add the defined globals and threadvars to the module's struct:
    for def in globalDefs(tree):
      if sfThread in def.flags:
        data.struct.threadvars.add def
      else:
        data.struct.globals.add def
        # if the global has a destructor, we need to queue the procedure
        # already
        queueDestructor(iter, graph, def)

    processTopLevel(m, tree, source, graph)

    # process and queue the dependencies:
    queueAll(ctx, iter, tree)

    let stmts = generateAST(graph, m.idgen, m.sym, tree, source)
    genModule(data.init, stmts) # might raise late dependencies

    for it in g.extra.items:
      queue(iter, it)

    # we processed/consumed all elements
    g.extra.setLen(0)

proc processProc(ctx: var CodegenCtx, prc: Procedure, graph: ModuleGraph,
                 m: var LocalModuleData, iter: var ProcedureIter) =
  # queue all procedures used by the initialization logic for the
  # procedure-level globals. They need to be queued from the module to
  # which the procedure is *attached*, not from the one it's *first used*
  # from
  queueAll(ctx, iter, prc.extra.tree)

  for it in prc.globals.items:
    m.struct.privateFields.add it
    # also queue the used destructors here -- doing so when generating the
    # calls would be too late
    queueDestructor(iter, graph, it)

proc generateCode*(graph: ModuleGraph) =
  echo "codegen"

  var ctx = CodegenCtx(list: ModuleListRef(graph.backend))
  var g = newGlobals()

  reserve(ctx.modules, ctx.list.modules)

  var main: ModuleId
  var iter: ProcedureIter

  iter.processOptions.incl poLiftGlobals

  # first create a ``BModule`` instance for all modules that we know about:
  for i, m in ctx.list.modules.pairs:
    var local = newModule(graph, m.sym)
    local.idgen = m.idgen
    ctx.modules[i] = LocalModuleData(bmod: local, init: setupInitProc(g, local))

    if sfMainModule in m.sym.flags:
      main = i

  # process the modules (i.e. generated code for their top-level statements).
  # Since dependency processing potentially requires access to modules other
  # than the one for which the top-level statements are code-gen'ed, it can't
  # happen as part of the above loop
  # TODO: don't use the module-closed order here. Either setup the name for
  #       globals in ``jsgen`` or, better yet, use a different appraoch
  for id in ctx.list.modulesClosed:
    processModule(ctx, ctx.list.modules[id], graph, g, ctx.modules[id], iter)

  while hasNext(iter, graph):
    let prc = next(iter, graph, ctx.list[])
    let id = ctx.list[].lookupModule(prc.sym)
    let m {.cursor.} = ctx.list.modules[id]
    let bmod = ctx.modules[id].bmod

    case prc.isImported
    of false:
      processProc(ctx, prc, graph, ctx.modules[id], iter)
      queueAll(ctx, iter, prc.tree)

      # lambda-lifting is disabled for the JS backend, which means that we
      # require special handling for closure procedures
      let inner = iter.takeInner()

      for it in inner.items:
        processProc(ctx, it, graph, ctx.modules[id], iter)
        g.innerProcs[it.sym.id] =
          generateAST(graph, m.idgen, it.sym, it.tree, it.source)

      let body = generateAST(graph, m.idgen, prc.sym, prc.tree, prc.source)
      let r = genProc(ctx.modules[id].init, prc.sym, body)

      if sfCompilerProc in prc.sym.flags:
        # compilerprocs go into the constants section ...
        g.constants.add(r)
      else:
        # ... other procedures into the normal code section
        g.code.add(r)

    of true:
      discard "nothing to do"

    # queue the late dependencies:
    for it in g.extra.items:
      queue(iter, it)

    # we processed/consumed all elements
    g.extra.setLen(0)

  # generate the clean up logic for the program, and append it to the end
  # of the main module's init procedure:
  genStmt(ctx.modules[main].init, generateProgramDestructor(ctx, graph))

  # all procedures are generated. Emit the top-level code for all modules in
  # the order they were closed:
  for i in ctx.list[].modulesClosed:
    g.code.add ctx.modules[i].init.locals
    g.code.add ctx.modules[i].init.body

  var code = genHeader() & wholeCode(g)
  let outFile = graph.config.prepareToWriteOutput()

  # generate and write out the source map, if requested:
  if optSourcemap in graph.config.globalOptions:
    var map: SourceMap
    (code, map) = genSourceMap($(code), outFile.string)
    writeFile(outFile.string & ".map", $(%map))

  # write the generated code to disk:
  discard writeRopeIfNotEqual(code, outFile)