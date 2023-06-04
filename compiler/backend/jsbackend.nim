## The code-generation orchestrator for the JavaScript backend. It generates
## the JS code for the semantically analysed AST of the whole progam by
## invoking ``jsgen``.
##
## The general direction is to move more logic out of the code generator (such
## as figuring out the set of alive procedures) and into the orchestrator,
## leaving only the core of code generation to ``jsgen``.

import
  std/[
    json
  ],
  compiler/ast/[
    ast,
    lineinfos
  ],
  compiler/backend/[
    backends,
    cgmeth,
    jsgen
  ],
  compiler/front/[
    options
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    collectors,
    sourcemap
  ],
  compiler/utils/[
    containers,
    ropes
  ]

proc writeModules(graph: ModuleGraph, globals: PGlobals) =
  let
    config = graph.config
    outFile = config.prepareToWriteOutput()

  var code = genHeader() & wholeCode(globals)
  if optSourcemap in config.globalOptions:
    var map: SourceMap
    (code, map) = genSourceMap($(code), outFile.string)
    writeFile(outFile.string & ".map", $(%map))

  discard writeRopeIfNotEqual(code, outFile)

proc generateCodeForMain(globals: PGlobals, graph: ModuleGraph, m: BModule,
                         modules: ModuleList) =
  # generate the code for the initializing, running, and de-initializing
  # the program
  var body = newNode(nkStmtList)
  generateMain(graph, modules, body)
  generateTeardown(graph, modules, body)

  genTopLevelStmt(globals, m, body)

proc generateCode*(graph: ModuleGraph, mlist: sink ModuleList) =
  ## Entry point into the JS backend. Generates the code for all modules and
  ## writes it to the output file.
  let
    globals = newGlobals()

  generateMethodDispatchers(graph)

  var modules: SeqMap[FileIndex, BModule]

  # setup the ``BModule`` instances and create definitions for all
  # globals (order doesn't matter):
  for m in closed(mlist):
    let bmod = newModule(graph, m.sym)
    bmod.idgen = m.idgen

    defineGlobals(globals, bmod, m.structs.globals)
    defineGlobals(globals, bmod, m.structs.globals2)
    # no special handling for thread-local variables (yet)
    defineGlobals(globals, bmod, m.structs.threadvars)

    modules[m.sym.position.FileIndex] = bmod

  # generate the code for all modules:
  for m in closed(mlist):
    let
      bmod = modules[m.sym.position.FileIndex]

    # invoke ``jsgen`` for the top-level declarative code:
    genTopLevelStmt(globals, bmod, m.decls)

    # HACK: we mark the procedure with the ``sfGlobal`` flag in order to
    #       signal ``jsgen`` that a special stack-trace entry should be used
    m.init.flags.incl sfGlobal
    genTopLevelProcedure(globals, bmod, m.init)

    if sfMainModule in m.sym.flags:
      finishDeinit(graph, mlist)
      generateCodeForMain(globals, graph, bmod, mlist)

    # we don't need the ``BModule`` instance anymore:
    modules[m.sym.position.FileIndex] = nil

  # write the generated code to disk:
  writeModules(graph, globals)