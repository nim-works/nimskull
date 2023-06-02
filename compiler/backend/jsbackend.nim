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
    ast
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
    passes,
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

  # generate the code for all modules:
  for index in mlist.modulesClosed.items:
    let
      m {.cursor.} = mlist.modules[index]
      bmod = newModule(graph, m.sym)

    bmod.idgen = m.idgen

    # invoke ``jsgen`` for the top-level declarative code:
    genTopLevelStmt(globals, bmod, m.decls)

    # HACK: we mark the procedure with the ``sfGlobal`` flag in order to
    #       signal ``jsgen`` that a special stack-trace entry should be used
    m.init.flags.incl sfGlobal
    genTopLevelProcedure(globals, bmod, m.init)

    if sfMainModule in m.sym.flags:
      generateCodeForMain(globals, graph, bmod, mlist)

  # write the generated code to disk:
  writeModules(graph, globals)