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
  compiler/backend/[
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

    # invoke ``jsgen`` for all top-level code:
    for n in m.stmts.items:
      genTopLevelStmt(globals, bmod, n)

    # close the module:
    finalCodegenActions(graph, globals, bmod)

  # write the generated code to disk:
  writeModules(graph, globals)