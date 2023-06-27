## The code-generation orchestrator for the JavaScript backend. Takes the
## semantically analysed AST of the whole program, generates the JavaScript
## code for it (delegated to ``jsgen``), and assembles everything into a
## single JavaScript file.

import
  std/[
    json,
    tables
  ],
  compiler/ast/[
    ast,
    lineinfos
  ],
  compiler/backend/[
    backends,
    jsgen
  ],
  compiler/front/[
    options
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    modulelowering,
    sourcemap
  ],
  compiler/utils/[
    containers,
    ropes
  ]

type
  BModuleList = SeqMap[FileIndex, BModule]
  PartialTable = Table[int, PProc]

proc prepare(globals: PGlobals, modules: BModuleList, d: var DiscoveryData) =
  ## Emits the definitions for all constants, globals, and threadvars
  ## discovered while producing the current event.
  for _, s in visit(d.constants):
    genConstant(globals, modules[moduleId(s).FileIndex], s)

  for _, s in visit(d.globals):
    defineGlobal(globals, modules[moduleId(s).FileIndex], s)

  for _, s in visit(d.threadvars):
    defineGlobal(globals, modules[moduleId(s).FileIndex], s)

proc processLate(globals: PGlobals, discovery: var DiscoveryData) =
  # queue the late dependencies:
  for it in globals.extra.items:
    register(discovery, it)

  # we processed/consumed all elements
  globals.extra.setLen(0)

proc processEvent(g: PGlobals, graph: ModuleGraph, modules: BModuleList,
                  discovery: var DiscoveryData, partial: var PartialTable,
                  evt: sink BackendEvent) =
  ## The orchestrator's event processor.
  let bmod = modules[evt.module]
  prepare(g, modules, discovery)

  case evt.kind
  of bekModule:
    discard "nothing to do"
  of bekPartial:
    var p = partial.getOrDefault(evt.sym.id)
    if p == nil:
      p = startProc(g, bmod, evt.sym)
      partial[evt.sym.id] = p

    let body = generateAST(graph, bmod.idgen, evt.sym, evt.body)
    genStmt(p, body)

    processLate(g, discovery)
  of bekProcedure:
    let
      body = generateAST(graph, bmod.idgen, evt.sym, evt.body)
      r = genProc(g, bmod, evt.sym, body)

    if sfCompilerProc in evt.sym.flags:
      # compilerprocs go into the constants section ...
      g.constants.add(r)
    else:
      # ... other procedures into the normal code section
      g.code.add(r)

    processLate(g, discovery)

proc writeModules(graph: ModuleGraph, globals: PGlobals) =
  let
    config = graph.config
    outFile = config.prepareToWriteOutput()

  var code = genHeader() & wholeCode(globals)
  if optSourcemap in config.globalOptions:
    var map: SourceMap
    (code, map) = genSourceMap($(code), outFile.string)
    writeFile(outFile.string & ".map", $(%map))

  # write the generated code to disk:
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

  var
    modules: BModuleList
    discovery: DiscoveryData
    partial: PartialTable

  # setup the ``BModule`` instances:
  for m in closed(mlist):
    let bmod = newModule(graph, m.sym)
    bmod.idgen = m.idgen
    modules[m.sym.position.FileIndex] = bmod

  for evt in process(graph, mlist, discovery, {}, BackendConfig()):
    processEvent(globals, graph, modules, discovery, partial, evt)

  # finish the partial procedures:
  for p in partial.values:
    globals.code.add finishProc(p)

  # wrap up:
  let main = modules[graph.config.projectMainIdx2]
  reset(modules) # we don't need the data anymore

  generateCodeForMain(globals, graph, main, mlist)

  # write the generated code to disk:
  writeModules(graph, globals)
