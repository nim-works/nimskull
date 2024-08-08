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
    cgir,
    jsgen
  ],
  compiler/front/[
    options
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/mir/[
    mirenv,
    mirtrees
  ],
  compiler/sem/[
    modulelowering,
    sourcemap
  ],
  compiler/utils/[
    containers,
    idioms,
    ropes
  ]

from compiler/mir/mirbridge import canonicalize

type
  BModuleList = SeqMap[FileIndex, BModule]
  PartialTable = Table[int, PProc]

proc prepare(globals: PGlobals, modules: BModuleList, n: MirNode) =
  ## Responds to the discovery of entity `n`.
  case n.kind
  of mnkProc, mnkConst:
    discard "nothing to forward declare or register"
  of mnkGlobal:
    let s = globals.env[n.global]
    defineGlobal(globals, modules[moduleId(s).FileIndex], n.global)
  else:
    unreachable()

proc processEvent(g: PGlobals, graph: ModuleGraph, modules: BModuleList,
                  discovery: var DiscoveryData, partial: var PartialTable,
                  evt: sink BackendEvent) =
  ## The orchestrator's event processor.
  let bmod = modules[evt.module]

  case evt.kind
  of bekDiscovered:
    prepare(g, modules, evt.entity)
  of bekModule:
    discard "nothing to do"
  of bekConstant:
    let s = g.env[evt.cnst]
    genConstant(g, modules[moduleId(s).FileIndex], evt.cnst)
  of bekPartial:
    var p = partial.getOrDefault(evt.sym.id)
    if p == nil:
      p = startProc(g, bmod, evt.id, Body())
      partial[evt.sym.id] = p

    let body = generateIR(graph, bmod.idgen, g.env, evt.sym, evt.body)
    genPartial(p, merge(p.fullBody, body))
  of bekProcedure:
    let
      body = generateIR(graph, bmod.idgen, g.env, evt.sym, evt.body)
      r = genProc(g, bmod, evt.id, body)

    if sfCompilerProc in evt.sym.flags:
      # compilerprocs go into the constants section ...
      g.constants.add(r)
    else:
      # ... other procedures into the normal code section
      g.code.add(r)

  of bekImported:
    discard "ignored for now"

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

  let owner = m.module
  genTopLevelStmt(globals, m):
    canonicalize(graph, m.idgen, globals.env, owner, body, TranslationConfig())

proc generateCode*(graph: ModuleGraph, mlist: sink ModuleList) =
  ## Entry point into the JS backend. Generates the code for all modules and
  ## writes it to the output file.
  let
    globals = newGlobals(graph)
    bconf = BackendConfig(tconfig: TranslationConfig(magicsToKeep: NonMagics))

  var
    modules: BModuleList
    discovery: DiscoveryData
    partial: PartialTable

  # setup the ``BModule`` instances:
  for m in closed(mlist):
    let bmod = newModule(graph, m.sym)
    bmod.idgen = m.idgen

    if m.init.ast[bodyPos].kind != nkEmpty: # dead-code elimination
      # HACK: we mark the procedure with the ``sfModuleInit`` flag in order to
      #       signal to ``jsgen`` that a special stack-trace entry needs to
      #       be created for the procedure
      m.init.flags.incl sfModuleInit

    modules[m.sym.position.FileIndex] = bmod

  for evt in process(graph, mlist, globals.env, discovery, bconf):
    processEvent(globals, graph, modules, discovery, partial, evt)

  # finish the partial procedures:
  for p in partial.values:
    globals.code.add finishProc(p)

  # wrap up:
  let main = modules[graph.config.projectMainIdx]
  reset(modules) # we don't need the data anymore

  generateCodeForMain(globals, graph, main, mlist)

  # write the generated code to disk:
  writeModules(graph, globals)
