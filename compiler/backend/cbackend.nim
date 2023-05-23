## The code-generation orchestrator for the C backend. It generates the C code
## for the semantically analysed AST of the whole progam by invoking ``cgen``.
##
## The general direction is to move more logic out of the code generator (such
## as figuring out the set of alive procedures) and into the orchestrator,
## leaving only the core of code generation to ``cgen``.

import
  compiler/ast/[
    ast
  ],
  compiler/backend/[
    cgen,
    cgendata,
    extccomp
  ],
  compiler/front/[
    options
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    collectors
  ],
  compiler/utils/[
    containers,
    pathutils
  ]

from compiler/sem/passes import skipCodegen

proc generateCode*(graph: ModuleGraph, mlist: sink ModuleList) =
  ## Entry point for C code-generation. Only the C code is generated -- nothing
  ## is written to disk yet.
  let
    config = graph.config

  var g = newModuleList(graph)

  # first create a module list entry for each input module. This has to happen
  # *before* the code generator is invoked.
  for key, val in mlist.modules.pairs:
    let m = newModule(g, val.sym, config)
    m.idgen = val.idgen

  # setup the module for the generated header, if required:
  if optGenIndex in config.globalOptions:
    let f = if config.headerFile.len > 0: AbsoluteFile config.headerFile
            else: config.projectFull
    g.generatedHeader = rawNewModule(g, mlist.modules[config.projectMainIdx2].sym,
      changeFileExt(completeCfilePath(config, f), hExt))
    incl g.generatedHeader.flags, isHeaderFile

  # the main part: invoke the code generator for all top-level code
  for index in mlist.modulesClosed.items:
    let
      m {.cursor.} = mlist.modules[index]
      bmod = g.modules[index.int]

    # pass all top-level code to the code generator:
    for it in m.stmts.items:
      if not skipCodegen(bmod.config, it):
        genTopLevelStmt(bmod, it)

    # wrap up the main part of code generation for the module. Note that this
    # doesn't mean that they're closed for writing; invoking the code generator
    # for other modules' code can still add new code to this module's sections
    finalCodegenActions(graph, g.modules[index.int], newNode(nkStmtList))

  # the callsite still expects `graph.backend` to point to the ``BModuleList``
  # so that ``cgenWriteModules`` can query it
  # XXX: this is the wrong approach -- the code generator must not be
  #      responsible for writing the generated C translation units to disk.
  graph.backend = g