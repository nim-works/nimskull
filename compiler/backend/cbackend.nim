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
    backends,
    cgen,
    cgendata,
    cgmeth,
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

proc configureInitProcedure(prc: PSym, name: string) =
  ## Generating and emitting the modules' initialization procedures is
  ## currently still done by the code generator. In order to support this in
  ## a clean way, the procedures are treated as imported.
  prc.flags.incl sfImportc
  prc.loc.r = name # set the mangled name

proc generateCodeForMain(m: BModule, modules: ModuleList) =
  ## Generates and emits the C code for the program's or library's entry
  ## point.
  let p = newProc(nil, m)
  # we don't want error or stack-trace code in the main procedure:
  p.flags.incl nimErrorFlagDisabled
  p.options = {}

  # generate the body:
  let body = newNode(nkStmtList)
  generateMain(m.g.graph, modules, body)
  if {optGenStaticLib, optGenDynLib, optNoMain} * m.config.globalOptions == {}:
    # only emit the teardown logic when we're building a standalone program
    # XXX: the teardown logic should be generated into a separate C function
    #      in this case; otherwise there's no way to free the module structs
    generateTeardown(m.g.graph, modules, body)

  # now generate the C code for the body:
  genProcBody(p, body)
  var code: string
  code.add(p.s(cpsLocals))
  code.add(p.s(cpsInit))
  code.add(p.s(cpsStmts))
  # emitting and adjusting for the selected OS and target is still done by
  # the code generator (but this is going to change in the future):
  genMainProc(m, code)

proc generateCode*(graph: ModuleGraph, mlist: sink ModuleList) =
  ## Entry point for C code-generation. Only the C code is generated -- nothing
  ## is written to disk yet.
  let
    config = graph.config

  generateMethodDispatchers(graph)

  var g = newModuleList(graph)

  # first create a module list entry for each input module. This has to happen
  # *before* the code generator is invoked.
  for key, val in mlist.modules.pairs:
    let m = newModule(g, val.sym, config)
    m.idgen = val.idgen

  # setup the module for the generated header, if required:
  if optGenIndex in config.globalOptions:
    let f =
      if config.headerFile.len > 0:
        AbsoluteFile(config.headerFile)
      else:
        config.projectFull
    g.generatedHeader = rawNewModule(g, mlist.modules[config.projectMainIdx2].sym,
      changeFileExt(completeCfilePath(config, f), hExt))
    incl g.generatedHeader.flags, isHeaderFile

  # generate the declarations for all globals first, so that the symbols all
  # have mangled names already; the order doesn't matter
  for key, m in mlist.modules.pairs:
    let bmod = g.modules[key.int]
    for s in m.structs.globals.items:
      defineGlobalVar(bmod, newSymNode(s))

    for s in m.structs.globals2.items:
      defineGlobalVar(bmod, newSymNode(s))

    for s in m.structs.threadvars.items:
      fillGlobalLoc(bmod, s, newSymNode(s))
      declareThreadVar(bmod, s, sfImportc in s.flags)

  # the main part: invoke the code generator for all top-level code
  for m in closed(mlist):
    let bmod = g.modules[m.sym.position]

    # process the declarative statements first:
    genTopLevelStmt(bmod, m.decls)
    # generate code for the init procedure:
    genTopLevelStmt(bmod, m.init.ast[bodyPos])
    # XXX: ^^ this is completely wrong. The init procedure should be treated
    #      like any other procedure, but ``cgen`` still depends on appending
    #      to it from all over the place. Untangling this will require
    #      multiple intermediate refactorings.

    # wrap up the main part of code generation for the module. Note that this
    # doesn't mean that they're closed for writing; invoking the code generator
    # for other modules' code can still add new code to this module's sections
    finalCodegenActions(graph, bmod, newNode(nkStmtList))

  # the main part of code generation is done. Generate the init procedure,
  # and then we're done. Note that no more dependencies (new globals,
  # procedure, constants, etc.) must be raised here
  for m in closed(mlist):
    let bmod = g.modules[m.sym.position]

    let
      hasInit = genInitCode(bmod)
      hasDatInit = genDatInitCode(bmod)

    if not hasInit:
      # communicate to the dead-code elimination later performed by
      # ``backends.generateMain`` that the init procedure has no content
      m.init.ast[bodyPos] = graph.emptyNode
    else:
      configureInitProcedure(m.init, getInitName(bmod))

    if hasDatInit:
      # the data-init procedure is currently empty by default. We signal that
      # the call to it should not be elided by changing the body to an empty
      # statement list
      # XXX: this is only a temporary solution until populating the procedure
      #      is the responsibility of the orchestrator or an earlier step
      m.dataInit.ast[bodyPos] = newNode(nkStmtList)
      configureInitProcedure(m.dataInit, getDatInitName(bmod))

    finalizeModule(bmod)
    if sfMainModule in m.sym.flags:
      finalizeMainModule(bmod)
      generateCodeForMain(bmod, mlist)

    # code generation for the module is done; it's C code will not change
    # anymore beyond this point
    # future direction: this part is going to be turned into an iterator
    # yielding the C file's content

  # the callsite still expects `graph.backend` to point to the ``BModuleList``
  # so that ``cgenWriteModules`` can query it
  # XXX: this is the wrong approach -- the code generator must not be
  #      responsible for writing the generated C translation-units to disk.
  graph.backend = g