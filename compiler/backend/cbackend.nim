## The code-generation orchestrator for the C backend. It takes the
## semantically analysed AST of the whole program as input and is responsible
## for assembling the final C code.
##
## Generating the actual code is implemented by `cgen`, with the orchestrator
## directing the code generator, managing emission of ``.inline`` procedures,
## and assembling the C code fragments produced into complete files (currently
## by dispatching to `cgen`).
##
## Inlining
## --------
##
## Inlining of procedures marked as ``.inline`` is currently not implemented
## at the |NimSkull| side, but is left to the optimizer of the used C
## compiler. For the C inliner to inline functions (when link-time
## optimizations are not employed, which by default, they aren't), the full
## C definition of an inline procedure must be present in all C translation
## units where the procedure is used.
##
## When the orchestrator encounters an alive inline procedure, it fully
## transforms and lowers its body (which is then cached), generates the code
## for it with the context of the module the procedure is first seen in, and
## then records all the direct dependencies it has on other inline procedures.
## In addition, an inline procedure is registered with each module it is
## directly used in.
##
## Once the code for all alive procedure has been generated (i.e., the main
## part of code generation done), the generated code of each inline procedure
## (along with its recorded transitive ``.inline`` dependencies) is emitted
## into the modules they were previously registered with.

import
  std/[
    hashes,
    intsets,
    tables
  ],
  compiler/ast/[
    ast,
    lineinfos,
    ndi
  ],
  compiler/backend/[
    backends,
    cgen,
    cgendata,
    extccomp
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/sem/[
    modulelowering
  ],
  compiler/utils/[
    containers,
    pathutils,
    ropes
  ]

import std/options as std_options

# XXX: reports are a legacy facility that is going to be phased out. A
#      note on how to move forward is left at each usage site in this
#      module
from compiler/front/msgs import localReport
from compiler/ast/reports import ReportKind
from compiler/ast/reports_sem import SemReport

type
  InlineProc = object
    ## Information about an inline procedure.
    sym: PSym
    body: PNode
      ## the fully processed body of the procedure

    deps: PackedSet[uint32]
      ## other inline procedures this procedure directly references
      ## (i.e., depends on)

  ModuleId = FileIndex

  InliningData = object
    ## Management data for the inlining logic.
    inlineProcs: Store[uint32, InlineProc]
      ## stores the additional information plus the generated code for each
      ## alive inline procedure
    inlineMap: Table[int, uint32]
      ## maps a symbol ID to the associated ``InlineProc`` ID

    inlined: OrdinalSeq[ModuleId, PackedSet[uint32]]
      ## for each module, the starting set of inline procedures that need
      ## to be duplicated into the module's corresponding C file

func hash(x: PSym): int = hash(x.id)

proc writeMangledLocals(p: BProc) =
  ## Writes the mangled names of `p`'s locals to the module's NDI file.
  for it in p.locals.items:
    # writing out mangled names for compiler-inserted variables (temporaries)
    # is not necessary (lode is guaranteed to be a symbol)
    if it.lode.sym.kind != skTemp:
      writeMangledName(p.module.ndi, it.lode.sym, it.r, p.config)

func registerInline(g: var InliningData, prc: PSym): uint32 =
  ## If not already registered, registers the inline procedure `prc` with
  ## `g`. This only sets up an ``InlineProc`` stub -- the entry is
  ## not populated yet.
  if prc.id in g.inlineMap:
    # XXX: double lookup
    result = g.inlineMap[prc.id]
  else:
    result = g.inlineProcs.add(InlineProc(sym: prc))
    g.inlineMap[prc.id] = result

proc dependOnCompilerProc(g: var InliningData, d: var DiscoveryData, m: ModuleId,
                          graph: ModuleGraph, name: string) =
  let sym = getCompilerProc(graph, name)
  assert sym != nil, "compilerproc missing"
  register(d, sym)
  # a compilerproc can also be an inline procedure:
  if sym.typ.callConv == ccInline:
    g.inlined[m].incl registerInline(g, sym)

func dependOnInline(g: var InliningData, m: ModuleId, id: Option[uint32], dep: PSym) =
  ## Raise a dependency on an inline procedure `dep`, with `m` being the
  ## current module and `id` the ID of the inline procedure the in whose
  ## context the dependency is raised.
  let other = registerInline(g, dep)
  g.inlined[m].incl other

  if id.isSome:
    # remember the dependency:
    g.inlineProcs[id.unsafeGet].deps.incl other

proc prepare(g: BModuleList, d: var DiscoveryData) =
  ## Emits the definitions for constants, globals, and threadvars discovered
  ## as part of producing the current event.

  # emit definitions for constants:
  for _, s in visit(d.constants):
    genConstDefinition(g.modules[moduleId(s)], s)

  # emit definitions for the lifted globals we've discovered:
  for _, s in visit(d.globals):
    defineGlobalVar(g.modules[moduleId(s)], newSymNode(s))

  for _, s in visit(d.threadvars):
    let bmod = g.modules[moduleId(s)]
    fillGlobalLoc(bmod, s, newSymNode(s))
    declareThreadVar(bmod, s, sfImportc in s.flags)

proc processEvent(g: BModuleList, inl: var InliningData, discovery: var DiscoveryData, partial: var Table[PSym, BProc], evt: sink BackendEvent) =
  ## The orchestrator's event processor.
  let bmod = g.modules[evt.module.int]

  prepare(g, discovery)

  proc handleInline(inl: var InliningData, m: ModuleId, prc: PSym,
                    body: MirTree): Option[uint32] {.nimcall.} =
    ## Registers the dependency on inline procedure that `body` has
    ## with module `m` and, if an inline procedure, also `prc`. Returns
    ## the inline ID of the `prc`, or 'none', if `prc` is not an inline
    ## procedure.
    result =
      if prc.typ.callConv == ccInline:
        some(registerInline(inl, prc))
      else:
        none(uint32)

    # remember usages of inline procedures for the purpose of emitting
    # them later:
    for dep in deps(body, NonMagics):
      if dep.kind in routineKinds and dep.typ.callConv == ccInline:
        dependOnInline(inl, m, result, dep)

  proc processLate(bmod: BModule, data: var DiscoveryData, inl: var InliningData,
                   m: ModuleId, inlineId: Option[uint32]) {.nimcall.} =
    ## Registers late-dependencies raised by the code generator with `data`.
    ##
    ## The code generator currently emits calls to ``.compilerprocs``, which
    ## the alive set needs to know about.
    for it in bmod.extra.items:
      register(data, it)
      if it.typ.callConv == ccInline:
        dependOnInline(inl, m, inlineId, it)

    # we processed/consumed all elements
    bmod.extra.setLen(0)

    # we also need to update the alive entities with the used hooks:
    for bmod, s in bmod.g.hooks.items:
      # queue the hook from the module it was used from:
      registerLate(data, s, bmod.module.position.ModuleId)

    bmod.g.hooks.setLen(0)

  case evt.kind
  of bekModule:
    # the code generator emits a call for setting up the TLS, which is a
    # procedure dependency that needs to be communicated
    if sfSystemModule in bmod.module.flags and
       emulatedThreadVars(g.config):
      dependOnCompilerProc(inl, discovery, evt.module, g.graph,
                           "initThreadVarsEmulation")

  of bekPartial:
    # register inline dependencies:
    let inlineId = handleInline(inl, evt.module, evt.sym, evt.body.tree)

    var p = getOrDefault(partial, evt.sym)
    if p == nil:
      p = startProc(g.modules[evt.module.int], evt.sym)
      partial[evt.sym] = p

    let body = generateAST(g.graph, bmod.idgen, evt.sym, evt.body)
    # emit into the procedure:
    genStmts(p, body)

    processLate(bmod, discovery, inl, evt.module, inlineId)
  of bekProcedure:
    let inlineId = handleInline(inl, evt.module, evt.sym, evt.body.tree)

    # mark the procedure as declared, first. This gets around an unnecessary
    # emit of the prototype in the case of self-recursion
    bmod.declaredThings.incl(evt.sym.id)
    let
      body = generateAST(g.graph, bmod.idgen, evt.sym, evt.body)
      p    = startProc(bmod, evt.sym, body)

    # we can't generate with ``genProc`` because we still need to output
    # the mangled names
    genStmts(p, body)
    writeMangledLocals(p)
    let r = finishProc(p, evt.sym)

    if inlineId.isSome:
      # remember the generated body:
      inl.inlineProcs[inlineId.unsafeGet].body = body

    # add the C function to the C file's procedure section:
    bmod.s[cfsProcs].add(r)

    processLate(bmod, discovery, inl, evt.module, inlineId)
  of bekImported:
    # an imported procedure available
    symInDynamicLib(bmod, evt.sym)

proc emit(m: BModule, inl: InliningData, prc: InlineProc, r: var Rope) =
  ## Emits the inline procedure `prc` and all its inline dependencies into
  ## `r`.

  # guard against recurisve inline procedures and the procedure having
  # been emitted already
  if m.declaredThings.containsOrIncl(prc.sym.id):
    return

  # emit the dependencies first:
  for dep in prc.deps.items:
    emit(m, inl, inl.inlineProcs[dep], r)

  assert prc.body != nil, "missing body"
  # conservatively emit a prototype for all procedures to make sure that
  # recursive procedures work:
  genProcPrototype(m, prc.sym)
  r.add(genProc(m, prc.sym, prc.body))

proc generateHeader(g: BModuleList, inl: InliningData, data: DiscoveryData,
                    s: PSym): BModule =
  ## Generates a C header file containing the prototypes of all
  ## ``.exportc``'ed entities. For inline procedure, the full definition is
  ## emitted.
  let f =
    if g.config.headerFile.len > 0: AbsoluteFile(g.config.headerFile)
    else:                           g.config.projectFull

  result = rawNewModule(g, s,
                        changeFileExt(completeCfilePath(g.config, f), hExt))
  result.flags.incl(isHeaderFile)

  # fill the header file with all exported entities:
  for _, s in all(data.constants):
    if sfExportc in s.flags:
      useConst(result, s)

  for _, s in all(data.globals):
    if sfExportc in s.flags:
      genVarPrototype(result, newSymNode(s))

  for _, s in all(data.procedures):
    # we don't want to emit compilerprocs (which are automatically marked
    # as ``exportc``)
    if {sfExportc, sfCompilerProc} * s.flags == {sfExportc}:
      if ccInline == s.typ.callConv:
        # inline procedure get inlined into the header
        let id = inl.inlineMap[s.id]
        var r: string
        emit(result, inl, inl.inlineProcs[id], r)
        result.s[cfsProcs].add(r)
      else:
        # for non-inline procedure, only a prototype is placed in the header
        genProcPrototype(result, s)

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
  genStmts(p, body)
  var code: string
  code.add(p.s(cpsLocals))
  code.add(p.s(cpsInit))
  code.add(p.s(cpsStmts))
  # emitting and adjusting for the selected OS and target is still done by
  # the code generator:
  # XXX: ^^ this is going to change in the future
  genMainProc(m, code)

proc generateCode*(graph: ModuleGraph, g: BModuleList, mlist: sink ModuleList)

proc generateCode*(graph: ModuleGraph, mlist: sink ModuleList) =
  ## Entry point for C code generation. Only the C code is generated -- nothing
  ## is written to disk yet.
  let
    config = graph.config

  var g = newModuleList(graph)

  # first create a module list entry for each input module. This has to happen
  # *before* the code generator is invoked.
  for key, val in mlist.modules.pairs:
    let m = newModule(g, val.sym, config)
    m.idgen = val.idgen

  generateCode(graph, g, mlist)

  # the callsite still expects `graph.backend` to point to the ``BModuleList``
  # so that ``cgenWriteModules`` can query it
  # XXX: this is the wrong approach -- the code generator must not be
  #      responsible for writing the generated C translation-units to disk.
  graph.backend = g

proc generateCode*(graph: ModuleGraph, g: BModuleList, mlist: sink ModuleList) =
  ## Implements the main part of the C code-generation orchestrator. Expects an
  ## already populated ``BModuleList``.

  # pre-process the init procedures:
  for key, m in mlist.modules.pairs:
    let bmod = g.modules[key.int]

    # the init and data-init procedures use special names in the
    # generated code:
    m.init.extname = getInitName(bmod)
    m.dataInit.extname = getDatInitName(bmod)

    # mark the init procedure so that the code generator can detect and
    # special-case it:
    m.init.flags.incl sfTopLevel

  # ----- main event processing -----
  let
    config = BackendConfig()

  var
    inl:       InliningData
    discovery: DiscoveryData
    partial:   Table[PSym, BProc]

  inl.inlined.newSeq(g.modules.len)

  # discover and generate code for all alive procedures:
  for ac in process(graph, mlist, discovery, NonMagics, config):
    processEvent(g, inl, discovery, partial, ac)

  # finish the partial procedures:
  for s, p in partial.pairs:
    writeMangledLocals(p)
    p.module.s[cfsProcs].add(finishProc(p, s))

  # -------------------------
  # all alive entities must have been discovered when reaching here; it is
  # not allowed to raise new ones beyond this point

  block:
    # write the mangled names of the various entities to the NDI files
    for it in g.procs.items:
      let
        s = it.sym
        m = g.modules[moduleId(s)]
      writeMangledName(m.ndi, s, it.name, g.config)
      # parameters:
      for p in it.params.items:
        if p.k != locNone: # not all parameters have locs
          writeMangledName(m.ndi, s, p.r, g.config)

    template write(loc: TLoc) =
      let s = loc.lode.sym
      writeMangledName(g.modules[moduleId(s)].ndi, s, loc.r, g.config)

    for it in g.globals.items:
      write(it)

    for it in g.consts.items:
      write(it)

  # now emit a duplicate of each inline procedure into the C files where the
  # procedure is used. Due to how ``cgen`` currently works, this means
  # generating C code for the procedure again
  for i, m in mlist.modules.pairs:
    let bmod = g.modules[i.int]

    var r: Rope
    for it in inl.inlined[i].items:
      emit(bmod, inl, inl.inlineProcs[it], r)

    # append the generated procedures to the module:
    bmod.s[cfsProcs].add(r)

  if optGenIndex in graph.config.globalOptions:
    # XXX: the header is just a normal C file artifact of ``generateCode``,
    #      don't store it with ``BModuleList``
    g.generatedHeader = generateHeader(g, inl, discovery,
                                       mlist[graph.config.projectMainIdx2].sym)

  # not pretty, but here's the earliest point where we know about the set of
  # all actually-used dynamic libraries
  # XXX: instead of reporting them here, we could return the list to the
  #      caller, which is in a better position to decide what to do with
  #      it
  for lib in discovery.libs.items:
    localReport(graph.config):
      SemReport(kind: rsemHintLibDependency,
                str: graph.getLib(lib).path.strVal)

  # finalize code generation for the modules and generate and emit the code
  # for the 'main' procedure:
  for m in closed(mlist):
    let
      bmod = g.modules[m.sym.position]
      hasDatInit = genDatInitCode(bmod)

    if hasDatInit:
      # the data-init procedure is currently empty by default. We signal that
      # the call to it should not be elided, by changing the body to an empty
      # statement list
      # XXX: this is only a temporary solution until populating the procedure
      #      is the responsibility of the orchestrator (or an earlier step)
      m.dataInit.ast[bodyPos] = newNode(nkStmtList)

    # XXX: ``cgenWriteModules`` still expects a populated ``modulesClosed``
    #      list
    g.modulesClosed.add bmod

    finalizeModule(bmod)
    if sfMainModule in m.sym.flags:
      finalizeMainModule(bmod)
      generateCodeForMain(bmod, mlist)

    # code generation for the module is done; its C code will not change
    # anymore beyond this point
    # future direction: this part is going to be turned into an iterator
    # yielding the C file's content