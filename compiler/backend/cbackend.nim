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
    ast_query,
    ast_types,
    lineinfos,
    ndi
  ],
  compiler/backend/[
    backends,
    cgen,
    cgendata,
    cgir,
    extccomp
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    mirbridge,
    mirenv,
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
    idioms,
    pathutils,
    platform,
    ropes
  ]

import std/options as std_options

from compiler/ast/ast import id, newNode, newTree, newSymNode

# XXX: reports are a legacy facility that is going to be phased out. A
#      note on how to move forward is left at each usage site in this
#      module
from compiler/front/msgs import localReport
from compiler/ast/reports import ReportKind
from compiler/ast/reports_sem import SemReport

type
  InlineProc = object
    ## Information about an inline procedure.
    id: ProcedureId
    body: Body
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
    inlineMap: Table[ProcedureId, uint32]
      ## maps a procedure ID to the associated ``InlineProc`` ID

    inlined: OrdinalSeq[ModuleId, PackedSet[uint32]]
      ## for each module, the starting set of inline procedures that need
      ## to be duplicated into the module's corresponding C file

proc writeMangledLocals(p: BProc) =
  ## Writes the mangled names of `p`'s locals to the module's NDI file.
  for i, it in p.locals.pairs:
    # only write a mapping for locals that have both a user-provided
    # and mangled name (compile-time-only parameters don't have one)
    if p.body[i].name != nil and it.r.len > 0:
      writeMangledName(p.module.ndi, it.lode.info, p.body[i].name, it.r,
                       p.config)

func registerInline(g: var InliningData, prc: ProcedureId): uint32 =
  ## If not already registered, registers the inline procedure `prc` with
  ## `g`. This only sets up an ``InlineProc`` stub -- the entry is
  ## not populated yet.
  if prc in g.inlineMap:
    # XXX: double lookup
    result = g.inlineMap[prc]
  else:
    result = g.inlineProcs.add(InlineProc(id: prc))
    g.inlineMap[prc] = result

proc dependOnCompilerProc(g: var InliningData, env: var MirEnv, m: ModuleId,
                          graph: ModuleGraph, name: string) =
  let sym = getCompilerProc(graph, name)
  assert sym != nil, "compilerproc missing"
  let id = env.procedures.add(sym)
  # a compilerproc can also be an inline procedure:
  if sym.typ.callConv == ccInline:
    g.inlined[m].incl registerInline(g, id)

func dependOnInline(g: var InliningData, m: ModuleId, id: Option[uint32],
                    dep: ProcedureId) =
  ## Raise a dependency on an inline procedure `dep`, with `m` being the
  ## current module and `id` the ID of the inline procedure the in whose
  ## context the dependency is raised.
  let other = registerInline(g, dep)
  g.inlined[m].incl other

  if id.isSome:
    # remember the dependency:
    g.inlineProcs[id.unsafeGet].deps.incl other

proc prepare(g: BModuleList, n: MirNode) =
  ## Responds to the discovery of entity `n`.
  case n.kind
  of mnkProc, mnkConst:
    # the definition is emitted once the body is available
    discard "nothing to do"
  of mnkGlobal:
    let
      s = g.env[n.global]
      bmod = g.modules[moduleId(s)]

    # can be either a threadvar or normal global variable
    if sfThread in s.flags:
      fillGlobalLoc(bmod, n.global)
      declareThreadVar(bmod, n.global, sfImportc in s.flags)
    else:
      defineGlobalVar(bmod, n.global)
  else:
    unreachable(n.kind)

proc processEvent(g: BModuleList, inl: var InliningData,
                  discovery: var DiscoveryData,
                  partial: var Table[ProcedureId, BProc],
                  evt: sink BackendEvent) =
  ## The orchestrator's event processor.
  let bmod = g.modules[evt.module.int]

  proc handleInline(inl: var InliningData, env: MirEnv, m: ModuleId, prc: PSym,
                    body: MirTree): Option[uint32] {.nimcall.} =
    ## Registers the dependency on inline procedure that `body` has
    ## with module `m` and, if an inline procedure, also `prc`. Returns
    ## the inline ID of the `prc`, or 'none', if `prc` is not an inline
    ## procedure.
    result =
      if prc.typ.callConv == ccInline:
        some(registerInline(inl, env.procedures[prc]))
      else:
        none(uint32)

    # remember usages of inline procedures for the purpose of emitting
    # them later:
    for dep in deps(body):
      if dep.kind == mnkProc and env[dep.prc].typ.callConv == ccInline:
        dependOnInline(inl, m, result, dep.prc)

  proc processLate(bmod: BModule, data: var DiscoveryData, inl: var InliningData,
                   m: ModuleId, inlineId: Option[uint32]) {.nimcall.} =
    ## Registers late-dependencies raised by the code generator with `data`.
    ##
    ## The code generator is currently responsible for discovering some type-
    ## attached procedures, and procedure processing needs to know about which
    ## module they're registered from.
    for it in bmod.extra.items:
      if bmod.g.env[it].typ.callConv == ccInline:
        dependOnInline(inl, m, inlineId, it)

    # we processed/consumed all elements
    bmod.extra.setLen(0)

    # we also need to update the alive entities with the used hooks:
    for bmod, id in bmod.g.hooks.items:
      # the hook needs to be queued from the module it was first referenced
      # from:
      setModuleOverride(data, id, bmod.module.position.ModuleId)

    bmod.g.hooks.setLen(0)

  case evt.kind
  of bekDiscovered:
    prepare(g, evt.entity)
  of bekModule:
    # the code generator emits a call for setting up the TLS, which is a
    # procedure dependency that needs to be communicated
    if sfMainModule in bmod.module.flags and
       emulatedThreadVars(g.config):
      dependOnCompilerProc(inl, g.env, evt.module, g.graph,
                           "initThreadVarsEmulation")

  of bekConstant:
    # emit the definition now that the body is available
    let s = g.env[evt.cnst]
    genConstDefinition(g.modules[moduleId(s)], evt.cnst)
  of bekPartial:
    # register inline dependencies:
    let inlineId = handleInline(inl, g.env, evt.module, evt.sym, evt.body.code)

    var p = getOrDefault(partial, evt.id)
    if p == nil:
      p = startProc(g.modules[evt.module.int], evt.id, Body())
      partial[evt.id] = p

    let body = generateIR(g.graph, bmod.idgen, g.env, evt.sym, evt.body)
    # emit into the procedure:
    genPartial(p, merge(p.body, body))

    processLate(bmod, discovery, inl, evt.module, inlineId)
  of bekProcedure:
    let inlineId = handleInline(inl, g.env, evt.module, evt.sym, evt.body.code)

    # mark the procedure as declared, first. This gets around an unnecessary
    # emit of the prototype in the case of self-recursion
    bmod.declaredThings.incl(evt.sym.id)
    let
      body = generateIR(g.graph, bmod.idgen, g.env, evt.sym, evt.body)
      p    = startProc(bmod, evt.id, body)

    # we can't generate with ``genProc`` because we still need to output
    # the mangled names
    genStmts(p, p.body.code)
    writeMangledLocals(p)
    let r = finishProc(p, evt.id)

    if inlineId.isSome:
      # remember the generated body:
      inl.inlineProcs[inlineId.unsafeGet].body = body

    # add the C function to the C file's procedure section:
    bmod.s[cfsProcs].add(r)

    processLate(bmod, discovery, inl, evt.module, inlineId)
  of bekImported:
    # an imported procedure became available
    symInDynamicLib(bmod, evt.id)

proc emit(m: BModule, inl: InliningData, prc: InlineProc,
          r: var Rope) =
  ## Emits the inline procedure `prc` and all its inline dependencies into
  ## `r`.

  # guard against recurisve inline procedures and the procedure having
  # been emitted already
  if m.declaredThings.containsOrIncl(m.g.env[prc.id].id):
    return

  # emit the dependencies first:
  for dep in prc.deps.items:
    emit(m, inl, inl.inlineProcs[dep], r)

  assert prc.body.code != nil, "missing body"
  # conservatively emit a prototype for all procedures to make sure that
  # recursive procedures work:
  genProcPrototype(m, prc.id)
  r.add(genProc(m, prc.id, prc.body))

proc generateHeader(g: BModuleList, inl: InliningData, env: MirEnv,
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
  for id, s in items(env.constants):
    if sfExportc in s.flags:
      useConst(result, id)

  for id, s in items(env.globals):
    if {sfExportc, sfThread} * s.flags == {sfExportc}:
      genVarPrototype(result, id)

  for id, s in items(env.procedures):
    # we don't want to emit compilerprocs (which are automatically marked
    # as ``exportc``)
    if {sfExportc, sfCompilerProc} * s.flags == {sfExportc}:
      if ccInline == s.typ.callConv:
        # inline procedure get inlined into the header
        let iid = inl.inlineMap[id]
        var r: string
        emit(result, inl, inl.inlineProcs[iid], r)
        result.s[cfsProcs].add(r)
      else:
        # for non-inline procedure, only a prototype is placed in the header
        genProcPrototype(result, id)

proc generateCodeForMain(m: BModule, modules: ModuleList) =
  ## Generates and emits the C code for the program's or library's entry
  ## point.

  # generate the body:
  let body = newNode(nkStmtList)

  # if TLS emulation is used, the storage has to be setup before any other
  # code is run:
  if emulatedThreadVars(m.config) and m.config.target.targetOS != osStandalone:
    body.add newTree(nkCall,
      ast.newSymNode(m.g.graph.getCompilerProc("initThreadVarsEmulation")))

  generateMain(m.g.graph, modules, body)
  if {optGenStaticLib, optGenDynLib, optNoMain} * m.config.globalOptions == {}:
    # only emit the teardown logic when we're building a standalone program
    # XXX: the teardown logic should be generated into a separate C function
    #      in this case; otherwise there's no way to free the module structs
    generateTeardown(m.g.graph, modules, body)

  # now generate the C code for the body:
  let p = newProc(nil, m)
  # we don't want error or stack-trace code in the main procedure:
  p.flags.incl nimErrorFlagDisabled
  p.options = {}
  p.body = canonicalize(m.g.graph, m.idgen, m.g.env, m.module, body,
                        TranslationConfig())

  genStmts(p, p.body.code)
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
    config = BackendConfig(tconfig: TranslationConfig(magicsToKeep: NonMagics))

  var
    inl:       InliningData
    discovery: DiscoveryData
    partial:   Table[ProcedureId, BProc]

  inl.inlined.newSeq(g.modules.len)

  # discover and generate code for all alive procedures:
  for ac in process(graph, mlist, g.env, discovery, config):
    processEvent(g, inl, discovery, partial, ac)

  # finish the partial procedures:
  for id, p in partial.pairs:
    writeMangledLocals(p)
    p.module.s[cfsProcs].add(finishProc(p, id))

  # -------------------------
  # all alive entities must have been discovered when reaching here; it is
  # not allowed to raise new ones beyond this point

  block:
    # write the mangled names of the various entities to the NDI files
    for id, loc in g.procs.pairs:
      let s = g.env[id]
      writeMangledName(g.modules[moduleId(s)].ndi, s, loc.name, g.config)

    template write(loc: TLoc, id: untyped) =
      let s = g.env[id]
      writeMangledName(g.modules[moduleId(s)].ndi, s, loc.r, g.config)

    for id, loc in g.globals.pairs:
      write(loc, id)

    for id, loc in g.consts.pairs:
      write(loc, id)

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
    g.generatedHeader = generateHeader(g, inl, g.env,
                                       mlist[graph.config.projectMainIdx].sym)

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
