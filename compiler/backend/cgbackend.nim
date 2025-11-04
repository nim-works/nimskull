## Implements a generic backend, which is effectively a driver for transf,
## mid-end processing, and producing CGIR modules. For efficiency, CGIR
## production currently uses a "jumbo" approach, where the code for the full
## static program is compiled into a single CGIR module.

import
  std/[
    algorithm,
    hashes,
    sequtils,
    tables
  ],
  std/private/[
    containers
  ],
  experimental/[
    colortext # for output coloring
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    ast_idgen,
    idents,
    lineinfos
  ],
  compiler/backend/[
    backends,
    ccgutils,
    cgir2,
    mir2cg,
    pretty,
    validation
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/mir/[
    mirbodies,
    mirbridge,
    mirgen,
    mirenv,
    mirtrees,
    mirtypes
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/sem/[
    modulelowering
  ],
  compiler/utils/[
    tracer
  ]

import std/options as std_options

from compiler/ast/ast import id, newNode, newTree, newSymNode, copySym

# TODO: move the `Emit` type to some other module and remove the dependency
#       on `cgen`
from compiler/backend/cgen import Emit

# XXX: reports are a legacy facility that is going to be phased out
from compiler/ast/reports import ReportKind
from compiler/ast/reports_sem import SemReport

export mir2cg.Capability

type
  ModuleId = FileIndex

  BModule = object
    ## Accumulator for single module artifacts.
    sym: PSym
    idgen: IdGenerator

    globals: seq[(StringId, int32)]
    procs: seq[(StringId, int32)]
    emits: Emit

  BModuleList = object
    ## Bundles all global processor state.
    graph: ModuleGraph
    config: ConfigRef
    env: MirEnv
    modules: Table[ModuleId, BModule]
    total: Context
      ## CGIR translation context for the single output module

  OutModule* = object
    ## A view into the total CCGIR module, derived from a source module. The
    ## entities are ordered by their sem-level ID.
    sym*: PSym
    globals*: seq[StringId]
      ## globals defined in the module
    procs*: seq[StringId]
      ## procedures defined in the module
    emits*: Emit
      ## top-level emit/asm statements for the module

proc prepare(g: var BModuleList, n: MirNode) =
  ## Handles early discovery of new entity `n`.
  case n.kind
  of mnkProc:
    # the definition is emitted once the body is available
    let s = g.env[n.prc]
    if sfImportc in s.flags and {exfNoDecl, exfDynamicLib} * s.extFlags == {}:
      # importc'ed procedures
      discard g.total.defineForeignProc(g.env, n.prc)
  of mnkConst:
    discard "registered once the body is available"
  of mnkGlobal:
    let s = g.env[n.global]
    let name = g.total.name(s)
    if name.isSome:
      discard g.total.defineGlobal(g.env, n.global)
      if sfImportc notin s.flags:
        g.modules[moduleId(s).FileIndex].globals.add (name.unsafeGet, s.itemId.item)
  else:
    unreachable(n.kind)

proc echoOutput(config: ConfigRef, m: CgModule, s: PSym, name: StringId) =
  let pname = s.name.s
  if irCgir in config.toDebugIr or config.isDebugEnabled(irCgir, pname):
    config.writeln("-- CGIR: " & pname)
    config.writeln(render(m, m.ast, m.procs[name]))
    config.writeln("-- end")

proc addProc(g: var BModuleList, id: ProcedureId, s: PSym, body: sink MirBody) =
  echoOutput(g.graph.config, s, body, g.env) # MIR logging
  g.graph.config.timeTracer.traceSym(tikCodegen, s)
  let module = moduleId(s).FileIndex
  let name = g.total.defineProc(g.env, id, body)
  echoOutput(g.graph.config, g.total.current, s, name) # CGIR logging
  if s.typ.callConv != ccInline:
    # inline procedures don't have a "home" module
    g.modules[module].procs.add (name, s.itemId.item)

proc processEvent(g: var BModuleList,
                  discovery: var DiscoveryData,
                  partial: var Table[ProcedureId, seq[PSym]],
                  evt: sink BackendEvent) =
  ## The orchestrator's event processor.
  case evt.kind
  of bekDiscovered:
    prepare(g, evt.entity)
  of bekModule:
    if sfSystemModule in g.modules[evt.module].sym.flags:
      # the 'NimMain' procedure has a hidden dependency on
      # `nimUnhandledException`
      discard g.env.procedures.add(
        g.graph.getCompilerProc("nimUnhandledException"))

      if optNoMain notin g.graph.config.globalOptions:
        # add the C entry point to the set of live procedures
        discard g.env.procedures.add(g.graph.getCompilerProc("c_entry"))
  of bekEmit:
    let stmt =  translateTopLevelEmit(g.total, g.env, evt.stmt)
    case evt.section
    of secIncludes:   g.modules[evt.module].emits.includes.add stmt
    of secProcedures: g.modules[evt.module].emits.procs.add stmt
    of secTypes:      g.modules[evt.module].emits.types.add stmt
    of secVars:       g.modules[evt.module].emits.globals.add stmt
  of bekConstant:
    # emit the definition now that the body is available
    let s = g.env[evt.cnst]
    if exfNoDecl notin s.extFlags:
      let name = g.total.defineConst(g.env, evt.cnst)
      if sfImportc notin s.flags:
        g.modules[moduleId(s).FileIndex].globals.add (name, s.itemId.item)
  of bekPartial:
    # turn the fragment into a full procedure, add it to the build, and
    # remember it for later
    let module = moduleId(evt.sym).FileIndex
    let nsym = copySym(evt.sym, nextSymId g.modules[module].idgen)
    nsym.options.excl optStackTrace

    let id = g.env.procedures.add(nsym)
    g.addProc(id, nsym, evt.body)
    partial.mgetOrPut(evt.id, @[]).add nsym
  of bekProcedure:
    g.addProc(evt.id, evt.sym, evt.body)
  of bekImported:
    let s = g.env[evt.id]
    let name = g.total.defineDynlibProc(g.env, evt.id)
    # procedures imported from dynamic libraries are really globals
    g.modules[moduleId(s).FileIndex].globals.add (name, s.itemId.item)

proc getInitName(m: PSym): string =
  if sfMainModule in m.flags:
    # use a known name to make troubleshooting a bit easier
    result = "NimMainModule"
  else:
    if {sfSystemModule, sfMainModule} * m.flags == {}:
      result = m.owner.name.s.mangle
      result.add "_"
    result.add m.name.s.mangle
    result.add "Init000"

proc validate(conf: ConfigRef, m: CgModule): bool =
  ## Makes sure `m` is well-formed, logging all problems and returning 'false'
  ## when it's not.
  # TODO: integrate error emission with the compiler's message/
  #       diagnostics subystem
  var foundError = false
  checkSyntax(m,
    proc(m: CgModule, ast: Ast, ctx: MsgContext, err: sink string) =
      foundError = true
      conf.write("error: " + fgRed)
      conf.writeln(err)
      conf.writeln(render(m, ast, ctx.where))
  )

  if not foundError:
    checkSemantics(m,
      proc(m: CgModule, ast: Ast, ctx: MsgContext, err: sink string) =
        foundError = true
        conf.writeln(render(m, ast, ctx.where))
        conf.write("error: " + fgRed)
        conf.writeln(err)
    )

  result = not foundError

proc generateCode*(graph: ModuleGraph, mlist: sink ModuleList,
                   caps: set[Capability]):
    tuple[all: CgModule, modules: seq[OutModule]] =
  ## Takes the whole-program representation `mlist` and generates a single CG
  ## module containing all live code for it.
  var g = BModuleList(
    graph: graph,
    config: graph.config,
    env: initMirEnv(graph),
    total: initContext(graph, caps)
  )

  for key, m in mlist.modules.pairs:
    g.modules[key] = BModule(sym: m.sym, idgen: m.idgen)
    # give the init procedure an easier-to-guess external name:
    m.init.extname = getInitName(m.sym)

  # ----- main event processing -----
  let
    config = BackendConfig(
      tconfig: TranslationConfig(
        magicsToKeep: NonMagics
      )
    )

  var
    discovery: DiscoveryData
    partial:   Table[ProcedureId, seq[PSym]]

  # discover and generate code for all live procedures:
  for ac in process(graph, mlist, g.env, discovery, config):
    processEvent(g, discovery, partial, ac)

  # note: all entities registered with the MIR env beyond this point need to be
  # translated manually

  # finish the partial procedures
  for id, procs in partial.pairs:
    # the body consists of a series of calls to all the sub procedures
    var body = newNode(nkStmtList)
    for it in procs.items:
      body.add newTree(nkCall, newSymNode(it))

    let s = g.env[id]
    let mbody = generateCode(graph, g.env, s, config.tconfig, body)
    g.addProc(id, s, mbody)

  if optThreads in graph.config.globalOptions:
    let
      s = generateThreadTeardownProc(graph, mlist.mainModule.idgen, mlist)
      id = g.env.procedures.add(s)
      mbody = generateCode(graph, g.env, s, config.tconfig, s.ast[bodyPos])
    g.addProc(id, s, mbody)

  # now that all the required module init procedures are available, create the
  # 'NimMain' procedure and add it to the main module:
  block:
    let
      s = generateNimMain(graph, mlist.mainModule.idgen, mlist)
      id = g.env.procedures.add(s)
    # note: the body uses post-transf AST and can thus be translated to
    # MIR directly
    let body = generateCode(graph, g.env, s, config.tconfig, s.ast[bodyPos])
    g.addProc(id, s, body)

  # register the RTTI globals with their respective home module:
  for (name, id) in g.total.getRtti().items:
    let item = (name, -id.item)
    if id.module.FileIndex == InvalidFileIdx:
      # globals that don't have a home module are registered with the main module
      # XXX: ideally, these globals would be placed into a dedicated output
      #      module that only contains the definitions of RTTI for
      #      structural types
      g.modules[mainModule(mlist).sym.position.FileIndex].globals.add item
    else:
      g.modules[id.module.FileIndex].globals.add item

  # tacked on-support for ABI checks. For every imported typed, add a top-level
  # emit handling the size enforcement
  # TODO: make the order relate to the source layout by sorting the per-module
  #       types by their item ID first
  if g.config.isDefined("checkAbi"):
    for id in canonical(g.env.types):
      if g.env.types.headerFor(id, Lowered).kind == tkImported and
         g.env.types.headerFor(id, Lowered).size(g.env.types) >= 0:
        g.modules[moduleId(g.env.types[id].sym).FileIndex].emits.procs.add:
          genAbiCheck(g.total, g.env, id)

  # not pretty, but here's the earliest point where we know about the set of
  # all actually-used dynamic libraries
  # XXX: instead of reporting them here, we could return the list to the
  #      caller, which is in a better position to decide what to do with
  #      it
  for lib in discovery.libs.items:
    localReport(graph.config):
      SemReport(kind: rsemHintLibDependency,
                str: graph.getLib(lib).path.strVal)

  let m = close(g.total)

  if g.config.isDefined("validateCgir") and not validate(g.config, m):
    quit(1)

  result[0] = m
  # prepare the module view accumulators
  for it in g.modules.mvalues:
    if it.globals.len > 0 or it.procs.len > 0:
      # give the entities an order that is tied (roughly) to their
      # source location
      sort(it.globals, proc(a, b: auto): int = a[1] - b[1])
      sort(it.procs, proc(a, b: auto): int = a[1] - b[1])
    else:
      it.sym = nil # mark as empty

  # return the modules in the order they were closed in
  for it in mlist.modulesClosed.items:
    var m: BModule
    discard g.modules.pop(it, m)
    if m.sym != nil:
      result.modules.add OutModule(
        sym: m.sym,
        globals: m.globals.mapIt(it[0]),
        procs: m.procs.mapIt(it[0]),
        emits: m.emits
      )
