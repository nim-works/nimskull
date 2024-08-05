## The code-generation orchestrator for the C backend.

import
  std/[
    algorithm,
    hashes,
    intsets,
    os,
    tables
  ],
  compiler/ast/[
    ast_idgen,
    ast_types,
    lineinfos
  ],
  compiler/backend/[
    backends,
    cformat,
    cgendata,
    cgen,
    cir,
    extccomp
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    mirbodies,
    mirenv,
    mirtrees
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    modulelowering
  ],
  compiler/utils/[
    containers,
    idioms,
    measure,
    pathutils,
    ropes
  ]

# XXX: move toFullPath somewhere else, like ``options`` (where ``ConfigRef``
#      resides)
from compiler/front/msgs import toFullPath, localReport

# XXX: imports for the legacy reports
import compiler/ast/report_enums
from compiler/ast/reports_sem import SemReport,
  reportStr

type
  ModuleId = FileIndex

  BModule* = object
    ## Per-module data. A ``BModule`` instance usually corresponds to a
    ## |NimSkull| module, but doesn't necessarily have to.
    idgen*: IdGenerator

    all: CombinedCAst
      ## the C AST of everything part of the module: functions, globals, etc.
    procs: seq[tuple[id: ProcedureId, body: CNodeIndex]]
      ## all procedures attached to the module (except inline procedures)
    globals: seq[tuple[id: GlobalId, def: CNodeIndex]]
      ## all globals attached to the module
    constants: seq[tuple[id: ConstId, def: CNodeIndex]]
      ## all constants attached to the module

  BModuleList* = object
    ## The "top level" type for the orchestrator, owning all state related
    ## to code generation.
    graph: ModuleGraph

    modules*: OrdinalSeq[ModuleId, BModule]

    all: CombinedCAst
      ## the C AST of everything not directly attached to a single module,
      ## such as declarations, inline procedure bodies, etc.

    inline: Table[ProcedureId, CNodeIndex]
      ## inline procedure -> body. Inline procedures are emitted into all C
      ## TUs they're used in, so their bodies are stored globally
    types: Table[TypeId, tuple[hash: Hash; decl, def: CNodeIndex]]

    # the declarations for the various entities are needed across modules.
    # They're generated once and are then cached here
    procs: SeqMap[ProcedureId, CNodeIndex]
    consts: SeqMap[ConstId, CNodeIndex]
    globals: SeqMap[GlobalId, CNodeIndex]
    data: Table[DataId, tuple[hash: Hash, node: CNodeIndex]]
      ## not all data entries need to be used in practice, so a table is used

  PartialTable = Table[ProcedureId, MirBody]
    ## Table for holding the incremental procedures

  Output = ref object of RootObj
    ## The interface with the legacy backend management.
    modules: seq[tuple[m: PSym, content: string]]
      ## all modules to add to the build, together with their content
    headers: seq[tuple[path: AbsoluteFile, content: string]]

  UniqueId = distinct uint32
    ## 2 bit namespace, 30 bit ID. Combines procedure, global, const, and data
    ## IDs into a single ID type. Falls apart if there are ever more than 2^30
    ## entities per namespace, which seems unlikely.

const NonMagics = {}

template toUnique(x: ProcedureId): UniqueId =
  UniqueId((0 shl 30) or uint32(x))
template toUnique(x: GlobalId): UniqueId =
  UniqueId((1 shl 30) or uint32(x))
template toUnique(x: ConstId): UniqueId =
  UniqueId((2 shl 30) or uint32(x))
template toUnique(x: DataId): UniqueId =
  UniqueId((3 shl 30) or uint32(x))

template module(g: BModuleList, s: PSym): BModule =
  g.modules[s.moduleId.FileIndex]

proc initModuleList*(graph: ModuleGraph, num: Natural): BModuleList =
  ## Sets up a backend module-list with `num` modules.
  result = BModuleList(graph: graph)
  result.modules.newSeq(num)

proc initModule*(idgen: IdGenerator): BModule =
  BModule(idgen: idgen)

proc processEvent(g: var BModuleList, cg: var CodeGenEnv,
                  partial: var PartialTable, evt: sink BackendEvent) =
  measure("processEvent")

  template append(body: CAst, id, list: untyped) =
    let b = body
    let m = cg.env[id].moduleId.FileIndex
    g.modules[m].list.add (id, g.modules[m].all.append(b))

  case evt.kind
  of bekDiscovered:
    if evt.entity.kind == mnkGlobal:
      let id = evt.entity.global
      append genGlobal(cg, id), id, globals

  of bekModule:
    discard "nothing to do"
  of bekConstant:
    let id = evt.cnst
    append genConst(cg, id, cg.env[cg.env.bodies[id]]), id, constants
  of bekPartial:
    # append to the in-progress body -- code generation happens once complete
    discard partial.mgetOrPut(evt.id, MirBody()).append(evt.body)
  of bekProcedure:
    # TODO: integrate MIR output with ``--showir``
    let code = genProc(cg, evt.id, evt.body)
    # TODO: integrate CIR output with ``--showir``
    # TODO: scan the body for referenced types and data; those are generated
    #       on use

    if cg.env[evt.id].typ.callConv == ccInline:
      # add to the global AST
      g.inline[evt.id] = g.all.append(code)
    else:
      append code, evt.id, procs
  of bekImported:
    # TODO: implement me
    discard

proc assemble(g: BModuleList, cg: CodeGenEnv, m: BModule,
              current: ModuleId): string =
  ## Gathers everything that needs to be in the final C translation unit (=TU),
  ## brings these entities into a stable order, and renders the result into
  ## C code. This is the final step for processing module `m`.
  measure("assemble")
  type
    StructEnt = tuple[hash: Hash, node: CNodeIndex]
      ## global entity; order established by structural hash
    GlobalEnt = tuple[item: ItemId, node: CNodeIndex]
      ## global entity; order established by module + item ID
    LocalEnt  = tuple[item: int32, node: CNodeIndex]
      ## module-local entity; order established by item ID

  var
    fwdTypes:    seq[StructEnt]
    types:       seq[StructEnt]
    data:        seq[StructEnt]
    externDecls: seq[GlobalEnt]
    defs:        seq[LocalEnt]
    fwd:         seq[GlobalEnt]
    inline:      seq[GlobalEnt]
    procs:       seq[LocalEnt]

    symMarker: PackedSet[UniqueId]
    typeFwdMarker, typeMarker: PackedSet[TypeId]

  proc scan(g: BModuleList, cg: CodeGenEnv, ast: CombinedCAst,
            n: CNodeIndex) {.closure.} =
    # XXX: meh, a closure
    template guard(id, body: untyped) =
      if not containsOrIncl(symMarker, toUnique id):
        body

    # TODO: imported symbols and types, as well as header dependencies need to
    #       be considered here
    for it in all(ast, n):
      case it.kind
      of cnkWeakType:
        # only a forward declaration is needed
        if not containsOrIncl(typeFwdMarker, it.typ):
          let (hash, _, n) = g.types[it.typ]
          fwdTypes.add (hash, n)
      of cnkType:
        if not containsOrIncl(typeMarker, it.typ):
          let (hash, n, _) = g.types[it.typ]
          types.add (hash, n)
      of cnkProcSym:
        let s = cg.env[it.prc]
        if s.typ.callConv == ccInline:
          guard it.prc:
            inline.add (s.itemId, g.inline[it.prc])
        elif cg.env[it.prc].moduleId.ModuleId != current:
          guard it.prc:
            fwd.add (s.itemId, g.procs[it.prc])
      of cnkGlobalSym:
        if cg.env[it.global].moduleId.ModuleId != current:
          guard it.global:
            externDecls.add (cg.env[it.global].itemId, g.globals[it.global])
      of cnkConstSym:
        if it.cnst.isAnon():
          let id = extract(it.cnst)
          guard id:
            data.add g.data[id]
        elif cg.env[it.cnst].moduleId.ModuleId != current:
          guard it.cnst:
            externDecls.add (cg.env[it.cnst].itemId, g.consts[it.cnst])
      else:
        discard "not relevant"

  # add the local entities to the lists and scan them for their dependencies:
  template addAll(src, dst: untyped) =
    for (id, n) in src.items:
      dst.add (cg.env[id].itemId.item, n)
      scan(g, cg, m.all, n)

  addAll(m.procs, procs)
  addAll(m.globals, defs)
  addAll(m.constants, defs)

  # scan the inline procedures for their dependencies (which might discover
  # new inline procedure dependencies)
  var i = 0
  while i < inline.len:
    scan(g, cg, g.all, inline[i][1])
    inc i

  # scan the types:
  i = 0
  while i < types.len:
    # TODO: use a dedicated scanning procedure; only types can be referenced
    #       from types
    scan(g, cg, g.all, types[i][1])
    inc i

  # TODO: forward declarations for procedures also need to be pulled in here.
  #       The most simple (and efficient) solution would be emitting one for
  #       *every* procedure, though this would result in larger artifacts...

  # ------
  # except for function forward declarations, the content of the TU is known
  # now. Sort everything

  proc cmp(a, b: LocalEnt): int  = a.item - b.item
  proc cmp(a, b: StructEnt): int = a.hash - b.hash
  proc cmp(a, b: GlobalEnt): int =
    if a.item.module == b.item.module:  a.item.item - b.item.item
    else:                               a.item.module - b.item.module

  sort(fwdTypes, cmp)
  sort(types, cmp)
  sort(data, cmp)
  sort(externDecls, cmp)
  sort(fwd, cmp)
  sort(inline, cmp)
  sort(defs, cmp)
  sort(procs, cmp)

  # ------
  # sorting is done, now format everything

  # TODO: data entries are super special: their name is based on the final
  #       position in the module, meaning that we can only now compute it. Do
  #       so

  # TODO: emit the preamble (i.e., "generated by...")
  # TODO: emit the includes

  template format(ast: CombinedCAst, list: untyped) =
    for (_, it) in list.items:
      format(cg, ast, it, result)

  format(g.all, fwdTypes)
  format(g.all, types)
  format(g.all, data)
  format(g.all, externDecls)
  format(m.all, defs)
  format(g.all, fwd)
  format(g.all, inline)
  format(m.all, procs)

proc generateCode*(graph: ModuleGraph, g: sink BModuleList,
                   mlist: sink ModuleList): Output =
  ## Implements the main part of the C code-generation orchestrator. Expects an
  ## already populated ``BModuleList``. Returns the list with all code
  ## generation artifacts.
  measure("backend")

  # pre-process the init procedures:
  for key, m in mlist.modules.pairs:
    # TODO: assign the external names for the init procedures
    discard

  # ----- main event processing -----
  let
    config = BackendConfig(tconfig: TranslationConfig(magicsToKeep: NonMagics))

  var
    cg = CodeGenEnv(env: initMirEnv(graph))
    discovery: DiscoveryData
    partial:   PartialTable

  # discover and generate code for all alive entities:
  for evt in process(graph, mlist, cg.env, discovery, config):
    processEvent(g, cg, partial, evt)

  # finish the partial procedures:
  for id, p in partial.pairs:
    # generate the code and append to the attached-to module:
    let idx = g.module(cg.env[id]).all.append(genProc(cg, id, p))
    g.module(cg.env[id]).procs.add (id, idx)

  # production of the CIR for all alive entities is done

  # TODO: generate the main procedure
  # TODO: report the used dynamic libraries
  # TODO: generate a header, if requested

  result = Output()
  # assemble the final C code for each module:
  for id, m in mlist.modules.pairs:
    let code = assemble(g, cg, g.modules[id], id)
    if code.len > 0:
      result.modules.add (m.sym, code)

proc generateCode*(graph: ModuleGraph, mlist: sink ModuleList) =
  ## Entry point for C code generation. Only the C code is generated -- nothing
  ## is written to disk yet.
  var g = initModuleList(graph, 0)

  # setup the module entries:
  for key, m in mlist.modules.pairs:
    # XXX: meh, not a good solution. The list should be setup up-front
    if m.sym.position >= g.modules.len:
      setLen(g.modules, m.sym.position + 1)
    g.modules[key] = initModule(m.idgen)

  # the output is communicated through the module graph
  graph.backend = generateCode(graph, g, mlist)

# ---------------
# output handling

# XXX: consider moving this to a separate module. It's unrelated to code
#      generation orchestration

proc getCFile(config: ConfigRef, m: PSym): AbsoluteFile =
  let p = AbsoluteFile toFullPath(config, m.position.FileIndex)
  # XXX: toFullPath should return an AbsoluteFile already
  result = changeFileExt(completeCfilePath(config, withPackageName(config, p)),
                         ".nim.c")

proc writeFile(config: ConfigRef, cfile: Cfile, code: string): bool =
  ## Writes `code` to `cfile`, and returns whether the C file needs to be
  ## recompiled.
  if optForceFullMake notin config.globalOptions:
    if not equalsFile(code, cfile.cname):
      if not writeRope(code, cfile.cname):
        localReport(config, reportStr(rsemCannotOpenFile, cfile.cname.string))
      result = true
    elif fileExists(cfile.obj) and
         os.fileNewer(cfile.obj.string, cfile.cname.string):
      result = false
    else:
      result = true
  else:
    if not writeRope(code, cfile.cname):
      localReport(config, reportStr(rsemCannotOpenFile, cfile.cname.string))
    result = true

proc writeModules*(backend: RootRef, config: ConfigRef) =
  ## Writes the files previously collected into `backend` to disk and adds
  ## them to the final build.
  let output = Output backend
  for m, code in output.modules.items:
    measure("write module")
    let cfile = getCFile(config, m)
    var cf = Cfile(nimname: m.name.s, cname: cfile,
                   obj: completeCfilePath(config, toObjFile(config, cfile)),
                   flags: {})

    # write to disk:
    if not writeFile(config, cf, code):
      cf.flags = {CfileFlag.Cached} # already up-to-date

    # add to the build:
    addFileToCompile(config, cf)

  for (path, content) in output.headers:
    # nothing to add to the compilation; just write header to disk
    discard writeRope(content, path)
