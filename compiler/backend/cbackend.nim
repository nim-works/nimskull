## The code-generation orchestrator for the C backend.

import
  std/[
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

  BModuleList* = object
    ## The "top level" type for the orchestrator, owning all state related
    ## to code generation.
    graph: ModuleGraph
    env: MirEnv

    modules*: OrdinalSeq[ModuleId, BModule]

  PartialTable = Table[ProcedureId, MirBody]
    ## Table for holding the incremental procedures

  Output = ref object of RootObj
    ## The interface with the legacy backend management.
    modules: seq[tuple[m: PSym, content: string]]
      ## all modules to add to the build, together with their content
    headers: seq[tuple[path: AbsoluteFile, content: string]]

const NonMagics = {}

proc initModuleList*(graph: ModuleGraph, num: Natural): BModuleList =
  ## Sets up a backend module-list with `num` modules.
  result = BModuleList(graph: graph, env: initMirEnv(graph))
  result.modules.newSeq(num)

proc initModule*(idgen: IdGenerator): BModule =
  BModule(idgen: idgen)

proc processEvent(g: var BModuleList, discovery: DiscoveryData,
                  partial: var PartialTable, evt: sink BackendEvent) =
  discard

proc assemble(m: Module): string =
  ## Combines the various AST fragments of the module and renders them into
  ## C code.

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
    discovery: DiscoveryData
    partial:   PartialTable

  # discover and generate code for all alive entities:
  for evt in process(graph, mlist, g.env, discovery, config):
    processEvent(g, discovery, partial, evt)

  # finish the partial procedures:
  for id, p in partial.pairs:
    # TODO: implement me
    discard

  # production of the CIR for all alive entities is done

  # TODO: generate the main procedure
  # TODO: report the used dynamic libraries
  # TODO: generate a header, if requested

  result = Output()
  # assemble the final C code for each module:
  for id, m in mlist.modules.pairs:
    discard assemble(m)
    # TODO: register in the Output structure

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
