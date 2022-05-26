#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the passes functionality. A pass must implement the
## `TPass` interface.

import
  compiler/front/[
    options,
    msgs,
  ],
  compiler/modules/[
    modulegraphs,
  ],
  compiler/ast/[
    ast,
    llstream,
    syntaxes,
    reports,
    lineinfos,
  ],
  compiler/utils/[
    pathutils,
  ]

type
  TPassData* = tuple[input: PNode, closeOutput: PNode]


proc makePass*(open: TPassOpen = nil,
               process: TPassProcess = nil,
               close: TPassClose = nil,
               isFrontend = false): TPass =

  ## a pass is a tuple of procedure vars ``TPass.close`` may produce additional
  ## nodes. These are passed to the other close procedures.
  ## This mechanism used to be used for the instantiation of generics.

  result.open = open
  result.close = close
  result.process = process
  result.isFrontend = isFrontend

proc skipCodegen*(config: ConfigRef; n: PNode): bool {.inline.} =
  ## can be used by codegen passes to determine whether they should do
  ## something with `n`. Currently, this ignores `n` and uses the global
  ## error count instead.
  result = config.errorCounter > 0

const
  maxPasses = 10

type
  TPassContextArray = array[0..maxPasses - 1, PPassContext]

proc clearPasses*(g: ModuleGraph) =
  g.passes.setLen(0)

proc registerPass*(g: ModuleGraph; p: TPass) =
  internalAssert(
    g.config,
    g.passes.len < maxPasses,
    "Cannot register more than " & $maxPasses & " passes")

  g.passes.add(p)

proc openPasses(g: ModuleGraph; a: var TPassContextArray;
                module: PSym; idgen: IdGenerator) =
  for i in 0..<g.passes.len:
    if not isNil(g.passes[i].open):
      a[i] = g.passes[i].open(g, module, idgen)
    else: a[i] = nil

proc closePasses(graph: ModuleGraph; a: var TPassContextArray) =
  var m: PNode = nilPNode
  for i in 0..<graph.passes.len:
    if not isNil(graph.passes[i].close):
      m = graph.passes[i].close(graph, a[i], m)
    a[i] = nil                # free the memory here

proc processTopLevelStmt(graph: ModuleGraph, n: PNode, a: var TPassContextArray): bool =
  # this implements the code transformation pipeline
  var m = n
  for i in 0..<graph.passes.len:
    if not isNil(graph.passes[i].process):
      m = graph.passes[i].process(a[i], m)
      if isNil(m): return false
  result = true

proc resolveMod(conf: ConfigRef; module, relativeTo: string): FileIndex =
  let fullPath = findModule(conf, module, relativeTo)
  if fullPath.isEmpty:
    result = InvalidFileIdx
  else:
    result = fileInfoIdx(conf, fullPath)

proc processImplicits(graph: ModuleGraph; implicits: seq[string], nodeKind: TNodeKind,
                      a: var TPassContextArray; m: PSym) =
  # XXX fixme this should actually be relative to the config file!
  let relativeTo = toFullPath(graph.config, m.info)
  for module in items(implicits):
    # implicit imports should not lead to a module importing itself
    if m.position != resolveMod(graph.config, module, relativeTo).int32:
      var importStmt = newNodeI(nodeKind, m.info)
      var str = newStrNode(nkStrLit, module)
      str.info = m.info
      importStmt.add str
      if not processTopLevelStmt(graph, importStmt, a): break

const
  imperativeCode = {low(TNodeKind)..high(TNodeKind)} - {nkTemplateDef, nkProcDef, nkMethodDef,
    nkMacroDef, nkConverterDef, nkIteratorDef, nkFuncDef, nkPragma,
    nkExportStmt, nkExportExceptStmt, nkFromStmt, nkImportStmt, nkImportExceptStmt}

proc prepareConfigNotes(graph: ModuleGraph; module: PSym) =
  # don't be verbose unless the module belongs to the main package:
  if module.getnimblePkgId == graph.config.mainPackageId:
    graph.config.asgn(cnCurrent, cnMainPackage)

  else:
    # QUESTION what are the exact conditions that lead to this branch being
    # executed? For example, if I compile `tests/arc/thard_alignment.nim`,
    # this sets configuration entries to the 'foreign' state after
    # compilation has finished. This tests (despite being quite large) does
    # not do any imports.
    if graph.config.mainPackageNotes == {}:
      graph.config.asgn(cnMainPackage, cnCurrent)

    graph.config.asgn(cnCurrent, cnForeign)

proc moduleHasChanged*(graph: ModuleGraph; module: PSym): bool {.inline.} =
  result = true
  #module.id >= 0 or isDefined(graph.config, "nimBackendAssumesChange")

proc partOfStdlib(x: PSym): bool =
  var it = x.owner
  while it != nil and it.kind == skPackage and it.owner != nil:
    it = it.owner
  result = it != nil and it.name.s == "stdlib"

proc processModule*(graph: ModuleGraph; module: PSym; idgen: IdGenerator;
                    stream: PLLStream): bool {.discardable.} =
  if graph.stopCompile(): return true
  var
    p: Parser
    a: TPassContextArray
    s: PLLStream
    fileIdx = module.fileIdx

  prepareConfigNotes(graph, module)
  openPasses(graph, a, module, idgen)
  if stream == nil:
    let filename = toFullPathConsiderDirty(graph.config, fileIdx)
    s = llStreamOpen(filename, fmRead)
    if s == nil:
      localReport(
        graph.config,
        reportStr(rsemCannotOpenFile, filename.string))

      return false
  else:
    s = stream
  while true:
    openParser(p, fileIdx, s, graph.cache, graph.config)

    if not partOfStdlib(module) or module.name.s == "distros":
      # XXX what about caching? no processing then? what if I change the
      # modules to include between compilation runs? we'd need to track that
      # in ROD files. I think we should enable this feature only
      # for the interactive mode.
      if module.name.s != "nimscriptapi":
        processImplicits(
          graph, graph.config.active.implicitImports, nkImportStmt, a, module)
        processImplicits(
          graph, graph.config.active.implicitIncludes, nkIncludeStmt, a, module)

    while true:
      if graph.stopCompile(): break
      var n = parseTopLevelStmt(p)
      if n.kind == nkEmpty: break
      if n.kind in imperativeCode:
        # read everything until the next proc declaration etc.
        var sl = newNodeI(nkStmtList, n.info)
        sl.add n
        var rest: PNode = nilPNode
        while true:
          var n = parseTopLevelStmt(p)
          if n.kind == nkEmpty or n.kind notin imperativeCode:
            rest = n
            break
          sl.add n
        #echo "-----\n", sl
        if not processTopLevelStmt(graph, sl, a): break
        if rest != nil:
          #echo "-----\n", rest
          if not processTopLevelStmt(graph, rest, a): break
      else:
        #echo "----- single\n", n
        if not processTopLevelStmt(graph, n, a): break
    closeParser(p)
    if s.kind != llsStdIn: break
  closePasses(graph, a)
  if graph.config.backend notin {backendC, backendCpp, backendObjc}:
    # We only write rod files here if no C-like backend is active.
    # The C-like backends have been patched to support the IC mechanism.
    # They are responsible for closing the rod files. See `cbackend.nim`.
    closeRodFile(graph, module)
  result = true
