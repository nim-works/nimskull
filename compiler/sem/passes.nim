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
    pathutils
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
  var m: PNode = nil
  for i in 0..<graph.passes.len:
    if not isNil(graph.passes[i].close):
      m = graph.passes[i].close(graph, a[i], m)
    a[i] = nil                # free the memory here

proc processTopLevelStmt(
    graph: ModuleGraph,
    toProcess: PNode,
    a: var TPassContextArray): bool =
  ## Main processing pipeline entry point - accepts a toplevel node,
  ## collection of pass contexts and a main module graph which defines passes
  ## to use.
  # First step works with the base tree as entry point
  var processingResult = toProcess
  for i in 0 ..< graph.passes.len:
    if not isNil(graph.passes[i].process):
      # Iterate over all processing passes, re-assigning the evaluation
      # results each time.
      processingResult = graph.passes[i].process(a[i], processingResult)
      if isNil(processingResult):
        return false

  result = true

proc resolveMod(conf: ConfigRef; module, relativeTo: string): FileIndex =
  let fullPath = findModule(conf, module, relativeTo)
  if fullPath.isEmpty:
    result = InvalidFileIdx
  else:
    result = fileInfoIdx(conf, fullPath)

proc processImplicits(
    graph: ModuleGraph,
    implicits: seq[string],
    nodeKind: TNodeKind,
    a: var TPassContextArray,
    m: PSym
  ) =

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

proc processModule*(
    graph: ModuleGraph,
    module: PSym,
    idgen: IdGenerator,
    defaultStream: PLLStream
  ): bool {.discardable.} =

  if graph.stopCompile():
    return true

  var
    parser: Parser
    passesArray: TPassContextArray
    stream: PLLStream
    fileIdx = module.fileIdx

  prepareConfigNotes(graph, module)
  openPasses(graph, passesArray, module, idgen)
  if defaultStream.isNil():
    let filename = toFullPathConsiderDirty(graph.config, fileIdx)
    stream = llStreamOpen(filename, fmRead)
    if stream.isNil():
      localReport(
        graph.config,
        reportStr(rsemCannotOpenFile, filename.string))

      return false

  else:
    stream = defaultStream

  # Loop over all top-level statements in the file
  var hasContent = true
  while hasContent:
    # Start file parsing
    openParser(parser, fileIdx, stream, graph.cache, graph.config)

    if not partOfStdlib(module) or module.name.s == "distros":
      # XXX what about caching? no processing then? what if I change the
      # modules to include between compilation runs? we'd need to track that
      # in ROD files. I think we should enable this feature only
      # for the interactive mode.
      if module.name.s != "nimscriptapi":
        processImplicits(
          graph, graph.config.active.implicitImports,
          nkImportStmt, passesArray, module)

        processImplicits(
          graph, graph.config.active.implicitIncludes,
          nkIncludeStmt, passesArray, module)

    # Until toplevel compilation fails (returns `false` from processing),
    # execute the compilation
    var processingOk = true
    while processingOk:
      # Processing was ok but compilation was halted via something else.
      if graph.stopCompile():
        break

      # Get next 'first' statement in the file
      var firstStatement = parser.parseTopLevelStmt().toPNode()
      if firstStatement.kind == nkEmpty:
        break

      # if this is some kind of imperative code - collect subsequent
      # statements as well
      #
      # TODO:BUG despite looking similar feature-wise these two branches
      # actually process the code in different manner - there is a BUG in
      # phase ordering which makes the following scenario possible:
      # sequence of imperative code blocks `I2 I1 I3` is concatenated in
      # one group and processed by the semantic pass in one go. `when` is
      # also considered an imperative node kind, which means conditional
      # declarations are concatenated together. In most use cases it is not
      # particularly problemantic, but when declaration of the symbols such
      # as `nimGCvisit` is concerned this means if `I1` defines symbol and
      # `I2` uses it *in the backend* grouping enables this code to be
      # processed:
      #
      # - `I2` is compiled, does not need  in sem
      # - `I1` defines symbol
      # - `I2` in `[I2, I1, I3]` needs  in backend -  is present
      #
      # without ordering
      #
      # - `I2` is compiled, does not need symbol in sem
      # - `I2` needs symbol in backend - it is not present yet => fail
      #
      # This can be reproduced by disabling first branch and compiling any
      # file - should fail in `system.nim` processing with `system needs
      # 'nimGCvisit'` error.
      if firstStatement.kind in imperativeCode:
        # read everything until the next proc declaration etc.
        var toplevelStatements = newNodeI(nkStmtList, firstStatement.info)
        toplevelStatements.add firstStatement

        # Tail data that is would have already been parsed by the time we
        # collect all the necessary statements - it should be processed as
        # a separate trailing action.
        var rest: PNode = nil

        # Iterate until non-imperative statement is found (or end is
        # reached, in which case parser retuns an empty node)
        while rest.isNil():
          let top = parser.parseTopLevelStmt()
          let nextStatement = top.toPNode()
          if nextStatement.kind == nkEmpty or
             nextStatement.kind notin imperativeCode:
            rest = nextStatement

          else:
            toplevelStatements.add nextStatement

        processingOk = processTopLevelStmt(
          graph, toplevelStatements, passesArray)

        if not rest.isNil() and processingOk:
          processingOk = processTopLevelStmt(
            graph, rest, passesArray)

      else:
        # Otherwise evaluate the code
        processingOk = processTopLevelStmt(
          graph, firstStatement, passesArray)

    parser.closeParser()

    # Ended parsing for the current file - more text can be expected only
    # on the stdin-driven input stream.
    hasContent = stream.kind == llsStdIn

  closePasses(graph, passesArray)

  if graph.config.backend != backendC:
    # We only write rod files here if no C-like backend is active.
    # The C-like backends have been patched to support the IC mechanism.
    # They are responsible for closing the rod files. See `cbackend.nim`.
    closeRodFile(graph, module)

  result = true
