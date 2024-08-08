#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Implements the module handling, including the caching of modules.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast,
    astalgo,
    idents,
    lexer,
    llstream,
    lineinfos,
    syntaxes,
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    passes,
  ],
  compiler/modules/[
    modulegraphs,
    magicsys,
  ],
  compiler/utils/[
    pathutils
  ],
  compiler/ic/[
    replayer
  ]

# TODO: `modules` shouldn't report "semantic analysis" errors
from compiler/ast/reports_sem import reportSym
from compiler/ast/reports_external import ExternalReport
from compiler/ast/report_enums import ReportKind

proc resetSystemArtifacts*(g: ModuleGraph) =
  magicsys.resetSysTypes(g)

template getModuleIdent(graph: ModuleGraph, filename: AbsoluteFile): PIdent =
  getIdent(graph.cache, splitFile(filename).name)

template packageId(): untyped {.dirty.} = ItemId(module: PackageModuleId, item: int32(fileIdx))

proc getPackage(graph: ModuleGraph, fileIdx: FileIndex): PSym =
  ## returns the package symbol (skPackage) for yet to be defined module for
  ## `fileIdx`
  let
    filename = AbsoluteFile toFullPath(graph.config, fileIdx)
    info = newLineInfo(fileIdx, 1, 1)
    desc = getPkgDesc(graph.config, filename.string)
    rootPkg = getIdent(graph.cache, desc.pkgRootName)
    existingRootPkgSym = graph.packageSyms.strTableGet(rootPkg)
    rootPkgSym =
      if existingRootPkgSym.isNil:
        let temp = newSym(skPackage, rootPkg, packageId(), nil, info)
        graph.packageSyms.strTableAdd(temp)
        temp
      else:
        existingRootPkgSym

  if desc.pkgSubpath == "":
    result = rootPkgSym
  else:
    let
      subPkg = getIdent(graph.cache, desc.pkgName)
      existingSubPkgSym = graph.packageSyms.strTableGet(subPkg)
    result =
      if existingSubPkgSym.isNil:
        let subPkgSym = newSym(skPackage, subPkg, packageId(), rootPkgSym, info)
        graph.packageSyms.strTableAdd(subPkgSym)
        subPkgSym
      else:
        existingSubPkgSym

proc partialInitModule(result: PSym; graph: ModuleGraph; fileIdx: FileIndex; filename: AbsoluteFile) =
  let packSym = getPackage(graph, fileIdx)
  result.owner = packSym
  result.position = int fileIdx

  #initStrTable(result.tab(graph))
  when false:
    strTableAdd(result.tab, result) # a module knows itself
    # This is now implemented via
    #   c.moduleScope.addSym(module) # a module knows itself
    # in sem.nim, around line 527

proc newModule(graph: ModuleGraph; fileIdx: FileIndex): PSym =
  let filename = AbsoluteFile toFullPath(graph.config, fileIdx)
  # We cannot call ``newSym`` here, because we have to circumvent the ID
  # mechanism, which we do in order to assign each module a persistent ID.
  result = PSym(kind: skModule, itemId: ItemId(module: int32(fileIdx), item: 0'i32),
                name: getModuleIdent(graph, filename),
                info: newLineInfo(fileIdx, 1, 1))
  if not isNimIdentifier(result.name.s):
    localReport(
      graph.config,
      newLineInfo(fileIdx, 0, -1),
      reportSym(rsemInvalidModuleName, result)
    )

  partialInitModule(result, graph, fileIdx, filename)
  graph.registerModule(result)

proc compileModule*(graph: ModuleGraph; fileIdx: FileIndex; flags: TSymFlags, fromModule: PSym = nil): PSym =
  var flags = flags
  if fileIdx == graph.config.projectMainIdx: flags.incl sfMainModule
  result = graph.getModule(fileIdx)

  template processModuleAux(moduleStatus) =
    ## do the actual processing of a module, outputing whether it's started and
    ## then doing the actual processing.
    onProcessing(graph, fileIdx, moduleStatus, fromModule = fromModule)
    var s: PLLStream
    if sfMainModule in flags:
      s =
        case graph.config.inputMode
        of pimStdin: llStreamOpen(stdin)
        of pimCmd:   llStreamOpen(graph.config.commandArgs[0])
        of pimFile:  nil # handled by ``processModule``
    discard processModule(graph, result, idGeneratorFromModule(result), s)

  if result == nil:
    var cachedModules: seq[FileIndex]
    result = moduleFromRodFile(graph, fileIdx, cachedModules)
    let filename = AbsoluteFile toFullPath(graph.config, fileIdx)
    if result == nil:
      result = newModule(graph, fileIdx)
      result.flags.incl flags
      registerModule(graph, result)
      processModuleAux("import")
    else:
      if sfSystemModule in flags:
        graph.systemModule = result
      partialInitModule(result, graph, fileIdx, filename)
    for m in cachedModules:
      registerModuleById(graph, m)
      replayStateChanges(graph.packed[m.int].module, graph)
      replayGenericCacheInformation(graph, m.int)
      replayLibs(graph, m.int)
  elif graph.isDirty(result):
    result.flags.excl sfDirty
    # reset module fields:
    initStrTables(graph, result)
    result.ast = nil
    processModuleAux("import(dirty)")
    graph.markClientsDirty(fileIdx)


proc importModule*(graph: ModuleGraph; s: PSym, fileIdx: FileIndex): PSym =
  ## this is called by the semantic checking phase
  assert graph.config != nil

  result = compileModule(graph, fileIdx, {}, s)
  graph.addDep(s, fileIdx)

  # restore the notes for outer module:
  if s.getnimblePkgId == graph.config.mainPackageId or
     isDefined(graph.config, "booting"):
    graph.config.asgn(cnCurrent, cnMainPackage)
  else:
    graph.config.asgn(cnCurrent, cnForeign)


proc includeModule*(graph: ModuleGraph; s: PSym, fileIdx: FileIndex): PNode =
  result = syntaxes.parseFile(fileIdx, graph.cache, graph.config).toPNode()
  graph.addDep(s, fileIdx)
  graph.addIncludeDep(s.position.FileIndex, fileIdx)

proc connectCallbacks*(graph: ModuleGraph) =
  graph.includeFileCallback = includeModule
  graph.importModuleCallback = importModule

proc compileSystemModule*(graph: ModuleGraph) =
  if graph.systemModule == nil:
    connectCallbacks(graph)
    graph.config.m.systemFileIdx = fileInfoIdx(graph.config,
        graph.config.libpath / RelativeFile"system.nim")
    discard graph.compileModule(graph.config.m.systemFileIdx, {sfSystemModule})

proc wantMainModule*(conf: ConfigRef) =
  if conf.projectFull.isEmpty:
    # user didn't specify a project file, time to pack it in
    conf.fatalReport(gCmdLineInfo, ExternalReport(kind: rextCmdRequiresFile))

  conf.projectMainIdx = fileInfoIdx(conf, addFileExt(conf.projectFull, NimExt))

proc compileProject*(graph: ModuleGraph; projectFileIdx = InvalidFileIdx) =
  connectCallbacks(graph)
  let conf = graph.config
  wantMainModule(conf)
  configComplete(graph)

  let systemFileIdx = fileInfoIdx(conf, conf.libpath / RelativeFile"system.nim")
  let projectFile = if projectFileIdx == InvalidFileIdx: conf.projectMainIdx else: projectFileIdx

  let packSym = getPackage(graph, projectFile)
  graph.config.mainPackageId = packSym.getnimblePkgId
  graph.importStack.add projectFile

  if projectFile == systemFileIdx:
    discard graph.compileModule(projectFile, {sfMainModule, sfSystemModule})
  else:
    graph.compileSystemModule()
    discard graph.compileModule(projectFile, {sfMainModule})

proc makeModule*(graph: ModuleGraph; filename: AbsoluteFile): PSym =
  result = graph.newModule(fileInfoIdx(graph.config, filename))
  registerModule(graph, result)

proc makeModule*(graph: ModuleGraph; filename: string): PSym =
  result = makeModule(graph, AbsoluteFile filename)

proc makeStdinModule*(graph: ModuleGraph): PSym = graph.makeModule(AbsoluteFile"stdin")
