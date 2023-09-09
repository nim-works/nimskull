when not defined(nimcore):
  {.error: "nimcore MUST be defined for Nim's core tooling".}

import std/[os, net]
import std/options as stdOptions
import
  compiler/ast/[
    idents,
    lineinfos,
    ast,
    syntaxes,
    parser,
    ast_parsed_types,
    ast_types
  ],
  compiler/modules/[
    modules,
    modulegraphs
  ],
  compiler/front/[
    options,
    optionsprocessor,
    # commands,
    msgs,
    cmdlinehelper,
    cli_reporter
  ],
  compiler/utils/[
    # prefixmatches,
    pathutils
  ],
  compiler/sem/[
    sem,
    passes,
    passaux,
  ]

from compiler/ast/reports import Report,
  category,
  kind,
  location

from compiler/front/main import customizeForBackend

from compiler/tools/suggest import isTracked, listUsages, suggestSym, `$`

export Suggest
export IdeCmd
export AbsoluteFile
type
  CachedMsgs = seq[Report]
  NimSuggest* = ref object
    graph: ModuleGraph
    idle: int
    cachedMsgs: CachedMsgs

proc defaultStructuredReportHook(conf: ConfigRef, report: Report): TErrorHandling =
  discard

proc initNimSuggest*(project: string, nimPath: string = ""): NimSuggest =
  var retval: ModuleGraph
  proc mockCommand(graph: ModuleGraph) =
    retval = graph
    let conf = graph.config
    clearPasses(graph)
    registerPass graph, verbosePass
    registerPass graph, semPass
    conf.setCmd cmdIdeTools

    add(conf.searchPaths, conf.libpath)

    conf.setErrorMaxHighMaybe
    # do not print errors, but log them
    conf.writelnHook = proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
      discard
    conf.structuredReportHook = defaultStructuredReportHook

    # compile the project before showing any input so that we already
    # can answer questions right away:
    compileProject(graph)


  proc mockCmdLine(pass: TCmdLinePass, argv: openArray[string];
        conf: ConfigRef) =
    conf.writeHook = proc(conf: ConfigRef, s: string, flags: MsgFlags) = discard
    
    let a = unixToNativePath(project)
    if dirExists(a) and not fileExists(a.addFileExt("nim")):
      conf.projectName = findProjectNimFile(conf, a)
      # don't make it worse, report the error the old way:
      if conf.projectName.len == 0: conf.projectName = a
    else:
      conf.projectName = a
  let
    cache = newIdentCache()
    conf = newConfigRef(cli_reporter.reportHook)
    self = NimProg(
      suggestMode: true,
      processCmdLine: mockCmdLine
    )
  conf.astDiagToLegacyReport = cli_reporter.legacyReportBridge
  self.initDefinesProg(conf, "nimsuggest")

  self.processCmdLineAndProjectPath(conf, [])

  # Find Nim's prefix dir.
  if nimPath == "":
    let binaryPath = findExe("nim")
    if binaryPath == "":
      raise newException(IOError,
          "Cannot find Nim standard library: Nim compiler not in PATH")
    conf.prefixDir = AbsoluteDir binaryPath.splitPath().head.parentDir()
    if not dirExists(conf.prefixDir / RelativeDir"lib"):
      conf.prefixDir = AbsoluteDir""
  else:
    conf.prefixDir = AbsoluteDir nimPath

  var graph = newModuleGraph(cache, conf)
  graph.onMarkUsed = proc (g: ModuleGraph; info: TLineInfo; s: PSym; usageSym: var PSym; isDecl: bool) =
    suggestSym(g, info, s, usageSym, isDecl)
  graph.onSymImport = graph.onMarkUsed # same callback
  if self.loadConfigsAndProcessCmdLine(cache, conf, graph, []):
    customizeForBackend(graph, conf, backendC)
    mockCommand(graph)

  retval.doStopCompile = proc (): bool = false
  return NimSuggest(graph: retval, idle: 0, cachedMsgs: @[])

proc findNode(n: PNode; trackPos: TLineInfo): PSym =
  if n.kind == nkSym:
    if isTracked(n.info, trackPos, n.sym.name.s.len): return n.sym
  else:
    for i in 0 ..< safeLen(n):
      let res = findNode(n[i], trackPos)
      if res != nil: return res

proc symFromInfo(graph: ModuleGraph; trackPos: TLineInfo; moduleIdx: FileIndex): PSym =
  let m = graph.getModule(moduleIdx)
  if m != nil and m.ast != nil:
    result = findNode(m.ast, trackPos)

proc getSymNode(node: ParsedNode): ParsedNode =
  result = node
  if result.kind == pnkPostfix:
    result = result[^1]
  elif result.kind == pnkPragmaExpr:
    result = getSymNode(result[0])

proc pnkToSymKind(kind: ParsedNodeKind): TSymKind =
  result = skUnknown
  case kind
  of pnkConstSection, pnkConstDef: result = skConst
  of pnkLetSection: result = skLet
  of pnkVarSection: result = skVar
  of pnkProcDef: result = skProc
  of pnkFuncDef: result = skFunc
  of pnkMethodDef: result = skMethod
  of pnkConverterDef: result = skConverter
  of pnkIteratorDef: result = skIterator
  of pnkMacroDef: result = skMacro
  of pnkTemplateDef: result = skTemplate
  of pnkTypeDef, pnkTypeSection: result = skType
  else: discard

proc getName(node: ParsedNode): string =
  if node.kind == pnkIdent:
    result = node.startToken.ident.s
  elif node.kind == pnkAccQuoted:
    result = "`"
    for t in node.idents:
      result.add t.ident.s
    result.add "`"

proc parsedNodeToSugget(n: ParsedNode; moduleName: string): Suggest =
  if n.kind in {pnkError, pnkEmpty}: return
  if n.kind notin {pnkConstSection..pnkTypeDef, pnkIdentDefs}: return
  new(result)
  let token = getToken(n)
  var name = ""

  if n.kind in {pnkProcDef..pnkTypeDef, pnkIdentDefs}:
    var node: ParsedNode = getSymNode(n[0])
    if node.kind != pnkError:
      name = getName(node)

  if name != "":
    result.qualifiedPath = @[moduleName, name]
  result.line = token.line.int
  result.column = token.col.int
  result.symkind = byte pnkToSymKind(n.kind)

proc outline(graph: ModuleGraph; file: AbsoluteFile; fileIdx: FileIndex) =
  let conf = graph.config
  var parser: Parser
  var sug: Suggest
  var parsedNode: ParsedNode
  var s: ParsedNode
  let m = splitFile(file.string)
  const Sections = {pnkTypeSection, pnkConstSection, pnkLetSection, pnkVarSection}
  template suggestIt(parsedNode: ParsedNode) =
    sug = parsedNodeToSugget(parsedNode, m.name)
    if sug != nil:
      sug.filepath = file.string
      conf.suggestionResultHook(sug)
  if setupParser(parser, fileIdx, graph.cache, conf):
    while true:
      parsedNode = parser.parseTopLevelStmt()
      if parsedNode.kind == pnkEmpty:
        break

      if parsedNode.kind in Sections:
        for node in parsedNode.sons:
          suggestIt(node)
      else:
        suggestIt(parsedNode)
    closeParser(parser)

proc executeNoHooks(cmd: IdeCmd, file, dirtyfile: AbsoluteFile, line, col: int,
             graph: ModuleGraph) =
  let conf = graph.config
  conf.ideCmd = cmd

  var isKnownFile = true
  let dirtyIdx = fileInfoIdx(conf, file, isKnownFile)

  if not dirtyfile.isEmpty: msgs.setDirtyFile(conf, dirtyIdx, dirtyfile)
  else: msgs.setDirtyFile(conf, dirtyIdx, AbsoluteFile"")

  conf.m.trackPos = newLineInfo(dirtyIdx, line, col)
  conf.m.trackPosAttached = false
  conf.errorCounter = 0
  var moduleIdx: FileIndex
  var needCompile = true
  if conf.ideCmd in {ideUse, ideDus} and
      dirtyfile.isEmpty:
    needCompile = false
  if conf.ideCmd == ideOutline:
    needCompile = false
    outline(graph, file, dirtyIdx)

  if needCompile:
    if not isKnownFile:
      moduleIdx = dirtyIdx
      # stderr.writeLine "Compile unknown module: " & toFullPath(conf, moduleIdx)
      discard graph.compileModule(moduleIdx, {})
    else:
      moduleIdx = graph.parentModule(dirtyIdx)
      # stderr.writeLine "Compile known module: " & toFullPath(conf, moduleIdx)
      graph.markDirty dirtyIdx
      graph.markClientsDirty dirtyIdx
      # partially recompiling the project means that that VM and JIT state
      # would become stale, which we prevent by discarding all of it:
      graph.vm = nil
      if conf.ideCmd != ideMod:
        discard graph.compileModule(moduleIdx, {})
  if conf.ideCmd in {ideUse, ideDus}:
    let u = graph.symFromInfo(conf.m.trackPos, moduleIdx)
    if u != nil:
      listUsages(graph, u)
    else:
      stderr.writeLine "found no symbol at position: " & (conf $ conf.m.trackPos)

proc runCmd*(nimsuggest: NimSuggest, cmd: IdeCmd, file,
      dirtyfile: AbsoluteFile, line, col: int): seq[Suggest] =
  var retval: seq[Suggest] = @[]
  let conf = nimsuggest.graph.config
  conf.ideCmd = cmd
  conf.suggestionResultHook = proc (s: Suggest) =
    retval.add(s)
  
  if conf.ideCmd == ideKnown:
    retval.add(Suggest(section: ideKnown, quality: ord(fileInfoKnown(conf, file))))
  elif conf.ideCmd == ideProject:
    retval.add(Suggest(section: ideProject,
        filePath: string conf.projectFull))
  else:
    # if conf.ideCmd == ideChk:
    #   for cm in nimsuggest.cachedMsgs: errorHook(conf, cm.info, cm.msg, cm.sev)
    if conf.ideCmd == ideChk:
      conf.structuredReportHook = proc (conf: ConfigRef, report: Report): TErrorHandling =
        let loc = report.location()
        if stdOptions.isSome(loc):
          let info = loc.get()
          retval.add(Suggest(section: ideChk, filePath: toFullPath(conf,
                  info),
            line: toLinenumber(info), column: toColumn(info), 
            forth: $severity(conf, report)))
        return doNothing
    else:
      conf.structuredReportHook = defaultStructuredReportHook
    executeNoHooks(conf.ideCmd, file, dirtyfile, line, col,
            nimsuggest.graph)
  return retval
