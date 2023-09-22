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
    ast_types,
    report_enums
  ],
  compiler/modules/[
    modules,
    modulegraphs
  ],
  compiler/front/[
    options,
    optionsprocessor,
    msgs,
    cmdlinehelper,
    cli_reporter
  ],
  compiler/utils/[
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

from compiler/tools/suggest import findTrackedSym, executeCmd, listUsages, suggestSym, `$`

export Suggest
export IdeCmd
export AbsoluteFile
type
  CachedMsgs = seq[Report]
  NimSuggest* = ref object
    graph: ModuleGraph
    idle: int
    cachedMsgs: CachedMsgs

proc defaultReportHook(conf: ConfigRef, report: Report): TErrorHandling =
  doNothing

proc initNimSuggest*(project: string, nimPath: string = ""): NimSuggest =
  var retval: ModuleGraph
  var cachedMsgs: CachedMsgs = @[]
  proc mockCommand(graph: ModuleGraph) =
    retval = graph
    let conf = graph.config
    clearPasses(graph)
    registerPass graph, verbosePass
    registerPass graph, semPass
    conf.setCmd cmdIdeTools

    add(conf.searchPaths, conf.libpath)

    conf.setErrorMaxHighMaybe

    # compile the project before showing any input so that we already
    # can answer questions right away:
    compileProject(graph)


  proc mockCmdLine(pass: TCmdLinePass, argv: openArray[string];
        conf: ConfigRef) =
    let a = unixToNativePath(project)
    if dirExists(a) and not fileExists(a.addFileExt("nim")):
      conf.projectName = findProjectNimFile(conf, a)
      # don't make it worse, report the error the old way:
      if conf.projectName.len == 0: conf.projectName = a
    else:
      conf.projectName = a
  proc reportHook(conf: ConfigRef, report: Report): TErrorHandling =
    result = doNothing
    if report.kind notin {rsemProcessing, rsemProcessingStmt}:
      # pre-filter to save memory
      cachedMsgs.add(report)
  let
    cache = newIdentCache()
    conf = newConfigRef(reportHook)
    self = NimProg(
      suggestMode: true,
      processCmdLine: mockCmdLine
    )
  conf.writeHook = proc(conf: ConfigRef, s: string, flags: MsgFlags) = discard
  conf.writelnHook = proc(conf: ConfigRef, s: string, flags: MsgFlags) = discard
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
  return NimSuggest(graph: retval, idle: 0, cachedMsgs: cachedMsgs)

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

proc processFlags(sug: Suggest; n: ParsedNode) =
  var
    identDeprecated: bool
    colonDeprecated: bool
  for s in n.sons:
    identDeprecated = s.kind == pnkIdent and getName(s) == "deprecated"
    colonDeprecated = s.kind == pnkExprColonExpr and getName(s[0]) == "deprecated" 
    if identDeprecated or colonDeprecated:
      sug.flags.incl SuggestFlag.deprecated

proc parsedNodeToSugget(n: ParsedNode; originKind: ParsedNodeKind; module: string): Suggest =
  if n.kind in {pnkError, pnkEmpty}: return
  if n.kind notin {pnkProcDef..pnkVarTuple}: return
  new(result)
  var token = getToken(n)
  var name = ""

  if n.kind in pnkRoutineDefs and n[pragmasPos].kind == pnkPragma:
    processFlags(result, n[pragmasPos])
  elif n[0].kind == pnkPragmaExpr and n[0][^1].kind == pnkPragma:
    processFlags(result, n[0][^1])

  if n.kind != pnkVarTuple:
    var node: ParsedNode = getSymNode(n[0])
    token = getToken(node)
    if node.kind != pnkError:
      name = getName(node)
      when false:
        if n.kind in pnkRoutineDefs and node.kind == pnkAccQuoted:
          let identsLen = n[paramsPos].sons.len
          for i in countup(1, identsLen - 1):
            name.add getName(n[paramsPos][i][1])
            if i != identsLen - 1:
              name.add ","
  else:
    name.add "("
    for c in n.sons:
      if c.kind == pnkEmpty: break
      name.add getName(c) & ","
    name.add ")"
  if name != "":
    result.qualifiedPath = @[module, name]
  result.line = token.line.int
  result.column = token.col.int
  result.tokenLen = name.len
  result.symkind = byte pnkToSymKind(originKind)

proc outline(graph: ModuleGraph; fileIdx: FileIndex) =
  let conf = graph.config
  var parser: Parser
  var sug: Suggest
  var parsedNode: ParsedNode
  let name = toFilename(conf, fileIdx)

  const Sections = {pnkTypeSection, pnkConstSection, pnkLetSection}
  template suggestIt(parsedNode: ParsedNode; originKind: ParsedNodeKind) =
    sug = parsedNodeToSugget(parsedNode, originKind, name)
    if sug != nil:
      sug.filepath = toFullPath(conf, fileIdx)
      conf.suggestionResultHook(sug)

  if setupParser(parser, fileIdx, graph.cache, conf):
    while true:
      parsedNode = parser.parseTopLevelStmt()
      if parsedNode.kind == pnkEmpty:
        break

      if parsedNode.kind in Sections:
        for node in parsedNode.sons:
          suggestIt(node, parsedNode.kind)
      else:
        suggestIt(parsedNode, parsedNode.kind)
    closeParser(parser)

proc executeNoHooks(cmd: IdeCmd, file, dirtyfile: AbsoluteFile, line, col: int,
             graph: ModuleGraph) =
  let conf = graph.config
  executeCmd(cmd, file, dirtyfile, line, col, graph)
  if conf.ideCmd in {ideUse, ideDus}:
    let u = graph.findTrackedSym()
    if u != nil:
      listUsages(graph, u)
    else:
      stderr.writeLine "found no symbol at position: " & (conf $ conf.m.trackPos)
  elif conf.ideCmd == ideOutline:
    let dirtyIdx = fileInfoIdx(conf, file)
    outline(graph, dirtyIdx)

proc reportToSuggest(conf: ConfigRef, info: TLineInfo, r: Report): Suggest =
  Suggest(section: ideChk, filePath: toFullPath(conf, info),
    line: toLinenumber(info), column: toColumn(info),
    doc: conf.reportShort(r), forth: $severity(conf, r))

proc fetchCachedReports*(ins: NimSuggest, file: AbsoluteFile): seq[Suggest] =
  let rLen = ins.cachedMsgs.len
  if rLen == 0: return
  let conf = ins.graph.config
  let fileIdx = fileInfoIdx(conf, file)
  var outs: seq[int] = @[]
  for i, report in ins.cachedMsgs:
    let loc = report.location()
    if stdOptions.isSome(loc):
      let info = loc.get()
      if info.fileIndex == fileIdx:
        outs.add i
        result.add(reportToSuggest(conf, info, report))
  for i in countdown(outs.len - 1, 0):
    ins.cachedMsgs.delete(outs[i])

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
    retval.add(Suggest(section: ideProject, filePath: string conf.projectFull))
  else:
    template addReport(report: Report) =
      let loc = report.location()
      if stdOptions.isSome(loc):
        let info = loc.get()
        retval.add(reportToSuggest(conf, info, report))

    if conf.ideCmd == ideChk:
      conf.structuredReportHook = proc (conf: ConfigRef, report: Report): TErrorHandling =
        result = doNothing
        case report.category
        of repParser, repLexer, repSem, repVM:
          if report.category == repSem and
            report.kind in {rsemProcessing, rsemProcessingStmt}:
            # skip processing statements
            return
          addReport(report)
        else: discard

    else:
      conf.structuredReportHook = defaultReportHook
    executeNoHooks(conf.ideCmd, file, dirtyfile, line, col, nimsuggest.graph)
  return retval
