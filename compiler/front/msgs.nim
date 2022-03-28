#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/[strutils, os, tables, macros, times],
  std/private/miscdollars

import
  std/options as std_options

import
  utils/[ropes, pathutils, strutils2],
  ast/[reports, lineinfos],
  front/[options]

from ast/ast_types import PSym

export InstantiationInfo
export TErrorHandling

template instLoc*(depth: int = -2): InstantiationInfo =
  ## grabs where in the compiler an error was instanced to ease debugging.
  ##
  ## whether to use full paths depends on --excessiveStackTrace compiler option.
  instantiationInfo(depth, fullPaths = compileOption"excessiveStackTrace")

template toStdOrrKind(stdOrr): untyped =
  if stdOrr == stdout: stdOrrStdout else: stdOrrStderr

proc flushDot*(conf: ConfigRef) =
  ## safe to call multiple times
  let stdOrr = if optStdout in conf.globalOptions: stdout else: stderr
  let stdOrrKind = toStdOrrKind(stdOrr)
  if stdOrrKind in conf.lastMsgWasDot:
    conf.lastMsgWasDot.excl stdOrrKind
    write(stdOrr, "\n")

proc toCChar*(c: char; result: var string) {.inline.} =
  case c
  of '\0'..'\x1F', '\x7F'..'\xFF':
    result.add '\\'
    result.add toOctal(c)
  of '\'', '\"', '\\', '?':
    result.add '\\'
    result.add c
  else:
    result.add c

proc makeCString*(s: string): Rope =
  result = nil
  var res = newStringOfCap(int(s.len.toFloat * 1.1) + 1)
  res.add("\"")
  for i in 0..<s.len:
    # line wrapping of string litterals in cgen'd code was a bad idea, e.g. causes: bug #16265
    # It also makes reading c sources or grepping harder, for zero benefit.
    # const MaxLineLength = 64
    # if (i + 1) mod MaxLineLength == 0:
    #   res.add("\"\L\"")
    toCChar(s[i], res)
  res.add('\"')
  result.add(rope(res))

proc newFileInfo(fullPath: AbsoluteFile, projPath: RelativeFile): TFileInfo =
  result.fullPath = fullPath
  #shallow(result.fullPath)
  result.projPath = projPath
  #shallow(result.projPath)
  result.shortName = fullPath.extractFilename
  result.quotedName = result.shortName.makeCString
  result.quotedFullName = fullPath.string.makeCString
  result.lines = @[]
  when defined(nimpretty):
    if not result.fullPath.isEmpty:
      try:
        result.fullContent = readFile(result.fullPath.string)
      except IOError:
        #rawMessage(errCannotOpenFile, result.fullPath)
        # XXX fixme
        result.fullContent = ""

when defined(nimpretty):
  # Wrapped in `when defined()` because `.fullContent` is not defined
  # without it.
  proc fileSection*(conf: ConfigRef; fid: FileIndex; a, b: int): string =
    substr(conf[fid].fullContent, a, b)

proc canonicalCase(path: var string) =
  ## the idea is to only use this for checking whether a path is already in
  ## the table but otherwise keep the original case
  when FileSystemCaseSensitive: discard
  else: toLowerAscii(path)

proc fileInfoKnown*(conf: ConfigRef; filename: AbsoluteFile): bool =
  var
    canon: AbsoluteFile
  try:
    canon = canonicalizePath(conf, filename)
  except OSError:
    canon = filename
  canon.string.canonicalCase
  result = conf.m.filenameToIndexTbl.hasKey(canon.string)

proc fileInfoIdx*(conf: ConfigRef; filename: AbsoluteFile; isKnownFile: var bool): FileIndex =
  var
    canon: AbsoluteFile
    pseudoPath = false

  try:
    canon = canonicalizePath(conf, filename)
    shallow(canon.string)
  except OSError:
    canon = filename
    # The compiler uses "filenames" such as `command line` or `stdin`
    # This flag indicates that we are working with such a path here
    pseudoPath = true

  var canon2: string
  forceCopy(canon2, canon.string) # because `canon` may be shallow
  canon2.canonicalCase

  if conf.m.filenameToIndexTbl.hasKey(canon2):
    isKnownFile = true
    result = conf.m.filenameToIndexTbl[canon2]
  else:
    isKnownFile = false
    result = conf.m.fileInfos.len.FileIndex
    #echo "ID ", result.int, " ", canon2
    conf.m.fileInfos.add(newFileInfo(canon, if pseudoPath: RelativeFile filename
                                            else: relativeTo(canon, conf.projectPath)))
    conf.m.filenameToIndexTbl[canon2] = result

proc fileInfoIdx*(conf: ConfigRef; filename: AbsoluteFile): FileIndex =
  var dummy: bool
  result = fileInfoIdx(conf, filename, dummy)

proc newLineInfo*(fileInfoIdx: FileIndex, line, col: int): TLineInfo =
  result.fileIndex = fileInfoIdx
  if line < int high(uint16):
    result.line = uint16(line)
  else:
    result.line = high(uint16)
  if col < int high(int16):
    result.col = int16(col)
  else:
    result.col = -1

proc newLineInfo*(conf: ConfigRef; filename: AbsoluteFile, line, col: int): TLineInfo {.inline.} =
  result = newLineInfo(fileInfoIdx(conf, filename), line, col)

const gCmdLineInfo* = newLineInfo(commandLineIdx, 1, 1)

proc concat(strings: openArray[string]): string =
  var totalLen = 0
  for s in strings: totalLen += s.len
  result = newStringOfCap totalLen
  for s in strings: result.add s

proc suggestWriteln*(conf: ConfigRef; s: string) =
  if eStdOut in conf.m.errorOutputs:
    writelnHook(conf, s)

proc msgQuit*(x: int8) = quit x
proc msgQuit*(x: string) = quit x

proc suggestQuit*() =
  raise newException(ESuggestDone, "suggest done")

const
  # NOTE: currently line info line numbers start with 1,
  # but column numbers start with 0, however most editors expect
  # first column to be 1, so we need to +1 here
  ColOffset*   = 1
  commandLineDesc* = "command line"

proc getInfoContextLen*(conf: ConfigRef): int = return conf.m.msgContext.len
proc setInfoContextLen*(conf: ConfigRef; L: int) = setLen(conf.m.msgContext, L)

proc pushInfoContext*(
    conf: ConfigRef;
    info: TLineInfo,
    detail: PSym = nil
  ) =
  ## Add entry to the message context information stack.
  conf.m.msgContext.add((info, detail))

proc popInfoContext*(conf: ConfigRef) =
  ## Remove one entry from the message context information stack
  setLen(conf.m.msgContext, conf.m.msgContext.len - 1)

proc getInfoContext*(conf: ConfigRef; index: int): TLineInfo =
  let i =
    if index < 0:
      conf.m.msgContext.len + index
    else:
      index

  if i >=% conf.m.msgContext.len:
    result = unknownLineInfo
  else:
    result = conf.m.msgContext[i].info

template toFilename*(conf: ConfigRef; fileIdx: FileIndex): string =
  if fileIdx.int32 < 0 or conf == nil:
    (if fileIdx == commandLineIdx: commandLineDesc else: "???")
  else:
    conf[fileIdx].shortName

proc toProjPath*(conf: ConfigRef; fileIdx: FileIndex): string =
  if fileIdx.int32 < 0 or conf == nil:
    if fileIdx == commandLineIdx:
      commandLineDesc
    else:
      "???"
  else:
    conf[fileIdx].projPath.string

proc toFullPath*(conf: ConfigRef; fileIdx: FileIndex): string =
  if fileIdx.int32 < 0 or conf == nil:
    result = (if fileIdx == commandLineIdx: commandLineDesc else: "???")
  else:
    result = conf[fileIdx].fullPath.string

proc toReportLineInfo*(conf: ConfigRef, info: TLineInfo): ReportLineInfo =
  ReportLineInfo(
    file: conf.toFullPath(info.fileIndex),
     line: info.line, col: info.col)

proc setDirtyFile*(conf: ConfigRef; fileIdx: FileIndex; filename: AbsoluteFile) =
  assert fileIdx.int32 >= 0
  conf[fileIdx].dirtyFile = filename
  setLen conf[fileIdx].lines, 0

proc setHash*(conf: ConfigRef; fileIdx: FileIndex; hash: string) =
  assert fileIdx.int32 >= 0
  shallowCopy(conf[fileIdx].hash, hash)

proc getHash*(conf: ConfigRef; fileIdx: FileIndex): string =
  assert fileIdx.int32 >= 0
  shallowCopy(result, conf[fileIdx].hash)

proc toFullPathConsiderDirty*(conf: ConfigRef; fileIdx: FileIndex): AbsoluteFile =
  if fileIdx.int32 < 0:
    result = AbsoluteFile(if fileIdx == commandLineIdx: commandLineDesc else: "???")
  elif not conf[fileIdx].dirtyFile.isEmpty:
    result = conf[fileIdx].dirtyFile
  else:
    result = conf[fileIdx].fullPath

template toFilename*(conf: ConfigRef; info: TLineInfo): string =
  toFilename(conf, info.fileIndex)

template toProjPath*(conf: ConfigRef; info: TLineInfo): string =
  toProjPath(conf, info.fileIndex)

template toFullPath*(conf: ConfigRef; info: TLineInfo): string =
  toFullPath(conf, info.fileIndex)

template toFullPathConsiderDirty*(conf: ConfigRef; info: TLineInfo): string =
  string toFullPathConsiderDirty(conf, info.fileIndex)

proc toFilenameOption*(conf: ConfigRef, fileIdx: FileIndex, opt: FilenameOption): string =
  case opt
  of foAbs: result = toFullPath(conf, fileIdx)
  of foRelProject: result = toProjPath(conf, fileIdx)
  of foCanonical:
    let absPath = toFullPath(conf, fileIdx)
    result = canonicalImportAux(conf, absPath.AbsoluteFile)
  of foName: result = toProjPath(conf, fileIdx).lastPathPart
  of foLegacyRelProj:
    let
      absPath = toFullPath(conf, fileIdx)
      relPath = toProjPath(conf, fileIdx)
    result = if (relPath.len > absPath.len) or (relPath.count("..") > 2):
               absPath
             else:
               relPath
  of foStacktrace:
    if optExcessiveStackTrace in conf.globalOptions:
      result = toFilenameOption(conf, fileIdx, foAbs)
    else:
      result = toFilenameOption(conf, fileIdx, foName)

proc formatPath*(conf: ConfigRef, path: string): string =
  ## Format absolute file path for error message reporting. If path is not
  ## registered in the `filenameToIndexTbl` and is not a path to the
  ## compiler source, return it unchanged. If configuration is nil also
  ## return path unchanged.
  if isNil(conf):
    return path

  if path in conf.m.filenameToIndexTbl:
    # Check if path is registered in filename table index - in that case
    # formatting is done using `FileInfo` data from the config.
    let id = conf.m.filenameToIndexTbl[path]
    result = toFilenameOption(conf, id, conf.filenameOption)
  else:
    # Path not registered in the filename table - most likely an
    # instantiation info report location
    when compileOption"excessiveStackTrace":
      # instLoc(), when `--excessiveStackTrace` is used, generates full
      # paths that /might/ need to be filtered if `--filenames:canonical`.
      const compilerRoot = currentSourcePath().parentDir().parentDir()
      if conf.filenameOption == foCanonical and
         path.startsWith(compilerRoot):
        result = path[(compilerRoot.len + 1) .. ^1]
      else:
        result = path
    else:
      result = path

proc toMsgFilename*(conf: ConfigRef; fileIdx: FileIndex): string =
  toFilenameOption(conf, fileIdx, conf.filenameOption)

template toMsgFilename*(conf: ConfigRef; info: TLineInfo): string =
  toMsgFilename(conf, info.fileIndex)

proc toLinenumber*(info: TLineInfo): int {.inline.} =
  result = int info.line

proc toColumn*(info: TLineInfo): int {.inline.} =
  result = info.col

proc toFileLineCol(info: InstantiationInfo): string {.inline.} =
  result.toLocation(info.filename, info.line, info.column + ColOffset)

proc toFileLineCol*(conf: ConfigRef; info: TLineInfo): string {.inline.} =
  ## Construct `file(line, col)` string from report location information
  result.toLocation(
    toMsgFilename(conf, info), info.line.int, info.col.int + ColOffset)

proc toReportPoint*(
  conf: ConfigRef; info: TLineInfo): ReportLineInfo {.inline.} =
  ## Construct report location instance based on the information from
  ## `info`

  ReportLineInfo(
    file: toMsgFilename(conf, info),
    line: info.line,
    col: info.col + ColOffset)

proc `$`*(conf: ConfigRef; info: TLineInfo): string = toFileLineCol(conf, info)

proc `$`*(info: TLineInfo): string {.error.} = discard

proc msgWrite*(conf: ConfigRef; s: string, flags: MsgFlags = {}) =
  ## Writes given message string to stderr by default.
  ## If ``--stdout`` option is given, writes to stdout instead. If message hook
  ## is present, then it is used to output message rather than stderr/stdout.
  ## This behavior can be altered by given optional flags.
  ##
  ## This is used for 'nim dump' etc. where we don't have nimsuggest
  ## support.
  ##
  ## This procedure is used as a default implementation of the
  ## `ConfigRef.writeHook`.
  let sep = if msgNoUnitSep notin flags: conf.unitSep else: ""

  if optStdout in conf.globalOptions or msgStdout in flags:
    if eStdOut in conf.m.errorOutputs:
      write(stdout, s)
      write(stdout, sep)
      flushFile(stdout)
  else:
    if eStdErr in conf.m.errorOutputs:
      write(stderr, s)
      write(stderr, sep)

      # On Windows stderr is fully-buffered when piped, regardless of C std.
      when defined(windows):
        flushFile(stderr)

proc log*(s: string) =
  var f: File
  if open(f, getHomeDir() / "nimsuggest.log", fmAppend):
    f.writeLine(s)
    close(f)

proc quit(conf: ConfigRef; withTrace: bool) {.gcsafe.} =
  if conf.isDefined("nimDebug"):
    quitOrRaise(conf)
  elif defined(debug) or withTrace or conf.hasHint(rintStackTrace):
    {.gcsafe.}:
      if stackTraceAvailable():
        discard conf.report(InternalReport(
          kind: rintStackTrace,
          trace: getStackTraceEntries()))
      else:
        discard conf.report(InternalReport(
          kind: rintMissingStackTrace))
  quit 1

proc errorActions(
    conf: ConfigRef,
    report: Report,
    eh: TErrorHandling
  ): tuple[action: TErrorHandling, withTrace: bool] =

  if conf.isCompilerFatal(report):
    # Fatal message such as ICE (internal compiler), errFatal,
    return (doAbort, true)
  elif conf.isCodeError(report):
    # Regular code error
    inc(conf.errorCounter)
    conf.exitcode = 1'i8

    if conf.errorMax <= conf.errorCounter:
      # only really quit when we're not in the new 'nim check --def' mode:
      if conf.ideCmd == ideNone:
        return (doAbort, false)
    elif eh == doAbort and conf.cmd != cmdIdeTools:
      return (doAbort, false)
    elif eh == doRaise:
      {.warning: "[IMPLEMENT] Convert report to string message?".}
      return (doRaise, false)

  return (doNothing, false)

proc `==`*(a, b: TLineInfo): bool =
  result = a.line == b.line and a.fileIndex == b.fileIndex

proc exactEquals*(a, b: TLineInfo): bool =
  result = a.fileIndex == b.fileIndex and a.line == b.line and a.col == b.col

proc getContext*(conf: ConfigRef; lastinfo: TLineInfo): seq[ReportContext] =
  ## Get list of context context entries from the current message context
  ## information. Context messages can later be used in the
  ## `SemReport.context` field
  var info = lastinfo
  for i in 0 ..< conf.m.msgContext.len:
    let context = conf.m.msgContext[i]
    if context.info != lastinfo and context.info != info:
      if context.detail.isNil or context.detail.kind == skUnknown:
        result.add ReportContext(
          kind: sckInstantiationFrom,
          location: context.info)
      else:
        result.add ReportContext(
          kind: sckInstantiationOf,
          location: context.info,
          entry: context.detail)

    info = context.info

proc addSourceLine(conf: ConfigRef; fileIdx: FileIndex, line: string) =
  conf[fileIdx].lines.add line

proc numLines*(conf: ConfigRef, fileIdx: FileIndex): int =
  ## xxx there's an off by 1 error that should be fixed; if a file ends with "foo" or "foo\n"
  ## it will return same number of lines (ie, a trailing empty line is discounted)
  result = conf[fileIdx].lines.len
  if result == 0:
    try:
      for line in lines(toFullPathConsiderDirty(conf, fileIdx).string):
        addSourceLine conf, fileIdx, line
    except IOError:
      discard
    result = conf[fileIdx].lines.len

proc sourceLine*(conf: ConfigRef; i: TLineInfo): string =
  ## 1-based index (matches editor line numbers); 1st line is for i.line = 1
  ## last valid line is `numLines` inclusive
  if i.fileIndex.int32 < 0: return ""
  let num = numLines(conf, i.fileIndex)
  # can happen if the error points to EOF:
  if i.line.int > num: return ""

  result = conf[i.fileIndex].lines[i.line.int - 1]

proc getSurroundingSrc(conf: ConfigRef; info: TLineInfo): string =
  if conf.hasHint(rintSource) and info != unknownLineInfo:
    const indent = "  "
    result = "\n" & indent & $sourceLine(conf, info)
    if info.col >= 0:
      result.add "\n" & indent & spaces(info.col) & '^'

proc handleReport*(
    conf: ConfigRef,
    report: Report,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
  ) {.noinline.} =

  var report = report
  report.reportFrom = toReportLineInfo(reportFrom)
  if report.category == repSem and report.location.isSome():
    report.semReport.context = conf.getContext(report.location.get())

  let
    userAction = conf.report(report)
    (action, trace) =
      case userAction
      of doDefault:
        errorActions(conf, report, eh)
      else:
        (userAction, false)

  case action
  of doAbort:   quit(conf, trace)
  of doRaise:   raiseRecoverableError("report")
  of doNothing: discard
  of doDefault: assert(
    false,
    "Default error handing action must be turned into ignore/raise/abort")

proc handleReport*(
    conf: ConfigRef,
    id: ReportId,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
  ) =
  if true or conf.canReport(id):
    conf.m.writtenSemReports.incl id
    conf.handleReport(conf.m.reports.getReport(id), reportFrom, eh)

template globalAssert*(
    conf: ConfigRef;
    cond: untyped, info: TLineInfo = unknownLineInfo, arg = "") =
  ## avoids boilerplate
  if not cond:
    var arg2 = "'$1' failed" % [astToStr(cond)]
    if arg.len > 0: arg2.add "; " & astToStr(arg) & ": " & arg
    handleReport(conf, info, errGenerated, arg2, doRaise, instLoc())

template globalReport*(
  conf: ConfigRef; info: TLineInfo, report: ReportTypes) =
  ## `local` means compilation keeps going until errorMax is reached (via
  ## `doNothing`), `global` means it stops.
  handleReport(
    conf, wrap(report, instLoc(), info), instLoc(), doRaise)

template globalReport*(conf: ConfigRef, report: ReportTypes) =
  handleReport(
    conf, wrap(report, instLoc()), instLoc(), doRaise)

template localReport*(conf: ConfigRef; info: TLineInfo, report: ReportTypes) =
  {.line.}:
    handleReport(
      conf, wrap(report, instLoc(), info), instLoc(), doNothing)

template localReport*(conf: ConfigRef; info: TLineInfo, report: ReportTypes) =
  handleReport(
    conf, wrap(report, instLoc(), info), instLoc(), doNothing)

template localReport*(conf: ConfigRef; node: PNode, report: SemReport) =
  var tmp = report
  if isNil(tmp.ast):
    tmp.ast = node
  handleReport(
    conf, wrap(tmp, instLoc(), node.info), instLoc(), doNothing)

proc temporaryStringError*(conf: ConfigRef, info: TLineInfo, text: string) =
  assert false

template localReport*(conf: ConfigRef, report: ReportTypes) =
  handleReport(
    conf, wrap(report, instLoc()), instLoc(), doNothing)

template localReport*(conf: ConfigRef, report: Report) =
  handleReport(conf, report, instLoc(), doNothing)

proc semReportCountMismatch*(
    kind: ReportKind,
    expected, got: distinct SomeInteger,
    node: PNode = nil,
  ): SemReport =
  result = SemReport(kind: kind, ast: node)
  result.countMismatch = (toInt128(expected), toInt128(got))

template semReportIllformedAst*(
    conf: ConfigRef, node: PNode, explain: string): untyped =
  handleReport(
    conf,
    wrap(
      SemReport(kind: rsemIllformedAst, ast: node ),
      instLoc(),
      node.info),
    instLoc(),
    doNothing)

proc joinAnyOf*[T](values: seq[T], quote: bool = false): string =
  proc q(s: string): string =
    if quote:
      "'" & s & "'"
    else:
      s

  if len(values) == 1:
    result.add q($values[0])
  elif len(values) == 2:
    result.add q($values[0]) & " or " & q($values[1])
  else:
    for idx in 0 ..< len(values) - 1:
      if idx > 0:
        result.add ", "
      result.add q($values[idx])

    result.add " or "
    result.add q($values[^1])

template semReportIllformedAst*(
  conf: ConfigRef, node: PNode, expected: set[TNodeKind]): untyped =
  var exp: seq[TNodeKind]
  for e in expected:
    exp.add e

  var msg = "Expected "
  msg.add joinAnyOf(exp)
  msg.add ", but found "
  msg.add $node.kind

  semReportIllformedAst(conf, node, msg)

template localReport*(conf: ConfigRef, info: TLineInfo, report: ReportTypes) =
  handleReport(conf, wrap(report, instLoc(), info), instLoc(), doNothing)

template internalError*(
    conf: ConfigRef, repKind: InternalReportKind, fail: string): untyped =
  conf.handleReport(
    wrap(InternalReport(
      kind: repKind,
      msg: fail),
         instLoc()),
    instLoc(),
    doAbort
  )

template internalError*(
    conf: ConfigRef, info: TLineInfo,
    repKind: InternalReportKind, fail: string): untyped =
  ## Causes an internal error
  conf.handleReport(wrap(
    InternalReport(
      kind: repKind, msg: fail), instLoc(), info),
                    instLoc(), doAbort)

template internalError*(
    conf: ConfigRef,
    info: TLineInfo,
    fail: string,
  ): untyped =
  ## Causes an internal error
  conf.handleReport(wrap(
    InternalReport(kind: rintUnreachable, msg: fail),
    instLoc(), info), instLoc(), doAbort)

template internalError*(
    conf: ConfigRef,
    fail: string
  ): untyped =
  ## Causes an internal error
  conf.handleReport(wrap(InternalReport(
    kind: rintUnreachable, msg: fail), instLoc()), instLoc(), doAbort)

template internalAssert*(
    conf: ConfigRef, condition: bool, info: TLineInfo, failMsg: string = "") =
  ## Causes an internal error if the provided condition evaluates to false
  if not condition:
    conf.handleReport(wrap(
      InternalReport(kind: rintAssert, msg: failMsg),
      instLoc(), info), instLoc(), doAbort)

template internalAssert*(
    conf: ConfigRef, condition: bool, failMsg: string = "") =
  ## Causes an internal error if the provided condition evaluates to false
  if not condition:
    conf.handleReport(wrap(InternalReport(
      kind: rintAssert, msg: failMsg), instLoc()), instLoc(), doAbort)

proc quotedFilename*(conf: ConfigRef; i: TLineInfo): Rope =
  if i.fileIndex.int32 < 0:
    result = makeCString "???"
  elif optExcessiveStackTrace in conf.globalOptions:
    result = conf[i.fileIndex].quotedFullName
  else:
    result = conf[i.fileIndex].quotedName

proc listWarnings*(conf: ConfigRef) =
  conf.localReport(InternalReport(
    kind: rintListWarnings,
    enabledOptions: repWarningKinds * conf.notes))

proc listHints*(conf: ConfigRef) =
  conf.localReport(InternalReport(
    kind: rintListHints,
    enabledOptions: repHintKinds * conf.notes))

proc uniqueModuleName*(conf: ConfigRef; fid: FileIndex): string =
  ## The unique module name is guaranteed to only contain {'A'..'Z',
  ## 'a'..'z', '0'..'9', '_'} so that it is useful as a C identifier
  ## snippet.
  let
    path = AbsoluteFile toFullPath(conf, fid)
    rel =
      if path.string.startsWith(conf.libpath.string):
        relativeTo(path, conf.libpath).string
      else:
        relativeTo(path, conf.projectPath).string
    trunc = if rel.endsWith(".nim"): rel.len - len(".nim") else: rel.len
  result = newStringOfCap(trunc)
  for i in 0..<trunc:
    let c = rel[i]
    case c
    of 'a'..'z':
      result.add c
    of {os.DirSep, os.AltSep}:
      result.add 'Z' # because it looks a bit like '/'
    of '.':
      result.add 'O' # a circle
    else:
      # We mangle upper letters and digits too so that there cannot
      # be clashes with our special meanings of 'Z' and 'O'
      result.addInt ord(c)

proc genSuccessX*(conf: ConfigRef) =
  ## Generate and write report for the successful compilation parameters
  var params = UsedBuildParams(linesCompiled: conf.linesCompiled)
  if conf.cmd in cmdBackends:
    params = UsedBuildParams(
      isCompilation: true,
      gc: $conf.selectedGC,
      threads: optThreads in conf.globalOptions,
      optimize:
        if optOptimizeSpeed in conf.options: "speed"
        elif optOptimizeSize in conf.options: "size"
        else: "debug",
      buildMode:
        if isDefined(conf, "danger"): "danger"
        elif isDefined(conf, "release"): "release"
        else: "debug"
    )

  params.sec = epochTime() - conf.lastCmdTime

  params.project =
    case conf.filenameOption
    of foAbs:
      $conf.projectFull
    else:
      $conf.projectName

  params.output =
    if optCompileOnly in conf.globalOptions and conf.cmd != cmdJsonscript:
      $conf.jsonBuildFile
    elif conf.outFile.isEmpty and
         conf.cmd notin {cmdJsonscript} + cmdDocLike + cmdBackends:
      # for some cmd we expect a valid absOutFile
      "unknownOutput"
    else:
      $conf.absOutFile

  when declared(system.getMaxMem):
    params.mem = getMaxMem()
    params.isMaxMem = true
  else:
    params.mem = getTotalMem()

  if conf.filenameOption != foAbs:
    params.output = params.output.AbsoluteFile.extractFilename

  discard conf.report(InternalReport(
    kind: rintSuccessX, buildParams: params))
