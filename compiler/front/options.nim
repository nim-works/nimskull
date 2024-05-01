#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/[os, strutils, strtabs, sets, tables, packedsets],
  compiler/utils/[prefixmatches, pathutils, platform],
  compiler/ast/[lineinfos],
  compiler/modules/nimpaths

import compiler/front/in_options
export in_options

when not FileSystemCaseSensitive:
  from compiler/utils/strutils2 import toLowerAscii
from terminal import isatty
from times import utc, fromUnix, local, getTime, format, DateTime
from std/private/globs import nativeToUnixPath

from compiler/ast/ast_types import
  TNodeKind,  # used in conversion from `ParsedNode` to `PNode`
  TNodeFlag,  # used in conversion from `ParsedNode` to `PNode`
  `comment=`, # used in conversion from `ParsedNode` to `PNode`
  NodeId,     # used as a reportId/diagId proxy
  PNode,      # because of reports, more leakage
  PSym,       # Contextual details of the instantiation stack optionally refers
              # to the used symbol
  PAstDiag    # used to bridge ast diags to legacy reports

# xxx: legacy Reports to be removed
import compiler/ast/report_enums
from compiler/ast/reports import
  Report,
  wrap,
  kind, severity,
  ReportTypes

const
  hasTinyCBackend* = defined(tinyc)
  useEffectSystem* = true
  useWriteTracking* = false
  copyrightYear* = "2022"
  nimEnableCovariance* = defined(nimEnableCovariance)

const
  harmlessOptions* = {optForceFullMake, optNoLinking, optRun, optUseColors, optStdout}
  genSubDir* = RelativeDir"nimskullcache"
  # XXX: |Nimskull| extensions and config files
  NimExt* = "nim"
  RodExt* = "rod"
  HtmlExt* = "html"
  JsonExt* = "json"
  TagsExt* = "tags"
  TexExt* = "tex"
  IniExt* = "ini"
  DefaultConfig* = RelativeFile"nim.cfg"
  DefaultConfigNims* = RelativeFile"config.nims"
  DocConfig* = RelativeFile"nimdoc.cfg"
  DocTexConfig* = RelativeFile"nimdoc.tex.cfg"
  htmldocsDir* = htmldocsDirname.RelativeDir
  docRootDefault* = "@default" # using `@` instead of `$` to avoid shell quoting complications
  spellSuggestSecretSauce* = -1

const
  cmdBackends* = {cmdCompileToC, cmdCompileToJS, cmdCompileToVM, cmdCrun}
  cmdDocLike* = {cmdDoc, cmdDoc2tex, cmdJsondoc, cmdCtags, cmdBuildindex}

type
  ReportSet* = object
    ids: PackedSet[uint32]

  MsgConfig* = object ## does not need to be stored in the incremental cache
    trackPos*: TLineInfo
    trackPosAttached*: bool ## whether the tracking position was attached to
                            ## some close token.

    errorOutputs*: TErrorOutputs ## Allowed output streams for messages.
    # REFACTOR this field is mostly touched in sem for 'performance'
    # reasons - don't write out error messages when compilation failed,
    # don't generate list of call candidates when `compiles()` fails and so
    # on. This should be replaced with `.inTryExpr` or something similar,
    # and let the reporting hook deal with all the associated heuristics.

    msgContext*: seq[tuple[info: TLineInfo, detail: PSym]] ## \ Contextual
    ## information about instantiation stack - "template/generic
    ## instantiation of" message is constructed from this field. Right now
    ## `.detail` field is only used in the `sem.semMacroExpr()`,
    ## `seminst.generateInstance()` and `semexprs.semTemplateExpr()`. In
    ## all other cases this field is left empty (SemReport is `skUnknown`)
    writtenSemReports*: ReportSet
    lastError*: TLineInfo
    filenameToIndexTbl*: Table[string, FileIndex]
    rawPathToIndexTbl*: Table[string, FileIndex] ## maps non-canonicalized
    ## paths of known-files to the corresponding file index
    fileInfos*: seq[TFileInfo] ## Information about all known source files
    ## is stored in this field - full/relative paths, list of line etc.
    ## (For full list see `TFileInfo`)
    systemFileIdx*: FileIndex

  TCmdLinePass* = enum
    passCmd1,                 # first pass over the command line
    passCmd2,                 # second pass over the command line
    passPP                    # preprocessor called processCommand()

proc initMsgConfig*(): MsgConfig =
  result.msgContext = @[]
  result.lastError = unknownLineInfo
  result.filenameToIndexTbl = initTable[string, FileIndex]()
  result.fileInfos = @[]
  result.errorOutputs = {eStdOut, eStdErr}
  result.filenameToIndexTbl["???"] = FileIndex(-1)
  result.rawPathToIndexTbl = initTable[string, FileIndex]()

func incl*(s: var ReportSet, id: NodeId) = s.ids.incl uint32(id)
func contains*(s: var ReportSet, id: NodeId): bool = s.ids.contains uint32(id)

type
  NimVer* = tuple[major: int, minor: int, patch: int]
  TStringSeq* = seq[string]

  IdeCmd* = enum
    ideNone, ideSug, ideCon, ideDef, ideUse, ideDus, ideChk, ideMod,
    ideHighlight, ideOutline, ideKnown, ideMsg, ideProject

  CfileFlag* {.pure.} = enum
    Cached,    ## no need to recompile this time
    External   ## file was introduced via .compile pragma

  Cfile* = object
    nimname*: string           ## Original name of the nim file, constructed
                               ## from the module name.
    cname*, obj*: AbsoluteFile
    flags*: set[CfileFlag]
    customArgs*: string

  CfileList* = seq[Cfile]

  SuggestFlag* {.pure.} = enum
    deprecated = 1
    isGlobal = 2

  Suggest* = ref object
    section*: IdeCmd
    qualifiedPath*: seq[string]
    name*: string               ## display name
    filePath*: string
    line*: int                  ## Starts at 1
    column*: int                ## Starts at 0
    doc*: string                ## Unescaped documentation string
    forth*: string              ## type
    quality*: range[0..100]     ## matching quality
    contextFits*: bool          ## type/non-type context matches
    prefix*: PrefixMatch
    symkind*: byte
    scope*:int
    localUsages*, globalUsages*: int # usage counters
    tokenLen*: int
    flags*: set[SuggestFlag]

  Suggestions* = seq[Suggest]

  StdOrrKind* = enum
    stdOrrStdout
    stdOrrStderr

  MsgFlag* = enum  ## flags altering msgWriteln behavior
    msgStdout,     ## force writing to stdout, even stderr is default
    msgNoUnitSep   ## the message is a complete "paragraph".
  MsgFlags* = set[MsgFlag]

  TErrorHandling* = enum
    doDefault ## Default action, custom report hook can return this in
              ## order for automatic handing to decide appropriate
              ## reaction.
    doNothing ## Don't do anything
    doAbort   ## Immediately abort compilation
    doRaise   ## Raise recoverable error

  ProjectInputMode* = enum
    pimStdin ## the contents of the main module are provided by stdin
    pimCmd   ## the contents of the main module are provided by a command-line
             ## argument
    pimFile  ## the main module is a file

  ReportHook* = proc(conf: ConfigRef, report: Report): TErrorHandling {.closure.}

  HackController* = object
    ## additional configuration switches to control the behavior of the
    ## debug printer. Most of them are for compiler debugging, and for now
    ## they can't be set up from the cli/defines - in the future this will
    ## be changed. For now you can just edit `defaultHackController` value
    ## in this module as you see fit.
    semStack*: bool       ## Show `| context` entries in the call tracer
    reportInTrace*: bool  ## Error messages are shown with matching indentation
                          ## if report was triggered during execution of the
                          ## sem trace
    semTraceData*: bool   ## For each sem step show processed data, or only
                          ## procedure calls.
    bypassWriteHookForTrace*: bool ## Output trace reports directly into
    ## the `echo` instead of going through the `ConfigRef.writeln` hook.
    ## This is useful for environments such as nimsuggest, which discard
    ## the output.

  ConfigRef* = ref object
    ## every global configuration fields marked with '*' are subject to the
    ## incremental compilation mechanisms (+) means "part of the dependency"

    # active configuration handling
    active*: CurrentConf

    spellSuggestMax*: int ## max number of spelling suggestions for typos

    # 'active' configuration end

    # Set and only read for `testCompileOptionArg`, so not sure if this is
    # 'active' configuration
    verbosity*: CompilerVerbosity     ## how verbose the compiler is

    # 'arguments' aka a single string aka 'joining strings for external
    # program is bad'
    arguments*: string ## the arguments to be passed to the program that
                       ## should be run

    linesCompiled*: int   # all lines that have been compiled
    m*: MsgConfig
    unitSep*: string ## Unit separator between compiler messages
    evalTemplateCounter*: int ## Template instantiation depth used to guard
    ## against infinite expansion recursion
    exitcode*: int8

    hintProcessingDots*: bool ## true for dots, false for filenames

    lastCmdTime*: float       ## Start of the last compiler commmand - set
    ## in the `main.mainCommand` and then read to generate 'successX'
    ## message

    headerFile*: string
    ideCmd*: IdeCmd
    oldNewlines*: bool

    mainPackageId*: int
    errorCounter*: int
    hintCounter*: int
    warnCounter*: int
    errorMax*: int ## Maximum number of errors before compilation will be terminated
    maxLoopIterationsVM*: int ## VM: max iterations of all loops

    packageCache*: StringTableRef      ## absolute path -> absolute path

    jsonBuildFile*: AbsoluteFile
    nimStdlibVersion*: NimVer
    cfileSpecificOptions*: StringTableRef ## File specific compilation options for C backend.
    ## Modified by `{.localPassc.}`
    inputMode*: ProjectInputMode    ## how the main module is sourced
    lastMsgWasDot*: set[StdOrrKind] ## the last compiler message was a single '.'
    projectMainIdx*: FileIndex      ## the canonical path id of the main module
    commandLineSrcIdx*: FileIndex   ## used by `commands` to base paths off for
                                    ## path, lib, and other additions; default
                                    ## to `lineinfos.commandLineIdx` and
                                    ## altered by `nimconf` as needed
    command*: string                ## the main command (e.g. cc, check, scan, etc)
    commandArgs*: seq[string]       ## any arguments after the main command
    commandLine*: string
    extraCmds*: seq[string]        ## for writeJsonBuildInstructions
    keepComments*: bool            ## whether the parser needs to keep comments
    docSeeSrcUrl*: string          ## if empty, no seeSrc will be
    ## generated. The string uses the formatting variables `path` and
    ## `line`.
    docRoot*: string ## see nim --fullhelp for --docRoot
    docCmd*: string ## see nim --fullhelp for --docCmd

    configFiles*: seq[AbsoluteFile] ## List of config files that have been
    ## processed during compilation.

    externalToLink*: seq[string]  ## files to link in addition to the file
    ## we compiled. Modified by the `{.link.}` pragma and `--link`
    ## command-line flag.
    linkOptions*: string ## Additional linking options, modified by the
    ## `{.passl.}` pragma
    compileOptions*: string ## Additional compilation options, modified by
    ## the `{.passc.}` pragma
    cCompilerPath*: string
    toCompile*: CfileList         # (*)
    suggestionResultHook*: proc (result: Suggest) {.closure.}
    suggestMaxResults*: int
    lastLineInfo*: TLineInfo
    writelnHook*: proc(
      conf: ConfigRef,
      output: string,
      flags: MsgFlags
    ) {.closure.} ## All
    ## textual output from the compiler goes through this callback.
    writeHook*: proc(conf: ConfigRef, output: string, flags: MsgFlags) {.closure.}
    structuredReportHook*: ReportHook
      ## callback that is invoked when an enabled report is passed to report
      ## handling. The callback is meant to handle rendering/displaying of
      ## the report
    astDiagToLegacyReport*: proc(conf: ConfigRef, d: PAstDiag): Report
    setMsgFormat*: proc(config: ConfigRef, fmt: MsgFormatKind) {.closure.}
      ## callback that sets the message format for legacy reporting, needs to
      ## set before CLI handling, because reports are just that awful
    hack*: HackController ## Configuration values for debug printing
    when defined(nimDebugUtils):
      debugUtilsStack*: seq[string] ## which proc name to stop trace output
      ## len is also used for output indent level

    when defined(nimDebugUnreportedErrors):
      unreportedErrors*: OrderedTable[NodeId, PNode]

const 
  IdeLocCmds* = {ideSug, ideCon, ideDef, ideUse, ideDus}
    ## IDE commands requiring source locations, related `MsgConfig.trackPos`

template `[]`*(conf: ConfigRef, idx: FileIndex): TFileInfo =
  conf.m.fileInfos[idx.uint32]

template passField(fieldname, fieldtype: untyped): untyped =
  template `fieldname`*(conf: ConfigRef): fieldtype =
    conf.active.fieldname

  template `fieldname=`*(conf: ConfigRef, val: fieldtype) =
    conf.active.fieldname = val

template passSetField(fieldname, fieldtype, itemtype: untyped): untyped =
  passField(fieldname, fieldtype)

  template incl*(conf: ConfigRef, item: itemtype | fieldtype) =
    conf.active.fieldname.incl item

  template excl*(conf: ConfigRef, item: itemtype | fieldtype) =
    conf.active.fieldname.excl item

template passStrTableField(fieldname: untyped): untyped =
  passField(fieldname, StringTableRef)

  template `fieldname Set`*(conf: ConfigRef, key: string, value: string) =
    conf.active.fieldname[key] = value

  template `fieldname Get`*(conf: ConfigRef, key: string): string =
    conf.active.fieldname[key]

  template `fieldname Del`*(conf: ConfigRef, key: string) =
    conf.active.fieldname.del key

template passSeqField(fieldname, itemtype: untyped): untyped =
  passField(fieldname, seq[itemtype])
  template `fieldname Add`*(conf: ConfigRef, item: itemtype | seq[itemtype]) =
    conf.active.fieldname.add item

passField backend,            TBackend
passField symbolFiles,        SymbolFilesOption
passField prefixDir,          AbsoluteDir
passField libpath,            AbsoluteDir
passField nimcacheDir,        AbsoluteDir
passField target,             Target
passField cppDefines,         HashSet[string]
passField cmd,                Command
passField selectedGC,         TGCMode
passField exc,                ExceptionSystem
passField cCompiler,          TSystemCC
passField filenameOption,     FilenameOption
passField numberOfProcessors, int
passField outFile,            RelativeFile
passField outDir,             AbsoluteDir
passField depfile,            AbsoluteFile
passField projectPath,        AbsoluteDir
passField projectName,        string
passField projectFull,        AbsoluteFile

passSeqField implicitImports,   string
passSeqField implicitIncludes,  string
passSeqField cIncludes,         AbsoluteDir
passSeqField cLibs,             AbsoluteDir
passSeqField cLinkedLibs,       string
passSeqField linkOptionsCmd,    string
passSeqField compileOptionsCmd, string
passSeqField nimblePaths,       AbsoluteDir
passSeqField searchPaths,       AbsoluteDir
passSeqField lazyPaths,         AbsoluteDir

passSetField localOptions,   TOptions,           TOption
passSetField globalOptions,  TGlobalOptions,     TGlobalOption
passSetField features,       set[Feature],       Feature

passStrTableField dllOverrides
passStrTableField configVars
passStrTableField symbols
passStrTableField macrosToExpand
passStrTableField arcToExpand

proc getCompileOptionsStr*(conf: ConfigRef): string =
  ## Returns the combination of the current C compile options and the
  ## global `--passC` options (combined in that exact order).
  ##
  ## Global `--passC` options already present in the current C compile
  ## options are *not* include again.
  result = conf.compileOptions

  for option in conf.compileOptionsCmd:
    if strutils.find(result, option, 0) < 0:
      if result.len == 0 or result[^1] != ' ': result.add " "
      result.add option

proc getLinkOptionsStr*(conf: ConfigRef): string =
  ## Returns the combination of the current C linker options and the global
  ## `--passL` options (combined in that exact order).
  conf.linkOptions & " " & conf.linkOptionsCmd.join(" ")

proc defineSymbol*(conf: ConfigRef, symbol: string, value: string = "true") =
  conf.symbolsSet(symbol, value)

proc undefSymbol*(conf: ConfigRef; symbol: string) =
  conf.symbolsDel(symbol)

iterator definedSymbolNames*(conf: ConfigRef): string =
  for key, val in pairs(conf.symbols):
    yield key

proc countDefinedSymbols*(conf: ConfigRef): int =
  conf.symbols.len

template changed(conf: ConfigRef, s: ConfNoteSet, body: untyped) =
  # Template for debugging purposes - single place to track all changes in
  # the enabled note sets.
  when defined(debug):
    let before {.used.} = conf.active.noteSets[s]
    body
    let after {.used.} = conf.active.noteSets[s]

    # let n = rintMsgOrigin
    # if (n in before) != (n in after):
    #   if n notin after:
    #     writeStackTrace()
    #     echo "changed conf $# -> $#" % [$(n in before), $(n in after)]

  else:
    body

proc incl*(conf: ConfigRef, nset: ConfNoteSet, note: ReportKind) =
  ## Include report kind in specified note set
  changed(conf, nset):
    conf.active.noteSets[nset].incl note

proc excl*(conf: ConfigRef, nset: ConfNoteSet, note: ReportKind) =
  ## Exclude report kind from the specified note set
  changed(conf, nset):
    conf.active.noteSets[nset].excl note

proc asgn*(conf: ConfigRef, nset: ConfNoteSet, notes: ReportKinds) =
  ## Assign to specified note set
  changed(conf, nset):
    conf.active.noteSets[nset] = notes

proc asgn*(conf: ConfigRef, sto, sfrom: ConfNoteSet) =
  ## Assign between two specified note sets
  conf.active.noteSets[sto] = conf.active.noteSets[sfrom]

proc flip*(
  conf: ConfigRef, nset: ConfNoteSet, note: ReportKind, state: bool) =
  ## Include or exlude node from the specified note set based on the
  ## `state`
  if state:
    conf.incl(nset, note)
  else:
    conf.excl(nset, note)

func options*(conf: ConfigRef): TOptions =
  ## Get list of active local options
  result = conf.localOptions

template changedOpts(conf: ConfigRef, body: untyped) =
  when defined(debug):
    let before = conf.localOptions
    body
    let after = conf.localOptions
    let removed {.used.} = (optHints in before) and (optHints notin after)

  else:
    body

proc `options=`*(conf: ConfigRef, opts: TOptions) =
  ## Assign to list of active local options
  changedOpts(conf):
    conf.localOptions = opts

proc modifiedyNotes*(conf: ConfigRef): ReportKinds =
  ## Get list of reports modified from the command line or config
  conf.active.noteSets[cnModifiedy]

proc cmdlineNotes*(conf: ConfigRef): ReportKinds =
  ## Get list of report filters modified from the command line
  conf.active.noteSets[cnCmdline]

proc foreignPackageNotes*(conf: ConfigRef): ReportKinds =
  ## Get list of reports for foreign packages
  conf.active.noteSets[cnForeign]

proc notes*(conf: ConfigRef): ReportKinds =
  ## Get list of active notes
  conf.active.noteSets[cnCurrent]

proc warningAsErrors*(conf: ConfigRef): ReportKinds =
  ## Get list of warning notes that are treated like errors
  conf.active.noteSets[cnWarnAsError]

proc hintsAsErrors*(conf: ConfigRef): ReportKinds =
  ## Get list of hint notes that are treated like errors
  conf.active.noteSets[cnHintAsError]

proc mainPackageNotes*(conf: ConfigRef): ReportKinds =
  ## Get list of notes for main package
  conf.active.noteSets[cnMainPackage]

proc `modifiedyNotes=`*(conf: ConfigRef, nset: ReportKinds) =
  ## Set list of notes modified from the cli/config
  conf.asgn cnModifiedy, nset

proc `cmdlineNotes=`*(conf: ConfigRef, nset: ReportKinds) =
  ## Set list of notes modified from the CLI
  conf.asgn cnCmdline, nset

proc `foreignPackageNotes=`*(conf: ConfigRef, nset: ReportKinds) =
  ## Set list of notes for foreign packages
  conf.asgn cnForeign, nset

proc `notes=`*(conf: ConfigRef, nset: ReportKinds) =
  ## Set list of active notes
  conf.asgn cnCurrent, nset

proc `warningAsErrors=`*(conf: ConfigRef, nset: ReportKinds) =
  ## Set list of warning notes to be treated as errors
  conf.asgn cnWarnAsError, nset

proc `hintsAsErrors=`*(conf: ConfigRef, nset: ReportKinds) =
  ## Set list of hint notes that are treated like erorrs
  conf.asgn cnHintAsError, nset

proc `mainPackageNotes=`*(conf: ConfigRef, nset: ReportKinds) =
  ## Set list of notes for main package
  conf.asgn cnMainPackage, nset

proc writelnHook*(conf: ConfigRef, msg: string, flags: MsgFlags = {}) =
  ## Write string using writeln hook
  conf.writelnHook(conf, msg, flags)

proc writeHook*(conf: ConfigRef, msg: string, flags: MsgFlags = {}) =
  ## Write string using write hook
  conf.writeHook(conf, msg, flags)

proc writeln*(conf: ConfigRef, args: varargs[string, `$`]) =
  ## writeln hook overload for varargs
  writelnHook(conf, args.join(""))

proc write*(conf: ConfigRef, args: varargs[string, `$`]) =
  ## write hook overload for varargs
  writeHook(conf, args.join(""))

proc setReportHook*(conf: ConfigRef, hook: ReportHook) =
  ## Set active report hook. Must not be nil
  assert hook != nil
  conf.structuredReportHook = hook

proc getReportHook*(conf: ConfigRef): ReportHook =
  ## Get active report hook
  conf.structuredReportHook

proc report*(conf: ConfigRef, inReport: Report): TErrorHandling =
  ## Write `inReport`
  assert inReport.kind != repNone, "Cannot write out empty report"
  assert(conf.structuredReportHook != nil,
         "Cannot write report with empty report hook")
  return conf.structuredReportHook(conf, inReport)

proc canReport*(conf: ConfigRef, id: NodeId): bool =
  ## Check whether report with given ID can actually be written out, or it
  ## has already been seen. This check is used to prevent multiple reports
  ## from the `nkError` node.
  id notin conf.m.writtenSemReports

proc canReport*(conf: ConfigRef, node: PNode): bool =
  ## Check whether `nkError` node can be reported
  # conf.canReport(node.nodeId)
  true # xxx: short circuit this nonsense

template report*[R: ReportTypes](
    conf: ConfigRef, inReport: R): TErrorHandling =
  ## Pass structured report object into `conf.structuredReportHook`,
  ## converting to `Report` variant and updaing instantiation info.
  report(conf, wrap(inReport, instLoc()))

template report*[R: ReportTypes](
    conf: ConfigRef, tinfo: TLineInfo, inReport: R): TErrorHandling =
  ## Write out new report, updating it's location info using `tinfo` and
  ## it's instantiation info with `instantiationInfo()` of the template.
  report(conf, wrap(inReport, instLoc(), tinfo))

# REFACTOR: we shouldn't need to dig into the internalReport and query severity
#           directly
from compiler/ast/reports_internal import severity

func isCompilerFatal*(conf: ConfigRef, report: Report): bool =
  ## Check if report stores fatal compilation error
  report.category == repInternal and
  report.internalReport.severity() == rsevFatal or
  report.kind == rextCmdRequiresFile

func severity*(conf: ConfigRef, report: ReportTypes | Report): ReportSeverity =
  # style checking is a hint by default, but can be globally overriden to
  # be treated as error via `--styleCheck:error`, and this is handled in
  # the different configuration as hints/warnings as errors
  if report.kind in repLinterKinds and optStyleError in conf.globalOptions:
    result = rsevError
  else:
    result = report.severity(conf.warningAsErrors + conf.hintsAsErrors)

func isCodeError*(conf: ConfigRef, report: Report): bool =
  ## Check if report stores a regular code error, or warning/hint that has
  ## been configured to be treated as error under "warningAsError"
  conf.severity(report) == rsevError

func ignoreMsgBecauseOfIdeTools(conf: ConfigRef, msg: ReportKind): bool =
  msg notin (repErrorKinds + repFatalKinds) and
  conf.cmd == cmdIdeTools and
  optIdeDebug notin conf.globalOptions

func useColor*(conf: ConfigRef): bool =
  optUseColors in conf.globalOptions

proc parseNimVersion*(a: string): NimVer =
  # could be moved somewhere reusable
  if a.len > 0:
    let b = a.split(".")
    assert b.len == 3, a
    template fn(i) = result[i] = b[i].parseInt # could be optimized if needed
    fn(0)
    fn(1)
    fn(2)

proc assignIfDefault*[T](result: var T, val: T, def = default(T)) =
  ## if `result` was already assigned to a value (that wasn't `def`), this is a noop.
  if result == def: result = val

template setErrorMaxHighMaybe*(conf: ConfigRef) =
  ## do not stop after first error (but honor --errorMax if provided)
  assignIfDefault(conf.errorMax, high(int))

proc setNoteDefaults*(conf: ConfigRef, note: ReportKind, enabled = true) =
  template fun(op) =
    conf.op cnCurrent, note
    conf.op cnMainPackage, note
    conf.op cnForeign, note

  if enabled: fun(incl) else: fun(excl)

proc setNote*(conf: ConfigRef, note: ReportKind, enabled = true) =
  ## see also `prepareConfigNotes` which sets notes
  if note notin conf.cmdlineNotes:
    if enabled:
      incl(conf, cnCurrent, note)

    else:
      excl(conf, cnCurrent, note)

proc hasHint*(conf: ConfigRef, note: ReportKind): bool =
  # ternary states instead of binary states would simplify logic
  if optHints notin conf.options:
    false
  elif note in {rextConf, rsemProcessing}:
    # could add here other special notes like hintSource
    # these notes apply globally.
    note in conf.mainPackageNotes
  else:
    note in conf.notes

proc hasWarn*(conf: ConfigRef, note: ReportKind): bool {.inline.} =
  ## Check if warnings are enabled and specific report kind is contained in
  ## the notes
  optWarns in conf.options and note in conf.notes

func isEnabled*(conf: ConfigRef, report: ReportKind): bool =
  ## Check whether report kind is allowed to be generated by the compiler.
  ## Uses `options.hasHint`, `options.hasWarn` to check whether particular
  ## report is enabled, otherwise use query global/local options.

  # Reports related to experimental features and inconsistent CLI flags
  # (such as `--styleCheck` which controls both CLI flags and hints) are
  # checked for with higher priority
  case report
  of repNilcheckKinds:
    strictNotNil in conf.features
  of rdbgVmExecTraceMinimal:
    conf.active.isVmTrace
  of rlexLinterReport, rsemLinterReport, rsemLinterReportUse:
    # Regular linter report is enabled if style check is either hint or
    # error, AND not `usages`
    {optStyleHint, optStyleError} * conf.globalOptions != {}
  else:
    case report
    # All other reports follow default hint category handing:
    of repHintKinds:                 conf.hasHint(report)
    of repWarningKinds:              conf.hasWarn(report)
    of repErrorKinds, repFatalKinds: true
    of repTraceKinds:
      # Semantic trace kinds are enabled by default and probably should
      # not be changed - but these reports might (in theory) be
      # modified at runtime.
      report in conf.notes
    else:
      (report in conf.notes) and
        not ignoreMsgBecauseOfIdeTools(conf, report)

func isEnabled*(conf: ConfigRef, report: Report): bool =
  ## Macro expansion configuration is done via `--expandMacro=name`
  ## configuration, and requires full report information to check.
  report.kind == rsemExpandMacro and
    conf.macrosToExpand.hasKey(report.semReport.sym.name.s) or
    conf.isEnabled(report.kind)

type
  ReportWritabilityKind* = enum
    writeEnabled
    writeDisabled
    writeForceEnabled

func writabilityKind*(conf: ConfigRef, r: Report): ReportWritabilityKind =
  let compTimeCtx = conf.m.errorOutputs == {}
    ## indicates whether we're in a `compiles` or `constant expression
    ## evaluation` context. `sem` and `semexprs` in particular will clear
    ## `conf.m.errorOutputs` as a signal for this. For more details see the
    ## comment for `MsgConfig.errorOutputs`.
  if r.category == repDebug and compTimeCtx:
    # Force write of the report messages using regular stdout if compTimeCtx
    # is enabled
    writeForceEnabled
  elif compTimeCtx:
    # Or we are in the special hack mode for `compiles()` processing
    # Return without writing
    writeDisabled
  else:
    writeEnabled

const
  oldExperimentalFeatures* = {dotOperators, callOperator}

  ChecksOptions* = {optObjCheck, optFieldCheck, optRangeCheck,
    optOverflowCheck, optBoundsCheck, optAssert, optNaNCheck, optInfCheck,
    optStyleCheck}

  DefaultOptions* = {optObjCheck, optFieldCheck, optRangeCheck,
    optBoundsCheck, optOverflowCheck, optAssert, optWarns,
    optHints, optStackTrace, optLineTrace, # consider adding `optStackTraceMsgs`
    optTrMacros, optStyleCheck, optCursorInference}
  DefaultGlobalOptions* = {optThreadAnalysis, optExcessiveStackTrace}

proc getSrcTimestamp(): DateTime =
  try:
    utc(fromUnix(parseInt(getEnv("SOURCE_DATE_EPOCH", "not a number"))))
  except ValueError:
    # Environment variable malformed.
    # https://reproducible-builds.org/specs/source-date-epoch/: "If the
    # value is malformed, the build process SHOULD exit with a non-zero
    # error code", which this doesn't do. This uses local time, because
    # that maintains compatibility with existing usage.
    utc getTime()

proc getDateStr*(): string =
  result = format(getSrcTimestamp(), "yyyy-MM-dd")

proc getClockStr*(): string =
  result = format(getSrcTimestamp(), "HH:mm:ss")

template newPackageCache*(): untyped =
  newStringTable(when FileSystemCaseSensitive:
                   modeCaseInsensitive
                 else:
                   modeCaseSensitive)

proc isDefined*(conf: ConfigRef; symbol: string): bool

const defaultHackController = HackController(
  semStack: off,
  reportInTrace: off,
  semTraceData: on,
  bypassWriteHookForTrace: true
)

proc computeNotesVerbosity(): tuple[
    main: array[CompilerVerbosity, ReportKinds],
    foreign: ReportKinds,
    base: ReportKinds
  ] =
  ## Create configuration sets for the default compilation report verbosity

  # Mandatory reports - cannot be turned off, present in all verbosity
  # settings
  result.base = (repErrorKinds + repInternalKinds)

  # Somewhat awkward handling - stack trace report cannot be error (because
  # actual error report must follow), so it is a hint-level report (can't
  # be debug because it is a user-facing, can't be "trace" because it is
  # not for compiler developers use only)
  result.base.incl {rvmStackTrace}

  when defined(debugOptions):
    # debug report for transition of the configuration options
    result.base.incl {rdbgOptionsPush, rdbgOptionsPop}

  when defined(nimVMDebugExecute):
    result.base.incl {
      rdbgVmExecTraceFull # execution of the generated code listings
    }

  result.main[compVerbosityMax] =
    result.base + repWarningKinds + repHintKinds - {
    rsemObservableStores,
    rsemResultUsed,
    rsemAnyEnumConvert,
    rbackLinking,

    rbackLinking,
    rbackCompiling,
    rcmdLinking,
    rcmdCompiling,

    rintErrKind
  }

  if defined(release):
    result.main[compVerbosityMax].excl rintStackTrace

  result.main[compVerbosityHigh] = result.main[compVerbosityMax] - {
    rsemUninit,
    rsemExtendedContext,
    rsemProcessingStmt,
    rsemWarnGcUnsafe,
    rextConf,
  }

  result.main[compVerbosityDefault] = result.main[compVerbosityHigh] -
    repPerformanceHints -
    {
      rsemProveField,
      rsemErrGcUnsafe,
      rsemHintLibDependency,
      rsemGlobalVar,

      rintMsgOrigin,

      rextPath,

      rlexSourceCodeFilterOutput,
    }

  result.main[compVerbosityMin] = result.main[compVerbosityDefault] - {
    rintSuccessX,
    rextConf,
    rsemProcessing,
    rsemPattern,
    rcmdExecuting,
    rbackLinking,
  }

  result.foreign = result.base + {
    rsemProcessing,
    rsemUserHint,
    rsemUserWarning,
    rsemUserHint,
    rsemUserWarning,
    rsemUserError,
    rintQuitCalled,
    rsemImplicitObjConv
  }

  for idx, n in @[
    result.foreign,
    # result.base,
    result.main[compVerbosityMax],
    result.main[compVerbosityHigh],
    result.main[compVerbosityDefault],
    result.main[compVerbosityMin],
  ]:
    assert rbackLinking notin n
    assert rsemImplicitObjConv in n, $idx
    assert rvmStackTrace in n, $idx

const
  NotesVerbosity* = computeNotesVerbosity()

proc initConfigRefCommon(conf: ConfigRef) =
  conf.symbols = newStringTable(modeStyleInsensitive)
  conf.selectedGC = gcUnselected
  conf.verbosity = compVerbosityDefault
  conf.hintProcessingDots = true
  conf.options = DefaultOptions
  conf.globalOptions = DefaultGlobalOptions
  conf.filenameOption = foAbs
  conf.foreignPackageNotes = NotesVerbosity.foreign
  conf.notes = NotesVerbosity.main[conf.verbosity]
  conf.hack = defaultHackController
  conf.mainPackageNotes = NotesVerbosity.main[conf.verbosity]
  when defined(nimDebugUtils):
    # ensures that `nimDebugUtils` is defined for the compiled code so it can
    # access the `system.nimCompilerDebugRegion` template
    if not conf.symbols.hasKey("nimDebugUtils"):
      conf.symbols["nimDebugUtils"] = ""

proc newConfigRef*(hook: ReportHook): ConfigRef =
  result = ConfigRef(
    structuredReportHook: hook,
    m: initMsgConfig(),
    headerFile: "",
    packageCache: newPackageCache(),
    cfileSpecificOptions: newStringTable(modeCaseSensitive),
    inputMode: pimFile,
    projectMainIdx: FileIndex(0'i32), # the canonical path id of the main module
    command: "", # the main command (e.g. cc, check, scan, etc)
    commandArgs: @[], # any arguments after the main command
    commandLine: "",
    commandLineSrcIdx: commandLineIdx, # set the command line as the source
    keepComments: true, # whether the parser needs to keep comments
    docSeeSrcUrl: "",
    active: CurrentConf(
      backend:        backendInvalid,
      cppDefines:     initHashSet[string](),
      features:       {},
      cCompiler:      ccGcc,
      macrosToExpand: newStringTable(modeStyleInsensitive),
      arcToExpand:    newStringTable(modeStyleInsensitive),
      configVars:     newStringTable(modeStyleInsensitive),
      dllOverrides:   newStringTable(modeCaseInsensitive),
      outFile:        RelativeFile"",
      outDir:         AbsoluteDir"",
    ),
    suggestMaxResults: 10_000,
    maxLoopIterationsVM: 10_000_000,
    spellSuggestMax: spellSuggestSecretSauce,
  )
  initConfigRefCommon(result)

  # xxx: do a silly dance because the APIs are dumb
  var target = result.target
  setTargetFromSystem(target)
  result.target = target

  # enable colors by default on terminals
  if terminal.isatty(stderr):
    incl(result, optUseColors)

proc newPartialConfigRef*(): ConfigRef =
  ## create a new ConfigRef that is only good enough for error reporting.
  result = ConfigRef()
  initConfigRefCommon(result)

proc cppDefine*(c: ConfigRef; define: string) =
  c.active.cppDefines.incl define

proc getStdlibVersion*(conf: ConfigRef): NimVer =
  if conf.nimStdlibVersion == (0,0,0):
    let s = conf.symbols.getOrDefault("nimVersion", "")
    conf.nimStdlibVersion = s.parseNimVersion
  result = conf.nimStdlibVersion

proc isDefined*(conf: CurrentConf; symbol: string): bool =
  if conf.symbols.hasKey(symbol):
    result = true
  elif cmpIgnoreStyle(symbol, CPU[conf.target.targetCPU].name) == 0:
    result = true
  elif cmpIgnoreStyle(symbol, platform.OS[conf.target.targetOS].name) == 0:
    result = true
  else:
    case symbol.normalize
    of "x86": result = conf.target.targetCPU == cpuI386
    of "itanium": result = conf.target.targetCPU == cpuIa64
    of "x8664": result = conf.target.targetCPU == cpuAmd64
    of "posix", "unix":
      result = conf.target.targetOS in {osLinux, osMorphos, osSkyos, osIrix, osPalmos,
                            osQnx, osAtari, osAix,
                            osHaiku, osVxWorks, osSolaris, osNetbsd,
                            osFreebsd, osOpenbsd, osDragonfly, osMacosx, osIos,
                            osAndroid, osNintendoSwitch, osFreeRTOS, osCrossos}
    of "linux":
      result = conf.target.targetOS in {osLinux, osAndroid}
    of "bsd":
      result = conf.target.targetOS in {osNetbsd, osFreebsd, osOpenbsd, osDragonfly, osCrossos}
    of "freebsd":
      result = conf.target.targetOS in {osFreebsd, osCrossos}
    of "emulatedthreadvars":
      result = platform.OS[conf.target.targetOS].props.contains(ospLacksThreadVars)
    of "msdos": result = conf.target.targetOS == osDos
    of "mswindows", "win32": result = conf.target.targetOS == osWindows
    of "macintosh":
      result = conf.target.targetOS in {osMacos, osMacosx, osIos}
    of "osx", "macosx":
      result = conf.target.targetOS in {osMacosx, osIos}
    of "sunos": result = conf.target.targetOS == osSolaris
    of "nintendoswitch":
      result = conf.target.targetOS == osNintendoSwitch
    of "freertos", "lwip":
      result = conf.target.targetOS == osFreeRTOS
    of "littleendian": result = CPU[conf.target.targetCPU].endian == littleEndian
    of "bigendian": result = CPU[conf.target.targetCPU].endian == bigEndian
    of "cpu8": result = CPU[conf.target.targetCPU].bit == 8
    of "cpu16": result = CPU[conf.target.targetCPU].bit == 16
    of "cpu32": result = CPU[conf.target.targetCPU].bit == 32
    of "cpu64": result = CPU[conf.target.targetCPU].bit == 64
    of "nimrawsetjmp":
      result = conf.target.targetOS in {osSolaris, osNetbsd, osFreebsd, osOpenbsd,
                            osDragonfly, osMacosx}
    else: discard

proc isDefined*(conf: ConfigRef, symbol: string): bool =
  conf.active.isDefined(symbol)

proc getDefined*(conf: ConfigRef, sym: string): string =
  conf.symbols[sym]

template quitOrRaise*(conf: ConfigRef, msg = "") =
  # xxx in future work, consider whether to also intercept `msgQuit` calls
  if conf.isDefined("nimDebug"):
    doAssert false, msg
  else:
    quit(msg) # quits with QuitFailure

proc importantComments*(conf: ConfigRef): bool {.inline.} = conf.cmd in cmdDocLike + {cmdIdeTools}

template compilationCachePresent*(conf: ConfigRef): untyped =
  false
#  conf.symbolFiles in {v2Sf, writeOnlySf}

template optPreserveOrigSource*(conf: ConfigRef): untyped =
  optEmbedOrigSrc in conf.globalOptions

proc mainCommandArg*(conf: ConfigRef): string =
  ## This is intended for commands like check or parse
  ## which will work on the main project file unless
  ## explicitly given a specific file argument
  if conf.commandArgs.len > 0:
    result = conf.commandArgs[0]
  else:
    result = conf.projectName

proc existsConfigVar*(conf: ConfigRef; key: string): bool =
  result = hasKey(conf.configVars, key)

proc getConfigVar*(conf: ConfigRef; key: string, default = ""): string =
  result = conf.configVars.getOrDefault(key, default)

proc setConfigVar*(conf: ConfigRef; key, val: string) =
  conf.configVars[key] = val

proc getOutFile*(conf: ConfigRef; filename: RelativeFile, ext: string): AbsoluteFile =
  # explains regression https://github.com/nim-lang/Nim/issues/6583#issuecomment-625711125
  # Yet another reason why "" should not mean ".";  `""/something` should raise
  # instead of implying "" == "." as it's bug prone.
  doAssert conf.outDir.string.len > 0
  result = conf.outDir / changeFileExt(filename, ext)

proc absOutFile*(conf: ConfigRef): AbsoluteFile =
  doAssert not conf.outDir.isEmpty
  doAssert not conf.outFile.isEmpty
  result = conf.outDir / conf.outFile
  when defined(posix):
    if dirExists(result.string): result.string.add ".out"

proc prepareToWriteOutput*(conf: ConfigRef): AbsoluteFile =
  ## Create the output directory and returns a full path to the output file
  result = conf.absOutFile
  createDir result.string.parentDir

proc getPrefixDir*(conf: ConfigRef): AbsoluteDir =
  ## Gets the prefix dir, usually the parent directory where the binary
  ## resides.
  ##
  ## This is overridden by some tools (namely nimsuggest) via the
  ## ``conf.prefixDir`` field. This should resolve to root of nim sources,
  ## whether running nim from a local clone or using installed nim, so that
  ## these exist: `result/doc/advopt.txt` and `result/lib/system.nim`
  if not conf.prefixDir.isEmpty:
    result = conf.prefixDir
  else:
    result = AbsoluteDir splitPath(getAppDir()).head

proc setDefaultLibpath*(conf: ConfigRef) =
  ## set default value (can be overwritten):
  if conf.libpath.isEmpty:
    # choose default libpath:
    var prefix = getPrefixDir(conf)
    when defined(posix):
      if prefix == AbsoluteDir"/usr":
        conf.libpath = AbsoluteDir"/usr/lib/nim"
      elif prefix == AbsoluteDir"/usr/local":
        conf.libpath = AbsoluteDir"/usr/local/lib/nim"
      else:
        conf.libpath = prefix / RelativeDir"lib"
    else:
      conf.libpath = prefix / RelativeDir"lib"

    # Special rule to support other tools (nimble) which import the compiler
    # modules and make use of them.
    let realNimPath = findExe("nim")
    # Find out if $nim/../../lib/system.nim exists.
    let parentNimLibPath = realNimPath.parentDir.parentDir / "lib"
    if not fileExists(conf.libpath.string / "system.nim") and
        fileExists(parentNimLibPath / "system.nim"):
      conf.libpath = AbsoluteDir parentNimLibPath

proc canonicalizePath*(conf: ConfigRef; path: AbsoluteFile): AbsoluteFile =
  result = AbsoluteFile path.string.expandFilename

proc setFromProjectName*(conf: ConfigRef; projectName: string) =
  try:
    conf.projectFull = canonicalizePath(conf, AbsoluteFile projectName)
  except OSError:
    conf.projectFull = AbsoluteFile projectName
  let p = splitFile(conf.projectFull)
  let dir = if p.dir.isEmpty: AbsoluteDir getCurrentDir() else: p.dir
  conf.projectPath = AbsoluteDir canonicalizePath(conf, AbsoluteFile dir)
  conf.projectName = p.name

proc removeTrailingDirSep*(path: string): string =
  if (path.len > 0) and (path[^1] == DirSep):
    result = substr(path, 0, path.len - 2)
  else:
    result = path

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

proc makeCString*(s: string): string =
  result = newStringOfCap(int(s.len.toFloat * 1.1) + 1)
  result.add("\"")
  for i in 0..<s.len:
    # line wrapping of string litterals in cgen'd code was a bad idea, e.g. causes: bug #16265
    # It also makes reading c sources or grepping harder, for zero benefit.
    # const MaxLineLength = 64
    # if (i + 1) mod MaxLineLength == 0:
    #   res.add("\"\L\"")
    toCChar(s[i], result)
  result.add('\"')

proc newFileInfo(fullPath: AbsoluteFile, projPath: RelativeFile): TFileInfo =
  result.fullPath = fullPath
  #shallow(result.fullPath)
  result.projPath = projPath
  #shallow(result.projPath)
  result.shortName = fullPath.extractFilename
  result.quotedName = result.shortName.makeCString
  result.quotedFullName = fullPath.string.makeCString
  result.lines = @[]

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
  result = conf.m.rawPathToIndexTbl.getOrDefault(filename.string, InvalidFileIdx)
  if result != InvalidFileIdx:
    return
  var
    canon: AbsoluteFile
    pseudoPath = false

  try:
    canon = canonicalizePath(conf, filename)
  except OSError:
    canon = filename
    # The compiler uses "filenames" such as `command line` or `stdin`
    # This flag indicates that we are working with such a path here
    pseudoPath = true

  var canon2 = canon.string
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

  conf.m.rawPathToIndexTbl[filename.string] = result

proc fileInfoIdx*(conf: ConfigRef; filename: AbsoluteFile): FileIndex =
  var dummy: bool
  result = fileInfoIdx(conf, filename, dummy)

proc newLineInfo*(conf: ConfigRef; filename: AbsoluteFile, line, col: int): TLineInfo {.inline.} =
  result = newLineInfo(fileInfoIdx(conf, filename), line, col)

proc disableNimblePath*(conf: ConfigRef) =
  conf.incl optNoNimblePath
  conf.lazyPaths = @[]
  conf.nimblePaths = @[]

proc clearNimblePath*(conf: ConfigRef) =
  conf.lazyPaths = @[]
  conf.nimblePaths = @[]

include compiler/modules/packagehandling

proc getOsCacheDir(): string =
  when defined(posix):
    result = getEnv("XDG_CACHE_HOME", getHomeDir() / ".cache") / "nimskull"
  else:
    result = getHomeDir() / genSubDir.string

proc getNimcacheDir*(conf: CurrentConf): AbsoluteDir =
  proc nimcacheSuffix(conf: CurrentConf): string =
    if conf.cmd == cmdCheck: "_check"
    elif isDefined(conf, "release") or isDefined(conf, "danger"): "_r"
    else: "_d"

  # XXX projectName should always be without a file extension!
  result = if not conf.nimcacheDir.isEmpty:
             conf.nimcacheDir
           elif conf.backend == backendJs:
             if conf.outDir.isEmpty:
               conf.projectPath / genSubDir
             else:
               conf.outDir / genSubDir
           else:
            AbsoluteDir(getOsCacheDir() / splitFile(conf.projectName).name &
               nimcacheSuffix(conf))

proc getNimcacheDir*(conf: ConfigRef): AbsoluteDir =
  conf.active.getNimcacheDir()

proc pathSubs*(conf: ConfigRef; p, config: string): string =
  ## Substitute text `p` with configuration paths, such as project name,
  ## nim cache directory, project directory etc. `config` is an argument of
  ## the configuration file path in case `$config` template is used.
  let home = removeTrailingDirSep(os.getHomeDir())
  result = unixToNativePath(p % [
    "nim", getPrefixDir(conf).string,
    "lib", conf.libpath.string,
    "home", home,
    "config", config,
    "projectname", conf.projectName,
    "projectpath", conf.projectPath.string,
    "projectdir", conf.projectPath.string,
    "nimcache", getNimcacheDir(conf).string]).expandTilde

iterator nimbleSubs*(conf: ConfigRef; p: string): string =
  ## Iterate over possible interpolations of the path string `p` and known
  ## package directories.
  let pl = p.toLowerAscii
  if "$nimblepath" in pl or "$nimbledir" in pl:
    for i in countdown(conf.nimblePaths.len-1, 0):
      let nimblePath = removeTrailingDirSep(conf.nimblePaths[i].string)
      yield p % ["nimblepath", nimblePath, "nimbledir", nimblePath]
  else:
    yield p

proc toGeneratedFile*(
    conf: CurrentConf | ConfigRef,
    path: AbsoluteFile,
    ext: string
  ): AbsoluteFile =
  ## converts "/home/a/mymodule.nim", "rod" to "/home/a/nimskullcache/mymodule.rod"
  result = getNimcacheDir(conf) / RelativeFile(
    path.string.splitPath.tail.changeFileExt(ext))

proc completeGeneratedFilePath*(conf: ConfigRef; f: AbsoluteFile,
                                createSubDir: bool = true): AbsoluteFile =
  let subdir = getNimcacheDir(conf.active)
  if createSubDir:
    try:
      createDir(subdir.string)
    except OSError:
      conf.quitOrRaise "cannot create directory: " & subdir.string
  result = subdir / RelativeFile f.string.splitPath.tail
  #echo "completeGeneratedFilePath(", f, ") = ", result

proc completeGeneratedExtFilePath*(conf: ConfigRef, f: AbsoluteFile
                                  ): AbsoluteFile =
  ## Returns the absolute file path within the cache directory for file `f`.
  ## This procedure is meant to be used for external files with names not
  ## controlled by the compiler -- a sub-directory is used to prevent
  ## collisions.
  let subdir = getNimcacheDir(conf.active) / RelativeDir("external")
  try:
    createDir(subdir.string)
  except OSError:
    conf.quitOrRaise "cannot create directory: " & subdir.string
  result = subdir / RelativeFile(f.string.splitPath.tail)

proc toRodFile*(conf: ConfigRef; f: AbsoluteFile; ext = RodExt): AbsoluteFile =
  result = changeFileExt(completeGeneratedFilePath(conf,
    withPackageName(conf, f)), ext)

proc rawFindFile(conf: ConfigRef; f: RelativeFile; suppressStdlib: bool): AbsoluteFile =
  ## Find file using list of explicit search paths
  for it in conf.searchPaths:
    if suppressStdlib and it.string.startsWith(conf.libpath.string):
      continue
    result = it / f
    if fileExists(result):
      return canonicalizePath(conf, result)
  result = AbsoluteFile""

proc rawFindFile2(conf: ConfigRef; f: RelativeFile): AbsoluteFile =
  ## Find file using list of lazy paths. If relative file is found bring
  ## lazy path forward. TODO - lazy path reordering appears to be an
  ## 'optimization' feature, but it might have some implicit dependencies
  ## elsewhere.
  for i, it in conf.lazyPaths:
    result = it / f
    if fileExists(result):
      # bring to front
      for j in countdown(i, 1):
        swap(conf.active.lazyPaths[j], conf.active.lazyPaths[j-1])

      return canonicalizePath(conf, result)
  result = AbsoluteFile""

when not declared(isRelativeTo):
  proc isRelativeTo(path, base: string): bool =
    # pending #13212 use os.isRelativeTo
    let path = path.normalizedPath
    let base = base.normalizedPath
    let ret = relativePath(path, base)
    result = path.len > 0 and not ret.startsWith ".."

const stdlibDirs = [
  "pure", "core", "arch",
  "pure/collections",
  "pure/concurrency",
  "pure/unidecode", "impure",
  "wrappers", "wrappers/linenoise",
  "windows", "posix", "js"]

const
  pkgPrefix = "pkg/"
  stdPrefix = "std/"

proc getRelativePathFromConfigPath*(conf: ConfigRef; f: AbsoluteFile, isTitle = false): RelativeFile =
  let f = $f
  if isTitle:
    for dir in stdlibDirs:
      let path = conf.libpath.string / dir / f.lastPathPart
      if path.cmpPaths(f) == 0:
        return RelativeFile(stdPrefix & f.splitFile.name)
  template search(paths) =
    for it in paths:
      let it = $it
      if f.isRelativeTo(it):
        return relativePath(f, it).RelativeFile
  search(conf.searchPaths)
  search(conf.lazyPaths)

proc findFile*(conf: ConfigRef; f: string; suppressStdlib = false): AbsoluteFile =
  ## Find module file using search paths or lazy search paths (in that
  ## order). If suppress stdlib is used - do not try to return files that
  ## start with current `conf.libpath` prefix. First explicit search paths
  ## are queried, and then lazy load paths (generated from directories) are
  ## used.
  if f.isAbsolute:
    result = if f.fileExists: AbsoluteFile(f) else: AbsoluteFile""
  else:
    result = rawFindFile(conf, RelativeFile f, suppressStdlib)
    if result.isEmpty:
      result = rawFindFile(conf, RelativeFile f.toLowerAscii, suppressStdlib)
      if result.isEmpty:
        result = rawFindFile2(conf, RelativeFile f)
        if result.isEmpty:
          result = rawFindFile2(conf, RelativeFile f.toLowerAscii)

proc findModule*(conf: ConfigRef; modulename, currentModule: string): AbsoluteFile =
  ## Return absolute path to the imported module `modulename`. Imported
  ## path can be relative to the `currentModule`, absolute one, `std/` or
  ## `pkg/`-prefixed. In case of `pkg/` prefix it is dropped and search is
  ## performed again, while ignoring stdlib.
  ##
  ## Search priority is
  ##
  ## 1. `pkg/` prefix
  ## 2. Stdlib prefix
  ## 3. Relative to the current file
  ## 4. Search in the `--path` (see `findFile` and `rawFindFile`)
  ##
  ## If the module is found and exists module override, apply it last.
  var m = addFileExt(modulename, NimExt)
  if m.startsWith(pkgPrefix):
    result = findFile(conf, m.substr(pkgPrefix.len), suppressStdlib = true)
  else:
    if m.startsWith(stdPrefix):
      let stripped = m.substr(stdPrefix.len)
      for candidate in stdlibDirs:
        let path = (conf.libpath.string / candidate / stripped)
        if fileExists(path):
          result = AbsoluteFile path
          break
    else: # If prefixed with std/ why would we add the current module path!
      let currentPath = currentModule.splitFile.dir
      result = AbsoluteFile currentPath / m
    if not fileExists(result):
      result = findFile(conf, m)

proc findProjectNimFile*(conf: ConfigRef; pkg: string): string =
  ## Find configuration file for a current project
  const extensions = [".nims", ".cfg", ".nimble"]
    # xxx: remove '.nimble' (and nimble files from compiler src)
  var
    candidates: seq[string] = @[]
    dir = pkg
    prev = dir
    nimblepkg = ""
  let pkgname = pkg.lastPathPart()
  while true:
    for k, f in os.walkDir(dir, relative = true):
      if k == pcFile and f != "config.nims":
        let (_, name, ext) = splitFile(f)
        if ext in extensions:
          let x = changeFileExt(dir / name, ".nim")
          if fileExists(x):
            candidates.add x
          if ext == ".nimble":
            if nimblepkg.len == 0:
              nimblepkg = name
              # Since nimble packages can have their source in a subfolder,
              # check the last folder we were in for a possible match.
              if dir != prev:
                let x = prev / x.extractFilename()
                if fileExists(x):
                  candidates.add x
            else:
              # If we found more than one nimble file, chances are that we
              # missed the real project file, or this is an invalid nimble
              # package. Either way, bailing is the better choice.
              return ""
    let pkgname = if nimblepkg.len > 0: nimblepkg else: pkgname
    for c in candidates:
      if pkgname in c.extractFilename(): return c
    if candidates.len > 0:
      return candidates[0]
    prev = dir
    dir = parentDir(dir)
    if dir == "": break
  return ""

proc canonicalImportAux*(conf: ConfigRef, file: AbsoluteFile): string =
  ## canonical module import filename, e.g.: system.nim, std/tables.nim,
  ## system/assertions.nim, etc. Canonical module import filenames follow the
  ## same rules as canonical imports (see `canonicalImport`), except the module
  ## name is followed by a `.nim` file extension, and the directory separators
  ## are OS specific.
  let
    desc = getPkgDesc(conf, file.string)
    (_, moduleName, ext) = file.splitFile
  if desc.pkgKnown and
     desc.pkgFile != AbsoluteFile(conf.getNimbleFile(conf.projectFull.string)):
    # we ignore the pkg root name for intra-package module imports, allows for
    # easier pkg renames (without changing all files using canonical imports).
    result = desc.pkgRootName
    if desc.pkgSubpath != "":
      result = result / desc.pkgSubpath
  else:
    result = desc.pkgSubpath
  result = if result == "": moduleName else: result / moduleName
  result = result.changeFileExt(ext) # since we lost it above

proc canonicalImport*(conf: ConfigRef, file: AbsoluteFile): string =
  ## Shows the canonical module import, e.g.: system, std/tables,
  ## fusion/pointers, system/assertions, std/private/asciitables
  ## 
  ## A canonical import path is:
  ## 
  ## - typically `pkgroot/pkgsubpath/module`
  ## - if a module is at the base of a package, then `pkgroot/module`
  ## - if a module is within the project's package, `pkgroot` is skipped like
  ##   so `pkgsubpath/module` or `module` (if the module is at the package root).
  let ret = canonicalImportAux(conf, file)
  result = ret.nativeToUnixPath.changeFileExt("")

proc canonDynlibName*(s: string): string =
  ## Get 'canonical' dynamic library name - without optional `lib` prefix
  ## on linux and optional version pattern or extension. `libgit2.so -> git2`
  let start = if s.startsWith("lib"): 3 else: 0
  let ende = strutils.find(s, {'(', ')', '.'})
  if ende >= 0:
    result = s.substr(start, ende-1)
  else:
    result = s.substr(start)

proc inclDynlibOverride*(conf: ConfigRef; lib: string) =
  conf.dllOverrides[lib.canonDynlibName] = "true"

proc isDynlibOverride*(conf: ConfigRef; lib: string): bool =
  result = optDynlibOverrideAll in conf.globalOptions or
     conf.dllOverrides.hasKey(lib.canonDynlibName)

proc parseIdeCmd*(s: string): IdeCmd =
  case s:
  of "sug": ideSug
  of "con": ideCon
  of "def": ideDef
  of "use": ideUse
  of "dus": ideDus
  of "chk": ideChk
  of "mod": ideMod
  of "highlight": ideHighlight
  of "outline": ideOutline
  of "known": ideKnown
  of "msg": ideMsg
  of "project": ideProject
  else: ideNone

proc `$`*(c: IdeCmd): string =
  case c:
  of ideSug: "sug"
  of ideCon: "con"
  of ideDef: "def"
  of ideUse: "use"
  of ideDus: "dus"
  of ideChk: "chk"
  of ideMod: "mod"
  of ideNone: "none"
  of ideHighlight: "highlight"
  of ideOutline: "outline"
  of ideKnown: "known"
  of ideMsg: "msg"
  of ideProject: "project"

proc floatInt64Align*(conf: ConfigRef): int16 =
  ## Returns either 4 or 8 depending on reasons.
  if conf != nil and conf.target.targetCPU == cpuI386:
    #on Linux/BSD i386, double are aligned to 4bytes (except with -malign-double)
    if conf.target.targetOS != osWindows:
      # on i386 for all known POSIX systems, 64bits ints are aligned
      # to 4bytes (except with -malign-double)
      return 4
  return 8

const
  commandLineDesc* = "command line"

template toFilename*(conf: ConfigRef; fileIdx: FileIndex): string =
  if fileIdx.int32 < 0 or conf == nil:
    (if fileIdx == commandLineIdx: commandLineDesc else: "???")
  else:
    conf[fileIdx].shortName

template toFilename*(conf: ConfigRef; info: TLineInfo): string =
  toFilename(conf, info.fileIndex)

proc inFile*(
    conf: ConfigRef,
    info: TLineInfo,
    file: string,
    lrange: Slice[int] = low(int) .. high(int)
  ): bool {.deprecated: "DEBUG proc, do not use in the final build!",
            noSideEffect.} =
  ## `true` if `info` has `file`name and is within the specified line range
  ## (`lrange`), else `false`. Meant for debugging -- it's slow.
  {.cast(noSideEffect).}: # ignore side-effect tracking
    return file in toFilename(conf, info) and info.line.int in lrange

func inDebug*(conf: ConfigRef): bool {.
  deprecated: "DEBUG proc, do not use in the final build!",
  noSideEffect.} =
  ## Check whether 'nim compiler debug' is defined right now.
  return conf.isDefined("nimCompilerDebug")
