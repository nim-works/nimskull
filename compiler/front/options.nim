#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/[os, strutils, strtabs, sets, tables],
  utils/[prefixmatches, pathutils, platform],
  ast/[reports, lineinfos],
  modules/nimpaths

import ./in_options
export in_options

from terminal import isatty
from times import utc, fromUnix, local, getTime, format, DateTime
from std/private/globs import nativeToUnixPath

const
  hasTinyCBackend* = defined(tinyc)
  useEffectSystem* = true
  useWriteTracking* = false
  copyrightYear* = "2022"

  nimEnableCovariance* = defined(nimEnableCovariance)

const
  harmlessOptions* = {optForceFullMake, optNoLinking, optRun, optUseColors, optStdout}
  genSubDir* = RelativeDir"nimcache"
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
  oKeepVariableNames* = true
  spellSuggestSecretSauce* = -1

const
  cmdBackends* = {cmdCompileToC, cmdCompileToCpp, cmdCompileToOC, cmdCompileToJS, cmdCrun}
  cmdDocLike* = {cmdDoc0, cmdDoc, cmdDoc2tex, cmdJsondoc0, cmdJsondoc,
                 cmdCtags, cmdBuildindex}

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
    nimname*: string ## Original name of the nim file, constructed from the
    ## module name.
    cname*, obj*: AbsoluteFile
    flags*: set[CfileFlag]
    customArgs*: string

  CfileList* = seq[Cfile]

  Suggest* = ref object
    section*: IdeCmd
    qualifiedPath*: seq[string]
    name*: ptr string         ## not used beyond sorting purposes; name is also
                              ## part of 'qualifiedPath'
    filePath*: string
    line*: int                   ## Starts at 1
    column*: int                 ## Starts at 0
    doc*: string           ## Unescaped documentation string
    forth*: string               ## type
    quality*: range[0..100]   ## matching quality
    isGlobal*: bool ## is a global variable
    contextFits*: bool ## type/non-type context matches
    prefix*: PrefixMatch
    symkind*: byte
    scope*, localUsages*, globalUsages*: int # more usages is better
    tokenLen*: int
    version*: int
  Suggestions* = seq[Suggest]

  ProfileInfo* = object
    time*: float
    count*: int

  ProfileData* = ref object
    data*: TableRef[TLineInfo, ProfileInfo]

  StdOrrKind* = enum
    stdOrrStdout
    stdOrrStderr

  MsgFlag* = enum  ## flags altering msgWriteln behavior
    msgStdout,     ## force writing to stdout, even stderr is default
    msgNoUnitSep  ## the message is a complete "paragraph".
  MsgFlags* = set[MsgFlag]

  TErrorHandling* = enum
    doDefault ## Default action, custom report hook can return this in
              ## order for automatic handing to decide appropriate
              ## reaction.
    doNothing ## Don't do anything
    doAbort ## Immediately abort compilation
    doRaise ## Raise recoverable error

  ReportHook* = proc(conf: ConfigRef, report: Report): TErrorHandling {.closure.}

  HackController* = object
    ## additional configuration switches to control the behavior of the
    ## debug printer. Most of them are for compiler debugging, and for now
    ## they can't be set up from the cli/defines - in the future this will
    ## be changed. For now you can just edit `defaultHackController` value
    ## in this module as you see fit.
    semStack*: bool  ## Show `| context` entries in the call tracer

    reportInTrace*: bool ## Error messages are shown with matching indentation
    ## if report was triggered during execution of the sem trace

    semTraceData*: bool ## For each sem step show processed data, or only
    ## procedure calls.

  ConfigRef* {.acyclic.} = ref object ## every global configuration
                          ## fields marked with '*' are subject to
                          ## the incremental compilation mechanisms
                          ## (+) means "part of the dependency"

    # active configuration handling
    active*: CurrentConf

    spellSuggestMax*: int ## max number of spelling suggestions for typos

    # 'active' configuration end

    # Set and only read for `testCompileOptionArg`, so not sure if this is
    # 'active' configuration
    verbosity*: int           ## how verbose the compiler is

    # 'arguments' aka a single string aka 'joining strings for external
    # program is bad'
    arguments*: string ## the arguments to be passed to the program that
                       ## should be run


    linesCompiled*: int   # all lines that have been compiled
    m*: MsgConfig
    unitSep*: string ## Unit separator between compiler messages
    evalTemplateCounter*: int ## Template instantiation depth used to guard
    ## against infinite expansion recursion
    evalMacroCounter*: int ## Macro instantiation depth, used to guard
    ## against infinite macro expansion recursion
    exitcode*: int8


    # `--eval` flag handling
    cmdInput*: string    ## Code to evaluate from `--eval` switch
    projectIsCmd*: bool  ## whether we're compiling from a command input (`--eval` switch)
    implicitCmd*: bool   ## whether some flag triggered an implicit `command` (`--eval`)


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

    packageCache*: StringTableRef

    jsonBuildFile*: AbsoluteFile
    nimStdlibVersion*: NimVer
    moduleOverrides*: StringTableRef
    cfileSpecificOptions*: StringTableRef ## File specific compilation options for C backend.
    ## Modified by `{.localPassc.}`
    projectIsStdin*: bool           ## whether we're compiling from stdin
    lastMsgWasDot*: set[StdOrrKind] ## the last compiler message was a single '.'
    projectMainIdx*: FileIndex      ## the canonical path id of the main module
    projectMainIdx2*: FileIndex     ## consider merging with projectMainIdx
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
    suggestVersion*: int
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
    vmProfileData*: ProfileData

    hack*: HackController ## Configuration values for debug printing

    when defined(nimDebugUtils):
      debugUtilsStack*: seq[string] ## which proc name to stop trace output
      ## len is also used for output indent level

    when defined(nimDebugUnreportedErrors):
      unreportedErrors*: OrderedTable[ReportId, PNode]

template `[]`*(conf: ConfigRef, idx: FileIndex): TFileInfo =
  conf.m.fileInfos[idx.uint32]

template passField(fieldname, fieldtype: untyped): untyped =
  proc `fieldname`*(conf: ConfigRef): fieldtype =
    conf.active.fieldname

  proc `fieldname=`*(conf: ConfigRef, val: fieldtype) =
    conf.active.fieldname = val


template passSetField(fieldname, fieldtype, itemtype: untyped): untyped =
  passField(fieldname, fieldtype)

  proc incl*(conf: ConfigRef, item: itemtype | fieldtype) =
    conf.active.fieldname.incl item

  proc excl*(conf: ConfigRef, item: itemtype | fieldtype) =
    conf.active.fieldname.excl item

template passStrTableField(fieldname: untyped): untyped =
  passField(fieldname, StringTableRef)

  proc `fieldname Set`*(conf: ConfigRef, key: string, value: string) =
    conf.active.fieldname[key] = value

  proc `fieldname Get`*(conf: ConfigRef, key: string): string =
    conf.active.fieldname[key]

  proc `fieldname Del`*(conf: ConfigRef, key: string) =
    conf.active.fieldname.del key

template passSeqField(fieldname, itemtype: untyped): untyped =
  passField(fieldname, seq[itemtype])
  proc `fieldname Add`*(conf: ConfigRef, item: itemtype | seq[itemtype]) =
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
passField projectPath,        AbsoluteDir
passField projectName,        string
passField projectFull,        AbsoluteFile
passField cppCustomNamespace, string

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
passSetField legacyFeatures, set[LegacyFeature], LegacyFeature

passStrTableField dllOverrides
passStrTableField configVars
passStrTableField symbols
passStrTableField macrosToExpand
passStrTableField arcToExpand


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
    let before = conf.active.noteSets[s]
    body
    let after = conf.active.noteSets[s]

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
    let removed = (optHints in before) and (optHints notin after)

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
  ## Write string usign write hook
  conf.writeHook(conf, msg, flags)

proc writeln*(conf: ConfigRef, args: varargs[string, `$`]) =
  ## writeln hook overload for varargs
  writelnHook(conf, args.join(""))

proc write*(conf: ConfigRef, args: varargs[string, `$`]) =
  ## write hook overload for varargs
  writeHook(conf, args.join(""))

proc setReportHook*(conf: ConfigRef, hook: ReportHook) =
  ## Set active report hook. Must not be nil
  assert not hook.isNil
  conf.structuredReportHook = hook

proc getReportHook*(conf: ConfigRef): ReportHook =
  ## Get active report hook
  conf.structuredReportHook

proc report*(conf: ConfigRef, inReport: Report): TErrorHandling =
  ## Write `inReport`
  assert inReport.kind != repNone, "Cannot write out empty report"
  assert(
    not conf.structuredReportHook.isNil,
    "Cannot write report with empty report hook")
  return conf.structuredReportHook(conf, inReport)

proc canReport*(conf: ConfigRef, id: ReportId): bool =
  ## Check whether report with given ID can actually be written out, or it
  ## has already been seen. This check is used to prevent multiple reports
  ## from the `nkError` node.
  id notin conf.m.writtenSemReports

proc canReport*(conf: ConfigRef, node: PNode): bool =
  ## Check whether `nkError` node can be reported
  conf.canReport(node.reportId)

proc report*(conf: ConfigRef, id: ReportId): TErrorHandling =
  ## Write out existing stored report unless it has already been reported
  ## (can happen with multiple `nkError` visitations).
  ##
  ## .. note:: This check is done only for reports that are generated via
  ##           IDs, because that's the only way report can (supposedly)
  ##           enter the error message system twice.
  return conf.report(conf.m.reports.getReport(id))

proc report*(conf: ConfigRef, node: PNode): TErrorHandling =
  ## Write out report from the nkError node
  assert node.kind == nkError
  return conf.report(node.reportId)

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

proc addReport*(conf: ConfigRef, report: Report): ReportId =
  ## Add new postponed report, return it's stored ID
  result = conf.m.reports.addReport(report)
  assert not result.isEmpty(), $result

proc getReport*(conf: ConfigRef, report: ReportId): Report =
  ## Get postponed report from the current report list
  assert not report.isEmpty(), $report
  result = conf.m.reports.getReport(report)

proc getReport*(conf: ConfigRef, err: PNode): Report =
  ## Get postponed report from the nkError node
  conf.getReport(err.reportId)

template store*(conf: ConfigRef, report: ReportTypes): untyped =
  ## Add report to the postponed list, return new report ID
  conf.addReport(wrap(report, instLoc()))

template store*(
    conf: ConfigRef, linfo: TLineInfo, report: ReportTypes): untyped =
  ## Add report with given location information to the postponed list
  conf.addReport(wrap(report, instLoc(), linfo))

func isCompilerFatal*(conf: ConfigRef, report: Report): bool =
  ## Check if report stores fatal compilation error
  report.category == repInternal and
  report.internalReport.severity() == rsevFatal

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
  ## the
  optWarns in conf.options and note in conf.notes

func isEnabled*(conf: ConfigRef, report: ReportKind): bool =
  ## Check whether report kind is allowed to be generated by the compiler.
  ## Uses `options.hasHint`, `options.hasWarn` to check whether particular
  ## report is enabled, otherwise use query global/local options.


  # Reports related to experimental features and inconsistent CLI flags
  # (such as `--styleCheck` which controls both CLI flags and hints) are
  # checked for with higher priority
  case report:
    of repNilcheckKinds:
      result = strictNotNil in conf.features

    of rdbgVmExecTraceMinimal:
      result = conf.active.isVmTrace

    of rlexLinterReport, rsemLinterReport, :
      # Regular linter report is enabled if style check is either hint or
      # error, AND not `usages`
      result = 0 < len({optStyleHint, optStyleError} * conf.globalOptions) and
               optStyleUsages notin conf.globalOptions

    of rsemLinterReportUse:
      result = 0 < len({optStyleHint, optStyleError} * conf.globalOptions)

    else:
      # All other reports follow default hint category handing:
      case report:
        of repHintKinds:
          result = conf.hasHint(report)

        of repWarningKinds:
          result = conf.hasWarn(report)

        of repErrorKinds, repFatalKinds:
          result = true

        of repTraceKinds:
          result = true

        else:
          result = (report in conf.notes) and
          not ignoreMsgBecauseOfIdeTools(conf, report)

func isEnabled*(conf: ConfigRef, report: Report): bool =
  ## Macro expansion configuration is done via `--expandMacro=name`
  ## configuration, and requires full report information to check.
  if report.kind == rsemExpandMacro and
     conf.macrosToExpand.hasKey(report.semReport.sym.name.s):
    result = true

  else:
    result = conf.isEnabled(report.kind)

type
  ReportWritabilityKind* = enum
    writeEnabled
    writeDisabled
    writeForceEnabled

func writabilityKind*(conf: ConfigRef, r: Report): ReportWritabilityKind =
  const forceWrite = {
    rsemExpandArc # Not considered a hint for now
  }

  let tryhack = conf.m.errorOutputs == {}
  # REFACTOR this check is an absolute hack, `errorOutputs` need to be
  # removed. For more details see `lineinfos.MsgConfig.errorOutputs`
  # comment

  if (r.kind == rdbgVmCodeListing) and (
    (
     # If special expand target is not defined, debug all generated code
     ("expandVmListing" notin conf.symbols)
    ) or (
     # Otherwise check if listing target is not nil, and it's name is the
     # name we are targeting.
     (not r.debugReport.vmgenListing.sym.isNil()) and
     (
       r.debugReport.vmgenListing.sym.name.s ==
       conf.symbols["expandVmListing"]
  ))):
    return writeForceEnabled

  elif r.kind == rdbgVmCodeListing or (
    # Optionally Ignore context stacktrace
    r.kind == rdbgTraceLine and not conf.hack.semStack
  ):
    return writeDisabled

  elif (
     (conf.isEnabled(r) and r.category == repDebug and tryhack) or
     # Force write of the report messages using regular stdout if tryhack is
     # enabled
     r.kind in rintCliKinds
     # or if we are writing command-line help/usage information - it must
     # always be printed
  ):
    return writeForceEnabled

  elif (
    # Not explicitly enabled
    not conf.isEnabled(r) and
    # And not added for forced write
    r.kind notin forceWrite
  ) or (
    # Or we are in the special hack mode for `compiles()` processing
    tryhack
  ):

    # Return without writing
    return writeDisabled

  else:
    return writeEnabled

proc hcrOn*(conf: ConfigRef): bool =
  return optHotCodeReloading in conf.globalOptions

const
  oldExperimentalFeatures* = {
    implicitDeref, dotOperators, callOperator, parallel}

  ChecksOptions* = {optObjCheck, optFieldCheck, optRangeCheck,
    optOverflowCheck, optBoundsCheck, optAssert, optNaNCheck, optInfCheck,
    optStyleCheck}

  DefaultOptions* = {optObjCheck, optFieldCheck, optRangeCheck,
    optBoundsCheck, optOverflowCheck, optAssert, optWarns, optRefCheck,
    optHints, optStackTrace, optLineTrace, # consider adding `optStackTraceMsgs`
    optTrMacros, optStyleCheck, optCursorInference}
  DefaultGlobalOptions* = {optThreadAnalysis, optExcessiveStackTrace}

proc getSrcTimestamp(): DateTime =
  try:
    result = utc(fromUnix(parseInt(getEnv("SOURCE_DATE_EPOCH",
                                          "not a number"))))
  except ValueError:
    # Environment variable malformed.
    # https://reproducible-builds.org/specs/source-date-epoch/: "If the
    # value is malformed, the build process SHOULD exit with a non-zero
    # error code", which this doesn't do. This uses local time, because
    # that maintains compatibility with existing usage.
    result = utc getTime()

proc getDateStr*(): string =
  result = format(getSrcTimestamp(), "yyyy-MM-dd")

proc getClockStr*(): string =
  result = format(getSrcTimestamp(), "HH:mm:ss")

template newPackageCache*(): untyped =
  newStringTable(when FileSystemCaseSensitive:
                   modeCaseInsensitive
                 else:
                   modeCaseSensitive)

proc newProfileData(): ProfileData =
  ProfileData(data: newTable[TLineInfo, ProfileInfo]())

proc isDefined*(conf: ConfigRef; symbol: string): bool

const defaultHackController = HackController(
  semStack: on,
  reportInTrace: off,
  semTraceData: on
)

proc initConfigRefCommon(conf: ConfigRef) =
  conf.symbols = newStringTable(modeStyleInsensitive)
  conf.selectedGC = gcRefc
  conf.verbosity = 1
  conf.hintProcessingDots = true
  conf.options = DefaultOptions
  conf.globalOptions = DefaultGlobalOptions
  conf.filenameOption = foAbs
  conf.foreignPackageNotes = NotesVerbosity.foreign
  conf.notes = NotesVerbosity.main[1]
  conf.hack = defaultHackController
  conf.mainPackageNotes = NotesVerbosity.main[1]
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
    moduleOverrides: newStringTable(modeStyleInsensitive),
    cfileSpecificOptions: newStringTable(modeCaseSensitive),
    projectIsStdin: false, # whether we're compiling from stdin
    projectMainIdx: FileIndex(0'i32), # the canonical path id of the main module
    command: "", # the main command (e.g. cc, check, scan, etc)
    commandArgs: @[], # any arguments after the main command
    commandLine: "",
    keepComments: true, # whether the parser needs to keep comments
    docSeeSrcUrl: "",
    active: CurrentConf(
      backend:        backendInvalid,
      cppDefines:     initHashSet[string](),
      features:       {},
      legacyFeatures: {},
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
    vmProfileData: newProfileData(),
    spellSuggestMax: spellSuggestSecretSauce,
  )
  initConfigRefCommon(result)
  result.target = result.target.withIt do:
    setTargetFromSystem(it)

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
proc usesWriteBarrier*(conf: ConfigRef): bool {.inline.} = conf.selectedGC >= gcRefc

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

proc disableNimblePath*(conf: ConfigRef) =
  conf.incl optNoNimblePath
  conf.lazyPaths = @[]
  conf.nimblePaths = @[]

proc clearNimblePath*(conf: ConfigRef) =
  conf.lazyPaths = @[]
  conf.nimblePaths = @[]

include modules/packagehandling

proc getOsCacheDir(): string =
  when defined(posix):
    result = getEnv("XDG_CACHE_HOME", getHomeDir() / ".cache") / "nim"
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
  ## converts "/home/a/mymodule.nim", "rod" to "/home/a/nimcache/mymodule.rod"
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

proc patchModule(conf: ConfigRef, result: var AbsoluteFile) =
  ## If there is a known module override for a given
  ## `package/<name(result)>` replace result with new module override.
  if not result.isEmpty and conf.moduleOverrides.len > 0:
    let key = getPackageName(conf, result.string) & "_" & splitFile(result).name
    if conf.moduleOverrides.hasKey(key):
      let ov = conf.moduleOverrides[key]
      if ov.len > 0: result = AbsoluteFile(ov)

when (NimMajor, NimMinor) < (1, 1) or not declared(isRelativeTo):
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
  patchModule(conf, result)

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
  patchModule(conf, result)

proc findProjectNimFile*(conf: ConfigRef; pkg: string): string =
  ## Find configuration file for a current project
  const extensions = [".nims", ".cfg", ".nimcfg", ".nimble"]
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
  ## Shows the canonical module import, e.g.: system, std/tables,
  ## fusion/pointers, system/assertions, std/private/asciitables
  var ret = getRelativePathFromConfigPath(conf, file, isTitle = true)
  let dir = getNimbleFile(conf, $file).parentDir.AbsoluteDir
  if not dir.isEmpty:
    let relPath = relativeTo(file, dir)
    if not relPath.isEmpty and (ret.isEmpty or relPath.string.len < ret.string.len):
      ret = relPath
  if ret.isEmpty:
    ret = relativeTo(file, conf.projectPath)
  result = ret.string

proc canonicalImport*(conf: ConfigRef, file: AbsoluteFile): string =
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
