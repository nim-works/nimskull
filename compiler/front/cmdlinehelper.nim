#
#
#           The Nim Compiler
#        (c) Copyright 2018 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Helpers for binaries that use compiler passes, e.g.: nim, nimsuggest, nimfix

import
  std/[
    os
  ],
  std/options as std_options,
  compiler/ast/[
    idents,
    lineinfos,
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    scriptconfig,
    nimconf,
    commands,
    msgs,
    options,
    optionprocessor,
    condsyms
  ],
  compiler/utils/[
    pathutils,
    idioms
  ],
  compiler/backend/[
    extccomp
  ]

from std/strutils import endsWith

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_lexer import LexerReport
from compiler/ast/reports_parser import ParserReport
from compiler/ast/reports_internal import InternalReport
from compiler/ast/reports_external import ExternalReport
from compiler/ast/reports_debug import DebugReport
from compiler/ast/report_enums import ReportKind
from compiler/ast/reports import Report,
  ReportCategory,
  toReportLineInfo
from compiler/front/cli_reporter import reportHook
from compiler/front/sexp_reporter import reportHook
from compiler/modules/nimblecmd import nimblePkgInvalid

proc prependCurDir*(f: AbsoluteFile): AbsoluteFile =
  when defined(unix):
    if os.isAbsolute(f.string): result = f
    else: result = AbsoluteFile("./" & f.string)
  else:
    result = f

type
  NimProg* = ref object
    suggestMode*: bool
    supportsStdinFile*: bool
    processCmdLine*: proc(pass: TCmdLinePass, cmd: string; config: ConfigRef)

proc legacyReportBridge(r: ProcessNoteResult): Option[ExternalReport] =
  case r.kind
  of procNoteInvalidOption:
    some ExternalReport(
      kind: rextCfgInvalidOption,
      cmdlineProvided: r.switch)
  of procNoteInvalidHint:
    some ExternalReport(
      kind: rextInvalidHint,
      cmdlineProvided: r.invalidHintOrWarning)
  of procNoteInvalidWarning:
    some ExternalReport(
      kind: rextInvalidWarning,
      cmdlineProvided: r.invalidHintOrWarning)
  of procNoteExpectedOnOrOff:
    some ExternalReport(kind: rextExpectedOnOrOff,
                    cmdlineSwitch: r.switch,
                    cmdlineProvided: r.argVal)
  of procNoteOnlyAllOffSupported:
    some ExternalReport(kind: rextOnlyAllOffSupported,
                    cmdlineSwitch: r.switch,
                    cmdlineProvided: r.argVal)
  of procNoteSuccess:
    none[ExternalReport]()

proc legacyReportProcNote*(conf: ConfigRef, r: ProcessNoteResult, info: TLineInfo) =
  ## processes a hint/warn/error config switch, then bridges into legacy
  ## reports to keep the rest of the codebase isolated from them.
  let legacy = legacyReportBridge(r)
  if legacy.isSome:
    conf.localReport(info, legacy.unsafeGet())

proc legacyReportProcSwitch*(conf: ConfigRef, r: ProcSwitchResult,
                             info: TLineInfo) =
  ## processes a config switch, then bridges into legacy reports to keep the
  ## rest of the codebase isolated from them.
  # TODO: before merge push out reports and move to CLI events/output
  if r.deprecatedNoopSwitchArg:
    conf.localReport(info):
      ExternalReport(kind: rextCfgArgDeprecatedNoop,
                     cmdlineSwitch: r.givenSwitch,
                     cmdlineProvided: r.givenArg)
  case r.kind
  of procSwitchSuccess: discard
  of procSwitchErrInvalid:
    conf.localReport(info):
      ExternalReport(kind: rextCfgInvalidOption,
                     cmdlineSwitch: r.givenSwitch)
  of procSwitchErrArgExpected:
    conf.localReport(info, ExternalReport(kind: rextCfgExpectedArgument,
                                          cmdlineSwitch: r.givenSwitch))
  of procSwitchErrArgForbidden:
    conf.localReport(info, ExternalReport(kind: rextCfgExpectedNoArgument,
                                          cmdlineSwitch: r.givenSwitch,
                                          cmdlineProvided: r.givenArg))
  of procSwitchErrArgMalformedKeyValPair:
    conf.localReport(info, ExternalReport(kind: rextCfgArgMalformedKeyValPair,
                                          cmdlineSwitch: r.givenSwitch,
                                          cmdlineProvided: r.givenArg))
  of procSwitchErrArgExpectedOnOrOff:
    conf.localReport(info, ExternalReport(kind: rextExpectedOnOrOff,
                                          cmdlineSwitch: r.givenSwitch,
                                          cmdlineProvided: r.givenArg))
  of procSwitchErrArgExpectedOnOffOrList:
    conf.localReport(info, ExternalReport(kind: rextCfgExpectedOnOffOrList,
                                          cmdlineSwitch: r.givenSwitch,
                                          cmdlineProvided: r.givenArg))
  of procSwitchErrArgExpectedAllOrOff:
    conf.localReport(info, ExternalReport(kind: rextOnlyAllOffSupported,
                                          cmdlineSwitch: r.givenSwitch,
                                          cmdlineProvided: r.givenArg))
  of procSwitchErrArgExpectedFromList:
    conf.localReport(info):
      ExternalReport(kind: rextCfgArgExpectedValueFromList,
                     cmdlineSwitch: r.givenSwitch,
                     cmdlineAllowed: allowedCompileOptionsArgs(r.switch))
  of procSwitchErrArgNotInValidList:
    conf.localReport(info):
      ExternalReport(kind: rextCfgArgExpectedValueFromList,
                     cmdlineSwitch: r.givenSwitch,
                     cmdlineProvided: r.givenArg,
                     cmdlineAllowed: allowedCompileOptionsArgs(r.switch))
  of procSwitchErrArgUnknownCCompiler:
    conf.localReport(info):
      ExternalReport(kind: rextUnknownCCompiler,
                     passedCompiler: r.givenArg,
                     knownCompilers: listCCnames())
  of procSwitchErrArgUnknownExperimentalFeature:
    conf.localReport(info):
      ExternalReport(kind: rextCfgArgUnknownExperimentalFeature,
                     cmdlineProvided: r.givenArg,
                     cmdlineAllowed: allowedCompileOptionsArgs(r.switch))
  of procSwitchErrArgPathInvalid:
    conf.localReport(info):
      ExternalReport(kind: rextInvalidPath,
                     cmdlineSwitch: r.givenSwitch,
                     cmdlineProvided: r.pathAttempted)
  of procSwitchErrArgNimblePath:
    for res in r.processedNimblePath.nimblePathResult.pkgs:
      case res.status
      of nimblePkgInvalid:    
        conf.localReport(info):
          ExternalReport(kind: rextInvalidPackageName, packageName: res.path)
      else:
        discard "ignore successes for now"
  of procSwitchErrArgInvalidHintOrWarning:
    let legacy = legacyReportBridge(r.processNoteResult)
    if legacy.isSome:
      conf.localReport(info, legacy.unsafeGet())
  case r.switch
  of cmdSwitchNimblepath:
    if r.processedNimblePath.didProcess:
      for np in r.processedNimblePath.nimblePathResult.addedPaths:
        conf.localReport(info):
          ExternalReport(kind: rextPath, packagePath: np.string)
  else:
    discard

proc handleConfigEvent(
    conf: ConfigRef,
    evt: ConfigFileEvent,
    reportFrom: InstantiationInfo
  ) =
  # REFACTOR: this is a temporary bridge into existing reporting

  let kind =
    case evt.kind
    of cekParseExpectedX, cekParseExpectedCloseX:
      # xxx: rlexExpectedToken is not a "lexer" error, but a misguided
      #      attempt at code reuse -- fix after reporting is untangled.
      rlexExpectedToken
    of cekParseExpectedIdent:
      rparIdentExpected
    of cekInvalidDirective:
      rlexCfgInvalidDirective
    of cekWriteConfig:
      rintNimconfWrite
    of cekDebugTrace:
      rdbgCfgTrace
    of cekInternalError:
      rintIce
    of cekLexerErrorDiag, cekLexerWarningDiag, cekLexerHintDiag:
      evt.lexerDiag.kind.lexDiagToLegacyReportKind
    of cekDebugReadStart:
      rdbgStartingConfRead
    of cekDebugReadStop:
      rdbgFinishedConfRead
    of cekProgressConfStart:
      rextConf
    of cekFlagAssignment:
      legacyReportProcSwitch(conf, evt.flagResult, evt.flagInfo)
      return

  let rep =
    case evt.kind
    of cekInternalError, cekLexerErrorDiag, cekLexerWarningDiag,
        cekLexerHintDiag:
      evt.lexerDiag.lexerDiagToLegacyReport
    else:
      case kind
      of rlexCfgInvalidDirective:
        Report(
          category: repLexer,
          lexReport: LexerReport(
            location: std_options.some evt.location,
            reportInst: evt.instLoc.toReportLineInfo,
            msg: evt.msg,
            kind: kind))
      of rparIdentExpected:
        Report(
          category: repParser,
          parserReport: ParserReport(
            location: std_options.some evt.location,
            reportInst: evt.instLoc.toReportLineInfo,
            msg: evt.msg,
            kind: kind))
      of rintNimconfWrite:
        Report(
          category: repInternal,
          internalReport: InternalReport(
            location: std_options.some evt.location,
            reportInst: evt.instLoc.toReportLineInfo,
            msg: evt.msg,
            kind: kind))
      of rdbgCfgTrace:
        Report(
          category: repDebug,
          debugReport: DebugReport(
            location: std_options.some evt.location,
            reportInst: evt.instLoc.toReportLineInfo,
            kind: kind,
            str: evt.msg))
      of rdbgStartingConfRead, rdbgFinishedConfRead:
        Report(
          category: repDebug,
          debugReport: DebugReport(
            reportInst: evt.instLoc.toReportLineInfo,
            kind: kind,
            filename: evt.msg))
      of rextConf:
        Report(
          category: repExternal,
          externalReport: ExternalReport(
            reportInst: evt.instLoc.toReportLineInfo,
            kind: kind,
            msg: evt.msg))
      else:
        unreachable("handleConfigEvent unexpected kind: " & $kind)

  let eh = case evt.kind
           of cekInternalError: doAbort
           else:                doNothing
  handleReport(conf, rep, reportFrom, eh)

proc legacyReportsMsgFmtSetter(conf: ConfigRef, fmt: MsgFormatKind) =
  ## this actually sets the report hook, but the intention is formatter only,
  ## but the "reports" doesn't allow for that.
  case fmt
  of msgFormatText: conf.setReportHook cli_reporter.reportHook
  of msgFormatSexp: conf.setReportHook sexp_reporter.reportHook

proc initDefinesProg*(self: NimProg, conf: ConfigRef, name: string) =
  condsyms.initDefines(conf.symbols)
  defineSymbol conf, name
  # "reports" strikes again, this bit of silliness is to stop reports from
  # infecting the `commands` module among others. Only really needed for CLI
  # parsing; don't need to care about the rest
  conf.setMsgFormat = legacyReportsMsgFmtSetter

proc processCmdLineAndProjectPath*(self: NimProg, conf: ConfigRef, cmd: string = "") =
  self.processCmdLine(passCmd1, cmd, conf)
  if conf.inputMode == pimFile and self.supportsStdinFile and conf.projectName == "-":
    conf.inputMode = pimStdin

  case conf.inputMode
  of pimCmd:
    handleCmdInput(conf)
  of pimStdin:
    handleStdinInput(conf)
  of pimFile:
    if conf.projectName != "":
      setFromProjectName(conf, conf.projectName)
    else:
      conf.projectPath = AbsoluteDir canonicalizePath(conf, AbsoluteFile getCurrentDir())

proc loadConfigs*(
  cfg: RelativeFile, cache: IdentCache,
  conf: ConfigRef) {.inline.} =
  ## wrapper around `nimconf.loadConfigs` to connect to legacy reporting
  proc handleScriptEvent(evt: CfgScriptEvt) =
    # REFACTOR: this is a temporary bridge into existing reporting
    let scriptEvt = evt.scriptEvt
    case scriptEvt.kind
    of scriptEvtDbgStart:
      conf.localReport DebugReport(
        kind: rdbgStartingConfRead,
        filename: evt.scriptPath.string)
    of scriptEvtDbgEnd:
      conf.localReport DebugReport(
        kind: rdbgFinishedConfRead,
        filename: evt.scriptPath.string)
    of scriptEvtRun:
      let runData = scriptEvt.scriptEvtRunData
      case runData.kind
      of scriptEvtRunProcessSwitch:
        conf.legacyReportProcSwitch(runData.switchResult, runData.info)
      of scriptEvtRunProcessSingleNoteWarn,
          scriptEvtRunProcessSingleNoteHint:
        conf.legacyReportProcNote(runData.noteResult, runData.info)

  loadConfigs(cfg, cache, conf, handleConfigEvent, handleScriptEvent)

proc loadConfigsAndProcessCmdLine*(self: NimProg, cache: IdentCache; conf: ConfigRef;
                                   graph: ModuleGraph): bool =
  ## Load all the necessary configuration files and command-line options.
  ## Main entry point for configuration processing.
  if self.suggestMode:
    conf.setCmd cmdIdeTools
  if conf.cmd == cmdNimscript:
    incl(conf, optWasNimscript)

  # load all config files
  loadConfigs(DefaultConfig, cache, conf)

  # `nim foo.nims` means execute the nimscript
  if not self.suggestMode and conf.cmd == cmdNone and
      conf.projectFull.string.endsWith ".nims":
    conf.setCmd cmdNimscript

  # now process command line arguments again, because some options in the
  # command line can overwrite the config file's settings
  extccomp.initVars(conf)
  self.processCmdLine(passCmd2, "", conf)
  if conf.cmd == cmdNone:
    localReport(conf, ExternalReport(kind: rextCommandMissing))

  graph.suggestMode = self.suggestMode
  return true