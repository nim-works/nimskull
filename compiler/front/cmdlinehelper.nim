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

from compiler/front/commands import CliEvent, logError, cliEvtErrCmdMissing

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

proc legacyReportProcNote(conf: ConfigRef, r: ProcessNoteResult, info: TLineInfo) =
  ## processes a hint/warn/error config switch, then bridges into legacy
  ## reports to keep the rest of the codebase isolated from them.
  let legacy = legacyReportBridge(r)
  if legacy.isSome:
    conf.localReport(info, legacy.unsafeGet())

proc legacyReportProcSwitch(conf: ConfigRef, r: ProcSwitchResult,
                             info: TLineInfo) =
  ## processes a config switch, then bridges into legacy reports to keep the
  ## rest of the codebase isolated from them.
  # TODO: before merge push out reports and move to CLI events/output
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
    if conf.hasHint(rextPath) and r.processedNimblePath.didProcess:
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
    of cekFlagError:
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

#[

`nimconf` is used by `cmdlinehelper` which in turn is used by each of `nim` and
`nimsuggest` modules/programs. To understand error handling and output needs,
I'll start by focusing on `nimconf`.

Highlevel Input/Outputs of `nimconf`
```
                                                    ┌─
                                                    │1. (hidden, ConfigRef mutation)
                                                    │
                                                    │2. write directive output
                                                    │
                                                    │3. trace directive output
                        ┌───────────────────┐       │
                        │ nimconf           │       │4. compiler dbg context read start / stop
                        │                   │       │
ConfigRef, *.cfg ──────►│                   ├──────►│5. config file progress ("hint")
                        │                   │       │
                        │  suppress:        │       │6. wrapped lexer fatal
                        │  -lexer hint/warn │       │
                        └───────────────────┘       │7. wrapped lexer error diag
                                                    │
                                                    │8. flag processing errors
                                                    │
                                                    │9. flag processing hint/warn
                                                    └─
```

Some observations about output:
- we can ignore 1 for this discussion
- 2 the user is directing us to output
- 3 is something hax added, but it's compiler dev debug output
- 4 is compiler dev debug output, but also a "bracketing" context
- 5 is progress indication, not really a "hint", more like "info"
- 6, 7, & 8 are things to output to the user, 6 is "less ignorable"
- 9 could get escalated, but honestly feels weird for config; otherwise need to output

Some observations about control flow:
- 1 to 5 have no implication on control flow
- 6 is fatal for both lexer/nimconf, but cmdlinehelper could recover/stop
- 7 and 7 are an error for nimconf, and it could recover/stop
- 9, if escalation is allowed, could be an error and have control flow implications
- finally, the question of whether to recover or not is dependent upon which app

Taking the apps into account:
- nimsuggest wants to recover as much as possible
- nim doesn't, even in if a `check`, work with an invalid config is dangerous
- nimsuggest might want to redirect this output to its log, sometimes stdout

Some concepts given the above:
  - event: just data produced by a module, might represent an error, but context could change that
  - diagnostics: errors/warnings/hints these are about some user input or its processing
  - severity: diagnositics can have severity bumped
  - generation/sending: error diagnostics must be generated (data/logic) and sent upstream, hint/warn can be stopped
  - suppression/write: diagnostics can be ignored (generated but never written)
  - fatal: these aren't diags, but another kind of event, signals inability to recover
  - error diags/fatal: must be generated
  - error recovery: origin can send/not send diag and then recover, or give up control
  - fatal recovery: origin refuses to keep control, can wrap/recover upstream
  - output format: can vary, even if 'CLI-only' colour, short/long/etc, stderr/out, logfile, etc
  - output destination: stderr/out, log file, blackhole, etc


big questions:
  - where is the most
  - where is the last responsible place to try and output something to the user, regardless of output destination/format?

event generated/seen for first time by nimconf
  any: (also see section questions below)
    - nimconf: add context/wrap in an object
    - cmdlinehelper: add context/wrap in an object

  lexer internal error:
    nim.nim:
      - nimconf: can't continue
      - cmdlinehelper: output error and finish
    nimsuggest.nim:
      - nimconf: can't continue
      - cmdlinehelper: output

  Questions:
    - should we add context? if so: by passing events 'up', or already available in a 'context stack'?

output:
  lexer internal error (for lexer and config fatal; cmdline recoverable):
    - ??: suppress fatal (verbosity) or write to receiver
    - receiver: figure out destination and format
    - receiver: write to destination in format

  lexer/flag error:
    - ??: suppress diag (verbosity) or write to receiver
    - receiver: figure out destination and format
    - receiver: write to destination

  trace:
    - <suppression isn't allowed because the user is debugging?>
    - ??: write to receiver
    - receiver: figure out destination and format
    - receiver: write to destination in format

  write:
    - ??: <supression disallowed, it's a user directive>
    - ??: write to receiver
    - receiver: figure out destination and format
    - receiver: ??destination based suppression?? ()
    - receiver: write to destination in format

  Questions:
    - allow further/any opportunities to intercept?
    - is the receiver the caller/parent or something else (e.g. msgs + ConfigRef)?
]#

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
  if conf.cmd == cmdNone and not self.suggestMode:
    # xxx: the suggestMode check is not required but reminds us that this code
    #      is misplaced.
    # we didn't set the result to false because this isn't config processing
    # and the caller should check error counts... this is more convoluted than
    # it needs to be, but incrementally refactoring.
    commands.logError(conf, CliEvent(kind: cliEvtErrCmdMissing,
                                    pass: passCmd2,
                                    srcCodeOrigin: instLoc()))

  graph.suggestMode = self.suggestMode
  return true