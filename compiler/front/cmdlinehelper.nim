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
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    nimconf,
    commands,
    msgs,
    options,
    condsyms,
    cli_reporter,
    sexp_reporter,
  ],
  compiler/utils/[
    pathutils,
    idioms
  ],
  compiler/backend/[
    extccomp
  ]

from std/strutils import endsWith, `%`

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_lexer import LexerReport
from compiler/ast/reports_parser import ParserReport
from compiler/ast/reports_internal import InternalReport
from compiler/ast/reports_external import ExternalReport
from compiler/ast/report_enums import ReportKind
from compiler/ast/reports import Report,
  ReportCategory,
  toReportLineInfo

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

proc handleConfigEvent(
    conf: ConfigRef,
    evt: ConfigFileEvent,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
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
    of cekInternalError:
      rintIce
    of cekLexerErrorDiag, cekLexerWarningDiag, cekLexerHintDiag:
      evt.lexerDiag.kind.lexDiagToLegacyReportKind
    of cekProgressConfStart:
      rextConf

  let rep =
    case evt.kind
    of cekInternalError, cekLexerErrorDiag, cekLexerWarningDiag,
        cekLexerHintDiag:
      evt.lexerDiag.lexerDiagToLegacyReport
    else:
      case kind
      of rlexExpectedToken:
        let msgTempl =
          case evt.kind
          of cekParseExpectedCloseX: "closing '$1'"
          of cekParseExpectedX:      "'$1'"
          else: unreachable()
        Report(
          category: repLexer,
          lexReport: LexerReport(
            location: std_options.some evt.location,
            reportInst: evt.instLoc.toReportLineInfo,
            msg: msgTempl % evt.msg,
            kind: kind))
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
      of rextConf:
        Report(
          category: repExternal,
          externalReport: ExternalReport(
            reportInst: evt.instLoc.toReportLineInfo,
            kind: kind,
            msg: evt.msg))
      else:
        unreachable("handleConfigEvent unexpected kind: " & $kind)
  
  handleReport(conf, rep, reportFrom, eh)

proc legacyReportsMsgFmtSetter(conf: ConfigRef, fmt: MsgFormatKind) =
  ## this actually sets the report hook, but the intention is formatter only,
  ## but the "reports" doesn't allow for that.
  case fmt
  of msgFormatText: conf.setReportHook cli_reporter.reportHook
  of msgFormatSexp:
    doAssert conf.cmd != cmdIdeTools, "don't screw up nimsuggest"
    conf.setReportHook sexp_reporter.reportHook

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
  loadConfigs(cfg, cache, conf, handleConfigEvent)

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

proc loadConfigsAndRunMainCommand*(
    self: NimProg, cache: IdentCache; conf: ConfigRef; graph: ModuleGraph): bool =

  ## Alias for loadConfigsAndProcessCmdLine, here for backwards compatibility
  loadConfigsAndProcessCmdLine(self, cache, conf, graph)
