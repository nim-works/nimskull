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
    lexer,
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    nimconf,
    commands,
    msgs,
    options,
    optionsprocessor,
    condsyms,
    cli_reporter,
    sexp_reporter,
  ],
  compiler/utils/[
    pathutils,
  ],
  compiler/backend/[
    extccomp
  ]

from experimental/colortext import ForegroundColor, toString

from std/strutils import endsWith, `%`

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/report_enums import ReportKind

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

type
  ConfDiagSeverity = enum
    configDiagSevFatal = "Fatal:"
    configDiagSevError = "Error:"
    configDiagSevWarn  = "Warning:"
    configDiagSevInfo  = "Hint:"

proc writeConfigEvent(conf: ConfigRef,
                       evt: ConfigFileEvent,
                       writeFrom: InstantiationInfo) =
  case evt.kind
  of cekProgressConfStart:
    if not conf.isEnabled(rextConf): return
  of cekProgressPathAdded:
    if not conf.isEnabled(rextPath): return
  else:
    discard "continue processing"

  let
    showKindSuffix = conf.hasHint(rintErrKind)
    pathInfo =
      case evt.kind
      of cekParseExpectedX, cekParseExpectedCloseX, cekParseExpectedIdent,
           cekInvalidDirective, cekWriteConfig, cekProgressPathAdded:
        evt.location
      of cekInternalError, cekLexerErrorDiag, cekLexerWarningDiag:
        evt.lexerDiag.location
      of cekFlagError:
        evt.flagInfo
      of cekProgressConfStart:
        unknownLineInfo
    useColor = if conf.cmd == cmdIdeTools: false else: conf.useColor()
    msgKindTxt =
      case evt.kind
      of cekParseExpectedX, cekParseExpectedCloseX, cekParseExpectedIdent,
          cekInvalidDirective, cekWriteConfig, cekInternalError,
          cekLexerErrorDiag, cekFlagError:
        ""
      of cekLexerWarningDiag:
        case evt.lexerDiag.kind
        of lexDiagDeprecatedOctalPrefix: "[$1]" % $lexDiagDeprecatedOctalPrefix
        else:                            ""
      of cekProgressPathAdded:           "[$1]" % $cekProgressPathAdded
      of cekProgressConfStart:           "[$1]" % $cekProgressConfStart
    msgKindSuffix =
      if msgKindTxt == "": ""
      else:
        if useColor: stylize(msgKindTxt, fgCyan)
        else:        msgKindTxt
    msgOrigin =
      if conf.hasHint(rintMsgOrigin):
        cliFmtMsgOrigin(evt.instLoc, showKindSuffix, useColor)
      else:
        ""
    path =
      case evt.kind
      of cekParseExpectedX, cekParseExpectedCloseX, cekParseExpectedIdent,
           cekInvalidDirective, cekWriteConfig, cekProgressPathAdded:
        conf.cliFmt(evt.location, useColor)
      of cekInternalError, cekLexerErrorDiag, cekLexerWarningDiag:
        conf.cliFmt(evt.lexerDiag.location, useColor)
      of cekFlagError:
        conf.cliFmt(evt.flagInfo, useColor)
      of cekProgressConfStart:
        ""
  const severityColors: array[ConfDiagSeverity, ForegroundColor] = [
    fgRed, fgRed, fgYellow, fgGreen
  ]

  template styleSeverity(sev: ConfDiagSeverity): string =
    if useColor:
      stylize($sev, severityColors[sev])
    else:
      $sev

  let
    severity =
      case evt.kind
      of cekInternalError:
        styleSeverity configDiagSevFatal
      of cekLexerErrorDiag..cekFlagError:
        styleSeverity configDiagSevError
      of cekLexerWarningDiag:
        styleSeverity configDiagSevWarn
      of cekProgressConfStart, cekProgressPathAdded:
        styleSeverity configDiagSevInfo # xxx: not really a hint
      of cekWriteConfig:
        "" # not a log/diagnostic like the rest; has none

  conf.writeln:
    case evt.kind
    of cekParseExpectedX:
      # path severity msg [kindsuffix] [msgOrigin]
      let msg = "expected '$1'" % evt.msg
      "$# $# $#$#" % [path, severity, msg, msgOrigin]
    of cekParseExpectedCloseX:
      let msg = "expected closing '$1'" % evt.msg
      "$# $# $#$#" % [path, severity, msg, msgOrigin]
    of cekParseExpectedIdent:
      let msg = "identifier expected, but found '$1'" % evt.msg
      "$# $# $#$#" % [path, severity, msg, msgOrigin]
    of cekInvalidDirective:
      let msg = "invalid directive: '$1'" % evt.msg
      "$# $# $#$#" % [path, severity, msg, msgOrigin]
    of cekInternalError, cekLexerErrorDiag:
      let msg = diagToHumanStr(evt.lexerDiag)
      "$# $# $#$#" % [path, severity, msg, msgOrigin]
    of cekLexerWarningDiag:
      let msg = diagToHumanStr(evt.lexerDiag)
      "$# $# $#$#$#$#" % [path, severity, msg,
                          if msgKindSuffix == "": "" else: " ", # spacing
                          msgKindSuffix,
                          msgOrigin]
    of cekFlagError:
      let msg = procResultToHumanStr(evt.flagResult)
      "$# $# $#$#" % [path, severity, msg, msgOrigin]
    of cekProgressPathAdded:
      let msg = "added path: '$1'" % evt.msg
      "$# $# $#$#$#$#" % [path, severity, msg,
                          if msgKindSuffix == "": "" else: " ", # spacing
                          msgKindSuffix,
                          msgOrigin]
    of cekProgressConfStart:
      let msg = "used config file '$1'" % evt.msg
      "$# $#$#$#$#" % [severity, msg,
                       if msgKindSuffix == "": "" else: " ", # spacing
                       msgKindSuffix,
                       msgOrigin]
    of cekWriteConfig:
      evt.msg # TODO: use the lineinfo as msgorigin

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
  conf: ConfigRef, stopOnError = true): bool {.inline.} =
  ## wrapper around `nimconf.loadConfigs` to connect to legacy reporting
  loadConfigs(cfg, cache, conf, writeConfigEvent, stopOnError)

proc loadConfigsAndProcessCmdLine*(self: NimProg, cache: IdentCache; conf: ConfigRef;
                                   graph: ModuleGraph): bool =
  ## Load all the necessary configuration files and command-line options.
  ## Main entry point for configuration processing.
  if self.suggestMode:
    conf.setCmd cmdIdeTools
  if conf.cmd == cmdNimscript:
    incl(conf, optWasNimscript)

  # load all config files
  if loadConfigs(DefaultConfig, cache, conf, stopOnError = not self.suggestMode):
  # `nim foo.nims` means execute the nimscript
    if not self.suggestMode and conf.cmd == cmdNone and
        conf.projectFull.string.endsWith ".nims":
      conf.setCmd cmdNimscript

    # now process command line arguments again, because some options in the
    # command line can overwrite the config file's settings
    extccomp.initVars(conf)
    self.processCmdLine(passCmd2, "", conf)
    graph.suggestMode = self.suggestMode
    if conf.cmd == cmdNone:
      conf.logError(CliEvent(kind: cliEvtErrCmdMissing,
                              srcCodeOrigin: instLoc(),
                              pass: passCmd2))
      result = false
    else:
      result = true
  else:
    result = false

proc loadConfigsAndRunMainCommand*(
    self: NimProg, cache: IdentCache; conf: ConfigRef; graph: ModuleGraph): bool =

  ## Alias for loadConfigsAndProcessCmdLine, here for backwards compatibility
  loadConfigsAndProcessCmdLine(self, cache, conf, graph)
