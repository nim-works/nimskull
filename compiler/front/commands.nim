#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module handles the parsing of command line arguments.

# We do this here before the 'import' statement so 'defined' does not get
# confused with 'TGCMode.gcMarkAndSweep' etc.
template bootSwitch(name, expr, userString) =
  # Helper to build boot constants, for debugging you can 'echo' the else part.
  const name = if expr: " " & userString else: ""

bootSwitch(usedRelease, defined(release), "-d:release")
bootSwitch(usedDanger, defined(danger), "-d:danger")
# `useLinenoise` deprecated in favor of `nimUseLinenoise`, kept for backward compatibility
bootSwitch(useLinenoise, defined(nimUseLinenoise) or defined(useLinenoise), "-d:nimUseLinenoise")
bootSwitch(usedBoehm, defined(boehmgc), "--gc:boehm")
bootSwitch(usedMarkAndSweep, defined(gcmarkandsweep), "--gc:markAndSweep")
bootSwitch(usedGoGC, defined(gogc), "--gc:go")
bootSwitch(usedNoGC, defined(nogc), "--gc:none")

import
  std/[
    os,
    strutils,
    parseopt,
    sequtils,
    strtabs,
  ],
  compiler/modules/[
    nimblecmd,
  ],
  compiler/ast/[
    lineinfos,
    wordrecg,
  ],
  compiler/front/[
    options,
    optionprocessor,
    msgs,
  ],
  compiler/backend/[
    extccomp
  ],
  experimental/[
    colortext,    # required for pretty output; TODO: factor out
  ],
  compiler/utils/[
    nversion,
    pathutils,
    platform,
    idioms
  ]

# TODO: remove remaining legacy reports stuff.
from compiler/ast/report_enums import ReportKind,
  ReportKinds,
  repHintKinds,
  repHintGroups,
  repWarningKinds,
  repWarningGroups

bootSwitch(usedTinyC, hasTinyCBackend, "-d:tinyc")

const
  pathFmtStr = "$#($#, $#)" ## filename(line, column)

func stylize(
    conf: ConfigRef,
    str: string,
    color: ForegroundColor,
    styles: set[Style] = {}
  ): string =
  # TODO: create a CLI output module -- not `cli_reporter`
  if str.len == 0 or not conf.useColor:
    result = str
  else:
    result = "\e[$#m" % $color.int
    for s in styles:
      result.addf "\e[$#m", s.int
    result.add str
    result.add "\e[0m"

func stylize(
    conf: ConfigRef,
    str: string,
    color: ForegroundColor,
    style: Style
  ): string {.inline.} =
  conf.stylize(str, color, {style})

# TODO: temporary, move into `msgs` or `commands`
type
  CmdOutputKind* = enum
    cmdOutUser        ## a command's primary output, e.g. dump's data dump
    cmdOutStatus      ## command's status, e.g. build success message
    cmdOutUserProf    ## user requested profiling output
    cmdOutInternalDbg ## explicitly secondary output for compiler tracing

proc write*(conf: ConfigRef, dest: static[CmdOutputKind], msg: string) =
  let flags =
    case dest
    of cmdOutUser:
      {msgNoUnitSep, msgStdout}
    of cmdOutInternalDbg:
      {msgNoUnitSep, msgStdout}
    of cmdOutUserProf, cmdOutStatus:
      {msgNoUnitSep} # xxx: force stderr?
  conf.msgWrite(msg, flags)

proc writeln*(conf: ConfigRef, dest: static[CmdOutputKind], msg: string) =
  write(conf, dest, msg & "\n")

proc cmdFail*(conf: ConfigRef, msg: string) =
  inc conf.errorCounter
  writeln(conf, cmdOutStatus, msg)

proc writeLog(conf: ConfigRef, msg: string, srcLoc: InstantiationInfo) =
  var result = msg
  if conf.hasHint(rintMsgOrigin):
    let path = pathFmtStr % [srcLoc.filename, 
                             $srcLoc.line,
                             $srcLoc.column]
    result.addf "\n$# msg instantiated here $#",
                [conf.stylize(path, fgDefault, styleBright),
                 conf.stylize("[MsgOrigin]", fgCyan)]
  conf.msgWrite(result & "\n")

proc logGcStats*(conf: ConfigRef, stats: string, srcLoc = instLoc()) =
  ## log a 'debug' level message with the GC `stats`
  # TODO: document log levels, eventual introduction of `channels`,
  #       suppression, formatting, etc
  if optCmdExitGcStats in conf.globalOptions:
    conf.writeLog(stats, srcLoc)

proc logExecStart*(conf: ConfigRef, cmd: string, srcLoc = instLoc()) =
  ## use when a command invocation begins a shell exec as part of its
  ## operations; not currently meant for shell execs initiated by input source
  ## code or scripts.
  # xxx: maybe allow configurable command action logging
  if conf.verbosity > compVerbosityDefault:
    conf.writeLog(cmd, srcLoc)

proc processArgument(pass: TCmdLinePass; p: OptParser;
                      argsCount: var int; config: ConfigRef): bool =
  if argsCount == 0:
    # nim filename.nims  is the same as "nim e filename.nims":
    if p.key.endsWith(".nims"):
      config.setCmd cmdNimscript
      incl(config, optWasNimscript)
      config.projectName = unixToNativePath(p.key)
      config.arguments = cmdLineRest(p)
      result = true
    elif pass != passCmd2: setCommandEarly(config, p.key)
  else:
    if pass == passCmd1: config.commandArgs.add p.key
    if argsCount == 1:
      # support UNIX style filenames everywhere for portable build scripts:
      if config.projectName.len == 0 and config.inputMode == pimFile:
        config.projectName = unixToNativePath(p.key)
      config.arguments = cmdLineRest(p)
      result = true
  inc argsCount

type
  CliEventKind* = enum
    # errors - cli command
    cliEvtErrInvalidCommand # main.nim
    cliEvtErrCmdExpectedNoAdditionalArgs # nim.nim
      ## command disallows additional args
    cliEvtErrRunCmdFailed  # commands.nim and nim.nim
    cliEvtErrGenDependFailed # main.nim
    # errors - general flag/option/switches (TODO: standardize on "flag")
    cliEvtErrUnexpectedRunOpt # commands.nim and nim.nim
    cliEvtErrFlagArgForbidden # help/version/fullhelp/etc disallow args
    cliEvtErrFlagProcessing # commands.nim
    # errors - specific flag/option/switch
    cliEvtErrNoCliParamsProvided # commands.nim and nim.nim
    # warnings - general flags/options/switches
    cliEvtWarnSwitchValDeprecatedNoop
    # hints - general flag/options/switches and/or processing
    cliEvtHintPathAdded
      ## currently only triggered if nimble adds a path
      # xxx: this doesn't feel like a hint, more like "info" or "trace"

  CliEvent* = object
    # xxx: should these be 'flat' log events? put another way is the 
    #      `procResult` field a design smell?
    srcCodeOrigin*: InstantiationInfo
    pass*: TCmdLinePass
    case kind*: CliEventKind
      of cliEvtErrInvalidCommand,
          cliEvtErrCmdExpectedNoAdditionalArgs,
          cliEvtErrUnexpectedRunOpt:
        cmd*: string
        unexpectedArgs*: string
      of cliEvtErrFlagArgForbidden:
        givenFlg*: string
        givenArg*: string
      of cliEvtErrRunCmdFailed,
          cliEvtErrGenDependFailed:
        shellCmd*: string
        exitCode*: int
      of cliEvtErrFlagProcessing,
          cliEvtWarnSwitchValDeprecatedNoop:
        origParseOptKey*, origParseOptVal*: string
        procResult*: ProcSwitchResult
      of cliEvtHintPathAdded:
        pathAdded*: string
      of cliEvtErrNoCliParamsProvided:
        discard

const
  cliLogAllKinds = {low(CliEventKind) .. high(CliEventKind)}
  cliEvtErrors   = {cliEvtErrInvalidCommand .. cliEvtErrNoCliParamsProvided}
  cliEvtWarnings = {cliEvtWarnSwitchValDeprecatedNoop}
  cliEvtHints    = {cliEvtHintPathAdded}

static:
  const unaccountedForEvtKinds =
    cliLogAllKinds - cliEvtErrors - cliEvtWarnings - cliEvtHints
  doAssert unaccountedForEvtKinds == {}, "Uncategorized event kinds: " &
                                            $unaccountedForEvtKinds

proc procSwitchResultToEvents*(conf: ConfigRef, pass: TCmdLinePass,
                               origParseOptKey, origParseOptVal: string,
                               r: ProcSwitchResult): seq[CliEvent] =
  # Note: the order in which this generates events is the order in which
  #       they're output, rearrange code carefully.
  case r.kind
  of procSwitchSuccess: discard
  else:
    result.add:
      CliEvent(kind: cliEvtErrFlagProcessing,
                pass: pass,
                origParseOptKey: origParseOptKey,
                origParseOptVal: origParseOptVal,
                procResult: r,
                srcCodeOrigin: instLoc())
  case r.switch
  of cmdSwitchNimblepath:
    if conf.hasHint(rextPath) and r.processedNimblePath.didProcess:
      for res in r.processedNimblePath.nimblePathResult.addedPaths:
        result.add:
          CliEvent(kind: cliEvtHintPathAdded, pathAdded: res.string)
  else:
    discard

proc writeLog(conf: ConfigRef, msg: string, evt: CliEvent) {.inline.} =
  conf.writeLog(msg, evt.srcCodeOrigin)

proc logError*(conf: ConfigRef, evt: CliEvent) =
  # TODO: consolidate log event rendering, between this and "reports", but with
  #       less of the reports baggage
  let msg =
    case evt.kind
    of cliEvtErrInvalidCommand:
      "Invalid command - $1" % evt.cmd
    of cliEvtErrUnexpectedRunOpt:
      "'$1' cannot handle --run" % evt.cmd
    of cliEvtErrCmdExpectedNoAdditionalArgs:
      "$1 command does not support additional arguments: '$2'" %
        [evt.cmd, evt.unexpectedArgs]
    of cliEvtErrRunCmdFailed,
        cliEvtErrGenDependFailed: # make a better message for gen depend
      "execution of an external program '$1' failed with exit code '$2'" %
        [evt.shellCmd, $evt.exitCode]
    of cliEvtErrNoCliParamsProvided:
      "no command-line parameters provided"
    of cliEvtErrFlagArgForbidden:
      "$1 expects no arguments, but '$2' found" %
        [evt.givenFlg, evt.givenArg]
    of cliEvtErrFlagProcessing:
      let procResult = evt.procResult
      case procResult.kind
      of procSwitchSuccess: unreachable()
      of procSwitchErrInvalid:
        "Invalid command line option - " & procResult.givenArg
      of procSwitchErrArgExpected:
        "argument for command line option expected: '$1'" %
          procResult.givenSwitch
      of procSwitchErrArgForbidden:
        "$1 expects no arguments, but '$2' found" %
          [procResult.givenSwitch, procResult.givenArg]
      of procSwitchErrArgMalformedKeyValPair:
        "option '$#' has malformed `key:value` argument: '$#" %
          [procResult.givenSwitch, procResult.givenArg]
      of procSwitchErrArgExpectedOnOrOff:
        "'on' or 'off' expected for $1, but '$2' found" %
          [procResult.givenSwitch, procResult.givenArg]
      of procSwitchErrArgExpectedOnOffOrList:
        "'on', 'off', or 'list' expected for $1, but '$2' found" %
          [procResult.givenSwitch, procResult.givenArg]
      of procSwitchErrArgExpectedAllOrOff:
        "only 'all:off' is supported for $1, found $2" %
          [procResult.givenSwitch, procResult.givenArg]
      of procSwitchErrArgExpectedFromList:
        "expected value for switch '$1'. Expected one of $2, but got nothing" %
          [procResult.givenSwitch,
           allowedCompileOptionsArgs(procResult.switch).join(", ")]
      of procSwitchErrArgNotInValidList:
        "Unexpected value for switch '$1'. Expected one of $2, but got '$3'" %
          [procResult.givenSwitch,
           allowedCompileOptionsArgs(procResult.switch).join(", "),
           procResult.givenArg]
      of procSwitchErrArgUnknownCCompiler:
        "unknown C compiler: '$1'. Available options are: $2" %
          [procResult.givenArg, listCCnames().join(", ")]
      of procSwitchErrArgUnknownExperimentalFeature:
        "unknown experiemental feature: '$1'. Available options are: $2" %
          [procResult.givenArg,
           allowedCompileOptionsArgs(procResult.switch).join(", ")]
      of procSwitchErrArgNimblePath:
        let
          nimbleResult = procResult.processedNimblePath
          msgPrefix = "in nimblepath ('$#') invalid package " %
                                nimbleResult.nimblePathAttempted.string
          invalidPaths = nimbleResult.nimblePathResult.pkgs
                            .filterIt(it.status == nimblePkgInvalid)
                            .mapIt(it.path)
        case invalidPaths.len
        of 0: unreachable("compiler bug")
        of 1: msgPrefix & "name: '$#'" % invalidPaths[0]
        else: (msgPrefix & "names:" & repeat("\n  '$#'", invalidPaths.len)) %
                invalidPaths
      of procSwitchErrArgPathInvalid:
        "invalid path (option '$#'): $#" %
          [procResult.givenSwitch, procResult.pathAttempted]
      of procSwitchErrArgInvalidHintOrWarning:
        let processNoteResult = procResult.processNoteResult
        # TODO: improve these messages so they're more hint/warning specific,
        #       we have more information available than we're using. eg: it's
        #       not an invalid option, but error/warning/hint/etc switch
        let temp =
          case processNoteResult.kind
          of procNoteSuccess: unreachable()
          of procNoteInvalidOption:
            "Invalid command line option - " & processNoteResult.switch
          of procNoteInvalidHint:
            "Invalid hint - " & processNoteResult.invalidHintOrWarning
          of procNoteInvalidWarning:
            "Invalid warning - " & processNoteResult.invalidHintOrWarning
          of procNoteExpectedOnOrOff:
            "'on' or 'off' expected for $1, but '$2' found" %
              [processNoteResult.switch, processNoteResult.argVal]
          of procNoteOnlyAllOffSupported:
            "only 'all:off' is supported for $1, found $2" %
              [processNoteResult.switch, processNoteResult.argVal]
        temp
    of cliEvtWarnings, cliEvtHints:
      unreachable($evt.kind)
  inc conf.errorCounter
  conf.writeLog(msg, evt)

proc logWarn(conf: ConfigRef, evt: CliEvent) =
  # TODO: see items under `logError`
  let msg =
    case evt.kind
    of cliEvtWarnSwitchValDeprecatedNoop:
      "'$#' is deprecated for flag '$#', now a noop" %
        [evt.procResult.givenArg, evt.procResult.givenSwitch]
    of cliEvtErrors, cliEvtHints:
      unreachable($evt.kind)

  inc conf.warnCounter
  conf.writeLog(msg, evt)

proc logHint(conf: ConfigRef, evt: CliEvent) =
  # TODO: see items under `logError`
  let msg =
    case evt.kind
    of cliEvtHintPathAdded:
      "added path: '$1'" % evt.pathAdded
    of cliEvtErrors, cliEvtWarnings:
      unreachable($evt.kind)

  inc conf.hintCounter
  if conf.verbosity > compVerbosityDefault:
    conf.writeLog(msg, evt)

proc cliEventLogger*(conf: ConfigRef, evt: CliEvent) =
  ## a basic event logger that will write to standard err/out as apporpriate
  ## and follow `conf` settings.
  case evt.kind
  of cliEvtErrors:   conf.logError(evt)
  of cliEvtWarnings: conf.logWarn(evt)
  of cliEvtHints:    conf.logHint(evt)

type
  CliData = object
    ## Information used to construct messages for CLI reports - `--help`,
    ## `--fullhelp`
    version*: string ## Language version
    sourceHash*: string ## Compiler source code git hash
    sourceDate*: string ## Compiler source code date
    boot*: seq[string] ## nim compiler boot flags
    cpu*: TSystemCPU ## Target CPU
    os*: TSystemOS ## Target OS

const
  sourceHash {.strdefine.} = "" # defined by koch
  sourceDate {.strdefine.} = "" # defined by koch
  cliData = CliData(version: VersionAsString,
                    sourceHash: sourceHash,
                    sourceDate: sourceDate,
                    boot: @[usedRelease,
                            usedDanger,
                            usedTinyC,
                            useLinenoise,
                            usedBoehm,
                            usedMarkAndSweep,
                            usedGoGC,
                            usedNoGC],
                    os: nameToOS(system.hostOS),
                    cpu: nameToCPU(system.hostCPU))
  HelpMessage = "Nimskull Compiler Version $1 [$2: $3]\n"
  CommitMessage = "Source hash: $1\n" &
                  "Source date: $2\n"
  Usage = slurp"../doc/basicopt.txt".replace(" //", "   ")
  AdvancedUsage = slurp"../doc/advopt.txt".replace(" //", "   ") %
    typeof(Feature).toSeq.mapIt($it).join("|") # '|' separated features

proc showMsg*(conf: ConfigRef, msg: string) =
  ## show a message to the user, meant for informational/status cirucmstances.
  ## Depending upon settings the message might not necessarily be output.
  ## 
  ## For command output, eg: `dump`'s conditionals and search paths use a
  ## different routine (not implemented at time or writing).
  # TODO: implement procs for actual command output
  conf.msgWrite(msg, {msgNoUnitSep})

func cliMsgLede(data: CliData): string {.inline.} =
  HelpMessage % [
    VersionAsString,
    platform.OS[data.os].name,
    CPU[data.cpu].name
  ]

func helpOnErrorMsg*(conf: ConfigRef): string =
  cliMsgLede(cliData) & Usage

proc writeHelp(conf: ConfigRef) =
  conf.showMsg helpOnErrorMsg(conf)
  msgQuit(0)

proc writeAdvancedUsage(conf: ConfigRef) =
  conf.showMsg:
    cliMsgLede(cliData) & AdvancedUsage
  msgQuit(0)

proc writeFullhelp(conf: ConfigRef) =
  conf.showMsg:
    cliMsgLede(cliData) & Usage & AdvancedUsage
  msgQuit(0)

proc writeVersionInfo(conf: ConfigRef) =
  let
    commitMsg =
      if sourceHash != "":
        "\n" & CommitMessage % [sourceHash, sourceDate]
      else:
        ""
  conf.showMsg:
    cliMsgLede(cliData) &
    commitMsg &
    "\nactive boot switches: " & cliData.boot.join(" ")
  msgQuit(0)

proc processCmdLine*(pass: TCmdLinePass, cmd: string, config: ConfigRef) =
  ## Process input command-line parameters into `config` settings. Input is
  ## a joined list of command-line arguments with multiple options and/or
  ## configurations.
  var
    p = parseopt.initOptParser(cmd) # xxx: `cmd` is always empty, this relies
                                    #      on `parseOpt` using `os` to get the
                                    #      cli params
    argsCount = 0

  let startingErrCount = config.errorCounter

  config.commandLine.setLen 0
    # bugfix: otherwise, config.commandLine ends up duplicated

  template expectNoArg(flg, arg: string) =
    if arg != "":
      config.cliEventLogger:
        CliEvent(kind: cliEvtErrFlagArgForbidden,
                givenFlg: flg,
                givenArg: arg,
                pass: passCmd1,
                srcCodeOrigin: instLoc())

  while true:
    parseopt.next(p)
    case p.kind
    of cmdEnd: break
    of cmdLongOption, cmdShortOption:
      config.commandLine.add " "

      # add '-' prefix based on long/short option
      config.commandLine.add:
        case p.kind
        of cmdLongOption: "--"
        of cmdShortOption: "-"
        of cmdArgument, cmdEnd: "" # technically unreachable
      config.commandLine.add p.key.quoteShell # quoteShell to be future proof

      if p.val.len > 0:
        config.commandLine.add ':'
        config.commandLine.add p.val.quoteShell

      # this only happens in passCmd1 as each of these triggers a quit
      case p.key.normalize
      of "version", "v":
        # only kept because of user expectations
        expectNoArg(p.key, p.val)
        writeVersionInfo(config)
      of "help", "h":
        # only kept because of user expectations
        expectNoArg(p.key, p.val)
        writeHelp(config)
      of "advanced":
        # deprecate/make it a switch for the help sub-command
        expectNoArg(p.key, p.val)
        writeAdvancedUsage(config)
      of "fullhelp":
        # deprecate/make it a switch for the help sub-command
        expectNoArg(p.key, p.val)
        writeFullhelp(config)
      else:
        discard "continue processing as below"

      if p.key == "": # `-` was passed to indicate main project is stdin
        p.key = "-"
        if processArgument(pass, p, argsCount, config):
          break
      else:
        # Main part of the configuration processing -
        # `commands.processSwitch` processes input switches a second time
        # and puts them in necessary configuration fields.
        let
          res = processSwitch(pass, p, config)
          evts = procSwitchResultToEvents(config, pass, p.key, p.val, res)
        for e in evts.items:
          config.cliEventLogger(e)
    of cmdArgument:
      config.commandLine.add " "
      config.commandLine.add p.key.quoteShell
      if processArgument(pass, p, argsCount, config):
        break
    if config.errorCounter > startingErrCount:
      break

  if pass == passCmd2:
    if {optRun, optWasNimscript} * config.globalOptions == {} and
        config.arguments.len > 0 and config.cmd notin {
          cmdTcc, cmdNimscript, cmdCrun}:
      config.cliEventLogger:
        CliEvent(kind: cliEvtErrUnexpectedRunOpt,
                  cmd: config.command,
                  pass: pass,
                  srcCodeOrigin: instLoc())


when false:
  # started on creating a flag/switch description

  const
    # TODO: rework to contain full description of the switch with an object,
    #       whether it's an on/off; no arg, has on/off/list; off/list, path,
    #       global opt/config, has valid range/what is it, etc
    switchTxtToCmd* = [ 
      cmdSwitchFromcmd            : {fullSwitchTxtFromcmd},
      cmdSwitchPath               : {fullSwitchTxtPath, smolSwitchTxtPath},
      cmdSwitchNimblepath         : {fullSwitchTxtNimblepath},
      cmdSwitchNonimblepath       : {fullSwitchTxtNonimblepath},
      cmdSwitchClearnimblepath    : {fullSwitchTxtClearnimblepath},
      cmdSwitchExcludepath        : {fullSwitchTxtExcludepath},
      cmdSwitchNimcache           : {fullSwitchTxtNimcache},
      cmdSwitchOut                : {fullSwitchTxtOut, smolSwitchTxtOut},
      cmdSwitchOutdir             : {fullSwitchTxtOutdir},
      cmdSwitchDepfile            : {fullSwitchTxtDepfile},
      cmdSwitchUsenimcache        : {fullSwitchTxtUsenimcache},
      cmdSwitchDocseesrcurl       : {fullSwitchTxtDocseesrcurl},
      cmdSwitchDocroot            : {fullSwitchTxtDocroot},
      cmdSwitchBackend            : {fullSwitchTxtBackend, smolSwitchTxtBackend},
      cmdSwitchDoccmd             : {fullSwitchTxtDoccmd},
      cmdSwitchDefine             : {fullSwitchTxtDefine, smolSwitchTxtDefine},
      cmdSwitchUndef              : {fullSwitchTxtUndef, smolSwitchTxtUndef},
      cmdSwitchCompile            : {fullSwitchTxtCompile},
      cmdSwitchLink               : {fullSwitchTxtLink},
      cmdSwitchDebuginfo          : {fullSwitchTxtDebuginfo},
      cmdSwitchEmbedsrc           : {fullSwitchTxtEmbedsrc},
      cmdSwitchCompileonly        : {fullSwitchTxtCompileonly, smolSwitchTxtCompileonly},
      cmdSwitchNolinking          : {fullSwitchTxtNolinking},
      cmdSwitchNomain             : {fullSwitchTxtNomain},
      cmdSwitchForcebuild         : {fullSwitchTxtForcebuild, smolSwitchTxtForcebuild},
      cmdSwitchProject            : {fullSwitchTxtProject},
      cmdSwitchGc                 : {fullSwitchTxtGc},
      cmdSwitchWarnings           : {fullSwitchTxtWarnings, smolSwitchTxtWarnings},
      cmdSwitchWarning            : {fullSwitchTxtWarning},
      cmdSwitchHint               : {fullSwitchTxtHint},
      cmdSwitchWarningaserror     : {fullSwitchTxtWarningaserror},
      cmdSwitchHintaserror        : {fullSwitchTxtHintaserror},
      cmdSwitchHints              : {fullSwitchTxtHints},
      cmdSwitchThreadanalysis     : {fullSwitchTxtThreadanalysis},
      cmdSwitchStacktrace         : {fullSwitchTxtStacktrace},
      cmdSwitchStacktracemsgs     : {fullSwitchTxtStacktracemsgs},
      cmdSwitchExcessivestacktrace: {fullSwitchTxtExcessivestacktrace},
      cmdSwitchLinetrace          : {fullSwitchTxtLinetrace},
      cmdSwitchDebugger           : {fullSwitchTxtDebugger},
      cmdSwitchProfiler           : {fullSwitchTxtProfiler},
      cmdSwitchMemtracker         : {fullSwitchTxtMemtracker},
      cmdSwitchChecks             : {fullSwitchTxtChecks},
      cmdSwitchFloatchecks        : {fullSwitchTxtFloatchecks},
      cmdSwitchInfchecks          : {fullSwitchTxtInfchecks},
      cmdSwitchNanchecks          : {fullSwitchTxtNanchecks},
      cmdSwitchObjchecks          : {fullSwitchTxtObjchecks},
      cmdSwitchFieldchecks        : {fullSwitchTxtFieldchecks},
      cmdSwitchRangechecks        : {fullSwitchTxtRangechecks},
      cmdSwitchBoundchecks        : {fullSwitchTxtBoundchecks},
      cmdSwitchOverflowchecks     : {fullSwitchTxtOverflowchecks},
      cmdSwitchStaticboundchecks  : {fullSwitchTxtStaticboundchecks},
      cmdSwitchStylechecks        : {fullSwitchTxtStylechecks},
      cmdSwitchLinedir            : {fullSwitchTxtLinedir},
      cmdSwitchAssertions         : {fullSwitchTxtAssertions},
      cmdSwitchThreads            : {fullSwitchTxtThreads},
      cmdSwitchTlsemulation       : {fullSwitchTxtTlsemulation},
      cmdSwitchImplicitstatic     : {fullSwitchTxtImplicitstatic},
      cmdSwitchTrmacros           : {fullSwitchTxtTrmacros},
      cmdSwitchOpt                : {fullSwitchTxtOpt},
      cmdSwitchApp                : {fullSwitchTxtApp},
      cmdSwitchPassc              : {fullSwitchTxtPassc},
      cmdSwitchPassl              : {fullSwitchTxtPassl},
      cmdSwitchCincludes          : {fullSwitchTxtCincludes},
      cmdSwitchClibdir            : {fullSwitchTxtClibdir},
      cmdSwitchClib               : {fullSwitchTxtClib},
      cmdSwitchHeader             : {fullSwitchTxtHeader},
      cmdSwitchIndex              : {fullSwitchTxtIndex},
      cmdSwitchImport             : {fullSwitchTxtImport},
      cmdSwitchInclude            : {fullSwitchTxtInclude},
      cmdSwitchListcmd            : {fullSwitchTxtListcmd},
      cmdSwitchAsm                : {fullSwitchTxtAsm},
      cmdSwitchGenmapping         : {fullSwitchTxtGenmapping},
      cmdSwitchOs                 : {fullSwitchTxtOs},
      cmdSwitchCpu                : {fullSwitchTxtCpu},
      cmdSwitchRun                : {fullSwitchTxtRun},
      cmdSwitchMaxloopiterationsvm: {fullSwitchTxtMaxloopiterationsvm},
      cmdSwitchErrormax           : {fullSwitchTxtErrormax},
      cmdSwitchVerbosity          : {fullSwitchTxtVerbosity},
      cmdSwitchParallelbuild      : {fullSwitchTxtParallelbuild},
      # cmdSwitchVersion            : {fullSwitchTxtVersion, smolSwitchTxtVersion},
      # cmdSwitchAdvanced           : {fullSwitchTxtAdvanced},
      # cmdSwitchFullhelp           : {fullSwitchTxtFullhelp},
      # cmdSwitchHelp               : {fullSwitchTxtHelp, smolSwitchTxtHelp},
      cmdSwitchIncremental        : {fullSwitchTxtIncremental, aliasSwitchTxtIncremental},
      cmdSwitchSkipcfg            : {fullSwitchTxtSkipcfg},
      cmdSwitchSkipprojcfg        : {fullSwitchTxtSkipprojcfg},
      cmdSwitchSkipusercfg        : {fullSwitchTxtSkipusercfg},
      cmdSwitchSkipparentcfg      : {fullSwitchTxtSkipparentcfg},
      cmdSwitchGenscript          : {fullSwitchTxtGenscript},
      cmdSwitchColors             : {fullSwitchTxtColors},
      cmdSwitchLib                : {fullSwitchTxtLib},
      cmdSwitchPutenv             : {fullSwitchTxtPutenv},
      cmdSwitchCc                 : {fullSwitchTxtCc},
      cmdSwitchStdout             : {fullSwitchTxtStdout},
      cmdSwitchFilenames          : {fullSwitchTxtFilenames},
      cmdSwitchMsgformat          : {fullSwitchTxtMsgformat},
      cmdSwitchProcessing         : {fullSwitchTxtProcessing},
      cmdSwitchUnitsep            : {fullSwitchTxtUnitsep},
      cmdSwitchListfullpaths      : {fullSwitchTxtListfullpaths},
      cmdSwitchSpellsuggest       : {fullSwitchTxtSpellsuggest},
      cmdSwitchDeclaredlocs       : {fullSwitchTxtDeclaredlocs},
      cmdSwitchDynliboverride     : {fullSwitchTxtDynliboverride},
      cmdSwitchDynliboverrideall  : {fullSwitchTxtDynliboverrideall},
      cmdSwitchExperimental       : {fullSwitchTxtExperimental},
      cmdSwitchExceptions         : {fullSwitchTxtExceptions},
      cmdSwitchCppdefine          : {fullSwitchTxtCppdefine},
      cmdSwitchSeqsv2             : {fullSwitchTxtSeqsv2},
      cmdSwitchStylecheck         : {fullSwitchTxtStylecheck},
      cmdSwitchShowallmismatches  : {fullSwitchTxtShowallmismatches},
      cmdSwitchDocinternal        : {fullSwitchTxtDocinternal},
      cmdSwitchMultimethods       : {fullSwitchTxtMultimethods},
      cmdSwitchExpandmacro        : {fullSwitchTxtExpandmacro},
      cmdSwitchExpandarc          : {fullSwitchTxtExpandarc},
      cmdSwitchBenchmarkvm        : {fullSwitchTxtBenchmarkvm},
      cmdSwitchProfilevm          : {fullSwitchTxtProfilevm},
      cmdSwitchSinkinference      : {fullSwitchTxtSinkinference},
      cmdSwitchCursorinference    : {fullSwitchTxtCursorinference},
      cmdSwitchPanics             : {fullSwitchTxtPanics},
      cmdSwitchSourcemap          : {fullSwitchTxtSourcemap},
      cmdSwitchDeepcopy           : {fullSwitchTxtDeepcopy},
      cmdSwitchProjStdin          : {smolSwitchTxtProjStdin},
      cmdSwitchCmdexitgcstats     : {fullSwitchTxtCmdexitgcstats},
      cmdSwitchConfigVar          : {fullSwitchTxtConfigVar},
    ]