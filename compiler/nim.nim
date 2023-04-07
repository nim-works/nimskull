#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import std/[os, parseopt]
from osproc import execCmd
when defined(windows) and not defined(nimKochBootstrap):
  # remove workaround pending bootstrap >= 1.5.1
  # refs https://github.com/nim-lang/Nim/issues/18334#issuecomment-867114536
  # alternative would be to prepend `currentSourcePath.parentDir.quoteShell`
  when defined(gcc):
    when defined(x86):
      {.link: "../icons/nim.res".}
    else:
      {.link: "../icons/nim_icon.o".}

  when defined(amd64) and defined(vcc):
    {.link: "../icons/nim-amd64-windows-vcc.res".}
  when defined(i386) and defined(vcc):
    {.link: "../icons/nim-i386-windows-vcc.res".}

import
  compiler/front/[
    msgs, main, cmdlinehelper, options, commands
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/utils/[
    pathutils
  ],
  compiler/ast/[
    idents
  ]

from std/browsers import openDefaultBrowser
from compiler/utils/nodejs import findNodeJs

from compiler/ast/lineinfos import instLoc

when hasTinyCBackend:
  import compiler/backend/tccgen

when defined(profiler) or defined(memProfiler):
  {.hint: "Profiling support is turned on!".}
  import sdt/nimprof


proc getNimRunExe(conf: ConfigRef): string =
  # xxx consider defining `conf.getConfigVar("nimrun.exe")` to allow users to
  # customize the binary to run the command with, e.g. for custom `nodejs` or `wine`.
  if conf.isDefined("mingw"):
    if conf.isDefined("i386"): result = "wine"
    elif conf.isDefined("amd64"): result = "wine64"

type
  CmdLineHandlingResult = enum
    cliFinished             # might still have errors, check `conf.errorCount`
    cliErrNoParamsProvided
    cliErrConfigProcessing
    cliErrCommandProcessing

proc processCmdLine(pass: TCmdLinePass, cmd: string, config: ConfigRef) =
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

  while true:
    parseopt.next(p)
    case p.kind:
      of cmdEnd: break
      of cmdLongOption, cmdShortOption:
        config.commandLine.add " "
        config.commandLine.addCmdPrefix p.kind
        config.commandLine.add p.key.quoteShell # quoteShell to be future proof
        if p.val.len > 0:
          config.commandLine.add ':'
          config.commandLine.add p.val.quoteShell

        if p.key == "": # `-` was passed to indicate main project is stdin
          p.key = "-"
          if processArgument(pass, p, argsCount, config):
            break
        else:
          # Main part of the configuration processing -
          # `commands.processSwitch` processes input switches a second time
          # and puts them in necessary configuration fields.
          let res = processSwitch(pass, p, config)
          
          if res.deprecatedNoopSwitchArg:
            config.cliEventLogger:
              CliEvent(kind: cliEvtWarnSwitchValDeprecatedNoop,
                       pass: pass,
                       origParseOptKey: p.key,
                       origParseOptVal: p.val,
                       procResult: res,
                       srcCodeOrigin: instLoc())

          case res.kind
          of procSwitchSuccess:
            discard "eventually should handle success events for tracing"
          else:
            config.cliEventLogger:
              CliEvent(kind: cliEvtErrFlagProcessing,
                       pass: pass,
                       origParseOptKey: p.key,
                       origParseOptVal: p.val,
                       procResult: res,
                       srcCodeOrigin: instLoc())
            break # always bail on error for CLI parsing
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

proc handleCmdLine(cache: IdentCache; conf: ConfigRef): CmdLineHandlingResult =
  ## Main entry point to the compiler - dispatches command-line commands
  ## into different subsystems, sets up configuration options for the
  ## `conf`:arg: and so on.
  # TODO: remove the need for all the `conf.errorCounter` checks.
  var self = NimProg(supportsStdinFile: true,
                     processCmdLine: processCmdLine)

  self.initDefinesProg(conf, "nim_compiler")
  if paramCount() == 0:
    return cliErrNoParamsProvided

  self.processCmdLineAndProjectPath(conf)
  if conf.errorCounter != 0: return
  var graph = newModuleGraph(cache, conf)

  if not self.loadConfigsAndProcessCmdLine(cache, conf, graph) or
      conf.errorCounter != 0:
    return

  mainCommand(graph)
  if optCmdExitGcStats in conf.globalOptions:
    conf.logGcStats(GC_getStatistics())

  if conf.errorCounter != 0: return
  when hasTinyCBackend:
    if conf.cmd == cmdTcc:
      tccgen.run(conf, conf.arguments)
  if optRun in conf.globalOptions:
    let output = conf.absOutFile
    case conf.cmd
    of cmdBackends, cmdTcc:
      let nimRunExe = getNimRunExe(conf)
      var cmdPrefix: string
      if nimRunExe.len > 0: cmdPrefix.add nimRunExe.quoteShell
      case conf.backend
      of backendC: discard
      of backendJs:
        # D20210217T215950:here this flag is needed for node < v15.0.0, otherwise
        # tasyncjs_fail` would fail, refs https://nodejs.org/api/cli.html#cli_unhandled_rejections_mode
        if cmdPrefix.len == 0: cmdPrefix = findNodeJs().quoteShell
        cmdPrefix.add " --unhandled-rejections=strict"
      of backendNimVm:
        if cmdPrefix.len == 0:
          cmdPrefix = changeFileExt(getAppDir() / "vmrunner", ExeExt)
      else: doAssert false, $conf.backend
      if cmdPrefix.len > 0: cmdPrefix.add " "
        # without the `cmdPrefix.len > 0` check, on windows you'd get a cryptic:
        # `The parameter is incorrect`
      let cmd = cmdPrefix & output.quoteShell & ' ' & conf.arguments
      conf.logExecStart(cmd)
      let code = execCmd(cmd)
      if code != 0:
        conf.logError(CliEvent(kind: cliEvtErrRunCmdFailed,
                                shellCmd: cmd,
                                exitCode: code))
    of cmdDocLike, cmdRst2html, cmdRst2tex: # bugfix(cmdRst2tex was missing)
      if conf.arguments.len > 0:
        # reserved for future use
        conf.logError(CliEvent(
          kind: cliEvtErrCmdExpectedNoAdditionalArgs,
          cmd: conf.command,
          unexpectedArgs: conf.arguments))
      openDefaultBrowser($output)
    else:
      # support as needed
      conf.logError(CliEvent(
        kind: cliEvtErrUnexpectedRunOpt,
        cmd: conf.command
      ))

when declared(GC_setMaxPause):
  GC_setMaxPause 2_000

when compileOption("gc", "refc"):
  # the new correct mark&sweep collector is too slow :-/
  GC_disableMarkAndSweep()

when not defined(selftest):
  import compiler/front/cli_reporter # xxx: last bit of legacy reports to remove
  var conf = newConfigRef(cli_reporter.reportHook)
  conf.astDiagToLegacyReport = cli_reporter.legacyReportBridge
  conf.writeHook =
    proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
      msgs.msgWrite(conf, msg, flags)

  conf.writelnHook =
    proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
      conf.writeHook(conf, msg & "\n", flags)

  case handleCmdLine(newIdentCache(), conf)
  of cliErrNoParamsProvided:
    conf.logError(CliEvent(kind: cliEvtErrNoCliParamsProvided))
    conf.showMsg(helpOnErrorMsg(conf))
  of cliErrConfigProcessing, cliErrCommandProcessing, cliFinished:
    # TODO: more specific handling here
    discard "error messages reported internally"

  when declared(GC_setMaxPause):
    echo GC_getStatistics()

  msgQuit(int8(conf.errorCounter > 0))
