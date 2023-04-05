#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import std/[os]
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

proc handleCmdLine(cache: IdentCache; conf: ConfigRef): CmdLineHandlingResult =
  ## Main entry point to the compiler - dispatches command-line commands
  ## into different subsystems, sets up configuration options for the
  ## `conf`:arg: and so on.
  # TODO: remove the need for all the `conf.errorCounter` checks.
  proc eventLogger(self: NimProg, conf: ConfigRef, evt: CliEvent) =
    commands.cliEventLogger(conf, evt)

  let
    cliParams = commandLineParams()
    self = NimProg(supportsStdinFile: true,
                   processCmdLine: processCmdLine,
                   eventReceiver: eventLogger)

  self.initDefinesProg(conf, "nim_compiler")
  if paramCount() == 0:
    return cliErrNoParamsProvided

  self.processCmdLineAndProjectPath(conf, cliParams)
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
