##[

the boring bits that really aren't very relevant to dust.

]##

import std/times
import std/os
import std/parseopt

import
  compiler / ast / [
    idents,
    lineinfos,
  ],
  compiler / front / [
      cmdlinehelper,
      # commands,
      condsyms,
      options,
      optionsprocessor,
  ],
  compiler / modules / [
    modules,
    modulegraphs,
  ],
  compiler / utils / pathutils

from compiler / front / commands import procSwitchResultToEvents,
                                        cliEventLogger

from compiler/front/main import customizeForBackend


proc processArgument(pass: TCmdLinePass; p: OptParser;
                     argsCount: var int; config: ConfigRef): bool =
  if argsCount == 0:
    # the first argument is the file
    config.projectName = unixToNativePath(p.key)
    config.arguments = cmdLineRest(p) # consume the rest
    result = true
  inc argsCount


proc cmdLine(pass: TCmdLinePass, cmd: openArray[string]; config: ConfigRef) =
  ## parse the command-line into the config
  var p = initOptParser(cmd)
  var argsCount = 0

  # the 'check' command (which best approximates dust's operation) is implied
  if pass != passCmd2:
    setCommandEarly(config, "check")

  config.commandLine.setLen 0  # some bug
  while true:
    next(p)
    case p.kind
    of cmdEnd:
      break
    of cmdLongOption, cmdShortOption:
      config.commandLine.add " "
      config.commandLine.add:
        if p.kind == cmdLongOption: "--" else: "-"
      config.commandLine.add p.key.quoteShell
      if p.val.len > 0:
        config.commandLine.add ':'
        config.commandLine.add p.val.quoteShell
      if p.key == " ":
        p.key = "-"
        if processArgument(pass, p, argsCount, config):
          break
      else:
        # Main part of the configuration processing -
        # `optionsprocessor.processSwitch` processes input switches a second
        # time and puts them in necessary configuration fields.
        let res = processSwitch(pass, p, config)
        for e in procSwitchResultToEvents(config, pass, p.key, p.val, res):
          config.cliEventLogger(e)
    of cmdArgument:
      config.commandLine.add " "
      config.commandLine.add p.key.quoteShell
      if processArgument(pass, p, argsCount, config):
        break


proc compile*(graph: ModuleGraph) =
  ## compile a module graph
  let config = graph.config
  config.lastCmdTime = epochTime()
  if config.libpath notin config.searchPaths:
    config.searchPaths.add config.libpath     # make sure we can import

  initDefines(config.symbols)

  config.setErrorMaxHighMaybe                 # for now, we honor errorMax
  defineSymbol(config, "nimcheck")            # useful for static: reasons

  customizeForBackend(graph, config, backendC)# use C as the default target

  compileProject graph                        # process the graph


proc setup*(cache: IdentCache; config: ConfigRef; graph: ModuleGraph,
            argv: openArray[string]): bool =
  let prog = NimProg(
    supportsStdinFile: false,
    processCmdLine: cmdLine
  )
  initDefinesProg(prog, config, "dust")
  processCmdLineAndProjectPath(prog, config, argv)
  result = loadConfigsAndProcessCmdLine(prog, cache, config, graph, argv)

  # force enable/disable some options
  incl config, optStaticBoundsCheck
  excl config, optWarns
  excl config, optHints


proc wantMainModule*(config: ConfigRef): bool =
  ## Sets the main module to the file whose path was provided on the command
  ## line, returning false if this isn't possible (because there's no path).
  if config.projectFull.isEmpty:
    result = false
  else:
    modules.wantMainModule(config)
    result = true
