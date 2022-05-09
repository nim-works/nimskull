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
    parseutils,
    parseopt,
    sequtils,
    strtabs,
    pathnorm
  ],
  compiler/modules/[
    nimblecmd,
  ],
  compiler/ast/[
    lineinfos,
    reports,
    wordrecg,
  ],
  compiler/front/[
    options,
    msgs,
    cli_reporter,
    sexp_reporter
  ],
  compiler/backend/[
    extccomp
  ],
  compiler/utils/[
    nversion,
    pathutils,
    platform
  ]


from compiler/ast/ast import setUseIc, eqTypeFlags, tfGcSafe, tfNoSideEffect

bootSwitch(usedTinyC, hasTinyCBackend, "-d:tinyc")

type
  TCmdLinePass* = enum
    passCmd1,                 # first pass over the command line
    passCmd2,                 # second pass over the command line
    passPP                    # preprocessor called processCommand()


proc getNimSourceData(): tuple[hash, date: string] {.compileTime.} =
  ## Retrieve metadata about the compiler source code.
  const
    # These are defined by koch
    nimSourceHash {.strdefine.} = ""
    nimSourceDate {.strdefine.} = ""
  result = (nimSourceHash, nimSourceDate)

proc getCliData(conf: ConfigRef): InternalCliData =
  ## Get CLI data from current configuration and nim compiler configuration
  ## (source code/date defines, boot switches)
  let (sourceHash, sourceDate) = getNimSourceData()

  InternalCliData(
    version: VersionAsString,
    sourceHash: sourceHash,
    sourceDate: sourceDate,
    boot: @[
      usedRelease,
      usedDanger,
      usedTinyC,
      useLinenoise,
      usedBoehm,
      usedMarkAndSweep,
      usedGoGC,
      usedNoGC
    ],
    cpu: conf.target.hostCPU,
    os: conf.target.hostOS
  )


proc helpOnError(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    conf.localReport():
      InternalReport(kind: rintCliHelp, cliData: conf.getCliData())

    msgQuit(0)

proc writeAdvancedUsage(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    conf.localReport():
      InternalReport(
        kind: rintCliAdvancedUsage, cliData: conf.getCliData())

    msgQuit(0)

proc writeFullhelp(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    conf.localReport():
      InternalReport(
        kind: rintCliFullHelp, cliData: conf.getCliData())

    msgQuit(0)

proc writeVersionInfo(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    conf.localReport():
      InternalReport(
        kind: rintCliVersion, cliData: conf.getCliData())

    msgQuit(0)

proc writeCommandLineUsage*(conf: ConfigRef) =
  conf.localReport(InternalReport(
    kind: rintCliHelp, cliData: conf.getCliData()))

proc addPrefix(switch: string): string =
  if switch.len <= 1: result = "-" & switch
  else: result = "--" & switch

# Full list of all the command line options. Necessary to provide "invalid
# command line options - did you mean ...?". In theory generation of this
# list could be automated by maintaining a `{.compiletime.}` variable that
# is populated by macro which scans every `case` used, but for the time
# being it is easier to manually keep this list up-to-date.
const optNames = @[
  # processSwitch
  "eval", "path", "p", "nimblepath", "babelpath", "nonimblepath",
  "nobabelpath", "clearnimblepath", "excludepath", "nimcache", "out", "o",
  "outdir", "usenimcache", "docseesrcurl", "docroot", "backend", "b",
  "doccmd", "define", "d", "undef", "u", "compile", "link", "debuginfo",
  "embedsrc", "compileonly", "c", "nolinking", "nomain", "forcebuild", "f",
  "project", "warnings", "w", "warning", "hint", "warningaserror",
  "hintaserror", "hints", "threadanalysis", "stacktrace",
  "stacktracemsgs", "excessivestacktrace", "linetrace",
  "debugger", "g", "profiler", "memtracker", "hotcodereloading", "checks",
  "floatchecks", "infchecks", "nanchecks", "objchecks", "fieldchecks",
  "rangechecks", "boundchecks", "refchecks", "overflowchecks",
  "staticboundchecks", "stylechecks", "linedir", "assertions", "threads",
  "tlsemulation", "implicitstatic", "patterns", "opt", "app", "passc",
  "passl", "cincludes", "clibdir", "clib", "header", "index", "import",
  "include", "listcmd", "asm", "genmapping", "os", "cpu", "run",
  "maxloopiterationsvm", "errormax", "verbosity", "parallelbuild",
  "version", "advanced", "fullhelp", "help", "symbolfiles", "skipcfg",
  "skipprojcfg", "skipusercfg", "skipparentcfg", "genscript", "colors",
  "lib", "putenv", "cc", "track", "trackdirty", "suggest", "def",
  "context", "usages", "defusages", "stdout", "filenames", "processing",
  "unitsep", "listfullpaths", "spellsuggest", "declaredlocs",
  "dynliboverride", "dynliboverrideall", "experimental", "legacy",
  "nocppexceptions", "exceptions", "cppdefine", "newruntime", "seqsv2",
  "stylecheck", "showallmismatches", "cppcompiletonamespace",
  "docinternal", "multimethods", "expandmacro", "expandarc", "useversion",
  "benchmarkvm", "profilevm", "sinkinference", "cursorinference", "panics",
  "sourcemap", "deepcopy", "nilseqs",
]

proc invalidCmdLineOption(conf: ConfigRef; pass: TCmdLinePass, switch: string, info: TLineInfo) =
  conf.localReport(info, ExternalReport(
    kind: rextInvalidCommandLineOption,
    cmdlineProvided: switch,
    cmdlineAllowed: optNames
  ))

proc splitSwitch(conf: ConfigRef; switch: string, cmd, arg: var string, pass: TCmdLinePass,
                 info: TLineInfo) =
  cmd = ""
  var i = 0
  if i < switch.len and switch[i] == '-': inc(i)
  if i < switch.len and switch[i] == '-': inc(i)
  while i < switch.len:
    case switch[i]
    of 'a'..'z', 'A'..'Z', '0'..'9', '_', '.': cmd.add(switch[i])
    else: break
    inc(i)
  if i >= switch.len: arg = ""
  # cmd:arg => (cmd,arg)
  elif switch[i] in {':', '='}: arg = substr(switch, i + 1)
  # cmd[sub]:rest => (cmd,[sub]:rest)
  elif switch[i] == '[': arg = substr(switch, i)
  else: invalidCmdLineOption(conf, pass, switch, info)

template switchOn(arg: string): bool =
  # xxx use `switchOn` wherever appropriate
  case arg.normalize
  of "", "on": true
  of "off": false
  else:
    conf.localReport ExternalReport(
      kind: rextExpectedOnOrOff, cmdlineProvided: arg)

    false

proc processOnOffSwitch(
    conf: ConfigRef; op: TOptions, arg: string, pass: TCmdLinePass,
    info: TLineInfo, switch: string
  ) =
  case arg.normalize
  of "", "on": conf.incl op
  of "off": conf.excl op
  else:
    conf.localReport(info, ExternalReport(
      kind: rextExpectedOnOrOff, cmdlineProvided: arg))

proc processOnOffSwitchOrList(conf: ConfigRef; op: TOptions, arg: string, pass: TCmdLinePass,
                              info: TLineInfo, switch: string): bool =
  result = false
  case arg.normalize
  of "on": conf.incl op
  of "off": conf.excl op
  of "list": result = true
  else:
    conf.localReport ExternalReport(
      kind: rextExpectedOnOrOffOrList, cmdlineProvided: arg, cmdlineSwitch: switch)

proc processOnOffSwitchG(conf: ConfigRef; op: TGlobalOptions, arg: string, pass: TCmdLinePass,
                         info: TLineInfo, switch: string) =
  case arg.normalize
  of "", "on": conf.incl op
  of "off": conf.excl op
  else:
    conf.localReport ExternalReport(
      kind: rextExpectedOnOrOff, cmdlineProvided: arg, cmdlineSwitch: switch)

proc expectArg(conf: ConfigRef; switch, arg: string, pass: TCmdLinePass, info: TLineInfo) =
  if arg == "":
    conf.localReport ExternalReport(
      kind: rextExpectedCmdArgument, cmdlineProvided: switch, cmdlineSwitch: switch)

proc expectNoArg(conf: ConfigRef; switch, arg: string, pass: TCmdLinePass, info: TLineInfo) =
  if arg != "":
    conf.localReport ExternalReport(
      kind: rextExpectedNoCmdArgument, cmdlineProvided: switch, cmdlineSwitch: switch)

proc processSpecificNote*(arg: string, state: TSpecialWord, pass: TCmdLinePass,
                         info: TLineInfo; orig: string; conf: ConfigRef) =
  var id = ""  # arg = key or [key] or key:val or [key]:val;  with val=on|off
  var i = 0
  var notes: ReportKinds
  var isBracket = false
  if i < arg.len and arg[i] == '[':
    isBracket = true
    inc(i)
  while i < arg.len and (arg[i] notin {':', '=', ']'}):
    id.add(arg[i])
    inc(i)
  if isBracket:
    if i < arg.len and arg[i] == ']': inc(i)
    else: invalidCmdLineOption(conf, pass, orig, info)

  if i == arg.len: discard
  elif i < arg.len and (arg[i] in {':', '='}): inc(i)
  else: invalidCmdLineOption(conf, pass, orig, info)

  proc findNote(noteSet: ReportKinds, onFail: ReportKind, groups: seq[(string, ReportKinds)]) =
    # Check groups like `--hint/warning[all]` or `--hint[Performance]` (very
    # vague term that maps onto multiple report kinds, such as "copies to
    # sink") first, because report groups have the same string values:
    # (`rlexLinterReport = "Name"`, `rsemLinterReport = "Name"`)
    for (groupName, flags) in groups:
      if cmpIgnoreStyle(groupName, id) == 0:
        notes = flags
        return

    # unfortunately, hintUser and warningUser clash, otherwise
    # implementation would simplify a bit
    let x = findStr(noteSet, id, onFail)
    if x != onFail:
      notes = {x}

    else:
      var r = ExternalReport(kind: onFail)
      r.cmdlineProvided = id
      for kind in noteSet:
        r.cmdlineAllowed.add $kind

      conf.localReport r

  if state in {wHint, wHintAsError}:
    findNote(repHintKinds, rextInvalidHint, repHintGroups)
  else:
    findNote(repWarningKinds, rextInvalidWarning, repWarningGroups)

  var val = substr(arg, i).normalize
  if val == "":
    val = "on"

  if val notin ["on", "off"]:
    # xxx in future work we should also allow users to have control over
    # `foreignPackageNotes` so that they can enable
    # `hints|warnings|warningAsErrors` for all the code they depend on.
    conf.localReport ExternalReport(
      kind: rextExpectedOnOrOff, cmdlineProvided: arg)

  else:
    let isOn = val == "on"
    if isOn and id.normalize == "all":
      conf.localReport ExternalReport(kind: rextOnlyAllOffSupported)

    for n in notes:
      if n notin conf.cmdlineNotes or pass == passCmd1:
        if pass == passCmd1:
          conf.incl(cnCmdline, n)

        conf.incl(cnModifiedy, n)

        if state in {wWarningAsError, wHintAsError}:
          # xxx rename warningAsErrors to noteAsErrors
          conf.flip(cnWarnAsError, n, isOn)

        else:
          conf.flip(cnCurrent, n, isOn)
          conf.flip(cnMainPackage, n, isOn)

        if not isOn:
          conf.excl(cnForeign, n)

proc processCompile(conf: ConfigRef; filename: string) =
  var found = findFile(conf, filename)
  if found.isEmpty: found = AbsoluteFile filename
  extccomp.addExternalFileToCompile(conf, found)

template warningOptionNoop(switch: string) =
  conf.localReport(info,
    ExternalReport(
      kind: rextDeprecated,
      msg: "'$#' is deprecated, now a noop" % switch))

template deprecatedAlias(oldName, newName: string) =
  conf.localReport(
    info,
    ExternalReport(
      kind: rextDeprecated,
      msg: "'$#' is a deprecated alias for '$#'" % [oldName, newName]))

# List of GC names for the error generation. It cannot be created
# from enum set using `getEnumNames` because nim cmdline has
# multiple names for the same garbage collector.
const gcNames = @[
  "boehm", "refc", "markandsweep", "destructors", "arc", "orc",
  "hooks", "go", "none", "stack", "regision", "v2", "generational"]


const cmdNames = @[
  "c", "cc", "compile", "compiletoc", "cpp", "compiletocpp", "objc",
  "compiletooc", "js", "compiletojs", "r", "run", "check", "e",
  "doc2", "doc", "doc2tex", "rst2html", "rst2tex", "jsondoc2",
  "jsondoc", "ctags", "buildindex", "gendepend", "dump", "parse", "rod",
  "secret", "nop", "help", "jsonscript",
]


proc testCompileOptionArg*(conf: ConfigRef; switch, arg: string, info: TLineInfo): bool =
  case switch.normalize
  of "gc":
    case arg.normalize
    of "boehm": result = conf.selectedGC == gcBoehm
    of "refc": result = conf.selectedGC == gcRefc
    of "markandsweep": result = conf.selectedGC == gcMarkAndSweep
    of "destructors", "arc": result = conf.selectedGC == gcArc
    of "orc": result = conf.selectedGC == gcOrc
    of "hooks": result = conf.selectedGC == gcHooks
    of "go": result = conf.selectedGC == gcGo
    of "none": result = conf.selectedGC == gcNone
    of "stack", "regions": result = conf.selectedGC == gcRegions
    of "v2", "generational": warningOptionNoop(arg)
    else:
      conf.localReport(info, ExternalReport(
        kind: rextUnexpectedValue,
        cmdlineProvided: arg,
        cmdlineSwitch: "gc",
        cmdlineAllowed: gcNames))

  of "opt":
    case arg.normalize
    of "speed": result = contains(conf.options, optOptimizeSpeed)
    of "size": result = contains(conf.options, optOptimizeSize)
    of "none": result = conf.options * {optOptimizeSpeed, optOptimizeSize} == {}
    else:
      conf.localReport(info, ExternalReport(
        kind: rextUnexpectedValue,
        cmdlineSwitch: "opt",
        cmdlineProvided: arg,
        cmdlineAllowed: @["speed", "size", "none"]))

  of "verbosity": result = $conf.verbosity == arg
  of "app":
    case arg.normalize
    of "gui": result = contains(conf.globalOptions, optGenGuiApp)
    of "console": result = not contains(conf.globalOptions, optGenGuiApp)
    of "lib": result = contains(conf.globalOptions, optGenDynLib) and
                      not contains(conf.globalOptions, optGenGuiApp)
    of "staticlib": result = contains(conf.globalOptions, optGenStaticLib) and
                      not contains(conf.globalOptions, optGenGuiApp)
    else:
      conf.localReport(info, ExternalReport(
        kind: rextUnexpectedValue,
        cmdlineSwitch: "app",
        cmdlineProvided: arg,
        cmdlineAllowed: @["gui", "console", "lib", "staticlib"]))

  of "dynliboverride":
    result = isDynlibOverride(conf, arg)
  of "exceptions":
    case arg.normalize
    of "cpp": result = conf.exc == excCpp
    of "setjmp": result = conf.exc == excSetjmp
    of "quirky": result = conf.exc == excQuirky
    of "goto": result = conf.exc == excGoto
    else:
      conf.localReport(info, ExternalReport(
        kind: rextUnexpectedValue,
        cmdlineSwitch: "exceptions",
        cmdlineProvided: arg,
        cmdlineAllowed: @["cpp", "setjmp", "quirky", "goto"]))

  else: invalidCmdLineOption(conf, passCmd1, switch, info)

proc testCompileOption*(conf: ConfigRef; switch: string, info: TLineInfo): bool =
  case switch.normalize
  of "debuginfo": result = contains(conf.globalOptions, optCDebug)
  of "compileonly", "c": result = contains(conf.globalOptions, optCompileOnly)
  of "nolinking": result = contains(conf.globalOptions, optNoLinking)
  of "nomain": result = contains(conf.globalOptions, optNoMain)
  of "forcebuild", "f": result = contains(conf.globalOptions, optForceFullMake)
  of "warnings", "w": result = contains(conf.options, optWarns)
  of "hints": result = contains(conf.options, optHints)
  of "threadanalysis": result = contains(conf.globalOptions, optThreadAnalysis)
  of "stacktrace": result = contains(conf.options, optStackTrace)
  of "stacktracemsgs": result = contains(conf.options, optStackTraceMsgs)
  of "linetrace": result = contains(conf.options, optLineTrace)
  of "debugger": result = contains(conf.globalOptions, optCDebug)
  of "profiler": result = contains(conf.options, optProfiler)
  of "memtracker": result = contains(conf.options, optMemTracker)
  of "checks", "x": result = conf.options * ChecksOptions == ChecksOptions
  of "floatchecks":
    result = conf.options * {optNaNCheck, optInfCheck} == {optNaNCheck, optInfCheck}
  of "infchecks": result = contains(conf.options, optInfCheck)
  of "nanchecks": result = contains(conf.options, optNaNCheck)
  of "objchecks": result = contains(conf.options, optObjCheck)
  of "fieldchecks": result = contains(conf.options, optFieldCheck)
  of "rangechecks": result = contains(conf.options, optRangeCheck)
  of "boundchecks": result = contains(conf.options, optBoundsCheck)
  of "refchecks":
    conf.localReport ExternalReport(
      kind: rextDeprecated, msg: "refchecks option is deprecated")
    result = contains(conf.options, optRefCheck)
  of "overflowchecks": result = contains(conf.options, optOverflowCheck)
  of "staticboundchecks": result = contains(conf.options, optStaticBoundsCheck)
  of "stylechecks": result = contains(conf.options, optStyleCheck)
  of "linedir": result = contains(conf.options, optLineDir)
  of "assertions", "a": result = contains(conf.options, optAssert)
  of "run", "r": result = contains(conf.globalOptions, optRun)
  of "symbolfiles": result = conf.symbolFiles != disabledSf
  of "genscript": result = contains(conf.globalOptions, optGenScript)
  of "threads": result = contains(conf.globalOptions, optThreads)
  of "tlsemulation": result = contains(conf.globalOptions, optTlsEmulation)
  of "implicitstatic": result = contains(conf.options, optImplicitStatic)
  of "patterns", "trmacros":
    if switch.normalize == "patterns": deprecatedAlias(switch, "trmacros")
    result = contains(conf.options, optTrMacros)
  of "excessivestacktrace": result = contains(conf.globalOptions, optExcessiveStackTrace)
  of "nilseqs", "nilchecks", "taintmode": warningOptionNoop(switch)
  else: invalidCmdLineOption(conf, passCmd1, switch, info)

proc processPath(conf: ConfigRef; path: string, info: TLineInfo, switch: string,
                 notRelativeToProj = false): AbsoluteDir =
  let p = if os.isAbsolute(path) or '$' in path:
            path
          elif notRelativeToProj:
            getCurrentDir() / path
          else:
            conf.projectPath.string / path
  try:
    result = AbsoluteDir pathSubs(conf, p, toFullPath(conf, info).splitFile().dir)
  except ValueError:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidPath, cmdlineProvided: p, cmdlineSwitch: switch))
    result = AbsoluteDir p

proc processCfgPath(conf: ConfigRef; path: string, info: TLineInfo, switch: string): AbsoluteDir =
  let path = if path.len > 0 and path[0] == '"': strutils.unescape(path)
             else: path
  let basedir = toFullPath(conf, info).splitFile().dir
  let p = if os.isAbsolute(path) or '$' in path:
            path
          else:
            basedir / path
  try:
    result = AbsoluteDir pathSubs(conf, p, basedir)
  except ValueError:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidPath, cmdlineProvided: p, cmdlineSwitch: switch))
    result = AbsoluteDir p

proc makeAbsolute(s: string): AbsoluteFile =
  if isAbsolute(s):
    AbsoluteFile pathnorm.normalizePath(s)
  else:
    AbsoluteFile pathnorm.normalizePath(os.getCurrentDir() / s)

proc setTrackingInfo(conf: ConfigRef; dirty, file, line, column: string,
                     info: TLineInfo) =
  ## set tracking info, common code for track, trackDirty, & ideTrack
  var ln, col: int
  if parseUtils.parseInt(line, ln) <= 0:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidNumber, cmdlineProvided: line))
  if parseUtils.parseInt(column, col) <= 0:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidNumber, cmdlineProvided: column))

  let a = makeAbsolute(file)
  if dirty == "":
    conf.m.trackPos = newLineInfo(conf, a, ln, col)
  else:
    let dirtyOriginalIdx = fileInfoIdx(conf, a)
    if dirtyOriginalIdx.int32 >= 0:
      msgs.setDirtyFile(conf, dirtyOriginalIdx, makeAbsolute(dirty))
    conf.m.trackPos = newLineInfo(dirtyOriginalIdx, ln, col)

proc trackDirty(conf: ConfigRef; arg: string, info: TLineInfo) =
  var a = arg.split(',')
  if a.len != 4:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidValue,
      cmdlineProvided: arg, cmdlineError: "DIRTY_BUFFER,ORIGINAL_FILE,LINE,COLUMN expected"))
  setTrackingInfo(conf, a[0], a[1], a[2], a[3], info)

proc track(conf: ConfigRef; arg: string, info: TLineInfo) =
  var a = arg.split(',')
  if a.len != 3:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidValue,
      cmdlineProvided: arg, cmdlineError: "FILE,LINE,COLUMN expected"))
  setTrackingInfo(conf, "", a[0], a[1], a[2], info)

proc trackIde(conf: ConfigRef; cmd: IdeCmd, arg: string, info: TLineInfo) =
  ## set the tracking info related to an ide cmd, supports optional dirty file
  var a = arg.split(',')
  case a.len
  of 4:
    setTrackingInfo(conf, a[0], a[1], a[2], a[3], info)
  of 3:
    setTrackingInfo(conf, "", a[0], a[1], a[2], info)
  else:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidValue,
      cmdlineProvided: arg, cmdlineError: "[DIRTY_BUFFER,]ORIGINAL_FILE,LINE,COLUMN expected"))
  conf.ideCmd = cmd

proc dynlibOverride(conf: ConfigRef; switch, arg: string, pass: TCmdLinePass, info: TLineInfo) =
  if pass in {passCmd2, passPP}:
    expectArg(conf, switch, arg, pass, info)
    options.inclDynlibOverride(conf, arg)

proc handleStdinOrCmdInput(conf: ConfigRef) =
  conf.projectFull = conf.projectName.AbsoluteFile
  conf.projectPath = AbsoluteDir getCurrentDir()
  if conf.outDir.isEmpty:
    conf.outDir = getNimcacheDir(conf)

proc handleStdinInput*(conf: ConfigRef) =
  conf.projectName = "stdinfile"
  conf.projectIsStdin = true
  handleStdinOrCmdInput(conf)

proc handleCmdInput*(conf: ConfigRef) =
  conf.projectName = "cmdfile"
  handleStdinOrCmdInput(conf)

proc parseCommand*(command: string): Command =
  # NOTE when adding elements to this list, sync with `cmdNames` const
  case command.normalize
  of "c", "cc", "compile", "compiletoc": cmdCompileToC
  of "cpp", "compiletocpp": cmdCompileToCpp
  of "objc", "compiletooc": cmdCompileToOC
  of "js", "compiletojs": cmdCompileToJS
  of "r": cmdCrun
  of "run": cmdTcc
  of "check": cmdCheck
  of "e": cmdNimscript
  of "doc2", "doc": cmdDoc
  of "doc2tex": cmdDoc2tex
  of "rst2html": cmdRst2html
  of "rst2tex": cmdRst2tex
  of "jsondoc2", "jsondoc": cmdJsondoc
  of "ctags": cmdCtags
  of "buildindex": cmdBuildindex
  of "gendepend": cmdGendepend
  of "dump": cmdDump
  of "parse": cmdParse
  of "rod": cmdRod
  of "secret": cmdInteractive
  of "nop", "help": cmdNop
  of "jsonscript": cmdJsonscript
  else: cmdUnknown

proc setCmd*(conf: ConfigRef, cmd: Command) =
  ## sets cmd, backend so subsequent flags can query it (e.g. so --gc:arc can be ignored for backendJs)
  # Note that `--backend` can override the backend, so the logic here must remain reversible.
  conf.cmd = cmd
  case cmd
  of cmdCompileToC, cmdCrun, cmdTcc: conf.backend = backendC
  of cmdCompileToCpp: conf.backend = backendCpp
  of cmdCompileToOC: conf.backend = backendObjc
  of cmdCompileToJS: conf.backend = backendJs
  else: discard

proc setCommandEarly*(conf: ConfigRef, command: string) =
  conf.command = command
  setCmd(conf, command.parseCommand)
  # command early customizations
  # must be handled here to honor subsequent `--hint:x:on|off`
  case conf.cmd
  of cmdRst2html, cmdRst2tex: # xxx see whether to add others: cmdGendepend, etc.
    conf.foreignPackageNotes = NotesVerbosity.base + {rintSuccessX}
  else:
    conf.foreignPackageNotes = NotesVerbosity.foreign

proc specialDefine(conf: ConfigRef, key: string; pass: TCmdLinePass) =
  # Keep this syncronized with the default config/nim.cfg!
  if cmpIgnoreStyle(key, "nimQuirky") == 0:
    conf.exc = excQuirky
  elif cmpIgnoreStyle(key, "release") == 0 or cmpIgnoreStyle(key, "danger") == 0:
    if pass in {passCmd1, passPP}:
      conf.excl {optStackTrace, optLineTrace, optLineDir, optOptimizeSize}
      conf.excl {optExcessiveStackTrace, optCDebug}
      conf.incl optOptimizeSpeed
  if cmpIgnoreStyle(key, "danger") == 0 or cmpIgnoreStyle(key, "quick") == 0:
    if pass in {passCmd1, passPP}:
      conf.excl {optObjCheck, optFieldCheck, optRangeCheck, optBoundsCheck,
        optOverflowCheck, optAssert, optStackTrace, optLineTrace, optLineDir}
      conf.excl {optCDebug}

proc processSwitch*(switch, arg: string, pass: TCmdLinePass, info: TLineInfo;
                    conf: ConfigRef) =
  var
    key, val: string
  proc invalidSwitchValue(
    wanted: seq[string], cmdlineError: string = ""): ExternalReport =

    ExternalReport(
      kind: rextInvalidValue,
      cmdlineProvided: arg,
      cmdlineAllowed: wanted,
      cmdlineError: cmdlineError,
      cmdlineSwitch: switch)

  case switch.normalize
  of "eval":
    expectArg(conf, switch, arg, pass, info)
    conf.projectIsCmd = true
    conf.cmdInput = arg # can be empty (a nim file with empty content is valid too)
    if conf.cmd == cmdNone:
      conf.command = "e"
      conf.setCmd cmdNimscript # better than `cmdCrun` as a default
      conf.implicitCmd = true
  of "path", "p":
    expectArg(conf, switch, arg, pass, info)
    for path in nimbleSubs(conf, arg):
      addPath(conf, if pass == passPP: processCfgPath(conf, path, info, switch)
                    else: processPath(conf, path, info, switch), info)
  of "nimblepath", "babelpath":
    if switch.normalize == "babelpath": deprecatedAlias(switch, "nimblepath")
    if pass in {passCmd2, passPP} and optNoNimblePath notin conf.globalOptions:
      expectArg(conf, switch, arg, pass, info)
      var path = processPath(conf, arg, info, switch, notRelativeToProj=true)
      let nimbleDir = AbsoluteDir getEnv("NIMBLE_DIR")
      if not nimbleDir.isEmpty and pass == passPP:
        path = nimbleDir / RelativeDir"pkgs"
      nimblePath(conf, path, info)
  of "nonimblepath", "nobabelpath":
    if switch.normalize == "nobabelpath": deprecatedAlias(switch, "nonimblepath")
    expectNoArg(conf, switch, arg, pass, info)
    disableNimblePath(conf)
  of "clearnimblepath":
    expectNoArg(conf, switch, arg, pass, info)
    clearNimblePath(conf)
  of "excludepath":
    expectArg(conf, switch, arg, pass, info)
    let path = processPath(conf, arg, info, switch)
    conf.searchPaths = conf.searchPaths.filterIt(it != path)
    conf.lazyPaths = conf.lazyPaths.filterIt(it != path)
  of "nimcache":
    expectArg(conf, switch, arg, pass, info)
    var arg = arg
    # refs bug #18674, otherwise `--os:windows` messes up with `--nimcache` set
    # in config nims files, e.g. via: `import os; switch("nimcache", "/tmp/somedir")`
    if conf.target.targetOS == osWindows and DirSep == '/': arg = arg.replace('\\', '/')
    conf.nimcacheDir = processPath(conf, arg, info, switch, notRelativeToProj=true)
  of "out", "o":
    expectArg(conf, switch, arg, pass, info)
    let f = splitFile(processPath(conf, arg, info, switch, notRelativeToProj=true).string)
    conf.outFile = RelativeFile f.name & f.ext
    conf.outDir = toAbsoluteDir f.dir
  of "outdir":
    expectArg(conf, switch, arg, pass, info)
    conf.outDir = processPath(conf, arg, info, switch, notRelativeToProj=true)
  of "usenimcache":
    processOnOffSwitchG(conf, {optUseNimcache}, arg, pass, info, switch)
  of "docseesrcurl":
    expectArg(conf, switch, arg, pass, info)
    conf.docSeeSrcUrl = arg
  of "docroot":
    conf.docRoot = if arg.len == 0: docRootDefault else: arg
  of "backend", "b":
    let backend = parseEnum(arg.normalize, TBackend.default)
    if backend == TBackend.default:
      conf.localReport(
        info, invalidSwitchValue @["c", "cpp", "js", "objc"])

    conf.backend = backend
  of "doccmd": conf.docCmd = arg
  of "define", "d":
    expectArg(conf, switch, arg, pass, info)
    if {':', '='} in arg:
      splitSwitch(conf, arg, key, val, pass, info)
      specialDefine(conf, key, pass)
      defineSymbol(conf, key, val)
    else:
      specialDefine(conf, arg, pass)
      defineSymbol(conf, arg)
  of "undef", "u":
    expectArg(conf, switch, arg, pass, info)
    undefSymbol(conf, arg)
  of "compile":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}: processCompile(conf, arg)
  of "link":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}:
      addExternalFileToLink(conf, AbsoluteFile arg)
  of "debuginfo":
    processOnOffSwitchG(conf, {optCDebug}, arg, pass, info, switch)
  of "embedsrc":
    processOnOffSwitchG(conf, {optEmbedOrigSrc}, arg, pass, info, switch)
  of "compileonly", "c":
    processOnOffSwitchG(conf, {optCompileOnly}, arg, pass, info, switch)
  of "nolinking":
    processOnOffSwitchG(conf, {optNoLinking}, arg, pass, info, switch)
  of "nomain":
    processOnOffSwitchG(conf, {optNoMain}, arg, pass, info, switch)
  of "forcebuild", "f":
    processOnOffSwitchG(conf, {optForceFullMake}, arg, pass, info, switch)
  of "project":
    processOnOffSwitchG(
      conf, {optWholeProject, optGenIndex}, arg, pass, info, switch)
  of "gc":
    if conf.backend == backendJs: return # for: bug #16033
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}:
      case arg.normalize
      of "boehm":
        conf.selectedGC = gcBoehm
        defineSymbol(conf, "boehmgc")
        conf.incl optTlsEmulation # Boehm GC doesn't scan the real TLS
      of "refc":
        conf.selectedGC = gcRefc
      of "markandsweep":
        conf.selectedGC = gcMarkAndSweep
        defineSymbol(conf, "gcmarkandsweep")
      of "destructors", "arc":
        conf.selectedGC = gcArc
        defineSymbol(conf, "gcdestructors")
        defineSymbol(conf, "gcarc")
        conf.incl optSeqDestructors
        conf.incl optTinyRtti
        if pass in {passCmd2, passPP}:
          defineSymbol(conf, "nimSeqsV2")
          defineSymbol(conf, "nimV2")
        if conf.exc == excNone and conf.backend != backendCpp:
          conf.exc = excGoto
      of "orc":
        conf.selectedGC = gcOrc
        defineSymbol(conf, "gcdestructors")
        defineSymbol(conf, "gcorc")
        conf.incl optSeqDestructors
        conf.incl optTinyRtti
        if pass in {passCmd2, passPP}:
          defineSymbol(conf, "nimSeqsV2")
          defineSymbol(conf, "nimV2")
        if conf.exc == excNone and conf.backend != backendCpp:
          conf.exc = excGoto
      of "hooks":
        conf.selectedGC = gcHooks
        defineSymbol(conf, "gchooks")
        conf.incl optSeqDestructors
        processOnOffSwitchG(conf, {optSeqDestructors}, arg, pass, info, switch)
        if pass in {passCmd2, passPP}:
          defineSymbol(conf, "nimSeqsV2")
      of "go":
        conf.selectedGC = gcGo
        defineSymbol(conf, "gogc")
      of "none":
        conf.selectedGC = gcNone
        defineSymbol(conf, "nogc")
      of "stack", "regions":
        conf.selectedGC = gcRegions
        defineSymbol(conf, "gcregions")
      of "v2": warningOptionNoop(arg)
      else:
        conf.localReport(
          info, invalidSwitchValue gcNames)

  of "warnings", "w":
    if processOnOffSwitchOrList(conf, {optWarns}, arg, pass, info, switch):
      listWarnings(conf)
  of "warning": processSpecificNote(arg, wWarning, pass, info, switch, conf)
  of "hint": processSpecificNote(arg, wHint, pass, info, switch, conf)
  of "warningaserror":
    processSpecificNote(arg, wWarningAsError, pass, info, switch, conf)
  of "hintaserror":
    processSpecificNote(arg, wHintAsError, pass, info, switch, conf)
  of "hints":
    if processOnOffSwitchOrList(conf, {optHints}, arg, pass, info, switch):
      listHints(conf)
  of "threadanalysis":
    if conf.backend == backendJs: discard
    else:
      processOnOffSwitchG(conf, {optThreadAnalysis}, arg, pass, info, switch)
  of "stacktrace":
    processOnOffSwitch(conf, {optStackTrace}, arg, pass, info, switch)
  of "stacktracemsgs":
    processOnOffSwitch(conf, {optStackTraceMsgs}, arg, pass, info, switch)
  of "excessivestacktrace":
    processOnOffSwitchG(conf, {optExcessiveStackTrace}, arg, pass, info, switch)
  of "linetrace":
    processOnOffSwitch(conf, {optLineTrace}, arg, pass, info, switch)
  of "debugger":
    case arg.normalize
    of "on", "native", "gdb":
      conf.incl optCDebug
      conf.incl optLineDir
      #defineSymbol(conf.symbols, "nimTypeNames") # type names are used in gdb pretty printing
    of "off":
      conf.excl optCDebug
    else:
      conf.localReport(
        info, invalidSwitchValue @["native", "gdb", "on", "off"])

  of "g": # alias for --debugger:native
    conf.incl optCDebug
    conf.incl optLineDir
    #defineSymbol(conf.symbols, "nimTypeNames") # type names are used in gdb pretty printing
  of "profiler":
    processOnOffSwitch(conf, {optProfiler}, arg, pass, info, switch)
    if optProfiler in conf.options:
      defineSymbol(conf, "profiler")
    else:
      undefSymbol(conf, "profiler")
  of "memtracker":
    processOnOffSwitch(conf, {optMemTracker}, arg, pass, info, switch)
    if optMemTracker in conf.options:
      defineSymbol(conf, "memtracker")
    else:
      undefSymbol(conf, "memtracker")
  of "hotcodereloading":
    processOnOffSwitchG(conf, {optHotCodeReloading}, arg, pass, info, switch)
    if conf.hcrOn:
      defineSymbol(conf, "hotcodereloading")
      defineSymbol(conf, "useNimRtl")
      # hardcoded linking with dynamic runtime for MSVC for smaller binaries
      # should do the same for all compilers (wherever applicable)
      if isVSCompatible(conf):
        extccomp.addCompileOptionCmd(conf, "/MD")
    else:
      undefSymbol(conf, "hotcodereloading")
      undefSymbol(conf, "useNimRtl")
  of "checks", "x":
    processOnOffSwitch(conf, ChecksOptions, arg, pass, info, switch)
  of "floatchecks":
    processOnOffSwitch(
      conf, {optNaNCheck, optInfCheck}, arg, pass, info, switch)
  of "infchecks":
    processOnOffSwitch(conf, {optInfCheck}, arg, pass, info, switch)
  of "nanchecks":
    processOnOffSwitch(conf, {optNaNCheck}, arg, pass, info, switch)
  of "objchecks":
    processOnOffSwitch(conf, {optObjCheck}, arg, pass, info, switch)
  of "fieldchecks":
    processOnOffSwitch(conf, {optFieldCheck}, arg, pass, info, switch)
  of "rangechecks":
    processOnOffSwitch(conf, {optRangeCheck}, arg, pass, info, switch)
  of "boundchecks":
    processOnOffSwitch(conf, {optBoundsCheck}, arg, pass, info, switch)
  of "refchecks":
    processOnOffSwitch(conf, {optRefCheck}, arg, pass, info, switch)
  of "overflowchecks":
    processOnOffSwitch(conf, {optOverflowCheck}, arg, pass, info, switch)
  of "staticboundchecks":
    processOnOffSwitch(conf, {optStaticBoundsCheck}, arg, pass, info, switch)
  of "stylechecks":
    processOnOffSwitch(conf, {optStyleCheck}, arg, pass, info, switch)
  of "linedir":
    processOnOffSwitch(conf, {optLineDir}, arg, pass, info, switch)
  of "assertions", "a":
    processOnOffSwitch(conf, {optAssert}, arg, pass, info, switch)
  of "threads":
    if conf.backend == backendJs:
      discard
    else:
      processOnOffSwitchG(conf, {optThreads}, arg, pass, info, switch)
    #if optThreads in conf.globalOptions: conf.setNote(warnGcUnsafe)
  of "tlsemulation":
    processOnOffSwitchG(conf, {optTlsEmulation}, arg, pass, info, switch)
  of "implicitstatic":
    processOnOffSwitch(conf, {optImplicitStatic}, arg, pass, info, switch)
  of "patterns", "trmacros":
    if switch.normalize == "patterns":
      deprecatedAlias(switch, "trmacros")
    processOnOffSwitch(conf, {optTrMacros}, arg, pass, info, switch)
  of "opt":
    expectArg(conf, switch, arg, pass, info)
    case arg.normalize
    of "speed":
      incl(conf, optOptimizeSpeed)
      excl(conf, optOptimizeSize)
    of "size":
      excl(conf, optOptimizeSpeed)
      incl(conf, optOptimizeSize)
    of "none":
      excl(conf, optOptimizeSpeed)
      excl(conf, optOptimizeSize)
    else:
      conf.localReport(info, ExternalReport(
        kind: rextInvalidValue,
        cmdlineProvided: arg,
        cmdlineAllowed: @["speed", "size", "none"],
        cmdlineSwitch: switch))
  of "app":
    expectArg(conf, switch, arg, pass, info)
    case arg.normalize
    of "gui":
      conf.incl optGenGuiApp
      defineSymbol(conf, "executable")
      defineSymbol(conf, "guiapp")
    of "console":
      conf.excl optGenGuiApp
      defineSymbol(conf, "executable")
      defineSymbol(conf, "consoleapp")
    of "lib":
      incl(conf, optGenDynLib)
      excl(conf, optGenGuiApp)
      defineSymbol(conf, "library")
      defineSymbol(conf, "dll")
    of "staticlib":
      incl(conf, optGenStaticLib)
      excl(conf, optGenGuiApp)
      defineSymbol(conf, "library")
      defineSymbol(conf, "staticlib")
    else:
      conf.localReport(
        info, invalidSwitchValue  @["gui", "console", "lib", "staticlib"])
  of "passc", "t":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}: extccomp.addCompileOptionCmd(conf, arg)
  of "passl", "l":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}: extccomp.addLinkOptionCmd(conf, arg)
  of "cincludes":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}:
      conf.cIncludesAdd processPath(conf, arg, info, switch)
  of "clibdir":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}:
      conf.cLibsAdd processPath(conf, arg, info, switch)
  of "clib":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}:
      conf.cLinkedLibsAdd processPath(conf, arg, info, switch).string
  of "header":
    if conf != nil: conf.headerFile = arg
    incl(conf, optGenIndex)
  of "index":
    processOnOffSwitchG(conf, {optGenIndex}, arg, pass, info, switch)
  of "import":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}:
      conf.implicitImportsAdd findModule(
        conf, arg, toFullPath(conf, info)).string
  of "include":
    expectArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}:
      conf.implicitIncludesAdd findModule(
        conf, arg, toFullPath(conf, info)).string

  of "listcmd":
    processOnOffSwitchG(conf, {optListCmd}, arg, pass, info, switch)
  of "asm":
    processOnOffSwitchG(conf, {optProduceAsm}, arg, pass, info, switch)
  of "genmapping":
    processOnOffSwitchG(conf, {optGenMapping}, arg, pass, info, switch)
  of "os":
    expectArg(conf, switch, arg, pass, info)
    let theOS = platform.nameToOS(arg)
    if theOS == osNone:
      conf.localReport(
        info, invalidSwitchValue platform.listOSnames())
    else:
      conf.target = conf.target.withIt do:
        setTarget(it, theOS, conf.target.targetCPU)
  of "cpu":
    expectArg(conf, switch, arg, pass, info)
    let cpu = platform.nameToCPU(arg)
    if cpu == cpuNone:
      conf.localReport(
        info, invalidSwitchValue platform.listCPUnames())

    else:
      conf.target = conf.target.withIt do:
        setTarget(it, conf.target.targetOS, cpu)

  of "run", "r":
    processOnOffSwitchG(conf, {optRun}, arg, pass, info, switch)
  of "maxloopiterationsvm":
    expectArg(conf, switch, arg, pass, info)
    conf.maxLoopIterationsVM = parseInt(arg)
  of "errormax":
    expectArg(conf, switch, arg, pass, info)
    # Note: `nim check` (etc) can overwrite this.
    # `0` is meaningless, give it a useful meaning as in clang's -ferror-limit
    # If user doesn't set this flag and the code doesn't either, it'd
    # have the same effect as errorMax = 1
    let ret = parseInt(arg)
    conf.errorMax = if ret == 0: high(int) else: ret
  of "verbosity":
    expectArg(conf, switch, arg, pass, info)
    let verbosity = parseInt(arg)
    if verbosity notin {0 .. 3}:
      conf.localReport(
        info, invalidSwitchValue @["0", "1", "2", "3"])
    conf.verbosity = verbosity
    var verb = NotesVerbosity.main[conf.verbosity]
    ## We override the default `verb` by explicitly modified (set/unset) notes.
    conf.notes = (conf.modifiedyNotes * conf.notes + verb) -
      (conf.modifiedyNotes * verb - conf.notes)
    conf.mainPackageNotes = conf.notes
  of "parallelbuild":
    expectArg(conf, switch, arg, pass, info)
    conf.numberOfProcessors = parseInt(arg)
  of "version", "v":
    expectNoArg(conf, switch, arg, pass, info)
    writeVersionInfo(conf, pass)
  of "advanced":
    expectNoArg(conf, switch, arg, pass, info)
    writeAdvancedUsage(conf, pass)
  of "fullhelp":
    expectNoArg(conf, switch, arg, pass, info)
    writeFullhelp(conf, pass)
  of "help", "h":
    expectNoArg(conf, switch, arg, pass, info)
    helpOnError(conf, pass)
  of "symbolfiles", "incremental", "ic":
    if switch.normalize == "symbolfiles": deprecatedAlias(switch, "incremental")
      # xxx maybe also ic, since not in help?
    if pass in {passCmd2, passPP}:
      case arg.normalize
      of "on": conf.symbolFiles = v2Sf
      of "off": conf.symbolFiles = disabledSf
      of "writeonly": conf.symbolFiles = writeOnlySf
      of "readonly": conf.symbolFiles = readOnlySf
      of "v2": conf.symbolFiles = v2Sf
      of "stress": conf.symbolFiles = stressTest
      else:
        conf.localReport(
          info, invalidSwitchValue @["on", "off", "writeonly", "readonly", "v2", "stress"])
    setUseIc(conf.symbolFiles != disabledSf)
  of "skipcfg":
    processOnOffSwitchG(conf, {optSkipSystemConfigFile}, arg, pass, info, switch)
  of "skipprojcfg":
    processOnOffSwitchG(conf, {optSkipProjConfigFile}, arg, pass, info, switch)
  of "skipusercfg":
    processOnOffSwitchG(conf, {optSkipUserConfigFile}, arg, pass, info, switch)
  of "skipparentcfg":
    processOnOffSwitchG(conf, {optSkipParentConfigFiles}, arg, pass, info, switch)
  of "genscript", "gendeps":
    if switch.normalize == "gendeps": deprecatedAlias(switch, "genscript")
    processOnOffSwitchG(conf, {optGenScript}, arg, pass, info, switch)
    processOnOffSwitchG(conf, {optCompileOnly}, arg, pass, info, switch)
  of "colors": processOnOffSwitchG(conf, {optUseColors}, arg, pass, info, switch)
  of "lib":
    expectArg(conf, switch, arg, pass, info)
    conf.libpath = processPath(conf, arg, info, switch, notRelativeToProj=true)
  of "putenv":
    expectArg(conf, switch, arg, pass, info)
    splitSwitch(conf, arg, key, val, pass, info)
    os.putEnv(key, val)
  of "cc":
    expectArg(conf, switch, arg, pass, info)
    setCC(conf, arg, info)
  of "track":
    expectArg(conf, switch, arg, pass, info)
    track(conf, arg, info)
  of "trackdirty":
    expectArg(conf, switch, arg, pass, info)
    trackDirty(conf, arg, info)
  of "suggest":
    expectNoArg(conf, switch, arg, pass, info)
    conf.ideCmd = ideSug
  of "def":
    expectArg(conf, switch, arg, pass, info)
    trackIde(conf, ideDef, arg, info)
  of "context":
    expectNoArg(conf, switch, arg, pass, info)
    conf.ideCmd = ideCon
  of "usages":
    expectArg(conf, switch, arg, pass, info)
    trackIde(conf, ideUse, arg, info)
  of "defusages":
    expectArg(conf, switch, arg, pass, info)
    trackIde(conf, ideDus, arg, info)
  of "stdout":
    processOnOffSwitchG(conf, {optStdout}, arg, pass, info, switch)
  of "filenames":
    case arg.normalize
    of "abs": conf.filenameOption = foAbs
    of "canonical": conf.filenameOption = foCanonical
    of "legacyrelproj": conf.filenameOption = foLegacyRelProj
    else:
      conf.localReport(info, invalidSwitchValue @["abs", "canonical", "legacyRelProj"])

  of "msgformat":
    case arg.normalize:
      of "text":
        conf.setReportHook cli_reporter.reportHook

      of "sexp":
        conf.setReportHook sexp_reporter.reportHook

      else:
        conf.localReport(info, invalidSwitchValue @["text", "sexp"])

  of "processing":
    incl(conf, cnCurrent, rsemProcessing)
    incl(conf, cnMainPackage, rsemProcessing)
    case arg.normalize
    of "dots": conf.hintProcessingDots = true
    of "filenames": conf.hintProcessingDots = false
    of "off":
      excl(conf, cnCurrent, rsemProcessing)
      excl(conf, cnMainPackage, rsemProcessing)
    else:
      conf.localReport(info, invalidSwitchValue @["dots", "filenames", "off"])
  of "unitsep":
    conf.unitSep = if switchOn(arg): "\31" else: ""
  of "listfullpaths":
    # xxx in future work, use `warningDeprecated`
    conf.filenameOption = if switchOn(arg): foAbs else: foCanonical
  of "spellsuggest":
    if arg.len == 0: conf.spellSuggestMax = spellSuggestSecretSauce
    elif arg == "auto": conf.spellSuggestMax = spellSuggestSecretSauce
    else: conf.spellSuggestMax = parseInt(arg)
  of "declaredlocs":
    processOnOffSwitchG(conf, {optDeclaredLocs}, arg, pass, info, switch)
  of "dynliboverride":
    dynlibOverride(conf, switch, arg, pass, info)
  of "dynliboverrideall":
    processOnOffSwitchG(conf, {optDynlibOverrideAll}, arg, pass, info, switch)
  of "experimental":
    if arg.len == 0:
      conf.incl oldExperimentalFeatures
    else:
      try:
        conf.incl parseEnum[Feature](arg)
      except ValueError:
        conf.localReport(
          info, invalidSwitchValue(
            getEnumNames({low(Feature) .. high(Feature)}),
            "unknown experimental feature"))
  of "legacy":
    try:
      conf.incl parseEnum[LegacyFeature](arg)
    except ValueError:
      conf.localReport(
        info, invalidSwitchValue(
          getEnumNames({low(LegacyFeature) .. high(LegacyFeature)}),
          "unknown obsolete feature"))
  of "nocppexceptions":
    expectNoArg(conf, switch, arg, pass, info)
    conf.exc = low(ExceptionSystem)
    defineSymbol(conf, "noCppExceptions")
  of "exceptions":
    case arg.normalize
    of "cpp": conf.exc = excCpp
    of "setjmp": conf.exc = excSetjmp
    of "quirky": conf.exc = excQuirky
    of "goto": conf.exc = excGoto
    else:
      conf.localReport(info, invalidSwitchValue @["cpp", "setjmp", "quirky", "goto"])
  of "cppdefine":
    expectArg(conf, switch, arg, pass, info)
    if conf != nil:
      conf.cppDefine(arg)
  of "newruntime":
    expectNoArg(conf, switch, arg, pass, info)
    if pass in {passCmd2, passPP}:
      doAssert(conf != nil)
      incl(conf, destructor)
      incl(conf, optTinyRtti)
      incl(conf, optOwnedRefs)
      incl(conf, optSeqDestructors)
      defineSymbol(conf, "nimV2")
      conf.selectedGC = gcHooks
      defineSymbol(conf, "gchooks")
      defineSymbol(conf, "nimSeqsV2")
      defineSymbol(conf, "nimOwnedEnabled")
  of "seqsv2":
    processOnOffSwitchG(conf, {optSeqDestructors}, arg, pass, info, switch)
    if pass in {passCmd2, passPP}:
      defineSymbol(conf, "nimSeqsV2")
  of "stylecheck":
    case arg.normalize
    of "off":
      conf.globalOptions = conf.globalOptions - {optStyleHint, optStyleError}
    of "hint":
      conf.globalOptions = conf.globalOptions + {optStyleHint} - {optStyleError}
    of "error":
      conf.globalOptions = conf.globalOptions + {optStyleError}
    of "usages":
      conf.incl optStyleUsages
    else:
      conf.localReport(
        info, invalidSwitchValue @["off", "hint", "error", "usages"])
  of "showallmismatches":
    processOnOffSwitchG(conf, {optShowAllMismatches}, arg, pass, info, switch)
  of "cppcompiletonamespace":
    if arg.len > 0:
      conf.cppCustomNamespace = arg
    else:
      conf.cppCustomNamespace = "Nim"
    defineSymbol(conf, "cppCompileToNamespace", conf.cppCustomNamespace)
  of "docinternal":
    processOnOffSwitchG(conf, {optDocInternal}, arg, pass, info, switch)
  of "multimethods":
    processOnOffSwitchG(conf, {optMultiMethods}, arg, pass, info, switch)
  of "expandmacro":
    expectArg(conf, switch, arg, pass, info)
    conf.macrosToExpand[arg] = "T"
  of "expandarc":
    expectArg(conf, switch, arg, pass, info)
    conf.arcToExpand[arg] = "T"
  of "useversion":
    expectArg(conf, switch, arg, pass, info)
    case arg
    of "1.0":
      defineSymbol(conf, "NimMajor", "1")
      defineSymbol(conf, "NimMinor", "0")
      # old behaviors go here:
      defineSymbol(conf, "nimOldRelativePathBehavior")
      undefSymbol(conf, "nimDoesntTrackDefects")
      ast.eqTypeFlags.excl {tfGcSafe, tfNoSideEffect}
      conf.incl optNimV1Emulation
    of "1.2":
      defineSymbol(conf, "NimMajor", "1")
      defineSymbol(conf, "NimMinor", "2")
      conf.incl optNimV12Emulation
    else:
      conf.localReport(info, invalidSwitchValue(
        @["1.0", "1.2"],
        "unknown Nim version; currently supported values are: `1.0`, `1.2`"))
    # always be compatible with 1.x.100:
    defineSymbol(conf, "NimPatch", "100")
  of "benchmarkvm":
    processOnOffSwitchG(conf, {optBenchmarkVM}, arg, pass, info, switch)
  of "profilevm":
    processOnOffSwitchG(conf, {optProfileVM}, arg, pass, info, switch)
  of "sinkinference":
    processOnOffSwitch(conf, {optSinkInference}, arg, pass, info, switch)
  of "cursorinference":
    # undocumented, for debugging purposes only:
    processOnOffSwitch(conf, {optCursorInference}, arg, pass, info, switch)
  of "panics":
    processOnOffSwitchG(conf, {optPanics}, arg, pass, info, switch)
    if optPanics in conf.globalOptions:
      defineSymbol(conf, "nimPanics")
  of "sourcemap": # xxx document in --fullhelp
    conf.incl optSourcemap
    conf.incl optLineDir
  of "deepcopy":
    processOnOffSwitchG(conf, {optEnableDeepCopy}, arg, pass, info, switch)
  of "": # comes from "-" in for example: `nim c -r -` (gets stripped from -)
    handleStdinInput(conf)
  of "nilseqs", "nilchecks", "mainmodule", "m", "symbol", "taintmode",
     "cs", "deadcodeelim":
    warningOptionNoop(switch)

  else:
    if strutils.find(switch, '.') >= 0: options.setConfigVar(conf, switch, arg)
    else: invalidCmdLineOption(conf, pass, switch, info)

proc processCommand*(switch: string, pass: TCmdLinePass; config: ConfigRef) =
  var cmd, arg: string
  splitSwitch(config, switch, cmd, arg, pass, gCmdLineInfo)
  processSwitch(cmd, arg, pass, gCmdLineInfo, config)

proc processSwitch*(pass: TCmdLinePass; p: OptParser; config: ConfigRef) =
  # hint[X]:off is parsed as (p.key = "hint[X]", p.val = "off")
  # we transform it to (key = hint, val = [X]:off)
  var bracketLe = strutils.find(p.key, '[')
  if bracketLe >= 0:
    var key = substr(p.key, 0, bracketLe - 1)
    var val = substr(p.key, bracketLe) & ':' & p.val
    processSwitch(key, val, pass, gCmdLineInfo, config)
  else:
    processSwitch(p.key, p.val, pass, gCmdLineInfo, config)

proc processArgument*(pass: TCmdLinePass; p: OptParser;
                      argsCount: var int; config: ConfigRef): bool =
  if argsCount == 0 and config.implicitCmd:
    argsCount.inc
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
      if config.projectName.len == 0:
        config.projectName = unixToNativePath(p.key)
      config.arguments = cmdLineRest(p)
      result = true
  inc argsCount

proc addCmdPrefix*(result: var string, kind: CmdLineKind) =
  # consider moving this to std/parseopt
  case kind
  of cmdLongOption: result.add "--"
  of cmdShortOption: result.add "-"
  of cmdArgument, cmdEnd: discard

proc processCmdLine*(pass: TCmdLinePass, cmd: string; config: ConfigRef) =
  ## Process input command-line parameters into `config` settings. Input is
  ## a joined list of command-line arguments with multiple options and/or
  ## configurations.
  var p = parseopt.initOptParser(cmd)
  var argsCount = 0

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
          processSwitch(pass, p, config)

      of cmdArgument:
        config.commandLine.add " "
        config.commandLine.add p.key.quoteShell
        if processArgument(pass, p, argsCount, config):
          break

  if pass == passCmd2:
    if {optRun, optWasNimscript} * config.globalOptions == {} and
        config.arguments.len > 0 and config.cmd notin {
          cmdTcc, cmdNimscript, cmdCrun}:
      localReport(config, ExternalReport(kind: rextExpectedRunOptForArgs))
