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
    msgs,
    cli_reporter,
    sexp_reporter
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


from compiler/ast/ast import setUseIc

# TODO: remove remaining legacy reports stuff.
from compiler/ast/report_enums import ReportKind,
  ReportKinds,
  repHintKinds,
  repHintGroups,
  repWarningKinds,
  repWarningGroups

bootSwitch(usedTinyC, hasTinyCBackend, "-d:tinyc")

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

  TCmdLinePass* = enum
    passCmd1,                 # first pass over the command line
    passCmd2,                 # second pass over the command line
    passPP                    # preprocessor called processCommand()

  CliLogKind* = enum
    cliLogErrInvalidCommand
    cliLogErrExpectedNoCmdArguments
    cliLogErrUnexpectedRunOpt
    cliLogErrRunCmdFailed
    cliLogErrGenDependFailed
    cliLogErrInvalidCommandLineOption
    cliLogErrSwitchExpectedOnOrOff
    cliLogErrSwitchExpectedOnOffOrList
    cliLogErrSwitchExpectedAllOrOff
    cliLogErrSwitchExpectedArg
    cliLogErrSwitchExpectedNoArg
    cliLogErrSwitchInvalidValue
    cliLogErrUnknownCCompiler
    cliLogErrUnknownExperimentalFeature
    cliLogErrInvalidPath
    cliLogErrNoCmdLineParamsProvided
    cliLogErrInvalidHint
    cliLogErrInvalidWarning
    cliLogWarnDeprecatedGcStatsHintPresent
    cliLogWarnDeprecatedAlias
    cliLogWarnSwitchValDeprecatedNoop
    cliLogWarnSwitchDeprecatedNoop

  CliLogMsg* = object
    # TODO: add support for instantiation location
    # TODO: rename to something with "event"/"evt" instead of "msg"
    srcCodeOrigin*: InstantiationInfo
    case kind*: CliLogKind
      of cliLogErrInvalidCommand,
          cliLogErrExpectedNoCmdArguments,
          cliLogErrUnexpectedRunOpt:
        cmd*: string
        unexpectedArgs*: string
      of cliLogErrRunCmdFailed,
          cliLogErrGenDependFailed:
        shellCmd*: string
        exitCode*: int
      of cliLogErrInvalidCommandLineOption:
        wrongOpt*: string
      of cliLogErrSwitchExpectedOnOrOff,
          cliLogErrSwitchExpectedOnOffOrList,
          cliLogErrSwitchExpectedNoArg,
          cliLogErrSwitchExpectedAllOrOff,
          cliLogWarnSwitchDeprecatedNoop,
          cliLogWarnSwitchValDeprecatedNoop,
          cliLogErrInvalidPath:
        switch*: string
        argVal*: string
      of cliLogErrSwitchExpectedArg:
        switchMissingArg*: string
      of cliLogErrSwitchInvalidValue,
          cliLogErrUnknownCCompiler,
          cliLogErrUnknownExperimentalFeature:
        forSwitch*: string
        givenVal*: string
        allowed*: seq[string]
      of cliLogErrInvalidHint,
          cliLogErrInvalidWarning:
        invalidErrHintWarn*: string
      of cliLogErrNoCmdLineParamsProvided:
        discard
      of cliLogWarnDeprecatedGcStatsHintPresent:
        discard
      of cliLogWarnDeprecatedAlias:
        oldName*: string
        newName*: string

const
  cliLogAllKinds = {low(CliLogKind) .. high(CliLogKind)}
  cliLogErrors   = {cliLogErrInvalidCommand .. cliLogErrInvalidWarning}
  cliLogWarnings = cliLogAllKinds - cliLogErrors

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

proc writeLog(conf: ConfigRef, msg: string, evt: CliLogMsg) {.inline.} =
  conf.writeLog(msg, evt.srcCodeOrigin)

proc getNimSourceData(): tuple[hash, date: string] {.compileTime.} =
  ## Retrieve metadata about the compiler source code.
  const
    # These are defined by koch
    nimSourceHash {.strdefine.} = ""
    nimSourceDate {.strdefine.} = ""
  result = (nimSourceHash, nimSourceDate)

proc getCliData(conf: ConfigRef): CliData =
  ## Get CLI data from current configuration and nim compiler configuration
  ## (source code/date defines, boot switches)
  let (sourceHash, sourceDate) = getNimSourceData()

  CliData(
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

proc genFeatureDesc[T: enum](t: typedesc[T]): string {.compileTime.} =
  result = ""
  for f in T:
    if result.len > 0: result.add "|"
    result.add $f

const
  HelpMessage = "Nimskull Compiler Version $1 [$2: $3]\n"
  CommitMessage = "Source hash: $1\n" &
                  "Source date: $2\n"
  Usage = slurp"../doc/basicopt.txt".replace(" //", "   ")
  AdvancedUsage = slurp"../doc/advopt.txt".replace(" //", "   ") %
    genFeatureDesc(Feature)

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
  let data = conf.getCliData()
  cliMsgLede(data) & Usage

proc writeHelp(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    conf.showMsg helpOnErrorMsg(conf)
    msgQuit(0)

proc writeAdvancedUsage(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    let data = conf.getCliData()
    conf.showMsg:
      cliMsgLede(data) & AdvancedUsage
    msgQuit(0)

proc writeFullhelp(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    let data = conf.getCliData()
    conf.showMsg:
      cliMsgLede(data) & Usage & AdvancedUsage
    msgQuit(0)

proc writeVersionInfo(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    let
      data = conf.getCliData()
      commitMsg =
        if data.sourceHash != "":
          "\n" & CommitMessage % [data.sourceHash, data.sourceDate]
        else:
          ""
    conf.showMsg:
      cliMsgLede(data) &
      commitMsg &
      "\nactive boot switches: " & data.boot.join(" ")
    msgQuit(0)

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
  "fromcmd", "path", "p", "nimblepath", "nonimblepath",
  "clearnimblepath", "excludepath", "nimcache", "out", "o",
  "outdir", "depfile", "usenimcache", "docseesrcurl", "docroot", "backend", "b",
  "doccmd", "define", "d", "undef", "u", "compile", "link", "debuginfo",
  "embedsrc", "compileonly", "c", "nolinking", "nomain", "forcebuild", "f",
  "project", "warnings", "w", "warning", "hint", "warningaserror",
  "hintaserror", "hints", "threadanalysis", "stacktrace",
  "stacktracemsgs", "excessivestacktrace", "linetrace",
  "debugger", "profiler", "memtracker", "checks",
  "floatchecks", "infchecks", "nanchecks", "objchecks", "fieldchecks",
  "rangechecks", "boundchecks", "overflowchecks",
  "staticboundchecks", "stylechecks", "linedir", "assertions", "threads",
  "tlsemulation", "implicitstatic", "trmacros", "opt", "app", "passc",
  "passl", "cincludes", "clibdir", "clib", "header", "index", "import",
  "include", "listcmd", "asm", "genmapping", "os", "cpu", "run",
  "maxloopiterationsvm", "errormax", "verbosity", "parallelbuild",
  "version", "advanced", "fullhelp", "help", "skipcfg",
  "skipprojcfg", "skipusercfg", "skipparentcfg", "genscript", "colors",
  "lib", "putenv", "cc", "stdout", "filenames", "processing",
  "unitsep", "listfullpaths", "spellsuggest", "declaredlocs",
  "dynliboverride", "dynliboverrideall", "experimental",
  "exceptions", "cppdefine", "seqsv2", "stylecheck", "showallmismatches",
  "docinternal", "multimethods", "expandmacro", "expandarc",
  "benchmarkvm", "profilevm", "sinkinference", "cursorinference", "panics",
  "sourcemap", "deepcopy", "cmdexitgcstats"
]

proc logGcStats*(conf: ConfigRef, stats: string, srcLoc = instLoc()) =
  ## log a 'debug' level message with the GC `stats`
  # TODO: document log levels, eventual introduction of `channels`,
  #       suppression, formatting, etc
  if optCmdExitGcStats in conf.globalOptions or conf.hasHint(rintGCStats):
    conf.writeLog(stats, srcLoc)

proc logExecStart*(conf: ConfigRef, cmd: string, srcLoc = instLoc()) =
  ## use when a command invocation begins a shell exec as part of its
  ## operations; not currently meant for shell execs initiated by input source
  ## code or scripts.
  # xxx: maybe allow configurable command action logging
  if conf.verbosity > compVerbosityDefault:
    conf.writeLog(cmd, srcLoc)

proc logError*(conf: ConfigRef, evt: CliLogMsg) =
  # TODO: consolidate log event rendering, between this and "reports", but with
  #       less of the reports baggage
  let msg =
    case evt.kind
    of cliLogErrInvalidCommand:
      "Invalid command - $1" % evt.cmd
    of cliLogErrUnexpectedRunOpt:
      "'$1 cannot handle --run" % evt.cmd
    of cliLogErrExpectedNoCmdArguments:
      "$1 command does not support additional arguments: '$2'" %
        [evt.cmd, evt.unexpectedArgs]
    of cliLogErrRunCmdFailed,
        cliLogErrGenDependFailed: # make a better message for gen depend
      "execution of an external program '$1' failed with exit code '$2'" %
        [evt.shellCmd, $evt.exitCode]
    of cliLogErrSwitchExpectedOnOrOff:
      "'on' or 'off' expected for $1, but '$2' found" %
        [evt.switch, evt.argVal]
    of cliLogErrSwitchExpectedOnOffOrList:
      "'on', 'off', or 'list' expected for $1, but '$2' found" %
        [evt.switch, evt.argVal]
    of cliLogErrSwitchExpectedNoArg:
      "$1 expects no arguments, but '$2' found" % [evt.switch, evt.argVal]
    of cliLogErrSwitchExpectedAllOrOff:
      "only 'all:off' is supported for $1, found $2" % [evt.switch, evt.argVal]
    of cliLogErrSwitchExpectedArg:
      "argument for command line option expected: '$1'" % evt.switch
    of cliLogErrSwitchInvalidValue:
      "Unexpected value for switch '$1'. Expected one of $2, but got '$3'" %
        [evt.forSwitch, evt.allowed.mapIt("'$1'" % it).join(", "), evt.givenVal]
    of cliLogErrUnknownCCompiler:
      "unknown C compiler: '$1'. Available options are: $2" %
        [evt.givenVal, evt.allowed.join(", ")]
    of cliLogErrUnknownExperimentalFeature:
      "unknown experiemental feature: '$1'. Available options are: $2" %
        [evt.givenVal, evt.allowed.join(", ")]
    of cliLogErrInvalidCommandLineOption:
      "Invalid command line option - " & evt.wrongOpt
    of cliLogErrInvalidPath:
      "invalid path (option '$#'): $#" % [evt.switch, evt.argVal]
    of cliLogErrInvalidHint:
      "Invalid hint - " & evt.invalidErrHintWarn
    of cliLogErrInvalidWarning:
      "Invalid warning - " & evt.invalidErrHintWarn
    of cliLogErrNoCmdLineParamsProvided:
      "no command-line parameters provided"
    of cliLogWarnings:
      unreachable($evt.kind)

  inc conf.errorCounter
  conf.writeLog(msg, evt)

proc logWarn(conf: ConfigRef, evt: CliLogMsg) =
  # TODO: see items under `logError`
  let msg =
    case evt.kind
    of cliLogWarnDeprecatedGcStatsHintPresent:
      "GCStats hint is deprecated use `--cmdexitgcstats` CLI flag"
    of cliLogWarnDeprecatedAlias:
      "'$#' is a deprecated alias for '$#'" % [evt.oldName, evt.newName]
    of cliLogWarnSwitchValDeprecatedNoop:
      "'$#' is deprecated for flag '$#', now a noop" % [evt.argVal, evt.switch]
    of cliLogWarnSwitchDeprecatedNoop:
      "'$#' is deprecated, now a noop" % evt.switch
    of cliLogErrors:
      unreachable($evt.kind)
    
  inc conf.warnCounter
  conf.writeLog(msg, evt)

proc invalidCmdLineOption(conf: ConfigRef, switch: string) =
  conf.logError(CliLogMsg(kind: cliLogErrInvalidCommandLineOption, wrongOpt: switch))

proc splitSwitch(conf: ConfigRef; switch: string, cmd, arg: var string) =
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
  else: invalidCmdLineOption(conf, switch)

template switchOn(conf: ConfigRef, s, arg: string): bool =
  # xxx use `switchOn` wherever appropriate
  case arg.normalize
  of "", "on": true
  of "off": false
  else:
    # TODO: capture the switch this is for so we have a better message
    conf.logError(CliLogMsg(kind: cliLogErrSwitchExpectedOnOrOff,
                            switch: s,
                            argVal: arg))
    false

proc processOnOffSwitch(conf: ConfigRef; op: TOptions, arg, switch: string) =
  case arg.normalize
  of "", "on": conf.incl op
  of "off": conf.excl op
  else:
    conf.logError(CliLogMsg(kind: cliLogErrSwitchExpectedOnOrOff,
                            switch: switch,
                            argVal: arg))

proc processOnOffSwitchOrList(conf: ConfigRef; op: TOptions, arg, switch: string): bool =
  result = false
  case arg.normalize
  of "on": conf.incl op
  of "off": conf.excl op
  of "list": result = true
  else:
    conf.logError(CliLogMsg(kind: cliLogErrSwitchExpectedOnOffOrList,
                            switch: switch,
                            argVal: arg))

proc processOnOffSwitchG(conf: ConfigRef; op: TGlobalOptions, arg, switch: string) =
  case arg.normalize
  of "", "on": conf.incl op
  of "off": conf.excl op
  else:
    conf.logError(CliLogMsg(kind: cliLogErrSwitchExpectedOnOrOff,
                            switch: switch,
                            argVal: arg))

proc expectArg(conf: ConfigRef; switch, arg: string) =
  if arg == "":
    conf.logError(CliLogMsg(kind: cliLogErrSwitchExpectedArg,
                            switchMissingArg: switch))

proc expectNoArg(conf: ConfigRef; switch, arg: string) =
  if arg != "":
    conf.logError(CliLogMsg(kind: cliLogErrSwitchExpectedNoArg,
                            switch: switch,
                            argVal: arg))

type
  ProcessNoteResultKind* = enum
    procNoteSuccess
    procNoteInvalidOption
    procNoteInvalidHint
    procNoteInvalidWarning
    procNoteExpectedOnOrOff
    procNoteOnlyAllOffSupported
  ProcessNoteResult* = object
    deprecatedGcStatsHintPresent*: bool
    case kind*: ProcessNoteResultKind
      of procNoteSuccess:
        discard
      of procNoteInvalidOption,
          procNoteExpectedOnOrOff,
          procNoteOnlyAllOffSupported:
        switch*: string
        argVal*: string # not used for `procNoteInvalidOption`
      of procNoteInvalidHint, procNoteInvalidWarning:
        invalidHintOrWarning*: string

proc processSpecificNote*(arg: string, state: TSpecialWord, pass: TCmdLinePass,
                          orig: string; conf: ConfigRef): ProcessNoteResult =
  var
    id = ""  # arg = key or [key] or key:val or [key]:val;  with val=on|off
    i = 0
    notes: ReportKinds
    isBracket = false

  if i < arg.len and arg[i] == '[':
    isBracket = true
    inc(i)

  while i < arg.len and (arg[i] notin {':', '=', ']'}):
    id.add(arg[i])
    inc(i)

  if isBracket:
    if i < arg.len and arg[i] == ']': inc(i)
    else: return ProcessNoteResult(kind: procNoteInvalidOption, switch: orig)
    # else: invalidCmdLineOption(conf, pass, orig, info)

  if i == arg.len: discard
  elif i < arg.len and (arg[i] in {':', '='}): inc(i)
  else: return ProcessNoteResult(kind: procNoteInvalidOption, switch: orig)

  defer:
    if notes == {rintGCStats}:
      # only check the singular case to avoid `--hints[all]` triggering it
      result.deprecatedGcStatsHintPresent = true

  # TODO: `ReportKinds` being used for notes/groups/etc is just wrong, it
  #        defines far more elements than one can actually control. A purpose
  #        built enum is required.

  proc findNote(noteSet: ReportKinds, groups: seq[(string, ReportKinds)]): ReportKinds =
    # Check groups like `--hint/warning[all]` or `--hint[Performance]` (very
    # vague term that maps onto multiple report kinds, such as "copies to
    # sink") first, because report groups have the same string values:
    # (`rlexLinterReport = "Name"`, `rsemLinterReport = "Name"`)
    for (groupName, flags) in groups:
      if cmpIgnoreStyle(groupName, id) == 0:
        return flags

    # report enums can have the same string value, indicating that they should
    # be grouped, this is why we iterate through the set and comparing by name
    for rk in items(noteSet - {repNone}):
      if cmpIgnoreStyle($rk, id) == 0:
        result.incl rk

    if result == {}:
      result = {repNone}

  # unfortunately, hintUser and warningUser clash, otherwise
  # implementation would simplify a bit
  notes =
    if state in {wHint, wHintAsError}:
      findNote(repHintKinds, repHintGroups)
    else:
      findNote(repWarningKinds, repWarningGroups)

  if notes == {repNone}:
    return
      if state in {wHint, wHintAsError}:
        ProcessNoteResult(kind: procNoteInvalidHint,
                          invalidHintOrWarning: id)
      else:
        ProcessNoteResult(kind: procNoteInvalidWarning,
                          invalidHintOrWarning: id)

  var val = substr(arg, i).normalize
  if val == "":
    val = "on"

  if val notin ["on", "off"]:
    # xxx in future work we should also allow users to have control over
    # `foreignPackageNotes` so that they can enable
    # `hints|warnings|warningAsErrors` for all the code they depend on.
    return ProcessNoteResult(kind: procNoteExpectedOnOrOff, switch: arg, argVal: val)
  else:
    let isOn = val == "on"
    if isOn and id.normalize == "all":
      return ProcessNoteResult(kind: procNoteOnlyAllOffSupported, switch: arg, argVal: val)

    for n in notes:
      if n notin conf.cmdlineNotes or pass == passCmd1:
        if pass == passCmd1:
          conf.incl(cnCmdline, n)

        conf.incl(cnModifiedy, n)

        if state in {wWarningAsError, wHintAsError}:
          conf.flip(cnWarnAsError, n, isOn)
        else:
          conf.flip(cnCurrent, n, isOn)
          conf.flip(cnMainPackage, n, isOn)

        if not isOn:
          conf.excl(cnForeign, n)
  
  result = ProcessNoteResult(kind: procNoteSuccess)

proc processSpecificNoteAndLog(arg: string, state: TSpecialWord, pass: TCmdLinePass,
                               orig: string; conf: ConfigRef) =
  let r = processSpecificNote(arg, state, pass, orig, conf)
  case r.kind
  of procNoteInvalidOption:
    conf.logError(CliLogMsg(kind: cliLogErrInvalidCommandLineOption,
                            wrongOpt: r.switch))
  of procNoteInvalidHint:
    conf.logError(CliLogMsg(kind: cliLogErrInvalidHint,
                            invalidErrHintWarn: r.invalidHintOrWarning))
  of procNoteInvalidWarning:
    conf.logError(CliLogMsg(kind: cliLogErrInvalidWarning,
                            invalidErrHintWarn: r.invalidHintOrWarning))
  of procNoteExpectedOnOrOff:
    conf.logError(CliLogMsg(kind: cliLogErrSwitchExpectedOnOrOff,
                            switch: r.switch,
                            argVal: r.argVal))
  of procNoteOnlyAllOffSupported:
    conf.logError(CliLogMsg(kind: cliLogErrSwitchExpectedAllOrOff,
                            switch: r.switch,
                            argVal: r.argVal))
  of procNoteSuccess:
    discard "TODO: log a trace for success?"

  if r.deprecatedGcStatsHintPresent:
    conf.logWarn(CliLogMsg(kind: cliLogWarnDeprecatedGcStatsHintPresent))

proc processCompile(conf: ConfigRef; filename: string) =
  var found = findFile(conf, filename)
  if found.isEmpty: found = AbsoluteFile filename
  extccomp.addExternalFileToCompile(conf, found)

# List of GC names for the error generation. It cannot be created
# from enum set using `getEnumNames` because nim cmdline has
# multiple names for the same garbage collector.
const gcNames = @[
  "boehm", "refc", "markandsweep", "destructors", "arc", "orc",
  "hooks", "go", "none", "stack", "regions"]

  cmdNames = @[
    "c", "cc", "compile", "compiletoc",
    "compiletooc", "js", "compiletojs", "r", "run", "check", "e",
    "doc2", "doc", "doc2tex", "rst2html", "rst2tex", "jsondoc2",
    "jsondoc", "ctags", "buildindex", "gendepend", "dump", "parse", "rod",
    "secret", "nop", "help", "jsonscript",]

type
  CompileOptArgCheckResult* = enum
    compileOptArgCheckSuccessTrue
    compileOptArgCheckSuccessFalse
    compileOptArgCheckWarnFalseDeprecated
    compileOptArgCheckFailedWithInvalidOption
    compileOptArgCheckFailedWithUnexpectedValue

func allowedCompileOptionArgs*(switch: string): seq[string] =
  case switch.normalize
  of "gc": gcNames
  of "opt": @["speed", "size", "none"]
  of "app": @["gui", "console", "lib", "staticlib"]
  of "exceptions": @["native", "setjmp", "quirky", "goto"]
  else: unreachable("not really, this is a compiler bug")

func testCompileOptionArg*(conf: ConfigRef; switch, arg: string): CompileOptArgCheckResult =
  template asResult(exp: bool): CompileOptArgCheckResult =
    {.line.}:
      if exp: compileOptArgCheckSuccessTrue
      else:   compileOptArgCheckSuccessFalse

  case switch.normalize
  of "gc":
    case arg.normalize
    of "boehm": asResult conf.selectedGC == gcBoehm
    of "refc": asResult conf.selectedGC == gcRefc
    of "markandsweep": asResult conf.selectedGC == gcMarkAndSweep
    of "destructors", "arc": asResult conf.selectedGC == gcArc
    of "orc": asResult conf.selectedGC == gcOrc
    of "hooks": asResult conf.selectedGC == gcHooks
    of "go": asResult conf.selectedGC == gcGo
    of "none": asResult conf.selectedGC == gcNone
    of "stack", "regions": asResult conf.selectedGC == gcRegions
    else: compileOptArgCheckFailedWithUnexpectedValue
  of "opt":
    case arg.normalize
    of "speed": asResult optOptimizeSpeed in conf.options
    of "size": asResult optOptimizeSize in conf.options
    of "none": asResult conf.options * {optOptimizeSpeed,optOptimizeSize} == {}
    else: compileOptArgCheckFailedWithUnexpectedValue
  of "verbosity": asResult $conf.verbosity == arg
  of "app":
    case arg.normalize
    of "gui": asResult optGenGuiApp in conf.globalOptions
    of "console": asResult optGenGuiApp notin conf.globalOptions
    of "lib": asResult optGenDynLib in conf.globalOptions and
                      optGenGuiApp notin conf.globalOptions
    of "staticlib": asResult optGenStaticLib in conf.globalOptions and
                      optGenGuiApp notin conf.globalOptions
    else: compileOptArgCheckFailedWithUnexpectedValue
  of "dynliboverride": asResult isDynlibOverride(conf, arg)
  of "exceptions":
    case arg.normalize
    of "native": asResult conf.exc == excNative
    of "setjmp": asResult conf.exc == excSetjmp
    of "quirky": asResult conf.exc == excQuirky
    of "goto": asResult conf.exc == excGoto
    else: compileOptArgCheckFailedWithUnexpectedValue
  else: compileOptArgCheckFailedWithInvalidOption

type
  CompileOptCheckResult* = enum
    compileOptCheckSuccessTrue
    compileOptCheckSuccessFalse
    compileOptCheckWarnFalseDeprecated
    compileOptCheckFailedWithInvalidOption

func testCompileOption*(conf: ConfigRef; switch: string): CompileOptCheckResult =
  template asResult(exp: bool): CompileOptCheckResult =
    {.line.}:
      if exp: compileOptCheckSuccessTrue
      else:   compileOptCheckSuccessFalse

  case switch.normalize
  of "debuginfo": asResult optCDebug in conf.globalOptions
  of "compileonly", "c": asResult optCompileOnly in conf.globalOptions
  of "nolinking": asResult optNoLinking in conf.globalOptions
  of "nomain": asResult optNoMain in conf.globalOptions
  of "forcebuild", "f": asResult optForceFullMake in conf.globalOptions
  of "warnings", "w": asResult optWarns in conf.options
  of "hints": asResult optHints in conf.options
  of "threadanalysis": asResult optThreadAnalysis in conf.globalOptions
  of "stacktrace": asResult optStackTrace in conf.options
  of "stacktracemsgs": asResult optStackTraceMsgs in conf.options
  of "linetrace": asResult optLineTrace in conf.options
  of "debugger": asResult optCDebug in conf.globalOptions
  of "profiler": asResult optProfiler in conf.options
  of "memtracker": asResult optMemTracker in conf.options
  of "checks", "x": asResult conf.options * ChecksOptions == ChecksOptions
  of "floatchecks":
    asResult conf.options * {optNaNCheck, optInfCheck} == {optNaNCheck, optInfCheck}
  of "infchecks": asResult optInfCheck in conf.options
  of "nanchecks": asResult optNaNCheck in conf.options
  of "objchecks": asResult optObjCheck in conf.options
  of "fieldchecks": asResult optFieldCheck in conf.options
  of "rangechecks": asResult optRangeCheck in conf.options
  of "boundchecks": asResult optBoundsCheck in conf.options
  of "overflowchecks": asResult optOverflowCheck in conf.options
  of "staticboundchecks": asResult optStaticBoundsCheck in conf.options
  of "stylechecks": asResult optStyleCheck in conf.options
  of "linedir": asResult optLineDir in conf.options
  of "assertions", "a": asResult optAssert in conf.options
  of "run", "r": asResult optRun in conf.globalOptions
  of "incremental": asResult conf.symbolFiles != disabledSf
  of "genscript": asResult optGenScript in conf.globalOptions
  of "threads": asResult optThreads in conf.globalOptions
  of "tlsemulation": asResult optTlsEmulation in conf.globalOptions
  of "implicitstatic": asResult optImplicitStatic in conf.options
  of "trmacros": asResult optTrMacros in conf.options
  of "excessivestacktrace": asResult optExcessiveStackTrace in conf.globalOptions
  of "cmdexitgcstats": asResult optCmdExitGcStats in conf.globalOptions
  else: compileOptCheckFailedWithInvalidOption

type
  ProcessPathResult = object
    case success: bool
      of true:
        path: AbsoluteDir
      of false:
        badPath: string

proc processPath(conf: ConfigRef; path: string, switch: string,
                 notRelativeToProj = false): ProcessPathResult =
  let
    p = if os.isAbsolute(path) or '$' in path:
          path
        elif notRelativeToProj:
          getCurrentDir() / path
        else:
          conf.projectPath.string / path
    info = newLineInfo(conf.commandLineSrcIdx, 0, -1)
    # xxx: we hack commandLineSrcIdx at callers like `nimconf` to get different
    #      info here; rework so it's all handled via returns and remove the
    #      need for info.
  try:
    ProcessPathResult(
      success: true,
      path: AbsoluteDir conf.pathSubs(p, conf.toFullPath(info).splitFile().dir))
  except ValueError:
    ProcessPathResult(success: false, badPath: p)

proc processCfgPath(conf: ConfigRef; path: string,
                    switch: string): ProcessPathResult =
  let
    path = if path.len > 0 and path[0] == '"': strutils.unescape(path)
           else: path
    info = newLineInfo(conf.commandLineSrcIdx, 0, -1)
    basedir = toFullPath(conf, info).splitFile().dir
    p = if os.isAbsolute(path) or '$' in path:
            path
          else:
            basedir / path
  try:
    ProcessPathResult(success: true,
                      path: AbsoluteDir pathSubs(conf, p, basedir))
  except ValueError:
    ProcessPathResult(success: false, badPath: p)

proc dynlibOverride(conf: ConfigRef; switch, arg: string, pass: TCmdLinePass) =
  if pass in {passCmd2, passPP}:
    expectArg(conf, switch, arg)
    options.inclDynlibOverride(conf, arg)

proc handleStdinOrCmdInput(conf: ConfigRef) =
  conf.projectFull = conf.projectName.AbsoluteFile
  conf.projectPath = AbsoluteDir getCurrentDir()
  if conf.outDir.isEmpty:
    conf.outDir = getNimcacheDir(conf)

proc handleStdinInput*(conf: ConfigRef) =
  conf.projectName = "stdinfile"
  handleStdinOrCmdInput(conf)

proc handleCmdInput*(conf: ConfigRef) =
  conf.projectName = "cmdfile"
  handleStdinOrCmdInput(conf)

proc parseCommand*(command: string): Command =
  # NOTE: when adding elements to this list, sync with `cmdNames` const
  # TODO: rework this plus `cmdNames` etc... to be more like `extccomp.TInfoCC`
  case command.normalize
  of "c", "cc", "compile", "compiletoc": cmdCompileToC
  of "js", "compiletojs": cmdCompileToJS
  of "vm", "compiletovm": cmdCompileToVM
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
  of "scan": cmdScan
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
  of cmdCompileToJS: conf.backend = backendJs
  of cmdCompileToVM: conf.backend = backendNimVm
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

proc processPathAndLog(conf: ConfigRef; path: string, switch: string,
                       notRelativeToProj = false): AbsoluteDir =
  # TODO: remove the need for this
  let p = processPath(conf, path, switch, notRelativeToProj)
  case p.success
  of true: p.path
  of false:
    conf.logError(CliLogMsg(kind: cliLogErrInvalidPath,
                            switch: switch,
                            argVal: p.badPath))
    AbsoluteDir p.badPath # allows continuation of processing

proc processCfgPathAndLog(conf: ConfigRef; path: string,
                          switch: string): AbsoluteDir =
  # TODO: remove the need for this
  let p = processCfgPath(conf, path, switch)
  case p.success
  of true: p.path
  of false:
    conf.logError(CliLogMsg(kind: cliLogErrInvalidPath,
                            switch: switch,
                            argVal: p.badPath))
    AbsoluteDir p.badPath # allows continuation of processing

proc processSwitch*(switch, arg: string, pass: TCmdLinePass, conf: ConfigRef) =
  # TODO: rework from logging to returning a result
  var key, val: string
  func invalidSwitchValue(allowed: seq[string]): CliLogMsg =
    CliLogMsg(kind: cliLogErrSwitchInvalidValue, forSwitch: switch,
              givenVal: arg, allowed: allowed)

  case switch.normalize
  of "fromcmd":
    expectNoArg(conf, switch, arg)
    conf.inputMode = pimCmd
  of "path", "p":
    expectArg(conf, switch, arg)
    for path in nimbleSubs(conf, arg):
      conf.addPath:
        case pass
        of passPP: processCfgPathAndLog(conf, path, switch)
        else:      processPathAndLog(conf, path, switch)
  of "nimblepath":
    if pass in {passCmd2, passPP} and optNoNimblePath notin conf.globalOptions:
      expectArg(conf, switch, arg)
      var path = processPathAndLog(conf, arg, switch, notRelativeToProj=true)
      # TODO: move up nimble stuff, then set path once
      let nimbleDir = AbsoluteDir getEnv("NIMBLE_DIR")
      if not nimbleDir.isEmpty and pass == passPP:
        path = nimbleDir / RelativeDir"pkgs"
      nimblePath(conf, path, conf.commandLineSrcIdx)
  of "nonimblepath":
    expectNoArg(conf, switch, arg)
    disableNimblePath(conf)
  of "clearnimblepath":
    expectNoArg(conf, switch, arg)
    clearNimblePath(conf)
  of "excludepath":
    expectArg(conf, switch, arg)
    let path = processPathAndLog(conf, arg, switch)
    conf.searchPaths = conf.searchPaths.filterIt(it != path)
    conf.lazyPaths = conf.lazyPaths.filterIt(it != path)
  of "nimcache":
    expectArg(conf, switch, arg)
    var arg = arg
    # refs bug #18674, otherwise `--os:windows` messes up with `--nimcache` set
    # in config nims files, e.g. via: `import os; switch("nimcache", "/tmp/somedir")`
    if conf.target.targetOS == osWindows and DirSep == '/': arg = arg.replace('\\', '/')
    conf.nimcacheDir = processPathAndLog(conf, arg, switch,
                                         notRelativeToProj=true)
  of "out", "o":
    expectArg(conf, switch, arg)
    let f = splitFile(string processPathAndLog(conf, arg, switch,
                                               notRelativeToProj=true))
    conf.outFile = RelativeFile f.name & f.ext
    conf.outDir = toAbsoluteDir f.dir
  of "outdir":
    expectArg(conf, switch, arg)
    conf.outDir = processPathAndLog(conf, arg, switch, notRelativeToProj=true)
  of "depfile":
    expectArg(conf, switch, arg)
    conf.depfile =
      AbsoluteFile conf.processPathAndLog(arg, switch, notRelativeToProj=true)
  of "usenimcache":
    processOnOffSwitchG(conf, {optUseNimcache}, arg, switch)
  of "docseesrcurl":
    expectArg(conf, switch, arg)
    conf.docSeeSrcUrl = arg
  of "docroot":
    conf.docRoot = if arg.len == 0: docRootDefault else: arg
  of "backend", "b":
    let backend = parseEnum(arg.normalize, TBackend.default)
    if backend == TBackend.default:
      conf.logError(invalidSwitchValue(@["c", "js", "vm"]))
    conf.backend = backend
  of "doccmd": conf.docCmd = arg
  of "define", "d":
    expectArg(conf, switch, arg)
    if {':', '='} in arg:
      splitSwitch(conf, arg, key, val)
      specialDefine(conf, key, pass)
      defineSymbol(conf, key, val)
    else:
      specialDefine(conf, arg, pass)
      defineSymbol(conf, arg)
  of "undef", "u":
    expectArg(conf, switch, arg)
    undefSymbol(conf, arg)
  of "compile":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}: processCompile(conf, arg)
  of "link":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}:
      addExternalFileToLink(conf, AbsoluteFile arg)
  of "debuginfo":
    processOnOffSwitchG(conf, {optCDebug}, arg, switch)
  of "embedsrc":
    processOnOffSwitchG(conf, {optEmbedOrigSrc}, arg, switch)
  of "compileonly", "c":
    processOnOffSwitchG(conf, {optCompileOnly}, arg, switch)
  of "nolinking":
    processOnOffSwitchG(conf, {optNoLinking}, arg, switch)
  of "nomain":
    processOnOffSwitchG(conf, {optNoMain}, arg, switch)
  of "forcebuild", "f":
    processOnOffSwitchG(conf, {optForceFullMake}, arg, switch)
  of "project":
    processOnOffSwitchG(
      conf, {optWholeProject, optGenIndex}, arg, switch)
  of "gc":
    if conf.backend in {backendJs, backendNimVm}: return # for: bug #16033
    expectArg(conf, switch, arg)
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
        if conf.exc in {excNone, excNative}:
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
        if conf.exc in {excNone, excNative}:
          conf.exc = excGoto
      of "hooks":
        conf.selectedGC = gcHooks
        defineSymbol(conf, "gchooks")
        conf.incl optSeqDestructors
        processOnOffSwitchG(conf, {optSeqDestructors}, arg, switch)
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
      else:
        conf.logError(invalidSwitchValue(gcNames))
  of "warnings", "w":
    if processOnOffSwitchOrList(conf, {optWarns}, arg, switch):
      listWarnings(conf)
  of "warning":
    processSpecificNoteAndLog(arg, wWarning, pass, switch, conf)
  of "hint":
    processSpecificNoteAndLog(arg, wHint, pass, switch, conf)
  of "warningaserror":
    processSpecificNoteAndLog(arg, wWarningAsError, pass, switch, conf)
  of "hintaserror":
    processSpecificNoteAndLog(arg, wHintAsError, pass, switch, conf)
  of "hints":
    if processOnOffSwitchOrList(conf, {optHints}, arg, switch):
      listHints(conf)
  of "threadanalysis":
    if conf.backend == backendJs: discard
    else:
      processOnOffSwitchG(conf, {optThreadAnalysis}, arg, switch)
  of "stacktrace":
    processOnOffSwitch(conf, {optStackTrace}, arg, switch)
  of "stacktracemsgs":
    processOnOffSwitch(conf, {optStackTraceMsgs}, arg, switch)
  of "excessivestacktrace":
    processOnOffSwitchG(conf, {optExcessiveStackTrace}, arg, switch)
  of "linetrace":
    processOnOffSwitch(conf, {optLineTrace}, arg, switch)
  of "debugger":
    case arg.normalize
    of "on", "native", "gdb":
      conf.incl optCDebug
      conf.incl optLineDir
      #defineSymbol(conf.symbols, "nimTypeNames") # type names are used in gdb pretty printing
    of "off":
      conf.excl optCDebug
    else:
      conf.logError(invalidSwitchValue(@["native", "gdb", "on", "off"]))
      # conf.localReport(
      #   info, invalidSwitchValue @["native", "gdb", "on", "off"])
  of "profiler":
    processOnOffSwitch(conf, {optProfiler}, arg, switch)
    if optProfiler in conf.options:
      defineSymbol(conf, "profiler")
    else:
      undefSymbol(conf, "profiler")
  of "memtracker":
    processOnOffSwitch(conf, {optMemTracker}, arg, switch)
    if optMemTracker in conf.options:
      defineSymbol(conf, "memtracker")
    else:
      undefSymbol(conf, "memtracker")
  of "checks", "x":
    processOnOffSwitch(conf, ChecksOptions, arg, switch)
  of "floatchecks":
    processOnOffSwitch(
      conf, {optNaNCheck, optInfCheck}, arg, switch)
  of "infchecks":
    processOnOffSwitch(conf, {optInfCheck}, arg, switch)
  of "nanchecks":
    processOnOffSwitch(conf, {optNaNCheck}, arg, switch)
  of "objchecks":
    processOnOffSwitch(conf, {optObjCheck}, arg, switch)
  of "fieldchecks":
    processOnOffSwitch(conf, {optFieldCheck}, arg, switch)
  of "rangechecks":
    processOnOffSwitch(conf, {optRangeCheck}, arg, switch)
  of "boundchecks":
    processOnOffSwitch(conf, {optBoundsCheck}, arg, switch)
  of "overflowchecks":
    processOnOffSwitch(conf, {optOverflowCheck}, arg, switch)
  of "staticboundchecks":
    processOnOffSwitch(conf, {optStaticBoundsCheck}, arg, switch)
  of "stylechecks":
    processOnOffSwitch(conf, {optStyleCheck}, arg, switch)
  of "linedir":
    processOnOffSwitch(conf, {optLineDir}, arg, switch)
  of "assertions", "a":
    processOnOffSwitch(conf, {optAssert}, arg, switch)
  of "threads":
    if conf.backend == backendJs:
      discard
    else:
      processOnOffSwitchG(conf, {optThreads}, arg, switch)
    #if optThreads in conf.globalOptions: conf.setNote(warnGcUnsafe)
  of "tlsemulation":
    processOnOffSwitchG(conf, {optTlsEmulation}, arg, switch)
  of "implicitstatic":
    processOnOffSwitch(conf, {optImplicitStatic}, arg, switch)
  of "trmacros":
    processOnOffSwitch(conf, {optTrMacros}, arg, switch)
  of "opt":
    expectArg(conf, switch, arg)
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
      conf.logError(invalidSwitchValue(@["speed", "size", "none"]))
  of "app":
    expectArg(conf, switch, arg)
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
      conf.logError(invalidSwitchValue(@["gui", "console", "lib", "staticlib"]))
  of "passc", "t":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}: extccomp.addCompileOptionCmd(conf, arg)
  of "passl", "l":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}: extccomp.addLinkOptionCmd(conf, arg)
  of "cincludes":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}:
      conf.cIncludesAdd processPathAndLog(conf, arg, switch)
  of "clibdir":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}:
      conf.cLibsAdd processPathAndLog(conf, arg, switch)
  of "clib":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}:
      conf.cLinkedLibsAdd processPathAndLog(conf, arg, switch).string
  of "header":
    if conf != nil: conf.headerFile = arg
    incl(conf, optGenIndex)
  of "index":
    processOnOffSwitchG(conf, {optGenIndex}, arg, switch)
  of "import":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}:
      let info = newLineInfo(conf.commandLineSrcIdx, 0, -1)
      conf.implicitImportsAdd findModule(
        conf, arg, toFullPath(conf, info)).string
  of "include":
    expectArg(conf, switch, arg)
    if pass in {passCmd2, passPP}:
      let info = newLineInfo(conf.commandLineSrcIdx, 0, -1)
      conf.implicitIncludesAdd findModule(
        conf, arg, toFullPath(conf, info)).string
  of "listcmd":
    processOnOffSwitchG(conf, {optListCmd}, arg, switch)
  of "asm":
    processOnOffSwitchG(conf, {optProduceAsm}, arg, switch)
  of "genmapping":
    processOnOffSwitchG(conf, {optGenMapping}, arg, switch)
  of "os":
    expectArg(conf, switch, arg)
    let theOS = platform.nameToOS(arg)
    if theOS == osNone:
      conf.logError(invalidSwitchValue(platform.listOSnames()))
    else:
      conf.target =
        block:
          var t = conf.target
          setTarget(t, theOS, conf.target.targetCPU)
          t
  of "cpu":
    expectArg(conf, switch, arg)
    let cpu = platform.nameToCPU(arg)
    if cpu == cpuNone:
      conf.logError(invalidSwitchValue(platform.listCPUnames()))
    else:
      conf.target =
        block:
          var t = conf.target
          setTarget(t, conf.target.targetOS, cpu)
          t
  of "run", "r":
    processOnOffSwitchG(conf, {optRun}, arg, switch)
  of "maxloopiterationsvm":
    expectArg(conf, switch, arg)
    conf.maxLoopIterationsVM = parseInt(arg)
  of "errormax":
    expectArg(conf, switch, arg)
    # Note: `nim check` (etc) can overwrite this.
    # `0` is meaningless, give it a useful meaning as in clang's -ferror-limit
    # If user doesn't set this flag and the code doesn't either, it'd
    # have the same effect as errorMax = 1
    let ret = parseInt(arg)
    conf.errorMax = if ret == 0: high(int) else: ret
  of "verbosity":
    expectArg(conf, switch, arg)
    let verbosity = parseInt(arg)
    case verbosity
    of 0: conf.verbosity = compVerbosityMin
    of 1: conf.verbosity = compVerbosityDefault
    of 2: conf.verbosity = compVerbosityHigh
    of 3: conf.verbosity = compVerbosityMax
    else:
      conf.logError(invalidSwitchValue( @["0", "1", "2", "3"]))
    let verb = NotesVerbosity.main[conf.verbosity]
    ## We override the default `verb` by explicitly modified (set/unset) notes.
    conf.notes = (conf.modifiedyNotes * conf.notes + verb) -
      (conf.modifiedyNotes * verb - conf.notes)
    conf.mainPackageNotes = conf.notes
  of "parallelbuild":
    expectArg(conf, switch, arg)
    conf.numberOfProcessors = parseInt(arg)
  of "version", "v":
    expectNoArg(conf, switch, arg)
    writeVersionInfo(conf, pass)
  of "advanced":
    expectNoArg(conf, switch, arg)
    writeAdvancedUsage(conf, pass)
  of "fullhelp":
    expectNoArg(conf, switch, arg)
    writeFullhelp(conf, pass)
  of "help", "h":
    expectNoArg(conf, switch, arg)
    writeHelp(conf, pass)
  of "incremental", "ic":
    if pass in {passCmd2, passPP}:
      case arg.normalize
      of "on": conf.symbolFiles = v2Sf
      of "off": conf.symbolFiles = disabledSf
      of "writeonly": conf.symbolFiles = writeOnlySf
      of "readonly": conf.symbolFiles = readOnlySf
      of "v2": conf.symbolFiles = v2Sf
      of "stress": conf.symbolFiles = stressTest
      else:
        conf.logError:
          invalidSwitchValue:
            @["on", "off", "writeonly", "readonly", "v2", "stress"]
    setUseIc(conf.symbolFiles != disabledSf)
  of "skipcfg":
    processOnOffSwitchG(conf, {optSkipSystemConfigFile}, arg, switch)
  of "skipprojcfg":
    processOnOffSwitchG(conf, {optSkipProjConfigFile}, arg, switch)
  of "skipusercfg":
    processOnOffSwitchG(conf, {optSkipUserConfigFile}, arg, switch)
  of "skipparentcfg":
    processOnOffSwitchG(conf, {optSkipParentConfigFiles}, arg, switch)
  of "genscript":
    processOnOffSwitchG(conf, {optGenScript}, arg, switch)
    processOnOffSwitchG(conf, {optCompileOnly}, arg, switch)
  of "colors": processOnOffSwitchG(conf, {optUseColors}, arg, switch)
  of "lib":
    expectArg(conf, switch, arg)
    conf.libpath = processPathAndLog(conf, arg, switch, notRelativeToProj=true)
  of "putenv":
    expectArg(conf, switch, arg)
    splitSwitch(conf, arg, key, val)
    os.putEnv(key, val)
  of "cc":
    expectArg(conf, switch, arg)
    case setCC(conf, arg)
    of ccNone:
      conf.logError(CliLogMsg(kind: cliLogErrUnknownCCompiler,
                              givenVal: arg,
                              allowed: listCCnames()))
    else:
      discard "valid compiler set"
  of "stdout":
    processOnOffSwitchG(conf, {optStdout}, arg, switch)
  of "filenames":
    case arg.normalize
    of "abs": conf.filenameOption = foAbs
    of "canonical": conf.filenameOption = foCanonical
    of "legacyrelproj": conf.filenameOption = foLegacyRelProj
    else:
      conf.logError(invalidSwitchValue(@["abs", "canonical", "legacyRelProj"]))
  of "msgformat":
    case arg.normalize:
      of "text":
        conf.setReportHook cli_reporter.reportHook
      of "sexp":
        conf.setReportHook sexp_reporter.reportHook
      else:
        conf.logError(invalidSwitchValue(@["text", "sexp"]))
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
      conf.logError(invalidSwitchValue(@["dots", "filenames", "off"]))
  of "unitsep":
    conf.unitSep = if conf.switchOn(switch.normalize, arg): "\31" else: ""
  of "listfullpaths":
    # xxx: this should probably get subsubed with filenames
    conf.filenameOption =
      if conf.switchOn(switch.normalize, arg): foAbs
      else:                                    foCanonical
  of "spellsuggest":
    if arg.len == 0: conf.spellSuggestMax = spellSuggestSecretSauce
    elif arg == "auto": conf.spellSuggestMax = spellSuggestSecretSauce
    else: conf.spellSuggestMax = parseInt(arg)
  of "declaredlocs":
    processOnOffSwitchG(conf, {optDeclaredLocs}, arg, switch)
  of "dynliboverride":
    dynlibOverride(conf, switch, arg, pass)
  of "dynliboverrideall":
    processOnOffSwitchG(conf, {optDynlibOverrideAll}, arg, switch)
  of "experimental":
    if arg.len == 0:
      conf.incl oldExperimentalFeatures
    else:
      try:
        conf.incl parseEnum[Feature](arg)
      except ValueError:
        conf.logError:
          CliLogMsg(kind: cliLogErrUnknownExperimentalFeature,
                    givenVal: arg,
                    allowed: getEnumNames({low(Feature) .. high(Feature)}))
  of "exceptions":
    case arg.normalize
    of "native": conf.exc = excNative
    of "setjmp": conf.exc = excSetjmp
    of "quirky": conf.exc = excQuirky
    of "goto": conf.exc = excGoto
    else:
      conf.logError(invalidSwitchValue(@["native", "setjmp", "quirky", "goto"]))
  of "cppdefine":
    expectArg(conf, switch, arg)
    if conf != nil:
      conf.cppDefine(arg)
  of "seqsv2":
    processOnOffSwitchG(conf, {optSeqDestructors}, arg, switch)
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
      conf.logError(invalidSwitchValue(@["off", "hint", "error", "usages"]))
  of "showallmismatches":
    processOnOffSwitchG(conf, {optShowAllMismatches}, arg, switch)
  of "docinternal":
    processOnOffSwitchG(conf, {optDocInternal}, arg, switch)
  of "multimethods":
    processOnOffSwitchG(conf, {optMultiMethods}, arg, switch)
  of "expandmacro":
    expectArg(conf, switch, arg)
    conf.macrosToExpand[arg] = "T"
  of "expandarc":
    expectArg(conf, switch, arg)
    conf.arcToExpand[arg] = "T"
  of "benchmarkvm":
    processOnOffSwitchG(conf, {optBenchmarkVM}, arg, switch)
  of "profilevm":
    processOnOffSwitchG(conf, {optProfileVM}, arg, switch)
  of "sinkinference":
    processOnOffSwitch(conf, {optSinkInference}, arg, switch)
  of "cursorinference":
    # undocumented, for debugging purposes only:
    processOnOffSwitch(conf, {optCursorInference}, arg, switch)
  of "panics":
    processOnOffSwitchG(conf, {optPanics}, arg, switch)
    if optPanics in conf.globalOptions:
      defineSymbol(conf, "nimPanics")
  of "sourcemap": # xxx document in --fullhelp
    conf.incl optSourcemap
    conf.incl optLineDir
  of "deepcopy":
    processOnOffSwitchG(conf, {optEnableDeepCopy}, arg, switch)
  of "": # comes from "-" in for example: `nim c -r -` (gets stripped from -)
    conf.inputMode = pimStdin
  of "cmdexitgcstats":
    conf.incl optCmdExitGcStats
    # for legacy
    conf.incl(cnCmdline, rintGCStats)
    conf.incl(cnModifiedy, rintGCStats)
  else:
    if strutils.find(switch, '.') >= 0: options.setConfigVar(conf, switch, arg)
    else: invalidCmdLineOption(conf, switch)

proc processCommand*(switch: string, pass: TCmdLinePass; config: ConfigRef) =
  var cmd, arg: string
  splitSwitch(config, switch, cmd, arg)
  processSwitch(cmd, arg, pass, config)

proc processSwitch*(pass: TCmdLinePass; p: OptParser; config: ConfigRef) =
  # hint[X]:off is parsed as (p.key = "hint[X]", p.val = "off")
  # we transform it to (key = hint, val = [X]:off)
  var bracketLe = strutils.find(p.key, '[')
  if bracketLe >= 0:
    var key = substr(p.key, 0, bracketLe - 1)
    var val = substr(p.key, bracketLe) & ':' & p.val
    processSwitch(key, val, pass, config)
  else:
    processSwitch(p.key, p.val, pass, config)

proc processArgument*(pass: TCmdLinePass; p: OptParser;
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
      config.logError(CliLogMsg(kind: cliLogErrUnexpectedRunOpt,
                                cmd: config.command))
