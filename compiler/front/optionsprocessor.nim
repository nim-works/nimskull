## this modules processes various options given as flags and applies them to
## a given `ConfigRef`. This allows for command line, configuration, comptime,
## etc configuration building and updates through a unified interface



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
  ],
  compiler/backend/[
    extccomp,
  ],
  compiler/utils/[
    pathutils,
    platform,
    idioms,
  ]

from compiler/ast/ast import setUseIc

# TODO: remove remaining legacy reports stuff.
from compiler/ast/report_enums import ReportKind,
  ReportKinds,
  repHintKinds,
  repHintGroups,
  repWarningKinds,
  repWarningGroups

type
  # TODO:
  # - rename to `Cmd` to `Cfg`, as only the CLI/cmd has version, help,
  #   fullhelp, advanced and perhaps others
  # - separate flags/switches and commands
  CmdSwitchKind* = enum
    cmdSwitchFromcmd
    cmdSwitchPath
    cmdSwitchNimblepath
    cmdSwitchNonimblepath
    cmdSwitchClearnimblepath
    cmdSwitchExcludepath
    cmdSwitchNimcache
    cmdSwitchOut
    cmdSwitchOutdir
    cmdSwitchDepfile
    cmdSwitchUsenimcache
    cmdSwitchDocseesrcurl
    cmdSwitchDocroot
    cmdSwitchBackend
    cmdSwitchDoccmd
    cmdSwitchDefine
    cmdSwitchUndef
    cmdSwitchCompile
    cmdSwitchLink
    cmdSwitchDebuginfo
    cmdSwitchEmbedsrc
    cmdSwitchCompileonly
    cmdSwitchNolinking
    cmdSwitchNomain
    cmdSwitchForcebuild
    cmdSwitchProject
    cmdSwitchGc
    cmdSwitchWarnings
    cmdSwitchWarning
    cmdSwitchHint
    cmdSwitchWarningaserror
    cmdSwitchHintaserror
    cmdSwitchHints
    cmdSwitchThreadanalysis
    cmdSwitchStacktrace
    cmdSwitchStacktracemsgs
    cmdSwitchExcessivestacktrace
    cmdSwitchLinetrace
    cmdSwitchDebugger
    cmdSwitchProfiler
    cmdSwitchMemtracker
    cmdSwitchChecks
    cmdSwitchFloatchecks
    cmdSwitchInfchecks
    cmdSwitchNanchecks
    cmdSwitchObjchecks
    cmdSwitchFieldchecks
    cmdSwitchRangechecks
    cmdSwitchBoundchecks
    cmdSwitchOverflowchecks
    cmdSwitchStaticboundchecks
    cmdSwitchStylechecks
    cmdSwitchLinedir
    cmdSwitchAssertions
    cmdSwitchThreads
    cmdSwitchTlsemulation
    cmdSwitchImplicitstatic
    cmdSwitchTrmacros
    cmdSwitchOpt
    cmdSwitchApp
    cmdSwitchPassc
    cmdSwitchPassl
    cmdSwitchCincludes
    cmdSwitchClibdir
    cmdSwitchClib
    cmdSwitchHeader
    cmdSwitchIndex
    cmdSwitchImport
    cmdSwitchInclude
    cmdSwitchListcmd
    cmdSwitchAsm
    cmdSwitchGenmapping
    cmdSwitchOs
    cmdSwitchCpu
    cmdSwitchRun
    cmdSwitchMaxloopiterationsvm
    cmdSwitchErrormax
    cmdSwitchVerbosity
    cmdSwitchParallelbuild
    # cmdSwitchVersion      # CLI only, forces a quit
    # cmdSwitchAdvanced     # CLI only, forces a quit
    # cmdSwitchFullhelp     # CLI only, forces a quit
    # cmdSwitchHelp         # CLI only, forces a quit
    cmdSwitchIncremental
    cmdSwitchSkipcfg
    cmdSwitchSkipprojcfg
    cmdSwitchSkipusercfg
    cmdSwitchSkipparentcfg
    cmdSwitchGenscript
    cmdSwitchColors
    cmdSwitchLib
    cmdSwitchPutenv
    cmdSwitchCc
    cmdSwitchStdout
    cmdSwitchFilenames
    # cmdSwitchMsgformat    # CLI only
    cmdSwitchProcessing
    cmdSwitchUnitsep
    cmdSwitchListfullpaths
    cmdSwitchSpellsuggest
    cmdSwitchDeclaredlocs
    cmdSwitchDynliboverride
    cmdSwitchDynliboverrideall
    cmdSwitchExperimental
    cmdSwitchExceptions
    cmdSwitchCppdefine
    cmdSwitchSeqsv2
    cmdSwitchStylecheck
    cmdSwitchShowallmismatches
    cmdSwitchDocinternal
    cmdSwitchMultimethods
    cmdSwitchExpandmacro
    cmdSwitchExpandarc
    cmdSwitchBenchmarkvm
    cmdSwitchProfilevm
    cmdSwitchSinkinference
    cmdSwitchCursorinference
    cmdSwitchPanics
    cmdSwitchSourcemap
    cmdSwitchDeepcopy
    cmdSwitchProjStdin
    cmdSwitchCmdexitgcstats
    cmdSwitchConfigVar

  # Full list of all the command line options.
  CmdSwitchTextKind* = enum
    fullSwitchTxtFromcmd             = "fromcmd"
    fullSwitchTxtPath                = "path",        smolSwitchTxtPath        = "p",
    fullSwitchTxtNimblepath          = "nimblepath"
    fullSwitchTxtNonimblepath        = "nonimblepath"
    fullSwitchTxtClearnimblepath     = "clearnimblepath"
    fullSwitchTxtExcludepath         = "excludepath"
    fullSwitchTxtNimcache            = "nimcache"
    fullSwitchTxtOut                 = "out",         smolSwitchTxtOut         = "o",
    fullSwitchTxtOutdir              = "outdir"
    fullSwitchTxtDepfile             = "depfile"
    fullSwitchTxtUsenimcache         = "usenimcache"
    fullSwitchTxtDocseesrcurl        = "docseesrcurl"
    fullSwitchTxtDocroot             = "docroot"
    fullSwitchTxtBackend             = "backend",     smolSwitchTxtBackend     = "b",
    fullSwitchTxtDoccmd              = "doccmd"
    fullSwitchTxtDefine              = "define",      smolSwitchTxtDefine      = "d",
    fullSwitchTxtUndef               = "undef",       smolSwitchTxtUndef       = "u",
    fullSwitchTxtCompile             = "compile"
    fullSwitchTxtLink                = "link"
    fullSwitchTxtDebuginfo           = "debuginfo"
    fullSwitchTxtEmbedsrc            = "embedsrc"
    fullSwitchTxtCompileonly         = "compileonly", smolSwitchTxtCompileonly = "c",
    fullSwitchTxtNolinking           = "nolinking"
    fullSwitchTxtNomain              = "nomain"
    fullSwitchTxtForcebuild          = "forcebuild",  smolSwitchTxtForcebuild  = "f",
    fullSwitchTxtGc                  = "gc",
    fullSwitchTxtProject             = "project"
    fullSwitchTxtWarnings            = "warnings",    smolSwitchTxtWarnings    = "w",
    fullSwitchTxtWarning             = "warning"
    fullSwitchTxtHint                = "hint"
    fullSwitchTxtWarningaserror      = "warningaserror"
    fullSwitchTxtHintaserror         = "hintaserror"
    fullSwitchTxtHints               = "hints"
    fullSwitchTxtThreadanalysis      = "threadanalysis"
    fullSwitchTxtStacktrace          = "stacktrace"
    fullSwitchTxtStacktracemsgs      = "stacktracemsgs"
    fullSwitchTxtExcessivestacktrace = "excessivestacktrace"
    fullSwitchTxtLinetrace           = "linetrace"
    fullSwitchTxtDebugger            = "debugger",
    fullSwitchTxtProfiler            = "profiler"
    fullSwitchTxtMemtracker          = "memtracker"
    fullSwitchTxtChecks              = "checks"
    fullSwitchTxtFloatchecks         = "floatchecks"
    fullSwitchTxtInfchecks           = "infchecks"
    fullSwitchTxtNanchecks           = "nanchecks"
    fullSwitchTxtObjchecks           = "objchecks"
    fullSwitchTxtFieldchecks         = "fieldchecks"
    fullSwitchTxtRangechecks         = "rangechecks"
    fullSwitchTxtBoundchecks         = "boundchecks"
    fullSwitchTxtOverflowchecks      = "overflowchecks"
    fullSwitchTxtStaticboundchecks   = "staticboundchecks"
    fullSwitchTxtStylechecks         = "stylechecks"
    fullSwitchTxtLinedir             = "linedir"
    fullSwitchTxtAssertions          = "assertions"
    fullSwitchTxtThreads             = "threads"
    fullSwitchTxtTlsemulation        = "tlsemulation"
    fullSwitchTxtImplicitstatic      = "implicitstatic"
    fullSwitchTxtTrmacros            = "trmacros"
    fullSwitchTxtOpt                 = "opt"
    fullSwitchTxtApp                 = "app"
    fullSwitchTxtPassc               = "passc"
    fullSwitchTxtPassl               = "passl"
    fullSwitchTxtCincludes           = "cincludes"
    fullSwitchTxtClibdir             = "clibdir"
    fullSwitchTxtClib                = "clib"
    fullSwitchTxtHeader              = "header"
    fullSwitchTxtIndex               = "index"
    fullSwitchTxtImport              = "import"
    fullSwitchTxtInclude             = "include"
    fullSwitchTxtListcmd             = "listcmd"
    fullSwitchTxtAsm                 = "asm"
    fullSwitchTxtGenmapping          = "genmapping"
    fullSwitchTxtOs                  = "os"
    fullSwitchTxtCpu                 = "cpu"
    fullSwitchTxtRun                 = "run"
    fullSwitchTxtMaxloopiterationsvm = "maxloopiterationsvm"
    fullSwitchTxtErrormax            = "errormax"
    fullSwitchTxtVerbosity           = "verbosity"
    fullSwitchTxtParallelbuild       = "parallelbuild"
    # fullSwitchTxtVersion             = "version"
    # smolSwitchTxtVersion             = "v"
    # fullSwitchTxtAdvanced            = "advanced"
    # fullSwitchTxtFullhelp            = "fullhelp"
    # fullSwitchTxtHelp                = "help"
    # smolSwitchTxtHelp                = "h"
    fullSwitchTxtIncremental         = "incremental"
    aliasSwitchTxtIncremental        = "ic'"
    fullSwitchTxtSkipcfg             = "skipcfg"
    fullSwitchTxtSkipprojcfg         = "skipprojcfg"
    fullSwitchTxtSkipusercfg         = "skipusercfg"
    fullSwitchTxtSkipparentcfg       = "skipparentcfg"
    fullSwitchTxtGenscript           = "genscript"
    fullSwitchTxtColors              = "colors"
    fullSwitchTxtLib                 = "lib"
    fullSwitchTxtPutenv              = "putenv"
    fullSwitchTxtCc                  = "cc"
    fullSwitchTxtStdout              = "stdout"
    fullSwitchTxtFilenames           = "filenames"
    # fullSwitchTxtMsgformat           = "msgformat"
    fullSwitchTxtProcessing          = "processing"
    fullSwitchTxtUnitsep             = "unitsep"
    fullSwitchTxtListfullpaths       = "listfullpaths"
    fullSwitchTxtSpellsuggest        = "spellsuggest"
    fullSwitchTxtDeclaredlocs        = "declaredlocs"
    fullSwitchTxtDynliboverride      = "dynliboverride"
    fullSwitchTxtDynliboverrideall   = "dynliboverrideall"
    fullSwitchTxtExperimental        = "experimental"
    fullSwitchTxtExceptions          = "exceptions"
    fullSwitchTxtCppdefine           = "cppdefine"
    fullSwitchTxtSeqsv2              = "seqsv2"
    fullSwitchTxtStylecheck          = "stylecheck"
    fullSwitchTxtShowallmismatches   = "showallmismatches"
    fullSwitchTxtDocinternal         = "docinternal"
    fullSwitchTxtMultimethods        = "multimethods"
    fullSwitchTxtExpandmacro         = "expandmacro"
    fullSwitchTxtExpandarc           = "expandarc"
    fullSwitchTxtBenchmarkvm         = "benchmarkvm"
    fullSwitchTxtProfilevm           = "profilevm"
    fullSwitchTxtSinkinference       = "sinkinference"
    fullSwitchTxtCursorinference     = "cursorinference"
    fullSwitchTxtPanics              = "panics"
    fullSwitchTxtSourcemap           = "sourcemap"
    fullSwitchTxtDeepcopy            = "deepcopy"
    fullSwitchTxtCmdexitgcstats      = "cmdexitgcstats"
    smolSwitchTxtProjStdin           = ""               # `nim c -r -`, the `-` gets stripped
    fullSwitchTxtConfigVar           = "*.*"            # cfg var dummy entry
    fullSwitchTxtInvalid             = "!ERROR!"

const
    cmdSwitchToTxt = [
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
      # cmdSwitchMsgformat          : {fullSwitchTxtMsgformat},
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

    cmdTxtToSwitch: array[CmdSwitchTextKind, CmdSwitchKind] =
      block:
        # for each CmdSwitchKind we might have one or two text flags, iterate
        # through each switch and add one or two text -> switch entries
        var temp: array[CmdSwitchTextKind, CmdSwitchKind]
        for cmd, txts in cmdSwitchToTxt.pairs:
          for txt in txts.items:
            temp[txt] = cmd
        temp


proc splitSwitch(switch: string, cmd, arg: var string): bool =
  ## splits a `switch` string into constituent parts populated into the out
  ## params `cmd` and `arg`; `false` is returned if errors are encountered with
  ## `cmd` and `arg` containing incomplete results, otherwise `true`.
  result = true # assume it'll work
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
  else: result = false

type
  ProcessNoteResultKind* = enum
    procNoteSuccess
    procNoteInvalidOption
    procNoteInvalidHint
    procNoteInvalidWarning
    procNoteExpectedOnOrOff
    procNoteOnlyAllOffSupported

  ProcessNoteResult* = object
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
  # TODO: updated to specify hint vs warn, etc in the return value
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

  if i == arg.len: discard
  elif i < arg.len and (arg[i] in {':', '='}): inc(i)
  else: return ProcessNoteResult(kind: procNoteInvalidOption, switch: orig)

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

proc processCompile(conf: ConfigRef; filename: string) =
  # TODO: inline
  var found = findFile(conf, filename)
  if found.isEmpty: found = AbsoluteFile filename
  extccomp.addExternalFileToCompile(conf, found)

# List of GC names for the error generation. It cannot be created
# from enum set using `getEnumNames` because nim cmdline has
# multiple names for the same garbage collector.
const
  gcNames = @[
    "native", "boehm", "refc", "markandsweep", "destructors", "arc", "orc",
    "hooks", "go", "none", "stack", "regions",]

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

func allowedCompileOptionsArgs*(switch: CmdSwitchKind): seq[string] =
  # xxx: make this exhaustive somehow
  case switch
  of cmdSwitchBackend     : validBackends.toSeq.mapIt($it)
  of cmdSwitchGc          : gcNames
  of cmdSwitchDebugger    : @["native", "gdb", "on", "off"]
  of cmdSwitchOpt         : @["speed", "size", "none"]
  of cmdSwitchApp         : @["gui", "console", "lib", "staticlib"]
  of cmdSwitchOs          : platform.listOSnames()
  of cmdSwitchCpu         : platform.listCPUnames()
  of cmdSwitchVerbosity   : @["0", "1", "2", "3"]
  of cmdSwitchIncremental : @["on", "off", "writeonly", "readonly", "v2", "stress"]
  of cmdSwitchCc          : listCCnames()
  of cmdSwitchFilenames   : @["abs", "canonical", "legacyRelProj"]
  of cmdSwitchProcessing  : @["dots", "filenames", "off"]
  of cmdSwitchExperimental: experimentalFeatures.toSeq.mapIt($it)
  of cmdSwitchExceptions  : @["native", "goto"]
  of cmdSwitchStylecheck  : @["off", "hint", "error", "usages"]
  else: unreachable("this is a compiler bug")

func allowedCompileOptionArgs*(switch: string): seq[string] =
  let s =
    try:
      parseEnum[CmdSwitchTextKind](switch.normalize)
    except ValueError:
      unreachable("not really, this is a compiler bug")
  allowedCompileOptionsArgs(cmdTxtToSwitch[s])

func testCompileOptionArg*(conf: ConfigRef; switch, arg: string): CompileOptArgCheckResult =
  template asResult(exp: bool): CompileOptArgCheckResult =
    {.line.}:
      if exp: compileOptArgCheckSuccessTrue
      else:   compileOptArgCheckSuccessFalse

  case switch.normalize
  of "gc":
    case arg.normalize
    of "native": asResult conf.selectedGC == gcNative
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

proc specialDefine(conf: ConfigRef, key: string; pass: TCmdLinePass) =
  # Keep this syncronized with the default config/nim.cfg!
  if cmpIgnoreStyle(key, "release") == 0 or cmpIgnoreStyle(key, "danger") == 0:
    if pass in {passCmd1, passPP}:
      conf.excl {optStackTrace, optLineTrace, optLineDir, optOptimizeSize}
      conf.excl {optExcessiveStackTrace, optCDebug}
      conf.incl optOptimizeSpeed
  if cmpIgnoreStyle(key, "danger") == 0 or cmpIgnoreStyle(key, "quick") == 0:
    if pass in {passCmd1, passPP}:
      conf.excl {optObjCheck, optFieldCheck, optRangeCheck, optBoundsCheck,
        optOverflowCheck, optAssert, optStackTrace, optLineTrace, optLineDir}
      conf.excl {optCDebug}

type
  # TODO: standardize on "flag"
  ProcSwitchResultKind* = enum
    procSwitchSuccess
    procSwitchErrInvalid
    procSwitchErrArgExpected             ## flag expected argument
    procSwitchErrArgForbidden            ## flag expected no arguments
    procSwitchErrArgExpectedFromList     ## no arg given, expect form list
    procSwitchErrArgNotInValidList       ## `--flag:v` where `v` is not an
                                         ## allowed value
    procSwitchErrArgPathInvalid
    procSwitchErrArgMalformedKeyValPair  ## `--define:abc=def` where `abc=def`
                                         ## is malformed syntax
    procSwitchErrArgExpectedOnOrOff
    procSwitchErrArgExpectedOnOffOrList
    procSwitchErrArgExpectedAllOrOff     ## flag expected 'on'/'off', or 'list'
                                         ## of values
    procSwitchErrArgUnknownCCompiler
    procSwitchErrArgUnknownExperimentalFeature
    procSwitchErrArgNimblePath
    procSwitchErrArgInvalidHintOrWarning ## rest is under `ProcessNoteResult`

  ProcSwitchResult* = object
    srcCodeOrigin*: InstantiationInfo
    givenSwitch*, givenArg*: string  # xxx: shouldn't be needed
    case switch*: CmdSwitchKind:     ## the switch being processed, ignored if
                                     ## `kind` is `procSwitchErrInvalid`
      of cmdSwitchNimblepath:
        processedNimblePath*: ProcSwitchNimblePathResult
      else:
        discard
    case kind*: ProcSwitchResultKind:
      of procSwitchSuccess:
        # Note: if expanding with more info, then multi-level variant might be
        #       better, first for switch validity and then for arg validity
        discard # maybe expand with info about what happened
      of procSwitchErrInvalid:
        discard # couldn't match it to any switch
      of procSwitchErrArgExpected,
          procSwitchErrArgForbidden,
          procSwitchErrArgMalformedKeyValPair,
          procSwitchErrArgExpectedOnOrOff,
          procSwitchErrArgExpectedOnOffOrList,
          procSwitchErrArgExpectedAllOrOff,
          procSwitchErrArgExpectedFromList,
          procSwitchErrArgNotInValidList,
          procSwitchErrArgUnknownCCompiler,
          procSwitchErrArgUnknownExperimentalFeature,
          procSwitchErrArgNimblePath:
        discard # givenArg covers this
      of procSwitchErrArgPathInvalid:
        pathAttempted*: string
      of procSwitchErrArgInvalidHintOrWarning:
        processNoteResult*: ProcessNoteResult

  ProcSwitchNimblePathResult* = object
    case didProcess*: bool:
      of true:
        nimblePathAttempted*: AbsoluteDir
        nimblePathResult*: NimblePathResult
      of false:
        discard
  
  ProcSwitchResultErrorKind* =
    range[procSwitchErrInvalid..procSwitchErrArgInvalidHintOrWarning]

const procSwitchResultErrorKinds* =
  {procSwitchErrInvalid..procSwitchErrArgInvalidHintOrWarning}

proc processSwitch*(switch, arg: string, pass: TCmdLinePass,
                    conf: ConfigRef): ProcSwitchResult =
  var
    key, val: string
    switchSet = false
  # xxx: shouldn't need these by further specifying cases where these differ
  #      from the params provided by the caller

  defer:
    assert switchSet, "must be set prior to finish processing"

  template setSwitchAndSrc(s: CmdSwitchKind) =
    assert not switchSet, "this should only be called once per switch"
    result = ProcSwitchResult(
      switch: s,
      givenSwitch: switch,
      givenArg: arg,
      srcCodeOrigin: instLoc(),
    )
    switchSet = true

  template expectArg(s, arg: string) =
    if arg == "":
      result = ProcSwitchResult(kind: procSwitchErrArgExpected,
                                switch: result.switch,
                                givenSwitch: s,
                                givenArg: arg,
                                srcCodeOrigin: instLoc())
      return

  template expectNoArg(s, arg: string) =
    if arg != "":
      result = ProcSwitchResult(kind: procSwitchErrArgForbidden,
                                switch: result.switch,
                                givenSwitch: s,
                                givenArg: arg,
                                srcCodeOrigin: instLoc())
      return

  template expectArgValue(cond: bool, arg, s: string) =
    if cond:
      result = ProcSwitchResult(kind: procSwitchErrArgExpectedFromList,
                                switch: result.switch,
                                givenSwitch: s,
                                givenArg: arg,
                                srcCodeOrigin: instLoc())
      return

  template invalidArgValue(arg, s: string) =
    result = ProcSwitchResult(kind: procSwitchErrArgNotInValidList,
                              switch: result.switch,
                              givenSwitch: s,
                              givenArg: arg,
                              srcCodeOrigin: instLoc())
    return

  template argProcessPath(conf: ConfigRef; path, s: string,
                          notRelativeToProj = false): AbsoluteDir =
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
      AbsoluteDir conf.pathSubs(p, conf.toFullPath(info).splitFile().dir)
    except ValueError:
      result = ProcSwitchResult(kind: procSwitchErrArgPathInvalid,
                                switch: result.switch,
                                givenSwitch: s,
                                givenArg: arg,
                                pathAttempted: p,
                                srcCodeOrigin: instLoc())
      return

  template argProcessCfgPath(conf: ConfigRef, arg, s: string): AbsoluteDir =
    let
      path = if arg.len > 0 and arg[0] == '"': strutils.unescape(arg)
             else: arg
      info = newLineInfo(conf.commandLineSrcIdx, 0, -1)
      # xxx: we hack commandLineSrcIdx at callers like `nimconf` to get different
      #      info here; rework so it's all handled via returns and remove the
      #      need for info.
      basedir = toFullPath(conf, info).splitFile().dir
      p = if os.isAbsolute(path) or '$' in path:
              path
            else:
              basedir / path
    try:
      AbsoluteDir pathSubs(conf, p, basedir)
    except ValueError:
      result = ProcSwitchResult(kind: procSwitchErrArgPathInvalid,
                                switch: result.switch,
                                givenSwitch: s,
                                givenArg: arg,
                                pathAttempted: p,
                                srcCodeOrigin: instLoc())
      return

  template argSplit(s, arg: string; key, val: var string) =
    if not splitSwitch(arg, key, val):
      result = ProcSwitchResult(kind: procSwitchErrArgMalformedKeyValPair,
                                switch: result.switch,
                                givenSwitch: s,
                                givenArg: arg,
                                srcCodeOrigin: instLoc())
      return

  template switchOn(s, arg: string): bool =
    case arg.normalize
    of "", "on": true
    of "off": false
    else:
      result = ProcSwitchResult(kind: procSwitchErrArgExpectedOnOrOff,
                                switch: result.switch,
                                givenSwitch: s,
                                givenArg: arg,
                                srcCodeOrigin: instLoc())
      return

  template processOnOffSwitch(conf: ConfigRef, op: TOptions, arg, s: string) =
    {.line.}:
      case switchOn(s, arg)
      of true:  conf.incl op
      of false: conf.excl op

  template processOnOffSwitchOrList(conf: ConfigRef, op: TOptions,
                                    arg, s: string): bool =
    var res = false
    case arg.normalize
    of "on": conf.incl op
    of "off": conf.excl op
    of "list": res = true
    else:
      result = ProcSwitchResult(kind: procSwitchErrArgExpectedOnOffOrList,
                                switch: result.switch,
                                givenSwitch: s,
                                givenArg: arg,
                                srcCodeOrigin: instLoc())
      return
    res

  template processOnOffSwitchG(conf: ConfigRef; op: TGlobalOptions,
                               arg, s: string) =
    {.line.}:
      case switchOn(s, arg)
      of true:  conf.incl op
      of false: conf.excl op

  template invalidCmdLineOption(conf: ConfigRef, s: string) =
    switchSet = true
    result = ProcSwitchResult(kind: procSwitchErrInvalid,
                              switch: result.switch,
                              givenSwitch: s,
                              srcCodeOrigin: instLoc())
    return

  template processSpecificNoteAndLog(arg: string, state: TSpecialWord,
                                     pass: TCmdLinePass, orig: string,
                                     conf: ConfigRef) =
    let r = processSpecificNote(arg, state, pass, orig, conf)
    case r.kind
    of procNoteInvalidOption,
        procNoteInvalidHint,
        procNoteInvalidWarning,
        procNoteExpectedOnOrOff,
        procNoteOnlyAllOffSupported:
      result = ProcSwitchResult(kind: procSwitchErrArgInvalidHintOrWarning,
                                switch: result.switch,
                                givenSwitch: orig,
                                givenArg: arg,
                                processNoteResult: r,
                                srcCodeOrigin: instLoc())
      return
    of procNoteSuccess:
      discard "TODO: log a trace for success?"

  case switch.normalize
  of "fromcmd":
    setSwitchAndSrc cmdSwitchFromcmd
    expectNoArg(switch, arg)
    conf.inputMode = pimCmd
  of "path", "p":
    setSwitchAndSrc cmdSwitchPath
    expectArg(switch, arg)
    for path in nimbleSubs(conf, arg):
      let p =
        case pass
        of passPP: argProcessCfgPath(conf, path, switch)
        else:      argProcessPath(conf, path, switch)
      conf.addPath p
  of "nimblepath":
    setSwitchAndSrc cmdSwitchNimblepath
    if pass in {passCmd2, passPP} and optNoNimblePath notin conf.globalOptions:
      expectArg(switch, arg)
      var path = argProcessPath(conf, arg, switch, notRelativeToProj=true)
      # TODO: move up nimble stuff, then set path once
      let nimbleDir = AbsoluteDir getEnv("NIMBLE_DIR")
      if not nimbleDir.isEmpty and pass == passPP:
        path = nimbleDir / RelativeDir"pkgs"
      let res = nimblePath(conf, path)
      result.processedNimblePath =
        ProcSwitchNimblePathResult(didProcess: true,
                                   nimblePathAttempted: path,
                                   nimblePathResult: res)
      if res.pkgs.anyIt(it.status == nimblePkgInvalid):
        result = ProcSwitchResult(
          kind: procSwitchErrArgNimblePath,
          switch: cmdSwitchNimblepath,
          processedNimblePath: result.processedNimblePath,
          givenSwitch: result.givenSwitch,
          givenArg: result.givenArg,
          srcCodeOrigin: instLoc())
  of "nonimblepath":
    setSwitchAndSrc cmdSwitchNonimblepath
    expectNoArg(switch, arg)
    disableNimblePath(conf)
  of "clearnimblepath":
    setSwitchAndSrc cmdSwitchClearnimblepath
    expectNoArg(switch, arg)
    clearNimblePath(conf)
  of "excludepath":
    setSwitchAndSrc cmdSwitchExcludepath
    expectArg(switch, arg)
    let path = argProcessPath(conf, arg, switch)
    conf.searchPaths = conf.searchPaths.filterIt(it != path)
    conf.lazyPaths = conf.lazyPaths.filterIt(it != path)
  of "nimcache":
    setSwitchAndSrc cmdSwitchNimcache
    expectArg(switch, arg)
    var arg = arg
    # refs bug #18674, otherwise `--os:windows` messes up with `--nimcache` set
    # in config nims files, e.g. via: `import os; switch("nimcache", "/tmp/somedir")`
    if conf.target.targetOS == osWindows and DirSep == '/':
      arg = arg.replace('\\', '/')
    conf.nimcacheDir = argProcessPath(conf, arg, switch,
                                      notRelativeToProj=true)
  of "out", "o":
    setSwitchAndSrc cmdSwitchOut
    expectArg(switch, arg)
    let f = splitFile(string argProcessPath(conf, arg, switch,
                                            notRelativeToProj=true))
    conf.outFile = RelativeFile f.name & f.ext
    conf.outDir = toAbsoluteDir f.dir
  of "outdir":
    setSwitchAndSrc cmdSwitchOutdir
    expectArg(switch, arg)
    conf.outDir = argProcessPath(conf, arg, switch, notRelativeToProj=true)
  of "depfile":
    setSwitchAndSrc cmdSwitchDepfile
    expectArg(switch, arg)
    conf.depfile =
      AbsoluteFile argProcessPath(conf, arg, switch, notRelativeToProj=true)
  of "usenimcache":
    setSwitchAndSrc cmdSwitchUsenimcache
    processOnOffSwitchG(conf, {optUseNimcache}, arg, switch)
  of "docseesrcurl":
    setSwitchAndSrc cmdSwitchDocseesrcurl
    expectArg(switch, arg)
    conf.docSeeSrcUrl = arg
  of "docroot":
    setSwitchAndSrc cmdSwitchDocroot
    conf.docRoot = if arg.len == 0: docRootDefault else: arg
  of "backend", "b":
    setSwitchAndSrc cmdSwitchBackend
    let backend = parseEnum(arg.normalize, backendInvalid)
    expectArgValue(backend == backendInvalid, arg, switch)
    conf.backend = backend
  of "doccmd":
    setSwitchAndSrc cmdSwitchDoccmd # xxx: not a flag... sigh
    conf.docCmd = arg
  of "define", "d":
    setSwitchAndSrc cmdSwitchDefine
    expectArg(switch, arg)
    if {':', '='} in arg:
      argSplit(switch, arg, key, val)
      specialDefine(conf, key, pass)
      defineSymbol(conf, key, val)
    else:
      specialDefine(conf, arg, pass)
      defineSymbol(conf, arg)
  of "undef", "u":
    setSwitchAndSrc cmdSwitchUndef
    expectArg(switch, arg)
    undefSymbol(conf, arg)
  of "compile":
    setSwitchAndSrc cmdSwitchCompile
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}: processCompile(conf, arg)
  of "link":
    setSwitchAndSrc cmdSwitchLink
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}:
      addExternalFileToLink(conf, AbsoluteFile arg)
  of "debuginfo":
    setSwitchAndSrc cmdSwitchDebuginfo
    processOnOffSwitchG(conf, {optCDebug}, arg, switch)
  of "embedsrc":
    setSwitchAndSrc cmdSwitchEmbedsrc
    processOnOffSwitchG(conf, {optEmbedOrigSrc}, arg, switch)
  of "compileonly", "c":
    setSwitchAndSrc cmdSwitchCompileonly
    processOnOffSwitchG(conf, {optCompileOnly}, arg, switch)
  of "nolinking":
    setSwitchAndSrc cmdSwitchNolinking
    processOnOffSwitchG(conf, {optNoLinking}, arg, switch)
  of "nomain":
    setSwitchAndSrc cmdSwitchNomain
    processOnOffSwitchG(conf, {optNoMain}, arg, switch)
  of "forcebuild", "f":
    setSwitchAndSrc cmdSwitchForcebuild
    processOnOffSwitchG(conf, {optForceFullMake}, arg, switch)
  of "project":
    setSwitchAndSrc cmdSwitchProject
    processOnOffSwitchG(conf, {optWholeProject, optGenIndex}, arg, switch)
  of "gc":
    setSwitchAndSrc cmdSwitchGc
    if conf.backend in {backendJs, backendNimVm}: return # for: bug #16033
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}:
      case arg.normalize
      of "native":
        conf.selectedGC = gcNative
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
        invalidArgValue(arg, switch)
  of "warnings", "w":
    setSwitchAndSrc cmdSwitchWarnings
    if processOnOffSwitchOrList(conf, {optWarns}, arg, switch):
      listWarnings(conf) # TODO: replace reports, must work in cli/cfg/fold
  of "warning":
    setSwitchAndSrc cmdSwitchWarning
    processSpecificNoteAndLog(arg, wWarning, pass, switch, conf)
  of "hint":
    setSwitchAndSrc cmdSwitchHint
    processSpecificNoteAndLog(arg, wHint, pass, switch, conf)
  of "warningaserror":
    setSwitchAndSrc cmdSwitchWarningaserror
    processSpecificNoteAndLog(arg, wWarningAsError, pass, switch, conf)
  of "hintaserror":
    setSwitchAndSrc cmdSwitchHintaserror
    processSpecificNoteAndLog(arg, wHintAsError, pass, switch, conf)
  of "hints":
    setSwitchAndSrc cmdSwitchHints
    if processOnOffSwitchOrList(conf, {optHints}, arg, switch):
      listHints(conf) # TODO: replace reports, must work in cli/cfg/fold
  of "threadanalysis":
    setSwitchAndSrc cmdSwitchThreadanalysis
    if conf.backend == backendJs: discard
    else:
      processOnOffSwitchG(conf, {optThreadAnalysis}, arg, switch)
  of "stacktrace":
    setSwitchAndSrc cmdSwitchStacktrace
    processOnOffSwitch(conf, {optStackTrace}, arg, switch)
  of "stacktracemsgs":
    setSwitchAndSrc cmdSwitchStacktracemsgs
    processOnOffSwitch(conf, {optStackTraceMsgs}, arg, switch)
  of "excessivestacktrace":
    setSwitchAndSrc cmdSwitchExcessivestacktrace
    processOnOffSwitchG(conf, {optExcessiveStackTrace}, arg, switch)
  of "linetrace":
    setSwitchAndSrc cmdSwitchLinetrace
    processOnOffSwitch(conf, {optLineTrace}, arg, switch)
  of "debugger":
    setSwitchAndSrc cmdSwitchDebugger
    case arg.normalize
    of "on", "native", "gdb":
      conf.incl optCDebug
      conf.incl optLineDir
      #defineSymbol(conf.symbols, "nimTypeNames") # type names are used in gdb pretty printing
    of "off":
      conf.excl optCDebug
    else:
      invalidArgValue(arg, switch)
  of "profiler":
    setSwitchAndSrc cmdSwitchProfiler
    processOnOffSwitch(conf, {optProfiler}, arg, switch)
    if optProfiler in conf.options:
      defineSymbol(conf, "profiler")
    else:
      undefSymbol(conf, "profiler")
  of "memtracker":
    setSwitchAndSrc cmdSwitchMemtracker
    processOnOffSwitch(conf, {optMemTracker}, arg, switch)
    if optMemTracker in conf.options:
      defineSymbol(conf, "memtracker")
    else:
      undefSymbol(conf, "memtracker")
  of "checks", "x":
    setSwitchAndSrc cmdSwitchChecks
    processOnOffSwitch(conf, ChecksOptions, arg, switch)
  of "floatchecks":
    setSwitchAndSrc cmdSwitchFloatchecks
    processOnOffSwitch(conf, {optNaNCheck, optInfCheck}, arg, switch)
  of "infchecks":
    setSwitchAndSrc cmdSwitchInfchecks
    processOnOffSwitch(conf, {optInfCheck}, arg, switch)
  of "nanchecks":
    setSwitchAndSrc cmdSwitchNanchecks
    processOnOffSwitch(conf, {optNaNCheck}, arg, switch)
  of "objchecks":
    setSwitchAndSrc cmdSwitchObjchecks
    processOnOffSwitch(conf, {optObjCheck}, arg, switch)
  of "fieldchecks":
    setSwitchAndSrc cmdSwitchFieldchecks
    processOnOffSwitch(conf, {optFieldCheck}, arg, switch)
  of "rangechecks":
    setSwitchAndSrc cmdSwitchRangechecks
    processOnOffSwitch(conf, {optRangeCheck}, arg, switch)
  of "boundchecks":
    setSwitchAndSrc cmdSwitchBoundchecks
    processOnOffSwitch(conf, {optBoundsCheck}, arg, switch)
  of "overflowchecks":
    setSwitchAndSrc cmdSwitchOverflowchecks
    processOnOffSwitch(conf, {optOverflowCheck}, arg, switch)
  of "staticboundchecks":
    setSwitchAndSrc cmdSwitchStaticboundchecks
    processOnOffSwitch(conf, {optStaticBoundsCheck}, arg, switch)
  of "stylechecks":
    setSwitchAndSrc cmdSwitchStylechecks
    processOnOffSwitch(conf, {optStyleCheck}, arg, switch)
  of "linedir":
    setSwitchAndSrc cmdSwitchLinedir
    processOnOffSwitch(conf, {optLineDir}, arg, switch)
  of "assertions", "a":
    setSwitchAndSrc cmdSwitchAssertions
    processOnOffSwitch(conf, {optAssert}, arg, switch)
  of "threads":
    setSwitchAndSrc cmdSwitchThreads
    if conf.backend == backendJs:
      discard
    else:
      processOnOffSwitchG(conf, {optThreads}, arg, switch)
    #if optThreads in conf.globalOptions: conf.setNote(warnGcUnsafe)
  of "tlsemulation":
    setSwitchAndSrc cmdSwitchTlsemulation
    processOnOffSwitchG(conf, {optTlsEmulation}, arg, switch)
  of "implicitstatic":
    setSwitchAndSrc cmdSwitchImplicitstatic
    processOnOffSwitch(conf, {optImplicitStatic}, arg, switch)
  of "trmacros":
    setSwitchAndSrc cmdSwitchTrmacros
    processOnOffSwitch(conf, {optTrMacros}, arg, switch)
  of "opt":
    setSwitchAndSrc cmdSwitchOpt
    expectArg(switch, arg)
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
      invalidArgValue(arg, switch)
  of "app":
    setSwitchAndSrc cmdSwitchApp
    expectArg(switch, arg)
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
      invalidArgValue(arg, switch)
  of "passc", "t":
    setSwitchAndSrc cmdSwitchPassc
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}: extccomp.addCompileOptionCmd(conf, arg)
  of "passl", "l":
    setSwitchAndSrc cmdSwitchPassl
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}: extccomp.addLinkOptionCmd(conf, arg)
  of "cincludes":
    setSwitchAndSrc cmdSwitchCincludes
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}:
      conf.cIncludesAdd argProcessPath(conf, arg, switch)
  of "clibdir":
    setSwitchAndSrc cmdSwitchClibdir
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}:
      conf.cLibsAdd argProcessPath(conf, arg, switch)
  of "clib":
    setSwitchAndSrc cmdSwitchClib
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}:
      conf.cLinkedLibsAdd argProcessPath(conf, arg, switch).string
  of "header":
    setSwitchAndSrc cmdSwitchHeader
    if conf != nil: conf.headerFile = arg
    incl(conf, optGenIndex)
  of "index":
    setSwitchAndSrc cmdSwitchIndex
    processOnOffSwitchG(conf, {optGenIndex}, arg, switch)
  of "import":
    setSwitchAndSrc cmdSwitchImport
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}:
      let info = newLineInfo(conf.commandLineSrcIdx, 0, -1)
      conf.implicitImportsAdd findModule(
        conf, arg, toFullPath(conf, info)).string
  of "include":
    setSwitchAndSrc cmdSwitchInclude
    expectArg(switch, arg)
    if pass in {passCmd2, passPP}:
      # xxx: pretty sure this should do path validation
      let info = newLineInfo(conf.commandLineSrcIdx, 0, -1)
      conf.implicitIncludesAdd findModule(
        conf, arg, toFullPath(conf, info)).string
  of "listcmd":
    setSwitchAndSrc cmdSwitchListcmd
    processOnOffSwitchG(conf, {optListCmd}, arg, switch)
  of "asm":
    setSwitchAndSrc cmdSwitchAsm
    processOnOffSwitchG(conf, {optProduceAsm}, arg, switch)
  of "genmapping":
    setSwitchAndSrc cmdSwitchGenmapping
    processOnOffSwitchG(conf, {optGenMapping}, arg, switch)
  of "os":
    setSwitchAndSrc cmdSwitchOs
    expectArg(switch, arg)
    let theOS = platform.nameToOS(arg)
    if theOS == osNone:
      invalidArgValue(arg, switch)
    else:
      conf.target =
        block:
          var t = conf.target
          setTarget(t, theOS, conf.target.targetCPU)
          t
  of "cpu":
    setSwitchAndSrc cmdSwitchCpu
    expectArg(switch, arg)
    let cpu = platform.nameToCPU(arg)
    if cpu == cpuNone:
      invalidArgValue(arg, switch)
    else:
      conf.target =
        block:
          var t = conf.target
          setTarget(t, conf.target.targetOS, cpu)
          t
  of "run", "r":
    setSwitchAndSrc cmdSwitchRun
    processOnOffSwitchG(conf, {optRun}, arg, switch)
  of "maxloopiterationsvm":
    setSwitchAndSrc cmdSwitchMaxloopiterationsvm
    expectArg(switch, arg)
    conf.maxLoopIterationsVM = parseInt(arg)
  of "errormax":
    setSwitchAndSrc cmdSwitchErrormax
    expectArg(switch, arg)
    # Note: `nim check` (etc) can overwrite this.
    # `0` is meaningless, give it a useful meaning as in clang's -ferror-limit
    # If user doesn't set this flag and the code doesn't either, it'd
    # have the same effect as errorMax = 1
    let ret = parseInt(arg)
    conf.errorMax = if ret == 0: high(int) else: ret
  of "verbosity":
    setSwitchAndSrc cmdSwitchVerbosity
    expectArg(switch, arg)
    let verbosity = parseInt(arg)
    case verbosity
    of 0: conf.verbosity = compVerbosityMin
    of 1: conf.verbosity = compVerbosityDefault
    of 2: conf.verbosity = compVerbosityHigh
    of 3: conf.verbosity = compVerbosityMax
    else:
      invalidArgValue(arg, switch)
    let verb = NotesVerbosity.main[conf.verbosity]
    ## We override the default `verb` by explicitly modified (set/unset) notes.
    conf.notes = (conf.modifiedyNotes * conf.notes + verb) -
      (conf.modifiedyNotes * verb - conf.notes)
    conf.mainPackageNotes = conf.notes
  of "parallelbuild":
    setSwitchAndSrc cmdSwitchParallelbuild
    expectArg(switch, arg)
    conf.numberOfProcessors = parseInt(arg)
  of "incremental", "ic":
    setSwitchAndSrc cmdSwitchIncremental
    if pass in {passCmd2, passPP}:
      case arg.normalize
      of "on": conf.symbolFiles = v2Sf
      of "off": conf.symbolFiles = disabledSf
      of "writeonly": conf.symbolFiles = writeOnlySf
      of "readonly": conf.symbolFiles = readOnlySf
      of "v2": conf.symbolFiles = v2Sf
      of "stress": conf.symbolFiles = stressTest
      else:
        invalidArgValue(arg, switch)
    setUseIc(conf.symbolFiles != disabledSf)
  of "skipcfg":
    setSwitchAndSrc cmdSwitchSkipcfg
    processOnOffSwitchG(conf, {optSkipSystemConfigFile}, arg, switch)
  of "skipprojcfg":
    setSwitchAndSrc cmdSwitchSkipprojcfg
    processOnOffSwitchG(conf, {optSkipProjConfigFile}, arg, switch)
  of "skipusercfg":
    setSwitchAndSrc cmdSwitchSkipusercfg
    processOnOffSwitchG(conf, {optSkipUserConfigFile}, arg, switch)
  of "skipparentcfg":
    setSwitchAndSrc cmdSwitchSkipparentcfg
    processOnOffSwitchG(conf, {optSkipParentConfigFiles}, arg, switch)
  of "genscript":
    setSwitchAndSrc cmdSwitchGenscript
    processOnOffSwitchG(conf, {optGenScript}, arg, switch)
    processOnOffSwitchG(conf, {optCompileOnly}, arg, switch)
  of "colors":
    setSwitchAndSrc cmdSwitchColors
    processOnOffSwitchG(conf, {optUseColors}, arg, switch)
  of "lib":
    setSwitchAndSrc cmdSwitchLib
    expectArg(switch, arg)
    conf.libpath = argProcessPath(conf, arg, switch, notRelativeToProj=true)
  of "putenv":
    setSwitchAndSrc cmdSwitchPutenv
    expectArg(switch, arg)
    argSplit(switch, arg, key, val)
    os.putEnv(key, val)
  of "cc":
    setSwitchAndSrc cmdSwitchCc
    expectArg(switch, arg)
    case setCC(conf, arg)
    of ccNone:
      result = ProcSwitchResult(kind: procSwitchErrArgUnknownCCompiler,
                                switch: result.switch,
                                givenSwitch: switch,
                                givenArg: arg,
                                srcCodeOrigin: instLoc())
      return
    else:
      discard "valid compiler set"
  of "stdout":
    setSwitchAndSrc cmdSwitchStdout
    processOnOffSwitchG(conf, {optStdout}, arg, switch)
  of "filenames":
    setSwitchAndSrc cmdSwitchFilenames
    case arg.normalize
    of "abs": conf.filenameOption = foAbs
    of "canonical": conf.filenameOption = foCanonical
    of "legacyrelproj": conf.filenameOption = foLegacyRelProj
    else:
      invalidArgValue(arg, switch)
  of "processing":
    setSwitchAndSrc cmdSwitchProcessing
    incl(conf, cnCurrent, rsemProcessing)
    incl(conf, cnMainPackage, rsemProcessing)
    case arg.normalize
    of "dots": conf.hintProcessingDots = true
    of "filenames": conf.hintProcessingDots = false
    of "off":
      excl(conf, cnCurrent, rsemProcessing)
      excl(conf, cnMainPackage, rsemProcessing)
    else:
      invalidArgValue(arg, switch)
  of "unitsep":
    setSwitchAndSrc cmdSwitchUnitsep
    conf.unitSep = if switchOn(switch.normalize, arg): "\31" else: ""
  of "listfullpaths":
    setSwitchAndSrc cmdSwitchListfullpaths
    # xxx: this should probably get subsubed with filenames
    conf.filenameOption =
      if switchOn(switch.normalize, arg): foAbs
      else:                               foCanonical
  of "spellsuggest":
    setSwitchAndSrc cmdSwitchSpellsuggest
    if arg.len == 0: conf.spellSuggestMax = spellSuggestSecretSauce
    elif arg == "auto": conf.spellSuggestMax = spellSuggestSecretSauce
    else: conf.spellSuggestMax = parseInt(arg)
  of "declaredlocs":
    setSwitchAndSrc cmdSwitchDeclaredlocs
    processOnOffSwitchG(conf, {optDeclaredLocs}, arg, switch)
  of "dynliboverride":
    setSwitchAndSrc cmdSwitchDynliboverride
    case pass
    of passCmd2, passPP:
      expectArg(switch, arg)
      options.inclDynlibOverride(conf, arg)
    else:
      discard
  of "dynliboverrideall":
    setSwitchAndSrc cmdSwitchDynliboverrideall
    processOnOffSwitchG(conf, {optDynlibOverrideAll}, arg, switch)
  of "experimental":
    setSwitchAndSrc cmdSwitchExperimental
    if arg.len == 0:
      conf.incl oldExperimentalFeatures
    else:
      try:
        conf.incl parseEnum[Feature](arg)
      except ValueError:
        result = ProcSwitchResult(
                    kind: procSwitchErrArgUnknownExperimentalFeature,
                    switch: result.switch,
                    givenSwitch: switch,
                    givenArg: arg,
                    srcCodeOrigin: instLoc())
        return
  of "exceptions":
    setSwitchAndSrc cmdSwitchExceptions
    case arg.normalize
    of "native": conf.exc = excNative
    of "goto": conf.exc = excGoto
    else:
      invalidArgValue(arg, switch)
  of "cppdefine":
    setSwitchAndSrc cmdSwitchCppdefine
    expectArg(switch, arg)
    if conf != nil:
      conf.cppDefine(arg)
  of "seqsv2":
    setSwitchAndSrc cmdSwitchSeqsv2
    processOnOffSwitchG(conf, {optSeqDestructors}, arg, switch)
    if pass in {passCmd2, passPP}:
      defineSymbol(conf, "nimSeqsV2")
  of "stylecheck":
    setSwitchAndSrc cmdSwitchStylecheck
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
      invalidArgValue(arg, switch)
  of "showallmismatches":
    setSwitchAndSrc cmdSwitchShowallmismatches
    processOnOffSwitchG(conf, {optShowAllMismatches}, arg, switch)
  of "docinternal":
    setSwitchAndSrc cmdSwitchDocinternal
    processOnOffSwitchG(conf, {optDocInternal}, arg, switch)
  of "multimethods":
    setSwitchAndSrc cmdSwitchMultimethods
    processOnOffSwitchG(conf, {optMultiMethods}, arg, switch)
  of "expandmacro":
    setSwitchAndSrc cmdSwitchExpandmacro
    expectArg(switch, arg)
    conf.macrosToExpand[arg] = "T"
  of "expandarc":
    setSwitchAndSrc cmdSwitchExpandarc
    expectArg(switch, arg)
    conf.arcToExpand[arg] = "T"
  of "benchmarkvm":
    setSwitchAndSrc cmdSwitchBenchmarkvm
    processOnOffSwitchG(conf, {optBenchmarkVM}, arg, switch)
  of "profilevm":
    setSwitchAndSrc cmdSwitchProfilevm
    processOnOffSwitchG(conf, {optProfileVM}, arg, switch)
  of "sinkinference":
    setSwitchAndSrc cmdSwitchSinkinference
    processOnOffSwitch(conf, {optSinkInference}, arg, switch)
  of "cursorinference":
    setSwitchAndSrc cmdSwitchCursorinference
    # undocumented, for debugging purposes only:
    processOnOffSwitch(conf, {optCursorInference}, arg, switch)
  of "panics":
    setSwitchAndSrc cmdSwitchPanics
    processOnOffSwitchG(conf, {optPanics}, arg, switch)
    if optPanics in conf.globalOptions:
      defineSymbol(conf, "nimPanics")
  of "sourcemap": # xxx document in --fullhelp
    setSwitchAndSrc cmdSwitchSourcemap
    conf.incl optSourcemap
    conf.incl optLineDir
  of "deepcopy":
    setSwitchAndSrc cmdSwitchDeepcopy
    processOnOffSwitchG(conf, {optEnableDeepCopy}, arg, switch)
  of "": # comes from "-" in for example: `nim c -r -` (gets stripped from -)
    setSwitchAndSrc cmdSwitchProjStdin
    conf.inputMode = pimStdin
  of "cmdexitgcstats":
    setSwitchAndSrc cmdSwitchCmdexitgcstats
    # Print GC statistics for the compiler run
    conf.incl optCmdExitGcStats
  else:
    if strutils.find(switch, '.') >= 0:
      setSwitchAndSrc cmdSwitchConfigVar
      # xxx: this should change: eats up way too much namespace and user typos
      #      are trivially silienced, perhaps `-s`?
      options.setConfigVar(conf, switch, arg)
    else: invalidCmdLineOption(conf, switch)

proc processSwitch*(pass: TCmdLinePass, p: OptParser,
                    config: ConfigRef): ProcSwitchResult =
  # hint[X]:off is parsed as (p.key = "hint[X]", p.val = "off")
  # we transform it to (key = hint, val = [X]:off)
  let bracketLe = strutils.find(p.key, '[')
  if bracketLe >= 0:
    let
      key = substr(p.key, 0, bracketLe - 1)
      val = substr(p.key, bracketLe) & ':' & p.val
    processSwitch(key, val, pass, config)
  else:
    processSwitch(p.key, p.val, pass, config)

func procResultToHumanStr*(procResult: ProcSwitchResult): string =
  ## creates a human readable string message for a switch result, does not
  ## include any extra information such as line info, severity, and so on.
  case procResult.kind
  of procSwitchSuccess: ""
  of procSwitchErrInvalid:
    "Invalid command line option - " & procResult.givenSwitch
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
      of procNoteSuccess: ""
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

proc setCmd*(conf: ConfigRef, cmd: Command) =
  ## sets cmd, backend so subsequent flags can query it (e.g. so --gc:arc can be ignored for backendJs)
  # Note that `--backend` can override the backend, so the logic here must remain reversible.
  conf.cmd = cmd
  case cmd
  of cmdCompileToC, cmdCrun, cmdTcc: conf.backend = backendC
  of cmdCompileToJS: conf.backend = backendJs
  of cmdCompileToVM: conf.backend = backendNimVm
  else: discard

proc parseCommand(command: string): Command =
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