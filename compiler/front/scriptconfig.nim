#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Implements the new configuration system for Nim. Uses Nim as a scripting
## language.

import
  std/[
    os,
    times,
    osproc,
    strtabs,
  ],
  compiler/ast/[
    ast,
    idents,
    lineinfos,
    wordrecg,
    llstream,
  ],
  compiler/modules/[
    modules,
    modulegraphs,
  ],
  compiler/sem/[
    passes,
    sem,
  ],
  compiler/vm/[
    compilerbridge,
    vmconv,
    vmdef,
    vmhooks,
    vmops
  ],
  compiler/front/[
    msgs,
    condsyms,
    options,
    commands,
  ],
  compiler/utils/[
    pathutils
  ]

from compiler/vm/vmlegacy import legacyReportsVmTracer

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_debug import DebugReport
from compiler/ast/reports_external import ExternalReport
from compiler/ast/report_enums import ReportKind

# we support 'cmpIgnoreStyle' natively for efficiency:
from std/strutils import cmpIgnoreStyle, contains

proc listDirs(a: VmArgs, filter: set[PathComponent]) =
  let dir = getString(a, 0)
  var result: seq[string] = @[]
  for kind, path in walkDir(dir):
    if kind in filter: result.add path

  writeTo(result, a.getResultHandle(), a.mem[])

proc processSingleNote(arg: string, state: TSpecialWord, info: TLineInfo,
                       orig: string; conf: ConfigRef) =
  let r = processSpecificNote(arg, state, passPP, orig, conf)
  case r.kind
  of procNoteInvalidOption:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidCommandLineOption,
      cmdlineProvided: r.switch))
  of procNoteInvalidHint:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidHint,
      cmdlineProvided: r.invalidHintOrWarning))
  of procNoteInvalidWarning:
    conf.localReport(info, ExternalReport(
      kind: rextInvalidWarning,
      cmdlineProvided: r.invalidHintOrWarning))
  of procNoteExpectedOnOrOff:
    conf.localReport(info, ExternalReport(kind: rextExpectedOnOrOff,
                                          cmdlineSwitch: r.switch,
                                          cmdlineProvided: r.argVal))
  of procNoteOnlyAllOffSupported:
    conf.localReport(info, ExternalReport(kind: rextOnlyAllOffSupported,
                                          cmdlineSwitch: r.switch,
                                          cmdlineProvided: r.argVal))
  of procNoteSuccess:
    discard

proc processSingleSwitch(switch, arg: string; info: TLineInfo, conf: ConfigRef) =
  let r = processSwitch(switch, arg, passPP, conf)
  case r.kind
  case procResult.kind
  of procSwitchSuccess: discard
  of procSwitchErrInvalid:
    conf.localReport(info):
      ExternaleReport(kind: rextInvalidCommandLineOption,
                      cmdlineProvided: r.givenArg)
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
  of procSwitchErrArgPathInvalid:
    "invalid path (option '$#'): $#" %
      [procResult.givenSwitch, procResult.pathAttempted]
  of procSwitchErrArgInvalidHintOrWarning:
    let processNoteResult = procResult.processNoteResult
    # xxx: clean-up these messages so they're more hint/warning specific,
    #      we have more information available than we're using. eg: it's
    #      not an invalid option, but error/warning/hint/etc switch
    let temp =
      case processNoteResult.kind
      of procNoteSuccess: discard
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
  else:
    discard

proc setupVM*(module: PSym; cache: IdentCache; scriptName: string;
              graph: ModuleGraph; idgen: IdGenerator): PEvalContext =
  result = newCtx(module, cache, graph, idgen, legacyReportsVmTracer)
  # for backwards compatibility, allow meta expressions in nimscript (this
  # matches the previous behaviour)
  result.flags = {cgfAllowMeta}
  result.mode = emRepl
  registerBasicOps(result[])
  let conf = graph.config

  # captured vars:
  var errorMsg: string
  var vthisDir = scriptName.splitFile.dir

  template cbconf(name, body) {.dirty.} =
    result.registerCallback "stdlib.system." & astToStr(name),
      proc (a: VmArgs) =
        body

  template cbexc(name, exc, body) {.dirty.} =
    result.registerCallback "stdlib.system." & astToStr(name),
      proc (a: VmArgs) =
        errorMsg = ""
        try:
          body
        except exc:
          errorMsg = getCurrentExceptionMsg()

  template cbos(name, body) {.dirty.} =
    cbexc(name, OSError, body)

  template wrapInCmdLineSrcIdxSwap(body) =
    let oldIdx = conf.commandLineSrcIdx
    conf.commandLineSrcIdx = module.info.fileIndex
    body
    conf.commandLineSrcIdx = oldIdx

  # Idea: Treat link to file as a file, but ignore link to directory to prevent
  # endless recursions out of the box.
  cbos listFilesImpl:
    listDirs(a, {pcFile, pcLinkToFile})
  cbos listDirsImpl:
    listDirs(a, {pcDir})
  cbos removeDir:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      os.removeDir(getString(a, 0), getBool(a, 1))
  cbos removeFile:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      os.removeFile getString(a, 0)
  cbos createDir:
    os.createDir getString(a, 0)

  result.registerCallback "stdlib.system.getError",
    proc (a: VmArgs) = setResult(a, errorMsg)

  cbos setCurrentDir:
    os.setCurrentDir getString(a, 0)
  cbos getCurrentDir:
    setResult(a, os.getCurrentDir())
  cbos moveFile:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      os.moveFile(getString(a, 0), getString(a, 1))
  cbos moveDir:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      os.moveDir(getString(a, 0), getString(a, 1))
  cbos copyFile:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      os.copyFile(getString(a, 0), getString(a, 1))
  cbos copyDir:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      os.copyDir(getString(a, 0), getString(a, 1))
  cbos getLastModificationTime:
    setResult(a, getLastModificationTime(getString(a, 0)).toUnix)
  cbos findExe:
    setResult(a, os.findExe(getString(a, 0)))

  cbos rawExec:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      setResult(a, osproc.execCmd getString(a, 0))

  cbconf getEnv:
    setResult(a, os.getEnv(a.getString 0, a.getString 1))
  cbconf existsEnv:
    setResult(a, os.existsEnv(a.getString 0))
  cbconf putEnv:
    os.putEnv(a.getString 0, a.getString 1)
  cbconf delEnv:
    os.delEnv(a.getString 0)
  cbconf dirExists:
    setResult(a, os.dirExists(a.getString 0))
  cbconf fileExists:
    setResult(a, os.fileExists(a.getString 0))

  cbconf projectName:
    setResult(a, conf.projectName)
  cbconf projectDir:
    setResult(a, conf.projectPath.string)
  cbconf projectPath:
    setResult(a, conf.projectFull.string)
  cbconf thisDir:
    setResult(a, vthisDir)
  cbconf put:
    options.setConfigVar(conf, getString(a, 0), getString(a, 1))
  cbconf get:
    setResult(a, options.getConfigVar(conf, a.getString 0))
  cbconf exists:
    setResult(a, options.existsConfigVar(conf, a.getString 0))
  cbconf nimcacheDir:
    setResult(a, options.getNimcacheDir(conf).string)
  cbconf paramStr:
    setResult(a, os.paramStr(int a.getInt 0))
  cbconf paramCount:
    setResult(a, os.paramCount())
  cbconf cmpIgnoreStyle:
    setResult(a, strutils.cmpIgnoreStyle(a.getString 0, a.getString 1))
  cbconf cmpIgnoreCase:
    setResult(a, strutils.cmpIgnoreCase(a.getString 0, a.getString 1))
  cbconf setCommand:
    conf.setCommandEarly(a.getString 0)
    let arg = a.getString 1
    incl(conf, optWasNimscript)
    if arg.len > 0: setFromProjectName(conf, arg)
  cbconf getCommand:
    setResult(a, conf.command)
  cbconf switch:
    wrapInCmdLineSrcIdxSwap:
      processSingleSwitch(a.getString 0, a.getString 1, module.info, conf)
  cbconf hintImpl:
    wrapInCmdLineSrcIdxSwap:
      processSingleNote(a.getString 0, wHint, module.info, a.getString 1, conf)
  cbconf warningImpl:
    wrapInCmdLineSrcIdxSwap:
      processSingleNote(a.getString 0, wWarning, module.info, a.getString 1,
                        conf)
  cbconf patchFile:
    let key = a.getString(0) & "_" & a.getString(1)
    var val = a.getString(2).addFileExt(NimExt)
    if {'$', '~'} in val:
      val = pathSubs(conf, val, vthisDir)
    elif not isAbsolute(val):
      val = vthisDir / val
    conf.moduleOverrides[key] = val
  cbconf selfExe:
    setResult(a, os.getAppFilename())
  cbconf cppDefine:
    options.cppDefine(conf, a.getString(0))
  cbexc stdinReadLine, EOFError:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      setResult(a, "")
      setResult(a, stdin.readLine())
  cbexc stdinReadAll, EOFError:
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      setResult(a, "")
      setResult(a, stdin.readAll())

proc runNimScript*(cache: IdentCache; scriptName: AbsoluteFile;
                   freshDefines=true; conf: ConfigRef, stream: PLLStream) =

  conf.localReport DebugReport(
    kind: rdbgStartingConfRead,
    filename: scriptName.string
  )

  let oldSymbolFiles = conf.symbolFiles
  conf.symbolFiles = disabledSf

  let graph = newModuleGraph(cache, conf)
  connectCallbacks(graph)
  if freshDefines:
    initDefines(conf.symbols)

  defineSymbol(conf, "nimscript")
  defineSymbol(conf, "nimconfig")
  registerPass(graph, semPass)
  registerPass(graph, evalPass)

  conf.searchPathsAdd(conf.libpath)

  let oldGlobalOptions = conf.globalOptions
  let oldSelectedGC = conf.selectedGC
  undefSymbol(conf, "nimv2")
  conf.excl {optTinyRtti, optSeqDestructors}
  conf.selectedGC = gcUnselected

  var m = graph.makeModule(scriptName)
  incl(m.flags, sfMainModule)
  var vm = setupVM(m, cache, scriptName.string, graph, graph.idgen)
  let disallowDanger =
    defined(nimsuggest) or graph.config.cmd == cmdCheck or
    vmopsDanger notin graph.config.features
  # the VM instance used for NimScript execution is also used for the
  # compile-time evaluation, so we have to register the macro/compile-time
  # ops on the instance. Combined with how callback registration works,
  # this has the following consequences:
  # - CTFE and macro evaluation happening during semantic analysis of the
  #  NimScript file are run with NimScript privileges, e.g. file-system write
  #  access
  # - the ``vmopsDanger`` option has no effect on callbacks registered
  #  during `setupVM`
  # - NimScript has access to the macro/compile-time APIs
  registerAdditionalOps(vm[], disallowDanger)
  graph.vm = vm

  graph.compileSystemModule()
  discard graph.processModule(m, vm.idgen, stream)

  # watch out, "newruntime" can be set within NimScript itself and then we need
  # to remember this:
  case conf.selectedGC
  of gcUnselected:
    conf.selectedGC = oldSelectedGC
  of gcArc, gcOrc:
    conf.incl {optTinyRtti, optSeqDestructors}
    defineSymbol(conf, "nimv2")
  else:
    discard

  # ensure we load 'system.nim' again for the real non-config stuff!
  resetSystemArtifacts(graph)
  # do not remove the defined symbols
  #initDefines()
  undefSymbol(conf, "nimscript")
  undefSymbol(conf, "nimconfig")
  conf.symbolFiles = oldSymbolFiles

  conf.localReport DebugReport(
    kind: rdbgFinishedConfRead,
    filename: scriptName.string
  )
