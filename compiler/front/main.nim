#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# implements the command dispatcher and several commands

when not defined(nimcore):
  {.error: "nimcore MUST be defined for Nim's core tooling".}

import
  std/[sequtils, strutils, os, times, tables, sha1, with, json],
  compiler/ast/[
    llstream,    # Input data stream
    ast,
    lexer,
    idents,
    syntaxes,
    lineinfos    # Positional data
  ],
  compiler/front/[
    options,
    condsyms,
    msgs,
    cmdlinehelper,
    nimconf,     # Configuration file reading
    depfiles,
    commands
  ],
  compiler/sem/[
    modulelowering,
    sem,         # Implementation of the semantic pass
    passes,      # Main procs for compilation pass setups
    passaux
  ],
  compiler/modules/[
    depends,     # Generate dependency information
    modules,
    modulegraphs # Project module graph
  ],
  compiler/backend/[
    extccomp,    # Calling C compiler
    cgen,        # C code generation
  ],
  compiler/utils/[
    platform,    # Target platform data
    nversion,
    pathutils,   # Input file handling
    astrepr,     # Output parsed data, for compiler development
    idioms,
  ],
  compiler/vm/[
    compilerbridge, # Configuration file evaluation, `nim e`
    vmbackend,      # VM code generation
  ]

import compiler/ic/[
    cbackend,
    integrity
  ]
from compiler/ic/ic import rodViewer

from std/osproc import execCmd

import compiler/backend/cbackend as cbackend2

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_internal import InternalReport
from compiler/ast/report_enums import ReportKind,
  repHintKinds,
  repWarningKinds,
  rstWarnings

from compiler/front/optionsprocessor import processSwitch,
  procSwitchSuccess
from compiler/front/scripting import runNimScript

when not defined(leanCompiler):
  import
    compiler/backend/jsbackend,
    compiler/tools/[docgen, docgen2]

when defined(nimDebugUnreportedErrors):
  import std/exitprocs
  import compiler/utils/astrepr

  proc echoAndResetUnreportedErrors(conf: ConfigRef) =
    if conf.unreportedErrors.len > 0:
      echo "Unreported errors:"
      for nodeId, node in conf.unreportedErrors:
        var reprConf = defaultTReprConf
        reprConf.flags.incl trfShowNodeErrors
        echo conf.treeRepr(node)
      conf.unreportedErrors.clear

type
  InternalStateDump = ref object
    version*: string
    nimExe*: string
    prefixdir*: string
    libpath*: string
    projectPath*: string
    definedSymbols*: seq[string]
    libPaths*: seq[string]
    lazyPaths*: seq[string]
    nimbleDir*: string
    outdir*: string
    `out`*: string
    nimcache*: string
    hints*, warnings*: seq[tuple[name: string, enabled: bool]]

  UsedBuildParams* = object
    cmd*: Command
    project*: string
    output*: string
    linesCompiled*: int
    mem*: int
    isMaxMem*: bool
    sec*: float
    case isCompilation*: bool # TODO: redundant with `cmd in cmdBackends`
      of true:
        backend*: string
        gc*: string
        threads*: bool
        buildMode*: string
        optimize*: string
      of false:
        discard

proc semanticPasses(g: ModuleGraph) =
  registerPass g, verbosePass
  registerPass g, semPass

proc writeGccDepfile(conf: ConfigRef) =
  ## Writes target's dependencies in the format understood by most build
  ## systems. See https://github.com/nim-works/nimskull/pull/376.
  let
    depfile = open(conf.depfile.string, fmWrite)
    target = conf.outFile.string
    paths = conf.m.fileInfos.mapIt(it.fullPath.string)
  depfile.writeGccDepfile(target, paths)
  depfile.close()

func execExternalProgramFailedMsg*(cmd: string, exitCode: int): string =
  "execution of an external program '$1' failed with exit code '$2'" %
    [cmd, $exitCode]

proc commandGenDepend(graph: ModuleGraph) =
  semanticPasses(graph)
  registerPass(graph, gendependPass)
  compileProject(graph)
  let project = graph.config.projectFull
  writeDepsFile(graph)
  generateDot(graph, project)
  let cmd = "dot -Tpng -o$1 $2" %
                [project.changeFileExt("png").string,
                project.changeFileExt("dot").string]
  graph.config.logExecStart(cmd)
  let code = execCmd(cmd)
  if code != 0:
    graph.config.logError(execExternalProgramFailedMsg(cmd, code))

proc commandCheck(graph: ModuleGraph) =
  let conf = graph.config
  conf.setErrorMaxHighMaybe
  defineSymbol(conf, "nimcheck")
  semanticPasses(graph)  # use an empty backend for semantic checking only
  compileProject(graph)

when not defined(leanCompiler):
  proc commandDoc2(graph: ModuleGraph; ext: string) =
    handleDocOutputOptions graph.config
    graph.config.setErrorMaxHighMaybe
    semanticPasses(graph)
    case ext:
    of TexExt:  registerPass(graph, docgen2TexPass)
    of JsonExt: registerPass(graph, docgen2JsonPass)
    of HtmlExt: registerPass(graph, docgen2Pass)
    else: unreachable($ext)
    compileProject(graph)
    finishDoc2Pass(graph.config.projectName)

template prepareForCodegen(g: ModuleGraph) =
  # XXX: the template is going to be removed once compilation is uniform
  #      enough for the logic here to be located in a single place
  # if there were errors, don't enter the backend / code generation
  # phase
  if g.config.errorCounter > 0:
    return
  # the backend / code generation phase generally expects errors to terminate
  # the compiler, so make sure that they do
  g.config.errorMax = 1

proc commandCompileToC(graph: ModuleGraph) =
  let conf = graph.config
  extccomp.initVars(conf)
  semanticPasses(graph)
  if conf.symbolFiles == disabledSf:
    registerPass(graph, collectPass)

    if {optRun, optForceFullMake} * conf.globalOptions == {optRun} or isDefined(conf, "nimBetterRun"):
      if not changeDetectedViaJsonBuildInstructions(conf, conf.jsonBuildInstructionsFile):
        # nothing changed
        graph.config.notes = graph.config.mainPackageNotes
        return

  if not extccomp.ccHasSaneOverflow(conf):
    conf.defineSymbol("nimEmulateOverflowChecks")

  compileProject(graph)
  prepareForCodegen(graph)
  if conf.symbolFiles == disabledSf:
    cbackend2.generateCode(graph, graph.takeModuleList())
    cgenWriteModules(graph.backend, conf)
  else:
    if isDefined(conf, "nimIcIntegrityChecks"):
      checkIntegrity(graph)
    cbackend.generateCode(graph)
    # graph.backend can be nil under IC when nothing changed at all:
    if graph.backend != nil:
      cgenWriteModules(graph.backend, conf)
  if conf.cmd != cmdTcc and graph.backend != nil:
    extccomp.callCCompiler(conf)
    extccomp.writeJsonBuildInstructions(conf)
    if conf.depfile.string.len != 0:
      writeGccDepfile(conf)
    if optGenScript in graph.config.globalOptions:
      writeDepsFile(graph)

proc commandJsonScript(graph: ModuleGraph) =
  extccomp.runJsonBuildInstructions(graph.config, graph.config.jsonBuildInstructionsFile)

proc commandCompileToJS(graph: ModuleGraph) =
  let conf = graph.config
  when defined(leanCompiler):
    globalReport(conf, unknownLineInfo, InternalReport(
      kind: rintUsingLeanCompiler,
      msg: "Compiler was not build with js support"))
  else:
    conf.target =
      block:
        var t = conf.target
        setTarget(t, osJS, cpuJS)
        t

    defineSymbol(conf, "ecmascript") # For backward compatibility
    semanticPasses(graph)
    registerPass(graph, collectPass)
    compileProject(graph)

    prepareForCodegen(graph)
    jsbackend.generateCode(graph, graph.takeModuleList())

    if conf.depfile.string.len != 0:
      writeGccDepfile(conf)
    if optGenScript in conf.globalOptions:
      writeDepsFile(graph)

proc commandCompileToVM(graph: ModuleGraph) =
  semanticPasses(graph)
  registerPass(graph, collectPass)
  compileProject(graph)

  prepareForCodegen(graph)
  # The VM-backend doesn't use a pass for the actual code generation, but a
  # separate procedure instead (similar to the C-backend for IC)
  vmbackend.generateCode(graph, graph.takeModuleList())

proc interactivePasses(graph: ModuleGraph) =
  initDefines(graph.config.symbols)
  defineSymbol(graph.config, "nimscript")
  registerPass(graph, verbosePass)
  registerPass(graph, semPass)
  registerPass(graph, evalPass)

proc commandInteractive(graph: ModuleGraph) =
  graph.config.setErrorMaxHighMaybe
  interactivePasses(graph)
  compileSystemModule(graph)
  if graph.config.commandArgs.len > 0:
    discard graph.compileModule(fileInfoIdx(graph.config, graph.config.projectFull), {})
  else:
    var m = graph.makeStdinModule()
    incl(m.flags, sfMainModule)
    var idgen = IdGenerator(module: m.itemId.module, symId: m.itemId.item, typeId: 0)
    let s = llStreamOpenStdIn(onPrompt = proc() = flushDot(graph.config))
    processModule(graph, m, idgen, s)

proc commandScan(cache: IdentCache, config: ConfigRef) =
  var f = addFileExt(AbsoluteFile mainCommandArg(config), NimExt)
  var stream = llStreamOpen(f, fmRead)
  if stream != nil:
    var
      L: Lexer
      tok: Token
    initToken(tok)
    openLexer(L, f, stream, cache, config)
    while true:
      rawGetTok(L, tok)
      printTok(config, tok)
      if tok.tokType == tkEof: break
    closeLexer(L)
  else:
    localReport(config, InternalReport(
      kind: rintCannotOpenFile, msg: f.string))

proc commandView(graph: ModuleGraph) =
  let f = toAbsolute(mainCommandArg(graph.config), AbsoluteDir getCurrentDir()).addFileExt(RodExt)
  rodViewer(f, graph.config, graph.cache)

const
  PrintRopeCacheStats = false

when PrintRopeCacheStats:
  import utils/ropes

proc hashMainCompilationParams*(conf: ConfigRef): string =
  ## doesn't have to be complete; worst case is a cache hit and recompilation.
  var state = newSha1State()
  with state:
    update os.getAppFilename() # nim compiler
    update conf.commandLine # excludes `arguments`, as it should
    update $conf.projectFull # so that running `nim r main` from 2 directories caches differently
  result = $SecureHash(state.finalize())

proc setOutFile(conf: ConfigRef) =
  proc libNameTmpl(conf: ConfigRef): string {.inline.} =
    result = if conf.target.targetOS == osWindows: "$1.lib" else: "lib$1.a"

  if conf.outFile.isEmpty:
    var base = conf.projectName
    if optUseNimcache in conf.globalOptions:
      base.add "_" & hashMainCompilationParams(conf)
    let targetName =
      if conf.backend == backendJs: base & ".js"
      elif conf.backend == backendNimVm: base & ".nimbc" # nim bytecode
      elif optGenDynLib in conf.globalOptions:
        platform.OS[conf.target.targetOS].dllFrmt % base
      elif optGenStaticLib in conf.globalOptions: libNameTmpl(conf) % base
      else: base & platform.OS[conf.target.targetOS].exeExt
    conf.outFile = RelativeFile targetName

proc genSuccessX(conf: ConfigRef): UsedBuildParams =
  ## Generate and write report for the successful compilation parameters
  let
    cmd = conf.cmd
    sec = epochTime() - conf.lastCmdTime
    compilation = cmd in cmdBackends
    project = 
      case conf.filenameOption
      of foAbs: $conf.projectFull
      else: $conf.projectName
    (mem, isMaxMem) =
      when declared(system.getMaxMem): (getMaxMem(), true)
      else:                            (getTotalMem(), false)
    output =
      block:
        let temp =
          if optCompileOnly in conf.globalOptions and cmd != cmdJsonscript:
            $conf.jsonBuildFile
          elif conf.outFile.isEmpty and
              cmd notin {cmdJsonscript} + cmdDocLike + cmdBackends:
            # for some cmd we expect a valid absOutFile
            "unknownOutput"
          else:
            $conf.absOutFile
        if conf.filenameOption == foAbs: temp
        else:                            temp.AbsoluteFile.extractFilename
    linesCompiled = conf.linesCompiled

  if compilation:
    let
      backend = $conf.backend
      gc = $conf.selectedGC
      threads = optThreads in conf.globalOptions
      optimize =
        if optOptimizeSpeed in conf.options: "speed"
        elif optOptimizeSize in conf.options: "size"
        else: "debug"
      buildMode =
        if isDefined(conf, "danger"): "danger"
        elif isDefined(conf, "release"): "release"
        else: "debug"

    UsedBuildParams(
      cmd: cmd,
      project: project,
      output: output,
      linesCompiled: linesCompiled,
      mem: mem,
      isMaxMem: isMaxMem,
      sec: sec,
      isCompilation: true,
      backend: backend,
      gc: gc,
      threads: threads,
      buildMode: buildMode,
      optimize: optimize)
  else:
    UsedBuildParams(
      cmd: cmd,
      project: project,
      output: output,
      linesCompiled: linesCompiled,
      mem: mem,
      isMaxMem: isMaxMem,
      sec: sec,
      isCompilation: false)

proc `$`(params: UsedBuildParams): string =
  let
    prefix = "Hint: " # TODO: add colour (fgGreen)
    build =
      if params.isCompilation:
        "gc: $1; $2opt: $3; $4" % [
            params.gc,
            if params.threads: "threads: on; " else: "",
            if params.optimize == "debug":
              "none (DEBUG BUILD, `-d: release` generates faster code)"
            else:
              params.optimize,
            if params.buildMode == "debug": "" else: "$1; " % params.buildMode
          ]
      else:
        ""
    mem = formatSize(params.mem)
    memUnits = if params.isMaxMem: "peakmem" else: "totmem"
    suffix = "[SuccessX]" # TODO: add colour (fgCyan) and add msg origin

  "$1$2 $3 lines; $4s; $5 $6; proj: $7; out: $8 $9" % [
      #[1]# prefix,
      #[2]# build,
      #[3]# $params.linesCompiled,
      #[4]# params.sec.formatFloat(precision = 3),
      #[5]# $mem,
      #[6]# $memUnits,
      #[7]# $params.project,
      #[8]# params.output,
      #[9]# suffix
    ]

proc customizeForBackend*(graph: ModuleGraph, conf: ConfigRef,
                          backend: TBackend) =
  ## Sets backend specific options. This must be called by all commands.
  ## `backend` is the backend to choose when no backend has been selected
  ## so far.
  if conf.backend == backendInvalid:
    # only set if wasn't already set, to allow override via `nim c -b:js`
    conf.backend = backend

  defineSymbol(graph.config, $conf.backend)
  case conf.backend
  of backendC:
    case conf.exc
    of excNone, excNative: conf.exc = excGoto
    of excGoto:            discard

    if conf.selectedGC == gcUnselected:
      # the default gc for the C backend is ORC. We can't just set it to
      # ``gcOrc`` directly, however, as additional defines, etc. are required
      # XXX: the dependency on ``optionsprocessor`` hints that a different
      #      approach for setting up the GC related settings is needed
      let r = processSwitch("gc", "orc", passCmd2, conf)
      doAssert r.kind == procSwitchSuccess, "set default gc failed: " & $r.kind
  of backendJs, backendNimVm:
    # force the exception and GC mode to 'native'
    conf.exc = excNative
    conf.selectedGC = gcNative
  of backendInvalid:
    unreachable()

proc mainCommand*(graph: ModuleGraph) =
  ## Execute main compiler command
  let
    conf = graph.config
    cache = graph.cache

  # In "nim serve" scenario, each command must reset the registered passes
  clearPasses(graph)
  conf.lastCmdTime = epochTime()
  conf.searchPathsAdd(conf.libpath)

  proc compileToBackend() =
    customizeForBackend(graph, conf, conf.backend)
    setOutFile(conf)
    case conf.backend
    of backendC: commandCompileToC(graph)
    of backendJs: commandCompileToJS(graph)
    of backendNimVm: commandCompileToVM(graph)
    of backendInvalid: unreachable()

  template docLikeCmd(body) =
    when defined(leanCompiler):
      conf.quitOrRaise "compiler built without documentation generator"
    else:
      wantMainModule(conf)
      let docConf = if conf.cmd == cmdDoc2tex: DocTexConfig else: DocConfig
      if loadConfigs(docConf, cache, conf):
        defineSymbol(conf, "nimdoc")
        body

  ## command prepass
  if conf.cmd == cmdCrun: conf.incl {optRun, optUseNimcache}
  if conf.cmd notin cmdBackends + {cmdTcc, cmdNimscript}:
    customizeForBackend(graph, conf, backendC)
  if conf.outDir.isEmpty:
    # doc like commands can generate a lot of files (especially with --project)
    # so by default should not end up in $PWD nor in $projectPath.
    var ret = if optUseNimcache in conf.globalOptions: getNimcacheDir(conf)
              else: conf.projectPath
    doAssert ret.string.isAbsolute # `AbsoluteDir` is not a real guarantee
    if conf.cmd in cmdDocLike + {cmdRst2html, cmdRst2tex}: ret = ret / htmldocsDir
    conf.outDir = ret

  when defined(nimDebugUnreportedErrors):
    addExitProc proc = echoAndResetUnreportedErrors(conf)

  ## process all commands
  case conf.cmd
  of cmdBackends: compileToBackend()
  of cmdTcc:
    when hasTinyCBackend:
      let cc = extccomp.setCC(conf, "tcc")
      doAssert cc == ccTcc, "what happened to tcc?"
      if conf.backend != backendC:
        conf.logError("'run' requires c backend, got: '$1'" % $conf.backend)
      else:
        compileToBackend()
    else:
      conf.logError("'run' command not available; rebuild with -d:tinyc")
  of cmdDoc:
    docLikeCmd():
      conf.setNoteDefaults(rsemLockLevelMismatch, false) # issue #13218
      conf.setNoteDefaults(rbackRstRedefinitionOfLabel, false) # issue #13218
        # because currently generates lots of false positives due to conflation
        # of labels links in doc comments, e.g. for random.rand:
        #  ## * `rand proc<#rand,Rand,Natural>`_ that returns an integer
        #  ## * `rand proc<#rand,Rand,range[]>`_ that returns a float
      commandDoc2(graph, HtmlExt)
      if optGenIndex in conf.globalOptions and optWholeProject in conf.globalOptions:
        commandBuildIndex(conf, $conf.outDir)
  of cmdRst2html:
    # XXX: why are warnings disabled by default for rst2html and rst2tex?
    for warn in rstWarnings:
      conf.setNoteDefaults(warn, true)
    conf.setNoteDefaults(rbackRstRedefinitionOfLabel, false) # similar to issue #13218
    when defined(leanCompiler):
      conf.quitOrRaise "compiler built without documentation generator"
    else:
      if loadConfigs(DocConfig, cache, conf):
        commandRst2Html(cache, conf)
  of cmdRst2tex, cmdDoc2tex:
    for warn in rstWarnings:
      conf.setNoteDefaults(warn, true)
    when defined(leanCompiler):
      conf.quitOrRaise "compiler built without documentation generator"
    else:
      if conf.cmd == cmdRst2tex:
        if loadConfigs(DocTexConfig, cache, conf):
          commandRst2TeX(cache, conf)
      else:
        docLikeCmd commandDoc2(graph, TexExt)
  of cmdJsondoc: docLikeCmd commandDoc2(graph, JsonExt)
  of cmdCtags: docLikeCmd commandTags(cache, conf)
  of cmdBuildindex: docLikeCmd commandBuildIndex(conf, $conf.projectFull, conf.outFile)
  of cmdGendepend: commandGenDepend(graph)
  of cmdDump:
    wantMainModule(conf)
    var state = InternalStateDump()
    for s in definedSymbolNames(conf):
      state.definedSymbols.add $s

    for dir in conf.searchPaths:
      state.libPaths.add(dir.string)

    for dir in conf.lazyPaths:
      state.lazyPaths.add(dir.string)

    for a in repHintKinds:
      state.hints.add(($a, a in conf.notes))

    for a in repWarningKinds:
      state.warnings.add(($a, a in conf.notes))

    state.version     = VersionAsString
    state.nimExe      = getAppFilename()
    state.prefixdir   = conf.getPrefixDir().string
    state.libpath     = conf.libpath.string
    state.projectPath = conf.projectFull.string
    state.outdir      = conf.outDir.string
    state.`out`       = conf.outFile.string
    state.nimcache    = getNimcacheDir(conf).string

    if conf.getConfigVar("dump.format") == "json":
      var definedSymbols = newJArray()
      for s in state.definedSymbols:
        definedSymbols.elems.add(%s)

      var libpaths = newJArray()
      for dir in conf.searchPaths:
        libpaths.elems.add(%dir.string)

      var lazyPaths = newJArray()
      for dir in conf.lazyPaths:
        lazyPaths.elems.add(%dir.string)

      var hints = newJObject()
      for (a, s) in state.hints:
        hints[$a] = %(s)

      var warnings = newJObject()
      for (a, s) in state.warnings:
        warnings[$a] = %(s)

      let dumpStr = $(%[
          (key: "version",         val: %state.version),
          (key: "nimExe",          val: %state.nimExe),
          (key: "prefixdir",       val: %state.prefixdir),
          (key: "libpath",         val: %state.libpath),
          (key: "project_path",    val: %state.projectPath),
          (key: "defined_symbols", val: definedSymbols),
          (key: "lib_paths",       val: libpaths),
          (key: "lazyPaths",       val: lazyPaths),
          (key: "outdir",          val: %state.outdir),
          (key: "out",             val: %state.out),
          (key: "nimcache",        val: %state.nimcache),
          (key: "hints",           val: hints),
          (key: "warnings",        val: warnings),
        ])
      # skip past all the report hook stupidity
      conf.writeln(cmdOutUser, dumpStr)
    else:
      # skip past all the report hook stupidity
      conf.writeln(cmdOutUser, "-- list of currently defined symbols --")
      for s in state.definedSymbols:
        conf.writeln(cmdOutUser, s)
      conf.writeln(cmdOutUser, "-- end of list --")
      for it in state.libPaths:
        conf.writeln(cmdOutUser, it)

  of cmdCheck:
    commandCheck(graph)

  of cmdParse:
    wantMainModule(conf)
    var reprConf = defaultTReprConf
    reprConf.flags.excl trfShowNodeIds
    reprConf.flags.incl trfShowNodeLineInfo
    echo conf.treeRepr(parseFile(conf.projectMainIdx, cache, conf), reprConf)

  of cmdScan:
    wantMainModule(conf)
    commandScan(cache, conf)

  of cmdRod:
    wantMainModule(conf)
    commandView(graph)
    #msgWriteln(conf, "Beware: Indentation tokens depend on the parser's state!")
  of cmdInteractive: commandInteractive(graph)
  of cmdNimscript:
    if conf.inputMode == pimFile and not fileExists(conf.projectFull):
      localReport(conf, InternalReport(
        kind: rintCannotOpenFile, msg: conf.projectFull.string))

    let s =
      case conf.inputMode
      of pimStdin: llStreamOpenStdIn()
      of pimCmd:   llStreamOpen(conf.commandArgs[0])
      of pimFile:  llStreamOpen(conf.projectFull, fmRead)

    # XXX: the ``runNimScript`` from ``scripting`` is used, but the script
    #      is not meant for configuration. While this has no practical
    #      consequences right now, it's still a domain violation
    runNimScript(cache, conf.projectFull, freshDefines = false, conf, s)
  of cmdNop: discard
  of cmdJsonscript:
    setOutFile(graph.config)
    commandJsonScript(graph)
  of cmdUnknown, cmdNone, cmdIdeTools, cmdNimfix:
    conf.logError(CliEvent(kind: cliEvtErrInvalidCommand, cmd: conf.command,
                           srcCodeOrigin: instLoc()))

  if optProfileVM in conf.globalOptions:
    conf.writeln cmdOutUserProf, dumpVmProfilerData(graph)

  if conf.errorCounter == 0 and conf.cmd notin {cmdTcc, cmdDump, cmdNop}:
    if conf.isEnabled(rintSuccessX):
      conf.writeln(cmdOutStatus, $genSuccessX(conf))

  when defined(nimDebugUnreportedErrors):
    echoAndResetUnreportedErrors(conf)

  when PrintRopeCacheStats:
    echo "rope cache stats: "
    echo "  tries : ", gCacheTries
    echo "  misses: ", gCacheMisses
    echo "  int tries: ", gCacheIntTries
    echo "  efficiency: ", formatFloat(1-(gCacheMisses.float/gCacheTries.float),
                                       ffDecimal, 3)
