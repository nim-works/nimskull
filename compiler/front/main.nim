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
    depfiles
  ],
  compiler/sem/[
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
    pathutils    # Input file handling
  ],
  compiler/vm/[
    vm,          # Configuration file evaluation, `nim e`
    vmbackend,   # VM code generation
    vmprofiler
  ]

import compiler/ic/[
    cbackend,
    integrity,
    navigator
  ]
from compiler/ic/ic import rodViewer

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_internal import InternalReport,
  InternalStateDump
from compiler/ast/reports_external import ExternalReport
from compiler/ast/report_enums import ReportKind,
  repHintKinds,
  repWarningKinds,
  rstWarnings

when not defined(leanCompiler):
  import
    compiler/backend/jsgen,
    compiler/tools/[docgen, docgen2]

when defined(nimDebugUnreportedErrors):
  import std/exitprocs
  import compiler/utils/astrepr

  proc echoAndResetUnreportedErrors(conf: ConfigRef) =
    if conf.unreportedErrors.len > 0:
      echo "Unreported errors:"
      for reportId, node in conf.unreportedErrors:
        var reprConf = defaultTReprConf
        reprConf.flags.incl trfShowNodeErrors
        echo conf.treeRepr(node)
      conf.unreportedErrors.clear

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

proc commandGenDepend(graph: ModuleGraph) =
  semanticPasses(graph)
  registerPass(graph, gendependPass)
  compileProject(graph)
  let project = graph.config.projectFull
  writeDepsFile(graph)
  generateDot(graph, project)
  execExternalProgram(
    graph.config,
    (
      "dot -Tpng -o" &
        changeFileExt(project, "png").string &
        ' ' &
        changeFileExt(project, "dot").string
    ),
    rcmdExecuting
  )

proc commandCheck(graph: ModuleGraph) =
  let conf = graph.config
  conf.setErrorMaxHighMaybe
  defineSymbol(conf, "nimcheck")
  semanticPasses(graph)  # use an empty backend for semantic checking only
  compileProject(graph)

  if conf.symbolFiles != disabledSf:
    case conf.ideCmd
    of ideDef: navDefinition(graph)
    of ideUse: navUsages(graph)
    of ideDus: navDefusages(graph)
    else: discard
    writeRodFiles(graph)

when not defined(leanCompiler):
  proc commandDoc2(graph: ModuleGraph; ext: string) =
    handleDocOutputOptions graph.config
    graph.config.setErrorMaxHighMaybe
    semanticPasses(graph)
    case ext:
    of TexExt:  registerPass(graph, docgen2TexPass)
    of JsonExt: registerPass(graph, docgen2JsonPass)
    of HtmlExt: registerPass(graph, docgen2Pass)
    else: doAssert false, $ext
    compileProject(graph)
    finishDoc2Pass(graph.config.projectName)

proc commandCompileToC(graph: ModuleGraph) =
  let conf = graph.config
  extccomp.initVars(conf)
  semanticPasses(graph)
  if conf.symbolFiles == disabledSf:
    registerPass(graph, cgenPass)

    if {optRun, optForceFullMake} * conf.globalOptions == {optRun} or isDefined(conf, "nimBetterRun"):
      if not changeDetectedViaJsonBuildInstructions(conf, conf.jsonBuildInstructionsFile):
        # nothing changed
        graph.config.notes = graph.config.mainPackageNotes
        return

  if not extccomp.ccHasSaneOverflow(conf):
    conf.defineSymbol("nimEmulateOverflowChecks")

  compileProject(graph)
  if graph.config.errorCounter > 0:
    return # issue #9933
  if conf.symbolFiles == disabledSf:
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
    conf.exc = excNative
    conf.target =
      block:
        var t = conf.target
        setTarget(t, osJS, cpuJS)
        t

    defineSymbol(conf, "ecmascript") # For backward compatibility
    semanticPasses(graph)
    registerPass(graph, JSgenPass)
    compileProject(graph)
    if conf.depfile.string.len != 0:
      writeGccDepfile(conf)
    if optGenScript in conf.globalOptions:
      writeDepsFile(graph)

proc commandCompileToVM(graph: ModuleGraph) =
  let conf = graph.config
  # XXX: there doesn't exist an exception mode for "external" (maybe excQuirky
  #      would fit?) so excNative is used, since the same is done for the
  #      JS backend
  conf.exc = excNative

  semanticPasses(graph)
  registerPass(graph, vmgenPass)
  compileProject(graph)

  # The VM-backend doesn't use a pass for the actual code generation, but a
  # separate function instead (similar to the C-backend for IC)
  vmbackend.generateCode(graph)

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

proc setOutFile*(conf: ConfigRef) =
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

proc mainCommand*(graph: ModuleGraph) =
  ## Execute main compiler command
  let conf = graph.config
  let cache = graph.cache

  # In "nim serve" scenario, each command must reset the registered passes
  clearPasses(graph)
  conf.lastCmdTime = epochTime()
  conf.searchPathsAdd(conf.libpath)

  proc customizeForBackend(backend: TBackend) =
    ## Sets backend specific options but don't compile to backend yet in
    ## case command doesn't require it. This must be called by all commands.
    if conf.backend == backendInvalid:
      # only set if wasn't already set, to allow override via `nim c -b:js`
      conf.backend = backend

    defineSymbol(graph.config, $conf.backend)
    case conf.backend
    of backendC:
      if conf.exc == excNone: conf.exc = excSetjmp
    of backendJs, backendNimVm:
      discard
    of backendInvalid: doAssert false

  proc compileToBackend() =
    customizeForBackend(conf.backend)
    setOutFile(conf)
    case conf.backend
    of backendC: commandCompileToC(graph)
    of backendJs: commandCompileToJS(graph)
    of backendNimVm: commandCompileToVM(graph)
    of backendInvalid: doAssert false

  template docLikeCmd(body) =
    when defined(leanCompiler):
      conf.quitOrRaise "compiler wasn't built with documentation generator"
    else:
      wantMainModule(conf)
      let docConf = if conf.cmd == cmdDoc2tex: DocTexConfig else: DocConfig
      loadConfigs(docConf, cache, conf, graph.idgen)
      defineSymbol(conf, "nimdoc")
      body

  ## command prepass
  if conf.cmd == cmdCrun: conf.incl {optRun, optUseNimcache}
  if conf.cmd notin cmdBackends + {cmdTcc}: customizeForBackend(backendC)
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
      extccomp.setCC(conf, "tcc", unknownLineInfo)
      if conf.backend != backendC:
        globalReport(conf, ExternalReport(
          kind: rextExpectedCbackednForRun, usedCompiler: $conf.backend))

      compileToBackend()
    else:
      globalReport(conf, ExternalReport(
        kind: rextExpectedTinyCForRun))
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
      conf.quitOrRaise "compiler wasn't built with documentation generator"
    else:
      loadConfigs(DocConfig, cache, conf, graph.idgen)
      commandRst2Html(cache, conf)
  of cmdRst2tex, cmdDoc2tex:
    for warn in rstWarnings:
      conf.setNoteDefaults(warn, true)
    when defined(leanCompiler):
      conf.quitOrRaise "compiler wasn't built with documentation generator"
    else:
      if conf.cmd == cmdRst2tex:
        loadConfigs(DocTexConfig, cache, conf, graph.idgen)
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

    conf.localReport(InternalReport(kind: rintDumpState, stateDump: state))

  of cmdCheck:
    commandCheck(graph)

  of cmdParse:
    wantMainModule(conf)
    discard parseFile(conf.projectMainIdx, cache, conf)

  of cmdRod:
    wantMainModule(conf)
    commandView(graph)
    #msgWriteln(conf, "Beware: Indentation tokens depend on the parser's state!")
  of cmdInteractive: commandInteractive(graph)
  of cmdNimscript:
    if conf.projectIsCmd or conf.projectIsStdin: discard
    elif not fileExists(conf.projectFull):
      localReport(conf, InternalReport(
        kind: rintCannotOpenFile, msg: conf.projectFull.string))

    # main NimScript logic handled in `loadConfigs`.
  of cmdNop: discard
  of cmdJsonscript:
    setOutFile(graph.config)
    commandJsonScript(graph)
  of cmdUnknown, cmdNone, cmdIdeTools, cmdNimfix:
    localReport(conf, ExternalReport(
      msg: conf.command, kind: rextInvalidCommand))

  if conf.errorCounter == 0 and conf.cmd notin {cmdTcc, cmdDump, cmdNop}:
    if optProfileVM in conf.globalOptions:
      echo conf.dump(conf.vmProfileData)
    genSuccessX(conf)

  when defined(nimDebugUnreportedErrors):
    echoAndResetUnreportedErrors(conf)

  when PrintRopeCacheStats:
    echo "rope cache stats: "
    echo "  tries : ", gCacheTries
    echo "  misses: ", gCacheMisses
    echo "  int tries: ", gCacheIntTries
    echo "  efficiency: ", formatFloat(1-(gCacheMisses.float/gCacheTries.float),
                                       ffDecimal, 3)
