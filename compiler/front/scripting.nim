#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Use Nimskull as a scripting language.

import
  std/[
    os,
    times,
    osproc,
  ],
  compiler/ast/[
    ast,
    idents,
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
    gorgeimpl,
    vmconv,
    vmdef,
    vmhooks,
    vmops
  ],
  compiler/front/[
    condsyms,
    options,
  ],
  compiler/utils/[
    pathutils
  ]

from compiler/vm/vmlegacy import legacyReportsVmTracer

# we support 'cmpIgnoreStyle' natively for efficiency:
from std/strutils import cmpIgnoreStyle, contains

proc setupVM*(module: PSym; cache: IdentCache; scriptName: string;
              graph: ModuleGraph; idgen: IdGenerator): TCtx =
  # For Nimble we need to export 'setupVM'.
  result = initCtx(module, cache, graph, idgen, legacyReportsVmTracer)
  # for backwards compatibility, allow meta expressions in nimscript (this
  # matches the previous behaviour)
  result.flags = {cgfAllowMeta}
  result.mode = emRepl
  registerBasicOps(result)

  proc listDirs(a: VmArgs, filter: set[PathComponent]) =
    let dir = getString(a, 0)
    var res: seq[string] = @[]
    for kind, path in walkDir(dir):
      if kind in filter: res.add path
    writeTo(res, a.getResultHandle(), a.mem[])

  # captured vars:
  var
    errorMsg: string
    vthisDir = scriptName.splitFile.dir

  template cbconf(name, body) {.dirty.} =
    result.registerCallback "stdlib.system." & astToStr(name),
      proc (a: VmArgs) =
        body

  template cbeff(name, exc, m, body) {.dirty.} =
    result.registerCallback "stdlib." & m & "." & astToStr(name),
      proc (a: VmArgs) =
        errorMsg = ""
        try:
          body
        except exc:
          errorMsg = getCurrentExceptionMsg()

  template cbexc(name, exc, body) {.dirty.} =
    cbeff(name, exc, "system", body)

  template cbio(name, body) {.dirty.} =
    cbeff(name, IOError, "io", body)

  template cbos(name, body) {.dirty.} =
    cbexc(name, OSError, body)

  template guardEffect(body) {.dirty.} =
    # might not be needed once this is no longer allowed for configs
    if defined(nimsuggest) or graph.config.cmd == cmdCheck:
      discard
    else:
      body

  result.registerCallback "stdlib.system.getError",
    proc (a: VmArgs) = setResult(a, errorMsg)

  # Idea: Treat link to file as a file, but ignore link to directory to prevent
  # endless recursions out of the box.
  cbos listFilesImpl:
    listDirs(a, {pcFile, pcLinkToFile})
  cbos listDirsImpl:
    listDirs(a, {pcDir})
  cbos removeDir:
    guardEffect:
      os.removeDir(getString(a, 0), getBool(a, 1))
  cbos removeFile:
    guardEffect:
      os.removeFile getString(a, 0)
  cbos createDir:
    os.createDir getString(a, 0)
  cbos setCurrentDir:
    os.setCurrentDir getString(a, 0)
  cbos getCurrentDir:
    setResult(a, os.getCurrentDir())
  cbos moveFile:
    guardEffect:
      os.moveFile(getString(a, 0), getString(a, 1))
  cbos moveDir:
    guardEffect:
      os.moveDir(getString(a, 0), getString(a, 1))
  cbos copyFile:
    guardEffect:
      os.copyFile(getString(a, 0), getString(a, 1))
  cbos copyDir:
    guardEffect:
      os.copyDir(getString(a, 0), getString(a, 1))
  cbos getLastModificationTime:
    setResult(a, getLastModificationTime(getString(a, 0)).toUnix)
  cbos findExe:
    setResult(a, os.findExe(getString(a, 0)))
  cbos rawExec:
    guardEffect:
      setResult(a, osproc.execCmd getString(a, 0))
  cbos rawExecEx:
    setResult(a):
      opGorge(getString(a, 0), getString(a, 1), getString(a, 2),
              a.info, a.config)
  cbio writeFile:
    guardEffect:
      system.writeFile(getString(a, 0), getString(a, 1))
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
  cbconf thisDir:
    setResult(a, vthisDir)
  cbconf get:
    setResult(a, options.getConfigVar(graph.config, a.getString 0))
  cbconf exists:
    setResult(a, options.existsConfigVar(graph.config, a.getString 0))
  cbconf nimcacheDir:
    setResult(a, options.getNimcacheDir(graph.config).string)
  cbconf paramStr:
    setResult(a, os.paramStr(int a.getInt 0))
  cbconf paramCount:
    setResult(a, os.paramCount())
  cbconf cmpIgnoreStyle:
    setResult(a, strutils.cmpIgnoreStyle(a.getString 0, a.getString 1))
  cbconf cmpIgnoreCase:
    setResult(a, strutils.cmpIgnoreCase(a.getString 0, a.getString 1))
  cbconf selfExe:
    setResult(a, os.getAppFilename())
  cbexc stdinReadLine, EOFError:
    guardEffect:
      setResult(a, "")
      setResult(a, stdin.readLine())
  cbexc stdinReadAll, EOFError:
    guardEffect:
      setResult(a, "")
      setResult(a, stdin.readAll())

proc runNimScript*(cache: IdentCache; scriptName: AbsoluteFile;
                   freshDefines=true; conf: ConfigRef, stream: PLLStream) =
  conf.symbolFiles = disabledSf

  let graph = newModuleGraph(cache, conf)
  connectCallbacks(graph)
  if freshDefines:
    initDefines(conf.symbols)

  defineSymbol(conf, "nimscript")
  registerPass(graph, semPass)
  registerPass(graph, evalPass)

  conf.searchPathsAdd(conf.libpath)

  var m = graph.makeModule(scriptName)
  incl(m.flags, sfMainModule)
  # use a dedicated ID generator for the module; don't reuse the graph's
  let idgen = idGeneratorFromModule(m)

  var vm = setupVM(m, cache, scriptName.string, graph, idgen)
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
  registerAdditionalOps(vm, disallowDanger)
  graph.vm = PEvalContext(vm: vm)

  graph.compileSystemModule()
  discard graph.processModule(m, idgen, stream)

  undefSymbol(conf, "nimscript")
