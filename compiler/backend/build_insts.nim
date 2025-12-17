import
  compiler/backend/[
    extccomp
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    pathutils,
  ],
  compiler/ast/[
    lineinfos,
  ],
  std/[
    jsonutils,
    sequtils,
    strutils,
    sugar,
    json,
    sha1,
    os
  ]

from compiler/ast/report_enums import ReportKind
from compiler/ast/reports_cmd import CmdReport
from compiler/ast/reports_backend import BackendReport

template writePrettyCmds(cmd: CmdReport) =
  if cmd.msg.len > 0:
    # TODO: don't use `localReport`. Log the message/diagnostic directly
    conf.localReport(cmd)

template hashNimExe(): string = $secureHashFile(os.getAppFilename())

proc getBuildInstructionsFile*(conf: ConfigRef): AbsoluteFile =
  # `outFile` is better than `projectName`, as it allows having different json
  # files for a given source file compiled with different options; it also
  # works out of the box with `hashMainCompilationParams`.
  result = getNimcacheDir(conf) / conf.outFile.changeFileExt("json")

const cacheVersion = "D20251217T200000" # update when `BuildCache` spec changes
type BuildCache = object
  cacheVersion: string
  outputFile: string
  compile: seq[(string, string)]
  link: seq[string]
  linkcmd: string
  extraCmds: seq[string]
  configFiles: seq[string] # the hash shouldn't be needed
  inputMode: ProjectInputMode
  currentDir: string
  cmdline: string
  depfiles: seq[(string, string)]
  packageIndex: (string, string)
  packageManifests: seq[(string, string)]
  nimexe: string

proc writeBuildInstructions*(conf: ConfigRef) =
  var linkFiles = collect(for it in conf.externalToLink:
    var it = it
    if conf.noAbsolutePaths: it = it.extractFilename
    it.addFileExt(CC[conf.cCompiler].objExt))
  for it in conf.toCompile: linkFiles.add it.obj.string
  var bcache = BuildCache(
    cacheVersion: cacheVersion,
    outputFile: conf.absOutFile.string,
    compile: collect(for i, it in conf.toCompile:
      if CfileFlag.Cached notin it.flags: (it.cname.string, getCompileCFileCmd(conf, it))),
    link: linkFiles,
    linkcmd: getLinkCmd(conf, conf.absOutFile, linkFiles.quoteShellCommand),
    extraCmds: getExtraCmds(conf, conf.absOutFile),
    inputMode: conf.inputMode,
    configFiles: conf.configFiles.mapIt(it.string),
    currentDir: getCurrentDir())
  if optRun in conf.globalOptions or isDefined(conf, "nimBetterRun"):
    bcache.cmdline = conf.commandLine
    bcache.depfiles = collect(for it in conf.m.fileInfos:
      let path = it.fullPath.string
      if isAbsolute(path): # TODO: else?
        (path, $secureHashFile(path)))
    bcache.nimexe = hashNimExe()
    bcache.packageIndex = block:
      let path = $conf.packageDir / ".skull" / "index.json"
      (path, $secureHashFile(path))
    bcache.packageManifests = collect(for pkg in conf.packageIndex.packages:
      let path = absolutePath($conf.packageDir / $pkg.path / "package.skull.toml")
      (path, $secureHashFile(path))
    )
  conf.jsonBuildFile = conf.getBuildInstructionsFile()
  conf.jsonBuildFile.string.writeFile(bcache.toJson.pretty)

proc buildInstructionsChanges*(conf: ConfigRef; jsonFile: AbsoluteFile): bool =
  if not fileExists(jsonFile) or not fileExists(conf.absOutFile): return true
  var bcache: BuildCache
  try: bcache.fromJson(jsonFile.string.parseFile)
  except IOError, OSError, ValueError:
    echo getCurrentException().msg
    stderr.write "Warning: JSON processing failed for: $#\n" % jsonFile.string
    return true
  if bcache.currentDir != getCurrentDir() or # fixes bug #16271
     bcache.configFiles != conf.configFiles.mapIt(it.string) or
     bcache.cacheVersion != cacheVersion or bcache.outputFile != conf.absOutFile.string or
     bcache.cmdline != conf.commandLine or bcache.nimexe != hashNimExe() or
     bcache.inputMode != conf.inputMode: return true
  if bcache.inputMode != pimFile: return true
    # xxx optimize by returning false if stdin input was the same
  for (file, hash) in bcache.depfiles:
    if $secureHashFile(file) != hash: return true
  block:
    let file = $conf.packageDir / ".skull" / "index.json"
    if $secureHashFile(file) != bcache.packageIndex[1]: return true
  for (file, hash) in bcache.packageManifests:
    if $secureHashFile(file) != hash: return true

proc runBuildInstructions*(conf: ConfigRef; jsonFile: AbsoluteFile) =
  var bcache: BuildCache
  try: bcache.fromJson(jsonFile.string.parseFile)
  except:
    let e = getCurrentException()
    conf.quitOrRaise "\ncaught exception:\n$#\nstacktrace:\n$#error evaluating JSON file: $#" %
      [e.msg, e.getStackTrace(), jsonFile.string]
  let output = bcache.outputFile
  createDir output.parentDir
  let outputCurrent = $conf.absOutFile
  if output != outputCurrent or bcache.cacheVersion != cacheVersion:
    conf.globalReport BackendReport(
      kind: rbackJsonScriptMismatch,
      jsonScriptParams: (outputCurrent, output, jsonFile.string))

  var cmds: TStringSeq
  var prettyCmds: seq[CmdReport]
  let prettyCb = proc (idx: int) = writePrettyCmds(prettyCmds[idx])
  for (name, cmd) in bcache.compile:
    cmds.add cmd
    prettyCmds.add displayProgressCC(conf, name, cmd)

  execCmdsInParallel(conf, cmds, prettyCb)
  execLinkCmd(conf, bcache.linkcmd)

  for cmd in bcache.extraCmds:
    execExternalProgram(conf, cmd, rcmdExecuting)