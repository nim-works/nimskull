## Implements a cache for build instructions, plus the routines for interacting
## with the cache.

import
  std/[
    jsonutils,
    sequtils,
    strutils,
    tables,
    sugar,
    json,
    sha1,
    os
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
  ]

type
  BuildCache* = object
    cacheVersion*: string
    outputFile*: string
    compile*: seq[(string, string)]
    link*: seq[string]
    linkcmd*: string
    extraCmds*: seq[string]
    configFiles: seq[string] # the hash shouldn't be needed
    inputMode: ProjectInputMode
    currentDir: string
    cmdline: string
    depfiles: seq[(string, string)]
    packageIndex: (string, string)
    packageManifests: seq[(string, string)]
    nimexe: string

  BuildChangeKind* = enum
    bcNone, bcGeneral, bcPackage

const cacheVersion* = "D20251217T200000" # update when `BuildCache` spec changes

template hashNimExe(): string = $secureHashFile(os.getAppFilename())

proc getBuildInstructionsFile*(conf: ConfigRef): AbsoluteFile =
  # `outFile` is better than `projectName`, as it allows having different json
  # files for a given source file compiled with different options; it also
  # works out of the box with `hashMainCompilationParams`.
  result = getNimcacheDir(conf) / conf.outFile.changeFileExt("json")

proc writeBuildInstructions*(conf: ConfigRef; bcache: sink BuildCache) =
  ## Populates shared build data and writes it to `outFile`.
  bcache.cacheVersion = cacheVersion
  bcache.outputFile = conf.absOutFile.string
  bcache.inputMode = conf.inputMode
  bcache.configFiles = conf.configFiles.mapIt(it.string)
  bcache.currentDir = getCurrentDir()

  if optRun in conf.globalOptions or isDefined(conf, "nimBetterRun"):
    bcache.cmdline = conf.commandLine
    bcache.depfiles = collect(for it in conf.m.fileInfos:
      let path = it.fullPath.string
      if isAbsolute(path):
        (path, $secureHashFile(path)))
    bcache.nimexe = hashNimExe()

    if dirExists(conf.packageDir):
      bcache.packageIndex = block:
        let path = $conf.packageDir / ".skull" / "index.json"
        if fileExists(path): (path, $secureHashFile(path)) else: ("", "")

      bcache.packageManifests = collect(for pkg in conf.packageIndex.packages.values:
        let manifestPath = $pkg.path / "package.skull.toml"
        if fileExists(manifestPath):
          (manifestPath, $secureHashFile(manifestPath))
      )

  conf.jsonBuildFile = conf.getBuildInstructionsFile()
  conf.jsonBuildFile.string.writeFile(bcache.toJson.pretty)

proc buildInstructionsStatus*(conf: ConfigRef; jsonFile: AbsoluteFile): BuildChangeKind =
  ## Returns true if the build instructions are out of date.
  if not fileExists(jsonFile) or not fileExists(conf.absOutFile): return bcGeneral
  var bcache: BuildCache
  try: bcache.fromJson(jsonFile.string.parseFile)
  except IOError, OSError, ValueError:
    echo getCurrentException().msg
    stderr.write "Warning: JSON processing failed for: $#\n" % jsonFile.string
    return bcGeneral
  if bcache.currentDir != getCurrentDir() or # fixes bug #16271
     bcache.configFiles != conf.configFiles.mapIt(it.string) or
     bcache.cacheVersion != cacheVersion or bcache.outputFile != conf.absOutFile.string or
     bcache.cmdline != conf.commandLine or bcache.nimexe != hashNimExe() or
     bcache.inputMode != conf.inputMode: return bcGeneral
  if bcache.inputMode != pimFile: return bcGeneral
    # xxx optimize by returning false if stdin input was the same
  for (file, hash) in bcache.depfiles:
    if $secureHashFile(file) != hash: return bcGeneral

  if bcache.packageIndex[0].len > 0:
    let file = $conf.packageDir / ".skull" / "index.json"
    if not fileExists(file): return bcGeneral
    if $secureHashFile(file) != bcache.packageIndex[1]: return bcGeneral
    # If the package index hasn't changed, but a manifest has changed, then
    # report it
    for (file, hash) in bcache.packageManifests:
      if not fileExists(file): return bcPackage
      if $secureHashFile(file) != hash: return bcPackage