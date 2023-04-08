#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Implements some helper procs for Nimble (Nim's package manager) support.

import
  std/[
    parseutils,
    strutils,
    os,
    sha1,
    tables,
    sequtils
  ],
  compiler/front/[
    options,
  ],
  compiler/utils/[
    pathutils
  ]


proc addPath*(conf: ConfigRef; path: AbsoluteDir) =
  if not conf.searchPaths.contains(path):
    conf.active.searchPaths.insert(path, 0)

type
  Version* = distinct string
  NimblePkgAddResult* = enum
    nimblePkgInvalid
    nimblePkgAdded
    nimblePkgUpdated
    nimblePkgOlder
  PackageInfo = Table[string, tuple[version, checksum: string]]
  # xxx: `PackageInfo` should probably be improved to contain all the necessary
  #      information and then be treated as the overall result.

proc `$`*(ver: Version): string {.borrow.}

proc newVersion*(ver: string): Version =
  doAssert(ver.len == 0 or ver[0] in {'#', '\0'} + Digits,
           "Wrong version: " & ver)
  return Version(ver)

func isSpecial(ver: Version): bool =
  ($ver).len > 0 and ($ver)[0] == '#'

func isValidVersion(v: string): bool =
  v.len > 0 and v[0] in {'#'} + Digits

proc `<`*(ver: Version, ver2: Version): bool =
  ## This is synced from Nimble's version module.

  # Handling for special versions such as "#head" or "#branch".
  if ver.isSpecial or ver2.isSpecial:
    if ver2.isSpecial and ($ver2).normalize == "#head":
      return ($ver).normalize != "#head"

    if not ver2.isSpecial:
      # `#aa111 < 1.1`
      return ($ver).normalize != "#head"

  # Handling for normal versions such as "0.1.0" or "1.0".
  var sVer = string(ver).split('.')
  var sVer2 = string(ver2).split('.')
  for i in 0..<max(sVer.len, sVer2.len):
    var sVerI = 0
    if i < sVer.len:
      discard parseInt(sVer[i], sVerI)
    var sVerI2 = 0
    if i < sVer2.len:
      discard parseInt(sVer2[i], sVerI2)
    if sVerI < sVerI2:
      return true
    elif sVerI == sVerI2:
      discard
    else:
      return false

proc getPathVersionChecksum*(p: string): tuple[name, version, checksum: string] =
  ## Splits path ``p`` in the format
  ## ``/home/user/.nimble/pkgs/package-0.1-febadeaea2345e777f0f6f8433f7f0a52edd5d1b`` into
  ## ``("/home/user/.nimble/pkgs/package", "0.1", "febadeaea2345e777f0f6f8433f7f0a52edd5d1b")``

  const checksumSeparator = '-'
  const versionSeparator = '-'
  const specialVersionSepartator = "-#"
  const separatorNotFound = -1

  var checksumSeparatorIndex = p.rfind(checksumSeparator)
  if checksumSeparatorIndex != separatorNotFound:
    result.checksum = p.substr(checksumSeparatorIndex + 1)
    if not result.checksum.isValidSha1Hash():
      result.checksum = ""
      checksumSeparatorIndex = p.len()
  else:
    checksumSeparatorIndex = p.len()

  var versionSeparatorIndex = p.rfind(
    specialVersionSepartator, 0, checksumSeparatorIndex - 1)
  if versionSeparatorIndex != separatorNotFound:
    result.version = p.substr(
      versionSeparatorIndex + 1, checksumSeparatorIndex - 1)
  else:
    versionSeparatorIndex = p.rfind(
      versionSeparator, 0, checksumSeparatorIndex - 1)
    if versionSeparatorIndex != separatorNotFound:
      result.version = p.substr(
        versionSeparatorIndex + 1, checksumSeparatorIndex - 1)
    else:
      versionSeparatorIndex = checksumSeparatorIndex

  result.name = p[0..<versionSeparatorIndex]

proc addPackage(packages: var PackageInfo, p: string): NimblePkgAddResult =
  ## parses a package description string `p`, if valid adds it to `packages`
  ## and returns true, otherwise returns false, as `p` is invalid.
  let (name, ver, checksum) = getPathVersionChecksum(p)
  if isValidVersion(ver):
    let
      version = newVersion(ver)
      exists = packages.hasKey(name)
    if not exists or packages.getOrDefault(name).version.newVersion < version:
      packages[name] = if checksum.isValidSha1Hash(): ($version, checksum)
                       else:                          ($version, "")
      if exists: nimblePkgUpdated else: nimblePkgAdded
    else:
      nimblePkgOlder
  else:
    nimblePkgInvalid

iterator chosen(packages: PackageInfo): string =
  for key, val in pairs(packages):
    var res = key
    if val.version.len != 0:
      res &= '-'
      res &= val.version
    if val.checksum.len != 0:
      res &= '-'
      res &= val.checksum
    yield res

when defined(tnimblecmd): # unit test
  export PackageInfo, chosen, addPackage

proc addNimblePath(conf: ConfigRef; p: string) =
  ## takes the nimble package path `p`, resolves a `nimble-link` if it exists,
  ## then adds that to `conf`'s `lazyPaths`.
  var path = p
  let nimbleLinks = toSeq(walkPattern(p / "*.nimble-link"))
  if nimbleLinks.len > 0:
    # If the user has more than one .nimble-link file then... we just ignore it.
    # Spec for these files is available in Nimble's readme:
    # https://github.com/nim-lang/nimble#nimble-link
    let nimbleLinkLines = readFile(nimbleLinks[0]).splitLines()
    path = nimbleLinkLines[1]
    if not path.isAbsolute():
      path = p / path

  if not conf.searchPaths.contains(AbsoluteDir path):
    conf.active.lazyPaths.insert(AbsoluteDir path, 0)

type
  NimbleDirAdd = seq[tuple[path: string, status: NimblePkgAddResult]]

proc addPathRec(conf: ConfigRef; dir: string): NimbleDirAdd =
  var
    packages: PackageInfo
    pos = dir.len-1
  if dir[pos] in {DirSep, AltSep}: inc(pos)
  for k, p in os.walkDir(dir):
    if k == pcDir and p[pos] != '.':
      let status = addPackage(packages, p)
      case status
      of nimblePkgInvalid, nimblePkgAdded, nimblePkgUpdated:
        result.add (path: p, status: status)
      of nimblePkgOlder:
        discard "we ignore these"
  for p in packages.chosen:
    addNimblePath(conf, p)

type
  NimblePathResult* = object
    pkgs*: NimbleDirAdd
    addedPaths*: seq[AbsoluteDir]

proc nimblePath*(conf: ConfigRef; path: AbsoluteDir): NimblePathResult =
  let initialPathCount = conf.active.lazyPaths.len
  result.pkgs = addPathRec(conf, path.string)
  addNimblePath(conf, path.string)
  let currentPathCount = conf.active.lazyPaths.len

  if initialPathCount < currentPathCount:
    let totalAddedPaths = currentPathCount - initialPathCount
    result.addedPaths = newSeqOfCap[AbsoluteDir](totalAddedPaths)
    for i in 0..<totalAddedPaths:
      let idx = totalAddedPaths - 1 - i
      # add them in insertion order
      result.addedPaths.add conf.active.lazyPaths[idx]

  let i = conf.nimblePaths.find(path)
  if i != -1:
    conf.active.nimblePaths.delete(i)
  conf.active.nimblePaths.insert(path, 0)
