#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

iterator myParentDirs(p: string): string =
  # XXX os's parentDirs is stupid (multiple yields) and triggers an old bug...
  var current = p
  while true:
    current = current.parentDir
    if current.len == 0: break
    yield current

proc getFaeFile(conf: ConfigRef; path: string): string =
  var parents = 0
  block packageSearch:
    for d in myParentDirs(path):
      if conf.packageCache.hasKey(d):
        #echo "from cache ", d, " |", packageCache[d], "|", path.splitFile.name
        return conf.packageCache[d]
      inc parents
      for file in walkFiles(d / "package.skull.toml"):
        result = file
        break packageSearch
  # we also store if we didn't find anything:
  for d in myParentDirs(path):
    #echo "set cache ", d, " |", result, "|", parents
    conf.packageCache[d] = result
    dec parents
    if parents <= 0: break

proc getFaePkg(conf: ConfigRef; path: string): string =
  ## returns id to of package, e.g.: `github.com/luyten-orion/faepkg`
  # xxx: make this private
  let file = getFaeFile(conf, path)
  if file.len > 0:
    let manifest = readFile(file)
    for line in manifest.splitLines:
      # TODO: Use parseutils instead?
      if line.replace(" ", "").startsWith("name="):
        let qStart = line.find('"') + 1
        # TODO: Cache this
        result = line[qStart..<line.split('#', 1)[0].rfind('"', qStart)]
        break

proc demanglePackageName*(path: string): string =
  # legacy stuff for backends
  result = path.multiReplace({"@@": "@", "@h": "#", "@s": "/", "@m": "", "@c": ":"})

proc withPackageName*(conf: ConfigRef; path: AbsoluteFile): AbsoluteFile =
  # legacy stuff for backends

  proc getPackageName(conf: ConfigRef; path: string): string =
    ## returns fae package id, e.g.: `github.com/luyten-orion/faepkg`
    result = getFaePkg(conf, path)

  proc fakePackageName(conf: ConfigRef; path: AbsoluteFile): string =
    ## Convert `path` so that 2 modules with same name
    ## in different directory get different name and they can be
    ## placed in a directory.
    ## foo-#head/../bar becomes @foo-@hhead@s..@sbar
    result = "@m" & relativeTo(path, conf.projectPath).string.multiReplace(
      {$os.DirSep: "@s", $os.AltSep: "@s", "#": "@h", "@": "@@", ":": "@c"})

  let x = getPackageName(conf, $path)
  let (p, file, ext) = path.splitFile
  if x == "stdlib":
    # Hot code reloading now relies on 'stdlib_system' names etc.
    result = p / RelativeFile((x & '_' & file) & ext)
  else:
    result = p / RelativeFile(fakePackageName(conf, path))

type
  PkgDesc* = object
    ## describes the package, and optional sub-package, used in conjunction
    ## with a module to determine its relationship to a package.
    # todo: support project/default vs unknown vs explicit package
    case pkgKnown*: bool:
      of true:
        pkgFile*: AbsoluteFile ## if applicable, package file
      of false:
        discard
    pkgRootName*: string  ## name of the package root
    pkgRoot*: AbsoluteDir ## path to the root or project path if unknown pkg
    pkgSubpath*: string   ## if not empty, sub-package it's a part of
    pkgName*: string      ## fully escaped package name with any subpaths, same
                          ## as `pkgRootName` if no subpaths present


proc getPkgDesc*(conf: ConfigRef, modulePath: string): PkgDesc =
  ## get a description of a package for a given module path
  # TODO: reserve 'unknown' as a package root name or change it
  template mangle(s: string): string =
    ## convert a path to a package name part
    s.multiReplace({$os.DirSep: "@s",
                    $os.AltSep: "@s",
                    "#": "@h",
                    "@": "@@",
                    ":": "@c"})
  let pkgFile = getFaeFile(conf, modulePath) # <--- Nimble search here
  var (pkgFileRoot, pkgFileName, _) = pkgFile.splitFile
  let
    pkgId = getFaePkg(conf, modulePath)
    pkgKnown = pkgFileName != ""
  
  echo "pkgFile: ", pkgFile
  echo "pkgId: ", pkgId

  result =
    if pkgKnown:
      PkgDesc(pkgKnown: true,
              pkgFile: AbsoluteFile pkgFile,
              pkgRootName: pkgId, pkgRoot: AbsoluteDir pkgFileRoot)
    else:
      # TODO: Investigate all the places "unknown" is used?
      PkgDesc(pkgKnown: false,
              pkgRootName: "unknown", pkgRoot: conf.projectPath)

  result.pkgSubpath =
    block:
      let relativePath = relativePath(modulePath.parentDir,
                                      result.pkgRoot.string)
      if relativePath == ".":
        ""
      else:
        relativePath

  result.pkgName =
    if pkgKnown and result.pkgSubpath == "":
      result.pkgRootName
    else:
      result.pkgRootName & "@p" & mangle(result.pkgSubpath)