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

proc getNimbleFile(conf: ConfigRef; path: string): string =
  ## returns absolute path to nimble file, e.g.: /pathto/cligen.nimble
  # xxx: make this private
  var parents = 0
  block packageSearch:
    for d in myParentDirs(path):
      if conf.packageCache.hasKey(d):
        #echo "from cache ", d, " |", packageCache[d], "|", path.splitFile.name
        return conf.packageCache[d]
      inc parents
      for file in walkFiles(d / "*.nimble"):
        result = file
        break packageSearch
  # we also store if we didn't find anything:
  for d in myParentDirs(path):
    #echo "set cache ", d, " |", result, "|", parents
    conf.packageCache[d] = result
    dec parents
    if parents <= 0: break

proc demanglePackageName*(path: string): string =
  # legacy stuff for backends
  result = path.multiReplace({"@@": "@", "@h": "#", "@s": "/", "@m": "", "@c": ":"})

proc withPackageName*(conf: ConfigRef; path: AbsoluteFile): AbsoluteFile =
  # legacy stuff for backends

  proc getPackageName(conf: ConfigRef; path: string): string =
    ## returns nimble package name, e.g.: `cligen`
    let path = getNimbleFile(conf, path)
    result = path.splitFile.name

  proc fakePackageName(conf: ConfigRef; path: AbsoluteFile): string =
    ## Convert `path` so that 2 modules with same name
    ## in different directory get different name and they can be
    ## placed in a directory.
    ## foo-#head/../bar becomes @foo-@hhead@s..@sbar
    result = "@m" & relativeTo(path, conf.projectPath).string.multiReplace(
      {$os.DirSep: "@s", $os.AltSep: "@s", "#": "@h", "@": "@@", ":": "@c"})

  let x = getPackageName(conf, path.string)
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
  let
    pkgFile = getNimbleFile(conf, modulePath)
    (pkgFileRoot, pkgFileName, _) = pkgFile.splitFile
    pkgKnown = pkgFileName != ""
    pkgRootName = if pkgKnown: pkgFileName else: "unknown"
    pkgRoot = if pkgKnown: pkgFileRoot else: conf.projectPath.string
    relativePath = relativePath(modulePath.parentDir, pkgRoot)
    pkgSubpath = if relativePath == ".": "" else: relativePath
    pkgName =
      if pkgKnown and pkgSubpath == "": pkgRootName
      else: pkgRootName & "@p" & mangle(pkgSubpath)
  result =
    if pkgKnown:
      PkgDesc(pkgKnown: true, pkgFile: AbsoluteFile pkgFile)
    else:
      PkgDesc(pkgKnown: false)
  result.pkgRootName = pkgRootName
  result.pkgRoot = AbsoluteDir pkgRoot
  result.pkgSubpath = pkgSubpath
  result.pkgName = pkgName