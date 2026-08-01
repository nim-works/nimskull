#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

proc getPackageId*(conf: ConfigRef; path: string): string =
  ## returns the id of a package from its path or an empty string if not
  ## found. e.g.: `github.com/luyten-orion/faepkg`
  var d = path
  if not d.dirExists(): d = d.parentDir

  if d.len > 0 and conf.packageCache.hasKey(d):
    return conf.packageCache[d]
  
  var
    owningId = ""
    maxPathLen = -1

  for id, pkg in conf.packageIndex.packages.pairs:
    if path.startsWith($pkg.path) and ($pkg.path).len > maxPathLen:
      maxPathLen = pkg.path.len
      owningId = id
  
  result = owningId
  if d.len > 0:
    conf.packageCache[d] = result

proc demanglePackageName*(path: string): string =
  # legacy stuff for backends
  result = path.multiReplace({"@@": "@", "@h": "#", "@s": "/", "@m": "", "@c": ":"})

proc withPackageName*(conf: ConfigRef; path: AbsoluteFile): AbsoluteFile =
  # legacy stuff for backends

  proc fakePackageName(conf: ConfigRef; path: AbsoluteFile): string =
    ## Convert `path` so that 2 modules with same name
    ## in different directory get different name and they can be
    ## placed in a directory.
    ## foo-#head/../bar becomes @foo-@hhead@s..@sbar
    result = "@m" & relativeTo(path, conf.projectPath).string.multiReplace(
      {$os.DirSep: "@s", $os.AltSep: "@s", "#": "@h", "@": "@@", ":": "@c"})

  let x = getPackageId(conf, $path)
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
    pkgKnown*: bool
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
  let pkgId = getPackageId(conf, modulePath)
  let pkgKnown = pkgId.len > 0

  result =
    if pkgKnown:
      let pkg = conf.packageIndex.packages[pkgId]
      PkgDesc(pkgKnown: true,
              pkgRootName: pkgId, pkgRoot: pkg.path.AbsoluteDir)
    else:
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
