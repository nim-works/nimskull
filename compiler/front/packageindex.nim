## Package index loading and validation.

import
  std/[
    json,
    os,
    strutils,
    tables
  ],
  compiler/ast/[
    lineinfos
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/utils/[
    pathutils,
  ]

from compiler/ast/reports_packages import PackageReport
from compiler/ast/report_enums import ReportKind

proc resolvePackagePaths(conf: ConfigRef, package: var IndexedPackage, baseDir: string) =
  if package.path.len == 0: return
  
  if not package.path.isAbsolute:
    package.path = baseDir / package.path
  package.path.normalizePath()
  
  if not package.srcDir.isAbsolute:
    package.srcDir = package.path / package.srcDir
  package.srcDir.normalizePath()
  if not package.srcDir.isRelativeTo(package.path):
    localReport(conf, PackageReport(kind: rpkgSrcDirNotRelativeToPackageDir,
      subject: $package.srcDir, target: $package.path))

  if not package.entrypoint.isAbsolute:
    package.entrypoint = package.srcDir / package.entrypoint
  package.entrypoint.normalizePath()
  if not package.entrypoint.isRelativeTo(package.path):
    localReport(conf, PackageReport(kind: rpkgEntrypointNotRelativeToPackageDir,
      subject: $package.entrypoint, target: $package.srcDir))

proc checkDuplicateAliases(conf: ConfigRef, packages: Table[string, IndexedPackage]) =
  ## Emits a warning for each duplicate alias inside a package's dependencies.
  for id, pkg in packages.pairs:
    var seen: Table[string, string]  # alias -> package id
    for dep in pkg.dependencies:
      let alias = dep.alias.nimIdentNormalize()
      if alias in seen:
        localReport(conf, PackageReport(
          kind: rpkgDuplicateAliasForPackageDependencies,
          parentPackage: pkg.path,
          package: seen[alias],
          alias: alias
        ))
      else:
        seen[alias] = dep.package

proc loadPackageIndexFile*(conf: ConfigRef; searchDir: string): tuple[found: bool, dir: string] =
  ## Searches upward from `searchDir` looking for `.skull/index.json`.
  ## If found, parse and load the index, if malformed, reports an error.
  ## Returns a tuple of found and dir, where `found` is a bool and `dir` is
  ## the path to the `.skull` folder (or empty if not found).
  var curDir = searchDir
  while curDir.len > 0:
    let indexPath = curDir / ".skull" / "index.json"
    try:
      conf.packageIndex = parseFile(indexPath).to(PackageIndex)
      return (true, curDir)
    except IOError:
      discard
    except JsonParsingError, JsonKindError:
      localReport(conf, PackageReport(kind: rpkgIndexPresentButMalformed))
      return (true, curDir)
    let parent = curDir.parentDir()
    if parent == curDir: break
    curDir = parent
  result = (false, "")

proc finalizePackageIndex*(conf: ConfigRef; projectDir: string) =
  ## Add `stdlib` and `project-local` packages, resolve all paths,
  ## and check for duplicate aliases. Must be called after loading the index.
  conf.packageIndex.packages["stdlib"] = IndexedPackage(path: $conf.libpath)
  conf.packageIndex.packages["unknown"] = IndexedPackage(path: "")
  if conf.packageDir.isEmpty and not projectDir.startsWith($conf.libpath):
    conf.packageIndex.packages["project-local"] = IndexedPackage(path: projectDir)
  for name, pkg in conf.packageIndex.packages.mpairs:
    pkg.dependencies.add DependencyLink(package: "stdlib", alias: "std")
    conf.resolvePackagePaths(pkg, $conf.packageDir)
  # Validate dependencies
  checkDuplicateAliases(conf, conf.packageIndex.packages)

proc loadPackageIndex*(conf: ConfigRef) =
  ## Load the package index from disk.
  let (found, dir) = loadPackageIndexFile(conf, $conf.projectPath)
  conf.packageDir = if found: AbsoluteDir dir else: AbsoluteDir""
  finalizePackageIndex(conf, $conf.projectPath)