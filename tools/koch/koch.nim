#
#
#         Maintenance program for Nim
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#
#    See doc/koch.txt for documentation.
#

when not defined(windows):
  const
    Z3StableCommit = "65de3f748a6812eecd7db7c478d5fc54424d368b" # the version of Z3 that DrNim uses

when defined(gcc) and defined(windows):
  when defined(x86):
    {.link: "icons/koch.res".}
  else:
    {.link: "icons/koch_icon.o".}

when defined(amd64) and defined(windows) and defined(vcc):
  {.link: "icons/koch-amd64-windows-vcc.res".}
when defined(i386) and defined(windows) and defined(vcc):
  {.link: "icons/koch-i386-windows-vcc.res".}

import std/[json, os, strutils, parseopt, osproc]
  # Using `std/os` instead of `os` to fail early if config isn't set up properly.
  # If this fails with: `Error: cannot open file: std/os`, see
  # https://github.com/nim-lang/Nim/pull/14291 for explanation + how to fix.

import builder
import kochdocs
import deps

const VersionAsString = system.NimVersion

const
  HelpText = """
+-----------------------------------------------------------------+
|         Maintenance program for Nim                             |
|             Version $1|
|             (c) 2017 Andreas Rumpf                              |
+-----------------------------------------------------------------+

Usage:
  koch [options] command [options for command]

Options:
  --help, -h               shows this help and quits
  --latest                 bundle the installers with bleeding edge versions of
                           external components.
  --stable                 bundle the installers with stable versions of
                           external components (default).
  --nim:path               use specified path for nim binary. This can also be used to
                           override the bootstrapping compiler.
  --localdocs[:path]       only build local documentations. If a path is not
                           specified (or empty), the default is used.
Possible Commands:
  boot [options]           bootstraps with given command line options
  distrohelper [bindir]    helper for distro packagers

Boot options:
  -d:release               produce a release version of the compiler
  -d:nimUseLinenoise       use the linenoise library for interactive mode
                           `nim secret` (not needed on Windows)
  -d:leanCompiler          produce a compiler without JS codegen or
                           documentation generator in order to use less RAM
                           for bootstrapping

Commands for core developers:
  runCI                    runs continuous integration (CI), e.g. from travis
  docs [options]           generates the full documentation
  csource -d:danger        builds the C sources for installation
  pdf                      builds the PDF documentation
  installdeps [options]    installs external dependency (e.g. tinyc) to dist/
  tests [options]          run the testsuite (run a subset of tests by
                           specifying a category, e.g. `tests cat async`)
  temp options             creates a temporary compiler for testing
  testTools                run tooling testsuite
Web options:
  --googleAnalytics:UA-... add the given google analytics code to the docs. To
                           build the official docs, use UA-48159761-1
"""

let
  nimSource = getEnv("KOCH_NIM_SOURCE")
    ## The Nim source code location as given by `koch.py`

  kochExe =
    when defined(windows):
      # Use the `cmd` wrapper for Windows to automate finding Python
      nimSource / "koch.cmd"
    else:
      nimSource / "koch.py"
    ## The path to `koch`'s launcher

proc kochExec*(cmd: string) =
  exec kochExe.quoteShell & " " & cmd

proc kochExecFold*(desc, cmd: string) =
  execFold(desc, kochExe.quoteShell & " " & cmd)

template withDir(dir, body) =
  let old = getCurrentDir()
  try:
    setCurrentDir(dir)
    body
  finally:
    setCurrentDir(old)

let origDir = getCurrentDir()
if nimSource == "":
  quit "This program is not meant to be executed directly, please use koch.py"
setCurrentDir(nimSource)

proc tryExec(cmd: string): bool =
  echo(cmd)
  result = execShellCmd(cmd) == 0

proc getSourceMetadata(): tuple[hash, date: string] =
  ## Returns the metadata about Nim's source code
  ##
  ## Empty strings are returned if this information is not available
  # Since obtaining this data requires external calls, we cache them
  var hash {.global.}: string
  var date {.global.}: string

  if hash == "" or date == "":
    try:
      let releaseMetadata = parseFile(nimSource / "release.json")
      hash = getStr releaseMetadata["commit"]
      date = getStr releaseMetadata["commit_date"]
    except OSError, IOError:
      # If the file does not exist, then this is not a release tarball, try
      # obtaining the data from git instead
      let hashCall = execCmdEx("git -C " & quoteShell(nimSource) & " rev-parse --verify HEAD")
      if hashCall.exitCode == 0:
        hash = hashCall.output.strip()

      let dateCall = execCmdEx("git -C " & quoteShell(nimSource) & " log -1 --format=%cs HEAD")
      if dateCall.exitCode == 0:
        date = dateCall.output.strip()

  result = (hash, date)

proc defineSourceMetadataArgs(): seq[string] =
  ## Produce arguments to pass to the compiler to embed source metadata in the
  ## built compiler, as a seq
  let (hash, date) = getSourceMetadata()
  if hash != "" and date != "":
    result = @["-d:nimSourceHash=" & hash, "-d:nimSourceDate=" & date]

proc defineSourceMetadata(): string =
  ## Produce arguments to pass to the compiler to embed source metadata in the
  ## built compiler
  quoteShellCommand(defineSourceMetadataArgs())

proc safeRemove(filename: string) =
  if fileExists(filename): removeFile(filename)

proc overwriteFile(source, dest: string) =
  safeRemove(dest)
  moveFile(source, dest)

proc copyExe(source, dest: string) =
  safeRemove(dest)
  copyFile(dest=dest, source=source)
  inclFilePermissions(dest, {fpUserExec, fpGroupExec, fpOthersExec})

const
  compileNimInst = "tools/niminst/niminst"
  distDir = "dist"

proc csource(args: string) =
  nimexec(("cc $1 -r $3 --var:version=$2 --var:mingw=none csource " &
           "--main:compiler/nim.nim $4 compiler/installer.ini $1") %
       [args, VersionAsString, compileNimInst, quoteShell("--nim:" & findNim())])

proc bundleC2nim(args: string) =
  cloneDependency(distDir, "https://github.com/nim-lang/c2nim.git")
  nimCompile("dist/c2nim/c2nim",
             options = "--noNimblePath --path:. " & args)

proc bundleNimsuggest(args: string) =
  nimCompileFold("Compile nimsuggest", "nimsuggest/nimsuggest.nim",
                 options = "-d:danger " & defineSourceMetadata() & " " & args)

proc buildVccTool(args: string) =
  let input = "tools/vccexe/vccexe.nim"
  if contains(args, "--cc:vcc"):
    nimCompileFold("Compile Vcc", input, "build", options = args)
    let fileName = input.splitFile.name
    moveFile(exe("build" / fileName), exe("bin" / fileName))
  else:
    nimCompileFold("Compile Vcc", input, options = args)

proc bundleNimpretty(args: string) =
  nimCompileFold("Compile nimpretty", "nimpretty/nimpretty.nim",
                 options = "-d:release " & defineSourceMetadata() & " " & args)

proc bundleWinTools(args: string) =
  nimCompile("tools/finish.nim", outputDir = "", options = args)

  buildVccTool(args)
  nimCompile("tools/nimgrab.nim", options = "-d:ssl " & args)
  nimCompile("tools/nimgrep.nim", options = args)
  nimCompile("testament/testament.nim", options = args)
  when false:
    # not yet a tool worth including
    nimCompile(r"tools\downloader.nim",
               options = r"--cc:vcc --app:gui -d:ssl --noNimblePath --path:..\ui " & args)

proc ensureCleanGit() =
  let (outp, status) = osproc.execCmdEx("git diff")
  #if outp.len != 0:
  #  quit "Not a clean git repository; 'git diff' not empty!"
  #if status != 0:
  #  quit "Not a clean git repository; 'git diff' returned non-zero!"

proc archive(args: string) =
  ensureCleanGit()
  nimexec("cc -r $2 --var:version=$1 --var:mingw=none --main:compiler/nim.nim scripts compiler/installer.ini" %
       [VersionAsString, compileNimInst])
  let (commit, date) = getSourceMetadata()
  exec("$# --var:version=$# --var:mingw=none --var:commit=$# --var:commitdate=$# --main:compiler/nim.nim --format:tar.xz $# archive compiler/installer.ini" %
       ["tools" / "niminst" / "niminst".exe, VersionAsString, quoteShell(commit), quoteShell(date), args])

proc buildTool(toolname, args: string) =
  nimexec("cc $# $#" % [args, toolname])
  copyFile(dest="bin" / splitFile(toolname).name.exe, source=toolname.exe)

proc buildTools(args: string = "") =
  bundleNimsuggest(args)
  nimCompileFold("Compile nimgrep", "tools/nimgrep.nim",
                 options = "-d:release " & defineSourceMetadata() & " " & args)
  when defined(windows): buildVccTool(args)
  bundleNimpretty(args)
  nimCompileFold("Compile testament", "testament/testament.nim", options = "-d:release " & defineSourceMetadata() & " " & args)

  # pre-packages a debug version of nim which can help in many cases investigate issuses
  # withouth having to rebuild compiler.
  # `-d:nimDebugUtils` only makes sense when temporarily editing/debugging compiler
  # `-d:debug` should be changed to a flag that doesn't require re-compiling nim
  # `--opt:speed` is a sensible default even for a debug build, it doesn't affect nim stacktraces
  nimCompileFold("Compile nim_dbg", "compiler/nim.nim", options =
      "--opt:speed --stacktrace -d:debug --stacktraceMsgs -d:nimCompilerStacktraceHints --excessiveStackTrace:off " & defineSourceMetadata() & " " & args,
      outputName = "nim_dbg")

proc addTools(b: var Builder) =
  let tools = [
    Target b.addExecutable("nimsuggest", "nimsuggest/nimsuggest.nim", "-d:danger"),
    b.addExecutable("nimgrep", "tools/nimgrep.nim", "-d:release"),
    b.addExecutable("nimpretty", "nimpretty/nimpretty.nim", "-d:release"),
    b.addExecutable("testament", "testament/testament.nim", "-d:release"),
    b.addExecutable("nim_dbg", "compiler/nim.nim", "--opt:speed", "--stacktrace", "-d:debug", "--stacktraceMsgs", "-d:nimCompilerStacktraceHints", "--excessiveStackTrace:off")
  ]
  discard b.addAlias("tools", tools)

proc nsis(latest: bool; args: string) =
  bundleNimsuggest(args)
  bundleWinTools(args)
  # make sure we have generated the niminst executables:
  buildTool("tools/niminst/niminst", args)
  #buildTool("tools/nimgrep", args)
  # produce 'nim_debug.exe':
  #exec "nim c compiler" / "nim.nim"
  #copyExe("compiler/nim".exe, "bin/nim_debug".exe)
  exec(("tools" / "niminst" / "niminst --var:version=$# --var:mingw=mingw$#" &
        " nsis compiler/installer.ini") % [VersionAsString, $(sizeof(pointer)*8)])

proc geninstall(args="") =
  nimexec("cc -r $# --var:version=$# --var:mingw=none --main:compiler/nim.nim scripts compiler/installer.ini $#" %
       [compileNimInst, VersionAsString, args])

proc install(args: string) =
  geninstall()
  exec("sh ./install.sh $#" % args)

type
  BinArchiveTarget {.pure.} = enum
    ## Target for the binary archive
    Windows
    Unix

proc binArchive(target: BinArchiveTarget, args: string) =
  ## Builds binary archive for `target`
  # Boot the compiler
  kochExec("boot -d:danger")
  # Build the tools
  buildTools()

  # Build the binary archive
  let binaryArgs =
    case target
    of Windows:
      quoteShellCommand(["--format:zip", "--binaries:windows"])
    of Unix:
      quoteShellCommand(["--format:tar.xz", "--binaries:unix"])

  archive(binaryArgs & " " & args)

proc addArchive(b: var Builder) =
  let
    niminst = b.addExecutable("niminst", "tools/niminst/niminst.nim")

  let
    defaultArgs = [
      "--var:version=" & VersionAsString,
      "--var:mingw=none",
      "--main:" & (nimSource / "compiler" / "nim.nim")
    ]

  let compilerManifest = nimSource / "compiler" / "installer.ini"

  let buildScript: Target = b.addRun("installscripts", niminst, @defaultArgs & @["scripts", compilerManifest])
  discard b.addRun("archive", niminst, @defaultArgs & @["--format:tar.xz", "archive", compilerManifest], dependsOn = {buildScript})
  discard b.addRun("winrelease", niminst, @defaultArgs & @["--format:zip", "--binaries:windows", "archive", compilerManifest], dependsOn = {buildScript})
  discard b.addRun("unixrelease", niminst, @defaultArgs & @["--format:tar.xz", "--binaries:unix", "archive", compilerManifest], dependsOn = {buildScript})

when false:
  proc web(args: string) =
    nimexec("js tools/dochack/dochack.nim")
    nimexec("cc -r tools/nimweb.nim $# web/website.ini --putenv:nimversion=$#" %
        [args, VersionAsString])

  proc website(args: string) =
    nimexec("cc -r tools/nimweb.nim $# --website web/website.ini --putenv:nimversion=$#" %
        [args, VersionAsString])

  proc pdf(args="") =
    exec("$# cc -r tools/nimweb.nim $# --pdf web/website.ini --putenv:nimversion=$#" %
        [findNim().quoteShell(), args, VersionAsString], additionalPATH=findNim().splitFile.dir)

# -------------- boot ---------------------------------------------------------

proc findStartNim: string =
  # Try "bin/nim-boot", which is built by `koch.sh`.
  #
  # The compiler specified by "--nim" is preferred over this.
  #
  # If that fails, we try to build nim with the "build.(sh|bat)" script.
  if nimExe.len > 0:
    return nimExe

  const nimBoot = "bin" / "nim-boot".exe
  if fileExists(nimBoot):
    return nimBoot

  when defined(posix):
    const buildScript = "build.sh"
    if fileExists(buildScript):
      if tryExec("./" & buildScript): return "bin" / "nim".exe
  else:
    const buildScript = "build.bat"
    if fileExists(buildScript):
      if tryExec(buildScript): return "bin" / "nim".exe

  echo("Found no nim compiler and every attempt to build one failed!")
  quit("FAILURE")

proc thVersion(i: int): string =
  result = ("compiler" / "nim" & $i).exe

template doUseCpp(): bool = getEnv("NIM_COMPILE_TO_CPP", "false") == "true"

proc boot(args: string) =
  ## bootstrapping is a process that involves 3 steps:
  ## 1. use csourcesAny to produce nim1.exe. This nim1.exe is buggy but
  ## rock solid for building a Nim compiler. It shouldn't be used for anything else.
  ## 2. use nim1.exe to produce nim2.exe. nim2.exe is the one you really need.
  ## 3. We use nim2.exe to build nim3.exe. nim3.exe is equal to nim2.exe except for timestamps.
  ## This step ensures a minimum amount of quality. We know that nim2.exe can be used
  ## for Nim compiler development.
  var output = "compiler" / "nim".exe
  var finalDest = "bin" / "nim".exe
  # default to use the 'c' command:
  let useCpp = doUseCpp()
  let smartNimcache = (if "release" in args or "danger" in args: "nimcache/r_" else: "nimcache/d_") &
                      hostOS & "_" & hostCPU

  let nimStart = findStartNim().quoteShell()
  for i in 0..2:
    # Nim versions < (1, 1) expect Nim's exception type to have a 'raiseId' field for
    # C++ interop. Later Nim versions do this differently and removed the 'raiseId' field.
    # Thus we always bootstrap the first iteration with "c" and not with "cpp" as
    # a workaround.
    let defaultCommand = if useCpp and i > 0: "cpp" else: "c"
    let bootOptions = if args.len == 0 or args.startsWith("-"): defaultCommand else: ""
    echo "iteration: ", i+1
    # The configs are skipped for bootstrap
    var extraOption = " --skipUserCfg --skipParentCfg" & " " & defineSourceMetadata()
    var nimi = i.thVersion
    var smartNimcache = smartNimcache
    if i == 0:
      nimi = nimStart
      extraOption.add " -d:nimKochBootstrap"

      # Older bootstrapping compiler might not support magics used in the
      # newer stdlib, so disable those warnings.
      extraOption.add " --warning[UnknownMagic]:off"

      # Use a separate cache for bootstrapping, as the bootstrap compiler is
      # (usually) an older version
      smartNimcache.add "_boot"

      let ret = execCmdEx(nimStart & " --version")
      doAssert ret.exitCode == 0
      let version = ret.output.splitLines[0]
      if version.startsWith "Nim Compiler Version 0.20.0":
        extraOption.add " --lib:lib" # see https://github.com/nim-lang/Nim/pull/14291

    # in order to use less memory, we split the build into two steps:
    # --compileOnly produces a $project.json file and does not run GCC/Clang.
    # jsonbuild then uses the $project.json file to build the Nim binary.
    exec "$# $# $# --nimcache:$# $# --compileOnly compiler" / "nim.nim" %
      [nimi, bootOptions, extraOption, smartNimcache, args]
    exec "$# jsonscript $# --nimcache:$# $# compiler" / "nim.nim" %
      [nimi, extraOption, smartNimcache, args]

    if sameFileContent(output, i.thVersion):
      copyExe(output, finalDest)
      echo "executables are equal: SUCCESS!"
      return
    copyExe(output, (i+1).thVersion)
  copyExe(output, finalDest)
  when not defined(windows): echo "[Warning] executables are still not equal"

# -------------- clean --------------------------------------------------------

const
  cleanExt = [
    ".ppu", ".o", ".obj", ".dcu", ".~pas", ".~inc", ".~dsk", ".~dpr",
    ".map", ".tds", ".err", ".bak", ".pyc", ".exe", ".rod", ".pdb", ".idb",
    ".idx", ".ilk"
  ]
  ignore = [
    ".bzrignore", "nim", "nim.exe", "koch", "koch.exe", ".gitignore"
  ]

proc cleanAux(dir: string) =
  for kind, path in walkDir(dir):
    case kind
    of pcFile:
      var (_, name, ext) = splitFile(path)
      if ext == "" or cleanExt.contains(ext):
        if not ignore.contains(name):
          echo "removing: ", path
          removeFile(path)
    of pcDir:
      case splitPath(path).tail
      of "nimcache":
        echo "removing dir: ", path
        removeDir(path)
      of "dist", ".git", "icons": discard
      else: cleanAux(path)
    else: discard

proc removePattern(pattern: string) =
  for f in walkFiles(pattern):
    echo "removing: ", f
    removeFile(f)

proc clean(args: string) =
  removePattern("web/*.html")
  removePattern("doc/*.html")
  cleanAux(getCurrentDir())
  for kind, path in walkDir(getCurrentDir() / "build"):
    if kind == pcDir:
      echo "removing dir: ", path
      removeDir(path)

# -------------- builds a release ---------------------------------------------

proc winReleaseArch(arch: string) =
  doAssert arch in ["32", "64"]
  let cpu = if arch == "32": "i386" else: "amd64"

  template withMingw(path, body) =
    let prevPath = getEnv("PATH")
    putEnv("PATH", (if path.len > 0: path & PathSep else: "") & prevPath)
    try:
      body
    finally:
      putEnv("PATH", prevPath)

  withMingw r"..\mingw" & arch & r"\bin":
    # Rebuilding koch is necessary because it uses its pointer size to
    # determine which mingw link to put in the NSIS installer.
    inFold "winrelease koch":
      nimexec "c --cpu:$# koch" % cpu
    kochExecFold("winrelease boot", "boot -d:release --cpu:$#" % cpu)
    kochExecFold("winrelease zip", "zip -d:release")
    overwriteFile r"build\nim-$#.zip" % VersionAsString,
             r"web\upload\download\nim-$#_x$#.zip" % [VersionAsString, arch]

proc winRelease*() =
  # Now used from "tools/winrelease" and not directly supported by koch
  # anymore!
  # Build -docs file:
  when true:
    inFold "winrelease buildDocs":
      buildDocs(gaCode)
    withDir "web/upload/" & VersionAsString:
      inFold "winrelease zipdocs":
        exec "7z a -tzip docs-$#.zip *.html" % VersionAsString
    overwriteFile "web/upload/$1/docs-$1.zip" % VersionAsString,
                  "web/upload/download/docs-$1.zip" % VersionAsString
  when true:
    inFold "winrelease csource":
      csource("-d:danger")
  when sizeof(pointer) == 4:
    winReleaseArch "32"
  when sizeof(pointer) == 8:
    winReleaseArch "64"

# -------------- tests --------------------------------------------------------

template `|`(a, b): string = (if a.len > 0: a else: b)

proc tests(b: var Builder, args: openArray[string]) =
  let testament = b.fromName[:Executable]("testament")

  # Add the test target depending on testament then build it immediately
  b.build:
    let defaultArgs = @["--nim:" & findNim()]
    let args =
      if args.len > 0:
        defaultArgs & @args
      else:
        defaultArgs & "all"

    b.addRun("tests", testament, args)

proc temp(args: string) =
  proc splitArgs(a: string): (string, string) =
    # every --options before the command (indicated by starting
    # with not a dash) is part of the bootArgs, the rest is part
    # of the programArgs:
    let args = os.parseCmdLine a
    result = ("", "")
    var i = 0
    while i < args.len and args[i][0] == '-':
      result[0].add " " & quoteShell(args[i])
      inc i
    while i < args.len:
      result[1].add " " & quoteShell(args[i])
      inc i

  let output = nimSource / "compiler" / "nim".exe
  let finalDest = nimSource / "bin" / "nim_temp".exe
  # 125 is the magic number to tell git bisect to skip the current commit.
  var (bootArgs, programArgs) = splitArgs(args)
  if "doc" notin programArgs and
      "threads" notin programArgs and
      "js" notin programArgs and "rst2html" notin programArgs:
    bootArgs = " -d:leanCompiler" & bootArgs
  let nimexec = findNim().quoteShell()
  exec(nimexec & " c -d:debug --debugger:native -d:nimBetterRun " & bootArgs & " " & (nimSource / "compiler" / "nim"), 125)
  copyExe(output, finalDest)
  setCurrentDir(origDir)
  if programArgs.len > 0: exec(finalDest & " " & programArgs)

proc xtemp(cmd: string) =
  copyExe(nimSource / "bin" / "nim".exe, nimSource / "bin" / "nim_backup".exe)
  try:
    withDir(nimSource):
      temp""
    copyExe(nimSource / "bin" / "nim_temp".exe, nimSource / "bin" / "nim".exe)
    exec(cmd)
  finally:
    copyExe(nimSource / "bin" / "nim_backup".exe, nimSource / "bin" / "nim".exe)

proc icTest(args: string) =
  temp("")
  let inp = os.parseCmdLine(args)[0]
  let content = readFile(inp)
  let nimExe = nimSource / "bin" / "nim_temp".exe
  var i = 0
  for fragment in content.split("#!EDIT!#"):
    let file = inp.replace(".nim", "_temp.nim")
    writeFile(file, fragment)
    var cmd = nimExe & " cpp --ic:on -d:nimIcIntegrityChecks --listcmd "
    if i == 0:
      cmd.add "-f "
    cmd.add quoteShell(file)
    exec(cmd)
    inc i

proc buildDrNim(args: string) =
  if not dirExists("dist/nimz3"):
    exec("git clone https://github.com/zevv/nimz3.git dist/nimz3")
  when defined(windows):
    if not dirExists("dist/dlls"):
      exec("git clone -q https://github.com/nim-lang/dlls.git dist/dlls")
    copyExe("dist/dlls/libz3.dll", "bin/libz3.dll")
    nimexecFold("build drnim", "c -o:$1 $2 drnim/drnim" % ["bin/drnim".exe, args])
  else:
    if not dirExists("dist/z3"):
      exec("git clone -q https://github.com/Z3Prover/z3.git dist/z3")
      withDir("dist/z3"):
        exec("git fetch")
        exec("git checkout " & Z3StableCommit)
        createDir("build")
        withDir("build"):
          exec("""cmake -DZ3_BUILD_LIBZ3_SHARED=FALSE -G "Unix Makefiles" ../""")
          exec("make -j4")
    nimexecFold("build drnim", "cpp --dynlibOverride=libz3 -o:$1 $2 drnim/drnim" % ["bin/drnim".exe, args])
  # always run the tests for now:
  exec("testament/testament".exe & " --nim:" & "drnim".exe & " pat drnim/tests")


proc hostInfo(): string =
  "hostOS: $1, hostCPU: $2, int: $3, float: $4, cpuEndian: $5, cwd: $6" %
    [hostOS, hostCPU, $int.sizeof, $float.sizeof, $cpuEndian, getCurrentDir()]

proc installDeps(dep: string, commit = "") =
  # the hashes/urls are version controlled here, so can be changed seamlessly
  # and tied to a nim release (mimicking git submodules)
  var commit = commit
  case dep
  of "tinyc":
    if commit.len == 0: commit = "916cc2f94818a8a382dd8d4b8420978816c1dfb3"
    cloneDependency(distDir, "https://github.com/timotheecour/nim-tinyc-archive", commit)
  else: doAssert false, "unsupported: " & dep
  # xxx: also add linenoise, niminst etc, refs https://github.com/nim-lang/RFCs/issues/206

proc testTools(cmd: string) =
  nimexecFold("Run nimdoc tests", "r nimdoc/tester")
  nimexecFold("Run rst2html tests", "r nimdoc/rsttester")
  nimexecFold("Run nimpretty tests", "r nimpretty/tester.nim")
  # refs #18385, build with -d:release instead of -d:danger for testing
  # We could also skip building nimsuggest in buildTools, or build it with -d:release
  # in bundleNimsuggest depending on some environment variable when we are in CI. One advantage
  # of rebuilding is this won't affect bin/nimsuggest when running runCI locally
  nimexecFold("build nimsuggest_testing", "c -o:bin/nimsuggest_testing -d:release nimsuggest/nimsuggest")
  nimexecFold("Run nimsuggest tests", "r nimsuggest/tester")

proc runCI(cmd: string) =
  doAssert cmd.len == 0, cmd # avoid silently ignoring
  echo "runCI: ", cmd
  echo hostInfo()
  # boot without -d:nimHasLibFFI to make sure this still works
  # `--lib:lib` is needed for bootstrap on openbsd, for reasons described in
  # https://github.com/nim-lang/Nim/pull/14291 (`getAppFilename` bugsfor older nim on openbsd).
  kochExecFold("Boot in release mode", "boot -d:release -d:nimStrictMode --lib:lib")

  let batchParam = "--batch:$1" % "NIM_TESTAMENT_BATCH".getEnv("_")
  buildTools()
  ## run tests
  nimexecFold("Test nimscript", "e tests/test_nimscript.nims")
  when defined(windows):
    nimexecFold("Compile tester", "c --usenimcache --os:genode -d:posix --compileOnly testament/testament")

  # main bottleneck here
  # xxx: even though this is the main bottleneck, we could speedup the rest via batching with `--batch`.
  # BUG: with initOptParser, `--batch:'' all` interprets `all` as the argument of --batch, pending bug #14343
  nimexecFold("Run tester", "c -r --putenv:NIM_TESTAMENT_REMOTE_NETWORKING:1 -d:nimStrictMode testament/testament $# all" % batchParam)

  testTools(cmd)

  when not defined(bsd):
    if not doUseCpp:
      # the BSDs are overwhelmed already, so only run this test on the other machines:
      kochExecFold("Boot Nim ORC", "boot -d:release --gc:orc --lib:lib")

proc valgrind(cmd: string) =
  # somewhat hacky: '=' sign means "pass to valgrind" else "pass to Nim"
  let args = parseCmdLine(cmd)
  var nimcmd = ""
  var valcmd = ""
  for i, a in args:
    if i == args.len-1:
      # last element is the filename:
      valcmd.add ' '
      valcmd.add changeFileExt(a, ExeExt)
      nimcmd.add ' '
      nimcmd.add a
    elif '=' in a:
      valcmd.add ' '
      valcmd.add a
    else:
      nimcmd.add ' '
      nimcmd.add a
  nimexec("c" & nimcmd)
  let supp = nimSource / "tools" / "nimgrind.supp"
  exec("valgrind --suppressions=" & supp & valcmd)

proc showHelp(b: Builder, success: bool) =
  var help = HelpText % [VersionAsString & spaces(44-len(VersionAsString))]

  help.add "Builder targets:\p"
  for _, targetName in b.targets:
    help.add:
      "  " & targetName & "\p"

  quit(help):
    if success: QuitSuccess else: QuitFailure

proc branchDone() =
  let thisBranch = execProcess("git symbolic-ref --short HEAD").strip()
  if thisBranch != "devel" and thisBranch != "":
    exec("git checkout devel")
    exec("git branch -D " & thisBranch)
    exec("git pull --rebase")

when isMainModule:
  var op = initOptParser()
  var
    latest = false
    localDocsOnly = false
    localDocsOut = ""
    bld = initBuilder(nimSource, nimSource / "build", findNim())

  bld.addDefaultCompilerArgs(defineSourceMetadataArgs())
  bld.addTools()
  bld.addArchive()
  while true:
    op.next()
    case op.kind
    of cmdLongOption, cmdShortOption:
      case normalize(op.key)
      of "help", "h": showHelp(bld, success = true)
      of "latest": latest = true
      of "stable": latest = false
      of "nim":
        nimExe = op.val.absolutePath # absolute so still works with changeDir
        bld.setCompiler(nimExe)
      of "localdocs":
        localDocsOnly = true
        if op.val.len > 0:
          localDocsOut = op.val.absolutePath
      else: showHelp(bld, success = false)
    of cmdArgument:
      case normalize(op.key)
      of "boot": boot(op.cmdLineRest)
      of "clean": clean(op.cmdLineRest)
      of "doc", "docs": buildDocs(op.cmdLineRest, localDocsOnly, localDocsOut)
      of "doc0", "docs0":
        # undocumented command for Araq-the-merciful:
        buildDocs(op.cmdLineRest & gaCode)
      of "pdf": buildPdfDoc(op.cmdLineRest, "doc/pdf")
      of "csource", "csources": csource(op.cmdLineRest)
      of "nsis": nsis(latest, op.cmdLineRest)
      of "geninstall": geninstall(op.cmdLineRest)
      of "distrohelper": geninstall()
      of "install": install(op.cmdLineRest)
      of "installdeps": installDeps(op.cmdLineRest)
      of "runci": runCI(op.cmdLineRest)
      of "test", "tests": tests(bld, op.remainingArgs)
      of "testtools": testTools(op.cmdLineRest)
      of "temp": temp(op.cmdLineRest)
      of "xtemp": xtemp(op.cmdLineRest)
      of "wintools": bundleWinTools(op.cmdLineRest)
      of "pushcsource":
        quit "use this instead: https://github.com/nim-lang/csources_v1/blob/master/push_c_code.nim"
      of "valgrind": valgrind(op.cmdLineRest)
      of "c2nim": bundleC2nim(op.cmdLineRest)
      of "drnim": buildDrNim(op.cmdLineRest)
      of "ic": icTest(op.cmdLineRest)
      of "branchdone": branchDone()
      else:
        if op.key in bld:
          bld.addDefaultCompilerArgs(op.remainingArgs)
          bld.build(op.key)
        else:
          showHelp(bld, success = false)
      break
    of cmdEnd:
      showHelp(bld, success = false)
