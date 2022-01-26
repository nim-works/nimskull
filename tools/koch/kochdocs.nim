## Part of 'koch' responsible for the documentation generation.

import os, strutils, osproc, sets, pathnorm, sequtils, json
# XXX: Remove this feature check once the csources supports it.
when defined(nimHasCastPragmaBlocks):
  import std/pegs
from std/private/globs import nativeToUnixPath, walkDirRecFilter, PathEntry
from packages/docutils/highlite import nimKeywordsSynchronizationCheck
import ".."/".."/compiler/utils/[nversion]
import ".."/".."/compiler/modules/[nimpaths]

const
  # errormax: subsequent errors are probably consequences of 1st one; a simple
  # bug could cause unlimited number of errors otherwise, hard to debug in CI.
  docDefines = "-d:nimExperimentalAsyncjsThen -d:nimExperimentalLinenoiseExtra"
  nimArgs = "--errormax:3 --hint:Conf:off --hint:Path:off --hint:Processing:off --hint:XDeclaredButNotUsed:off --warning:UnusedImport:off -d:boot $#" % [docDefines]
  gitUrl = "https://github.com/nim-works/nimskull"
  docHtmlOutput = "doc/html"

var nimExe*: string
var nimSource*: string
  ## The compiler source code location
const allowList = ["jsbigints.nim", "jsheaders.nim", "jsformdata.nim", "jsfetch.nim", "jsutils.nim"]

template isJsOnly(file: string): bool =
  file.isRelativeTo("lib/js") or
  file.extractFilename in allowList

proc exe*(f: string): string =
  result = addFileExt(f, ExeExt)
  when defined(windows):
    result = result.replace('/','\\')

proc findNimImpl*(): tuple[path: string, ok: bool] =
  if nimExe.len > 0: return (nimExe, true)
  let nim = "nim".exe
  result.path = "bin" / nim
  result.ok = true
  if fileExists(result.path): return
  for dir in split(getEnv("PATH"), PathSep):
    result.path = dir / nim
    if fileExists(result.path): return
  # assume there is a symlink to the exe or something:
  return (nim, false)

proc findNim*(): string = findNimImpl().path

proc exec*(cmd: string, errorcode: int = QuitFailure, additionalPath = "") =
  let prevPath = getEnv("PATH")
  if additionalPath.len > 0:
    var absolute = additionalPath
    if not absolute.isAbsolute:
      absolute = getCurrentDir() / absolute
    echo("Adding to $PATH: ", absolute)
    putEnv("PATH", (if prevPath.len > 0: prevPath & PathSep else: "") & absolute)
  echo(cmd)
  if execShellCmd(cmd) != 0: quit("FAILURE", errorcode)
  putEnv("PATH", prevPath)

template inFold*(desc, body) =
  if existsEnv("TRAVIS"):
    echo "travis_fold:start:" & desc.replace(" ", "_")
  elif existsEnv("GITHUB_ACTIONS"):
    echo "::group::" & desc
  elif existsEnv("TF_BUILD"):
    echo "##[group]" & desc

  body

  if existsEnv("TRAVIS"):
    echo "travis_fold:end:" & desc.replace(" ", "_")
  elif existsEnv("GITHUB_ACTIONS"):
    echo "::endgroup::"
  elif existsEnv("TF_BUILD"):
    echo "##[endgroup]"

proc execFold*(desc, cmd: string, errorcode: int = QuitFailure, additionalPath = "") =
  ## Execute shell command. Add log folding for various CI services.
  # https://github.com/travis-ci/travis-ci/issues/2285#issuecomment-42724719
  let desc = if desc.len == 0: cmd else: desc
  inFold(desc):
    exec(cmd, errorcode, additionalPath)

proc execCleanPath*(cmd: string,
                   additionalPath = ""; errorcode: int = QuitFailure) =
  # simulate a poor man's virtual environment
  let prevPath = getEnv("PATH")
  when defined(windows):
    let cleanPath = r"$1\system32;$1;$1\System32\Wbem" % getEnv"SYSTEMROOT"
  else:
    const cleanPath = r"/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin"
  putEnv("PATH", cleanPath & PathSep & additionalPath)
  echo(cmd)
  if execShellCmd(cmd) != 0: quit("FAILURE", errorcode)
  putEnv("PATH", prevPath)

proc nimexec*(cmd: string) =
  # Consider using `nimCompile` instead
  exec findNim().quoteShell() & " " & cmd

proc nimexecFold*(desc, cmd: string) =
  # nimexec but with execFold
  execFold desc:
    findNim().quoteShell() & " " & cmd

proc nimCompile*(input: string, outputDir = "bin", mode = "c", options = "") =
  let output = outputDir / input.splitFile.name.exe
  let cmd = findNim().quoteShell() & " " & mode & " -o:" & output & " " & options & " " & input
  exec cmd

proc nimCompileFold*(desc, input: string, outputDir = "bin", mode = "c", options = "", outputName = "") =
  let outputName2 = if outputName.len == 0: input.splitFile.name.exe else: outputName.exe
  let output = outputDir / outputName2
  let cmd = findNim().quoteShell() & " " & mode & " -o:" & output & " " & options & " " & input
  execFold(desc, cmd)

func cmpBase(a, b: Version): int =
  ## Compare only the base version of `a` and `b`.
  result = cmp(a.major, b.major)
  if result == 0:
    result = cmp(a.minor, b.minor)
    if result == 0:
      result = cmp(a.patch, b.patch)

proc getSourceMetadata*(): tuple[hash, date, versionSuffix: string] =
  ## Returns the metadata about Nim's source code
  ##
  ## Empty strings are returned if this information is not available
  # Since obtaining this data requires external calls, we cache them
  var hash {.global.}: string
  var date {.global.}: string
  var versionSuffix {.global.}: string

  if hash == "" or date == "":
    try:
      let releaseMetadata = parseFile(nimSource / "release.json")
      hash = getStr releaseMetadata["commit"]
      date = getStr releaseMetadata["commit_date"]
      let releaseVersion = nversion.parse(getStr releaseMetadata["version"])
      if releaseVersion.cmpBase(CompilerVersion) != 0:
        quit:
          "The compiler base version (" & VersionAsString & ")" &
          " is different from release metadata: " & $releaseVersion

      versionSuffix = releaseVersion.suffix
    except OSError, IOError:
      # If the file does not exist, then this is not a release tarball, try
      # obtaining the data from git instead
      let hashCall = execCmdEx("git -C " & quoteShell(nimSource) & " rev-parse --verify HEAD")
      if hashCall.exitCode == 0:
        hash = hashCall.output.strip()

      let dateCall = execCmdEx("git -C " & quoteShell(nimSource) & " log -1 --format=%cs HEAD")
      if dateCall.exitCode == 0:
        date = dateCall.output.strip()

      let nearestReleaseTagCall = execCmdEx:
        "git" & " -C " & quoteShell(nimSource) & " describe" &
        # Match all tags
        " --tags" &
        # Match versioned releases
        " --match " & quoteShell"*.*.*" &
        # Excluding pre-releases
        " --exclude " & quoteShell"*-*"

      # If there is a tag nearby
      if nearestReleaseTagCall.exitCode == 0:
        # If `-` is not in the tag, it means the tag is of the same commit as
        # what is being compiled.
        if '-' notin nearestReleaseTagCall.output:
          let taggedVersion = nearestReleaseTagCall.output.strip()

          if taggedVersion != VersionAsString:
            quit:
              "The compiler version (" & VersionAsString & ")" &
              " is different from the tagged release: " & taggedVersion

          versionSuffix = ""

        # If `-` is in the tag, then the tag is of a previous release.
        else:
          let
            # Git produces the following format: <tag>-<distance>-g<short commit id>
            #
            # We are interested in the tag and the commit distance.
            splitOutput = nearestReleaseTagCall.output.strip().rsplit('-')
            taggedVersion = nversion.parse(splitOutput[0])
            distance = splitOutput[1] # The distance from the last tag

          # In case the previous release version is equal to or larger than this release
          if taggedVersion.cmpBase(CompilerVersion) >= 0:
            quit:
              "The pre-release compiler version (" & VersionAsString & ")" &
              " is equal or smaller to a tagged release (" & $taggedVersion & ")." &
              " Consider bumping the compiler version in compiler/nversion.nim."

          # Use a suffix of -dev.<distance-from-last-tag>
          versionSuffix = "-dev" & '.' & distance

      # If there are no tags at all
      else:
        # Count the number of commits in the repo
        let commitCountCall = execCmdEx:
          "git -C " & quoteShell(nimSource) & " rev-list --count HEAD"

        if commitCountCall.exitCode == 0:
          let commitCount = commitCountCall.output.strip()

          # Use a suffix of -dev.<number-of-commit-since-first-commit>
          versionSuffix = "-dev" & '.' & commitCount

        else:
          echo "Warning: could not verify with git whether this is a release or pre-release"

          # TODO: maybe have a suffix for this case

      let dirtyIndexCall = execCmdEx:
        "git -C " & quoteShell(nimSource) & " diff-index --quiet HEAD"

      if dirtyIndexCall.exitCode == 1:
        # If there are uncommitted changes, mark the version as "dirty"
        versionSuffix &= "+dirty"

  result = (hash, date, versionSuffix)

proc targetCompilerVersion*(): string =
  ## Return the compiler version to be built as a string
  VersionAsString & getSourceMetadata().versionSuffix

proc getRst2html(): seq[string] =
  for a in walkDirRecFilter("doc"):
    let path = a.path
    if a.kind == pcFile and path.splitFile.ext == ".rst" and path.lastPathPart notin
        ["docs.rst", "nimfix.rst"]:
          # maybe we should still show nimfix, could help reviving it
          # `docs` is redundant with `overview`, might as well remove that file?
      result.add path
  doAssert "doc/manual/var_t_return.rst".unixToNativePath in result # sanity check

const
  rstPdfList = """
manual.rst
lib.rst
tut1.rst
tut2.rst
tut3.rst
nimc.rst
niminst.rst
gc.rst
""".splitWhitespace().mapIt("doc" / it)

  doc0 = """
lib/system/threads.nim
lib/system/channels_builtin.nim
""".splitWhitespace() # ran by `nim doc0` instead of `nim doc`

  withoutIndex = """
lib/wrappers/mysql.nim
lib/wrappers/sqlite3.nim
lib/wrappers/postgres.nim
lib/wrappers/tinyc.nim
lib/wrappers/odbcsql.nim
lib/wrappers/pcre.nim
lib/wrappers/openssl.nim
lib/posix/posix.nim
lib/posix/linux.nim
lib/posix/termios.nim
""".splitWhitespace()

  # some of these are include files so shouldn't be docgen'd
  ignoredModules = """
lib/pure/future.nim
lib/pure/collections/hashcommon.nim
lib/pure/collections/tableimpl.nim
lib/pure/collections/setimpl.nim
lib/pure/ioselects/ioselectors_kqueue.nim
lib/pure/ioselects/ioselectors_select.nim
lib/pure/ioselects/ioselectors_poll.nim
lib/pure/ioselects/ioselectors_epoll.nim
lib/posix/posix_macos_amd64.nim
lib/posix/posix_other.nim
lib/posix/posix_nintendoswitch.nim
lib/posix/posix_nintendoswitch_consts.nim
lib/posix/posix_linux_amd64.nim
lib/posix/posix_linux_amd64_consts.nim
lib/posix/posix_other_consts.nim
lib/posix/posix_freertos_consts.nim
lib/posix/posix_openbsd_amd64.nim
lib/posix/posix_haiku.nim
""".splitWhitespace()

when (NimMajor, NimMinor) < (1, 1) or not declared(isRelativeTo):
  proc isRelativeTo(path, base: string): bool =
    let path = path.normalizedPath
    let base = base.normalizedPath
    let ret = relativePath(path, base)
    result = path.len > 0 and not ret.startsWith ".."

proc getDocList(): seq[string] =
  var docIgnore: HashSet[string]
  for a in doc0: docIgnore.incl a
  for a in withoutIndex: docIgnore.incl a
  for a in ignoredModules: docIgnore.incl a

  # don't ignore these even though in lib/system (not include files)
  const goodSystem = """
lib/system/io.nim
lib/system/nimscript.nim
lib/system/assertions.nim
lib/system/iterators.nim
lib/system/dollars.nim
lib/system/widestrs.nim
""".splitWhitespace()

  proc follow(a: PathEntry): bool =
    result = a.path.lastPathPart notin ["nimcache", htmldocsDirname,
                                        "includes", "deprecated", "genode"] and
      not a.path.isRelativeTo("lib/fusion") # fusion was un-bundled but we need to keep this in case user has it installed
  for entry in walkDirRecFilter("lib", follow = follow):
    let a = entry.path
    if entry.kind != pcFile or a.splitFile.ext != ".nim" or
       (a.isRelativeTo("lib/system") and a.nativeToUnixPath notin goodSystem) or
       a.nativeToUnixPath in docIgnore:
         continue
    result.add a
  result.add normalizePath("nimsuggest/sexp.nim")

let doc = getDocList()

proc sexec(cmds: openArray[string]) =
  ## Serial queue wrapper around exec.
  for cmd in cmds:
    echo(cmd)
    let (outp, exitCode) = osproc.execCmdEx(cmd)
    if exitCode != 0: quit outp

proc mexec(cmds: openArray[string]) =
  ## Multiprocessor version of exec
  let r = execProcesses(cmds, {poStdErrToStdOut, poParentStreams, poEchoCmd})
  if r != 0:
    echo "external program failed, retrying serial work queue for logs!"
    sexec(cmds)

proc buildDocSamples(nimArgs, destPath: string) =
  ## Special case documentation sample proc.
  ##
  ## TODO: consider integrating into the existing generic documentation builders
  ## now that we have a single `doc` command.
  exec(findNim().quoteShell() & " doc $# -o:$# $#" %
    [nimArgs, destPath / "docgen_sample.html", "doc" / "docgen_sample.nim"])

proc buildDocPackages(nimArgs, destPath: string) =
  # compiler docs; later, other packages (perhaps tools, testament etc)
  let nim = findNim().quoteShell()
    # to avoid broken links to manual from compiler dir, but a multi-package
    # structure could be supported later

  proc docProject(outdir, options, mainproj: string) =
    exec("$nim doc --project --outdir:$outdir --git.url:$gitUrl $options $nimArgs $mainproj" % [
      "nim", nim,
      "outdir", outdir,
      "nimArgs", nimArgs,
      "gitUrl", gitUrl,
      "options", options,
      "mainproj", mainproj,
      ])
  let extra = "-u:boot"
  # xxx keep in sync with what's in $nim_prs_D/config/nimdoc.cfg, or, rather,
  # start using nims instead of nimdoc.cfg
  docProject(destPath/"compiler", extra, "compiler/index.nim")

proc buildDoc(nimArgs, destPath: string) =
  # call nim for the documentation:
  let rst2html = getRst2html()
  var
    commands = newSeq[string](rst2html.len + len(doc0) + len(doc) + withoutIndex.len)
    i = 0
  let nim = findNim().quoteShell()
  for d in items(rst2html):
    commands[i] = nim & " rst2html --git.url:$# -o:$# --index:on $# $#" %
      [gitUrl, destPath / changeFileExt(splitFile(d).name, "html"), nimArgs, d]
    i.inc
  for d in items(doc0):
    commands[i] = nim & " doc0 --git.url:$# -o:$# --index:on $# $#" %
      [gitUrl, destPath / changeFileExt(splitFile(d).name, "html"), nimArgs, d]
    i.inc
  for d in items(doc):
    let extra = if isJsOnly(d): "--backend:js" else: ""
    var nimArgs2 = nimArgs
    if d.isRelativeTo("compiler"): doAssert false
    commands[i] = nim & " doc $# --git.url:$# --outdir:$# --index:on $# $#" %
      [extra, gitUrl, destPath, nimArgs2, d]
    i.inc
  for d in items(withoutIndex):
    commands[i] = nim & " doc --git.url:$# -o:$# $# $#" %
      [gitUrl, destPath / changeFileExt(splitFile(d).name, "html"), nimArgs, d]
    i.inc

  mexec(commands)
  exec(nim & " buildIndex -o:$1/theindex.html $1" % [destPath])
    # caveat: this works so long it's called before `buildDocPackages` which
    # populates `compiler/` with unrelated idx files that shouldn't be in index,
    # so should work in CI but you may need to remove your generated html files
    # locally after calling `./koch docs`. The clean fix would be for `idx` files
    # to be transient with `--project` (eg all in memory).

proc nim2pdf(src: string, dst: string, nimArgs: string) =
  # xxx expose as a `nim` command or in some other reusable way.
  let outDir = "build" / "xelatextmp" # xxx factor pending https://github.com/timotheecour/Nim/issues/616
  # note: this will generate temporary files in gitignored `outDir`: aux toc log out tex
  exec("$# rst2tex $# --outdir:$# $#" % [findNim().quoteShell(), nimArgs, outDir.quoteShell, src.quoteShell])
  let texFile = outDir / src.lastPathPart.changeFileExt("tex")
  for i in 0..<3: # call LaTeX three times to get cross references right:
    let xelatexLog = outDir / "xelatex.log"
    # `>` should work on windows, if not, we can use `execCmdEx`
    let cmd = "xelatex -interaction=nonstopmode -output-directory=$# $# > $#" % [outDir.quoteShell, texFile.quoteShell, xelatexLog.quoteShell]
    exec(cmd) # on error, user can inspect `xelatexLog`
  moveFile(texFile.changeFileExt("pdf"), dst)

proc nimKeywordsBuildCheck() =
  ## check to see if `highlite` and `docs/keywords.txt` are in sync as part of
  ## the build.
  nimKeywordsSynchronizationCheck(readFile("doc/keywords.txt"))

proc buildPdfDoc*(nimArgs, destPath: string) =
  var pdfList: seq[string]
  nimKeywordsBuildCheck()
  createDir(destPath)
  if os.execShellCmd("xelatex -version") != 0:
    doAssert false, "xelatex not found" # or, raise an exception
  else:
    for src in items(rstPdfList):
      let dst = destPath / src.lastPathPart.changeFileExt("pdf")
      pdfList.add dst
      nim2pdf(src, dst, nimArgs)
  echo "\nOutput PDF files: \n  ", pdfList.join(" ") # because `nim2pdf` is a bit verbose

proc buildJS(): string =
  let nim = findNim()
  result = getDocHacksJs(nimr = getCurrentDir(), nim)

proc buildDocsDir*(args: string, dir: string) =
  let args =
    nimArgs &
    " " & quoteShell("--putenv:nimversion=" & targetCompilerVersion()) &
    " " & args
  let docHackJsSource = buildJS()
  createDir(dir)
  nimKeywordsBuildCheck()
  buildDocSamples(args, dir)
  buildDoc(args, dir) # bottleneck
  copyFile(dir / "overview.html", dir / "index.html")
  buildDocPackages(args, dir)
  copyFile(docHackJsSource, dir / docHackJsSource.lastPathPart)

proc buildDocs*(args: string) =
  nimKeywordsBuildCheck()

  var args = args

  buildDocsDir(args, docHtmlOutput)
