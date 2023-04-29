#
#
#            Nim Tester
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Include for the tester that contains test suites that test special features
## of the compiler.

# included from testament.nim

import std/strformat
import std/private/gitutils

const
  specialCategories = [
    "assert",
    "debugger",
    "dll",
    "gc",
    "js",
    "ic",
    "lib",
    "manyloc",
    "threads",
    "untestable", # see trunner_special
    "testdata",
    "nimcache",
    "osproc",
    "shouldfail",
    "destructor"
  ]

proc isTestFile*(file: string): bool =
  let (_, name, ext) = splitFile(file)
  result = ext == ".nim" and name.startsWith("t")

# --------------------- DLL generation tests ----------------------------------

proc runBasicDLLTest(c, r: var TResults, cat: Category, options: string) =
  const rpath = when defined(macosx):
      " --passL:-rpath --passL:@loader_path"
    else:
      ""

  # XXX: nimrtl is currently defunct and the tests making use of it are
  #      disabled
  when false:
    var test1 = makeTest("lib/nimrtl.nim", options & " --outdir:tests/dll", cat)
    test1.spec.action = actionCompile
    testSpec c, test1
  var test2 = makeTest("tests/dll/server.nim", options & " --threads:on" & rpath, cat)
  test2.spec.action = actionCompile
  testSpec c, test2
  var test4 = makeTest("tests/dll/visibility.nim", options & " --app:lib" & rpath, cat)
  test4.spec.action = actionCompile
  testSpec c, test4

  # windows looks in the dir of the exe (yay!):
  when not defined(windows):
    # posix relies on crappy LD_LIBRARY_PATH (ugh!):
    const libpathenv = when defined(haiku): "LIBRARY_PATH"
                       else: "LD_LIBRARY_PATH"
    var libpath = getEnv(libpathenv)
    # Temporarily add the lib directory to LD_LIBRARY_PATH:
    putEnv(libpathenv, "tests/dll" & (if libpath.len > 0: ":" & libpath else: ""))
    defer: putEnv(libpathenv, libpath)

  testSpec r, makeTest("tests/dll/client.nim", options & " --threads:on" & rpath, cat)
  testSpec r, makeTest("tests/dll/visibility.nim", options & rpath, cat)

  if "boehm" notin options:
    # force build required - see the comments in the .nim file for more details
    for target in cat.defaultTargets():
      var hcri = makeTest("tests/dll/nimhcr_integration.nim",
                                    options & " --forceBuild --hotCodeReloading:on" & rpath, cat)
      let nimcache = nimcacheDir(hcri.name, hcri.options, target)
      let cmd = prepareTestCmd(hcri.spec.getCmd, hcri.name,
                                  hcri.options, nimcache, target)
      hcri.testArgs = cmd.parseCmdLine
      testSpec r, hcri

proc dllTests(r: var TResults, cat: Category, options: string) =
  # dummy compile result:
  var c = initResults()

  runBasicDLLTest c, r, cat, options
  runBasicDLLTest c, r, cat, options & " -d:release"
  when not defined(windows):
    # still cannot find a recent Windows version of boehm.dll:
    runBasicDLLTest c, r, cat, options & " --gc:boehm"
    runBasicDLLTest c, r, cat, options & " -d:release --gc:boehm"

# ------------------------------ GC tests -------------------------------------

proc gcTests(r: var TResults, cat: Category, options: string) =
  template testWithoutMs(filename: untyped) =
    testSpec r, makeTest("tests/gc" / filename, options, cat)
    testSpec r, makeTest("tests/gc" / filename, options &
                  " -d:release -d:useRealtimeGC --gc:refc", cat)
    when filename != "gctest":
      testSpec r, makeTest("tests/gc" / filename, options &
                    " --gc:orc", cat)
      testSpec r, makeTest("tests/gc" / filename, options &
                    " --gc:orc -d:release", cat)

  template testWithoutBoehm(filename: untyped) =
    testWithoutMs filename
    testSpec r, makeTest("tests/gc" / filename, options &
                  " --gc:markAndSweep", cat)
    testSpec r, makeTest("tests/gc" / filename, options &
                  " -d:release --gc:markAndSweep", cat)

  template test(filename: untyped) =
    testWithoutBoehm filename
    when not defined(windows) and not defined(android):
      # AR: cannot find any boehm.dll on the net, right now, so disabled
      # for windows:
      testSpec r, makeTest("tests/gc" / filename, options &
                    " --gc:boehm", cat)
      testSpec r, makeTest("tests/gc" / filename, options &
                    " -d:release --gc:boehm", cat)

  testWithoutBoehm "foreign_thr"
  test "gcemscripten"
  test "growobjcrash"
  test "gcbench"
  test "gcleak"
  test "gcleak2"
  testWithoutBoehm "gctest"
  test "gcleak3"
  test "gcleak4"
  # Disabled because it works and takes too long to run:
  #test "gcleak5"
  testWithoutBoehm "weakrefs"
  test "cycleleak"
  testWithoutBoehm "closureleak"
  testWithoutMs "refarrayleak"

  testWithoutBoehm "tlists"
  testWithoutBoehm "thavlak"

  test "stackrefleak"
  test "cyclecollector"
  testWithoutBoehm "trace_globals"

# ------------------------- threading tests -----------------------------------

proc threadTests(r: var TResults, cat: Category, options: string) =
  template test(filename: untyped) =
    testSpec r, makeTest(filename, options, cat)
    testSpec r, makeTest(filename, options & " -d:release", cat)
    testSpec r, makeTest(filename, options & " --tlsEmulation:on", cat)
  for t in os.walkFiles("tests/threads/t*.nim"):
    test(t)

# ------------------------- debugger tests ------------------------------------

proc debuggerTests(r: var TResults, cat: Category, options: string) =
  if fileExists("tools/nimgrep.nim"):
    var t = makeTest("tools/nimgrep", options & " --debugger:on", cat)
    t.spec.action = actionCompile
    # force target to C because of MacOS 10.15 SDK headers bug
    # https://github.com/nim-lang/Nim/pull/15612#issuecomment-712471879
    t.spec.targets = {targetC}
    testSpec r, t

# ------------------------- JS tests ------------------------------------------

proc jsTests(r: var TResults, cat: Category, options: string) =
  template test(filename: untyped) =
    testSpec r, makeTest(filename, options, cat)
    testSpec r, makeTest(filename, options & " -d:release", cat)

  for t in os.walkFiles("tests/js/t*.nim"):
    test(t)

  for testfile in ["exception/texceptions", "exception/texcpt1",
                   "exception/texcsub", "exception/tfinally",
                   "exception/tfinally2", "exception/tfinally3",
                   "stdlib/types/tactiontable",
                   "misc/tints", "misc/tunsignedinc",
                   "js/tjsasync"]:
    test "tests/" & testfile & ".nim"

  for testfile in ["strutils", "json", "random", "times", "logging"]:
    test "lib/pure/" & testfile & ".nim"

# ------------------------- manyloc -------------------------------------------

proc findMainFile(dir: string): string =
  # finds the file belonging to ".nim.cfg"; if there is no such file
  # it returns the some ".nim" file if there is only one:
  const cfgExt = ".nim.cfg"
  result = ""
  var nimFiles = 0
  for kind, file in os.walkDir(dir):
    if kind == pcFile:
      if file.endsWith(cfgExt): return file[0..^(cfgExt.len+1)] & ".nim"
      elif file.endsWith(".nim"):
        if result.len == 0: result = file
        inc nimFiles
  if nimFiles != 1: result.setLen(0)

proc manyLoc(r: var TResults, cat: Category, options: string) =
  for kind, dir in os.walkDir("tests/manyloc"):
    if kind == pcDir:
      when defined(windows):
        if dir.endsWith"nake": continue
      if dir.endsWith"named_argument_bug": continue
      let mainfile = findMainFile(dir)
      if mainfile != "":
        var test = makeTest(mainfile, options, cat)
        test.spec.action = actionCompile
        testSpec r, test


const stdlibToExcludeFromJS = [
  "lib/pure/browsers.nim",
  "lib/pure/cgi.nim",
  "lib/pure/concurrency/cpuinfo.nim",
  "lib/pure/concurrency/threadpool.nim",
  "lib/pure/distros.nim",
  "lib/pure/dynlib.nim",
  "lib/pure/encodings.nim",
  "lib/pure/endians.nim",
  "lib/pure/httpclient.nim",
  "lib/pure/marshal.nim",
  "lib/pure/md5.nim",
  "lib/pure/memfiles.nim",
  "lib/pure/nativesockets.nim",
  "lib/pure/net.nim",
  "lib/pure/nimprof.nim",
  "lib/pure/oids.nim",
  "lib/pure/osproc.nim",
  "lib/pure/selectors.nim",
  "lib/pure/smtp.nim",
  "lib/pure/ssl_certs.nim",
  "lib/pure/terminal.nim",
] ## these can't run on the JS target

proc testStdlib(r: var TResults, pattern, options: string, cat: Category) =
  var files: seq[string]

  proc isValid(file: string): bool =
    for dir in parentDirs(file, inclusive = false):
      if dir.lastPathPart in ["includes", "nimcache"]:
        # e.g.: lib/pure/includes/osenv.nim gives: Error: This is an include file for os.nim!
        return false
    let name = extractFilename(file)
    if name.splitFile.ext != ".nim": return false
    for namei in disabledFiles:
      if namei.cmpPaths(name) == 0:
        return false
    return true

  for testFile in os.walkDirRec(pattern):
    if isValid(testFile):
      files.add testFile

  files.sort # reproducible order
  let targets = cat.defaultTargets
  for testFile in files:
    var testObj = makeTest(testFile, options, cat)
    testObj.spec.action = actionCompile
    
    let name = testFile.replace(DirSep, '/')
    testObj.spec.targets = targets
    if name in stdlibToExcludeFromJS:
      testObj.spec.targets.excl targetJS
    
    testSpec r, testObj

# ---------------- IC tests ---------------------------------------------

proc icTests(r: var TResults; testsDir: string, cat: Category, options: string) =
  const
    tooltests = ["compiler/nim.nim"]
    incrementalOn = " --incremental:on -d:nimIcIntegrityChecks "
  
  let targets = cat.defaultTargets()

  template editedTest(x: untyped) =
    var test = makeTest(file, x & options, cat)
    test.spec.targets = targets
    testSpecWithNimcache(r, test, nimcache)

  template checkTest() =
    var test = makeTestWithDummySpec(file, options, cat)
    test.spec.cmd = compilerPrefix & " check --hint:Conf:off --warnings:off --ic:on $options " & file
    testSpecWithNimcache(r, test, nimcache)

  for file in tooltests:
    for target in targets:
      let nimcache = nimcacheDir(file, options, target)
      removeDir(nimcache)

      let oldPassed = r.passed
      checkTest()

      if r.passed == oldPassed+1:
        checkTest()
        if r.passed == oldPassed+2:
          checkTest()

  const tempExt = "_temp.nim"
  for it in walkDirRec(testsDir):
  # for it in ["tests/ic/timports.nim"]: # debugging: to try a specific test
    if isTestFile(it) and not it.endsWith(tempExt):
      for target in targets:
        let nimcache = nimcacheDir(it, options, target)
        removeDir(nimcache)

        let content = readFile(it)
        for fragment in content.split("#!EDIT!#"):
          let file = it.replace(".nim", tempExt)
          writeFile(file, fragment)
          let oldPassed = r.passed
          editedTest(incrementalOn)
          if r.passed != oldPassed+1: break

# ----------------------------------------------------------------------------

# const AdditionalCategories = ["debugger", "lib", "ic"]
const AdditionalCategories = ["debugger", "lib"]
const MegaTestCat = "megatest"

proc `&.?`(a, b: string): string =
  # candidate for the stdlib?
  result = if b.startsWith(a): b else: a & b

proc processSingleTest(r: var TResults, cat: Category, options, test: string) =
  doAssert fileExists(test), test & " test does not exist"
  testSpec r, makeTest(test, options, cat)

proc isJoinableSpec(spec: TSpec, targets: set[TTarget]): bool =
  # xxx simplify implementation using an allow list of fields that are allowed
  # to be set to non-default values (use `fieldPairs`), to avoid issues like
  # bug #16576.
  result = useMegatest and not spec.sortoutput and
    spec.action == actionRun and
    not fileExists(spec.file.changeFileExt("cfg")) and
    not fileExists(spec.file.changeFileExt("nim.cfg")) and
    not fileExists(parentDir(spec.file) / "nim.cfg") and
    spec.cmd.len == 0 and
    spec.err notin {reDisabled, reKnownIssue} and
    not spec.unjoinable and
    spec.exitCode == 0 and
    spec.input.len == 0 and
    spec.nimout.len == 0 and
    spec.nimoutFull == false and
      # so that tests can have `nimoutFull: true` with `nimout.len == 0` with
      # the meaning that they expect empty output.
    spec.matrix.len == 0 and
    spec.outputCheck != ocSubstr and
    spec.ccodeCheck.len == 0 and
    (spec.targets * targets != {} or spec.targets == {})
  if result:
    if spec.file.readFile.contains "when isMainModule":
      result = false

proc quoted(a: string): string =
  # todo: consider moving to system.nim
  result.addQuoted(a)

proc runJoinedTest(r: var TResults, targets: set[TTarget], testsDir, options: string) =
  # xxx: this proc is batch aware by way of `specs.testamentData0`... needless
  #      to say the whole batching thing is just dumb. Test combining, aka
  #      megatest, and batching should all be handled once in `testament`.
  #      See: https://github.com/nim-works/nimskull/pull/135 for how to handle
  #      the batching part at least.
  var specs: seq[TSpec] = @[]
  for kind, dir in walkDir(testsDir):
    assert dir.startsWith(testsDir)
    let cat = dir[testsDir.len .. ^1]
    if kind == pcDir and cat notin specialCategories:
      for file in walkDirRec(testsDir / cat):
        if isTestFile(file):
          try:
            let spec = parseSpec(file, cat.Category.defaultTargets, nativeTarget())
            if isJoinableSpec(spec, targets):
              specs.add spec
          except ValueError:
            msg Undefined:
              "parseSpec raised ValueError for: '$1', assuming this will be handled outside of megatest" % file
            continue

  proc cmp(a: TSpec, b: TSpec): auto = cmp(a.file, b.file)
  sort(specs, cmp = cmp) # reproducible order
  msg Undefined: "joinable specs: " & $specs.len

  if simulate:
    var s = "runJoinedTest: "
    for a in specs:
      s.add a.file & " "
    msg Undefined: s
    return

  template toMarker(file, i): string =
    "megatest:processing: [$1] $2" % [$i, file]

  template backendErrorLogger(res: TResultEnum, errorOutput: string) =
    ## Helper to log megatest failure output to test results
    ##
    ## We expect the program to exit soon after
    if backendLogging:
      backend.writeTestResult(ReportParams(
        name: MegaTestCat,
        cat: MegaTestCat,
        targetStr: $target,
        action: actionRun,
        success: res,
        expected: "",
        given: errorOutput,
        knownIssues: @[]
      ))

      # Flush all buffers
      backend.close()

  var megatests: array[TTarget, string] = ["", "", ""] # c, js, vm

  let
    batch = testamentData0.testamentBatch
    outDir = nimcacheDir(testsDir / "megatest_" & $batch, "", targetC)
      ## a nimcacheDir abused to hold the sources of test files to import

  for i, runSpec in specs:
    let
      file = runSpec.file
      file2 = outDir / ("megatest_a_$1.nim" % $i)
      code = "echo $1\nstatic: echo \"CT:\", $1\n" % [toMarker(file, i).quoted]
    createDir(file2.parentDir)
    writeFile(file2, code)

    let
      testStr = "import $1\nimport $2 as megatest_b_$3\n" %
                    [file2.quoted, file.quoted, $i]
                # use `import`, as `include` didn't work with `trecmod2.nim`
      targetsToRun = runSpec.targets * targets

    if targetC in targetsToRun:
      megatests[targetC].add testStr

    if targetJs in targetsToRun:
      megatests[targetJs].add testStr

    if targetVM in targetsToRun:
      megatests[targetVM].add testStr

  const mtCacheDir = "megatest_$1_$2"
  let
    root = getCurrentDir()
    cacheDirs: array[TTarget, string] = [
      nimcacheDir(testsDir / mtCacheDir % [$batch, $targetC], "", targetC),
      nimcacheDir(testsDir / mtCacheDir % [$batch, $targetJS], "", targetJS),
      nimcacheDir(testsDir / mtCacheDir % [$batch, $targetVM], "", targetVM)
    ]

  for target, megatest in megatests.pairs:
    if megatest == "":
      continue

    # so it uses testsDir / "config.nims"
    let megatestFile = testsDir / "megatest_$1_$2.nim" %
                        [$testamentData0.testamentBatch, $target]
    writeFile(megatestFile, megatest)

    var args = @[$target, "--nimCache:" & cacheDirs[target], "-d:testing", 
                 "-d:nimMegatest", "--listCmd", "--listFullPaths:on",
                 "--path:" & root]
    if target == targetJS:
      args.add "-d:nodejs"
    args.add options.parseCmdLine
    args.add megatestFile

    var (cmdLine, buf, exitCode) = execCmdEx2(command = compilerPrefix,
                                              args = args,
                                              input = "")
    if exitCode != 0:
      backendErrorLogger(reNimcCrash, buf)
      msg Undefined: "$ " & cmdLine & "\n" & buf
      quit(failString & "megatest compilation failed")

    case target
    of targetC:
      (buf, exitCode) =
        execCmdEx(megatestFile.changeFileExt(ExeExt).dup normalizeExe)
    of targetJS:
      let nodejs = findNodeJS()
      if nodejs == "":
        quit("nodejs binary not in PATH")
      let execCmd = "$1 $2 $3" % [nodeJs,
                                  "--unhandled-rejections=strict",
                                  megatestFile.changeFileExt("js").dup]

      (buf, exitCode) = execCmdEx(execCmd)
    of targetVM:
      let
        vm = changeFileExt(compilerPrefix.getFileDir() / "vmrunner", ExeExt)
        execCmd = "$1 $2" % [vm, megatestFile.changeFileExt("nimbc").dup]
      (buf, exitCode) = execCmdEx(execCmd)

    if exitCode != 0:
      backendErrorLogger(reOutputsDiffer, buf)
      msg Undefined: buf
      quit(failString & "megatest execution failed")

    let
      outputExceptedFile = "outputExpected_$1_$2.txt" % [$batch, $target]
      outputGottenFile = "outputGotten_$1_$2.txt" % [$batch, $target]
    writeFile(outputGottenFile, buf)
    
    var outputExpected = ""
    for i, runSpec in specs:
      if target notin runSpec.targets:
        continue

      outputExpected.add toMarker(runSpec.file, i) & "\n"
      if runSpec.output.len > 0:
        outputExpected.add runSpec.output
        if not runSpec.output.endsWith "\n":
          outputExpected.add '\n'

    if buf != outputExpected:
      writeFile(outputExceptedFile, outputExpected)
      let diff = diffFiles(outputGottenFile, outputExceptedFile).output
      backendErrorLogger(reOutputsDiffer, diff)
      msg Undefined: diff
      msg Undefined: "$1megatest output different, see $2 vs $3" %
                      [failString, outputGottenFile, outputExceptedFile]
      # outputGottenFile, outputExceptedFile not removed on purpose for debugging.
      quit 1
    else:
      msg Undefined: "megatest $1 output OK" % $target


# ---------------------------------------------------------------------------

proc processCategory(r: var TResults, cat: Category, targets: set[TTarget],
                     options, testsDir: string,
                     runJoinableTests: bool) =
  let cat2 = cat.string.split("/")[0].normalize
  case cat2
  of "js":
    # only run the JS tests on Windows or Linux because some CI and other OSes
    # like Haiku might hijack/lack nodejs:
    if not defined(linux) and not defined(windows):
      discard
    else:
      jsTests(r, cat, options)
  of "dll":
    dllTests(r, cat, options)
  of "gc":
    gcTests(r, cat, options)
  of "debugger":
    debuggerTests(r, cat, options)
  of "manyloc":
    manyLoc r, cat, options
  of "threads":
    threadTests r, cat, options & " --threads:on"
  of "lib":
    testStdlib(r, "lib/pure/", options, cat)
  of "ic":
    icTests(r, testsDir / cat2, cat, options)
  of "untestable":
    # These require special treatment e.g. because they depend on a third party
    # dependency; see `trunner_special` which runs some of those.
    discard
  of "megatest":
    runJoinedTest(r, targets, testsDir, options)
  else:
    var testsRun = 0
    var files: seq[string]
    for file in walkDirRec(testsDir &.? cat.string):
      if isTestFile(file): files.add file
    files.sort # give reproducible order
    for i, name in files:
      var test = makeTest(name, options, cat)
      if runJoinableTests or
          not isJoinableSpec(test.spec, targets) or
          cat.string in specialCategories:
        discard "run the test"
      else:
        test.spec.err = reJoined
      testSpec r, test
      inc testsRun
    if testsRun == 0:
      const allowedDirs = ["deps", "htmldocs", "pkgs"]
        # `pkgs` because bug #16556 creates `pkgs` dirs and this can affect some users
        # that try an old version of choosenim.
      doAssert cat.string in allowedDirs,
        "Invalid category specified: '$#' not in allow list: $#" % [cat.string, $allowedDirs]
