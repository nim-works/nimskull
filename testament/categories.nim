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


proc initTest(
    name: string,
    options: seq[ShellArg],
    cat: Category,
    spec: SpecId
  ): GivenTest =
  ## make a test with the given spec, meant to be used internally as a
  ## constructor mostly
  result.cat = cat
  result.name = name
  result.options = options
  result.spec = spec

proc makeTest(
    test: string, options: seq[ShellArg], cat: CategoryId): GivenTest =
  ## make a test from inferring the test's filename from `test` and parsing the
  ## spec within that file.
  initTest(test, options, cat,
           parseSpec(
             addFileExt(test, ".nim"),
             cat.defaultTargets,
             nativeTarget()))

proc makeTestWithDummySpec(test, options: string, cat: Category): TTest =
  var spec = initSpec(addFileExt(test, ".nim"))
  spec.action = actionCompile
  spec.targets = cat.defaultTargets()

  initTest(test, options, cat, spec)



# --------------------- DLL generation tests ----------------------------------

proc runBasicDLLTest(
    c, r: var TResults, cat: Category, options: string, execution: Execution) =
  const rpath = when defined(macosx):
      " --passL:-rpath --passL:@loader_path"
    else:
      ""

  var test1 = makeTest("lib/nimrtl.nim", options & " --outdir:tests/dll", cat)
  test1.spec.action = actionCompile
  testSpec c, test1, execution
  var test2 = makeTest("tests/dll/server.nim", options & " --threads:on" & rpath, cat)
  test2.spec.action = actionCompile
  testSpec c, test2, execution
  var test4 = makeTest("tests/dll/visibility.nim", options & " --app:lib" & rpath, cat)
  test4.spec.action = actionCompile
  testSpec c, test4, execution

  # windows looks in the dir of the exe (yay!):
  when not defined(windows):
    # posix relies on crappy LD_LIBRARY_PATH (ugh!):
    const libpathenv = when defined(haiku): "LIBRARY_PATH"
                       else: "LD_LIBRARY_PATH"
    var libpath = getEnv(libpathenv)
    # Temporarily add the lib directory to LD_LIBRARY_PATH:
    putEnv(libpathenv, "tests/dll" & (if libpath.len > 0: ":" & libpath else: ""))
    defer: putEnv(libpathenv, libpath)

  testSpec(
    r, makeTest("tests/dll/client.nim", options & " --threads:on" & rpath, cat), execution)
  testSpec(
    r, makeTest("tests/dll/visibility.nim", options & rpath, cat), execution)

  if "boehm" notin options:
    # force build required - see the comments in the .nim file for more details
    for target in cat.defaultTargets():
      var hcri = makeTest("tests/dll/nimhcr_integration.nim",
                                    options & " --forceBuild --hotCodeReloading:on" & rpath, cat)
      let nimcache = nimcacheDir(hcri.name, hcri.options, target)
      let cmd = prepareTestCompileCmd(hcri.spec.getCmd, hcri.name,
                                  hcri.options, nimcache, target)
      hcri.testArgs = cmd.parseCmdLine
      testSpec r, hcri, execution

proc dllTests(r: var TResults, cat: Category, options: string, execution: Execution) =
  # dummy compile result:
  var c = initResults()

  runBasicDLLTest c, r, cat, options, execution
  runBasicDLLTest c, r, cat, options & " -d:release", execution
  when not defined(windows):
    # still cannot find a recent Windows version of boehm.dll:
    runBasicDLLTest c, r, cat, options & " --gc:boehm", execution
    runBasicDLLTest c, r, cat, options & " -d:release --gc:boehm", execution

# ------------------------------ GC tests -------------------------------------

proc gcTests(r: var TResults, cat: Category, options: string, execution: Execution) =
  template testWithoutMs(filename: untyped) =
    testSpec r, makeTest("tests/gc" / filename, options, cat), execution
    testSpec r, makeTest("tests/gc" / filename, options &
                  " -d:release -d:useRealtimeGC", cat), execution
    when filename != "gctest":
      testSpec r, makeTest("tests/gc" / filename, options &
                    " --gc:orc", cat), execution
      testSpec r, makeTest("tests/gc" / filename, options &
                    " --gc:orc -d:release", cat), execution

  template testWithoutBoehm(filename: untyped) =
    testWithoutMs filename
    testSpec r, makeTest("tests/gc" / filename, options &
                  " --gc:markAndSweep", cat), execution
    testSpec r, makeTest("tests/gc" / filename, options &
                  " -d:release --gc:markAndSweep", cat), execution

  template test(filename: untyped) =
    testWithoutBoehm filename
    when not defined(windows) and not defined(android):
      # AR: cannot find any boehm.dll on the net, right now, so disabled
      # for windows:
      testSpec r, makeTest("tests/gc" / filename, options &
                    " --gc:boehm", cat), execution
      testSpec r, makeTest("tests/gc" / filename, options &
                    " -d:release --gc:boehm", cat), execution

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


type
  GcTestKinds = enum
    gcOther
    gcMarkSweep
    gcBoehm

proc setupGcTests(execState: var Execution, catId: CategoryId) =
  ## setup tests for the gc category, requires special handling due to
  ## testament limitations.

  const
    withoutMs = {gcOther}
    withoutBoehm = {gcOther, gcMarkSweep}
    noConditions = {gcOther, gcMarkSweep, gcBoehm}

  let testData = [
    ("foreign_thr.nim", withoutBoehm),
    ("gcemscripten.nim", noConditions),
    ("growobjcrash.nim", noConditions),
    ("gcbench.nim", noConditions),
    ("gcleak.nim", noConditions),
    ("gcleak2.nim", noConditions),
    ("gctest.nim", withoutBoehm),
    ("gcleak3.nim", noConditions),
    ("gcleak4.nim", noConditions),
    ("weakrefs.nim", withoutBoehm),
    ("cycleleak.nim", noConditions),
    ("closureleak.nim", withoutBoehm),
    ("refarrayleak.nim", withoutMs),
    ("tlists.nim", withoutBoehm),
    ("thavlak.nim", withoutBoehm),
    ("stackrefleak.nim", noConditions),
    ("cyclecollector.nim", noConditions),
    ("trace_globals.nim", withoutBoehm)
  ]

  for (testFile, gcConditions) in testData:
    let testId: TestId = execState.testFiles.len
    execState.testFiles.add TestFile(file: "tests/gc" / testFile, catId: catId)
    execState.testOpts[testId] = TestOptionData()

    if gcMarkSweep in gcConditions:
      execState.testOpts[testId].optMatrix.add "" # run the test as is
      execState.testOpts[testId].optMatrix.add " -d:release --gc:useRealtimeGC"
      if testFile != "gctest":
        execState.testOpts[testId].optMatrix.add " --gc:orc"
        execState.testOpts[testId].optMatrix.add " -d:release --gc:orc"

    if gcMarkSweep in gcConditions:
      execState.testOpts[testId].optMatrix.add " --gc:markAndSweep"
      execState.testOpts[testId].optMatrix.add " -d:release --gc:markAndSweep"

    if gcBoehm in gcConditions:
      when not defined(windows) and not defined(android):
        # cannot find any boehm.dll on the net, right now, so disabled for
        # windows:
        execState.testOpts[testId].optMatrix.add " --gc:boehm"
        execState.testOpts[testId].optMatrix.add " -d:release --gc:boehm"

# ------------------------- threading tests -----------------------------------

proc threadTests(r: var TResults, cat: Category, options: string, execution: Execution) =
  template test(filename: untyped) =
    testSpec r, makeTest(filename, options, cat), execution
    testSpec r, makeTest(filename, options & " -d:release", cat), execution
    testSpec r, makeTest(filename, options & " --tlsEmulation:on", cat), execution
  for t in os.walkFiles("tests/threads/t*.nim"):
    test(t)

proc setupThreadTests(execState: var Execution, catId: CategoryId) =
  for t in os.walkFiles("tests/threads/t*.nim"):
    let testId: TestId = execState.testFiles.len
    execState.testFiles.add TestFile(file: t, catId: catId)
    execState.testOpts[testId] = TestOptionData(
      optMatrix: @["", "-d:release", "--tlsEmulation:on"])

# ------------------------- debugger tests ------------------------------------

proc debuggerTests(r: var TResults, cat: Category, options: string, execution: Execution) =
  if fileExists("tools/nimgrep.nim"):
    var t = makeTest("tools/nimgrep", options & " --debugger:on", cat)
    t.spec.action = actionCompile
    # force target to C because of MacOS 10.15 SDK headers bug
    # https://github.com/nim-lang/Nim/pull/15612#issuecomment-712471879
    t.spec.targets = {targetC}
    testSpec r, t, execution

# ------------------------- JS tests ------------------------------------------

proc jsTests(r: var TResults, cat: Category, options: string, execution: Execution) =
  template test(filename: untyped) =
    testSpec r, makeTest(filename, options, cat), execution
    testSpec r, makeTest(filename, options & " -d:release", cat), execution

  for t in os.walkFiles("tests/js/t*.nim"):
    test(t)
  for testfile in ["exception/texceptions", "exception/texcpt1",
                   "exception/texcsub", "exception/tfinally",
                   "exception/tfinally2", "exception/tfinally3",
                   "actiontable/tactiontable", "method/tmultimjs",
                   "varres/tvarres0", "varres/tvarres3", "varres/tvarres4",
                   "varres/tvartup", "misc/tints", "misc/tunsignedinc",
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

proc manyLoc(r: var TResults, cat: Category, options: string, execution: Execution) =
  for kind, dir in os.walkDir("tests/manyloc"):
    if kind == pcDir:
      when defined(windows):
        if dir.endsWith"nake": continue
      if dir.endsWith"named_argument_bug": continue
      let mainfile = findMainFile(dir)
      if mainfile != "":
        var test = makeTest(mainfile, options, cat)
        test.spec.action = actionCompile
        testSpec r, test, execution


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

proc testStdlib(
    r: var TResults, pattern, options: string, cat: Category, execution: Execution) =
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
    
    testSpec r, testObj, execution

proc setupStdlibTests(execState: var Execution, catId: CategoryId) =
  proc isValid(file: string): bool =
    for dir in parentDirs(file, inclusive = false):
      if dir.lastPathPart in ["includes", "nimcache"]:
        # e.g.: lib/pure/includes/osenv.nim gives: Error: This is an include file for os.nim!
        return false
    let name = extractFilename(file)
    if name.splitFile.ext != ".nim": return false
    for namei in disabledFiles:
      # because of `LockFreeHash.nim` which has case
      if namei.cmpPaths(name) == 0: return false
    return true

  for testFile in os.walkDirRec("lib/pure/"):
    if isValid(testFile):
      let testId: TestId = execState.testFiles.len
      execState.testFiles.add TestFile(file: testFile, catId: catId)
      execState.testOpts[testId] = TestOptionData(action: some(actionCompile))

# ---------------- IC tests ---------------------------------------------

proc icTests(
    r: var TResults,
    testsDir: string,
    cat: Category,
    options: string,
    isNavigatorTest: bool,
    execution: Execution
  ) =

  const
    tooltests = ["compiler/nim.nim"]
    incrementalOn = " --incremental:on -d:nimIcIntegrityChecks "
    navTestConfig = " --ic:on -d:nimIcNavigatorTests --hint:Conf:off --warnings:off "
  
  let targets =
    if isNavigatorTest:
      {nativeTarget()}
    else:
      cat.defaultTargets()

  template editedTest(x: untyped) =
    var test = makeTest(file, x & options, cat)
    if isNavigatorTest:
      test.spec.action = actionCompile
    test.spec.targets = targets
    testSpecWithNimcache(r, test, nimcache, execution)

  template checkTest() =
    var test = makeTestWithDummySpec(file, options, cat)
    test.spec.cmd = compilerPrefix & " check --hint:Conf:off --warnings:off --ic:on $options " & file
    testSpecWithNimcache(r, test, nimcache, execution)

  if not isNavigatorTest:
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
          editedTest(if isNavigatorTest: navTestConfig else: incrementalOn)
          if r.passed != oldPassed+1: break

# ----------------------------------------------------------------------------

# const AdditionalCategories = ["debugger", "lib", "ic", "navigator"]
const AdditionalCategories = ["debugger", "lib"]
const MegaTestCat = "megatest"

proc `&.?`(a, b: string): string =
  # candidate for the stdlib?
  result = if b.startsWith(a): b else: a & b

proc processSingleTest(
    r: var TResults, cat: Category, options, test: string, execution: Execution) =
  doAssert fileExists(test), test & " test does not exist"
  testSpec r, makeTest(test, options, cat), execution

proc isJoinableSpec(spec: TSpec): bool =
  # xxx simplify implementation using an allow list of fields that are allowed
  # to be set to non-default values (use `fieldPairs`), to avoid issues like
  # bug #16576.
  result = useMegatest and not spec.sortoutput and
    spec.action == actionRun and
    not fileExists(spec.file.changeFileExt("cfg")) and
    not fileExists(spec.file.changeFileExt("nims")) and
    not fileExists(parentDir(spec.file) / "nim.cfg") and
    not fileExists(parentDir(spec.file) / "config.nims") and
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
    (spec.targets == {} or spec.targets == {targetC})
  if result:
    if spec.file.readFile.contains "when isMainModule":
      result = false

proc quoted(a: string): string =
  # todo: consider moving to system.nim
  result.addQuoted(a)

proc runJoinedTest(r: var TResults, cat: Category, testsDir: string, options: string) =
  ## returns a list of tests that have problems
  #[
  xxx create a reusable megatest API after abstracting out testament specific code,
  refs https://github.com/timotheecour/Nim/issues/655
  and https://github.com/nim-lang/gtk2/pull/28; it's useful in other contexts.
  ]#
  var specs: seq[TSpec] = @[]
  for kind, dir in walkDir(testsDir):
    assert dir.startsWith(testsDir)
    let cat = dir[testsDir.len .. ^1]
    if kind == pcDir and cat notin specialCategories:
      for file in walkDirRec(testsDir / cat):
        if isTestFile(file):
          var spec: TSpec
          try:
            spec = parseSpec(file, cat.Category.defaultTargets, nativeTarget())
          except ValueError:
            # e.g. for `tests/navigator/tincludefile.nim` which have multiple
            # specs; this will be handled elsewhere
            msg Undefined: "parseSpec raised ValueError for: '$1', assuming this will be handled outside of megatest" % file
            continue
          if isJoinableSpec(spec):
            specs.add spec

  proc cmp(a: TSpec, b: TSpec): auto = cmp(a.file, b.file)
  sort(specs, cmp = cmp) # reproducible order
  msg Undefined: "joinable specs: " & $specs.len

  if simulate:
    var s = "runJoinedTest: "
    for a in specs: s.add a.file & " "
    msg Undefined: s
    return

  var megatest: string
  # xxx (minor) put outputExceptedFile, outputGottenFile, megatestFile under here or `buildDir`
  var outDir = nimcacheDir(testsDir / "megatest", "", targetC)
  template toMarker(file, i): string =
    "megatest:processing: [$1] $2" % [$i, file]
  for i, runSpec in specs:
    let file = runSpec.file
    let file2 = outDir / ("megatest_a_$1.nim" % $i)
    # `include` didn't work with `trecmod2.nim`, so using `import`
    let code = "echo $1\nstatic: echo \"CT:\", $1\n" % [toMarker(file, i).quoted]
    createDir(file2.parentDir)
    writeFile(file2, code)
    megatest.add "import $1\nimport $2 as megatest_b_$3\n" % [file2.quoted, file.quoted, $i]

  let megatestFile = testsDir / "megatest.nim" # so it uses testsDir / "config.nims"
  writeFile(megatestFile, megatest)

  template backendErrorLogger(res: TResultEnum, errorOutput: string) =
    ## Helper to log megatest failure output to test results
    ##
    ## We expect the program to exit soon after
    if backendLogging:
      backend.writeTestResult(ReportParams(
        name: MegaTestCat,
        cat: MegaTestCat,
        targetStr: "c",
        action: actionRun,
        success: res,
        expected: "",
        given: errorOutput,
        knownIssues: @[]
      ))

      # Flush all buffers
      backend.close()

  let root = getCurrentDir()

  var args = @["c", "--nimCache:" & outDir, "-d:testing", "-d:nimMegatest", "--listCmd",
               "--listFullPaths:on", "--path:" & root]
  args.add options.parseCmdLine
  args.add megatestFile
  var (cmdLine, buf, exitCode) = execCmdEx2(command = compilerPrefix, args = args, input = "")
  if exitCode != 0:
    backendErrorLogger(reNimcCrash, buf)
    msg Undefined: "$ " & cmdLine & "\n" & buf
    quit(failString & "megatest compilation failed")

  (buf, exitCode) = execCmdEx(megatestFile.changeFileExt(ExeExt).dup normalizeExe)
  if exitCode != 0:
    backendErrorLogger(reOutputsDiffer, buf)
    msg Undefined: buf
    quit(failString & "megatest execution failed")

  const outputExceptedFile = "outputExpected.txt"
  const outputGottenFile = "outputGotten.txt"
  writeFile(outputGottenFile, buf)
  var outputExpected = ""
  for i, runSpec in specs:
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
    msg Undefined: failString & "megatest output different, see $1 vs $2" % [outputGottenFile, outputExceptedFile]
    # outputGottenFile, outputExceptedFile not removed on purpose for debugging.
    quit 1
  else:
    msg Undefined: "megatest output OK"


# ---------------------------------------------------------------------------

proc processCategory(r: var TResults, cat: Category,
                     options, testsDir: string,
                     runJoinableTests: bool, execution: Execution) =
  let cat2 = cat.string.split("/")[0].normalize
  case cat2
  of "js":
    # only run the JS tests on Windows or Linux because some CI and other OSes
    # like Haiku might hijack/lack nodejs:
    if not defined(linux) or not defined(windows):
      discard
    else:
      jsTests(r, cat, options, execution)
  of "dll":
    dllTests(r, cat, options, execution)
  of "gc":
    gcTests(r, cat, options, execution)
  of "debugger":
    debuggerTests(r, cat, options, execution)
  of "manyloc":
    manyLoc(r, cat, options, execution)
  of "threads":
    threadTests(r, cat, options & " --threads:on", execution)
  of "lib":
    testStdlib(r, "lib/pure/", options, cat, execution)
  of "ic":
    icTests(r, testsDir / cat2, cat, options, isNavigatorTest=false, execution)
  of "navigator":
    icTests(r, testsDir / cat2, cat, options, isNavigatorTest=true, execution)
  of "untestable":
    # These require special treatment e.g. because they depend on a third party
    # dependency; see `trunner_special` which runs some of those.
    discard
  of "megatest":
    runJoinedTest(r, cat, testsDir, options)
  else:
    var testsRun = 0
    var files: seq[string]
    for file in walkDirRec(testsDir &.? cat.string):
      if isTestFile(file): files.add file
    files.sort # give reproducible order
    for i, name in files:
      var test = makeTest(name, options, cat)
      if runJoinableTests or not isJoinableSpec(test.spec) or cat.string in specialCategories:
        discard "run the test"
      else:
        test.spec.err = reJoined
      testSpec(r, test, execution)
      inc testsRun
    if testsRun == 0:
      const allowedDirs = ["deps", "htmldocs", "pkgs"]
        # `pkgs` because bug #16556 creates `pkgs` dirs and this can affect some users
        # that try an old version of choosenim.
      doAssert cat.string in allowedDirs,
        "Invalid category specified: '$#' not in allow list: $#" % [cat.string, $allowedDirs]
