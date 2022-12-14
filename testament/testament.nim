import
  std/[
    options,
    tables,
    strutils,
    os,
    md5,
    pegs,
    parseopt,
    algorithm,
    packedsets
  ],
  experimental/[
    dod_helpers,
    shellrunner
  ]

import specs, test_diff
import compiler/utils/nodejs


declareIdType(Test)
declareIdType(Run)
declareIdType(Action)
declareIdType(Category)

type
  Category = distinct string ## Name of the test category
  EntryId = int ## Index of the selected matrix entry

  TResults = object
    # DOC WTF why is this even necessary?
    total, passed, knownIssuesSucceded, skipped: int
      ## TODO rename passed to passedOrAllowedFailure
    data: string

  GivenTest = object
    # name: string
    cat: Category
    options: seq[ShellArg]
    testArgs: seq[string]
    spec: TestId
    file: TestId

  TestRun = object
    ## a test run: reject, compile, or compile + run along with time to
    ## check; runs for a test must be contiguous and ordered
    test: TestId
    matrixEntry: EntryId
    target: GivenTarget
    nimcache: string # TODO CLEAN use absolute directory instead
    startTime: float
    debugInfo: string

  RetryInfo = object
    test: TestId       ## which test failed
    target: GivenTarget ## the specific target

  RetryList = OrderedTable[TestId, RetryInfo]
      ## record failures in here so the user can choose to retry them

  TestFile = object
    file: string
    catId: CategoryId

  TestAction = object
    runId: RunId
    case kind: GivenTestAction      # NOTE: might not need `partOfRun` at all
    of actionReject:
      discard
    of actionRun:
      compileActionId: ActionId ## id of the preceeding compile action
    of actionCompile:
      partOfRun: bool

  RunTime = object
    ## time tracking for test run activities
    compileStart: float      ## when the compile process start
    compileEnd: float        ## when the compile process ends
    compileCheckStart: float ## when compile output check started
    compileCheckEnd: float   ## when compile output check finished
    runStart: float          ## for run, start of execution
    runEnd: float            ## for run, end of execution
    runCheckStart: float     ## start of run output check
    runCheckEnd: float       ## end of run output check


  CompileOutCompare* = object
    ## Result of the compiler diagnostics comparison
    compileSuccess: bool ## Whether compilation succeded in the first
    ## place, irrespective of the subsequent spec checks.
    runResult: TestedResultKind
    codegen: CodegenCheckCompare

  CodegenCheckCompare* = object
    ## Result of the codegen check comparison
    codeSizeOverflow: Option[tuple[generated, allowed: int]]
    missingPegs: seq[string]
    missingCode: Option[string]

  RunResult = object
    ## Final result of the test run
    nimout: string           ## nimout from compile, empty if not required
    nimExit: int             ## exit code produced by the compiler
    nimMsg: string           ## last message, if any, from the compiler
    nimFile: string          ## filename from last compiler message, if present
    nimLine: int             ## line from last compiler message, if present
    nimColumn: int           ## colunn from last compiler message, if present
    prgOut: string           ## program output, if any
    prgOutSorted: string     ## sorted version of `prgOut`; kept for legacy
                             ## reason, remove when no longer necessary
    prgExit: int             ## program exit, if any
    lastAction: ActionId     ## last action in this run
    compileCmp: CompileOutCompare
    runCmp: RunCompare


  RunCompare = object

  TestOptionData = object
    ## Additional configuration for the test execution
    optMatrix: seq[seq[ShellArg]]   ## matrix of cli options for this test
    action: Option[GivenTestAction] ## possible action override


  DebugInfo = OrderedTable[RunId, string]
  # REFACTOR - debug info should be per action instead of per run

  TestOptions = OrderedTable[TestId, TestOptionData]
    ## for legacy reasons (eg: `dllTests`) we need to be able to set per test
    ## options, ideally this would be done with `matrix`, but it's not
    ## sophisticated enough to support spare configuration like this needs


# Files and tests are indexed using the same key
declareStoreType(TestFile, Files, TestId)
declareStoreType(GivenTest, Tests, TestId)
declareStoreType(DeclaredSpec, Specs, TestId)

declareStoreType(TestAction, Actions, ActionId)

# Run configurations, their direct output results, comparisons and times
# are all indexed usin the same `RunId` key type.
declareStoreType(TestRun, TestRuns, RunId)
declareStoreType(RunResult, Results, RunId)
declareStoreType(RunCompare, Compares, RunId)
declareStoreType(RunTime, Times, RunId)

declareStoreType(Category, Categories, CategoryId)


type
  TestamentConf = object
    useColors: bool # = true
    backendLogging: bool # = true
    simulate: bool # = false
    optVerbose: bool # = false
    useMegatest: bool # = true
    optFailing: bool # = false
    batchArg: string
    testamentNumBatch: int
    testamentBatch: int
    compilerPrefix: string
    skips: seq[string]


  Execution = object
    ## state object to data relevant for a testament run
    conf: TestamentConf
    gTargets: set[GivenTarget]
    retryContainer: RetryContainer
    skips: seq[string]

    userTestOptions: string ## options passed to tests by the user
    flags: ExecutionFlags   ## various options set by the user
    skipsFile: string       ## test files to skip loaded from `--skipFrom`
    targetsStr: string      ## targets as specified by the user
    filter: TestFilter      ## fitler used to assemble the test suite
    targets: set[GivenTarget]    ## specified targets or `noTargetsSpecified`

    workingDir: string       ## working directory to begin execution in
    nodeJs: string           ## path to nodejs binary
    nimSpecified: bool       ## whether the user specified the nim
    testArgs: string         ## arguments passed to tests by the user
    # environment input / setup
    compilerPath: string     ## compiler command to use
    testsDir: string         ## where to look for tests
    rootDir: string          ## Absolute path to root directory for `testsDir`

    # test discovery data
    categories: Categories ## categories discovered for this execution
    ## first one is a default empty category `""`
    files: Files ## files for this execution
    specs: Specs ## spec for each file
    testOpts: TestOptions ## per test options, because legacy category magic

    # test execution data
    tests: Tests     ## Original collection of tests
    runs: TestRuns   ## All test runs that need to be executed
    times: Times     ## run timing information for each test run
    results: Results ## actual information for a given run
    actions: Actions ## test actions for each run, phases of a run; actions
                     ## for a run must be continugous and ordered

    debugInfo: DebugInfo     ## debug info related to runs for tests, should be
                             ## per action instead of per run.
    runProgress: RunProgress ## current run progress based on action progress


    # test execution related data
    retryList: RetryList     ## list of failures to potentially retry later

  RunProgress = object
    ## Acts as a tracker for a producer/consumer model, where `lastCheckedRun`
    ## is where the consumer left off, and `mostRecentRun` is where the
    ## producer left off.
    lastCheckedRun: RunId ## run last reviewed for reporting/consumption
    mostRecentRun: RunId  ## run that completed most recently/production

  ExecutionFlag = enum
    outputColour       ## colour the output
    outputResults      ## print results to the console
    outputFailureOnly  ## only output failures
    outputVerbose      ## increase output verbosity
    logBackend         ## enable backend logging
    dryRun             ## do not run the tests, only indicate which would run
    rerunFailed        ## only run tests failed in the previous run
    runKnownIssues     ## also execute tests marked as known issues

  ExecutionFlags = set[ExecutionFlag]

  TestFilterKind {.pure.} = enum
    tfkAll = "all"        ## all tests
    tfkHtml = "html"      ## generate html, yes not really a 'filter'
    tfkCache = "cache"    ## Create execution cache, not a filter
    tfkCategories = "cat" ## one or more categories
    tfkPCats = "pcat"     ## legacy support for parallel category
    tfkGlob = "glob"
    tfkSingle = "r"       ## single test

  GlobPattern* = string

  TestFilter = object
    case kind: TestFilterKind
    of tfkAll, tfkHtml, tfkCache:
      discard

    of tfkCategories, tfkPCats:
      # FIXME: currently multiple categories are unsupported
      cats: Categories
    of tfkGlob:
      pattern: GlobPattern

    of tfkSingle:
      test: string

  ParseCliResult = enum
    parseSuccess       ## successfully parsed cli params
    parseQuitWithUsage ## parsing failed, quit with usage message


func `$`*(cat: Category): string = "Category($#)" % $cat.string
func `<`(a, b: TestFile): bool = a.file < b.file
func cmp(a, b: TestFile): int = cmp(a.file, b.file)


const noMatrixEntry = -1

func getRunContext(e: Execution, id: RunId): tuple[
    run: TestRun, res: RunResult, test: GivenTest, spec: DeclaredSpec] =
  result.run = e.runs[id]
  result.res = e.results[id]
  result.test = e.tests[result.run.test]
  result.spec = e.specs[result.test.spec]


proc compileOutCheck(e: Execution, id: RunId): CompileOutCompare =
  ## Compare all test output messages. This proc does structured or
  ## unstructured comparison comparison and immediately reports it's
  ## results.
  ##
  ## It is used to for performing 'reject' action checks - it compares
  ## both inline and regular messages - in addition to `nimoutCheck`
  # REFACTOR remove results, turn into data producer procedure

  # If structural comparison is requested - drop directly to it and handle
  # the success/failure modes in the branch
  let (run, res, test, spec) = e.getRunContext(id)

  if spec.nimoutSexp or 0 < spec.inlineErrors.len():
    let data = CompileOutputCheck(
      enforceFullMatch: spec.nimoutFull,
      inlineErrors: spec.inlineErrors,
      # testName: test.name,
      givenNimout: res.nimout,
      expectedFile: e.files[test.file].file,
      expectedNimout: spec.nimout,
    )

    if spec.nimoutSexp:
      let outCompare = sexpCheck(data)
      # Full match of the output results.
      if outCompare.match:
        result.runResult = reSuccess
      else:
        # Write out error message.
        result.runResult = reMsgsDiffer
        # result.nimout = given.msg
        # TODO figure out how these argumens are mapped to the real report
        # content.
        #
        # r.addResult(
        #   run, run.expected.msg, given.msg,
        #   ,
        #   givenSpec = unsafeAddr given,
        #   outCompare = outCompare
        # )

    # Checking for inline errors.
    else:
      # QUESTION - `checkForInlineErrors` does not perform any comparisons
      # for the regular message spec, it just compares annotated messages.
      # How can it report anything properly then?
      #
      # MAYBE it is related the fact testament misuses the `inlineErrors`,
      # and wrongly assumes they are /only/ errors, despite actually parsing
      # anything that starts with `#[tt.` as inline annotation? Even in this
      # case this does not make any sense, because comparisons is done only
      # for inline error messages.
      #
      # MAYBE this is just a way to mitigate the more complex problem of
      # mixing in inline error messages and regular `.nimout`? I 'solved' it
      # using `stablematch` and weighted ordering, so most likely the person
      # who wrote this solved the same problem using "I don't care" approach.
      #
      # https://github.com/nim-lang/Nim/commit/9a110047cbe2826b1d4afe63e3a1f5a08422b73f#diff-a161d4667e86146f2f8003f08f765b8d9580ae92ec5fb6679c80c07a5310a551R362-R364
      let outCompare = checkForInlineErrors(data)
      # TODO generate run report

  # Check for `.errormsg` in expected and given spec first
  elif strip(spec.msg) notin strip(res.nimout):
    discard
    # IMPLEMENT create run output message
    # r.addResult(run, run.expected.msg, given.msg, reMsgsDiffer)

  # Compare expected and resulted spec messages
  elif not compileOutputCheck(
    full = spec.nimoutFull,
    expected = spec.nimout,
    given = res.nimout
  ):
    # Report general message mismatch error
    # TODO IMPLEMENT
    discard
    # r.addResult(run, run.expected.nimout, given.nimout, reMsgsDiffer)

  # Check for filename mismatches
  elif extractFilename(
    e.files[test.file].file
  ) != extractFilename(
    res.nimFile
  ) and "internal error:" notin spec.msg:
    # Report error for the the error file mismatch
    # TODO IMPLEMENT
    discard
    # r.addResult(run, run.expected.file, given.file, reFilesDiffer)

  # Check for produced and given error message locations
  elif spec.line != res.nimLine and spec.line != 0 or
       spec.column != res.nimColumn and spec.column != 0:
    # Report error for the location mismatch
    # TODO IMPLEMENT
    discard
    # r.addResult(run, $run.expected.line & ':' & $run.expected.column,
    #             $given.line & ':' & $given.column, reLinesDiffer)

  # None of the unstructured checks found mismatches, reporting test passed
  else:
    # TODO IMPLEMENT ok
    discard
    # r.addResult(run, run.expected.msg, given.msg, reSuccess)
    # inc(r.passed)

proc nimcacheDir(
    filename: string,
    options: seq[ShellArg],
    target: GivenTarget
  ): string =
  ## Give each test a private nimcache dir so they don't clobber each other's.
  let hashInput = options.toStr().join("") & $target
  result = "nimcache" / (filename & '_' & hashInput.getMD5())

proc generatedFile(
    file: TestFile,
    options: seq[ShellArg],
    target: GivenTarget
  ): string =
  ## Get path to the generated file name from the test.
  case target:
    of targetJS:
      result = file.file.changeFileExt("js")
    else:
      let
        (_, name, _) = file.file.splitFile
        ext = target.ext
      result = "$#@m$#" % [
        nimcacheDir(file.file, options, target),
        name.changeFileExt(ext)
      ]

proc needsCodegenCheck(spec: DeclaredSpec): bool =
  ## If there is any checks that need to be performed for a generated code
  ## file
  spec.maxCodeSize > 0 or spec.ccodeCheck.len > 0

proc codegenCheck(
    e: Execution,
    id: RunId
  ): CodegenCheckCompare =
  ## Check for any codegen mismatches in file generated from `test` run.
  ## Only file that was immediately generated is tested.
  let (run, res, test, spec) = e.getRunContext(id)
  let genFile = generatedFile(e.files[test.file], test.options, run.target)

  try:
    let contents = readFile(genFile)
    for check in spec.ccodeCheck:
      if check.len > 0 and check[0] == '\\':
        # little hack to get 'match' support:
        if not contents.match(check.peg):
          result.missingPegs.add check

      elif contents.find(check.peg) < 0:
        result.missingPegs.add check

    if spec.maxCodeSize > 0 and contents.len > spec.maxCodeSize:
      result.codeSizeOverflow = some((
        generated: contents.len,
        allowed: spec.maxCodeSize
      ))

  except IOError:
    result.missingCode = some genFile

func nativeTarget(): GivenTarget =
  targetC

func defaultTargets*(category: Category): set[GivenTarget] =
  ## Return list of targets for a given category
  const standardTargets = {nativeTarget()}
  case category.string:
    of "lang":
      {targetC, targetJs, targetVM}
    of "arc", "avr", "destructor", "distros",
       "dll", "gc", "osproc", "parallel",
       "realtimeGC", "threads", "views",
       "valgrind":
      standardTargets - {targetJs}
    of "compilerapi", "compilerunits", "ic",
       "navigator", "lexer", "testament":
      {nativeTarget()}
    of "js":
      {targetJs}
    else:
      standardTargets

proc isTestFile*(file: string): bool =
  let (_, name, ext) = splitFile(file)
  result = ext == ".nim" and name.startsWith("t")

const AdditionalCategories = ["debugger", "lib"]

# include categories

proc loadSkipFrom(name: string): seq[string] =
  if name.len == 0: return
  # One skip per line, comments start with #
  # used by `nlvm` (at least)
  for line in lines(name):
    let sline = line.strip()
    if sline.len > 0 and not sline.startsWith('#'):
      result.add sline

proc prepareTestFilesAndSpecs(e: var Execution) =
  ## for the filters specified load all the specs
  # IMPLEMENT create specific type to avoid accidental mutation

  # NOTE read-only state in let to avoid mutation, put into types
  let
    testsDir = e.testsDir
    filter = e.filter

  e.skips = loadSkipFrom(e.skipsFile)

  proc testFilesFromCat(e: var Execution, cat: Category) =
    if cat.string notin ["testdata", "nimcache"]:
      let catId = e.categories.add(cat)
      case cat.string.normalize():
        of "gc":
          assert false, "TODO"
          # setupGcTests(e, catId)
        of "threads":
          assert false, "TODO"
          # setupThreadTests(e, catId)
        of "lib":
          assert false, "TODO"
          # IMPLEMENT: implement this proc and all the subsequent handling
          # setupStdlibTests(e, catId)
        else:
          for file in walkDirRec(testsDir & cat.string):
            if file.isTestFile:
              e.files.add TestFile(
                file: file, catId: catId)

  case filter.kind:
    of tfkAll:
      let testsDir = testsDir
      for kind, dir in walkDir(testsDir):
        if kind == pcDir:
          # The category name is extracted from the directory
          # eg: 'tests/compiler' -> 'compiler'
          let cat = dir[testsDir.len .. ^1]
          testFilesFromCat(e, Category(cat))

      for cat in AdditionalCategories:
        testFilesFromCat(e, Category(cat))

    of tfkCategories:
      for cat in filter.cats:
        testFilesFromCat(e, cat)

    of tfkGlob:
      let cat = e.categories.add Category("<glob>")
      let pattern = filter.pattern
      if dirExists(pattern):
        for kind, name in walkDir(pattern):
          if kind in {pcFile, pcLinkToFile} and name.endsWith(".nim"):
            e.files.add TestFile(file: name, catId: cat)

      else:
        for name in walkPattern(pattern):
          e.files.add TestFile(file: name, catId: cat)

    of tfkSingle:
      let cat = e.categories.add Category(parentDir(filter.test))
      let test = filter.test
      # IMPLEMENT: replace with proper error handling
      doAssert fileExists(test), test & " test does not exist"
      if isTestFile(test):
        e.files.add TestFile(file: test, catId: cat)

    else:
      assert false, "TODO ???"

  e.files.data.sort()

  # parse all specs
  for testId, test in pairs(e.files):
    e.tests.add GivenTest(file: testId, spec: testId)
    e.specs.add parseSpec(
      SpecParseConfig(
        filename: addFileExt(test.file, ".nim"),
        caGivenTargets: e.categories[test.catId].defaultTargets(),
        nativeTarget: nativeTarget(),
        skips: e.conf.skips,
        compilerPrefix: e.conf.compilerPrefix,
        retryContainer: e.retryContainer
      )
    )

    if e.testOpts.hasKey(testId):
      # apply additional test matrix, if specified
      let optMatrix = e.testOpts[testId].optMatrix
      let specId = e.tests[testId].spec
      e.specs[specId].matrix =
        case e.specs[specId].matrix.len
        of 0:
          optMatrix
        else:
          # (saem)REFACTOR - this is a hack, we shouldn't modify the
          # spec.matrix

          # (haxscramper)QUESTION Doesn't "specify additional test matrix
          # parameter" imply modification of the test matrix? It seems like
          # the most direct way to implement this feature: additional test
          # matrix parameter is ... added to the test matrix, seems pretty
          # logical to me.
          var tmp: seq[seq[ShellArg]] = @[]
          for options in optMatrix.items:
            for entry in e.specs[specId].matrix.items:
              # REFACTOR Switch to `seq[string]` for matrix flags
              tmp.add options & entry
          tmp

      # apply action override, if specified
      let actionOverride = e.testOpts[testId].action
      if actionOverride.isSome:
        e.specs[specId].action = actionOverride.get

const
  testResultsDir = "testresults"
  cacheResultsDir = testResultsDir / "cacheresults"
  noTargetsSpecified: set[GivenTarget] = {}
  defaultExecFlags = {outputColour}
  defaultBatchSize = 10
  defaultCatId = CategoryId(0)


func requestedTargets(execState: Execution): set[GivenTarget] =
  ## get the requested targets by the user or the defaults
  if execState.targets == noTargetsSpecified:
    {targetC, targetJS}
  else:
    execState.targets


proc makeName(e: Execution, id: RunId): string =
  let
    run = e.runs[id]
    test = e.tests[run.test]
    spec = e.specs[test.spec]
    target = run.target
    matrixEntry = e.runs[id].matrixEntry
    file = e.files[test.file].file
    allowFailure = spec.err in { grKnownIssue }

  result = file.changeFileExt("").replace(DirSep, '/')
  result.add '_' & $target
  if matrixEntry != noMatrixEntry:
    result.add "[$1]" % $matrixEntry.int
  if allowFailure:
    result.add " (allowed to fail) "
  if test.options.len > 0:
    result.add ' ' & test.options.toStr().join(" ")

  if matrixEntry != noMatrixEntry:
    result.add ' ' & spec.matrix[run.matrixEntry].toStr().join(" ")


proc prepareTestRuns(execState: var Execution) =
  ## create a list of necessary testRuns
  # IMPLEMENT: create specific type to avoid accidental mutation

  # NOTE: read-only items; only testRuns are written
  let
    testSpecs = execState.specs
    testFiles = execState.files
    tests = execState.tests
    categories = execState.categories

  for testId, spec in testSpecs.pairs:
    let
      specTargets =
        if spec.targets == noTargetsSpecified:
          categories[testFiles[tests[testId].file].catId].defaultTargets
        else:
          spec.targets
      targetsToRun = specTargets * execState.requestedTargets

    for target in targetsToRun:
      # TODO: create a "target matrix" to cover both js release vs non-release

      var entryIndices: seq[EntryId]
      case spec.matrix.len:
        of 0: # no tests to run
          entryIndices.add noMatrixEntry

        else:
          for entryId, _ in spec.matrix.pairs():
            entryIndices.add(entryId)

      for entryId in entryIndices:
        execState.runs.add TestRun(
          test: testId,
          target: target,
          matrixEntry: entryId,
        )

        execState.times.add RunTime()
        execState.results.add RunResult(lastAction: EmptyActionId)

proc prepareTestActions(execState: var Execution) =
  ## create a list of necessary test actions
  # IMPLEMENT : create specific type to avoid accidental mutation

  # NOTE: these are what are read, so a dedicate type would offer a
  # read-only view of those, but allow actions mutation
  let
    testRuns = execState.runs
    testSpecs = execState.specs

  # TODO: handle disabled and known issue

  for runId, run in testRuns.pairs:
    let actionKind = testSpecs[run.test].action
    case actionKind:
      of actionReject:
        execState.actions.add:
          TestAction(runId: runId, kind: actionReject)

      of actionCompile:
        execState.actions.add:
          TestAction(runId: runId, kind: actionCompile)

      of actionRun:
        let compileActionId = execState.actions.add:
          TestAction(
            runId: runId, kind: actionCompile, partOfRun: true)

        execState.actions.add:
          TestAction(
            runId: runId, kind: actionRun, compileActionId: compileActionId)


proc runTests(e: var Execution) =
  var next: seq[ActionId]
  for id, _ in e.actions:
    next.add(id)

  var completedCompile: PackedSet[ActionId]
  proc pickNextAction(e: var Execution): ActionId =
    result = EmptyActionId
    for idx in countdown(next.high(), 0):
      let act = next[idx]
      case e.actions[act].kind:
        of actionCompile, actionReject:
          next.delete(idx)
          return act

        of actionRun:
          if act in completedCompile:
            next.delete(idx)
            if e.results[e.actions[act].runId].compileCmp.compileSuccess:
              return act


  while 0 < next.len():
    var batch: seq[ActionId]
    while 0 < next.len() and batch.len() < defaultBatchSize:
      batch.add e.pickNextAction()

#   batch.execute()
#   batch.processExecutionResults()
#   for run in getTestRunsThatWereCompletedJustNow():
#     logInformationToTheUser()

# reportFullRunStatitisticsAndBackendInformation()



const
  failString* = "FAIL: " # ensures all failures can be searched with 1 keyword in CI logs
  testsDir = "tests" & DirSep
  knownIssueSuccessString* = "KNOWNISSUE: "
  resultsFile = "testresults.html"
  Usage = """Usage:
  testament [options] command [arguments]

Command:
  p|pat|pattern <glob>        run all the tests matching the given pattern
  all                         run all tests
  c|cat|category <category>   run all the tests of a certain category
  r|run <test>                run single test file
  html                        generate $1 from the database
  cache                       generate execution cache results
Arguments:
  arguments are passed to the compiler
Options:
  --retry                      runs tests that failed the last run
  --print                      print results to the console
  --verbose                    print commands (compiling and running tests)
  --simulate                   see what tests would be run but don't run them (for debugging)
  --failing                    only show failing/ignored tests
  --targets:"c js vm"          run tests for specified targets (default: all)
  --nim:path                   use a particular nim executable (default: $$PATH/nim)
  --directory:dir              Change to directory dir before reading the tests or doing anything else.
  --colors:on|off              Turn messages coloring on|off.
  --backendLogging:on|off      Disable or enable backend logging. By default turned on.
  --megatest:on|off            Enable or disable megatest. Default is on.
  --skipFrom:file              Read tests to skip from `file` - one test per line, # comments ignored
  --includeKnownIssues         runs tests that are marked as known issues

On Azure Pipelines, testament will also publish test results via Azure Pipelines' Test Management API
provided that System.AccessToken is made available via the environment variable SYSTEM_ACCESSTOKEN.

Experimental: using environment variable `NIM_TESTAMENT_REMOTE_NETWORKING=1` enables
tests with remote networking (as in CI).
""" % resultsFile


proc parseOpts(execState: var Execution, p: var OptParser): ParseCliResult =
  result = parseSuccess
  p.next()
  while p.kind in {cmdLongOption, cmdShortOption}:
    # read options agnostic of casing
    case p.key.normalize
    of "print": execState.flags.incl outputResults
    of "verbose": execState.flags.incl outputVerbose
    of "failing": execState.flags.incl outputFailureOnly
    of "targets":
      execState.targetsStr = p.val
      execState.gTargets = parseTargets(execState.targetsStr)
    of "nim":
      execState.conf.compilerPrefix = addFileExt(p.val.absolutePath, ExeExt)
    of "directory":
      setCurrentDir(p.val)
    of "colors":
      case p.val:
      of "on":  execState.flags.incl outputColour
      of "off": execState.flags.excl outputColour
      else: return parseQuitWithUsage
    of "batch":
      discard
    of "simulate":
      execState.flags.incl dryRun
    of "megatest":
      case p.val:
      of "on": execState.conf.useMegatest = true
      of "off": execState.conf.useMegatest = false
      else: return parseQuitWithUsage
    of "backendlogging":
      case p.val:
      of "on": execState.flags.incl logBackend
      of "off": execState.flags.excl logBackend
      else: return parseQuitWithUsage
    of "skipfrom": execState.skipsFile = p.val
    of "retry": execState.flags.incl rerunFailed
    else:
      return parseQuitWithUsage
    p.next()

proc parseArgs(execState: var Execution, p: var OptParser): ParseCliResult =
  result = parseSuccess

  var filterAction: string
  if p.kind != cmdArgument:
    return parseQuitWithUsage
  filterAction = p.key.normalize
  p.next()

  case filterAction
  of "all":
    # filter nothing, run everything
    execState.filter = TestFilter(kind: tfkAll)
  of "c", "cat", "category":
    # only specified category
    # HACK consider removing pcat concept or make parallel the default
    execState.filter = TestFilter(
        kind: tfkCategories,
        cats: initCategories [Category(p.key)])
  of "pcat":
    execState.filter = TestFilter(
        kind: tfkPCats,
        cats: initCategories [Category(p.key)])
  of "r", "run":
    # single test
    execState.filter = TestFilter(kind: tfkSingle, test: p.key)
  of "html":
    # generate html
    execState.filter = TestFilter(kind: tfkHtml)

  of "cache":
    # generate html
    execState.filter = TestFilter(kind: tfkCache)

  else:
    return parseQuitWithUsage

  execState.userTestOptions = p.cmdLineRest


proc main() =
  var
    p         = initOptParser() # cli parser
    execState = Execution(
      runProgress: RunProgress(
        lastCheckedRun: EmptyRunId,
        mostRecentRun: EmptyRunId,
      ),
      flags: defaultExecFlags,
      testsDir: "tests" & DirSep,
      rootDir: getCurrentDir(),
      gTargets: {low(GivenTarget) .. high(GivenTarget)},
      retryContainer: RetryContainer(retry: false),
      conf: TestamentConf(
        compilerPrefix: findExe("nim")
      )
    )

  case parseOpts(execState, p):
    of parseQuitWithUsage: quit Usage
    of parseSuccess:       discard

  # next part should be the the filter action, eg: cat, r, etc...
  case parseArgs(execState, p):
    of parseQuitWithUsage: quit Usage
    of parseSuccess:       discard

  if execState.workingDir != "":
    setCurrentDir(execState.workingDir)

  if targetJS in execState.targets:
    let nodeExe = findNodeJs()
    if nodeExe == "":
      quit "Failed to find nodejs binary in path", 1
    execState.nodejs = nodeExe

  prepareTestFilesAndSpecs(execState)
  prepareTestRuns(execState)
  prepareTestActions(execState)
  runTests(execState)

when isMainModule:
  main()
