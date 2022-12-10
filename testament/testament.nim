import
  std/[
    options,
    tables,
    strutils
  ],
  experimental/[
    dod_helpers
  ]

import specs, test_diff

declareIdType(Test)
declareIdType(File)
declareIdType(Run)
declareIdType(Spec)
declareIdType(Entry)
declareIdType(Action)
declareIdType(Category)

# TestId = int         # TODO: make this a distinct
# RunId = int          ## test run's id/index # TODO: make this a distinct
# EntryId = int        ## matrix entry index # TODO: make this a distinct
# ActionId = int       ## a test action's id # TODO: make this a distinct
# CategoryId = int     ## a category's id # TODO: make this a distinct


type
  Category = distinct string ## Name of the test category

  TResults = object
    # DOC WTF why is this even necessary?
    total, passed, knownIssuesSucceded, skipped: int
      ## TODO rename passed to passedOrAllowedFailure
    data: string

  GivenTest = object
    name: string
    cat: Category
    options: string
    testArgs: seq[string]
    spec: SpecId
    file: FileId

  TestRun = object
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

  # GivenSpec is in specs.nim
  # GivenSpec is the the spec in a single test file, but we run (`TestRun`) a test
  # for every target and matrix entry, which itself is a number of actions and
  # checks.

  # TestRun = object
  #   testId: TestId           ## test id for which this belongs
  #   target: TestTarget       ## which target to run for
  #   matrixEntry: EntryId     ## which item from the matrix was used

  # IMPLEMENT add 'check' to remove `cmd: "nim check"...` from tests
  # TestActionKind = enum
  #   testActionSkip           ## skip this test; check the spec for why
  #   testActionReject,        ## reject the compilation
  #   testActionCompile,       ## compile some source
  #   testActionRun            ## run the compiled program


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

  RunResult = object # CLEAN rename ot the 'RunResults' ("actual" implies
                     # there is something like "fake" run, which is not the
                     # case if I understand the implementation correctly.)
    ## actual data for a run
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

  RunCompare = object
    runResult: TestedResultKind

  TestOptionData = object
    optMatrix: seq[string]   ## matrix of cli options for this test
    action: Option[GivenTestAction] ## possible action override


  DebugInfo = OrderedTable[RunId, string]
  # REFACTOR - debug info should be per action instead of per run

  TestOptions = OrderedTable[TestId, TestOptionData]
    ## for legacy reasons (eg: `dllTests`) we need to be able to set per test
    ## options, ideally this would be done with `matrix`, but it's not
    ## sophisticated enough to support spare configuration like this needs


declareStoreType(TestRun, TestRuns, RunId)
declareStoreType(TestFile, Files, FileId)
declareStoreType(DeclaredSpec, Specs, SpecId)
declareStoreType(TestAction, Actions, ActionId)
declareStoreType(RunResult, Results, RunId)
declareStoreType(RunCompare, Compares, RunId)
declareStoreType(RunTime, Times, RunId)
declareStoreType(GivenTest, Tests, TestId)

type
  TestamentConf = object
    useColors: bool # = true
    backendLogging: bool # = true
    simulate: bool # = false
    optVerbose: bool # = false
    useMegatest: bool # = true
    optFailing: bool # = false


  Execution = object
    ## state object to data relevant for a testament run
    conf: TestamentConf

    userTestOptions: string ## options passed to tests by the user
    flags: ExecutionFlags   ## various options set by the user
    skipsFile: string       ## test files to skip loaded from `--skipFrom`
    targetsStr: string      ## targets as specified by the user
    filter: TestFilter      ## fitler used to assemble the test suite
    targets: seq[GivenTarget]    ## specified targets or `noTargetsSpecified`

    workingDir: string       ## working directory to begin execution in
    nodeJs: string           ## path to nodejs binary
    nimSpecified: bool       ## whether the user specified the nim
    testArgs: string         ## arguments passed to tests by the user
    isCompilerRepo: bool     ## whether this is the compiler repository, used
                             ## to legacy to handle `AdditionalCategories`

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
    tests: Tests
    runs: TestRuns ## a test run: reject, compile, or compile + run along
    ## with time to check; runs for a test must be contiguous and ordered
    times: Results   ## run timing information for each test run
    results: Results   ## actual information for a given run
    debugInfo: DebugInfo     ## debug info related to runs for tests, should be
                             ## per action instead of per run.
    runProgress: RunProgress ## current run progress based on action progress

    actions: Actions ## test actions for each run, phases of a run; actions
                     ## for a run must be continugous and ordered

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

  Categories = seq[Category]

  ParseCliResult = enum
    parseSuccess       ## successfully parsed cli params
    parseQuitWithUsage ## parsing failed, quit with usage message


func `$`*(cat: Category): string = "Category($#)" % $cat.string

const noMatrixEntry = -1

proc cmpMsgs(e: Execution, id: RunId): RunCompare =
  ## Compare all test output messages. This proc does structured or
  ## unstructured comparison comparison and immediately reports it's
  ## results.
  ##
  ## It is used to for performing 'reject' action checks - it compares
  ## both inline and regular messages - in addition to `nimoutCheck`
  # REFACTOR remove results, turn into data producer procedure

  # If structural comparison is requested - drop directly to it and handle
  # the success/failure modes in the branch
  let
    run = e.runs[id]
    test = e.tests[run.test]
    spec = e.specs[test.spec]

  if spec.nimoutSexp:
    let data = SexpCheckData(
      enforceFullMatch: spec.nimoutFull,
      inlineErrors: spec.inlineErrors,
      testName: test.name,
      givenNimout: e.results[id].nimout,
      expectedFile: e.files[test.file].file,
      expectedNimout: spec.nimout,
    )

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
  elif 0 < spec.inlineErrors.len():
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
    checkForInlineErrors(r, run, given)

  # Check for `.errormsg` in expected and given spec first
  elif strip(run.expected.msg) notin strip(given.msg):
    r.addResult(run, run.expected.msg, given.msg, reMsgsDiffer)

  # Compare expected and resulted spec messages
  elif not nimoutCheck(run.expected, given):
    # Report general message mismatch error
    r.addResult(run, run.expected.nimout, given.nimout, reMsgsDiffer)

  # Check for filename mismatches
  elif extractFilename(run.expected.file) != extractFilename(given.file) and
      "internal error:" notin run.expected.msg:
    # Report error for the the error file mismatch
    r.addResult(run, run.expected.file, given.file, reFilesDiffer)

  # Check for produced and given error message locations
  elif run.expected.line != given.line and
       run.expected.line != 0 or
       run.expected.column != given.column and
       run.expected.column != 0:
    # Report error for the location mismatch
    r.addResult(run, $run.expected.line & ':' & $run.expected.column,
                $given.line & ':' & $given.column, reLinesDiffer)

  # None of the unstructured checks found mismatches, reporting test passed
  else:
    r.addResult(run, run.expected.msg, given.msg, reSuccess)
    inc(r.passed)
