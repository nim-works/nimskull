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
    packedsets,
    osproc,
    times,
    streams,
    sugar
  ],
  experimental/[
    dod_helpers,
    shellrunner,
    colortext
  ]

import specs, test_diff, backend
import compiler/utils/nodejs
import hmisc/core/all
startHax()

declareIdType(Test)
declareIdType(Run)
declareIdType(Action)
declareIdType(Category)

## Blanket method to encaptsulate all echos while testament is detangled.
## Using this means echo cannot be called with separation of args and must
## instead pass a single concatenated string so that optional parameters
## can be included
type
  MessageType = enum
    Undefined
    Progress
    ProcessCmdCall

template msg(msgType: MessageType, parts: varargs[string, `$`]): untyped =
  let (file, line, col) = instantiationInfo()
  stdout.write "$#($#): " % [ file, $line ]
  stdout.writeLine parts
  flushFile stdout


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
    run: RunId
    case kind: GivenTestAction      # NOTE: might not need `partOfRun` at all
      of actionReject:
        discard
      of actionRun:
        compileAction: ActionId ## id of the preceeding compile action
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

  CodegenCheckCompare* = object
    ## Result of the codegen check comparison
    codeSizeOverflow: Option[tuple[generated, allowed: int]]
    missingPegs: seq[string]
    missingCode: Option[string]
    runResult: TestedResultKind

  CompileCompare* = object
    compileOut: CompileOutCompare
    codegenOut: CodegenCheckCompare

  RunCompare = object
    runResult: TestedResultKind

  ResultCategory = enum
    rcNone
    rcPass
    rcSkip
    rcFail
    rcKnownIssue
    rcKnownIssuePassed

  Result = object
    ## Final result of the test run
    nimout: string           ## nimout from compile, empty if not required
    nimExit: int             ## exit code produced by the compiler
    # CLEAN rename to 'last message', repack into the separate
    # structure/tuple with sane field names. Something like "last diag" or
    # something.
    nimMsg: string           ## last message, if any, from the compiler
    nimFile: string          ## filename from last compiler message, if
                             ## present
    nimLine: int             ## line from last compiler message, if present
    nimColumn: int           ## colunn from last compiler message, if present
    prgOut: string           ## program output, if any
    prgExit: int             ## program exit, if any
    lastAction: ActionId     ## last action in this run
    compileCmp: CompileCompare
    foundSuccess: bool ## "success" message was found in the compiler
                       ## output.
    runCmp: RunCompare
    compareCategory: ResultCategory


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
declareStoreType(Result, Results, RunId)
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
    batchSize: int
    compilerPrefix: string
    skips: seq[string]


  Execution = ref object
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
    testArgs: seq[ShellArg] ## arguments passed to tests by the user
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

func getRun(e: Execution, id: RunId): TestRun = e.runs[id]
func getAct(e: Execution, id: ActionId): TestAction = e.actions[id]
func getRun(e: Execution, id: Actionid): TestRun = e.getRun(e.getAct(id).run)
func getSpec(e: Execution, id: TestId): DeclaredSpec = e.specs[id]
func getSpec(e: Execution, id: RunId): DeclaredSpec =
  e.getSpec(e.getRun(id).test)
func getSpec(e: Execution, id: ActionId): DeclaredSpec =
  e.getSpec(e.getRun(id).test)
func getFile(e: Execution, id: TestId): TestFile = e.files[id]
func getFile(e: Execution, id: ActionId): TestFile =
  e.getFile(e.getRun(id).test)

func getResult(e: Execution, id: RunId): Result = e.results[id]
func getResult(e: Execution, id: Actionid): Result =
  e.getResult(e.getAct(id).run)


func getRunContext(e: Execution, id: RunId): tuple[
    run: TestRun, res: Result, test: GivenTest, spec: DeclaredSpec] =
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
        result.runResult = reMsgsDiffer

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

  # Compare expected and resulted spec messages
  elif not compileOutputCheck(
    full = spec.nimoutFull,
    expected = spec.nimout,
    given = res.nimout
  ):
    # Report general message mismatch error
    result.runResult = reMsgsDiffer

  # No explicit diagnostics target specified in the test, all other checks
  # passed successfully, the whole test is considered 'ok'
  elif spec.targetDiag.isNone():
    result.runResult = reSuccess

  # Target diagnosic was specified explicitly -- either partially (only
  # message) or completely (line, file, column, message etc.)
  else:
    let diag = spec.targetDiag.get()
    if diag.msg != res.nimMsg:
      if res.nimMsg.len() == 0:
        result.runResult = reNoDiagnostic

      # Check for `.errormsg` in expected and given spec first
      elif strip(diag.msg) notin strip(res.nimout):
        result.runResult = reMsgSubstringNotFound

      else:
        result.runResult = reMsgsDiffer

    elif extractFilename(spec.file) != extractFilename(res.nimFile):
      # File name is missing from the compiler diagnostic
      if res.nimFile.len() == 0:
        result.runResult = reNoDiagnosticFile

      else:
        result.runResult = reFilesDiffer

    # Check for produced and given error message locations
    elif diag.line != res.nimLine and diag.line != 0 or
         diag.col != res.nimColumn and diag.col != 0:
      # Report error for the location mismatch
      result.runResult = reLinesDiffer

    else:
      result.runResult = reSuccess

proc nimcacheDir(
    filename: string,
    options: seq[ShellArg],
    target: GivenTarget
  ): string =
  ## Give each test a private nimcache dir so they don't clobber each other's.
  let hashInput = options.toStr().join("") & $target
  result = "nimcache" / (filename & '_' & hashInput.getMD5())

proc absoluteNimcache(e: Execution, nimcache: string): string =
  if nimcache.isAbsolute():
    nimcache

  else:
    joinpath(e.rootDir, nimcache)

proc generatedFile(
    file: TestFile, target: GivenTarget, nimcache: string): string =
  ## Get path to the generated file name from the test.
  case target:
    of targetJS:
      result = file.file.changeFileExt("js")
    else:
      let
        (_, name, _) = file.file.splitFile
        ext = target.ext

      result = joinpath(nimcache, "@m" & name.changeFileExt(ext))

proc needsCodegenCheck(spec: DeclaredSpec): bool =
  ## If there is any checks that need to be performed for a generated code
  ## file
  0 < spec.maxCodeSize or 0 < spec.ccodeCheck.len()

proc codegenCheck(
    e: Execution,
    id: RunId
  ): CodegenCheckCompare =
  ## Check for any codegen mismatches in file generated from `test` run.
  ## Only file that was immediately generated is tested.
  let (run, res, test, spec) = e.getRunContext(id)
  let genFile = generatedFile(
    e.files[test.file],
    run.target,
    e.absoluteNimcache(nimcacheDir(
      e.files[test.file].file, test.options, run.target))
  )

  try:
    let contents = readFile(genFile)
    for check in spec.ccodeCheck:
      if 0 < check.len() and check[0] == '\\':
        # little hack to get 'match' support:
        if not contents.match(check.peg):
          result.missingPegs.add check

      elif contents.find(check.peg) < 0:
        result.missingPegs.add check

    if 0 < spec.maxCodeSize and contents.len > spec.maxCodeSize:
      result.runResult = reExcessiveCodeSize
      result.codeSizeOverflow = some((
        generated: contents.len,
        allowed: spec.maxCodeSize
      ))

    else:
      result.runResult = reSuccess

  except IOError:
    result.runResult = reCodeNotFound
    result.missingCode = some genFile

  assert result.runResult != reNone

proc compileCheck(e: var Execution, run: RunId): CompileCompare =
  result.compileOut = compileOutCheck(e, run)
  assert result.compileOut.runResult != reNone
  result.codegenOut = codegenCheck(e, run)
  assert result.codegenOut.runResult != reNone
  if e.results[run].nimExit == 0:
    result.compileOut.compileSuccess = true

func getPrgOutput(e: Execution, run: RunId): string =
  if e.specs[e.runs[run].test].sortOutput:
    var text = e.results[run].prgOut
    text.stripLineEnd()
    result = splitLines(text).sorted().join("\n") & "\n"

  else:
    result = e.results[run].prgOut


proc runCheck(e: var Execution, run: RunId): RunCompare =
  let
    spec = e.specs[e.runs[run].test]
    prgOut = e.getPrgOutput(run)

  if e.results[run].prgExit != spec.exitCode:
    result.runResult = reExitcodesDiffer

  elif spec.outputCheck == ocEqual and
     not spec.output.equalModuloLastNewline(prgOut):
    result.runResult = reOutputsDiffer

  elif spec.outputCheck == ocSubstr and spec.output notin prgOut:
    result.runResult = reOutputsDiffer

  else:
    result.runResult = reSuccess

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
        execState.results.add Result(lastAction: EmptyActionId)

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
    var last = EmptyActionId

    case actionKind:
      of actionReject:
        last = execState.actions.add:
          TestAction(run: runId, kind: actionReject)

      of actionCompile:
        last = execState.actions.add:
          TestAction(run: runId, kind: actionCompile)

      of actionRun:
        let compileAction = execState.actions.add:
          TestAction(
            run: runId, kind: actionCompile, partOfRun: true)

        last = execState.actions.add:
          TestAction(
            run: runId, kind: actionRun, compileAction: compileAction)

    execState.results[runId].lastAction = last

type
  TestBatch = ref object
    cmdToActId: seq[ActionId]


proc exeFile(
    testRun: TestRun, specFilePath: string, rootDir: string): string =
  ## Get name of the executable file for the test run
  # CLEAN into a smaller blocks without huge if/else gaps
  let
    target = testRun.target
    isJsTarget = target == targetJs
    (dirPart, specName, _) = splitFile(specFilePath)
    matrixEntry = testRun.matrixEntry

  let exeName = join(
    if matrixEntry == noMatrixEntry:
      @[specName, target.cmd.toStr()]
    else:
      @[specName, $matrixEntry, target.cmd.toStr()],
    "_"
  )

  let exeExt =
    if isJsTarget:
      "js"
    else:
      ExeExt

  let fileDir =
    if dirPart.isAbsolute():
      dirPart

    else:
      joinPath(rootDir, dirPart)

  result = changeFileExt(joinPath(fileDir, exeName), exeExt)

proc getFileDir(filename: string): string =
  result = filename.splitFile().dir
  if not result.isAbsolute():
    result = getCurrentDir() / result

proc getCompileCmd(e: Execution, id: ActionId): ShellCmd =
  let
    action = e.actions[id]
    runId = action.run
    testRun = e.runs[runId]
    testId = testRun.test
    spec = e.specs[testId]
    testFile = e.files[testId]
    filename = testFile.file
    target = testRun.target

  let matrixOptions =
    if testRun.matrixEntry == noMatrixEntry:
      newSeq[ShellArg]()
    else:
      spec.matrix[testRun.matrixEntry]

  result = getCmd(e.conf.compilerPrefix, spec)
  let nimcache = nimcacheDir(
    testFile.file, matrixOptions & e.testArgs, target)

  var options = target.defaultOptions() & e.testArgs
  options.add shArg("--nimCache=$#" % e.absoluteNimcache(nimcache))
  options.add shArg(
    "--out=$#" % testRun.exeFile(filename, e.rootDir))

  result = result.interpolate({
    "target": @[$target],
    "nim": @[e.conf.compilerPrefix],
    "options": options.toStr(),
    "file": @[filename],
    "filedir": @[filename.getFileDir()]
  })


proc getRunCmd(e: Execution, id: ActionId): ShellCmd =
  let
    run = e.getRun(id)
    spec = e.getSpec(id)
    isJsTarget = run.target == targetJs
    # specFile = execState.testSpecs[testId].file
    exeFile = run.exeFile(e.getFile(id).file, e.rootDir)

  let exeCmd =
    if isJsTarget:
      findNodeJs()
    elif spec.useValgrind != disabled:
      "valgrind"
    else:
      exeFile.dup(normalizeExe)

  let leakCheck =
    if spec.useValgrind == leaking:
      "yes"
    else:
      "no"

  let args =
    if isJsTarget:
      @[shArg"--unhandled-rejections=strict", shArg(exeFile)]
    elif spec.useValgrind != disabled:
      @[shArg("--error-exitcode=1"),
        shArg("--leak-check=" & leakCheck),
        shArg(exeFile)]
    else:
      @[]

  result = shell(exeCmd, args)

proc getCmd(e: Execution, id: ActionId): ShellCmd =
  case e.getAct(id).kind:
    of actionCompile, actionReject:
      result = getCompileCmd(e, id)

    of actionRun:
      result = getRunCmd(e, id)






proc actionInput(e: Execution, act: ActionId): Option[string] =
  if e.actions[act].kind == actionRun:
    return some e.specs[e.runs[e.actions[act].run].test].input

proc setTime(
    e: var Execution,
    act: ActionId,
    isCheck: bool,
    isStart: bool
  ) =
  let run = e.actions[act].run
  case e.actions[act].kind:
    of actionRun:
      if isStart:
        if isCheck:
          e.times[run].runCheckStart = epochTime()
        else:
          e.times[run].runStart = epochTime()

      else:
        if isCheck:
          e.times[run].runCheckEnd = epochTime()
        else:
          e.times[run].runEnd = epochTime()

    of actionCompile, actionReject:
      if isStart:
        if isCheck:
          e.times[run].compileCheckStart = epochTime()

        else:
          e.times[run].compileStart = epochTime()

      else:
        if isCheck:
          e.times[run].compileCheckEnd = epochTime()

        else:
          e.times[run].compileEnd = epochTime()


proc decolorize(str: string): string =
  ## Remove ANSI SGR escape sequences from the text and return plan version
  ## of the text.
  # IMPLEMENT
  result = str


proc addOut(e: var Execution, act: ActionId, str: string) =
  ## Add new piece of text to the corresponding field of the run results.
  ## Input string is stripped of the SGR characters in order to make
  ## further output comparisons function properly.
  let
    run = e.actions[act].run
    str = str.decolorize()

  case e.actions[act].kind:
    of actionRun:
      e.results[run].prgOut.add str

    of actionCompile, actionReject:
      e.results[run].nimOut.add str

proc setExit(e: var Execution, act: ActionId, exit: int) =
  let run = e.actions[act].run
  case e.actions[act].kind:
    of actionRun: e.results[run].prgExit = exit
    of actionCompile, actionReject: e.results[run].nimExit = exit

proc getCmd(e: Execution, batch: TestBatch, id: int): ShellCmd =
  e.getCmd(batch.cmdToActId[id])

proc initBatchHooks(e: Execution, batch: TestBatch): tuple[
    onRunStart: proc(id: int),
    onProcess: proc(id: int, p: Process),
    onComplete: proc(id: int, p: Process)
  ] =

  var e = e
  proc act(id: int): ActionId = batch.cmdToActId[id]

  proc onTestRunStart(id: int) =
    if e.conf.optVerbose:
      msg Undefined: "executing: " & e.getCmd(batch, id).toStr()

    e.setTime(act(id), isStart = true, isCheck = false)
    e.setExit(act(id), 0)

  proc onTestProcess(id: int, p: Process) =
    let testInput = e.actionInput(act(id))
    if testInput.isSome():
      let instream = inputStream(p)
      instream.write(testInput.get())
      close instream

  proc onTestRunComplete(id: int, p: Process) =
    if e.conf.optVerbose:
      msg Undefined: "finished execution of '$#' with code $#" % [
        e.getCmd(batch, id).toStr().join(" "),
        $p.peekExitCode()
      ]

    e.setTime(act(id), isStart = false, isCheck = false)

  result.onRunStart = onTestRunStart
  result.onProcess = onTestProcess
  result.onComplete = onTestRunComplete

proc initBatch(actions: seq[ActionId]): TestBatch =
  TestBatch(cmdToActId: actions)

proc runBatch(e: Execution, batch: TestBatch): seq[ShellResult] =
  let (onStart, onProcess, onEnd) = e.initBatchHooks(batch)
  var cmds: seq[ShellCmd]
  for act in batch.cmdToActId:
    cmds.add e.getCmd(act)

  result = cmds.exec(
    maxParallel = e.conf.batchSize,
    beforeRunEvent = onStart,
    startRunEvent = onProcess,
    afterRunEvent = onEnd
  )

proc trimUnitSep(x: sink string): string =
  result = x
  let start = result.len()
  if 0 < start and result[^1] == '\31':
    setLen(result, start - 1)

proc isSuccess(input: string): bool =
  # not clear how to do the equivalent of pkg/regex's: re"FOO(.*?)BAR" in
  # pegs note: this doesn't handle colors, eg: `\e[1m\e[0m\e[32mHint:`;
  # while we could handle colors, there would be other issues such as
  # handling other flags that may appear in user config (eg:
  # `--filenames`). Passing `XDG_CONFIG_HOME= testament args...` can be
  # used to ignore user config stored in XDG_CONFIG_HOME, refs
  # https://wiki.archlinux.org/index.php/XDG_Base_Directory
  input.startsWith("Hint: ") and input.endsWith("[SuccessX]")

proc setPegFields(e: var Execution, act: ActionId) =
  let
    lineError =
      peg"{[^(]*} '(' {\d+} ', ' {\d+} ') ' ('Error') ':' \s* {.*}"
    otherError = peg"'Error:' \s* {.*}"
    diagnostics = lineError / otherError

  let run = e.actions[act].run

  var err = none(string)
  for line in e.results[run].nimout.splitLines():
    let line = trimUnitSep(line)
    if line =~ diagnostics:
      err = some(line)

    elif line.isSuccess():
      e.results[run].foundSuccess = true

  if err.isSome():
    let err = err.get().trimUnitSep()

    e.results[run].nimMsg = err
    if err =~ lineError:
      e.results[run].nimMsg = matches[3]
      e.results[run].nimFile = extractFilename(matches[0])
      e.results[run].nimLine = parseInt(matches[1])
      e.results[run].nimColumn = parseInt(matches[2])

    elif err =~ otherError:
      e.results[run].nimMsg = matches[0]

proc classifyActionResult(e: Execution, act: ActionId): ResultCategory =
  let
    rcmp = e.getResult(act).runCmp
    ccmp = e.getResult(act).compileCmp
    spec = e.getSpec(act)

  proc aux(tested: TestedResultKind): ResultCategory =
    if tested == reSuccess: rcPass
    elif spec.isSkipped(): rcSkip
    elif tested == reNone: assert(false) ; rcFail
    else: rcFail

  # echov rcmp.runResult
  # echov aux(rcmp.runResult)
  # echov ccmp.compileOut.runResult
  # echov aux(ccmp.compileOut.runResult)
  # echov ccmp.codegenOut.runResult
  # echov aux(ccmp.codegenOut.runResult)

  let results = {
    aux(rcmp.runResult),
    aux(ccmp.compileOut.runResult),
    aux(ccmp.codegenOut.runResult)
  }

  if spec.isKnownIssue():
    # If this is a known issue then at least *something* must fail
    if len(results * {rcFail, rcSkip}) == 0:
      result = rcKnownIssuePassed

    else:
      result = rcKnownIssue

  else:
    if spec.err == grDisabled:
      result = rcSkip

    elif len(results * {rcFail}) == 0:
      # If nothing failed, consider test to be passed
      result = rcPass

    else:
      result = rcFail

proc setActionResults(
  e: var Execution, act: ActionId, res: ShellResult) =
  ## Set result of the action execution
  let run = e.actions[act].run

  e.addOut(act, res.stdout)
  e.addOut(act, res.stderr)
  e.setExit(act, res.retcode)
  e.setPegFields(act)

  # echov e.actions[act].kind
  case e.actions[act].kind:
    of actionRun:
      e.results[run].runCmp = runCheck(e, run)

    of actionCompile, actionReject:
      e.results[run].compileCmp = compileCheck(e, run)

  if act == e.getResult(act).lastAction:
    e.results[run].compareCategory = e.classifyActionResult(act)

proc formatCompare(e: Execution, act: ActionId): ColText =
  let
    res = e.getResult(act)
    spec = e.getSpec(act)

  assert res.compileCmp.compileOut.runResult != reNone
  assert res.compileCmp.codegenOut.runResult != reNone
  assert res.runCmp.runResult != reNone
  coloredResult()
  case e.getResult(act).compareCategory:
    of rcPass:
      add "PASS: " + fgGreen
      add e.makeName(e.getAct(act).run)

    of rcFail:
      add "FAIL: " + fgRed
      add e.makeName(e.getAct(act).run)
      let res = e.getResult(act)
      # echov res.compileCmp.compileOut
      # echov res.compileCmp.codegenOut
      # echov res.runCmp

      # echov res.compileCmp
      block:
        let r = res.compileCmp.codegenOut
        case r.runResult:
          of reCodeNotFound:
            add "\n"
            add "Generated code not found "
            add r.missingCode.get() + fgCyan
            add "\n"
            add "command was "
            add e.getCompileCmd(act).toStr().join(" ") + fgYellow

          else:
            echov r.runResult

      block:
        let r = res.runCmp
        echov r
        case r.runResult:
          of reOutputsDiffer:
            add "\n"
            add "Expected and given output mismatch"
            add "\n"
            let (diff, same) = diffStrings(res.prgOut, spec.output)
            add diff

          else:
            echov $r.runResult

    of rcSkip:
      add "SKIP: " + fgYellow
      add e.makeName(e.getAct(act).run)

    of rcKnownIssue:
      add "KNOWN ISSUE: " + fgYellow
      add e.makeName(e.getAct(act).run)

    else:
      assert false


proc immediateReport(e: Execution, act: ActionId) =
  ## Print immediate report on the action or test completion.
  let
    run = e.runs[e.actions[act].run]
    last = e.results[e.actions[act].run].lastAction
  if act != last:
    return

  echo formatCompare(e, act)

proc parseBatchOuts(
  e: var Execution, batch: TestBatch, results: seq[ShellResult]) =

  for idx, act in batch.cmdToActId:
    setActionResults(e, act, results[idx])

proc runTests(e: var Execution) =
  var next: seq[ActionId]
  for id, _ in e.actions:
    next.add(id)

  var completed: PackedSet[ActionId]
  proc pickNextAction(e: var Execution): ActionId =
    assert globalTick() < 20
    result = EmptyActionId

  while 0 < next.len():
    var actions: seq[ActionId]
    for idx in countdown(next.high(), 0):
      if actions.len() == e.conf.batchSize:
        break

      let act = next[idx]
      case e.actions[act].kind:
        of actionCompile, actionReject:
          next.delete(idx)
          actions.add(act)

        of actionRun:
          if e.actions[act].compileAction in completed:
            next.delete(idx)
            if e.results[e.actions[
              act].run].compileCmp.compileOut.compileSuccess:
              actions.add(act)


    let batch = initBatch(actions)
    let results = e.runBatch(batch)
    e.parseBatchOuts(batch, results)
    for act in actions:
      completed.incl(act)
      e.immediateReport(act)

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
        compilerPrefix: findExe("nim"),
        batchSize: 10
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
  echo "done"
