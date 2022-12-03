#
#
#            Testament
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Testament runs tests for the compiler.

import
  std/[
    strutils,
    pegs,
    os,
    osproc,
    streams,
    json,
    parseopt,
    browsers,
    terminal,
    algorithm,
    times,
    md5,
    intsets,
    macros,
    tables,
    options,
    sequtils
  ]
import backend, htmlgen, specs
from std/sugar import dup
import compiler/utils/nodejs
import lib/stdtest/testutils
from lib/stdtest/specialpaths import splitTestFile
import experimental/[sexp, sexp_diff, colortext, colordiff]

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

proc isNimRepoTests(): bool =
  # this logic could either be specific to cwd, or to some file derived from
  # the input file, eg testament r /pathto/tests/foo/tmain.nim; we choose
  # the former since it's simpler and also works with `testament all`.
  let file = "testament"/"testament.nim.cfg"
  result = file.fileExists

import experimental/dod_helpers



type
  Category = distinct string

  TResults = object
    total, passed, knownIssuesSucceded, skipped: int
      ## TODO rename passed to passedOrAllowedFailure
    data: string

  TTest = object
    name: string
    cat: Category
    options: string
    testArgs: seq[string]
    startTime: float
    duration: Option[float]
      ## allows newer code pass duration to legacy code
    spec: TSpec

  TestRun = object
    test: TTest
    expected: TSpec
    testId: TestId
    matrixEntry: EntryId
    target: TestTarget
    nimcache: string
    startTime: float
    debugInfo: string

  TestTarget = TTarget
  TestId = int         # TODO: make this a distinct
  RunId = int          ## test run's id/index # TODO: make this a distinct
  EntryId = int        ## matrix entry index # TODO: make this a distinct
  ActionId = int       ## a test action's id # TODO: make this a distinct
  CategoryId = int     ## a category's id # TODO: make this a distinct

  TestTargets = set[TestTarget]

  RetryInfo = object
    test: TestId       ## which test failed
    target: TestTarget ## the specific target

  RetryList = OrderedTable[TestId, RetryInfo]
      ## record failures in here so the user can choose to retry them

  TestFile = object
    file: string
    catId: CategoryId

  # TSpec is in specs.nim
  # TSpec is the the spec in a single test file, but we run (`TestRun`) a test
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
    case kind: TTestAction      # NOTE: might not need `partOfRun` at all
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

  RunActual = object
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
    runResult: TResultEnum   ## current result, invalid if `lastAction` unset

  RunActuals = seq[RunActual]

  TestOptionData = object
    optMatrix: seq[string]   ## matrix of cli options for this test
    action: Option[TTestAction] ## possible action override


  DebugInfo = OrderedTable[RunId, string]
  # REFACTOR - debug info should be per action instead of per run

  TestOptions = OrderedTable[TestId, TestOptionData]
    ## for legacy reasons (eg: `dllTests`) we need to be able to set per test
    ## options, ideally this would be done with `matrix`, but it's not
    ## sophisticated enough to support spare configuration like this needs

  Execution = object
    ## state object to data relevant for a testament run
    userTestOptions: string ## options passed to tests by the user
    flags: ExecutionFlags   ## various options set by the user
    skipsFile: string       ## test files to skip loaded from `--skipFrom`
    targetsStr: string      ## targets as specified by the user
    filter: TestFilter      ## fitler used to assemble the test suite
    targets: TestTargets    ## specified targets or `noTargetsSpecified`

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
    categories: Categories   ## categories discovered for this execution
                             ## first one is a default empty category `""`
    testFiles: seq[TestFile] ## files for this execution
    testSpecs: seq[TSpec]    ## spec for each file
    testOpts:  TestOptions   ## per test options, because legacy category magic

    # test execution data
    testRuns: seq[TestRun]   ## a test run: reject, compile, or compile + run
                             ## along with time to check; runs for a test must
                             ## be contiguous and ordered
    runTimes: seq[RunTime]   ## run timing information for each test run
    runActuals: RunActuals   ## actual information for a given run
    debugInfo: DebugInfo     ## debug info related to runs for tests, should be
                             ## per action instead of per run.
    runProgress: RunProgress ## current run progress based on action progress

    actions: seq[TestAction] ## test actions for each run, phases of a run;
                             ## actions for a run must be continugous and
                             ## ordered

    # test execution related data
    retryList: RetryList     ## list of failures to potentially retry later

    # legacy compat stuff -- try to remove
    legacyTestResults: TResults  ## Legacy compatability for result reporting
                                 ## kept to limit refactor work to running
                                 ## tests and not include reporting.
    legacyTestData: seq[string]  ## `TResults` had a `data` field of type
                                 ## `string` where we appended a message per
                                 ## test... which seems horribly wasteful.
                                 ## keep it around until we know we can kill it

  RunProgress = object
    ## Acts as a tracker for a producer/consumer model, where `lastCheckedRun`
    ## is where the consumer left off, and `mostRecentRun` is where the
    ## producer left off.
    lastCheckedRun: RunId ## run last reviewed for reporting/consumption
    mostRecentRun: RunId  ## run that completed most recently/production

  ExecutionFlag = enum
    outputColour,      ## colour the output
    outputResults,     ## print results to the console
    outputFailureOnly, ## only output failures
    outputVerbose,     ## increase output verbosity
    logBackend         ## enable backend logging
    dryRun,            ## do not run the tests, only indicate which would run
    rerunFailed,       ## only run tests failed in the previous run
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

# ----------------------------------------------------------------------------

# TODO: remove global state

var
  useColors = true
  backendLogging = true
  simulate = false
  optVerbose = false
  useMegatest = true
  optFailing = false

# ----------------------------------------------------------------------------

proc trimUnitSep(x: var string) =
  let L = x.len
  if L > 0 and x[^1] == '\31':
    setLen x, L-1

# REFACTOR move all diff-related formatting into the separate file to
# reduce clutter in this one.
proc diffStrings*(a, b: string): tuple[output: string, same: bool] =
  let a = a.split("\n")
  let b = b.split("\n")
  var maxA = 0
  var maxB = 0
  for line in a:
    maxA = max(maxA, line.len)

  for line in b:
    maxB = max(maxB, line.len)

  var conf = diffFormatter()
  conf.sideBySide = maxA + maxB + 8 < terminalWidth()
  conf.groupLine = true

  let diff = myersDiff(a, b)
  if len(diff) == 0:
    result.same = true

  else:
    result.same = false
    result.output = diff.shiftDiffed(a, b).
      formatDiffed(a, b, conf).toString(useColors)

proc format(tcmp: TOutCompare): ColText =
  ## Pretty-print structured output comparison for further printing.
  var conf = diffFormatter()

  coloredResult()

  var first = true
  proc addl() =
    if not first:
      add "\n"

    first = false

  for (pair, weight) in tcmp.sortedMapping:
    if 0 < weight:
      addl()
      addl()
      let exp = tcmp.expectedReports[pair[0]]
      add "Expected"
      if exp.inline.isSome():
        let inline = exp.inline.get()
        addf(" inline $# annotation at $#($#, $#)",
          inline.kind + fgGreen,
          exp.file + fgYellow,
          $inline.line + fgCyan,
          $inline.col + fgCyan
        )

      addf(":\n\n- $#\n\nGiven:\n\n+ $#\n\n",
        exp.node.toLine(sortfield = true),
        tcmp.givenReports[pair[1]].node.toLine(sortfield = true)
      )

      add tcmp.diffMap[pair].describeDiff(conf).indent(2)


  for exp in tcmp.ignoredExpected:
    addl()
    addl()
    addf(
      "Missing expected annotation:\n\n? $#\n\n",
      tcmp.expectedReports[exp].node.toLine(sortfield = true)
    )

  if tcmp.cantIgnoreGiven:
    for give in tcmp.ignoredGiven:
      addl()
      addl()
      addf(
        "Unexpected given annotation:\n\n? $#\n\n",
        tcmp.expectedReports[give].node.toLine(sortfield = true)
      )

## Blanket method to encaptsulate all echos while testament is detangled.
## Using this means echo cannot be called with separation of args and must
## instead pass a single concatenated string so that optional parameters
## can be included
type
  MessageType = enum
    Undefined,
    Progress,
    ProcessCmdCall

proc msg(msgType: MessageType; parts: varargs[string, `$`]) =
  if optFailing and not optVerbose and msgType == ProcessCmdCall:
    return
  stdout.writeLine parts
  flushFile stdout


proc verboseCmd(cmd: string) =
  if optVerbose:
    msg Undefined: "executing: " & cmd

# ----------------------------------------------------------------------------

let
  pegLineError =
    peg"{[^(]*} '(' {\d+} ', ' {\d+} ') ' ('Error') ':' \s* {.*}"
  pegOtherError = peg"'Error:' \s* {.*}"
  pegOfInterest = pegLineError / pegOtherError

var gTargets = {low(TTarget)..high(TTarget)}

proc isSuccess(input: string): bool =
  # not clear how to do the equivalent of pkg/regex's: re"FOO(.*?)BAR" in
  # pegs note: this doesn't handle colors, eg: `\e[1m\e[0m\e[32mHint:`;
  # while we could handle colors, there would be other issues such as
  # handling other flags that may appear in user config (eg:
  # `--filenames`). Passing `XDG_CONFIG_HOME= testament args...` can be
  # used to ignore user config stored in XDG_CONFIG_HOME, refs
  # https://wiki.archlinux.org/index.php/XDG_Base_Directory
  input.startsWith("Hint: ") and input.endsWith("[SuccessX]")

proc getFileDir(filename: string): string =
  result = filename.splitFile().dir
  if not result.isAbsolute():
    result = getCurrentDir() / result

proc execCmdEx2(command: string, args: openArray[string]; workingDir, input: string = ""): tuple[
                cmdLine: string,
                output: string,
                exitCode: int] {.tags:
                [ExecIOEffect, ReadIOEffect, RootEffect], gcsafe.} =

  result.cmdLine.add quoteShell(command)
  for arg in args:
    result.cmdLine.add ' '
    result.cmdLine.add quoteShell(arg)
  verboseCmd(result.cmdLine)
  var p = startProcess(command, workingDir = workingDir, args = args,
                       options = {poStdErrToStdOut, poUsePath})
  var outp = outputStream(p)

  # There is no way to provide input for the child process
  # anymore. Closing it will create EOF on stdin instead of eternal
  # blocking.
  let instream = inputStream(p)
  instream.write(input)
  close instream

  result.exitCode = -1
  var line = newStringOfCap(120)
  while true:
    if outp.readLine(line):
      result.output.add line
      result.output.add '\n'
    else:
      result.exitCode = peekExitCode(p)
      if result.exitCode != -1: break
  close(p)

proc nimcacheDir(filename, options: string, target: TTarget): string =
  ## Give each test a private nimcache dir so they don't clobber each other's.
  let hashInput = options & $target
  result = "nimcache" / (filename & '_' & hashInput.getMD5)

# REFACTOR use command + options
proc prepareTestCompileCmd(cmdTemplate, filename, options, nimcache: string,
                     target: TTarget, extraOptions = "", outfile = ""): string =

  var options = target.defaultOptions & ' ' & options
  if nimcache.len > 0: options.add(" --nimCache:$#" % nimcache.quoteShell())
  if 0 < outfile.len: options.add(" --out=$#" % outfile.quoteShell())
  options.add ' ' & extraOptions
  # we avoid using `parseCmdLine` which is buggy, refs bug #14343
  result = cmdTemplate % [
    "target", target.cmd,
    "options", options,
    "file", filename.quoteShell,
    "filedir", filename.getFileDir(),
    "nim", compilerPrefix
  ]

proc callCompiler(
  cmdTemplate, filename, options, nimcache: string,
  target: TTarget, extraOptions = "", outfile = ""): TSpec =
  ## Execute nim compiler with given `filename`, `options` and `nimcache`.
  ## Compile to target specified in the `target` and return compilation
  ## results as a new `TSpec` value. Resulting spec contains `.nimout` set
  ## from the compiler run results as well as known inline messages (output
  ## is immedately scanned for results).
  result.cmd = prepareTestCompileCmd(
    cmdTemplate, filename, options, nimcache, target, extraOptions, outfile)
  verboseCmd(result.cmd)
  var p = startProcess(command = result.cmd,
                       options = {poStdErrToStdOut, poUsePath, poEvalCommand})
  let outp = p.outputStream
  var foundSuccessMsg = false
  var foundErrorMsg = false
  var err = ""
  var x = newStringOfCap(120)
  result.nimout = ""
  while true:
    if outp.readLine(x):
      trimUnitSep x
      result.nimout.add(x & '\n')
      if x =~ pegOfInterest:
        # `err` should contain the last error message
        err = x
        foundErrorMsg = true
      elif x.isSuccess:
        foundSuccessMsg = true
    elif not running(p):
      break
  close(p)
  result.msg = ""
  result.file = ""
  result.output = ""
  result.line = 0
  result.column = 0

  result.err = reNimcCrash
  let exitCode = p.peekExitCode
  case exitCode
  of 0:
    if foundErrorMsg:
      result.debugInfo.add " compiler exit code was 0 but some Error's were found."
    else:
      result.err = reSuccess
  of 1:
    if not foundErrorMsg:
      result.debugInfo.add " compiler exit code was 1 but no Error's were found."
    if foundSuccessMsg:
      result.debugInfo.add " compiler exit code was 1 but no `isSuccess` was true."
  else:
    result.debugInfo.add " expected compiler exit code 0 or 1, got $1." % $exitCode

  if err =~ pegLineError:
    result.file = extractFilename(matches[0])
    result.line = parseInt(matches[1])
    result.column = parseInt(matches[2])
    result.msg = matches[3]
  elif err =~ pegOtherError:
    result.msg = matches[0]
  trimUnitSep result.msg

proc initResults: TResults =
  result.total = 0
  result.passed = 0
  result.knownIssuesSucceded = 0
  result.skipped = 0
  result.data = ""

macro ignoreStyleEcho(args: varargs[typed]): untyped =
  let typForegroundColor = bindSym"ForegroundColor".getType
  let typBackgroundColor = bindSym"BackgroundColor".getType
  let typStyle = bindSym"Style".getType
  let typTerminalCmd = bindSym"TerminalCmd".getType
  result = newCall(bindSym"echo")
  for arg in children(args):
    if arg.kind == nnkNilLit: continue
    let typ = arg.getType
    if typ.kind != nnkEnumTy or
       typ != typForegroundColor and
       typ != typBackgroundColor and
       typ != typStyle and
       typ != typTerminalCmd:
      result.add(arg)

template maybeStyledEcho(args: varargs[untyped]): untyped =
  if useColors:
    styledEcho(args)
  else:
    ignoreStyleEcho(args)


proc `$`(x: TResults): string =
  result = """
Tests passed: $2 / $1 <br />
Tests known issues succeeded: $3 / $1 <br />
Tests skipped: $4 / $1 <br />
""" % [$x.total, $x.passed, $x.knownIssuesSucceded, $x.skipped]

proc getName(test: TTest): string =
  result = test.name.replace(DirSep, '/')
  if test.options.len > 0:
    result.add ' ' & test.options

proc getName(run: TestRun): string =
  result = run.test.name.replace(DirSep, '/')
  result.add ' ' & $run.target
  if run.test.options.len > 0:
    result.add ' ' & run.test.options

proc logToConsole(param: ReportParams, givenSpec: ptr TSpec = nil) =
  ## Format test infomation to the console. `test` contains information
  ## about the test itself, `param` contains additional data about test
  ## execution.

  let durationStr = param.duration.formatFloat(
    ffDecimal, precision = 2).align(5)

  template dispNonSkipped(color, outcome) =
    if not optFailing or color == fgRed:
      maybeStyledEcho(
        color, outcome, fgCyan, param.debugInfo, alignLeft(param.name, 60),
        fgBlue, " (", durationStr, " sec)")

  template disp(msg) =
    if not optFailing:
      maybeStyledEcho(
        styleDim, fgYellow, msg & ' ', styleBright, fgCyan, param.name)

  template dispFail(param: ReportParams) =
    dispNonSkipped(fgRed, failString)
    doAssert param.cat.len > 0, "no category for test: $1" % param.origName
    
    maybeStyledEcho(
      styleBright, fgCyan, "Test \"", param.origName, "\"",
      " in category \"", param.cat, "\"")

    maybeStyledEcho styleBright, fgRed, "Failure: ", $param.success

  template onFailGivenSpecDebugInfo(givenSpec: ptr TSpec) =
    if givenSpec != nil and givenSpec.debugInfo.len > 0:
      msg Undefined: "debugInfo: " & givenSpec.debugInfo

  case param.success
  of reSuccess:
    dispNonSkipped(fgGreen, "PASS: ")
  of reDisabled, reKnownIssue:
    disp:
      if param.inCurrentBatch:
        "SKIP:"
      else:
        "NOTINBATCH:"
  of reJoined:
    disp("JOINED:")
  of reInvalidSpec:
    dispFail(param)
    msg Undefined: param.given
  of reBuildFailed, reNimcCrash, reInstallFailed:
    dispFail(param)
    onFailGivenSpecDebugInfo(givenSpec)
    # expected is empty, no reason to print it.
    msg Undefined: param.given
  else:
    dispFail(param)
    onFailGivenSpecDebugInfo(givenSpec)

    if param.outCompare.isNil:
      # REFACTOR error message formatting should be based on the
      # `TestReport` data structure that contains all the necessary
      # information to generate the error message.
      maybeStyledEcho fgYellow, "Expected:"
      maybeStyledEcho styleBright, param.expected, "\n"
      maybeStyledEcho fgYellow, "Gotten:"
      maybeStyledEcho styleBright, param.given, "\n"
      msg Undefined:
        diffStrings(param.expected, param.given).output
    else:
      msg Undefined:
        param.outCompare.format().toString(useColors)


proc logToBackend(param: ReportParams) =
  if backendLogging:
    backend.writeTestResult(param)


proc addResult(r: var TResults, param: ReportParams, givenSpec: ptr TSpec) =
  ## Report results to backend, end user (write to command-line) and so on.

  # REFACTOR: first of all, name: it does not /add/ it fucking *writes*
  # output. "write" and "add" are two different things. Second: determine
  # why `givenSpec` is an optional in the first place -- aren't we printing
  # out results of the test? And each test has a spec attached to it.

  # REFACTOR (old note, leaving it out until this whole mess is cleaned)
  # instead of `ptr tspec` we could also use `option[tspec]`; passing
  # `givenspec` makes it easier to get what we need instead of having to
  # pass individual fields, or abusing existing ones like expected vs
  # given. test.name is easier to find than test.name.extractfilename a bit
  # hacky but simple and works with tests/testament/tshould_not_work.nim

  logToBackend(param)

  # REFACTOR what is this, no string interpolation until the data is
  # properly collected into a DOD group; then it can be reported.
  r.data.addf("$#\t$#\t$#\t$#", param.name, param.expected, param.given,
                                $param.success)

  # Write to console
  logToConsole(param, givenSpec)

proc addResult(
    r: var TResults,
    run: TestRun,
    expected, given: string,
    successOrig: TResultEnum,
    givenSpec: ptr TSpec = nil,
    outCompare: TOutCompare = nil
  ) =
  ## Report final test run to backend, end user (write to command-line) and
  ## etc

  # REFACTOR this is a legacy garbage that needs to be turned into
  # something like `initTestReport()` which will return `TestReport` which
  # is then added into a `dod` sequence of reports (and written out so the
  # user can see the progress instead of waiting in front of the dead
  # prompt, but that's basically the same as `echo done` at the end of this
  # proc, from the data flow perspective it is largely irrelevant IMO)
  let
    duration = epochTime() - run.startTime
    timeout = run.expected.timeout
    success =
      if timeout > 0.0 and duration > timeout:
        reTimeout
      else:
        successOrig
    targetStr = $run.target
    param = ReportParams(
      duration: duration,
      name: run.getName(),
      origName: run.test.name,
      cat: run.test.cat.string,
      action: run.test.spec.action,
      targetStr: targetStr,
      debugInfo: run.debugInfo,
      outCompare: outCompare,
      success: success,
      knownIssues: if isNil(givenSpec): @[] else: givenSpec[].knownIssues,
      inCurrentBatch: run.test.spec.inCurrentBatch,
      expected: expected,
      given: given
    )

  addResult(r, param, givenSpec)

proc addResult(r: var TResults, test: TTest) =
  ## Report test failure/skip etc to backend, end user (write to command-line)
  ## and so on.
  # REFACTOR same as previous procedure -- convert into `initTestReport()`
  # and return the data. Unroll the reportig stack and streamline the data
  # flow.
  const allowedTestStatus = {reInvalidSpec, reDisabled, reKnownIssue, reJoined}
  doAssert test.spec.err in allowedTestStatus,
           "Test: $1 with status: $2, should have ran" %
              [test.getName(), $test.spec.err]
  let
    given =
      case test.spec.err
      of reInvalidSpec: test.spec.parseErrors
      of reKnownIssue: test.spec.knownIssues.join("\n")
      else: ""
    param = ReportParams(
      duration: epochTime() - test.startTime,
      name: test.getName(),
      origName: test.name,
      cat: test.cat.string,
      action: test.spec.action,
      targetStr: "",
      debugInfo: "",
      success: test.spec.err,
      inCurrentBatch: test.spec.inCurrentBatch,
      knownIssues: test.spec.knownIssues,
      expected: "",
      given: given,
      outCompare: nil
    )
  
  addResult(r, param, nil)


proc checkForInlineErrors(r: var TResults, run: TestRun, given: TSpec) =
  ## Check for inline error annotations in the nimout results, comparing
  ## them with the output of the compiler.
  # REFACTOR refactor to return results

  let pegLine = peg"{[^(]*} '(' {\d+} ', ' {\d+} ') ' {[^:]*} ':' \s* {.*}"
  var covered = initIntSet()
  for line in splitLines(given.nimout):
    # Iterate over each line in the output

    # Searching for the `file(line, col) Severity: text` pattern
    if line =~ pegLine:
      let file = extractFilename(matches[0])
      let line = try: parseInt(matches[1]) except: -1
      let col = try: parseInt(matches[2]) except: -1
      let kind = matches[3]
      let msg = matches[4]

      if file == extractFilename run.test.name:
        # If annotation comes from the target file
        var i = 0
        for x in run.expected.inlineErrors:
          if x.line == line and (x.col == col or x.col < 0) and
              x.kind == kind and x.msg in msg:
            # And annotaiton has matching line, column and message
            # information, register it as 'covered'
            covered.incl i
          inc i

  block coverCheck:
    for j in 0..high(run.expected.inlineErrors):
      #  For each output message that was not covered by annotations, add it
      # to the output as 'missing'
      if j notin covered:
        var e: string
        let exp = run.expected.inlineErrors[j]
        e = run.test.name
        e.add '('
        e.addInt exp.line
        if exp.col > 0:
          e.add ", "
          e.addInt exp.col
        e.add ") "
        e.add exp.kind
        e.add ": "
        e.add exp.msg

        r.addResult(run, e, given.nimout, reMsgsDiffer)
        break coverCheck

    r.addResult(run, "", given.msg, reSuccess)
    inc(r.passed)

proc nimoutCheck(expected, given: TSpec): bool =
  ## Check if expected nimout values match with specified ones. This check
  ## implements comparison of the unstructured data.
  result = true
  if expected.nimoutFull:
    if expected.nimout != given.nimout:
      result = false
  elif expected.nimout.len > 0 and
       # NOTE this function should be made more generic and moved into the
       # diff library, it might have some pretty interesting use-cases in
       # places where determinization of the output gives UX improvements.
       not greedyOrderedSubsetLines(expected.nimout, given.nimout):
    result = false

proc sexpCheck(test: TTest, expected, given: TSpec): TOutCompare =
  ## Check if expected nimout values match with specified ones. Thish check
  ## implements a structured comparison of the data and returns full report
  ## about all the mismatches that can be formatted as needed.
  ## This procedure determines whether `given` spec matches `expected` test
  ## results.
  var r = TOutCompare()
  r.cantIgnoreGiven = expected.nimoutFull

  for exp in expected.inlineErrors:
    var parsed = parseSexp(exp.msg)
    var loc = convertSexp([sexp(test.name), sexp(exp.line)])
    if exp.col > 0:
      loc.add sexp(exp.col)

    parsed.addField("location", loc)
    parsed.addField("severity", newSSymbol(exp.kind))
    r.expectedReports.add TOutReport(
      inline: some exp, node: parsed, file: expected.file)

  for line in splitLines(expected.nimout):
    if 0 < line.len:
      r.expectedReports.add TOutReport(node: parseSexp(line))

  for line in splitLines(given.nimout):
    if 0 < line.len:
      r.givenReports.add TOutReport(node: parseSexp(line))

  proc reportCmp(a, b: int): int =
    # Best place for further optimization and configuration - if more
    # comparison speed is needed, try starting with error kind, file, line
    # comparison, then doing a regular msg != msg compare and only then
    # deep structural diff.
    if r.expectedReports[a].node[0] != r.givenReports[b].node[0]:
      result += 10

    let diff = diff(r.expectedReports[a].node, r.givenReports[b].node)
    r.diffMap[(a, b)] = diff
    result += diff.len

  (r.ignoredExpected, r.ignoredGiven, r.sortedMapping) = stableMatch(
    r.expectedReports.len,
    r.givenReports.len,
    reportCmp,
    Descending
  )

  if 0 < r.sortedMapping[0].cost:
    r.match = false
  elif 0 < r.ignoredGiven.len and expected.nimoutFull:
    r.match = false
  else:
    r.match = true

  return r

proc cmpMsgs(r: var TResults, run: TestRun, given: TSpec) =
  ## Compare all test output messages. This proc does structured or
  ## unstructured comparison comparison and immediately reports it's
  ## results.
  ##
  ## It is used to for performing 'reject' action checks - it compares
  ## both inline and regular messages - in addition to `nimoutCheck`
  # REFACTOR remove results, turn into data producer procedure

  # If structural comparison is requested - drop directly to it and handle
  # the success/failure modes in the branch
  if run.expected.nimoutSexp:
    let outCompare = run.test.sexpCheck(run.expected, given)
    # Full match of the output results.
    if outCompare.match:
      r.addResult(run, run.expected.msg, given.msg, reSuccess)
      inc(r.passed)
    else:
      # Write out error message.
      r.addResult(
        run, run.expected.msg, given.msg,
        reMsgsDiffer,
        givenSpec = unsafeAddr given,
        outCompare = outCompare
      )

  # Checking for inline errors.
  elif run.expected.inlineErrors.len > 0:
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

proc generatedFile(test: TTest, target: TTarget): string =
  ## Get path to the generated file name from the test.
  case target:
    of targetJS:
      result = test.name.changeFileExt("js")
    else:
      let
        testFile = test.spec.file
        (_, name, _) = testFile.splitFile
        ext = target.ext
      result = nimcacheDir(testFile, test.options, target) / (
        "@m" & name.changeFileExt(ext))

proc needsCodegenCheck(spec: TSpec): bool =
  ## If there is any checks that need to be performed for a generated code
  ## file
  spec.maxCodeSize > 0 or spec.ccodeCheck.len > 0

proc codegenCheck(
    test: TTest,
    target: TTarget,
    spec: TSpec,
    expectedMsg: var string,
    given: var TSpec
  ) =
  ## Check for any codegen mismatches in file generated from `test` run.
  ## Only file that was immediately generated is tested.
  # REFACTOR convert into procedure that creates `CodegenCheckResult`
  # object which contains the required information in a structured manner.
  # No need to pass mutable strings that get appended to. Also
  # specification should not be edited under any circumstances (aside from
  # initial construction) nor should it be passed as a mutable parameter.
  try:
    # CLEAN single section
    let genFile = generatedFile(test, target)
    let contents = readFile(genFile)
    for check in spec.ccodeCheck:
      if check.len > 0 and check[0] == '\\':
        # little hack to get 'match' support:
        if not contents.match(check.peg):
          given.err = reCodegenFailure
      elif contents.find(check.peg) < 0:
        given.err = reCodegenFailure
      expectedMsg = check
    if spec.maxCodeSize > 0 and contents.len > spec.maxCodeSize:
      given.err = reCodegenFailure
      given.msg = "generated code size: " & $contents.len
      expectedMsg = "max allowed size: " & $spec.maxCodeSize
  except ValueError:
    # NOTE REFACTOR that's an excellent candidate for separation into the
    # test report kind.
    given.err = reInvalidPeg
    msg Undefined: getCurrentExceptionMsg()
  except IOError:
    given.err = reCodeNotFound
    msg Undefined: getCurrentExceptionMsg()

proc compilerOutputTests(run: TestRun, given: var TSpec; r: var TResults) =
  ## Test output of the compiler for correctness and add comparison results
  ## to the `TResults` output.
  # REFACTOR into a functio that returns result of the output test
  # comparison instead of mutating `TResults` in-place.

  # CLEAN into single block
  var expectedmsg: string = ""
  var givenmsg: string = ""
  var outCompare: TOutCompare
  if given.err == reSuccess:
    # Check size??? of the generated C code. If fails then add error
    # message.
    if run.expected.needsCodegenCheck:
      codegenCheck(run.test, run.target, run.expected, expectedmsg, given)
      givenmsg = given.msg

    if run.expected.nimoutSexp:
      # If test requires structural comparison - run it and then check
      # output results for any failures.
      outCompare = run.test.sexpCheck(run.expected, given)
      if not outCompare.match:
        given.err = reMsgsDiffer

    else:
      # Use unstructured data comparison for the expected and given outputs
      if not nimoutCheck(run.expected, given):
        given.err = reMsgsDiffer

        # Just like unstructured comparison - assign expected/given pair.
        # In that case deep structural comparison is not necessary so we
        # are just pasing strings around, they will be diffed only on
        # reporting.
        expectedmsg = run.expected.nimout
        givenmsg = given.nimout.strip

  else:
    givenmsg = "$ " & given.cmd & '\n' & given.nimout
  
  if given.err == reSuccess:
    inc(r.passed)

  # Write out results of the compiler output testing
  r.addResult(
    run, expectedmsg, givenmsg, given.err,
    givenSpec = addr given,
    # Supply results of the optional structured comparison.
    outCompare = outCompare
  )

proc checkDisabled(r: var TResults, test: TTest): bool =
  ## Check if test has been enabled (not `disabled: true`, and not joined).
  ## Return true if test can be executed.
  if test.spec.err in {reDisabled, reKnownIssue, reJoined}:
    # targetC is a lie, but parameter is required
    r.addResult(test)
    inc(r.skipped)
    inc(r.total)
    result = false
  else:
    result = true

var count = 0

proc equalModuloLastNewline(a, b: string): bool =
  # allow lazy output spec that omits last newline, but really those should
  # be fixed instead
  result = a == b or b.endsWith("\n") and a == b[0 ..< ^1]

func extraOptions(run: TestRun): string =
  if run.test.spec.matrix.len > 0 and run.matrixEntry != noMatrixEntry:
    run.test.spec.matrix[run.matrixEntry]
  else:
    ""

proc exeFile(
    testRun: TestRun, specFilePath: string, rootDir: string): string =
  ## Get name of the executable file for the test run
  # CLEAN into a smaller blocks without huge if/else gaps
  let
    target = testRun.target
    isJsTarget = target == targetJs
    (dirPart, specName, _) = splitFile(specFilePath)
    matrixEntry = testRun.matrixEntry
    exeName =
      (if matrixEntry == noMatrixEntry:
        @[specName, target.cmd]
      else:
        @[specName, $matrixEntry, target.cmd]).join("_")
    exeExt =
      if isJsTarget:
        "js"
      else:
        ExeExt

    fileDir = if dirPart.isAbsolute():
                dirPart

              else:
                joinPath(rootDir, dirPart)

  result = changeFileExt(joinPath(fileDir, exeName), exeExt)



proc testSpecHelper(r: var TResults, run: var TestRun, execution: Execution) =
  run.startTime = epochTime()
  run.test.startTime = run.startTime # REFACTOR set the same for legacy
                                     # reasons

  proc callCompilerImpl(run: TestRun): TSpec =
    # NOTE this used to also pass: `--stdout --hint:Path:off`, but was done
    # inconsistently with other branches
    callCompiler(
      run.expected.getCmd,
      run.test.name,
      run.test.options,
      run.nimcache,
      run.target,
      run.extraOptions,
      outfile = run.exeFile(
        execution.testFiles[run.testId].file, execution.rootDir)
    )
  
  case run.expected.action
  of actionCompile:
    var given = run.callCompilerImpl()
    compilerOutputTests(run, given, r)

  of actionReject:
    let given = run.callCompilerImpl()
    # Scan compiler output fully for all mismatches and report if any found
    cmpMsgs(r, run, given)

  of actionRun:
    var given = run.callCompilerImpl()
    if given.err != reSuccess:
      r.addResult(
        run, "", "$ " & given.cmd & '\n' & given.nimout,
        given.err, givenSpec = given.addr)
    else:
      let
        isJsTarget = run.target == targetJS
        ext =
          case run.target
          of targetJS: "js"
          of targetVM: "nimbc"
          else: ExeExt

      var exeFile = changeFileExt(run.test.name, ext)
      
      if not fileExists(exeFile):
        r.addResult(run, run.expected.output,
                    "executable not found: " & exeFile, reExeNotFound)
      else:
        let nodejs = if isJsTarget: findNodeJs() else: ""
      
        if isJsTarget and nodejs == "":
          r.addResult(run, run.expected.output, "nodejs binary not in PATH",
                      reExeNotFound)
        else:
          var exeCmd: string
          var args = run.test.testArgs
          
          if isJsTarget:
            exeCmd = nodejs
            # see D20210217T215950
            args = @["--unhandled-rejections=strict", exeFile] & args
          elif run.target == targetVM:
            let nimDir = compilerPrefix.getFileDir()
            exeCmd = changeFileExt(nimDir / "vmrunner", ExeExt)
            args = @[exeFile]
          else:
            exeCmd = exeFile.dup(normalizeExe)
            if run.expected.useValgrind != disabled:
              var valgrindOptions = @["--error-exitcode=1"]
              if run.expected.useValgrind != leaking:
                valgrindOptions.add "--leak-check=yes"
              args = valgrindOptions & exeCmd & args
              exeCmd = "valgrind"

          var (_, buf, exitCode) = execCmdEx2(
            exeCmd, args, input = run.expected.input)

          # Treat all failure codes from nodejs as 1. Older versions of
          # nodejs used to return other codes, but for us it is sufficient
          # to know that it's not 0.
          if exitCode != 0:
            exitCode = 1
          
          let bufB =
            if run.expected.sortoutput:
              var buf2 = buf
              buf2.stripLineEnd
              var x = splitLines(buf2)
              sort(x, system.cmp)
              join(x, "\n") & '\n'
            else:
              buf
          
          if exitCode != run.expected.exitCode:
            r.addResult(run,
                        "exitcode: " & $run.expected.exitCode,
                        "exitcode: " & $exitCode & "\n\nOutput:\n" & bufB,
                        reExitcodesDiffer)
          elif (
            run.expected.outputCheck == ocEqual and
            not run.expected.output.equalModuloLastNewline(bufB)
          ) or (
            run.expected.outputCheck == ocSubstr and
            run.expected.output notin bufB
          ):
            given.err = reOutputsDiffer
            r.addResult(run, run.expected.output, bufB, reOutputsDiffer)
          else:
            compilerOutputTests(run, given, r)

proc targetHelper(r: var TResults, run: var TestRun, execution: Execution) =
  inc(r.total)
  if run.target notin gTargets:
    r.addResult(run, "", "", reDisabled)
    inc(r.skipped)
  elif simulate:
    inc count
    msg Undefined: "testSpec count: " & $count & " expected: " & $run.expected
  else:
    testSpecHelper(r, run, execution)

func nativeTarget(): TTarget =
  targetC

func defaultTargets(category: Category): set[TTarget] =
  const standardTargets = {nativeTarget()}
  case category.string
  of "lang":
    {targetC, targetJs, targetVM}
  of "arc", "avr", "destructor", "distros", "dll", "gc", "osproc", "parallel",
     "realtimeGC", "threads", "views", "valgrind":
    standardTargets - {targetJs}
  of "compilerapi", "compilerunits", "ic", "navigator", "lexer", "testament":
    {nativeTarget()}
  of "js":
    {targetJs}
  else:
    standardTargets

proc testSpec(r: var TResults, test: TTest, execution: Execution) =
  var expected = test.spec
  if expected.parseErrors.len > 0:
    r.addResult(test)
    inc(r.total)
    return

  if not checkDisabled(r, test):
    return

  if expected.targets == {}:
    expected.targets = test.cat.defaultTargets()

  for target in expected.targets:
    var runTemplate = TestRun(
      test: test,
      expected: test.spec,
      matrixEntry: noMatrixEntry,
      target: target,
      nimcache: nimcacheDir(test.name, test.options, target),
      startTime: epochTime()
    )

    testSpecHelper(r, runTemplate, execution)

    if test.spec.matrix.len > 0:
      for entryId, _ in test.spec.matrix.pairs:
        var run = runTemplate
        run.matrixEntry = entryId
        targetHelper(r, run, execution)
    else:
      var run = runTemplate
      run.matrixEntry = noMatrixEntry
      targetHelper(r, run, execution)

proc testSpecWithNimcache(
    r: var TResults,
    test: TTest, nimcache: string, execution: Execution) {.used.} =

  if not checkDisabled(r, test): return
  for target in (test.spec.targets * gTargets):
    inc(r.total)
    var testRun = TestRun(
      test: test,
      expected: test.spec,
      matrixEntry: noMatrixEntry,
      target: target,
      nimcache: nimcache
    )
    testSpecHelper(r, testRun, execution)

proc initTest(test, options: string; cat: Category, spec: TSpec): TTest =
  ## make a test with the given spec, meant to be used internally as a
  ## constructor mostly
  result.cat = cat
  result.name = test
  result.options = options
  result.spec = spec
  result.startTime = epochTime()

proc makeTest(test, options: string, cat: Category): TTest =
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

# TODO: fix these files
const disabledFilesDefault = @[
  "tableimpl.nim",
  "setimpl.nim",
  "hashcommon.nim",

  # Requires compiling with '--threads:on'
  "sharedlist.nim",
  # Error: undeclared identifier: 'hasThreadSupport'
  "ioselectors_epoll.nim",
  "ioselectors_kqueue.nim",
  "ioselectors_poll.nim",

  # Error: undeclared identifier: 'Timeval'
  "ioselectors_select.nim",
]

const
  # array of modules disabled from compilation test of stdlib.
  disabledFiles = disabledFilesDefault

include categories

proc loadSkipFrom(name: string): seq[string] =
  if name.len == 0: return
  # One skip per line, comments start with #
  # used by `nlvm` (at least)
  for line in lines(name):
    let sline = line.strip()
    if sline.len > 0 and not sline.startsWith('#'):
      result.add sline

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
      gTargets = parseTargets(execState.targetsStr)
    of "nim":
      compilerPrefix = addFileExt(p.val.absolutePath, ExeExt)
    of "directory":
      setCurrentDir(p.val)
    of "colors":
      case p.val:
      of "on":  execState.flags.incl outputColour
      of "off": execState.flags.excl outputColour
      else: return parseQuitWithUsage
    of "batch":
      testamentData0.batchArg = p.val
      if p.val != "_" and p.val.len > 0 and p.val[0] in {'0'..'9'}:
        let s = p.val.split("_")
        doAssert s.len == 2, $(p.val, s)
        testamentData0.testamentBatch = s[0].parseInt
        testamentData0.testamentNumBatch = s[1].parseInt
        doAssert testamentData0.testamentNumBatch > 0
        doAssert testamentData0.testamentBatch >= 0 and testamentData0.testamentBatch < testamentData0.testamentNumBatch
    of "simulate":
      execState.flags.incl dryRun
    of "megatest":
      case p.val:
      of "on": useMegatest = true
      of "off": useMegatest = false
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
        cats: @[Category(p.key)])
  of "pcat":
    execState.filter = TestFilter(
        kind: tfkPCats,
        cats: @[Category(p.key)])
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

if paramCount() == 0:
  quit Usage


const
  testResultsDir = "testresults"
  cacheResultsDir = testResultsDir / "cacheresults"
  noTargetsSpecified: TestTargets = {}
  defaultExecFlags = {outputColour}
  defaultBatchSize = 10
  defaultCatId: CategoryId = 0

func requestedTargets(execState: Execution): set[TTarget] =
  ## get the requested targets by the user or the defaults
  if execState.targets == noTargetsSpecified:
    {targetC, targetJS}
  else:
    execState.targets

func `<`(a, b: TestFile): bool {.inline.} =
  a.file < b.file
func cmp(a, b: TestFile): int {.inline.} =
  cmp(a.file, b.file)

proc prepareTestFilesAndSpecs(execState: var Execution) =
  ## for the filters specified load all the specs
  # IMPLEMENT create specific type to avoid accidental mutation

  # NOTE read-only state in let to avoid mutation, put into types
  let
    testsDir = execState.testsDir
    filter = execState.filter
    isCompilerRepo = execState.isCompilerRepo

  # REFACTOR: legacy set `specs.skips`
  skips = loadSkipFrom(execState.skipsFile)

  template testFilesFromCat(execState: var Execution, cat: Category) =
    if cat.string notin ["testdata", "nimcache"]:
      let catId = execState.categories.len
      execState.categories.add cat

      var handled = true

      let normCat = cat.string.normalize
      if isCompilerRepo:
        case normCat
        of "gc":
          setupGcTests(execState, catId)
        of "threads":
          setupThreadTests(execState, catId)
        of "lib":
          # IMPLEMENT: implement this proc and all the subsequent handling
          setupStdlibTests(execState, catId)
        else:
          handled = false

      if not handled:
        for file in walkDirRec(testsDir & cat.string):
          if file.isTestFile:
            execState.testFiles.add:
              TestFile(file: file, catId: catId)

  case filter.kind:
    of tfkAll:
      let testsDir = testsDir
      for kind, dir in walkDir(testsDir):
        if kind == pcDir:
          # The category name is extracted from the directory
          # eg: 'tests/compiler' -> 'compiler'
          let cat = dir[testsDir.len .. ^1]
          testFilesFromCat(execState, Category(cat))
      if isCompilerRepo: # handle `AdditionalCategories`
        for cat in AdditionalCategories:
          testFilesFromCat(execState, Category(cat))

    of tfkCategories:
      for cat in filter.cats:
        testFilesFromCat(execState, cat)

    of tfkGlob:
      execState.categories = @[Category "<glob>"]
      let pattern = filter.pattern
      if dirExists(pattern):
        for kind, name in walkDir(pattern):
          if kind in {pcFile, pcLinkToFile} and name.endsWith(".nim"):
            execState.testFiles.add TestFile(file: name)
      else:
        for name in walkPattern(pattern):
          execState.testFiles.add TestFile(file: name)

    of tfkSingle:
      execState.categories = @[Category parentDir(filter.test)]
      let test = filter.test
      # IMPLEMENT: replace with proper error handling
      doAssert fileExists(test), test & " test does not exist"
      if isTestFile(test):
        execState.testFiles.add TestFile(file: test)

    else:
      assert false, "TODO ???"

  execState.testFiles.sort # ensures we have a reproducible ordering

  # parse all specs
  for testId, test in pairs(execState.testFiles):
    execState.testSpecs.add parseSpec(
      addFileExt(test.file, ".nim"),
      execState.categories[test.catId].defaultTargets(),
      nativeTarget()
    )

    if execState.testOpts.hasKey(testId):
      # apply additional test matrix, if specified
      let optMatrix = execState.testOpts[testId].optMatrix
      execState.testSpecs[testId].matrix =
        case execState.testSpecs[testId].matrix.len
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
          var tmp: seq[string] = @[]
          for o in optMatrix.items:
            for e in execState.testSpecs[testId].matrix.items:
              # REFACTOR Switch to `seq[string]` for matrix flags
              tmp.add o & " " & e
          tmp

      # apply action override, if specified
      let actionOverride = execState.testOpts[testId].action
      if actionOverride.isSome:
        execState.testSpecs[testId].action = actionOverride.get

proc makeName(test: TestFile,
              testRun: TestRun,
              allowFailure: bool
              ): string =
  let
    target = testRun.target
    matrixEntry = testRun.matrixEntry
  result = test.file.changeFileExt("").replace(DirSep, '/')
  result.add '_' & $target
  if matrixEntry != noMatrixEntry:
    result.add "[$1]" % $matrixEntry
  if allowFailure:
    result.add " (allowed to fail) "
  # if test.options.len > 0:
  #   result.add ' ' & test.options



proc prepareTestRuns(execState: var Execution) =
  ## create a list of necessary testRuns
  # IMPLEMENT: create specific type to avoid accidental mutation

  # NOTE: read-only items; only testRuns are written
  let
    testSpecs = execState.testSpecs
    testFiles = execState.testFiles
    categories = execState.categories

  for testId, spec in testSpecs.pairs:
    let
      specTargets =
        if spec.targets == noTargetsSpecified:
          categories[testFiles[testId].catId].defaultTargets
        else:
          spec.targets
      targetsToRun = specTargets * execState.requestedTargets

    for target in targetsToRun:
      # TODO: create a "target matrix" to cover both js release vs non-release

      let baseTest = TTest(
        cat: categories[testFiles[testId].catId],
        spec: spec
      )

      case spec.matrix.len:
        of 0: # no tests to run
          var run = TestRun(
            testId: testId,
            test: baseTest,
            target: target,
            matrixEntry: noMatrixEntry)

          run.test.name = testFiles[testId].makeName(
            run, spec.err == reKnownIssue)

          execState.testRuns.add run
          execState.runTimes.add RunTime()
          execState.runActuals.add RunActual()

        else:
          for entryId, _ in spec.matrix.pairs:
            var run = TestRun(
              testId: testId,
              target: target,
              matrixEntry: entryId,
              test: baseTest
            )

            run.test.options = spec.matrix[entryId]
            run.test.name = testFiles[testId].makeName(
              run, spec.err == reKnownIssue)

            execState.testRuns.add run
            execState.runTimes.add RunTime()
            execState.runActuals.add RunActual()

proc prepareTestActions(execState: var Execution) =
  ## create a list of necessary test actions
  # IMPLEMENT : create specific type to avoid accidental mutation

  # NOTE: these are what are read, so a dedicate type would offer a
  # read-only view of those, but allow actions mutation
  let
    testRuns = execState.testRuns
    testSpecs = execState.testSpecs

  # TODO: handle disabled and known issue

  for runId, run in testRuns.pairs:
    let actionKind = testSpecs[run.testId].action
    case actionKind
    of actionReject, actionCompile:
      execState.actions.add:
        TestAction(runId: runId, kind: actionKind)
    of actionRun:
      let compileActionId = execState.actions.len
      execState.actions.add:
        TestAction(runId: runId, kind: actionCompile, partOfRun: true)
      execState.actions.add:
        TestAction(runId: runId,
                   kind: actionRun,
                   compileActionId: compileActionId)

proc reportTestRunResult(
    legacyResults: var TResults,
    execution: Execution,
    cat: Category,
    testFile: TestFile,
    spec: TSpec,
    testRun: TestRun,
    runActual: RunActual,
    runTime: RunTime,
    action: TestAction,
    cmd: string,
    debugInfo: string
  ) =
  ## Report results of a single test run
  # REFACTOR birdge newer testament internals (`ExecutionState` and
  # friends) to the legacy reporting and generate output accordingly.

  let
    duration = runTime.compileEnd - runTime.compileStart +
                runTime.runEnd - runTime.runStart
    target = testRun.target
    allowFailure = spec.err == reKnownIssue

  var
    # REFACTOR test run should contianer ID of the specification it was
    # used for as well as information about it's matrix parameters. The ID
    # should be used instead of this copy.
    givenAsSpec = TSpec(
      cmd: cmd,
      nimout: runActual.nimout,
      msg: runActual.nimMsg,
      file: runActual.nimFile,
      output: runActual.prgOut,
      line: runActual.nimLine,
      column: runActual.nimColumn,
      err: runActual.runResult,
      debugInfo: debugInfo)
    legacyTest = testRun.test

  case spec.action
  of actionCompile:
    compilerOutputTests(testRun, givenAsSpec, legacyResults)

  of actionRun:
    case action.kind
    of actionCompile:
      case givenAsSpec.err:
        of reSuccess:
          discard # success will be determined after `actionRun`
        of reExeNotFound:
          legacyResults.addResult(
            testRun,
            spec.output,
            "executable not found: " & testRun.exeFile(
              givenAsSpec.file, execution.rootDir),
            reExeNotFound
          )
        else:
          # all other errors
          legacyResults.addResult(
            testRun,
            "",
            "$ " & givenAsSpec.cmd & '\n' & givenAsSpec.nimout,
            givenAsSpec.err,
            givenSpec = givenAsSpec.addr
          )
    of actionRun:
      case givenAsSpec.err:
        of reExitcodesDiffer:
          legacyResults.addResult(
            testRun,
            "exitcode: " & $spec.exitCode,
            "exitcode: $#\n\nOutput:\n$#" % [
              case runActual.prgExit:
                # HACK Legacy. Treat all failure codes from nodejs as 1.
                # Older versions of nodejs used to return other codes, but
                # for us it is sufficient to know that it's not 0.
                of 0: $0
                else: $1
              ,
              $givenAsSpec.cmd
            ], reExitcodesDiffer)

        of reOutputsDiffer:
          legacyResults.addResult(
            testRun,
            testRun.expected.output,
            givenAsSpec.nimout,
            reOutputsDiffer
          )

        else:
          compilerOutputTests(
            testRun, givenAsSpec, legacyResults)

    of actionReject:
      doAssert false, "we should never get here"
  of actionReject:
    # Scan compiler output fully for all mismatches and report if any found
    cmpMsgs(legacyResults, testRun, givenAsSpec)

proc runTestBatch(
    execState: var Execution,
    testCmds: var seq[string],
    processOpts: set[ProcessOption],
    batchSize: int,
    exitCodes: seq[int],
    outputs: seq[string],
    startTimes, endTimes: seq[float],
    onTestRunStart: proc (i: int),
    onTestProcess, onTestRunComplete: proc (i: int, p: Process),
    cmdIdToActId: var seq[int],
    cmdIdToActKind: var seq[TTestAction],
    cmdIdToInput: var seq[Option[string]],
    batches: var int
  ) =

  inc batches

  # TODO - handle executable not found issues for `actionRun`, test this
  #        before the action is swapped in from next to current batch (it
  #        should have already been created). If it's not, note progress
  #        and remove from runnable actions.

  # run actions
  discard osproc.execProcesses(
    testCmds,
    processOpts,
    batchSize,
    onTestRunStart,
    onTestProcess,
    onTestRunComplete
  )

  for id in 0 ..< cmdIdToActId.len:
    let
      actionId = cmdIdToActId[id]
      action = execState.actions[actionId]
      runId = action.runId
      testRun = execState.testRuns[runId]
      testId = testRun.testId
      spec = execState.testSpecs[testId]
      cmd = testCmds[id]
      testFile = execState.testFiles[testId]
      # durationStr = duration.formatFloat(ffDecimal, precision = 2).align(5)

    execState.runActuals[runId].lastAction = actionId

    case action.kind
    of actionCompile, actionReject:
      execState.runTimes[runId].compileStart = startTimes[id]
      execState.runTimes[runId].compileEnd = endTimes[id]

      # REFACTOR into a proc

      # look for compilation errors or success messages
      let output = newStringStream(outputs[id])
      var
        line = newStringOfCap(120)
        err = ""
        foundSuccessMsg = false
      while output.readLine(line):
        trimUnitSep line
        execState.runActuals[runId].nimout.add(line & '\n')
        if line =~ pegOfInterest:
          # `err` should contain the last error message
          err = line
        elif line.isSuccess:
          foundSuccessMsg = true
      output.close

      # REFACTOR Highly likely this can be turned into a procedure that
      # returns `typeof(execState.runActuals[runId])` as a result which is
      # then assigned to the real `runActuals` sequence at the specified
      # run ID.

      # validate exit code and collect action debug info
      execState.runActuals[runId].nimExit = exitCodes[id]
      execState.runActuals[runId].runResult = reNimcCrash
      case exitCodes[id]
      of 0:
        if err != "":
          execState.debugInfo.mgetOrPut(runId, "").add:
            " compiler exit code was 0 but some Error's were found"
        else:
          execState.runActuals[runId].runResult = reSuccess
      of 1:
        if err == "": # no error found
          execState.debugInfo.mgetOrPut(runId, "").add:
            " compiler exit code was 1 but no Error's were found."
        if foundSuccessMsg:
          execState.debugInfo.mgetOrPut(runId, "").add:
            " compiler exit code was 1 but found a success message (see: testament.isSuccess)."
      else:
        execState.debugInfo.mgetOrPut(runId, "").add:
          " expected compiler exit code 0 or 1, got $1." % $exitCodes[id]

      # set the last error message and get any relevant position info
      if err =~ pegLineError:
        execState.runActuals[runId].nimMsg = matches[3]
        execState.runActuals[runId].nimFile = extractFilename(matches[0])
        execState.runActuals[runId].nimLine = parseInt(matches[1])
        execState.runActuals[runId].nimColumn = parseInt(matches[2])
      elif err =~ pegOtherError:
        execState.runActuals[runId].nimMsg = matches[0]
      execState.runActuals[runId].nimMsg.trimUnitSep

      let exeFile = exeFile(testRun, spec.file, execState.rootDir)
      if spec.action == actionCompile and
         execState.runActuals[runId].runResult == reSuccess and
         not fileExists(exeFile):
        execState.runActuals[runId].runResult = reExeNotFound

    of actionRun:
      execState.runTimes[runId].runStart = startTimes[id]
      execState.runTimes[runId].runEnd = endTimes[id]

      let output = newStringStream(outputs[id])
      var line = newStringOfCap(120)
      while output.readLine(line):
        execState.runActuals[runId].prgOut.add(line & '\n')
      output.close

      execState.runActuals[runId].prgExit = exitCodes[id]

      # REFACTOR very ridiculous approach to comparing output, legacy junk
      # sorry, i can't even being to explain this mess; brought over from
      # `testSpecHelper`
      let
        expect = spec.output
        exitCode =
          if execState.runActuals[runId].prgExit != 0:
            # Treat all failure codes from nodejs as 1. Older versions of
            # nodejs used to return other codes, but for us it is sufficient
            # to know that it's not 0.
            1
          else:
            0
        testResult =
          if exitCode != spec.exitCode:
            reExitcodesDiffer
          else:
            let
              check = spec.outputCheck
              given =
                if spec.sortoutput:
                  var buffer = execState.runActuals[runId].prgOut
                  buffer.stripLineEnd
                  var x = buffer.splitLines
                  sort(x, system.cmp)
                  x.join("\n") & '\n'
                else:
                  execState.runActuals[runId].prgOut

            if spec.sortoutput:
              # REFACTOR - storing this for legacy test reporting
              execState.runActuals[runId].prgOutSorted = given

            if (check == ocEqual and not expect.equalModuloLastNewline(given)) or
               (check == ocSubstr and expect notin given):
              reOutputsDiffer
            else:
              reSuccess

      # TODO - this isn't quite right because we're not skipping broken
      #        compiles yet.
      execState.runActuals[runId].runResult = testResult

    var legacyResults = execState.legacyTestResults

    # REFACTOR reduce number of arguments in this function.
    reportTestRunResult(
      legacyResults = legacyResults,
      execution = execState,
      cat = execState.categories[testFile.catId],
      testFile = testFile,
      spec = spec,
      testRun = testRun,
      runActual = execState.runActuals[runId],
      runTime = execState.runTimes[runId],
      action = action,
      cmd = cmd,
      debugInfo = execState.debugInfo.getOrDefault(runId, "")
    )

    execState.legacyTestResults = legacyResults # write them back

    # echo "batch: ", batches, ", runId: ", runId, ", actionId: ", actionId, ", duration: ", durationStr, ", cmd: ", testCmds[id]

  # clear old batch information
  testCmds.setLen(0)
  cmdIdToActId.setLen(0)
  cmdIdToActKind.setLen(0)
  cmdIdToInput.setLen(0)

proc runTests(execState: var Execution) =
  ## execute test runs in batches of test actions
  # <<mutation>> IMPLEMENT: create specific type to avoid accidental
  # mutation

  let
    testRuns = execState.testRuns   ## immutable view of test runs
    testActions = execState.actions ## immutable view of test actions
    batchSize = defaultBatchSize    ## parallel processes to execute
                                    # TODO: use processor count
    testArgs = execState.testArgs   ## arguments from the cli for each test
    verbose = outputVerbose in execState.flags
    dryRun = ExecutionFlag.dryRun in execState.flags
    totalCats = execState.categories.len

  # test commands and a mapping for command id (`osproc.execProcesses`) to
  # actionId, along with a pair of `next` ones so we can serialize the compile
  # and execute phase of compile program then run compiled program.
  var
    testCmds = newSeqOfCap[string](batchSize)
    cmdIdToActId = newSeqOfCap[int](batchSize)
    cmdIdToActKind = newSeqOfCap[TTestAction](batchSize)
    cmdIdToInput = newSeqOfCap[Option[string]](batchSize)
    nextTestCmds = newSeqOfCap[string](batchSize)
    nextCmdIdToActId = newSeqOfCap[int](batchSize)
    nextCmdIdToActKind = newSeqOfCap[TTestAction](batchSize)
    nextCmdIdToInput = newSeqOfCap[Option[string]](batchSize)
    # QUESTION ASSUME I assume thread war is added here because of the
    # subsequent `execProcesses`, but IIRC they do not use threading -- at
    # least on unix platforms the implementation is done as a combination
    # of `fork`, `exec` and `wait` which then collects all the required
    # data and triggers callbacks.
    exitCodes {.threadvar.}: seq[int]
    outputs {.threadvar.}: seq[string]
    startTimes {.threadvar.}: seq[float]
    endTimes {.threadvar.}: seq[float]
    batches = 0
    startedCategoryIds: seq[CategoryId]

  exitCodes = newSeq[int](batchSize)
  outputs = newSeq[string](batchSize)
  startTimes = newSeq[float](batchSize)
  endTimes = newSeq[float](batchSize)

  # REFACTOR: this function requires additional context to be available,
  # making it harder to move out of body. It mutates some of the state as
  # well, so one possible solution would be to make the surrounding state
  # of the `runTest` function *explicit* -- create a private
  # `RunTestContext` ref object and then create a `initOnTestRunStartCb`
  # which takes a context and returns a closure.
  #
  # NOTE: This approach might also address the [[mutation]] question above,
  # although as usual, lack of proper `const`-ness for fields will make it
  # harder at places. And this function is unlikely to work under `func` so
  # ...
  proc onTestRunStart(id: int) =
    if verbose:
      msg Undefined: "executing: " & testCmds[id]

    startTimes[id] = epochTime()
    # reset
    exitCodes[id] = 0
    outputs[id] = ""
    endTimes[id] = 0.0

  proc onTestProcess(id: int, p: Process) =
    let testInput = cmdIdToInput[id]
    if testInput.isSome:
      let instream = inputStream(p)
      instream.write(testInput.get())
      close instream
    else:
      discard

  proc onTestRunComplete(id: int, p: Process) =
    if verbose:
      msg Undefined: "finished execution of '$#' with code $#" % [
        testCmds[id],
        $p.peekExitCode()
      ]

    endTimes[id] = epochTime()
    exitCodes[id] = p.peekExitCode()
    let outp = p.outputStream
    outputs[id] = outp.readAll
    outp.close()

  for actionId, action in testActions:
    let
      runId = action.runId
      testRun = testRuns[runId]
      testId = testRun.testId
      spec = execState.testSpecs[testId]
      testFile = execState.testFiles[testId]
      matrixOptions =
        if testRun.matrixEntry == noMatrixEntry:
          ""
        else:
          spec.matrix[testRun.matrixEntry]
      target = testRun.target
      nimcache = nimcacheDir(testFile.file, matrixOptions & testArgs, target)
      compileCmd = prepareTestCompileCmd(
        cmdTemplate = spec.getCmd,
        filename = testFile.file,
        options = testArgs,
        nimcache = nimcache,
        target = target,
        extraOptions = matrixOptions,
        outfile = testRun.exeFile(testFile.file, execState.rootDir))

      testInput =
        case action.kind
        of actionRun: some(spec.input)
        else: options.none[string]()
      catId = testFile.catId


    if catId notin startedCategoryIds:
      let cat = execState.categories[catId].string
      startedCategoryIds.add catId

      msg Progress:
        "progress[all]: $1/$2 starting: cat: $3" % [$catId, $totalCats, cat]

    if dryRun:
      let
        count = testId  # assumes it's in ascending order
        expected = testRuns.len
      case action.kind
      of actionCompile, actionReject:
        msg Undefined:
          "testSpec count: " & $count & " expected: " & $expected
      of actionRun:
        discard # we'll have reported the compile action already

      continue # don't actually run any tests

    # handle skipping of disabled and known issues
    case spec.err:
      of reDisabled, reKnownIssue:
        execState.legacyTestResults.addResult(
          testRun, "", "", spec.err)

        continue
      else:
        discard # keep processing

    case action.kind:
      of actionRun:
        let
          isJsTarget = target == targetJs
          # specFile = execState.testSpecs[testId].file
          exeFile = testRun.exeFile(testFile.file, execState.rootDir)
          exeCmd =
            if isJsTarget:
              findNodeJs()
            elif spec.useValgrind != disabled:
              "valgrind"
            else:
              exeFile.dup(normalizeExe)
          args =
            if isJsTarget:
              @["--unhandled-rejections=strict", exeFile]
            elif spec.useValgrind != disabled:
              let leakCheck =
                if spec.useValgrind == leaking:
                  "yes"
                else:
                  "no"

              @["--error-exitcode=1", "--leak-check=" & leakCheck, exeFile]
            else:
              @[]
          # REFACTOR remove string-based operatoins, use shell helper
          # instead.
          runCmd = map(@[exeCmd] & args, quoteShell).join(" ")

        if testCmds.len == 0: # we've already processed its dependency
          testCmds.add runCmd
          cmdIdToActId.add actionId
          cmdIdToActKind.add action.kind
          cmdIdToInput.add testInput
        else: # dependency is in the current batch, add to the next
          nextTestCmds.add runCmd
          nextCmdIdToActId.add actionId
          nextCmdIdToActKind.add action.kind
          nextCmdIdToInput.add testInput

      of actionCompile, actionReject:
        # add to this batch
        testCmds.add compileCmd
        cmdIdToActId.add actionId
        cmdIdToActKind.add action.kind
        cmdIdToInput.add testInput

    let
      lastActionId = testActions.len - 1
      lastAction = actionId == lastActionId
      currBatchFull = testCmds.len == batchSize
      processOpts = {poStdErrToStdOut, poUsePath}

    if currBatchFull or lastAction:
      # REFACTOR this is an absolute abomination of an implementation, it
      # should be refactored into something that uses 3-4 arguments, not
      # fucking 15.
      #
      # IDEA a possible solution would be to introduce a "TestBatch" object
      # which is populated with the required procedures. In this case part
      # of the function above could also me moved elsewhere and turned into
      # constructor-like procedure.
      #
      # NOTE each batch contains a fair bit of mutable state that gets
      # reset at the end of the `runTestBatch` implementation
      # (`testCmds.setLen(0)`), but I think this can be changed into the
      # dod sequence of test batches that is iterated over. Batch execution
      # results are stored
      runTestBatch(execState,
                   testCmds,
                   processOpts,
                   batchSize,
                   exitCodes,
                   outputs,
                   startTimes, endTimes,
                   onTestRunStart, onTestProcess, onTestRunComplete,
                   cmdIdToActId,
                   cmdIdToActKind,
                   cmdIdToInput,
                   batches)

      var i = 0
      while i < nextCmdIdToActId.len:
        let
          cid = i
          aid = nextCmdIdToActId[cid]
          currAct = execState.actions[aid]
          currRunResult = execState.runActuals[currAct.runId].runResult

        case currRunResult:
          of reSuccess:
            # this item is ok, we can proceed to the next one
            inc i
          else:
            # this isn't ok, remove it and the next item will take its
            # place; so don't increment `i` because the "item comes to us"
            nextTestCmds.delete(cid)
            nextCmdIdToActId.delete(cid)
            nextCmdIdToActKind.delete(cid)
            nextCmdIdToInput.delete(cid)

      # copy next cmds and compileCmd id to run id map to current
      testCmds = nextTestCmds
      cmdIdToActId = nextCmdIdToActId
      cmdIdToActKind = nextCmdIdToActKind
      cmdIdToInput = nextCmdIdToInput

      # clear old next batch information
      nextTestCmds.setLen(0)
      nextCmdIdToActId.setLen(0)
      nextCmdIdToActKind.setLen(0)
      nextCmdIdToInput.setLen(0)

    let nextBatchFull = nextTestCmds.len == batchSize

    if nextBatchFull or lastAction:
      runTestBatch(execState,
                   testCmds,
                   processOpts,
                   batchSize,
                   exitCodes,
                   outputs,
                   startTimes, endTimes,
                   onTestRunStart, onTestProcess, onTestRunComplete,
                   cmdIdToActId,
                   cmdIdToActKind,
                   cmdIdToInput, batches)

  # REFACTOR move into a separate procedure
  var
    earliest = epochTime() # will get minimized in the loop below
    latest: float
    effort: float
    compEffortTotal: float
    compDurLow = epochTime()   # will get minimized in the loop below
    compDurHigh: float

  for rt in execState.runTimes:
    earliest = min(rt.compileStart, earliest)
    latest = max(max(rt.runEnd, rt.compileEnd), latest)
    let compEffort = rt.compileEnd - rt.compileStart
    compEffortTotal = compEffortTotal + compEffort
    effort = effort + compEffort + rt.runEnd - rt.runStart
    compDurLow = min(compDurLow, compEffort)
    compDurHigh = max(compDurHigh, compEffort)

  let
    elapsed = latest - earliest
    avgCompTime = compEffortTotal / float execState.runTimes.len

  # REFACTOR structured output etc; format using standalone procs --
  # default cleanup requirements.
  echo "requested targets: $1, specs: $2, runs: $3, actions: $4, batches: $5, elapsed: $6, effort: $7, compEffort: $8, averageCompTime: $9, compLow: $10, compHigh: $11" % [
      $execState.requestedTargets,
      $execState.testSpecs.len,
      $execState.testRuns.len,
      $execState.actions.len,
      $batches,
      elapsed.formatFloat(ffDecimal, precision = 2).align(5),
      effort.formatFloat(ffDecimal, precision = 2).align(5),
      compEffortTotal.formatFloat(ffDecimal, precision = 2).align(5),
      avgCompTime.formatFloat(ffDecimal, precision = 2).align(5),
      compDurLow.formatFloat(ffDecimal, precision = 2).align(5),
      compDurHigh.formatFloat(ffDecimal, precision = 2).align(5)
    ]

proc main2() =
  backend.open()
  var
    p         = initOptParser() # cli parser
    execState = Execution(
      flags: defaultExecFlags,
      testsDir: "tests" & DirSep,
      rootDir: getCurrentDir(),
      isCompilerRepo: isNimRepoTests() # legacy for `AdditionalCategories`
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

  # Options have all been parsed; we now act on parsed actions
  # Prepare the results container

  # if optPrintResults:
  #   if action == "html": openDefaultBrowser(resultsFile)
  #   else: msg Undefined: $r & r.data
  # azure.finalize()
  # addExitProc azure.finalize

  prepareTestFilesAndSpecs(execState)
  prepareTestRuns(execState)
  prepareTestActions(execState)
  runTests(execState)

  backend.close()
  # var failed = r.total - r.passed - r.skipped
  # if failed != 0:
  #   msg Undefined: "FAILURE! total: " & $r.total & " passed: " & $r.passed & " skipped: " &
  #     $r.skipped & " failed: " & $failed
  #   quit(QuitFailure)


main2()
