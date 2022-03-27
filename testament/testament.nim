#
#
#            Testament
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Testament runs tests for the compiler.

import std/[
  strutils, pegs, os, osproc, streams, json, parseopt, browsers,
  terminal, algorithm, times, md5, intsets, macros, tables,
  options
]
import backend, htmlgen, specs
from std/sugar import dup
import utils/nodejs
import lib/stdtest/testutils
from lib/stdtest/specialpaths import splitTestFile
import experimental/[sexp, sexp_diff, colortext, colordiff]

const
  failString* = "FAIL: " # ensures all failures can be searched with 1 keyword in CI logs
  testsDir = "tests" & DirSep
  resultsFile = "testresults.html"
  Usage = """Usage:
  testament [options] command [arguments]

Command:
  all                         run all tests
  c|cat|category <category>   run all the tests of a certain category
  r|run <test>                run single test file
  html                        generate $1 from the database
Arguments:
  arguments are passed to the compiler
Options:
  --retry                   runs tests that failed the last run
  --print                   print results to the console
  --verbose                 print commands (compiling and running tests)
  --simulate                see what tests would be run but don't run them (for debugging)
  --failing                 only show failing/ignored tests
  --targets:"c cpp js objc" run tests for specified targets (default: all)
  --nim:path                use a particular nim executable (default: $$PATH/nim)
  --directory:dir           Change to directory dir before reading the tests or doing anything else.
  --colors:on|off           Turn messages coloring on|off.
  --backendLogging:on|off   Disable or enable backend logging. By default turned on.
  --megatest:on|off         Enable or disable megatest. Default is on.
  --skipFrom:file           Read tests to skip from `file` - one test per line, # comments ignored

<<<<<<< HEAD
=======
On Azure Pipelines, testament will also publish test results via Azure Pipelines' Test Management API
provided that System.AccessToken is made available via the environment variable SYSTEM_ACCESSTOKEN.

>>>>>>> 81debe236 (clean-up testament file)
Experimental: using environment variable `NIM_TESTAMENT_REMOTE_NETWORKING=1` enables
tests with remote networking (as in CI).
""" % resultsFile

type
  Category = distinct string

  TResults = object
    total, passed, failedButAllowed, skipped: int
      ## xxx rename passed to passedOrAllowedFailure
    data: string

  TTest = object
    name: string
    cat: Category
    options: string
    testArgs: seq[string]
<<<<<<< HEAD
    startTime: float
    spec: TSpec

  TestRun = object
    test: TTest
    expected: TSpec
    matrixEntry: int
    target: TTarget
    nimcache: string
    startTime: float
    debugInfo: string

  Execution = object
    ## state object to data relevant for a testament run
    userTestOptions: string ## options passed to tests by the user
    flags: ExecutionFlags   ## various options set by the user
    skipsFile: string       ## test files to skip loaded from `--skipFrom`
    targetsStr: string      ## targets as specified by the user
    filter: TestFilter      ## fitler used to assemble the test suite
  
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
    tfkCategories = "cat" ## one or more categories
    tfkPCats = "pcat"     ## legacy support for parallel category
    tfkSingle = "r"       ## single test

  TestFilter = object
    case kind: TestFilterKind
    of tfkAll, tfkHtml:
      discard
    of tfkCategories, tfkPCats:
      # xxx: currently multiple categories are unsupported
      cats: Categories
    of tfkSingle:
      test: string

  Categories = seq[Category]

  ParseCliResult = enum
    parseSuccess       ## successfully parsed cli params
    parseQuitWithUsage ## parsing failed, quit with usage message

const noMatrixEntry = -1

=======
    spec: TSpec
    startTime: float
    debugInfo: string

>>>>>>> 81debe236 (clean-up testament file)
# ----------------------------------------------------------------------------

# xxx: yay, global state

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

type
  TOutReport = object
    inline: Option[InlineError]
    node: SexpNode
    file: string

  TOutCompare = ref object
    ## Result of comparing two data outputs for a given spec
    match: bool
    expectedReports: seq[TOutReport]
    givenReports: seq[TOutReport]
    sortedMapping: seq[tuple[pair: (int, int), cost: int]]
    diffMap: Table[(int, int), seq[SexpMismatch]]
    ignoredExpected: seq[int]
    ignoredGiven: seq[int]
    cantIgnoreGiven: bool

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
  var
    conf = diffFormatter()

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

proc prepareTestCmd(cmdTemplate, filename, options, nimcache: string,
                     target: TTarget, extraOptions = ""): string =
  var options = target.defaultOptions & ' ' & options
  if nimcache.len > 0: options.add(" --nimCache:$#" % nimcache.quoteShell)
  options.add ' ' & extraOptions
  # we avoid using `parseCmdLine` which is buggy, refs bug #14343
  result = cmdTemplate % ["target", target.cmd,
                      "options", options, "file", filename.quoteShell,
                      "filedir", filename.getFileDir(), "nim", compilerPrefix]

proc callNimCompiler(cmdTemplate, filename, options, nimcache: string,
                     target: TTarget, extraOptions = ""): TSpec =
  ## Execute nim compiler with given `filename`, `options` and `nimcache`.
  ## Compile to target specified in the `target` and return compilation
  ## results as a new `TSpec` value. Resulting spec contains `.nimout` set
  ## from the compiler run results as well as known inline messages (output
  ## is immedately scanned for results).
  result.cmd = prepareTestCmd(cmdTemplate, filename, options, nimcache, target,
                          extraOptions)
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
  result.failedButAllowed = 0
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
Tests passed or allowed to fail: $2 / $1 <br />
Tests failed and allowed to fail: $3 / $1 <br />
Tests skipped: $4 / $1 <br />
""" % [$x.total, $x.passed, $x.failedButAllowed, $x.skipped]

proc getName(test: TTest): string =
  result = test.name.replace(DirSep, '/')
  if test.options.len > 0:
    result.add ' ' & test.options

proc getName(run: TestRun): string =
  result = run.test.name.replace(DirSep, '/')
  result.add ' ' & $run.target
  if run.test.options.len > 0:
    result.add ' ' & run.test.options

type
  ReportParams = object
    ## Contains additional data about report execution state.
    duration: float
    name: string
    origName: string
    cat: string
    action: TTestAction
    targetStr: string
    debugInfo: string
    outCompare: TOutCompare
    success: TResultEnum
    inCurrentBatch: bool
    expected, given: string

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
    backend.writeTestResult(name = param.name,
                            category = param.cat,
                            target = param.targetStr,
                            action = $param.action,
                            result = $param.success,
                            expected = param.expected,
                            given = param.given)


proc addResult(r: var TResults, param: ReportParams, givenSpec: ptr TSpec) =
  ## Report results to backend, end user (write to command-line) and so on.
  
  # instead of `ptr tspec` we could also use `option[tspec]`; passing
  # `givenspec` makes it easier to get what we need instead of having to
  # pass individual fields, or abusing existing ones like expected vs
  # given. test.name is easier to find than test.name.extractfilename a bit
  # hacky but simple and works with tests/testament/tshould_not_work.nim

  logToBackend(param)

  # TODO DOC what is this
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
  ## Report final test run to backend, end user (write to command-line) and etc
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
      inCurrentBatch: run.test.spec.inCurrentBatch,
      expected: expected,
      given: given
    )
  
  addResult(r, param, givenSpec)

proc addResult(r: var TResults, test: TTest) =
  ## Report test failure/skip etc to backend, end user (write to command-line)
  ## and so on.
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
      expected: "",
      given: given,
      outCompare: nil
    )
  
  addResult(r, param, nil)


proc checkForInlineErrors(r: var TResults, run: TestRun, given: TSpec) =
  ## Check for inline error annotations in the nimout results, comparing
  ## them with the output of the compiler.

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
  elif expected.nimout.len > 0 and not greedyOrderedSubsetLines(expected.nimout, given.nimout):
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
    r.expectedReports.add TOutReport(inline: some exp, node: parsed, file: expected.file)

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

  # If structural comparison is requested - drop directly to it and handle
  # the success/failure modes in the branch
  if run.expected.nimoutSexp:
    echo "executing structural comparison"
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
  if target == targetJS:
    test.name.changeFileExt("js")
  else:
    let (_, name, _) = test.name.splitFile
    let ext = target.ext
    nimcacheDir(test.name, test.options, target) / "@m" & name.changeFileExt(ext)

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
  try:
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
    given.err = reInvalidPeg
    msg Undefined: getCurrentExceptionMsg()
  except IOError:
    given.err = reCodeNotFound
    msg Undefined: getCurrentExceptionMsg()

proc compilerOutputTests(run: var TestRun, given: var TSpec; r: var TResults) =
  ## Test output of the compiler for correctness
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

proc testSpecHelper(r: var TResults, run: var TestRun) =
  run.startTime = epochTime()
  run.test.startTime = run.startTime # xxx: set the same for legacy reasons

  template callNimCompilerImpl(): untyped =
    # xxx this used to also pass: `--stdout --hint:Path:off`, but was done
    # inconsistently with other branches
    callNimCompiler(
      run.expected.getCmd, run.test.name, run.test.options, run.nimcache, run.target, run.extraOptions)
  
  case run.expected.action
  of actionCompile:
    var given = callNimCompilerImpl()
    compilerOutputTests(run, given, r)

  of actionReject:
    let given = callNimCompilerImpl()
    # Scan compiler output fully for all mismatches and report if any found
    cmpMsgs(r, run, given)

  of actionRun:
    var given = callNimCompilerImpl()
    if given.err != reSuccess:
      r.addResult(
        run, "", "$ " & given.cmd & '\n' & given.nimout,
        given.err, givenSpec = given.addr)
    else:
      let isJsTarget = run.target == targetJS
      var exeFile = changeFileExt(run.test.name, if isJsTarget: "js" else: ExeExt)
      
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

proc targetHelper(r: var TResults, run: var TestRun) =
  inc(r.total)
  if run.target notin gTargets:
    r.addResult(run, "", "", reDisabled)
    inc(r.skipped)
  elif simulate:
    inc count
    msg Undefined: "testSpec count: " & $count & " expected: " & $run.expected
  else:
    testSpecHelper(r, run)

func nativeTarget(): TTarget =
  if getEnv("NIM_COMPILE_TO_CPP", "false") == "true":
    targetCpp
  else:
    targetC

func defaultTargets(category: Category): set[TTarget] =
  const standardTargets = {nativeTarget()}
  case category.string
  of "arc", "avr", "destructor", "distros", "dll", "gc", "osproc", "parallel",
     "realtimeGC", "threads", "views", "valgrind":
    standardTargets - {targetJs}
  of "compilerapi", "compilerunits", "ic", "navigator", "lexer", "testament":
    {nativeTarget()}
  of "js":
    {targetJs}
  of "cpp":
    {targetCpp}
  else:
    standardTargets

proc testSpec(r: var TResults, test: TTest) =
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
    let runTemplate = TestRun(
        test: test,
        expected: test.spec,
        matrixEntry: noMatrixEntry,
        target: target,
        nimcache: nimcacheDir(test.name, test.options, target)
      )

    if test.spec.matrix.len > 0:
      for entryId, _ in test.spec.matrix.pairs:
        var run = runTemplate
        run.matrixEntry = entryId
        targetHelper(r, run)
    else:
      var run = runTemplate
      run.matrixEntry = noMatrixEntry
      targetHelper(r, run)

proc testSpecWithNimcache(r: var TResults, test: TTest; nimcache: string) {.used.} =
  if not checkDisabled(r, test): return
  for target in test.spec.targets:
    inc(r.total)
    var testRun = TestRun(
      test: test,
      expected: test.spec,
      matrixEntry: noMatrixEntry,
      target: target,
      nimcache: nimcache
    )
    testSpecHelper(r, testRun)

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
  "LockFreeHash.nim",
  "tableimpl.nim",
  "setimpl.nim",
  "hashcommon.nim",

  # Requires compiling with '--threads:on`
  "sharedlist.nim",
  "sharedtables.nim",

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
    # xxx: consider removing pcat concept or make parallel the default
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
  else:
    return parseQuitWithUsage

  execState.userTestOptions = p.cmdLineRest

proc main() =
  ## Define CLI Options/Args Parsing loops
  var
    execState = Execution(flags: {outputColour})
    p = initOptParser()

  ## Main procedure
  backend.open()
  ## Cli options and misc
  var
    optPrintResults = false
    targetsStr      = ""
    isMainProcess   = true
    skipFrom        = ""
  
  ## First parse options
  case parseOpts(execState, p)
  of parseQuitWithUsage: quit Usage
  of parseSuccess: discard

  # parseOpts will complete with next option loaded
  # Next option should be cmdarg

  case parseArgs(execState, p)
  of parseQuitWithUsage: quit Usage
  of parseSuccess: discard
  
  let
    action = $execState.filter.kind
    options = execState.userTestOptions

  optPrintResults = outputResults in execState.flags
  useColors = outputColour in execState.flags
  backendLogging = logBackend in execState.flags
  simulate = dryRun in execState.flags
  skipFrom = execState.skipsFile
  optVerbose = outputVerbose in execState.flags
  optFailing = outputFailureOnly in execState.flags
  retryContainer.retry = rerunFailed in execState.flags

  if rerunFailed in execState.flags:
    (retryContainer.cats, retryContainer.names) = backend.getRetries()

  ## Options have all been parsed; we now act on parsed actions
  # Prepare the results container
  var r = initResults()

  case action
  of "all": # Run all tests
    var cats: seq[string]
    var cmds: seq[string]
    ## def qol procedure
    proc progressStatus(idx: int) =
      msg Progress:
        "progress[all]: $1/$2 starting: cat: $3" % [$idx, $cats.len, cats[idx]]
    # Prepare myself
    var myself = quoteShell(getAppFilename())
    block prepare_myself:
      if optFailing:
        myself &= " " & quoteShell("--failing")
      if retryContainer.retry:
        myself &= " " & quoteShell("--retry")
      if targetsStr.len > 0:
        myself &= " " & quoteShell("--targets:" & targetsStr)
      myself &= " " & quoteShell("--nim:" & compilerPrefix)
      if testamentData0.batchArg.len > 0:
        myself &= " --batch:" & testamentData0.batchArg
      if skipFrom.len > 0:
        myself &= " " & quoteShell("--skipFrom:" & skipFrom)

    # Traverse the test directory
    for kind, dir in walkDir(testsDir):
      assert testsDir.startsWith(testsDir)
      # The category name is extracted from the directory
      # eg: 'tests/compiler' -> 'compiler'
      let cat = dir[testsDir.len .. ^1]
      if retryContainer.retry and cat notin retryContainer.cats:
        continue
      if kind == pcDir and cat notin ["testdata", "nimcache"]:
        cats.add cat
    cats.add AdditionalCategories
    # User may pass an option to skip the megatest category, default is useMegaTest
    if useMegatest: cats.add MegaTestCat
    # We now prepare the command line arguments for our child processes

    let rest = if options.len > 0: " " & options else: ""
    for cat in cats:
      # Remember that if we are performing the megatest category, then
      # all joinable tests will be covered in that, so we use the parallel cat
      # action
      let runtype = if useMegatest: " pcat " else: " cat "
      cmds.add(myself & runtype & quoteShell(cat) & rest)

    if simulate: # 'see what tests would be run but don't run them (for debugging)'
      skips = loadSkipFrom(skipFrom)
      for i, cati in cats:
        progressStatus(i)
        processCategory(r, Category(cati), options, testsDir, runJoinableTests = false)
    else:
      let processOpts =
        if optFailing and not optVerbose:
          {poStdErrToStdOut, poUsePath, poParentStreams}
        else:
          {poEchoCmd, poStdErrToStdOut, poUsePath, poParentStreams}
      let qval = osproc.execProcesses(cmds, processOpts, beforeRunEvent = progressStatus)
      if backendLogging:
        backend.cacheResults()
      quit qval

  of "c", "cat", "category": # Run all tests of a certain category
    skips = loadSkipFrom(skipFrom)
    var cat = Category(p.key)
    processCategory(r, cat, options, testsDir, runJoinableTests = true)
  of "pcat": # Run cat in parallel
    # Run all tests of a certain category in parallel; does not include joinable
    # tests which are covered in the 'megatest' category.
    skips = loadSkipFrom(skipFrom)
    isMainProcess = false
    var cat = Category(p.key)
    p.next
    processCategory(r, cat, options, testsDir, runJoinableTests = false)
  of "r", "run": # Run single test file
    let (cat, path) = splitTestFile(p.key)
    processSingleTest(r, cat.Category, options, path)
  of "html": # Generate html from the database
    generateHtml(resultsFile, optFailing)
  else:
    # Invalid action
    quit Usage

  if optPrintResults:
    if action == "html": openDefaultBrowser(resultsFile)
    else: msg Undefined: $r & r.data
  backend.close()
  var failed = r.total - r.passed - r.skipped
  if failed != 0:
    msg Undefined: "FAILURE! total: " & $r.total & " passed: " & $r.passed & " skipped: " &
      $r.skipped & " failed: " & $failed
    quit(QuitFailure)
  if isMainProcess:
    msg Undefined: "Used " & compilerPrefix & " to run the tests. Use --nim to override."

if paramCount() == 0:
  quit Usage
main()
