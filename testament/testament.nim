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
  options, sequtils, hashes
]
import system/platforms
import backend, htmlgen, specs
from std/sugar import dup
import compiler/utils/nodejs
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
  --tryFailing                 Run tests marked as "known issue" and verify that they're still failing

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
    startTime: float
    spec: TSpec
    inCurrentBatch: bool ## whether the test is part of the current
                         ## batch

  TestRun = object
    test: TTest
    expected: TSpec
    matrixEntry: int
    target: TTarget
    nimcache: string
    startTime: float
    debugInfo: string
    cmd: string       ## the shell command for invoking the compiler for
                      ## the test

  CompilerOutput = object
    ## Describes the output of a compiler invocation.
    nimout: string ## compiler output
    output: string ## test output

    # information related to reported compiler errors (unused if
    # none were reported):
    file, msg: string
    line, column: int

    debugInfo: seq[string] ## debug info to give more context
    success: bool          ## whether compilation was succesful

  TestResult = object
    ## Represents the result of a single test run.
    expected, given: string
    compare: TOutCompare ## result from the structural output comparision,
                         ## or nil
    success: TResultEnum ## the success or failure code

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
    tfkCache = "cache"    ## Create execution cache, not a filter
    tfkCategories = "cat" ## one or more categories
    tfkPCats = "pcat"     ## legacy support for parallel category
    tfkSingle = "r"       ## single test

  TestFilter = object
    case kind: TestFilterKind
    of tfkAll, tfkHtml, tfkCache:
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

  RetryContainer = object
    ## Global object which contains information related to the --retry flag.
    ## See the `retryContainer` global.
    retry: bool
      ## true when --retry flag has been passed
    cats: seq[string]
      ## contains categories with failed tests
    names: seq[(string, string)]
      ## contains pair of failed test name and its target

  TestamentData = ref object
    ## Groups various globals into a single object.
    batchArg: string
    testamentNumBatch: int
    testamentBatch: int

const noMatrixEntry = -1

# ----------------------------------------------------------------------------

# xxx: yay, global state

var
  useColors = true
  backendLogging = true
  simulate = false
  optVerbose = false
  useMegatest = true
  optFailing = false
  retryContainer = RetryContainer(retry: false)
    ## global `RetryContainer` object
  skips: seq[string]
    ## names of test files that are to be disabled

let
  testamentData0 = TestamentData()

# ----------------------------------------------------------------------------

proc trimUnitSep(x: var string) =
  let L = x.len
  if L > 0 and x[^1] == '\31':
    setLen x, L-1

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
    if weight > 0:
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

func makeResult(expected, given: sink string, code: TResultEnum;
                compare: TOutCompare = nil): TestResult {.inline.} =
  ## Convenience routine for constructing a ``TestResult``.
  TestResult(
    expected: expected,
    given: given,
    compare: compare,
    success: code)

proc callNimCompiler(cmd: string): CompilerOutput =
  ## Executes the |NimSkull| compiler via the shell command `cmd` and returns
  ## the invocation's pre-processed output.
  verboseCmd(cmd)
  var p = startProcess(command = cmd,
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

  result.success = false
  let exitCode = p.peekExitCode
  case exitCode
  of 0:
    if foundErrorMsg:
      result.debugInfo.add " compiler exit code was 0 but some Error's were found."
    else:
      result.success = true
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

proc logToConsole(param: ReportParams, given: ptr CompilerOutput = nil) =
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

  template onFailGivenSpecDebugInfo(given: ptr CompilerOutput) =
    if given != nil and given.debugInfo.len > 0:
      msg Undefined: "debugInfo: " & given.debugInfo

  case param.success
  of reSuccess:
    dispNonSkipped(fgGreen, "PASS: ")
  of reDisabled:
    disp:
      if param.inCurrentBatch:
        "SKIP:"
      else:
        "NOTINBATCH:"
  of reKnownIssue:
    disp("KNOWNISSUE:")
  of reJoined:
    disp("JOINED:")
  of reInvalidSpec:
    dispFail(param)
    msg Undefined: param.given
  of reBuildFailed, reNimcCrash, reInstallFailed, reUnexpectedSuccess:
    dispFail(param)
    onFailGivenSpecDebugInfo(given)
    # expected is empty, no reason to print it.
    msg Undefined: param.given
  else:
    dispFail(param)
    onFailGivenSpecDebugInfo(given)

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


proc addResult(r: var TResults, param: ReportParams, given: ptr CompilerOutput) =
  ## Report results to backend, end user (write to command-line) and so on.
  
  # instead of `ptr tspec` we could also use `option[tspec]`; passing
  # `given` makes it easier to get what we need instead of having to
  # pass individual fields, or abusing existing ones like expected vs
  # given. test.name is easier to find than test.name.extractfilename a bit
  # hacky but simple and works with tests/testament/tshould_not_work.nim

  logToBackend(param)

  # TODO DOC what is this
  r.data.addf("$#\t$#\t$#\t$#", param.name, param.expected, param.given,
                                $param.success)

  # Write to console
  logToConsole(param, given)

proc addResult(
    r: var TResults,
    run: TestRun,
    expected, given: sink string,
    success: TResultEnum,
    output: ptr CompilerOutput = nil,
    outCompare: TOutCompare = nil
  ) =
  ## Report final test run to backend, end user (write to command-line) and etc
  let
    targetStr = $run.target
    param = ReportParams(
      duration: epochTime() - run.startTime,
      name: run.getName(),
      origName: run.test.name,
      cat: run.test.cat.string,
      action: run.test.spec.action,
      targetStr: targetStr,
      debugInfo: run.debugInfo,
      outCompare: outCompare,
      success: success,
      knownIssues: if isNil(output): @[] else: run.test.spec.knownIssues,
      inCurrentBatch: run.test.inCurrentBatch,
      expected: expected,
      given: given
    )
  
  addResult(r, param, output)

proc addResult(r: var TResults, test: TTest, reason: TResultEnum) =
  ## Report test failure/skip etc to backend, end user (write to command-line)
  ## and so on.
  const allowedTestStatus = {reInvalidSpec, reDisabled, reKnownIssue, reJoined}
  doAssert reason in allowedTestStatus,
           "Test: $1 with status: $2, should have ran" %
              [test.getName(), $reason]
  let
    given =
      case reason
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
      success: reason,
      inCurrentBatch: test.inCurrentBatch,
      knownIssues: test.spec.knownIssues,
      expected: "",
      given: given,
      outCompare: nil
    )
  
  addResult(r, param, nil)


proc checkForInlineErrors(run: TestRun, given: CompilerOutput): TestResult =
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

        result = makeResult(e, given.nimout, reMsgsDiffer)
        break coverCheck

    result = makeResult("", given.msg, reSuccess)

proc nimoutCheck(expected: TSpec, nimout: string): bool =
  ## Check if expected nimout values match with specified ones. This check
  ## implements comparison of the unstructured data.
  result = true
  if expected.nimoutFull:
    if expected.nimout != nimout:
      result = false
  elif expected.nimout.len > 0 and not greedyOrderedSubsetLines(expected.nimout, nimout):
    result = false

proc sexpCheck(test: TTest, expected: TSpec, nimout: string): TOutCompare =
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
    if line.len > 0:
      r.expectedReports.add TOutReport(node: parseSexp(line))

  var outputParseFailed = false
  for line in splitLines(nimout):
    if line.len > 0:
      let parsedSexp = parseSexp(line)
      if parsedSexp.kind != SList:
        outputParseFailed = true
      r.givenReports.add TOutReport(node: parsedSexp)

  if outputParseFailed:
    r.match = false
    r.failed = true
    return r

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

  if r.sortedMapping[0].cost > 0:
    r.match = false
  elif r.ignoredGiven.len > 0 and expected.nimoutFull:
    r.match = false
  else:
    r.match = true

  return r

proc cmpMsgs(run: TestRun, given: CompilerOutput): TestResult =
  ## Compare all test output messages. This proc does structured or
  ## unstructured comparison comparison and returns the result.
  ##
  ## It is used to for performing 'reject' action checks - it compares
  ## both inline and regular messages - in addition to `nimoutCheck`

  # If structural comparison is requested - drop directly to it and handle
  # the success/failure modes in the branch
  if run.expected.nimoutSexp:
    let outCompare = run.test.sexpCheck(run.expected, given.nimout)
    # Full match of the output results.
    if outCompare.match:
      makeResult(run.expected.msg, given.msg, reSuccess)
    elif outCompare.failed:
      # little janky, but that's just sexp reporting
      makeResult(run.expected.nimout, given.nimout.strip, reMsgsDiffer)
    else:
      makeResult(run.expected.msg, given.msg, reMsgsDiffer, outCompare)
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
    checkForInlineErrors(run, given)

  # Check for `.errormsg` in expected and given spec first
  elif strip(run.expected.msg) notin strip(given.msg):
    makeResult(run.expected.msg, given.msg, reMsgsDiffer)

  # Compare expected and resulted spec messages
  elif not nimoutCheck(run.expected, given.nimout):
    # Report general message mismatch error
    makeResult(run.expected.nimout, given.nimout, reMsgsDiffer)

  # Check for filename mismatches
  elif extractFilename(run.expected.file) != extractFilename(given.file) and
      "internal error:" notin run.expected.msg:
    # Report error for the the error file mismatch
    makeResult(run.expected.file, given.file, reFilesDiffer)

  # Check for produced and given error message locations
  elif run.expected.line != given.line and
       run.expected.line != 0 or
       run.expected.column != given.column and
       run.expected.column != 0:
    # Report error for the location mismatch
    makeResult($run.expected.line & ':' & $run.expected.column,
               $given.line & ':' & $given.column, reLinesDiffer)

  # None of the unstructured checks found mismatches, report a success
  else:
    makeResult(run.expected.msg, given.msg, reSuccess)

proc generatedFile(test: TTest, target: TTarget): string =
  ## Get path to the generated file name from the test.
  if target == targetJS:
    test.name.changeFileExt("js")
  else:
    let (_, name, _) = test.name.splitFile
    let ext = target.ext
    nimcacheDir(test.name, test.options, target) / "@m" & name.changeFileExt(ext)

proc needsCodegenCheck(run: TestRun): bool =
  ## If there is any checks that need to be performed for a generated code
  ## file
  run.target == targetC and
    (run.expected.maxCodeSize > 0 or
     run.expected.ccodeCheck.len > 0)

proc codegenCheck(
    test: TTest,
    target: TTarget,
    spec: TSpec
  ): TestResult =
  ## Check for any codegen mismatches in file generated from `test` run.
  ## Only file that was immediately generated is tested.

  # XXX: only reports last failure, an iterator or early exit might be better?
  result.success = reSuccess
  try:
    let genFile = generatedFile(test, target)
    let contents = readFile(genFile)
    for check in spec.ccodeCheck:
      if check.len > 0 and check[0] == '\\':
        # little hack to get 'match' support:
        if not contents.match(check.peg):
          result.success = reCodegenFailure
      elif contents.find(check.peg) < 0:
        result.success = reCodegenFailure
      result.expected = check
    if spec.maxCodeSize > 0 and contents.len > spec.maxCodeSize:
      result = makeResult("max allowed size: " & $spec.maxCodeSize,
                          "generated code size: " & $contents.len,
                          reCodegenFailure)
  except ValueError:
    result.success = reInvalidPeg
    msg Undefined: getCurrentExceptionMsg()
  except IOError:
    result.success = reCodeNotFound
    msg Undefined: getCurrentExceptionMsg()

proc compilerOutputTests(run: TestRun, given: CompilerOutput): TestResult =
  ## Test output of the compiler for correctness
  if given.success:
    # Check size??? of the generated C code. If fails then add error
    # message.
    if run.needsCodegenCheck:
      result = codegenCheck(run.test, run.target, run.expected)
      if result.success != reSuccess:
        # no need to perform the output tests
        return

    # XXX: try to merge this with ``cmpMsgs`` -- there is considerable overlap
    if run.expected.nimoutSexp:
      # If test requires structural comparison - run it and then check
      # output results for any failures.
      let outCompare = run.test.sexpCheck(run.expected, given.nimout)
      if outCompare.match:
        result = makeResult("", "", reSuccess)
      elif outCompare.failed:
        # there was an error while comparing; don't report the comparison
        # result (`outCompare`)
        result = makeResult(run.expected.nimout, given.nimout.strip,
                            reMsgsDiffer)
      else:
        # structural comparison detected as mismatch
        result = makeResult("", "", reMsgsDiffer, outCompare)

    elif not nimoutCheck(run.expected, given.nimout):
        # Just like unstructured comparison - assign expected/given pair.
        # In that case deep structural comparison is not necessary so we
        # are just pasing strings around, they will be diffed only on
        # reporting.
        result = makeResult(run.expected.nimout, given.nimout.strip,
                            reMsgsDiffer)
    else:
      result = makeResult("", "", reSuccess)

  else:
    result = makeResult("", "$ " & run.cmd & '\n' & given.nimout,
                        reNimcCrash)

proc skip(r: var TResults, test: TTest, reason: TResultEnum) =
  ## Records with the backend that the given test is skipped.
  r.addResult(test, reason)
  inc(r.skipped)
  inc(r.total)

proc checkDisabled(err: TResultEnum): bool =
  ## Check if test has been enabled (not `disabled: true`, and not joined).
  ## Return true if test can be executed.
  err in {reDisabled, reKnownIssue, reJoined}

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
  run.cmd = prepareTestCmd(run.expected.getCmd, run.test.name,
                           run.test.options, run.nimcache, run.target,
                           run.extraOptions)
  
  let given = callNimCompiler(run.cmd)
  var res: TestResult # must be initialized on all paths

  case run.expected.action
  of actionCompile:
    res = compilerOutputTests(run, given)

  of actionReject:
    # Scan compiler output fully for all mismatches and report if any found
    res = cmpMsgs(run, given)

  of actionRun:
    res = compilerOutputTests(run, given)
    if res.success == reSuccess:
      let
        isJsTarget = run.target == targetJS
        ext =
          case run.target
          of targetJS: "js"
          of targetVM: "nimbc"
          else: ExeExt

      var exeFile = changeFileExt(run.test.name, ext)
      
      if not fileExists(exeFile):
        res = makeResult(run.expected.output,
                         "executable not found: " & exeFile, reExeNotFound)
      else:
        let nodejs = if isJsTarget: findNodeJs() else: ""
      
        if isJsTarget and nodejs == "":
          res = makeResult(run.expected.output, "nodejs binary not in PATH",
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
            res = makeResult(
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
            res = makeResult(run.expected.output, bufB, reOutputsDiffer)
          else:
            res = makeResult("", "", reSuccess)

  block: # reject if the test took longer than expected
    # XXX: testing the time-taken here means that we're also measuring the
    #      time testament took for its various output checks
    let
      duration = epochTime() - run.startTime
      timeout = run.expected.timeout

    if timeout > 0.0 and duration > timeout:
      res.success = reTimeout

  if run.expected.knownIssues.len > 0:
    # the test has known issue(s) and is expected to fail
    if res.success == reSuccess:
      # it didn't fail
      res = makeResult("", "", reUnexpectedSuccess)
    else:
      res = makeResult("", "", reKnownIssue)

  case res.success
  of reSuccess:
    inc r.passed
  of reKnownIssue:
    inc r.skipped # counts as a skipped test
  else:
    discard "failure; no counter to increment"

  # output the test result to the backend:
  r.addResult(run, res.expected, res.given, res.success,
              addr given, res.compare)

proc run(r: var TResults, runs: var openArray[TestRun]) =
  ## Executes the given `runs`.
  for run in runs.mitems:
    run.startTime = epochTime()
    inc(r.total)
    # XXX: remove the usage of globals here
    if run.target notin gTargets:
      r.addResult(run, "", "", reDisabled)
      inc(r.skipped)
    elif simulate:
      inc count
      msg Undefined: "testSpec count: " & $count & " expected: " & $run.expected
    else:
      testSpecHelper(r, run)

func nativeTarget(): TTarget {.inline.} =
  targetC

func defaultTargets(category: Category): set[TTarget] =
  const standardTargets = {nativeTarget()}
  case category.string
  of "lang", "lang_callable":
    {targetC, targetJs, targetVM}
  of "arc", "avr", "destructor", "distros", "dll", "gc", "osproc", "parallel",
     "realtimeGC", "threads", "views", "valgrind":
    standardTargets - {targetJs}
  of "compilerapi", "compilerunits", "ic", "lexer", "testament":
    {nativeTarget()}
  of "js":
    {targetJs}
  else:
    standardTargets

proc computeEarly(spec: TSpec, inCurrentBatch: bool): TResultEnum =
  ## Computes the early result for a test based on its `spec` and
  ## the current configuration.
  let cmpString =
    when defined(windows):  spec.file.replace(r"\", r"/")
    else:                   spec.file

  if retryContainer.retry and not retryContainer.names.anyIt(cmpString == it[0]):
    # we're retrying failed test and the test is not in the retry-set
    reDisabled
  elif spec.parseErrors.len > 0:
    reInvalidSpec
  elif targetOS in spec.disabledOs or targetCPU in spec.disabledCpu:
    reDisabled
  elif spec.disable32bit and sizeof(int) == 4:
    reDisabled
  elif skips.anyIt(it in spec.file):
    reDisabled # manually skipped
  elif not inCurrentBatch:
    reDisabled
  elif spec.knownIssues.len > 0:
    reKnownIssue
  else:
    reSuccess

proc produceRuns(r: var TResults, test: TTest, early: TResultEnum,
                  runs: var seq[TestRun]) =
  ## Takes a test description (`test`) and the computed early result and
  ## produces a list of test-run descriptions that are appended to
  ## `runs`. If `early` indicates failure, nothing is added to `runs`, but a
  ## failure (or test skip) is instead recorded with `r`.
  if early == reInvalidSpec:
    # if the specification is invalid, we cannot go on
    r.addResult(test, early)
    inc(r.total)
    return

  if checkDisabled(early):
    r.skip(test, early)
    return

  var expected = test.spec

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
        runs.add run
    else:
      var run = runTemplate
      run.matrixEntry = noMatrixEntry
      runs.add run

proc testSpec(r: var TResults, test: TTest) =
  ## Legacy procedure only used by the custom-category tests.
  let res = computeEarly(test.spec, test.inCurrentBatch)
  var runs: seq[TestRun]
  produceRuns r, test, res, runs
  run(r, runs)

proc testSpecWithNimcache(
    r: var TResults, test: TTest; nimcache: string) {.used.} =
  let res = computeEarly(test.spec, test.inCurrentBatch)
  if checkDisabled(res):
    r.skip(test, res)
    return

  for target in (test.spec.targets * gTargets):
    inc(r.total)
    var testRun = TestRun(
      test: test,
      expected: test.spec,
      matrixEntry: noMatrixEntry,
      target: target,
      nimcache: nimcache
    )
    testSpecHelper(r, testRun)

proc isCurrentBatch(data: TestamentData; filename: string): bool =
  if data.testamentNumBatch != 0:
    hash(filename) mod data.testamentNumBatch == data.testamentBatch
  else:
    true

proc initTest(test, options: string; cat: Category, spec: TSpec): TTest =
  ## make a test with the given spec, meant to be used internally as a
  ## constructor mostly
  result.cat = cat
  result.name = test
  result.options = options
  result.spec = spec
  result.startTime = epochTime()
  result.inCurrentBatch = isCurrentBatch(testamentData0, result.spec.file) or
                          result.spec.unbatchable

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
        doAssert testamentData0.testamentBatch >= 0 and
                  testamentData0.testamentBatch < testamentData0.testamentNumBatch
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
    of "tryfailing": execState.flags.incl runKnownIssues
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
  of "cache":
    # generate html
    execState.filter = TestFilter(kind: tfkCache)
  else:
    return parseQuitWithUsage

  execState.userTestOptions = p.cmdLineRest

proc main() =
  ## Define CLI Options/Args Parsing loops
  var
    execState = Execution(flags: {outputColour, logBackend})
    p = initOptParser(getExecArgs())

  ## Main procedure
  backend.open()
  ## Cli options and misc
  var
    optPrintResults = false
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
    targetsStr = execState.targetsStr
    runKnownIssues = runKnownIssues in execState.flags

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
    var
      cats: seq[string]
      cmds: seq[string]
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
      if runKnownIssues:
        myself &= " " & quoteShell("--tryfailing")
      if targetsStr.len > 0:
        myself &= " " & quoteShell("--targets:" & targetsStr)
      myself &= " " & quoteShell("--nim:" & compilerPrefix)
      if testamentData0.batchArg.len > 0:
        myself &= " --batch:" & testamentData0.batchArg
      if skipFrom.len > 0:
        myself &= " " & quoteShell("--skipFrom:" & skipFrom)
      if not backendLogging:
        myself &= " " & quoteShell("--backendLogging:off")

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
    if useMegatest:
      cats.add MegaTestCat
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
        processCategory(r, Category(cati), gTargets, options, testsDir,
                        runJoinableTests = false, runKnownIssues)
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
  of "cache":
    # Create cached result directory from stored files
    backend.cacheResults()
  of "c", "cat", "category": # Run all tests of a certain category
    skips = loadSkipFrom(skipFrom)
    var cat = Category(p.key)
    processCategory(r, cat, gTargets, options, testsDir,
                    runJoinableTests = true, runKnownIssues)
  of "pcat": # Run cat in parallel
    # Run all tests of a certain category in parallel; does not include joinable
    # tests which are covered in the 'megatest' category.
    skips = loadSkipFrom(skipFrom)
    isMainProcess = false
    var cat = Category(p.key)
    p.next
    processCategory(r, cat, gTargets, options, testsDir,
                    runJoinableTests = false, runKnownIssues)
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
