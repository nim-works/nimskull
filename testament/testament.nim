#
#
#            Nim Testament
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This program verifies Nim against the testcases.

import std/[
  strutils, pegs, os, osproc, streams, json, exitprocs, parseopt, browsers,
  terminal, algorithm, times, md5, intsets, macros, tables,
  options
]
import backend, azure, htmlgen, specs
from std/sugar import dup
from std/sequtils import map
import utils/nodejs
import lib/stdtest/testutils
from lib/stdtest/specialpaths import splitTestFile
import experimental/[sexp, sexp_diff, colortext, colordiff]

# TODO - things to do before this can be finalized:
# * get the tests running
# * remove legacy cruft inside testament
# * Test name, exe, printable name generation
# * Update usage and remove unsupported features
# * update docs so testament is for the compiler only

const
  failString* = "FAIL: "
    ## ensures all failures can be searched with 1 keyword in CI logs
  knownIssueSuccessString* = "KNOWNISSUE: "
    ## ensures all now succeeding known issues can be searched with 1 keyword
    ## in CI logs
  testsDir = "tests" & DirSep
  resultsFile = "testresults.html"
  Usage = """Usage:
  testament [options] command [arguments]

Command:
  p|pat|pattern <glob>        run all the tests matching the given pattern
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
  --skipFrom:file           Read tests to skip from `file` - one test per line, # comments ignored.
  --includeKnownIssues      runs tests that are marked as known issues

On Azure Pipelines, testament will also publish test results via Azure Pipelines' Test Management API
provided that System.AccessToken is made available via the environment variable SYSTEM_ACCESSTOKEN.

Experimental: using environment variable `NIM_TESTAMENT_REMOTE_NETWORKING=1` enables
tests with remote networking (as in CI).
""" % resultsFile

proc trimUnitSep(x: var string) =
  let L = x.len
  if L > 0 and x[^1] == '\31':
    setLen x, L-1

var
  useColors = true
  backendLogging = true
  simulate = false
  optVerbose = false
  useMegatest = true
  optFailing = false

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

proc verboseCmd(cmd: string) {.inline.} =
  if optVerbose:
    msg Undefined: "executing: " & cmd

proc isNimRepoTests(): bool =
  # this logic could either be specific to cwd, or to some file derived from
  # the input file, eg testament r /pathto/tests/foo/tmain.nim; we choose
  # the former since it's simpler and also works with `testament all`.
  let file = "testament"/"testament.nim.cfg"
  result = file.fileExists

type
  Category = distinct string

  TResults = object
    total, passed, knownIssuesSucceeded, skipped: int
    data: string

  TTest = object
    name: string
    cat: Category
    options: string
    testArgs: seq[string]
    spec: TSpec
    startTime: float
    duration: Option[float]
      ## allows newer code pass duration to legacy code
    debugInfo: string

# ----------------------------------------------------------------------------

let
  pegLineError =
    peg"{[^(]*} '(' {\d+} ', ' {\d+} ') ' ('Error') ':' \s* {.*}"
  pegOtherError = peg"'Error:' \s* {.*}"
  pegOfInterest = pegLineError / pegOtherError

var
  gTargets = {low(TTarget)..high(TTarget)}
  targetsSet = false

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
                     target: TTarget, extraOptions = "", outfile = ""): string =
  var options = target.defaultOptions & ' ' & options
  if nimcache.len > 0: options.add(" --nimCache:$#" % nimcache.quoteShell)
  options.add ' ' & extraOptions
  let outfileOption =
    if outfile == "":
      "" # don't do anything
    else:
      "--out:" & outfile.quoteShell

  # we avoid using `parseCmdLine` which is buggy, refs bug #14343
  result = cmdTemplate % [
              "target", target.cmd,
              "options", options,
              "file", filename.quoteShell,
              "filedir", filename.getFileDir(),
              "nim", compilerPrefix,
              "outfileOption", outfileOption]

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
  result.knownIssuesSucceeded = 0
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
""" % [$x.total, $x.passed, $x.knownIssuesSucceeded, $x.skipped]

proc printableName(test: TTest, target: TTarget, allowFailure: bool): string =
  var name = test.name.replace(DirSep, '/')
  name.add ' ' & $target
  if allowFailure:
    name.add " (allowed to fail) "
  if test.options.len > 0:
    name.add ' ' & test.options

  return name


type
  ReportParams = object
    ## Contains additional data about report execution state.
    name: string
    duration: float
    outCompare: TOutCompare
    expected, given: string
    success: TResultEnum

proc logToConsole(
    test: TTest,
    param: ReportParams,
    givenSpec: ptr TSpec = nil
  ) =
  ## Format test infomation to the console. `test` contains information
  ## about the test itself, `param` contains additional data about test
  ## execution.

  let durationStr = param.duration.formatFloat(
    ffDecimal, precision = 2).align(5)

  template dispNonSkipped(color, outcome) =
    if not optFailing or color == fgRed:
      maybeStyledEcho(
        color, outcome, fgCyan, test.debugInfo, alignLeft(param.name, 60),
        fgBlue, " (", durationStr, " sec)")

  template disp(msg) =
    if not optFailing:
      maybeStyledEcho(
        styleDim, fgYellow, msg & ' ', styleBright, fgCyan, param.name)

  if param.success == reSuccess:
    dispNonSkipped(fgGreen, "PASS: ")

  elif param.success == reDisabled:
    if test.spec.inCurrentBatch:
      disp("SKIP:")

    else:
      disp("NOTINBATCH:")

  elif param.success == reJoined:
    disp("JOINED:")

  else:
    dispNonSkipped(fgRed, failString)
    if test.cat.string.len > 0:
      maybeStyledEcho(
        styleBright, fgCyan, "Test \"", test.name, "\"",
        " in category \"", test.cat.string, "\"")

    else:
      maybeStyledEcho(styleBright, fgCyan, "Test \"", test.name, "\"")

    maybeStyledEcho styleBright, fgRed, "Failure: ", $param.success
    if givenSpec != nil and givenSpec.debugInfo.len > 0:
      msg Undefined: "debugInfo: " & givenSpec.debugInfo

    if param.success in {reBuildFailed, reNimcCrash, reInstallFailed}:
      # expected is empty, no reason to print it.
      msg Undefined: param.given

    else:
      if not isNil(param.outCompare):
        msg Undefined:
          param.outCompare.format().toString(useColors)

      else:
        # REFACTOR error message formatting should be based on the
        # `TestReport` data structure that contains all the necessary
        # inforamtion that is necessary in order to generate error message.
        maybeStyledEcho fgYellow, "Expected:"
        maybeStyledEcho styleBright, param.expected, "\n"
        maybeStyledEcho fgYellow, "Gotten:"
        maybeStyledEcho styleBright, param.given, "\n"
        msg Undefined:
          diffStrings(param.expected, param.given).output


proc logToBackend(
    test: TTest,
    param: ReportParams
  ) =

  let (outcome, msg) =
    case param.success
    of reSuccess:
      ("Passed", "")
    of reDisabled, reJoined:
      ("Skipped", "")
    of reBuildFailed, reNimcCrash, reInstallFailed:
      ("Failed", "Failure: " & $param.success & '\n' & param.given)
    else:
      ("Failed", "Failure: " & $param.success & "\nExpected:\n" &
        param.expected & "\n\n" & "Gotten:\n" & param.given)
  if isAzure:
    azure.addTestResult(
      param.name, test.cat.string, int(param.duration * 1000), msg, param.success)

  else:
    var p = startProcess("appveyor", args = ["AddTest", test.name.replace("\\", "/") & test.options,
                         "-Framework", "nim-testament", "-FileName",
                         test.cat.string,
                         "-Outcome", outcome, "-ErrorMessage", msg,
                         "-Duration", $(param.duration * 1000).int],
                         options = {poStdErrToStdOut, poUsePath, poParentStreams})
    discard waitForExit(p)
    close(p)


proc addResult(
    r: var TResults,
    test: TTest,
    target: TTarget,
    expected, given: string,
    successOrig: TResultEnum,
    allowFailure = false,
    givenSpec: ptr TSpec = nil,
    outCompare: TOutCompare = nil
  ) =
  ## Report final test results to backend, end user (write to command-line)
  ## and so on.
  # instead of `ptr tspec` we could also use `option[tspec]`; passing
  # `givenspec` makes it easier to get what we need instead of having to
  # pass individual fields, or abusing existing ones like expected vs
  # given. test.name is easier to find than test.name.extractfilename a bit
  # hacky but simple and works with tests/testament/tshould_not_work.nim

  # Compute test duration, final success status, prepare formatting variables
  var param: ReportParams

  param.name = test.printableName(target, allowFailure)
  param.duration =
    if test.duration.isSome:
      test.duration.get
    else:
      epochTime() - test.startTime
  param.outCompare = outCompare
  param.expected = expected
  param.given = given
  param.success =
    if test.spec.timeout > 0.0 and param.duration > test.spec.timeout:
      reTimeout
    else:
      successOrig

  if backendLogging:
    backend.writeTestResult(name = param.name,
                            category = test.cat.string,
                            target = $target,
                            action = $test.spec.action,
                            result = $param.success,
                            expected = expected,
                            given = given)

  # TODO DOC what is this
  r.data.addf("$#\t$#\t$#\t$#", param.name, expected, given, $param.success)

  # Write to console
  logToConsole(test, param, givenSpec)

  if backendLogging and (isAppVeyor or isAzure):
    # Write to logger
    logToBackend(test, param)

proc checkForInlineErrors(r: var TResults, expected, given: TSpec, test: TTest, target: TTarget) =
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

      if file == extractFilename test.name:
        # If annotation comes from the target file
        var i = 0
        for x in expected.inlineErrors:
          if x.line == line and (x.col == col or x.col < 0) and
              x.kind == kind and x.msg in msg:
            # And annotaiton has matching line, column and message
            # information, register it as 'covered'
            covered.incl i
          inc i

  block coverCheck:
    for j in 0..high(expected.inlineErrors):
      #  For each output message that was not covered by annotations, add it
      # to the output as 'missing'
      if j notin covered:
        var e: string
        let exp = expected.inlineErrors[j]
        e = test.name
        e.add '('
        e.addInt exp.line
        if exp.col > 0:
          e.add ", "
          e.addInt exp.col
        e.add ") "
        e.add exp.kind
        e.add ": "
        e.add exp.msg

        r.addResult(test, target, e, given.nimout, reMsgsDiffer)
        break coverCheck

    r.addResult(test, target, "", given.msg, reSuccess)
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
  ## Check if expected nimout values match with specified ones. This check
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

proc cmpMsgs(
    r: var TResults, expected, given: TSpec, test: TTest, target: TTarget
  ) =
  ## Compare all test output messages. This proc does structured or
  ## unstructured comparison comparison and immediately reports it's
  ## results.
  ##
  ## It is used to for performing 'reject' action checks - it compares
  ## both inline and regular messages - in addition to `nimoutCheck`

  # If structural comparison is requested - drop directly to it and handle
  # the success/failure modes in the branch
  if expected.nimoutSexp:
    let outCompare = test.sexpCheck(expected, given)
    # Full match of the output results.
    if outCompare.match:
      r.addResult(test, target, expected.msg, given.msg, reSuccess)
      inc(r.passed)

    else:
      # Write out error message.
      r.addResult(
        test, target, expected.msg, given.msg, reMsgsDiffer,
        givenSpec = unsafeAddr given,
        outCompare = outCompare
      )


  # Checking for inline errors.
  elif expected.inlineErrors.len > 0:
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
    checkForInlineErrors(r, expected, given, test, target)

  # Check for `.errormsg` in expected and given spec first
  elif strip(expected.msg) notin strip(given.msg):
    r.addResult(test, target, expected.msg, given.msg, reMsgsDiffer)

  # Compare expected and resulted spec messages
  elif not nimoutCheck(expected, given):
    # Report general message mismatch error
    r.addResult(test, target, expected.nimout, given.nimout, reMsgsDiffer)

  # Check for filename mismatches
  elif extractFilename(expected.file) != extractFilename(given.file) and
      "internal error:" notin expected.msg:
    # Report error for the the error file mismatch
    r.addResult(test, target, expected.file, given.file, reFilesDiffer)

  # Check for produced and given error message locations
  elif expected.line != given.line and
       expected.line != 0 or
       expected.column != given.column and
       expected.column != 0:
    # Report error for the location mismatch
    r.addResult(test, target, $expected.line & ':' & $expected.column,
                      $given.line & ':' & $given.column, reLinesDiffer)

  # None of the unstructured checks found mismatches, reporting thest
  # as passed.
  else:
    r.addResult(test, target, expected.msg, given.msg, reSuccess)
    inc(r.passed)

proc generatedFile(test: TTest, target: TTarget): string =
  ## Get path to the generated file name from the test.
  result =
    case target
    of targetJS:
      test.name.changeFileExt("js")
    else:
      let
        testFile = test.spec.file
        (_, name, _) = testFile.splitFile
        ext = target.ext
      nimcacheDir(testFile, test.options, target) / "@m" & name.changeFileExt(ext)

proc needsCodegenCheck(spec: TSpec): bool =
  ## If there is any checks that need to be performed for a generated code
  ## file
  result = spec.maxCodeSize > 0 or spec.ccodeCheck.len > 0

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

proc compilerOutputTests(test: TTest, target: TTarget, given: var TSpec,
                         expected: TSpec; r: var TResults) =
  ## Test output of the compiler for correctness
  var expectedmsg: string = ""
  var givenmsg: string = ""
  var outCompare: TOutCompare
  if given.err == reSuccess:
    # Check size??? of the generated C code. If fails then add error
    # message.
    if expected.needsCodegenCheck:
      codegenCheck(test, target, expected, expectedmsg, given)
      givenmsg = given.msg

    if expected.nimoutSexp:
      # If test requires structural comparison - run it and then check
      # output results for any failures.
      outCompare = test.sexpCheck(expected, given)
      if not outCompare.match:
        given.err = reMsgsDiffer

    else:
      # Use unstructured data comparison for the expected and given outputs
      if not nimoutCheck(expected, given):
        given.err = reMsgsDiffer

        # Just like unstructured comparison - assign expected/given pair.
        # In that case deep structural comparison is not necessary so we
        # are just pasing strings around, they will be diffed only on
        # reporting.
        expectedmsg = expected.nimout
        givenmsg = given.nimout.strip

  else:
    givenmsg = "$ " & given.cmd & '\n' & given.nimout
  if given.err == reSuccess:
    inc(r.passed)

  # Write out results of the compiler output testing
  r.addResult(
    test, target, expectedmsg, givenmsg, given.err,
    givenSpec = addr given,
    # Supply results of the optional structured comparison.
    outCompare = outCompare
  )

proc getTestSpecTarget(): TTarget =
  if getEnv("NIM_COMPILE_TO_CPP", "false") == "true":
    result = targetCpp
  else:
    result = targetC

proc checkDisabled(r: var TResults, test: TTest): bool =
  ## Check if test has been enabled (not `disabled: true`, and not joined).
  ## Return true if test can be executed.
  if test.spec.err in {reDisabled, reJoined}:
    # targetC is a lie, but parameter is required
    r.addResult(test, targetC, "", "", test.spec.err)
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

proc checkCmdHelper(r: var TResults,
                    test: TTest,
                    target: TTarget,
                    expected: TSpec,
                    given: var TSpec,
                    cmdOut: string,
                    exitCode: int) =
  ## extracted from `testSpecHelper` to allow for reuse in new testament runner

  case given.err
  of reExitcodesDiffer:
    r.addResult(test,
                target,
                "exitcode: " & $expected.exitCode,
                "exitcode: " & $exitCode & "\n\nOutput:\n" & cmdOut,
                reExitcodesDiffer)
  of reOutputsDiffer:
    r.addResult(test, target, expected.output, cmdOut, reOutputsDiffer)
  else:
    compilerOutputTests(test, target, given, expected, r)

proc testSpecHelper(r: var TResults, test: var TTest, expected: TSpec,
                    target: TTarget, nimcache: string, extraOptions = "") =
  test.startTime = epochTime()
  template callNimCompilerImpl(): untyped =
    # xxx this used to also pass: `--stdout --hint:Path:off`, but was done
    # inconsistently with other branches
    callNimCompiler(
      expected.getCmd, test.name, test.options, nimcache, target, extraOptions)
  case expected.action
  of actionCompile:
    var given = callNimCompilerImpl()
    compilerOutputTests(test, target, given, expected, r)
  of actionRun:
    var given = callNimCompilerImpl()
    if given.err != reSuccess:
      r.addResult(
        test, target, "", "$ " & given.cmd & '\n' & given.nimout,
        given.err, givenSpec = given.addr)
    else:
      let isJsTarget = target == targetJS
      var exeFile = changeFileExt(test.name, if isJsTarget: "js" else: ExeExt)
      if not fileExists(exeFile):
        r.addResult(test, target, expected.output,
                    "executable not found: " & exeFile, reExeNotFound)
      else:
        let nodejs = if isJsTarget: findNodeJs() else: ""
        if isJsTarget and nodejs == "":
          r.addResult(
            test, target, expected.output,
            "nodejs binary not in PATH", reExeNotFound)

        else:
          var exeCmd: string
          var args = test.testArgs
          if isJsTarget:
            exeCmd = nodejs
            # see D20210217T215950
            args = @["--unhandled-rejections=strict", exeFile] & args
          else:
            exeCmd = exeFile.dup(normalizeExe)
            if expected.useValgrind != disabled:
              var valgrindOptions = @["--error-exitcode=1"]
              if expected.useValgrind != leaking:
                valgrindOptions.add "--leak-check=yes"
              args = valgrindOptions & exeCmd & args
              exeCmd = "valgrind"

          var (_, buf, exitCode) = execCmdEx2(
            exeCmd, args, input = expected.input)

          # Treat all failure codes from nodejs as 1. Older versions of
          # nodejs used to return other codes, but for us it is sufficient
          # to know that it's not 0.
          if exitCode != 0: exitCode = 1
          let bufB =
            if expected.sortoutput:
              var buf2 = buf
              buf2.stripLineEnd
              var x = splitLines(buf2)
              sort(x, system.cmp)
              join(x, "\n") & '\n'
            else:
              buf
          if exitCode != expected.exitCode:
            given.err = reExitcodesDiffer
            # r.addResult(test, target, "exitcode: " & $expected.exitCode,
            #                   "exitcode: " & $exitCode & "\n\nOutput:\n" &
            #                   bufB, reExitcodesDiffer)
          elif (
            expected.outputCheck == ocEqual and
            not expected.output.equalModuloLastNewline(bufB)
          ) or (
            expected.outputCheck == ocSubstr and
            expected.output notin bufB
          ):
            given.err = reOutputsDiffer
            # r.addResult(test, target, expected.output, bufB, reOutputsDiffer)
          else:
            discard
            # compilerOutputTests(test, target, given, expected, r)
          checkCmdHelper(r, test, target, expected, given, bufB, exitCode)
  of actionReject:
    let given = callNimCompilerImpl()
    # Scan compiler output fully for all mismatches and report if any found
    cmpMsgs(r, expected, given, test, target)

proc targetHelper(r: var TResults, test: TTest, expected: TSpec, extraOptions = "") =
  for target in expected.targets:
    inc(r.total)
    if target notin gTargets:
      r.addResult(test, target, "", "", reDisabled)
      inc(r.skipped)
    elif simulate:
      inc count
      msg Undefined: "testSpec count: " & $count & " expected: " & $expected
    else:
      let nimcache = nimcacheDir(test.name, test.options, target)
      var testClone = test
      testSpecHelper(r, testClone, expected, target, nimcache, extraOptions)

proc testSpec(r: var TResults, test: TTest, targets: set[TTarget] = {}) =
  var expected = test.spec
  if expected.parseErrors.len > 0:
    # targetC is a lie, but a parameter is required
    r.addResult(test, targetC, "", expected.parseErrors, reInvalidSpec)
    inc(r.total)
    return
  if not checkDisabled(r, test): return

  expected.targets.incl targets
  # still no target specified at all
  if expected.targets == {}:
    expected.targets = {getTestSpecTarget()}
  if test.spec.matrix.len > 0:
    for m in test.spec.matrix:
      targetHelper(r, test, expected, m)
  else:
    targetHelper(r, test, expected)

proc testSpecWithNimcache(r: var TResults, test: TTest; nimcache: string) {.used.} =
  if not checkDisabled(r, test): return
  for target in test.spec.targets:
    inc(r.total)
    var testClone = test
    testSpecHelper(r, testClone, test.spec, target, nimcache)

proc makeTest(test, options: string, cat: Category): TTest =
  result.cat = cat
  result.name = test
  result.options = options
  result.spec = parseSpec(addFileExt(test, ".nim"))
  result.startTime = epochTime()

proc makeRawTest(test, options: string, cat: Category): TTest {.used.} =
  result.cat = cat
  result.name = test
  result.options = options
  result.spec = initSpec(addFileExt(test, ".nim"))
  result.spec.action = actionCompile
  result.spec.targets = {getTestSpecTarget()}
  result.startTime = epochTime()

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

type  
  TestId = int         # xxx: make this a distinct
  RunId = int          ## test run's id/index # xxx: make this a distinct
  EntryId = int        ## matrix entry index # xxx: make this a distinct
  ActionId = int       ## a test action's id # xxx: make this a distinct
  CategoryId = int     ## a category's id # xxx: make this a distinct
  TestTarget = TTarget # xxx: renamed because I dislike the TXxx convention

  Categories = seq[Category]

  GlobPattern = string

  TestFilterKind {.pure.} = enum
    tfkAll,        ## all tests
    tfkCategories, ## one or more categories
    tfkGlob,       ## glob file pattern
    tfkSingle      ## single test

  TestFilter = object
    restOfCmdLine: string
    case kind: TestFilterKind
    of tfkAll:
      discard
    of tfkCategories:
      # xxx: currently multiple categories are unsupported
      cats: Categories
    of tfkGlob:
      pattern: GlobPattern
    of tfkSingle:
      test: string

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
    ## track the option flags that got set for an execution

  RetryInfo = object
    test: TestId       ## which test failed
    target: TestTarget ## the specific target

  RetryList = OrderedTable[TestId, RetryInfo]
      ## record failures in here so the user can choose to retry them

  DebugInfo = OrderedTable[RunId, string]
  # Refactor - debug info should be per action instead of per run

  TestTargets = set[TestTarget]
  
  # Category is in specs.nim

  TestFile = object
    file: string
    catId: CategoryId

  # TSpec is in specs.nim
  # TSpec is the the spec in a single test file, but we run (`TestRun`) a test
  # for every target and matrix entry, which itself is a number of actions and
  # checks.

  TestRun = object
    testId: TestId           ## test id for which this belongs
    target: TestTarget       ## which target to run for
    matrixEntry: EntryId     ## which item from the matrix was used

  # xxx: add 'check' to remove `cmd: "nim check"...` from tests
  # TestActionKind = enum
  #   testActionSkip           ## skip this test; check the spec for why
  #   testActionReject,        ## reject the compilation
  #   testActionCompile,       ## compile some source
  #   testActionRun            ## run the compiled program

  TestAction = object
    runId: RunId
    case kind: TTestAction      # xxx: might not need `partOfRun` at all
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

  TestOptions = OrderedTable[TestId, TestOptionData]
    ## for legacy reasons (eg: `dllTests`) we need to be able to set per test
    ## options, ideally this would be done with `matrix`, but it's not
    ## sophisticated enough to support spare configuration like this needs

  Execution = object
    # user and execution inputs
    filter: TestFilter       ## filter that was configured
    flags: ExecutionFlags    ## various options set by the user
    skipsFile: string        ## test files to skip loaded from `--skipFrom`
    targets: TestTargets     ## specified targets or `noTargetsSpecified`
    workingDir: string       ## working directory to begin execution in
    nodeJs: string           ## path to nodejs binary
    nimSpecified: bool       ## whether the user specified the nim
    testArgs: string         ## arguments passed to tests by the user
    isCompilerRepo: bool     ## whether this is the compiler repository, used
                             ## to legacy to handle `AdditionalCategories`

    # environment input / setup
    compilerPath: string     ## compiler command to use
    testsDir: string         ## where to look for tests

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

  ParseCliResult = enum
    parseSuccess       ## successfully parsed cli params
    parseQuitWithUsage ## parsing failed, quit with usage message

include categories

proc loadSkipFrom(name: string): seq[string] =
  if name.len == 0: return
  # One skip per line, comments start with #
  # used by `nlvm` (at least)
  for line in lines(name):
    let sline = line.strip()
    if sline.len > 0 and not sline.startsWith('#'):
      result.add sline

proc main() =
  ## Define CLI Options/Args Parsing loops
  template parseOptions(p: var OptParser): untyped =
    while p.kind in {cmdLongOption, cmdShortOption}:
      # read options agnostic of casing
      case p.key.normalize
      of "print": optPrintResults = true
      of "verbose": optVerbose = true
      of "failing": optFailing = true
      of "targets":
        targetsStr = p.val
        gTargets = parseTargets(targetsStr)
        targetsSet = true
      of "nim":
        compilerPrefix = addFileExt(p.val.absolutePath, ExeExt)
      of "directory":
        setCurrentDir(p.val)
      of "colors":
        case p.val:
        of "on":
          useColors = true
        of "off":
          useColors = false
        else:
          quit Usage
      of "withMegaTestRun":
        # if megatest was included, then all parallel invocations of testatment
        # need to know, this way we can tell if pcat should run joinable tests
        # or not
        testamentData0.withMegaTestRun = true
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
        simulate = true
      of "megatest":
        case p.val:
        of "on":
          useMegatest = true
          testamentData0.withMegaTestRun = true
        of "off":
          useMegatest = false
        else:
          quit Usage
      of "backendlogging":
        case p.val:
        of "on":
          backendLogging = true
        of "off":
          backendLogging = false
        else:
          quit Usage
      of "skipfrom":
        skipFrom = p.val
      of "includeKnownIssues":
        testamentData0.includeKnownIssues =
          case p.val
          of "on":  true
          of "off": false
          else: quit Usage
      of "retry":
        retryContainer.retry = true
        (retryContainer.cats, retryContainer.names) = backend.getRetries()
      else:
        quit Usage
      p.next()
  template parseArgs(p: var OptParser): untyped =
    if p.kind != cmdArgument:
      quit Usage
    action = p.key.normalize
    p.next()
  ## Main procedure
  azure.init()
  backend.open()
  ## Cli options and misc
  var
    optPrintResults = false
    targetsStr      = ""
    isMainProcess   = true
    skipFrom        = ""
    p               = initOptParser() # Init cli parser
  p.next()
  ## First parse options
  parseOptions(p)
  # template will complete with next option loaded
  
  # Next option should be cmdarg
  var action: string
  parseArgs(p)
  # template will complete with next option loaded

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

    let rest = if p.cmdLineRest.len > 0: " " & p.cmdLineRest else: ""
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
    if isNimRepoTests():
      cats.add AdditionalCategories
    # User may pass an option to skip the megatest category, default is useMegaTest
    if useMegatest:
      cats.add MegaTestCat
    # We now prepare the command line arguments for our child processes

    for cat in cats:
      # Remember that if we are performing the megatest category, then
      # all joinable tests will be covered in that, so we use the parallel cat
      # action
      let runtype = if useMegatest: " --withMegaTestRun pcat " else: " cat "
      cmds.add(myself & runtype & quoteShell(cat) & rest)

    if simulate: # 'see what tests would be run but don't run them (for debugging)'
      skips = loadSkipFrom(skipFrom)
      for i, cati in cats:
        progressStatus(i)
        processCategory(r, Category(cati), p.cmdLineRest, testsDir, runJoinableTests = false)
    else:
      addExitProc azure.finalize
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
    let cat = Category(p.key)
    processCategory(r, cat, p.cmdLineRest, testsDir, runJoinableTests = true)
  of "pcat": # Run cat in parallel
    # Run all tests of a certain category in parallel; does not include joinable
    # tests which are covered in the 'megatest' category.
    skips = loadSkipFrom(skipFrom)
    isMainProcess = false
    let
      cat = Category(p.key)
      withMegaTestRun = testamentData0.withMegaTestRun
    p.next
    processCategory(r, cat, p.cmdLineRest, testsDir,
                    runJoinableTests = not withMegaTestRun)
  of "p", "pat", "pattern": # Run all tests matching the given pattern  
    skips = loadSkipFrom(skipFrom)
    let pattern = p.key
    p.next
    processPattern(r, pattern, p.cmdLineRest, simulate)
  of "r", "run": # Run single test file
    let (cat, path) = splitTestFile(p.key)
    processSingleTest(r, cat.Category, p.cmdLineRest, path, gTargets, targetsSet)
  of "html": # Generate html from the database
    generateHtml(resultsFile, optFailing)
  else:
    # Invalid action
    quit Usage

  if optPrintResults:
    if action == "html": openDefaultBrowser(resultsFile)
    else: msg Undefined: $r & r.data
  azure.finalize()
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
# main()

const
  testResultsDir = "testresults"
  cacheResultsDir = testResultsDir / "cacheresults"
  noTargetsSpecified: TestTargets = {}
  defaultExecFlags = {outputColour}
  defaultBatchSize = 10
  noMatrixEntry: EntryId = -1
  defaultCatId: CategoryId = 0

proc parseOpts(execState: var Execution, p: var OptParser): ParseCliResult =
  # xxx: create a narrower type to disallow mutation elsewhere
  result = parseSuccess
  p.next()
  while p.kind in {cmdLongOption, cmdShortOption}:
    # read options agnostic of casing
    case p.key.normalize
    of "print": execState.flags.incl outputResults
    of "verbose": execState.flags.incl outputVerbose
    of "failing": execState.flags.incl outputFailureOnly
    of "targets":
      var targetStr = p.val
      execState.targets = parseTargets(targetStr)
    of "nim", "compiler":
      execState.compilerPath = addFileExt(p.val.absolutePath, ExeExt)
      # xxx: legacy, remove once `prepareTestCmd`, etc are ported
      compilerPrefix = execState.compilerPath
    of "directory":
      execState.workingDir = p.val
    of "colors", "colours":
      case p.val:
      of "on": execState.flags.incl outputColour
      of "off": execState.flags.excl outputColour
      else: return parseQuitWithUsage
    of "simulate":
      execState.flags.incl dryRun
    of "backendlogging":
      case p.val:
      of "on": execState.flags.incl logBackend
      of "off": execState.flags.excl logBackend
      else: return parseQuitWithUsage
    of "includeknownissues":
      case p.val:
      of "on": execState.flags.incl runKnownIssues
      of "off": execState.flags.excl runKnownIssues
      else: return parseQuitWithUsage
    of "retry":
      execState.flags.incl rerunFailed
    of "skipFrom":
      execState.skipsFile = p.val
    else:
      return parseQuitWithUsage
    p.next()

proc parseArg(execState: var Execution, p: var OptParser): ParseCliResult =
  # xxx: create types to avoid accidental mutation
  # next part should be the the filter action, eg: cat, r, etc...
  result = parseSuccess

  var filterAction: string
  if p.kind != cmdArgument:
    quit Usage
  filterAction = p.key.normalize
  p.next()

  case filterAction
  of "all":
    # filter nothing, run everything
    execState.filter = TestFilter(kind: tfkAll, restOfCmdLine: p.cmdLineRest)
  of "c", "cat", "category":
    # only specified category
    execState.filter = TestFilter(
        kind: tfkCategories,
        cats: @[Category(p.key)],
        restOfCmdLine: p.cmdLineRest
      )
  of "pcat":
    # run category's tests in parallel
    # xxx: consider removing pcat concept or make parallel the default
    execState.filter = TestFilter(
        kind: tfkCategories,
        cats: @[Category(p.key)],
        restOfCmdLine: p.cmdLineRest
      )
  of "p", "pat", "pattern":
    # tests files matching the given pattern
    execState.filter = TestFilter(
        kind: tfkGlob,
        pattern: p.key,
        restOfCmdLine: p.cmdLineRest
      )
  of "r", "run":
    # single test
    execState.filter = TestFilter(
        kind: tfkSingle,
        test: p.key,
        restOfCmdLine: p.cmdLineRest
      )
  else: return parseQuitWithUsage

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
  # xxx: create specific type to avoid accidental mutation
  
  # xxx: read-only state in let to avoid mutation, put into types
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
          # xxx: implement this proc and all the subsequent handling
          setupStdlibTests(execState, catId)
        else:
          handled = false
      
      if not handled:
        for file in walkDirRec(testsDir & cat.string):
          if file.isTestFile:
            execState.testFiles.add:
              TestFile(file: file, catId: catId)

  case filter.kind
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
    execState.categories = @[Category ""]
    let pattern = filter.pattern
    if dirExists(pattern):
      for kind, name in walkDir(pattern):
        if kind in {pcFile, pcLinkToFile} and name.endsWith(".nim"):
          execState.testFiles.add TestFile(file: name)
    else:
      for name in walkPattern(pattern):
        execState.testFiles.add TestFile(file: name)
  of tfkSingle:
    execState.categories = @[Category ""]
    let test = filter.test
    # xxx: replace with proper error handling
    doAssert fileExists(test), test & " test does not exist"
    if isTestFile(test):
      execState.testFiles.add TestFile(file: test)

  execState.testFiles.sort # ensures we have a reproducible ordering

  # parse all specs
  for testId, test in execState.testFiles.pairs:
    execState.testSpecs.add parseSpec(addFileExt(test.file, ".nim"))

    if execState.testOpts.hasKey(testId):
      # apply additional test matrix, if specified
      let optMatrix = execState.testOpts[testId].optMatrix
      execState.testSpecs[testId].matrix =
        case execState.testSpecs[testId].matrix.len
        of 0:
          optMatrix
        else:
          # REFACTOR - this is a hack, we shouldn't modify the spec.matrix
          var tmp: seq[string] = @[]
          for o in optMatrix.items:
            for e in execState.testSpecs[testId].matrix.items:
              tmp.add o & " " & e
          tmp
      
      # apply action override, if specified
      let actionOverride = execState.testOpts[testId].action
      if actionOverride.isSome:
        execState.testSpecs[testId].action = actionOverride.get

proc prepareTestRuns(execState: var Execution) =
  ## create a list of necessary testRuns
  # xxx: create specific type to avoid accidental mutation
  
  # xxx: read-only items; only testRuns are written
  let testSpecs = execState.testSpecs

  for testId, spec in testSpecs.pairs:
    let
      specTargets =
        if spec.targets == noTargetsSpecified:
          {targetC, targetCpp, targetJs}
        else:
          spec.targets
      targetsToRun = specTargets * execState.requestedTargets

    for target in targetsToRun:
      # TODO: create a "target matrix" to cover both js release vs non-release

      case spec.matrix.len
      of 0: # no tests to run
        execState.testRuns.add:
          TestRun(testId: testId, target: target, matrixEntry: noMatrixEntry)
        execState.runTimes.add RunTime()
        execState.runActuals.add RunActual()
      else:
        for entryId, _ in spec.matrix.pairs:
          execState.testRuns.add:
            TestRun(testId: testId, target: target, matrixEntry: entryId)
          execState.runTimes.add RunTime()
          execState.runActuals.add RunActual()

proc prepareTestActions(execState: var Execution) =
  ## create a list of necessary test actions
  # xxx: create specific type to avoid accidental mutation
  
  # xxx: these are what are read, so a dedicate type would offer a read-only
  #      view of those, but allow actions mutation
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

proc exeFile(testRun: TestRun, specFilePath: string): string =
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
  
  changeFileExt(joinPath(dirPart, exeName), exeExt)

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

proc reportTestRunResult(
    legacyResults: var TResults,
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
  ## birdge newer testament internals (`ExecutionState` and friends) to the
  ## legacy reporting and generate output accordingly.

  let
    duration = runTime.compileEnd - runTime.compileStart +
                runTime.runEnd - runTime.runStart
    target = testRun.target
    allowFailure = spec.err == reKnownIssue

  var
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
    legacyTest = TTest(
      cat: cat,
      name: testFile.makeName(testRun, allowFailure),
      options:
        if testRun.matrixEntry == noMatrixEntry:
          ""
        else:
          spec.matrix[testRun.matrixEntry],
      spec: spec,
      duration: some(duration))

  case spec.action
  of actionCompile:
    compilerOutputTests(legacyTest, target, givenAsSpec, spec, legacyResults)
  of actionRun:
    case action.kind
    of actionCompile:
      case givenAsSpec.err
      of reSuccess:
        discard # success will be determined after `actionRun`
      of reExeNotFound:
        legacyResults.addResult(
          legacyTest,
          target,
          spec.output,
          "executable not found: " & testRun.exeFile(givenAsSpec.file),
          reExeNotFound
        )
      else:
        # all other errors
        legacyResults.addResult(
          legacyTest,
          target,
          "",
          "$ " & givenAsSpec.cmd & '\n' & givenAsSpec.nimout,
          givenAsSpec.err,
          givenSpec = givenAsSpec.addr
        )
    of actionRun:
      checkCmdHelper(
        legacyResults,
        legacyTest,
        testRun.target,
        spec,
        givenAsSpec,
        runActual.prgOutSorted,
        case runActual.prgExit:
          # xxx - sigh... weird legacy
          # Treat all failure codes from nodejs as 1. Older versions of
          # nodejs used to return other codes, but for us it is sufficient
          # to know that it's not 0.
          of 0: 0
          else: 1
        )
    of actionReject:
      doAssert false, "we should never get here"
  of actionReject:
    # Scan compiler output fully for all mismatches and report if any found
    cmpMsgs(legacyResults, spec, givenAsSpec, legacyTest, target)

template runTestBatch(execState: var Execution,
                      testCmds: seq[string],
                      processOpts: set[ProcessOption],
                      batchSize: int,
                      exitCodes: seq[int],
                      outputs: seq[string],
                      startTimes, endTimes: seq[float],
                      onTestRunStart: proc (i: int),
                      onTestProcess, onTestRunComplete: proc (i: int, p: Process),
                      cmdIdToActId: var seq[int],
                      cmdIdToActKind: var seq[TTestAction],
                      cmdIdToInput: var seq[Option[string]]
                      ) =
  inc batches

  # TODO - handle executable not found issues for `actionRun`, test this before
  #        the action is swapped in from next to current batch (it should have
  #        already been created). If it's not, note progress and remove from
  #        runnable actions.

  # run actions
  #echo "execProcesses with: ", testCmds, processOpts, batchSize #, " and map: ", cmdIdToActId
  discard osproc.execProcesses(
      testCmds,
      processOpts,
      batchSize,
      onTestRunStart,
      onTestProcess,
      onTestRunComplete
    )

  for id in 0..<cmdIdToActId.len:
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
      
      # xxx - refactor into a proc

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
      
      let exeFile = exeFile(testRun, spec.file)
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

      # xxx - very ridiculous approach to comparing output, legacy junk
      #       sorry, i can't even being to explain this mess; brought over
      #       from `testSpecHelper`
      let
        expect = spec.output
        exitCode =
          if execState.runActuals[runId].prgExit != 0:
            # xxx - sigh... weird legacy
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

    reportTestRunResult(
        legacyResults = legacyResults,
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
  # xxx: create specific type to avoid accidental mutation

  let
    testRuns = execState.testRuns   ## immutable view of test runs
    testActions = execState.actions ## immutable view of test actions
    batchSize = defaultBatchSize    ## parallel processes to execute
                                    # xxx: use processor count
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
      cmd = prepareTestCmd(spec.getCmd,
                           testFile.file,
                           testArgs,
                           nimcache,
                           target,
                           matrixOptions,
                           testRun.exeFile(testFile.file))
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
    case spec.err
    of reDisabled, reKnownIssue:
      let
        allowFailure = spec.err == reKnownIssue
        legacyTest = TTest(
            cat: execState.categories[testFile.catId],
            name: testFile.makeName(testRun, allowFailure),
            spec: spec,
            duration: some(0.0),
            options:
              if testRun.matrixEntry == noMatrixEntry:
                ""
              else:
                spec.matrix[testRun.matrixEntry]
          )
      execState.legacyTestResults.addResult(
          legacyTest,
          target,
          "",
          "",
          spec.err,
          allowFailure
        )
      continue
    else:
      discard # keep processing

    case action.kind
    of actionRun:
      let
        isJsTarget = target == targetJs
        # specFile = execState.testSpecs[testId].file
        exeFile = testRun.exeFile(testFile.file)
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
        cmd = (@[exeCmd] & args).map(quoteShell).join(" ")

      if testCmds.len == 0: # we've already processed its dependency
        testCmds.add cmd
        cmdIdToActId.add actionId
        cmdIdToActKind.add action.kind
        cmdIdToInput.add testInput
      else: # dependency is in the current batch, add to the next
        nextTestCmds.add cmd
        nextCmdIdToActId.add actionId
        nextCmdIdToActKind.add action.kind
        nextCmdIdToInput.add testInput

    of actionCompile, actionReject:
      # add to this batch
      testCmds.add cmd
      cmdIdToActId.add actionId
      cmdIdToActKind.add action.kind
      cmdIdToInput.add testInput

    let
      lastActionId = testActions.len - 1
      lastAction = actionId == lastActionId
      currBatchFull = testCmds.len == batchSize
      processOpts = {poStdErrToStdOut, poUsePath}

    if currBatchFull or lastAction:
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
                   cmdIdToInput)
      
      var i = 0
      while i < nextCmdIdToActId.len:
        let
          cid = i
          aid = nextCmdIdToActId[cid]
          currAct = execState.actions[aid]
          currRunResult = execState.runActuals[currAct.runId].runResult

        case currRunResult
        of reSuccess:
          # this item is ok, we can proceed to the next one
          inc i
        else:
          # this isn't ok, remove it and the next item will take its place; so
          # don't increment `i` because the "item comes to us"
          nextTestCmds.delete(cid)
          nextCmdIdToActId.delete(cid)
          nextCmdIdToActKind.delete(cid)
          nextCmdIdToInput.delete(cid)

      # copy next cmds and cmd id to run id map to current
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
                   cmdIdToInput)
  
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
  azure.init()
  backend.open()

  var
    p         = initOptParser() # cli parser
    execState = Execution(
      flags: defaultExecFlags,
      testsDir: "tests" & DirSep,
      isCompilerRepo: isNimRepoTests() # legacy for `AdditionalCategories`
    )

  case parseOpts(execState, p)
  of parseQuitWithUsage: quit Usage
  of parseSuccess:       discard

  # next part should be the the filter action, eg: cat, r, etc...
  case parseArg(execState, p)
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
  azure.finalize()
  addExitProc azure.finalize

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
