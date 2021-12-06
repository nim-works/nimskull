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
import utils/nodejs
import lib/stdtest/testutils
from lib/stdtest/specialpaths import splitTestFile
import experimental/[sexp, sexp_diff, colortext, colordiff]

proc trimUnitSep(x: var string) =
  let L = x.len
  if L > 0 and x[^1] == '\31':
    setLen x, L-1

var useColors = true
var backendLogging = true
var simulate = false
var optVerbose = false
var useMegatest = true
var optFailing = false

import std/sugar

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
    res: ColText

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
    debugInfo: string

# ----------------------------------------------------------------------------

let
  pegLineError =
    peg"{[^(]*} '(' {\d+} ', ' {\d+} ') ' ('Error') ':' \s* {.*}"
  pegOtherError = peg"'Error:' \s* {.*}"
  pegOfInterest = pegLineError / pegOtherError

var gTargets = {low(TTarget)..high(TTarget)}
var targetsSet = false

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

proc getName(test: TTest, target: TTarget, allowFailure: bool): string =
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
    duration: float
    name: string
    outCompare: TOutCompare
    success: TResultEnum

    expected, given: string

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

  # Compute test test duration, final success status, prepare formatting variables
  var param: ReportParams

  param.expected = expected
  param.given = given
  param.outCompare = outCompare
  param.duration = epochTime() - test.startTime
  param.success =
    if test.spec.timeout > 0.0 and param.duration > test.spec.timeout:
      reTimeout
    else:
      successOrig


  param.name = test.getName(target, allowFailure)

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

  var map = r.diffMap
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
    echo "executing structural comparison"
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
  if target == targetJS:
    result = test.name.changeFileExt("js")
  else:
    let (_, name, _) = test.name.splitFile
    let ext = target.ext
    result = nimcacheDir(test.name, test.options, target) / "@m" & name.changeFileExt(ext)

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
            r.addResult(test, target, "exitcode: " & $expected.exitCode,
                              "exitcode: " & $exitCode & "\n\nOutput:\n" &
                              bufB, reExitcodesDiffer)
          elif (
            expected.outputCheck == ocEqual and
            not expected.output.equalModuloLastNewline(bufB)
          ) or (
            expected.outputCheck == ocSubstr and
            expected.output notin bufB
          ):
            given.err = reOutputsDiffer
            r.addResult(test, target, expected.output, bufB, reOutputsDiffer)
          else:
            compilerOutputTests(test, target, given, expected, r)
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
      of "pedantic": discard # deadcode refs https://github.com/nim-lang/Nim/issues/16731
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
  var optPrintResults = false
  var targetsStr      = ""
  var isMainProcess   = true
  var skipFrom        = ""
  # Init cli parser
  var p               = initOptParser()
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
    if useMegatest: cats.add MegaTestCat
    # We now prepare the command line arguments for our child processes

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
    var cat = Category(p.key)
    processCategory(r, cat, p.cmdLineRest, testsDir, runJoinableTests = true)
  of "pcat": # Run cat in parallel
    # Run all tests of a certain category in parallel; does not include joinable
    # tests which are covered in the 'megatest' category.
    skips = loadSkipFrom(skipFrom)
    isMainProcess = false
    var cat = Category(p.key)
    p.next
    processCategory(r, cat, p.cmdLineRest, testsDir, runJoinableTests = false)
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
main()
