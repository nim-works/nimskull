#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Nim Contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## :Author: Zahary Karadjov
##
## This module implements boilerplate to make unit testing easy.
##
## The test status and name is printed after any output or traceback.
##
## Tests can be nested, however failure of a nested test will not mark the
## parent test as failed. Setup and teardown are inherited. Setup can be
## overridden locally.
##
## Compiled test files as well as `nim c -r <testfile.nim>`
## exit with 0 for success (no failed tests) or 1 for failure.
##
## Running a single test
## =====================
##
## Specify the test name as a command line argument.
##
## .. code::
##
##   nim c -r test "my test name" "another test"
##
## Multiple arguments can be used.
##
## Running a single test suite
## ===========================
##
## Specify the suite name delimited by `"::"`.
##
## .. code::
##
##   nim c -r test "my test name::"
##
## Selecting tests by pattern
## ==========================
##
## A single ``"*"`` can be used for globbing.
##
## Delimit the end of a suite name with `"::"`.
##
## Tests matching **any** of the arguments are executed.
##
## .. code::
##
##   nim c -r test fast_suite::mytest1 fast_suite::mytest2
##   nim c -r test "fast_suite::mytest*"
##   nim c -r test "auth*::" "crypto::hashing*"
##   # Run suites starting with 'bug #' and standalone tests starting with '#'
##   nim c -r test 'bug #*::' '::#*'
##
## Examples
## ========
##
## .. code:: nim
##
##   suite "description for this stuff":
##     echo "suite setup: run once before the tests"
##
##     setup:
##       echo "run before each test"
##
##     teardown:
##       echo "run after each test"
##
##     test "essential truths":
##       # give up and stop if this fails
##       require(true)
##
##     test "slightly less obvious stuff":
##       # print a nasty message and move on, skipping
##       # the remainder of this block
##       check(1 != 1)
##       check("asd"[2] == 'd')
##
##     test "out of bounds error is thrown on bad access":
##       let v = @[1, 2, 3]  # you can do initialization here
##       expect(IndexDefect):
##         discard v[4]
##
##     echo "suite teardown: run once after the tests"
##
## Limitations/Bugs
## ================
## Since `check` will rewrite some expressions for supporting checkpoints
## (namely assigns expressions to variables), some type conversions are not supported.
## For example `check 4.0 == 2 + 2` won't work. But `doAssert 4.0 == 2 + 2` works.
## Make sure both sides of the operator (such as `==`, `>=` and so on) have the same type.
##

import std/private/since
import std/exitprocs

import std/[macros, strutils, streams, times, sets, sequtils]

when declared(stdout):
  import std/os

const useTerminal = not defined(js)

when useTerminal:
  import std/terminal

type
  TestStatus* = enum ## The status of a test when it is done.
    OK,
    FAILED,
    SKIPPED

  OutputLevel* = enum ## The output verbosity of the tests.
    PRINT_ALL,        ## Print as much as possible.
    PRINT_FAILURES,   ## Print only the failed tests.
    PRINT_NONE        ## Print nothing.

  TestResult* = object
    suiteName*: string
      ## Name of the test suite that contains this test case.
      ## Can be ``nil`` if the test case is not in a suite.
    testName*: string
      ## Name of the test case
    status*: TestStatus

  OutputFormatter* = ref object of RootObj

  ConsoleOutputFormatter* = ref object of OutputFormatter
    colorOutput: bool
      ## Have test results printed in color.
      ## Default is `auto` depending on `isatty(stdout)`, or override it with
      ## `-d:nimUnittestColor:auto|on|off`.
      ##
      ## Deprecated: Setting the environment variable `NIMTEST_COLOR` to `always`
      ## or `never` changes the default for the non-js target to true or false respectively.
      ## Deprecated: the environment variable `NIMTEST_NO_COLOR`, when set, changes the
      ## default to true, if `NIMTEST_COLOR` is undefined.
    outputLevel: OutputLevel
      ## Set the verbosity of test results.
      ## Default is `PRINT_ALL`, or override with:
      ## `-d:nimUnittestOutputLevel:PRINT_ALL|PRINT_FAILURES|PRINT_NONE`.
      ##
      ## Deprecated: the `NIMTEST_OUTPUT_LVL` environment variable is set for the non-js target.
    isInSuite: bool
    isInTest: bool

  JUnitOutputFormatter* = ref object of OutputFormatter
    stream: Stream
    testErrors: seq[string]
    testStartTime: float
    testStackTrace: string

var
  abortOnError* {.threadvar.}: bool ## Set to true in order to quit
                                    ## immediately on fail. Default is false,
                                    ## or override with `-d:nimUnittestAbortOnError:on|off`.
                                    ##
                                    ## Deprecated: can also override depending on whether
                                    ## `NIMTEST_ABORT_ON_ERROR` environment variable is set.

  checkpoints {.threadvar.}: seq[string]
  formatters {.threadvar.}: seq[OutputFormatter]
  testsFilters {.threadvar.}: HashSet[string]
  disabledParamFiltering {.threadvar.}: bool

const
  outputLevelDefault = PRINT_ALL
  nimUnittestOutputLevel {.strdefine.} = $outputLevelDefault
  nimUnittestColor {.strdefine.} = "auto" ## auto|on|off
  nimUnittestAbortOnError {.booldefine.} = false

template deprecateEnvVarHere() =
  # xxx issue a runtime warning to deprecate this envvar.
  discard

abortOnError = nimUnittestAbortOnError
when declared(stdout):
  if existsEnv("NIMTEST_ABORT_ON_ERROR"):
    deprecateEnvVarHere()
    abortOnError = true

method suiteStarted*(formatter: OutputFormatter, suiteName: string) {.base, gcsafe.} =
  discard
method testStarted*(formatter: OutputFormatter, testName: string) {.base, gcsafe.} =
  discard
method failureOccurred*(formatter: OutputFormatter, checkpoints: seq[string],
    stackTrace: string) {.base, gcsafe.} =
  ## ``stackTrace`` is provided only if the failure occurred due to an exception.
  ## ``checkpoints`` is never ``nil``.
  discard
method testEnded*(formatter: OutputFormatter, testResult: TestResult) {.base, gcsafe.} =
  discard
method suiteEnded*(formatter: OutputFormatter) {.base, gcsafe.} =
  discard

proc addOutputFormatter*(formatter: OutputFormatter) =
  formatters.add(formatter)

proc delOutputFormatter*(formatter: OutputFormatter) =
  keepIf(formatters, proc (x: OutputFormatter): bool =
    x != formatter)

proc resetOutputFormatters* {.since: (1, 1).} =
  formatters = @[]

proc newConsoleOutputFormatter*(outputLevel: OutputLevel = outputLevelDefault,
                                colorOutput = true): ConsoleOutputFormatter =
  ConsoleOutputFormatter(
    outputLevel: outputLevel,
    colorOutput: colorOutput
  )

proc colorOutput(): bool =
  let color = nimUnittestColor
  case color
  of "auto":
    when declared(stdout): result = isatty(stdout)
    else: result = false
  of "on": result = true
  of "off": result = false
  else: doAssert false, $color

  when declared(stdout):
    if existsEnv("NIMTEST_COLOR"):
      deprecateEnvVarHere()
      let colorEnv = getEnv("NIMTEST_COLOR")
      if colorEnv == "never":
        result = false
      elif colorEnv == "always":
        result = true
    elif existsEnv("NIMTEST_NO_COLOR"):
      deprecateEnvVarHere()
      result = false

proc defaultConsoleFormatter*(): ConsoleOutputFormatter =
  var colorOutput = colorOutput()
  var outputLevel = nimUnittestOutputLevel.parseEnum[:OutputLevel]
  when declared(stdout):
    const a = "NIMTEST_OUTPUT_LVL"
    if existsEnv(a):
      # xxx issue a warning to deprecate this envvar.
      outputLevel = getEnv(a).parseEnum[:OutputLevel]
  result = newConsoleOutputFormatter(outputLevel, colorOutput)

method suiteStarted*(formatter: ConsoleOutputFormatter, suiteName: string) =
  template rawPrint() = echo("\n[Suite] ", suiteName)
  when useTerminal:
    if formatter.colorOutput:
      styledEcho styleBright, fgBlue, "\n[Suite] ", resetStyle, suiteName
    else: rawPrint()
  else: rawPrint()
  formatter.isInSuite = true

method testStarted*(formatter: ConsoleOutputFormatter, testName: string) =
  formatter.isInTest = true

method failureOccurred*(formatter: ConsoleOutputFormatter,
                        checkpoints: seq[string], stackTrace: string) =
  if stackTrace.len > 0:
    echo stackTrace
  let prefix = if formatter.isInSuite: "    " else: ""
  for msg in items(checkpoints):
    echo prefix, msg

method testEnded*(formatter: ConsoleOutputFormatter, testResult: TestResult) =
  formatter.isInTest = false

  if formatter.outputLevel != OutputLevel.PRINT_NONE and
      (formatter.outputLevel == OutputLevel.PRINT_ALL or testResult.status == TestStatus.FAILED):
    let prefix = if testResult.suiteName.len > 0: "  " else: ""
    template rawPrint() = echo(prefix, "[", $testResult.status, "] ",
        testResult.testName)
    when useTerminal:
      if formatter.colorOutput:
        var color = case testResult.status
          of TestStatus.OK: fgGreen
          of TestStatus.FAILED: fgRed
          of TestStatus.SKIPPED: fgYellow
        styledEcho styleBright, color, prefix, "[", $testResult.status, "] ",
            resetStyle, testResult.testName
      else:
        rawPrint()
    else:
      rawPrint()

method suiteEnded*(formatter: ConsoleOutputFormatter) =
  formatter.isInSuite = false

proc xmlEscape(s: string): string =
  result = newStringOfCap(s.len)
  for c in items(s):
    case c:
    of '<': result.add("&lt;")
    of '>': result.add("&gt;")
    of '&': result.add("&amp;")
    of '"': result.add("&quot;")
    of '\'': result.add("&apos;")
    else:
      if ord(c) < 32:
        result.add("&#" & $ord(c) & ';')
      else:
        result.add(c)

proc newJUnitOutputFormatter*(stream: Stream): JUnitOutputFormatter =
  ## Creates a formatter that writes report to the specified stream in
  ## JUnit format.
  ## The ``stream`` is NOT closed automatically when the test are finished,
  ## because the formatter has no way to know when all tests are finished.
  ## You should invoke formatter.close() to finalize the report.
  result = JUnitOutputFormatter(
    stream: stream,
    testErrors: @[],
    testStackTrace: "",
    testStartTime: 0.0
  )
  stream.writeLine("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  stream.writeLine("<testsuites>")

proc close*(formatter: JUnitOutputFormatter) =
  ## Completes the report and closes the underlying stream.
  formatter.stream.writeLine("</testsuites>")
  formatter.stream.close()

method suiteStarted*(formatter: JUnitOutputFormatter, suiteName: string) =
  formatter.stream.writeLine("\t<testsuite name=\"$1\">" % xmlEscape(suiteName))

method testStarted*(formatter: JUnitOutputFormatter, testName: string) =
  formatter.testErrors.setLen(0)
  formatter.testStackTrace.setLen(0)
  formatter.testStartTime = epochTime()

method failureOccurred*(formatter: JUnitOutputFormatter,
                        checkpoints: seq[string], stackTrace: string) =
  ## ``stackTrace`` is provided only if the failure occurred due to an exception.
  ## ``checkpoints`` is never ``nil``.
  formatter.testErrors.add(checkpoints)
  if stackTrace.len > 0:
    formatter.testStackTrace = stackTrace

method testEnded*(formatter: JUnitOutputFormatter, testResult: TestResult) =
  let time = epochTime() - formatter.testStartTime
  let timeStr = time.formatFloat(ffDecimal, precision = 8)
  formatter.stream.writeLine("\t\t<testcase name=\"$#\" time=\"$#\">" % [
      xmlEscape(testResult.testName), timeStr])
  case testResult.status
  of TestStatus.OK:
    discard
  of TestStatus.SKIPPED:
    formatter.stream.writeLine("<skipped />")
  of TestStatus.FAILED:
    let failureMsg = if formatter.testStackTrace.len > 0 and
                        formatter.testErrors.len > 0:
                       xmlEscape(formatter.testErrors[^1])
                     elif formatter.testErrors.len > 0:
                       xmlEscape(formatter.testErrors[0])
                     else: "The test failed without outputting an error"

    var errs = ""
    if formatter.testErrors.len > 1:
      var startIdx = if formatter.testStackTrace.len > 0: 0 else: 1
      var endIdx = if formatter.testStackTrace.len > 0:
          formatter.testErrors.len - 2
        else: formatter.testErrors.len - 1

      for errIdx in startIdx..endIdx:
        if errs.len > 0:
          errs.add("\n")
        errs.add(xmlEscape(formatter.testErrors[errIdx]))

    if formatter.testStackTrace.len > 0:
      formatter.stream.writeLine("\t\t\t<error message=\"$#\">$#</error>" % [
          failureMsg, xmlEscape(formatter.testStackTrace)])
      if errs.len > 0:
        formatter.stream.writeLine("\t\t\t<system-err>$#</system-err>" % errs)
    else:
      formatter.stream.writeLine("\t\t\t<failure message=\"$#\">$#</failure>" %
          [failureMsg, errs])

  formatter.stream.writeLine("\t\t</testcase>")

method suiteEnded*(formatter: JUnitOutputFormatter) =
  formatter.stream.writeLine("\t</testsuite>")

proc glob(matcher, filter: string): bool =
  ## Globbing using a single `*`. Empty `filter` matches everything.
  if filter.len == 0:
    return true

  if not filter.contains('*'):
    return matcher == filter

  let beforeAndAfter = filter.split('*', maxsplit = 1)
  if beforeAndAfter.len == 1:
    # "foo*"
    return matcher.startsWith(beforeAndAfter[0])

  if matcher.len < filter.len - 1:
    return false # "12345" should not match "123*345"

  return matcher.startsWith(beforeAndAfter[0]) and matcher.endsWith(
      beforeAndAfter[1])

proc matchFilter(suiteName, testName, filter: string): bool =
  if filter == "":
    return true
  if testName == filter:
    # corner case for tests containing "::" in their name
    return true
  let suiteAndTestFilters = filter.split("::", maxsplit = 1)

  if suiteAndTestFilters.len == 1:
    # no suite specified
    let testFilter = suiteAndTestFilters[0]
    return glob(testName, testFilter)

  return glob(suiteName, suiteAndTestFilters[0]) and
         glob(testName, suiteAndTestFilters[1])

when defined(testing): export matchFilter

proc shouldRun(currentSuiteName, testName: string): bool =
  ## Check if a test should be run by matching suiteName and testName against
  ## test filters.
  if testsFilters.len == 0:
    return true

  for f in testsFilters:
    if matchFilter(currentSuiteName, testName, f):
      return true

  return false

proc ensureInitialized() =
  if formatters.len == 0:
    formatters = @[OutputFormatter(defaultConsoleFormatter())]

  if not disabledParamFiltering:
    when declared(paramCount):
      # Read tests to run from the command line.
      for i in 1 .. paramCount():
        testsFilters.incl(paramStr(i))

# These two procs are added as workarounds for
# https://github.com/nim-lang/Nim/issues/5549
proc suiteEnded() =
  for formatter in formatters:
    formatter.suiteEnded()

proc testStarted(testName: string) =
  for formatter in formatters:
    formatter.testStarted(testName)

proc testEnded(testResult: TestResult) =
  for formatter in formatters:
    formatter.testEnded(testResult)

proc exceptionTypeName(e: ref Exception): string {.inline.} =
  if e == nil: "<foreign exception>"
  else: $e.name

proc suiteStarted(name: string) =
  ensureInitialized()
  for formatter in formatters:
    formatter.suiteStarted(name)

template testInternal(testSuiteName, testNameArg, testBody: untyped): untyped =
  bind shouldRun, checkpoints, formatters, testEnded, exceptionTypeName, setProgramResult
  if shouldRun(testSuiteName, testNameArg):
    checkpoints = @[]
    var testStatusIMPL {.inject.} = TestStatus.OK
    testStarted(testNameArg)
    try:
      testBody
    except:
      let e = getCurrentException()
      let eTypeDesc = "[" & exceptionTypeName(e) & "]"
      checkpoint("Unhandled exception: " & getCurrentExceptionMsg() & " " & eTypeDesc)
      if e == nil: # foreign
        fail()
      else:
        var stackTrace {.inject.} = e.getStackTrace()
        fail()
    finally:
      if testStatusIMPL == TestStatus.FAILED:
        setProgramResult 1
      let testResult = TestResult(
        suiteName: testSuiteName,
        testName: testNameArg,
        status: testStatusIMPL
      )
      testEnded(testResult)
      checkpoints.setLen(0)

macro suite*(name: string,body: untyped): untyped =
  ## Declare a test suite identified by `name` with optional ``setup``
  ## and/or ``teardown`` section.
  ##
  ## A test suite is a series of one or more related tests sharing a
  ## common fixture (``setup``, ``teardown``). The fixture is executed
  ## for EACH test.
  ##
  ## .. code-block:: nim
  ##  suite "test suite for addition":
  ##    setup:
  ##      let result = 4
  ##
  ##    test "2 + 2 = 4":
  ##      check(2+2 == result)
  ##
  ##    test "(2 + -2) != 4":
  ##      check(2 + -2 != result)
  ##
  ##    # No teardown needed
  ##
  ## The suite will run the individual test cases in the order in which
  ## they were listed. With default global settings the above code prints:
  ##
  ## .. code-block::
  ##
  ##  [Suite] test suite for addition
  ##    [OK] 2 + 2 = 4
  ##    [OK] (2 + -2) != 4

  body.expectKind(nnkStmtList)
  # parse top level suite constructs and put their structure into local variables.
  var setup: NimNode
  var teardown: NimNode
  var tests: seq[NimNode]

  # stuff that really should not be placed top level in a unittest.
  result = newStmtList()

  for command in body:
    if command.kind in nnkCallKinds:
      let typ = command[0]
      typ.expectKind nnkIdent
      if typ.eqIdent("test"):
        command.expectLen 3
        let testName = command[1]
        testName.expectKind {nnkStrLit..nnkTripleStrLit}
        tests.add command
      elif typ.eqIdent("setup"):
        command.expectLen 2
        if setup == nil:
          setup = command
        else:
          error("double definition of setup", command)
      elif typ.eqIdent("teardown"):
        command.expectLen 2
        if teardown == nil:
          teardown = command
        else:
          error("double definition of teardown", command)
      else:
        error("illegal construct for unittest suite", command)
    else:
      warning("please only put `test`, `setup` and `teardown` at top level of `suite`", command)
      # for backwards compatibility, preserve these constructs unprocessed (really it should be an error)
      result.add command

  # start generating code from the earlier constructs
  result.add newCall(bindSym"suiteStarted", name)

  for test in tests:
    let tryBody = newStmtList()
    if setup != nil:
      tryBody.add setup[1]
    if teardown != nil:
      tryBody.add nnkDefer.newTree(teardown[1])
    tryBody.add test[2]
    let testName = test[1]
    result.add newCall(bindSym"testInternal", name, testName, tryBody)
  result.add newCall(bindSym"suiteEnded")

template test*(name: string, body: untyped) =
  ## Define a single test case identified by `name`.
  ##
  ## .. code-block:: nim
  ##
  ##  test "roses are red":
  ##    let roses = "red"
  ##    check(roses == "red")
  ##
  ## The above code outputs:
  ##
  ## .. code-block::
  ##
  ##  [OK] roses are red
  ensureInitialized()
  testInternal("", name, body)

proc checkpoint*(msg: string) =
  ## Set a checkpoint identified by `msg`. Upon test failure all
  ## checkpoints encountered so far are printed out. Example:
  ##
  ## .. code-block:: nim
  ##
  ##  checkpoint("Checkpoint A")
  ##  check((42, "the Answer to life and everything") == (1, "a"))
  ##  checkpoint("Checkpoint B")
  ##
  ## outputs "Checkpoint A" once it fails.
  checkpoints.add(msg)
  # TODO: add support for something like SCOPED_TRACE from Google Test

template fail* =
  ## Print out the checkpoints encountered so far and quit if ``abortOnError``
  ## is true. Otherwise, erase the checkpoints and indicate the test has
  ## failed (change exit code and test status). This template is useful
  ## for debugging, but is otherwise mostly used internally. Example:
  ##
  ## .. code-block:: nim
  ##
  ##  checkpoint("Checkpoint A")
  ##  complicatedProcInThread()
  ##  fail()
  ##
  ## outputs "Checkpoint A" before quitting.
  bind ensureInitialized, setProgramResult
  when declared(testStatusIMPL):
    testStatusIMPL = TestStatus.FAILED
  else:
    setProgramResult 1

  ensureInitialized()

    # var stackTrace: string = nil
  for formatter in formatters:
    when declared(stackTrace):
      formatter.failureOccurred(checkpoints, stackTrace)
    else:
      formatter.failureOccurred(checkpoints, "")

  if abortOnError: quit(1)

  checkpoints = @[]

template skip* =
  ## Mark the test as skipped. Should be used directly
  ## in case when it is not possible to perform test
  ## for reasons depending on outer environment,
  ## or certain application logic conditions or configurations.
  ## The test code is still executed.
  ##
  ## .. code-block:: nim
  ##
  ##  if not isGLContextCreated():
  ##    skip()
  bind checkpoints

  testStatusIMPL = TestStatus.SKIPPED
  checkpoints = @[]

proc print[T: not typedesc](name: string, value: T) =
  when compiles(string($value)):
    checkpoint(name & " was " & $value)

proc print[T](name: string, typ: typedesc[T]) =
  checkpoint(name & " was " & $typ)

proc untype(arg: NimNode): NimNode =
  case arg.kind
  of nnkCharLit..nnkUInt64Lit:
    result = newNimNode(arg.kind, arg)
    result.intVal = arg.intVal
  of nnkFloatLit..nnkFloat128Lit:
    result = newNimNode(arg.kind, arg)
    result.floatVal = arg.floatVal
  of nnkStrLit..nnkTripleStrLit:
    result = newNimNode(arg.kind, arg)
    result.strVal = arg.strVal
  of nnkSym, nnkOpenSymChoice, nnkClosedSymChoice:
    result = newIdentNode(arg.repr)
  of nnkIdent:
    result = arg
  else:
    result = newNimNode(arg.kind, arg)
    for n in arg:
      result.add untype(n)

macro check*(conditions: untyped): untyped =
  ## Verify if a statement or a list of statements is true.
  ## A helpful error message and set checkpoints are printed out on
  ## failure (if ``outputLevel`` is not ``PRINT_NONE``).
  runnableExamples:
    import std/strutils

    check("AKB48".toLowerAscii() == "akb48")

    let teams = {'A', 'K', 'B', '4', '8'}

    check:
      "AKB48".toLowerAscii() == "akb48"
      'C' notin teams

  # the call to `untype` is a workaround for https://github.com/nim-works/nimskull/issues/193
  let checked = untype(conditions)

  proc inspectArgs(exp: NimNode): tuple[assigns, check, printOuts: NimNode] =
    result.check = copyNimTree(exp)
    result.assigns = newNimNode(nnkStmtList)
    result.printOuts = newNimNode(nnkStmtList)

    if exp[0].kind in {nnkIdent, nnkOpenSymChoice, nnkClosedSymChoice, nnkSym} and
        $exp[0] in ["not", "in", "notin", "==", "<=",
                    ">=", "<", ">", "!=", "is", "isnot"]:

      for i in 1 ..< exp.len:
        if exp[i].kind notin nnkLiterals:
          let argStr = exp[i].toStrLit
          let paramAst = exp[i]
          if exp[i].kind == nnkIdent:
            result.printOuts.add newCall(bindSym"print", argStr, paramAst)
          if exp[i].kind in nnkCallKinds + {nnkDotExpr, nnkBracketExpr, nnkPar} and
                  (exp[i].typeKind notin {ntyTypeDesc} or $exp[0] notin ["is", "isnot"]):
            let callVar = genSym(nskVar, "c")
            result.assigns.add newVarStmt(callVar, paramAst)
            result.check[i] = callVar
            result.printOuts.add newCall(bindSym"print", argStr, callVar)
          if exp[i].kind == nnkExprEqExpr:
            # ExprEqExpr
            #   Ident "v"
            #   IntLit 2
            result.check[i] = exp[i][1]
          if exp[i].typeKind notin {ntyTypeDesc}:
            let arg = genSym(nskVar, "p")
            result.assigns.add newVarStmt(arg, paramAst)
            result.printOuts.add newCall(bindSym"print", argStr, arg)
            if exp[i].kind != nnkExprEqExpr:
              result.check[i] = arg
            else:
              result.check[i][1] = arg

  case checked.kind
  of nnkCallKinds:

    let (assigns, check, printOuts) = inspectArgs(checked)
    let lineinfo = newStrLitNode(checked.lineInfo)
    let callLit = checked.toStrLit
    result = quote do:
      block:
        `assigns`
        if not `check`:
          checkpoint(`lineinfo` & ": Check failed: " & `callLit`)
          `printOuts`
          fail()

  of nnkStmtList:
    result = newNimNode(nnkStmtList)
    for node in checked:
      if node.kind != nnkCommentStmt:
        result.add(newCall(newIdentNode("check"), node))

  else:
    let lineinfo = newStrLitNode(checked.lineInfo)
    let callLit = checked.toStrLit

    result = quote do:
      if not `checked`:
        checkpoint(`lineinfo` & ": Check failed: " & `callLit`)
        fail()

template require*(conditions: untyped) =
  ## Same as `check` except any failed test causes the program to quit
  ## immediately. Any teardown statements are not executed and the failed
  ## test output is not generated.
  let savedAbortOnError = abortOnError
  block:
    abortOnError = true
    check conditions
  abortOnError = savedAbortOnError

macro expect*(args: varargs[untyped]): untyped =
  ## Test if `body` raises an exception found in the passed `exceptions`.
  ## The test passes if the raised exception is part of the acceptable
  ## exceptions. Otherwise, it fails.
  runnableExamples:
    import std/[math, random, strutils]
    proc defectiveRobot() =
      randomize()
      case rand(1..4)
      of 1: raise newException(OSError, "CANNOT COMPUTE!")
      of 2: discard parseInt("Hello World!")
      of 3: raise newException(IOError, "I can't do that Dave.")
      else: assert 2 + 2 == 5

    expect IOError, OSError, ValueError, AssertionDefect:
      defectiveRobot()

  args.expectMinLen(2)

  let exceptBranch1 = newNimNode(nnkExceptBranch)
  for exp in args[0 ..< ^1]:
    exceptBranch1.add(exp)
  exceptBranch1.add nnkDiscardStmt.newTree(newEmptyNode())

  let body = if args[^1].kind == nnkStmtList: args[^1] else: newStmtList(args[^1])

  let lineInfoStr: string = exceptBranch1[0].lineInfo
  let msg1 = newLit(lineInfoStr & ": Expect Failed, no exception was thrown.")

  body.add newCall(bindSym"checkpoint", msg1)
  body.add newCall(bindSym"fail")

  let msg2 = newLit(lineInfoStr & ": Expect Failed, unexpected exception was thrown.")
  let wrongException = newStmtList(
    newCall(bindSym"checkpoint", msg2),
    newCall(bindSym"fail")
  )
  let exceptBranch2 = nnkExceptBranch.newTree(wrongException)

  result = nnkTryStmt.newTree(body, exceptBranch1, exceptBranch2)

proc disableParamFiltering* =
  ## disables filtering tests with the command line params
  disabledParamFiltering = true
