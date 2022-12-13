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
import lib/stdtest/testutils
from lib/stdtest/specialpaths import splitTestFile
import experimental/[sexp, sexp_diff, colortext, colordiff]


proc isNimRepoTests(): bool =
  # this logic could either be specific to cwd, or to some file derived from
  # the input file, eg testament r /pathto/tests/foo/tmain.nim; we choose
  # the former since it's simpler and also works with `testament all`.
  let file = "testament"/"testament.nim.cfg"
  result = file.fileExists






# ----------------------------------------------------------------------------

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


if paramCount() == 0:
  quit Usage



func requestedTargets(execState: Execution): set[TTarget] =
  ## get the requested targets by the user or the defaults
  if execState.targets == noTargetsSpecified:
    {targetC, targetJS}
  else:
    execState.targets



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
  if test.options.len > 0:
    result.add ' ' & test.options





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
    # REFACTOR test run should contian the ID of the specification it's
    # associated with as well as information about its matrix parameters.
    # The ID should be used instead of this copy.
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
      # REFACTOR it looks like this piece does repetitive work for
      # determining whether there is an error. I suspect this can be
      # changed into a function that fully analyzes the run result,
      # comparing it with a specification and returning the final
      # `TestRunResult` object which is then queried for any necessary
      # content.
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
    # QUESTION ASSUME I assume thread var is added here because of the
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
  #
  # TODO print out list of failed tests in a simplified manner maybe?
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

