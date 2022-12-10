#
#
#            Nim Tester
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/[
    sequtils,
    parseutils,
    strutils,
    os,
    streams,
    parsecfg,
    tables,
    hashes,
    sets
  ],
  experimental/[
    shellrunner
  ],
  test_diff

type TestamentData* = ref object
  # better to group globals under 1 object; could group the other ones here too
  batchArg*: string
  testamentNumBatch*: int
  testamentBatch*: int

# global mutable state for funsies
var
  compilerPrefix* = findExe("nim")
  skips*: seq[string]

let testamentData0* = TestamentData()

type
  GivenTestAction* = enum
    ## Test action that is specified in the file. All types of test actions
    ## can have their runtime and compile-time results checked afterwards.
    actionRun = "run" ## Compile and run the file
    actionCompile = "compile" ## Only compile file, but don't run.
    actionReject = "reject" ## Reject file compilation -- test should fail
                            ## because it contains code that is invalid.

  GivenOutputCheck* = enum
    ## How runtime test output should be checked against the Spec output
    ## string.
    ocIgnore = "ignore" ## Runtime output is ignored
    ocEqual = "equal" ## Runtime output should be fully equal to the
                      ## specified one.
    ocSubstr = "substr" ## Specified output is a substring of the runtime
                        ## output.

  GivenResultKind* = enum
    grKnownIssue
    grDisabled
    grInvalidSpec

  TestedResultKind* = enum
    ## Possible test execution and validation results.
    reNimcCrash        ## nim compiler seems to have crashed
    reMsgsDiffer       ## error messages differ
    reFilesDiffer      ## expected and given filenames differ
                       # (DOC given where? what filenames are checked
                       # against each other? something to do with misplaced
                       # error message annotations or?)
    reLinesDiffer      ## expected and given line numbers differ
    reOutputsDiffer    ## Runtime output string did not pass the check
    reExitcodesDiffer  ## exit codes of program or of valgrind differ
    reTimeout          ## Test run time exceeded the limit
    reInvalidPeg       ## Incorrectly specified peg pattern in the
                       ## specification
    reCodegenFailure
    reCodeNotFound
    reExeNotFound      ## Specified compiler exec could not be found
    reInstallFailed    ## package installation failed
    reBuildFailed      ## package building failed
    reDisabled         ## test is disabled
    reJoined           ## test is disabled because it was joined into the
                       ## megatest
    reInvalidSpec      ## test had problems to parse the spec
    reKnownIssue       ## test has a known issue(s) and is expected to fail
    reSuccess          ## test was successful

  GivenTarget* = enum
    ## Target to compile and run tests
    targetC = "c" ## Native compilation
    targetJS = "js" ## Javascript backend
    targetVM = "vm" ## Built-in interpreter VM

  SpecifiedTarget* = enum
    ## Possible target names that can be provided in the test
    ## specification. Subtractions have a higher priority compared to the
    ## additions and processed in the second pass.
    addTargetC = "c"
    remTargetC = "!c"
    addTargetJS = "js"
    remTargetJS = "!js"
    addTargetVM = "vm"
    remTargetVM = "!vm"
    addTargetNative = "native" # TODO DOC what is native target for
                               # `parseSpec` -- it is not a constant value,
                               # this thing can be different.
    addCategoryTargets = "default" # TODO DOC 'default' target -- same
                                   # question as for 'native'
    remCategoryTargets = "!default"

  ValgrindSpec* = enum
    disabled, enabled, leaking

  DeclaredSpec* = object
    ## Test specification written in the user-provided test file.
    # xxx make sure `isJoinableSpec` takes into account each field here.
    description*: string ## document the purpose of the test
    action*: GivenTestAction ## Test action specified in the test file
    file*: string ## File that test spec has been parsed from
    cmd*: ShellCmd ## Command to execute for the test
    input*: string ## `stdin` input that will be piped into program after
                   ## it has been compiled.
    outputCheck*: GivenOutputCheck ## Kind of the output checking for the
    ## executed compiled program.
    sortoutput*: bool ## Sort runtime output before checking it against
                      ## given one.
    output*: string ## Expected runtime output for the test run.

    # REFACTOR Use inline error object instead?
    line*: int ## Expected line for the diagnostics message issued during
               ## test compilation.
    column*: int ## Expected column for compile-time diagnostics
    msg*: string ## Expected compile-time diagnostics message

    exitCode*: int
    ccodeCheck*: seq[string] ## List of peg patterns that need to be
    ## searched for in the generated code. Used for backend code testing.
    maxCodeSize*: int ## Maximum allowed code size (in bytes) for the test.
    err*: GivenResultKind # REFACTOR Separate into 'expected output' (they
    # can be specified in the real test) and 'real output' (they can
    # actually happen - "invlid peg" etc.)
    inCurrentBatch*: bool
    targets*: set[GivenTarget] ## Collection of targets to run test on.
    specifiedTargets*: set[SpecifiedTarget] ## targets as described by the spec
    matrix*: seq[seq[ShellArg]] ## Collection of additional flags that will
    ## be used to create multiple run configurations from a single test.
    nimoutSexp*: bool ## Structured S-expression of compile-time output.
    nimout*: string ## Subsequence of the compile-time output.
    nimoutFull*: bool    ## whether nimout is all compiler output or a subset
    parseErrors*: string ## when the spec definition is invalid, this is
                         ## not empty.
    unbatchable*: bool
      ## whether this test can be batchable via `NIM_TESTAMENT_BATCH`; only
      ## very few tests are not batchable; the ones that are not could be
      ## turned batchable by making the dependencies explicit
    useValgrind*: ValgrindSpec
    timeout*: float ## in seconds, fractions possible, but don't rely on
    ## much precision
    inlineErrors*: seq[GivenInlineError] ## line information to error
                                              ## message
    debugInfo*: string ## debug info to give more context
    knownIssues*: seq[string] ## known issues to be fixed
    labels*: seq[string] ## user-added metadata

  RetryContainer* = object
    ## Global object which contains information related to the --retry flag.
    ## See `var retryContainer`.
    retry*: bool  ## true when --retry flag has been passed
    cats*: seq[string] ## contains categories with failed tests
    names*: seq[(string, string)] ## contains pair of failed test name and
                                  ## its target

# Exported global RetryContainer object. REFACTOR move to the execution
# field.
var retryContainer* = RetryContainer(retry: false)

proc getCmd*(s: DeclaredSpec): ShellCmd =
  ## Get runner command for a Spec test specification
  if s.cmd.empty():
    result = shell(
      compilerPrefix,
      shSub("target"),
      shArg("--hints=on"),
      shArg("--define=testing"),
      shArg("--clearNimblePath"),
      shArg("--nimblePath=build/deps/pks"),
      shSub("options"),
      shSub("file")
    )

  else:
    result = s.cmd

func ext*(t: GivenTarget): string =
  ## read-only field providing the extension string for the Spec target
  case t:
    of targetC:    "nim.c"
    of targetJS:   "js"
    of targetVM:   ""

func cmd*(t: GivenTarget): ShellArg =
  ## read-only field providing the command string for the Spec target
  case t:
    of targetC:    shArg("c")
    of targetJS:   shArg("js")
    of targetVM:   shArg("vm")

func defaultOptions*(a: GivenTarget): seq[ShellArg] =
  case a:
    of targetJS: @[shArg("-d:nodejs")]
      # once we start testing for `nim js -d:nimbrowser` (eg selenium or
      # similar), we can adapt this logic; or a Spec js test can override
      # with `-u:nodejs`.
    else: @[]

when not declared(parseCfgBool):
  # candidate for the stdlib:
  proc parseCfgBool(s: string): bool =
    case normalize(s)
    of "y", "yes", "true", "1", "on": result = true
    of "n", "no", "false", "0", "off": result = false
    else: raise newException(ValueError, "cannot interpret as a bool: " & s)

const
  InlineErrorMarker = "#[tt."

proc extractErrorMsg(
    s: string, i: int, line: var int, col: var int
  ): tuple[nextpos: int, errors: seq[GivenInlineError]] =
  ## Extract error message starting from the current position and return
  ## the next position to start parsing from.
  result.nextpos = i + len(InlineErrorMarker)
  inc col, len(InlineErrorMarker)
  var kind = ""
  while result.nextpos < s.len and s[result.nextpos] in IdentChars:
    kind.add s[result.nextpos]
    inc result.nextpos
    inc col

  var caret = (line, -1)

  template skipWhitespace =
    while result.nextpos < s.len and s[result.nextpos] in Whitespace:
      if s[result.nextpos] == '\n':
        col = 1
        inc line
      else:
        inc col
      inc result.nextpos

  skipWhitespace()
  if result.nextpos < s.len and s[result.nextpos] == '^':
    caret = (line-1, col)
    inc result.nextpos
    inc col
    skipWhitespace()

  var msg = ""
  while result.nextpos < s.len-1:
    if s[result.nextpos] == '\n':
      inc result.nextpos
      inc line
      col = 1
    elif s[result.nextpos] == ']' and s[result.nextpos+1] == '#':
      while msg.len > 0 and msg[^1] in Whitespace:
        setLen msg, msg.len - 1

      inc result.nextpos
      inc col, 2
      result.errors.add GivenInlineError(
        kind: kind, msg: msg, line: caret[0], col: caret[1])
      break
    else:
      msg.add s[result.nextpos]
      inc result.nextpos
      inc col

proc extractSpec(filename: string; spec: var DeclaredSpec): string =
  const
    tripleQuote = "\"\"\""
    specStart = "discard " & tripleQuote
  var s = readFile(filename)

  var i = 0
  var a = -1
  var b = -1
  var line = 1
  var col = 1
  while i < s.len:
    if (i == 0 or s[i-1] != ' ') and s.continuesWith(specStart, i):
      # `s[i-1] == '\n'` would not work because of
      # `tests/stdlib/tbase64.nim` which contains BOM
      # (https://en.wikipedia.org/wiki/Byte_order_mark)
      const lineMax = 10
      if a != -1:
        raise newException(
          ValueError,
          "testament spec violation: duplicate `specStart` found: " &
            $(filename, a, b, line))
      elif line > lineMax:
        # not overly restrictive, but prevents mistaking some `specStart`
        # as spec if deep inside a test file
        raise newException(
          ValueError,
          "testament spec violation: `specStart` should be before line $1, or be indented; info: $2" % [
            $lineMax, $(filename, a, b, line)])
      i += specStart.len
      a = i
    elif a > -1 and b == -1 and s.continuesWith(tripleQuote, i):
      b = i
      i += tripleQuote.len
    elif s[i] == '\n':
      inc line
      inc i
      col = 1
    elif s.continuesWith(InlineErrorMarker, i):
      # Found `#[tt.` - extract it
      let (nextpos, errors) = extractErrorMsg(s, i, line, col)
      i = nextpos
      spec.inlineErrors = errors
      for err in errors:
        if err.kind == "Error":
          spec.action = actionReject

    else:
      inc col
      inc i

  if a >= 0 and b > a:
    result = s.substr(a, b-1).multiReplace({"'''": tripleQuote, "\\31": "\31"})
  elif a >= 0:
    raise newException(
      ValueError,
      "testament spec violation: `specStart` found but not trailing `tripleQuote`: $1" %
        $(filename, a, b, line))
  else:
    result = ""

proc parseTargets*(value: string): set[GivenTarget] =
  ## Get list of allowed run targets for the testament
  for v in value.normalize.splitWhitespace:
    result.incl:
      case v
      of "c":          targetC
      of "js":         targetJS
      of "vm":         targetVM
      else: raise newException(ValueError, "invalid target: '$#'" % v)

proc parseSpecifiedTargets*(value: string): set[SpecifiedTarget] =
  ## Get target specification
  for v in value.normalize.splitWhitespace:
    result.incl:
      case v
      of "c":            addTargetC
      of "js":           addTargetJS
      of "vm":           addTargetVM
      of "native":       addTargetNative
      of "default":      addCategoryTargets
      of "!c":           remTargetC
      of "!js":          remTargetJS
      of "!vm":          remTargetVM
      of "!default":     remCategoryTargets
      else: raise newException(ValueError,
                               "invalid target specificatoin: '$#'" % v)

proc addLine*(self: var string; a: string) =
  self.add a
  self.add "\n"

proc addLine*(self: var string; a, b: string) =
  self.add a
  self.add b
  self.add "\n"

proc initSpec*(filename: string): DeclaredSpec =
  result.file = filename

proc isCurrentBatch*(
    testamentData: TestamentData, filename: string): bool =
  if testamentData.testamentNumBatch != 0:
    hash(filename) mod testamentData.testamentNumBatch == testamentData.testamentBatch
  else:
    true

proc parseSpec*(filename: string,
                caGivenTargets: set[GivenTarget],
                nativeTarget: GivenTarget): DeclaredSpec =
  ## Extract and parse specification for a Spec file path
  result.file = filename

  when defined(windows):
    let cmpString = result.file.replace(r"\", r"/")
  else:
    let cmpString = result.file
  if retryContainer.retry and
     not retryContainer.names.anyIt(cmpString == it[0]):
    result.err = grDisabled
    return result

  let specStr = extractSpec(filename, result)
  var ss = newStringStream(specStr)
  var p: CfgParser
  open(p, ss, filename, 1)
  var flags: HashSet[string]
  var nimoutFound = false
  while true:
    var e = next(p)
    case e.kind
    of cfgKeyValuePair:
      let key = e.key.normalize
      const allowMultipleOccurences = ["disabled", "ccodecheck" , "knownissue"]
        ## list of flags that are correctly handled when passed multiple times
        ## (instead of being overwritten)
      if key notin allowMultipleOccurences:
        doAssert key notin flags, $(key, filename)
      flags.incl key
      case key
      of "description":
        # XXX: this allows for a 'description' to be provided, need to
        #      incorporate it into the the actual test runner and output.
        # result.description = e.value
        discard
      of "nimoutformat":
        case e.value.normalize:
          of "sexp":
            result.nimoutSexp = true

          of "text":
            result.nimoutSexp = false

          else:
            result.parseErrors.addLine "unexpected nimout format: got ", e.value

      of "action":
        case e.value.normalize
        of "compile":
          result.action = actionCompile
        of "run":
          result.action = actionRun
        of "reject":
          result.action = actionReject
        else:
          result.parseErrors.addLine "cannot interpret as action: ", e.value
      of "file":
        if result.msg.len == 0 and result.nimout.len == 0:
          result.parseErrors.addLine "errormsg or msg needs to be specified before file"
        result.file = e.value
      of "line":
        if result.msg.len == 0 and result.nimout.len == 0:
          result.parseErrors.addLine "errormsg, msg or nimout needs to be specified before line"
        discard parseInt(e.value, result.line)
      of "column":
        if result.msg.len == 0 and result.nimout.len == 0:
          result.parseErrors.addLine "errormsg or msg needs to be specified before column"
        discard parseInt(e.value, result.column)
      of "output":
        if result.outputCheck != ocSubstr:
          result.outputCheck = ocEqual
        result.output = e.value
      of "input":
        result.input = e.value
      of "outputsub":
        result.outputCheck = ocSubstr
        result.output = strip(e.value)
      of "sortoutput":
        try:
          result.sortoutput = parseCfgBool(e.value)
        except:
          result.parseErrors.addLine getCurrentExceptionMsg()
      of "exitcode":
        discard parseInt(e.value, result.exitCode)
        result.action = actionRun
      of "errormsg":
        result.msg = e.value
        result.action = actionReject
      of "nimout":
        result.nimout = e.value
        nimoutFound = true
      of "nimoutfull":
        result.nimoutFull = parseCfgBool(e.value)
      of "batchable":
        result.unbatchable = not parseCfgBool(e.value)
      of "valgrind":
        when defined(linux) and sizeof(int) == 8:
          result.useValgrind = if e.value.normalize == "leaks": leaking
                               else: ValgrindSpec(parseCfgBool(e.value))
          if result.useValgrind != disabled:
            result.outputCheck = ocSubstr
        else:
          # Windows lacks valgrind. Silly OS.
          # Valgrind only supports OSX <= 17.x
          result.useValgrind = disabled
      of "disabled":
        case e.value.normalize
        of "y", "yes", "true", "1", "on": result.err = grDisabled
        of "n", "no", "false", "0", "off": discard
        of "win", "windows":
          when defined(windows): result.err = grDisabled
        of "linux":
          when defined(linux): result.err = grDisabled
        of "bsd":
          when defined(bsd): result.err = grDisabled
        of "osx", "macosx": # xxx remove `macosx` alias?
          when defined(osx): result.err = grDisabled
        of "unix":
          when defined(unix): result.err = grDisabled
        of "posix":
          when defined(posix): result.err = grDisabled
        of "32bit":
          if sizeof(int) == 4:
            result.err = grDisabled
        of "freebsd":
          when defined(freebsd): result.err = grDisabled
        of "arm64":
          when defined(arm64): result.err = grDisabled
        of "i386":
          when defined(i386): result.err = grDisabled
        of "openbsd":
          when defined(openbsd): result.err = grDisabled
        of "netbsd":
          when defined(netbsd): result.err = grDisabled
        else:
          result.parseErrors.addLine(
            "cannot interpret as a bool or platform name: ", e.value)
      of "cmd":
        result.cmd = parseShellCmd(e.value)

        if result.cmd.bin == "nim":
          result.cmd.bin = compilerPrefix

      of "ccodecheck":
        result.ccodeCheck.add e.value
      of "maxcodesize":
        discard parseInt(e.value, result.maxCodeSize)
      of "timeout":
        try:
          result.timeout = parseFloat(e.value)
        except ValueError:
          result.parseErrors.addLine "cannot interpret as a float: ", e.value
      of "targets", "target":
        try:
          result.specifiedTargets.incl parseSpecifiedTargets(e.value)
        except ValueError as e:
          result.parseErrors.addLine e.msg
        
        # do a two pass add / remove, this way 'default !js' is the same as
        # '!js default'

        # do the additions first
        for st in result.specifiedTargets:
          case st
          of addTargetC: result.targets.incl targetC
          of addTargetJS: result.targets.incl targetJS
          of addTargetVM: result.targets.incl targetVM
          of addTargetNative: result.targets.incl nativeTarget
          of addCategoryTargets: result.targets = result.targets + caGivenTargets
          of remTargetC, remTargetJS, remTargetVM,
             remCategoryTargets:
               discard
        
        if result.targets == {}:
          # nothing was specified, so assume the defaults
          result.targets = caGivenTargets

        # do the removals next
        for st in result.specifiedTargets:
          case st
          of remTargetC: result.targets.excl targetC
          of remTargetJS: result.targets.excl targetJS
          of remTargetVM: result.targets.excl targetVM
          of remCategoryTargets: result.targets = result.targets - caGivenTargets
          of addTargetC, addTargetJS, addTargetVM,
             addTargetNative, addCategoryTargets:
               discard
        
        # if nothing was specified, setup category defaults
        if result.targets == {}:
          result.parseErrors.addLine "Empty targets set, ", e.value

      of "matrix":
        for v in e.value.split(';'):
          result.matrix.add(parseShellArgs(v.strip()))

      of "knownissue":
        case e.value.normalize
          of "n", "no", "false", "0": discard
          else:
            result.knownIssues.add e.value
            result.err = grKnownIssue

      of "labels":
         discard """
         Adding only key support for now.
         This allows on-band addition of tags inside the tests, without
         requiring full parsing implementation.
         """
      else:
        result.parseErrors.addLine "invalid key for test spec: ", e.key

    of cfgSectionStart:
      result.parseErrors.addLine "section ignored: ", e.section
    of cfgOption:
      result.parseErrors.addLine "command ignored: ", e.key & ": " & e.value
    of cfgError:
      result.parseErrors.addLine e.msg
    of cfgEof:
      break
  close(p)

  # IMPLEMENT spec parser diagnostics -- missing description, malformed
  # test specification and so on.
  if skips.anyIt(it in result.file):
    result.err = grDisabled
  if nimoutFound and result.nimout.len == 0 and not result.nimoutFull:
    result.parseErrors.addLine "empty `nimout` is vacuously true, use `nimoutFull:true` if intentional"

  if result.parseErrors.len > 0:
    result.err = grInvalidSpec

  result.inCurrentBatch = isCurrentBatch(testamentData0, filename) or
                          result.unbatchable

  if not result.inCurrentBatch:
    result.err = grDisabled
