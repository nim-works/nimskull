#
#
#            Nim Tester
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import sequtils, parseutils, strutils, os, streams, parsecfg,
  tables, hashes, sets

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
  TTestAction* = enum
    actionRun = "run"
    actionCompile = "compile"
    actionReject = "reject"

  TOutputCheck* = enum
    ocIgnore = "ignore"
    ocEqual = "equal"
    ocSubstr = "substr"

  TResultEnum* = enum
    reNimcCrash,       # nim compiler seems to have crashed
    reMsgsDiffer,      # error messages differ
    reFilesDiffer,     # expected and given filenames differ
    reLinesDiffer,     # expected and given line numbers differ
    reOutputsDiffer,
    reExitcodesDiffer, # exit codes of program or of valgrind differ
    reTimeout,
    reInvalidPeg,
    reCodegenFailure,
    reCodeNotFound,
    reExeNotFound,
    reInstallFailed    # package installation failed
    reBuildFailed      # package building failed
    reDisabled,        # test is disabled
    reJoined,          # test is disabled because it was joined into the megatest
    reInvalidSpec      # test had problems to parse the spec
    reKnownIssue       # test has a known issue(s) and is expected to fail
    reSuccess          # test was successful

  TTarget* = enum
    targetC = "c"
    targetCpp = "cpp"
    targetObjC = "objc"
    targetJS = "js"

  SpecifiedTarget* = enum
    addTargetC = "c"
    remTargetC = "!c"
    addTargetCpp = "cpp"
    remTargetCpp = "!cpp"
    addTargetObjC = "objc"
    remTargetObjC = "!objc"
    addTargetJS = "js"
    remTargetJS = "!js"
    addTargetNative = "native"
    addCategoryTargets = "default"
    remCategoryTargets = "!default"

  InlineError* = object
    kind*: string
    msg*: string
    line*, col*: int

  ValgrindSpec* = enum
    disabled, enabled, leaking

  TSpec* = object
    # xxx make sure `isJoinableSpec` takes into account each field here.
    description*: string        ## document the purpose of the test
    action*: TTestAction
    file*: string ## File that test spec has been parsed from
    cmd*: string ## Command to execute for the test
    input*: string ## `stdin` input that will be piped into program after
                   ## it has been compiled.
    outputCheck*: TOutputCheck ## Kind of the output checking for the
                               ## executed compiled program.
    sortoutput*: bool ## Sort runtime output
    output*: string ## Expected runtime output
    line*, column*: int
    exitCode*: int
    msg*: string
    ccodeCheck*: seq[string] ## List of peg patterns that need to be
    ## searched for in the generated code. Used for backend code testing.
    maxCodeSize*: int ## Maximum allowed code size (in bytes) for the test.
    err*: TResultEnum
    inCurrentBatch*: bool
    targets*: set[TTarget]
    specifiedTargets*: set[SpecifiedTarget] ## targets as described by the spec
    matrix*: seq[string]
    nimoutSexp*: bool
    nimout*: string
    nimoutFull*: bool    ## whether nimout is all compiler output or a subset
    parseErrors*: string ## when the spec definition is invalid, this is
                         ## not empty.
    unjoinable*: bool
    unbatchable*: bool
      ## whether this test can be batchable via `NIM_TESTAMENT_BATCH`; only
      ## very few tests are not batchable; the ones that are not could be
      ## turned batchable by making the dependencies explicit
    useValgrind*: ValgrindSpec
    timeout*: float ## in seconds, fractions possible, but don't rely on
    ## much precision
    inlineErrors*: seq[InlineError] ## line information to error message
    debugInfo*: string ## debug info to give more context
    knownIssues*: seq[string] ## known issues to be fixed

  RetryContainer* = object
    ## Global object which contains information related to the --retry flag.
    ## See `var retryContainer`.
    retry*: bool  # true when --retry flag has been passed
    cats*: seq[string] # contains categories with failed tests
    names*: seq[(string, string)] # contains pair of failed test name and its target

# Exported global RetryContainer object.
var retryContainer* = RetryContainer(retry: false)

proc getCmd*(s: TSpec): string =
  ## Get runner command for a given test specification
  if s.cmd.len == 0:
    result = compilerPrefix & " $target --hints:on -d:testing --clearNimblePath --nimblePath:build/deps/pkgs $options $file"
  else:
    result = s.cmd

func ext*(t: TTarget): string {.inline.} =
  ## read-only field providing the extension string for the given target
  case t:
    of targetC:    "nim.c"
    of targetCpp:  "nim.cpp"
    of targetObjC: "nim.m"
    of targetJS:   "js"

func cmd*(t: TTarget): string {.inline.} =
  ## read-only field providing the command string for the given target
  case t:
    of targetC:    "c"
    of targetCpp:  "cpp"
    of targetObjC: "objc"
    of targetJS:   "js"

func defaultOptions*(a: TTarget): string {.inline.} =
  case a
  of targetJS: "-d:nodejs"
    # once we start testing for `nim js -d:nimbrowser` (eg selenium or similar),
    # we can adapt this logic; or a given js test can override with `-u:nodejs`.
  else: ""

when not declared(parseCfgBool):
  # candidate for the stdlib:
  proc parseCfgBool(s: string): bool =
    case normalize(s)
    of "y", "yes", "true", "1", "on": result = true
    of "n", "no", "false", "0", "off": result = false
    else: raise newException(ValueError, "cannot interpret as a bool: " & s)

const
  inlineErrorMarker = "#[tt."

proc extractErrorMsg(s: string; i: int; line: var int; col: var int; spec: var TSpec): int =
  ## Get position of the error message in input text `s`.
  result = i + len(inlineErrorMarker)
  inc col, len(inlineErrorMarker)
  var kind = ""
  while result < s.len and s[result] in IdentChars:
    kind.add s[result]
    inc result
    inc col

  var caret = (line, -1)

  template skipWhitespace =
    while result < s.len and s[result] in Whitespace:
      if s[result] == '\n':
        col = 1
        inc line
      else:
        inc col
      inc result

  skipWhitespace()
  if result < s.len and s[result] == '^':
    caret = (line-1, col)
    inc result
    inc col
    skipWhitespace()

  var msg = ""
  while result < s.len-1:
    if s[result] == '\n':
      inc result
      inc line
      col = 1
    elif s[result] == ']' and s[result+1] == '#':
      while msg.len > 0 and msg[^1] in Whitespace:
        setLen msg, msg.len - 1

      inc result
      inc col, 2
      if kind == "Error": spec.action = actionReject
      spec.unjoinable = true
      spec.inlineErrors.add InlineError(kind: kind, msg: msg, line: caret[0], col: caret[1])
      break
    else:
      msg.add s[result]
      inc result
      inc col

proc extractSpec(filename: string; spec: var TSpec): string =
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
    elif s.continuesWith(inlineErrorMarker, i):
      # Found `#[tt.` - extract it
      i = extractErrorMsg(s, i, line, col, spec)
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

proc parseTargets*(value: string): set[TTarget] =
  ## Get list of allowed run targets for the testament
  for v in value.normalize.splitWhitespace:
    result.incl:
      case v
      of "c":          targetC
      of "cpp", "c++": targetCpp
      of "objc":       targetObjC
      of "js":         targetJS
      else: raise newException(ValueError, "invalid target: '$#'" % v)

proc parseSpecifiedTargets*(value: string): set[SpecifiedTarget] =
  ## Get target specification
  for v in value.normalize.splitWhitespace:
    result.incl:
      case v
      of "c":            addTargetC
      of "cpp", "c++":   addTargetCpp
      of "objc":         addTargetObjC
      of "js":           addTargetJS
      of "native":       addTargetNative
      of "default":      addCategoryTargets
      of "!c":           remTargetC
      of "!cpp", "!c++": remTargetCpp
      of "!objc":        remTargetObjC
      of "!js":          remTargetJS
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

proc initSpec*(filename: string): TSpec =
  result.file = filename

proc isCurrentBatch*(testamentData: TestamentData; filename: string): bool =
  if testamentData.testamentNumBatch != 0:
    hash(filename) mod testamentData.testamentNumBatch == testamentData.testamentBatch
  else:
    true

proc parseSpec*(filename: string,
                catTargets: set[TTarget],
                nativeTarget: TTarget): TSpec =
  ## Extract and parse specification for a given file path
  result.file = filename

  when defined(windows):
    let cmpString = result.file.replace(r"\", r"/")
  else:
    let cmpString = result.file
  if retryContainer.retry and not retryContainer.names.anyIt(cmpString == it[0]):
    result.err = reDisabled
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
      of "joinable":
        result.unjoinable = not parseCfgBool(e.value)
      of "valgrind":
        when defined(linux) and sizeof(int) == 8:
          result.useValgrind = if e.value.normalize == "leaks": leaking
                               else: ValgrindSpec(parseCfgBool(e.value))
          result.unjoinable = true
          if result.useValgrind != disabled:
            result.outputCheck = ocSubstr
        else:
          # Windows lacks valgrind. Silly OS.
          # Valgrind only supports OSX <= 17.x
          result.useValgrind = disabled
      of "disabled":
        case e.value.normalize
        of "y", "yes", "true", "1", "on": result.err = reDisabled
        of "n", "no", "false", "0", "off": discard
        of "win", "windows":
          when defined(windows): result.err = reDisabled
        of "linux":
          when defined(linux): result.err = reDisabled
        of "bsd":
          when defined(bsd): result.err = reDisabled
        of "osx", "macosx": # xxx remove `macosx` alias?
          when defined(osx): result.err = reDisabled
        of "unix":
          when defined(unix): result.err = reDisabled
        of "posix":
          when defined(posix): result.err = reDisabled
        of "32bit":
          if sizeof(int) == 4:
            result.err = reDisabled
        of "freebsd":
          when defined(freebsd): result.err = reDisabled
        of "arm64":
          when defined(arm64): result.err = reDisabled
        of "i386":
          when defined(i386): result.err = reDisabled
        of "openbsd":
          when defined(openbsd): result.err = reDisabled
        of "netbsd":
          when defined(netbsd): result.err = reDisabled
        else:
          result.parseErrors.addLine "cannot interpret as a bool or platform name: ", e.value
      of "cmd":
        if e.value.startsWith("nim "):
          result.cmd = compilerPrefix & e.value[3..^1]
        else:
          result.cmd = e.value
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
          of addTargetCpp: result.targets.incl targetCpp
          of addTargetObjc: result.targets.incl targetObjC
          of addTargetJS: result.targets.incl targetJS
          of addTargetNative: result.targets.incl nativeTarget
          of addCategoryTargets: result.targets = result.targets + catTargets
          of remTargetC, remTargetCpp, remTargetObjc, remTargetJS,
             remCategoryTargets:
               discard
        
        if result.targets == {}:
          # nothing was specified, so assume the defaults
          result.targets = catTargets

        # do the removals next
        for st in result.specifiedTargets:
          case st
          of remTargetC: result.targets.excl targetC
          of remTargetCpp: result.targets.excl targetCpp
          of remTargetObjc: result.targets.excl targetObjC
          of remTargetJS: result.targets.excl targetJS
          of remCategoryTargets: result.targets = result.targets - catTargets
          of addTargetC, addTargetCpp, addTargetObjc, addTargetJS,
             addTargetNative, addCategoryTargets:
               discard
        
        # if nothing was specified, setup category defaults
        if result.targets == {}:
          result.parseErrors.addLine "Empty targets set, ", e.value

      of "matrix":
        for v in e.value.split(';'):
          result.matrix.add(v.strip)
      of "knownissue":
        case e.value.normalize
        of "n", "no", "false", "0": discard
        else:
            result.knownIssues.add e.value
            result.err = reKnownIssue
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

  if skips.anyIt(it in result.file):
    result.err = reDisabled
  if nimoutFound and result.nimout.len == 0 and not result.nimoutFull:
    result.parseErrors.addLine "empty `nimout` is vacuously true, use `nimoutFull:true` if intentional"

  if result.parseErrors.len > 0:
    result.err = reInvalidSpec

  result.inCurrentBatch = isCurrentBatch(testamentData0, filename) or result.unbatchable
  if not result.inCurrentBatch:
    result.err = reDisabled