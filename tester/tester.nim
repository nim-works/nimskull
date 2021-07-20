#
#
#            Nim Tester
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This is a combination library and program.
##
## Library, when included by a test:
## * the API for which will turn the module into a CLI:
##  - default: when run will describe the test module
##  - can run the tests themselves
##  - can be run as part of a larger pipeline
##  - can be included as a module as part of a larger test program
## * provides access to the tester API for tests
##
## Program, when executing the binary produced:
## * can build, discover, describe, and run tests in different ways

## design
##
## - tester will find tests and categories based on `files system names` below
## - then it'll run them based on the test type, currently only compile
## 
## file system names:
## - tc_*.nim: test that is run through the compiler and possibly executed
## - .*|m_* and dir: ignore these
## - * and dir: these are categories, sub-directories are sub-categories
##
## Reference:
## * compiler testing survey paper:
##  * https://software-lab.org/publications/csur2019_compiler_testing.pdf

when false:
  # This is the rough spec I'm aiming for, most of it will default away
  discard """
  version: 1
  description: 'compiler failure example'
  action: reject

  [compile]
  flags: ''
  output: ''
  outputfull: ''
  exitCode: 1    # not sure
  timeout: 1.5
  maxcodesize: 666

  [error]
  msg: ''
  line: 1
  col: 2
  """

  discard """
  version: 1
  description: 'run success example'
  action: run       # default

  [compile]
  # see example above

  [exec]
  args: ''
  input: ''
  sortoutput: true

  [expect]
  output: ''
  exitCode: 0
  timeout: 0.5
  """

from std/parseopt import initOptParser, next, CmdLineKind, remainingArgs
from std/strutils import normalize, count, `%`, continuesWith, multiReplace
from std/os import addFileExt, absolutePath, ExeExt, DirSep, dirExists,
                   walkDir, parentDir, lastPathPart, PathComponent
from std/re import startsWith, re
from std/streams import newStringStream, Stream

const Usage = """Usage:
  tester [options] command [arguments]

Command:
  all            run all tests default
Arguments:
  arguments are passed to the compiler
Options:
  --nim:path     use a particular nim executable (default: $$PATH/nim)
"""

type
  Params* = object
    ## cli parameters
    nimLocation*: string

  Results* = object
    ## results for this test run
    total*: int
    passed*: int
    skipped*: int

  TestDir* = string  # xxx: considering making narrower set of procs

  ExecutionStatus* {.pure.} = enum
    esNotStarted, esStarted, esRunning, esDone

  CSCompile* = object
    flags*: string
    timeout*: float
    maxcodesize*: int

  CSActionKind* = enum
    csakReject, csakRun

  CompilerSpec* = object
    version*: int
    description*: string
    compile*: CSCompile
    case action*: CSActionKind
    of csakReject:
      msg*: string
      line*: int
      col*: int
    of csakRun:
      args*: string
      input*: string
      sortoutput*: bool
      output*: string
      exitCode*: int
      timeout*: float


  Execution* = object
    ## this is a bag of mutable state while a run is happening, try to keep it
    ## organized by lifecycle... or something
    status*: ExecutionStatus
    testFile*: string
    results*: Results
    # testContent*: string        ## raw contents of this file
    compilerSpec*: CompilerSpec

  Context* = object
    ## context for the tester and this particular run
    cmd*: string                ## cmd we're going to run
    compilerArgs*: seq[string]  ## arguments to be passed to the compiler
    testDir*: TestDir           ## director with the tests
    params: Params              ## params for testers
    exec: Execution             ## running state for an execution

const
  defaultTestDir = TestDir("newtests" & DirSep)
    ## defaults to newtests because keeping the old testament in place for now
  cmdDefault = "all"

  inlineErrorMarker = "#[tt."


# -- execution

proc extractErrorMsg(ctx: var Context, s: string; i: int; line: var int; col: var int): int =
  ## xxx: implement me
  0

func extractSpec(ctx: var Context, content: string): string =
  ## lifted from testament/spec
  const
    tripleQuote = "\"\"\""
    startMarker = "discard " & tripleQuote
    specStartMax = 10
      ## if it's lower than this, something is likely off
    notInitialized = -1
      ## used to compare `specStart` and `specEnd` below
  
  var
    i = 0          ## the index
    specStart = -1 ## index in string for where the spec starts
    specEnd = -1   ## index in string for where the spec ends
    line = 1       ## current line
    col = 1        ## current column
  while i < content.len:
    let
      startOfFile = i == 0
      prevNotSpace = not startOfFile and content[i-1] != ' '
        ## can't use '\n' because files can have a BOM
        ## (https://en.wikipedia.org/wiki/Byte_order_mark)
    if (startOfFile or prevNotSpace) and content.continuesWith(startMarker, i):
      if specStart != notInitialized:
        raise newException(
            ValueError,
            "Spec parsing error, duplicate `specMarker` found: " &
            $(ctx.exec.testFile, specStart, specEnd, line)
          )
      elif line > specStartMax:
        raise newException(
            ValueError,
            "Spec not found within $1 lines or indented; info $2" %
            [$specStartMax, $(ctx.exec.testFile, specStart, specEnd, line)]
          )
      else:
        i += startMarker.len
        specStart = i
    elif specStart != notInitialized and specEnd == notInitialized and
         content.continuesWith(tripleQuote, i):
      specEnd = i
      i += tripleQuote.len
    elif content[i] == '\n':
      inc line
      inc i
      col = 1
    elif content.continuesWith(inlineErrorMarker, i):
      i = extractErrorMsg(ctx, content, i, line, col)
    else:
      inc col
      inc i
  
  if specStart >= 0 and specEnd > specStart:
    result = content.substr(specStart, specEnd - 1).multiReplace({"'''": tripleQuote, "\\31": "\31"})
  elif specStart >= 0:
    raise newException(
        ValueError,
        "Spec `startMarker` found, but no trailing `tripleQuote`: " &
          $(ctx.exec.testFile, specStart, specEnd, line)
      )
  else:
    result = ""

proc getSpec(ctx: var Context, content: string): CompilerSpec =
  discard

proc handleFile(ctx: var Context, file: string) =
   if file.lastPathPart.startsWith(re"tc_"):
      # test we want to keep
      echo "found tests: ", file

      ctx.exec.testFile = file
      let testContent = readFile(file)
      ctx.exec.compilerSpec = ctx.getSpec(testContent)
   else:
      echo "ignoring: ", file

proc handleDirectory(ctx: var Context, testDir = ctx.testDir.string) =
  echo "handling directory: ", testDir

  for kind, ent in walkDir(testDir, relative = true):
    if kind in {pcLinkToFile, pcLinkToDir}:
      # skip symlinks
      continue

    if kind == pcFile:
      handleFile(ctx, ent)
    elif kind == pcDir and not ent.lastPathPart.startsWith(re"\.|m_"):
      handleDirectory(ctx)
    else:
      echo "ignoring dir: ", ent

proc execute*(ctx: var Context) =
  ## run the actual tests
  echo "this is it executing"
  ctx.exec.status = esStarted
  handleDirectory(ctx)

# -- main or when tester is an executable

proc main() =
  var
    params: Params ## where we squirel away params we find
    p = initOptParser() ## gotta parse dem opts
  
  # parse out the options
  while p.kind in {cmdLongOption, cmdShortOption}:
    p.next()
    case p.key.normalize
    of "nim":
      params.nimLocation = addFileExt(p.val.absolutePath, ExeExt)
    else:
      quit Usage
  
  # if we've got one or more then the current one better be a command
  let
    remainingArgCount = p.remainingArgs.len
    hasMoreArgs = remainingArgCount > 0
    isValidCmd = hasMoreArgs and p.kind == cmdArgument
    cmd =
      if isValidCmd:
        p.key.normalize
      else:
        cmdDefault
    compilerArgs =
      block:
        if hasMoreArgs:
          p.next()
        p.remainingArgs()

  # also initializes the results (all zero'd)
  var ctx = Context(
      cmd: cmd,
      compilerArgs: compilerArgs,
      params: params,
      testDir: defaultTestDir
    )

  execute(ctx)

when isMainModule:
  main()


# stuff below here is mostly for comedy's sake

# super fancy version that I might build some day
discard """
version: 100
compile:
  with:
    - flags: ['']
  also:
    sortoutput: false
  expect:
    - exitCode: ?equal 0
    - output: ?match ''
    - outputfull: ?match ''
    - errormsg:
      - msg: ?equal ''
      - line: ?equal 1
      - col: ?equal 2
      - detail: ?equal ''
  ensure:
    timeout: 1.5
    maxcodesize: 666
execute
  with:
    - args: ['']
    - input: ''
  also:
    sortoutput: true
  expect:
    - exitCode: 0
    - output: ?equal ''
    - stdout: ?equal ''
    - errout: ?equal ''
  ensure:
    timeout: 0.5
    maxcodesize: 666
"""

# import std/macros

# const testerAsRunner {.booldefine.}: bool = false

# when testerAsRunner:
#   proc main() =
#     echo "run test run"

#   when isMainModule:
#     main()
# else:
#   type
#     SpecKind* {.pure.} = enum
#       skCompilation

#     Spec* = object
#       name*: string
#       description*: string
#       case kind*: SpecKind
#       of skCompilation:
#         expectation: CompilationExpectation

#     ExpectationKind* {.pure.} = enum
#       ekCompilationFailure
    
#     Line* = int

#     CompilationExpectation* = object
#       case kind*: ExpectationKind
#       of ekCompilationFailure:
#         errorMsg*: string
#         file*:     string
#         line*:     Line

#   # proc compilationSpecBuilder(name, desc: string, e: CompilationExpectation): Spec =

#   macro compilationWillFail*(a: varargs[untyped]): untyped =
#     discard

#   macro compilationSpec*(name: string = "", desc: string,
#                          e: untyped): untyped =
#     quote:
#       when defined(testerTestRun):
#         const testerTestRun {.strdefine.}: string = "1"
#         echo "this would have been test run: ", testerTestRun
#       else:
#         block:
#           mixin compilationWillFail
#           from std/os import commandLineParams, extractFileName
#           let
#             # figure out whether we should run
#             cliParams = commandLineParams()
#             hasParams = cliParams.len > 0
#             hasRun = hasParams and cliParams[0] == "run"

#           proc compilationWillFail(errorMsg: string,
#                                    file: string = "",
#                                    line: int): CompilationExpectation =
#             let file =
#               if file == "":
#                 file.currentSourcePath().extractFileName()
#               else:
#                 file
#             CompilationExpectation(
#               kind: ekCompilationFailure,
#               errorMsg: errorMsg,
#               file: file,
#               line: line
#             )
#           proc compilationSpecBuilder(name, desc: string,
#                                       e: CompilationExpectation): Spec =
#             Spec(name: name, description: desc, kind: skCompilation,
#                  expectation: e)
            
#           let
#             # build up the spec data
#             spec = compilationSpecBuilder(`name`, `desc`, `e`)

#           if hasRun:
#             echo "this would have been the test running, run: ", testerTestRun, repr spec
#           else:
#             echo "this would be in library mode: ", repr spec

