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

# This is the rough spec I'm aiming for, most of it will default away
# notation:
#  * [] - are lists
#  * '' - are strings
#  * ?  - prefix means implied/default -- wasn't thorough about this

discard """
version: 1
description: ''
[compile.fail]
flags: ''
output: ?match ''
outputfull: ?match ''
[error]
msg: ?equal ''
line: ?equal 1
col: ?equal 2
[ensure]
timeout: 1.5
maxcodesize: 666
"""

discard """
version: 1
description: ''
[compile]
flags: ''
output-?match: ?match ''
outputfull: ?match ''
[compile.ensure]
timeout: 1.5
maxcodesize: 666

[exec]
args: ''
input: ''
sortoutput: true
[expect]
exitCode: 0
output: ?equal ''
stdout: ?equal ''
errout: ?equal ''
[exec.ensure]
timeout: 0.5
"""

from std/parseopt import initOptParser, next, CmdLineKind, remainingArgs
from std/strutils import normalize, count
from std/os import addFileExt, absolutePath, ExeExt, DirSep, dirExists,
                   walkDir, parentDir, lastPathPart, PathComponent
from std/re import startsWith, re

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

  StrCheck* = object
    ## operator and value to check something against
    op*: StrOp
    val*: string

  ExecutionStatus* {.pure.} = enum
    esNotStarted, esStarted, esRunning, esDone

  CSCompileExpectKind* {.pure.} = enum
    cscekNoOutput, cscekOutput, cscekOutputFull

  CSCompileExpect* = object
    errormsg*: StrCheck
    case kind*: CSCompileExpectKind
    of cscekOutput:
      output*: StrCheck
    of cscekOutputFull:
      outputfull*: StrCheck
    of cscekNoOutput:
      discard

  CSCompile* = object
    withFlags*: seq[string]
    alsoSortOutput*: bool
    expect*: CSCompileExpect
    ensureTimeout*: float
    ensureMaxCodeSize*: int

  CSExecute* = object
    withArgs*: seq[string]
    withInput*: seq[string]
    alsoSortOutput*: bool
    expect*: CSCExecuteExpect
    ensureTimeout*: float

  CompilerSpec* = object
    version*: int
    description*: string
    compile*: CSCompile
    execute*: CSExecute

  Execution* = object
    ## this is a bag of mutable state while a run is happening, try to keep it
    ## organized by lifecycle... or something
    status*: ExecutionStatus
    testFile*: string
    results*: Results
    testContent*: string        ## raw contents of this file
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

# -- more pure code

func processTestCompiler(ctx: var Context, file: string) =
  ctx.exec.testFile = file


# -- effectful execution machinery

proc handleFile(ctx: var Context, file: string) =
   if file.lastPathPart.startsWith(re"tc_"):
      # test we want to keep
      echo "found tests: ", file
      processTestCompiler(file)
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

