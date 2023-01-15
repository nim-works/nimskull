#
#
#           The Nim Compiler
#        (c) Copyright 2018 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains the ``TMsgKind`` enum as well as the
## ``TLineInfo`` object.

import
  std/hashes,
  compiler/utils/[ropes, pathutils]

type
  FileIndex* = distinct int32

  TLineInfo* = object          ## This is designed to be as small as
    ## possible, because it is used in syntax nodes. We save space here by
    ## using two int16 and an int32. On 64 bit and on 32 bit systems this
    ## is only 8 bytes.

    line*: uint16
    col*: int16
    fileIndex*: FileIndex

  LineColPair* = tuple
    line: typeof(TLineInfo.line)
    col: typeof(TLineInfo.col)

const
  InvalidFileIdx* = FileIndex(-1)
  unknownLineInfo* = TLineInfo(line: 0, col: -1, fileIndex: InvalidFileIdx)
  trackPosInvalidFileIdx* = FileIndex(-2) ## special marker so that no
  ## suggestions are produced within comments and string literals
  commandLineIdx* = FileIndex(-3)

func clampLineCol*(line, col: int): LineColPair {.inline.} =
  if line < int high(uint16):
    result.line = uint16(line)
  else:
    result.line = high(uint16)
  if col < int high(int16):
    result.col = int16(col)
  else:
    result.col = -1

proc newLineInfo*(fileIndex: FileIndex, line, col: int): TLineInfo {.inline.} =
  result.fileIndex = fileIndex
  (result.line, result.col) = clampLineCol(line, col)

proc `==`*(a, b: FileIndex): bool {.borrow.}

proc hash*(i: TLineInfo): Hash =
  hash (i.line.int, i.col.int, i.fileIndex.int)

func isKnown*(info: TLineInfo): bool =
  ## Check if `info` represents valid source file location
  info != unknownLineInfo


type
  InstantiationInfo* = typeof(instantiationInfo())

template instLoc*(depth: int = -2): InstantiationInfo =
  ## grabs where in the compiler an error was instanced to ease debugging.
  ##
  ## whether to use full paths depends on --excessiveStackTrace compiler option.
  instantiationInfo(depth, fullPaths = compileOption"excessiveStackTrace")


type
  TFileInfo* = object
    fullPath*: AbsoluteFile    ## This is a canonical full filesystem path
    projPath*: RelativeFile    ## This is relative to the project's root
    shortName*: string         ## short name of the module
    quotedName*: Rope          ## cached quoted short name for codegen
                               ## purposes
    quotedFullName*: Rope      ## cached quoted full name for codegen
                               ## purposes

    lines*: seq[string]        ## the source code of the module used for
                               ## better error messages and embedding the
                               ## original source in the generated code

    dirtyFile*: AbsoluteFile   ## the file that is actually read into memory
                               ## and parsed; usually "" but is used
                               ## for 'nimsuggest'
    hash*: string              ## the checksum of the file
    dirty*: bool               ## for 'nimfix' like tooling

  TErrorOutput* = enum
    eStdOut
    eStdErr

  TErrorOutputs* = set[TErrorOutput]

  ERecoverableError* = object of ValueError
  ESuggestDone* = object of ValueError

proc raiseRecoverableError*(msg: string) {.noinline.} =
  raise newException(ERecoverableError, msg)


# Refactor: miscellaneous stuff bellow, not sure where to put this yet

type
  CompilerVerbosity* = enum
    ## verbosity of the compiler, number is used as an array index and the
    ## string matches what's passed on the CLI.
    compVerbosityMin = (0, "0")
    compVerbosityDefault = (1, "1")
    compVerbosityHigh = (2, "2")
    compVerbosityMax = (3, "3")


type
  Severity* {.pure.} = enum ## VS Code only supports these three
    Hint, Warning, Error


const
  explanationsBaseUrl* = "https://nim-works.github.io/nimskull"
    # was: "https://nim-lang.org/docs" but we're now usually showing devel docs
    # instead of latest release docs.

proc createDocLink*(urlSuffix: string): string =
  # os.`/` is not appropriate for urls.
  result = explanationsBaseUrl
  if urlSuffix.len > 0 and urlSuffix[0] == '/':
    result.add urlSuffix
  else:
    result.add "/" & urlSuffix
