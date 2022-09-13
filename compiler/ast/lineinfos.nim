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
  std/[tables, hashes],
  compiler/utils/[ropes, pathutils]

from ast_types import
  PSym,     # Contextual details of the instantnation stack optionally refer to
            # the used symbol
  TLineInfo,
  FileIndex # Forward-declared to avoid cyclic dependencies

export FileIndex, TLineInfo

import reports

type
  CompilerVerbosity* = enum
    ## verbosity of the compiler, number is used as an array index and the
    ## string matches what's passed on the CLI.
    compVerbosityMin = (0, "0")
    compVerbosityDefault = (1, "1")
    compVerbosityHigh = (2, "2")
    compVerbosityMax = (3, "3")

const
  explanationsBaseUrl* = "https://nim-lang.github.io/Nim"
    # was: "https://nim-lang.org/docs" but we're now usually showing devel docs
    # instead of latest release docs.

proc createDocLink*(urlSuffix: string): string =
  # os.`/` is not appropriate for urls.
  result = explanationsBaseUrl
  if urlSuffix.len > 0 and urlSuffix[0] == '/':
    result.add urlSuffix
  else:
    result.add "/" & urlSuffix

proc computeNotesVerbosity(): tuple[
    main: array[CompilerVerbosity, ReportKinds],
    foreign: ReportKinds,
    base: ReportKinds
  ] =
  ## Create configuration sets for the default compilation report verbosity

  # Mandatory reports - cannot be turned off, present in all verbosity
  # settings
  result.base = (repErrorKinds + repInternalKinds)


  # Somewhat awkward handing - stack trace report cannot be error (because
  # actual error report must follow), so it is a hint-level report (can't
  # be debug because it is a user-facing, can't be "trace" because it is
  # not for compiler developers use only)
  result.base.incl {rvmStackTrace}

  when defined(debugOptions):
    # debug report for transition of the configuration options
    result.base.incl {rdbgOptionsPush, rdbgOptionsPop}

  when defined(nimVMDebugExecute):
    result.base.incl {
      rdbgVmExecTraceFull # execution of the generated code listings
    }

  when defined(nimVMDebugGenerate):
    result.base.incl {
      rdbgVmCodeListing    # immediately generated code listings
    }

  when defined(nimDebugUtils):
    # By default enable only semantic debug trace reports - other changes
    # might be put in there *temporarily* to aid the debugging.
    result.base.incl repDebugTraceKinds

  result.main[compVerbosityMax] =
    result.base + repWarningKinds + repHintKinds - {
    rsemObservableStores,
    rsemResultUsed,
    rsemAnyEnumConvert,
    rbackLinking,

    rbackLinking,
    rbackCompiling,
    rcmdLinking,
    rcmdCompiling,

    rintErrKind
  }

  if defined(release):
    result.main[compVerbosityMax].excl rintStackTrace

  result.main[compVerbosityHigh] = result.main[compVerbosityMax] - {
    rsemUninit,
    rsemExtendedContext,
    rsemProcessingStmt,
    rsemWarnGcUnsafe,
    rextConf,
  }

  result.main[compVerbosityDefault] = result.main[compVerbosityHigh] -
    repPerformanceHints -
    {
      rsemProveField,
      rsemErrGcUnsafe,
      rsemHintLibDependency,
      rsemGlobalVar,

      rintGCStats,
      rintMsgOrigin,

      rextPath,

      rlexSourceCodeFilterOutput,
    }

  result.main[compVerbosityMin] = result.main[compVerbosityDefault] - {
    rintSuccessX,
    rextConf,
    rsemProcessing,
    rsemPattern,
    rcmdExecuting,
    rbackLinking,
  }

  result.foreign = result.base + {
    rsemProcessing,
    rsemUserHint,
    rsemUserWarning,
    rsemUserHint,
    rsemUserWarning,
    rsemUserError,
    rintQuitCalled,
    rsemImplicitObjConv
  }

  for idx, n in @[
    result.foreign,
    # result.base,
    result.main[compVerbosityMax],
    result.main[compVerbosityHigh],
    result.main[compVerbosityDefault],
    result.main[compVerbosityMin],
  ]:
    assert rbackLinking notin n
    assert rsemImplicitObjConv in n, $idx
    assert rvmStackTrace in n, $idx

const
  NotesVerbosity* = computeNotesVerbosity()


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

proc `==`*(a, b: FileIndex): bool {.borrow.}

proc hash*(i: TLineInfo): Hash =
  hash (i.line.int, i.col.int, i.fileIndex.int)

proc raiseRecoverableError*(msg: string) {.noinline.} =
  raise newException(ERecoverableError, msg)

func isKnown*(info: TLineInfo): bool =
  ## Check if `info` represents valid source file location
  info != unknownLineInfo

type
  Severity* {.pure.} = enum ## VS Code only supports these three
    Hint, Warning, Error

const
  trackPosInvalidFileIdx* = FileIndex(-2) ## special marker so that no
  ## suggestions are produced within comments and string literals
  commandLineIdx* = FileIndex(-3)

type
  MsgConfig* = object ## does not need to be stored in the incremental cache
    trackPos*: TLineInfo
    trackPosAttached*: bool ## whether the tracking position was attached to
                            ## some close token.

    errorOutputs*: TErrorOutputs ## Allowed output streams for messages.
    # REFACTOR this field is mostly touched in sem for 'performance'
    # reasons - don't write out error messages when compilation failed,
    # don't generate list of call candidates when `compiles()` fails and so
    # on. This should be replaced with `.inTryExpr` or something similar,
    # and let the reporting hook deal with all the associated heuristics.

    msgContext*: seq[tuple[info: TLineInfo, detail: PSym]] ## \ Contextual
    ## information about instantiation stack - "template/generic
    ## instantiation of" message is constructed from this field. Right now
    ## `.detail` field is only used in the `sem.semMacroExpr()`,
    ## `seminst.generateInstance()` and `semexprs.semTemplateExpr()`. In
    ## all other cases this field is left empty (SemReport is `skUnknown`)
    reports*: ReportList ## Intermediate storage for the
    writtenSemReports*: ReportSet
    lastError*: TLineInfo
    filenameToIndexTbl*: Table[string, FileIndex]
    fileInfos*: seq[TFileInfo] ## Information about all known source files
    ## is stored in this field - full/relative paths, list of line etc.
    ## (For full list see `TFileInfo`)
    systemFileIdx*: FileIndex

proc initMsgConfig*(): MsgConfig =
  result.msgContext = @[]
  result.lastError = unknownLineInfo
  result.filenameToIndexTbl = initTable[string, FileIndex]()
  result.fileInfos = @[]
  result.errorOutputs = {eStdOut, eStdErr}
  result.filenameToIndexTbl["???"] = FileIndex(-1)
