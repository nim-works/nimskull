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

import ropes, tables, pathutils, hashes

from ast_types import
  PSym,     # Contextual details of the instantnation stack optionally refer to
            # the used symbol
  TLineInfo,
  FileIndex # Forward-declared to avoid cyclic dependencies

export FileIndex, TLineInfo

import reports

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
    main: array[0..3, ReportKinds],
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
  result.base.incl {rsemVmStackTrace}

  when defined(debugOptions):
    # debug report for transition of the configuration options
    result.base.incl {rdbgOptionsPush, rdbgOptionsPop}

  when defined(nimVMDebug):
    result.base.incl {
      rdbgVmExecTraceFull, # execution of the generated code listings
      rdbgVmCodeListing    # immediately generated code listings
    }

  when defined(nimDebugUtils):
    result.base.incl {
      rdbgTraceStart, # Begin report
      rdbgTraceStep, # in/out
      rdbgTraceLine,
      rdbgTraceEnd # End report
    }

  result.main[3] = result.base + repWarningKinds + repHintKinds - {
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
    result.main[3].excl rintStackTrace


  result.main[2] = result.main[3] - {
    rsemUninit,
    rsemExtendedContext,
    rsemProcessingStmt,
    rsemWarnGcUnsafe,
    rextConf,
  }

  result.main[1] = result.main[2] - {
    rsemProveField,
    rsemErrGcUnsafe,
    rextPath,
    rsemHintLibDependency,
    rsemGlobalVar,
    rintGCStats,
  }

  result.main[0] = result.main[1] - {
    rintSuccessX,
    rextConf,
    rsemProcessing,
    rsemPattern,
    rcmdExecuting,
    rbackLinking,
    rintMsgOrigin
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
    result.main[3],
    result.main[2],
    result.main[1],
    result.main[0],
  ]:
    assert rbackLinking notin n
    assert rsemImplicitObjConv in n, $idx
    assert rsemVmStackTrace in n, $idx


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
    dirty*: bool               ## for 'nimfix' / 'nimpretty' like tooling
    when defined(nimpretty):
      fullContent*: string

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

const
  InvalidFileIdx* = FileIndex(-1)
  unknownLineInfo* = TLineInfo(line: 0, col: -1, fileIndex: InvalidFileIdx)

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
