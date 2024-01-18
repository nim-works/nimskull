#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/[strutils, os, tables, macros, times],
  std/private/miscdollars

import
  std/options as std_options

import
  compiler/utils/[ropes, pathutils, idioms],
  compiler/ast/[report_enums, reports, lineinfos, reports_internal],
  compiler/front/[options]

# TODO: the selective imports here identify the compile/interpret specific
#       parts for this module, and we can use that to break this up.
from compiler/ast/ast_types import
  PNode, TNodeKind, NodeId, `[]`,
  PAstDiag, AstDiagKind, AstDiagVmKind, AstDiagVmGenKind,
  PSym, TSymKind,
  SemTypeMismatch,
  TMagic

# TODO: `ReportContext` is used for "context setting", it happening in `msgs`
#       and it involving `ConfigRef`, all seems off. Likely a dependency that
#       can be simplified out, also the functionality itself could be ill
#       conceived.
from compiler/ast/reports_base_sem import ReportContext, ReportContextKind

# when you have data where it belongs, it's easy to see this stuff... should
# `msgs` really depend upon `SemReport`?
from compiler/ast/reports_sem import SemReport

export InstantiationInfo
export TErrorHandling

proc handleReport*(
    conf: ConfigRef,
    r: Report,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing) {.noinline.}

template toStdOrrKind(stdOrr): untyped =
  if stdOrr == stdout: stdOrrStdout else: stdOrrStderr

proc flushDot*(conf: ConfigRef) =
  ## safe to call multiple times
  # xxx: this proc is a bad idea, no need to have all sorts of callers to it
  let stdOrr = if optStdout in conf.globalOptions: stdout else: stderr
  let stdOrrKind = toStdOrrKind(stdOrr)
  if stdOrrKind in conf.lastMsgWasDot:
    conf.lastMsgWasDot.excl stdOrrKind
    write(stdOrr, "\n")

const gCmdLineInfo* = newLineInfo(commandLineIdx, 1, 1)

proc msgQuit*(x: int8) = quit x
proc msgQuit*(x: string) = quit x

proc suggestQuit*() =
  raise newException(ESuggestDone, "suggest done")

const
  # NOTE: currently line info line numbers start with 1,
  # but column numbers start with 0, however most editors expect
  # first column to be 1, so we need to +1 here
  ColOffset*   = 1

proc getInfoContextLen*(conf: ConfigRef): int = return conf.m.msgContext.len
proc setInfoContextLen*(conf: ConfigRef; L: int) = setLen(conf.m.msgContext, L)

proc pushInfoContext*(
    conf: ConfigRef;
    info: TLineInfo,
    detail: PSym = nil
  ) =
  ## Add entry to the message context information stack.
  conf.m.msgContext.add((info, detail))

proc popInfoContext*(conf: ConfigRef) =
  ## Remove one entry from the message context information stack
  setLen(conf.m.msgContext, conf.m.msgContext.len - 1)

proc getInfoContext*(conf: ConfigRef; index: int): TLineInfo =
  let i =
    if index < 0:
      conf.m.msgContext.len + index
    else:
      index

  if i >=% conf.m.msgContext.len:
    result = unknownLineInfo
  else:
    result = conf.m.msgContext[i].info

proc toProjPath*(conf: ConfigRef; fileIdx: FileIndex): string =
  if fileIdx.int32 < 0 or conf == nil:
    if fileIdx == commandLineIdx:
      commandLineDesc
    else:
      "???"
  else:
    conf[fileIdx].projPath.string

proc toFullPath*(conf: ConfigRef; fileIdx: FileIndex): string =
  if fileIdx.int32 < 0 or conf == nil:
    result = (if fileIdx == commandLineIdx: commandLineDesc else: "???")
  else:
    result = conf[fileIdx].fullPath.string

proc toReportLineInfo*(conf: ConfigRef, info: TLineInfo): ReportLineInfo =
  ReportLineInfo(
    file: conf.toFullPath(info.fileIndex),
     line: info.line, col: info.col)

proc setDirtyFile*(conf: ConfigRef; fileIdx: FileIndex; filename: AbsoluteFile) =
  assert fileIdx.int32 >= 0
  conf[fileIdx].dirtyFile = filename
  setLen conf[fileIdx].lines, 0

proc setHash*(conf: ConfigRef; fileIdx: FileIndex; hash: string) =
  assert fileIdx.int32 >= 0
  shallowCopy(conf[fileIdx].hash, hash)

proc getHash*(conf: ConfigRef; fileIdx: FileIndex): string =
  assert fileIdx.int32 >= 0
  shallowCopy(result, conf[fileIdx].hash)

proc toFullPathConsiderDirty*(conf: ConfigRef; fileIdx: FileIndex): AbsoluteFile =
  if fileIdx.int32 < 0:
    result = AbsoluteFile(if fileIdx == commandLineIdx: commandLineDesc else: "???")
  elif not conf[fileIdx].dirtyFile.isEmpty:
    result = conf[fileIdx].dirtyFile
  else:
    result = conf[fileIdx].fullPath

template toProjPath*(conf: ConfigRef; info: TLineInfo): string =
  toProjPath(conf, info.fileIndex)

template toFullPath*(conf: ConfigRef; info: TLineInfo): string =
  toFullPath(conf, info.fileIndex)

template toFullPathConsiderDirty*(conf: ConfigRef; info: TLineInfo): string =
  string toFullPathConsiderDirty(conf, info.fileIndex)

proc toFilenameOption*(conf: ConfigRef, fileIdx: FileIndex, opt: FilenameOption): string =
  case opt
  of foAbs: result = toFullPath(conf, fileIdx)
  of foRelProject: result = toProjPath(conf, fileIdx)
  of foCanonical:
    let absPath = toFullPath(conf, fileIdx)
    result = canonicalImportAux(conf, absPath.AbsoluteFile)
  of foName: result = toProjPath(conf, fileIdx).lastPathPart
  of foLegacyRelProj:
    let
      absPath = toFullPath(conf, fileIdx)
      relPath = toProjPath(conf, fileIdx)
    result = if (relPath.len > absPath.len) or (relPath.count("..") > 2):
               absPath
             else:
               relPath
  of foStacktrace:
    if optExcessiveStackTrace in conf.globalOptions:
      result = toFilenameOption(conf, fileIdx, foAbs)
    else:
      result = toFilenameOption(conf, fileIdx, foName)

proc formatPath*(conf: ConfigRef, path: string): string =
  ## Format absolute file path for error message reporting. If path is not
  ## registered in the `filenameToIndexTbl` and is not a path to the
  ## compiler source, return it unchanged. If configuration is nil also
  ## return path unchanged.
  if isNil(conf):
    return path

  if path in conf.m.filenameToIndexTbl:
    # Check if path is registered in filename table index - in that case
    # formatting is done using `FileInfo` data from the config.
    let id = conf.m.filenameToIndexTbl[path]
    result = toFilenameOption(conf, id, conf.filenameOption)
  else:
    # Path not registered in the filename table - most likely an
    # instantiation info report location
    when compileOption"excessiveStackTrace":
      # instLoc(), when `--excessiveStackTrace` is used, generates full
      # paths that /might/ need to be filtered if `--filenames:canonical`.
      const compilerRoot = currentSourcePath().parentDir().parentDir()
      if conf.filenameOption == foCanonical and
         path.startsWith(compilerRoot):
        result = path[(compilerRoot.len + 1) .. ^1]
      else:
        result = path
    else:
      result = path

proc toMsgFilename*(conf: ConfigRef; fileIdx: FileIndex): string =
  toFilenameOption(conf, fileIdx, conf.filenameOption)

template toMsgFilename*(conf: ConfigRef; info: TLineInfo): string =
  toMsgFilename(conf, info.fileIndex)

proc toLinenumber*(info: TLineInfo): int {.inline.} =
  result = int info.line

proc toColumn*(info: TLineInfo): int {.inline.} =
  result = info.col

proc toFileLineCol*(info: InstantiationInfo): string {.inline.} =
  result.toLocation(info.filename, info.line, info.column + ColOffset)

proc toFileLineCol*(conf: ConfigRef; info: TLineInfo): string {.inline.} =
  ## Construct `file(line, col)` string from report location information
  result.toLocation(
    toMsgFilename(conf, info), info.line.int, info.col.int + ColOffset)

proc toReportPoint*(
  conf: ConfigRef; info: TLineInfo): ReportLineInfo {.inline.} =
  ## Construct report location instance based on the information from
  ## `info`

  ReportLineInfo(
    file: toMsgFilename(conf, info),
    line: info.line,
    col: info.col + ColOffset)

proc `$`*(conf: ConfigRef; info: TLineInfo): string = toFileLineCol(conf, info)

proc `$`*(info: TLineInfo): string {.error.} = discard

proc msgWrite*(conf: ConfigRef; s: string, flags: MsgFlags = {}) =
  ## Writes given message `s`tring to stderr by default.
  ## If ``--stdout`` option is given, writes to stdout instead. If message hook
  ## is present, then it is used to output message rather than stderr/stdout.
  ## This behavior can be altered by given optional `flags`.
  ##
  ## This is used for 'nim dump' etc. where we don't have nimsuggest
  ## support.
  ##
  ## This procedure is used as a default implementation of the
  ## `ConfigRef.writeHook`.
  let
    sep = if msgNoUnitSep notin flags: conf.unitSep else: ""
    isDot = s == "."

  template newLineIfRequired(stdOrr: untyped) =
    let stdOrrKind = toStdOrrKind(stdOrr)
    if stdOrrKind in conf.lastMsgWasDot and not isDot:
      write(stdOrr, "\n")
    if isDot:
      conf.lastMsgWasDot.incl stdOrrKind
    else:
      conf.lastMsgWasDot.excl stdOrrKind

  if optStdout in conf.globalOptions or msgStdout in flags:
    if eStdOut in conf.m.errorOutputs:
      newLineIfRequired(stdout)
      write(stdout, s)
      write(stdout, sep)
      flushFile(stdout)
  else:
    if eStdErr in conf.m.errorOutputs:
      newLineIfRequired(stderr)
      write(stderr, s)
      write(stderr, sep)

      # On Windows stderr is fully-buffered when piped, regardless of C std.
      when defined(windows):
        flushFile(stderr)

proc quit(conf: ConfigRef; withTrace: bool) {.gcsafe.} =
  if conf.isDefined("nimDebug"):
    quitOrRaise(conf)
  elif defined(debug) or withTrace or conf.hasHint(rintStackTrace):
    {.gcsafe.}:
      if stackTraceAvailable():
        discard conf.report(InternalReport(
          kind: rintStackTrace,
          trace: getStackTraceEntries()))
      else:
        discard conf.report(InternalReport(
          kind: rintMissingStackTrace))
  quit 1

proc errorActions(
    conf: ConfigRef,
    report: Report,
    eh: TErrorHandling
  ): tuple[action: TErrorHandling, withTrace: bool] =
  result = (doNothing, false)
  if conf.isCompilerFatal(report):
    # Fatal message such as ICE (internal compiler), errFatal,
    result = (doAbort, true)
  elif conf.isCodeError(report):
    # Regular code error
    inc(conf.errorCounter)
    conf.exitcode = 1'i8

    if conf.errorCounter >= conf.errorMax:
      # only really quit when we're not in the new 'nim check --def' mode:
      if conf.ideCmd == ideNone:
        result = (doAbort, false)
    elif eh == doAbort and conf.cmd != cmdIdeTools:
      result = (doAbort, false)
    elif eh == doRaise:
      result = (doRaise, false)

proc getContext*(conf: ConfigRef; lastinfo: TLineInfo): seq[ReportContext] =
  ## Get list of context context entries from the current message context
  ## information. Context messages can later be used in the
  ## `SemReport.context` field
  var info = lastinfo
  for i in 0 ..< conf.m.msgContext.len:
    let context = conf.m.msgContext[i]
    if context.info != lastinfo and context.info != info:
      if context.detail.isNil or context.detail.kind == skUnknown:
        result.add ReportContext(
          kind: sckInstantiationFrom,
          location: context.info)
      else:
        result.add ReportContext(
          kind: sckInstantiationOf,
          location: context.info,
          entry: context.detail)

    info = context.info

proc numLines*(conf: ConfigRef, fileIdx: FileIndex): int =
  ## xxx there's an off by 1 error that should be fixed; if a file ends with "foo" or "foo\n"
  ## it will return same number of lines (ie, a trailing empty line is discounted)
  result = conf[fileIdx].lines.len
  if result == 0:
    try:
      for line in lines(toFullPathConsiderDirty(conf, fileIdx).string):
        conf[fileIdx].lines.add line
    except IOError:
      discard
    result = conf[fileIdx].lines.len

proc sourceLine*(conf: ConfigRef; i: TLineInfo): string =
  ## 1-based index (matches editor line numbers); 1st line is for i.line = 1
  ## last valid line is `numLines` inclusive
  if i.fileIndex.int32 < 0: return ""
  let num = numLines(conf, i.fileIndex)
  # can happen if the error points to EOF:
  if i.line.int > num: return ""

  result = conf[i.fileIndex].lines[i.line.int - 1]

proc getSurroundingSrc*(conf: ConfigRef; info: TLineInfo): string =
  if conf.hasHint(rintSource) and info != unknownLineInfo:
    const indent = "  "
    result = "\n" & indent & $sourceLine(conf, info)
    if info.col >= 0:
      result.add "\n" & indent & spaces(info.col) & '^'

# xxx: All the SemReport stuff needs to go, it should just be the sem layer
#      defined/provided diagnostics/etc that we shouldn't muck with. The
#      code below is a temporary bridge to work around this until fixed.
func astDiagVmToLegacyReportKind*(
  evt: AstDiagVmKind
  ): ReportKind {.inline.} =
  case evt
  of adVmOpcParseExpectedExpression: rvmOpcParseExpectedExpression
  of adVmUserError: rvmUserError
  of adVmUnhandledException: rvmUnhandledException
  of adVmCannotCast: rvmCannotCast
  of adVmCannotModifyTypechecked: rvmCannotModifyTypechecked
  of adVmNilAccess: rvmNilAccess
  of adVmAccessOutOfBounds: rvmAccessOutOfBounds
  of adVmAccessTypeMismatch: rvmAccessTypeMismatch
  of adVmAccessNoLocation: rvmAccessNoLocation
  of adVmErrInternal: rvmErrInternal
  of adVmIndexError: rvmIndexError
  of adVmOutOfRange: rvmOutOfRange
  of adVmOverOrUnderflow: rvmOverOrUnderflow
  of adVmDivisionByConstZero: rvmDivisionByConstZero
  of adVmArgNodeNotASymbol: rvmNodeNotASymbol
  of adVmNodeNotASymbol: rvmNodeNotASymbol
  of adVmNodeNotAProcSymbol: rvmNodeNotAProcSymbol
  of adVmIllegalConv: rvmIllegalConv
  of adVmIllegalConvFromXToY: rvmIllegalConvFromXToY
  of adVmMissingCacheKey: rvmMissingCacheKey
  of adVmCacheKeyAlreadyExists: rvmCacheKeyAlreadyExists
  of adVmFieldNotFound: rvmFieldNotFound
  of adVmNotAField: rvmNotAField
  of adVmFieldUnavailable: rvmFieldInavailable
  of adVmCannotSetChild: rvmCannotSetChild
  of adVmCannotAddChild: rvmCannotAddChild
  of adVmCannotGetChild: rvmCannotGetChild
  of adVmNoType: rvmNoType
  of adVmTooManyIterations: rvmTooManyIterations

func astDiagVmGenToLegacyReportKind*(
  diag: AstDiagVmGenKind
  ): ReportKind {.inline.} =
  case diag
  of adVmGenMissingImportcCompleteStruct: rvmMissingImportcCompleteStruct
  of adVmGenTooManyRegistersRequired: rvmTooManyRegistersRequired
  of adVmGenNotUnused: rvmNotUnused
  of adVmGenTooLargeOffset: rvmTooLargetOffset
  of adVmGenCodeGenUnhandledMagic: rvmCannotGenerateCode
  of adVmGenCannotCast: rvmCannotCast
  of adVmGenCannotEvaluateAtComptime: rvmCannotEvaluateAtComptime
  of adVmGenCannotImportc: rvmCannotImportc
  of adVmGenCannotCallMethod: rvmCannotCallMethod

func astDiagToLegacyReportKind*(
  diag: AstDiagKind,
  vmGenDiag: Option[AstDiagVmGenKind] = none(AstDiagVmGenKind),
  vmEvent: Option[AstDiagVmKind] = none(AstDiagVmKind)
  ): ReportKind {.inline.} =
  ## with the introduction of `adSemDefNameSym` style diagnostics, this
  ## function is no longer all that sensible. `AstDiagKind` will move towards
  ## very broad categories and they'll no longer map to "reports".
  case diag
  of adWrappedError: rsemWrappedError
  of adSemTypeMismatch: rsemTypeMismatch
  of adSemTypeNotAllowed: rsemTypeNotAllowed
  of adSemUndeclaredIdentifier: rsemUndeclaredIdentifier
  of adSemConflictingExportnims: rsemConflictingExportnims
  of adSemAmbiguousIdent: rsemAmbiguousIdent
  of adSemAmbiguousIdentWithCandidates: rsemAmbiguousIdentWithCandidates
  of adSemExpectedIdentifier: rsemExpectedIdentifier
  of adSemExpectedIdentifierInExpr: rsemExpectedIdentifierInExpr
  of adSemExpectedIdentifierWithExprContext: rsemExpectedIdentifierWithExprContext
  of adSemExpectedIdentifierQuoteLimit: rsemExpectedIdentifierQuoteLimit
  of adSemOnlyDeclaredIdentifierFoundIsError: rsemOnlyDeclaredIdentifierFoundIsError
  of adSemModuleAliasMustBeIdentifier: rsemModuleAliasMustBeIdentifier
  of adSemCannotImportItself: rsemCannotImportItself
  of adSemInvalidPragma: rsemInvalidPragma
  of adSemIllegalCustomPragma: rsemIllegalCustomPragma
  of adSemExternalLocalNotAllowed: rsemExternalLocalNotAllowed
  of adSemStringLiteralExpected: rsemStringLiteralExpected
  of adSemIntLiteralExpected: rsemIntLiteralExpected
  of adSemOnOrOffExpected: rsemOnOrOffExpected
  of adSemCallconvExpected: rsemCallconvExpected
  of adSemUnknownExperimental: rsemUnknownExperimental
  of adSemWrongIdent: rsemWrongIdent
  of adSemPragmaOptionExpected: rsemPragmaOptionExpected
  of adSemUnexpectedPushArgument: rsemUnexpectedPushArgument
  of adSemMismatchedPopPush: rsemMismatchedPopPush
  of adSemExcessiveCompilePragmaArgs: rsemExcessiveCompilePragmaArgs
  of adSemEmptyAsm: rsemEmptyAsm
  of adSemAsmEmitExpectsStringLiteral: rsemIllformedAst
  of adSemLinePragmaExpectsTuple: rsemLinePragmaExpectsTuple
  of adSemRaisesPragmaExpectsObject: rsemRaisesPragmaExpectsObject
  of adSemLocksPragmaExpectsList: rsemLocksPragmaExpectsList
  of adSemLocksPragmaBadLevelRange: rsemLocksPragmaBadLevel
  of adSemLocksPragmaBadLevelString: rsemLocksPragmaBadLevel
  of adSemBorrowPragmaNonDot: rsemBorrowPragmaNonDot
  of adSemInvalidExtern: rsemInvalidExtern
  of adSemMisplacedEffectsOf: rsemMisplacedEffectsOf
  of adSemMissingPragmaArg: rsemMissingPragmaArg
  of adSemCannotPushCast: rsemCannotPushCast
  of adSemCastRequiresStatement: rsemCastRequiresStatement
  of adSemPragmaRecursiveDependency: rsemPragmaRecursiveDependency
  of adSemImportjsRequiresJs: rsemImportjsRequiresJs
  of adSemBitsizeRequires1248: rsemBitsizeRequires1248
  of adSemAlignRequiresPowerOfTwo: rsemAlignRequiresPowerOfTwo
  of adSemNoReturnHasReturn: rsemNoReturnHasReturn
  of adSemMisplacedDeprecation: rsemMisplacedDeprecation
  of adSemCustomUserError: rsemCustomUserError
  of adSemFatalError: rsemFatalError
  of adSemNoUnionForJs: rsemNoUnionForJs
  of adSemBitsizeRequiresPositive: rsemBitsizeRequiresPositive
  of adSemExperimentalRequiresToplevel: rsemExperimentalRequiresToplevel
  of adSemImplicitPragmaError: rsemImplicitPragmaError
  of adSemPragmaDynlibRequiresExportc: rsemPragmaDynlibRequiresExportc
  of adSemIncompatibleDefaultExpr: rsemIncompatibleDefaultExpr
  of adSemWrongNumberOfArguments: rsemWrongNumberOfArguments
  of adVmUnsupportedNonNil: rvmUnsupportedNonNil
  of adVmDerefNilAccess: rvmNilAccess
  of adVmDerefAccessOutOfBounds: rvmAccessOutOfBounds
  of adVmDerefAccessTypeMismatch: rvmAccessTypeMismatch
  of adCyclicTree: rsemCyclicTree
  of adVmGenError:
    assert vmGenDiag.isSome
    astDiagVmGenToLegacyReportKind(vmGenDiag.unsafeGet)
  of adVmError:
    assert vmEvent.isSome
    astDiagVmToLegacyReportKind(vmEvent.unsafeGet)
  of adVmQuit: rvmQuit
  of adSemUnavailableLocation: rsemUnavailableLocation
  of adSemRawTypeMismatch: rsemRawTypeMismatch
  of adSemExpressionCannotBeCalled: rsemExpressionCannotBeCalled
  of adSemCallTypeMismatch: rsemCallTypeMismatch
  of adSemCallNotAProcOrField: rsemCallNotAProcOrField
  of adSemImplicitDotCallNotAProcOrField: rsemCallNotAProcOrField
  of adSemCallInCompilesContextNotAProcOrField: rsemCompilesReport
  of adSemUndeclaredField: rsemUndeclaredField
  of adSemCannotInstantiate: rsemCannotInstantiate
  of adSemWrongNumberOfGenericParams: rsemWrongNumberOfGenericParams
  of adSemCalleeHasAnError: rsemCalleeHasAnError
  of adSemExpressionHasNoType: rsemExpressionHasNoType
  of adSemTypeExpected: rsemTypeExpected
  of adSemIllformedAst: rsemIllformedAst
  of adSemIllformedAstExpectedPragmaOrIdent: rsemIllformedAst
  of adSemIllformedAstExpectedOneOf: rsemIllformedAst
  of adSemImplementationExpected: rsemImplementationExpected
  of adSemImplementationNotAllowed: rsemImplementationNotAllowed
  of adSemInvalidExpression: rsemInvalidExpression
  of adSemExpectedNonemptyPattern: rsemExpectedNonemptyPattern
  of adSemInvalidControlFlow: rsemInvalidControlFlow
  of adSemExpectedLabel: rsemExpectedLabel
  of adSemForExpectedIterator: rsemForExpectsIterator
  of adSemContinueCannotHaveLabel: rsemContinueCannotHaveLabel
  of adSemUseOrDiscardExpr: rsemUseOrDiscardExpr
  of adSemCannotConvertToRange: rsemCannotConvertToRange
  of adSemProveInit: rsemProveInit
  of adSemCannotInferTypeOfLiteral: rsemCannotInferTypeOfLiteral
  of adSemProcHasNoConcreteType: rsemProcHasNoConcreteType
  of adSemPragmaDisallowedForTupleUnpacking: rsemPragmaDisallowedForTupleUnpacking
  of adSemIllegalCompileTime: rsemIllegalCompileTime
  of adSemDifferentTypeForReintroducedSymbol: rsemDifferentTypeForReintroducedSymbol
  of adSemThreadvarCannotInit: rsemThreadvarCannotInit
  of adSemTypeKindMismatch: rsemTypeKindMismatch
  of adSemWrongNumberOfVariables: rsemWrongNumberOfVariables
  of adSemLetNeedsInit: rsemLetNeedsInit
  of adSemConstExpressionExpected: rsemConstExpressionExpected
  of adSemSelectorMustBeOfCertainTypes: rsemSelectorMustBeOfCertainTypes
  of adSemInvalidPragmaBlock: rsemInvalidPragmaBlock
  of adSemConceptPredicateFailed: rsemConceptPredicateFailed
  of adSemDotOperatorsNotEnabled: rsemEnableDotOperatorsExperimental
  of adSemCallOperatorsNotEnabled: rsemEnableCallOperatorExperimental
  of adSemUnexpectedPattern: rsemUnexpectedPattern
  of adSemConstantOfTypeHasNoValue: rsemConstantOfTypeHasNoValue
  of adSemTypeConversionArgumentMismatch: rsemTypeConversionArgumentMismatch
  of adSemUnexpectedEqInObjectConstructor: rsemUnexpectedEqInObjectConstructor
  of adSemIllegalConversion: rsemIllegalConversion
  of adSemCannotBeConvertedTo: rsemCannotBeConvertedTo
  of adSemCannotCastToNonConcrete: rsemCannotCastToNonConcrete
  of adSemCannotCastTypes: rsemCannotCastTypes
  of adSemMagicExpectTypeOrValue: rsemExpectedTypeOrValue
  of adSemLowHighInvalidArgument: rsemInvalidArgumentFor
  of adSemIsOperatorTakes2Args: rsemIsOperatorTakes2Args
  of adSemUnknownIdentifier: rsemUnknownIdentifier
  of adSemInvalidTupleConstructorKey: rsemInvalidTupleConstructor
  of adSemNoTupleTypeForConstructor: rsemNoTupleTypeForConstructor
  of adSemExpectedOrdinalArrayIdx: rsemExpectedOrdinal
  of adSemIndexOutOfBounds: rsemIndexOutOfBounds
  of adSemInvalidOrderInArrayConstructor: rsemInvalidOrderInArrayConstructor
  of adSemStackEscape: rsemStackEscape
  of adSemVarForOutParamNeeded: rsemVarForOutParamNeeded
  of adSemExprHasNoAddress: rsemExprHasNoAddress
  of adSemExpectedOrdinal: rsemExpectedOrdinal
  of adSemConstExprExpected: rsemConstExprExpected
  of adSemExpectedRangeType: rsemExpectedRange
  of adSemExpectedObjectForOf: rsemExpectedObjectForOf
  of adSemCannotBeOfSubtype: rsemCannotBeOfSubtype
  of adSemRecursiveDependencyIterator: rsemRecursiveDependencyIterator
  of adSemCallIndirectTypeMismatch: rsemCallIndirectTypeMismatch
  of adSemSystemNeeds: rsemSystemNeeds
  of adSemDisallowedNilDeref: rsemDisallowedNilDeref
  of adSemCannotDeref: rsemCannotDeref
  of adSemInvalidTupleSubscript: rsemInvalidTupleSubscript
  of adSemLocalEscapesStackFrame: rsemLocalEscapesStackFrame
  of adSemImplicitAddrIsNotFirstParam: rsemImplicitAddrIsNotFirstParam
  of adSemCannotAssignTo: rsemCannotAssignTo
  of adSemYieldExpectedTupleConstr: rsemYieldExpectedTupleConstr
  of adSemCannotReturnTypeless: rsemCannotReturnTypeless
  of adSemExpectedValueForYield: rsemExpectedValueForYield
  of adSemNamedExprExpected: rsemNamedExprExpected
  of adSemFieldInitTwice: rsemFieldInitTwice
  of adSemDisallowedTypedescForTupleField: rsemDisallowedTypedescForTupleField
  of adSemNamedExprNotAllowed: rsemNamedExprNotAllowed
  of adSemCannotMixTypesAndValuesInTuple: rsemCannotMixTypesAndValuesInTuple
  of adSemNoReturnTypeDeclared: rsemNoReturnTypeDeclared
  of adSemReturnNotAllowed: rsemReturnNotAllowed
  of adSemFieldAssignmentInvalid: rsemFieldAssignmentInvalid
  of adSemFieldNotAccessible: rsemFieldNotAccessible
  of adSemObjectRequiresFieldInit: rsemObjectRequiresFieldInit
  of adSemObjectRequiresFieldInitNoDefault: rsemObjectRequiresFieldInitNoDefault
  of adSemExpectedObjectType: rsemExpectedObjectType
  of adSemExpectedObjectOfType: rsemExpectedObjectType
  of adSemObjectDoesNotHaveDefaultValue: rsemObjectDoesNotHaveDefaultValue
  of adSemDistinctDoesNotHaveDefaultValue: rsemDistinctDoesNotHaveDefaultValue
  of adSemFoldRangeCheckForLiteralConversionFailed: rsemCantConvertLiteralToRange
  of adSemIndexOutOfBoundsStatic: rsemStaticOutOfBounds
  of adSemStaticFieldNotFound: rsemStaticFieldNotFound
  of adSemInvalidIntDefine: rsemInvalidIntdefine
  of adSemInvalidBoolDefine: rsemInvalidBooldefine
  of adSemFoldOverflow: rsemSemfoldOverflow
  of adSemFoldDivByZero: rsemSemfoldDivByZero
  of adSemFoldCannotComputeOffset: rsemCantComputeOffsetof
  of adSemDefNameSym: rsemExpectedIdentifier
  of adSemCompilerOptionInvalid: rsemCompilerOptionInvalid
  of adSemDeprecatedCompilerOpt: rsemDeprecatedCompilerOpt
  of adSemCompilerOptionArgInvalid: rsemCompilerOptionArgInvalid
  of adSemDeprecatedCompilerOptArg: rsemDeprecatedCompilerOptArg

func astDiagToLegacyReportKind*(diag: PAstDiag): ReportKind {.inline.} =
  case diag.kind
  of adVmError:
    astDiagVmToLegacyReportKind(diag.vmErr.kind)
  of adVmGenError:
    astDiagVmGenToLegacyReportKind(diag.vmGenErr.kind)
  else:
    astDiagToLegacyReportKind(diag.kind)

proc report*(conf: ConfigRef, node: PNode): TErrorHandling =
  ## Write out report from the nkError node
  # xxx: legacy report temporarily here until we can rip it out
  assert node.kind == nkError
  return conf.report(conf.astDiagToLegacyReport(conf, node.diag))

proc fillReportAndHandleVmTrace(c: ConfigRef, r: var Report,
                                reportFrom: InstantiationInfo) =
  if r.category in { repSem, repVM } and r.location.isSome():
    r.context = c.getContext(r.location.get())

  if r.category == repVM and r.vmReport.trace != nil:
    handleReport(c, wrap(r.vmReport.trace[]), reportFrom)

proc handleReport*(
    conf: ConfigRef,
    r: Report,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing) {.noinline.} =
  ## Takes the report `r` and handles it. If the report is "enabled" according
  ## to the active configuration, it is passed to the active report hook and,
  ## if the report corresponds to an error, error handling is performed.
  ## `eh` is currently only a suggestion, and it is sometimes ignored depending
  ## on the currently active configuration.
  var rep = r
  rep.reportFrom = toReportLineInfo(reportFrom)

  if not conf.isEnabled(rep):
    # the report is disabled -> neither invoke the report hook nor perform
    # error handling
    return

  var userAction = doNothing
  case writabilityKind(conf, rep)
  of writeDisabled:
    discard "don't invoke the hook"
  of writeEnabled:
    # go through the report hook
    fillReportAndHandleVmTrace(conf, rep, reportFrom)
    userAction = conf.report(rep)
  of writeForceEnabled:
    # also go through the report hook, but temporarily override ``writeln``
    # with something that always echoes something
    fillReportAndHandleVmTrace(conf, rep, reportFrom)
    let oldHook = conf.writelnHook
    conf.writelnHook = proc (conf: ConfigRef, msg: string, flags: MsgFlags) =
      echo msg

    userAction = conf.report(rep)
    conf.writelnHook = oldHook

  # ``errorActions`` also increments the error counter, so make sure to always
  # call it
  var (action, trace) = errorActions(conf, rep, eh)

  # decide what to do, based on the hook-provided action and the computed
  # action. The more severe handling out of the two wins
  case userAction
  of doAbort:
    # a hook-requested abort always overrides the computed handling
    (action, trace) = (doAbort, false)
  of doRaise:
    case action
    of doRaise, doAbort:
      discard "a hook-requested raise doesn't override an abort"
    of doNothing, doDefault:
      (action, trace) = (doRaise, false)
  of doNothing, doDefault:
    discard "use the computed strategy"

  # now perform the selected action:
  case action
  of doAbort:   quit(conf, trace)
  of doRaise:   raiseRecoverableError("report")
  of doNothing: discard
  of doDefault: unreachable(
    "Default error handing action must be turned into ignore/raise/abort")

template globalAssert*(
    conf: ConfigRef;
    cond: untyped, info: TLineInfo = unknownLineInfo, arg = "") =
  ## avoids boilerplate
  if not cond:
    var arg2 = "'$1' failed" % [astToStr(cond)]
    if arg.len > 0: arg2.add "; " & astToStr(arg) & ": " & arg
    handleReport(conf, info, errGenerated, arg2, doRaise, instLoc())

template fatalReport*(conf: ConfigRef, info: TLineInfo, report: ReportTypes) =
  # this works around legacy reports stupidity
  handleReport(conf, wrap(report, instLoc(), info), instLoc(), doAbort)

template globalReport*(
  conf: ConfigRef; info: TLineInfo, report: ReportTypes) =
  ## `local` means compilation keeps going until errorMax is reached (via
  ## `doNothing`), `global` means it stops.
  handleReport(
    conf, wrap(report, instLoc(), info), instLoc(), doRaise)

template globalReport*(conf: ConfigRef, report: ReportTypes) =
  handleReport(
    conf, wrap(report, instLoc()), instLoc(), doRaise)

template localReport*(conf: ConfigRef; info: TLineInfo, report: ReportTypes) =
  {.line.}:
    handleReport(
      conf, wrap(report, instLoc(), info), instLoc(), doNothing)

template localReport*(conf: ConfigRef; node: PNode, report: SemReport) =
  var tmp = report
  if isNil(tmp.ast):
    tmp.ast = node
  handleReport(
    conf, wrap(tmp, instLoc(), node.info), instLoc(), doNothing)

proc temporaryStringError*(conf: ConfigRef, info: TLineInfo, text: string) =
  assert false

template localReport*(conf: ConfigRef, report: ReportTypes) =
  handleReport(
    conf, wrap(report, instLoc()), instLoc(), doNothing)

template localReport*(conf: ConfigRef, report: Report) =
  handleReport(conf, report, instLoc(), doNothing)

# xxx: `internalError` and `internalAssert` in conjunction with `handleReport`,
#      and the whole concept of "reports" indicating error handling action at a
#      callsite, is *terrible*. While it will result in the compiler exiting,
#      it is currently implemented very indirectly, through
#      ``isCompilerFatal``.

proc doInternalUnreachable*(conf: ConfigRef, info: TLineInfo, msg: string,
                            instLoc: InstantiationInfo) {.noreturn, inline.} =
  ## this proc firewalls other code from legacy reports, used in conjunction
  ## with the `internalError` templates
  let
    intRep = InternalReport(kind: rintUnreachable, msg: msg)
    rep =
      if info == unknownLineInfo:
        wrap(intRep, instLoc)
      else:
        wrap(intRep, instLoc, info)

  conf.handleReport(rep, instLoc, doAbort)
  unreachable("not aborted")

template internalError*(
    conf: ConfigRef,
    info: TLineInfo,
    fail: string,
  ): untyped =
  ## Causes an internal error. Always ends the currently executing routine.
  doInternalUnreachable(conf, info, fail, instLoc())

template internalError*(conf: ConfigRef, fail: string): untyped =
  ## Causes an internal error. Always ends the currently executing routine.
  doInternalUnreachable(conf, unknownLineInfo, fail, instLoc())

proc doInternalAssert*(conf: ConfigRef,
                       instLoc: InstantiationInfo,
                       msg: string,
                       info = unknownLineInfo) {.noreturn, inline.} =
  ## this proc firewalls other code from legacy reports, used in conjunction
  ## with the `internalAssert` templates
  let
    intRep = InternalReport(kind: rintAssert, msg: msg)
    rep =
      if info == unknownLineInfo:
        wrap(intRep, instLoc)
      else:
        wrap(intRep, instLoc, info)

  conf.handleReport(rep, instLoc, doAbort)
  unreachable("not aborted")

template internalAssert*(
    conf: ConfigRef, condition: bool, info: TLineInfo, failMsg: string = "") =
  ## Causes an internal error if the provided condition evaluates to false.
  ## Always ends the currently executing routine.
  if not condition:
    doInternalAssert(conf, instLoc(), failMsg, info)

template internalAssert*(conf: ConfigRef, condition: bool, failMsg = "") =
  ## Causes an internal error if the provided condition evaluates to false.
  ## Always ends the currently executing routine.
  if not condition:
    doInternalAssert(conf, instLoc(), failMsg)

# xxx: All the LexerReport stuff needs to go, it should just be the lexer
#      defined/provided diagnostics/etc that we shouldn't muck with. The
#      code below is a temporary bridge to work around this until fixed.

from compiler/ast/lexer import LexerDiag, LexerDiagKind, prettyTok,
                               diagToHumanStr
from compiler/ast/reports_lexer import LexerReport

func lexDiagToLegacyReportKind*(diag: LexerDiagKind): ReportKind {.inline.} =
  case diag
  of lexDiagMalformedNumUnderscores: rlexMalformedNumUnderscores
  of lexDiagMalformedIdentUnderscores: rlexMalformedIdentUnderscores
  of lexDiagMalformedTrailingUnderscre: rlexMalformedTrailingUnderscre
  of lexDiagInvalidToken: rlexInvalidToken
  of lexDiagInvalidTokenSpaceBetweenNumAndIdent: rlexInvalidTokenSpaceBetweenNumAndIdent
  of lexDiagNoTabs: rlexNoTabs
  of lexDiagInvalidIntegerLiteralOctalPrefix: rlexInvalidIntegerLiteralOctalPrefix
  of lexDiagInvalidIntegerSuffix: rlexInvalidIntegerSuffix
  of lexDiagNumberNotInRange: rlexNumberNotInRange
  of lexDiagExpectedHex: rlexExpectedHex
  of lexDiagInvalidIntegerLiteral: rlexInvalidIntegerLiteral
  of lexDiagInvalidNumericLiteral: rlexInvalidNumericLiteral
  of lexDiagInvalidCharLiteral: rlexInvalidCharLiteral
  of lexDiagInvalidCharLiteralConstant: rlexInvalidCharLiteralConstant
  of lexDiagInvalidCharLiteralPlatformNewline: rlexInvalidCharLiteralPlatformNewline
  of lexDiagInvalidCharLiteralUnicodeCodepoint: rlexInvalidCharLiteralUnicodeCodepoint
  of lexDiagMissingClosingApostrophe: rlexMissingClosingApostrophe
  of lexDiagInvalidUnicodeCodepointEmpty: rlexInvalidUnicodeCodepointEmpty
  of lexDiagInvalidUnicodeCodepointGreaterThan0x10FFFF: rlexInvalidUnicodeCodepointGreaterThan0x10FFFF
  of lexDiagUnclosedTripleString: rlexUnclosedTripleString
  of lexDiagUnclosedSingleString: rlexUnclosedSingleString
  of lexDiagUnclosedComment: rlexUnclosedComment
  of lexDiagDeprecatedOctalPrefix: rlexDeprecatedOctalPrefix
  of lexDiagLineTooLong: rlexLineTooLong
  of lexDiagNameXShouldBeY: rlexLinterReport

func lexerDiagToLegacyReport*(diag: LexerDiag): Report {.inline.} =
  let
    kind = diag.kind.lexDiagToLegacyReportKind()
    rep = LexerReport(
            location: std_options.some diag.location,
            reportInst: diag.instLoc.toReportLineInfo,
            kind: kind,
            msg: diagToHumanStr(diag))
  result = Report(category: repLexer, lexReport: rep)

proc handleLexerDiag*(
    conf: ConfigRef,
    diag: LexerDiag,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
  ) {.inline.} =
  # REFACTOR: this is a temporary bridge into existing reporting
  let rep = diag.lexerDiagToLegacyReport()
  handleReport(conf, rep, reportFrom, eh)

# xxx: All the ParserReport stuff needs to go, it should just be the parser
#      defined/provided diagnostics/etc that we shouldn't muck with. The
#      code below is a temporary bridge to work around this until fixed.
from compiler/ast/reports_parser import ParserReport
from compiler/ast/ast_parsed_types import ParseDiag, ParseDiagKind

func parseDiagToLegacyReportKind(d: ParseDiagKind): ReportKind {.inline.} =
  case d
  of pdkInvalidIndentation: rparInvalidIndentation
  of pdkInvalidIndentationWithForgotEqualSignHint: rparInvalidIndentation
  of pdkNestableRequiresIndentation: rparNestableRequiresIndentation
  of pdkIdentExpected: rparIdentExpected
  of pdkIdentExpectedEmptyAccQuote: rparIdentExpectedEmptyAccQuote
  of pdkExprExpected: rparExprExpected
  of pdkMissingToken: rparMissingToken
  of pdkUnexpectedToken: rparUnexpectedToken
  of pdkAsmStmtExpectsStrLit: rparAsmStmtExpectsStrLit
  of pdkFuncNotAllowed: rparFuncNotAllowed
  of pdkTupleTypeWithPar: rparTupleTypeWithPar
  of pdkMisplacedParameterVar: rparMisplacedParameterVar
  of pdkConceptNotInType: rparConceptNotinType
  of pdkMisplacedExport: rparMisplacedExport
  of pdkPragmaBeforeGenericParameters: rparPragmaBeforeGenericParameters
  of pdkInconsistentSpacing: rparInconsistentSpacing
  of pdkPragmaDoesNotFollowTypeName: rparPragmaNotFollowingTypeName
  of pdkEnablePreviewDotOps: rparEnablePreviewDotOps

proc parseDiagToLegacyReport(d: ParseDiag): Report =
  let
    kind = d.kind.parseDiagToLegacyReportKind
    rep =
      case d.kind
      of pdkInvalidIndentation,
          pdkNestableRequiresIndentation,
          pdkIdentExpectedEmptyAccQuote,
          pdkFuncNotAllowed,
          pdkTupleTypeWithPar,
          pdkMisplacedParameterVar,
          pdkConceptNotInType,
          pdkMisplacedExport,
          pdkPragmaBeforeGenericParameters,
          pdkPragmaDoesNotFollowTypeName,
          pdkEnablePreviewDotOps:
        ParserReport(kind: kind,
                     location: std_options.some d.location,
                     reportInst: d.instLoc.toReportLineInfo)
      of pdkInvalidIndentationWithForgotEqualSignHint:
        ParserReport(kind: rparInvalidIndentationWithForgotEqualSignHint,
                     location: std_options.some d.location,
                     reportInst: d.instLoc.toReportLineInfo,
                     eqInfo: d.eqLineInfo)
      of pdkMissingToken:
        var ex: seq[string]
        for t in d.missedToks:
          ex.add $t
        ParserReport(kind: rparMissingToken,
                     location: std_options.some d.location,
                     reportInst: d.instLoc.toReportLineInfo,
                     expected: ex)
      of pdkUnexpectedToken:
        ParserReport(kind: rparUnexpectedToken,
                     location: std_options.some d.location,
                     reportInst: d.instLoc.toReportLineInfo,
                     expectedKind: $d.expected,
                     found: prettyTok(d.actual))
      of pdkIdentExpected,
          pdkAsmStmtExpectsStrLit,
          pdkExprExpected,
          pdkInconsistentSpacing:
        ParserReport(kind: kind,
                     location: std_options.some d.location,
                     reportInst: d.instLoc.toReportLineInfo,
                     found: prettyTok(d.found))
  
  result = Report(category: repParser, parserReport: rep)

proc handleParserDiag*(
    conf: ConfigRef,
    diag: ParseDiag,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
  ) {.inline.} =
  # REFACTOR: this is a temporary bridge into existing reporting
  let rep = parseDiagToLegacyReport(diag)
  handleReport(conf, rep, reportFrom, eh)

proc handleReport*(
    conf: ConfigRef,
    diag: PAstDiag,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
  ) {.inline.} =
  # REFACTOR: this is a temporary bridge into existing reporting
  let rep = conf.astDiagToLegacyReport(conf, diag)
  handleReport(conf, rep, reportFrom, eh)

proc semReportCountMismatch*(
    kind: ReportKind,
    expected, got: distinct SomeInteger,
    node: PNode = nil,
  ): SemReport =
  assert kind in rsemReportCountMismatch, "not a count mismatch report, got: " & $kind
  result = SemReport(kind: kind, ast: node)
  result.countMismatch = (expected, got)

proc joinAnyOf*[T](values: seq[T], quote: bool = false): string =
  proc q(s: string): string =
    if quote:
      "'" & s & "'"
    else:
      s

  case len(values)
  of 1:
    result.add q($values[0])
  of 2:
    result.add q($values[0]) & " or " & q($values[1])
  else:
    for idx in 0..<len(values) - 1:
      if idx > 0:
        result.add ", "
      result.add q($values[idx])

    result.add " or "
    result.add q($values[^1])

template createSemIllformedAstMsg*(node: PNode,
                                   expected: set[TNodeKind]): string =
  var exp: seq[TNodeKind]
  for e in expected:
    exp.add e

  "Expected $1, but found $2" % [joinAnyOf(exp), $node.kind]

proc illformedAstReport(node: PNode, explain: string): SemReport {.inline.} =
  SemReport(kind: rsemIllformedAst, ast: node, str: explain)

template semReportIllformedAst*(
    conf: ConfigRef, node: PNode, explain: string): untyped =
  handleReport(
    conf,
    wrap(
      illformedAstReport(node, explain),
      instLoc(),
      node.info),
    instLoc(),
    doNothing)

template semReportIllformedAst*(
  conf: ConfigRef, node: PNode, expected: set[TNodeKind]): untyped =
  semReportIllformedAst(conf, node, createSemIllformedAstMsg(node, expected))

template localReport*(conf: ConfigRef, info: TLineInfo, report: ReportTypes) =
  handleReport(conf, wrap(report, instLoc(), info), instLoc(), doNothing)

proc quotedFilename*(conf: ConfigRef; i: TLineInfo): Rope =
  if i.fileIndex.int32 < 0:
    result = makeCString "???"
  elif optExcessiveStackTrace in conf.globalOptions:
    result = conf[i.fileIndex].quotedFullName
  else:
    result = conf[i.fileIndex].quotedName

proc listWarnings*(conf: ConfigRef) =
  conf.localReport(InternalReport(
    kind: rintListWarnings,
    enabledOptions: repWarningKinds * conf.notes))

proc listHints*(conf: ConfigRef) =
  conf.localReport(InternalReport(
    kind: rintListHints,
    enabledOptions: repHintKinds * conf.notes))

proc uniqueModuleName*(conf: ConfigRef; fid: FileIndex): string =
  ## The unique module name is guaranteed to only contain {'A'..'Z',
  ## 'a'..'z', '0'..'9', '_'} so that it is useful as a C identifier
  ## snippet.
  let
    path = AbsoluteFile toFullPath(conf, fid)
    rel =
      if path.string.startsWith(conf.libpath.string):
        relativeTo(path, conf.libpath).string
      else:
        relativeTo(path, conf.projectPath).string
    trunc = if rel.endsWith(".nim"): rel.len - len(".nim") else: rel.len
  result = newStringOfCap(trunc)
  for i in 0..<trunc:
    let c = rel[i]
    case c
    of 'a'..'z':
      result.add c
    of {os.DirSep, os.AltSep}:
      result.add 'Z' # because it looks a bit like '/'
    of '.':
      result.add 'O' # a circle
    else:
      # We mangle upper letters and digits too so that there cannot
      # be clashes with our special meanings of 'Z' and 'O'
      result.addInt ord(c)
