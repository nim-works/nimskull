## module with semantic analysis legacy reports definitions

import
  compiler/ast/[
    report_enums,
    ast_types,
    lineinfos,
    reports_base,
    reports_base_sem,
  ],
  compiler/sem/[
    nilcheck_enums,
  ],
  compiler/utils/[
    int128,
  ],
  std/options


type
  SemGcUnsafetyKind* = enum
    sgcuCallsUnsafe
    sgcuAccessesGcGlobal
    sgcuIndirectCallVia
    sgcuIndirectCallHere

  SemSideEffectCallKind* = enum
    ssefUsesGlobalState
    ssefCallsSideEffect
    ssefCallsViaHiddenIndirection
    ssefCallsViaIndirection
    ssefParameterMutation

  SemNilHistory* = object
    ## keep history for each transition
    info*: TLineInfo ## the location
    nilability*: Nilability ## the nilability
    kind*: NilTransition ## what kind of transition was that
    node*: PNode ## the node of the expression

  SemReport* = object of SemishReportBase
    ast*: PNode
    typ*: PType
    sym*: PSym
    str*: string
    spellingCandidates*: seq[SemSpellCandidate]

    case kind*: ReportKind
      of rsemDuplicateModuleImport:
        previous*: PSym

      of rsemCannotInstantiateWithParameter:
        arguments*: tuple[got, expected: seq[PNode]]

      of rsemUnavailableTypeBound:
        missingTypeBoundElaboration*: tuple[
          anotherRead: Option[TLineInfo],
          tryMakeSinkParam: bool
        ]

      of rsemDuplicateCaseLabel:
        overlappingGroup*: PNode

      of rsemCannotBorrow:
        borrowPair*: tuple[mutatedHere, connectedVia: TLineInfo]

      of rsemXCannotRaiseY:
        raisesList*: PNode

      of rsemStrictNotNilExpr, rsemStrictNotNilResult:
        nilIssue*: Nilability
        nilHistory*: seq[SemNilHistory]

      of rsemExpectedIdentifierInExpr,
         rsemExpectedIdentifierWithExprContext,
         rsemExpectedOrdinal,
         rsemUseOrDiscardExpr,
         rsemOnlyDeclaredIdentifierFoundIsError,
         rsemCantConvertLiteralToRange:
        wrongNode*: PNode

      of rsemWarnGcUnsafeListing, rsemErrGcUnsafeListing:
        gcUnsafeTrace*: tuple[
          isUnsafe: PSym,
          unsafeVia: PSym,
          unsafeRelation: SemGcUnsafetyKind,
        ]

      of rsemDeprecatedCompilerOptArg:
        compilerOptArg*: string

      of rsemCompilerOptionArgInvalid:
        badCompilerOptArg*: string
        allowedOptArgs*: seq[string]

      of rsemHasSideEffects:
        sideEffectTrace*: seq[tuple[isUnsafe: PSym,
                                    unsafeVia: PSym,
                                    trace: SemSideEffectCallKind,
                                    location: TLineInfo,
                                    level: int
                                  ]]
        sideEffectMutateConnection*: TLineInfo

      of rsemEffectsListingHint:
        effectListing*: tuple[tags, exceptions: seq[PType]]

      of rsemReportCountMismatch,
         rsemWrongNumberOfVariables:
        countMismatch*: tuple[expected, got: int]
      
      of rsemReportBigOrdsEnergy:
        expectedCount*, got*: Int128

      of rsemInvalidExtern:
        externName*: string

      of rsemWrongIdent:
        expectedIdents*: seq[string]

      of rsemStaticIndexLeqUnprovable, rsemStaticIndexGeProvable:
        rangeExpression*: tuple[a, b: PNode]

      of rsemExprHasNoAddress:
        isUnsafeAddr*: bool

      of rsemUndeclaredIdentifier, rsemCallNotAProcOrField:
        recursiveDeps*: seq[tuple[importer, importee: FileIndex]]
        notProcOrField*: PNode          ## `rsemCallNotAProcOrField`, the field
        explicitCall*: bool             ## Whether `rsemCallNotAProcOrField`
                                        ##   error was due to an explicit dot
                                        ##   call expression, eg: `obj.cal()`
        unexpectedCandidate*: seq[PSym] ## Symbols that are syntactically valid
                                        ##   in this context, but semantically
                                        ##   are not allowed - for example
                                        ##   `object.iterator()` call outside
                                        ##   of a `for` loop.

      of rsemDisjointFields,
         rsemUnsafeRuntimeDiscriminantInit,
         rsemConflictingDiscriminantInit,
         rsemMissingCaseBranches,
         rsemConflictingDiscriminantValues:
        fieldMismatches*: tuple[first, second: seq[PSym]]
        nodes*: seq[PNode]

      of rsemCannotInstantiate:
        ownerSym*: PSym

      of rsemReportTwoSym + rsemReportOneSym + rsemReportListSym:
        symbols*: seq[PSym]

      of rsemExpandMacro, rsemPattern, rsemExpandArc:
        expandedAst*: PNode

      of rsemLockLevelMismatch, rsemMethodLockMismatch:
        anotherMethod*: PSym
        lockMismatch*: tuple[expected, got: string]

      of rsemTypeMismatch,
         rsemSuspiciousEnumConv,
         rsemTypeKindMismatch,
         rsemSemfoldInvalidConversion,
         rsemCannotConvertTypes,
         rsemImplicitObjConv,
         rsemIllegalConversion,
         rsemConceptInferenceFailed,
         rsemCannotCastTypes,
         rsemGenericTypeExpected,
         rsemCannotBeOfSubtype,
         rsemDifferentTypeForReintroducedSymbol:
        typeMismatch*: seq[SemTypeMismatch]

      of rsemSymbolKindMismatch:
        expectedSymbolKind*: set[TSymKind]

      of rsemTypeNotAllowed:
        allowedType*: tuple[
          allowed: PType,
          actual: PType,
          kind: TSymKind,
          allowedFlags: TTypeAllowedFlags
        ]

      of rsemCallTypeMismatch, rsemNonMatchingCandidates:
        callMismatches*: seq[SemCallMismatch] ## Description of all the failed
                                              ## candidates.

      of rsemStaticOutOfBounds:
        indexSpec*: tuple[usedIdx, minIdx, maxIdx: Int128]

      of rsemProcessing:
        processing*: tuple[
          isNimscript: bool,
          importStackLen: int,
          moduleStatus: string,
          fileIdx: FileIndex
        ]

      of rsemLinterReport, rsemLinterReportUse:
        info*: TLineInfo
        linterFail*: tuple[wanted, got: string]

      of rsemDiagnostics:
        diag*: SemDiagnostics

      else:
        discard


func severity*(report: SemReport): ReportSeverity =
  case SemReportKind(report.kind)
  of rsemErrorKinds:   rsevError
  of rsemWarningKinds: rsevWarning
  of rsemHintKinds:    rsevHint
  of rsemFatalError:   rsevFatal

proc reportSymbols*(
    kind: ReportKind,
    symbols: seq[PSym],
    typ: PType = nil,
    ast: PNode = nil
  ): SemReport =
  case kind
  of rsemReportTwoSym: assert symbols.len == 2
  of rsemReportOneSym: assert symbols.len == 1
  of rsemReportListSym: discard
  else: assert false, $kind

  result = SemReport(kind: kind, ast: ast)
  result.symbols = symbols
  result.typ = typ

func reportSem*(kind: ReportKind): SemReport = SemReport(kind: kind)

func reportAst*(
    kind: ReportKind,
    ast: PNode, str: string = "", typ: PType = nil, sym: PSym = nil
  ): SemReport =
  SemReport(kind: kind, ast: ast, str: str, typ: typ, sym: sym)

func reportTyp*(
    kind: ReportKind,
    typ: PType, ast: PNode = nil, sym: PSym = nil, str: string = ""
  ): SemReport =
  SemReport(kind: kind, typ: typ, ast: ast, sym: sym, str: str)
func reportStr*(
    kind: ReportKind,
    str: string, ast: PNode = nil, typ: PType = nil, sym: PSym = nil
  ): SemReport =
  SemReport(kind: kind, ast: ast, str: str, typ: typ, sym: sym)

func reportSym*(
    kind: ReportKind,
    sym: PSym, ast: PNode = nil, str: string = "", typ: PType = nil,
  ): SemReport =
  SemReport(kind: kind, ast: ast, str: str, typ: typ, sym: sym)