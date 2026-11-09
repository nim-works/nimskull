## Provides all magic and built-in symbols exported by the compiler.

# TODO: move all magic types and routine declarations into this module

include system/inclrtl # currently needed for the `.benign` pragma

# TODO: make `NimNodeKind`, `NimTypeKind`, and `NimSymKind` magic types (or
#       have them be defined in the compiler)

template skipEnumValue(define: untyped, predecessor: untyped; gap = 1): untyped =
  ## This template is used to keep the ordinal values of the ``TNodeKind``
  ## enum in sync with the ``NimNodeKind`` enum.
  ##
  ## It is expected that each ``nkX`` has the same underlying value as the
  ## corresponding ``nnkX``, but this expectation is violated when removing
  ## a ``TNodeKind`` entry (plus the corresponding ``NimNodeKind``) and then
  ## compiling with a compiler that still has the removed ``TNodeKind`` entry
  ## (this can happen when bootstrapping, for example).
  ##
  ## To keep the required compatibility, when a ``TNodeKind`` and corresponding
  ## ``NimNodeKind`` are removed, the successor of the removed enum entry uses
  ## ``skipEnumValue`` to leave a gap in the case that `define`, which is used
  ## to indicate that the enum entry is not present in the compiler, is not
  ## defined.
  ##
  ## `gap` specifies the amount of enum fields to skip.
  when defined(define):
    ord(predecessor) + 1
  else:
    # leave a gap where the removed node kinds are located
    ord(predecessor) + gap + 1

type
  NimNodeKind* = enum
    nnkError,  ## erroneous AST node
    nnkEmpty, nnkIdent, nnkSym,
    nnkType, nnkCharLit, nnkIntLit, nnkInt8Lit,
    nnkInt16Lit, nnkInt32Lit, nnkInt64Lit, nnkUIntLit, nnkUInt8Lit,
    nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit, nnkFloatLit,
    nnkFloat32Lit, nnkFloat64Lit,
    nnkStrLit = skipEnumValue(nimskullNoFloat128, nnkFloat64Lit)
    nnkRStrLit, nnkTripleStrLit, nnkNilLit,
    nnkDotCall = skipEnumValue(nimHasNkComesFromNodeRemoved, nnkNilLit)
    nnkCommand, nnkCall, nnkCallStrLit, nnkInfix,
    nnkPrefix, nnkPostfix, nnkHiddenCallConv,
    nnkExprEqExpr,
    nnkExprColonExpr, nnkIdentDefs, nnkVarTuple,
    nnkPar, nnkObjConstr, nnkCurly, nnkCurlyExpr,
    nnkBracket, nnkBracketExpr, nnkPragmaExpr, nnkRange,
    nnkDotExpr, nnkCheckedFieldExpr, nnkDerefExpr, nnkIfExpr,
    nnkElifExpr, nnkElseExpr, nnkLambda, nnkDo, nnkAccQuoted,
    nnkTableConstr, nnkBind,
    nnkClosedSymChoice,
    nnkOpenSymChoice,
    nnkHiddenStdConv,
    nnkHiddenSubConv, nnkConv, nnkCast, nnkStaticExpr,
    nnkAddr, nnkHiddenAddr, nnkHiddenDeref, nnkObjDownConv,
    nnkObjUpConv, nnkChckRangeF, nnkChckRange64, nnkChckRange,
    nnkStringToCString, nnkCStringToString, nnkAsgn,
    nnkFastAsgn, nnkGenericParams, nnkFormalParams, nnkOfInherit,
    nnkImportAs, nnkProcDef, nnkMethodDef, nnkConverterDef,
    nnkMacroDef, nnkTemplateDef, nnkIteratorDef, nnkOfBranch,
    nnkElifBranch, nnkExceptBranch, nnkElse,
    nnkAsmStmt, nnkPragma, nnkPragmaBlock, nnkIfStmt, nnkWhenStmt,
    nnkForStmt,
    nnkWhileStmt = skipEnumValue(nimHasNkParForStmtNodeRemoved, nnkForStmt)
    nnkCaseStmt,
    nnkTypeSection, nnkVarSection, nnkLetSection, nnkConstSection,
    nnkConstDef, nnkTypeDef,
    nnkYieldStmt, nnkDefer, nnkTryStmt, nnkFinally, nnkRaiseStmt,
    nnkReturnStmt, nnkBreakStmt, nnkContinueStmt, nnkBlockStmt, nnkStaticStmt,
    nnkDiscardStmt, nnkStmtList,
    nnkImportStmt,
    nnkImportExceptStmt,
    nnkExportStmt,
    nnkExportExceptStmt,
    nnkFromStmt,
    nnkIncludeStmt,
    nnkBindStmt, nnkMixinStmt, nnkUsingStmt,
    nnkCommentStmt, nnkStmtListExpr, nnkBlockExpr,
    nnkWith = skipEnumValue(nimskullNoNkStmtListTypeAndNkBlockType, nnkBlockExpr, 2),
    nnkWithout,
    nnkTypeOfExpr, nnkObjectTy,
    nnkTupleTy, nnkTupleClassTy, nnkTypeClassTy, nnkStaticTy,
    nnkRecList, nnkRecCase, nnkRecWhen,
    nnkRefTy, nnkPtrTy, nnkVarTy,
    nnkConstTy, nnkMutableTy,
    nnkDistinctTy,
    nnkProcTy,
    nnkIteratorTy,         # iterator type
    nnkSharedTy,           # 'shared T'
    nnkEnumTy,
    nnkEnumFieldDef,
    nnkArgList, nnkPattern
    nnkHiddenTryStmt,
    nnkClosure,
    nnkGotoState,
    nnkFuncDef = skipEnumValue(nimHasNkBreakStateNodeRemoved, nnkGotoState, 2),
    nnkTupleConstr,
    nnkNimNodeLit = skipEnumValue(nimskullNoNkNone, nnkTupleConstr)

  NimTypeKind* = enum  # some types are no longer used, see ast.nim
    ntyNone, ntyBool, ntyChar, ntyEmpty,
    ntyAlias, ntyNil, ntyExpr, ntyStmt,
    ntyTypeDesc, ntyGenericInvocation, ntyGenericBody, ntyGenericInst,
    ntyGenericParam, ntyDistinct, ntyEnum, ntyOrdinal,
    ntyArray, ntyObject, ntyTuple, ntySet,
    ntyRange, ntyPtr, ntyRef, ntyVar,
    ntySequence, ntyProc, ntyPointer, ntyOpenArray,
    ntyString, ntyCString, ntyForward, ntyInt,
    ntyInt8, ntyInt16, ntyInt32, ntyInt64,
    ntyFloat, ntyFloat32, ntyFloat64,
    ntyUInt = skipEnumValue(nimskullNoFloat128, ntyFloat64)
    ntyUInt8, ntyUInt16, ntyUInt32, ntyUInt64,
    ntyUnused1 = skipEnumValue(nimHasTyOwnedRemoved, ntyUInt64),
    ntyUnused2,
    ntyVarargs,
    ntyUncheckedArray,
    ntyError,
    ntyBuiltinTypeClass, ntyUserTypeClass, ntyUserTypeClassInst,
    ntyCompositeTypeClass, ntyInferred, ntyAnd, ntyOr, ntyNot,
    ntyAnything, ntyStatic, ntyFromExpr,
    ntyVoid = skipEnumValue(nimHasTyConceptRemoved, ntyFromExpr)

  NimSymKind* = enum
    nskUnknown, nskConditional, nskDynLib, nskParam,
    nskGenericParam, nskTemp, nskModule, nskType, nskVar, nskLet,
    nskConst, nskResult,
    nskProc, nskFunc, nskMethod, nskIterator,
    nskConverter, nskMacro, nskTemplate, nskField,
    nskEnumField, nskForVar, nskLabel,
    nskStub, nskGenerated

proc `==`*(a, b: NimNode): bool {.magic: "EqNimrodNode", noSideEffect.}

proc sameType*(a, b: NimNode): bool {.magic: "SameNodeType", noSideEffect.}

proc len*(n: NimNode): int {.magic: "NLen", noSideEffect.}

proc `[]`*(n: NimNode, i: int): NimNode {.magic: "NChild", noSideEffect.}

proc `[]=`*(n: NimNode, i: int, child: NimNode) {.magic: "NSetChild",
  noSideEffect.}

proc add*(father, child: NimNode): NimNode {.magic: "NAdd",
  noSideEffect, locks: 0.}

proc add*(father: NimNode, children: varargs[NimNode]): NimNode {.
  magic: "NAddMultiple", noSideEffect, locks: 0.}

proc del*(father: NimNode, idx, n: int) {.magic: "NDel", noSideEffect.}

proc kind*(n: NimNode): NimNodeKind {.magic: "NKind", noSideEffect.}

proc intVal*(n: NimNode): BiggestInt {.magic: "NIntVal", noSideEffect.}

proc floatVal*(n: NimNode): BiggestFloat {.magic: "NFloatVal", noSideEffect.}

proc symKind*(symbol: NimNode): NimSymKind {.magic: "NSymKind", noSideEffect.}
proc getImpl*(symbol: NimNode): NimNode {.magic: "GetImpl", noSideEffect.}
proc strVal*(n: NimNode): string  {.magic: "NStrVal", noSideEffect.}

proc getImplTransformed*(symbol: NimNode): NimNode {.magic: "GetImplTransf", noSideEffect.}

proc owner*(sym: NimNode): NimNode {.magic: "SymOwner", noSideEffect.}

proc isInstantiationOf*(instanceProcSym, genProcSym: NimNode): bool {.magic: "SymIsInstantiationOf", noSideEffect.}

proc getType*(n: NimNode): NimNode {.magic: "NGetType", noSideEffect.}

proc getType*(n: typedesc): NimNode {.magic: "NGetType", noSideEffect.}

proc typeKind*(n: NimNode): NimTypeKind {.magic: "NGetType", noSideEffect.}

proc getTypeInst*(n: NimNode): NimNode {.magic: "NGetType", noSideEffect.}

proc getTypeInst*(n: typedesc): NimNode {.magic: "NGetType", noSideEffect.}

proc getTypeImpl*(n: NimNode): NimNode {.magic: "NGetType", noSideEffect.}

proc signatureHash*(n: NimNode): string {.magic: "NSigHash", noSideEffect.}

proc getTypeImpl*(n: typedesc): NimNode {.magic: "NGetType", noSideEffect.}

proc `intVal=`*(n: NimNode, val: BiggestInt) {.magic: "NSetIntVal", noSideEffect.}
proc `floatVal=`*(n: NimNode, val: BiggestFloat) {.magic: "NSetFloatVal", noSideEffect.}

proc `strVal=`*(n: NimNode, val: string) {.magic: "NSetStrVal", noSideEffect.}

proc newNimNode*(kind: NimNodeKind, lineInfoFrom: NimNode): NimNode
  {.magic: "NNewNimNode", noSideEffect.}

proc copyNimNode*(n: NimNode): NimNode {.magic: "NCopyNimNode", noSideEffect.}
proc copyNimTree*(n: NimNode): NimNode {.magic: "NCopyNimTree", noSideEffect.}

proc error*(msg: string, n: NimNode) {.magic: "NError", benign.}

proc warning*(msg: string, n: NimNode) {.magic: "NWarning", benign.}

proc hint*(msg: string, n: NimNode) {.magic: "NHint", benign.}

proc newIdentNode*(i: string): NimNode {.magic: "StrToIdent", noSideEffect.}

proc ident*(name: string): NimNode {.magic: "StrToIdent", noSideEffect.}

type
  BindSymRule* = enum    ## Specifies how `bindSym` behaves. The difference
                         ## between open and closed symbols can be found in
                         ## `<manual.html#symbol-lookup-in-generics-open-and-closed-symbols>`_
    brClosed,            ## only the symbols in current scope are bound
    brOpen,              ## open for overloaded symbols, but may be a single
                         ## symbol if not ambiguous (the rules match that of
                         ## binding in generics)
    brForceOpen          ## same as brOpen, but it will always be open even
                         ## if not ambiguous (this cannot be achieved with
                         ## any other means in the language currently)

proc bindSym*(ident: string | NimNode, rule: BindSymRule): NimNode {.
              magic: "NBindSym", noSideEffect.}

proc genSym*(ident: string): NimNode {.magic: "NGenSym", noSideEffect.}

proc callsite*(): NimNode {.magic: "NCallSite", benign.}

proc getLine*(arg: NimNode): int {.magic: "NLineInfo", noSideEffect.}
proc getColumn*(arg: NimNode): int {.magic: "NLineInfo", noSideEffect.}
proc getFile*(arg: NimNode): string {.magic: "NLineInfo", noSideEffect.}

proc copyLineInfo*(arg: NimNode, info: NimNode) {.magic: "NLineInfo", noSideEffect.}

proc parseExpr*(s: string, err: var string): NimNode {.
  magic: "ParseExprToAst", noSideEffect.}

proc parseStmt*(s: string, err: var string): NimNode {.
  magic: "ParseStmtToAst", noSideEffect.}

proc getAst*(macroOrTemplate: untyped): NimNode {.magic: "ExpandToAst", noSideEffect.}

proc quote*(bl: typed, op: string): NimNode {.magic: "QuoteAst", noSideEffect.}

proc evalToAst*[T](x: T): NimNode {.magic: "EvalToAst".}

proc eqIdent*(a: string; b: string): bool {.magic: "EqIdent", noSideEffect.}

proc eqIdent*(a: NimNode; b: string): bool {.magic: "EqIdent", noSideEffect.}

proc eqIdent*(a: string; b: NimNode): bool {.magic: "EqIdent", noSideEffect.}

proc eqIdent*(a: NimNode; b: NimNode): bool {.magic: "EqIdent", noSideEffect.}

proc nodeID*(n: NimNode): int {.magic: "NodeId".}

proc getSize*(arg: NimNode): int {.magic: "NSizeOf", noSideEffect.}

proc getAlign*(arg: NimNode): int {.magic: "NSizeOf", noSideEffect.}

proc getOffset*(arg: NimNode): int {.magic: "NSizeOf", noSideEffect.}

proc name*(t: typedesc): string {.magic: "TypeTrait".}

proc arity*(t: typedesc): int {.magic: "TypeTrait".}

proc genericHead*(t: typedesc): typedesc {.magic: "TypeTrait".}

proc stripGenericParams*(t: typedesc): typedesc {.magic: "TypeTrait".}

proc supportsCopyMem*(t: typedesc): bool {.magic: "TypeTrait".}

proc supportsZeroMem*(t: typedesc): bool {.magic: "TypeTrait".}

proc isNamedTuple*(T: typedesc): bool {.magic: "TypeTrait".}

proc distinctBase*(T: typedesc, recursive: static bool): typedesc {.magic: "TypeTrait".}

proc tupleLen*(T: typedesc[tuple]): int {.magic: "TypeTrait".}

proc rangeBase*(t: typedesc): typedesc {.magic: "TypeTrait".}

proc isCyclical*(t: typedesc): bool {.magic: "TypeTrait".}

proc nccValue*(c: string): int {.magic: "NccValue".}

proc nccInc*(c: string; by: int) {.magic: "NccInc".}

proc ncsAdd*(s: string; value: NimNode) {.magic: "NcsAdd".}

proc ncsIncl*(s: string; value: NimNode) {.magic: "NcsIncl".}

proc ncsLen*(s: string): int {.magic: "NcsLen".}

proc ncsAt*(s: string; i: int): NimNode {.magic: "NcsAt".}

proc nctPut*(t: string; key: string, value: NimNode) {.magic: "NctPut".}

proc nctLen*(t: string): int {.magic: "NctLen".}

proc nctGet*(t: string; key: string): NimNode {.magic: "NctGet".}

proc nctHasNext*(t: string; iter: int): bool {.magic: "NctHasNext".}

proc nctNext*(t: string; iter: int): (string, NimNode, int) {.magic: "NctNext".}

proc privateAccess*(t: typedesc) {.magic: "PrivateAccess".}
