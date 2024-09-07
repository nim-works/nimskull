## abstract syntax tree + symbol table querying operations
## 
## A companion module to `ast_types` containing things related to querying the
## AST and Symbol Table. Not all reads are here as some base level reading is
## required for manipulation, which are contained in `ast` itself.

import
  compiler/ast/[
    lineinfos, # Positional information
    idents,    # Ast identifiers
    ast_types  # Main ast type definitions
  ],
  compiler/utils/[
    int128,    # Values for integer nodes
    idioms,    # `unreachable`
  ]

const
  OverloadableSyms* = {
    skProc,
    skFunc,
    skMethod,
    skIterator,
    skConverter,
    skTemplate,
    skMacro,
    skEnumField,

    # BUGFIX: a module is overloadable so that a proc can have the
    # same name as an imported module. This is necessary because of
    # the poor naming choices in the standard library.
    skModule}

  skipForDiscardable* = {nkIfStmt, nkIfExpr, nkCaseStmt, nkOfBranch,
    nkElse, nkStmtListExpr, nkTryStmt, nkFinally, nkExceptBranch,
    nkElifBranch, nkElifExpr, nkElseExpr, nkBlockStmt, nkBlockExpr,
    nkHiddenStdConv, nkHiddenDeref}

  GenericTypes*: TTypeKinds = {tyGenericInvocation, tyGenericBody,
    tyGenericParam}

  StructuralEquivTypes*: TTypeKinds = {tyNil, tyTuple, tyArray,
    tySet, tyRange, tyPtr, tyRef, tyVar, tyLent, tySequence, tyProc, tyOpenArray,
    tyVarargs}

  ConcreteTypes*: TTypeKinds = { # types of the expr that may occur in::
                                 # var x = expr
    tyBool, tyChar, tyEnum, tyArray, tyObject,
    tySet, tyTuple, tyRange, tyPtr, tyRef, tyVar, tyLent, tySequence, tyProc,
    tyPointer,
    tyOpenArray, tyString, tyCstring, tyInt..tyInt64, tyFloat..tyFloat64,
    tyUInt..tyUInt64}
  
  IntegralTypes* = {tyBool, tyChar, tyEnum, tyInt..tyInt64,
    tyFloat..tyFloat64, tyUInt..tyUInt64} # weird name because it contains tyFloat
  
  ConstantDataTypes*: TTypeKinds = {tyArray, tySet,
                                    tyTuple, tySequence}
  
  NilableTypes*: TTypeKinds = {tyPointer, tyCstring, tyRef, tyPtr,
    tyProc, tyError} # TODO
  
  PtrLikeKinds*: TTypeKinds = {tyPointer, tyPtr} # for VM
  
  PersistentNodeFlags*: TNodeFlags = {nfDotSetter, nfDotField, nfLL,
                                      nfFromTemplate, nfDefaultRefsParam,
                                      nfWasGensym}
  
  namePos*          = 0 ## Name of the type/proc-like node
  patternPos*       = 1 ## empty except for term rewriting macros
  genericParamsPos* = 2 ## Generic parametesr in the procedure-like nodes
  paramsPos*        = 3 ## Formal parameters in the procedure-like nodes
  pragmasPos*       = 4 ## Position of the pragma in the procedure-like nodes
  miscPos*          = 5 ## used for undocumented and hacky stuff
  bodyPos*          = 6 ## position of body; use rodread.getBody() instead!
  resultPos*        = 7
  dispatcherPos*    = 8

  wrongNodePos*     = 0 ## Error the ast node we swapped
  errorKindPos*     = 1 ## Error kind enum as an intlit
  compilerInfoPos*  = 2 ## Error compiler source file as strlit, line & col
                        ## on info
  firstArgPos*      = 3 ## Error first 0..n additional nodes depends on
                        ## error kind

  nkCallKinds* = {nkCall, nkInfix, nkPrefix, nkPostfix,
                  nkCommand, nkCallStrLit, nkHiddenCallConv}
  nkIdentKinds* = {nkIdent, nkSym, nkAccQuoted, nkOpenSymChoice,
                   nkClosedSymChoice}

  nkPragmaCallKinds* = {nkExprColonExpr, nkCall, nkCallStrLit}

  nkLambdaKinds* = {nkLambda, nkDo}
  declarativeDefs* = {nkProcDef, nkFuncDef, nkMethodDef, nkIteratorDef, nkConverterDef}
  routineDefs* = declarativeDefs + {nkMacroDef, nkTemplateDef}
  procDefs* = nkLambdaKinds + declarativeDefs
  callableDefs* = nkLambdaKinds + routineDefs
  entityDefs* = callableDefs + {nkIdentDefs, nkVarTuple, nkConstDef, nkForStmt}
    ## all nodes that have definition slots. In other words, semantic analysis
    ## of these can introduce new symbols

  nkSymChoices* = {nkClosedSymChoice, nkOpenSymChoice}

  nkTypeExprs* = {
    nkTypeOfExpr,
    nkObjectTy,
    nkTupleTy,
    nkTupleClassTy,
    nkTypeClassTy,
    nkStaticTy,
    nkRefTy,
    nkPtrTy,
    nkVarTy,
    nkConstTy,
    nkMutableTy,
    nkDistinctTy,
    nkProcTy,
    nkIteratorTy,
    nkSharedTy,
    nkEnumTy,
  }

  # TODO: replace with `nk*Literals`, see above
  nkIntKinds*   = nkIntLiterals
  nkFloatKinds* = nkFloatLiterals
  nkStrKinds*   = nkStrLiterals

  nkAllNodeKinds* = {low(TNodeKind) .. high(TNodeKind)}

  skLocalVars* = {skVar, skLet, skForVar, skParam, skResult}
  skProcKinds* = {skProc, skFunc, skTemplate, skMacro, skIterator,
                  skMethod, skConverter}

  # memory layout consts for types
  defaultSize* = -1
  defaultAlignment* = -1
  defaultOffset* = -1

  FakeVarParams* = {mInc, mDec, mIncl, mExcl,
    mSetLengthStr, mSetLengthSeq, mAppendStrCh, mAppendStrStr, mSwap,
    mAppendSeqElem, mNewSeq, mReset, mShallowCopy, mDeepCopy, mMove,
    mWasMoved}
    ## An arguments to these magics never uses ``nkHiddenAddr``, even if the
    ## corresponding parameter is a 'var' parameter. The reason for this is
    ## that these magics are lowered into code that, because it gets inlined
    ## directly, doesn't mutate the arguments through indirection. This also
    ## implies that the "is address taken" analysis (see ``sfAddrTaken``) must
    ## not be performed for arguments to these magics.

proc getPIdent*(a: PNode): PIdent {.inline.} =
  ## Returns underlying `PIdent` for `{nkSym, nkIdent}`, or `nil`.
  # xxx consider whether also returning the 1st ident for {nkOpenSymChoice, nkClosedSymChoice}
  # which may simplify code.
  case a.kind
  of nkSym: a.sym.name
  of nkIdent: a.ident
  else: nil

proc getIdentLineInfo*(n: PNode): TLineInfo =
  ## Returns the line information of the identifier-like node in the
  ## (semantically valid) AST `n` appearing in a name slot.
  var n {.cursor.} = n
  # unpack the node until we reach the identifier or symbol
  if n.kind == nkPragmaExpr:
    n = n[0]
  if n.kind == nkPostfix:
    n = n[1]
  if n.kind == nkAccQuoted:
    n = n[0]

  result =
    case n.kind
    of nkIdent, nkSym: n.info
    else:              unreachable(n.kind)

proc getnimblePkg*(a: PSym): PSym =
  result = a
  while result != nil:
    case result.kind
    of skModule:
      result = result.owner
      assert result.kind == skPackage
    of skPackage:
      if result.owner == nil:
        break
      else:
        result = result.owner
    else:
      assert false, $result.kind

const
  moduleShift = when defined(cpu32): 20 else: 24

template id*(a: PIdObj): int =
  let x = a
  (x.itemId.module.int shl moduleShift) + x.itemId.item.int


const
  UnspecifiedLockLevel* = TLockLevel(-1'i16)
  MaxLockLevel* = 1000'i16
  UnknownLockLevel* = TLockLevel(1001'i16)
  AttachedOpToStr*: array[TTypeAttachedOp, string] = [
    "=destroy", "=copy", "=sink", "=trace", "=deepcopy"]
  AttachedOpToMagic*: array[TTypeAttachedOp, TMagic] = [
    mDestroy, mAsgn, mAsgn, mTrace, mDeepCopy]


proc `$`*(x: TLockLevel): string =
  if x.ord == UnspecifiedLockLevel.ord: result = "<unspecified>"
  elif x.ord == UnknownLockLevel.ord: result = "<unknown>"
  else: result = $int16(x)

proc `$`*(s: PSym): string =
  if s != nil:
    result = s.name.s & "@" & $s.id
  else:
    result = "<nil>"


proc getnimblePkgId*(a: PSym): int =
  let b = a.getnimblePkg
  result = if b == nil: -1 else: b.id


proc isCallExpr*(n: PNode): bool =
  result = n.kind in nkCallKinds

proc safeLen*(n: PNode): int {.inline.} =
  ## works even for leaves.
  case n.kind
  of nkWithoutSons: 0
  of nkWithSons:    n.len

proc getDeclPragma*(n: PNode): PNode =
  ## return the `nkPragma` node for declaration `n`, or `nil` if no pragma was found.
  ## Currently only supports routineDefs + {nkTypeDef}.
  case n.kind
  of routineDefs:
    if n[pragmasPos].kind != nkEmpty: result = n[pragmasPos]
  of nkTypeDef:
    #[
    type F3*{.deprecated: "x3".} = int

    TypeSection
      TypeDef
        PragmaExpr
          Postfix
            Ident "*"
            Ident "F3"
          Pragma
            ExprColonExpr
              Ident "deprecated"
              StrLit "x3"
        Empty
        Ident "int"
    ]#
    if n[0].kind == nkPragmaExpr:
      result = n[0][1]
  else:
    # support as needed for `nkIdentDefs` etc.
    result = nil
  if result != nil:
    assert result.kind == nkPragma, $(result.kind, n.kind)


template previouslyInferred*(t: PType): PType =
  if t.sons.len > 1: t.lastSon else: nil


proc astdef*(s: PSym): PNode =
  ## get only the definition (initializer) portion of the ast
  if s.ast != nil and s.ast.kind == nkIdentDefs:
    s.ast[2]
  else:
    s.ast


proc isMetaType*(t: PType): bool =
  return t.kind in tyMetaTypes or
         (t.kind == tyStatic and t.n == nil) or
         tfHasMeta in t.flags

proc isUnresolvedStatic*(t: PType): bool =
  return t.kind == tyStatic and t.n == nil

proc isUnresolvedSym*(s: PSym): bool =
  result = s.kind == skGenericParam
  if not result and s.typ != nil:
    result = tfInferrableStatic in s.typ.flags or
        (s.kind == skParam and s.typ.isMetaType) or
        (s.kind == skType and
        s.typ.flags * {tfGenericTypeParam, tfImplicitTypeParam} != {})

template fileIdx*(c: PSym): FileIndex =
  assert c.kind == skModule, "this should be used only on module symbols"
  c.position.FileIndex

template filename*(c: PSym): string =
  assert c.kind == skModule, "this should be used only on module symbols"
  c.position.FileIndex.toFilename


func lastSon*(n: Indexable): Indexable =
  {.cast(noSideEffect).}:
    # erroneously inferred side-effect
    n.sons[^1]


proc skipTypes*(t: PType, kinds: TTypeKinds): PType =
  ## Used throughout the compiler code to test whether a type tree contains or
  ## doesn't contain a specific type/types - it is often the case that only the
  ## last child nodes of a type tree need to be searched. This is a really hot
  ## path within the compiler!
  result = t
  while result.kind in kinds: result = lastSon(result)

proc skipDistincts*(t: PType): PType {.inline.} =
  ## Skips over all possible distinct instantiations, getting base.
  skipTypes(t, {tyAlias, tyGenericInst, tyDistinct})

proc skipTypes*(t: PType, kinds: TTypeKinds; maxIters: int): PType =
  result = t
  var i = maxIters
  while result.kind in kinds:
    result = lastSon(result)
    dec i
    if i == 0: return nil

proc skipTypesOrNil*(t: PType, kinds: TTypeKinds): PType =
  ## same as skipTypes but handles 'nil'
  result = t
  while result != nil and result.kind in kinds:
    if result.len == 0: return nil
    result = lastSon(result)

proc hasSonWith*(n: PNode, kind: TNodeKind): bool =
  for i in 0..<n.len:
    if n[i].kind == kind:
      return true
  result = false

proc hasNilSon*(n: PNode): bool =
  for i in 0..<n.safeLen:
    if n[i] == nil:
      return true
    elif hasNilSon(n[i]):
      return true
  result = false

proc containsNode*(n: PNode, kinds: TNodeKinds): bool =
  if n == nil: return
  case n.kind
  of nkWithoutSons: result = n.kind in kinds
  of nkWithSons:
    for i in 0..<n.len:
      if n.kind in kinds or containsNode(n[i], kinds): return true

proc hasSubnodeWith*(n: PNode, kind: TNodeKind): bool =
  case n.kind
  of nkWithoutSons - nkError, nkFormalParams:
    result = n.kind == kind
  of nkError:
    result = hasSubnodeWith(n.diag.wrongNode, kind)
  of nkWithSons - nkFormalParams:
    for i in 0..<n.len:
      if (n[i].kind == kind) or hasSubnodeWith(n[i], kind):
        return true
    result = false

proc getInt*(a: PNode): Int128 =
  case a.kind
  of nkUIntLiterals:
    result = toInt128(cast[uint64](a.intVal))
  of nkSIntLiterals:
    # XXX: enable this assert
    # assert a.typ.kind notin {tyChar, tyUint..tyUInt64}
    result = toInt128(a.intVal)
  else:
    raiseRecoverableError("cannot extract number from invalid AST node")

proc getInt64*(a: PNode): int64 {.deprecated: "use getInt".} =
  case a.kind
  of nkIntLiterals:
    result = a.intVal
  else:
    raiseRecoverableError("cannot extract number from invalid AST node")

proc getFloat*(a: PNode): BiggestFloat =
  case a.kind
  of nkFloatLiterals: result = a.floatVal
  of nkIntLiterals:
    result = BiggestFloat a.intVal
  else:
    raiseRecoverableError("cannot extract number from invalid AST node")
    #doAssert false, "getFloat"
    #internalError(a.info, "getFloat")
    #result = 0.0

proc getStr*(a: PNode): string =
  case a.kind
  of nkStrLiterals: result = a.strVal
  of nkNilLit:
    # let's hope this fixes more problems than it creates:
    result = ""
  else:
    raiseRecoverableError("cannot extract string from invalid AST node")
    #doAssert false, "getStr"
    #internalError(a.info, "getStr")
    #result = ""

proc getStrOrChar*(a: PNode): string =
  case a.kind
  of nkStrLiterals: result = a.strVal
  of nkIntLiterals: result = $chr(int(a.intVal))
  else:
    raiseRecoverableError("cannot extract string from invalid AST node")
    #doAssert false, "getStrOrChar"
    #internalError(a.info, "getStrOrChar")
    #result = ""

proc isGenericParams*(n: PNode): bool {.inline.} =
  ## used to judge whether a node is generic params.
  n != nil and n.kind == nkGenericParams

proc isGenericRoutine*(n: PNode): bool  {.inline.} =
  n != nil and n.kind in callableDefs and n[genericParamsPos].isGenericParams

proc isError*(n: PNode): bool {.inline.} =
  ## whether the node is an error, strictly checks nkError and is nil safe
  n != nil and n.kind == nkError

proc isError*(s: PSym): bool {.inline.} =
  ## whether the symbol is an error, strictly checks skError, an error node
  ## exists, and is nil safe.
  s != nil and s.kind == skError and s.ast.isError

proc isError*(t: PType): bool {.inline.} =
  ## whether the type is an error. useful because of compiler legacy, as
  ## `tyError` isn't an enum field rather a const refering to `tyProxy`.
  ##
  ## xxx: currently we have no way to disambiguate between legacy and new
  t != nil and t.kind == tyError

proc isErrorLike*(t: PType): bool {.inline.} =
  ## whether the type is an error. useful because of compiler legacy, as
  ## `tyError` isn't an enum field rather a const refering to `tyProxy`.
  ##
  ## xxx: currently we have no way to disambiguate between legacy and new
  t != nil and (t.kind == tyError or t.sym.isError)

proc isErrorLike*(s: PSym): bool {.inline.} =
  ## whether the symbol is an error. useful because of compiler legacy, as
  ## `skError` isn't an enum field rather a const refering to `skUnkonwn`. we
  ## disambiguate via the presence of the ast field being non-nil and of kind
  ## `nkError`
  s != nil and (s.isError or s.typ.isError)

proc isErrorLike*(n: PNode): bool {.inline.} =
  ## whether the node is an error, including error symbol, or error type
  ## xxx: longer term we should probably not produce nodes like these in the
  ##      first place and mark them as nkErrors with an appropriate error kind.
  n != nil and (
      case n.kind
      of nkError: true
      of nkSym: n.sym.isErrorLike
      of nkType: n.typ.isErrorLike
      else: n.typ.isError # if it has a type, it shouldn't be an error
    )

proc isGenericRoutineStrict*(s: PSym): bool {.inline.} =
  ## determines if this symbol represents a generic routine
  ## the unusual name is so it doesn't collide and eventually replaces
  ## `isGenericRoutine`
  s.kind in skProcKinds and s.ast.isGenericRoutine

proc isGenericRoutine*(s: PSym): bool {.inline.} =
  ## determines if this symbol represents a generic routine or an instance of
  ## one. This should be renamed accordingly and `isGenericRoutineStrict`
  ## should take this name instead.
  ##
  ## Warning/XXX: Unfortunately, it considers a proc kind symbol flagged with
  ## sfFromGeneric as a generic routine. Instead this should likely not be the
  ## case and the concepts should be teased apart:
  ## - generic definition
  ## - generic instance
  ## - either generic definition or instance
  s.kind in skProcKinds and (sfFromGeneric in s.flags or
                             s.ast.isGenericRoutine)

proc skipGenericOwner*(s: PSym): PSym =
  ## Generic instantiations are owned by their originating generic
  ## symbol. This proc skips such owners and goes straight to the owner
  ## of the generic itself (the module or the enclosing proc).
  result = if s.kind in skProcKinds and sfFromGeneric in s.flags:
             s.owner.owner
           else:
             s.owner

func isOwnedBy*(a, b: PSym): bool =
  ## Tests if `b` is the transitive owner of `a`, returns true if `a` got
  ## owned! :)
  var a = a.owner
  while a != nil and a.kind != skModule:
    if a == b: return true
    a = a.owner

proc originatingModule*(s: PSym): PSym =
  result = s.owner
  while result.kind != skModule: result = result.owner

proc isRoutine*(s: PSym): bool {.inline.} =
  result = s.kind in skProcKinds

proc isCompileTimeProc*(s: PSym): bool {.inline.} =
  result = s.kind == skMacro or
           s.kind in {skProc, skFunc} and sfCompileTime in s.flags

proc isRunnableExamples*(n: PNode): bool =
  # Templates and generics don't perform symbol lookups.
  result = n.kind == nkSym and n.sym.magic == mRunnableExamples or
    n.kind == nkIdent and n.ident.s == "runnableExamples"

proc requiredParams*(s: PSym): int =
  # Returns the number of required params (without default values)
  # XXX: Perhaps we can store this in the `offset` field of the
  # symbol instead?
  for i in 1..<s.typ.len:
    if s.typ.n[i].sym.ast != nil:
      return i - 1
  return s.typ.len - 1

proc requiredGenericParams*(s: PSym): int =
  # Returns the number of required generic parameters (without default
  # values).
  let params = s.ast[genericParamsPos]
  if params.kind == nkEmpty:
    # doesn't have generic parameters
    return

  for i in 0..<params.len:
    if params[i].sym.ast != nil:
      return i

  result = params.len

proc hasPattern*(s: PSym): bool {.inline.} =
  result = isRoutine(s) and s.ast[patternPos].kind != nkEmpty

iterator items*(n: PNode): PNode =
  for i in 0..<n.safeLen: yield n[i]

iterator pairs*(n: PNode): tuple[i: int, n: PNode] =
  for i in 0..<n.safeLen: yield (i, n[i])

proc isAtom*(n: PNode): bool {.inline.} =
  n.kind in nkWithoutSons - nkError

proc isEmptyType*(t: PType): bool {.inline.} =
  ## 'void' and 'typed' types are often equivalent to 'nil' these days:
  result = t == nil or t.kind in {tyVoid, tyTyped}

proc skipStmtList*(n: PNode): PNode =
  if n.kind in {nkStmtList, nkStmtListExpr}:
    for i in 0..<n.len-1:
      if n[i].kind notin {nkEmpty, nkCommentStmt}: return n
    result = n.lastSon
  else:
    result = n

proc isInfixAs*(n: PNode): bool =
  return n.kind == nkInfix and n[0].kind == nkIdent and n[0].ident.s == "as"

proc skipColon*(n: PNode): PNode =
  result = n
  if n.kind == nkExprColonExpr:
    result = n[1]

proc findUnresolvedStatic*(n: PNode): PNode =
  # n.typ == nil: see issue #14802
  if n.kind == nkSym and n.typ != nil and
     n.typ.kind == tyStatic and n.typ.n == nil:
    return n

  if n.kind != nkError:
    for son in n:
      let n = son.findUnresolvedStatic
      if n != nil: return n

  return nil

when false:
  proc containsNil*(n: PNode): bool =
    # only for debugging
    if n.isNil: return true
    for i in 0..<n.safeLen:
      if n[i].containsNil: return true


func hasDestructor*(t: PType): bool {.inline.} =
  ## Returns whether the underlying concrete type of `t` has attached lifetime
  ## tracking hooks (that is, is resource-like).
  result = tfHasAsgn in t.skipTypes(skipForHooks).flags

template incompleteType*(t: PType): bool =
  t.sym != nil and {sfForward, sfNoForward} * t.sym.flags == {sfForward}

template typeCompleted*(s: PSym) =
  incl s.flags, sfNoForward

template detailedInfo*(sym: PSym): string =
  sym.name.s

proc isInlineIterator*(typ: PType): bool {.inline.} =
  typ.kind == tyProc and tfIterator in typ.flags and typ.callConv != ccClosure

proc isClosureIterator*(typ: PType): bool {.inline.} =
  typ.kind == tyProc and tfIterator in typ.flags and typ.callConv == ccClosure

proc isClosure*(typ: PType): bool {.inline.} =
  typ.kind == tyProc and typ.callConv == ccClosure

proc isSinkParam*(s: PSym): bool {.inline.} =
  s.kind == skParam and s.typ.kind == tySink

proc isSinkType*(t: PType): bool {.inline.} =
  t.kind == tySink

const magicsThatCanRaise* = {
  mNone, mParseExprToAst, mParseStmtToAst, mEcho, mChckRange }

proc canRaiseConservative*(fn: PNode): bool =
  if fn.kind == nkSym and fn.sym.magic notin magicsThatCanRaise:
    result = false
  else:
    result = true

proc canRaise(fn: PNode): bool =
  if fn.kind == nkSym and (fn.sym.magic notin magicsThatCanRaise or
      {sfImportc, sfInfixCall} * fn.sym.flags == {sfImportc} or
      sfGeneratedOp in fn.sym.flags):
    result = false
  elif fn.kind == nkSym and fn.sym.magic == mEcho:
    result = true
  elif fn.typ != nil and fn.typ.kind == tyProc:
    let effects {.cursor.} = fn.typ.n[0]
    case effects.kind
    of nkSym:
      result = false # callable has no effects
    of nkEffectList:
      # if the effects were either not computed yet or there's no explicit
      # specification, assume that the procedure can raise
      result = effects.len < effectListLen or
               effects[exceptionEffects].isNil or
               effects[exceptionEffects].len > 0
    else:
      unreachable(effects.kind)
  else:
    # fn doesn't seem to be something callable, assume not raising
    result = false

proc canRaise*(panicsEnabled: bool, n: PNode): bool =
  ## 'true' if a call with `n` as the callee can exit via exceptional control-
  ## flow, otherwise 'false'. If panics are not enabled, this also includes
  ## all routines that are not certain magics, compiler procs, or imported.
  if n.kind == nkSym and ({sfNeverRaises, sfCompilerProc} * n.sym.flags != {}):
    false
  elif n.kind == nkSym and
       {sfImportc, sfInfixCall} * n.sym.flags == {sfImportc}:
    # imported JavaScript functions are excluded here, as they may raise
    # imported exceptions
    false
  elif panicsEnabled:
    # we know we can be strict:
    canRaise(n)
  else:
    # we have to be *very* conservative:
    canRaiseConservative(n)

proc skipAddr*(n: PNode): PNode {.inline.} =
  if n.kind == nkHiddenAddr: n[0] else: n

iterator genericParamsInMacroCall*(macroSym: PSym, call: PNode): (PSym, PNode) =
  ## For a macro call, yields the symbol for each generic parameter toghether
  ## with the *argument* provided to it
  let gp = macroSym.ast[genericParamsPos]
  for i in 0..<gp.len:
    let genericParam = gp[i].sym
    let posInCall = macroSym.typ.len + i
    if posInCall < call.len:
      yield (genericParam, call[posInCall])

proc endsInNoReturn*(n: PNode): bool =
  ## Checks if expression `n` ends in an unstructured exit (raise, return,
  ## etc.) or a call of a noreturn proc. This is meant to be called on a
  ## semmed `n`.
  var it = n
  while it.kind in {nkStmtList, nkStmtListExpr} and it.len > 0 or
        it.kind in {nkIfStmt, nkCaseStmt, nkBlockStmt, nkTryStmt} and it.typ.isEmptyType:
    case it.kind
    of nkStmtList, nkStmtListExpr, nkBlockStmt:
      it = it.lastSon
    of nkIfStmt, nkCaseStmt:
      it = it.lastSon.lastSon
    of nkTryStmt:
      it =
        case it[^1].kind
        of nkFinally:
          it[^2]
        of nkExceptBranch:
          it[^1]
        of nkAllNodeKinds - {nkFinally, nkExceptBranch}:
          unreachable()
    else:
      unreachable()
  result = it.kind in nkLastBlockStmts or
    it.kind in nkCallKinds and it[0].kind == nkSym and sfNoReturn in it[0].sym.flags

type
  NodePosName* = enum
    ## Named node position accessor
    PosLastIdent ## Last item in the identifier
    PosType ## Type position for variable declaration or a procedure. For
            ## procedure returns it's formal parameters (signature)
    PosInit ## Initialization expression
    PosProcBody ## Procedure body
    PosProcReturn ## Return value of the procedure in formal parameters
    PosProcArgs ## Formal parmeters in the procedure
    PosTypeBody ## Type definition body
    PosName ## Procedure name
    PosBody ## Generic statement body
    PosPragma ## Pragma position in the node body

  NodeSliceName* = enum
    ## Named node slice accessor
    SliceAllIdents ## All identifiers in the ident defs
    SliceAllArguments  ## All arguments in the formal parammeters
    SliceAllBranches ## All case statement branches
    SliceBranchExpressions ## All expressions in the `of` branch

proc `[]`*(node: PNode, pos: NodePosName): PNode =
  ## Get subnode by named position

  # NOTE: further expansion and clarifications of this node should happen
  # on as-needed basis. When adding clarifications for accessing different
  # *node kinds* please do update the documentation on the respective named
  # position enum values as well.
  case pos:
    of PosLastIdent: node[^3]
    of PosType:
      case node.kind:
        of routineDefs:
          node[3]

        else:
          node[^2]

    of PosInit: node[^1]
    of PosProcBody: node[6]
    of PosTypeBody: node[2]
    of PosProcArgs: node[3]
    of PosBody: node[^1]
    of PosProcReturn:
      assert node.kind == nkFormalParams, $node.kind
      node[0]
    of PosName: node[0]
    of PosPragma:
      case node.kind:
        of routineDefs:
          node[pragmasPos]

        of nkTypeDef:
          node[0][PosPragma]

        else:
          assert node.kind == nkPragmaExpr, $node.kind
          node[1]


proc `[]`*(node: PNode, slice: NodeSliceName): seq[PNode] =
  ## Access named node slice
  case slice:
    of SliceAllIdents: node.sons[0..^3]
    of SliceAllArguments, SliceAllBranches: node.sons[1..^1]
    of SliceBranchExpressions: node.sons[0 .. ^2]

iterator branches*(node: PNode): tuple[position: int, n: PNode] =
  ## Returns all branches of the ``case`` statement or expression `node` in
  ## order of occurrence. `position` is the 0-based position of the branch,
  ## not the index of the sub-node
  assert node.kind in {nkRecCase, nkCaseStmt}
  for i in 1..<node.len:
    yield (i-1, node[i])

iterator branchLabels*(node: PNode): (int, PNode) =
  ## Returns all labels of the branch-like constructs (i.e. ``of``, ``if``,
  ## ``elif``) that `node` represents, together with their position. For
  ## convenience, ``else`` branches are also allowed: they're treated as having
  ## no labels
  assert node.kind in {nkOfBranch, nkElifBranch, nkElse, nkElifExpr,
                       nkElseExpr, nkExceptBranch}
  for i in 0..<node.len-1:
    yield (i, node[i])

iterator names*(node: PNode): PNode =
  ## Returns the node for each name slot of the `node`, where `node` is the
  ## definition node for some symbol that is not a routine.
  assert node.kind in {nkIdentDefs, nkVarTuple, nkConstDef, nkForStmt}
  for i in 0..<node.len-2:
    yield node[i]

iterator forLoopDefs*(forStmt: PNode): PNode =
  ## Returns the nodes appearing in the name slots (including nested ones) of
  ## the provided ``nkForStmt`` node.
  assert forStmt.kind == nkForStmt
  for n in names(forStmt):
    if n.kind == nkVarTuple:
      for j in 0..<(n.len - 1):
        yield n[j]
    else:
      yield n
