#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This include file implements the semantic checking for magics.
# included from sem.nim

import
  compiler/ast/typesrenderer,
  compiler/front/optionsprocessor

proc semAddrArg(c: PContext; n: PNode): PNode =
  let n = semExprWithType(c, n)
  if n.isError:
    result = n
  elif isAssignable(c, n, true) in {arLValue, arLocalLValue}:
    analyseIfAddressTaken(n)
    result = n
  else:
    result = newError(c.config, n, PAstDiag(kind: adSemExprHasNoAddress))

proc semTypeOf(c: PContext; n: PNode): PNode =
  addInNimDebugUtils(c.config, "semTypeOf", n, result)
  var m = BiggestInt 1 # typeOfIter
  if n.len == 3:
    let mode = semConstExpr(c, n[2])
    if mode.kind != nkIntLit:
      localReport(c.config, VMReport(
        kind: rvmCannotEvaluateAtComptime,
        ast: n,
        location: some(n.info)))
    else:
      m = mode.intVal
  result = newNodeI(nkTypeOfExpr, n.info)
  let typExpr = semExprWithType(c, n[1], if m == 1: {efInTypeof} else: {})
  result.add typExpr
  if typExpr.isError:
    result = c.config.wrapError(result)
  else:
    result.typ = makeTypeDesc(c, typExpr.typ)

type
  SemAsgnMode = enum asgnNormal, noOverloadedSubscript, noOverloadedAsgn

proc semAsgn(c: PContext, n: PNode; mode=asgnNormal): PNode
proc semDeref(c: PContext, n: PNode): PNode
proc semSubscript(c: PContext, n: PNode, flags: TExprFlags): PNode

proc semArrGet(c: PContext; n: PNode; flags: TExprFlags): PNode =
  ## a basic array access `a[1]`, will be received here as a magic call in `n`
  ## with the following shape: `(nkCall (nkSym []) (nkSym a) (nkSym 1))`. This
  ## will:
  ## - if static (compile time) return the evaluated node
  ## - if no bracket operator is found for the type, an error
  ## - otherwise the typed array access expression

  addInNimDebugUtils(c.config, "semArrGet", n, result, flags)

  c.config.internalAssert(n.kind == nkCall, n.info,
                          "must be a call, got: " & $n.kind)
  checkMinSonsLen(n, 2, c.config)

  case n.len
  of 2:
    # this is a deref, `someArray[]` matches the generic `[]` ArrGet magic
    result = newNodeI(nkDerefExpr, n.info, 1)
    result[0] = n[1]
    result = semDeref(c, result)
  else:
    result = newNodeI(nkBracketExpr, n.info)
    for i in 1..<n.len:
      result.add(n[i])
    result = semSubscript(c, result, flags)

  if result.kind == nkCall:
    let x = copyTree(n)
    x[0] = newIdentNode(getIdent(c.cache, "[]"), n.info)
    result = bracketNotFoundError(c, x)

proc semArrPut(c: PContext; n: PNode; flags: TExprFlags): PNode =
  # rewrite `[]=`(a, i, x)  back to ``a[i] = x``.
  let b = newNodeI(nkBracketExpr, n.info)
  b.add(n[1].skipAddr)
  for i in 2..<n.len-1: b.add(n[i])
  result = newNodeI(nkAsgn, n.info, 2)
  result[0] = b
  result[1] = n.lastSon
  result = semAsgn(c, result, noOverloadedSubscript)

proc semAsgnOpr(c: PContext; n: PNode): PNode =
  result = newNodeI(nkAsgn, n.info, 2)
  result[0] = n[1]
  result[1] = n[2]
  result = semAsgn(c, result, noOverloadedAsgn)

proc semIsPartOf(c: PContext, n: PNode, flags: TExprFlags): PNode =
  var r = isPartOf(n[1], n[2])
  result = newIntNodeT(toInt128(ord(r)), n, c.idgen, c.graph)

proc expectIntLit(c: PContext, n: PNode): int =
  let x = c.semConstExpr(c, n)
  case x.kind
  of nkSIntLiterals: result = int(x.intVal)
  else: localReport(c.config, n, reportSem rsemIntLiteralExpected)

proc semInstantiationInfo(c: PContext, n: PNode): PNode =
  result = newNodeIT(nkTupleConstr, n.info, n.typ)
  let idx = expectIntLit(c, n[1])
  let useFullPaths = expectIntLit(c, n[2])
  let info = getInfoContext(c.config, idx)
  var filename = newNodeIT(nkStrLit, n.info, getSysType(c.graph, n.info, tyString))
  filename.strVal = if useFullPaths != 0: toFullPath(c.config, info) else: toFilename(c.config, info)
  var line = newNodeIT(nkIntLit, n.info, getSysType(c.graph, n.info, tyInt))
  line.intVal = toLinenumber(info)
  var column = newNodeIT(nkIntLit, n.info, getSysType(c.graph, n.info, tyInt))
  column.intVal = toColumn(info)
  # filename: string, line: int, column: int
  result.add(newTree(nkExprColonExpr, n.typ.n[0], filename))
  result.add(newTree(nkExprColonExpr, n.typ.n[1], line))
  result.add(newTree(nkExprColonExpr, n.typ.n[2], column))

proc toNode(t: PType, i: TLineInfo): PNode =
  result = newNodeIT(nkType, i, t)

const
  # these are types that use the bracket syntax for instantiation
  # they can be subjected to the type traits `genericHead` and
  # `Uninstantiated`
  tyUserDefinedGenerics* = {tyGenericInst, tyGenericInvocation,
                            tyUserTypeClassInst}

  tyMagicGenerics* = {tySet, tySequence, tyArray, tyOpenArray}

  tyGenericLike* = tyUserDefinedGenerics +
                   tyMagicGenerics +
                   {tyCompositeTypeClass}

proc uninstantiate(t: PType): PType =
  result = case t.kind
    of tyMagicGenerics: t
    of tyUserDefinedGenerics: t.base
    of tyCompositeTypeClass: uninstantiate t[1]
    else: t

proc getTypeDescNode(c: PContext; typ: PType, sym: PSym, info: TLineInfo): PNode =
  var resType = newType(tyTypeDesc, nextTypeId c.idgen, sym)
  rawAddSon(resType, typ)
  result = toNode(resType, info)

proc evalTypeTrait(c: PContext; traitCall: PNode, operand: PType, context: PSym): PNode =
  const skippedTypes = {tyTypeDesc, tyAlias, tySink}
  let trait = traitCall[0]
  c.config.internalAssert trait.kind == nkSym
  let orig = operand
  var operand = operand.skipTypes(skippedTypes)

  template operand2: PType =
    traitCall[2].typ.skipTypes({tyTypeDesc})

  template typeWithSonsResult(kind, sons): PNode =
    newTypeWithSons(context, kind, sons, c.idgen).toNode(traitCall.info)

  if operand.kind == tyGenericParam or (traitCall.len > 2 and operand2.kind == tyGenericParam):
    return traitCall  ## too early to evaluate

  let s = trait.sym.name.s
  case s
  of "or", "|":
    return typeWithSonsResult(tyOr, @[operand, operand2])
  of "and":
    return typeWithSonsResult(tyAnd, @[operand, operand2])
  of "not":
    if traitCall.len == 3:
      c.config.internalAssert traitCall[2].kind == nkNilLit
      # the operand is not generic anymore, let ``semTypeNode`` produce a
      # type with the not-nil modifier applied
      return makeTypeDesc(c, semTypeNode(c, traitCall, nil)).toNode(traitCall.info)
    else:
      return typeWithSonsResult(tyNot, @[operand])
  of "typeToString":
    var prefer = preferTypeName
    if traitCall.len >= 2:
      let preferStr = traitCall[2].strVal
      prefer = parseEnum[TPreferedDesc](preferStr)
    result = newStrNode(nkStrLit, operand.typeToString(prefer))
    result.typ = getSysType(c.graph, traitCall[1].info, tyString)
    result.info = traitCall.info
  of "name", "$":
    result = newStrNode(nkStrLit, operand.typeToString(preferTypeName))
    result.typ = getSysType(c.graph, traitCall[1].info, tyString)
    result.info = traitCall.info
  of "arity":
    result = newIntNode(nkIntLit, operand.len - ord(operand.kind==tyProc))
    result.typ = newType(tyInt, nextTypeId c.idgen, context)
    result.info = traitCall.info
  of "genericHead":
    var arg = operand
    case arg.kind
    of tyGenericInst:
      result = getTypeDescNode(c, arg.base, operand.owner, traitCall.info)
    # of tySequence: # this doesn't work
    #   var resType = newType(tySequence, operand.owner)
    #   result = toNode(resType, traitCall.info) # doesn't work yet
    else:
      localReport(
        c.config,
        traitCall.info,
        SemReport(
          kind: rsemGenericTypeExpected,
          typeMismatch: @[typeMismatch({tyGenericInst}, arg)]))

      result = newType(tyError, nextTypeId c.idgen, context).toNode(traitCall.info)
  of "stripGenericParams":
    result = uninstantiate(operand).toNode(traitCall.info)
  of "supportsCopyMem":
    let t = operand.skipTypes({tyVar, tyLent, tyGenericInst, tyAlias, tySink, tyInferred})
    let complexObj = containsGarbageCollectedRef(t) or
                     hasDestructor(t)
    result = newIntNodeT(toInt128(ord(not complexObj)), traitCall, c.idgen, c.graph)
  of "isNamedTuple":
    var operand = operand.skipTypes({tyGenericInst})
    let cond = operand.kind == tyTuple and operand.n != nil
    result = newIntNodeT(toInt128(ord(cond)), traitCall, c.idgen, c.graph)
  of "tupleLen":
    var operand = operand.skipTypes({tyGenericInst})
    assert operand.kind == tyTuple, $operand.kind
    result = newIntNodeT(toInt128(operand.len), traitCall, c.idgen, c.graph)
  of "distinctBase":
    var last = orig.skipTypes({tyTypeDesc})
    var arg = operand.skipTypes({tyGenericInst})
    let rec = semConstExpr(c, traitCall[2]).intVal != 0
    while arg.kind == tyDistinct:
      last = arg.base
      arg = last.skipTypes(skippedTypes + {tyGenericInst})
      if not rec: break
    result = getTypeDescNode(c, last, operand.owner, traitCall.info)
  of "rangeBase":
    # return the range's base type
    let arg = operand.skipTypes({tyGenericInst})
    if arg.kind == tyRange:
      result = getTypeDescNode(c, arg.base, operand.owner, traitCall.info)
    else:
      result = traitCall
      result[1] = c.config.newError(traitCall[1],
                                    PAstDiag(kind: adSemExpectedRangeType))
      result = c.config.wrapError(result)
  of "isCyclical":
    let r =
      if operand.skipTypes(abstractInst).kind in ConcreteTypes:
        isCyclePossible(operand, c.graph)
      else:
        false

    result = newIntNodeT(toInt128(ord(r)), traitCall, c.idgen, c.graph)
  else:
    localReport(c.config, traitCall.info, reportSym(
      rsemUnknownTrait, trait.sym))

    result = newNodeI(nkEmpty, traitCall.info)

proc semTypeTraits(c: PContext, n: PNode): PNode =
  checkMinSonsLen(n, 2, c.config)
  let t = n[1].typ
  c.config.internalAssert t != nil and t.kind == tyTypeDesc
  if t.len > 0:
    # This is either a type known to sem or a typedesc
    # param to a regular proc (again, known at instantiation)
    result = evalTypeTrait(c, n, t, getCurrOwner(c))
  else:
    # a typedesc variable, pass unmodified to evals
    result = n

proc semOrd(c: PContext, n: PNode): PNode =
  result = n
  let parType = n[1].typ
  if isOrdinalType(parType, allowEnumWithHoles=true):
    discard
  else:
    result = c.config.newError(n, PAstDiag(kind: adSemExpectedOrdinal,
                                           nonOrdTyp: parType))

    result.typ = errorType(c)

proc semBindSym(c: PContext, n: PNode): PNode =
  ## Evaluates a ``mNBindSym`` magic call, expanding it into either a
  ## symbol/symbol-choice or a ``NimNode`` literal, depending on the context
  addInNimDebugUtils(c.config, "semBindSym", n, result)

  # TODO: instead of manually testing whether the expression evaluates to a
  #       literal here, it would be simpler to explicitly use ``static``
  #       parameters for ``macros.bindSym``
  let sl = semConstExpr(c, n[1])
  if sl.kind notin {nkStrLit, nkRStrLit, nkTripleStrLit}:
    return newError(c.config, n, PAstDiag(kind: adSemStringLiteralExpected))

  let rule = semConstExpr(c, n[2])
  if rule.kind != nkIntLit or rule.intVal < 0 or
      rule.intVal > high(TSymChoiceRule).int:
    return newError(c.config, n, PAstDiag(kind: adSemConstExprExpected))

  let id = newIdentNode(getIdent(c.cache, sl.strVal), n.info)
  let s = qualifiedLookUp(c, id, {checkUndeclared})
  if s.isError:
    return s.ast
  elif s.isNil:
    return createUndeclaredIdentifierError(c, n[1], sl.strVal)

  let sc = symChoice(c, id, s, TSymChoiceRule(rule.intVal))
  if not inCompileTimeOnlyContext(c):
    # outside of static evaluation and macros, ``bindSym`` resolves to the
    # sym-choice nodes
    result = sc
  else:
    # inside static evaluation contexts and macros, the magic resolves to a
    # ``NimNode`` literal
    result = newTreeIT(nkNimNodeLit, n.info, n.typ): sc

proc semShallowCopy(c: PContext, n: PNode, flags: TExprFlags): PNode

proc semOf(c: PContext, n: PNode): PNode =
  ## Ensures that the 'of' operation `n` is valid and attempts to it, returning
  ## either a boolean literal, an error, or the original expression. The
  ## provided AST is expected to be typed and free of errors.
  assert n.len == 3
  assert n[1].kind != nkError
  assert n[2].kind != nkError
  if true:
    let a = skipTypes(n[1].typ, abstractPtrs)
    let b = skipTypes(n[2].typ, abstractPtrs)
    let x = skipTypes(n[1].typ, abstractPtrs-{tyTypeDesc})
    let y = skipTypes(n[2].typ, abstractPtrs-{tyTypeDesc})

    if x.kind == tyTypeDesc or y.kind != tyTypeDesc:
      result = newError(c.config, n, PAstDiag(kind: adSemExpectedObjectOfType))
    elif b.kind != tyObject or a.kind != tyObject:
      result = newError(c.config, n, PAstDiag(kind: adSemExpectedObjectOfType))
    else:
      let diff = inheritanceDiff(b, a)
      # | returns: 0 iff `a` == `b`
      # | returns: -x iff `a` is the x'th direct subclass of `b`
      # | returns: +x iff `a` is the x'th direct superclass of `b`
      # | returns: `maxint` iff `a` and `b` are not compatible at all
      if diff <= 0:
        # optimize to true:
        localReport(c.config, n, reportSem rsemConditionAlwaysTrue)
        result = newIntTypeNode(1, n.typ)
        result.info = n.info
      elif diff == high(int):
        if commonSuperclass(a, b) == nil:
          result = newError(c.config, n, PAstDiag(kind: adSemCannotBeOfSubtype))
        else:
          # optimize to 'false':
          localReport(c.config, n, reportSem rsemConditionAlwaysFalse)
          result = newIntTypeNode(0, n.typ)
          result.info = n.info
      else:
        # can only be evaluated at run-time
        result = n

proc semPrivateAccess(c: PContext, n: PNode): PNode =
  let t = n[1].typ[0].toObjectFromRefPtrGeneric
  c.currentScope.allowPrivateAccess.add t.sym
  result = newNodeIT(nkEmpty, n.info, getSysType(c.graph, n.info, tyVoid))

proc magicsAfterOverloadResolution(c: PContext, n: PNode,
                                   flags: TExprFlags): PNode =
  ## This is the preferred code point to implement magics.
  ## ``c`` the current module, a symbol table to a very good approximation
  ## ``n`` the ast like it would be passed to a real macro
  ## ``flags`` Some flags for more contextual information on how the
  ## "macro" is called.
  addInNimDebugUtils(c.config, "magicsAfterOverloadResolution", n, result,
                     flags)

  if n.isError:
    result = n
    return

  case n[0].sym.magic
  of mAddr:
    # XXX: wasn't this magic already processed in ``semMagic``?
    checkSonsLen(n, 2, c.config)
    result = n
    result[1] = semAddrArg(c, n[1])
    result.typ = makePtrType(c, result[1].typ)
  of mTypeOf:
    result = semTypeOf(c, n)
  of mSizeOf:
    result = foldSizeOf(c.config, n, n)
  of mAlignOf:
    result = foldAlignOf(c.config, n, n)
  of mOffsetOf:
    result = foldOffsetOf(c.config, n, n)
  of mArrGet:
    result = semArrGet(c, n, flags)
  of mArrPut:
    result = semArrPut(c, n, flags)
  of mAsgn:
    if n[0].sym.name.s == "=":
      result = semAsgnOpr(c, n)
    else:
      result = semShallowCopy(c, n, flags)
  of mEnumToStr:
    # overload resolution picked the generic enum-to-string magic.
    # Replace the symbol with that of the auto-generated procedure.
    let
      info = n[0].info
      prc = getToStringProc(c.graph, n[1].typ.skipTypes(abstractRange))
    result = n
    result[0] = newSymNode(prc, info)
  of mIsPartOf: result = semIsPartOf(c, n, flags)
  of mTypeTrait: result = semTypeTraits(c, n)
  of mAstToStr:
    result = newStrNodeT(renderTree(n[1], {renderNoComments}), n, c.graph)
    result.typ = getSysType(c.graph, n.info, tyString)
  of mInstantiationInfo: result = semInstantiationInfo(c, n)
  of mOrd: result = semOrd(c, n)
  of mOf: result = semOf(c, n)
  of mHigh, mLow: result = semLowHigh(c, n, n[0].sym.magic)
  of mShallowCopy: result = semShallowCopy(c, n, flags)
  of mNBindSym:
    result = semBindSym(c, n)
  of mProcCall:
    result = n
    result.typ = n[1].typ
  of mDotDot:
    result = n
  of mPlugin:
    let plugin = getPlugin(c.cache, n[0].sym)
    if plugin.isNil:
      localReport(c.config, n.info, reportSym(
        rsemCannotFindPlugin, sym = n[0].sym))
      result = n
    else:
      result = plugin(c, n)
  of mDestroy, mTrace:
    # these are explicit calls to hooks for which overload resolution picked
    # the generic magic from system. Leave them be; ``sempass2`` will patch
    # these calls with the correct procedure
    result = n
  of mSetLengthSeq:
    result = n
    let seqType = result[1].typ.skipTypes({tyPtr, tyRef, # in case we had auto-dereferencing
                                           tyVar, tyGenericInst, tySink,
                                           tyAlias, tyUserTypeClassInst})
    if seqType.kind == tySequence and seqType.base.requiresInit:
      localReport(c.config, n.info, reportTyp(
        rsemUnsafeSetLen, seqType.base))
  of mDefault:
    result = n
    c.config.internalAssert result[1].typ.kind == tyTypeDesc
    let constructed = result[1].typ.base
    if constructed.requiresInit:
      localReport(c.config, n.info, reportTyp(
        rsemUnsafeDefault, constructed))
  of mIsolate:
    if not checkIsolate(n[1]):
      localReport(c.config, n.info, reportAst(rsemCannotIsolate, n[1]))
    result = n
  of mPred:
    if n[1].typ.skipTypes(abstractInst).kind in {tyUInt..tyUInt64}:
      n[0].sym.magic = mSubU
    result = n
  of mPrivateAccess:
    result = semPrivateAccess(c, n)
  of mCompileOption:
    let a = evalConstExpr(c, n[1])
    case a.kind
    of nkError:
      result = copyTreeWithoutNode(n, n[1])
      result[1] = a
      result = c.config.wrapError(result)
    else:
      result =
        case optionsprocessor.testCompileOption(c.config, a.getStr)
        of compileOptCheckSuccessTrue:
          newIntNodeT(toInt128(ord(true)), n, c.idgen, c.graph)
        of compileOptCheckSuccessFalse:
          newIntNodeT(toInt128(ord(false)), n, c.idgen, c.graph)
        of compileOptCheckWarnFalseDeprecated:
          # xxx: there must be a nicer way to handle warnings, either inline as
          #      error or side-channel into diagnostics somewhere?
          if rsemDeprecatedCompilerOpt in c.config.warningAsErrors:
            c.config.newError(n, PAstDiag(kind: adSemDeprecatedCompilerOpt,
                                          badCompilerOpt: a))
          else:
            # TODO: remove legacy reports cruft
            c.config.localReport(n.info,
              SemReport(kind: rsemDeprecatedCompilerOpt, str: a.getStr))
            newIntNodeT(toInt128(ord(false)), n, c.idgen, c.graph)
        of compileOptCheckFailedWithInvalidOption:
          c.config.newError(n, PAstDiag(kind: adSemCompilerOptionInvalid,
                                        badCompilerOpt: a))
  of mCompileOptionArg:
    let
      a = evalConstExpr(c, n[1])
      b = evalConstExpr(c, n[2])
    if nkError in {a.kind, b.kind}:
      result = copyTreeWithoutNodes(n, n[1], n[2])
      result[1] = a
      result[2] = b
      result = c.config.wrapError(result)
    else:
      result =
        case optionsprocessor.testCompileOptionArg(c.config, a.getStr, b.getStr)
        of compileOptArgCheckSuccessTrue:
          newIntNodeT(toInt128(ord(true)), n, c.idgen, c.graph)
        of compileOptArgCheckSuccessFalse:
          newIntNodeT(toInt128(ord(false)), n, c.idgen, c.graph)
        of compileOptArgCheckWarnFalseDeprecated:
          if rsemDeprecatedCompilerOptArg in c.config.warningAsErrors:
            c.config.newError(n, PAstDiag(kind: adSemDeprecatedCompilerOptArg,
                                          compilerOpt: a,
                                          compilerOptArg: b))
          else:
            # TODO: remove legacy reports cruft
            c.config.localReport(n.info,
              SemReport(kind: rsemDeprecatedCompilerOptArg, str: a.getStr,
                        compilerOptArg: b.getStr))
            newIntNodeT(toInt128(ord(false)), n, c.idgen, c.graph)
        of compileOptArgCheckFailedWithUnexpectedValue:
          c.config.newError(n, PAstDiag(kind: adSemCompilerOptionArgInvalid,
                                        forCompilerOpt: a,
                                        badCompilerOptArg: b))
        of compileOptArgCheckFailedWithInvalidOption:
          c.config.newError(n, PAstDiag(kind: adSemCompilerOptionInvalid,
                                        badCompilerOpt: a))
  else:
    result = n
