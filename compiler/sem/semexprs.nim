#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## this module does the semantic checking for expressions
## included from sem.nim

when defined(nimCompilerStacktraceHints):
  import std/stackframes

proc semTemplateExpr(c: PContext, n: PNode, s: PSym,
                     flags: TExprFlags = {}): PNode =
  rememberExpansion(c, n.info, s)
  let info = getCallLineInfo(n)
  markUsed(c, info, s)
  onUse(info, s)
  # Note: This is n.info on purpose. It prevents template from creating an info
  # context when called from an another template
  pushInfoContext(c.config, n.info, s)
  result = evalTemplate(n, s, getCurrOwner(c), c.config, c.cache,
                        c.templInstCounter, c.idgen, efFromHlo in flags)
  if efNoSemCheck notin flags: result = semAfterMacroCall(c, n, result, s, flags)
  popInfoContext(c.config)

  # XXX: A more elaborate line info rewrite might be needed
  result.info = info

proc semFieldAccess(c: PContext, n: PNode, flags: TExprFlags = {}): PNode

template rejectEmptyNode(n: PNode) =
  # No matter what a nkEmpty node is not what we want here
  if n.kind == nkEmpty:
    semReportIllformedAst(c.config, n, "Unexpected empty node")

proc semOperand(c: PContext, n: PNode, flags: TExprFlags = {}): PNode =
  # same as 'semExprWithType' but doesn't check for proc vars
  rejectEmptyNode(n)
  result = semExpr(c, n, flags + {efOperand})
  
  if result.typ != nil:
    # XXX tyGenericInst here?
    if result.typ.kind == tyProc and hasUnresolvedParams(result, {efOperand}):
      result = c.config.newError(n, reportAst(rsemProcHasNoConcreteType, n))
    elif result.typ.kind in {tyVar, tyLent}:
      result = newDeref(result)
  elif {efWantStmt, efAllowStmt} * flags != {}:
    result.typ = newTypeS(tyVoid, c)
  else:
    result = c.config.newError(n, reportAst(rsemExpressionHasNoType, result))

proc semExprCheck(c: PContext, n: PNode, flags: TExprFlags): PNode =
  addInNimDebugUtils(c.config, "semExprCheck", n, result, flags)
  rejectEmptyNode(n)
  result = semExpr(c, n, flags+{efWantValue})

  let
    isEmpty = result.kind == nkEmpty
    isError = result.kind == nkError
    isTypeError = result.typ != nil and result.typ.kind == tyError

  if isError:
    discard # no need to do anything

  elif isEmpty or isTypeError:
    # bug #12741, redundant error messages are the lesser evil here:
    result = c.config.newError(n, reportSem rsemExpressionHasNoType)

proc semExprWithType(c: PContext, n: PNode, flags: TExprFlags = {}): PNode =
  addInNimDebugUtils(c.config, "semExprWithType", n, result, flags)
  result = semExprCheck(c, n, flags)
  if result.typ == nil and efInTypeof in flags:
    result.typ = c.voidType

  elif result.typ == nil or result.typ == c.enforceVoidContext:
    result = c.config.newError(n, reportAst(
      rsemExpressionHasNoType, result))

  elif result.typ.kind == tyError:
    # associates the type error to the current owner
    result.typ = errorType(c)

  elif result.typ.kind in {tyVar, tyLent}:
    result = newDeref(result)

proc semExprNoDeref(c: PContext, n: PNode, flags: TExprFlags = {}): PNode =
  result = semExprCheck(c, n, flags)
  if result.typ == nil:
    result = c.config.newError(n, reportAst(
      rsemExpressionHasNoType, result))

proc semSymGenericInstantiation(c: PContext, n: PNode, s: PSym): PNode =
  result = symChoice(c, n, s, scClosed)

proc inlineConst(c: PContext, n: PNode, s: PSym): PNode {.inline.} =
  if s.ast.isNil:
    result = c.config.newError(n, reportSym(
      rsemConstantOfTypeHasNoValue, s))
  else:
    result = copyTree(s.ast)
    result.typ = s.typ
    result.info = n.info

type
  TConvStatus = enum
    convOK,
    convNotNeedeed,
    convNotLegal,
    convNotInRange

proc checkConversionBetweenObjects(castDest, src: PType; pointers: int): TConvStatus =
  let diff = inheritanceDiff(castDest, src)
  return if diff == high(int) or (pointers > 1 and diff != 0):
      convNotLegal
    else:
      convOK

const
  IntegralTypes = {tyBool, tyEnum, tyChar, tyInt..tyUInt64}

proc checkConvertible(c: PContext, targetTyp: PType, src: PNode): TConvStatus =
  let srcTyp = src.typ.skipTypes({tyStatic})
  result = convOK
  if sameType(targetTyp, srcTyp) and targetTyp.sym == srcTyp.sym:
    # don't annoy conversions that may be needed on another processor:
    if targetTyp.kind notin IntegralTypes+{tyRange}:
      result = convNotNeedeed
    return
  var d = skipTypes(targetTyp, abstractVar)
  var s = srcTyp
  if s.kind in tyUserTypeClasses and s.isResolvedUserTypeClass:
    s = s.lastSon
  s = skipTypes(s, abstractVar-{tyTypeDesc})
  var pointers = 0
  while (d != nil) and (d.kind in {tyPtr, tyRef}):
    if d.kind != s.kind:
      break
    else:
      d = d.lastSon
      s = s.lastSon
    inc pointers

  let targetBaseTyp = skipTypes(targetTyp, abstractVarRange)
  let srcBaseTyp = skipTypes(srcTyp, abstractVarRange-{tyTypeDesc})

  if d == nil:
    result = convNotLegal
  elif d.skipTypes(abstractInst).kind == tyObject and s.skipTypes(abstractInst).kind == tyObject:
    result = checkConversionBetweenObjects(d.skipTypes(abstractInst), s.skipTypes(abstractInst), pointers)
  elif (targetBaseTyp.kind in IntegralTypes) and
      (srcBaseTyp.kind in IntegralTypes):
    if targetTyp.kind == tyEnum and srcBaseTyp.kind == tyEnum:
      localReport(c.config, src.info, SemReport(
        kind: rsemSuspiciousEnumConv,
        ast: src,
        typeMismatch: @[
          c.config.typeMismatch(formal = targetTyp, actual = srcBaseTyp)]))

    # `elif` would be incorrect here
    if targetTyp.kind == tyBool:
      discard "convOk"
    elif targetTyp.isOrdinalType:
      if src.kind in nkCharLit..nkUInt64Lit and
          src.getInt notin firstOrd(c.config, targetTyp)..lastOrd(c.config, targetTyp):
        result = convNotInRange
      elif src.kind in nkFloatLit..nkFloat64Lit:
        if not src.floatVal.inInt128Range:
          result = convNotInRange
        elif src.floatVal.toInt128 notin firstOrd(c.config, targetTyp)..lastOrd(c.config, targetTyp):
          result = convNotInRange
    elif targetBaseTyp.kind in tyFloat..tyFloat64:
      if src.kind in nkFloatLit..nkFloat64Lit and
          not floatRangeCheck(src.floatVal, targetTyp):
        result = convNotInRange
      elif src.kind in nkCharLit..nkUInt64Lit and
          not floatRangeCheck(src.intVal.float, targetTyp):
        result = convNotInRange
  else:
    # we use d, s here to speed up that operation a bit:
    case cmpTypes(c, d, s)
    of isNone, isGeneric:
      if not compareTypes(
        targetTyp.skipTypes(abstractVar),
        srcTyp,
        dcEqIgnoreDistinct
      ):
        result = convNotLegal
    else:
      discard

proc isCastable(c: PContext; dst, src: PType): bool =
  ## Checks whether the source type can be cast to the destination type.
  ## Casting is very unrestrictive; casts are allowed as long as
  ## castDest.size >= src.size, and typeAllowed(dst, skParam)
  #const
  #  castableTypeKinds = {tyInt, tyPtr, tyRef, tyCstring, tyString,
  #                       tySequence, tyPointer, tyNil, tyOpenArray,
  #                       tyProc, tySet, tyEnum, tyBool, tyChar}
  let src = src.skipTypes(tyUserTypeClasses)
  if skipTypes(dst, abstractInst-{tyOpenArray}).kind == tyOpenArray:
    return false
  if skipTypes(src, abstractInst-{tyTypeDesc}).kind == tyTypeDesc:
    return false
  if skipTypes(dst, abstractInst).kind == tyBuiltInTypeClass:
    return false
  let conf = c.config
  if conf.selectedGC in {gcArc, gcOrc}:
    let d = skipTypes(dst, abstractInst)
    let s = skipTypes(src, abstractInst)
    if d.kind == tyRef and s.kind == tyRef and s[0].isFinal != d[0].isFinal:
      return false
    elif d.kind in IntegralTypes and s.kind in {tyString, tySequence}:
      return false

  var dstSize, srcSize: BiggestInt
  dstSize = computeSize(conf, dst)
  srcSize = computeSize(conf, src)
  if dstSize == -3 or srcSize == -3: # szUnknownSize
    # The Nim compiler can't detect if it's legal or not.
    # Just assume the programmer knows what he is doing.
    return true
  if dstSize < 0:
    result = false
  elif srcSize < 0:
    result = false
  elif typeAllowed(dst, skParam, c) != nil:
    result = false
  elif dst.kind == tyProc and dst.callConv == ccClosure:
    result = src.kind == tyProc and src.callConv == ccClosure
  else:
    result = (dstSize >= srcSize) or
        (skipTypes(dst, abstractInst).kind in IntegralTypes) or
        (skipTypes(src, abstractInst-{tyTypeDesc}).kind in IntegralTypes)
  if result and src.kind == tyNil:
    result = dst.size <= conf.target.ptrSize

proc isSymChoice(n: PNode): bool {.inline.} =
  result = n.kind in nkSymChoices

proc maybeLiftType(t: var PType, c: PContext, info: TLineInfo) =
  # XXX: liftParamType started to perform addDecl
  # we could do that instead in semTypeNode by snooping for added
  # gnrc. params, then it won't be necessary to open a new scope here
  openScope(c)
  var lifted = liftParamType(c, skType, newNodeI(nkArgList, info),
                             t, ":anon", info)
  closeScope(c)
  if lifted != nil: t = lifted

proc semConv(c: PContext, n: PNode): PNode =
  if n.len != 2:
    result = c.config.newError(n, semReportCountMismatch(
      rsemTypeConversionArgumentMismatch, expected = 1, got = n.len - 1, n))
    return

  result = newNodeI(nkConv, n.info)

  var targetType = semTypeNode(c, n[0], nil)
  case targetType.kind
  of tyTypeDesc:
    c.config.internalAssert targetType.len > 0

    if targetType.base.kind == tyNone:
      return semTypeOf(c, n)
    else:
      targetType = targetType.base
  of tyStatic:
    var evaluated = semStaticExpr(c, n[1])
    if evaluated.kind == nkType or evaluated.typ.kind == tyTypeDesc:
      result = n
      result.typ = c.makeTypeDesc semStaticType(c, evaluated, nil)
      return
    elif targetType.base.kind == tyNone:
      return evaluated
    else:
      targetType = targetType.base
  else: discard

  maybeLiftType(targetType, c, n[0].info)

  var hasError = false

  template handleLentOrSunkType(c: PContext, op: PNode, info: TLineInfo, targetType: PType): PNode =
    let
      baseType = semTypeNode(c, op, nil).skipTypes({tyTypeDesc})
      t = newTypeS(targetType.kind, c)
      res = newNodeI(nkType, info)
    
    t.rawAddSonNoPropagationOfTypeFlags baseType
    
    res.typ = makeTypeDesc(c, t)
    res

  if targetType.kind in {tySink, tyLent}:
    return handleLentOrSunkType(c, n[1], n.info, targetType)
  else:
    let s = qualifiedLookUp(c, n[0], {})
    if s.isError:
      result.add s.ast
      hasError = true
    else:
      result.add copyTree(n[0])

  # special case to make MyObject(x = 3) produce a nicer error message:
  if n[1].kind == nkExprEqExpr and
      targetType.skipTypes(abstractPtrs).kind == tyObject:
    result = c.config.newError(n, reportSem rsemUnexpectedEqInObjectConstructor, posInfo = n[1].info)
    return

  template handleError(c: PContext, result: PNode, hasError: bool): PNode =
    if hasError and result.kind != nkError:
      c.config.wrapError(result)
    else:
      result

  var op = semExprWithType(c, n[1])
  if targetType.kind != tyGenericParam and targetType.isMetaType:
    let final = inferWithMetatype(c, targetType, op, true)
    result.add final
    result.typ = final.typ
    return handleError(c, result, hasError)

  result.typ = targetType
  # XXX op is overwritten later on, this is likely added too early
  # here or needs to be overwritten too then.
  result.add op

  if targetType.kind == tyGenericParam:
    result.typ = makeTypeFromExpr(c, copyTree(result))
    return handleError(c, result, hasError)

  if not isSymChoice(op):
    let status = checkConvertible(c, result.typ, op)
    case status
    of convOK:
      # handle SomeProcType(SomeGenericProc)
      if op.kind == nkSym and op.sym.isGenericRoutine:
        result[1] = fitNode(c, result.typ, result[1], result.info)
      elif op.kind in {nkPar, nkTupleConstr} and targetType.kind == tyTuple:
        op = fitNode(c, targetType, op, result.info)
    of convNotNeedeed:
      localReport(c.config, n.info, reportTyp(
        rsemConvFromXtoItselfNotNeeded, result.typ, ast = result))

    of convNotLegal:
      result = fitNode(c, result.typ, result[1], result.info)
      if result == nil:
        result = c.config.newError(n, SemReport(
          kind: rsemIllegalConversion,
          typeMismatch: @[
            c.config.typeMismatch(formal = result.typ, actual = op.typ)]))

    of convNotInRange:
      result = c.config.newError(n, reportAst(
        rsemCannotBeConvertedTo, op, typ = result.typ))

  else:
    for it in op:
      let status = checkConvertible(c, result.typ, it)
      if status in {convOK, convNotNeedeed}:
        markUsed(c, n.info, it.sym)
        onUse(n.info, it.sym)
        markIndirect(c, it.sym)
        return
          if hasError:
            handleError(c, result, hasError)
          else:
            it
    errorUseQualifier(c, n.info, op[0].sym)

proc semCast(c: PContext, n: PNode): PNode =
  ## Semantically analyze a casting ("cast[type](param)")
  checkSonsLen(n, 2, c.config)
  let targetType = semTypeNode(c, n[0], nil)
  let castedExpr = semExprWithType(c, n[1])
  if tfHasMeta in targetType.flags:
    result = c.config.newError(n, reportTyp(rsemCannotCastToNonConcrete, targetType),
                               posInfo = n[0].info)
    return

  if not isCastable(c, targetType, castedExpr.typ):
    result = c.config.newError(n, SemReport(
      kind: rsemCannotCastTypes,
      typeMismatch: @[
        c.config.typeMismatch(formal = targetType, actual = castedExpr.typ)]))
    return

  result = newNodeI(nkCast, n.info)
  result.typ = targetType
  result.add copyTree(n[0])
  result.add castedExpr

proc semLowHigh(c: PContext, n: PNode, m: TMagic): PNode =
  const
    opToStr: array[mLow..mHigh, string] = ["low", "high"]
  if n.len != 2:
    result = c.config.newError(n, reportStr(
      rsemExpectedTypeOrValue, opToStr[m]))
    return

  else:
    n[1] = semExprWithType(c, n[1], {efDetermineType})
    var typ = skipTypes(n[1].typ, abstractVarRange + {tyTypeDesc, tyUserTypeClassInst})
    case typ.kind
    of tySequence, tyString, tyCstring, tyOpenArray, tyVarargs:
      n.typ = getSysType(c.graph, n.info, tyInt)
    of tyArray:
      n.typ = typ[0] # indextype
      if n.typ.kind == tyRange and emptyRange(n.typ.n[0], n.typ.n[1]): #Invalid range
        n.typ = getSysType(c.graph, n.info, tyInt)
    of tyInt..tyInt64, tyChar, tyBool, tyEnum, tyUInt..tyUInt64, tyFloat..tyFloat64:
      n.typ = n[1].typ.skipTypes({tyTypeDesc})
    of tyGenericParam:
      # prepare this for resolving in semtypinst:
      # we must use copyTree here in order to avoid creating a cycle
      # that could easily turn into an infinite recursion in semtypinst
      n.typ = makeTypeFromExpr(c, n.copyTree)
    else:
      result = c.config.newError(n, reportTyp(
        rsemInvalidArgumentFor, typ, str = opToStr[m]))
      return

    result = n

proc fixupStaticType(c: PContext, n: PNode) =
  # This proc can be applied to evaluated expressions to assign
  # them a static type.
  #
  # XXX: with implicit static, this should not be necessary,
  # because the output type of operations such as `semConstExpr`
  # should be a static type (as well as the type of any other
  # expression that can be implicitly evaluated). For now, we
  # apply this measure only in code that is enlightened to work
  # with static types.
  if n.typ.kind != tyStatic:
    n.typ = newTypeWithSons(getCurrOwner(c), tyStatic, @[n.typ], c.idgen)
    n.typ.n = n # XXX: cycles like the one here look dangerous.
                # Consider using `n.copyTree`

proc isOpImpl(c: PContext, n: PNode, flags: TExprFlags): PNode =
  ## implements `is`, for `x is Y` where x is an expression and `Y` is a type
  ## or an expression whose type is compared with `x`'s type.
  c.config.internalAssert:
    n.len == 3 and n[1].typ != nil and
    n[2].kind in {nkStrLit..nkTripleStrLit, nkType}

  var
    res = false
    t1 = n[1].typ
    t2 = n[2].typ

  if t1.kind == tyTypeDesc and t2.kind != tyTypeDesc:
    t1 = t1.base

  if n[2].kind in {nkStrLit..nkTripleStrLit}:
    case n[2].strVal.normalize
    of "closure":
      let t = skipTypes(t1, abstractRange)
      res = t.kind == tyProc and
            t.callConv == ccClosure
    of "iterator":
      let t = skipTypes(t1, abstractRange)
      res = t.kind == tyProc and
            t.callConv == ccClosure and
            tfIterator in t.flags
    else:
      res = false
  else:
    if t1.skipTypes({tyGenericInst, tyAlias, tySink, tyDistinct}).kind != tyGenericBody:
      maybeLiftType(t2, c, n.info)
    else:
      #[
      for this case:
      type Foo = object[T]
      Foo is Foo
      ]#
      discard
    var m = newCandidate(c, t2)
    if efExplain in flags:
      m.error.diagnosticsEnabled = true
    res = typeRel(m, t2, t1) >= isSubtype # isNone
    if m.error.diagnosticsEnabled and m.error.diag.reports.len > 0:
      localReport(c.config, n.info, SemReport(
        ast: n,
        kind: rsemDiagnostics,
        diag: m.error.diag
      ))

    # `res = sameType(t1, t2)` would be wrong, e.g. for `int is (int|float)`

  result = newIntNode(nkIntLit, ord(res))
  result.typ = n.typ
  result.info = n.info

proc semIs(c: PContext, n: PNode, flags: TExprFlags): PNode =
  addInNimDebugUtils(c.config, "semIs", n, result, flags)

  if n.len != 3:
    result = c.config.newError(n, semReportCountMismatch(
      rsemWrongNumberOfArguments, 1, 2, n))
    return

  let boolType = getSysType(c.graph, n.info, tyBool)
  n.typ = boolType
  var liftLhs = true

  n[1] = semExprWithType(c, n[1], flags + {efDetermineType, efWantIterator})

  case n[2].kind
  of nkStrLit..nkTripleStrLit:
    n[2] = semExpr(c, n[2])
  of nkError:
    discard # below we'll wrap the result in an error
  else:
    let t2 = semTypeNode(c, n[2], nil)
    n[2] = newNodeIT(nkType, n[2].info, t2)
    if t2.kind == tyStatic:
      let evaluated = tryConstExpr(c, n[1])
      if evaluated != nil:
        c.fixupStaticType(evaluated)
        n[1] = evaluated
      else:
        result = newIntNode(nkIntLit, 0)
        result.typ = boolType
        result.info = n.info
        return
    elif t2.kind == tyTypeDesc and
        (t2.base.kind == tyNone or tfExplicit in t2.flags):
      # When the right-hand side is an explicit type, we must
      # not allow regular values to be matched against the type:
      liftLhs = false

  var lhsType = n[1].typ
  if n[1].isError or n[2].isError:
    result = wrapError(c.config, n)
  elif lhsType.kind == tyTypeDesc and (lhsType.base.kind == tyNone or
     (c.inGenericContext > 0 and lhsType.base.containsGenericType)):
    # BUGFIX: don't evaluate this too early: ``T is void``
    result = n
  else:
    if lhsType.kind != tyTypeDesc and liftLhs:
      n[1] = makeTypeSymNode(c, lhsType, n[1].info)
    result = isOpImpl(c, n, flags)

proc semOpAux(c: PContext, n: PNode): bool =
  ## Returns whether n contains errors
  ## This does not not wrap child errors in n
  ## the caller has to handle that
  const flags = {efDetermineType}
  for i in 1..<n.len:
    var a = n[i]
    if a.kind == nkExprEqExpr and a.len == 2:
      let
        info = a[0].info
        ident = legacyConsiderQuotedIdent(c, a[0], a)
      a[0] = newIdentNode(ident, info)
      a[1] = semExprWithType(c, a[1], flags)
      if a[1].isError:
        result = true
      a.typ = a[1].typ
    else:
      n[i] = semExprWithType(c, a, flags)
      if n[1].isError:
        result = true

proc overloadedCallOpr(c: PContext, n: PNode): PNode =
  # quick check if there is *any* () operator overloaded:
  var par = getIdent(c.cache, "()")
  var amb = false
  if searchInScopes(c, par, amb) == nil:
    result = nil
  else:
    result = newNodeI(nkCall, n.info)
    result.add newIdentNode(par, n.info)
    for i in 0..<n.len: result.add n[i]
    result = semExpr(c, result)

proc changeType(c: PContext, n: PNode, newType: PType, check: bool): PNode =
  result = n  # xxx: typically avoid this but this doesn't look like a semantic
              #      analysis proc resulting in a production but more an
              #      operation/helper mutating an existing production

  var hasError = false

  case n.kind
  of nkCurly, nkBracket:
    for i, kid in n.pairs:
      result[i] = changeType(c, kid, elemType(newType), check)
      if result[i].isError:
        hasError = true
  of nkPar, nkTupleConstr:
    let tup = newType.skipTypes({tyGenericInst, tyAlias, tySink, tyDistinct})
    
    case tup.kind
    of tyTuple:
      if n.len > 0 and n[0].kind == nkExprColonExpr:
        # named tuple
        for i, colonExpr in n.pairs:
          let
            key = colonExpr[0]
            val = colonExpr[1]
            
          case key.kind
          of nkSym:
            let elemTyp =
              if tup.n.isNil:
                tup[i]
              else:
                let f = getSymFromList(tup.n, key.sym.name)
                if f.isNil:
                  result = newError(c.config, n,
                                    reportSym(rsemUnknownIdentifier, key.sym))
                  return # hard error
                f.typ

            result[i][1] = changeType(c, val, elemTyp, check)

            if result[i].isError:
              hasError = true
          else:
            result = newError(c.config, key,
                              reportAst(rsemInvalidTupleConstructor, ast = n))
            return # hard error
      else:
        # positional args
        for i, elem in n.pairs:
          result[i] = changeType(c, elem, tup[i], check)

          if result[i].isError:
            hasError = true
    of tyObject:
      result = n    # xxx: feels like `changeType` the proc is a hack
      return        # original bug: https://github.com/nim-lang/Nim/issues/2602
    else:
      result = newError(c.config, n,
                        reportAst(rsemNoTupleTypeForConstructor, ast = n))
      return # hard error
  of nkCharLit..nkUInt64Lit:
    if check and n.kind != nkUInt64Lit and not sameType(n.typ, newType):
      let val = n.intVal
      if val < firstOrd(c.config, newType) or val > lastOrd(c.config, newType):
        result = newError(c.config, n,
                          reportAst(rsemCannotBeConvertedTo, n, typ = newType))
  of nkFloatLit..nkFloat64Lit:
    if check and not floatRangeCheck(n.floatVal, newType):
      result = newError(c.config, n,
                        reportAst(rsemCannotBeConvertedTo, n, typ = newType))
  of nkError:
    return    # return an error
  else:
    discard
  
  n.typ = newType # `n` is either the wrongNode in an error or same as `result`
  if hasError and result.kind != nkError:
    result = c.config.wrapError(result)


proc arrayConstrType(c: PContext, n: PNode): PType =
  var typ = newTypeS(tyArray, c)
  rawAddSon(typ, nil)     # index type
  if n.len == 0:
    rawAddSon(typ, newTypeS(tyEmpty, c)) # needs an empty basetype!
  else:
    var t = skipTypes(n[0].typ, {tyGenericInst, tyVar, tyLent, tyOrdinal, tyAlias, tySink})
    addSonSkipIntLit(typ, t, c.idgen)
  typ[0] = makeRangeType(c, 0, n.len - 1, n.info)
  result = typ

proc semArrayConstr(c: PContext, n: PNode, flags: TExprFlags): PNode =
  result = newNodeI(nkBracket, n.info)
  result.typ = newTypeS(tyArray, c)
  rawAddSon(result.typ, nil)     # index type
  var
    firstIndex, lastIndex: Int128
    indexType = getSysType(c.graph, n.info, tyInt)
    lastValidIndex = lastOrd(c.config, indexType)
  if n.len == 0:
    rawAddSon(result.typ, newTypeS(tyEmpty, c)) # needs an empty basetype!
    lastIndex = toInt128(-1)
  else:
    var x = n[0]
    if x.kind == nkExprColonExpr and x.len == 2:
      var idx = semConstExpr(c, x[0])
      if not isOrdinalType(idx.typ):
        result = n
        result[0][0] = c.config.newError(x[0]):
          block:
            var r = reportTyp(rsemExpectedOrdinal, idx.typ, ast = result)
            r.wrongNode = idx
            r
        result = c.config.wrapError(result)
        return

      else:
        firstIndex = getOrdValue(idx)
        lastIndex = firstIndex
        indexType = idx.typ
        lastValidIndex = lastOrd(c.config, indexType)
        x = x[1]

    let yy = semExprWithType(c, x)
    var typ = yy.typ
    result.add yy
    #var typ = skipTypes(result[0].typ, {tyGenericInst, tyVar, tyLent, tyOrdinal})
    for i in 1..<n.len:
      if lastIndex == lastValidIndex:
        let validIndex = makeRangeType(
          c, toInt64(firstIndex),
          toInt64(lastValidIndex), n.info, indexType)

        result = c.config.newError(n, SemReport(
          kind: rsemIndexOutOfBounds,
          typ: validIndex,
          countMismatch: (expected: toInt128(i), got: toInt128(n.len))
        ))
        return

      x = n[i]
      if x.kind == nkExprColonExpr and x.len == 2:
        var idx = semConstExpr(c, x[0])
        idx = fitNode(c, indexType, idx, x.info)
        if lastIndex + 1 != getOrdValue(idx):
          result = n
          result[i] = c.config.newError(x, SemReport(
            kind: rsemInvalidOrderInArrayConstructor,
            typ: idx.typ,
            countMismatch: (
              expected: lastIndex + 1,
              got: getOrdValue(idx))))
          result = c.config.wrapError(result)
          return

        x = x[1]

      let xx = semExprWithType(c, x, {})
      result.add xx
      if {xx.typ.kind, typ.kind} == {tyObject} and typ != xx.typ:
        # Check if both are objects before getting common type,
        # this prevents `[Derived(), Parent(), Derived()]` from working for non ref objects.
        result[i] = typeMismatch(c.config, x.info, typ, xx.typ, x)
      else:
        typ = commonType(c, typ, xx.typ)
      #n[i] = semExprWithType(c, x, {})
      #result.add fitNode(c, typ, n[i])
      inc(lastIndex)
    addSonSkipIntLit(result.typ, typ, c.idgen)
    for i in 0..<result.len:
      result[i] = fitNode(c, typ, result[i], result[i].info)
  result.typ[0] = makeRangeType(c, toInt64(firstIndex), toInt64(lastIndex), n.info,
                                     indexType)

proc fixAbstractType(c: PContext, n: PNode): PNode =
  assert n != nil
  
  var hasError = false

  result = n
  case n.kind
  of nkError:
    discard   # we'll just return below
  else:
    for i in 1..<n.len:
      let it = n[i]
      # do not get rid of nkHiddenSubConv for OpenArrays, codegen needs it:
      if it.kind == nkHiddenSubConv and
          skipTypes(it.typ, abstractVar).kind notin {tyOpenArray, tyVarargs}:
        if skipTypes(it[1].typ, abstractVar).kind in {tyNil, tyTuple, tySet} or
            it[1].isArrayConstr:
          var s = skipTypes(it.typ, abstractVar)
          
          if s.kind != tyUntyped:
            it[1] = changeType(c, it[1], s, check=true)
          
            if it[1].isError:
              hasError = true

          n[i] = it[1]
  
  if hasError and result.kind != nkError:
    result = c.config.wrapError(n)

proc isAssignable(c: PContext, n: PNode; isUnsafeAddr=false): TAssignableResult =
  result = parampatterns.isAssignable(c.p.owner, n, isUnsafeAddr)

proc isUnresolvedSym(s: PSym): bool =
  result = s.kind == skGenericParam
  if not result and s.typ != nil:
    result = tfInferrableStatic in s.typ.flags or
        (s.kind == skParam and s.typ.isMetaType) or
        (s.kind == skType and
        s.typ.flags * {tfGenericTypeParam, tfImplicitTypeParam} != {})

proc hasUnresolvedArgs(c: PContext, n: PNode): bool =
  # Checks whether an expression depends on generic parameters that
  # don't have bound values yet. E.g. this could happen in situations
  # such as:
  #  type Slot[T] = array[T.size, byte]
  #  proc foo[T](x: default(T))
  #
  # Both static parameter and type parameters can be unresolved.
  case n.kind
  of nkSym:
    return isUnresolvedSym(n.sym)
  of nkIdent, nkAccQuoted:
    let (ident, err) = considerQuotedIdent(c, n)
    if err != nil:
      localReport(c.config, err)
    var amb = false
    let sym = searchInScopes(c, ident, amb)
    if sym != nil:
      return isUnresolvedSym(sym)
    else:
      return false
  of nkError:
    return false
  else:
    for i in 0..<n.safeLen:
      if hasUnresolvedArgs(c, n[i]): return true
    return false

proc newHiddenAddrTaken(c: PContext, n: PNode): PNode =
  case n.kind
  of nkHiddenDeref:
    checkSonsLen(n, 1, c.config)
    result = n[0]
  else:
    result = newNodeIT(nkHiddenAddr, n.info, makeVarType(c, n.typ))
    result.add n

func isVarParam(t: PType): bool =
  # watch out: ``typeDesc[var int]`` is **not** a 'var' parameter
  t.skipTypes(abstractInst - {tyTypeDesc}).kind == tyVar

proc analyseIfAddressTaken(n: PNode) =
  ## Analyses if taking the address of lvalue expression `n` counts as an
  ## "address taken" and if it does, marks the symbol of the underlying
  ## location (if it's associated with a symbol) with the ``sfAddrTaken``
  ## flag.
  ## XXX: the exact meaning of "address taken" is rather fuzzy at the moment,
  ##      and the whole thing is likely not a good idea in the first place.
  ##      For locations, only ``guards.nim`` and the JS code-generator make use
  ##      of ``sfAddrTaken``, with both attaching slightly different meaning to
  ##      it, it seems. The JS code-generator uses the flag to know whether the
  ##      location has to be stored in a way that allows for indirectly
  ##      mutating it, and is generally only interested in whether the *whole*
  ##      location either has its address taken or is passed to a 'var'
  ##      parameter (e.g. it depends on the flag being present for ``x`` in
  ##      ``addr x``, but for ``addr x.y`` it does not). It's likely a better
  ##      idea to let the JS code-generator perform its own analysis instead of
  ##      relying on ``sfAddrTaken``
  ##      For ``guards``, it *seems* like the ``sfAddrTaken`` information is
  ##      used to know whether a fact cannot be made untrue through an
  ##      undetectable indirect mutation
  ##
  ## XXX: including the flag here has the additional issue of letting AST that
  ##      might turn out erroneous (e.g. ``(var i = 0; discard addr(i); error)``)
  ##      modify a symbol. Performing this analysis in ``sempass2`` would be the
  ##      better approach
  ##
  var
    n {.cursor.} = n
    hadBracket = false

  while true:
    case n.kind
    of nkConv:
      # skip lvalue conversion. We know that all conversions we encounter here
      # are lvalue conversions because the input `n` is an lvalue. In addition
      # we explicitly only consider ``nkConv`` (and not ``nkHiddenSubConv``,
      # etc.), as it's the only conversion operator used for lvalue conversion
      # (ignoring things like ``openArray`` conversion, but we're not
      # interested in those)
      n = n[1]
    of nkBracketExpr:
      # skip a single array-like access operation
      # XXX: the previous implementation did the same, but the "why" is not
      #      clear. The underlying issue is that the meaning of ``sfAddrTaken``
      #      for locals and globals is not really specified
      if hadBracket:
        break
      else:
        n = n[0]
        hadBracket = true
    of nkStmtListExpr:
      n = n.lastSon
    else:
      break

  if n.kind == nkSym:
    incl(n.sym.flags, sfAddrTaken)

proc passToVarParameter(c: PContext, n: PNode): PNode =
  ## Analyses the lvalue expression `n` that is meant to be passed to a ``var``
  ## parameter, wrapping it in an ``nkHiddenAddr`` node, if necessary
  # only create a mutable reference (i.e. ``nkHiddenAddr``) if the source isn't
  # one already
  if n.typ.skipTypes(abstractInst).kind != tyVar:
    analyseIfAddressTaken(n)
    result = newHiddenAddrTaken(c, n)
  elif n.kind in {nkHiddenSubConv, nkHiddenStdConv}:
    # this happens when passing a sub-type to super-type parameter or something
    # that is implictly convertible to an ``openArray`` to an ``openArray``
    # parameter
    result = newHiddenAddrTaken(c, n)
  else:
    result = n

proc analyseIfAddressTakenInCall(n: PNode) =
  ## Performs the "is address taken" analysis for all immediate arguments of
  ## the call `n`. The input AST is, except for the symbols that get marked
  ## with ``sfAddrTaken``, not mutated
  for i in 1..<n.len:
    if n[i].kind == nkHiddenAddr:
      analyseIfAddressTaken(n[i][0])

proc fixVarArgumentsAndAnalyse(c: PContext, n: PNode): PNode =
  ## Introduces an ``nkHiddenAddr`` node for each immediate argument part of
  ## the call expression `n` that is passed to a 'var' parameter. Only the
  ## immediate. The fixup has to be applied *after* overload is done and the
  ## arguments are otherwise sem-checked already.
  ##
  ## Also performs the "is address taken" analysis for each argument passed to
  ## a ``var`` parameter.
  ##
  ## Note that not all ``var`` parameters are considered, certain magics are
  ## ignored during this fixup
  addInNimDebugUtils(c.config, "fixVarArgumentsAndAnalyse", n, result)
  checkMinSonsLen(n, 1, c.config)

  if n.isError:
    return n

  func skipDeref(n: PNode): PNode =
    if n.kind == nkHiddenDeref: n[0]
    else:                       n

  result = n

  # get the real type of the callee
  # it may be a proc var with a generic alias type, so we skip over them
  let
    t = n[0].typ.skipTypes({tyGenericInst, tyAlias, tySink})
    magic = getMagic(n)

  var hasError = false

  if magic in {mNew, mNewFinalize, mNewSeq}:
    # XXX: this check doesn't really fit here. ``magicsAfterOverloadResolution``
    #       would be a better place for it
    # bug #5113: disallow newSeq(result) where result is a 'var T':
    let arg = skipDeref(n[1])

    if arg.kind == nkSym and arg.sym.kind == skResult and
       arg.typ.skipTypes(abstractInst).kind == tyVar:
      n[1] = c.config.newError(n[1], reportAst(rsemStackEscape, n[1]))
      hasError = true

  let allow =
    if c.inUncheckedAssignSection > 0:
      # allow passing a discriminator location to a 'var' parameter
      {arLValue, arLocalLValue, arDiscriminant}
    else:
      {arLValue, arLocalLValue}

  for i in 1 ..< n.len:
    let arg = n[i]

    if i >= t.len or              # unsafe varargs -> no formal type
       magic in FakeVarParams or  # not really a 'var' parameter
       not isVarParam(t[i]) or
       arg.kind == nkHiddenAddr: # already analysed
       # TODO: according to the commit (3620155d937bb5d3ffe79ca9a62e4552ed7bca1b)
       #       that added the "already analysed" check, this can happen for
       #       'var' parameters in (to?) templates. Check if this is still the
       #       case, and if it is, add a test
      continue

    if isAssignable(c, arg) in allow:
      let arg1 = skipDeref(arg)
      # make sure to analyse converter calls introduced by ``sigmatch``. They
      # might return a 'var T' themselves, hence the deref skipping above
      if arg1.kind == nkHiddenCallConv:
        analyseIfAddressTakenInCall(arg1)

      result[i] = passToVarParameter(c, arg)
    else:
      hasError = true
      result[i] = c.config.newError(arg):
        reportAst(rsemVarForOutParamNeeded, arg)

  if hasError:
    result = wrapError(c.config, result)

include semmagic

proc evalAtCompileTime(c: PContext, n: PNode): PNode =
  result = n
  if n.kind notin nkCallKinds or n[0].kind != nkSym: return
  var callee = n[0].sym
  # workaround for bug #537 (overly aggressive inlining leading to
  # wrong NimNode semantics):
  if n.typ != nil and tfTriggersCompileTime in n.typ.flags: return

  # constant folding that is necessary for correctness of semantic pass:
  if callee.magic != mNone and callee.magic in ctfeWhitelist and n.typ != nil:
    var call = newNodeIT(nkCall, n.info, n.typ)
    call.add(n[0])
    var allConst = true
    for i in 1..<n.len:
      var a = getConstExpr(c.module, n[i], c.idgen, c.graph)
      if a == nil:
        allConst = false
        a = n[i]
        if a.kind == nkHiddenStdConv: a = a[1]
      call.add(a)
    if allConst:
      result = semfold.getConstExpr(c.module, call, c.idgen, c.graph)
      if result.isNil: result = n
      else: return result

  block maybeLabelAsStatic:
    # XXX: temporary work-around needed for tlateboundstatic.
    # This is certainly not correct, but it will get the job
    # done until we have a more robust infrastructure for
    # implicit statics.
    if n.len > 1:
      for i in 1..<n.len:
        # see bug #2113, it's possible that n[i].typ for errornous code:
        if n[i].typ.isNil or n[i].typ.kind != tyStatic or
            tfUnresolved notin n[i].typ.flags:
          break maybeLabelAsStatic
      n.typ = newTypeWithSons(c, tyStatic, @[n.typ])
      n.typ.flags.incl tfUnresolved

  # optimization pass: not necessary for correctness of the semantic pass
  if callee.kind == skConst or
     {sfNoSideEffect, sfCompileTime} * callee.flags != {} and
     {sfForward, sfImportc} * callee.flags == {} and n.typ != nil:

    if callee.kind != skConst and
       sfCompileTime notin callee.flags and
       optImplicitStatic notin c.config.options: return

    if callee.magic notin ctfeWhitelist: return

    if callee.kind notin {skProc, skFunc, skConverter, skConst} or callee.isGenericRoutine:
      return

    if n.typ != nil and typeAllowed(n.typ, skConst, c) != nil: return

    var call = newNodeIT(nkCall, n.info, n.typ)
    call.add(n[0])
    for i in 1..<n.len:
      let a = getConstExpr(c.module, n[i], c.idgen, c.graph)
      if a == nil: return n
      call.add(a)

    #echo "NOW evaluating at compile time: ", call.renderTree
    if c.inStaticContext == 0 or sfNoSideEffect in callee.flags:
      if sfCompileTime in callee.flags:
        result = evalStaticExpr(c.module, c.idgen, c.graph, call, c.p.owner)
        if result.isNil:
          localReport(c.config, n.info, reportAst(
            rsemCannotInterpretNode, call))

        else:
          result = fixupTypeAfterEval(c, result, n)
      else:
        result = evalConstExpr(c.module, c.idgen, c.graph, call)
        if result.isNil:
          result = n

        else:
          result = fixupTypeAfterEval(c, result, n)
    else:
      result = n
    #if result != n:
    #  echo "SUCCESS evaluated at compile time: ", call.renderTree

proc semStaticExpr(c: PContext, n: PNode): PNode =
  inc c.inStaticContext
  openScope(c)
  let a = semExprWithType(c, n)
  closeScope(c)
  dec c.inStaticContext
  if a.findUnresolvedStatic != nil: return a
  result = evalStaticExpr(c.module, c.idgen, c.graph, a, c.p.owner)
  if result.isNil:
    localReport(c.config, n, reportSem rsemCannotInterpretNode)
    result = c.graph.emptyNode
  else:
    result = fixupTypeAfterEval(c, result, a)

proc semOverloadedCallAnalyseEffects(c: PContext, n: PNode,
                                     flags: TExprFlags): PNode =
  addInNimDebugUtils(c.config, "semOverloadedCallAnalyseEffects", n, result)
  if flags*{efInTypeof, efWantIterator, efWantIterable} != {}:
    # consider: 'for x in pReturningArray()' --> we don't want the restriction
    # to 'skIterator' anymore; skIterator is preferred in sigmatch already
    # for typeof support.
    # for ``typeof(countup(1,3))``, see ``tests/ttoseq``.
    result = semOverloadedCall(c, n,
      {skProc, skFunc, skMethod, skConverter, skMacro, skTemplate, skIterator}, flags)
  else:
    result = semOverloadedCall(c, n,
      {skProc, skFunc, skMethod, skConverter, skMacro, skTemplate}, flags)

  if result != nil and result.kind != nkError:
    c.config.internalAssert(result[0].kind == nkSym, "semOverloadedCallAnalyseEffects")

    let callee = result[0].sym
    case callee.kind
    of skMacro, skTemplate: discard
    else:
      if callee.kind == skIterator and callee.id == c.p.owner.id:
        # xxx: this is weird overall, but the generation of the error with `n`
        #      instead of `result[0]`, which is what we're setting down below
        #      seems rather wrong.
        let err = newError(c.config, n,
                    reportSym(rsemRecursiveDependencyIterator, callee))
        localReport(c.config, err)

        # error correction, prevents endless for loop elimination in transf.
        # See bug #2051:
        result[0] = newSymNode(errorSym(c, n, err))
      elif callee.kind == skIterator:
        if efWantIterable in flags:
          let typ = newTypeS(tyIterable, c)
          rawAddSon(typ, result.typ)
          result.typ = typ

proc semObjConstr(c: PContext, n: PNode, flags: TExprFlags): PNode

proc resolveIndirectCall(c: PContext; n: PNode;
                         t: PType): TCandidate =
  initCandidate(c, result, t)
  matches(c, n, result)

proc bracketedMacro(n: PNode): PSym =
  if n.len >= 1 and n[0].kind == nkSym:
    result = n[0].sym
    if result.kind notin {skMacro, skTemplate}:
      result = nil

proc setGenericParams(c: PContext, n: PNode) =
  for i in 1..<n.len:
    n[i].typ = semTypeNode(c, n[i], nil)

proc afterCallActions(c: PContext; n: PNode, flags: TExprFlags): PNode =
  if n.kind == nkError:
    return n
  if efNoSemCheck notin flags and n.typ != nil and n.typ.kind == tyError:
    # XXX: legacy path, remove once nkError is everywhere
    return errorNode(c, n)

  result = n
  let callee = result[0].sym
  case callee.kind
  of skMacro: result = semMacroExpr(c, result, callee, flags)
  of skTemplate: result = semTemplateExpr(c, result, callee, flags)
  else:
    semFinishOperands(c, result)
    activate(c, result)
    result = fixAbstractType(c, result)
    result = fixVarArgumentsAndAnalyse(c, result)
    if callee.magic != mNone:
      result = magicsAfterOverloadResolution(c, result, flags)
    when false:
      if result.typ != nil and
          not (result.typ.kind == tySequence and result.typ[0].kind == tyEmpty):
        liftTypeBoundOps(c, result.typ, n.info)
    #result = patchResolvedTypeBoundOp(c, result)
  if c.matchedConcept == nil:
    result = evalAtCompileTime(c, result)

proc semIndirectOp(c: PContext, n: PNode, flags: TExprFlags): PNode =
  addInNimDebugUtils(c.config, "semIndirectOp", n, result, flags)
  result = nil
  checkMinSonsLen(n, 1, c.config)
  if n.kind == nkError: return n
  let prc = n[0]
  if n[0].kind == nkDotExpr:
    checkSonsLen(n[0], 2, c.config)
    let n0 = semFieldAccess(c, n[0])
    case n0.kind
    of nkDotCall:
      # it is a static call!
      result = n0
      result.transitionSonsKind(nkCall)
      result.flags.incl nfExplicitCall
      for i in 1..<n.len: result.add n[i]
      return semExpr(c, result, flags)
    of nkError:
      result = n
      result[0] = n0
      return wrapError(c.config, result)
    else:
      n[0] = n0
  else:
    n[0] = semExpr(c, n[0], {efInCall})
    if n[0] != nil and n[0].isErrorLike:
      result = wrapError(c.config, n)
      return
    let t = n[0].typ
    if t != nil and t.kind in {tyVar, tyLent}:
      n[0] = newDeref(n[0])
    elif n[0].kind == nkBracketExpr:
      let s = bracketedMacro(n[0])
      if s != nil:
        setGenericParams(c, n[0])
        return semDirectOp(c, n, flags)

  discard semOpAux(c, n)
  var t: PType = nil
  if n[0].typ != nil:
    t = skipTypes(n[0].typ, abstractInst-{tyTypeDesc, tyDistinct})
  if t != nil and t.kind == tyProc:
    # This is a proc variable, apply normal overload resolution
    let m = resolveIndirectCall(c, n, t)
    if m.state != csMatch:
      if c.config.m.errorOutputs == {}:
        # speed up error generation:
        globalReport(c.config, n.info, SemReport(kind: rsemTypeMismatch))
        return c.graph.emptyNode
      else:
        var hasErrorType = false
        for i in 1..<n.len:
          if n[i].typ.kind == tyError:
            hasErrorType = true
            break

        if not hasErrorType:
          result = c.config.newError(n, reportTyp(
            rsemCallIndirectTypeMismatch, n[0].typ, ast = n))

        else:
          # XXX: legacy path, consolidate with nkError
          result = errorNode(c, n)

        return

      result = nil

    else:
      result = m.call
      instGenericConvertersSons(c, result, m)

  elif t != nil and t.kind == tyTypeDesc:
    if n.len == 1: return semObjConstr(c, n, flags)
    return semConv(c, n)
  else:
    result = overloadedCallOpr(c, n)
    # Now that nkSym does not imply an iteration over the proc/iterator space,
    # the old ``prc`` (which is likely an nkIdent) has to be restored:
    if result == nil:
      # XXX: hmm, what kind of symbols will end up here?
      # do we really need to try the overload resolution?
      n[0] = prc
      result = semOverloadedCallAnalyseEffects(c, n, flags)

      if result == nil:
        return c.config.newError(n, reportAst(rsemExpressionCannotBeCalled, n))
    elif result.kind notin nkCallKinds:
      # the semExpr() in overloadedCallOpr can even break this condition!
      # See bug #904 of how to trigger it:
      return result
  #result = afterCallActions(c, result, flags)
  if result.kind == nkError:
    return # we're done; return the error and move on
  elif result[0].kind == nkSym:
    result =
      if result[0].sym.isError:
        wrapError(c.config, result)
      else:
        afterCallActions(c, result, flags)
  else:
    result = fixAbstractType(c, result)
    result = fixVarArgumentsAndAnalyse(c, result)

proc semDirectOp(c: PContext, n: PNode, flags: TExprFlags): PNode =
  # this seems to be a hotspot in the compiler!
  #semLazyOpAux(c, n)
  result = semOverloadedCallAnalyseEffects(c, n, flags)
  if result != nil:
    if result.kind == nkError:
      return
    result = afterCallActions(c, result, flags)
  else: result = errorNode(c, n)

proc buildEchoStmt(c: PContext, n: PNode): PNode =
  # we MUST not check 'n' for semantics again here! But for now we give up:
  result = newNodeI(nkCall, n.info)
  let e = systemModuleSym(c.graph, getIdent(c.cache, "echo"))
  if e != nil:
    result.add(newSymNode(e))
  else:
    result.add newError(c.config, n, reportStr(rsemSystemNeeds, "echo"))

  result.add(n)
  result.add(newStrNode(nkStrLit, ": " & n.typ.typeToString))
  result = semExpr(c, result)

proc semExprNoType(c: PContext, n: PNode): PNode =
  ## guess: `n` is likely a statement, and so we expect it to have "no type"
  ## hence the 'NoType` suffix in the name.
  ##
  ## Semantic/type analysis is still done as we perform a check for `discard`.
  let isPush = c.config.hasHint(rsemExtendedContext)
  if isPush: pushInfoContext(c.config, n.info)
  result = discardCheck(c, semExpr(c, n, {efWantStmt}), {})
  if isPush: popInfoContext(c.config)

proc isTypeExpr(n: PNode): bool =
  case n.kind
  of nkType, nkTypeOfExpr: result = true
  of nkSym: result = n.sym.kind == skType
  else: result = false

proc createSetType(c: PContext; baseType: PType): PType =
  assert baseType != nil
  result = newTypeS(tySet, c)
  rawAddSon(result, baseType)

proc lookupInRecordAndBuildCheck(c: PContext, n, r: PNode, field: PIdent,
                                 check: var PNode): PSym =
  # transform in a node that contains the runtime check for the
  # field, if it is in a case-part...
  result = nil
  case r.kind
  of nkRecList:
    for i in 0..<r.len:
      result = lookupInRecordAndBuildCheck(c, n, r[i], field, check)
      if result != nil: return
  of nkRecCase:
    checkMinSonsLen(r, 2, c.config)
    if (r[0].kind != nkSym):
      semReportIllformedAst(c.config, r, {nkSym})

    result = lookupInRecordAndBuildCheck(c, n, r[0], field, check)
    if result != nil: return
    let setType = createSetType(c, r[0].typ)
    var s = newNodeIT(nkCurly, r.info, setType)
    for i in 1..<r.len:
      var it = r[i]
      case it.kind
      of nkOfBranch:
        result = lookupInRecordAndBuildCheck(c, n, lastSon(it), field, check)
        if result == nil:
          for j in 0..<it.len-1: s.add copyTree(it[j])
        else:
          if check == nil:
            check = newNodeI(nkCheckedFieldExpr, n.info)
            check.add c.graph.emptyNode # make space for access node
          s = newNodeIT(nkCurly, n.info, setType)
          for j in 0..<it.len - 1: s.add copyTree(it[j])
          var inExpr = newNodeIT(nkCall, n.info, getSysType(c.graph, n.info, tyBool))
          inExpr.add newSymNode(getSysMagic(c.graph, n.info, "contains", mInSet), n.info)
          inExpr.add s
          inExpr.add copyTree(r[0])
          check.add inExpr
          #check.add semExpr(c, inExpr)
          return
      of nkElse:
        result = lookupInRecordAndBuildCheck(c, n, lastSon(it), field, check)
        if result != nil:
          if check == nil:
            check = newNodeI(nkCheckedFieldExpr, n.info)
            check.add c.graph.emptyNode # make space for access node
          var inExpr = newNodeIT(nkCall, n.info, getSysType(c.graph, n.info, tyBool))
          inExpr.add newSymNode(getSysMagic(c.graph, n.info, "contains", mInSet), n.info)
          inExpr.add s
          inExpr.add copyTree(r[0])
          var notExpr = newNodeIT(nkCall, n.info, getSysType(c.graph, n.info, tyBool))
          notExpr.add newSymNode(getSysMagic(c.graph, n.info, "not", mNot), n.info)
          notExpr.add inExpr
          check.add notExpr
          return
      else:
        semReportIllformedAst(c.config, it, {nkElse, nkOfBranch})

  of nkSym:
    if r.sym.name.id == field.id:
      result = r.sym
  else:
    semReportIllformedAst(c.config, n, {nkSym, nkRecCase, nkRecList})

const
  tyTypeParamsHolders = {tyGenericInst, tyCompositeTypeClass}
  tyDotOpTransparent = {tyVar, tyLent, tyPtr, tyRef, tyAlias, tySink}

proc readTypeParameter(c: PContext, typ: PType,
                       paramName: PIdent, info: TLineInfo): PNode =
  # Note: This function will return emptyNode when attempting to read
  # a static type parameter that is not yet resolved (e.g. this may
  # happen in proc signatures such as `proc(x: T): array[T.sizeParam, U]`
  if typ.kind in {tyUserTypeClass, tyUserTypeClassInst}:
    for statement in typ.n:
      case statement.kind
      of nkTypeSection:
        for def in statement:
          if def[0].sym.name.id == paramName.id:
            # XXX: Instead of lifting the section type to a typedesc
            # here, we could try doing it earlier in semTypeSection.
            # This seems semantically correct and then we'll be able
            # to return the section symbol directly here
            let foundType = makeTypeDesc(c, def[2].typ)
            return newSymNode(copySym(def[0].sym, nextSymId c.idgen).linkTo(foundType), info)

      of nkConstSection:
        for def in statement:
          if def[0].sym.name.id == paramName.id:
            return def[2]

      else:
        discard

  if typ.kind != tyUserTypeClass:
    let ty = if typ.kind == tyCompositeTypeClass: typ[1].skipGenericAlias
             else: typ.skipGenericAlias
    let tbody = ty[0]
    for s in 0..<tbody.len-1:
      let tParam = tbody[s]
      if tParam.sym.name.id == paramName.id:
        let rawTyp = ty[s + 1]
        if rawTyp.kind == tyStatic:
          if rawTyp.n != nil:
            return rawTyp.n
          else:
            return c.graph.emptyNode
        else:
          let foundTyp = makeTypeDesc(c, rawTyp)
          return newSymNode(copySym(tParam.sym, nextSymId c.idgen).linkTo(foundTyp), info)

  return nil

proc semSym(c: PContext, n: PNode, sym: PSym, flags: TExprFlags): PNode =
  let s = getGenSym(c, sym)
  case s.kind
  of skConst:
    markUsed(c, n.info, s)
    onUse(n.info, s)
    let typ = skipTypes(s.typ, abstractInst-{tyTypeDesc})
    case typ.kind
    of  tyNil, tyChar, tyInt..tyInt64, tyFloat..tyFloat128,
        tyTuple, tySet, tyUInt..tyUInt64:
      if s.magic == mNone: result = inlineConst(c, n, s)
      else: result = newSymNode(s, n.info)
    of tyArray, tySequence:
      # Consider::
      #     const x = []
      #     proc p(a: openarray[int])
      #     proc q(a: openarray[char])
      #     p(x)
      #     q(x)
      #
      # It is clear that ``[]`` means two totally different things. Thus, we
      # copy `x`'s AST into each context, so that the type fixup phase can
      # deal with two different ``[]``.
      if s.ast.safeLen == 0: result = inlineConst(c, n, s)
      else: result = newSymNode(s, n.info)
    of tyStatic:
      if typ.n != nil:
        result = typ.n
        result.typ = typ.base
      else:
        result = newSymNode(s, n.info)
    else:
      result = newSymNode(s, n.info)
  of skMacro:
    if efNoEvaluateGeneric in flags and s.ast[genericParamsPos].len > 0 or
       (n.kind notin nkCallKinds and s.requiredParams > 0):
      markUsed(c, n.info, s)
      onUse(n.info, s)
      result = symChoice(c, n, s, scClosed)
    else:
      result = semMacroExpr(c, n, s, flags)
  of skTemplate:
    if efNoEvaluateGeneric in flags and s.ast[genericParamsPos].len > 0 or
       (n.kind notin nkCallKinds and s.requiredParams > 0) or
       sfCustomPragma in sym.flags:
      let info = getCallLineInfo(n)
      markUsed(c, info, s)
      onUse(info, s)
      result = symChoice(c, n, s, scClosed)
    else:
      result = semTemplateExpr(c, n, s, flags)
  of skParam:
    markUsed(c, n.info, s)
    onUse(n.info, s)
    if s.typ != nil and s.typ.kind == tyStatic and s.typ.n != nil:
      # XXX see the hack in sigmatch.nim ...
      return s.typ.n
    elif sfGenSym in s.flags:
      # the owner should have been set by now by addParamOrResult
      c.config.internalAssert s.owner != nil
    result = newSymNode(s, n.info)
  of skVar, skLet, skResult, skForVar:
    if s.magic == mNimvm:
      localReport(c.config, n, reportSem rsemIllegalNimvmContext)

    markUsed(c, n.info, s)
    onUse(n.info, s)
    result = newSymNode2(s, n.info)
    # We cannot check for access to outer vars for example because it's still
    # not sure the symbol really ends up being used:
    # var len = 0 # but won't be called
    # genericThatUsesLen(x) # marked as taking a closure?
    if hasWarn(c.config, rsemResultUsed):
      localReport(c.config, n, reportSem rsemResultUsed)
  of skGenericParam:
    onUse(n.info, s)
    if s.typ.kind == tyStatic:
      result = newSymNode(s, n.info)
      result.typ = s.typ
    elif s.ast != nil:
      result = semExpr(c, s.ast)
    else:
      n.typ = s.typ
      return n
  of skType:
    markUsed(c, n.info, s)
    onUse(n.info, s)
    if s.typ.kind == tyStatic and s.typ.base.kind != tyNone and s.typ.n != nil:
      return s.typ.n
    result = newSymNode(s, n.info)
    result.typ = makeTypeDesc(c, s.typ)
  of skField:
    # old code, not sure if it's live code:
    markUsed(c, n.info, s)
    onUse(n.info, s)
    if sfGenSym in s.flags:
      # the owner should have been set by now by addParamOrResult
      c.config.internalAssert s.owner != nil
    result = newSymNode(s, n.info)
  else:
    if s.kind == skError and not s.ast.isNil and s.ast.kind == nkError:
      # XXX: at the time of writing only `lookups.qualifiedlookup` sets up the
      #      PSym so the error is in the ast field
      result = s.ast
    else:
      let info = getCallLineInfo(n)
      markUsed(c, info, s)
      onUse(info, s)
      result = newSymNode(s, info)

proc tryReadingGenericParam(c: PContext, n: PNode, i: PIdent, t: PType): PNode =
  case t.kind
  of tyTypeParamsHolders:
    result = readTypeParameter(c, t, i, n.info)
    if result == c.graph.emptyNode:
      result = n
      n.typ = makeTypeFromExpr(c, n.copyTree)
  of tyUserTypeClasses:
    if t.isResolvedUserTypeClass:
      result = readTypeParameter(c, t, i, n.info)
    else:
      n.typ = makeTypeFromExpr(c, copyTree(n))
      result = n
  of tyGenericParam, tyAnything:
    n.typ = makeTypeFromExpr(c, copyTree(n))
    result = n
  else:
    discard

proc tryReadingTypeField(c: PContext, n: PNode, i: PIdent, ty: PType): PNode =
  var ty = ty.skipTypes(tyDotOpTransparent)
  case ty.kind
  of tyEnum:
    # look up if the identifier belongs to the enum:
    var f = PSym(nil)
    while ty != nil:
      f = getSymFromList(ty.n, i)
      if f != nil: break
      ty = ty.sons[0]         # enum inheritance
    if f != nil:
      result = newSymNode(f)
      result.info = n.info
      result.typ = ty
      markUsed(c, n.info, f)
      onUse(n.info, f)
  of tyObject, tyTuple:
    if ty.n != nil and ty.n.kind == nkRecList:
      let field = lookupInRecord(ty.n, i)
      if field != nil:
        n.typ = makeTypeDesc(c, field.typ)
        result = n
  of tyGenericInst:
    result = tryReadingTypeField(c, n, i, ty.lastSon)
    if result == nil:
      result = tryReadingGenericParam(c, n, i, ty)
  else:
    result = tryReadingGenericParam(c, n, i, ty)

proc builtinFieldAccess(c: PContext, n: PNode, flags: TExprFlags): PNode =
  ## returns nil if it's not a built-in field access
  checkSonsLen(n, 2, c.config)
  # tests/bind/tbindoverload.nim wants an early exit here, but seems to
  # work without now. template/tsymchoicefield doesn't like an early exit
  # here at all!
  when defined(nimsuggest):
    if c.config.cmd == cmdIdeTools:
      suggestExpr(c, n)
      if exactEquals(c.config.m.trackPos, n[1].info): suggestExprNoCheck(c, n)

  let s = qualifiedLookUp(c, n, {checkAmbiguity, checkUndeclared, checkModule})
  if s.isError:
    result = s.ast
  elif s != nil:
    if s.kind in OverloadableSyms:
      result = symChoice(c, n, s, scClosed)
      if result.kind == nkSym:
        result = semSym(c, n, s, flags)
    else:
      markUsed(c, n[1].info, s)
      result = semSym(c, n, s, flags)
    onUse(n[1].info, s)
    return

  n[0] = semExprWithType(c, n[0], flags+{efDetermineType, efWantIterable})
  var
    i = legacyConsiderQuotedIdent(c, n[1], n)
    ty = n[0].typ
    f: PSym = nil

  if ty.kind == tyTypeDesc:
    if ty.base.kind == tyNone:
      # This is a still unresolved typedesc parameter.
      # If this is a regular proc, then all bets are off and we must return
      # tyFromExpr, but when this happen in a macro this is not a built-in
      # field access and we leave the compiler to compile a normal call:
      if getCurrOwner(c).kind != skMacro:
        n.typ = makeTypeFromExpr(c, n.copyTree)
        return n
      else:
        return nil
    else:
      return tryReadingTypeField(c, n, i, ty.base)
  elif isTypeExpr(n[0]):
    return tryReadingTypeField(c, n, i, ty)
  elif ty.kind == tyError:
    # a type error doesn't have any builtin fields
    return nil

  if ty.kind in tyUserTypeClasses and ty.isResolvedUserTypeClass:
    ty = ty.lastSon
  ty = skipTypes(ty, {tyGenericInst, tyVar, tyLent, tyPtr, tyRef, tyAlias, tySink, tyStatic})
  while tfBorrowDot in ty.flags: ty = ty.skipTypes({tyDistinct, tyGenericInst, tyAlias})
  var check: PNode = nil
  if ty.kind == tyObject:
    while true:
      check = nil
      f = lookupInRecordAndBuildCheck(c, n, ty.n, i, check)
      if f != nil: break
      if ty[0] == nil: break
      ty = skipTypes(ty[0], skipPtrs)
    if f != nil:
      let visibilityCheckNeeded =
        if n[1].kind == nkSym and n[1].sym == f:
          false # field lookup was done already, likely by hygienic template or bindSym
        else: true
      if not visibilityCheckNeeded or fieldVisible(c, f):
        # is the access to a public field or in the same module or in a friend?
        markUsed(c, n[1].info, f)
        onUse(n[1].info, f)
        n[0] = makeDeref(n[0])
        n[1] = newSymNode(f) # we now have the correct field
        n.typ = f.typ
        if check == nil:
          result = n
        else:
          check[0] = n
          check.typ = n.typ
          result = check
  elif ty.kind == tyTuple and ty.n != nil:
    f = getSymFromList(ty.n, i)
    if f != nil:
      markUsed(c, n[1].info, f)
      onUse(n[1].info, f)
      n[0] = makeDeref(n[0])
      n[1] = newSymNode(f)
      n.typ = f.typ
      result = n

  # we didn't find any field, let's look for a generic param
  if result == nil:
    let t = n[0].typ.skipTypes(tyDotOpTransparent)
    result = tryReadingGenericParam(c, n, i, t)

proc dotTransformation(c: PContext, n: PNode): PNode =
  ## transforms a dotExpr into a dotCall, returns nkError if the "field" of the
  ## dotExpr is not a valid ident.
  
  c.config.internalAssert(n.kind == nkDotExpr,
                          "nkDotExpr expected, got: " & $n.kind)
  
  result = newNodeI(nkDotCall, n.info)

  if isSymChoice(n[1]):
    result.add n[1]
  else:
    result.flags.incl nfDotField
    let (ident, err) = considerQuotedIdent(c, n[1])

    # set the lhs of the new dotCall
    result.add:
      if err.isNil:
        newIdentNode(ident, n[1].info)
      else:
        # we have an error, set it in the first pos handle on return
        c.config.newError(n):
          block:
            var r = reportAst(rsemIdentExpectedInExpr, n[1])
            r.wrongNode = n
            r

  result.add copyTree(n[0])

  if result[0].kind == nkError: # handle the potential ident error
    result = c.config.wrapError(result)

proc semFieldAccess(c: PContext, n: PNode, flags: TExprFlags): PNode =
  # this is difficult, because the '.' is used in many different contexts
  # in Nim. We first allow types in the semantic checking.
  result = builtinFieldAccess(c, n, flags)
  if result == nil:
    result = dotTransformation(c, n)

proc buildOverloadedSubscripts(n: PNode, ident: PIdent): PNode =
  result = newNodeI(nkCall, n.info)
  result.add(newIdentNode(ident, n.info))
  for s in n: result.add s

proc semDeref(c: PContext, n: PNode): PNode =
  ## analyse deref such as `foo[]`, when given an nkBracketExpr converts it to
  ## an nkDerefExpr and evals it, if given an nkDerefExpr evals it, nkError is
  ## passed through.
  ##
  ## the behaviour is different if the deref target is a constant expression 
  ## (static) or not (dynamic).
  ##
  ## Constant Expression:
  ## - nil -> nkError
  ## - ptr|ref type -> nkDeref with derefed value as first son
  ## - not (ptr|ref) -> nil
  ##
  ## Dynamic Expression:
  ## - ptr|ref type -> nkDeref with derefed value as first son
  ## - not (ptr|ref) -> nil

  addInNimDebugUtils(c.config, "semDeref", n, result)

  proc semDerefEval(c: PContext, n: PNode): PNode {.inline.} =
    result = n # we allow mutation because we're evaluating `n`

    const tySkippedToGetRefType = {tyAlias, tyGenericInst, tyLent,
                                   tySink, tyVar}
    # xxx: should tySkippedToGetRefType be based off of `ast_types.skipPtrs`
    #      doing `skipPtrs - {tyRef, tyPtr}`? Also, not sure if tyVar and
    #      tyAlias are correct handled.

    let
      derefTarget = semExprWithType(c, n[0])
      targetAsConstExpr = getConstExpr(c.module, derefTarget, c.idgen, c.graph)
      isTargetConstExpr = targetAsConstExpr != nil
      isTargetConstNil = isTargetConstExpr and
                         targetAsConstExpr.kind == nkNilLit
      semmedTarget =
        if isTargetConstExpr:
          targetAsConstExpr
        else:
          derefTarget
      refTyp = skipTypes(semmedTarget.typ, tySkippedToGetRefType)
      derefType =
        case refTyp.kind
        of tyRef, tyPtr:
          refTyp.lastSon
        else:
          nil # xxx: should probably be an error type; derefing a non-ptr/ref

    case derefTarget.kind
    of nkError:
      result = c.config.wrapError(result)
    else:
      result[0] = semmedTarget
      result.typ = derefType

      if isTargetConstNil:
        result = c.config.newError(result, reportSem rsemDisallowedNilDeref)
      elif derefType.isNil:
        result = nil # xxx: this should be an nkError and recovered from

  case n.kind:
  of nkBracketExpr:
    checkSonsLen(n, 1, c.config)

    result = newNodeIT(nkDerefExpr, n.info, n.typ, children = 1)
    result[0] = n[0]

    result = semDerefEval(c, result)
  of nkDerefExpr:
    result = semDerefEval(c, n)
  of nkError:
    result = n
  else:
    c.config.internalError(n.info,
      "expected nkBracketExpr or nkDerefExpr, got: " & $n.kind)

  #GlobalError(n[0].info, errCircumNeedsPointer)

proc maybeInstantiateGeneric(c: PContext, n: PNode, s: PSym): PNode =
  ## Instantiates generic if not lacking implicit generics,
  ## otherwise returns n.
  let
    neededGenParams = s.ast[genericParamsPos].safeLen # might be an nkEmpty
    heldGenParams = n.len - 1
  var implicitParams = 0
  for x in s.ast[genericParamsPos]:
    if tfImplicitTypeParam in x.typ.flags:
      inc implicitParams
  if heldGenParams != neededGenParams and 
     implicitParams + heldGenParams == neededGenParams:
    # This is an implicit + explicit generic procedure without all args passed,
    # kicking back the sem'd symbol fixes #17212
    # Uncertain the hackiness of this solution.
    result = n
  else:
    result = explicitGenericInstantiation(c, n, s)
    if result.isError:
      discard
    elif result == n:
      n[0] = copyTree(result[0])
    else:
      n[0] = result

proc semSubscript(c: PContext, n: PNode, flags: TExprFlags): PNode =
  ## returns nil if not a built-in subscript operator; also called for the
  ## checking of assignments

  addInNimDebugUtils(c.config, "semSubscript", n, result, flags)

  c.config.internalAssert(n.kind == nkBracketExpr,
                          "expected nkBracketExpr, got: " & $n.kind)

  if n.len == 1:
    # xxx: this branch might be unnecessary as call sites for semSubscript may
    #      already handle nkDerefExpr scenarios
    result = semDeref(c, n)
    return

  checkMinSonsLen(n, 2, c.config)
  # make sure we don't evaluate generic macros/templates
  n[0] = semExprWithType(c, n[0],
                              {efNoEvaluateGeneric})
  var arr = skipTypes(n[0].typ, {tyGenericInst, tyUserTypeClassInst,
                                 tyVar, tyLent, tyPtr, tyRef, tyAlias, tySink})
  if arr.kind == tyStatic:
    if arr.base.kind == tyNone:
      result = n
      result.typ = semStaticType(c, n[1], nil)
      return
    elif arr.n != nil:
      return semSubscript(c, arr.n, flags)
    else:
      arr = arr.base

  case arr.kind
  of tyArray, tyOpenArray, tyVarargs, tySequence, tyString, tyCstring,
    tyUncheckedArray:
    if n.len != 2: return nil
    n[0] = makeDeref(n[0])
    for i in 1..<n.len:
      n[i] = semExprWithType(c, n[i],
                                  flags*{efInTypeof, efDetermineType})
    # Arrays index type is dictated by the range's type
    if arr.kind == tyArray:
      var indexType = arr[0]
      var arg = indexTypesMatch(c, indexType, n[1].typ, n[1])
      if arg != nil:
        n[1] = arg
        result = n
        result.typ = elemType(arr)
    # Other types have a bit more of leeway
    elif n[1].typ.skipTypes(abstractRange-{tyDistinct}).kind in
        {tyInt..tyInt64, tyUInt..tyUInt64}:
      result = n
      result.typ = elemType(arr)
  of tyTypeDesc:
    # The result so far is a tyTypeDesc bound
    # a tyGenericBody. The line below will substitute
    # it with the instantiated type.
    result = n
    result.typ = makeTypeDesc(c, semTypeNode(c, n, nil))
    #result = symNodeFromType(c, semTypeNode(c, n, nil), n.info)
  of tyTuple:
    if n.len != 2: return nil
    n[0] = makeDeref(n[0])
    # [] operator for tuples requires constant expression:
    n[1] = semConstExpr(c, n[1])
    if skipTypes(n[1].typ, {tyGenericInst, tyRange, tyOrdinal, tyAlias, tySink}).kind in
        {tyInt..tyInt64}:
      let idx = getOrdValue(n[1])
      if 0 <= idx and idx < arr.len:
        n.typ = arr[toInt(idx)]
      else:
        localReport(c.config, n.info, SemReport(
          kind: rsemInvalidTupleSubscript,
          ast: n[1],
          countMismatch: (expected: toInt128(arr.len - 1), got: idx)))

      result = n
    else:
      result = nil
  else:
    let s = if n[0].kind == nkSym: n[0].sym
            elif n[0].kind in nkSymChoices: n[0][0].sym
            else: nil
    if s != nil:
      case s.kind
      of skProc, skFunc, skMethod, skConverter, skIterator:
        # type parameters: partial generic specialization
        n[0] = semSymGenericInstantiation(c, n[0], s)
        result = maybeInstantiateGeneric(c, n, s)
      of skMacro, skTemplate:
        if efInCall in flags:
          # We are processing macroOrTmpl[] in macroOrTmpl[](...) call.
          # Return as is, so it can be transformed into complete macro or
          # template call in semIndirectOp caller.
          result = n
        else:
          # We are processing macroOrTmpl[] not in call. Transform it to the
          # macro or template call with generic arguments here.
          n.transitionSonsKind(nkCall)
          case s.kind
          of skMacro: result = semMacroExpr(c, n, s, flags)
          of skTemplate: result = semTemplateExpr(c, n, s, flags)
          else: discard
      of skType:
        result = symNodeFromType(c, semTypeNode(c, n, nil), n.info)
      else:
        discard

proc semArrayAccess(c: PContext, n: PNode, flags: TExprFlags): PNode =
  addInNimDebugUtils(c.config, "semArrayAccess", n, result, flags)

  case n.kind
  of nkBracketExpr:
    result = semSubscript(c, n, flags)
    if result == nil:
      # overloaded [] operator:
      result = semExpr(c,
                       buildOverloadedSubscripts(n, getIdent(c.cache, "[]")),
                       flags)
  of nkError:
    result = n
  else:
    c.config.internalError(n.info, "expected nkBracketExpr, got: " & $n.kind)

proc propertyWriteAccess(c: PContext, n, a: PNode): PNode =
  var id = legacyConsiderQuotedIdent(c, a[1],a)
  var setterId = newIdentNode(getIdent(c.cache, id.s & '='), a[1].info)
  # a[0] is already checked for semantics, that does ``builtinFieldAccess``
  # this is ugly. XXX Semantic checking should use the ``nfSem`` flag for
  # nodes?
  result = newTreeI(nkCall, n.info, setterId, a[0], semExprWithType(c, n[1]))
  result.flags.incl nfDotSetter
  result = semOverloadedCallAnalyseEffects(c, result, {})

  if result != nil:
    result = afterCallActions(c, result, {})
    #fixAbstractType(c, result)
    #analyseIfAddressTakenInCall(c, result)

proc takeImplicitAddr(c: PContext, formal: PType, n: PNode): PNode =
  ## See RFC #7373, calls returning 'var T' are assumed to
  ## return a view into the first argument (if there is one):
  addInNimDebugUtils(c.config, "takeImplicitAddr", n, result)

  let root = exprRoot(n)
  if root != nil and root.owner == c.p.owner:
    if root.kind in {skLet, skVar, skTemp} and sfGlobal notin root.flags:
      return newError(c.config, n, reportSym(
        rsemLocalEscapesStackFrame, root, ast = n))

    elif root.kind == skParam and root.position != 0:
      return newError(c.config, n, reportSym(
        rsemImplicitAddrIsNotFirstParam, root, ast = n))

  # TODO: what about expressions where ``root == nil`` (e.g. the root is a
  #       pointer dereference)?

  case isAssignable(c, n, formal.kind == tyLent)
  of arLValue:
    discard
  of arLocalLValue:
    # FIXME: this is not entirely correct. If inside an iterator (both inline
    #        and closure) and yielding a local as a ``var|lent``, it doesn't
    #        escape its stack frame. Yielding a local as a view is not an
    #        error
    return newError(c.config, n, reportSem rsemLocalEscapesStackFrame)
  else:
    return newError(c.config, n, reportSem rsemExprHasNoAddress)

  if n.kind == nkHiddenAddr:
    if sameType(formal, n.typ):
      result = n
    else:
      # the input is already a view, but it is of different type. While
      # correctable, this shouldn't happen in the first place
      result = newError(c.config, n, reportSem rsemExprHasNoAddress)
  else:
    result = newTreeIT(nkHiddenAddr, n.info, formal): n

proc asgnToResultVar(c: PContext, n, le, ri: PNode) {.inline.} =
  # refactor: to return a PNode instead and handle nkError
  
  if n.isError:
    return
  
  case le.kind
  of nkHiddenDeref:
    let x = le[0]
    case x.kind
    of nkSym:
      case x.sym.kind
      of skResult:
        if classifyViewType(x.typ) != noView:
          n[0] = x # 'result[]' --> 'result'
          n[1] = takeImplicitAddr(c, x.typ, ri)
          #echo x.info, " setting it for this type ", typeToString(x.typ), " ", n.info
      else:
        discard
    else:
      discard
  else:
    discard

template resultTypeIsInferrable(typ: PType): untyped =
  typ.isMetaType and typ.kind != tyTypeDesc

proc goodLineInfo(arg: PNode): TLineInfo =
  if arg.kind == nkStmtListExpr and arg.len > 0:
    goodLineInfo(arg[^1])
  else:
    arg.info

proc semAsgn(c: PContext, n: PNode; mode=asgnNormal): PNode =
  checkSonsLen(n, 2, c.config)
  var a = n[0]
  case a.kind
  of nkDotExpr:
    # r.f = x
    # --> `f=` (r, x)
    a = builtinFieldAccess(c, a, {efLValue})
    if a.isNil:
      result = propertyWriteAccess(c, n, n[0])
      if result != nil:
        return
      # we try without the '='; proc that return 'var' or macros are still
      # possible:
      a = dotTransformation(c, n[0])
      if a.kind == nkDotCall:
        a.transitionSonsKind(nkCall)
        a = semExprWithType(c, a, {efLValue})
  of nkBracketExpr:
    # a[i] = x
    # --> `[]=`(a, i, x)
    a = semSubscript(c, a, {efLValue})
    if a.isNil:
      case mode
      of noOverloadedSubscript:
        result = bracketNotFoundError(c, n)
      else:
        result = buildOverloadedSubscripts(n[0], getIdent(c.cache, "[]="))
        result.add(n[1])
        result = semExprNoType(c, result)
      return
  of nkCurlyExpr:
    # a{i} = x -->  `{}=`(a, i, x)
    result = buildOverloadedSubscripts(n[0], getIdent(c.cache, "{}="))
    result.add(n[1])
    return semExprNoType(c, result)
  of nkPar, nkTupleConstr:
    if a.len >= 2:
      # unfortunately we need to rewrite ``(x, y) = foo()`` already here so
      # that overloading of the assignment operator still works. Usually we
      # prefer to do these rewritings in transf.nim:
      return semStmt(c, lowerTupleUnpackingForAsgn(c.graph, n, c.idgen, c.p.owner), {})
    else:
      a = semExprWithType(c, a, {efLValue})
  else:
    a = semExprWithType(c, a, {efLValue})
  n[0] = a

  case a.kind
  of nkError:
    result = c.config.wrapError(n)
    return # refactor: return is "needed" because of final `result = n` below
  else:
    # a = b # both are vars, means: a[] = b[]
    # a = b # b no 'var T' means: a = addr(b)
    let le = a.typ

    if le.isNil:
      n[0] = c.config.newError(a, reportSem rsemExpressionHasNoType)
      result = c.config.wrapError(n)
      return # refactor: return is "needed" because of final `result = n` below

    elif (skipTypes(le, {tyGenericInst, tyAlias, tySink}).kind notin {tyVar} and
          isAssignable(c, a) in {arNone, arLentValue}) or
         (skipTypes(le, abstractVar).kind in {tyOpenArray, tyVarargs} and
          views notin c.features):
      # Direct assignment to a discriminant is allowed!
      n[0] = c.config.newError(a, reportAst(rsemCannotAssignTo, a, typ = le))
      result = c.config.wrapError(n)
      return

    else:
      let
        lhs = n[0]
        rhs = semExprWithType(c, n[1], {})
      if lhs.kind == nkSym and lhs.sym.kind == skResult:
        n.typ = c.enforceVoidContext
        if c.p.owner.kind != skMacro and resultTypeIsInferrable(lhs.sym.typ):
          let rhsTyp =
            if rhs.typ.kind in tyUserTypeClasses and
                rhs.typ.isResolvedUserTypeClass:
              rhs.typ.lastSon
            else:
              rhs.typ

          if cmpTypes(c, lhs.typ, rhsTyp) in {isGeneric, isEqual}:
            c.config.internalAssert c.p.resultSym != nil
            # Make sure the type is valid for the result variable
            typeAllowedCheck(c, n.info, rhsTyp, skResult)
            lhs.typ = rhsTyp
            c.p.resultSym.typ = rhsTyp
            c.p.owner.typ[0] = rhsTyp
          else:
            # XXX: if this is an nkError, should we modify the rhs and the
            #      overall assignment and return that, cascading upward?
            let r = typeMismatch(c.config, n.info, lhs.typ, rhsTyp, rhs)
            if r.kind == nkError:
              result = n
              result[1] = r # rhs error
              result = c.config.wrapError(result)
              return

      n[1] = fitNode(c, le, rhs, goodLineInfo(n[1]))
      when false: liftTypeBoundOps(c, lhs.typ, lhs.info)

      result = fixAbstractType(c, n)
      asgnToResultVar(c, result, n[0], n[1])
  result = n

proc semReturn(c: PContext, n: PNode): PNode =
  result = n
  checkSonsLen(n, 1, c.config)
  if c.p.owner.kind in {skConverter, skMethod, skProc, skFunc, skMacro} or
      (not c.p.owner.typ.isNil and isClosureIterator(c.p.owner.typ)):
    if n[0].kind != nkEmpty:
      if n[0].kind == nkAsgn and n[0][0].kind == nkSym and c.p.resultSym == n[0][0].sym:
        discard "return is already transformed"
      elif c.p.resultSym != nil:
        # transform ``return expr`` to ``result = expr; return``
        var a = newNodeI(nkAsgn, n[0].info)
        a.add newSymNode(c.p.resultSym)
        a.add n[0]
        n[0] = a
      else:
        localReport(c.config, n, reportSem rsemNoReturnTypeDeclared)
        return
      result[0] = semAsgn(c, n[0])
      # optimize away ``result = result``:
      if result[0][1].kind == nkSym and result[0][1].sym == c.p.resultSym:
        result[0] = c.graph.emptyNode
  else:
    localReport(c.config, n, reportSem rsemReturnNotAllowed)

proc semProcBody(c: PContext, n: PNode): PNode =
  openScope(c)
  result = semExpr(c, n)
  if c.p.resultSym != nil and not isEmptyType(result.typ):
    if result.kind == nkNilLit:
      # or ImplicitlyDiscardable(result):
      # new semantic: 'result = x' triggers the void context
      result.typ = nil
    elif result.kind == nkStmtListExpr and result.typ.kind == tyNil:
      # to keep backwards compatibility bodies like:
      #   nil
      #   # comment
      # are not expressions:
      fixNilType(c, result)
    else:
      var a = newNodeI(nkAsgn, n.info, 2)
      a[0] = newSymNode(c.p.resultSym)
      a[1] = result
      result = semAsgn(c, a)
  else:
    result = discardCheck(c, result, {})

  if c.p.owner.kind notin {skMacro, skTemplate} and
     c.p.resultSym != nil and c.p.resultSym.typ.isMetaType:
    if isEmptyType(result.typ):
      # we inferred a 'void' return type:
      c.p.resultSym.typ = errorType(c)
      c.p.owner.typ[0] = nil
    else:
      localReport(c.config, c.p.resultSym.info, reportSym(
        rsemCannotInferReturnType, c.p.owner))

  if isInlineIterator(c.p.owner.typ) and c.p.owner.typ[0] != nil and
      c.p.owner.typ[0].kind == tyUntyped:
    localReport(c.config, c.p.owner.info, reportSym(
        rsemCannotInferReturnType, c.p.owner))

  closeScope(c)

proc semYieldVarResult(c: PContext, n: PNode, restype: PType): PNode =
  ## Validates that the yield's expression is a tuple constructor when `restype`
  ## is a tuple type that contains views. Also introduces ``nkHiddenAddr``
  ## where needed
  addInNimDebugUtils(c.config, "semYieldVarResult", n, result)
  result = copyNode(n)

  let t = skipTypes(restype, {tyGenericInst, tyAlias, tySink})
  var hasError = false

  case t.kind
  of tyVar, tyLent:
    let unwrappedValue =
      case n[0].kind
      of nkHiddenStdConv, nkHiddenSubConv:
        n[0][1]
      else:
        n[0]

    result.add takeImplicitAddr(c, t, unwrappedValue)
    hasError = result[0].isError
  of tyTuple:
    # first, check if the tuple contains a *direct* view-type. If it does, the
    # 'yield' expression *must* be a literal tuple constructor.
    # NOTE: once moving view types out of the experimental state, this rule
    #       becomes obsolete
    var containsView = false
    for i in 0..<t.len:
      if t[i].kind in {tyVar, tyLent}:
        containsView = true
        break

    let tupleConstr =
      case n[0].kind
      of nkHiddenStdConv, nkHiddenSubConv:
        n[0][1]
      else:
        n[0]

    if containsView and tupleConstr.kind in {nkPar, nkTupleConstr}:
      # insert an ``nkHiddenAddr`` node (i.e. create a view) for each element
      # that is a view
      for i in 0..<t.len:
        let e = t[i]

        template useAddr(node: PNode) =
          let a = takeImplicitAddr(c, e, node)
          node = a
          hasError = hasError or a.isError

        if e.kind in {tyVar, tyLent}:
          case tupleConstr[i].kind
          of nkExprColonExpr:
            useAddr tupleConstr[i][1]
          else:
            useAddr tupleConstr[i]

      result.add n[0]
    elif containsView:
      # the tuple contains a view type but the expression is not a literal
      # tuple constructor
      # FIXME: this is confusing and inconsistent. For example, why is
      #        ``(x, y)`` allowed as the 'yield' expression for an iterator
      #        with return type ``(int, (int, var int))``, but ``x`` is not
      #        for ``(int, var int)``?
      hasError = true
      result.add newError(c.config, n[0],
                          SemReport(kind: rsemYieldExpectedTupleConstr,
                                    typ: t,
                                    ast: n[0]))
    else:
      result.add n[0]

  else:
    result.add n[0]

  if hasError:
    result = c.config.wrapError(result)

proc semYield(c: PContext, n: PNode): PNode =
  result = n
  checkSonsLen(n, 1, c.config)
  if c.p.owner == nil or c.p.owner.kind != skIterator:
    localReport(c.config, n, reportSem rsemUnexpectedYield)
  elif n[0].kind != nkEmpty:
    n[0] = semExprWithType(c, n[0]) # check for type compatibility:
    var iterType = c.p.owner.typ
    let restype = iterType[0]
    if restype != nil:
      if restype.kind != tyUntyped:
        n[0] = fitNode(c, restype, n[0], n.info)
      c.config.internalAssert(n[0].typ != nil, n.info, "semYield")

      if resultTypeIsInferrable(restype):
        let inferred = n[0].typ
        iterType[0] = inferred
        if c.p.resultSym != nil:
          c.p.resultSym.typ = inferred

      result = semYieldVarResult(c, n, restype)
    else:
      result = c.config.newError(result, reportSem rsemCannotReturnTypeless)

  elif c.p.owner.typ[0] != nil:
    result = c.config.newError(result, reportSem rsemExpectedValueForYield)

proc semDefined(c: PContext, n: PNode): PNode =
  checkSonsLen(n, 2, c.config)
  # we replace this node by a 'true' or 'false' node:
  result = newIntNode(nkIntLit, 0)
  result.intVal = ord isDefined(c.config,
                                legacyConsiderQuotedIdent(c, n[1], n).s)
  result.info = n.info
  result.typ = getSysType(c.graph, n.info, tyBool)

proc lookUpForDeclared(c: PContext, n: PNode, onlyCurrentScope: bool): PSym =
  case n.kind
  of nkIdent, nkAccQuoted:
    var amb = false
    let (ident, err) = considerQuotedIdent(c, n)
    if err != nil:
      localReport(c.config, err)
    result = if onlyCurrentScope:
               localSearchInScope(c, ident)
             else:
               searchInScopes(c, ident, amb)
  of nkDotExpr:
    result = nil
    if onlyCurrentScope: return
    checkSonsLen(n, 2, c.config)
    var m = lookUpForDeclared(c, n[0], onlyCurrentScope)
    if m != nil and m.kind == skModule:
      let ident = legacyConsiderQuotedIdent(c, n[1], n)
      if m == c.module:
        result = strTableGet(c.topLevelScope.symbols, ident)
      else:
        result = someSym(c.graph, m, ident)
  of nkSym:
    result = n.sym
  of nkOpenSymChoice, nkClosedSymChoice:
    result = n[0].sym
  else:
    localReport(c.config, n, reportSem rsemExpectedIdentifier)
    result = nil

proc semDeclared(c: PContext, n: PNode, onlyCurrentScope: bool): PNode =
  checkSonsLen(n, 2, c.config)
  # we replace this node by a 'true' or 'false' node:
  result = newIntNode(nkIntLit, 0)
  result.intVal = ord lookUpForDeclared(c, n[1], onlyCurrentScope) != nil
  result.info = n.info
  result.typ = getSysType(c.graph, n.info, tyBool)

proc expectString(c: PContext, n: PNode): string =
  var n = semConstExpr(c, n)
  if n.kind in nkStrKinds:
    return n.strVal
  else:
    localReport(c.config, n, reportSem rsemStringLiteralExpected)

proc newAnonSym(c: PContext; kind: TSymKind, info: TLineInfo): PSym =
  result = newSym(kind, c.cache.idAnon, nextSymId c.idgen, getCurrOwner(c), info)

proc semExpandToAst(c: PContext, n: PNode): PNode =
  let macroCall = n[1]

  if isCallExpr(macroCall):
    for i in 1..<macroCall.len:
      #if macroCall[0].typ[i].kind != tyUntyped:
      macroCall[i] = semExprWithType(c, macroCall[i], {})
    # performing overloading resolution here produces too serious regressions:
    let headSymbol = macroCall[0]
    var cands = 0
    var cand: PSym = nil
    var o: TOverloadIter
    var symx = initOverloadIter(o, c, headSymbol)
    while symx != nil:
      if symx.kind in {skTemplate, skMacro} and symx.typ.len == macroCall.len:
        cand = symx
        inc cands
      elif symx.isError:
        localReport(c.config, symx.ast)
      symx = nextOverloadIter(o, c, headSymbol)
    if cands == 0:
      localReport(c.config, n.info, semReportCountMismatch(
        rsemExpectedTemplateWithNArgs, macroCall.len - 1, 0, macroCall))

    elif cands >= 2:
      localReport(c.config, n.info, reportAst(
        rsemAmbiguousGetAst, macroCall))
    else:
      let info = macroCall[0].info
      macroCall[0] = newSymNode(cand, info)
      markUsed(c, info, cand)
      onUse(info, cand)

    # we just perform overloading resolution here:
    #n[1] = semOverloadedCall(c, macroCall, macroCall, {skTemplate, skMacro})
  else:
    localReport(c.config, n, reportSem rsemExpectedCallForGetAst)
  # Preserve the magic symbol in order to be handled in evals.nim
  c.config.internalAssert n[0].sym.magic == mExpandToAst
  #n.typ = getSysSym("NimNode").typ # expandedSym.getReturnType
  if n.kind == nkStmtList and n.len == 1: result = n[0]
  else: result = n
  result.typ = sysTypeFromName(c.graph, n.info, "NimNode")

proc semExpandToAst(c: PContext, n: PNode, magicSym: PSym,
                    flags: TExprFlags = {}): PNode =
  if n.len == 2:
    n[0] = newSymNode(magicSym, n.info)
    result = semExpandToAst(c, n)
  else:
    result = semDirectOp(c, n, flags)

proc processQuotations(c: PContext; n: var PNode, op: string,
                       quotes: var seq[PNode],
                       ids: var seq[PNode]) =
  template returnQuote(q) =
    quotes.add q
    n = newIdentNode(getIdent(c.cache, $quotes.len), n.info)
    ids.add n
    return

  template handlePrefixOp(prefixed) =
    if prefixed[0].kind == nkIdent:
      let examinedOp = prefixed[0].ident.s
      if examinedOp == op:
        returnQuote prefixed[1]
      elif examinedOp.startsWith(op):
        prefixed[0] = newIdentNode(
                        getIdent(c.cache, examinedOp.substr(op.len)),
                        prefixed.info)

  case n.kind
  of nkPrefix:
    checkSonsLen(n, 2, c.config)
    handlePrefixOp(n)
  of nkAccQuoted:
    if op == "``":
      returnQuote n[0]
    else: # [bug #7589](https://github.com/nim-lang/Nim/issues/7589)
      if n.len == 2 and n[0].ident.s == op:
        var tempNode = nkPrefix.newTree()
        tempNode.newSons(2)
        tempNode[0] = n[0]
        tempNode[1] = n[1]
        handlePrefixOp(tempNode)
  of nkIdent:
    if n.ident.s == "result":
      n = ids[0]
  else:
    discard # xxx: raise an error

  for i in 0..<n.safeLen:
    processQuotations(c, n[i], op, quotes, ids)

proc semQuoteAst(c: PContext, n: PNode): PNode =
  if n.len != 2 and n.len != 3:
    result = c.config.newError(result, semReportCountMismatch(
                               rsemWrongNumberOfArguments,
                               expected = 1,
                               got = result.len - 1,
                               node = result))
    return

  # We transform the do block into a template with a param for
  # each interpolation. We'll pass this template to getAst.
  var
    quotedBlock = n[^1]
    op = if n.len == 3: expectString(c, n[1]) else: "``"
    quotes = newSeq[PNode](2)
      # the quotes will be added to a nkCall statement
      # leave some room for the callee symbol and the result symbol
    ids = newSeq[PNode](1)
      # this will store the generated param names
      # leave some room for the result symbol

  if quotedBlock.kind != nkStmtList:
    semReportIllformedAst(c.config, n, {nkStmtList})

  # This adds a default first field to pass the result symbol
  ids[0] = newAnonSym(c, skParam, n.info).newSymNode
  processQuotations(c, quotedBlock, op, quotes, ids)

  var dummyTemplate = newProcNode(
    nkTemplateDef, quotedBlock.info, body = quotedBlock,
    params = c.graph.emptyNode,
    name = newAnonSym(c, skTemplate, n.info).newSymNode,
              pattern = c.graph.emptyNode, genericParams = c.graph.emptyNode,
              pragmas = c.graph.emptyNode, exceptions = c.graph.emptyNode)

  if ids.len > 0:
    dummyTemplate[paramsPos] = newNodeI(nkFormalParams, n.info)
    dummyTemplate[paramsPos].add:
      getSysSym(c.graph, n.info, "untyped").newSymNode # return type
    ids.add getSysSym(c.graph, n.info, "untyped").newSymNode # params type
    ids.add c.graph.emptyNode # no default value
    dummyTemplate[paramsPos].add newTreeI(nkIdentDefs, n.info, ids)

  var tmpl = semTemplateDef(c, dummyTemplate)
  quotes[0] = tmpl[namePos]
  # This adds a call to newIdentNode("result") as the first argument to the
  # template call
  let identNodeSym = getCompilerProc(c.graph, "newIdentNode")
  # so that new Nim compilers can compile old macros.nim versions, we check for
  # 'nil' here and provide the old fallback solution:
  let identNode = if identNodeSym == nil:
                    newIdentNode(getIdent(c.cache, "newIdentNode"), n.info)
                  else:
                    identNodeSym.newSymNode
  quotes[1] = newTreeI(nkCall, n.info, identNode, newStrNode(nkStrLit, "result"))
  result =
    c.semExpandToAst:
      newTreeI(nkCall, n.info,
        createMagic(c.graph, c.idgen, "getAst", mExpandToAst).newSymNode,
        newTreeI(nkCall, n.info, quotes))

proc tryExpr(c: PContext, n: PNode, flags: TExprFlags = {}): PNode =
  # watch out, hacks ahead:
  when defined(nimsuggest):
    # Remove the error hook so nimsuggest doesn't report errors there
    let tempHook = c.graph.config.structuredReportHook
    c.graph.config.structuredReportHook =
      proc(conf: ConfigRef, report: Report): TErrorHandling = discard

  let oldErrorCount = c.config.errorCounter
  let oldErrorMax = c.config.errorMax
  let oldCompilesId = c.compilesContextId
  # if this is a nested 'when compiles', do not increase the ID so that
  # generic instantiations can still be cached for this level.
  if c.compilesContextId == 0:
    inc c.compilesContextIdGenerator
    c.compilesContextId = c.compilesContextIdGenerator

  c.config.errorMax = high(int) # `setErrorMaxHighMaybe` not appropriate here

  # open a scope for temporary symbol inclusions:
  let oldScope = c.currentScope
  openScope(c)
  let oldOwnerLen = c.graph.owners.len
  let oldGenerics = c.generics
  let oldErrorOutputs = c.config.m.errorOutputs
  if efExplain notin flags: c.config.m.errorOutputs = {}
  let oldContextLen = msgs.getInfoContextLen(c.config)

  let oldInGenericContext = c.inGenericContext
  let oldInUnrolledContext = c.inUnrolledContext
  let oldInGenericInst = c.inGenericInst
  let oldInStaticContext = c.inStaticContext
  let oldProcCon = c.p
  c.generics = @[]
  var err: string
  try:
    result = semExpr(c, n, flags)
    if result != nil and efNoSem2Check notin flags:
      trackStmt(c, c.module, result, isTopLevel = false)
    if c.config.errorCounter != oldErrorCount and
       result != nil and result.kind != nkError:
      result = nil
  except ERecoverableError:
    discard
  # undo symbol table changes (as far as it's possible):
  c.compilesContextId = oldCompilesId
  c.generics = oldGenerics
  c.inGenericContext = oldInGenericContext
  c.inUnrolledContext = oldInUnrolledContext
  c.inGenericInst = oldInGenericInst
  c.inStaticContext = oldInStaticContext
  c.p = oldProcCon
  msgs.setInfoContextLen(c.config, oldContextLen)
  setLen(c.graph.owners, oldOwnerLen)
  c.currentScope = oldScope
  c.config.m.errorOutputs = oldErrorOutputs
  c.config.errorCounter = oldErrorCount
  c.config.errorMax = oldErrorMax
  when defined(nimsuggest):
    # Restore the error hook
    c.graph.config.structuredReportHook = tempHook

proc semCompiles(c: PContext, n: PNode, flags: TExprFlags): PNode =
  # we replace this node by a 'true' or 'false' node:
  if n.len != 2: return semDirectOp(c, n, flags)

  # xxx: need to further confirm, but `n[1]` below might need to be copied
  #      defensively, as inclusion of nkError nodes may mutate the original AST
  #      that was passed in via the compiles call.

  let
    exprVal = tryExpr(c, n[1], flags)
    didCompile = exprVal != nil and exprVal.kind != nkError
      ## this is the one place where we don't propagate nkError, wrapping the
      ## parent because this is a `compiles` call and should not leak across
      ## the AST boundary

  result = newIntNode(nkIntLit, ord(didCompile))
  result.info = n.info
  result.typ = getSysType(c.graph, n.info, tyBool)

proc semShallowCopy(c: PContext, n: PNode, flags: TExprFlags): PNode =
  if n.len == 3:
    # XXX ugh this is really a hack: shallowCopy() can be overloaded only
    # with procs that take not 2 parameters:
    result = newNodeI(nkFastAsgn, n.info)
    result.add(n[1])
    result.add(n[2])
    result = semAsgn(c, result)
  else:
    result = semDirectOp(c, n, flags)

proc setMs(n: PNode, s: PSym): PNode =
  result = n
  n[0] = newSymNode(s)
  n[0].info = n.info

proc semSizeof(c: PContext, n: PNode): PNode =
  case n.len
  of 2:
    #restoreOldStyleType(n[1])
    n[1] = semExprWithType(c, n[1], {efDetermineType})
    n.typ = getSysType(c.graph, n.info, tyInt)
    result = foldSizeOf(c.config, n, n)
  else:
    result = c.config.newError(n, reportStr(rsemExpectedTypeOrValue, "sizeof"))

proc semMagic(c: PContext, n: PNode, s: PSym, flags: TExprFlags): PNode =
  # this is a hotspot in the compiler!
  result = n
  case s.magic # magics that need special treatment
  of mAddr:
    markUsed(c, n.info, s)
    checkSonsLen(n, 2, c.config)
    result[0] = newSymNode(s, n[0].info)
    result[1] = semAddrArg(c, n[1])
    result.typ = makePtrType(c, result[1].typ)
  of mTypeOf:
    markUsed(c, n.info, s)
    result = semTypeOf(c, n)
  of mDefined:
    markUsed(c, n.info, s)
    result = semDefined(c, setMs(n, s))
  of mDeclared:
    markUsed(c, n.info, s)
    result = semDeclared(c, setMs(n, s), false)
  of mDeclaredInScope:
    markUsed(c, n.info, s)
    result = semDeclared(c, setMs(n, s), true)
  of mCompiles:
    markUsed(c, n.info, s)
    result = semCompiles(c, setMs(n, s), flags)
  of mIs:
    markUsed(c, n.info, s)
    result = semIs(c, setMs(n, s), flags)
  of mShallowCopy:
    markUsed(c, n.info, s)
    result = semShallowCopy(c, n, flags)
  of mExpandToAst:
    markUsed(c, n.info, s)
    result = semExpandToAst(c, n, s, flags)
  of mQuoteAst:
    markUsed(c, n.info, s)
    result = semQuoteAst(c, n)
  of mAstToStr:
    markUsed(c, n.info, s)
    checkSonsLen(n, 2, c.config)
    result = newStrNodeT(renderTree(n[1], {renderNoComments}), n, c.graph)
    result.typ = getSysType(c.graph, n.info, tyString)
  of mProcCall:
    markUsed(c, n.info, s)
    result = setMs(n, s)
    result[1] = semExpr(c, n[1])
    result.typ = n[1].typ
  of mPlugin:
    markUsed(c, n.info, s)
    # semDirectOp with conditional 'afterCallActions':
    #semLazyOpAux(c, n)
    result = semOverloadedCallAnalyseEffects(c, n, flags)
    if result == nil:
      result = errorNode(c, n)
    else:
      let callee = result[0].sym
      if callee.magic == mNone:
        semFinishOperands(c, result)
      activate(c, result)
      result = fixAbstractType(c, result)
      result = fixVarArgumentsAndAnalyse(c, result)
      if callee.magic != mNone:
        result = magicsAfterOverloadResolution(c, result, flags)
  of mRunnableExamples:
    markUsed(c, n.info, s)
    if c.config.cmd in cmdDocLike and n.len >= 2 and n.lastSon.kind == nkStmtList:
      when false:
        # some of this dead code was moved to `prepareExamples`
        if sfMainModule in c.module.flags:
          let inp = toFullPath(c.config, c.module.info)
          if c.runnableExamples == nil:
            c.runnableExamples = newTree(nkStmtList,
              newTree(nkImportStmt, newStrNode(nkStrLit, expandFilename(inp))))
          let imports = newTree(nkStmtList)
          var savedLastSon = copyTree n.lastSon
          extractImports(savedLastSon, imports)
          for imp in imports: c.runnableExamples.add imp
          c.runnableExamples.add newTree(nkBlockStmt, c.graph.emptyNode, copyTree savedLastSon)
      result = setMs(n, s)
    else:
      result = c.graph.emptyNode
  of mSizeOf:
    markUsed(c, n.info, s)
    result = semSizeof(c, setMs(n, s))
  else:
    result = semDirectOp(c, n, flags)

proc semWhen(c: PContext, n: PNode, semCheck = true): PNode =
  # If semCheck is set to false, `when` will return the verbatim AST of
  # the correct branch. Otherwise the AST will be passed through semStmt.
  addInNimDebugUtils(c.config, "semWhen", n, result)

  result = nil

  template setResult(e: untyped) =
    if semCheck: result = semExpr(c, e) # do not open a new scope!
    else: result = e

  # Check if the node is "when nimvm"
  # when nimvm:
  #   ...
  # else:
  #   ...
  var whenNimvm = false
  var typ = commonTypeBegin
  if n.len == 2 and n[0].kind == nkElifBranch and
      n[1].kind == nkElse:
    let exprNode = n[0][0]
    if exprNode.kind == nkIdent:
      whenNimvm = lookUp(c, exprNode).magic == mNimvm
    elif exprNode.kind == nkSym:
      whenNimvm = exprNode.sym.magic == mNimvm
    if whenNimvm: n.flags.incl nfLL

  for i in 0..<n.len:
    var it = n[i]
    case it.kind
    of nkElifBranch, nkElifExpr:
      checkSonsLen(it, 2, c.config)
      if whenNimvm:
        if semCheck:
          it[1] = semExpr(c, it[1])
          typ = commonType(c, typ, it[1].typ)
        result = n # when nimvm is not elimited until codegen
      else:
        let e = forceBool(c, semConstExpr(c, it[0]))
        if e.kind != nkIntLit:
          # can happen for cascading errors, assume false
          # InternalError(n.info, "semWhen")
          discard
        elif e.intVal != 0 and result == nil:
          setResult(it[1])
          return # we're not in nimvm and we already have a result
    of nkElse, nkElseExpr:
      checkSonsLen(it, 1, c.config)
      if result == nil or whenNimvm:
        if semCheck:
          it[0] = semExpr(c, it[0])
          typ = commonType(c, typ, it[0].typ)
        if result == nil:
          result = it[0]
    else:
      semReportIllformedAst(c.config, n, {
        nkElse, nkElseExpr, nkElifBranch, nkElifExpr})

  if result == nil:
    result = newNodeI(nkEmpty, n.info)
  if whenNimvm:
    result.typ = typ

proc semSetConstr(c: PContext, n: PNode): PNode =
  result = newNodeI(nkCurly, n.info)
  result.typ = newTypeS(tySet, c)
  result.typ.flags.incl tfIsConstructor
  if n.len == 0:
    rawAddSon(result.typ, newTypeS(tyEmpty, c))
  else:
    # only semantic checking for all elements, later type checking:
    var typ: PType = nil
    for i in 0..<n.len:
      if isRange(n[i]):
        checkSonsLen(n[i], 3, c.config)
        n[i][1] = semExprWithType(c, n[i][1])
        n[i][2] = semExprWithType(c, n[i][2])
        if typ == nil:
          typ = skipTypes(n[i][1].typ,
                          {tyGenericInst, tyVar, tyLent, tyOrdinal, tyAlias, tySink})
        n[i].typ = n[i][2].typ # range node needs type too
      elif n[i].kind == nkRange:
        # already semchecked
        if typ == nil:
          typ = skipTypes(n[i][0].typ,
                          {tyGenericInst, tyVar, tyLent, tyOrdinal, tyAlias, tySink})
      else:
        n[i] = semExprWithType(c, n[i])
        if typ == nil:
          typ = skipTypes(n[i].typ, {tyGenericInst, tyVar, tyLent, tyOrdinal, tyAlias, tySink})
    if not isOrdinalType(typ, allowEnumWithHoles=true):
      localReport(c.config, n, reportSem rsemExpectedOrdinal)
      typ = makeRangeType(c, 0, MaxSetElements - 1, n.info)

    elif lengthOrd(c.config, typ) > MaxSetElements:
      typ = makeRangeType(c, 0, MaxSetElements - 1, n.info)

    addSonSkipIntLit(result.typ, typ, c.idgen)
    for i in 0..<n.len:
      var m: PNode
      let info = n[i].info
      if isRange(n[i]):
        m = newNodeI(nkRange, info)
        m.add fitNode(c, typ, n[i][1], info)
        m.add fitNode(c, typ, n[i][2], info)

      elif n[i].kind == nkRange:
        m = n[i] # already semchecked

      else:
        m = fitNode(c, typ, n[i], info)

      result.add m

proc semTableConstr(c: PContext, n: PNode): PNode =
  # we simply transform ``{key: value, key2, key3: value}`` to
  # ``[(key, value), (key2, value2), (key3, value2)]``
  result = newNodeI(nkBracket, n.info)
  var lastKey = 0
  for i in 0..<n.len:
    var x = n[i]
    if x.kind == nkExprColonExpr and x.len == 2:
      for j in lastKey..<i:
        var pair = newNodeI(nkTupleConstr, x.info)
        pair.add(n[j])
        pair.add(x[1])
        result.add(pair)

      var pair = newNodeI(nkTupleConstr, x.info)
      pair.add(x[0])
      pair.add(x[1])
      result.add(pair)

      lastKey = i+1

  if lastKey != n.len:
    semReportIllformedAst(c.config, n, "")

  result = semExpr(c, result)


proc semTupleFieldsConstr(c: PContext, n: PNode, flags: TExprFlags): PNode =
  ## analyse tuple construction with fields specified `(foo: "bar", num: 13)`
  
  addInNimDebugUtils(c.config, "semTupleFieldsConstr", n, result, flags)

  c.config.internalAssert(n.kind == nkTupleConstr,
                          "expected nkTupleConstr, got: " & $n.kind)

  result = newNodeI(nkTupleConstr, n.info)

  var
    typ = newTypeS(tyTuple, c) ## the type of the tuple
    ids = initIntSet()         ## track if a field was already initialized
    hasError = false           ## so we know to wrap the result

  typ.n = newNodeI(nkRecList, n.info) # nkIdentDefs

  for i in 0..<n.len:
    # ensure we have a colon expression for field pairs and 
    if n[i].kind != nkExprColonExpr:
      result.add:
        c.config.newError(n[i], reportSem rsemNamedExprExpected)
      hasError = true
      continue # we can do anything else with this "field"
      # xxx: rewrite this if into a case

    # determine the field name (left hand side)
    let
      (id, err) = considerQuotedIdent(c, n[i][0])
      fieldName =
        case n[i][0].kind
        of nkSym, nkIdent, nkAccQuoted:
          if err != nil:   # ident error
            err
          elif containsOrIncl(ids, id.id):
            # init a field more than once
            c.config.newError(n[i][0], reportStr(rsemFieldInitTwice, id.s))
          else:
            n[i][0] # no issues, leave as is
        else:
          # lhs of the colon expr is an identifier of some kind
          c.config.newError(n[i][0], reportSem rsemNamedExprExpected)
    
    # handle any errors with the field and make the sym
    let fieldSym =
      case fieldName.kind
      of nkError: # we previously encountered an error
        hasError = true
        let errSym = newSym(skError, id, nextSymId(c.idgen), getCurrOwner(c),
                            fieldName.info)
        errSym.typ = c.errorType
        errSym.ast = fieldName
        errSym
      else:
        newSymS(skField, fieldName, c)
    fieldSym.position = i

    # determine the field value (right hand side)
    let fieldValue = semExprWithType(c, n[i][1], {})

    # field values can't have typeDescs
    n[i][1] =
      case fieldValue.typ.kind
      of tyTypeDesc:
        hasError = true
        let err = c.config.newError(fieldValue,
                    reportSem rsemDisallowedTypedescForTupleField)
        err.typ = errorType(c)
        err
      else:
        fieldValue

    # the field type is the rhs' type
    fieldSym.typ = skipIntLit(n[i][1].typ, c.idgen)
    rawAddSon(typ, fieldSym.typ)

    typ.n.add newSymNode(fieldSym) # the type remembers fields as a "schema"
    n[i][0] = newSymNode(fieldSym) # new node avoids corrupting the type's copy
    
    result.add n[i]
  result.typ = typ

  if hasError:
    result = c.config.wrapError(result)

proc semTuplePositionsConstr(c: PContext, n: PNode, flags: TExprFlags): PNode =
  ## analyse tuple construction based on position `("bar", 13)`, also handles
  ## tuple type declarations.

  addInNimDebugUtils(c.config, "semTuplePositionsConstr", n, result, flags)

  c.config.internalAssert(n.kind == nkTupleConstr,
                          "expected nkTupleConstr, got: " & $n.kind)
  
  let
    tupExp = n                  # we don't modify n, but compute the type:
    typ = newTypeS(tyTuple, c)  # leave typ.n nil!
  for i in 0..<tupExp.len:
    tupExp[i] = semExprWithType(c, tupExp[i], {}) # xxx: claim of not modifying
                                                  #      n is dubious
    addSonSkipIntLit(typ, tupExp[i].typ, c.idgen)
  tupExp.typ = typ

  var
    isTupleType: bool
    hasError = false
  if tupExp.len > 0: # don't interpret () as type
    isTupleType = tupExp[0].typ.kind == tyTypeDesc
    # check if either everything or nothing is tyTypeDesc
    for i in 1..<tupExp.len:
      if tupExp[i].kind == nkExprColonExpr:
        hasError = true
        # xxx: not sure if this modification is safe
        tupExp[i] = c.config.newError(tupExp[i],
                                      reportSem rsemNamedExprNotAllowed)
      elif isTupleType != (tupExp[i].typ.kind == tyTypeDesc):
        return newError(
          c.config, n, reportSem rsemCannotMixTypesAndValuesInTuple,
          posInfo = tupExp[i].info)

  if hasError:
    result = c.config.wrapError(tupExp)
  elif isTupleType: # reinterpret `(int, string)` as type expressions
    result = n
    result.typ = makeTypeDesc(c, semTypeNode(c, n, nil).skipTypes({tyTypeDesc}))
  else:
    result = tupExp

proc semTupleConstr(c: PContext, n: PNode, flags: TExprFlags): PNode =
  ## analyse tuple construction based on position of fields or return errors
  addInNimDebugUtils(c.config, "semTupleConstr", n, result, flags)
  case n.kind
  of nkTupleConstr:
    case n.len
    of 0:
      result = semTuplePositionsConstr(c, n, flags)
    else:
      case n[0].kind
      of nkExprColonExpr:
        result = semTupleFieldsConstr(c, n, flags)
      else:
        result = semTuplePositionsConstr(c, n, flags)
  of nkError:
    result = n
  else:
    c.config.internalError(n.info, "expected nkTupleConstr, got: " & $n.kind)


include semobjconstr

proc semBlock(c: PContext, n: PNode; flags: TExprFlags): PNode =
  addInNimDebugUtils(c.config, "semBlock", n, result, flags)

  assert n != nil

  case n.kind
  of nkError:
    result = n
  of nkBlockExpr, nkBlockStmt:
    checkSonsLen(n, 2, c.config)
    inc(c.p.nestedBlockCounter)
    openScope(c) # BUGFIX: label is in the scope of block!

    # handle the label
    let 
      givenLabl = n[0]
      lablRes =
        case givenLabl.kind
        of nkEmpty, nkError:
          givenLabl
        of nkIdent, nkSym, nkAccQuoted:
          let labl = newSymG(skLabel, givenLabl, c)
          
          if sfGenSym notin labl.flags:
            addDecl(c, labl)
          elif labl.owner == nil:
            labl.owner = c.p.owner
          
          let lablNode = newSymNode2(labl, givenLabl.info)

          suggestSym(c.graph, lablNode.info, labl, c.graph.usageSym)
          styleCheckDef(c.config, labl)
          onDef(lablNode.info, labl)

          lablNode
        else:
          c.config.newError(givenLabl,
                            reportAst(rsemIdentExpectedInExpr, givenLabl))
      bodyRes = semExpr(c, n[1], flags)

    result = copyNode(n)
    result.flags = n.flags # xxx: we should be able to perserve flags, this
                           #      code didn't used to do a copyNode, it changed
                           #      `n` directly
    result.add lablRes
    result.add bodyRes
    result.typ = bodyRes.typ
    
    # xxx: always transitions because nkError always(?) has an error type
    if isEmptyType(result.typ):
      result.transitionSonsKind(nkBlockStmt)
    else:
      result.transitionSonsKind(nkBlockExpr)

    if lablRes.kind == nkError or bodyRes.kind == nkError:
      result = c.config.wrapError(result)

    closeScope(c)  
    dec(c.p.nestedBlockCounter)
  else:
    c.config.internalError:
      "expected block expresssion or statement, got: " & $n.kind

  # inc(c.p.nestedBlockCounter)
  # # result = n
  # openScope(c) # BUGFIX: label is in the scope of block!
  # if n[0].kind != nkEmpty:
  #   var labl = newSymG(skLabel, n[0], c)
  #   if sfGenSym notin labl.flags:
  #     addDecl(c, labl)
  #   elif labl.owner == nil:
  #     labl.owner = c.p.owner
  #   n[0] = newSymNode(labl, n[0].info)
  #   suggestSym(c.graph, n[0].info, labl, c.graph.usageSym)
  #   styleCheckDef(c.config, labl)
  #   onDef(n[0].info, labl)
  # n[1] = semExpr(c, n[1], flags)
  # n.typ = n[1].typ
  # if isEmptyType(n.typ): n.transitionSonsKind(nkBlockStmt)
  # else: n.transitionSonsKind(nkBlockExpr)
  # closeScope(c)
  # dec(c.p.nestedBlockCounter)


proc semExportExcept(c: PContext, n: PNode): PNode =
  ## analyses an nkExportExceptStmt, producing an nkExportStmt, or nkError if
  ## analysis failed. If successful, at least partially, the target module to
  ## export's symbol along with its interface symbols are exported.
  addInNimDebugUtils(c.config, "semExportExcept", n, result)

  assert n != nil, "nil node instead of an export except statement"
  internalAssert(c.config, n.kind == nkExportExceptStmt,
    "only export except statements allowed, got: " & $n.kind)

  checkMinSonsLen(n, 2, c.config) # at least a module and one excluded ident

  var hasError = false
  
  # xxx: besides the `exceptSet` optimization above, this proc can be split
  #      into interleaved normalization and eval phases. Not a big win in and
  #      of itself but this is likely how sem comes apart and some IRs emerge.

  let
    # start normalizing the export except statement
    normExportExcept = newNodeI(nkExportExceptStmt, n.info)
    
    # normalize the module name part
    moduleName = semExpr(c, n[0])
    normModuleName =
      case moduleName.kind
      of nkSym:
        moduleName
      of nkError:
        # pass the nkError through
        hasError = true
        moduleName
      else:
        # something went very wrong
        hasError = true
        c.config.internalError(moduleName.info,
                              "Excepted nkSym, got: " & $n.kind)
        nil  # internalError means we don't actually end up with nil

  # eval of the name part
  if not normModuleName.isError:
    reexportSym(c, normModuleName.sym)
    markUsed(c, n.info, normModuleName.sym)

  # finish module name part normalization and eval
  normExportExcept.add normModuleName

  var exceptSet = initIntSet()
    ## stores ident ids of symbols to exclude from the export
    ##
    ## this is an optimization to avoid looping over the except nodes twice.
    ## once for ident checking during normalization and then again for
    ## populating this set, we collect it in one shot.

  # normalize the except list
  for i in 1..<n.len:
    let
      ex = n[i]
      (ident, err) = lookups.considerQuotedIdent(c, ex)

    normExportExcept.add:
      if err.isNil:
        exceptSet.incl(ident.id)
        ex
      else:
        hasError = true
        err

  if hasError:
    result = c.config.wrapError(normExportExcept)
  else:
    result = newNodeI(nkExportStmt, n.info)
    result.add normModuleName
    let exported = normModuleName.sym

    # eval the except items and compose it into the final production
    for s in allSyms(c.graph, exported):
      if s.kind in ExportableSymKinds + {skModule} and
        s.name.id notin exceptSet and sfError notin s.flags:
          reexportSym(c, s)
          result.add newSymNode(s, n.info)


proc semExport(c: PContext, n: PNode): PNode =
  proc specialSyms(c: PContext; s: PSym) {.inline.} =
    if s.kind == skConverter: addConverter(c, LazySym(sym: s))
    elif s.kind == skType and s.typ != nil and s.typ.kind == tyEnum and sfPure in s.flags:
      addPureEnum(c, LazySym(sym: s))

  result = newNodeI(nkExportStmt, n.info)
  for i in 0..<n.len:
    let a = n[i]
    var o: TOverloadIter
    var s = initOverloadIter(o, c, a)
    if s == nil:
      localReport(c.config, a, reportSem rsemCannotExport)

    elif s.kind == skModule:
      # forward everything from that module:
      reexportSym(c, s)
      for it in allSyms(c.graph, s):
        if it.kind in ExportableSymKinds+{skModule}:
          reexportSym(c, it)
          result.add newSymNode(it, a.info)
          specialSyms(c, it)
      markUsed(c, n.info, s)

    else:
      while s != nil:
        if s.kind == skEnumField:
          localReport(c.config, a.info, reportSym(rsemCannotExport, s))
        elif s.isError:
          localReport(c.config, s.ast)

        if s.kind in ExportableSymKinds+{skModule} and sfError notin s.flags:
          result.add(newSymNode(s, a.info))
          reexportSym(c, s)
          markUsed(c, n.info, s)
          specialSyms(c, s)
          if s.kind == skType and sfPure notin s.flags:
            var etyp = s.typ
            if etyp.kind in {tyBool, tyEnum}:
              for j in 0..<etyp.n.len:
                var e = etyp.n[j].sym
                c.config.internalAssert(e.kind == skEnumField, s.info, "rawImportSymbol")
                reexportSym(c, e)

        s = nextOverloadIter(o, c, a)


proc shouldBeBracketExpr(n: PNode): bool =
  assert n.kind in nkCallKinds
  let a = n[0]
  if a.kind in nkCallKinds:
    let b = a[0]
    if b.kind in nkSymChoices:
      for i in 0..<b.len:
        if b[i].kind == nkSym and b[i].sym.magic == mArrGet:
          let be = newNodeI(nkBracketExpr, n.info)
          for i in 1..<a.len: be.add(a[i])
          n[0] = be
          return true

proc asBracketExpr(c: PContext; n: PNode): PNode =
  proc isGeneric(c: PContext; n: PNode): bool =
    if n.kind in {nkIdent, nkAccQuoted}:
      let s = qualifiedLookUp(c, n, {})
      if s.isError:
        # XXX: move to propagating nkError, skError, and tyError
        localReport(c.config, s.ast)
        result = false
      else:
        result = s != nil and isGenericRoutineStrict(s)

  assert n.kind in nkCallKinds
  if n.len > 1 and isGeneric(c, n[1]):
    let b = n[0]
    if b.kind in nkSymChoices:
      for i in 0..<b.len:
        if b[i].kind == nkSym and b[i].sym.magic == mArrGet:
          result = newNodeI(nkBracketExpr, n.info)
          for i in 1..<n.len: result.add(n[i])
          return result
  return nil

proc hoistParamsUsedInDefault(c: PContext, call, letSection, defExpr: var PNode) =
  # This takes care of complicated signatures such as:
  # proc foo(a: int, b = a)
  # proc bar(a: int, b: int, c = a + b)
  #
  # The recursion may confuse you. It performs two duties:
  #
  # 1) extracting all referenced params from default expressions
  #    into a let section preceding the call
  #
  # 2) replacing the "references" within the default expression
  #    with these extracted skLet symbols.
  #
  # The first duty is carried out directly in the code here, while the second
  # duty is activated by returning a non-nil value. The caller is responsible
  # for replacing the input to the function with the returned non-nil value.
  # (which is the hoisted symbol)
  if defExpr.kind == nkSym and defExpr.sym.kind == skParam and defExpr.sym.owner == call[0].sym:
    let paramPos = defExpr.sym.position + 1

    if call[paramPos].kind != nkSym:
      let hoistedVarSym = newSym(skLet, getIdent(c.graph.cache, genPrefix), nextSymId c.idgen,
                                 c.p.owner, letSection.info, c.p.owner.options)
      hoistedVarSym.typ = call[paramPos].typ

      letSection.add newTreeI(nkIdentDefs, letSection.info,
        newSymNode(hoistedVarSym),
        newNodeI(nkEmpty, letSection.info),
        call[paramPos])

      call[paramPos] = newSymNode(hoistedVarSym) # Refer the original arg to its hoisted sym

    # arg we refer to is a sym, wether introduced by hoisting or not doesn't matter, we simply reuse it
    defExpr = call[paramPos]
  else:
    for i in 0..<defExpr.safeLen:
      hoistParamsUsedInDefault(c, call, letSection, defExpr[i])

proc getNilType(c: PContext): PType =
  result = c.nilTypeCache
  if result == nil:
    result = newTypeS(tyNil, c)
    result.size = c.config.target.ptrSize
    result.align = c.config.target.ptrSize.int16
    c.nilTypeCache = result

proc enumFieldSymChoice(c: PContext, n: PNode, s: PSym): PNode =
  var o: TOverloadIter
  var i = 0
  var a = initOverloadIter(o, c, n)
  while a != nil:
    if a.kind in OverloadableSyms-{skModule}:
      inc(i)
      if i > 1: break
    elif a.isError:
      localReport(c.config, a.ast)
    a = nextOverloadIter(o, c, n)
  let info = getCallLineInfo(n)
  if i <= 1:
    if sfGenSym notin s.flags:
      result = newSymNode(s, info)
      markUsed(c, info, s)
      onUse(info, s)
    else:
      result = n
  else:
    result = newNodeIT(nkClosedSymChoice, info, newTypeS(tyNone, c))
    a = initOverloadIter(o, c, n)
    while a != nil:
      if a.kind in OverloadableSyms-{skModule}:
        incl(a.flags, sfUsed)
        markOwnerModuleAsUsed(c, a)
        result.add newSymNode(a, info)
        onUse(info, a)
      elif a.isError:
        localReport(c.config, a.ast)
      a = nextOverloadIter(o, c, n)

proc semExpr(c: PContext, n: PNode, flags: TExprFlags = {}): PNode =
  when defined(nimCompilerStacktraceHints):
    setFrameMsg c.config$n.info & " " & $n.kind
  addInNimDebugUtils(c.config, "semExpr", n, result, flags)

  result = n
  if c.config.cmd == cmdIdeTools: suggestExpr(c, n)
  if nfSem in n.flags: return
  case n.kind
  of nkIdent, nkAccQuoted:
    # analyse _usage_ of identifiers (`nkIdent` and `nkAccQuoted`), this is not
    # meant to be used for identifiers appearing in a definition position.
    #
    # usage vs definition by example:
    # - given: `var foo = bar`
    # - `foo` is being defined -- not applicable
    # - `bar` is being used -- applicable
    #
    # This example uses a var definition, but the name of a proc, param, etc...
    # are all definitions (nkIdentDefs is a good hint), which are handled
    # elsewhere.

    # query
    let checks = if efNoEvaluateGeneric in flags:
        {checkUndeclared, checkPureEnumFields}
      elif efInCall in flags:
        {checkUndeclared, checkPureEnumFields, checkModule}
      else:
        {checkUndeclared, checkPureEnumFields, checkModule, checkAmbiguity}
    var s = qualifiedLookUp(c, n, checks) # query & production (errors)

    # production (outside of errors above)
    case s.kind
    of skProc, skFunc, skMethod, skConverter, skIterator:
      result = symChoice(c, n, s, scClosed)
      if result.kind == nkSym:
        markIndirect(c, result.sym)
    of skEnumField:
      if overloadableEnums in c.features:
        result = enumFieldSymChoice(c, n, s)
      else:
        result = semSym(c, n, s, flags)
    of skError:
      result = semSym(c, n, s, flags)
      # XXX: propogate the error type as it might not have been set, this
      #      should not be required.
      result.typ = s.typ
    else:
      result = semSym(c, n, s, flags)

    if c.matchedConcept == nil:
      # XXX: concepts has a "pre-pass", so we need to guard the symbol capture
      #      for lambda lifting probably because it would mess something up?
      semCaptureSym(s, c.p.owner)
  of nkSym:
    # because of the changed symbol binding, this does not mean that we
    # don't have to check the symbol for semantics here again!
    result = semSym(c, n, n.sym, flags)
  of nkEmpty, nkNone, nkCommentStmt, nkType:
    discard
  of nkNilLit:
    if result.typ == nil: result.typ = getNilType(c)
  of nkIntLit:
    if result.typ == nil: setIntLitType(c, result)
  of nkInt8Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyInt8)
  of nkInt16Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyInt16)
  of nkInt32Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyInt32)
  of nkInt64Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyInt64)
  of nkUIntLit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyUInt)
  of nkUInt8Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyUInt8)
  of nkUInt16Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyUInt16)
  of nkUInt32Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyUInt32)
  of nkUInt64Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyUInt64)
  #of nkFloatLit:
  #  if result.typ == nil: result.typ = getFloatLitType(result)
  of nkFloat32Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyFloat32)
  of nkFloat64Lit, nkFloatLit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyFloat64)
  of nkFloat128Lit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyFloat128)
  of nkStrLit..nkTripleStrLit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyString)
  of nkCharLit:
    if result.typ == nil: result.typ = getSysType(c.graph, n.info, tyChar)
  of nkDotExpr:
    result = semFieldAccess(c, n, flags)
    if result.kind == nkDotCall:
      result.transitionSonsKind(nkCall)
      result = semExpr(c, result, flags)
  of nkBind:
    localReport(c.config, n, reportSem rsemBindDeprecated)
    result = semExpr(c, n[0], flags)

  of nkTypeOfExpr, nkTupleTy, nkTupleClassTy, nkRefTy..nkEnumTy, nkStaticTy:
    if c.matchedConcept != nil and n.len == 1:
      let modifier = n.modifierTypeKindOfNode
      if modifier != tyNone:
        var baseType = semExpr(c, n[0]).typ.skipTypes({tyTypeDesc})
        result.typ = c.makeTypeDesc(c.newTypeWithSons(modifier, @[baseType]))
        return
    let typ = semTypeNode(c, n, nil).skipTypes({tyTypeDesc})
    result.typ = makeTypeDesc(c, typ)
  of nkStmtListType:
    let typ = semTypeNode(c, n, nil)
    result.typ = makeTypeDesc(c, typ)
  of nkCall, nkInfix, nkPrefix, nkPostfix, nkCommand, nkCallStrLit:
    # check if it is an expression macro:
    checkMinSonsLen(n, 1, c.config)
    #when defined(nimsuggest):
    #  if gIdeCmd == ideCon and c.config.m.trackPos == n.info: suggestExprNoCheck(c, n)
    let mode = if nfDotField in n.flags: {} else: {checkUndeclared}
    c.isAmbiguous = false
    var s = qualifiedLookUp(c, n[0], mode)
    if s != nil and not s.isError:
      # xxx: currently `qualifiedLookUp` will set the s.ast field to nkError
      #      if there was an nkError reported instead of the legacy localReport
      #      based flows we need to halt progress. This also needs to do a
      #      better job of raising/handling the fact that we just got an error.
      case s.kind
      of skMacro, skTemplate:
        result = semDirectOp(c, n, flags)
      of skType:
        # XXX think about this more (``set`` procs)
        let ambig = c.isAmbiguous
        if not (n[0].kind in {nkClosedSymChoice, nkOpenSymChoice, nkIdent} and ambig) and n.len == 2:
          result = semConv(c, n)
        elif ambig and n.len == 1:
          errorUseQualifier(c, n.info, s)
        elif n.len == 1:
          result = semObjConstr(c, n, flags)
        elif s.magic == mNone: result = semDirectOp(c, n, flags)
        else: result = semMagic(c, n, s, flags)
      of skProc, skFunc, skMethod, skConverter, skIterator:
        if s.magic == mNone: result = semDirectOp(c, n, flags)
        else: result = semMagic(c, n, s, flags)
      else:
        #liMessage(n.info, warnUser, renderTree(n));
        result = semIndirectOp(c, n, flags)
    elif (n[0].kind == nkBracketExpr or shouldBeBracketExpr(n)) and
        isSymChoice(n[0][0]):
      # indirectOp can deal with explicit instantiations; the fixes
      # the 'newSeq[T](x)' bug
      setGenericParams(c, n[0])
      result = semDirectOp(c, n, flags)
    elif nfDotField in n.flags:
      result = semDirectOp(c, n, flags)
    elif isSymChoice(n[0]):
      let b = asBracketExpr(c, n)
      if b != nil:
        result = semExpr(c, b, flags)
      else:
        result = semDirectOp(c, n, flags)
    else:
      result = semIndirectOp(c, n, flags)

    if nfDefaultRefsParam in result.flags:
      result = result.copyTree #XXX: Figure out what causes default param nodes to be shared.. (sigmatch bug?)
      # We've found a default value that references another param.
      # See the notes in `hoistParamsUsedInDefault` for more details.
      var hoistedParams = newNodeI(nkLetSection, result.info)
      for i in 1..<result.len:
        hoistParamsUsedInDefault(c, result, hoistedParams, result[i])
      result = newTreeIT(nkStmtListExpr, result.info, result.typ, hoistedParams, result)
  of nkWhen:
    if efWantStmt in flags:
      result = semWhen(c, n, true)
    else:
      result = semWhen(c, n, false)
      if result == n:
        # This is a "when nimvm" stmt.
        result = semWhen(c, n, true)
      else:
        result = semExpr(c, result, flags)
  of nkBracketExpr:
    checkMinSonsLen(n, 1, c.config)
    result = semArrayAccess(c, n, flags)
  of nkCurlyExpr:
    result = semExpr(c, buildOverloadedSubscripts(n, getIdent(c.cache, "{}")), flags)
  of nkPragmaExpr:
    let
      pragma = n[1]
      (pragmaName, err) = considerQuotedIdent(c, pragma[0])
    if err != nil:
      localReport(c.config, err)

    case whichKeyword(pragmaName)
    of wExplain:
      result = semExpr(c, n[0], flags + {efExplain})
    else:
      # what other pragmas are allowed for expressions? `likely`, `unlikely`
      result = invalidPragma(c, n)
  of nkPar:
    if n.len == 1:
      result = semExpr(c, n[0], flags)
    else:
      result = c.config.newError(n,
                    reportAst(
                      rsemIllformedAst,
                      n,
                      createSemIllformedAstMsg(n, {nkTupleConstr})))
  of nkTupleConstr:
    result = semTupleConstr(c, n, flags)
  of nkCurly: result = semSetConstr(c, n)
  of nkBracket: result = semArrayConstr(c, n, flags)
  of nkObjConstr: result = semObjConstr(c, n, flags)
  of nkLambdaKinds: result = semProcAux(c, n, skProc, lambdaPragmas, flags)
  of nkDerefExpr: result = semDeref(c, n)
  of nkAddr:
    result = n
    checkSonsLen(n, 1, c.config)
    result[0] = semAddrArg(c, n[0])
    result.typ = makePtrType(c, result[0].typ)
  of nkHiddenAddr, nkHiddenDeref:
    checkSonsLen(n, 1, c.config)
    n[0] = semExpr(c, n[0], flags)
  of nkCast: result = semCast(c, n)
  of nkIfExpr, nkIfStmt: result = semIf(c, n, flags)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv, nkHiddenCallConv:
    checkSonsLen(n, 2, c.config)
    considerGenSyms(c, n)
  of nkStringToCString, nkCStringToString, nkObjDownConv, nkObjUpConv:
    checkSonsLen(n, 1, c.config)
    considerGenSyms(c, n)
  of nkChckRangeF, nkChckRange64, nkChckRange:
    checkSonsLen(n, 3, c.config)
    considerGenSyms(c, n)
  of nkCheckedFieldExpr:
    checkMinSonsLen(n, 2, c.config)
    considerGenSyms(c, n)
  of nkTableConstr:
    result = semTableConstr(c, n)
  of nkClosedSymChoice, nkOpenSymChoice:
    # handling of sym choices is context dependent
    # the node is left intact for now
    discard
  of nkStaticExpr: result = semStaticExpr(c, n[0])
  of nkAsgn: result = semAsgn(c, n)
  of nkBlockStmt, nkBlockExpr: result = semBlock(c, n, flags)
  of nkStmtList, nkStmtListExpr: result = semStmtList(c, n, flags)
  of nkRaiseStmt: result = semRaise(c, n)
  of nkVarSection: result = semConstLetOrVar(c, n, skVar)
  of nkLetSection: result = semConstLetOrVar(c, n, skLet)
  of nkConstSection: result = semConstLetOrVar(c, n, skConst)
  of nkTypeSection: result = semTypeSection(c, n)
  of nkDiscardStmt: result = semDiscard(c, n)
  of nkWhileStmt: result = semWhile(c, n, flags)
  of nkTryStmt, nkHiddenTryStmt: result = semTry(c, n, flags)
  of nkBreakStmt, nkContinueStmt: result = semBreakOrContinue(c, n)
  of nkForStmt: result = semFor(c, n, flags)
  of nkCaseStmt: result = semCase(c, n, flags)
  of nkReturnStmt: result = semReturn(c, n)
  of nkUsingStmt: result = semUsing(c, n)
  of nkAsmStmt: result = semAsm(c, n)
  of nkYieldStmt: result = semYield(c, n)
  of nkPragma: result = pragma(c, c.p.owner, n, stmtPragmas, true)
  of nkIteratorDef: result = semIterator(c, n)
  of nkProcDef: result = semProc(c, n)
  of nkFuncDef: result = semFunc(c, n)
  of nkMethodDef: result = semMethod(c, n)
  of nkConverterDef: result = semConverterDef(c, n)
  of nkMacroDef: result = semMacroDef(c, n)
  of nkTemplateDef: result = semTemplateDef(c, n)
  of nkImportStmt:
    # this particular way allows 'import' in a 'compiles' context so that
    # template canImport(x): bool =
    #   compiles:
    #     import x
    #
    # works:
    if c.currentScope.depthLevel > 2 + c.compilesContextId:
      localReport(c.config, n, reportSem rsemImportRequiresToplevel)

    result = evalImport(c, n)
  of nkImportExceptStmt:
    if not isTopLevel(c): localReport(
      c.config, n, reportSem rsemImportRequiresToplevel)

    result = evalImportExcept(c, n)
  of nkFromStmt:
    if not isTopLevel(c): localReport(
      c.config, n, reportSem rsemImportRequiresToplevel)
    result = evalFrom(c, n)
  of nkIncludeStmt:
    #if not isTopLevel(c): localReport(c.config, n.info, errXOnlyAtModuleScope % "include")
    result = evalInclude(c, n)
  of nkExportStmt:
    if not isTopLevel(c): localReport(
      c.config, n, reportSem rsemExportRequiresToplevel)
    result = semExport(c, n)
  of nkExportExceptStmt:
    if not isTopLevel(c): localReport(
      c.config, n, reportSem rsemExportRequiresToplevel)
    result = semExportExcept(c, n)
  of nkPragmaBlock:
    result = semPragmaBlock(c, n)
  of nkStaticStmt:
    result = semStaticStmt(c, n)
  of nkDefer:
    if c.currentScope == c.topLevelScope:
      localReport(c.config, n, reportSem rsemUnexpectedToplevelDefer)
    n[0] = semExpr(c, n[0])
    if not n[0].typ.isEmptyType and not implicitlyDiscardable(n[0]):
      localReport(c.config, n, reportSem rsemExpectedTypelessDeferBody)
    #localReport(c.config, n.info, errGenerated, "'defer' not allowed in this context")
  of nkGotoState, nkState:
    if n.len != 1 and n.len != 2:
      semReportIllformedAst(c.config, n, "")

    for i in 0..<n.len:
      n[i] = semExpr(c, n[i])
  of nkMixinStmt: discard
  of nkBindStmt:
    if c.p != nil:
      if n.len > 0 and n[0].kind == nkSym:
        c.p.localBindStmts.add n
    else:
      localReport(c.config, n, reportSem rsemInvalidBindContext)
  of nkError:
    discard "ignore errors for now"
  else:
    result = c.config.newError(n, reportSem rsemInvalidExpression)

  if result != nil:
    incl(result.flags, nfSem)
