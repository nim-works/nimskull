#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## this module does the semantic checking of type declarations
## included from sem.nim

proc newOrPrevType(kind: TTypeKind, prev: PType, c: PContext): PType =
  if prev == nil:
    result = newTypeS(kind, c)
  else:
    result = prev
    if result.kind == tyForward: result.kind = kind
  #if kind == tyError: result.flags.incl tfCheckedForDestructor

proc newConstraint(c: PContext, k: TTypeKind): PType =
  result = newTypeS(tyBuiltInTypeClass, c)
  result.flags.incl tfCheckedForDestructor
  result.addSonSkipIntLit(newTypeS(k, c), c.idgen)

proc semEnum(c: PContext, n: PNode, prev: PType): PType =
  if n.len == 0: return newConstraint(c, tyEnum)
  elif n.len == 1:
    # don't create an empty tyEnum; fixes #3052
    return errorType(c)
  var
    counter, x: BiggestInt
    e: PSym
    base: PType
    identToReplace: ptr PNode
  counter = 0
  base = nil
  result = newOrPrevType(tyEnum, prev, c)
  result.n = newNodeI(nkEnumTy, n.info)
  checkMinSonsLen(n, 1, c.config)
  if n[0].kind != nkEmpty:
    base = semTypeNode(c, n[0][0], nil)
    if base.kind != tyEnum:
      localReport(c.config, n[0], reportSem(rsemInheritanceOnlyWorksWithAnEnum))

    counter = toInt64(lastOrd(c.config, base)) + 1
  rawAddSon(result, base)
  let isPure = result.sym != nil and sfPure in result.sym.flags
  var symbols: TStrTable
  if isPure: initStrTable(symbols)
  var hasNull = false
  for i in 1..<n.len:
    if n[i].kind == nkEmpty: continue
    case n[i].kind
    of nkEnumFieldDef:
      if n[i][0].kind == nkPragmaExpr:
        e = newSymS(skEnumField, n[i][0][0], c)
        identToReplace = addr n[i][0][0]
        n[i][0][1] = pragmaDecl(c, e, n[i][0][1], enumFieldPragmas)
        # check if we got any errors and if so report them
        for e in ifErrorWalkErrors(c.config, n[i][0][1]):
          localReport(c.config, e)
      else:
        e = newSymS(skEnumField, n[i][0], c)
        identToReplace = addr n[i][0]

      var v = semConstExpr(c, n[i][1])
      var strVal: PNode = nil
      case skipTypes(v.typ, abstractInst-{tyTypeDesc}).kind
      of tyTuple:
        if v.len == 2:
          strVal = v[1] # second tuple part is the string value
          if skipTypes(strVal.typ, abstractInst).kind in {tyString, tyCstring}:
            if not isOrdinalType(v[0].typ, allowEnumWithHoles=true):
              localReport(c.config, v[0].info, reportAst(
                rsemExpectedOrdinal, v[0], typ = v[0].typ))

            x = toInt64(getOrdValue(v[0])) # first tuple part is the ordinal
            n[i][1][0] = newIntTypeNode(x, getSysType(c.graph, unknownLineInfo, tyInt))
          else:
            localReport(c.config, strVal, reportSem(rsemStringLiteralExpected))
        else:
          localReport(c.config, v, reportSem(rsemWrongNumberOfVariables))
      of tyString, tyCstring:
        strVal = v
        x = counter
      else:
        if not isOrdinalType(v.typ, allowEnumWithHoles=true):
          localReport(c.config, v.info, reportAst(
            rsemExpectedOrdinal, v, typ = v.typ))

        x = toInt64(getOrdValue(v))
        n[i][1] = newIntTypeNode(x, getSysType(c.graph, unknownLineInfo, tyInt))
      if i != 1:
        if x != counter: incl(result.flags, tfEnumHasHoles)
        if x < counter:
          localReport(c.config, v.info, SemReport(
            kind: rsemInvalidOrderInEnum,
            sym: e,
            expectedCount: toInt128 counter,
            got: toInt128 x))

          x = counter
      e.ast = strVal # might be nil
      counter = x
    of nkSym:
      e = n[i].sym
    of nkIdent, nkAccQuoted:
      e = newSymS(skEnumField, n[i], c)
      identToReplace = addr n[i]
    of nkPragmaExpr:
      e = newSymS(skEnumField, n[i][0], c)
      n[i][1] = pragmaDecl(c, e, n[i][1], enumFieldPragmas)
      # check if we got any errors and if so report them
      for e in ifErrorWalkErrors(c.config, n[i][1]):
        localReport(c.config, e)

      identToReplace = addr n[i][0]
    else:
      c.config.localReport(n[i].info, reportAst(
        rsemIllformedAst, n[i],
        str = "Unexpected node kind for enum field definition - wanted " &
          "enumFielDef, sym, ident, accQuoted or pragmaExpr, but found " &
          $n[i].kind))

    e.typ = result
    e.position = int(counter)
    let symNode = newSymNode(e)
    if identToReplace != nil and
        c.config.cmd notin cmdDocLike: # A hack to produce documentation for enum fields.
      identToReplace[] = symNode
    if e.position == 0: hasNull = true
    if result.sym != nil and sfExported in result.sym.flags:
      e.flags.incl {sfUsed, sfExported}

    result.n.add symNode
    styleCheckDef(c.config, e)
    if sfGenSym notin e.flags:
      if not isPure:
        if overloadableEnums in c.features:
          addInterfaceOverloadableSymAt(c, c.currentScope, e)
        else:
          addInterfaceDecl(c, e)
      else:
        declarePureEnumField(c, e)
    if isPure and (let conflict = strTableInclReportConflict(symbols, e); conflict != nil):
      wrongRedefinition(c, e.info, e, conflict)

    inc(counter)
  if isPure and sfExported in result.sym.flags:
    addPureEnum(c, LazySym(sym: result.sym))

  if tfNotNil in e.typ.flags and not hasNull:
    result.flags.incl tfRequiresInit

  setToStringProc(c.graph, result, genEnumToStrProc(result, n.info, c.graph, c.idgen))

template semReportParamCountMismatch(
    config: ConfigRef, node: PNode, semtype: PType,
    expected, got: int,
    typename: string = ""
  ): untyped =

  var report = semReportCountMismatch(
    rsemWrongNumberOfGenericParams, expected, got)
  report.ast = node
  report.typ = semtype
  report.str = typename

  localReport(config, node.info, report)

proc semContainerArg(c: PContext; n: PNode, typ: PType; len = 2): PNode

proc semSet(c: PContext, n: PNode, prev: PType): PNode =
  let typ = newOrPrevType(tySet, prev, c)
  result = semContainerArg(c, n, typ)

  if result.kind != nkError:
    # TODO: the skipping and kind check looks very similar to the one in
    #       ``semArray``...
    var base = typ[0]
    if base.kind in {tyGenericInst, tyAlias, tySink}: base = lastSon(base)
    if base.kind notin {tyGenericParam, tyGenericInvocation}:
      if not isOrdinalType(base, allowEnumWithHoles = true):
        localReport(c.config, n.info, reportAst(
          rsemExpectedOrdinal, n, typ = base))
      elif lengthOrd(c.config, base) > MaxSetElements:
        localReport(c.config, n.info, SemReport(
          kind: rsemSetTooBig,
          expectedCount: toInt128 MaxSetElements,
          got: lengthOrd(c.config, base)))

proc semContainerArg(c: PContext; n: PNode, typ: PType; len = 2): PNode =
  if n.len == len:
    result = shallowCopy(n)
    result[0] = n[0]
    result[1] = semTypeNode2(c, n[1], nil)

    let base = result[1].typ
    if base.kind == tyVoid:
      # XXX: the error message looks off. ``tyAnything`` is also not a concrete
      #      type, but it's not rejected here
      localReport(c.config, n.info, reportTyp(
        rsemTIsNotAConcreteType, base))

    addSonSkipIntLit(typ, base, c.idgen)
    result.typ = typ
  else:
    c.config.semReportParamCountMismatch(n, typ, 1, n.len - 1)
    # TODO: diagnostic
    # addSonSkipIntLit(result, errorType(c), c.idgen)

proc semContainer(c: PContext, n: PNode, kind: TTypeKind, kindStr: string,
                  prev: PType): PNode =
  let typ = newOrPrevType(kind, prev, c)
  result = semContainerArg(c, n, typ)

proc collapse(x: PIdentResult, info: TLineInfo): PNode =
  if x.errNode != nil:
    result = x.errNode
  else:
    result = newIdentNode(x.ident, info)

proc semVarargs(c: PContext, n: PNode, prev: PType): PNode =
  let typ = newOrPrevType(tyVarargs, prev, c)
  if n.len == 2 or n.len == 3:
    result = semContainerArg(c, n, typ, n.len)
    if n.len == 3:
      result[2] = collapse(considerQuotedIdent(c, n[2]), n[2].info)
      typ.n = result[2]
  else:
    c.config.semReportParamCountMismatch(n, typ, 1, n.len - 1)
    # TODO: diagnostic
    # addSonSkipIntLit(result, errorType(c), c.idgen)

proc semVarOutType(c: PContext, n: PNode, prev: PType; kind: TTypeKind): PNode =
  case n.len
  of 1:
    result = shallowCopy(n)
    result[0] = semTypeNode2(c, n[0], nil)

    let typ = newOrPrevType(kind, prev, c)
    # TODO: skipping the typeDesc looks wrong. Only explicit ones should reach
    #       here, and those must not be skipped
    var base = result[0].typ.skipTypes({tyTypeDesc})
    if base.kind in {tyVar, tyLent}:
      localReport(c.config, n, reportSem(rsemVarVarNotAllowed))
      base = base[0]
    addSonSkipIntLit(typ, base, c.idgen)
    result.typ = typ
  of 0:
    result = copyNode(n)
    result.typ = newConstraint(c, kind)
  else:
    # TODO: diagnostic
    discard

proc semDistinct(c: PContext, n: PNode, prev: PType): PNode =
  if n.len == 0:
    result = copyNode(n)
    result.typ = newConstraint(c, tyDistinct)
    return

  let typ = newOrPrevType(tyDistinct, prev, c)
  result = newTreeI(nkDistinctTy, n.info, [semTypeNode2(c, n[0], nil)])
  result.typ = typ

  addSonSkipIntLit(typ, result[0].typ, c.idgen)
  if n.len > 1: typ.n = n[1]

proc semRangeAux(c: PContext, n: PNode, prev: PType): PNode =
  assert isRange(n)
  checkSonsLen(n, 3, c.config)
  # always create a 'valid' range type, but overwrite it later
  # because 'semExprWithType' can raise an exception. See bug #6895.
  # TODO: investigate
  #addSonSkipIntLit(result, errorType(c), c.idgen)

  if (n[1].kind == nkEmpty) or (n[2].kind == nkEmpty):
    # TODO: diagnostic
    return c.config.newError(n, PAstDiag(kind: adWrappedError))

  # XXX: consider using ``semGenericStmt`` instead
  var range: array[2, PNode]
  range[0] = semExprWithType(c, n[1])
  range[1] = semExprWithType(c, n[2])

  # the result node stores the unevaluated expressions
  result = newTreeI(nkRange, n.info, [range[0], range[1]])

  var rangeT: array[2, PType]
  for i in 0..1:
    rangeT[i] = range[i].typ.skipTypes({tyStatic}).skipIntLit(c.idgen)

  let hasUnknownTypes = c.inGenericContext > 0 and
    rangeT[0].kind == tyFromExpr or rangeT[1].kind == tyFromExpr

  if not hasUnknownTypes:
    if not sameType(rangeT[0].skipTypes({tyRange}), rangeT[1].skipTypes({tyRange})):
      # XXX: should this cascade and what about the follow-on statements like
      #      the for loop, etc below?
      let r = typeMismatch(c.config, n.info, rangeT[0], rangeT[1], n)
      if r.kind == nkError:
        localReport(c.config, r)

    elif not isOrdinalType(rangeT[0]) and rangeT[0].kind notin {tyFloat..tyFloat128} or
        rangeT[0].kind == tyBool:

      localReport(c.config, n.info, reportTyp(
        rsemExpectedOrdinalOrFloat, rangeT[0]))

    elif enumHasHoles(rangeT[0]):
      localReport(c.config, n.info, reportTyp(rsemExpectedUnholyEnum, rangeT[0]))

  let typ = newOrPrevType(tyRange, prev, c)
  typ.n = newNodeI(nkRange, n.info)
  for i in 0..1:
    if hasUnresolvedArgs(c, range[i]):
      typ.n.add makeStaticExpr(c, range[i])
      typ.flags.incl tfUnresolved
    else:
      typ.n.add evalConstExpr(c, range[i])

    if typ.n[i].kind in nkFloatLiterals and typ.n[i].floatVal.isNaN:
      localReport(c.config, range[i], reportSem rsemRangeDoesNotSupportNan)

  if weakLeValue(typ.n[0], typ.n[1]) == impNo:
    localReport(c.config, n, reportSem rsemRangeIsEmpty)

  addSonSkipIntLit(typ, rangeT[0], c.idgen)
  result.typ = typ

proc semRange(c: PContext, n: PNode, prev: PType): PNode =
  if n.len == 2:
    if isRange(n[1]):
      result = semRangeAux(c, n[1], prev)
      let n = result
      if n[0].kind in {nkCharLit..nkUInt64Lit} and n[0].intVal > 0:
        incl(result.typ.flags, tfRequiresInit)
      elif n[1].kind in {nkCharLit..nkUInt64Lit} and n[1].intVal < 0:
        incl(result.typ.flags, tfRequiresInit)
      elif n[0].kind in {nkFloatLit..nkFloat64Lit} and
          n[0].floatVal > 0.0:
        incl(result.typ.flags, tfRequiresInit)
      elif n[1].kind in {nkFloatLit..nkFloat64Lit} and
          n[1].floatVal < 0.0:
        incl(result.typ.flags, tfRequiresInit)
    else:
      if n[1].kind == nkInfix and
         legacyConsiderQuotedIdent(c, n[1][0], nil).s == "..<":
        localReport(c.config, n[0], reportSem rsemRangeRequiresDotDot)
      else:
        localReport(c.config, n[0], reportSem rsemExpectedRange)

      # TODO: diagnostic
      result = c.config.newError(n, PAstDiag(kind: adWrappedError))
  else:
    c.config.semReportParamCountMismatch(n, nil, 1, n.len - 1, "range")
    # TODO: diagnostic
    result = c.config.newError(n, PAstDiag(kind: adWrappedError))
    # result = newOrPrevType(tyError, prev, c)

proc semArrayIndex(c: PContext, n: PNode): PNode =
  if isRange(n):
    result = semRangeAux(c, n, nil)
  else:
    # XXX: consider using ``semGenericStmt`` instead. Doing so could the shrink
    #      the logic here a bit, and also prevent unresolved type variables
    #      from reaching into normal semantic analysis
    let e = semExprWithType(c, n)
    result = e
    if e.typ.kind == tyFromExpr:
      assert e != e.typ.n
      result.typ = makeRangeWithStaticExpr(c, e.typ.n)
    elif e.kind in {nkIntLit..nkUInt64Lit}:
      if e.intVal < 0:
        localReport(c.config, n.info,
          SemReport(
            kind: rsemArrayExpectsPositiveRange,
            expectedCount: toInt128 0,
            got: toInt128 e.intVal))

      result.typ = makeRangeType(c, 0, e.intVal-1, n.info, e.typ)
    elif e.kind == nkSym and e.typ.kind == tyStatic:
      if e.sym.ast != nil:
        return semArrayIndex(c, e.sym.ast)
      if not isOrdinalType(e.typ.lastSon):
        let info = if n.safeLen > 1: n[1].info else: n.info
        localReport(c.config, info, reportTyp(
          rsemExpectedOrdinal, e.typ.lastSon))

      result.typ = makeRangeWithStaticExpr(c, copyTree(e))
      if c.inGenericContext > 0:
        result.typ.flags.incl tfUnresolved

    elif e.kind in (nkCallKinds + {nkBracketExpr}) and hasUnresolvedArgs(c, e):
      if not isOrdinalType(e.typ.skipTypes({tyStatic, tyAlias, tyGenericInst, tySink})):
        localReport(c.config, n[1].info, reportTyp(
          rsemExpectedOrdinal, e.typ))
      # This is an int returning call, depending on an
      # yet unknown generic param (see tgenericshardcases).
      # We are going to construct a range type that will be
      # properly filled-out in semtypinst (see how tyStaticExpr
      # is handled there).
      result.typ = makeRangeWithStaticExpr(c, copyTree(e))
    elif e.kind == nkIdent:
      # this is an unresolved generic parameter
      result.typ = e.typ.skipTypes({tyTypeDesc})
    else:
      # TODO: use ``evalConstExpr`` instead
      let x = evalConstExpr(c, e)
      case x.kind
      of nkError:
        result = x
      of nkIntLit..nkUInt64Lit:
        result.typ = makeRangeType(c, 0, x.intVal-1, n.info,
                             result.typ.skipTypes({tyTypeDesc}))
      else:
        # illegal type errors are reported by the callsite
        result.typ = x.typ.skipTypes({tyTypeDesc})
        #localReport(c.config, n[1].info, errConstExprExpected)

proc semArray(c: PContext, n: PNode, prev: PType): PNode =
  if n.len == 3:
    # 3 = length(array indx base)
    result = shallowCopy(n)
    result[0] = n[0]
    result[1] = semArrayIndex(c, n[1])

    var indxB = result[1].typ
    if indxB.kind in {tyGenericInst, tyAlias, tySink}: indxB = lastSon(indxB)
    if indxB.kind notin {tyGenericParam, tyStatic, tyFromExpr}:
      if indxB.skipTypes({tyRange}).kind in {tyUInt, tyUInt64}:
        discard
      elif not isOrdinalType(indxB):
        localReport(c.config, n[1].info, reportTyp(
          rsemExpectedOrdinal, indxB))

      elif enumHasHoles(indxB):
        localReport(c.config, n[1].info, reportTyp(
          rsemExpectedUnholyEnum, indxB.skipTypes({tyRange})))

    result[2] = semTypeNode2(c, n[2], nil)
    # ensure we only construct a tyArray when there was no error (bug #3048):
    let typ = newOrPrevType(tyArray, prev, c)
    # bug #6682: Do not propagate initialization requirements etc for the
    # index type:
    rawAddSonNoPropagationOfTypeFlags(typ, result[1].typ)
    addSonSkipIntLit(typ, result[2].typ, c.idgen)
    result.typ = typ
  else:
    semReportParamCountMismatch(c.config, n, prev, 2, n.len - 1, "array")
    # TODO: diagnostic
    # result = newOrPrevType(tyError, prev, c)

proc semOrdinal(c: PContext, n: PNode, prev: PType): PNode =
  let typ = newOrPrevType(tyOrdinal, prev, c)
  result = semContainerArg(c, n, typ)

  let base = result[1].typ
  # TODO: this should very likely match the check in ``semArray``
  if base.kind != tyGenericParam:
    if not isOrdinalType(base):
      localReport(c.config, n[1].info, reportTyp(
        rsemExpectedOrdinal, base))

proc semTypeIdent(c: PContext, n: PNode): PSym =
  if n.kind == nkSym:
    result = getGenSym(c, n.sym)
  else:
    result = pickSym(c, n, {skType, skGenericParam, skParam})
    if result.isNil:
      result = qualifiedLookUp(c, n, {checkAmbiguity, checkUndeclared})
    if result.isError:
      markUsed(c, n.info, result)

      # XXX: move to propagating nkError, skError, and tyError
      localReport(c.config, result.ast)
    elif result != nil:
      markUsed(c, n.info, result)

      if result.kind == skParam and result.typ.kind == tyTypeDesc:
        # This is a typedesc param. is it already bound?
        # it's not bound when it's used multiple times in the
        # proc signature for example
        if c.inGenericInst > 0:
          let bound = result.typ[0].sym
          if bound != nil: return bound
          return result
        if result.typ.sym == nil:
          let err = newError(c.config, n, PAstDiag(kind: adSemTypeExpected))
          localReport(c.config, err)

          return errorSym(c, n, err)
        result = result.typ.sym.copySym(nextSymId c.idgen)
        result.typ = exactReplica(result.typ)
        result.typ.flags.incl tfUnresolved

      if result.kind == skGenericParam:
        if result.typ.kind == tyGenericParam and result.typ.len == 0 and
           tfWildcard in result.typ.flags:
          # collapse the wild-card param to a type
          result.transitionGenericParamToType()
          result.typ.flags.excl tfWildcard
          return
        else:
          let err = newError(c.config, n, PAstDiag(kind: adSemTypeExpected))
          localReport(c.config, err)

          return errorSym(c, n, err)
      if result.kind != skType and result.magic notin {mStatic, mType, mTypeOf}:
        # this implements the wanted ``var v: V, x: V`` feature ...
        var ov: TOverloadIter
        var amb = initOverloadIter(ov, c, n)
        while amb != nil:
          if amb.isError:
            localReport(c.config, amb.ast)
          if amb.kind != skType:
            amb = nextOverloadIter(ov, c, n)
        if amb != nil: result = amb
        else:
          let err = newError(c.config, n, PAstDiag(kind: adSemTypeExpected))
          if result.kind != skError:
            localReport(c.config, err)

          return errorSym(c, n, err)
      if result.typ.kind != tyGenericParam:
        # XXX get rid of this hack!
        var oldInfo = n.info
        let oldId = n.id
        reset(n[])
        n.id = oldId
        n.transitionNoneToSym()
        n.sym = result
        n.info = oldInfo
        n.typ = result.typ
    else:
      let err = newError(c.config, n, PAstDiag(kind: adSemExpectedIdentifier))
      localReport(c.config, err)
      result = errorSym(c, n, err)

proc semAnonTuple(c: PContext, n: PNode, prev: PType): PNode =
  if n.len == 0:
    # XXX: remove this check once macro output is sanitized
    localReport(c.config, n, reportSem rsemTypeExpected)

  result = shallowCopy(n)
  result.typ = newOrPrevType(tyTuple, prev, c)
  for i, it in n.pairs:
    result[i] = semTypeNode2(c, it, nil)
    addSonSkipIntLit(result.typ, result[i].typ, c.idgen)

proc semTuple(c: PContext, n: PNode, prev: PType): PNode =
  addInNimDebugUtils(c.config, "semTuple", n, prev, result.typ)
  var tup = newOrPrevType(tyTuple, prev, c)
  tup.n = newNodeI(nkRecList, n.info)
  var
    check = initIntSet()
    counter = 0

  result = copyNode(n) # inherit flags and line information
  # we normalize the AST to always be a ``nkTupleTy``
  case n.kind
  of nkBracketExpr:
    result.transitionSonsKind(nkTupleTy)
    result.sons.setLen(n.len - 1)
  of nkTupleTy:
    result.sons.setLen(n.len)
  else:
    unreachable()

  for i in ord(n.kind == nkBracketExpr)..<n.len:
    var
      a = n[i]
      typ: PType

    if a.kind != nkIdentDefs:
      c.config.globalReport(reportAst(
        rsemIllformedAst, a,
        str = "Expected identDefs for node, but found " & $a.kind))

    var item = shallowCopy(a)

    checkMinSonsLen(a, 3, c.config)
    if a[^2].kind != nkEmpty:
      item[^2] = semTypeNode2(c, a[^2], nil)
      typ = item[^2].typ
    else:
      item[^2] = c.config.newError(a[^2], PAstDiag(kind: adSemTypeExpected))
      typ = errorType(c)

    if a[^1].kind != nkEmpty:
      # TODO: use a diagnostic
      discard
    else:
      item[^1] = a[^1]

    for j in 0 ..< a.len - 2:
      var field = newSymGNode(skField, a[j], c)
      # TODO: recover from recoverable errors
      if field.kind != nkError:
        # set up the symbol's information
        field.sym.typ = typ
        field.sym.position = counter

        if containsOrIncl(check, field.sym.name.id):
          # TODO: use a diagnostic
          localReport(c.config, a[j].info, reportSym(
            rsemRedefinitionOf, field.sym))

      inc(counter)

      item[j] = field
      if field.kind != nkError:
        # only add non-erroneous fields to the type
        tup.n.add newSymNode(field.sym)
        addSonSkipIntLit(tup, typ, c.idgen)

        styleCheckDef(c.config, a[j].info, field.sym)

    result[i - ord(n.kind == nkBracketExpr)] = item

  # canonicalization: zero-length tuples don't have a `n` field set
  if tup.n.len == 0:
    tup.n = nil

  result.typ = tup

  if isTupleRecursive(tup):
    # TODO: diagnostic
    localReport(c.config, n.info, reportTyp(
      rsemIllegalRecursion, tup))

proc semIdentVis(c: PContext, kind: TSymKind, n: PNode,
                 allowed: TSymFlags): PSym =
  # identifier with visibility
  if n.kind == nkPostfix:
    if n.len == 2:
      # for gensym'ed identifiers the identifier may already have been
      # transformed to a symbol and we need to use that here:
      result = newSymG(kind, n[1], c)
      var (v, err) = considerQuotedIdent(c, n[0])
      if err != nil:
        localReport(c.config, err)
      if sfExported in allowed and v.id == ord(wStar):
        incl(result.flags, sfExported)
      else:
        if not (sfExported in allowed):
          localReport(c.config, n[0], reportSem rsemExportRequiresToplevel)
        else:
          localReport(c.config, n[0], reportSem rsemInvalidVisibility)
    else:
      c.config.semReportIllformedAst(
        n, "Expected two nodes for postfix expression, but found " & $n.len)

  else:
    result = newSymG(kind, n, c)

proc semIdentWithPragma(c: PContext, kind: TSymKind, n: PNode,
                        allowed: TSymFlags): PSym =
  addInNimDebugUtils(c.config, "semIdentWithPragma", n, result)
  if n.kind == nkPragmaExpr:
    checkSonsLen(n, 2, c.config)
    result = semIdentVis(c, kind, n[0], allowed)
    case kind
    of skType:
      # process pragmas later, because result.typ has not been set yet
      discard
    of skField: n[1] = pragmaDecl(c, result, n[1], fieldPragmas)
    of skVar:   n[1] = pragmaDecl(c, result, n[1], varPragmas)
    of skLet:   n[1] = pragmaDecl(c, result, n[1], letPragmas)
    of skConst: n[1] = pragmaDecl(c, result, n[1], constPragmas)
    else: discard
    # check if we got any errors and if so report them
    for e in ifErrorWalkErrors(c.config, n[1]):
      localReport(c.config, e)

  else:
    result = semIdentVis(c, kind, n, allowed)

proc checkForOverlap(c: PContext, t: PNode, currentEx, branchIndex: int) =
  let ex = t[branchIndex][currentEx].skipConv
  for i in 1..branchIndex:
    for j in 0..<t[i].len - 1:
      if i == branchIndex and j == currentEx: break
      if overlap(t[i][j].skipConv, ex):
        localReport(c.config, ex.info, SemReport(
          kind: rsemDuplicateCaseLabel,
          ast: ex,
          overlappingGroup: t[i][j].skipConv))

proc semBranchRange(c: PContext, t, a, b: PNode, covered: var Int128): PNode =
  checkMinSonsLen(t, 1, c.config)
  let ac = semConstExpr(c, a)
  let bc = semConstExpr(c, b)
  let at = fitNode(c, t[0].typ, ac, ac.info).skipConvTakeType
  let bt = fitNode(c, t[0].typ, bc, bc.info).skipConvTakeType

  result = newNodeI(nkRange, a.info)
  result.add(at)
  result.add(bt)
  if emptyRange(ac, bc):
    localReport(c.config, b, reportSem rsemRangeIsEmpty)
  else: covered = covered + getOrdValue(bc) + 1 - getOrdValue(ac)

proc semCaseBranchRange(c: PContext, t, b: PNode,
                        covered: var Int128): PNode =
  checkSonsLen(b, 3, c.config)
  result = semBranchRange(c, t, b[1], b[2], covered)

proc semCaseBranchSetElem(c: PContext, t, b: PNode,
                          covered: var Int128): PNode =
  if isRange(b):
    checkSonsLen(b, 3, c.config)
    result = semBranchRange(c, t, b[1], b[2], covered)
  elif b.kind == nkRange:
    checkSonsLen(b, 2, c.config)
    result = semBranchRange(c, t, b[0], b[1], covered)
  else:
    result = fitNode(c, t[0].typ, b, b.info)
    inc(covered)

proc semCaseBranch(c: PContext, t, branch: PNode, branchIndex: int,
                   covered: var Int128) =
  let lastIndex = branch.len - 2
  for i in 0..lastIndex:
    var b = branch[i]
    if b.kind == nkRange:
      branch[i] = b
    elif isRange(b):
      branch[i] = semCaseBranchRange(c, t, b, covered)
    else:
      # constant sets and arrays are allowed:
      var r = semConstExpr(c, b)
      if r.kind in {nkCurly, nkBracket} and r.len == 0 and branch.len == 2:
        # discarding ``{}`` and ``[]`` branches silently
        delSon(branch, 0)
        return
      elif r.kind notin {nkCurly, nkBracket} or r.len == 0:
        checkMinSonsLen(t, 1, c.config)
        var tmp = fitNode(c, t[0].typ, r, r.info)
        # the call to fitNode may introduce a call to a converter
        if tmp.kind in {nkHiddenCallConv}: tmp = semConstExpr(c, tmp)
        branch[i] = skipConv(tmp)
        inc(covered)
      else:
        if r.kind == nkCurly:
          r = deduplicate(c.config, r)

        # first element is special and will overwrite: branch[i]:
        branch[i] = semCaseBranchSetElem(c, t, r[0], covered)

        # other elements have to be added to ``branch``
        for j in 1..<r.len:
          branch.add(semCaseBranchSetElem(c, t, r[j], covered))
          # caution! last son of branch must be the actions to execute:
          swap(branch[^2], branch[^1])
    checkForOverlap(c, t, i, branchIndex)

  # Elements added above needs to be checked for overlaps.
  for i in lastIndex.succ..<branch.len - 1:
    checkForOverlap(c, t, i, branchIndex)

proc toCover(c: PContext, t: PType): Int128 =
  let t2 = skipTypes(t, abstractVarRange-{tyTypeDesc})
  if t2.kind == tyEnum and enumHasHoles(t2):
    result = toInt128(t2.n.len)
  else:
    # <----
    let t = skipTypes(t, abstractVar-{tyTypeDesc})
    # XXX: hack incoming. lengthOrd is incorrect for 64bit integer
    # types because it doesn't uset Int128 yet.  This entire branching
    # should be removed as soon as lengthOrd uses int128.
    if t.kind in {tyInt64, tyUInt64}:
      result = toInt128(1) shl 64
    elif t.kind in {tyInt, tyUInt}:
      result = toInt128(1) shl (c.config.target.intSize * 8)
    else:
      result = lengthOrd(c.config, t)

proc semRecordNodeAux(c: PContext, n: PNode, check: var IntSet, pos: var int,
                      father: PNode, rectype: PType, hasCaseFields = false)

proc getIntSetOfType(c: PContext, t: PType): IntSet =
  result = initIntSet()
  if t.enumHasHoles:
    let t = t.skipTypes(abstractRange)
    for field in t.n.sons:
      result.incl(field.sym.position)
  else:
    assert(lengthOrd(c.config, t) <= BiggestInt(MaxSetElements))
    for i in toInt64(firstOrd(c.config, t))..toInt64(lastOrd(c.config, t)):
      result.incl(i.int)

iterator processBranchVals(b: PNode): int =
  assert b.kind in {nkOfBranch, nkElifBranch, nkElse}
  if b.kind == nkOfBranch:
    for i in 0..<b.len-1:
      if b[i].kind in {nkIntLit, nkCharLit}:
        yield b[i].intVal.int
      elif b[i].kind == nkRange:
        for i in b[i][0].intVal..b[i][1].intVal:
          yield i.int

proc toLiterals*(vals: IntSet, t: PType): seq[PNode] =
  let t = t.skipTypes(abstractRange)

  var enumSymOffset = 0
  for val in vals:
    case t.kind:
      of tyEnum, tyBool:
        while t.n[enumSymOffset].sym.position < val:
          inc(enumSymOffset)

        result &= t.n[enumSymOffset]

      of tyChar:
        result.add newIntNode(nkCharLit, BiggestInt(val))

      else:
        result.add newIntNode(nkIntLit, BiggestInt(val))


proc toEnumFields(vals: IntSet, t: PType): seq[PSym] =
  block:
    let t = t.skipTypes(abstractRange)
    assert(t.kind in {tyEnum, tyBool}, $t.kind)

  for node in toLiterals(vals, t):
    result.add node.sym

proc missingInts(c: PContext, n: PNode): IntSet =
  var coveredCases = initIntSet()
  for i in 1..<n.len:
    for val in processBranchVals(n[i]):
      coveredCases.incl val

  return c.getIntSetOfType(n[0].typ) - coveredCases


proc formatMissingBranches(c: PContext, n: PNode): seq[PNode] =
  toLiterals(missingInts(c, n) , n[0].typ)

proc formatMissingEnums(c: PContext, n: PNode): seq[PSym] =
  toEnumFields(missingInts(c, n) , n[0].typ)

proc semRecordCase(c: PContext, n: PNode, check: var IntSet, pos: var int,
                   father: PNode, rectype: PType) =
  var a = copyNode(n)
  checkMinSonsLen(n, 2, c.config)
  semRecordNodeAux(c, n[0], check, pos, a, rectype, hasCaseFields = true)
  c.config.internalAssert(a[0].kind == nkSym, "semRecordCase: discriminant is no symbol")
  incl(a[0].sym.flags, sfDiscriminant)
  var covered = toInt128(0)
  var chckCovered = false
  var typ = skipTypes(a[0].typ, abstractVar-{tyTypeDesc})
  const shouldChckCovered = {tyInt..tyInt64, tyChar, tyEnum, tyUInt..tyUInt32, tyBool}
  case typ.kind
  of shouldChckCovered:
    chckCovered = true
  of tyFloat..tyFloat128, tyError:
    discard
  of tyRange:
    if skipTypes(typ[0], abstractInst).kind in shouldChckCovered:
      chckCovered = true
  of tyForward:
    errorUndeclaredIdentifier(c, n[0].info, typ.sym.name.s)
  elif not isOrdinalType(typ):
    localReport(c.config, n[0].info, reportTyp(
      rsemExpectedOrdinalOrFloat, typ))

  if firstOrd(c.config, typ) != 0:
    var rep = SemReport(
      kind: rsemExpectedLow0Discriminant,
      # TODO: fix storage and actually report data, previously captured:
      #       - expected: toInt128(0),
      #       - got: firstOrd(c.config, typ)),
      typ: typ,
      sym: a[0].sym)

    localReport(c.config, n.info, rep)
  elif lengthOrd(c.config, typ) > 0x00007FFF:
    var rep = SemReport(
      kind: rsemExpectedHighCappedDiscriminant,
      # TODO: fix storage and actually report data, previously captured:
      #       - expected: toInt128(32768),
      #       - got: firstOrd(c.config, typ)),
      typ: typ,
      sym: a[0].sym)

  for i in 1..<n.len:
    var b = copyTree(n[i])
    a.add b
    case n[i].kind
    of nkOfBranch:
      checkMinSonsLen(b, 2, c.config)
      semCaseBranch(c, a, b, i, covered)
    of nkElse:
      checkSonsLen(b, 1, c.config)
      if chckCovered and covered == toCover(c, a[0].typ):
        localReport(c.config, b.info, SemReport(kind: rsemUnreachableElse))
      chckCovered = false
    else:
      semReportIllformedAst(
        c.config, n,
        "Expected ofBranch or else for object case statement, but found" &
          $n[i].kind)

    delSon(b, b.len - 1)
    semRecordNodeAux(c, lastSon(n[i]), check, pos, b, rectype, hasCaseFields = true)
  if chckCovered and covered != toCover(c, a[0].typ):
    if a[0].typ.skipTypes(abstractRange).kind == tyEnum:
      localReport(c.config, a.info, SemReport(
        kind: rsemMissingCaseBranches,
        nodes: formatMissingBranches(c, a)))

    else:
      localReport(c.config, a, reportSem rsemMissingCaseBranches)

  father.add a

proc semRecordNodeAux(c: PContext, n: PNode, check: var IntSet, pos: var int,
                      father: PNode, rectype: PType, hasCaseFields: bool) =
  if n == nil: return
  case n.kind
  of nkRecWhen:
    var branch: PNode = nil   # the branch to take
    for i in 0..<n.len:
      var it = n[i]
      if it == nil:
        semReportIllformedAst(c.config, n, "nil")

      var idx = 1
      case it.kind
      of nkElifBranch:
        checkSonsLen(it, 2, c.config)
        if c.inGenericContext == 0:
          var e = semConstBoolExpr(c, it[0])
          if e.kind != nkIntLit: discard "don't report followup error"
          elif e.intVal != 0 and branch == nil: branch = it[1]
        else:
          it[0] = forceBool(c, semExprWithType(c, it[0]))
      of nkElse:
        checkSonsLen(it, 1, c.config)
        if branch == nil: branch = it[0]
        idx = 0
      else:
        semReportIllformedAst(
          c.config, n, "Expected elifBranch of else, but found" & $it.kind)

      if c.inGenericContext > 0:
        # use a new check intset here for each branch:
        var newCheck: IntSet
        assign(newCheck, check)
        var newPos = pos
        var newf = newNodeI(nkRecList, n.info)
        semRecordNodeAux(c, it[idx], newCheck, newPos, newf, rectype, hasCaseFields)
        it[idx] = if newf.len == 1: newf[0] else: newf
    if c.inGenericContext > 0:
      father.add n
    elif branch != nil:
      semRecordNodeAux(c, branch, check, pos, father, rectype, hasCaseFields)
    elif father.kind in {nkElse, nkOfBranch}:
      father.add newNodeI(nkRecList, n.info)
  of nkRecCase:
    semRecordCase(c, n, check, pos, father, rectype)
  of nkNilLit:
    if father.kind != nkRecList: father.add newNodeI(nkRecList, n.info)
  of nkRecList:
    # attempt to keep the nesting at a sane level:
    var a = if father.kind == nkRecList: father else: copyNode(n)
    for i in 0..<n.len:
      semRecordNodeAux(c, n[i], check, pos, a, rectype, hasCaseFields)
    if a != father: father.add a
  of nkIdentDefs:
    checkMinSonsLen(n, 3, c.config)
    var a: PNode
    if father.kind != nkRecList and n.len >= 4: a = newNodeI(nkRecList, n.info)
    else: a = newNodeI(nkEmpty, n.info)
    if n[^1].kind != nkEmpty:
      localReport(c.config, n[^1], reportSem rsemInitHereNotAllowed)
    var typ: PType
    if n[^2].kind == nkEmpty:
      localReport(c.config, n, reportSem rsemTypeExpected)
      typ = errorType(c)
    else:
      typ = semTypeNode(c, n[^2], nil)
      propagateToOwner(rectype, typ)
    var fieldOwner = if c.inGenericContext > 0: c.getCurrOwner
                     else: rectype.sym
    for i in 0..<n.len-2:
      var f = semIdentWithPragma(c, skField, n[i], {sfExported})
      let info = getIdentLineInfo(n[i])
      suggestSym(c.graph, info, f, c.graph.usageSym)
      f.typ = typ
      f.position = pos
      f.options = c.config.options
      if fieldOwner != nil and
         {sfImportc, sfExportc} * fieldOwner.flags != {} and
         not hasCaseFields and f.loc.r == "":
        f.loc.r = rope(f.name.s)
        f.flags.incl {sfImportc, sfExportc} * fieldOwner.flags
      inc(pos)
      if containsOrIncl(check, f.name.id):
        localReport(c.config, info, reportSym(rsemRedefinitionOf, f))

      if a.kind == nkEmpty:
        father.add newSymNode(f)
      else:
        a.add newSymNode(f)

      styleCheckDef(c.config, f)
    if a.kind != nkEmpty: father.add a
  of nkSym:
    # This branch only valid during generic object
    # inherited from generic/partial specialized parent second check.
    # There is no branch validity check here
    if containsOrIncl(check, n.sym.name.id):
      localReport(c.config, n.info, reportSym(rsemRedefinitionOf, n.sym))

    father.add n
  of nkEmpty:
    if father.kind in {nkElse, nkOfBranch}:
      father.add n
  else:
    semReportIllformedAst(c.config, n, "?")

proc addInheritedFieldsAux(c: PContext, check: var IntSet, pos: var int,
                           n: PNode) =
  case n.kind
  of nkRecCase:
    c.config.internalAssert(n[0].kind == nkSym, n.info, "addInheritedFieldsAux")

    addInheritedFieldsAux(c, check, pos, n[0])
    for i in 1..<n.len:
      case n[i].kind
      of nkOfBranch, nkElse:
        addInheritedFieldsAux(c, check, pos, lastSon(n[i]))
      else:
        internalError(c.config, n.info, "addInheritedFieldsAux(record case branch)")

  of nkRecList, nkRecWhen, nkElifBranch, nkElse:
    for i in int(n.kind == nkElifBranch)..<n.len:
      addInheritedFieldsAux(c, check, pos, n[i])
  of nkSym:
    incl(check, n.sym.name.id)
    inc(pos)
  else:
    internalError(c.config, n.info, "addInheritedFieldsAux()")

proc skipGenericInvocation(t: PType): PType {.inline.} =
  result = t
  if result.kind == tyGenericInvocation:
    result = result[0]
  while result.kind in {tyGenericInst, tyGenericBody, tyRef, tyPtr, tyAlias, tySink}:
    result = lastSon(result)

proc addInheritedFields(c: PContext, check: var IntSet, pos: var int,
                        obj: PType) =
  assert obj.kind == tyObject
  if (obj.len > 0) and (obj[0] != nil):
    addInheritedFields(c, check, pos, obj[0].skipGenericInvocation)
  addInheritedFieldsAux(c, check, pos, obj.n)

proc semObjectNode(c: PContext, n: PNode, prev: PType; flags: TTypeFlags): PType =
  var check = initIntSet()
  var pos = 0
  var base, realBase: PType = nil
  # n[0] contains the pragmas (if any). We process these later...
  checkSonsLen(n, 3, c.config)
  if n[1].kind != nkEmpty:
    realBase = semTypeNode(c, n[1][0], nil)
    base = skipTypesOrNil(realBase, skipPtrs)
    if base.isNil:
      localReport(c.config, n, reportSem rsemExpectObjectForBase)
    else:
      var concreteBase = skipGenericInvocation(base)
      if concreteBase.kind in {tyObject, tyGenericParam,
        tyGenericInvocation} and tfFinal notin concreteBase.flags:
        # we only check fields duplication of object inherited from
        # concrete object. If inheriting from generic object or partial
        # specialized object, there will be second check after instantiation
        # located in semGeneric.
        if concreteBase.kind == tyObject:
          if concreteBase.sym != nil and concreteBase.sym.magic == mException and
              sfSystemModule notin c.module.flags:
            localReport(c.config, n.info, reportSem(rsemInheritFromException))

          addInheritedFields(c, check, pos, concreteBase)
      else:
        if concreteBase.kind != tyError:
          localReport(c.config, n[1].info, reportTyp(
            rsemExpectNonFinalForBase, realBase))

        base = nil
        realBase = nil
  c.config.internalAssert(n.kind == nkObjectTy, n.info, "semObjectNode")
  result = newOrPrevType(tyObject, prev, c)
  rawAddSon(result, realBase)
  if realBase == nil and tfInheritable in flags:
    result.flags.incl tfInheritable
  if tfAcyclic in flags: result.flags.incl tfAcyclic
  if result.n.isNil:
    result.n = newNodeI(nkRecList, n.info)
  else:
    # partial object so add things to the check
    addInheritedFields(c, check, pos, result)
  semRecordNodeAux(c, n[2], check, pos, result.n, result)
  if n[0].kind != nkEmpty:
    # dummy symbol for `pragma`:
    var s = newSymS(skType, newIdentNode(getIdent(c.cache, "dummy"), n.info), c)
    s.typ = result
    n[0] = pragmaDecl(c, s, n[0], typePragmas)
    # check if we got any errors and if so report them
    for e in ifErrorWalkErrors(c.config, n[0]):
      localReport(c.config, e)

  if base == nil and tfInheritable notin result.flags:
    incl(result.flags, tfFinal)

  if c.inGenericContext == 0 and computeRequiresInit(c, result):
    result.flags.incl tfRequiresInit

proc semObjectNode2(c: PContext, n: PNode, prev: PType; flags: TTypeFlags): PNode =
  ## An adapter routine for bridging from node-expecting code to
  ## ``semObjectNode``.
  if n.len == 0:
    result = copyNode(n)
    result.typ = newConstraint(c, tyObject)
    return

  result = newTreeIT(nkType, n.info, semObjectNode(c, n, prev, flags))

proc normalizeAnyRef(n: PNode, kind: TNodeKind): PNode =
  assert n.kind in nkCallKinds
  let op = n[1]
  if n.len == 3 and (
    (op.kind == nkIdent and op.ident.s == "sink") or
     op.kind == nkNilLit):
    # this is an expression like ``sink ref T`` (or ``ptr(sink T)``). Turn it
    # into ``sink(ref[T])``
    result = newTreeI(nkCall, n.info, [op, newTreeI(kind, n.info, n[2])])
  else:
    result = n

proc semAnyRef(c: PContext; n: PNode; kind: TTypeKind; prev: PType): PNode =
  if n.len == 0:
    result = copyNode(n)
    result.typ = newConstraint(c, kind)
  else:
    let isCall = int ord(n.kind in nkCallKinds+{nkBracketExpr})
    let n = if n[0].kind == nkBracket: n[0] else: n
    checkMinSonsLen(n, 1, c.config)
    let body = n.lastSon
    var t = if prev != nil and body.kind == nkObjectTy:
              semObjectNode(c, body, nil, prev.flags)
            else:
              semTypeNode(c, body, nil)
    if t.kind == tyTypeDesc and tfUnresolved notin t.flags:
      t = t.base

    let nkind =
      case kind
      of tyRef: nkRefTy
      of tyPtr: nkPtrTy
      else: unreachable(kind)
    result = newNodeI(nkind, n.info)

    if t.kind == tyVoid:
      localReport(c.config, n.info, reportTyp(
        rsemTVoidNotAllowed, t, str = kind.toHumanStr))

    let typ = newOrPrevType(kind, prev, c)
    # TODO: either undeprecate or remove the region feature for pointers and
    #       refs
    # check every except the last is an object:
    for i in isCall..<n.len-1:
      let
        ni = n[i]
        x = semTypeNode2(c, ni, nil)
        region = x.typ

      if region.skipTypes({tyGenericInst, tyAlias, tySink}).kind notin {
            tyError, tyObject}:
        localReport(c.config, ni, reportSem rsemExpectedObjectForRegion)
      else:
        localReport(c.config, n.info, reportSem(rsemPtrRegionIsDeprecated))

      add(result, x)
      addSonSkipIntLit(typ, region, c.idgen)

    addSonSkipIntLit(typ, t, c.idgen)

    if typ.kind == tyRef and c.config.selectedGC in {gcArc, gcOrc}:
      typ.flags.incl tfHasAsgn

    result.typ = typ

proc findEnforcedStaticType(t: PType): PType =
  # This handles types such as `static[T] and Foo`,
  # which are subset of `static[T]`, hence they could
  # be treated in the same way
  if t == nil: return nil
  if t.kind == tyStatic: return t
  if t.kind == tyAnd:
    for s in t.sons:
      let t = findEnforcedStaticType(s)
      if t != nil: return t

proc addParamOrResult(c: PContext, param: PSym) =
  if sfGenSym in param.flags:
    # bug #XXX, fix the gensym'ed parameters owner:
    if param.owner == nil:
      param.owner = getCurrOwner(c)
  else: addDecl(c, param)

template shouldHaveMeta(t) =
  c.config.internalAssert tfHasMeta in t.flags
  # result.lastSon.flags.incl tfHasMeta

proc addImplicitGeneric(c: PContext; typeClass: PType, typId: PIdent;
                        info: TLineInfo; genericParams: PNode;
                        paramName: string): PType =
  if genericParams == nil:
    # This happens with anonymous proc types appearing in signatures
    # XXX: we need to lift these earlier
    return
  let finalTypId = if typId != nil: typId
                    else: getIdent(c.cache, paramName & ":type")
  # is this a bindOnce type class already present in the param list?
  for i in 0..<genericParams.len:
    if genericParams[i].sym.name.id == finalTypId.id:
      return genericParams[i].typ

  let owner = if typeClass.sym != nil: typeClass.sym
              else: getCurrOwner(c)
  var s = newSym(skType, finalTypId, nextSymId c.idgen, owner, info)
  if sfExplain in owner.flags: s.flags.incl sfExplain
  if typId == nil: s.flags.incl(sfAnon)
  s.linkTo(typeClass)
  typeClass.flags.incl tfImplicitTypeParam
  s.position = genericParams.len
  genericParams.add newSymNode(s)
  result = typeClass
  addDecl(c, s)

proc liftParamType(c: PContext, procKind: TSymKind, genericParams: PNode,
                   paramType: PType, paramName: string,
                   info: TLineInfo, anon = false): PType =
  if paramType == nil: return # (e.g. proc return type)

  template recurse(typ: PType, anonFlag = false): untyped =
    liftParamType(c, procKind, genericParams, typ, paramName, info, anonFlag)

  var paramTypId = if not anon and paramType.sym != nil: paramType.sym.name
                   else: nil

  case paramType.kind
  of tyAnything:
    result = addImplicitGeneric(c, newTypeS(tyGenericParam, c), nil, info, genericParams, paramName)

  of tyStatic:
    if paramType.base.kind != tyNone and paramType.n != nil:
      # this is a concrete static value
      return
    if tfUnresolved in paramType.flags: return # already lifted

    let lifted = recurse(paramType.base)
    let base = (if lifted != nil: lifted else: paramType.base)
    if base.isMetaType and procKind == skMacro:
      localReport(c.config, info, reportStr(
        rsemMacroBodyDependsOnGenericTypes, paramName, typ = paramType))

    result = addImplicitGeneric(c, c.newTypeWithSons(tyStatic, @[base]),
        paramTypId, info, genericParams, paramName)
    if result != nil: result.flags.incl({tfHasStatic, tfUnresolved})

  of tyTypeDesc:
    if tfUnresolved notin paramType.flags:
      # naked typedescs are not bindOnce types
      if paramType.base.kind == tyNone and paramTypId != nil and
          (paramTypId.id == getIdent(c.cache, "typedesc").id or
          paramTypId.id == getIdent(c.cache, "type").id):
        # XXX Why doesn't this check for tyTypeDesc instead?
        paramTypId = nil
      let t = c.newTypeWithSons(tyTypeDesc, @[paramType.base])
      incl t.flags, tfCheckedForDestructor
      result = addImplicitGeneric(c, t, paramTypId, info, genericParams, paramName)

  of tyDistinct:
    if paramType.len == 1:
      # disable the bindOnce behavior for the type class
      result = recurse(paramType.base, true)

  of tyTuple:
    for i in 0..<paramType.len:
      let t = recurse(paramType[i])
      if t != nil:
        paramType[i] = t
        result = paramType

  of tyAlias, tySink:
    result = recurse(paramType.base)

  of tySequence, tySet, tyArray, tyOpenArray,
     tyVar, tyLent, tyPtr, tyRef, tyProc:
    # XXX: this is a bit strange, but proc(s: seq)
    # produces tySequence(tyGenericParam, tyNone).
    # This also seems to be true when creating aliases
    # like: type myseq = distinct seq.
    # Maybe there is another better place to associate
    # the seq type class with the seq identifier.
    if paramType.kind == tySequence and paramType.lastSon.kind == tyNone:
      let typ = c.newTypeWithSons(tyBuiltInTypeClass,
                                  @[newTypeS(paramType.kind, c)])
      result = addImplicitGeneric(c, typ, paramTypId, info, genericParams, paramName)
    else:
      for i in 0..<paramType.len:
        if paramType[i] == paramType:
          globalReport(c.config, info, reportTyp(rsemIllegalRecursion, paramType))

        var lifted = recurse(paramType[i])
        if lifted != nil:
          paramType[i] = lifted
          result = paramType

  of tyGenericBody:
    result = newTypeS(tyGenericInvocation, c)
    result.rawAddSon(paramType)

    for i in 0..<paramType.len - 1:
      if paramType[i].kind == tyStatic:
        var staticCopy = paramType[i].exactReplica
        staticCopy.flags.incl tfInferrableStatic
        result.rawAddSon staticCopy
      else:
        result.rawAddSon newTypeS(tyAnything, c)

    if paramType.lastSon.kind == tyUserTypeClass:
      result.kind = tyUserTypeClassInst
      result.rawAddSon paramType.lastSon
      return addImplicitGeneric(c, result, paramTypId, info, genericParams, paramName)

    let x = instGenericContainer(c, paramType.sym.info, result,
                                  allowMetaTypes = true)
    result = newTypeWithSons(c, tyCompositeTypeClass, @[paramType, x])
    #result = newTypeS(tyCompositeTypeClass, c)
    #for i in 0..<x.len: result.rawAddSon(x[i])
    result = addImplicitGeneric(c, result, paramTypId, info, genericParams, paramName)

  of tyGenericInst:
    if paramType.lastSon.kind == tyUserTypeClass:
      var cp = copyType(paramType, nextTypeId c.idgen, getCurrOwner(c))
      copyTypeProps(c.graph, c.idgen.module, cp, paramType)

      cp.kind = tyUserTypeClassInst
      return addImplicitGeneric(c, cp, paramTypId, info, genericParams, paramName)

    for i in 1..<paramType.len-1:
      var lifted = recurse(paramType[i])
      if lifted != nil:
        paramType[i] = lifted
        result = paramType
        result.lastSon.shouldHaveMeta

    let liftBody = recurse(paramType.lastSon, true)
    if liftBody != nil:
      result = liftBody
      result.flags.incl tfHasMeta
      #result.shouldHaveMeta

  of tyGenericInvocation:
    for i in 1..<paramType.len:
      #if paramType[i].kind != tyTypeDesc:
      let lifted = recurse(paramType[i])
      if lifted != nil: paramType[i] = lifted

    let body = paramType.base
    if body.kind in {tyForward, tyError}:
      # this may happen for proc type appearing in a type section
      # before one of its param types
      return

    if body.lastSon.kind == tyUserTypeClass:
      let expanded = instGenericContainer(c, info, paramType,
                                          allowMetaTypes = true)
      result = recurse(expanded, true)

  of tyUserTypeClasses, tyBuiltInTypeClass, tyCompositeTypeClass,
     tyAnd, tyOr, tyNot:
    result = addImplicitGeneric(c,
        copyType(paramType, nextTypeId c.idgen, getCurrOwner(c)), paramTypId,
        info, genericParams, paramName)

  of tyGenericParam:
    markUsed(c, paramType.sym.info, paramType.sym)
    if tfWildcard in paramType.flags:
      paramType.flags.excl tfWildcard
      paramType.sym.transitionGenericParamToType()

  else: discard

proc semParamType(c: PContext, n: PNode, constraint: var PNode): PType =
  ## Semchecks the type of parameters.
  if n.kind == nkCurlyExpr:
    result = semTypeNode(c, n[0], nil)
    constraint = semNodeKindConstraints(n, c.config, 1)
  elif n.kind == nkCall and
      n[0].kind in {nkIdent, nkSym, nkOpenSymChoice, nkClosedSymChoice} and
      legacyConsiderQuotedIdent(c, n[0], nil).s == "{}":
    result = semTypeNode(c, n[1], nil)
    constraint = semNodeKindConstraints(n, c.config, 2)
  else:
    result = semTypeNode(c, n, nil)

proc newProcType(c: PContext; info: TLineInfo; prev: PType = nil): PType =
  result = newOrPrevType(tyProc, prev, c)
  result.callConv = lastOptionEntry(c).defaultCC
  result.n = newNodeI(nkFormalParams, info)
  rawAddSon(result, nil) # return type
  # result.n[0] used to be `nkType`, but now it's `nkEffectList` because
  # the effects are now stored in there too ... this is a bit hacky, but as
  # usual we desperately try to save memory:
  result.n.add newNodeI(nkEffectList, info)

proc isMagic(sym: PSym): bool =
  let nPragmas = sym.ast[pragmasPos]
  return hasPragma(nPragmas, wMagic)

proc semProcTypeNode(c: PContext, n, genericParams: PNode,
                     prev: PType, kind: TSymKind; isType=false): PType =
  # for historical reasons (code grows) this is invoked for parameter
  # lists too and then 'isType' is false.
  checkMinSonsLen(n, 1, c.config)
  result = newProcType(c, n.info, prev)
  var check = initIntSet()
  var counter = 0

  for i in 1..<n.len:
    var a = n[i]
    if a.kind != nkIdentDefs:
      # for some generic instantiations the passed ':env' parameter
      # for closures has already been produced (see bug #898). We simply
      # skip this parameter here. It'll then be re-generated in another LL
      # pass over this instantiation:
      if a.kind == nkSym and sfFromGeneric in a.sym.flags:
        continue

      semReportIllformedAst(c.config, a, "")

    checkMinSonsLen(a, 3, c.config)
    var
      typ: PType = nil
      def: PNode = nil
      constraint: PNode = nil
      hasType = a[^2].kind != nkEmpty
      hasDefault = a[^1].kind != nkEmpty

    if hasType:
      typ = semParamType(c, a[^2], constraint)
      # TODO: Disallow typed/untyped in procs in the compiler/stdlib
      if kind in {skProc, skFunc} and (typ.kind == tyTyped or typ.kind == tyUntyped):
        if not isMagic(getCurrOwner(c)):
          localReport(
            c.config,
            a[^2].info,
            reportTyp(rsemMisplacedMagicType, typ))


    if hasDefault:
      def = a[^1]
      block determineType:
        if genericParams.isGenericParams:
          def = semGenericStmt(c, def)
          if def.isError:
            localReport(c.config, def)
          if hasUnresolvedArgs(c, def):
            def.typ = makeTypeFromExpr(c, def.copyTree)
            break determineType

        # FIXME: don't analyse the `def` expression if the type was
        #        explicitly specified to be ``untyped``
        def = semExprWithType(c, def)
        if def.referencesAnotherParam(getCurrOwner(c)):
          def.flags.incl nfDefaultRefsParam

      if typ == nil:
        typ = def.typ
        if isEmptyContainer(typ):
          localReport(c.config, a, reportAst(
            rsemCannotInferParameterType, a[0]))

        if typ.kind == tyTypeDesc:
          # consider a proc such as:
          # proc takesType(T = int)
          # a naive analysis may conclude that the proc type is type[int]
          # which will prevent other types from matching - clearly a very
          # surprising behavior. We must instead fix the expected type of
          # the proc to be the unbound typedesc type:
          typ = newTypeWithSons(c, tyTypeDesc, @[newTypeS(tyNone, c)])
          typ.flags.incl tfCheckedForDestructor

      else:
        # if def.typ != nil and def.typ.kind != tyNone:
        # example code that triggers it:
        # proc sort[T](cmp: proc(a, b: T): int = cmp)
        if not containsGenericType(typ):
          # check type compatibility between def.typ and typ:
          def = fitNode(c, typ, def, def.info)
        elif typ.kind == tyStatic:
          def = semConstExpr(c, def)
          def = fitNode(c, typ, def, def.info)

    if def.isError:
      # xxx: yet another place where we report errors
      #      got lazy, but this should propagate
      localReport(c.config, def)

    if not hasType and not hasDefault:
      c.config.internalAssert(not isType, a.info, "':' expected")
      if kind in {skTemplate, skMacro}:
        typ = newTypeS(tyUntyped, c)
    elif skipTypes(typ, {tyGenericInst, tyAlias, tySink}).kind == tyVoid:
      continue

    for j in 0..<a.len-2:
      var arg = newSymG(skParam, if a[j].kind == nkPragmaExpr: a[j][0] else: a[j], c)
      if a[j].kind == nkPragmaExpr:
        a[j][1] = pragmaDecl(c, arg, a[j][1], paramPragmas)
        # check if we got any errors and if so report them
        for e in ifErrorWalkErrors(c.config, a[j][1]):
          localReport(c.config, e)

      if not hasType and not hasDefault and kind notin {skTemplate, skMacro}:
        let param = strTableGet(c.signatures, arg.name)
        if param != nil:
          typ = param.typ
        else:
          localReport(c.config, a, reportSym(
            rsemParameterRequiresAType, arg))

          typ = errorType(c)

      let lifted = liftParamType(c, kind, genericParams, typ,
                                 arg.name.s, arg.info)
      let finalType = if lifted != nil: lifted else: typ.skipIntLit(c.idgen)
      arg.typ = finalType
      arg.position = counter
      arg.constraint = constraint
      inc(counter)
      if def != nil and def.kind != nkEmpty:
        arg.ast = copyTree(def)
      if containsOrIncl(check, arg.name.id):
        localReport(c.config, a[j], reportSym(
          rsemParameterRedefinition, arg))

      result.n.add newSymNode(arg)
      rawAddSon(result, finalType)
      addParamOrResult(c, arg)
      styleCheckDef(c.config, a[j].info, arg)
      a[j] = newSymNode(arg)

  var r: PType
  if n[0].kind != nkEmpty:
    r = semTypeNode(c, n[0], nil)

  if r != nil and kind in {skMacro, skTemplate} and r.kind == tyTyped:
    # XXX: To implement the proposed change in the warning, just
    # delete this entire if block. The rest is (at least at time of
    # writing this comment) already implemented.
    let info = n[0].info
    localReport(c.config, info, reportSem(rsemTypedReturnDeprecated))
    r = nil

  if r != nil:
    # turn explicit 'void' return type into 'nil' because the rest of the
    # compiler only checks for 'nil':
    if skipTypes(r, {tyGenericInst, tyAlias, tySink}).kind != tyVoid:
      if kind notin {skMacro, skTemplate} and r.kind in {tyTyped, tyUntyped}:
        localReport(c.config, n[0], reportTyp(rsemMisplacedMagicType, r))
      # 'auto' as a return type does not imply a generic:
      elif r.kind == tyAnything:
        # 'p(): auto' and 'p(): untyped' are equivalent, but the rest of the
        # compiler is hardly aware of 'auto':
        r = newTypeS(tyUntyped, c)
      elif r.kind == tyStatic:
        # type allowed should forbid this type
        discard
      else:
        if r.sym == nil or sfAnon notin r.sym.flags:
          let lifted = liftParamType(c, kind, genericParams, r, "result",
                                     n[0].info)
          if lifted != nil:
            r = lifted
            #if r.kind != tyGenericParam:
            #echo "came here for ", typeToString(r)
            r.flags.incl tfRetType
        r = skipIntLit(r, c.idgen)
        if kind == skIterator:
          # see tchainediterators
          # in cases like iterator foo(it: iterator): typeof(it)
          # we don't need to change the return type to iter[T]
          result.flags.incl tfIterator
          # XXX Would be nice if we could get rid of this
      result[0] = r
      let oldFlags = result.flags
      propagateToOwner(result, r)
      if oldFlags != result.flags:
        # XXX This rather hacky way keeps 'tflatmap' compiling:
        if tfHasMeta notin oldFlags:
          result.flags.excl tfHasMeta
      result.n.typ = r

  if genericParams.isGenericParams:
    for n in genericParams:
      if {sfUsed, sfAnon} * n.sym.flags == {}:
        result.flags.incl tfUnresolved

      if tfWildcard in n.sym.typ.flags:
        n.sym.transitionGenericParamToType()
        n.sym.typ.flags.excl tfWildcard

proc semStmtListType(c: PContext, n: PNode, prev: PType): PNode =
  checkMinSonsLen(n, 1, c.config)
  result = shallowCopy(n)
  # make sure we always return a type statement list:
  result.transitionSonsKind(nkStmtListType)
  for i in 0..<n.len - 1:
    result[i] = semStmt(c, n[i], {})

  result[^1] = semTypeNode2(c, n[^1], prev)
  result.typ = result[^1].typ

proc semBlockType(c: PContext, n: PNode, prev: PType): PNode =
  inc(c.p.nestedBlockCounter)
  checkSonsLen(n, 2, c.config)
  result = shallowCopy(n)
  result[0] = n[0]

  openScope(c)
  if n[0].kind notin {nkEmpty, nkSym}:
    addDecl(c, newSymS(skLabel, n[0], c))

  result[1] = semStmtListType(c, n[1], prev)
  result.typ = result[1].typ
  closeScope(c)
  dec(c.p.nestedBlockCounter)

proc semGenericParamInInvocation(c: PContext, n: PNode): PNode =
  # check if the operand evaluates to a literal type:
  result = semTypeNodeAux(c, n, nil)
  if result.isNil:
    # it doesn't. It might be a static value
    result = tryConstExpr(c, n)
  elif result.kind != nkError:
    # it'a literal type, so wrap it in a ``tyTypeDesc``
    result.typ = makeTypeDesc(c, result.typ)

  if result.isNil:
    # it's neither a literal type nor static value
    result = c.config.newError(n, PAstDiag(kind: adSemTypeExpected))

proc semObjectTypeForInheritedGenericInst(c: PContext, n: PNode, t: PType) =
  var
    check = initIntSet()
    pos = 0
  let
    realBase = t[0]
    base = skipTypesOrNil(realBase, skipPtrs)
  if base.isNil:
    localReport(c.config, n.info, reportTyp(rsemIllegalRecursion, t))

  else:
    let concreteBase = skipGenericInvocation(base)
    if concreteBase.kind == tyObject and tfFinal notin concreteBase.flags:
      addInheritedFields(c, check, pos, concreteBase)
    else:
      if concreteBase.kind != tyError:
        localReport(c.config, n.info, reportTyp(
          rsemExpectNonFinalForBase, concreteBase))

  var newf = newNodeI(nkRecList, n.info)
  semRecordNodeAux(c, t.n, check, pos, newf, t)

func skipImplicitTypeDesc(t: PType): PType =
  if t.kind == tyTypeDesc and tfExplicit notin t.flags:
    result = t.base
  else:
    result = t

proc semGeneric(c: PContext, n: PNode, s: PSym, prev: PType): PNode =
  addInNimDebugUtils(c.config, "semGeneric", n, prev, result.typ)
  if s.typ == nil:
    return c.config.newError(n, PAstDiag(kind: adSemCannotInstantiate))

  var t = s.typ.skipTypes({tyAlias})
  if t.kind == tyCompositeTypeClass and t.base.kind == tyGenericBody:
    t = t.base

  let invoc = newOrPrevType(tyGenericInvocation, prev, c)
  addSonSkipIntLit(invoc, t, c.idgen)

  template addToResult(typ) =
    c.config.internalAssert typ != nil
    addSonSkipIntLit(invoc, typ, c.idgen)

  if t.kind == tyForward:
    result = shallowCopy(n)
    result[0] = n[0]
    for i in 1..<n.len:
      let elem = semGenericParamInInvocation(c, n[i])
      result[i] = elem
      assert elem != nil
      addToResult(elem.typ.skipImplicitTypeDesc())

    result.typ = invoc
    return
  elif t.kind != tyGenericBody:
    # we likely got code of the form TypeA[TypeB] where TypeA is
    # not generic.
    localReport(c.config, n.info, reportSym(
      rsemNoGenericParamsAllowed, s, typ = t))

    # TODO: proper diagnostic
    return c.config.newError(n, PAstDiag(kind: adSemCannotInstantiate))
  else:
    var m = newCandidate(c, t)
    m.isNoCall = true
    matches(c, n, m)

    if m.state != csMatch:
      localReport(c.config, n.info):
        block:
          var r = reportTyp(rsemCannotInstantiateWithParameter, t, ast = n)
          r.arguments.got = maybeResemArgs(c, n)
          r.arguments.expected = maybeResemArgs(c, t.n, 0)
          r

      # TODO: diagnostic
      return c.config.newError(n, PAstDiag(kind: adWrappedError))

    # there was a match, but that doesn't mean that the invocation produces a
    # concrete type: some arguments might be generic
    var isConcrete = true

    for i in 1..<m.call.len:
      var typ = m.call[i].typ
      # is this a 'typedesc' *parameter*? If so, use the typedesc type,
      # unstripped.
      if m.call[i].kind == nkSym and m.call[i].sym.kind == skParam and
          typ.kind == tyTypeDesc and containsGenericType(typ):
        isConcrete = false
        addToResult(typ)
      else:
        typ = typ.skipTypes({tyTypeDesc})
        if containsGenericType(typ): isConcrete = false
        addToResult(typ)

    result = m.call

    if isConcrete:
      if s.ast == nil and s.typ.kind != tyCompositeTypeClass:
        # XXX: What kind of error is this? is it still relevant?
        unreachable()
      else:
        result.typ = instGenericContainer(c, n.info, invoc)
    else:
      result.typ = invoc

  # TODO: unify this check with that from ``generateTypeInstance``. In addition,
  #       properly check for non-tuple recursion
  # special check for generic object with
  # generic/partial specialized parent
  let tx = result.typ.skipTypes(abstractPtrs, 50)
  if tx.isNil or isTupleRecursive(tx):
    localReport(c.config, n.info, reportTyp(
      rsemIllegalRecursion, result.typ[0]))

    # TODO: diagnostic
    return c.config.newError(result, PAstDiag(kind: adWrappedError))
  if tx != result.typ and tx.kind == tyObject:
    if tx[0] != nil:
      semObjectTypeForInheritedGenericInst(c, n, tx)
    var position = 0
    recomputeFieldPositions(tx, tx.n, position)

proc maybeAliasType(c: PContext; typeExpr, prev: PType): PType =
  if typeExpr.kind in {tyObject, tyEnum, tyDistinct, tyForward, tyGenericBody} and prev != nil:
    result = newTypeS(tyAlias, c)
    result.rawAddSon typeExpr
    result.sym = prev.sym
    assignType(prev, result)

proc fixupTypeOf(c: PContext, prev: PType, typExpr: PNode) =
  if prev != nil:
    let result = newTypeS(tyAlias, c)
    result.rawAddSon typExpr.typ
    result.sym = prev.sym
    assignType(prev, result)

proc fixupTypeExpr(c: PContext, t: PType, prev: PType): PType =
    ## Fixes types constructed by macros/templates
    result = t
    if prev != nil and prev.sym != nil:
      if result.sym.isNil:
        # Behold! you're witnessing enormous power yielded
        # by macros. Only macros can summon unnamed types
        # and cast spell upon AST. Here we need to give
        # it a name taken from left hand side's node
        result.sym = prev.sym
        result.sym.typ = result
      else:
        # Less powerful routine like template do not have
        # the ability to produce unnamed types. But still
        # it has wild power to push a type a bit too far.
        # So we need to hold it back using alias and prevent
        # unnecessary new type creation
        let alias = maybeAliasType(c, result, prev)
        if alias != nil: result = alias

proc semTypeExpr(c: PContext, n: PNode; prev: PType): PNode =
  let n = semExprWithType(c, n)
  if n.kind == nkError:
    result = n
  elif n.typ.kind == tyTypeDesc:
    # where ``semTypeExpr`` is used, the underlying type is expected, not a
    # typedesc
    # TODO: don't skip explicit typedescs
    let t = n.typ.base
    result = n
    result.typ = fixupTypeExpr(c, t, prev)
  else:
    result = c.config.newError(n, PAstDiag(kind: adSemTypeExpected))

proc freshType(c: PContext; res, prev: PType): PType {.inline.} =
  if prev.isNil:
    result = copyType(res, nextTypeId c.idgen, res.owner)
    copyTypeProps(c.graph, c.idgen.module, result, res)
  else:
    result = res

template modifierTypeKindOfNode(n: PNode): TTypeKind =
  case n.kind
  of nkVarTy: tyVar
  of nkRefTy: tyRef
  of nkPtrTy: tyPtr
  of nkStaticTy: tyStatic
  of nkTypeOfExpr: tyTypeDesc
  else: tyNone

proc semTypeClass(c: PContext, n: PNode, prev: PType): PType =
  # if n.len == 0: return newConstraint(c, tyTypeClass)
  let
    pragmas = n[1]
    inherited = n[2]

  result = newOrPrevType(tyUserTypeClass, prev, c)
  result.flags.incl tfCheckedForDestructor
  var owner = getCurrOwner(c)
  var candidateTypeSlot = newTypeWithSons(owner, tyAlias, @[c.errorType], c.idgen)
  result.sons = @[candidateTypeSlot]
  result.n = n

  if inherited.kind != nkEmpty:
    for n in inherited.sons:
      result.add semTypeNode(c, n, nil)

  openScope(c)
  for param in n[0]:
    var
      dummyName: PNode
      dummyType: PType

    let modifier = param.modifierTypeKindOfNode

    if modifier != tyNone:
      dummyName = param[0]
      dummyType = c.makeTypeWithModifier(modifier, candidateTypeSlot)
      # if modifier == tyRef:
        # dummyType.flags.incl tfNotNil
      if modifier == tyTypeDesc:
        dummyType.flags.incl {tfConceptMatchedTypeSym, tfCheckedForDestructor}
    else:
      dummyName = param
      dummyType = candidateTypeSlot

    # this can be true for 'nim check' on incomplete concepts,
    # see bug #8230
    if dummyName.kind == nkEmpty: continue

    c.config.internalAssert dummyName.kind == nkIdent
    var dummyParam = newSym(if modifier == tyTypeDesc: skType else: skVar,
                            dummyName.ident, nextSymId c.idgen, owner, param.info)
    dummyParam.typ = dummyType
    incl dummyParam.flags, sfUsed
    addDecl(c, dummyParam)

  result.n[3] = semConceptBody(c, n[3])
  if result.n[3].isError:
    localReport(c.config, result.n[3])
  closeScope(c)

proc semTypeClass2(c: PContext, n: PNode, prev: PType): PNode =
  result = newNodeIT(nkType, n.info, semTypeClass(c, n, prev))

proc applyTypeSectionPragmas(c: PContext; pragmas, operand: PNode): PNode =
  for p in pragmas:
    let key = if p.kind in nkPragmaCallKinds and p.len >= 1: p[0] else: p

    if p.kind == nkEmpty or whichPragma(p) != wInvalid:
      discard "builtin pragma"
    else:
      let (ident, err) = considerQuotedIdent(c, key)
      if strTableGet(c.userPragmas, ident) != nil:
        discard "User-defined pragma"
      else:
        var amb = false
        let sym = searchInScopes(c, ident, amb)
        # XXX: What to do here if amb is true?
        if sym != nil and sfCustomPragma in sym.flags:
          discard "Custom user pragma"
        else:
          # we transform ``(arg1, arg2: T) {.m, rest.}`` into ``m((arg1, arg2: T) {.rest.})`` and
          # let the semantic checker deal with it:
          var x = newNodeI(nkCall, key.info)
          x.add(key)
          if p.kind in nkPragmaCallKinds and p.len > 1:
            # pass pragma arguments to the macro too:
            for i in 1 ..< p.len:
              x.add(p[i])
          # Also pass the node the pragma has been applied to
          x.add(operand.copyTreeWithoutNode(p))
          # recursion assures that this works for multiple macro annotations too:
          var r = semOverloadedCall(c, x, {skMacro, skTemplate}, {efNoUndeclared})
          if r != nil:
            if r.kind == nkError:
              localReport(c.config, r)
              return

            doAssert r[0].kind == nkSym
            let m = r[0].sym
            case m.kind
            of skMacro: return semMacroExpr(c, r, m, {efNoSemCheck})
            of skTemplate: return semTemplateExpr(c, r, m, {efNoSemCheck})
            else: doAssert(false, "cannot happen")

proc semProcTypeWithScope(c: PContext, n: PNode,
                          prev: PType, kind: TSymKind): PType =
  checkSonsLen(n, 2, c.config)

  if n[1].kind != nkEmpty and n[1].len > 0:
    let macroEval = applyTypeSectionPragmas(c, n[1], n)
    if macroEval != nil:
      return semTypeNode(c, macroEval, prev)

  openScope(c)
  result = semProcTypeNode(c, n[0], nil, prev, kind, isType=true)
  # start with 'ccClosure', but of course pragmas can overwrite this:
  result.callConv = ccClosure
  # dummy symbol for `pragma`:
  var s = newSymS(kind, newIdentNode(getIdent(c.cache, "dummy"), n.info), c)
  s.typ = result
  if n[1].kind != nkEmpty and n[1].len > 0:
    n[1] = pragmaDecl(c, s, n[1], procTypePragmas)
  elif c.optionStack.len > 0:
    # we're still interested in implicit tags and raises pragmas
    n[1] = implicitPragmas(c, s, n[1], {wTags, wRaises})

  when true:
    # check if we got any errors and if so report them
    for e in ifErrorWalkErrors(c.config, n[1]):
      localReport(c.config, e)
    when useEffectSystem: setEffectsForProcType(c.graph, result, n[1])
  closeScope(c)

proc symFromExpectedTypeNode(c: PContext, n: PNode): PSym =
  if n.kind == nkType:
    result = symFromType(c, n.typ, n.info)
  else:
    let err = newError(c.config, n, PAstDiag(kind: adSemTypeExpected))
    localReport(c.config, err)
    result = errorSym(c, n, err)

proc semContainerLike(c: PContext, n: PNode): PNode =
  if n.len == 2:
    result = newNodeI(nkBracketExpr, n.info, 2)
    result[0] = n[0]
    result[1] = semTypeNode2(c, n[1], nil)
  else:
    # TODO: diagnostic
    discard

proc semStaticType(c: PContext, childNode: PNode, prev: PType): PNode =
  result = newTreeI(nkStaticTy, childNode.info,
                    [semTypeNode2(c, childNode, nil)])

  let typ = newOrPrevType(tyStatic, prev, c)
  var base = result[0].typ.skipTypes({tyTypeDesc, tyAlias})
  typ.rawAddSon(base)
  typ.flags.incl tfHasStatic
  result.typ = typ

proc semTypeOf(c: PContext; n: PNode; prev: PType): PNode =
  ## Computes the type of expression `n` in the context of a ``typeof``
  ## expression
  case n.kind
  of nkError:
    result = n
  else:
    openScope(c)
    result = semExprWithType(c, n, {efInTypeof})
    closeScope(c)

    if result.kind != nkError:
      fixupTypeOf(c, prev, result)

proc semTypeNodeAux(c: PContext, n: PNode, prev: PType): PNode =
  ## Same as ``semTypeNode``, but returns the analysed tree (or an
  ## error) instead of the computed type. Note that the resulting node's
  ## type is not wrapped in a *implicit* ``tyTypeDesc`` (explicit ones are not
  ## stripped).
  addInNimDebugUtils(c.config, "semTypeNodeAux", n, result, result.typ)
  result = nil
  inc c.inTypeContext

  if c.config.cmd == cmdIdeTools: suggestExpr(c, n)
  case n.kind
  of nkEmpty:
    # TODO: disallow
    result = n
  of nkTypeOfExpr:
    # for ``typeof(countup(1,3))``, see ``tests/ttoseq``.
    checkSonsLen(n, 1, c.config)
    result = semTypeOf(c, n[0], prev)
    if result.typ.kind == tyTypeDesc: result.typ.flags.incl tfExplicit
  of nkPar:
    if n.len == 1:
      result = semTypeNode2(c, n[0], prev)
    else:
      # can't use ``unreachable`` here until macro output is sanitized
      result =
        c.config.newError(n, PAstDiag(kind: adSemIllformedAstExpectedOneOf,
                                      expectedKinds: {nkTupleConstr}))
  of nkTupleConstr: result = semAnonTuple(c, n, prev)
  of nkCallKinds:
    let x = n[0]
    let ident = case x.kind
                of nkIdent: x.ident
                of nkSym: x.sym.name
                of nkClosedSymChoice, nkOpenSymChoice: x[0].sym.name
                else: nil
    if ident != nil and ident.s == "[]":
      let b = newNodeI(nkBracketExpr, n.info)
      for i in 1..<n.len: b.add(n[i])
      result = semTypeNode2(c, b, prev)
    elif ident != nil and ident.id == ord(wDotDot):
      result = semRangeAux(c, n, prev)

    # QUESTION: keep ``nil ref T``?
    # elif n[0].kind == nkNilLit and n.len == 2:
    #   result = semTypeNode2(c, n[1], prev)
    #   if result.typ.skipTypes({tyGenericInst, tyAlias, tySink}).kind in NilableTypes+GenericTypes:
    #     if tfNotNil in result.typ.flags:
    #       result = freshType(c, result, prev)
    #       result.flags.excl(tfNotNil)
    #   else:
    #     localReport(c.config, n, reportSem rsemTypeInvalid)

    elif n[0].kind notin nkIdentKinds:
      result = semTypeExpr(c, n, prev)
    else:
      let (op, err) = considerQuotedIdent(c, n[0])
      if err != nil:
        localReport(c.config, err)
      if op.id in {ord(wAnd), ord(wOr)} or op.s == "|":
        checkSonsLen(n, 3, c.config)
        result = shallowCopy(n)
        result[0] = x
        result[1] = semTypeNode2(c, n[1], nil)
        result[2] = semTypeNode2(c, n[2], nil)
        if nkError in {result[0].kind, result[1].kind}:
          discard
        else:
          let
            t1 = result[1].typ
            t2 = result[2].typ
          result.typ = if op.id == ord(wAnd): makeAndType(c, t1, t2)
                       else:                  makeOrType(c, t1, t2)
      elif op.id == ord(wNot):
        result = shallowCopy(n)
        result[0] = n[0]
        case n.len
        of 3:
          result[1] = semTypeNode2(c, n[1], prev)
          result[2] = n[2]

          var typ = result[1].typ
          if typ.kind == tyTypeDesc and tfUnresolved notin typ.flags:
            typ = typ.base
          if n[2].kind != nkNilLit:
            localReport(c.config, n, reportSem rsemMalformedNotNilType)
          if notnil notin c.features and strictNotNil notin c.features:
            localReport(c.config, n, reportSem rsemEnableNotNilExperimental)
          let resolvedType = typ.skipTypes({tyGenericInst, tyAlias, tySink})
          case resolvedType.kind
          of tyGenericParam, tyTypeDesc, tyFromExpr:
            # XXX: This is a really inappropraite hack, but it solves
            # https://github.com/nim-lang/Nim/issues/4907 for now.
            #
            # A proper solution is to introduce a new type kind such
            # as `tyNotNil[tyRef[SomeGenericParam]]`. This will allow
            # semtypinst to replace the generic param correctly in
            # situations like the following:
            #
            # type Foo[T] = object
            #   bar: ref T not nil
            #   baz: ref T
            #
            # The root of the problem is that `T` here must have a specific
            # ID that is bound to a concrete type during instantiation.
            # The use of `freshType` below breaks this. Another hack would
            # be to reuse the same ID for the not nil type, but this will
            # fail if the `T` parameter is referenced multiple times as in
            # the example above.
            #
            # I suggest revisiting this once the language decides on whether
            # `not nil` should be the default. We can then map nilable refs
            # to other types such as `Option[T]`.
            typ = makeTypeFromExpr(c, newTree(nkStmtListType, result.copyTree))
          of NilableTypes + {tyGenericInvocation, tyForward}:
            typ = freshType(c, typ, prev)
            typ.flags.incl(tfNotNil)
          else:
            # return c.config.newError(result, PAstDiag(kind: adSemTypeInvalid))
            localReport(c.config, n, reportSem rsemTypeInvalid)

          result.typ = typ
        of 2:
          result[1] = semTypeNode2(c, n[1], prev)
          result.typ = makeNotType(c, result[1].typ)
        else:
          localReport(c.config, n, reportSem rsemTypeInvalid)
      elif op.id == ord(wPtr):
        result = semTypeNode2(c, normalizeAnyRef(n, nkPtrTy), prev)
      elif op.id == ord(wRef):
        result = semTypeNode2(c, normalizeAnyRef(n, nkRefTy), prev)
      elif op.id == ord(wType):
        checkSonsLen(n, 2, c.config)
        result = semTypeOf(c, n[1], prev)
      elif op.s == "owned" and n.len == 2:
        # skip 'owned' in type expressions and produce a warning
        localReport(c.config, n, reportSem rsemOwnedTypeDeprecated)
        result = semTypeExpr(c, n[1], prev)
      elif (op.s == "sink" or op.s == "lent") and n.len == 2:
        # it's a 'sink T' or 'lent T' expression
        # XXX: consider introducing special words for both (i.e., ``wLent``,
        #      ``wSink``)
        result = newTreeI(n.kind, n.info, [n[0], semTypeNode2(c, n[1], nil)])

        let typ = newOrPrevType((if op.s == "sink": tySink else: tyLent), nil, c)
        typ.rawAddSonNoPropagationOfTypeFlags result[1].typ
        result.typ = typ
      else:
        if c.inGenericContext > 0 and n.kind == nkCall:
          result = copyTree(n)
          result.typ = makeTypeFromExpr(c, n)
        else:
          result = semTypeExpr(c, n, prev)
  of nkWhenStmt:
    var whenResult = semWhen(c, n, false)
    if whenResult.kind == nkStmtList:
      whenResult.transitionSonsKind(nkStmtListType)
    result = semTypeNode2(c, whenResult, prev)
  of nkBracketExpr:
    checkMinSonsLen(n, 2, c.config)
    var head = n[0]
    var s = if head.kind notin nkCallKinds: semTypeIdent(c, head)
            else: symFromExpectedTypeNode(c, semExpr(c, head))
    case s.magic
    of mArray: result = semArray(c, n, prev)
    of mOpenArray: result = semContainer(c, n, tyOpenArray, "openarray", prev)
    of mUncheckedArray: result = semContainer(c, n, tyUncheckedArray, "UncheckedArray", prev)
    of mRange: result = semRange(c, n, prev)
    of mSet: result = semSet(c, n, prev)
    of mOrdinal: result = semOrdinal(c, n, prev)
    of mSeq:
      result = semContainer(c, n, tySequence, "seq", prev)
      if optSeqDestructors in c.config.globalOptions:
        incl result.typ.flags, tfHasAsgn
    of mVarargs: result = semVarargs(c, n, prev)
    of mTypeDesc, mType, mTypeOf:
      result = semContainerLike(c, n)
      result.typ = makeTypeDesc(c, result[1].typ)
      result.typ.flags.incl tfExplicit
    of mStatic:
      result = semStaticType(c, n[1], prev)
    of mExpr:
      result = shallowCopy(n)
      result[0] = head

      let typ = copyType(s.typ, nextTypeId c.idgen, getCurrOwner(c))
      copyTypeProps(c.graph, c.idgen.module, typ, s.typ)
      for i in 1..<n.len:
        result[i] = semTypeNode2(c, n[i], nil)
        typ.rawAddSon(result[i].typ)

      result.typ = typ
    of mDistinct:
      let typ = newOrPrevType(tyDistinct, prev, c)
      result = semContainerArg(c, n, typ)
    of mVar:
      # normalize the expression first:
      result = newTreeI(nkVarTy, n.info, [n[1]])
      result = semVarOutType(c, result, prev, tyVar)
    of mRef: result = semAnyRef(c, n, tyRef, prev)
    of mPtr: result = semAnyRef(c, n, tyPtr, prev)
    of mTuple: result = semTuple(c, n, prev)
    else: result = semGeneric(c, n, s, prev)
  of nkDotExpr:
    # XXX: consider using ``semGenericStmt`` here instead
    let typeExpr = semExpr(c, n)
    if typeExpr.typ.isNil:
      # TODO: use a different diagnostic. No object type is expected here
      result = c.config.newError(n, PAstDiag(kind: adSemExpectedObjectType))
    elif typeExpr.typ.kind == tyFromExpr:
      result = typeExpr
    elif typeExpr.typ.kind != tyTypeDesc:
      result = c.config.newError(n, PAstDiag(kind: adSemTypeExpected))
    else:
      # TODO: use a case statement
      var typ = typeExpr.typ.base
      if typ.isMetaType and typ.kind != tyUserTypeClass:
           # the dot expression may refer to a concept type in
           # a different module. allow a normal alias then.
        result = semGenericStmt(c, n)
        if result.kind != nkError:
          result.typ = makeTypeFromExpr(c, result.copyTree)
      else:
        let alias = maybeAliasType(c, typ, prev)
        if alias != nil: typ = alias
        result = typeExpr
        result.typ = typ
  of nkIdent, nkAccQuoted:
    var s = semTypeIdent(c, n)
    if s.typ == nil:
      if s.kind != skError:
        result = c.config.newError(n, PAstDiag(kind: adSemTypeExpected))
      else:
        result = s.ast
    elif s.kind == skParam and s.typ.kind == tyTypeDesc:
      c.config.internalAssert s.typ.base.kind != tyNone and prev == nil
      result = copyNode(n)
      result.typ = s.typ.base
    elif prev == nil:
      result = copyNode(n)
      result.typ = s.typ
    else:
      result = copyNode(n)
      let alias = maybeAliasType(c, s.typ, prev)
      if alias != nil:
        result.typ = alias
      else:
        assignType(prev, s.typ)
        # bugfix: keep the fresh id for aliases to integral types:
        if s.typ.kind notin {tyBool, tyChar, tyInt..tyInt64, tyFloat..tyFloat128,
                             tyUInt..tyUInt64}:
          prev.itemId = s.typ.itemId
        result.typ = prev
  of nkSym:
    let s = getGenSym(c, n.sym)
    if s.typ != nil and (s.kind == skType or s.typ.kind == tyTypeDesc):
      var t =
        if s.kind == skType:
          s.typ
        else:
          c.config.internalAssert s.typ.base.kind != tyNone and prev == nil
          s.typ.base
      let alias = maybeAliasType(c, t, prev)
      result = copyNode(n)
      result.typ =
        if alias != nil:
          alias
        elif prev == nil:
          t
        else:
          assignType(prev, t)
          prev
      markUsed(c, n.info, n.sym)
    elif s.kind == skError:
      result = s.ast
    else:
      # this could be a constant or something else that is valid in a value
      # context
      result = nil
  of nkObjectTy: result = semObjectNode2(c, n, prev, {})
  of nkTupleTy: result = semTuple(c, n, prev)
  of nkTupleClassTy:
    result = copyNode(n)
    result.typ = newConstraint(c, tyTuple)
  of nkTypeClassTy: result = semTypeClass2(c, n, prev)
  of nkRefTy: result = semAnyRef(c, n, tyRef, prev)
  of nkPtrTy: result = semAnyRef(c, n, tyPtr, prev)
  of nkVarTy: result = semVarOutType(c, n, prev, tyVar)
  of nkDistinctTy: result = semDistinct(c, n, prev)
  of nkStaticTy: result = semStaticType(c, n[0], prev)
  of nkIteratorTy:
    if n.len == 0:
      result = copyNode(n)
      result.typ = newTypeS(tyBuiltInTypeClass, c)
      let child = newTypeS(tyProc, c)
      child.flags.incl tfIterator
      result.typ.addSonSkipIntLit(child, c.idgen)
    else:
      # XXX: an ``nkType`` node is used here until ``semProcTypeWithScope``
      #      returns a node
      result = newNodeI(nkType, n.info)
      result.typ = semProcTypeWithScope(c, n, prev, skIterator)
      if result.typ.kind == tyProc:
        result.typ.flags.incl(tfIterator)
        if n.lastSon.kind == nkPragma and hasPragma(n.lastSon, wInline):
          result.typ.callConv = ccInline
        else:
          result.typ.callConv = ccClosure
  of nkProcTy:
    if n.len == 0:
      result = copyNode(n)
      result.typ = newConstraint(c, tyProc)
    else:
      result = newNodeIT(nkType, n.info,
                         semProcTypeWithScope(c, n, prev, skProc))
  of nkEnumTy: result = newNodeIT(nkType, n.info, semEnum(c, n, prev))
  of nkType: result = n
  of nkStmtListType: result = semStmtListType(c, n, prev)
  of nkBlockType: result = semBlockType(c, n, prev)
  of nkError:
    result = n
  else:
    # this is not necessarily an error; let the callsite handle it
    result = nil

  # note: the typ of the result is allowed to be nil
  # XXX: ^^ that doesn't seem like a good idea...

  dec c.inTypeContext

proc semTypeNode2(c: PContext, n: PNode, prev: PType): PNode =
  result = semTypeNodeAux(c, n, prev)
  if result.isNil or result.typ.isNil:
    echo n.kind
    result = newError(c.config, n, PAstDiag(kind: adSemTypeExpected))

proc semTypeNode(c: PContext, n: PNode, prev: PType): PType =
  addInNimDebugUtils(c.config, "semTypeNode", n, prev, result)
  let r = semTypeNodeAux(c, n, prev)
  if r.isNil:
    localReport(c.config, n, reportSem(rsemTypeExpected))
    newOrPrevType(tyError, prev, c)
  elif r.kind != nkError:
    r.typ # may be nil
  else:
    localReport(c.config, r)
    newOrPrevType(tyError, prev, c)

proc setMagicType(conf: ConfigRef; m: PSym, kind: TTypeKind, size: int) =
  # source : https://en.wikipedia.org/wiki/Data_structure_alignment#x86
  m.typ.kind = kind
  m.typ.size = size
  # this usually works for most basic types
  # Assuming that since ARM, ARM64  don't support unaligned access
  # data is aligned to type size
  m.typ.align = size.int16

  # FIXME: proper support for clongdouble should be added.
  # long double size can be 8, 10, 12, 16 bytes depending on platform & compiler
  if kind in {tyFloat64, tyFloat, tyInt, tyUInt, tyInt64, tyUInt64} and size == 8:
    m.typ.align = int16(conf.floatInt64Align)

proc setMagicIntegral(conf: ConfigRef; m: PSym, kind: TTypeKind, size: int) =
  setMagicType(conf, m, kind, size)
  incl m.typ.flags, tfCheckedForDestructor

proc processMagicType(c: PContext, m: PSym) =
  case m.magic
  of mInt: setMagicIntegral(c.config, m, tyInt, c.config.target.intSize)
  of mInt8: setMagicIntegral(c.config, m, tyInt8, 1)
  of mInt16: setMagicIntegral(c.config, m, tyInt16, 2)
  of mInt32: setMagicIntegral(c.config, m, tyInt32, 4)
  of mInt64: setMagicIntegral(c.config, m, tyInt64, 8)
  of mUInt: setMagicIntegral(c.config, m, tyUInt, c.config.target.intSize)
  of mUInt8: setMagicIntegral(c.config, m, tyUInt8, 1)
  of mUInt16: setMagicIntegral(c.config, m, tyUInt16, 2)
  of mUInt32: setMagicIntegral(c.config, m, tyUInt32, 4)
  of mUInt64: setMagicIntegral(c.config, m, tyUInt64, 8)
  of mFloat: setMagicIntegral(c.config, m, tyFloat, c.config.target.floatSize)
  of mFloat32: setMagicIntegral(c.config, m, tyFloat32, 4)
  of mFloat64: setMagicIntegral(c.config, m, tyFloat64, 8)
  of mFloat128: setMagicIntegral(c.config, m, tyFloat128, 16)
  of mBool: setMagicIntegral(c.config, m, tyBool, 1)
  of mChar: setMagicIntegral(c.config, m, tyChar, 1)
  of mString:
    setMagicType(c.config, m, tyString, szUncomputedSize)
    rawAddSon(m.typ, getSysType(c.graph, m.info, tyChar))
    if optSeqDestructors in c.config.globalOptions:
      incl m.typ.flags, tfHasAsgn
  of mCstring:
    setMagicIntegral(c.config, m, tyCstring, c.config.target.ptrSize)
    rawAddSon(m.typ, getSysType(c.graph, m.info, tyChar))
  of mPointer: setMagicIntegral(c.config, m, tyPointer, c.config.target.ptrSize)
  of mNil: setMagicType(c.config, m, tyNil, c.config.target.ptrSize)
  of mExpr:
    if m.name.s == "auto":
      setMagicIntegral(c.config, m, tyAnything, 0)
    else:
      setMagicIntegral(c.config, m, tyUntyped, 0)
  of mStmt:
    setMagicIntegral(c.config, m, tyTyped, 0)
  of mTypeDesc, mType:
    setMagicIntegral(c.config, m, tyTypeDesc, 0)
    rawAddSon(m.typ, newTypeS(tyNone, c))
  of mStatic:
    setMagicType(c.config, m, tyStatic, 0)
    rawAddSon(m.typ, newTypeS(tyNone, c))
  of mVoidType:
    setMagicIntegral(c.config, m, tyVoid, 0)
  of mArray:
    setMagicType(c.config, m, tyArray, szUncomputedSize)
  of mOpenArray:
    setMagicType(c.config, m, tyOpenArray, szUncomputedSize)
  of mVarargs:
    setMagicType(c.config, m, tyVarargs, szUncomputedSize)
  of mRange:
    setMagicIntegral(c.config, m, tyRange, szUncomputedSize)
    rawAddSon(m.typ, newTypeS(tyNone, c))
  of mSet:
    setMagicIntegral(c.config, m, tySet, szUncomputedSize)
  of mUncheckedArray:
    setMagicIntegral(c.config, m, tyUncheckedArray, szUncomputedSize)
  of mSeq:
    setMagicType(c.config, m, tySequence, szUncomputedSize)
    if optSeqDestructors in c.config.globalOptions:
      incl m.typ.flags, tfHasAsgn
    if defined(nimsuggest) or c.config.cmd == cmdCheck: # bug #18985
      discard
    else:
      assert c.graph.sysTypes[tySequence] == nil
    c.graph.sysTypes[tySequence] = m.typ
  of mOrdinal:
    setMagicIntegral(c.config, m, tyOrdinal, szUncomputedSize)
    rawAddSon(m.typ, newTypeS(tyNone, c))
  of mPNimrodNode:
    m.typ.flags.incl {tfTriggersCompileTime, tfCheckedForDestructor}
  of mException: discard
  of mBuiltinType:
    case m.name.s
    of "lent": setMagicType(c.config, m, tyLent, c.config.target.ptrSize)
    of "sink": setMagicType(c.config, m, tySink, szUncomputedSize)

    else:
      localReport(c.config, m.info, reportSym(rsemTypeExpected, m))

  else:
    localReport(c.config, m.info, reportSym(rsemTypeExpected, m))

proc semGenericConstraints(c: PContext, x: PType): PType =
  result = newTypeWithSons(c, tyGenericParam, @[x])

proc semGenericParamList(c: PContext, n: PNode, father: PType = nil): PNode =

  template addSym(result: PNode, s: PSym): untyped =
    if father != nil: addSonSkipIntLit(father, s.typ, c.idgen)
    if sfGenSym notin s.flags: addDecl(c, s)
    result.add newSymNode(s)

  result = copyNode(n)
  if n.kind != nkGenericParams:
    semReportIllformedAst(
      c.config, n, "Expected generic parameter list")

    return
  for i in 0..<n.len:
    var a = n[i]
    case a.kind
    of nkSym: result.addSym(a.sym)
    of nkIdentDefs:
      var def = a[^1]
      let constraint = a[^2]
      var typ: PType

      if constraint.kind != nkEmpty:
        typ = semTypeNode(c, constraint, nil)
        if typ.kind != tyStatic or typ.len == 0:
          if typ.kind == tyTypeDesc:
            if typ[0].kind == tyNone:
              typ = newTypeWithSons(c, tyTypeDesc, @[newTypeS(tyNone, c)])
              incl typ.flags, tfCheckedForDestructor
          else:
            typ = semGenericConstraints(c, typ)

      if def.kind != nkEmpty:
        def = semConstExpr(c, def)
        if typ == nil:
          if def.typ.kind != tyTypeDesc:
            typ = newTypeWithSons(c, tyStatic, @[def.typ])
        else:
          # the following line fixes ``TV2*[T:SomeNumber=TR] = array[0..1, T]``
          # from manyloc/named_argument_bug/triengine:
          def.typ = def.typ.skipTypes({tyTypeDesc})
          if not containsGenericType(def.typ):
            def = fitNode(c, typ, def, def.info)

      if typ == nil:
        typ = newTypeS(tyGenericParam, c)
        if father == nil: typ.flags.incl tfWildcard

      typ.flags.incl tfGenericTypeParam

      for j in 0 ..< a.len-2:
        var finalType: PType
        if j == 0:
          finalType = typ
        else:
          finalType = copyType(typ, nextTypeId c.idgen, typ.owner)
          copyTypeProps(c.graph, c.idgen.module, finalType, typ)
        # it's important the we create an unique
        # type for each generic param. the index
        # of the parameter will be stored in the
        # attached symbol.
        var paramName = a[j]
        var covarianceFlag = tfUnresolved

        if paramName.safeLen == 2:
          if not nimEnableCovariance or paramName[0].ident.s == "in":
            if father == nil or sfImportc notin father.sym.flags:
              localReport(c.config, paramName.info, reportAst(
                rsemExpectedImportedType, paramName[0]))
          covarianceFlag = if paramName[0].ident.s == "in": tfContravariant
                          else: tfCovariant
          if father != nil: father.flags.incl tfCovariant
          paramName = paramName[1]

        var s = if finalType.kind == tyStatic or tfWildcard in typ.flags:
            newSymG(skGenericParam, paramName, c).linkTo(finalType)
          else:
            newSymG(skType, paramName, c).linkTo(finalType)

        if covarianceFlag != tfUnresolved: s.typ.flags.incl(covarianceFlag)
        if def.kind != nkEmpty: s.ast = def
        s.position = result.len
        result.addSym(s)
    else:
      semReportIllformedAst(c.config, n, "")
