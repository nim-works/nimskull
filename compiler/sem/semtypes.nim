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

proc reportMeta(c: PContext; info: TLineInfo; t: PType) =
  ## Reports errors for using the meta type `t`.
  # XXX: instead of happening here, "pretty-printing" the error should be
  #      responsibility of the renderer
  assert t.isMetaType

  proc checkMeta(c: PContext, info: TLineInfo, t: PType) =
    if t != nil and t.isMetaType:
      if t.kind == tyBuiltInTypeClass and t.len == 1 and t[0].kind == tyProc:
        localReport(c.config, info, reportTyp(
          rsemProcIsNotAConcreteType, t))
      else:
        localReport(c.config, info, reportTyp(
          rsemTIsNotAConcreteType, t))

  case t.kind
  of tySequence, tySet, tyArray, tyOpenArray, tyVar, tyLent, tyPtr, tyRef,
      tyProc, tyGenericInvocation, tyGenericInst, tyAlias, tySink:
    let start = ord(t.kind in {tyGenericInvocation, tyGenericInst})
    for i in start..<t.len:
      checkMeta(c, info, t[i])
  else:
    checkMeta(c, info, t)

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
  result = newOrPrevType(tyEnum, prev, c)
  result.n = newNodeI(nkEnumTy, n.info)
  checkMinSonsLen(n, 1, c.config)
  if n[0].kind != nkEmpty:
    localReport(c.config, n[0], reportAst(rsemIllformedAst, n[0]))

  rawAddSon(result, nil) # base type; always nil
  # add a preliminary storage type such that the enum can already be
  # used in its own definition
  rawAddSon(result, getSysType(c.graph, n.info, tyInt))

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

  # now that we know the full value range, select the correct storage type
  # of the enum:
  let tk =
    if firstOrd(c.config, result) < Zero:
      tyInt32 # use signed int32
    elif result.size != szUncomputedSize:
      # use the manually specified size for selecting the storage type
      let s = result.size
      if   s <= 1: tyUInt8
      elif s <= 2: tyUInt16
      elif s <= 4: tyInt32
      elif s <= 8: tyInt64
      else:        unreachable()
    else:
      let lastOrd = lastOrd(c.config, result)
      if lastOrd < (1 shl 8):
        tyUInt8
      elif lastOrd < (1 shl 16):
        tyUInt16
      elif lastOrd < (BiggestInt(1) shl 32):
        tyInt32
      else:
        tyInt64

  result[1] = getSysType(c.graph, n.info, tk)

  if isPure and sfExported in result.sym.flags:
    addPureEnum(c, LazySym(sym: result.sym))

  if tfNotNil in e.typ.flags and not hasNull:
    result.flags.incl tfRequiresInit

  setToStringProc(c.graph, result, genEnumToStrProc(result, n.info, c.graph, c.idgen))

template semReportParamCountMismatch(
    config: ConfigRef, node: PNode, semtype: PType,
    expected, got: int,
    typename: string = ""): untyped =
  var report = semReportCountMismatch(
    rsemWrongNumberOfGenericParams, expected, got)
  report.ast = node
  report.typ = semtype
  report.str = typename
  localReport(config, node.info, report)

proc semSet(c: PContext, n: PNode, prev: PType): PType =
  result = newOrPrevType(tySet, prev, c)
  if n.len == 2 and n[1].kind != nkEmpty:
    var base = semTypeNode(c, n[1], nil)
    addSonSkipIntLit(result, base, c.idgen)
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
  else:
    c.config.semReportParamCountMismatch(n, result, 1, n.len - 1)
    addSonSkipIntLit(result, errorType(c), c.idgen)

proc semContainerArg(c: PContext; n: PNode, kindStr: string; result: PType) =
  if n.len == 2:
    var base = semTypeNode(c, n[1], nil)
    if base.kind == tyVoid:
      localReport(c.config, n.info, reportTyp(
        rsemTIsNotAConcreteType, base))

    addSonSkipIntLit(result, base, c.idgen)

  else:
    c.config.semReportParamCountMismatch(n, result, 1, n.len - 1)
    addSonSkipIntLit(result, errorType(c), c.idgen)

proc semContainer(c: PContext, n: PNode, kind: TTypeKind, kindStr: string,
                  prev: PType): PType =
  result = newOrPrevType(kind, prev, c)
  semContainerArg(c, n, kindStr, result)

proc semVarargs(c: PContext, n: PNode, prev: PType): PType =
  result = newOrPrevType(tyVarargs, prev, c)
  if n.len == 2 or n.len == 3:
    var base = semTypeNode(c, n[1], nil)
    addSonSkipIntLit(result, base, c.idgen)
    if n.len == 3:
      let (ident, err) = considerQuotedIdent(c, n[2])
      if err != nil:
        localReport(c.config, err)
      result.n = newIdentNode(ident, n[2].info)
  else:
    c.config.semReportParamCountMismatch(n, result, 1, n.len - 1)
    addSonSkipIntLit(result, errorType(c), c.idgen)

proc semVarOutType(c: PContext, n: PNode, prev: PType; kind: TTypeKind): PType =
  if n.len == 1:
    result = newOrPrevType(kind, prev, c)
    var base = semTypeNode(c, n[0], nil).skipTypes({tyTypeDesc})
    if base.kind == tyVar:
      localReport(c.config, n, reportSem(rsemVarVarNotAllowed))
      base = base[0]
    addSonSkipIntLit(result, base, c.idgen)
  else:
    result = newConstraint(c, kind)

proc aliasesType(given: PType, target: PType): bool =
  ## `true` if `given` is not an alias (inclusive of generic instantiation or
  ## distincts) of the `target`.
  var curr = target
  while curr != given and curr.kind in {tyAlias, tyGenericInst, tyDistinct}:
    curr = curr.lastSon
  # TODO: check to see if we need to traverse `tyAnd` and `tyOr`, also ensure
  #       no `when` clause (including `not`) refers to `given`
  result = curr == given

proc semDistinct(c: PContext, n: PNode, prev: PType): PType =
  if n.len == 0: return newConstraint(c, tyDistinct)
  let t = semTypeNode(c, n[0], nil).skipIntLit(c.idgen)
  if prev != nil and prev.aliasesType(t):
    result = newOrPrevType(tyError, prev, c)
    c.config.localReport(n.info, reportTyp(rsemIllegalRecursion, t))
  else:
    result = newOrPrevType(tyDistinct, prev, c)
    rawAddSon(result, t)
  if n.len > 1: result.n = n[1]

from std/sequtils import mapIt

proc fixupTypeVars(c: PContext, n: var PNode): bool =
  ## Takes AST that is produced by the generic pre-pass as input and:
  ## 1. makes sure all unresolved type variables are referenced via their
  ##    *symbol*. Necesary because the pre-pass leaves those as raw identifiers
  ##    in some cases.
  ## 2. computes whether unresolved type variables are referenced
  ##
  ## If there are no type variables, 'false' is returned and the AST remains
  ## unchanged.
  case n.kind
  of nkSym:
    if isUnresolvedSym(n.sym):
      n.typ = n.sym.typ
      result = true
  of nkIdent, nkAccQuoted:
    let (ident, err) = considerQuotedIdent(c, n)
    assert err == nil, "should have been handled in semgnrc"

    var amb = false
    let sym = searchInScopes(c, ident, amb)
    if sym != nil and isUnresolvedSym(sym):
      # replace with the symbol of the type variable
      n = newSymNode(sym, n.info)
      result = true
  of nkWithoutSons - {nkSym, nkIdent}:
    discard "nothing to do"
  of nkWithSons - {nkAccQuoted}:
    for i in 0..<n.len:
      let r = fixupTypeVars(c, n[i])
      # propagate upwards whether a type var was fixed:
      result = result or r

proc semRangeAux(c: PContext, n: PNode, prev: PType): PType =
  assert isRange(n)
  checkSonsLen(n, 3, c.config)
  result = newOrPrevType(tyRange, prev, c)
  result.n = newNodeI(nkRange, n.info)
  # always create a 'valid' range type, but overwrite it later
  # because 'semExprWithType' can raise an exception. See bug #6895.
  addSonSkipIntLit(result, errorType(c), c.idgen)

  if (n[1].kind == nkEmpty) or (n[2].kind == nkEmpty):
    localReport(c.config, n, reportSem rsemRangeIsEmpty)

  # run the pre-pass for generic statements first: we don't know
  # whether or not the expression references type variables
  var range = [semGenericStmt(c, n[1]), semGenericStmt(c, n[2])]

  for it in range.mitems:
    if fixupTypeVars(c, it):
      if it.kind != nkSym:
        # the expression needs a type, and since its a dependent one, a
        # ``tyFromExpr`` is used
        it.typ = makeTypeFromExpr(c, it.copyTree)

      # make sure the expression later takes the correct path through
      # ``replaceTypeVarsN``:
      it = makeStaticExpr(c, it)
      result.flags.incl tfUnresolved
    else:
      # the expression must be constant
      let e = semExprWithType(c, it)
      it = evalConstExpr(c, e)

    result.n.add it

  let
    rangeT = range.mapIt(it.typ.skipTypes({tyStatic}).skipIntLit(c.idgen))
    hasUnknownTypes = tyFromExpr in {rangeT[0].kind, rangeT[1].kind}

  if not hasUnknownTypes:
    if not sameType(rangeT[0].skipTypes({tyRange}), rangeT[1].skipTypes({tyRange})):
      # XXX: errors from the previous analysis need to be taken into account
      let r = typeMismatch(c.config, n.info, rangeT[0], rangeT[1], n)
      if r.kind == nkError:
        localReport(c.config, r)

    elif not isOrdinalType(rangeT[0]) and rangeT[0].kind notin {tyFloat..tyFloat128} or
        rangeT[0].kind == tyBool:
      localReport(c.config, n.info, reportTyp(
        rsemExpectedOrdinalOrFloat, rangeT[0]))

    elif enumHasHoles(rangeT[0]):
      localReport(c.config, n.info, reportTyp(rsemExpectedUnholyEnum, rangeT[0]))

  if (result.n[0].kind in {nkFloatLit..nkFloat64Lit} and result.n[0].floatVal.isNaN) or
      (result.n[1].kind in {nkFloatLit..nkFloat64Lit} and result.n[1].floatVal.isNaN):
    localReport(c.config, n, reportSem rsemRangeDoesNotSupportNan)

  if weakLeValue(result.n[0], result.n[1]) == impNo:
    localReport(c.config, n, reportSem rsemRangeIsEmpty)

  result[0] = rangeT[0]

proc semRange(c: PContext, n: PNode, prev: PType): PType =
  result = nil
  if n.len == 2:
    if isRange(n[1]):
      result = semRangeAux(c, n[1], prev)
      let n = result.n
      if n[0].kind in {nkCharLit..nkUInt64Lit} and n[0].intVal > 0:
        incl(result.flags, tfRequiresInit)
      elif n[1].kind in {nkCharLit..nkUInt64Lit} and n[1].intVal < 0:
        incl(result.flags, tfRequiresInit)
      elif n[0].kind in {nkFloatLit..nkFloat64Lit} and
          n[0].floatVal > 0.0:
        incl(result.flags, tfRequiresInit)
      elif n[1].kind in {nkFloatLit..nkFloat64Lit} and
          n[1].floatVal < 0.0:
        incl(result.flags, tfRequiresInit)
    else:
      c.config.localReport(n[0]):
        if n[1].kind == nkInfix and
          legacyConsiderQuotedIdent(c, n[1][0], nil).s == "..<":
          reportSem rsemRangeRequiresDotDot
        else:
          reportSem rsemExpectedRange
      result = newOrPrevType(tyError, prev, c)
  else:
    c.config.semReportParamCountMismatch(n, nil, 1, n.len - 1, "range")
    result = newOrPrevType(tyError, prev, c)

proc semArrayIndex(c: PContext, n: PNode): PType =
  if isRange(n):
    result = semRangeAux(c, n, nil)
  else:
    # the expression may depend on type variables, meaning that we have to
    # treat it as generic first
    # XXX: only run the generics pre-pass if the possibiltiy of unresolved type
    #      variables being used actually exists. At the time of writing,
    #      ``inGenericContext`` cannot be used for this, as it is not increased
    #      for implicitly and explicitly generic routines
    var e = semGenericStmt(c, n)
    if fixupTypeVars(c, e):
      # an expression that references type variables
      if e.kind == nkSym:
        if e.typ.kind == tyStatic:
          result = makeRangeWithStaticExpr(c, e)
          result.flags.incl tfUnresolved
        else:
          # a type variable is used directly as the index type (e.g.,
          # ``array[T, ...]``). Don't create a range, but use the type
          # directly
          result = e.typ
      else:
        # the type of the expression depends on the expression itself; signal
        # this via a ``tyFromExpr``
        e.typ = makeTypeFromExpr(c, copyTree(e))
        result = makeRangeWithStaticExpr(c, e)
        result.flags.incl tfUnresolved
      return

    # an expression that doesn't reference type variables
    e = semExprWithType(c, e)
    if e.kind in {nkIntLit..nkUInt64Lit}:
      if e.intVal < 0:
        localReport(c.config, n.info,
          SemReport(
            kind: rsemArrayExpectsPositiveRange,
            expectedCount: toInt128 0,
            got: toInt128 e.intVal))

      result = makeRangeType(c, 0, e.intVal-1, n.info, e.typ)
    else:
      let x = semConstExpr(c, e)
      if x.kind in {nkIntLit..nkUInt64Lit}:
        result = makeRangeType(c, 0, x.intVal-1, n.info,
                             x.typ.skipTypes({tyTypeDesc}))
      else:
        result = x.typ.skipTypes({tyTypeDesc})
        #localReport(c.config, n[1].info, errConstExprExpected)

proc semArray(c: PContext, n: PNode, prev: PType): PType =
  var base: PType
  if n.len == 3:
    # 3 = length(array indx base)
    let indx = semArrayIndex(c, n[1])
    var indxB = indx
    if indxB.kind in {tyGenericInst, tyAlias, tySink}: indxB = lastSon(indxB)
    if indxB.kind notin {tyGenericParam, tyStatic, tyFromExpr}:
      if indxB.skipTypes({tyRange}).kind in {tyUInt, tyUInt64, tyFromExpr}:
        discard
      elif not isOrdinalType(indxB):
        localReport(c.config, n[1].info, reportTyp(
          rsemExpectedOrdinal, indxB))

      elif enumHasHoles(indxB):
        localReport(c.config, n[1].info, reportTyp(
          rsemExpectedUnholyEnum, indxB.skipTypes({tyRange})))

    base = semTypeNode(c, n[2], nil)
    # ensure we only construct a tyArray when there was no error (bug #3048):
    result = newOrPrevType(tyArray, prev, c)
    # bug #6682: Do not propagate initialization requirements etc for the
    # index type:
    rawAddSonNoPropagationOfTypeFlags(result, indx)
    addSonSkipIntLit(result, base, c.idgen)
  else:
    semReportParamCountMismatch(c.config, n, prev, 2, n.len - 1, "array")
    result = newOrPrevType(tyError, prev, c)

proc semOrdinal(c: PContext, n: PNode, prev: PType): PType =
  result = newOrPrevType(tyOrdinal, prev, c)
  if n.len == 2:
    var base = semTypeNode(c, n[1], nil)
    if base.kind != tyGenericParam:
      if not isOrdinalType(base):
        localReport(c.config, n[1].info, reportTyp(
          rsemExpectedOrdinal, base))

    addSonSkipIntLit(result, base, c.idgen)
  else:
    semReportParamCountMismatch(c.config, n, prev, 1, n.len - 1, "ordinal")
    result = newOrPrevType(tyError, prev, c)

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

proc semAnonTuple(c: PContext, n: PNode, prev: PType): PType =
  if n.len == 0:
    localReport(c.config, n, reportSem rsemTypeExpected)
  result = newOrPrevType(tyTuple, prev, c)
  for it in n:
    let t = semTypeNode(c, it, nil).skipIntLit(c.idgen)
    if prev != nil and prev.aliasesType(t):
      result = newOrPrevType(tyError, prev, c)
      c.config.localReport(n.info, reportTyp(rsemIllegalRecursion, t))
      break
    else:
      rawAddSon(result, t)

proc semTuple(c: PContext, n: PNode, prev: PType): PType =
  # TODO: replace with a node returning variant that can in band errors
  addInNimDebugUtils(c.config, "semTuple", n, prev, result)
  result = newOrPrevType(tyTuple, prev, c)
  result.n = newNodeI(nkRecList, n.info)
  var check = initIntSet()
  var counter = 0
  for i in ord(n.kind == nkBracketExpr)..<n.len:
    var a = n[i]
    if (a.kind != nkIdentDefs):
      c.config.globalReport(reportAst(
        rsemIllformedAst, a,
        str = "Expected identDefs for node, but found " & $a.kind))

    checkMinSonsLen(a, 3, c.config)
    var typ = semTypeNode(c, a[^2], nil)
    if typ.isNil:
      localReport(c.config, a, reportSem rsemTypeExpected)
      typ = errorType(c)
    elif prev != nil and c.inGenericContext == 0 and typ.isMetaType:
      # only disallow meta types for tuple constructors where the
      # resulting type is directly assigned a name (i.e.,
      # ``type Tup = tuple[...]``)
      reportMeta(c, a[^2].info, typ)
      typ = errorType(c)

    if a[^1].kind != nkEmpty:
      localReport(c.config, a[^1], reportSem rsemInitHereNotAllowed)

    for j in 0 ..< a.len - 2:
      let
        fieldNode = newSymGNode(skField, a[j], c)
        field = getDefNameSymOrRecover(fieldNode)
      field.typ = typ
      field.position = counter
      inc(counter)
      if containsOrIncl(check, field.name.id):
        localReport(c.config, a[j].info, reportSym(
          rsemRedefinitionOf, field))
      else:
        result.n.add newSymNode(field)
        addSonSkipIntLit(result, typ, c.idgen)

      styleCheckDef(c.config, a[j].info, field)

  if result.n.len == 0: result.n = nil
  if isTupleRecursive(result):
    localReport(c.config, n.info, reportTyp(
      rsemIllegalRecursion, result))

proc semIdentVis(c: PContext, kind: TSymKind, n: PNode,
                 allowed: TSymFlags): PSym =
  # TODO: replace with a node return variant that can in band errors
  # identifier with visibility
  if n.kind == nkPostfix:
    if n.len == 2:
      # for gensym'ed identifiers the identifier may already have been
      # transformed to a symbol and we need to use that here:
      let
        identNode = newSymGNode(kind, n[1], c)
        (star, err) = considerQuotedIdent(c, n[0])

      result = getDefNameSymOrRecover(identNode)

      # TODO: remove all this reports stupidity
      if identNode.kind == nkError:
        localReport(c.config, identNode)

      if err != nil:
        localReport(c.config, err)

      # xxx: we can move the export allowed check much earlier
      if sfExported in allowed and star.id == ord(wStar):
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
    result = getDefNameSymOrRecover(newSymGNode(kind, n, c))

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
          it[0] = semGenericStmt(c, it[0])
          # ensure that all type variables have their symbol bound:
          discard fixupTypeVars(c, it[0])
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
    var typ = semTypeNode(c, n[^2], nil)
    if typ.isNil:
      localReport(c.config, n, reportSem rsemTypeExpected)
      typ = errorType(c)
    elif c.inGenericContext == 0 and typ.isMetaType:
      reportMeta(c, n[^2].info, typ)
      typ = errorType(c)
    else:
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
         not hasCaseFields and f.extname == "":
        f.extname = rope(f.name.s)
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
  of nkEmpty:
    if father.kind in {nkElse, nkOfBranch}:
      father.add n
  else:
    semReportIllformedAst(c.config, n, "?")


proc semObjectNode(c: PContext, n: PNode, prev: PType; flags: TTypeFlags): PType =
  if n.len == 0:
    return newConstraint(c, tyObject)
  var check = initIntSet()
  var pos = 0
  var realBase: PType = nil
  # n[0] contains the pragmas (if any). We process these later...
  checkSonsLen(n, 3, c.config)
  if n[1].kind != nkEmpty:
    realBase = semTypeNode(c, n[1][0], nil)
    let base = skipToObject(realBase)
    case base.kind
    of tyObject:
      if tfFinal notin base.flags:
        # we only check field duplication for objects inheriting from
        # concrete objects. If inheriting from a generic object or partially
        # specialized object, the duplication check is performed during
        # instantiation
        if realBase.skipTypes(skipPtrs).kind == tyObject:
          pos += addInheritedFields(check, base)

        if base.sym != nil and base.sym.magic == mException and
           sfSystemModule notin c.module.flags:
          localReport(c.config, n.info, reportSem(rsemInheritFromException))
      else:
        localReport(c.config, n[1].info):
          reportTyp(rsemExpectNonFinalForBase, realBase)
        realBase = nil
    of tyGenericParam:
      # the base is a generic parameter (e.g., ``object of T``)
      discard "figured out later"
    of tyError:
      # don't cascade errors
      realBase = nil
    elif base.isMetaType:
      if c.inGenericContext > 0:
        discard "okay, handled during instantiation"
      else:
        localReport(c.config, n[1].info):
          reportTyp(rsemTIsNotAConcreteType, realBase)
        realBase = nil
    else:
      # not something that can be inherited from
      localReport(c.config, n[1].info):
        reportTyp(rsemExpectObjectForBase, realBase)
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
    pos += addInheritedFields(check, result)
  semRecordNodeAux(c, n[2], check, pos, result.n, result)
  if n[0].kind != nkEmpty:
    # dummy symbol for `pragma`:
    var s = newSymS(skType, newIdentNode(getIdent(c.cache, "dummy"), n.info), c)
    s.typ = result
    n[0] = pragmaDecl(c, s, n[0], typePragmas)
    # check if we got any errors and if so report them
    for e in ifErrorWalkErrors(c.config, n[0]):
      localReport(c.config, e)

  if realBase == nil and tfInheritable notin result.flags:
    incl(result.flags, tfFinal)

  if c.inGenericContext == 0 and computeRequiresInit(c, result):
    result.flags.incl tfRequiresInit

proc semAnyRef(c: PContext; n: PNode; kind: TTypeKind; prev: PType): PType =
  if n.len < 1:
    result = newConstraint(c, kind)
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
    if t.kind == tyVoid:
      localReport(c.config, n.info, reportTyp(
        rsemTVoidNotAllowed, t, str = kind.toHumanStr))


    result = newOrPrevType(kind, prev, c)
    var isNilable = false
    var wrapperKind = tyNone
    # check every except the last is an object:
    for i in isCall..<n.len-1:
      let ni = n[i]
      # echo "semAnyRef ", "n: ", n, "i: ", i, "ni: ", ni
      if ni.kind == nkNilLit:
        isNilable = true
      else:
        let region = semTypeNode(c, ni, nil)
        if region.kind == tySink:
          wrapperKind = region.kind
        elif region.skipTypes({tyGenericInst, tyAlias, tySink}).kind notin {
              tyError, tyObject}:
          localReport(c.config, n[i], reportSem rsemExpectedObjectForRegion)
          addSonSkipIntLit(result, region, c.idgen)

        else:
          localReport(c.config, n.info, reportSem(rsemPtrRegionIsDeprecated))
          addSonSkipIntLit(result, region, c.idgen)

    addSonSkipIntLit(result, t, c.idgen)
    # if not isNilable: result.flags.incl tfNotNil
    case wrapperKind
    of tySink:
      let t = newTypeS(tySink, c)
      t.rawAddSonNoPropagationOfTypeFlags result
      result = t
    else: discard

    if result.kind == tyRef and c.config.selectedGC in {gcArc, gcOrc}:
      result.flags.incl tfHasAsgn

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

    let x = result
    result = newTypeWithSons(c, tyCompositeTypeClass, @[paramType, x])
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
      # turn the user-type-class invocation into a ``tyUserTypeClassInst``,
      # making handling of the type easier for ``typeRel``
      result = copyType(paramType, nextTypeId(c.idgen), body.owner)
      result.kind = tyUserTypeClassInst
      # an unresolved ``tyUserTypeClassInst`` currently expects the
      # ``tyUserTypeClass`` as the last element
      result.sons.add body.lastSon

      result = recurse(result, true)

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
  # TODO: replace with a node return variant that can in band errors
  # for historical "reasons" (excuses) this is invoked for parameter lists too
  # and then 'isType' is false; this is of course all terrible design
  checkMinSonsLen(n, 1, c.config)
  result = newProcType(c, n.info, prev)
  var
    check = initIntSet()
    counter = 0
    untypedParamPos = 0 ## the first untyped param, if one exists at all, used
                        ## to enforce: all params following an untyped param
                        ## must be untyped

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

      template isUntyped(t: PType): bool =
        t.kind == tyUntyped or
          t.kind == tyVarargs and t.len > 0 and t[0].kind == tyUntyped

      if untypedParamPos == 0:
        if isUntyped(typ):
          untypedParamPos = i
      elif not isUntyped(typ):
        localReport(c.config, a[^2],
                    reportTyp(rsemUntypedParamsFollwedByMoreSpecificType, typ))

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
      let
        givenArg = if a[j].kind == nkPragmaExpr: a[j][0] else: a[j]
        argNode = newSymGNode(skParam, givenArg, c)
        arg = getDefNameSymOrRecover(argNode)

      if argNode.kind == nkError:
        localReport(c.config, argNode)

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

proc semStmtListType(c: PContext, n: PNode, prev: PType): PType =
  checkMinSonsLen(n, 1, c.config)
  for i in 0..<n.len - 1:
    n[i] = semStmt(c, n[i], {})
  if n.len > 0:
    result = semTypeNode(c, n[^1], prev)
    n.typ = result
    n[^1].typ = result
  else:
    result = nil

proc semBlockType(c: PContext, n: PNode, prev: PType): PType =
  inc(c.p.nestedBlockCounter)
  checkSonsLen(n, 2, c.config)
  openScope(c)
  if n[0].kind notin {nkEmpty, nkSym}:
    addDecl(c, newSymS(skLabel, n[0], c))
  result = semStmtListType(c, n[1], prev)
  n[1].typ = result
  n.typ = result
  closeScope(c)
  dec(c.p.nestedBlockCounter)

proc semGenericParamInInvocation(c: PContext, n: PNode): PType =
  result = semTypeNode(c, n, nil)
  n.typ = makeTypeDesc(c, result)

proc semGeneric(c: PContext, n: PNode, s: PSym, prev: PType): PType =
  if s.typ == nil:
    localReport(c.config, n.info, reportSym(
      rsemCannotInstantiate, s))

    return newOrPrevType(tyError, prev, c)

  var t = s.typ.skipTypes({tyAlias})
  if t.kind == tyCompositeTypeClass and t.base.kind == tyGenericBody:
    t = t.base

  result = newOrPrevType(tyGenericInvocation, prev, c)
  addSonSkipIntLit(result, t, c.idgen)

  template addToResult(typ) =
    c.config.internalAssert typ != nil
    addSonSkipIntLit(result, typ, c.idgen)

  if t.kind == tyForward:
    for i in 1..<n.len:
      var elem = semGenericParamInInvocation(c, n[i])
      addToResult(elem)
    return
  elif t.kind != tyGenericBody:
    # we likely got code of the form TypeA[TypeB] where TypeA is
    # not generic.
    localReport(c.config, n.info, reportSym(
      rsemNoGenericParamsAllowed, s, typ = t))

    return newOrPrevType(tyError, prev, c)
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

      return newOrPrevType(tyError, prev, c)

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

    if isConcrete:
      if s.ast == nil and s.typ.kind != tyCompositeTypeClass:
        # XXX: What kind of error is this? is it still relevant?
        localReport(c.config, n.info, reportTyp(
          rsemCannotInstantiate, t))

        result = newOrPrevType(tyError, prev, c)
      else:
        result = instGenericContainer(c, n.info, result)

  # special check for generic object with
  # generic/partial specialized parent
  let tx = result.skipTypes(abstractPtrs, 50)
  if tx.isNil or isTupleRecursive(tx):
    localReport(c.config, n.info, reportTyp(
      rsemIllegalRecursion, result[0]))

    return errorType(c)

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

proc semTypeExpr(c: PContext, n: PNode; prev: PType): PType =
  var n = semExprWithType(c, n)
  if n.typ.kind == tyTypeDesc:
    result = n.typ.base
    # fix types constructed by macros/template:
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
  else:
    localReport(c.config, n, reportSem rsemTypeExpected)
    result = errorType(c)

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
  addInNimDebugUtils(c.config, "semTypeClass", n, prev, result)
  # if n.len == 0: return newConstraint(c, tyTypeClass)
  let
    pragmas = n[1]
    inherited = n[2]

  result = newOrPrevType(tyUserTypeClass, prev, c)
  result.flags.incl tfCheckedForDestructor
  let owner = getCurrOwner(c)
  var
    candidateTypeSlot = newTypeWithSons(owner, tyAlias, @[c.errorType], c.idgen)
    cycleDetector = initIntSet()
  result.sons = @[candidateTypeSlot]
  result.n = n

  if inherited.kind != nkEmpty:
    for n in inherited.sons:
      let t = semTypeNode(c, n, nil)
      if prev != nil and prev.aliasesType(t):
        c.config.localReport(n.info, reportTyp(rsemIllegalRecursion, t))
      result.add t

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

proc symFromType(c: PContext; t: PType, info: TLineInfo): PSym =
  if t.sym != nil: return t.sym
  result = newSym(skType, getIdent(c.cache, "AnonType"), nextSymId c.idgen, t.owner, info)
  result.flags.incl sfAnon
  result.typ = t

proc symFromExpectedTypeNode(c: PContext, n: PNode): PSym =
  if n.kind == nkType:
    result = symFromType(c, n.typ, n.info)
  else:
    let err = newError(c.config, n, PAstDiag(kind: adSemTypeExpected))
    localReport(c.config, err)
    result = errorSym(c, n, err)

proc semStaticType(c: PContext, childNode: PNode, prev: PType): PType =
  result = newOrPrevType(tyStatic, prev, c)
  var base = semTypeNode(c, childNode, nil).skipTypes({tyTypeDesc, tyAlias})
  result.rawAddSon(base)
  result.flags.incl tfHasStatic

proc semTypeOf(c: PContext; n: PNode; prev: PType): PType =
  # xxx: this should be folded into semTypeOf2
  openScope(c)
  case n.kind
  of nkError:
    result = n.typ
    if result.n.isNil:
      result.n = n
    closeScope(c)
  else:
    let t = semExprWithType(c, n, {efInTypeof})
    closeScope(c)
    
    case t.kind
    of nkError:
      result = t.typ
      if result.n.isNil:
        result.n = t
    else:
      fixupTypeOf(c, prev, t)
      result = t.typ

proc semTypeOf2(c: PContext; n: PNode; prev: PType): PType =
  openScope(c)
  var m = BiggestInt 1 # typeOfIter
  if n.len == 3:
    let mode = semConstExpr(c, n[2])
    if mode.kind != nkIntLit:
      localReport(c.config, VMReport(
        ast: n,
        location: some(n.info),
        kind: rvmCannotEvaluateAtComptime))

    else:
      m = mode.intVal
  
  case n[1].kind
  of nkError:
    result = n[1].typ
    if result.n.isNil:
      result.n = n[1] # at time of writing: error in TType.n is a new thing
    closeScope(c)
  else:
    let t = semExprWithType(c, n[1], if m == 1: {efInTypeof} else: {})
    closeScope(c)

    case t.kind
    of nkError:
      result = t.typ
      if result.n.isNil:
        result.n = t
    else:
      fixupTypeOf(c, prev, t)
      result = t.typ


proc semTypeNode(c: PContext, n: PNode, prev: PType): PType =
  addInNimDebugUtils(c.config, "semTypeNode", n, prev, result)

  proc makeAndType(c: PContext, t1, t2: PType): PType =
    result = newTypeS(tyAnd, c)
    result.sons = @[t1, t2]
    propagateToOwner(result, t1)
    propagateToOwner(result, t2)
    result.flags.incl ((t1.flags + t2.flags) * {tfHasStatic}) + {tfHasMeta}

  proc makeOrType(c: PContext, t1, t2: PType): PType =
    result = newTypeS(tyOr, c)
    if t1.kind != tyOr and t2.kind != tyOr:
      result.sons = @[t1, t2]
    else:
      template addOr(t1) =
        if t1.kind == tyOr:
          for x in t1.sons: result.rawAddSon x
        else:
          result.rawAddSon t1
      addOr(t1)
      addOr(t2)
    propagateToOwner(result, t1)
    propagateToOwner(result, t2)
    result.flags.incl ((t1.flags + t2.flags) * {tfHasStatic}) + {tfHasMeta}

  proc makeNotType(c: PContext, t1: PType): PType =
    result = newTypeS(tyNot, c)
    result.sons = @[t1]
    propagateToOwner(result, t1)
    result.flags.incl (t1.flags * {tfHasStatic}) + {tfHasMeta}

  result = nil
  inc c.inTypeContext

  if c.config.cmd == cmdIdeTools: suggestExpr(c, n)
  case n.kind
  of nkEmpty: result = n.typ
  of nkTypeOfExpr:
    # for ``typeof(countup(1,3))``, see ``tests/ttoseq``.
    checkSonsLen(n, 1, c.config)
    result = semTypeOf(c, n[0], prev)
    if result.kind == tyTypeDesc: result.flags.incl tfExplicit
  of nkPar:
    if n.len == 1:
      result = semTypeNode(c, n[0], prev)
    else:
      result = semAnonTuple(c, n, prev)
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
      result = semTypeNode(c, b, prev)
    elif ident != nil and ident.id == ord(wDotDot):
      result = semRangeAux(c, n, prev)
    elif n[0].kind == nkNilLit and n.len == 2:
      result = semTypeNode(c, n[1], prev)
      if result.skipTypes({tyGenericInst, tyAlias, tySink}).kind in NilableTypes+GenericTypes:
        if tfNotNil in result.flags:
          result = freshType(c, result, prev)
          result.flags.excl(tfNotNil)
      else:
        localReport(c.config, n, reportSem rsemTypeInvalid)
    elif n[0].kind notin nkIdentKinds:
      result = semTypeExpr(c, n, prev)
    else:
      let (op, err) = considerQuotedIdent(c, n[0])
      if err != nil:
        localReport(c.config, err)
      if op.id in {ord(wAnd), ord(wOr)} or op.s == "|":
        checkSonsLen(n, 3, c.config)
        var
          t1 = semTypeNode(c, n[1], nil)
          t2 = semTypeNode(c, n[2], nil)
        if t1 == nil:
          localReport(c.config, n[1], reportSem rsemTypeExpected)
          result = newOrPrevType(tyError, prev, c)
        elif t2 == nil:
          localReport(c.config, n[2], reportSem rsemTypeExpected)
          result = newOrPrevType(tyError, prev, c)
        else:
          result =
            if prev != nil and (prev.aliasesType(t1) or prev.aliasesType(t2)):
              c.config.localReport(n.info,
                                   reportTyp(rsemIllegalRecursion, prev))
              newOrPrevType(tyError, prev, c)
            else:
              if op.id == ord(wAnd): makeAndType(c, t1, t2)
              else:                  makeOrType(c, t1, t2)

      elif op.id == ord(wNot):
        case n.len
        of 3:
          result = semTypeNode(c, n[1], prev)
          if result.kind == tyTypeDesc and tfUnresolved notin result.flags:
            result = result.base
          if n[2].kind != nkNilLit:
            localReport(c.config, n, reportSem rsemMalformedNotNilType)
          if notnil notin c.features and strictNotNil notin c.features:
            localReport(c.config, n, reportSem rsemEnableNotNilExperimental)
          let resolvedType = result.skipTypes({tyGenericInst, tyAlias, tySink})
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
            result = makeTypeFromExpr(c, newTree(nkStmtListType, n.copyTree))
          of NilableTypes + {tyGenericInvocation, tyForward}:
            result = freshType(c, result, prev)
            result.flags.incl(tfNotNil)
          else:
            localReport(c.config, n, reportSem rsemTypeInvalid)
        of 2:
          let negated = semTypeNode(c, n[1], prev)
          result = makeNotType(c, negated)
        else:
          localReport(c.config, n, reportSem rsemTypeInvalid)
      elif op.id == ord(wPtr):
        result = semAnyRef(c, n, tyPtr, prev)
      elif op.id == ord(wRef):
        result = semAnyRef(c, n, tyRef, prev)
      elif op.id == ord(wType):
        checkSonsLen(n, 2, c.config)
        result = semTypeOf(c, n[1], prev)
      elif op.s == "typeof" and n[0].kind == nkSym and n[0].sym.magic == mTypeOf:
        result = semTypeOf2(c, n, prev)
      elif op.s == "owned" and n.len == 2:
        # skip 'owned' in type expressions and produce a warning
        localReport(c.config, n, reportSem rsemOwnedTypeDeprecated)
        result = semTypeExpr(c, n[1], prev)
      elif (op.s == "sink" or op.s == "lent") and n.len == 2:
        # it's a 'sink T' or 'lent T' expression
        # XXX: consider introducing special words for both (i.e., ``wLent``,
        #      ``wSink``)
        result = newOrPrevType((if op.s == "sink": tySink else: tyLent), nil, c)
        result.rawAddSonNoPropagationOfTypeFlags semTypeNode(c, n[1], nil)
      else:
        if c.inGenericContext > 0 and n.kind == nkCall:
          result = makeTypeFromExpr(c, n.copyTree)
        else:
          result = semTypeExpr(c, n, prev)
  of nkWhenStmt:
    var whenResult = semWhen(c, n, false)
    if whenResult.kind == nkStmtList:
      whenResult.transitionSonsKind(nkStmtListType)
    result = semTypeNode(c, whenResult, prev)
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
        incl result.flags, tfHasAsgn
    of mVarargs: result = semVarargs(c, n, prev)
    of mTypeDesc, mType, mTypeOf:
      result = makeTypeDesc(c, semTypeNode(c, n[1], nil))
      result.flags.incl tfExplicit
    of mStatic:
      result = semStaticType(c, n[1], prev)
    of mExpr:
      result = semTypeNode(c, n[0], nil)
      if result != nil:
        let old = result
        result = copyType(result, nextTypeId c.idgen, getCurrOwner(c))
        copyTypeProps(c.graph, c.idgen.module, result, old)
        for i in 1..<n.len:
          result.rawAddSon(semTypeNode(c, n[i], nil))
    of mDistinct:
      result = newOrPrevType(tyDistinct, prev, c)
      addSonSkipIntLit(result, semTypeNode(c, n[1], nil), c.idgen)
    of mVar:
      result = newOrPrevType(tyVar, prev, c)
      var base = semTypeNode(c, n[1], nil)
      if base.kind in {tyVar, tyLent}:
        localReport(c.config, n.info, reportTyp(rsemVarVarNotAllowed, prev))

        base = base[0]
      addSonSkipIntLit(result, base, c.idgen)
    of mRef: result = semAnyRef(c, n, tyRef, prev)
    of mPtr: result = semAnyRef(c, n, tyPtr, prev)
    of mTuple: result = semTuple(c, n, prev)
    else: result = semGeneric(c, n, s, prev)
  of nkDotExpr:
    let typeExpr = semExpr(c, n)
    if typeExpr.typ.isNil:
      localReport(c.config, n.info, SemReport(kind: rsemExpectedObjectType))
      result = errorType(c)
    elif typeExpr.typ.kind == tyFromExpr:
      result = typeExpr.typ
    elif typeExpr.typ.kind != tyTypeDesc:
      localReport(c.config, n, reportSem rsemTypeExpected)
      result = errorType(c)
    else:
      result = typeExpr.typ.base
      if result.isMetaType and
         result.kind != tyUserTypeClass:
           # the dot expression may refer to a concept type in
           # a different module. allow a normal alias then.
        let preprocessed = semGenericStmt(c, n)
        if preprocessed.isError:
          localReport(c.config, preprocessed)
        result = makeTypeFromExpr(c, preprocessed.copyTree)
      else:
        let alias = maybeAliasType(c, result, prev)
        if alias != nil: result = alias
  of nkIdent, nkAccQuoted:
    var s = semTypeIdent(c, n)
    if s.typ == nil:
      if s.kind != skError:
        localReport(c.config, n, reportSem rsemTypeExpected)

      result = newOrPrevType(tyError, prev, c)
    elif s.kind == skParam and s.typ.kind == tyTypeDesc:
      c.config.internalAssert s.typ.base.kind != tyNone and prev == nil
      result = s.typ.base
    elif prev == nil:
      result = s.typ
    else:
      let alias = maybeAliasType(c, s.typ, prev)
      if alias != nil:
        result = alias
      else:
        assignType(prev, s.typ)
        # bugfix: keep the fresh id for aliases to integral types:
        if s.typ.kind notin {tyBool, tyChar, tyInt..tyInt64, tyFloat..tyFloat128,
                             tyUInt..tyUInt64}:
          prev.itemId = s.typ.itemId
        result = prev
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
      if alias != nil:
        result = alias
      elif prev == nil:
        result = t
      else:
        assignType(prev, t)
        result = prev
      markUsed(c, n.info, n.sym)
    else:
      if s.kind != skError:
        localReport(c.config, n.info, reportSym(rsemTypeExpected, s))

      result = newOrPrevType(tyError, prev, c)
  of nkObjectTy: result = semObjectNode(c, n, prev, {})
  of nkTupleTy: result = semTuple(c, n, prev)
  of nkTupleClassTy: result = newConstraint(c, tyTuple)
  of nkTypeClassTy: result = semTypeClass(c, n, prev)
  of nkRefTy: result = semAnyRef(c, n, tyRef, prev)
  of nkPtrTy: result = semAnyRef(c, n, tyPtr, prev)
  of nkVarTy: result = semVarOutType(c, n, prev, tyVar)
  of nkDistinctTy: result = semDistinct(c, n, prev)
  of nkStaticTy: result = semStaticType(c, n[0], prev)
  of nkIteratorTy:
    if n.len == 0:
      result = newTypeS(tyBuiltInTypeClass, c)
      let child = newTypeS(tyProc, c)
      child.flags.incl tfIterator
      result.addSonSkipIntLit(child, c.idgen)
    else:
      result = semProcTypeWithScope(c, n, prev, skIterator)
      if result.kind == tyProc:
        result.flags.incl(tfIterator)
        if n.lastSon.kind == nkPragma and hasPragma(n.lastSon, wInline):
          result.callConv = ccInline
        else:
          result.callConv = ccClosure
  of nkProcTy:
    if n.len == 0:
      result = newConstraint(c, tyProc)
    else:
      result = semProcTypeWithScope(c, n, prev, skProc)
  of nkEnumTy: result = semEnum(c, n, prev)
  of nkType: result = n.typ
  of nkStmtListType: result = semStmtListType(c, n, prev)
  of nkBlockType: result = semBlockType(c, n, prev)
  of nkError:
    localReport(c.config, n, reportSem rsemTypeExpected)
    result = newOrPrevType(tyError, prev, c)
    result.n = n # set the error and read it out else where
  else:
    localReport(c.config, n, reportSem rsemTypeExpected)
    result = newOrPrevType(tyError, prev, c)
  n.typ = result
  dec c.inTypeContext

proc semTypeNode2(c: PContext, n: PNode, prev: PType): PNode =
  ## Semantically analyzes the type AST `n`, and returns either the analyzed
  ## AST or an error.
  ##
  ## XXX: this procedure is meant to eventually replace the current
  ##      ``semTypeNode``, which mutates the input AST and returns a type
  let typ = semTypeNode(c, n, prev)
  if typ.isNil:
    result = newError(c.config, n, PAstDiag(kind: adSemTypeExpected))
  elif typ.kind == tyError and typ.n.kind == nkError:
    result = typ.n
  else:
    # the type is either valid or an error type that doesn't store a
    # diagnostic. For the latter, ``localReport`` was already used to report
    # the error, so no extra error node is created here
    result = copyTree(n)

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
  # TODO: replace with a node return variant that can in band errors

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

        let
          sKind =
            if finalType.kind == tyStatic or tfWildcard in typ.flags:
              skGenericParam
            else:
              skType
          sNode = newSymGNode(sKind, paramName, c)
          s = getDefNameSymOrRecover(sNode).linkTo(finalType)

        if sNode.kind == nkError:
          c.config.localReport(sNode)

        if covarianceFlag != tfUnresolved: s.typ.flags.incl(covarianceFlag)
        if def.kind != nkEmpty: s.ast = def
        s.position = result.len
        result.addSym(s)
    else:
      semReportIllformedAst(c.config, n, "")
