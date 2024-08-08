#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module does the semantic transformation of the fields* iterators.
##  included from semstmts.nim

type
  TFieldInstCtx = object  # either 'tup[i]' or 'field' is valid
    tupleType: PType      # if != nil we're traversing a tuple
    tupleIndex: int
    field: PSym
    replaceByFieldName: bool
    c: PContext

proc instFieldLoopBody(c: TFieldInstCtx, n: PNode, forLoop: PNode): PNode =
  if c.field != nil and isEmptyType(c.field.typ):
    result = newNode(nkEmpty)
    return
  case n.kind
  of nkWithoutSons - {nkIdent, nkSym}:
    result = copyNode(n)
  of nkIdent, nkSym:
    result = n
    let (ident, err) = considerQuotedIdent(c.c, n)
    if err != nil:
      localReport(c.c.config, err)
    if c.replaceByFieldName:
      if ident.id == legacyConsiderQuotedIdent(c.c, forLoop[0], nil).id:
        let fieldName = if c.tupleType.isNil: c.field.name.s
                        elif c.tupleType.n.isNil: "Field" & $c.tupleIndex
                        else: c.tupleType.n[c.tupleIndex].sym.name.s
        result = newStrNode(nkStrLit, fieldName)
        return
    # other fields:
    for i in ord(c.replaceByFieldName)..<forLoop.len-2:
      if ident.id == legacyConsiderQuotedIdent(c.c, forLoop[i], nil).id:
        var call = forLoop[^2]
        var tupl = call[i+1-ord(c.replaceByFieldName)]
        if c.field.isNil:
          result = newNodeI(nkBracketExpr, n.info)
          result.add(tupl)
          result.add(newIntNode(nkIntLit, c.tupleIndex))
        else:
          result = newNodeI(nkDotExpr, n.info)
          result.add(tupl)
          result.add(newSymNode(c.field, n.info))
        break
  of nkWithSons:
    if n.kind == nkContinueStmt:
      localReport(c.c.config, n, reportSem rsemFieldsIteratorCannotContinue)
    result = shallowCopy(n)
    for i in 0..<n.len:
      result[i] = instFieldLoopBody(c, n[i], forLoop)

type
  TFieldsCtx = object
    c: PContext
    m: TMagic

proc semForObjectFields(c: TFieldsCtx, typ, forLoop, father: PNode) =
  case typ.kind
  of nkSym:
    var fc: TFieldInstCtx  # either 'tup[i]' or 'field' is valid
    fc.c = c.c
    fc.field = typ.sym
    fc.replaceByFieldName = c.m == mFieldPairs
    openScope(c.c)
    inc c.c.inUnrolledContext
    let body = instFieldLoopBody(fc, lastSon(forLoop), forLoop)
    father.add(semStmt(c.c, body, {}))
    dec c.c.inUnrolledContext
    closeScope(c.c)
  of nkNilLit: discard
  of nkRecCase:
    let call = forLoop[^2]
    if call.len > 2:
      localReport(c.c.config, forLoop.info, reportAst(
        rsemParallelFieldsDisallowsCase, call))
      return
    # iterate over the selector:
    semForObjectFields(c, typ[0], forLoop, father)
    # we need to generate a case statement:
    var caseStmt = newNodeI(nkCaseStmt, forLoop.info)
    # generate selector:
    var access = newNodeI(nkDotExpr, forLoop.info, 2)
    access[0] = call[1]
    access[1] = newSymNode(typ[0].sym, forLoop.info)
    caseStmt.add(semExprWithType(c.c, access))
    # copy the branches over, but replace the fields with the for loop body:
    for i in 1..<typ.len:
      var branch = copyTree(typ[i])
      branch[^1] = newNodeI(nkStmtList, forLoop.info)
      semForObjectFields(c, typ[i].lastSon, forLoop, branch[^1])
      caseStmt.add(branch)
    father.add(caseStmt)
  of nkRecList:
    for t in items(typ):
      semForObjectFields(c, t, forLoop, father)
  else:
    semReportIllformedAst(c.c.config, typ, {
      nkRecList, nkRecCase, nkNilLit, nkSym})

proc semForFields(c: PContext, n, call: PNode, flags: TExprFlags): PNode =
  # so that 'break' etc. work as expected, we produce
  # a 'while true: stmt; break' loop ...
  let
    m = call[0].sym.magic
    info = n.info
    semmedLoop = copyNodeWithKids(n)
  semmedLoop[^2] = call
  result = newNodeI(nkWhileStmt, info, 2)

  var trueSymbol = systemModuleSym(c.graph, getIdent(c.cache, "true"))
  if trueSymbol.isNil:
    localReport(c.config, info, reportStr(rsemSystemNeeds, "true"))
    trueSymbol = newSym(
      skUnknown, getIdent(c.cache, "true"),
      nextSymId c.idgen, getCurrOwner(c), info)
    trueSymbol.typ = getSysType(c.graph, info, tyBool)

  result[0] = newSymNode(trueSymbol, info)
  var stmts = newNodeI(nkStmtList, info)
  result[1] = stmts

  if n.len-2 != call.len - 1 + ord(m == mFieldPairs):
    localReport(c.config, info, semReportCountMismatch(
      rsemWrongNumberOfVariables,
      expected = call.len - 1 + ord(m == mFieldPairs),
      got = n.len - 2))
    return result

  const skippedTypesForFields = abstractVar - {tyTypeDesc} + tyUserTypeClasses
  var tupleTypeA = skipTypes(call[1].typ, skippedTypesForFields)
  if tupleTypeA.kind notin {tyTuple, tyObject}:
    localReport(c.config, info, reportSem(rsemNoObjectOrTupleType))
    return result
  for i in 1..<call.len:
    let calli = call[i]
    var tupleTypeB = skipTypes(calli.typ, skippedTypesForFields)
    if not sameType(tupleTypeA, tupleTypeB):
      let r = typeMismatch(c.config, calli.info, tupleTypeA, tupleTypeB, calli)
      if r.kind == nkError:
        localReport(c.config, r)
  inc(c.execCon.nestedLoopCounter)
  if tupleTypeA.kind == tyTuple:
    var loopBody = semmedLoop[^1]
    for i in 0..<tupleTypeA.len:
      openScope(c)
      var fc: TFieldInstCtx
      fc.tupleType = tupleTypeA
      fc.tupleIndex = i
      fc.c = c
      fc.replaceByFieldName = m == mFieldPairs
      var body = instFieldLoopBody(fc, loopBody, semmedLoop)
      inc c.inUnrolledContext
      stmts.add(semStmt(c, body, {}))
      dec c.inUnrolledContext
      closeScope(c)
  else:
    var fc: TFieldsCtx
    fc.m = m
    fc.c = c
    var t = tupleTypeA
    while t.kind == tyObject:
      semForObjectFields(fc, t.n, semmedLoop, stmts)
      if t[0] == nil: break
      t = skipTypes(t[0], skipPtrs)
  dec(c.execCon.nestedLoopCounter)
  # for TR macros this 'while true: ...; break' loop is pretty bad, so
  # we avoid it now if we can:
  if containsNode(stmts, {nkBreakStmt}):
    var b = newNodeI(nkBreakStmt, info)
    b.add(newNodeI(nkEmpty, info))
    stmts.add(b)
  else:
    result = stmts

  if containsNode(result, {nkError}):
    result = c.config.wrapError(result)
  elif semmedLoop[^1].typ == c.enforceVoidContext:
    # propagate any enforced VoidContext:
    result.typ = c.enforceVoidContext
  elif result.len > 0 and efInTypeof in flags:
    result.typ = result.lastSon.typ
