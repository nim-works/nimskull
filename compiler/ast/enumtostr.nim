import
  compiler/ast/[
    ast,
    idents,
    lineinfos
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ]


proc genEnumToStrProc*(t: PType; info: TLineInfo; g: ModuleGraph; idgen: IdGenerator): PSym =
  # the compiler-generated enum-to-string procedure is treated as an
  # instantiation of the generic ``mEnumToStr`` magic
  let owner = getSysMagic(g, info, "$", mEnumToStr)
  result = newSym(skProc, getIdent(g.cache, "$"), nextSymId idgen, owner, info)

  let dest = newSym(skParam, getIdent(g.cache, "e"), nextSymId idgen, result, info)
  dest.typ = t

  let res = newSym(skResult, getIdent(g.cache, "result"), nextSymId idgen, result, info)
  res.typ = getSysType(g, info, tyString)

  result.typ = newType(tyProc, nextTypeId idgen, t.owner)
  result.typ.n = newNodeI(nkFormalParams, info)
  rawAddSon(result.typ, res.typ)
  result.typ.n.add newNodeI(nkEffectList, info)

  result.typ.addParam dest

  var caseStmt = newNodeI(nkCaseStmt, info)
  caseStmt.add(newSymNode dest)

  # copy the branches over, but replace the fields with the for loop body:
  for i in 0..<t.n.len:
    assert(t.n[i].kind == nkSym)
    let field = t.n[i].sym
    let val = if field.ast == nil: field.name.s else: field.ast.strVal
    caseStmt.add newTree(nkOfBranch, newIntTypeNode(field.position, t),
      newTree(nkStmtList, newTree(nkFastAsgn,
                                  newSymNode(res),
                                  newStrNode(val, res.typ, info))))

  let body = newTreeI(nkStmtList, info): caseStmt

  var n = newNodeI(nkProcDef, info, bodyPos+2)
  for i in 0..<n.len: n[i] = newNodeI(nkEmpty, info)
  n[namePos] = newSymNode(result)
  n[paramsPos] = result.typ.n
  n[bodyPos] = body
  n[resultPos] = newSymNode(res)
  result.ast = n
  # treat the symbol as a magic so that further processing can easily detect
  # auto-generated enum-to-string procedures
  result.magic = mEnumToStr
  result.flags.incl {sfFromGeneric, sfNeverRaises}

proc searchObjCaseImpl(obj: PNode; field: PSym): PNode =
  case obj.kind
  of nkSym:
    result = nil
  of nkElse, nkOfBranch:
    result = searchObjCaseImpl(obj.lastSon, field)
  else:
    if obj.kind == nkRecCase and obj[0].kind == nkSym and obj[0].sym == field:
      result = obj
    else:
      for x in obj:
        result = searchObjCaseImpl(x, field)
        if result != nil: break

proc searchObjCase(t: PType; field: PSym): PNode =
  result = searchObjCaseImpl(t.n, field)
  if result == nil and t.len > 0:
    result = searchObjCase(t[0].skipTypes({tyAlias, tyGenericInst, tyRef, tyPtr}), field)
  doAssert result != nil

proc genCaseObjDiscMapping*(t: PType; field: PSym; info: TLineInfo; g: ModuleGraph; idgen: IdGenerator): PSym =
  result = newSym(skProc, getIdent(g.cache, "objDiscMapping"), nextSymId idgen, t.owner, info)

  let dest = newSym(skParam, getIdent(g.cache, "e"), nextSymId idgen, result, info)
  dest.typ = field.typ

  let res = newSym(skResult, getIdent(g.cache, "result"), nextSymId idgen, result, info)
  res.typ = getSysType(g, info, tyUInt8)

  result.typ = newType(tyProc, nextTypeId idgen, t.owner)
  result.typ.n = newNodeI(nkFormalParams, info)
  rawAddSon(result.typ, res.typ)
  result.typ.n.add newNodeI(nkEffectList, info)

  result.typ.addParam dest

  var caseStmt = newNodeI(nkCaseStmt, info)
  caseStmt.add(newSymNode dest)

  let subObj = searchObjCase(t, field)
  for i in 1..<subObj.len:
    let ofBranch = subObj[i]
    var newBranch = newNodeI(ofBranch.kind, ofBranch.info)
    for j in 0..<ofBranch.len-1:
      newBranch.add ofBranch[j]

    newBranch.add newTree(nkStmtList, newTree(nkFastAsgn, newSymNode(res), newIntNode(nkInt8Lit, i)))
    caseStmt.add newBranch

  let body = newTreeI(nkStmtList, info): caseStmt

  var n = newNodeI(nkProcDef, info, bodyPos+2)
  for i in 0..<n.len: n[i] = newNodeI(nkEmpty, info)
  n[namePos] = newSymNode(result)
  n[paramsPos] = result.typ.n
  n[bodyPos] = body
  n[resultPos] = newSymNode(res)
  result.ast = n
  result.flags.incl {sfFromGeneric, sfNeverRaises}
