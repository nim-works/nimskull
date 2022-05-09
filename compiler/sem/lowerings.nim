#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements common simple lowerings.

const
  genPrefix* = ":tmp"         ## prefix for generated names

import
  compiler/ast/[
    ast,
    astalgo,
    types,
    idents,
    lineinfos,
    reports
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/front/[
    msgs,
    options
  ]

proc newDeref*(n: PNode): PNode {.inline.} =
  result = newTreeIT(nkHiddenDeref, n.info, n.typ[0]): n

proc newTupleAccessRaw*(tup: PNode, i: int): PNode =
  var lit = newNodeI(nkIntLit, tup.info)
  lit.intVal = i
  result = newTreeI(nkBracketExpr, tup.info):
    [copyTree(tup), lit]

proc newTupleAccess*(g: ModuleGraph; tup: PNode, i: int): PNode =
  if tup.kind == nkHiddenAddr:
    var lit = newNodeIT(nkIntLit, tup.info, getSysType(g, tup.info, tyInt))
    lit.intVal = i
    result = newTreeIT(nkHiddenAddr, tup.info, tup.typ.skipTypes(abstractInst+{tyPtr, tyVar, tyLent})):
      newTreeIT(nkBracketExpr, tup.info, tup.typ.skipTypes(abstractInst+{tyPtr, tyVar, tyLent})[i]):
        [tup[0], lit]
  else:
    var lit = newNodeIT(nkIntLit, tup.info, getSysType(g, tup.info, tyInt))
    lit.intVal = i
    result = newTreeIT(nkBracketExpr, tup.info, tup.typ.skipTypes(abstractInst)[i]):
      [copyTree(tup), lit]

proc newIdentDefs*(v: PNode): PNode =
  let emptyNode = newNodeI(nkEmpty, v.info)
  result = newTreeI(nkIdentDefs, v.info):
    [v, emptyNode, emptyNode]

proc newIdentDefs*(v, value: PNode): PNode =
  result = newTreeI(nkIdentDefs, v.info):
    [v, newNodeI(nkEmpty, v.info), value]

proc newAsgnStmt*(le, ri: PNode): PNode =
  result = newTreeI(nkAsgn, le.info): [le, ri]

proc newFastAsgnStmt*(le, ri: PNode): PNode =
  result = newTreeI(nkFastAsgn, le.info): [le, ri]

proc newFastMoveStmt*(g: ModuleGraph, le, ri: PNode): PNode =
  result = newTreeI(nkFastAsgn, le.info):
    [le,
     newTreeIT(nkCall, ri.info, ri.typ,
       newSymNode(getSysMagic(g, ri.info, "move", mMove)), ri)]

proc lowerTupleUnpacking*(g: ModuleGraph; n: PNode; idgen: IdGenerator; owner: PSym): PNode =
  assert n.kind == nkVarTuple
  let value = n.lastSon
  result = newNodeI(nkStmtList, n.info)

  var temp = newSym(skTemp, getIdent(g.cache, genPrefix), nextSymId(idgen),
                    owner, value.info, g.config.options)
  temp.typ = skipTypes(value.typ, abstractInst)
  incl(temp.flags, sfFromGeneric)

  let tempAsNode = newSymNode(temp)
  var v = newTreeI(nkVarSection, value.info):
    newIdentDefs(tempAsNode, value)

  for i in 0..<n.len-2:
    let val = newTupleAccess(g, tempAsNode, i)
    if n[i].kind == nkSym: v.add newIdentDefs(n[i], val)
    else: result.add newAsgnStmt(n[i], val)

  result.add(v)

proc evalOnce*(g: ModuleGraph; value: PNode; idgen: IdGenerator; owner: PSym): PNode =
  ## Turns (value) into (let tmp = value; tmp) so that 'value' can be re-used
  ## freely, multiple times. This is frequently required and such a builtin would also be
  ## handy to have in macros.nim. The value that can be reused is 'result.lastSon'!
  var temp = newSym(skTemp, getIdent(g.cache, genPrefix), nextSymId(idgen),
                    owner, value.info, g.config.options)
  temp.typ = skipTypes(value.typ, abstractInst)
  incl(temp.flags, sfFromGeneric)

  let tempAsNode = newSymNode(temp)
  let v = newTreeI(nkLetSection, value.info):
    newIdentDefs(tempAsNode)
  result = newTreeIT(nkStmtListExpr, value.info, value.typ):
    [v, newAsgnStmt(tempAsNode, value), tempAsNode]

proc newTryFinally*(body, final: PNode): PNode =
  result = newTree(nkHiddenTryStmt, body, newTree(nkFinally, final))

proc lowerTupleUnpackingForAsgn*(g: ModuleGraph; n: PNode; idgen: IdGenerator; owner: PSym): PNode =
  let value = n.lastSon

  let temp = newSym(skTemp, getIdent(g.cache, "_"), nextSymId(idgen), owner, value.info, owner.options)
  let tempAsNode = newSymNode(temp) #newIdentNode(getIdent(genPrefix & $temp.id), value.info)

  let v = newTreeI(nkLetSection, value.info):
    newIdentDefs(tempAsNode, value)
  result = newTreeI(nkStmtList, n.info): v

  let lhs = n[0]
  for i in 0..<lhs.len:
    result.add newAsgnStmt(lhs[i], newTupleAccessRaw(tempAsNode, i))

proc lowerSwap*(g: ModuleGraph; n: PNode; idgen: IdGenerator; owner: PSym): PNode =
  # note: cannot use 'skTemp' here cause we really need the copy for the VM :-(
  var temp = newSym(skVar, getIdent(g.cache, genPrefix), nextSymId(idgen), owner, n.info, owner.options)
  temp.typ = n[1].typ
  temp.flags.incl {sfFromGeneric, sfGenSym}

  let tempAsNode = newSymNode(temp)

  let v = newTreeI(nkVarSection, n.info):
    newIdentDefs(tempAsNode, n[1])

  result = newTreeI(nkStmtList, n.info):
    [v, newFastAsgnStmt(n[1], n[2]), newFastAsgnStmt(n[2], tempAsNode)]

proc createObj*(g: ModuleGraph; idgen: IdGenerator; owner: PSym, info: TLineInfo; final=true): PType =
  result = newType(tyObject, nextTypeId(idgen), owner)
  if final:
    rawAddSon(result, nil)
    incl result.flags, tfFinal
  else:
    rawAddSon(result, getCompilerProc(g, "RootObj").typ)
  result.n = newNodeI(nkRecList, info)
  let s = newSym(skType, getIdent(g.cache, "Env_" & toFilename(g.config, info) & "_" & $owner.name.s),
                  nextSymId(idgen),
                  owner, info, owner.options)
  incl s.flags, sfAnon
  s.typ = result
  result.sym = s

template fieldCheck {.dirty.} =
  when false:
    if tfCheckedForDestructor in obj.flags:
      echo "missed field ", field.name.s
      writeStackTrace()

proc rawAddField*(obj: PType; field: PSym) =
  assert field.kind == skField
  field.position = obj.n.len
  obj.n.add newSymNode(field)
  propagateToOwner(obj, field.typ)
  fieldCheck()

proc rawIndirectAccess*(a: PNode; field: PSym; info: TLineInfo): PNode =
  # returns a[].field as a node
  assert field.kind == skField
  let deref = newTreeIT(nkHiddenDeref, info, a.typ.skipTypes(abstractInst)[0]): a
  result = newTreeIT(nkDotExpr, info, field.typ):
    [deref, newSymNode(field)]

proc rawDirectAccess*(obj, field: PSym): PNode =
  # returns a.field as a node
  assert field.kind == skField
  result = newTreeIT(nkDotExpr, field.info, field.typ):
    [newSymNode(obj), newSymNode(field)]

proc lookupInRecord(n: PNode, id: ItemId): PSym =
  result = nil
  case n.kind
  of nkRecList:
    for i in 0..<n.len:
      result = lookupInRecord(n[i], id)
      if result != nil: return
  of nkRecCase:
    if n[0].kind != nkSym: return
    result = lookupInRecord(n[0], id)
    if result != nil: return
    for i in 1..<n.len:
      case n[i].kind
      of nkOfBranch, nkElse:
        result = lookupInRecord(lastSon(n[i]), id)
        if result != nil: return
      else: discard
  of nkSym:
    if n.sym.itemId.module == id.module and n.sym.itemId.item == -abs(id.item): result = n.sym
  else: discard

proc addField*(obj: PType; s: PSym; cache: IdentCache; idgen: IdGenerator) =
  # because of 'gensym' support, we have to mangle the name with its ID.
  # This is hacky but the clean solution is much more complex than it looks.
  var field = newSym(skField, getIdent(cache, s.name.s & $obj.n.len),
                     nextSymId(idgen), s.owner, s.info, s.options)
  field.itemId = ItemId(module: s.itemId.module, item: -s.itemId.item)
  let t = skipIntLit(s.typ, idgen)
  field.typ = t
  assert t.kind != tyTyped
  propagateToOwner(obj, t)
  field.position = obj.n.len
  field.flags = s.flags * {sfCursor}
  obj.n.add newSymNode(field)
  fieldCheck()

proc addUniqueField*(obj: PType; s: PSym; cache: IdentCache; idgen: IdGenerator): PSym {.discardable.} =
  result = lookupInRecord(obj.n, s.itemId)
  if result == nil:
    var field = newSym(skField, getIdent(cache, s.name.s & $obj.n.len), nextSymId(idgen),
                       s.owner, s.info, s.options)
    field.itemId = ItemId(module: s.itemId.module, item: -s.itemId.item)
    let t = skipIntLit(s.typ, idgen)
    field.typ = t
    assert t.kind != tyTyped
    propagateToOwner(obj, t)
    field.position = obj.n.len
    obj.n.add newSymNode(field)
    result = field

proc newDotExpr*(obj, b: PSym): PNode =
  let field = lookupInRecord(obj.typ.n, b.itemId)
  assert field != nil, b.name.s
  result = newTreeIT(nkDotExpr, obj.info, field.typ):
    [newSymNode(obj), newSymNode(field)]

proc indirectAccess*(a: PNode, b: ItemId, info: TLineInfo): PNode =
  # returns a[].b as a node
  let derefTyp = a.typ.skipTypes(abstractInst)[0]
  var t = derefTyp.skipTypes(abstractInst)
  var field: PSym
  while true:
    assert t.kind == tyObject
    field = lookupInRecord(t.n, b)
    if field != nil: break
    t = t[0]
    if t == nil: break
    t = t.skipTypes(skipPtrs)
  assert field != nil
  let deref = newTreeIT(nkHiddenDeref, info, derefTyp): a
  result = newTreeIT(nkDotExpr, info, field.typ):
    [deref, newSymNode(field)]

proc indirectAccess*(a: PNode, b: string, info: TLineInfo; cache: IdentCache): PNode =
  # returns a[].b as a node
  let derefTyp = a.typ.skipTypes(abstractInst)[0]
  var t = derefTyp.skipTypes(abstractInst)
  var field: PSym
  let bb = getIdent(cache, b)
  while true:
    assert t.kind == tyObject
    field = getSymFromList(t.n, bb)
    if field != nil: break
    t = t[0]
    if t == nil: break
    t = t.skipTypes(skipPtrs)
  assert field != nil
  let deref = newTreeIT(nkHiddenDeref, info, derefTyp): a
  result = newTreeIT(nkDotExpr, info, field.typ):
    [deref, newSymNode(field)]

proc getFieldFromObj*(t: PType; v: PSym): PSym =
  assert v.kind != skField
  var t = t
  while true:
    assert t.kind == tyObject
    result = lookupInRecord(t.n, v.itemId)
    if result != nil: break
    t = t[0]
    if t == nil: break
    t = t.skipTypes(skipPtrs)

proc indirectAccess*(a: PNode, b: PSym, info: TLineInfo): PNode =
  # returns a[].b as a node
  result = indirectAccess(a, b.itemId, info)

proc indirectAccess*(a, b: PSym, info: TLineInfo): PNode =
  result = indirectAccess(newSymNode(a), b, info)

proc genAddrOf*(n: PNode; idgen: IdGenerator; typeKind = tyPtr): PNode =
  var resTyp = newType(typeKind, nextTypeId(idgen), n.typ.owner)
  resTyp.rawAddSon(n.typ)
  result = newTreeIT(nkAddr, n.info, resTyp): n

proc genDeref*(n: PNode; k = nkHiddenDeref): PNode =
  result = newTreeIT(k, n.info, n.typ.skipTypes(abstractInst)[0]): n

proc callCodegenProc*(g: ModuleGraph; name: string;
                      info: TLineInfo = unknownLineInfo;
                      arg1, arg2, arg3, optionalArgs: PNode = nil): PNode =
  result = newNodeI(nkCall, info)
  let sym = magicsys.getCompilerProc(g, name)
  if sym == nil:
    g.config.localReport(info, reportStr(rsemSystemNeeds, name))

  else:
    result.add newSymNode(sym)
    if arg1 != nil: result.add arg1
    if arg2 != nil: result.add arg2
    if arg3 != nil: result.add arg3
    if optionalArgs != nil:
      for i in 1..<optionalArgs.len-2:
        result.add optionalArgs[i]
    result.typ = sym.typ[0]

proc newIntLit*(g: ModuleGraph; info: TLineInfo; value: BiggestInt): PNode =
  result = nkIntLit.newIntNode(value)
  result.typ = getSysType(g, info, tyInt)

proc genHigh*(g: ModuleGraph; n: PNode): PNode =
  if skipTypes(n.typ, abstractVar).kind == tyArray:
    result = newIntLit(g, n.info, toInt64(lastOrd(g.config, skipTypes(n.typ, abstractVar))))
  else:
    result = newTreeIT(nkCall, n.info, getSysType(g, n.info, tyInt)):
      [newSymNode(getSysMagic(g, n.info, "high", mHigh)), n]

proc genLen*(g: ModuleGraph; n: PNode): PNode =
  if skipTypes(n.typ, abstractVar).kind == tyArray:
    result = newIntLit(g, n.info, toInt64(lastOrd(g.config, skipTypes(n.typ, abstractVar)) + 1))
  else:
    result = newTreeIT(nkCall, n.info, getSysType(g, n.info, tyInt)):
      [newSymNode(getSysMagic(g, n.info, "len", mLengthSeq)), n]
