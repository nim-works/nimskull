## This module contains:
## 1. routines duplicated from elsewhere and adjusted to work with ``CgNode``
##    instead of ``PNode``
## 2. routines for translating ``PNode`` to ``CgNode``
## 3. utility routines for ``CgNode`` of which usage should either be phased
##    out or that should be moved somewhere else
##
## The plan is to eventually replace all usages of the routines part of #1 and
## #2 with something else (e.g., by running analysis in an earlier phase,
## lowering earlier, etc.) and then to phase all of them out.

import
  compiler/ast/[
    ast_types,
    ast_query,
    lineinfos,
    types
  ],
  compiler/backend/[
    cgir
  ],
  compiler/front/[
    options
  ],
  compiler/utils/[
    bitsets,
    idioms,
    int128
  ]

from compiler/mir/astgen import translateLit

func lastSon*(n: CgNode): CgNode {.inline.} =
  # XXX: replace usages with `n[^1]`
  {.cast(noSideEffect).}:
    n.childs[^1]

proc skipConv*(n: CgNode): CgNode {.inline.} =
  result = n
  while result.kind in {cnkConv, cnkHiddenConv}:
    result = result[1]

func getInt*(n: CgNode): Int128 =
  case n.kind
  of cnkUIntLit: toInt128(cast[BiggestUInt](n.intVal))
  of cnkIntLit:  toInt128(n.intVal)
  else:          unreachable(n.kind)

proc getOrdValue*(n: CgNode): Int128 =
  case n.kind
  of cnkUIntLit:    toInt128(cast[BiggestUInt](n.intVal))
  of cnkIntLit:     toInt128(n.intVal)
  of cnkHiddenConv: getOrdValue(n[1])
  else:             unreachable()

proc getMagic*(op: CgNode): TMagic {.inline.}  =
  case op.kind
  of cnkCall:
    case op[0].kind
    of cnkSym: op[0].sym.magic
    else:      mNone
  else:        mNone

proc isDiscriminantField*(n: CgNode): bool =
  case n.kind
  of cnkCheckedFieldAccess: sfDiscriminant in n[0][1].sym.flags
  of cnkFieldAccess:        sfDiscriminant in n[1].sym.flags
  else:                     false

func isOfBranch*(n: CgNode): bool {.inline.} =
  n.kind == cnkBranch and n.len > 1


proc isDeepConstExpr*(n: CgNode): bool =
  case n.kind
  of cnkLiterals, cnkNilLit:
    result = true
  of cnkSetConstr, cnkArrayConstr, cnkClosureConstr, cnkTupleConstr, cnkRange:
    result = true
    for it in n.items:
      if not isDeepConstExpr(it):
        result = false
        break
  of cnkObjConstr:
    let t = n.typ.skipTypes({tyGenericInst, tyDistinct, tyAlias, tySink})
    if t.kind == tyRef:
      # ref-constructions are never constant
      return false

    result = true
    for it in n.items:
      if not isDeepConstExpr(it[1]):
        result = false
        break
  else:
    result = false

proc getRoot*(n: CgNode): PSym =
  ## ``getRoot`` takes a *path* ``n``. A path is an lvalue expression
  ## like ``obj.x[i].y``. The *root* of a path is the symbol that can be
  ## determined as the owner; ``obj`` in the example.
  case n.kind
  of cnkSym:
    if n.sym.kind in {skVar, skResult, skTemp, skLet, skForVar, skParam}:
      result = n.sym
  of cnkFieldAccess, cnkBracketAccess, cnkDerefView, cnkDeref,
     cnkObjUpConv, cnkObjDownConv, cnkCheckedFieldAccess, cnkHiddenAddr,
     cnkAddr:
    result = getRoot(n[0])
  of cnkHiddenConv, cnkConv:
    result = getRoot(n[1])
  of cnkCall:
    if getMagic(n) == mSlice:
      result = getRoot(n[1])
  else: discard

proc isLValue*(n: CgNode): bool =
  ## Duplicate of `isLValue <compiler/sem/parampatters.html#isLvalue,PNode>`_,
  ## but simplified to the needs of the C code generator.
  # XXX: remove this as soon as possible
  case n.kind
  of cnkEmpty:
    n.typ.kind == tyVar
  of cnkSym:
    (n.sym.kind == skParam and n.sym.typ.kind in {tyVar, tySink}) or
      n.sym.kind in {skVar, skResult, skTemp}
  of cnkFieldAccess:
    let t = skipTypes(n[0].typ, abstractInst-{tyTypeDesc})
    t.kind in {tyVar, tySink, tyPtr, tyRef} or
      ((n[1].kind != cnkSym or sfDiscriminant notin n[1].sym.flags) and
       isLValue(n[0]))
  of cnkBracketAccess:
    let t = skipTypes(n[0].typ, abstractInst-{tyTypeDesc})
    t.kind in {tyVar, tySink, tyPtr, tyRef} or isLValue(n[0])
  of cnkHiddenConv, cnkConv:
    if skipTypes(n.typ, abstractPtrs-{tyTypeDesc}).kind in
        {tyOpenArray, tyTuple, tyObject}:
      isLValue(n[1])
    elif compareTypes(n.typ, n[1].typ, dcEqIgnoreDistinct):
      isLValue(n[1])
    else:
      false
  of cnkDerefView:
    let n0 = n[0]
    n0.typ.kind != tyLent or (n0.kind == cnkSym and n0.sym.kind == skResult)
  of cnkDeref, cnkHiddenAddr:
    true
  of cnkObjUpConv, cnkObjDownConv, cnkCheckedFieldAccess:
    isLValue(n[0])
  of cnkCall:
    (getMagic(n) == mSlice and isLValue(n[1])) or n.typ.kind in {tyVar}
  of cnkStmtListExpr:
    isLValue(n[^1])
  else:
    false

proc canRaiseConservative*(fn: CgNode): bool =
  ## Duplicate of `canRaiseConservative <ast_query.html#canRaiseConservative,PNode>`_.
  if fn.kind == cnkSym and fn.sym.magic notin magicsThatCanRaise:
    result = false
  else:
    result = true

proc canRaise*(fn: CgNode): bool =
  ## Duplicate of `canRaise <ast_query.html#canRaise,PNode>`_.
  if fn.kind == cnkSym and (fn.sym.magic notin magicsThatCanRaise or
      {sfImportc, sfInfixCall} * fn.sym.flags == {sfImportc} or
      sfGeneratedOp in fn.sym.flags):
    result = false
  elif fn.kind == cnkSym and fn.sym.magic == mEcho:
    result = true
  else:
    if fn.typ != nil and fn.typ.n != nil and fn.typ.n[0].kind == nkSym:
      result = false
    else:
      result = fn.typ != nil and fn.typ.n != nil and
        ((fn.typ.n[0].len < effectListLen) or
         (fn.typ.n[0][exceptionEffects] != nil and
          fn.typ.n[0][exceptionEffects].safeLen > 0))

proc toBitSet*(conf: ConfigRef; s: CgNode): TBitSet =
  ## Duplicate of `toBitSet <nimsets.html#toBitSet,ConfigRef,PNode>`_
  bitSetInit(result, int(getSize(conf, s.typ)))

  var first, j: Int128
  first = firstOrd(conf, s.typ[0])
  for it in s.items:
    if it.kind == cnkRange:
      j = getOrdValue(it[0])
      while j <= getOrdValue(it[1]):
        bitSetIncl(result, toInt64(j - first))
        inc(j)
    else:
      bitSetIncl(result, toInt64(getOrdValue(it) - first))

proc flattenStmts*(n: CgNode): CgNode =
  ## Duplicate of `flattenStmts <trees.html#flattenStmts,PNode>`_
  # XXX: this doesn't work as intended. The intention is to bring all 'def's
  #      to the top so that they can be special cased, but
  #      ``cnkStmtListExpr``s cannot be unnested, meaning that not all
  #      'def's are brought to the top-level
  proc unnestStmts(n: CgNode, result: var CgNode) =
    case n.kind
    of cnkStmtList:
      for it in n.items:
        unnestStmts(it, result)
    else:
      result.childs.add n

  result = CgNode(kind: cnkStmtList)
  unnestStmts(n, result)
  if result.len == 1:
    result = result[0]

proc toSymNode*(n: PNode): CgNode {.inline.} =
  CgNode(kind: cnkSym, info: n.info, typ: n.typ, sym: n.sym)

proc newSymNode*(s: PSym): CgNode {.inline.} =
  CgNode(kind: cnkSym, info: s.info, typ: s.typ, sym: s)

proc newStrNode*(str: sink string): CgNode {.inline.} =
  CgNode(kind: cnkStrLit, info: unknownLineInfo, strVal: str)

proc translate*(n: PNode): CgNode =
  ## Compatibility routine for translating a ``PNode`` value-construction tree
  ## to a ``CgNode`` tree.
  case n.kind
  of nkObjConstr:
    result = newExpr(cnkObjConstr, n.info, n.typ)
    for i, it in sliceIt(n.sons, 1, n.len-1):
      result.childs.add translate(it)
  of nkBracket:
    result = newExpr(cnkArrayConstr, n.info, n.typ)
    for it in n.items:
      result.childs.add translate(it)
  of nkCurly:
    result = newExpr(cnkSetConstr, n.info, n.typ)
    for it in n.items:
      result.childs.add translate(it)
  of nkTupleConstr:
    result = newExpr(cnkTupleConstr, n.info, n.typ)
    for it in n.items:
      let it = if it.kind == nkExprColonExpr: it[1] else: it
      result.childs.add translate(it)
  of nkClosure:
    result = newExpr(cnkClosureConstr, n.info, n.typ)
    result.childs = @[translate(n[0]), translate(n[1])]
  of nkRange:
    result = newNode(cnkRange, n.info)
    result.childs = @[translate(n[0]), translate(n[1])]
  of nkSym:
    result = newSymNode(n.sym)
    result.info = n.info
  of nkExprColonExpr:
    result = newNode(cnkBinding, n.info)
    result.childs = @[translate(n[0]), translate(n[1])]
  of nkLiterals:
    result = translateLit(n)
  of nkNilLit:
    result = newNode(cnkNilLit, n.info, n.typ)
  else:
    unreachable(n.kind)