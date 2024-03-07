#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## tree helper routines

import
  compiler/ast/[
    ast,
    wordrecg,
    idents,
  ],
  compiler/utils/[
    idioms,
  ]

proc cyclicTreeAux(n: PNode, visited: var seq[PNode]): bool =
  if n == nil: return
  for v in visited:
    if v == n: return true
  case n.kind
  of nkWithoutSons - nkError:
    discard
  of nkError:
    visited.add(n)
    if cyclicTreeAux(n.diag.wrongNode, visited): return true
    discard visited.pop()
  of nkWithSons:
    visited.add(n)
    for nSon in n.sons:
      if cyclicTreeAux(nSon, visited): return true
    discard visited.pop()

proc cyclicTree*(n: PNode): bool =
  var visited: seq[PNode] = @[]
  cyclicTreeAux(n, visited)

proc sameFloatIgnoreNan(a, b: BiggestFloat): bool {.inline.} =
  ## ignores NaN semantics, but ensures 0.0 == -0.0, see #13730
  cast[uint64](a) == cast[uint64](b) or a == b

template structEquiv*(
  self, relaxKindCheck, symCheck, floatCheck, commentCheck, typeCheck: untyped) =
  # This template contains the skeleton/common denominator of all
  # recursive PNode equivalence checks in the compiler code base
  # It might be possible to unify more of them with each other.
  result = false
  if a == b:
    result = true
  elif a != nil and b != nil and a.kind == b.kind or relaxKindCheck:
    case a.kind
    of nkSym: result = symCheck
    of nkIdent: result = a.ident.id == b.ident.id
    of nkIntLiterals: result = a.intVal == b.intVal
    of nkFloatLiterals: result = floatCheck
    of nkStrLiterals: result = a.strVal == b.strVal
    of nkCommentStmt: result = commentCheck
    of nkError:
      unreachable()
    of nkType:
      result = typeCheck
    of nkEmpty, nkNilLit:
      result = true
    of nkWithSons:
      if a.len == b.len:
        for i in 0..<a.len:
          if not self(a[i], b[i]): return false
        result = true

proc exprStructuralEquivalentLaxSym(a, b: PNode): bool =
  structEquiv(exprStructuralEquivalentLaxSym,
    relaxKindCheck = false,
    # don't go nuts here: same symbol as string is enough:
    symCheck = a.sym.name.id == b.sym.name.id,
    floatCheck = sameFloatIgnoreNan(a.floatVal, b.floatVal),
    commentCheck = a.comment == b.comment,
    typeCheck = true
  )

proc exprStructuralEquivalentStrictSym(a, b: PNode): bool =
  structEquiv(exprStructuralEquivalentStrictSym,
    relaxKindCheck = false,
    symCheck = a.sym == b.sym,
    floatCheck = sameFloatIgnoreNan(a.floatVal, b.floatVal),
    commentCheck = a.comment == b.comment,
    typeCheck = true
  )

proc exprStructuralEquivalent*(a, b: PNode; strictSymEquality=false): bool =
  if strictSymEquality:
    exprStructuralEquivalentStrictSym(a, b)
  else:
    exprStructuralEquivalentLaxSym(a, b)

proc getMagic*(op: PNode): TMagic =
  if op == nil: return mNone
  case op.kind
  of nkCallKinds:
    case op[0].kind
    of nkSym: result = op[0].sym.magic
    else: result = mNone
  else: result = mNone

proc isConstExpr*(n: PNode): bool =
  n.kind in nkLiterals or nfAllConst in n.flags

proc isCaseObj*(n: PNode): bool =
  if n.kind == nkRecCase: return true
  for i in 0..<n.safeLen:
    if n[i].isCaseObj: return true

proc isDeepConstExpr*(n: PNode; preventInheritance = false): bool =
  case n.kind
  of nkLiterals:
    result = true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = isDeepConstExpr(n[1], preventInheritance)
  of nkCurly, nkBracket, nkPar, nkTupleConstr, nkObjConstr, nkClosure, nkRange:
    for i in ord(n.kind == nkObjConstr)..<n.len:
      if not isDeepConstExpr(n[i], preventInheritance): return false
    if n.typ.isNil: result = true
    else:
      let t = n.typ.skipTypes({tyGenericInst, tyDistinct, tyAlias, tySink})
      if t.kind in {tyRef, tyPtr} or tfUnion in t.flags: return false
      if t.kind == tyObject:
        if preventInheritance and t[0] != nil:
          result = false
        elif isCaseObj(t.n):
          result = false
        else:
          result = true
      else:
        result = true
  else: discard

proc isRange*(n: PNode): bool {.inline.} =
  if n.kind in nkCallKinds:
    let callee = n[0]
    if (callee.kind == nkIdent and callee.ident.id == ord(wDotDot)) or
       (callee.kind == nkSym and callee.sym.name.id == ord(wDotDot)) or
       (callee.kind in {nkClosedSymChoice, nkOpenSymChoice} and
        callee[1].sym.name.id == ord(wDotDot)):
      result = true

proc whichPragma*(n: PNode): TSpecialWord =
  let key = if n.kind in nkPragmaCallKinds and n.len > 0: n[0] else: n
  case key.kind
  of nkIdent: result = whichKeyword(key.ident)
  of nkSym: result = whichKeyword(key.sym.name)
  of nkCast: result = wCast
  of nkClosedSymChoice, nkOpenSymChoice:
    result = whichPragma(key[0])
  else: result = wInvalid

proc isNoSideEffectPragma*(n: PNode): bool =
  var k = whichPragma(n)
  if k == wCast:
    k = whichPragma(n[1])
  result = k == wNoSideEffect

proc findPragma*(n: PNode, which: TSpecialWord): PNode =
  if n.kind == nkPragma:
    for son in n:
      if whichPragma(son) == which:
        return son

proc effectSpec*(n: PNode, effectType: TSpecialWord): PNode =
  for it in n:
    if it.kind == nkExprColonExpr and whichPragma(it) == effectType:
      result = it[1]
      if result.kind notin {nkCurly, nkBracket}:
        result = newNodeI(nkCurly, result.info)
        result.add(it[1])
      return

proc unnestStmts(n, result: PNode) =
  case n.kind
  of nkStmtList:
    # xxx: we don't handle nkStmtListExpr, hmm
    for x in items(n):
      unnestStmts(x, result)
  of nkCommentStmt, nkNilLit:
    # QUESTION: why don't we drop empties? what about defer?
    discard
  else:
    result.add n

proc flattenStmts*(n: PNode): PNode =
  result = newNodeI(nkStmtList, n.info)
  unnestStmts(n, result)
  if result.len == 1:
    result = result[0]

proc extractRange*(k: TNodeKind, n: PNode, a, b: int): PNode =
  result = newNodeI(k, n.info, b-a+1)
  for i in 0..b-a: result[i] = n[i+a]

proc isTrue*(n: PNode): bool =
  n.kind == nkSym and n.sym.kind == skEnumField and n.sym.position != 0 or
    n.kind == nkIntLit and n.intVal != 0

proc getRoot*(n: PNode): PSym =
  ## ``getRoot`` takes a *path* ``n``. A path is an lvalue expression
  ## like ``obj.x[i].y``. The *root* of a path is the symbol that can be
  ## determined as the owner; ``obj`` in the example.
  case n.kind
  of nkSym:
    if n.sym.kind in {skVar, skResult, skTemp, skLet, skForVar, skParam}:
      result = n.sym
  of nkDotExpr, nkBracketExpr, nkHiddenDeref, nkDerefExpr,
      nkObjUpConv, nkObjDownConv, nkCheckedFieldExpr, nkHiddenAddr, nkAddr:
    result = getRoot(n[0])
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    result = getRoot(n[1])
  of nkCallKinds:
    if getMagic(n) == mSlice: result = getRoot(n[1])
  else: discard

proc stupidStmtListExpr*(n: PNode): bool =
  for i in 0..<n.len-1:
    if n[i].kind notin {nkEmpty, nkCommentStmt}: return false
  result = true

proc dontInlineConstant*(orig, cnst: PNode): bool {.inline.} =
  # symbols that expand to a complex constant (array, etc.) should not be
  # inlined, unless it's the empty array:
  result = orig.kind != cnst.kind and
           cnst.kind in {nkCurly, nkPar, nkTupleConstr, nkBracket, nkObjConstr} and
           cnst.len > ord(cnst.kind == nkObjConstr)

proc flattenExpr*(expr: PNode, stmts: var seq[PNode]): PNode =
  ## Applies the following transformations to the expression `expr`:
  ## 1.) transform an AST like: ``(A (StmtListExpr (x) (y)) (z))`` into
  ##     ``(StmtListExpr (x) (A (y) (z)))``
  ## 2.) add all statements from a statement list expression to `stmts` and
  ##     pass on the last node
  ##
  ## The result of #2 is passed back to step #1 and the process is repeated
  ## until none of the two transformations apply anymore. Together, these
  ## transformations make processing easier for the following analysis steps,
  ## and the generated AST a bit less nested.
  proc forward(n: PNode, p: int): PNode =
    ## Performs transformation #1
    if n[p].kind == nkStmtListExpr:
      result = n[p]
      n[p] = result[^1]
      result[^1] = n
    else:
      result = n

  var it = expr
  while true:
    # we're looking for expression nodes that represent side-effect free
    # operations
    case it.kind
    of nkDotExpr, nkCheckedFieldExpr, nkBracketExpr, nkHiddenAddr, nkAddr,
      nkDerefExpr, nkHiddenDeref, nkCStringToString, nkStringToCString,
      nkObjDownConv, nkObjUpConv:
      it = forward(it, 0)
    of nkConv, nkHiddenStdConv, nkCast:
      it = forward(it, 1)
    else:
      # no AST to which transform #1 applies
      discard

    if it.kind == nkStmtListExpr:
      # transformation #2:
      for i in 0..<it.len-1:
        stmts.add it[i]

      it = it[^1]
    else:
      # we're done transforming
      break

  result = it
