#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements lifting for type-bound operations
## (``=sink``, ``=``, ``=destroy``, ``=deepCopy``).

import
  std/[
    tables
  ],
  compiler/ast/[
    lineinfos,
    idents,
    ast,
    renderer,
    types,
    trees
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
  ],
  compiler/sem/[
    semdata,
    sighashes,
    lowerings
  ],
  compiler/backend/[
    ccgutils
  ]

from compiler/ast/reports_sem import reportAst,
  reportSem,
  reportSym,
  reportTyp
from compiler/ast/report_enums import ReportKind

type
  TLiftCtx = object
    g: ModuleGraph
    info: TLineInfo # for construction
    kind: TTypeAttachedOp
    fn: PSym
    asgnForType: PType
    recurse: bool
    addMemReset: bool    # add wasMoved() call after destructor call
    canRaise: bool
    filterDiscriminator: PSym  # we generating destructor for case branch
    c: PContext # c can be nil, then we are called from lambdalifting!
    idgen: IdGenerator

template destructor*(t: PType): PSym = getAttachedOp(c.g, t, attachedDestructor)
template assignment*(t: PType): PSym = getAttachedOp(c.g, t, attachedAsgn)
template asink*(t: PType): PSym = getAttachedOp(c.g, t, attachedSink)

proc fillBody(c: var TLiftCtx; t: PType; body, x, y: PNode)
proc produceSym(g: ModuleGraph; c: PContext; typ: PType; kind: TTypeAttachedOp;
              info: TLineInfo; idgen: IdGenerator): PSym

proc createTypeBoundOps*(g: ModuleGraph; c: PContext; orig: PType; info: TLineInfo;
                         idgen: IdGenerator)

proc at(a, i: PNode, elemType: PType): PNode =
  result = newTreeIT(nkBracketExpr, a.info, elemType): [a, i]

proc destructorOverriden(g: ModuleGraph; t: PType): bool =
  let op = getAttachedOp(g, t, attachedDestructor)
  op != nil and sfOverriden in op.flags

proc fillBodyTup(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  for i in 0..<t.len:
    let lit = lowerings.newIntLit(c.g, x.info, i)
    let b = if c.kind == attachedTrace: y else: y.at(lit, t[i])
    fillBody(c, t[i], body, x.at(lit, t[i]), b)

proc dotField(x: PNode, f: PSym): PNode =
  result = newTreeIT(nkDotExpr, x.info, f.typ):
    [if x.typ.skipTypes(abstractInst).kind == tyVar: x.newDeref else: x,
     newSymNode(f, x.info)]

proc newAsgnStmt(le, ri: PNode): PNode =
  result = newTreeI(nkAsgn, le.info): [le, ri]

proc genBuiltin*(g: ModuleGraph; idgen: IdGenerator; magic: TMagic; name: string; i: PNode): PNode =
  result = newTreeI(nkCall, i.info):
    [createMagic(g, idgen, name, magic).newSymNode, i]

proc genBuiltin(c: var TLiftCtx; magic: TMagic; name: string; i: PNode): PNode =
  result = genBuiltin(c.g, c.idgen, magic, name, i)

proc defaultOp(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  if c.kind in {attachedAsgn, attachedDeepCopy, attachedSink}:
    body.add newAsgnStmt(x, y)
  elif c.kind == attachedDestructor and c.addMemReset:
    let call = genBuiltin(c, mDefault, "default", x)
    call.typ = t
    body.add newAsgnStmt(x, call)

proc genAddr(c: var TLiftCtx; x: PNode): PNode =
  if x.kind == nkHiddenDeref:
    checkSonsLen(x, 1, c.g.config)
    result = x[0]
  else:
    result = newTreeIT(nkHiddenAddr, x.info, makeVarType(x.typ.owner, x.typ, c.idgen)): x

proc genWhileLoop(c: var TLiftCtx; i, dest: PNode): PNode =
  let cmp = genBuiltin(c, mLtI, "<", i)
  cmp.add genLen(c.g, dest)
  cmp.typ = getSysType(c.g, c.info, tyBool)
  result = newTreeI(nkWhileStmt, c.info):
    [cmp, newNodeI(nkStmtList, c.info)]

proc genIf(c: var TLiftCtx; cond, action: PNode): PNode =
  result = newTree(nkIfStmt, newTree(nkElifBranch, cond, action))

proc genContainerOf(c: var TLiftCtx; objType: PType, field, x: PSym): PNode =
  # generate: cast[ptr ObjType](cast[int](addr(x)) - offsetOf(objType.field))
  let intType = getSysType(c.g, unknownLineInfo, tyInt)

  let addrOf = newTreeIT(nkAddr, c.info, makePtrType(x.owner, x.typ, c.idgen)):
    newDeref(newSymNode(x))
  let castExpr1 = newTreeIT(nkCast, c.info, intType):
    [newNodeIT(nkType, c.info, intType), addrOf]

  let dotExpr = newTreeIT(nkDotExpr, c.info, x.typ):
    [newNodeIT(nkType, c.info, objType), newSymNode(field)]

  let offsetOf = genBuiltin(c, mOffsetOf, "offsetof", dotExpr)
  offsetOf.typ = intType

  let minusExpr = genBuiltin(c, mSubI, "-", castExpr1)
  minusExpr.typ = intType
  minusExpr.add foldOffsetOf(c.g.config, offsetOf, offsetOf)

  let objPtr = makePtrType(objType.owner, objType, c.idgen)
  result = newTreeIT(nkCast, c.info, objPtr):
    [newNodeIT(nkType, c.info, objPtr), minusExpr]

proc destructorCall(c: var TLiftCtx; op: PSym; x: PNode): PNode =
  var destroy = newTreeIT(nkCall, x.info, op.typ[0]):
    [newSymNode(op), genAddr(c, x)]
  if sfNeverRaises notin op.flags:
    c.canRaise = true
  if c.addMemReset:
    result = newTree(nkStmtList):
      [destroy, genBuiltin(c, mWasMoved,  "wasMoved", x)]
  else:
    result = destroy

proc fillBodyObj(c: var TLiftCtx; n, body, x, y: PNode; enforceDefaultOp: bool) =
  case n.kind
  of nkSym:
    if c.filterDiscriminator != nil: return
    let f = n.sym
    let b = if c.kind == attachedTrace: y else: y.dotField(f)
    if sfCursor in f.flags or enforceDefaultOp:
      defaultOp(c, f.typ, body, x.dotField(f), b)
    else:
      fillBody(c, f.typ, body, x.dotField(f), b)
  of nkNilLit: discard
  of nkRecCase:
    # XXX This is only correct for 'attachedSink'!
    var localEnforceDefaultOp = enforceDefaultOp
    if c.kind == attachedSink:
      # the value needs to be destroyed before we assign the selector
      # or the value is lost
      let prevKind = c.kind
      c.kind = attachedDestructor
      fillBodyObj(c, n, body, x, y, enforceDefaultOp = false)
      c.kind = prevKind
      localEnforceDefaultOp = true

    if c.kind != attachedDestructor:
      # copy the selector before case stmt, but destroy after case stmt
      fillBodyObj(c, n[0], body, x, y, enforceDefaultOp = false)

    let oldfilterDiscriminator = c.filterDiscriminator
    if c.filterDiscriminator == n[0].sym:
      c.filterDiscriminator = nil # we have found the case part, proceed as normal

    # we need to generate a case statement:
    var caseStmt = newNodeI(nkCaseStmt, c.info)
    # XXX generate 'if' that checks same branches
    # generate selector:
    var access = dotField(x, n[0].sym)
    caseStmt.add(access)
    var emptyBranches = 0
    # copy the branches over, but replace the fields with the for loop body:
    for i in 1..<n.len:
      var branch = copyTree(n[i])
      branch[^1] = newNodeI(nkStmtList, c.info)

      fillBodyObj(c, n[i].lastSon, branch[^1], x, y,
                  enforceDefaultOp = localEnforceDefaultOp)
      if branch[^1].len == 0: inc emptyBranches
      caseStmt.add(branch)
    if emptyBranches != n.len-1:
      body.add(caseStmt)

    if c.kind == attachedDestructor:
      # destructor for selector is done after case stmt
      fillBodyObj(c, n[0], body, x, y, enforceDefaultOp = false)
    c.filterDiscriminator = oldfilterDiscriminator
  of nkRecList:
    for t in items(n):
      fillBodyObj(c, t, body, x, y, enforceDefaultOp)
  else:
    c.g.config.localReport(n.info, reportAst(
      rsemIllformedAst, n,
      str = "Unexpected node kind for 'fillBodyObj' - " &
        "wanted Sym, NilLit, RecList or RecCase, but found " & $n.kind))

proc fillBodyObjTImpl(c: var TLiftCtx; t: PType, body, x, y: PNode) =
  if t.len > 0 and t[0] != nil:
    fillBody(c, skipTypes(t[0], abstractPtrs), body, x, y)
  fillBodyObj(c, t.n, body, x, y, enforceDefaultOp = false)

proc fillBodyObjT(c: var TLiftCtx; t: PType, body, x, y: PNode) =
  var hasCase = isCaseObj(t.n)
  var obj = t
  while obj.len > 0 and obj[0] != nil:
    obj = skipTypes(obj[0], abstractPtrs)
    hasCase = hasCase or isCaseObj(obj.n)

  if hasCase and c.kind in {attachedAsgn, attachedDeepCopy}:
    # assignment for case objects is complex, we do:
    # =destroy(dest)
    # wasMoved(dest)
    # for every field:
    #   `=` dest.field, src.field
    # ^ this is what we used to do, but for 'result = result.sons[0]' it
    # destroys 'result' too early.
    # So this is what we really need to do:
    # let blob {.cursor.} = dest # remembers the old dest.kind
    # wasMoved(dest)
    # dest.kind = src.kind
    # for every field (dependent on dest.kind):
    #   `=` dest.field, src.field
    # =destroy(blob)
    var dummy = newSym(skTemp, getIdent(c.g.cache, lowerings.genPrefix), nextSymId c.idgen, c.fn, c.info)
    dummy.typ = y.typ
    if ccgIntroducedPtr(c.g.config, dummy, y.typ):
      # Because of potential aliasing when the src param is passed by ref, we need to check for equality here,
      # because the wasMoved(dest) call would zero out src, if dest aliases src.
      var cond = newTree(nkCall, newSymNode(c.g.getSysMagic(c.info, "==", mEqRef)),
        newTreeIT(nkAddr, c.info, makePtrType(c.fn, x.typ, c.idgen), x), newTreeIT(nkAddr, c.info, makePtrType(c.fn, y.typ, c.idgen), y))
      cond.typ = getSysType(c.g, x.info, tyBool)
      body.add genIf(c, cond, newTreeI(nkReturnStmt, c.info, newNodeI(nkEmpty, c.info)))
    var temp = newSym(skTemp, getIdent(c.g.cache, lowerings.genPrefix), nextSymId c.idgen, c.fn, c.info)
    temp.typ = x.typ
    incl(temp.flags, sfFromGeneric)
    let blob = newSymNode(temp)
    let v = newTreeI(nkVarSection, c.info):
      newIdentDefs(blob, x)
    body.add v
    #body.add newAsgnStmt(blob, x)

    # mWasMoved does not take the address
    var wasMovedCall = newTreeI(nkCall, c.info):
      [newSymNode(createMagic(c.g, c.idgen, "wasMoved", mWasMoved)), x]
    body.add wasMovedCall

    fillBodyObjTImpl(c, t, body, x, y)
    when false:
      # does not work yet due to phase-ordering problems:
      assert t.destructor != nil
      body.add destructorCall(c.g, t.destructor, blob)
    let prevKind = c.kind
    c.kind = attachedDestructor
    fillBodyObjTImpl(c, t, body, blob, y)
    c.kind = prevKind
  else:
    fillBodyObjTImpl(c, t, body, x, y)

proc boolLit*(g: ModuleGraph; info: TLineInfo; value: bool): PNode =
  result = newIntLit(g, info, ord value)
  result.typ = getSysType(g, info, tyBool)

proc getCycleParam(c: TLiftCtx): PNode =
  assert c.kind == attachedAsgn
  if c.fn.typ.len == 4:
    result = c.fn.typ.n.lastSon
    assert result.kind == nkSym
    assert result.sym.name.s == "cyclic"
  else:
    result = boolLit(c.g, c.info, true)

proc newHookCall(c: var TLiftCtx; op: PSym; x, y: PNode): PNode =
  #if sfError in op.flags:
  #  localReport(c.config, x.info, "usage of '$1' is a user-defined error" % op.name.s)
  result = newNodeI(nkCall, x.info)
  result.add newSymNode(op)
  if sfNeverRaises notin op.flags:
    c.canRaise = true
  if op.typ.sons[1].kind == tyVar:
    result.add genAddr(c, x)
  else:
    result.add x
  if y != nil:
    result.add y
  if op.typ.len == 4:
    assert y != nil
    if c.fn.typ.len == 4:
      result.add getCycleParam(c)
    else:
      # assume the worst: A cycle is created:
      result.add boolLit(c.g, y.info, true)

proc newOpCall(c: var TLiftCtx; op: PSym; x: PNode): PNode =
  result = newTreeIT(nkCall, x.info, op.typ[0]):
    [newSymNode(op), x]
  if sfNeverRaises notin op.flags:
    c.canRaise = true

proc newDeepCopyCall(c: var TLiftCtx; op: PSym; x, y: PNode): PNode =
  result = newAsgnStmt(x, newOpCall(c, op, y))

proc usesBuiltinArc(t: PType): bool =
  proc wrap(t: PType): bool {.nimcall.} = ast.isGCedMem(t)
  result = types.searchTypeFor(t, wrap)

proc useNoGc(c: TLiftCtx; t: PType): bool {.inline.} =
  result = optSeqDestructors in c.g.config.globalOptions and
    (tfHasGCedMem in t.flags or usesBuiltinArc(t))

proc requiresDestructor(c: TLiftCtx; t: PType): bool {.inline.} =
  result = optSeqDestructors in c.g.config.globalOptions and
    containsGarbageCollectedRef(t)

proc instantiateGeneric(c: var TLiftCtx; op: PSym; t, typeInst: PType): PSym =
  if c.c != nil and typeInst != nil:
    result = c.c.instTypeBoundOp(c.c, op, typeInst, c.info, attachedAsgn, 1)
  else:
    localReport(
      c.g.config,
      c.info,
      reportTyp(rsemCannotGenerateGenericDestructor, t))

    result = nil

proc considerAsgnOrSink(c: var TLiftCtx; t: PType; body, x, y: PNode;
                        field: var PSym): bool =
  if optSeqDestructors in c.g.config.globalOptions:
    var op = field
    let destructorOverriden = destructorOverriden(c.g, t)
    if op != nil and op != c.fn and
        (sfOverriden in op.flags or destructorOverriden):
      if sfError in op.flags:
        incl c.fn.flags, sfError
      body.add newHookCall(c, op, x, y)
      result = true
    elif op == nil and destructorOverriden:
      op = produceSym(c.g, c.c, t, c.kind, c.info, c.idgen)
      body.add newHookCall(c, op, x, y)
      result = true
  elif tfHasAsgn in t.flags:
    var op: PSym
    if sameType(t, c.asgnForType):
      # generate recursive call:
      if c.recurse:
        op = c.fn
      else:
        c.recurse = true
        return false
    else:
      op = field
      if op == nil:
        op = produceSym(c.g, c.c, t, c.kind, c.info, c.idgen)
    if sfError in op.flags:
      incl c.fn.flags, sfError
    # We also now do generic instantiations in the destructor lifting pass:
    if op.ast.isGenericRoutine:
      op = instantiateGeneric(c, op, t, t.typeInst)
      field = op
      #echo "trying to use ", op.ast
      #echo "for ", op.name.s, " "
      #debug(t)
      #return false
    assert op.ast[genericParamsPos].kind == nkEmpty
    body.add newHookCall(c, op, x, y)
    result = true

proc addDestructorCall(c: var TLiftCtx; orig: PType; body, x: PNode) =
  let t = orig.skipTypes(abstractInst - {tyDistinct})
  var op = t.destructor

  if op != nil and sfOverriden in op.flags:
    if op.ast.isGenericRoutine:
      # patch generic destructor:
      op = instantiateGeneric(c, op, t, t.typeInst)
      setAttachedOp(c.g, c.idgen.module, t, attachedDestructor, op)

  if op == nil and (useNoGc(c, t) or requiresDestructor(c, t)):
    op = produceSym(c.g, c.c, t, attachedDestructor, c.info, c.idgen)
    doAssert op != nil
    doAssert op == t.destructor

  if op != nil:
    body.add destructorCall(c, op, x)
  elif useNoGc(c, t):
    internalError(
      c.g.config, c.info, "type-bound operator could not be resolved")

proc considerUserDefinedOp(c: var TLiftCtx; t: PType; body, x, y: PNode): bool =
  case c.kind
  of attachedDestructor:
    var op = t.destructor
    if op != nil and sfOverriden in op.flags:

      if op.ast.isGenericRoutine:
        # patch generic destructor:
        op = instantiateGeneric(c, op, t, t.typeInst)
        setAttachedOp(c.g, c.idgen.module, t, attachedDestructor, op)

      body.add destructorCall(c, op, x)
      result = true
    #result = addDestructorCall(c, t, body, x)
  of attachedAsgn, attachedSink, attachedTrace:
    var op = getAttachedOp(c.g, t, c.kind)
    if op != nil and sfOverriden in op.flags:
      if op.ast.isGenericRoutine:
        # patch generic =trace:
        op = instantiateGeneric(c, op, t, t.typeInst)
        setAttachedOp(c.g, c.idgen.module, t, c.kind, op)

    result = considerAsgnOrSink(c, t, body, x, y, op)
    if op != nil:
      setAttachedOp(c.g, c.idgen.module, t, c.kind, op)

  of attachedDeepCopy:
    let op = getAttachedOp(c.g, t, attachedDeepCopy)
    if op != nil:
      body.add newDeepCopyCall(c, op, x, y)
      result = true

proc declareCounter(c: var TLiftCtx; body: PNode; first: BiggestInt): PNode =
  var temp = newSym(skTemp, getIdent(c.g.cache, lowerings.genPrefix), nextSymId(c.idgen), c.fn, c.info)
  temp.typ = getSysType(c.g, body.info, tyInt)
  incl(temp.flags, sfFromGeneric)

  result = newSymNode(temp)
  let v = newTreeI(nkVarSection, c.info):
    newIdentDefs(result, lowerings.newIntLit(c.g, body.info, first))
  body.add v

proc declareTempOf(c: var TLiftCtx; body: PNode; value: PNode): PNode =
  var temp = newSym(skTemp, getIdent(c.g.cache, lowerings.genPrefix), nextSymId(c.idgen), c.fn, c.info)
  temp.typ = value.typ
  incl(temp.flags, sfFromGeneric)

  result = newSymNode(temp)
  let v = newTreeI(nkVarSection, c.info):
    newIdentDefs(result, value)
  body.add v

proc addIncStmt(c: var TLiftCtx; body, i: PNode) =
  let incCall = genBuiltin(c, mInc, "inc", i)
  incCall.add lowerings.newIntLit(c.g, c.info, 1)
  body.add incCall

proc newSeqCall(c: var TLiftCtx; x, y: PNode): PNode =
  # don't call genAddr(c, x) here:
  result = genBuiltin(c, mNewSeq, "newSeq", x)
  let lenCall = genBuiltin(c, mLengthSeq, "len", y)
  lenCall.typ = getSysType(c.g, x.info, tyInt)
  result.add lenCall

proc setLenStrCall(c: var TLiftCtx; x, y: PNode): PNode =
  let lenCall = genBuiltin(c, mLengthStr, "len", y)
  lenCall.typ = getSysType(c.g, x.info, tyInt)
  result = genBuiltin(c, mSetLengthStr, "setLen", x) # genAddr(g, x))
  result.add lenCall

proc setLenSeqCall(c: var TLiftCtx; t: PType; x, y: PNode): PNode =
  let lenCall = genBuiltin(c, mLengthSeq, "len", y)
  lenCall.typ = getSysType(c.g, x.info, tyInt)
  var op = getSysMagic(c.g, x.info, "setLen", mSetLengthSeq)
  op = instantiateGeneric(c, op, t, t)
  result = newTree(nkCall, newSymNode(op, x.info), x, lenCall)

proc forallElements(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  let counterIdx = body.len
  let i = declareCounter(c, body, toInt64(firstOrd(c.g.config, t)))
  let whileLoop = genWhileLoop(c, i, x)
  let elemType = t.lastSon
  let b = if c.kind == attachedTrace: y else: y.at(i, elemType)
  fillBody(c, elemType, whileLoop[1], x.at(i, elemType), b)
  if whileLoop[1].len > 0:
    addIncStmt(c, whileLoop[1], i)
    body.add whileLoop
  else:
    body.sons.setLen counterIdx

proc fillSeqOp(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  case c.kind
  of attachedAsgn, attachedDeepCopy:
    # we generate:
    # setLen(dest, y.len)
    # var i = 0
    # while i < y.len: dest[i] = y[i]; inc(i)
    # This is usually more efficient than a destroy/create pair.
    body.add setLenSeqCall(c, t, x, y)
    forallElements(c, t, body, x, y)
  of attachedSink:
    let moveCall = genBuiltin(c, mMove, "move", x)
    moveCall.add y
    doAssert t.destructor != nil
    moveCall.add destructorCall(c, t.destructor, x)
    body.add moveCall
  of attachedDestructor:
    # destroy all elements:
    forallElements(c, t, body, x, y)
    body.add genBuiltin(c, mDestroy, "destroy", x)
  of attachedTrace:
    # TODO: this check is incorrect: it's not relevant whether the type is
    #       cyclic, but rather whether it has an attached =trace operator
    #       (either lifted or user provided)
    if isCyclePossible(t.elemType, c.g):
      # follow all elements:
      forallElements(c, t, body, x, y)

proc useSeqOrStrOp(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  createTypeBoundOps(c.g, c.c, t, body.info, c.idgen)
  # recursions are tricky, so we might need to forward the generated
  # operation here:
  var t = t
  if t.assignment == nil or t.destructor == nil:
    let h = sighashes.hashType(t, {CoType, CoDistinct})
    let canon = c.g.canonTypes.getOrDefault(h)
    if canon != nil: t = canon

  case c.kind
  of attachedAsgn, attachedDeepCopy:
    # XXX: replace these with assertions.
    if t.assignment == nil:
      return # protect from recursion
    body.add newHookCall(c, t.assignment, x, y)
  of attachedSink:
    # we always inline the move for better performance:
    let moveCall = genBuiltin(c, mMove, "move", x)
    moveCall.add y
    doAssert t.destructor != nil
    moveCall.add destructorCall(c, t.destructor, x)
    body.add moveCall
    # alternatively we could do this:
    when false:
      doAssert t.asink != nil
      body.add newHookCall(c, t.asink, x, y)
  of attachedDestructor:
    doAssert t.destructor != nil
    body.add destructorCall(c, t.destructor, x)
  of attachedTrace:
    if t.kind != tyString and isCyclePossible(t.elemType, c.g):
      let op = getAttachedOp(c.g, t, c.kind)
      if op == nil:
        return # protect from recursion
      body.add newHookCall(c, op, x, y)

proc fillStrOp(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  case c.kind
  of attachedAsgn, attachedDeepCopy:
    body.add callCodegenProc(c.g, "nimAsgnStrV2", c.info, genAddr(c, x), y)
  of attachedSink:
    let moveCall = genBuiltin(c, mMove, "move", x)
    moveCall.add y
    doAssert t.destructor != nil
    moveCall.add destructorCall(c, t.destructor, x)
    body.add moveCall
  of attachedDestructor:
    body.add genBuiltin(c, mDestroy, "destroy", x)
  of attachedTrace:
    discard "strings are atomic and have no inner elements that are to trace"

proc cyclicType*(t: PType, g: ModuleGraph): bool =
  case t.kind
  of tyRef: result = types.isCyclePossible(t.lastSon, g)
  of tyProc: result = t.callConv == ccClosure
  else: result = false

proc atomicRefOp(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  #[ bug #15753 is really subtle. Usually the classical write barrier for reference
  counting looks like this::

    incRef source  # increment first; this takes care of self-assignments1
    decRef dest
    dest[] = source

  However, 'decRef dest' might trigger a cycle collection and then the collector
  traverses the graph. It is crucial that when it follows the pointers the assignment
  'dest[] = source' already happened so that we don't do trial deletion on a wrong
  graph -- this causes premature freeing of objects! The correct barrier looks like
  this::

    let tmp = dest
    incRef source
    dest[] = source
    decRef tmp

  ]#
  var actions = newNodeI(nkStmtList, c.info)
  let elemType = t.lastSon

  createTypeBoundOps(c.g, c.c, elemType, c.info, c.idgen)
  let isCyclic = c.g.config.selectedGC == gcOrc and types.isCyclePossible(elemType, c.g)

  let tmp =
    if isCyclic and c.kind in {attachedAsgn, attachedSink}:
      declareTempOf(c, body, x)
    else:
      x

  if isFinal(elemType):
    addDestructorCall(c, elemType, actions, genDeref(tmp, nkDerefExpr))
    var alignOf = genBuiltin(c, mAlignOf, "alignof", newNodeIT(nkType, c.info, elemType))
    alignOf.typ = getSysType(c.g, c.info, tyInt)
    actions.add callCodegenProc(c.g, "nimRawDispose", c.info, tmp,
                                foldAlignOf(c.g.config, alignOf, alignOf))
  else:
    addDestructorCall(c, elemType, newNodeI(nkStmtList, c.info), genDeref(tmp, nkDerefExpr))
    actions.add callCodegenProc(c.g, "nimDestroyAndDispose", c.info, tmp)

  var cond: PNode
  if isCyclic:
    if isFinal(elemType):
      let typInfo = genBuiltin(c, mGetTypeInfoV2, "getTypeInfoV2", newNodeIT(nkType, x.info, elemType))
      typInfo.typ = getSysType(c.g, c.info, tyPointer)
      cond = callCodegenProc(c.g, "nimDecRefIsLastCyclicStatic", c.info, tmp, typInfo)
    else:
      cond = callCodegenProc(c.g, "nimDecRefIsLastCyclicDyn", c.info, tmp)
  else:
    cond = callCodegenProc(c.g, "nimDecRefIsLast", c.info, x)
  cond.typ = getSysType(c.g, x.info, tyBool)

  case c.kind
  of attachedSink:
    if isCyclic:
      body.add newAsgnStmt(x, y)
      body.add genIf(c, cond, actions)
    else:
      body.add genIf(c, cond, actions)
      body.add newAsgnStmt(x, y)
  of attachedAsgn:
    if isCyclic:
      body.add genIf(c, y, callCodegenProc(c.g,
          "nimIncRefCyclic", c.info, y, getCycleParam(c)))
      body.add newAsgnStmt(x, y)
      body.add genIf(c, cond, actions)
    else:
      body.add genIf(c, y, callCodegenProc(c.g, "nimIncRef", c.info, y))
      body.add genIf(c, cond, actions)
      body.add newAsgnStmt(x, y)
  of attachedDestructor:
    body.add genIf(c, cond, actions)
  of attachedDeepCopy: assert(false, "cannot happen")
  of attachedTrace:
    if isCyclic:
      if isFinal(elemType):
        let typInfo = genBuiltin(c, mGetTypeInfoV2, "getTypeInfoV2", newNodeIT(nkType, x.info, elemType))
        typInfo.typ = getSysType(c.g, c.info, tyPointer)
        body.add callCodegenProc(c.g, "nimTraceRef", c.info, genAddrOf(x, c.idgen), typInfo, y)
      else:
        # If the ref is polymorphic we have to account for this
        body.add callCodegenProc(c.g, "nimTraceRefDyn", c.info, genAddrOf(x, c.idgen), y)
      #echo "can follow ", elemType, " static ", isFinal(elemType)

proc atomicClosureOp(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  ## Closures are really like refs except they always use a virtual destructor
  ## and we need to do the refcounting only on the ref field which we call 'xenv':
  let xenv = genBuiltin(c, mAccessEnv, "accessEnv", x)
  xenv.typ = getSysType(c.g, c.info, tyPointer)

  let isCyclic = c.g.config.selectedGC == gcOrc
  let tmp =
    if isCyclic and c.kind in {attachedAsgn, attachedSink}:
      declareTempOf(c, body, xenv)
    else:
      xenv

  var actions = newTreeI(nkStmtList, c.info):
    callCodegenProc(c.g, "nimDestroyAndDispose", c.info, tmp)

  let decRefProc =
    if isCyclic: "nimDecRefIsLastCyclicDyn"
    else: "nimDecRefIsLast"
  let cond = callCodegenProc(c.g, decRefProc, c.info, tmp)
  cond.typ = getSysType(c.g, x.info, tyBool)

  case c.kind
  of attachedSink:
    if isCyclic:
      body.add newAsgnStmt(x, y)
      body.add genIf(c, cond, actions)
    else:
      body.add genIf(c, cond, actions)
      body.add newAsgnStmt(x, y)
  of attachedAsgn:
    let yenv = genBuiltin(c, mAccessEnv, "accessEnv", y)
    yenv.typ = getSysType(c.g, c.info, tyPointer)
    if isCyclic:
      body.add genIf(c, yenv, callCodegenProc(c.g, "nimIncRefCyclic", c.info, yenv, getCycleParam(c)))
      body.add newAsgnStmt(x, y)
      body.add genIf(c, cond, actions)
    else:
      body.add genIf(c, yenv, callCodegenProc(c.g, "nimIncRef", c.info, yenv))

      body.add genIf(c, cond, actions)
      body.add newAsgnStmt(x, y)
  of attachedDestructor:
    body.add genIf(c, cond, actions)
  of attachedDeepCopy: assert(false, "cannot happen")
  of attachedTrace:
    body.add callCodegenProc(c.g, "nimTraceRefDyn", c.info, genAddrOf(xenv, c.idgen), y)

proc closureOp(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  if c.kind == attachedDeepCopy:
    # a big problem is that we don't know the environment's type here, so we
    # have to go through some indirection; we delegate this to the codegen:
    let call = newTreeIT(nkCall, c.info, t):
      [newSymNode(createMagic(c.g, c.idgen, "deepCopy", mDeepCopy)), y]
    body.add newAsgnStmt(x, call)
  elif c.g.config.selectedGC in {gcArc, gcOrc}:
    let xx = genBuiltin(c, mAccessEnv, "accessEnv", x)
    xx.typ = getSysType(c.g, c.info, tyPointer)
    case c.kind
    of attachedSink:
      # we 'nil' y out afterwards so we *need* to take over its reference
      # count value:
      body.add genIf(c, xx, callCodegenProc(c.g, "nimDecWeakRef", c.info, xx))
      body.add newAsgnStmt(x, y)
    of attachedAsgn:
      let yy = genBuiltin(c, mAccessEnv, "accessEnv", y)
      yy.typ = getSysType(c.g, c.info, tyPointer)
      body.add genIf(c, yy, callCodegenProc(c.g, "nimIncRef", c.info, yy))
      body.add genIf(c, xx, callCodegenProc(c.g, "nimDecWeakRef", c.info, xx))
      body.add newAsgnStmt(x, y)
    of attachedDestructor:
      let des = genIf(c, xx, callCodegenProc(c.g, "nimDecWeakRef", c.info, xx))
      if body.len == 0:
        body.add des
      else:
        body.sons.insert(des, 0)
    of attachedDeepCopy: assert(false, "cannot happen")
    of attachedTrace: discard

proc fillBody(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  case t.kind
  of tyNone, tyEmpty, tyVoid: discard
  of tyPointer, tySet, tyBool, tyChar, tyEnum, tyInt..tyUInt64, tyCstring,
      tyPtr, tyUncheckedArray, tyVar, tyLent, tyVarargs, tyOpenArray:
    defaultOp(c, t, body, x, y)
  of tyRef:
    if c.g.config.selectedGC in {gcArc, gcOrc}:
      atomicRefOp(c, t, body, x, y)
    else:
      defaultOp(c, t, body, x, y)
  of tyProc:
    if t.callConv == ccClosure:
      if c.g.config.selectedGC in {gcArc, gcOrc}:
        atomicClosureOp(c, t, body, x, y)
      else:
        closureOp(c, t, body, x, y)
    else:
      defaultOp(c, t, body, x, y)
  of tyArray:
    if tfHasAsgn in t.flags or useNoGc(c, t):
      forallElements(c, t, body, x, y)
    else:
      defaultOp(c, t, body, x, y)
  of tySequence:
    if useNoGc(c, t):
      useSeqOrStrOp(c, t, body, x, y)
    elif optSeqDestructors in c.g.config.globalOptions:
      # note that tfHasAsgn is propagated so we need the check on
      # 'selectedGC' here to determine if we have the new runtime.
      discard considerUserDefinedOp(c, t, body, x, y)
    elif tfHasAsgn in t.flags:
      if c.kind in {attachedAsgn, attachedSink, attachedDeepCopy}:
        body.add newSeqCall(c, x, y)
      forallElements(c, t, body, x, y)
    else:
      defaultOp(c, t, body, x, y)
  of tyString:
    if useNoGc(c, t):
      useSeqOrStrOp(c, t, body, x, y)
    elif tfHasAsgn in t.flags:
      discard considerUserDefinedOp(c, t, body, x, y)
    else:
      defaultOp(c, t, body, x, y)
  of tyObject:
    if not considerUserDefinedOp(c, t, body, x, y):
      if c.kind in {attachedAsgn, attachedSink} and t.sym != nil and sfImportc in t.sym.flags:
        body.add newAsgnStmt(x, y)
      else:
        fillBodyObjT(c, t, body, x, y)
  of tyDistinct:
    if not considerUserDefinedOp(c, t, body, x, y):
      fillBody(c, t[0], body, x, y)
  of tyTuple:
    fillBodyTup(c, t, body, x, y)
  of tyFromExpr, tyProxy, tyBuiltInTypeClass, tyUserTypeClass,
     tyUserTypeClassInst, tyCompositeTypeClass, tyAnd, tyOr, tyNot, tyAnything,
     tyGenericParam, tyGenericBody, tyNil, tyUntyped, tyTyped,
     tyTypeDesc, tyGenericInvocation, tyForward, tyStatic:
    #internalError(c.g.config, c.info, "assignment requested for type: " & typeToString(t))
    discard
  of tyOrdinal, tyRange, tyInferred,
     tyGenericInst, tyAlias, tySink:
    fillBody(c, lastSon(t), body, x, y)

proc bindPseudoOp(g: ModuleGraph, c: PContext, idgen: IdGenerator,
                  kind: TTypeAttachedOp, typ: PType, info: TLineInfo) =
  ## If the `kind` slot is not alread filled, assigns a pseudo-operator to the
  ## slot of `typ`'s originating-from generic type.
  assert tfFromGeneric in typ.flags
  if c == nil or kind == attachedDeepCopy:
    # without a ``PContext``, we cannot create a pseudo-op, but it's also not
    # necessary, as a ``PContext`` is only missing for types created outside of
    # semantic analysis.
    # ``=deepCopy`` operators are currently not implicitly lifted like the
    # others, so we don't block the generic type's slot
    return

  # attach to the generic type, not to the ``tyGenericBody``
  let generic = typ.typeInst[0].lastSon
  if getAttachedOp(g, generic, kind) == nil:
    # no custom operator is bound to the `kind` slot for the generic type;
    # block it
    let
      name = g.cache.getIdent(AttachedOpToStr[kind])
      op   = newSym(skProc, name, nextSymId c.idgen, typ.owner, info)
    # mark the symbol as anonymous, making it possible to later detect it
    op.flags.incl sfAnon
    setAttachedOp(g, c.idgen.module, generic, kind, op)

proc produceSymDistinctType(g: ModuleGraph; c: PContext; typ: PType;
                            kind: TTypeAttachedOp; info: TLineInfo;
                            idgen: IdGenerator): PSym =
  assert typ.kind == tyDistinct
  let baseType = typ[0]
  if getAttachedOp(g, baseType, kind) == nil:
    discard produceSym(g, c, baseType, kind, info, idgen)
  result = getAttachedOp(g, baseType, kind)
  setAttachedOp(g, idgen.module, typ, kind, result)

  if tfFromGeneric in typ.flags:
    # block the generic type's operator slot
    bindPseudoOp(g, c, idgen, kind, typ, info)

proc symPrototype(g: ModuleGraph; typ: PType; owner: PSym; kind: TTypeAttachedOp;
              info: TLineInfo; idgen: IdGenerator): PSym =

  let procname = getIdent(g.cache, AttachedOpToStr[kind])
  result = newSym(skProc, procname, nextSymId(idgen), owner, info)
  let dest = newSym(skParam, getIdent(g.cache, "dest"), nextSymId(idgen), result, info)
  let src = newSym(skParam, getIdent(g.cache, if kind == attachedTrace: "env" else: "src"),
                   nextSymId(idgen), result, info)
  dest.typ = makeVarType(typ.owner, typ, idgen)
  if kind == attachedTrace:
    src.typ = getSysType(g, info, tyPointer)
  else:
    src.typ = typ

  result.typ = newProcType(info, nextTypeId(idgen), owner)
  result.typ.addParam dest
  if kind != attachedDestructor:
    result.typ.addParam src

  if kind == attachedAsgn and g.config.selectedGC == gcOrc and
      cyclicType(typ.skipTypes(abstractInst), g):
    let cycleParam = newSym(skParam, getIdent(g.cache, "cyclic"),
                            nextSymId(idgen), result, info)
    cycleParam.typ = getSysType(g, info, tyBool)
    result.typ.addParam cycleParam

  var n = newNodeI(nkProcDef, info, bodyPos+1)
  for i in 0..<n.len: n[i] = newNodeI(nkEmpty, info)
  n[namePos] = newSymNode(result)
  n[paramsPos] = result.typ.n
  n[bodyPos] = newNodeI(nkStmtList, info)
  result.ast = n
  result.flags.incl {sfFromGeneric, sfGeneratedOp}

proc genTypeFieldCopy(c: var TLiftCtx; t: PType; body, x, y: PNode) =
  let xx = genBuiltin(c, mAccessTypeField, "accessTypeField", x)
  let yy = genBuiltin(c, mAccessTypeField, "accessTypeField", y)
  xx.typ = getSysType(c.g, c.info, tyPointer)
  yy.typ = xx.typ
  body.add newAsgnStmt(xx, yy)

proc produceSym(g: ModuleGraph; c: PContext; typ: PType; kind: TTypeAttachedOp;
              info: TLineInfo; idgen: IdGenerator): PSym =
  if typ.kind == tyDistinct:
    return produceSymDistinctType(g, c, typ, kind, info, idgen)

  result = getAttachedOp(g, typ, kind)
  if result == nil:
    result = symPrototype(g, typ, typ.owner, kind, info, idgen)

  if typ.kind in {tyObject, tyEnum} and tfFromGeneric in typ.flags:
    # for nominal types (``tyDistinct`` is handled separately), block
    # the operator slot of the generic type
    bindPseudoOp(g, c, idgen, kind, typ, info)

  var a = TLiftCtx(info: info, g: g, kind: kind, c: c, asgnForType: typ, idgen: idgen,
                   fn: result)

  let dest = result.typ.n[1].sym
  let d = newDeref(newSymNode(dest))
  let src = if kind == attachedDestructor: newNodeIT(nkSym, info, getSysType(g, info, tyPointer))
            else: newSymNode(result.typ.n[2].sym)

  # register this operation already:
  setAttachedOpPartial(g, idgen.module, typ, kind, result)

  if kind == attachedSink and destructorOverriden(g, typ):
    ## compiler can use a combination of `=destroy` and memCopy for sink op
    dest.flags.incl sfCursor
    result.ast[bodyPos].add newOpCall(a, getAttachedOp(g, typ, attachedDestructor), d[0])
    result.ast[bodyPos].add newAsgnStmt(d, src)
  else:
    let (tk, baseTyp) =
      if g.config.selectedGC in {gcArc, gcOrc}:
        let t = skipTypes(typ, {tyOrdinal, tyRange, tyInferred, tyGenericInst,
                                tyStatic, tyAlias, tySink})
        (t.kind, t)
      else:
        (tyNone, typ) # no special casing for strings and seqs
    case tk
    of tySequence:
      fillSeqOp(a, typ, result.ast[bodyPos], d, src)
    of tyString:
      fillStrOp(a, typ, result.ast[bodyPos], d, src)
    else:
      fillBody(a, typ, result.ast[bodyPos], d, src)
      if tk == tyObject and a.kind in {attachedAsgn, attachedSink, attachedDeepCopy} and
         not isObjLackingTypeField(baseTyp):
        # bug #19205: Do not forget to also copy the hidden type field:
        genTypeFieldCopy(a, typ, result.ast[bodyPos], d, src)

  if not a.canRaise: incl result.flags, sfNeverRaises
  completePartialOp(g, idgen.module, typ, kind, result)


proc produceDestructorForDiscriminator*(g: ModuleGraph; typ: PType; field: PSym,
                                        info: TLineInfo; idgen: IdGenerator): PSym =
  assert(typ.skipTypes({tyAlias, tyGenericInst}).kind == tyObject)
  result = symPrototype(g, field.typ, typ.owner, attachedDestructor, info, idgen)
  var a = TLiftCtx(info: info, g: g, kind: attachedDestructor, asgnForType: typ, idgen: idgen,
                   fn: result)
  a.asgnForType = typ
  a.filterDiscriminator = field
  a.addMemReset = true
  let discrimantDest = result.typ.n[1].sym

  let dst = newSym(skVar, getIdent(g.cache, "dest"), nextSymId(idgen), result, info)
  dst.typ = makePtrType(typ.owner, typ, idgen)
  let dstSym = newSymNode(dst)
  let d = newDeref(dstSym)
  let v = newTreeI(nkVarSection, info):
    newIdentDefs(dstSym, genContainerOf(a, typ, field, discrimantDest))
  result.ast[bodyPos].add v
  let placeHolder = newNodeIT(nkSym, info, getSysType(g, info, tyPointer))
  fillBody(a, typ, result.ast[bodyPos], d, placeHolder)
  if not a.canRaise: incl result.flags, sfNeverRaises


template liftTypeBoundOps*(c: PContext; typ: PType; info: TLineInfo) =
  discard "now a nop"

proc patchBody(g: ModuleGraph; c: PContext; n: PNode; info: TLineInfo; idgen: IdGenerator) =
  if n.kind in nkCallKinds:
    if n[0].kind == nkSym and n[0].sym.magic == mDestroy:
      let t = n[1].typ.skipTypes(abstractVar)
      if getAttachedOp(g, t, attachedDestructor) == nil:
        discard produceSym(g, c, t, attachedDestructor, info, idgen)

      let op = getAttachedOp(g, t, attachedDestructor)
      if op != nil:
        g.config.internalAssert(not op.ast.isGenericRoutine, info, "resolved destructor is generic")
        g.config.internalAssert(op.magic != mDestroy, info, "patching mDestroy with mDestroy?")
        n[0] = newSymNode(op)
  for x in n: patchBody(g, c, x, info, idgen)

proc inst(g: ModuleGraph; c: PContext; t: PType; kind: TTypeAttachedOp; idgen: IdGenerator;
          info: TLineInfo) =
  let op = getAttachedOp(g, t, kind)
  if op != nil and op.ast != nil and op.ast.isGenericRoutine:
    if t.typeInst != nil:
      var a: TLiftCtx
      a.info = info
      a.g = g
      a.kind = kind
      a.c = c
      a.idgen = idgen

      let opInst = instantiateGeneric(a, op, t, t.typeInst)
      if opInst.ast != nil:
        patchBody(g, c, opInst.ast, info, a.idgen)
      setAttachedOp(g, idgen.module, t, kind, opInst)
    else:
      localReport(g.config, info, reportSem(rsemUnresolvedGenericParameter))

proc createTypeBoundOps(g: ModuleGraph; c: PContext; orig: PType; info: TLineInfo;
                        idgen: IdGenerator) =
  ## In the semantic pass this is called in strategic places
  ## to ensure we lift assignment, destructors and moves properly.
  ## The later 'injectdestructors' pass depends on it.
  if orig == nil or {tfCheckedForDestructor, tfHasMeta} * orig.flags != {}: return
  incl orig.flags, tfCheckedForDestructor

  let skipped = orig.skipTypes({tyGenericInst, tyAlias, tySink})
  if isEmptyContainer(skipped) or skipped.kind == tyStatic: return

  let h = sighashes.hashType(skipped, {CoType, CoDistinct})
  var canon = g.canonTypes.getOrDefault(h)
  if canon == nil:
    g.canonTypes[h] = skipped
    canon = skipped

  # multiple cases are to distinguish here:
  # 1. we don't know yet if 'typ' has a nontrival destructor.
  # 2. we have a nop destructor. --> mDestroy
  # 3. we have a lifted destructor.
  # 4. We have a custom destructor.
  # 5. We have a (custom) generic destructor.

  # we do not generate '=trace' procs if we
  # have the cycle detection disabled, saves code size.
  let lastAttached = if g.config.selectedGC == gcOrc: attachedTrace
                     else: attachedSink

  # bug #15122: We need to produce all prototypes before entering the
  # mind boggling recursion. Hacks like these imply we should rewrite
  # this module.
  var generics: array[attachedDestructor..attachedTrace, bool]
  for k in attachedDestructor..lastAttached:
    generics[k] = getAttachedOp(g, canon, k) != nil
    if not generics[k]:
      setAttachedOp(g, idgen.module, canon, k,
          symPrototype(g, canon, canon.owner, k, info, idgen))

  # we generate the destructor first so that other operators can depend on it:
  for k in attachedDestructor..lastAttached:
    if not generics[k]:
      discard produceSym(g, c, canon, k, info, idgen)
    else:
      inst(g, c, canon, k, idgen, info)
    if canon != orig:
      setAttachedOp(g, idgen.module, orig, k, getAttachedOp(g, canon, k))

  let op = getAttachedOp(g, orig, attachedDestructor)
  if op != nil and getBody(g, op).len != 0:
    #or not isTrival(orig.assignment) or
    # not isTrival(orig.sink):
    orig.flags.incl tfHasAsgn
    # ^ XXX Breaks IC!
