#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the transformator. It transforms the syntax tree
## to ease the work of the code generators. Does some transformations:
##
## * inlines iterators
## * inlines constants
## * performs constant folding
## * converts "continue" to "break"; disambiguates "break"
## * introduces method dispatchers
## * performs lambda lifting for closure support
## * transforms 'defer' into a 'try finally' statement

import
  compiler/ast/[
    ast,
    astalgo,
    trees,
    reports,
    idents,
    renderer,
    types,
    lineinfos,
    errorreporting
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/sem/[
    semfold,
    lowerings
  ],
  compiler/backend/[  # xxx: sigh
    cgmeth
  ],
  compiler/utils/[
    astrepr
  ]

proc transformBody*(g: ModuleGraph; idgen: IdGenerator, prc: PSym, cache: bool): PNode

import closureiters, lambdalifting

type
  PTransCon = ref object # part of TContext; stackable
    mapping: TIdNodeTable     # mapping from symbols to nodes
    owner: PSym               # current owner
    forStmt: PNode            # current for stmt
    forLoopBody: PNode   # transformed for loop body
    yieldStmts: int           # we count the number of yield statements,
                              # because we need to introduce new variables
                              # if we encounter the 2nd yield statement
    next: PTransCon           # for stacking

  PTransf = ref object
    module: PSym
    transCon: PTransCon      # top of a TransCon stack
    inlining: int            # > 0 if we are in inlining context (copy vars)
    nestedProcs: int         # > 0 if we are in a nested proc
    contSyms, breakSyms: seq[PSym]  # to transform 'continue' and 'break'
    deferDetected, tooEarly: bool
    graph: ModuleGraph
    idgen: IdGenerator

proc pushTransCon(c: PTransf, t: PTransCon) =
  t.next = c.transCon
  c.transCon = t

proc popTransCon(c: PTransf) =
  c.graph.config.internalAssert(c.transCon != nil, "popTransCon")

  c.transCon = c.transCon.next

proc getCurrOwner(c: PTransf): PSym =
  if c.transCon != nil: result = c.transCon.owner
  else: result = c.module

proc newTemp(c: PTransf, typ: PType, info: TLineInfo): PNode =
  let r = newSym(skTemp, getIdent(c.graph.cache, genPrefix), nextSymId(c.idgen), getCurrOwner(c), info)
  r.typ = typ #skipTypes(typ, {tyGenericInst, tyAlias, tySink})
  incl(r.flags, sfFromGeneric)
  let owner = getCurrOwner(c)
  if owner.isIterator and not c.tooEarly:
    result = freshVarForClosureIter(c.graph, r, c.idgen, owner)
  else:
    result = newSymNode(r)

proc transform(c: PTransf, n: PNode): PNode

proc transformSons(c: PTransf, n: PNode): PNode =
  result = shallowCopy(n)
  for i in 0..<n.len:
    result[i] = transform(c, n[i])

proc newAsgnStmt(c: PTransf, kind: TNodeKind, le: PNode, ri: PNode): PNode =
  result = newTreeI(kind, ri.info): [le, ri]

proc transformSymAux(c: PTransf, n: PNode): PNode =
  let s = n.sym
  if s.typ != nil and s.typ.callConv == ccClosure:
    if s.kind in routineKinds:
      discard transformBody(c.graph, c.idgen, s, true)
    if s.kind == skIterator:
      if c.tooEarly: return n
      else: return liftIterSym(c.graph, n, c.idgen, getCurrOwner(c))
    elif s.kind in {skProc, skFunc, skConverter, skMethod} and not c.tooEarly:
      # top level .closure procs are still somewhat supported for 'Nake':
      return makeClosure(c.graph, c.idgen, s, nil, n.info)
  #elif n.sym.kind in {skVar, skLet} and n.sym.typ.callConv == ccClosure:
  #  echo n.info, " come heer for ", c.tooEarly
  #  if not c.tooEarly:
  var b: PNode
  var tc = c.transCon
  if sfBorrow in s.flags and s.kind in routineKinds:
    # Recurse all the procs and swap syms
    var s = s
    while true:
      # Skips over all borrowed procs getting the last proc symbol without an implementation
      let body = getBody(c.graph, s)
      if body.kind == nkSym and sfBorrow in body.sym.flags and getBody(c.graph, body.sym).kind == nkSym:
        s = body.sym
      else:
        break
    b = getBody(c.graph, s)
    c.graph.config.internalAssert(b.kind == nkSym, n.info, "wrong AST for borrowed symbol")

    b = newSymNode(b.sym, n.info)
  elif c.inlining > 0:
    # see bug #13596: we use ref-based equality in the DFA for destruction
    # injections so we need to ensure unique nodes after iterator inlining
    # which can lead to duplicated for loop bodies! Consider:
    #[
      while remaining > 0:
        if ending == nil:
          yield ms
          break
        ...
        yield ms
    ]#
    b = newSymNode(n.sym, n.info)
  else:
    b = n
  while tc != nil:
    result = idNodeTableGet(tc.mapping, b.sym)
    if result != nil:
      # this slightly convoluted way ensures the line info stays correct:
      if result.kind == nkSym:
        result = copyNode(result)
        result.info = n.info
      return
    tc = tc.next
  result = b

proc transformSym(c: PTransf, n: PNode): PNode =
  result = transformSymAux(c, n)

proc freshVar(c: PTransf; v: PSym): PNode =
  let owner = getCurrOwner(c)
  if owner.isIterator and not c.tooEarly:
    result = freshVarForClosureIter(c.graph, v, c.idgen, owner)
  else:
    var newVar = copySym(v, nextSymId(c.idgen))
    incl(newVar.flags, sfFromGeneric)
    newVar.owner = owner
    result = newSymNode(newVar)

proc transformVarSection(c: PTransf, v: PNode): PNode =
  c.graph.config.internalAssert(v.kind in nkVariableSections,
                                "not a variable section, got: " & $v.kind)

  result = shallowCopy(v)

  for i in 0..<v.len:
    var it = v[i]
    case it.kind
    of nkCommentStmt:
      result[i] = it
    of nkIdentDefs:
      if it[0].kind == nkSym:
        internalAssert(c.graph.config, it.len == 3,
                       "var section must have three subnodes, got: " & $it.len)
        
        let x = freshVar(c, it[0].sym)
        
        idNodeTablePut(c.transCon.mapping, it[0].sym, x)
        
        var defs = newTreeI(nkIdentDefs, it.info):
          [x, it[1], transform(c, it[2])]
        
        if importantComments(c.graph.config):
          # keep documentation information:
          defs.comment = it.comment
        
        if x.kind == nkSym:
          x.sym.ast = defs[2]
        
        result[i] = defs
      else:
        # has been transformed into 'param.x' for closure iterators, so just
        # transform it:
        result[i] = transform(c, it)
    of nkVarTuple:
      let defs = newNodeI(it.kind, it.info, it.len)

      for j in 0..<it.len-2:
        defs[j] =
          if it[j].kind == nkSym:
            let x = freshVar(c, it[j].sym)
            idNodeTablePut(c.transCon.mapping, it[j].sym, x)
            x
          else:
            transform(c, it[j])
      
      assert(it[^2].kind == nkEmpty)
      
      defs[^2] = newNodeI(nkEmpty, it.info)
      defs[^1] = transform(c, it[^1])
      
      result[i] = defs
    else:
      c.graph.config.internalError(it.info):
        "transformVarSection expected identdefs, tuple, or comment, got: " &
          $it.kind

proc transformConstSection(c: PTransf, v: PNode): PNode =
  result = v
  when false:
    result = shallowCopy(v)
    for i in 0..<v.len:
      var it = v[i]
      if it.kind == nkCommentStmt:
        result[i] = it
      else:
        c.graph.config.internalAssert(it.kind == nkConstDef, it.info, "transformConstSection")
        c.graph.config.internalAssert(it[0].kind == nkSym, it.info, "transformConstSection")

        result[i] = it

proc hasContinue(n: PNode): bool =
  case n.kind
  of nkEmpty..nkNilLit, nkForStmt, nkWhileStmt: discard
  of nkContinueStmt: result = true
  else:
    for i in 0..<n.len:
      if hasContinue(n[i]): return true

proc newLabel(c: PTransf, n: PNode): PSym =
  result = newSym(skLabel, nil, nextSymId(c.idgen), getCurrOwner(c), n.info)
  result.name = getIdent(c.graph.cache, genPrefix)

proc transformBlock(c: PTransf, n: PNode): PNode =
  var labl: PSym
  if c.inlining > 0:
    labl = newLabel(c, n[0])
    idNodeTablePut(c.transCon.mapping, n[0].sym, newSymNode(labl))
  else:
    labl =
      if n[0].kind != nkEmpty:
        n[0].sym  # already named block? -> Push symbol on the stack
      else:
        newLabel(c, n)
  c.breakSyms.add(labl)
  result = transformSons(c, n)
  discard c.breakSyms.pop
  result[0] = newSymNode(labl)

proc transformLoopBody(c: PTransf, n: PNode): PNode =
  # What if it contains "continue" and "break"? "break" needs
  # an explicit label too, but not the same!

  # We fix this here by making every 'break' belong to its enclosing loop
  # and changing all breaks that belong to a 'block' by annotating it with
  # a label (if it hasn't one already).
  if hasContinue(n):
    let labl = newLabel(c, n)
    c.contSyms.add(labl)

    result = newTreeI(nkBlockStmt, n.info):
      [newSymNode(labl), transform(c, n)]
    discard c.contSyms.pop()
  else:
    result = transform(c, n)

proc transformWhile(c: PTransf; n: PNode): PNode =
  if c.inlining > 0:
    result = transformSons(c, n)
  else:
    let labl = newLabel(c, n)
    c.breakSyms.add(labl)

    var body = shallowCopy(n)
    for i in 0..<n.len-1:
      body[i] = transform(c, n[i])
    body[^1] = transformLoopBody(c, n[^1])

    result = newTreeI(nkBlockStmt, n.info):
      [newSymNode(labl), body]
    discard c.breakSyms.pop

proc transformBreak(c: PTransf, n: PNode): PNode =
  result = transformSons(c, n)
  if n[0].kind == nkEmpty and c.breakSyms.len > 0:
    let labl = c.breakSyms[^1]
    result[0] = newSymNode(labl)

proc introduceNewLocalVars(c: PTransf, n: PNode): PNode =
  case n.kind
  of nkSym:
    result = transformSym(c, n)
  of nkEmpty..pred(nkSym), succ(nkSym)..nkNilLit:
    # nothing to be done for leaves:
    result = n
  of nkVarSection, nkLetSection:
    result = transformVarSection(c, n)
  of nkClosure:
    # it can happen that for-loop-inlining produced a fresh
    # set of variables, including some computed environment
    # (bug #2604). We need to patch this environment here too:
    let a = n[1]
    if a.kind == nkSym:
      n[1] = transformSymAux(c, a)
    return n
  else:
    result = shallowCopy(n)
    for i in 0..<n.len:
      result[i] = introduceNewLocalVars(c, n[i])

proc transformAsgn(c: PTransf, n: PNode): PNode =
  let rhs = n[1]

  if rhs.kind != nkTupleConstr:
    return transformSons(c, n)

  # Unpack the tuple assignment into N temporary variables and then pack them
  # into a tuple: this allows us to get the correct results even when the rhs
  # depends on the value of the lhs
  let letSection = newNodeI(nkLetSection, n.info, rhs.len)
  let newTupleConstr = newNodeI(nkTupleConstr, n.info, rhs.len)
  for i, field in rhs:
    let val = if field.kind == nkExprColonExpr: field[1] else: field
    let def = newTreeI(nkIdentDefs, field.info):
      [newTemp(c, val.typ, field.info), newNodeI(nkEmpty, field.info), transform(c, val)]
    letSection[i] = def
    # NOTE: We assume the constructor fields are in the correct order for the
    # given tuple type
    newTupleConstr[i] = def[0]

  newTupleConstr.typ = rhs.typ

  let asgnNode = newTreeI(nkAsgn, n.info):
    [transform(c, n[0]), newTupleConstr]

  result = newTreeI(nkStmtList, n.info):
    [letSection, asgnNode]

proc transformYield(c: PTransf, n: PNode): PNode =
  proc asgnTo(lhs: PNode, rhs: PNode): PNode =
    # Choose the right assignment instruction according to the given ``lhs``
    # node since it may not be a nkSym (a stack-allocated skForVar) but a
    # nkDotExpr (a heap-allocated slot into the envP block)
    case lhs.kind
    of nkSym:
      internalAssert(
        c.graph.config, lhs.sym.kind == skForVar, "assign lhs must be mutable")

      result = newAsgnStmt(c, nkFastAsgn, lhs, rhs)
    of nkDotExpr:
      result = newAsgnStmt(c, nkAsgn, lhs, rhs)
    else:
      internalAssert(
        c.graph.config, false, "Unexpected symbol for assign")

  result = newNodeI(nkStmtList, n.info)
  var e = n[0]
  # c.transCon.forStmt.len == 3 means that there is one for loop variable
  # and thus no tuple unpacking:
  if e.typ.isNil: return result # can happen in nimsuggest for unknown reasons
  if c.transCon.forStmt.len != 3:
    e = skipConv(e)
    if e.kind == nkTupleConstr:
      for i in 0..<e.len:
        var v = e[i]
        if v.kind == nkExprColonExpr: v = v[1]
        if c.transCon.forStmt[i].kind == nkVarTuple:
          for j in 0..<c.transCon.forStmt[i].len-1:
            let lhs = c.transCon.forStmt[i][j]
            let rhs = transform(c, newTupleAccess(c.graph, v, j))
            result.add(asgnTo(lhs, rhs))
        else:
          let lhs = c.transCon.forStmt[i]
          let rhs = transform(c, v)
          result.add(asgnTo(lhs, rhs))
    elif e.kind notin {nkAddr, nkHiddenAddr}: # no need to generate temp for address operation
      # TODO do not use temp for nodes which cannot have side-effects
      var tmp = newTemp(c, e.typ, e.info)
      let v = newTreeI(nkVarSection, e.info):
        newIdentDefs(tmp, e)

      result.add transform(c, v)

      for i in 0..<c.transCon.forStmt.len - 2:
        let lhs = c.transCon.forStmt[i]
        let rhs = transform(c, newTupleAccess(c.graph, tmp, i))
        result.add(asgnTo(lhs, rhs))
    else:
      for i in 0..<c.transCon.forStmt.len - 2:
        let lhs = c.transCon.forStmt[i]
        let rhs = transform(c, newTupleAccess(c.graph, e, i))
        result.add(asgnTo(lhs, rhs))
  else:
    if c.transCon.forStmt[0].kind == nkVarTuple:
      var notLiteralTuple = false # we don't generate temp for tuples with const value: (1, 2, 3)
      let ev = e.skipConv
      if ev.kind == nkTupleConstr:
        for i in ev:
          if not isConstExpr(i):
            notLiteralTuple = true
            break
      else:
        notLiteralTuple = true

      if e.kind notin {nkAddr, nkHiddenAddr} and notLiteralTuple:
        # TODO do not use temp for nodes which cannot have side-effects
        var tmp = newTemp(c, e.typ, e.info)
        let v = newTreeI(nkVarSection, e.info):
          newIdentDefs(tmp, e)

        result.add transform(c, v)
        for i in 0..<c.transCon.forStmt[0].len-1:
          let lhs = c.transCon.forStmt[0][i]
          let rhs = transform(c, newTupleAccess(c.graph, tmp, i))
          result.add(asgnTo(lhs, rhs))
      else:
        for i in 0..<c.transCon.forStmt[0].len-1:
          let lhs = c.transCon.forStmt[0][i]
          let rhs = transform(c, newTupleAccess(c.graph, e, i))
          result.add(asgnTo(lhs, rhs))
    else:
      let lhs = c.transCon.forStmt[0]
      let rhs = transform(c, e)
      result.add(asgnTo(lhs, rhs))

  inc(c.transCon.yieldStmts)
  if c.transCon.yieldStmts <= 1:
    # common case
    result.add(c.transCon.forLoopBody)
  else:
    # we need to introduce new local variables:
    result.add(introduceNewLocalVars(c, c.transCon.forLoopBody))

  for idx in 0 ..< result.len:
    var changeNode = result[idx]
    changeNode.info = c.transCon.forStmt.info
    for i, child in changeNode:
      child.info = changeNode.info

proc transformAddrDeref(c: PTransf, n: PNode, a, b: TNodeKind): PNode =
  result = transformSons(c, n)
  if c.graph.config.backend == backendCpp or sfCompileToCpp in c.module.flags: return
  var n = result
  case n[0].kind
  of nkObjUpConv, nkObjDownConv, nkChckRange, nkChckRangeF, nkChckRange64:
    var m = n[0][0]
    if m.kind == a or m.kind == b:
      # addr ( nkConv ( deref ( x ) ) ) --> nkConv(x)
      n[0][0] = m[0]
      result = n[0]
      if n.typ.skipTypes(abstractVar).kind != tyOpenArray:
        result.typ = n.typ
      elif n.typ.skipTypes(abstractInst).kind in {tyVar}:
        result.typ = toVar(result.typ, n.typ.skipTypes(abstractInst).kind, c.idgen)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    var m = n[0][1]
    if m.kind == a or m.kind == b:
      # addr ( nkConv ( deref ( x ) ) ) --> nkConv(x)
      n[0][1] = m[0]
      result = n[0]
      if n.typ.skipTypes(abstractVar).kind != tyOpenArray:
        result.typ = n.typ
      elif n.typ.skipTypes(abstractInst).kind in {tyVar}:
        result.typ = toVar(result.typ, n.typ.skipTypes(abstractInst).kind, c.idgen)
  else:
    if n[0].kind == a or n[0].kind == b:
      # addr ( deref ( x )) --> x
      result = n[0][0]
      if n.typ.skipTypes(abstractVar).kind != tyOpenArray:
        result.typ = n.typ

proc generateThunk(c: PTransf; prc: PNode, dest: PType): PNode =
  ## Converts 'prc' into '(thunk, nil)' so that it's compatible with
  ## a closure.

  # we cannot generate a proper thunk here for GC-safety reasons
  # (see internal documentation):
  if c.graph.config.backend == backendJs: return prc
  c.graph.config.internalAssert(prc.kind != nkClosure, prc.info, "closure to closure created")

  let conv = newTreeIT(nkHiddenSubConv, prc.info, dest):
    [newNodeI(nkEmpty, prc.info), prc]

  result = newTreeIT(nkClosure, prc.info, dest):
    [conv, newNodeIT(nkNilLit, prc.info, getSysType(c.graph, prc.info, tyNil))]

proc transformConv(c: PTransf, n: PNode): PNode =
  # numeric types need range checks:
  var dest = skipTypes(n.typ, abstractVarRange)
  var source = skipTypes(n[1].typ, abstractVarRange)
  case dest.kind
  of tyInt..tyInt64, tyEnum, tyChar, tyUInt8..tyUInt32:
    # we don't include uint and uint64 here as these are no ordinal types ;-)
    if not isOrdinalType(source):
      # float -> int conversions. ugh.
      result = transformSons(c, n)
    elif firstOrd(c.graph.config, n.typ) <= firstOrd(c.graph.config, n[1].typ) and
        lastOrd(c.graph.config, n[1].typ) <= lastOrd(c.graph.config, n.typ):
      # BUGFIX: simply leave n as it is; we need a nkConv node,
      # but no range check:
      result = transformSons(c, n)
    else:
      # generate a range check:
      let rangeDest = skipTypes(n.typ, abstractVar)
      result = newTreeIT(
        if tyInt64 in {dest.kind, source.kind}: nkChckRange64 else: nkChckRange,
        n.info, n.typ):
        [transform(c, n[1]),
         newIntTypeNode(firstOrd(c.graph.config, rangeDest), rangeDest),
         newIntTypeNode(lastOrd(c.graph.config, rangeDest), rangeDest)]
  of tyFloat..tyFloat128:
    # XXX int64 -> float conversion?
    let rangeDest = skipTypes(n.typ, abstractVar)
    if rangeDest.kind == tyRange:
      result = newTreeIT(nkChckRangeF, n.info, n.typ):
        [transform(c, n[1]), copyTree(rangeDest.n[0]), copyTree(rangeDest.n[1])]
    else:
      result = transformSons(c, n)
  of tyOpenArray, tyVarargs:
    result = transform(c, n[1])
    #result = transformSons(c, n)
    result.typ = takeType(n.typ, n[1].typ, c.graph, c.idgen)
    #echo n.info, " came here and produced ", typeToString(result.typ),
    #   " from ", typeToString(n.typ), " and ", typeToString(n[1].typ)
  of tyCstring:
    if source.kind == tyString:
      result = newTreeIT(nkStringToCString, n.info, n.typ): transform(c, n[1])
    else:
      result = transformSons(c, n)
  of tyString:
    if source.kind == tyCstring:
      result = newTreeIT(nkCStringToString, n.info, n.typ): transform(c, n[1])
    else:
      result = transformSons(c, n)
  of tyRef, tyPtr:
    dest = skipTypes(dest, abstractPtrs)
    source = skipTypes(source, abstractPtrs)
    if source.kind == tyObject:
      let diff = inheritanceDiff(dest, source)
      if diff == 0 or diff == high(int):
        result = transform(c, n[1])
        result.typ = n.typ
      else:
        result = newTreeIT(
          if diff < 0: nkObjUpConv else: nkObjDownConv,
          n.info, n.typ): transform(c, n[1])
    else:
      result = transformSons(c, n)
  of tyObject:
    template convHint() =
      if n.kind == nkHiddenSubConv and n.typ.kind notin abstractVarRange:
        # Creates hint on converstion to parent type where information is lost,
        # presently hints on `proc(thing)` where thing converts to non var base.
        localReport(c.graph.config, n[1], SemReport(
          kind: rsemImplicitObjConv,
          typeMismatch: @[c.graph.config.typeMismatch(
            formal = dest, actual = source)]))

    let diff = inheritanceDiff(dest, source)
    if diff == 0 or diff == high(int):
      result = transform(c, n[1])
      result.typ = n.typ
    else:
      convHint()
      result = newTreeIT(
        if diff < 0: nkObjUpConv else: nkObjDownConv,
        n.info, n.typ): transform(c, n[1])
  of tyGenericParam, tyOrdinal:
    result = transform(c, n[1])
    # happens sometimes for generated assignments, etc.
  of tyProc:
    result = transformSons(c, n)
    if dest.callConv == ccClosure and source.callConv == ccNimCall:
      result = generateThunk(c, result[1], dest)
  else:
    result = transformSons(c, n)

type
  TPutArgInto = enum
    paDirectMapping, paFastAsgn, paFastAsgnTakeTypeFromArg
    paVarAsgn, paComplexOpenarray

proc putArgInto(arg: PNode, formal: PType): TPutArgInto =
  # This analyses how to treat the mapping "formal <-> arg" in an
  # inline context.
  if formal.kind == tyTypeDesc: return paDirectMapping
  if skipTypes(formal, abstractInst).kind in {tyOpenArray, tyVarargs}:
    case arg.kind
    of nkStmtListExpr:
      return paComplexOpenarray
    of nkBracket:
      return paFastAsgnTakeTypeFromArg
    else:
      # XXX incorrect, causes #13417 when `arg` has side effects.
      return paDirectMapping
  case arg.kind
  of nkEmpty..nkNilLit:
    result = paDirectMapping
  of nkDotExpr, nkDerefExpr, nkHiddenDeref, nkAddr, nkHiddenAddr:
    result = putArgInto(arg[0], formal)
  of nkCurly, nkBracket:
    for i in 0..<arg.len:
      if putArgInto(arg[i], formal) != paDirectMapping:
        return paFastAsgn
    result = paDirectMapping
  of nkPar, nkTupleConstr, nkObjConstr:
    for i in 0..<arg.len:
      let a = if arg[i].kind == nkExprColonExpr: arg[i][1]
              else: arg[0]
      if putArgInto(a, formal) != paDirectMapping:
        return paFastAsgn
    result = paDirectMapping
  else:
    if skipTypes(formal, abstractInst).kind in {tyVar, tyLent}: result = paVarAsgn
    else: result = paFastAsgn

proc findWrongOwners(c: PTransf, n: PNode) =
  if n.kind == nkVarSection:
    let x = n[0][0]
    c.graph.config.internalAssert(x.kind != nkSym or x.sym.owner == getCurrOwner(c),
      x.info, "bah " & x.sym.name.s & " " & x.sym.owner.name.s & " " & getCurrOwner(c).name.s)
  else:
    for i in 0..<n.safeLen: findWrongOwners(c, n[i])

proc isSimpleIteratorVar(c: PTransf; iter: PSym): bool =
  proc rec(n: PNode; owner: PSym; dangerousYields: var int) =
    case n.kind
    of nkEmpty..nkNilLit: discard
    of nkYieldStmt:
      if n[0].kind == nkSym and n[0].sym.owner == owner:
        discard "good: yield a single variable that we own"
      else:
        inc dangerousYields
    else:
      for c in n: rec(c, owner, dangerousYields)

  var dangerousYields = 0
  rec(getBody(c.graph, iter), iter, dangerousYields)
  result = dangerousYields == 0

template destructor(t: PType): PSym = getAttachedOp(c.graph, t, attachedDestructor)

proc transformFor(c: PTransf, n: PNode): PNode =
  # generate access statements for the parameters (unless they are constant)
  # put mapping from formal parameters to actual parameters
  c.graph.config.internalAssert(n.kind == nkForStmt, n.info, "transformFor")

  var call = n[^2]

  let labl = newLabel(c, n)
  result = newNodeI(nkBlockStmt, n.info, 2)
  result[0] = newSymNode(labl)
  if call.typ.isNil:
    # see bug #3051
    result[1] = newNode(nkEmpty)
  elif call.kind notin nkCallKinds or call[0].kind != nkSym or
      call[0].typ.skipTypes(abstractInst).callConv == ccClosure:
    c.breakSyms.add(labl)
    result[1] = n
    result[1][^1] = transformLoopBody(c, n[^1])
    result[1][^2] = transform(c, n[^2])
    result[1] = lambdalifting.liftForLoop(c.graph, result[1], c.idgen, getCurrOwner(c))
    discard c.breakSyms.pop
  else:
    var stmtList = newNodeI(nkStmtList, n.info)
    result[1] = stmtList

    c.breakSyms.add(labl)
    let loopBody = transformLoopBody(c, n[^1])
    discard c.breakSyms.pop

    let iter = call[0].sym

    var v = newNodeI(nkVarSection, n.info)
    for i in 0..<n.len - 2:
      if n[i].kind == nkVarTuple:
        for j in 0..<n[i].len-1:
          v.add newIdentDefs(copyTree(n[i][j])) # declare new vars
      else:
        if n[i].kind == nkSym and isSimpleIteratorVar(c, iter):
          incl n[i].sym.flags, sfCursor
        v.add newIdentDefs(copyTree(n[i])) # declare new vars
    stmtList.add(v)

    # this can fail for 'nimsuggest' and 'check':
    if iter.kind != skIterator: return result

    # Bugfix: inlined locals belong to the invoking routine, not to the invoked
    # iterator!
    var newC = PTransCon(
      owner: getCurrOwner(c),
      mapping: newIdNodeTable(),
      forStmt: n,
      forLoopBody: loopBody)
    # generate access statements for the parameters (unless they are constant)
    pushTransCon(c, newC)
    for i in 1..<call.len:
      var arg = transform(c, call[i])
      let ff = skipTypes(iter.typ, abstractInst)
      # can happen for 'nim check':
      if i >= ff.n.len: return result
      var formal = ff.n[i].sym
      let pa = putArgInto(arg, formal.typ)
      case pa
      of paDirectMapping:
        idNodeTablePut(newC.mapping, formal, arg)
      of paFastAsgn, paFastAsgnTakeTypeFromArg:
        var t = formal.typ
        if pa == paFastAsgnTakeTypeFromArg:
          t = arg.typ
        elif formal.ast != nil and formal.ast.typ.destructor != nil and t.destructor == nil:
          t = formal.ast.typ # better use the type that actually has a destructor.
        elif t.destructor == nil and arg.typ.destructor != nil:
          t = arg.typ
        # generate a temporary and produce an assignment statement:
        let temp = newTemp(c, t, formal.info)
        v.add newIdentDefs(temp)
        stmtList.add(newAsgnStmt(c, nkFastAsgn, temp, arg))
        idNodeTablePut(newC.mapping, formal, temp)
      of paVarAsgn:
        assert(skipTypes(formal.typ, abstractInst).kind in {tyVar})
        idNodeTablePut(newC.mapping, formal, arg)
        # XXX BUG still not correct if the arg has a side effect!
      of paComplexOpenarray:
        # arrays will deep copy here (pretty bad).
        let temp = newTemp(c, arg.typ, formal.info)
        v.add newIdentDefs(temp)
        stmtList.add(newAsgnStmt(c, nkFastAsgn, temp, arg))
        idNodeTablePut(newC.mapping, formal, temp)

    let body = transformBody(c.graph, c.idgen, iter, true)
    pushInfoContext(c.graph.config, n.info)
    inc(c.inlining)
    stmtList.add(transform(c, body))
    #findWrongOwners(c, stmtList.PNode)
    dec(c.inlining)
    popInfoContext(c.graph.config)
    popTransCon(c)

proc transformCase(c: PTransf, n: PNode): PNode =
  # removes `elif` branches of a case stmt
  # adds ``else: nil`` if needed for the code generator
  result = newNodeIT(nkCaseStmt, n.info, n.typ)
  var ifs: PNode = nil
  for it in n:
    var e = transform(c, it)
    case it.kind
    of nkElifBranch:
      if ifs == nil:
        # Generate the right node depending on whether `n` is used as a stmt or
        # as an expr
        let kind = if n.typ != nil: nkIfExpr else: nkIfStmt
        ifs = newNodeIT(kind, it.info, n.typ)
      ifs.add(e)
    of nkElse:
      if ifs == nil: result.add(e)
      else: ifs.add(e)
    else:
      result.add(e)
  if ifs != nil:
    var elseBranch = newTreeI(nkElse, n.info): ifs
    result.add(elseBranch)
  elif result.lastSon.kind != nkElse and not (
      skipTypes(n[0].typ, abstractVarRange).kind in
        {tyInt..tyInt64, tyChar, tyEnum, tyUInt..tyUInt64}):
    # fix a stupid code gen bug by normalizing:
    let elseBranch = newTreeI(nkElse, n.info): newNodeI(nkNilLit, n.info)
    result.add(elseBranch)

proc transformArrayAccess(c: PTransf, n: PNode): PNode =
  # XXX this is really bad; transf should use a proper AST visitor
  if n[0].kind == nkSym and n[0].sym.kind == skType:
    result = n
  else:
    result = shallowCopy(n)
    for i in 0..<n.len:
      result[i] = transform(c, skipConv(n[i]))

proc getMergeOp(n: PNode): PSym =
  case n.kind
  of nkCall, nkHiddenCallConv, nkCommand, nkInfix, nkPrefix, nkPostfix,
     nkCallStrLit:
    if n[0].kind == nkSym and n[0].sym.magic == mConStrStr:
      result = n[0].sym
  else: discard

proc flattenTreeAux(d, a: PNode, op: PSym) =
  let op2 = getMergeOp(a)
  if op2 != nil and
      (op2.id == op.id or op.magic != mNone and op2.magic == op.magic):
    for i in 1..<a.len: flattenTreeAux(d, a[i], op)
  else:
    d.add copyTree(a)

proc flattenTree(root: PNode): PNode =
  let op = getMergeOp(root)
  if op != nil:
    result = copyNode(root)
    result.add copyTree(root[0])
    flattenTreeAux(result, root, op)
  else:
    result = root

proc transformCall(c: PTransf, n: PNode): PNode =
  var n = flattenTree(n)
  let op = getMergeOp(n)
  let magic = getMagic(n)
  if op != nil and op.magic != mNone and n.len >= 3:
    result = newNodeIT(nkCall, n.info, n.typ)
    result.add(transform(c, n[0]))
    var j = 1
    while j < n.len:
      var a = transform(c, n[j])
      inc(j)
      if isConstExpr(a):
        while (j < n.len):
          let b = transform(c, n[j])
          if not isConstExpr(b): break
          a = evalOp(op.magic, n, a, b, nil, c.idgen, c.graph)
          inc(j)
      result.add(a)
    if result.len == 2: result = result[1]
  elif magic == mAddr:
    result = newTreeIT(nkAddr, n.info, n.typ): n[1]
    result = transformAddrDeref(c, result, nkDerefExpr, nkHiddenDeref)
  elif magic in {mNBindSym, mTypeOf, mRunnableExamples}:
    # for bindSym(myconst) we MUST NOT perform constant folding:
    result = n
  elif magic == mProcCall:
    # but do not change to its dispatcher:
    result = transformSons(c, n[1])
  elif magic == mStrToStr:
    result = transform(c, n[1])
  else:
    let s = transformSons(c, n)
    # bugfix: check after 'transformSons' if it's still a method call:
    # use the dispatcher for the call:
    if s[0].kind == nkSym and s[0].sym.kind == skMethod:
      when false:
        let t = lastSon(s[0].sym.ast)
        if t.kind != nkSym or sfDispatcher notin t.sym.flags:
          methodDef(s[0].sym, false)
      result = methodCall(s, c.graph.config)
    else:
      result = s

proc transformExceptBranch(c: PTransf, n: PNode): PNode =
  if n[0].isInfixAs() and not isImportedException(n[0][1].typ, c.graph.config):
    let excTypeNode = n[0][1]
    # Generating `let exc = (excType)(getCurrentException())`
    # -> getCurrentException()
    let excCall = callCodegenProc(c.graph, "getCurrentException")
    # -> (excType)
    let convNode = newTreeIT(nkHiddenSubConv, n[1].info, excTypeNode.typ.toRef(c.idgen)):
      [newNodeI(nkEmpty, n.info), excCall]
    # -> let exc = ...
    let identDefs = newTreeI(nkIdentDefs, n[1].info):
      [n[0][2], newNodeI(nkEmpty, n.info), convNode]

    let letSection = newTreeI(nkLetSection, n[1].info): identDefs
    # Place the let statement and body of the 'except' branch into new stmtList.
    let actions = newTreeIT(nkStmtListExpr, n[1].info, n[1].typ):
      [letSection, transform(c, n[1])]
    # Overwrite 'except' branch body with our stmtList.
    result = newTreeI(nkExceptBranch, n[1].info):
      # Replace the `Exception as foobar` with just `Exception`.
      [transform(c, n[0][1]), actions]
  else:
    result = transformSons(c, n)

proc commonOptimizations*(g: ModuleGraph; idgen: IdGenerator; c: PSym, n: PNode): PNode =
  result = n
  
  if result.kind == nkError:
    return

  for i in 0..<n.safeLen:
    result[i] = commonOptimizations(g, idgen, c, n[i])
  var op = getMergeOp(n)
  if (op != nil) and (op.magic != mNone) and (n.len >= 3):
    result = newNodeIT(nkCall, n.info, n.typ)
    result.add(n[0])
    var args = newNode(nkArgList)
    flattenTreeAux(args, n, op)
    var j = 0
    while j < args.len:
      var a = args[j]
      inc(j)
      if isConstExpr(a):
        while j < args.len:
          let b = args[j]
          if not isConstExpr(b): break
          a = evalOp(op.magic, result, a, b, nil, idgen, g)
          inc(j)
      result.add(a)
    if result.len == 2: result = result[1]
  else:
    var cnst = getConstExpr(c, n, idgen, g)
    # we inline constants if they are not complex constants:
    if cnst != nil and not dontInlineConstant(n, cnst):
      result = cnst
    else:
      result = n

proc transform(c: PTransf, n: PNode): PNode =
  when false:
    var oldDeferAnchor: PNode
    if n.kind in {nkElifBranch, nkOfBranch, nkExceptBranch, nkElifExpr,
                  nkElseExpr, nkElse, nkForStmt, nkWhileStmt, nkFinally,
                  nkBlockStmt, nkBlockExpr}:
      oldDeferAnchor = c.deferAnchor
      c.deferAnchor = n
  case n.kind
  of nkError:
    # XXX: yet another place to report on nkError
    result = n
    # debug n
    # echo n.compilerInstInfo
    c.graph.config.localReport(n)
    return
  of nkSym:
    result = transformSym(c, n)
  of nkEmpty..pred(nkSym), succ(nkSym)..nkNilLit:
    # nothing to be done for leaves:
    result = n
  of nkBracketExpr: result = transformArrayAccess(c, n)
  of procDefs:
    var s = n[namePos].sym
    if n.typ != nil and s.typ.callConv == ccClosure:
      result = transformSym(c, n[namePos])
      # use the same node as before if still a symbol:
      if result.kind == nkSym: result = n
    else:
      result = n
  of nkMacroDef:
    # XXX no proper closure support yet:
    when false:
      if n[genericParamsPos].kind == nkEmpty:
        var s = n[namePos].sym
        n[bodyPos] = transform(c, s.getBody)
        if n.kind == nkMethodDef: methodDef(s, false)
    result = n
  of nkForStmt:
    result = transformFor(c, n)
  of nkCaseStmt:
    result = transformCase(c, n)
  of nkWhileStmt: result = transformWhile(c, n)
  of nkBlockStmt, nkBlockExpr:
    result = transformBlock(c, n)
  of nkDefer:
    c.deferDetected = true
    result = transformSons(c, n)
    when false:
      let deferPart = newTreeI(nkFinally, n.info): n[0]
      let tryStmt = newNodeI(nkTryStmt, n.info)
      if c.deferAnchor.isNil:
        tryStmt.add c.root
        c.root = tryStmt
        result = tryStmt
      else:
        # modify the corresponding *action*, don't rely on nkStmtList:
        tryStmt.add c.deferAnchor[^1]
        c.deferAnchor[^1] = tryStmt
        result = newNodeI(nkCommentStmt, n.info)
      tryStmt.add deferPart
      # disable the original 'defer' statement:
      n.kind = nkEmpty
  of nkContinueStmt:
    let labl = c.contSyms[^1]
    result = newTreeI(nkBreakStmt, n.info): newSymNode(labl)
  of nkBreakStmt: result = transformBreak(c, n)
  of nkCallKinds:
    result = transformCall(c, n)
  of nkAddr, nkHiddenAddr:
    result = transformAddrDeref(c, n, nkDerefExpr, nkHiddenDeref)
  of nkDerefExpr, nkHiddenDeref:
    result = transformAddrDeref(c, n, nkAddr, nkHiddenAddr)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    result = transformConv(c, n)
  of nkDiscardStmt:
    result = n
    if n[0].kind != nkEmpty:
      result = transformSons(c, n)
      if isConstExpr(result[0]):
        # ensure that e.g. discard "some comment" gets optimized away
        # completely:
        result = newNode(nkCommentStmt)
  of nkCommentStmt, nkTemplateDef, nkImportStmt, nkStaticStmt,
      nkExportStmt, nkExportExceptStmt:
    return n
  of nkConstSection:
    # do not replace ``const c = 3`` with ``const 3 = 3``
    return transformConstSection(c, n)
  of nkTypeSection, nkTypeOfExpr, nkMixinStmt, nkBindStmt:
    # no need to transform type sections:
    return n
  of nkLetSection, nkVarSection:
    if c.inlining > 0:
      # we need to copy the variables for multiple yield statements:
      result = transformVarSection(c, n)
    else:
      result = transformSons(c, n)
  of nkYieldStmt:
    if c.inlining > 0:
      result = transformYield(c, n)
    else:
      result = transformSons(c, n)
  of nkAsgn:
    result = transformAsgn(c, n)
  of nkIdentDefs, nkConstDef:
    result = shallowCopy(n)
    result[0] = transform(c, n[0])
    # Skip the second son since it only contains an unsemanticized copy of the
    # variable type used by docgen
    let last = n.len-1
    for i in 1..<last: result[i] = n[i]
    result[last] = transform(c, n[last])
    # XXX comment handling really sucks:
    if importantComments(c.graph.config):
      result.comment = n.comment
  of nkClosure:
    # it can happen that for-loop-inlining produced a fresh
    # set of variables, including some computed environment
    # (bug #2604). We need to patch this environment here too:
    let a = n[1]
    if a.kind == nkSym:
      result = copyTree(n)
      result[1] = transformSymAux(c, a)
    else:
      result = n
  of nkExceptBranch:
    result = transformExceptBranch(c, n)
  of nkCheckedFieldExpr:
    result = transformSons(c, n)
    if result[0].kind != nkDotExpr:
      # simplfied beyond a dot expression --> simplify further.
      result = result[0]
  else:
    result = transformSons(c, n)
  when false:
    if oldDeferAnchor != nil: c.deferAnchor = oldDeferAnchor

  # Constants can be inlined here, but only if they cannot result in a cast
  # in the back-end (e.g. var p: pointer = someProc)
  let exprIsPointerCast = n.kind in {nkCast, nkConv, nkHiddenStdConv} and
                          n.typ != nil and
                          n.typ.kind == tyPointer
  if not exprIsPointerCast:
    var cnst = getConstExpr(c.module, result, c.idgen, c.graph)
    # we inline constants if they are not complex constants:
    if cnst != nil and not dontInlineConstant(n, cnst):
      result = cnst # do not miss an optimization

proc processTransf(c: PTransf, n: PNode, owner: PSym): PNode =
  # Note: For interactive mode we cannot call 'passes.skipCodegen' and skip
  # this step! We have to rely that the semantic pass transforms too errornous
  # nodes into an empty node.
  if nfTransf in n.flags: return n
  pushTransCon(c, PTransCon(owner: owner, mapping: newIdNodeTable()))
  result = transform(c, n)
  popTransCon(c)
  incl(result.flags, nfTransf)

proc flattenStmts(n: PNode) =
  var goOn = true
  while goOn:
    goOn = false
    var i = 0
    while i < n.len:
      let it = n[i]
      if it.kind in {nkStmtList, nkStmtListExpr}:
        n.sons[i..i] = it.sons[0..<it.len]
        goOn = true
      inc i

proc liftDeferAux(n: PNode) =
  if n.kind in {nkStmtList, nkStmtListExpr}:
    flattenStmts(n)
    var goOn = true
    while goOn:
      goOn = false
      let last = n.len-1
      for i in 0..last:
        if n[i].kind == nkDefer:
          let deferPart = newTreeI(nkFinally, n[i].info): n[i][0]
          var body = newNodeIT(n.kind, n[i].info, n.typ)
          if i < last:
            body.sons = n.sons[(i+1)..last]
          let tryStmt = newTreeIT(nkTryStmt, n[i].info, n.typ):
            [body, deferPart]
          n[i] = tryStmt
          n.sons.setLen(i+1)
          n.typ = tryStmt.typ
          goOn = true
          break
  for i in 0..n.safeLen-1:
    liftDeferAux(n[i])

template liftDefer(c, root) =
  if c.deferDetected:
    liftDeferAux(root)

proc transformBody*(g: ModuleGraph; idgen: IdGenerator; prc: PSym; cache: bool): PNode =
  assert prc.kind in routineKinds

  if prc.transformedBody != nil:
    result = prc.transformedBody
  elif nfTransf in getBody(g, prc).flags or prc.kind in {skTemplate}:
    result = getBody(g, prc)
  elif prc.kind == skError:
      # xxx: wrap this in an nkError for the whole body
      result = prc.ast
      assert result != nil and result.kind == nkError,
        "assume we've populated the nkError here"
  else:
    var c = PTransf(graph: g, module: prc.getModule, idgen: idgen)
    prc.transformedBody = newNode(nkEmpty) # protects from recursion
    result = liftLambdas(g, prc, getBody(g, prc), c.tooEarly, c.idgen)
    result = processTransf(c, result, prc)
    liftDefer(c, result)

    if prc.isIterator:
      result = g.transformClosureIterator(c.idgen, prc, result)

    incl(result.flags, nfTransf)

    if cache or prc.typ.callConv == ccInline:
      # genProc for inline procs will be called multiple times from different modules,
      # it is important to transform exactly once to get sym ids and locations right
      prc.transformedBody = result
    else:
      prc.transformedBody = nil
    # XXX Rodfile support for transformedBody!

proc transformStmt*(g: ModuleGraph; idgen: IdGenerator; module: PSym, n: PNode): PNode =
  if nfTransf in n.flags:
    result = n
  else:
    var c = PTransf(graph: g, module: module, idgen: idgen)
    result = processTransf(c, n, module)
    liftDefer(c, result)
    #result = liftLambdasForTopLevel(module, result)
    incl(result.flags, nfTransf)

proc transformExpr*(g: ModuleGraph; idgen: IdGenerator; module: PSym, n: PNode): PNode =
  if nfTransf in n.flags:
    result = n
  else:
    var c = PTransf(graph: g, module: module, idgen: idgen)
    result = processTransf(c, n, module)
    liftDefer(c, result)
    # expressions are not to be injected with destructor calls as that
    # the list of top level statements needs to be collected before.
    incl(result.flags, nfTransf)
