## Implements `suspend` lowering and the pre-processing passes necessary for
## it. The code is... not good and barely works, and that is putting it
## mildly. Still, it at least allows for already trying out `suspend`
## a little.
##
## This module will be gone by the time the PR is ready for being reviewed.

import
  std/[
    tables,
    hashes
  ],
  compiler/ast/[
    ast_types,
    ast_query,
    ast_idgen,
    ast,
    idents,
    types,
    lineinfos
  ],
  compiler/modules/[
    magicsys
  ],
  compiler/sem/[
    lowerings,
    tailcall_analysis,
    liftdestructors
  ],
  compiler/modules/[
    modulegraphs
  ]

type
  TransfCtx {.pure, inheritable.} = object
    ## Everything a transformation pass may need.
    owner: PSym
    graph: ModuleGraph
    idgen: IdGenerator

proc newSym(c: TransfCtx, kind: TSymKind, name: string, info: TLineInfo): PSym =
  newSym(kind, c.graph.cache.getIdent(name), nextSymId c.idgen, c.owner, info)

proc newTemp(c: TransfCtx, typ: PType, info: TLineInfo): PSym =
  assert typ != nil
  result = c.newSym(skTemp, ":tmp", info)
  result.typ = typ

proc newLetStmt(s: PSym, val: PNode): PNode =
  newTreeI(nkLetSection, s.info, newIdentDefs(newSymNode(s), val))

proc captureLvalue(c: TransfCtx, n: PNode, mutable: bool): (PSym, PNode) =
  let s = c.newSym(skTemp, ":tmp", n.info)
  if n.typ.kind in {tyVar, tyLent}:
    s.typ = n.typ
    result = (s, n)
  else:
    s.typ = newType((if mutable: tyVar else: tyLent), nextTypeId(c.idgen), c.owner)
    s.typ.rawAddSon(n.typ)
    result = (s, newTreeIT(nkHiddenAddr, n.info, s.typ, n))

proc simplify(n: PNode): PNode =
  ## * removes forms not relevant anymore (import, export, etc.)
  ## * removes statement-list expression operands appearing as the first
  ##   operand for some forms
  ## * some other simplifications
  proc forward(n: sink PNode, i: int): PNode =
    if n[i].kind == nkStmtListExpr:
      # turn the AST inside out
      swap(n[i], result)
      n[i] = result[^1]
      result[^1] = n
    else:
      result = n

  case n.kind
  of nkTypeSection, nkCommentStmt, nkIncludeStmt, nkImportStmt,
     nkImportExceptStmt, nkExportStmt, nkExportExceptStmt, nkFromStmt,
     nkMixinStmt, nkBindStmt, routineDefs:
    result = newNode(nkEmpty)
  of nkTypeOfExpr:
    result = newNodeIT(nkType, n.info, n.typ)
  of nkStmtList, nkStmtListExpr:
    # remove nodes from the list that reduce to nothing
    var insert = 0
    for i in 0..<(n.len - ord(n.kind == nkStmtListExpr)):
      let got = simplify(n[i])
      if got.kind != nkEmpty:
        n[insert] = got
        inc insert

    if n.kind == nkStmtListExpr:
      n[insert] = simplify(n[^1])
      inc insert

    n.sons.setLen(insert)
    result = n
  of nkDotExpr, nkCheckedFieldExpr, nkObjDownConv, nkObjUpConv:
    n[0] = simplify(n[0])
    result = forward(n, 0)
  of nkCast, nkHiddenStdConv, nkHiddenSubConv:
    n[1] = simplify(n[1])
    result = forward(n, 1)
  of nkBracketExpr:
    n[0] = simplify(n[0])
    n[1] = simplify(n[1])
    result = forward(n, 0)
  of nkTupleConstr:
    for i in 0..<n.len:
      n[i] = simplify(skipColon(n[i]))
    result = n
  of nkLambdaKinds:
    # replace with the just the symbol
    result = newSymNode(n[namePos].sym, n.info)
  of nkWithoutSons - {nkCommentStmt}:
    result = n # nothing to do
  else:
    for i in 0..<n.len:
      n[i] = simplify(n[i])
    result = n

proc simplifyIfs(n: PNode): PNode =
  ## Removes multi-arm 'if' statements, turning them all into 'if-then' or
  ## 'if-then-else' statements.
  case n.kind
  of nkIfStmt, nkIfExpr:
    if n.len > 2 or n[^1].kind notin {nkElse, nkElseExpr}:
      result = newTreeI(n.kind, n.info, simplifyIfs(n[0]))
      var prev = result
      for i in 1..<n.len:
        case n.kind
        of nkElifBranch, nkElifExpr:
          let next = newTreeI(n.kind, n[i].info, simplifyIfs(n[i]))
          prev.add next
          prev = next
        of nkElse, nkElseExpr:
          prev.add simplifyIfs(n[i])
        else:
          unreachable()
    else:
      n[0] = simplifyIfs(n[0])
      n[1] = simplifyIfs(n[1])
      result = n
  of nkWithoutSons:
    result = n # nothing to do
  of nkWithSons - {nkIfStmt, nkIfExpr}:
    for i in 0..<n.len:
      n[i] = simplifyIfs(n[i])
    result = n

proc nameResults(c: TransfCtx, n: PNode): PNode =
  ## Gives a name to temporaries where necessary. Unfinished.
  proc expr(n: PNode, sink, named: bool): PNode {.closure.}

  proc capture(c: TransfCtx, n: PNode): PNode =
    let tmp = c.newTemp(n.typ, n.info)
    newTreeIT(nkStmtListExpr, n.info, n.typ, newLetStmt(tmp, n), newSymNode(tmp))

  proc wrap(n: PNode, sink, named: bool): PNode =
    case n.kind
    of nkDerefExpr, nkHiddenDeref:
      n[0] = wrap(n[0], sink, false)
      result = n
    of nkHiddenAddr, nkAddr:
      result = wrap(n[0], false, false)
    of nkConv, nkHiddenStdConv, nkHiddenSubConv, nkObjDownConv, nkObjUpConv, nkStmtListExpr:
      n[^1] = wrap(n[^1], sink, false)
      result = n
    of nkElifExpr, nkElseExpr, nkOfBranch:
      n[^1] = wrap(n[^1], sink, named)
      result = n
    of nkCast:
      n[1] = wrap(n[1], false, false)
      result = n
    of nkCallKinds:
      let fntype = n[0].typ.skipTypesOrNil(abstractInst)
      n[0] = wrap(n[0], false, false)
      for i in 1..<n.len:
        let sink = fntype != nil and i < fntype.len and fntype[i].kind == tySink
        n[i] = wrap(n[i], sink, false)

      if not isEmptyType(n.typ) and hasDestructor(n.typ) and not sink:
        # a temporary is required so that the result can be destroyed
        result = capture(c, n)
      else:
        result = n
    of nkChckRange, nkChckRange64, nkChckRangeF:
      for i in 0..<n.len:
        n[i] = wrap(n[i], false, false)
      result = n
    of nkTupleConstr, nkBracket:
      for i in 0..<n.len:
        n[i] = wrap(n[i], sink, false)
      result = n
    of nkObjConstr:
      # TODO: handle ref constructors (which are always owned values)
      for i in 1..<n.len:
        n[i][1] = wrap(n[i][1], sink and (sfCursor notin n[i][0].sym.flags), false)
      result = n
    of nkCaseStmt:
      for i in 1..<n.len:
        n[i][^1] = wrap(n[i][^1], true, true)

      if named:
        result = n
      else:
        result = capture(c, n)
    of nkBlockExpr:
      n[1] = wrap(n[1], true, true)
      if named:
        result = n
      else:
        result = wrap(n[1], true, true)
    of nkIfExpr, nkTryStmt:
      for i in 0..<n.len:
        n[i] = wrap(n[i], true, true)
      if named:
        result = n
      else:
        # unnamed expression -> wrap
        let tmp = c.newTemp(n.typ, n.info)
        result = newTreeIT(nkStmtListExpr, n.info, n.typ, newLetStmt(tmp, n), newSymNode(tmp))
    of nkYieldStmt:
      let tmp = c.newTemp(n.typ, n.info)
      result = newTreeIT(nkStmtListExpr, n.info, n.typ, newLetStmt(tmp, n), newSymNode(tmp))
    else:
      # due to void expressions in calls and complex expressions,
      # statements have to be admitted
      result = n

  proc expr(n: PNode, sink, named: bool): PNode =
    # first process the sub-expressions, then wrap
    wrap(nameResults(c, n), sink, named)

  # TODO: implement this properly
  case n.kind
  of nkWithoutSons:
    result = n # nothing to do
  of nkDiscardStmt:
    n[0] = expr(n[0], false, false)
    result = n
  of nkAsmStmt:
    for i in 0..<n.len:
      n[i] = expr(n[i], false, false)
    result = n
  of nkElifBranch, nkElifExpr:
    n[0] = expr(n[0], false, false)
    n[1] = nameResults(c, n[1])
    result = n
  of nkCaseStmt:
    n[0] = expr(n[0], false, false)
    for i in 1..<n.len:
      n[i] = nameResults(c, n[i])
    result = n
  of nkCallKinds:
    for i in 0..<n.len:
      n[i] = nameResults(c, n[i])
    # call statements and expressions are processed the same
    result = wrap(n, false, false)
  of nkReturnStmt:
    if n[0].kind in nkCallKinds:
      n[0] = expr(n[0], true, false)
    else:
      n[0] = nameResults(c, n[0])
    result = n
  of nkRaiseStmt:
    n[0] = expr(n[0], true, false)
    result = n
  of nkAsgn, nkFastAsgn:
    n[0] = expr(n[0], false, false)
    # TODO: the below is wrong when the destination is a cursor location
    n[1] = expr(n[1], true, false)
    result = n
  of nkLetSection, nkVarSection:
    # complex expression appearing on the right have a name
    # already; nothing to do
    n[0][^1] = expr(n[0][^1], false, named=true)
    result = n
  of nkStmtListExpr:
    for i in 0..<n.len-1:
      n[i] = nameResults(c, n[i])
    # the expression part is handled by `wrap`
    result = n
  else:
    for i in 0..<n.len:
      n[i] = nameResults(c, n[i])
    result = n

proc lowerAndOr(op, a, b: PNode): PNode =
  ## Lowers 'and' or 'or' expression `n` into an 'if' expression.
  if op[0].sym.magic == mAnd:
    # ``a and b`` -> ``if a: b else: false``
    newTreeIT(nkIfExpr, op.info, op.typ,
      newTreeI(nkElifExpr, a.info, a, b),
      newTreeI(nkElseExpr, op.info, newIntTypeNode(0, op.typ)))
  else:
    # ``a or b`` -> ``if a: true else: b``
    newTreeIT(nkIfExpr, op.info, op.typ,
      newTreeI(nkElifExpr, a.info, a, newIntTypeNode(1, op.typ)),
      newTreeI(nkElseExpr, b.info, b))

proc removeAndOr(n: PNode): PNode =
  ## Lowers all 'and' and 'or' expressions.
  case n.kind
  of nkWithoutSons:
    result = n # nothing to do
  of nkCallKinds:
    if n[0].kind == nkSym and n[0].sym.magic in {mAnd, mOr}:
      result = lowerAndOr(n, removeAndOr(n[1]), removeAndOr(n[2]))
    else:
      for i in 0..<n.len:
        n[i] = removeAndOr(n[i])
      result = n
  of nkWithSons - nkCallKinds:
    for i in 0..<n.len:
      n[i] = removeAndOr(n[i])
    result = n

proc isPure(n: PNode): bool =
  ## Computes whether expression `n` can be freely reordered with effectful
  ## statement without affecting the result.
  case n.kind
  of nkLiterals:
    true
  of nkSym:
    # note: a temp is (hopefully) something that's been produced by a
    # capture, meaning it's an immutable local
    # **important:** sink parameters are not pure, but they're deliberately
    # considered as such, here! This trades correctness for suspend being
    # usable in more situations
    n.sym.kind in (skProcKinds + {skConst, skLet, skForVar, skTemp, skParam})
  of nkDotExpr, nkCheckedFieldExpr:
    isPure(n[0])
  of nkBracketExpr:
    isPure(n[0]) and isPure(n[1])
  else:
    false

proc isStable(n: PNode): bool =
  ## Computes whether expression `n` can be freely reordered with effectful
  ## statements without affecting the resolved-to *location*.
  # TODO: the doc comment is not really correct...
  case n.kind
  of nkSym:
    true # always points to the same location
  of nkDotExpr:
    isStable(n[0])
  of nkCheckedFieldExpr:
    # could raise dependent on the discriminator; don't allow reordering
    # impure expressions
    isPure(n[0])
  of nkBracketExpr:
    if n[0].typ.skipTypes(abstractInst).kind in {tyTuple, tyArray, tyOpenArray, tyVarargs}:
      isStable(n[0]) and isPure(n[1])
    else:
      isPure(n[0]) and isPure(n[1])
  else:
    false

proc unnestStatements(c: TransfCtx, n: PNode): PNode =
  ## Removes statements from some expression contexts. Unfinished.
  proc needsTransform(n: PNode): bool =
    case n.kind
    of nkStmtListExpr: true
    of nkWithoutSons: false
    of nkWithSons - {nkStmtListExpr}:
      for it in n.items:
        if needsTransform(it):
          return true
      false

  proc wrap(n: sink PNode, stmts: sink seq[PNode]; kind = nkStmtList): PNode =
    if stmts.len == 0:
      result = n
    else:
      result = newNode(kind)
      result.typ = n.typ
      stmts.add n
      result.sons = stmts

  proc process(n: PNode): PNode

  const ExprKinds = {nkCast, nkConv, nkHiddenStdConv, nkHiddenSubConv,
                     nkDotExpr, nkCheckedFieldExpr, nkObjDownConv,
                     nkObjUpConv, nkBracketExpr, nkChckRange, nkChckRange64,
                     nkChckRangeF, nkTupleConstr, nkObjConstr,
                     nkBracket} + nkCallKinds

  proc expr(n: PNode, stmts: var seq[PNode]): PNode =
    case n.kind
    of nkDotExpr, nkCheckedFieldExpr, nkObjDownConv, nkObjUpConv:
      n[0] = expr(n[0], stmts)
      result = n
    of nkCast, nkConv, nkHiddenStdConv, nkHiddenSubConv:
      n[1] = expr(n[1], stmts)
      result = n
    of nkBracketExpr:
      if needsTransform(n[1]):
        let got = expr(n[0], stmts)
        if not isStable(got):
          # TODO: depends on the context (lvalue vs. value)
          discard

        n[0] = got
        n[1] = expr(n[1], stmts)
      else:
        n[0] = expr(n[0], stmts)
      result = n
    of nkCallKinds, nkChckRange, nkChckRange64, nkChckRangeF, nkBracket,
       nkTupleConstr, nkObjConstr:
      # look for the first argument that needs processing
      var last = n.len - 1
      while last >= 0 and not needsTransform(n[last]):
        dec last

      # only capture the arguments (callee included) coming before the last
      # argument that'll be processed
      block:
        for i in 0..<last:
          # watch out to not process compile-time arguments
          # TODO: handle lent parameters
          if n[i].typ.isNil:
            # must be a statement argument
            stmts.add process(n[i])
          elif n[i].typ.kind notin {tyStatic, tyTypeDesc}:
            let got = expr(n[i], stmts)
            if isPure(got):
              n[i] = got
            elif n[0].typ != nil and i < n[0].typ.len and n[0].typ[i].kind == tyVar:
              # TODO: skip fn types
              let (s, val) = captureLvalue(c, got, mutable=true)
              stmts.add newLetStmt(s, val)
              n[i] = nkHiddenDeref.newTreeIT(got.info, got.typ, newSymNode(s))
            else:
              # TODO: use a cursor capture when the argument is not passed to a
              #       sink parameter
              let s = c.newTemp(got.typ, got.info)
              # the operand still needs to be processed
              stmts.add newLetStmt(s, got)
              n[i] = newSymNode(s)

      if last >= 0:
        n[last] = expr(n[last], stmts)
      result = n
    of nkStmtListExpr:
      for i in 0..<n.len-1:
        stmts.add n[i]
      result = expr(n[^1], stmts)
    of nkWithoutSons:
      result = n
    of nkWithSons - ExprKinds - {nkStmtListExpr}:
      result = process(n)

  proc wrapped(n: PNode): PNode =
    var stmts: seq[PNode]
    wrap(expr(n, stmts), stmts)

  proc process(n: PNode): PNode =
    case n.kind
    of nkStmtListExpr:
      # okay; happens for predicates and bodies of complex expressions
      for i in 0..<n.len-1:
        n[i] = process(n[i])
      n[^1] = wrapped(n[^1])
      result = n
    of nkCaseStmt:
      var stmts: seq[PNode]
      n[0] = expr(n[0], stmts)
      for i in 1..<n.len:
        n[i] = process(n[i])
      result = wrap(n, stmts)
    of nkDiscardStmt:
      var stmts: seq[PNode]
      n[0] = expr(n[0], stmts)
      result = wrap(n, stmts)
    of nkReturnStmt:
      case n[0].kind
      of nkAsgn:
        var stmts: seq[PNode]
        n[0][1] = expr(n[0][1], stmts)
        result = wrap(n, stmts)
      of nkCallKinds:
        # it's a tailcall
        var stmts: seq[PNode]
        n[0] = expr(n[0], stmts)
        result = wrap(n, stmts)
      else:
        result = n # empty return; nothing to do
    of nkAsgn:
      var stmts: seq[PNode]
      if needsTransform(n[1]):
        if isPure(n[0]):
          # no need to capture the destination
          n[1] = expr(n[1], stmts)
        else:
          let (s, e) = captureLvalue(c, n, mutable=true)
          stmts.add newLetStmt(s, e)
          n[0] = newSymNode(s)
          n[1] = expr(n[1], stmts)
      else:
        n[0] = expr(n[0], stmts)
      result = wrap(n, stmts)
    of nkVarSection, nkLetSection:
      var stmts: seq[PNode]
      n[0][^1] = expr(n[0][^1], stmts)
      result = wrap(n, stmts)
    of ExprKinds:
      var stmts: seq[PNode]
      let got = expr(n, stmts)
      result = wrap(got, stmts,
        (if n.typ.isEmptyType(): nkStmtList else: nkStmtListExpr))
    of nkWithoutSons:
      result = n # nothing to do
    else:
      for i in 0..<n.len:
        n[i] = process(n[i])
      result = n

  result = process(n)

proc simplifyPred(c: TransfCtx, n: PNode): PNode =
  ## Removes all statements from 'if' predicate expressions. Unfinished.
  case n.kind
  of nkIfStmt:
    if n[0][0].kind == nkStmtListExpr:
      # wrap in a block in order to preserve the scope
      let lab = c.newSym(skLabel, ":lab", n.info)
      lab.flags.incl sfUsed
      let stmts = newNodeI(nkStmtList, n.info)
      var pred = n[0][0]
      while pred.kind == nkStmtListExpr:
        for i in 0..<pred.len-1:
          stmts.add simplifyPred(c, pred[i])
        pred = pred[^1]

      proc newIfThen(a, b: PNode): PNode =
        newTree(nkIfStmt, newTree(nkElifBranch, a, b))

      if n.len == 2:
        if n[0][1].typ == c.graph.noreturnType:
          # no extra break + block is needed
          stmts.add newIfThen(pred, n[0][1])
          result = nkStmtList.newTree(
            nkBlockStmt.newTree(newSymNode(lab), stmts),
            n[1][0])
        else:
          # ``if a: b else: c`` ->
          #   ``block L1: (block L2: (if a: (b; break L1)); c)``
          let outer = c.newSym(skLabel, ":lab", n.info)
          outer.flags.incl sfUsed
          stmts.add newIfThen(pred,
            nkStmtList.newTree(
              n[0][1],
              newTreeI(nkBreakStmt, n[0][1].info, newSymNode(outer))))
          result = nkBlockStmt.newTreeI(n.info, newSymNode(outer),
            nkStmtList.newTree(
              nkBlockStmt.newTreeI(n.info, newSymNode(lab), stmts),
              n[1][0]))
      else:
        stmts.add newIfThen(pred, n[0][1])
        result = nkBlockStmt.newTreeI(n.info, newSymNode(lab), stmts)
    else:
      for i in 0..<n.len:
        n[i] = simplifyPred(c, n[i])
      result = n
  of nkWithoutSons:
    result = n # nothing to do
  of nkWithSons - {nkIfStmt}:
    for i in 0..<n.len:
      n[i] = simplifyPred(c, n[i])
    result = n

type SplitContext {.final.} = object of TransfCtx
  param: PSym
    ## the context parameter of the reified continuation
  ctx: PType
    ## type of the context parame
  map: Table[PSym, PSym]
    ## locals -> context object field
  conts: seq[PSym]
    ## the reified continuation procedures

proc copyTreeWith(n: PNode, at: int, x: sink PNode): PNode =
  result = shallowCopy(n)
  for i in 0..<n.len:
    if i == at:
      result[i] = x
    else:
      result[i] = copyTree(n[i])

type FilterContext {.final.} = object of TransfCtx
  param: PSym
  result: PSym
  origResult: PSym
  env: PType
  map: Table[int, PSym]
  blocks: seq[tuple[label: int, used: bool]]
    ## tracks which blocks are broken out of

proc restore(c: var FilterContext, s: PSym): PSym =
  result = copySym(s, nextSymId c.idgen)
  result.owner = c.owner
  c.map[s.id] = result

proc handleDefs(c: var FilterContext, n: PNode, init: sink PNode): PNode =
  if c.env == nil:
    result = copyTreeWith(n, n.len-1, init)
  else:
    result = shallowCopy(n)
    for i in 0..<n.len-2:
      result[i] = newSymNode(restore(c, n[i].sym))
    result[^2] = n[^2]
    result[^1] = init

proc fixupLocals(c: var FilterContext, n: PNode): PNode =
  # XXX: the name doesn't match the behaviour
  case n.kind
  of nkSym:
    let s = n.sym
    if c.env != nil and s.kind in {skForVar, skVar, skLet, skTemp, skParam} and
       sfGlobal notin s.flags:
      if n.sym.owner != c.owner:
        result = newSymNode(c.map[n.sym.id], n.info)
      else:
        result = n
    elif c.result != nil and s.kind == skResult:
      # replace with the new result variable
      result = newSymNode(c.result)
    else:
      result = n
  of nkIdentDefs, nkVarTuple:
    if c.env == nil:
      result = copyTree(n)
    else:
      result = handleDefs(c, n, fixupLocals(c, n[^1]))
  of nkWithoutSons - {nkSym}:
    result = copyNode(n)
  of nkWithSons - {nkIdentDefs, nkVarTuple}:
    result = shallowCopy(n)
    for i in 0..<n.len:
      result[i] = fixupLocals(c, n[i])

proc containsSuspend*(n: PNode): bool =
  ## Returns whether `n` contains a suspension point.
  case n.kind
  of nkWithoutSons: false
  of nkYieldStmt: true
  of routineDefs: false
  of nkWithSons - {nkYieldStmt} - routineDefs:
    for it in n.items:
      if containsSuspend(it):
        return true
    false

proc addLive(live: var seq[PSym], s: PSym) =
  # views cannot be used across suspensions and thus don't have to be
  # considered for saving
  if classifyBackendView(s.typ) == bvcNone:
     live.add s

proc prepare(c: var SplitContext, n: PNode, live: var seq[PSym]): PNode =
  ## * lowers the early suspend form ``(Suspend loc body)`` into the
  ##   statement form ``(Suspend cont body)``
  ## * creates and remembers the reified continuation proc for each ``suspend``
  ## * populates the continuation context object type for each continuation
  ## * some other things
  proc lower(c: var SplitContext, n: PNode, live: seq[PSym]): PNode =
    # save all statically live locals in the continuation context
    # TODO: only save locals that are used in the continuation, but keep in
    #       mind that locals with a destructor always need to be captured
    let env = n[0].typ[0]
    var cons = nkObjConstr.newTreeIT(n.info, env, newNodeIT(nkType, n.info, env))
    # add the fields in reverse, so that destruction also happens in reverse
    for i in countdown(live.high, 0):
      let it = live[i]
      cons.add nkExprColonExpr.newTree(
        newSymNode(addUniqueField(env, it, c.graph.cache, c.idgen)),
        newSymNode(it))

    # the continuation inherits everything, except its type and body, from the
    # parent routine
    let cont = newSym(skProc, c.owner.name, nextSymId(c.idgen), c.owner, c.owner.info, c.owner.options)
    cont.typ = n[0].typ[1]
    cont.flags.incl sfInjectDestructors
    for i in 1..<cont.typ.len:
      cont.typ.n[i].sym.owner = cont
    # there's no proper procedure body yet; it'll be set up later

    c.conts.add cont
    result = nkStmtList.newTree(
      newLetStmt(n[0].sym, nkTupleConstr.newTreeIT(n[0].info, n[0].typ, cons, newSymNode(cont))),
      nkYieldStmt.newTreeI(n.info, newSymNode(cont), n[1]))

    # explictly disarm the context parameter's destructor, so that it doesn't
    # erroneously block tail calls
    result.add nkCall.newTreeI(n.info, newSymNode(createMagic(c.graph, c.idgen, "wasMoved", mWasMoved)),
      newSymNode(cont.typ.n[^1].sym, n.info))

  case n.kind
  of nkYieldStmt:
    # a 'suspend' in a statement context
    result = lower(c, n, live)
  of nkLetSection, nkVarSection:
    if n[0][^1].kind == nkYieldStmt:
      # a 'suspend' expression
      result = lower(c, n[0][^1], live)
      # replace with the yield in the original position with the parameter
      # of continuation
      n[0][^1] = newSymNode(result[1][0].sym.typ.n[1].sym, n[0][^1].info)
      result.add n
    else:
      n[0][^1] = prepare(c, n[0][^1], live)
      result = n

    let def = n[0]
    if def.kind == nkVarTuple:
      for i in 0..<def.len-2:
        addLive(live, def[i].sym)
    else:
      addLive(live, def[0].sym)
  of nkBlockStmt, nkBlockExpr, nkElifBranch, nkElifExpr, nkElse, nkElseExpr,
     nkOfBranch, nkWhileStmt:
    # the body is within a new scope
    let start = live.len
    n[^1] = prepare(c, n[^1], live)
    if start < live.len:
      # discard the scope again
      live.shrink(start)
    result = n
  of nkWithoutSons:
    result = n # nothing to do
  else:
    for i in 0..<n.len:
      n[i] = prepare(c, n[i], live)
    result = n

proc restoreParameter(c: var FilterContext, s: PSym, to: PNode) =
  let f = getFieldFromObj(c.env, s)
  if f != nil:
    let ns = restore(c, s)
    if ns.kind == skParam:
      if ns.typ.kind == tySink:
        ns.typ = ns.typ.lastSon
        ns.kind = skVar
      else:
        ns.kind = skLet

    to.add newLetStmt(ns, nkDotExpr.newTreeIT(s.info, f.typ, newSymNode(c.param), newSymNode(f)))

proc filter(c: var FilterContext; n: PNode, live: bool): PNode =
  ## Given a pre-processed body, removes everything that doesn't belong to the
  ## reified continuation procedure `owner`. Unfinished.
  proc wrapScope(c: FilterContext, n: PNode, info: TLineInfo): PNode =
    let lab = newSymNode(c.newSym(skLabel, ":lab", info))
    if n.typ.isEmptyType():
      # inherit the type so that the noreturn type stays
      nkBlockStmt.newTreeIT(info, n.typ, lab, n)
    else:
      nkBlockExpr.newTreeIT(info, n.typ, lab, n)

  proc handleStmt(c: var FilterContext, n: PNode, live: var bool, stmts: var seq[PNode]) =
    if n.kind in {nkLetSection, nkVarSection} and not live and
       c.env != nil and getFieldFromObj(c.env, n[0][0].sym) != nil:
      # to preserve lifetimes and destruction order, the definitions for saved
      # locals need to stay in the same position
      if n[0].kind == nkVarTuple:
        # split into multiple statements
        for i in 0..<n[0].len-1:
          let f = getFieldFromObj(c.env, n[0][i].sym)
          let inp = nkDotExpr.newTreeIT(n[0][^1].info, f.typ, newSymNode(c.param, n.info), newSymNode(f))
          stmts.add newLetStmt(restore(c, n[0][i].sym), inp)
      else:
        let f = getFieldFromObj(c.env, n[0][0].sym)
        let inp = nkDotExpr.newTreeIT(n[0][^1].info, f.typ, newSymNode(c.param, n.info), newSymNode(f))
        stmts.add newTreeI(n.kind, n.info, newIdentDefs(newSymNode(restore(c, n[0][0].sym), n[0][0].info), inp))
      # `live` doesn't change
    elif n.kind in {nkStmtList, nkStmtListExpr}:
      for it in n.items:
        handleStmt(c, it, live, stmts)
    else:
      let got = filter(c, n, live)
      if got.kind != nkEmpty:
        stmts.add got
        live = got.typ != c.graph.noreturnType

  case n.kind
  of nkStmtList:
    var items: seq[PNode]
    var live = live
    handleStmt(c, n, live, items)

    case items.len
    of 0:
      result = c.graph.emptyNode
    of 1:
      result = items[0]
    else:
      result = newNodeI(nkStmtList, n.info)
      result.sons = items
      if not live:
        result.typ = c.graph.noreturnType

  of nkStmtListExpr:
    var items: seq[PNode]
    var live = live
    handleStmt(c, n, live, items)

    case items.len
    of 0:
      result = c.graph.emptyNode
    of 1:
      result = items[0]
    elif live:
      result = newNodeIT(nkStmtListExpr, n.info, items[^1].typ)
      result.sons = items
    else:
      result = newNodeIT(nkStmtList, n.info, c.graph.noreturnType)
      result.sons = items
  of nkElifBranch, nkElifExpr:
    assert live
    result = shallowCopy(n)
    result[0] = fixupLocals(c, n[0])
    result[1] = filter(c, n[1], live)
  of nkElse, nkElseExpr:
    assert live
    result = shallowCopy(n)
    result[0] = filter(c, n[0], live)
  of nkWhileStmt:
    if live:
      result = shallowCopy(n)
      result[0] = n[0] # constant expression
      result[1] = filter(c, n[1], live)
    else:
      let got = filter(c, n[1], false)
      if got.kind != nkEmpty:
        let start = filter(c, n[1], true)
        # to keep the lifetimes correct, wrap the entry in a block
        result = nkStmtList.newTree(
          nkBlockStmt.newTree(newSymNode(c.newSym(skLabel, ":lab", n.info)), got),
          # then emit a 'while' loop with the first half
          nkWhileStmt.newTreeI(n.info, n[0], start))
        # for the following loop:
        #   while true:
        #     a
        #     if b:
        #       c
        #       ... # <- start of the continuation
        #       d
        #     e
        # the transform will produce:
        #   block:
        #     block:
        #       d
        #     e
        #   while true:
        #     a
        #     if b:
        #       c
        #       ... # <- run continuation
        #     e
      else:
        result = got # the loop is dead code
  of nkBlockStmt, nkBlockExpr:
    c.blocks.add (n[0].sym.id, false)
    let got = filter(c, n[1], live)
    let (_, used) = c.blocks.pop()
    if got.kind == nkEmpty:
      result = got
    else:
      # keep the block, in order to preserve lifetimes
      result = shallowCopy(n)
      if got.typ == c.graph.noreturnType:
        result.transitionSonsKind(nkBlockStmt)
      if not used:
        # if the body doesn't exit normally, neither does the block
        result.typ = got.typ
      else:
        result.typ = nil
      result[0] = fixupLocals(c, n[0])
      result[1] = got
  of nkIfStmt, nkIfExpr:
    if live:
      result = shallowCopy(n)
      result[0] = filter(c, n[0], live)
      if n.len == 2:
        result[1] = filter(c, n[1], live)
    else:
      result = filter(c, n[0][1], false)
      if result.kind == nkEmpty and n.len == 2:
        # try the else branch
        result = filter(c, n[1][0], false)
      # only a single entry point is possible, meaning that the 'else'
      # branch is known to be dead code if the first branch isn't
      if result.kind != nkEmpty:
        result = wrapScope(c, result, n.info)
  of nkOfBranch:
    if live:
      result = copyTreeWith(n, n.len - 1, filter(c, n[^1], live))
    else:
      result = filter(c, n[^1], live)
      if result.kind != nkEmpty:
        result = wrapScope(c, result, n.info)
  of nkCaseStmt:
    if live:
      result = shallowCopy(n)
      result[0] = fixupLocals(c, n[0])
      for i in 1..<n.len:
        result[i] = filter(c, n[i], true)
    else:
      result = c.graph.emptyNode
      # replace with the branch that's the entry point
      for i in 1..<n.len:
        result = filter(c, n[i], false)
        if result.kind != nkEmpty:
          break
  of nkTryStmt:
    let got = filter(c, n[0], live)
    if live:
      result = shallowCopy(n)
      result[0] = got
      for i in 1..<n.len:
        result[i] = filter(c, n[i], true)
    elif got.kind != nkEmpty:
      result = shallowCopy(n)
      result[0] = got
      for i in 1..<n.len:
        result[i] = filter(c, n[i], true)
    else:
      # no need to check the except/finally clauses; they cannot contain
      # a suspend
      result = c.graph.emptyNode
  of nkYieldStmt:
    if live:
      # a suspension point; the code beyond is not part of the current
      # continuation
      result = nkYieldStmt.newTreeI(n.info, fixupLocals(c, n[1]))
      result.typ = c.graph.noreturnType
    elif n[0].sym.id == c.owner.id:
      # it's the entry point of the filtered-for continuation. Return
      # something that signals "live"
      result = nkDiscardStmt.newTreeI(n.info, c.graph.emptyNode)
    else:
      result = c.graph.emptyNode
  of nkVarSection, nkLetSection:
    let got = filter(c, n[0][^1], live)
    if live:
      # always introduce defs, so that loops work
      if got.typ == c.graph.noreturnType:
        result = got
      else:
        result = newTreeI(n.kind, n.info, handleDefs(c, n[0], got))
    else:
      if got.kind == nkEmpty:
        result = c.graph.emptyNode
      else:
        result = newTreeI(n.kind, n.info, handleDefs(c, n[0], got))
  else:
    if live:
      result = fixupLocals(c, n)
    else:
      result = c.graph.emptyNode

proc lowerSuspend*(g: ModuleGraph, idgen: IdGenerator; s: PSym,
                   body: PNode): PNode =
  # run the batch of pre-processing so that the actual lowering can happen on
  # a much more regular AST, where the relevant temporaries are explicit and
  # have names
  var tc = TransfCtx(owner: s, graph: g, idgen: idgen)
  var body = simplify(body)
  body = removeAndOr(body)
  # note: lowering and/or early means that lifetimes of temporaries in and/or
  # expression doesn't match what that of temporaries in unprocessed bodies.
  # However, getting this right is non-trivial and and/or are (at the time of
  # writing) underspecified to the point that this can be chalked up to
  # undefined behaviour
  body = simplifyIfs(body)
  body = nameResults(tc, body)
  body = unnestStatements(tc, body)
  body = simplifyPred(tc, body)

  var c = SplitContext(owner: s, graph: g, idgen: idgen)
  block:
    var live: seq[PSym]
    for i in 1..<s.typ.len:
      addLive(live, s.typ.n[i].sym)

    # also consider the closure environment parameter:
    if s.typ.callConv == ccClosure:
      live.add s.ast[paramsPos][^1].sym

    if not s.typ[0].isEmptyType():
      live.add s.ast[resultPos].sym

    body = prepare(c, body, live)

  # create the body and complete the symbol for each continuation:
  for it in c.conts.items:
    var f = FilterContext(graph: g, owner: it, idgen: idgen,
      env: it.typ[^1].skipTypes({tySink}), param: it.typ.n[^1].sym)
    if not s.typ[0].isEmptyType():
      f.result = copySym(s.ast[resultPos].sym, nextSymId(idgen))
      f.result.owner = it
      f.origResult = s.ast[resultPos].sym

    # now that the type is complete, allow size computation
    f.env.size = szUncomputedSize
    f.env.align = szUncomputedSize

    var got = newNode(nkStmtList)
    # restore all saved parameters:
    for i in 1..<s.typ.len:
      restoreParameter(f, s.typ.n[i].sym, got)
    # also consider the closure environment parameter:
    if s.typ.callConv == ccClosure:
      restoreParameter(f, s.ast[paramsPos][^1].sym, got)
    # restore the `result` variable's value, if present
    if f.result != nil:
      got.add nkAsgn.newTree(
        newSymNode(f.result),
        nkDotExpr.newTreeIT(unknownLineInfo, it.typ,
          newSymNode(f.param),
          newSymNode(getFieldFromObj(f.env, s.ast[resultPos].sym))))

    got.add filter(f, body, false)
    if got.len == 1:
      got = got[0]

    assert got.kind != nkEmpty
    got.flags.incl nfTransf

    it.ast = newProcNode(nkProcDef, s.info,
      name=newSymNode(it),
      params=nkFormalParams.newTree(copyTree(s.ast[paramsPos][0])),
      genericParams=c.graph.emptyNode,
      pragmas=c.graph.emptyNode,
      pattern=c.graph.emptyNode,
      exceptions=c.graph.emptyNode, # TODO: inherit
      body=got)
    for i in 1..<it.typ.len:
      it.ast[paramsPos].add newIdentDefs(newSymNode(it.typ.n[i].sym), newNodeIT(nkType, unknownLineInfo, it.typ[i]))

    if f.result != nil:
      # append the result symbol
      it.ast.sons.setLen(resultPos + 1)
      it.ast[resultPos] = newSymNode(f.result)

    # create the 'apply' procedure and also lift the type-bound ops
    # for the Continuation type, as those are not guaranteed to be
    # present already
    genApply(g, idgen, it)
    createTypeBoundOps(g, nil, it.ast[miscPos][1].typ, it.info, idgen)

    # add the hidden environment parameter:
    block:
      # HACK: this duplicates what `transf` does
      let env = newSym(skParam, getIdent(g.cache, ":env"), nextSymId(idgen),
                       it, it.info)
      env.position = it.typ.len - 1
      env.flags.incl sfFromGeneric
      env.typ = g.getSysType(it.info, tyPointer)
      it.ast[paramsPos].add newSymNode(env)

    # the body is fully transformed already, prevent transf from touching
    # it again
    g.setTransformed(it, got)

    resolveForwardOps(g, idgen, f.env, it.info)

  # finally, create the filtered body for the original routine:
  var f = FilterContext(graph: g, owner: s, idgen: idgen)
  result = filter(f, body, true)
