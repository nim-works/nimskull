#
#
#           The Nim Compiler
#        (c) Copyright 2018 Nim Contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

##[

This file implements closure iterator transformations.
The main idea is to split the closure iterator body to top level statements.
The body is split by yield statement.

Example:

.. code-block:: nim

    while a > 0:
      echo "hi"
      yield a
      dec a

Should be transformed to:

.. code-block:: nim

    STATE0:
      if not (a > 0):
        :state = 2 # Next state
        break :stateLoop # Proceed to the next state
      echo "hi"
      :state = 1 # Next state
      return a # yield
    STATE1:
      dec a
      :state = 0 # Next state
      break :stateLoop # Proceed to the next state
    STATE2:
      :state = -1 # End of execution

The lambdalifting transformation has to have happend already, as it is
responsible for setting up the environment type, to which we might need to
append new fields.

One special subtransformation is nkStmtListExpr lowering.
Example:

.. code-block:: nim

    template foo(): int =
      yield 1
      2

    iterator it(): int {.closure.} =
      if foo() == 2:
        yield 3

If a nkStmtListExpr has yield inside, it has first to be lowered to:

.. code-block:: nim

  yield 1
  :tmpSlLower = 2
  if :tmpSlLower == 2:
    yield 3

nkTryStmt Transformations:

If the iter has an nkTryStmt with a yield inside
- the closure iter is promoted to have exceptions (ctx.hasExceptions = true)
- exception table is created. This is a const array, where
  `abs(exceptionTable[i])` is a state idx to which we should jump from state
  `i` should exception be raised in state `i`. For all states in `try` block
  the target state is `except` block. For all states in `except` block
  the target state is `finally` block. For all other states there is no
  target state (0, as the first block can never be neither except nor finally).
  `exceptionTable[i]` is < 0 if `abs(exceptionTable[i])` is except block,
  and > 0, for finally block.
- local variable :curExc is created
- the iter body is wrapped into a

    .. code-block::

        try:
          closureIterSetupExc(:curExc)
          ...body...
        catch:
          :state = exceptionTable[:state]
          if :state == 0: raise # No state that could handle exception
          :unrollFinally = :state > 0 # Target state is finally
          if :state < 0:
              :state = -:state
          :curExc = getCurrentException()

nkReturnStmt within a try/except/finally now has to behave differently as we
want the nearest finally block to be executed before the return, thus it is
transformed to:
:tmpResult = returnValue (if return doesn't have a value, this is skipped)
:unrollFinally = true
goto nearestFinally (or -1 if not exists)

Example:

.. code-block:: nim

    try:
    yield 0
    raise ...
    except:
    yield 1
    return 3
    finally:
    yield 2

Is transformed to (yields are left in place for example simplicity,
in reality the code is subdivided even more, as described above):

.. code-block::

  STATE0: # Try
    yield 0
    raise ...
    :state = 2 # What would happen should we not raise
    break :stateLoop
  STATE1: # Except
    yield 1
    :tmpResult = 3           # Return
    :unrollFinally = true # Return
    :state = 2 # Goto Finally
    break :stateLoop
    :state = 2 # What would happen should we not return
    break :stateLoop
  STATE2: # Finally
    yield 2
    if :unrollFinally: # This node is created by `newEndFinallyNode`
      if :curExc.isNil:
        return :tmpResult
      else:
        closureIterSetupExc(nil)
        raise
    state = -1 # Goto next state. In this case we just exit
    break :stateLoop

]##



import
  std/[
    tables
  ],
  compiler/ast/[
    ast,
    idents,
    renderer,
    lineinfos,
    trees
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
    lowerings,
    lambdalifting
  ],
  compiler/utils/[
    idioms,
  ]

type
  Ctx = object
    g: ModuleGraph
    fn: PSym
    tmpResultSym: PSym # Used when we return, but finally has to interfere
    unrollFinallySym: PSym # Indicates that we're unrolling finally states (either exception happened or premature return)
    curExcSym: PSym # Current exception

    states: seq[tuple[label: int, body: PNode]] # The resulting states
    stateLoopLabel: PSym # Label to break on, when jumping between states.
    exitStateIdx: int # index of the last state
    tempVarId: int # unique name counter
    tempVars: PNode # Temp var decls, nkVarSection
    exceptionTable: seq[int] # For state `i` jump to state `exceptionTable[i]` if exception is raised
    hasExceptions: bool # Does closure have yield in try?
    curExcHandlingState: int # Negative for except, positive for finally
    nearestFinally: int # Index of the nearest finally block. For try/except it
                    # is their finally. For finally it is parent finally. Otherwise -1
    idgen: IdGenerator

const
  nkSkip = nkWithoutSons + {nkTemplateDef, nkTypeSection, nkStaticStmt,
            nkMixinStmt, nkBindStmt} + procDefs

proc boolLit(ctx: Ctx, info: TLineInfo, val: bool): PNode =
  result = newIntTypeNode(ord val, ctx.g.getSysType(info, tyBool))
  result.info = info

proc newStateAccess(ctx: var Ctx): PNode =
  result = rawIndirectAccess(newSymNode(getEnvParam(ctx.fn)),
      getStateField(ctx.g, ctx.fn), ctx.fn.info)

proc newStateAssgn(ctx: var Ctx, toValue: PNode): PNode =
  # Creates state assignment:
  #   :state = toValue
  newTree(nkAsgn, ctx.newStateAccess(), toValue)

proc newStateAssgn(ctx: var Ctx, stateNo: int = -2): PNode =
  # Creates state assignment:
  #   :state = stateNo
  ctx.newStateAssgn(newIntTypeNode(stateNo, ctx.g.getSysType(TLineInfo(), tyInt)))

proc newEnvVar(ctx: var Ctx, name: string, typ: PType): PSym =
  result = newSym(skVar, getIdent(ctx.g.cache, name), nextSymId(ctx.idgen), ctx.fn, ctx.fn.info)
  result.typ = typ
  assert(not typ.isNil)

  let envParam = getEnvParam(ctx.fn)
  result = addUniqueField(envParam.typ.lastSon, result, ctx.g.cache, ctx.idgen)

proc newEnvVarAccess(ctx: Ctx, s: PSym): PNode =
  result = rawIndirectAccess(newSymNode(getEnvParam(ctx.fn)), s, ctx.fn.info)

proc newTmpResultAccess(ctx: var Ctx): PNode =
  if ctx.tmpResultSym.isNil:
    ctx.tmpResultSym = ctx.newEnvVar(":tmpResult", ctx.fn.typ[0])
  ctx.newEnvVarAccess(ctx.tmpResultSym)

proc newUnrollFinallyAccess(ctx: var Ctx, info: TLineInfo): PNode =
  if ctx.unrollFinallySym.isNil:
    ctx.unrollFinallySym = ctx.newEnvVar(":unrollFinally", ctx.g.getSysType(info, tyBool))
  ctx.newEnvVarAccess(ctx.unrollFinallySym)

proc newCurExcAccess(ctx: var Ctx): PNode =
  if ctx.curExcSym.isNil:
    ctx.curExcSym = ctx.newEnvVar(":curExc", ctx.g.callCodegenProc("getCurrentException").typ)
  ctx.newEnvVarAccess(ctx.curExcSym)

proc newState(ctx: var Ctx, n, gotoOut: PNode): int =
  # Creates a new state, adds it to the context fills out `gotoOut` so that it
  # will goto this state.
  # Returns index of the newly created state

  result = ctx.states.len
  ctx.states.add (result, n)
  ctx.exceptionTable.add(ctx.curExcHandlingState)

  if not gotoOut.isNil:
    assert(gotoOut.len == 0)
    gotoOut.add(ctx.g.newIntLit(gotoOut.info, result))

proc toStmtList(n: PNode): PNode =
  result = n
  if result.kind notin {nkStmtList, nkStmtListExpr}:
    result = newTreeI(nkStmtList, n.info): n

proc addGotoOut(n: PNode, gotoOut: PNode): PNode =
  # Make sure `n` is a stmtlist, and ends with `gotoOut`
  result = toStmtList(n)
  if result.len == 0 or result[^1].kind != nkGotoState:
    result.add(gotoOut)

proc newTempVar(ctx: var Ctx, typ: PType): PSym =
  result = ctx.newEnvVar(":tmpSlLower" & $ctx.tempVarId, typ)
  inc ctx.tempVarId

proc hasYields(n: PNode): bool =
  # TODO: This is very inefficient. It traverses the node, looking for nkYieldStmt.
  case n.kind
  of nkYieldStmt:
    result = true
  of nkSkip:
    discard
  else:
    for c in n:
      if c.hasYields:
        result = true
        break

proc transformBreaksInBlock(ctx: var Ctx, n: PNode, label, after: PNode): PNode =
  result = n
  case n.kind
  of nkSkip:
    discard
  of nkBreakStmt:
    assert n[0].kind == nkSym
    if label.kind == nkSym and n[0].sym == label.sym:
      result = after
  else:
    for i in 0..<n.len:
      n[i] = ctx.transformBreaksInBlock(n[i], label, after)

proc newNullifyCurExc(ctx: var Ctx, info: TLineInfo): PNode =
  # :curEcx = nil
  let curExc = ctx.newCurExcAccess()
  curExc.info = info
  let nilnode = newNode(nkNilLit)
  nilnode.typ = curExc.typ
  result = newTree(nkAsgn, curExc, nilnode)

proc newOr(g: ModuleGraph, a, b: PNode): PNode {.inline.} =
  result = newTree(nkCall, newSymNode(g.getSysMagic(a.info, "or", mOr)), a, b)
  result.typ = g.getSysType(a.info, tyBool)
  result.info = a.info

proc collectExceptState(ctx: var Ctx, n: PNode): PNode {.inline.} =
  var ifStmt = newNodeI(nkIfStmt, n.info)
  let g = ctx.g
  for c in n:
    if c.kind == nkExceptBranch:
      var ifBranch: PNode

      if c.len > 1:
        var cond: PNode
        for i in 0..<c.len - 1:
          assert(c[i].kind == nkType)
          let nextCond = newTree(nkCall,
            newSymNode(g.getSysMagic(c.info, "of", mOf)),
            g.callCodegenProc("getCurrentException"),
            c[i])
          nextCond.typ = ctx.g.getSysType(c.info, tyBool)
          nextCond.info = c.info

          if cond.isNil:
            cond = nextCond
          else:
            cond = g.newOr(cond, nextCond)

        ifBranch = newNodeI(nkElifBranch, c.info)
        ifBranch.add(cond)
      else:
        if ifStmt.len == 0:
          ifStmt = newNodeI(nkStmtList, c.info)
          ifBranch = newNodeI(nkStmtList, c.info)
        else:
          ifBranch = newNodeI(nkElse, c.info)

      ifBranch.add(c[^1])
      ifStmt.add(ifBranch)

  if ifStmt.len != 0:
    result = newTree(nkStmtList, ctx.newNullifyCurExc(n.info), ifStmt)
  else:
    result = ctx.g.emptyNode

proc addElseToExcept(ctx: var Ctx, n: PNode) =
  if n.kind == nkStmtList and n[1].kind == nkIfStmt and n[1][^1].kind != nkElse:
    # Not all cases are covered
    let branchBody = newTreeI(nkStmtList, n.info):
      [newTree(nkAsgn, # :unrollFinally = true
         ctx.newUnrollFinallyAccess(n.info),
         newIntTypeNode(1, ctx.g.getSysType(n.info, tyBool))
       ),
       newTree(nkAsgn, # :curExc = getCurrentException()
         ctx.newCurExcAccess(),
         ctx.g.callCodegenProc("getCurrentException")
       ),
       newTree(nkGotoState, # goto nearestFinally
         ctx.g.newIntLit(n.info, ctx.nearestFinally)
       )]

    let elseBranch = newTree(nkElse, branchBody)
    n[1].add(elseBranch)

proc getFinallyNode(ctx: var Ctx, n: PNode): PNode =
  result = n[^1]
  if result.kind == nkFinally:
    result = result[0]
  else:
    result = ctx.g.emptyNode

proc hasYieldsInExpressions(n: PNode): bool =
  case n.kind
  of nkSkip:
    discard
  of nkStmtListExpr:
    if isEmptyType(n.typ):
      for c in n:
        if c.hasYieldsInExpressions:
          return true
    else:
      result = n.hasYields
  of nkCast:
    for i in 1..<n.len:
      if n[i].hasYieldsInExpressions:
        return true
  else:
    for c in n:
      if c.hasYieldsInExpressions:
        return true

proc exprToStmtList(n: PNode): tuple[s, res: PNode] =
  assert(n.kind == nkStmtListExpr)
  result.s = newNodeI(nkStmtList, n.info)

  var n = n
  while n.kind == nkStmtListExpr:
    result.s.sons.add(n.sons)
    result.s.sons.setLen(result.s.len - 1) # delete last son
    n = n[^1]

  result.res = n


proc newEnvVarAsgn(ctx: Ctx, s: PSym, v: PNode): PNode =
  if isEmptyType(v.typ):
    result = v
  else:
    result = newTree(nkFastAsgn, ctx.newEnvVarAccess(s), v)
    result.info = v.info

proc addExprAssgn(ctx: Ctx, output, input: PNode, sym: PSym) =
  if input.kind == nkStmtListExpr:
    let (st, res) = exprToStmtList(input)
    output.add(st)
    output.add(ctx.newEnvVarAsgn(sym, res))
  else:
    output.add(ctx.newEnvVarAsgn(sym, input))

proc convertExprBodyToAsgn(ctx: Ctx, exprBody: PNode, res: PSym): PNode =
  result = newNodeI(nkStmtList, exprBody.info)
  ctx.addExprAssgn(result, exprBody, res)

proc newNotCall(g: ModuleGraph; e: PNode): PNode =
  result = newTree(nkCall, newSymNode(g.getSysMagic(e.info, "not", mNot), e.info), e)
  result.typ = g.getSysType(e.info, tyBool)

proc lowerStmtListExprs(ctx: var Ctx, n: PNode, needsSplit: var bool): PNode =
  result = n
  case n.kind
  of nkSkip:
    discard

  of nkYieldStmt:
    var ns = false
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      let (st, ex) = exprToStmtList(n[0])
      n[0] = ex
      result = newTreeI(nkStmtList, n.info):
        [st, n]

    needsSplit = true

  of nkPar, nkObjConstr, nkTupleConstr, nkBracket:
    var ns = false
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      needsSplit = true

      ctx.g.config.internalAssert(n.typ != nil, "lowerStmtListExprs: constr typ.isNil")
      result = newNodeIT(nkStmtListExpr, n.info, n.typ)

      for i in 0..<n.len:
        case n[i].kind
        of nkExprColonExpr:
          if n[i][1].kind == nkStmtListExpr:
            let (st, ex) = exprToStmtList(n[i][1])
            result.add(st)
            n[i][1] = ex
        of nkStmtListExpr:
          let (st, ex) = exprToStmtList(n[i])
          result.add(st)
          n[i] = ex
        else: discard
      result.add(n)

  of nkIfStmt, nkIfExpr:
    var ns = false
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      needsSplit = true
      var tmp: PSym
      let isExpr = not isEmptyType(n.typ)
      if isExpr:
        tmp = ctx.newTempVar(n.typ)
        result = newNodeIT(nkStmtListExpr, n.info, n.typ)
      else:
        result = newNodeI(nkStmtList, n.info)

      var curS = result

      for branch in n:
        case branch.kind
        of nkElseExpr, nkElse:
          if isExpr:
            let branchBody = newNodeI(nkStmtList, branch.info)
            ctx.addExprAssgn(branchBody, branch[0], tmp)
            let newBranch = newTree(nkElse, branchBody)
            curS.add(newBranch)
          else:
            curS.add(branch)

        of nkElifExpr, nkElifBranch:
          var newBranch: PNode
          if branch[0].kind == nkStmtListExpr:
            let (st, res) = exprToStmtList(branch[0])
            let elseBody = newTree(nkStmtList, st)

            newBranch = newTree(nkElifBranch, res, branch[1])

            let newIf = newTree(nkIfStmt, newBranch)
            elseBody.add(newIf)
            if curS.kind == nkIfStmt:
              let newElse = newNodeI(nkElse, branch.info)
              newElse.add(elseBody)
              curS.add(newElse)
            else:
              curS.add(elseBody)
            curS = newIf
          else:
            newBranch = branch
            if curS.kind == nkIfStmt:
              curS.add(newBranch)
            else:
              let newIf = newTree(nkIfStmt, newBranch)
              curS.add(newIf)
              curS = newIf

          if isExpr:
            let branchBody = newNodeI(nkStmtList, branch[1].info)
            ctx.addExprAssgn(branchBody, branch[1], tmp)
            newBranch[1] = branchBody

        else:
          internalError(ctx.g.config, "lowerStmtListExpr(nkIf): " & $branch.kind)

      if isExpr: result.add(ctx.newEnvVarAccess(tmp))

  of nkTryStmt, nkHiddenTryStmt:
    var ns = false
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      needsSplit = true
      let isExpr = not isEmptyType(n.typ)

      if isExpr:
        result = newNodeIT(nkStmtListExpr, n.info, n.typ)
        let tmp = ctx.newTempVar(n.typ)

        n[0] = ctx.convertExprBodyToAsgn(n[0], tmp)
        for i in 1..<n.len:
          let branch = n[i]
          case branch.kind
          of nkExceptBranch:
            if branch[0].kind == nkType:
              branch[1] = ctx.convertExprBodyToAsgn(branch[1], tmp)
            else:
              branch[0] = ctx.convertExprBodyToAsgn(branch[0], tmp)
          of nkFinally:
            discard
          else:
            internalError(
              ctx.g.config, "lowerStmtListExpr(nkTryStmt): " & $branch.kind)

        result.add(n)
        result.add(ctx.newEnvVarAccess(tmp))

  of nkCaseStmt:
    var ns = false
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      needsSplit = true

      let isExpr = not isEmptyType(n.typ)

      if isExpr:
        let tmp = ctx.newTempVar(n.typ)
        result = newNodeIT(nkStmtListExpr, n.info, n.typ)

        if n[0].kind == nkStmtListExpr:
          let (st, ex) = exprToStmtList(n[0])
          result.add(st)
          n[0] = ex

        for i in 1..<n.len:
          let branch = n[i]
          case branch.kind
          of nkOfBranch:
            branch[^1] = ctx.convertExprBodyToAsgn(branch[^1], tmp)
          of nkElse:
            branch[0] = ctx.convertExprBodyToAsgn(branch[0], tmp)
          else:
            internalError(ctx.g.config, "lowerStmtListExpr(nkCaseStmt): " & $branch.kind)

        # the ``nkCaseStmt`` expression is a statement now, and the type needs
        # to reflect that
        n.typ = nil
        result.add(n)
        result.add(ctx.newEnvVarAccess(tmp))
      elif n[0].kind == nkStmtListExpr:
        let (st, ex) = exprToStmtList(n[0])
        n[0] = ex
        result = newTreeI(nkStmtList, n.info):
          [st, n]

  of nkCallKinds, nkChckRange, nkChckRangeF, nkChckRange64:
    var ns = false
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      needsSplit = true
      let isExpr = not isEmptyType(n.typ)

      if isExpr:
        result = newNodeIT(nkStmtListExpr, n.info, n.typ)
      else:
        result = newNodeI(nkStmtList, n.info)

      if n[0].kind == nkSym and n[0].sym.magic in {mAnd, mOr}: # `and`/`or` short cirquiting
        var cond = n[1]
        if cond.kind == nkStmtListExpr:
          let (st, ex) = exprToStmtList(cond)
          result.add(st)
          cond = ex

        let tmp = ctx.newTempVar(cond.typ)
        result.add(ctx.newEnvVarAsgn(tmp, cond))

        var check = ctx.newEnvVarAccess(tmp)
        if n[0].sym.magic == mOr:
          check = ctx.g.newNotCall(check)

        cond = n[2]
        let ifBody = newNodeI(nkStmtList, cond.info)
        if cond.kind == nkStmtListExpr:
          let (st, ex) = exprToStmtList(cond)
          ifBody.add(st)
          cond = ex
        ifBody.add(ctx.newEnvVarAsgn(tmp, cond))

        let ifBranch = newTree(nkElifBranch, check, ifBody)
        let ifNode = newTree(nkIfStmt, ifBranch)
        result.add(ifNode)
        result.add(ctx.newEnvVarAccess(tmp))
      else:
        for i in 0..<n.len:
          if n[i].kind == nkStmtListExpr:
            let (st, ex) = exprToStmtList(n[i])
            result.add(st)
            n[i] = ex

          if n[i].kind in nkCallKinds: # XXX: This should better be some sort of side effect tracking
            let tmp = ctx.newTempVar(n[i].typ)
            result.add(ctx.newEnvVarAsgn(tmp, n[i]))
            n[i] = ctx.newEnvVarAccess(tmp)

        result.add(n)

  of nkVarSection, nkLetSection:
    result = newNodeI(nkStmtList, n.info)
    for c in n:
      let varSect = newTreeI(n.kind, n.info): c
      var ns = false
      c[^1] = ctx.lowerStmtListExprs(c[^1], ns)
      if ns:
        needsSplit = true
        let (st, ex) = exprToStmtList(c[^1])
        result.add(st)
        c[^1] = ex
      result.add(varSect)

  of nkDiscardStmt, nkReturnStmt, nkRaiseStmt:
    var ns = false
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      needsSplit = true
      let (st, ex) = exprToStmtList(n[0])
      n[0] = ex
      result = newTreeI(nkStmtList, n.info):
        [st, n]

  of nkCast, nkHiddenStdConv, nkHiddenSubConv, nkConv, nkObjUpConv,
     nkObjDownConv, nkDerefExpr, nkHiddenDeref:
    var ns = false
    for i in ord(n.kind == nkCast)..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      needsSplit = true
      let (st, ex) = exprToStmtList(n[^1])
      n[^1] = ex
      result = newTreeIT(nkStmtListExpr, n.info, n.typ):
        [st, n]

  of nkAsgn, nkFastAsgn:
    var ns = false
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], ns)

    if ns:
      needsSplit = true
      result = newNodeI(nkStmtList, n.info)
      if n[0].kind == nkStmtListExpr:
        let (st, ex) = exprToStmtList(n[0])
        result.add(st)
        n[0] = ex

      if n[1].kind == nkStmtListExpr:
        let (st, ex) = exprToStmtList(n[1])
        result.add(st)
        n[1] = ex

      result.add(n)

  of nkBracketExpr:
    var lhsNeedsSplit = false
    var rhsNeedsSplit = false
    n[0] = ctx.lowerStmtListExprs(n[0], lhsNeedsSplit)
    n[1] = ctx.lowerStmtListExprs(n[1], rhsNeedsSplit)
    if lhsNeedsSplit or rhsNeedsSplit:
      needsSplit = true
      result = newNodeI(nkStmtListExpr, n.info)
      if lhsNeedsSplit:
        let (st, ex) = exprToStmtList(n[0])
        result.add(st)
        n[0] = ex

      if rhsNeedsSplit:
        let (st, ex) = exprToStmtList(n[1])
        result.add(st)
        n[1] = ex
      result.add(n)

  of nkWhileStmt:
    assert isTrue(n[0])
    n[1] = ctx.lowerStmtListExprs(n[1], needsSplit)

  of nkDotExpr, nkCheckedFieldExpr:
    var ns = false
    n[0] = ctx.lowerStmtListExprs(n[0], ns)
    if ns:
      needsSplit = true
      let (st, ex) = exprToStmtList(n[0])
      n[0] = ex
      result = newTreeIT(nkStmtListExpr, n.info, n.typ):
        [st, n]

  of nkBlockExpr:
    var ns = false
    n[1] = ctx.lowerStmtListExprs(n[1], ns)
    if ns:
      needsSplit = true
      let (st, ex) = exprToStmtList(n[1])
      result = newTreeIT(nkStmtListExpr, n.info, n.typ):
        [n, ex]
      n.transitionSonsKind(nkBlockStmt)
      n.typ = nil
      n[1] = st

  else:
    for i in 0..<n.len:
      n[i] = ctx.lowerStmtListExprs(n[i], needsSplit)

proc newEndFinallyNode(ctx: var Ctx, info: TLineInfo): PNode =
  # Generate the following code:
  #   if :unrollFinally:
  #       if :curExc.isNil:
  #         return :tmpResult
  #       else:
  #         raise
  let curExc = ctx.newCurExcAccess()
  let nilnode = newNodeIT(nkNilLit, info, curExc.typ)
  let cmp = newTreeIT(nkCall, info, ctx.g.getSysType(info, tyBool)):
    [newSymNode(ctx.g.getSysMagic(info, "==", mEqRef), info), curExc, nilnode]

  let asgn = newTree(nkFastAsgn):
    [newSymNode(getClosureIterResult(ctx.g, ctx.fn, ctx.idgen), info),
     ctx.newTmpResultAccess()]

  let retStmt = newTree(nkReturnStmt): asgn
  let branch = newTree(nkElifBranch): [cmp, retStmt]

  let nullifyExc = newTreeI(nkCall, info):
    [newSymNode(ctx.g.getCompilerProc("closureIterSetupExc")), nilnode]
  let raiseStmt = newTreeI(nkRaiseStmt, info): curExc
  let elseBranch = newTree(nkElse, newTree(nkStmtList, nullifyExc, raiseStmt))

  let ifBody = newTree(nkIfStmt): [branch, elseBranch]
  let elifBranch = newTreeI(nkElifBranch, info): [ctx.newUnrollFinallyAccess(info), ifBody]
  result = newTree(nkIfStmt): elifBranch

proc transformReturnsInTry(ctx: var Ctx, n: PNode): PNode =
  result = n
  # TODO: This is very inefficient. It traverses the node, looking for nkYieldStmt.
  case n.kind
  of nkReturnStmt:
    # We're somewhere in try, transform to finally unrolling
    assert(ctx.nearestFinally != 0)

    result = newNodeI(nkStmtList, n.info)

    block: # :unrollFinally = true
      let asgn = newTreeI(nkAsgn, n.info):
        [ctx.newUnrollFinallyAccess(n.info),
         newIntTypeNode(1, ctx.g.getSysType(n.info, tyBool))]
      result.add(asgn)

    if n[0].kind != nkEmpty:
      let asgnTmpResult = newTreeI(nkAsgn, n.info):
        [ctx.newTmpResultAccess(),
         if n[0].kind in {nkAsgn, nkFastAsgn}: n[0][1] else: n[0] ]
      result.add(asgnTmpResult)

    result.add(ctx.newNullifyCurExc(n.info))

    let goto = newTree(nkGotoState, ctx.g.newIntLit(n.info, ctx.nearestFinally))
    result.add(goto)

  of nkSkip:
    discard
  else:
    for i in 0..<n.len:
      n[i] = ctx.transformReturnsInTry(n[i])

proc transformClosureIteratorBody(ctx: var Ctx, n: PNode, gotoOut: PNode): PNode =
  result = n
  case n.kind
  of nkSkip: discard

  of nkStmtList, nkStmtListExpr:
    result = addGotoOut(result, gotoOut)
    for i in 0..<n.len:
      if n[i].hasYields:
        # Create a new split
        let go = newNodeI(nkGotoState, n[i].info)
        n[i] = ctx.transformClosureIteratorBody(n[i], go)

        let s = newNodeI(nkStmtList, n[i + 1].info)
        for j in i + 1..<n.len:
          s.add(n[j])

        n.sons.setLen(i + 1)
        discard ctx.newState(s, go)
        if ctx.transformClosureIteratorBody(s, gotoOut) != s:
          internalError(ctx.g.config, "transformClosureIteratorBody != s")
        break

  of nkYieldStmt:
    result = newTreeI(nkStmtList, n.info):
      [n, gotoOut]

  of nkElse, nkElseExpr:
    result[0] = addGotoOut(result[0], gotoOut)
    result[0] = ctx.transformClosureIteratorBody(result[0], gotoOut)

  of nkElifBranch, nkElifExpr, nkOfBranch:
    result[^1] = addGotoOut(result[^1], gotoOut)
    result[^1] = ctx.transformClosureIteratorBody(result[^1], gotoOut)

  of nkIfStmt, nkCaseStmt:
    for i in 0..<n.len:
      n[i] = ctx.transformClosureIteratorBody(n[i], gotoOut)
    if n[^1].kind != nkElse:
      # We don't have an else branch, but every possible branch has to end with
      # gotoOut, so add else here.
      let elseBranch = newTree(nkElse, gotoOut)
      n.add(elseBranch)

  of nkWhileStmt:
    # while true:
    #   s
    # ->
    # BEGIN_STATE:
    #   s
    #   goto BEGIN_STATE
    assert isTrue(n[0])
    result = newNodeI(nkGotoState, n.info)

    let s = newNodeI(nkStmtList, n.info)
    discard ctx.newState(s, result)

    var body = addGotoOut(n[1], result)
    body = ctx.transformClosureIteratorBody(body, result)
    s.add(body)

  of nkBlockStmt:
    result[1] = addGotoOut(result[1], gotoOut)
    result[1] = ctx.transformBreaksInBlock(result[1], result[0], gotoOut)
    result[1] = ctx.transformClosureIteratorBody(result[1], gotoOut)

  of nkTryStmt, nkHiddenTryStmt:
    # See explanation above about how this works
    ctx.hasExceptions = true

    result = newNodeI(nkGotoState, n.info)
    var tryBody = toStmtList(n[0])
    var exceptBody = ctx.collectExceptState(n)
    var finallyBody = newTree(nkStmtList, getFinallyNode(ctx, n))
    finallyBody = ctx.transformReturnsInTry(finallyBody)
    finallyBody.add(ctx.newEndFinallyNode(finallyBody.info))

    # The following index calculation is based on the knowledge how state
    # indexes are assigned
    let tryIdx = ctx.states.len
    var exceptIdx, finallyIdx: int
    if exceptBody.kind != nkEmpty:
      exceptIdx = -(tryIdx + 1)
      finallyIdx = tryIdx + 2
    else:
      exceptIdx = tryIdx + 1
      finallyIdx = tryIdx + 1

    let outToFinally = newNodeI(nkGotoState, finallyBody.info)

    block: # Create initial states.
      let oldExcHandlingState = ctx.curExcHandlingState
      ctx.curExcHandlingState = exceptIdx
      let realTryIdx = ctx.newState(tryBody, result)
      assert(realTryIdx == tryIdx)

      if exceptBody.kind != nkEmpty:
        ctx.curExcHandlingState = finallyIdx
        let realExceptIdx = ctx.newState(exceptBody, nil)
        assert(realExceptIdx == -exceptIdx)

      ctx.curExcHandlingState = oldExcHandlingState
      let realFinallyIdx = ctx.newState(finallyBody, outToFinally)
      assert(realFinallyIdx == finallyIdx)

    block: # Subdivide the states
      let oldNearestFinally = ctx.nearestFinally
      ctx.nearestFinally = finallyIdx

      let oldExcHandlingState = ctx.curExcHandlingState

      ctx.curExcHandlingState = exceptIdx

      if ctx.transformReturnsInTry(tryBody) != tryBody:
        internalError(ctx.g.config, "transformReturnsInTry != tryBody")
      if ctx.transformClosureIteratorBody(tryBody, outToFinally) != tryBody:
        internalError(ctx.g.config, "transformClosureIteratorBody != tryBody")

      ctx.curExcHandlingState = finallyIdx
      ctx.addElseToExcept(exceptBody)
      if ctx.transformReturnsInTry(exceptBody) != exceptBody:
        internalError(ctx.g.config, "transformReturnsInTry != exceptBody")
      if ctx.transformClosureIteratorBody(exceptBody, outToFinally) != exceptBody:
        internalError(ctx.g.config, "transformClosureIteratorBody != exceptBody")

      ctx.curExcHandlingState = oldExcHandlingState
      ctx.nearestFinally = oldNearestFinally
      if ctx.transformClosureIteratorBody(finallyBody, gotoOut) != finallyBody:
        internalError(ctx.g.config, "transformClosureIteratorBody != finallyBody")

  of nkGotoState, nkForStmt:
    internalError(ctx.g.config, "closure iter " & $n.kind)

  else:
    for i in 0..<n.len:
      n[i] = ctx.transformClosureIteratorBody(n[i], gotoOut)

proc stateFromGotoState(n: PNode): int =
  assert(n.kind == nkGotoState)
  result = n[0].intVal.int

proc transformStateAssignments(ctx: var Ctx, n: PNode): PNode =
  # This transforms 3 patterns:
  ########################## 1
  # yield e
  # goto STATE
  # ->
  # :state = STATE
  # return e
  ########################## 2
  # goto STATE
  # ->
  # :state = STATE
  # break :stateLoop
  ########################## 3
  # return e
  # ->
  # :state = -1
  # return e
  #
  result = n
  case n.kind
  of nkStmtList, nkStmtListExpr:
    if n.len != 0 and n[0].kind == nkYieldStmt:
      assert(n.len == 2)
      assert(n[1].kind == nkGotoState)

      result = newNodeI(nkStmtList, n.info)
      result.add(ctx.newStateAssgn(stateFromGotoState(n[1])))

      var retStmt = newNodeI(nkReturnStmt, n.info)
      if n[0][0].kind != nkEmpty:
        var a = newNodeI(nkAsgn, n[0][0].info)
        var retVal = n[0][0] #liftCapturedVars(n[0], owner, d, c)
        a.add newSymNode(getClosureIterResult(ctx.g, ctx.fn, ctx.idgen))
        a.add retVal
        retStmt.add(a)
      else:
        retStmt.add(ctx.g.emptyNode)

      result.add(retStmt)
    else:
      for i in 0..<n.len:
        n[i] = ctx.transformStateAssignments(n[i])

  of nkSkip:
    discard

  of nkReturnStmt:
    result = newNodeI(nkStmtList, n.info)
    result.add(ctx.newStateAssgn(-1))
    result.add(n)

  of nkGotoState:
    result = newNodeI(nkStmtList, n.info)
    result.add(ctx.newStateAssgn(stateFromGotoState(n)))
    result.add(newBreakStmt(n.info, ctx.stateLoopLabel))

  else:
    for i in 0..<n.len:
      n[i] = ctx.transformStateAssignments(n[i])

proc skipStmtList(ctx: Ctx; n: PNode): PNode =
  result = n
  while result.kind in {nkStmtList}:
    if result.len == 0: return ctx.g.emptyNode
    result = result[0]

proc skipEmptyStates(ctx: Ctx, stateIdx: int): int =
  # Returns first non-empty state idx for `stateIdx`. Returns `stateIdx` if
  # it is not empty
  var maxJumps = ctx.states.len # maxJumps used only for debugging purposes.
  var stateIdx = stateIdx
  while true:
    let label = stateIdx
    if label == ctx.exitStateIdx: break
    var newLabel = label
    if label == -1:
      newLabel = ctx.exitStateIdx
    else:
      let fs = skipStmtList(ctx, ctx.states[label].body)
      if fs.kind == nkGotoState:
        newLabel = fs[0].intVal.int
    if label == newLabel: break
    stateIdx = newLabel
    dec maxJumps
    if maxJumps == 0:
      assert(false, "Internal error")

  result = ctx.states[stateIdx].label

proc skipThroughEmptyStates(ctx: var Ctx, n: PNode): PNode=
  result = n
  case n.kind
  of nkSkip:
    discard
  of nkGotoState:
    result = copyTree(n)
    result[0].intVal = ctx.skipEmptyStates(result[0].intVal.int)
  else:
    for i in 0..<n.len:
      n[i] = ctx.skipThroughEmptyStates(n[i])

proc newArrayType(g: ModuleGraph; n: int, t: PType; idgen: IdGenerator; owner: PSym): PType =
  result = newType(tyArray, nextTypeId(idgen), owner)

  let rng = newType(tyRange, nextTypeId(idgen), owner)
  rng.n = newTree(nkRange, g.newIntLit(owner.info, 0), g.newIntLit(owner.info, n))
  rng.rawAddSon(t)

  result.rawAddSon(rng)
  result.rawAddSon(t)

proc createExceptionTable(ctx: var Ctx): PNode {.inline.} =
  result = newNodeI(nkBracket, ctx.fn.info)
  result.typ = ctx.g.newArrayType(ctx.exceptionTable.len, ctx.g.getSysType(ctx.fn.info, tyInt16), ctx.idgen, ctx.fn)

  for i in ctx.exceptionTable:
    let elem = newIntNode(nkIntLit, i)
    elem.typ = ctx.g.getSysType(ctx.fn.info, tyInt16)
    result.add(elem)

proc newCatchBody(ctx: var Ctx, info: TLineInfo): PNode {.inline.} =
  # Generates the code:
  # :state = exceptionTable[:state]
  # if :state == 0: raise
  # :unrollFinally = :state > 0
  # if :state < 0:
  #   :state = -:state
  # :curExc = getCurrentException()

  result = newNodeI(nkStmtList, info)

  let intTyp = ctx.g.getSysType(info, tyInt)
  let boolTyp = ctx.g.getSysType(info, tyBool)

  # :state = exceptionTable[:state]
  block:
    # exceptionTable[:state]
    let getNextState = newTree(nkBracketExpr,
      ctx.createExceptionTable(),
      ctx.newStateAccess())
    getNextState.typ = intTyp

    # :state = exceptionTable[:state]
    result.add(ctx.newStateAssgn(getNextState))

  # if :state == 0: raise
  block:
    let cond = newTree(nkCall,
      ctx.g.getSysMagic(info, "==", mEqI).newSymNode(),
      ctx.newStateAccess(),
      newIntTypeNode(0, intTyp))
    cond.typ = boolTyp

    let raiseStmt = newTree(nkRaiseStmt, ctx.g.emptyNode)
    let ifBranch = newTree(nkElifBranch, cond, raiseStmt)
    let ifStmt = newTree(nkIfStmt, ifBranch)
    result.add(ifStmt)

  # :unrollFinally = :state > 0
  block:
    let cond = newTree(nkCall,
      ctx.g.getSysMagic(info, "<", mLtI).newSymNode,
      newIntTypeNode(0, intTyp),
      ctx.newStateAccess())
    cond.typ = boolTyp

    let asgn = newTree(nkAsgn, ctx.newUnrollFinallyAccess(info), cond)
    result.add(asgn)

  # if :state < 0: :state = -:state
  block:
    let cond = newTree(nkCall,
      ctx.g.getSysMagic(info, "<", mLtI).newSymNode,
      ctx.newStateAccess(),
      newIntTypeNode(0, intTyp))
    cond.typ = boolTyp

    let negateState = newTree(nkCall,
      ctx.g.getSysMagic(info, "-", mUnaryMinusI).newSymNode,
      ctx.newStateAccess())
    negateState.typ = intTyp

    let ifBranch = newTree(nkElifBranch, cond, ctx.newStateAssgn(negateState))
    let ifStmt = newTree(nkIfStmt, ifBranch)
    result.add(ifStmt)

  # :curExc = getCurrentException()
  block:
    result.add(newTree(nkAsgn,
      ctx.newCurExcAccess(),
      ctx.g.callCodegenProc("getCurrentException")))

proc wrapIntoTryExcept(ctx: var Ctx, n: PNode): PNode {.inline.} =
  let setupExc = newTree(nkCall,
    newSymNode(ctx.g.getCompilerProc("closureIterSetupExc")),
    ctx.newCurExcAccess())

  let tryBody = newTree(nkStmtList, setupExc, n)
  let exceptBranch = newTree(nkExceptBranch, ctx.newCatchBody(ctx.fn.info))

  result = newTree(nkTryStmt, tryBody, exceptBranch)

proc wrapIntoStateLoop(ctx: var Ctx, n: PNode): PNode =
  # while true:
  #   block :stateLoop:
  #     local vars decl (if needed)
  #     body # Might get wrapped in try-except
  let loopBody = newNodeI(nkStmtList, n.info)
  result = newTree(nkWhileStmt, boolLit(ctx, n.info, true), loopBody)
  result.info = n.info

  let blockStmt = newNodeI(nkBlockStmt, n.info)
  blockStmt.add(newSymNode(ctx.stateLoopLabel))

  var blockBody = newTree(nkStmtList, n)
  if ctx.hasExceptions:
    blockBody = ctx.wrapIntoTryExcept(blockBody)

  blockStmt.add(blockBody)
  loopBody.add(blockStmt)

proc deleteEmptyStates(ctx: var Ctx) =
  let goOut = newTree(nkGotoState, ctx.g.newIntLit(TLineInfo(), -1))
  ctx.exitStateIdx = ctx.newState(goOut, nil)

  const unusedLabel = -1
    ## indicates that the label refers to an empty/redundant code block, and is
    ## going to be removed

  # Apply new state indexes and mark unused states with -1
  var iValid = 0
  for i, s in ctx.states.mpairs:
    let body = skipStmtList(ctx, s.body)
    if body.kind == nkGotoState and i in 1..(ctx.states.len-2):
      # this is an empty state
      s.label = unusedLabel
    else:
      s.label = iValid
      inc iValid

  for i, s in ctx.states.pairs:
    let body = skipStmtList(ctx, s.body)
    if body.kind != nkGotoState or i == 0:
      discard ctx.skipThroughEmptyStates(s.body)
      let excHandlState = ctx.exceptionTable[i]
      if excHandlState < 0:
        ctx.exceptionTable[i] = -ctx.skipEmptyStates(-excHandlState)
      elif excHandlState != 0:
        ctx.exceptionTable[i] = ctx.skipEmptyStates(excHandlState)

  # remove unused states except for the first (entry) and last one (exit)
  var i = 1
  while i < ctx.states.len - 1:
    if ctx.states[i].label == unusedLabel:
      ctx.states.delete(i)
      ctx.exceptionTable.delete(i)
    else:
      inc i

type
  PreprocessContext = object
    finallys: seq[PNode]
    config: ConfigRef
    blocks: seq[tuple[label: PSym, fin: int]]
    idgen: IdGenerator
  FreshVarsContext = object
    tab: Table[int, PSym]
    config: ConfigRef
    info: TLineInfo
    idgen: IdGenerator

proc freshVars(n: PNode; c: var FreshVarsContext): PNode =
  case n.kind
  of nkSym:
    let x = c.tab.getOrDefault(n.sym.id)
    if x == nil:
      result = n
    else:
      result = newSymNode(x, n.info)
  of nkSkip - {nkSym}:
    result = n
  of nkLetSection, nkVarSection:
    result = copyNode(n)
    for it in n:
      if it.kind in {nkIdentDefs, nkVarTuple}:
        let idefs = copyNode(it)
        for v in 0..it.len-3:
          if it[v].kind == nkSym:
            let x = copySym(it[v].sym, nextSymId(c.idgen))
            c.tab[it[v].sym.id] = x
            idefs.add newSymNode(x)
          else:
            idefs.add it[v]

        for rest in it.len-2 ..< it.len: idefs.add it[rest]
        result.add idefs
      else:
        result.add it
  of nkRaiseStmt:
    temporaryStringError(
      c.config, c.info,
      "unsupported control flow: 'finally: ... raise' duplicated because of 'break'")
  else:
    result = n
    for i in 0..<n.safeLen:
      result[i] = freshVars(n[i], c)

proc preprocess(c: var PreprocessContext; n: PNode): PNode =
  # in order to fix bug #15243 without risking regressions, we preprocess
  # the AST so that 'break' statements inside a 'try finally' also have the
  # finally section. We need to duplicate local variables here and also
  # detect: 'finally: raises X' which is currently not supported. We produce
  # an error for this case for now. All this will be done properly with Yuriy's
  # patch.
  result = n
  case n.kind
  of nkTryStmt:
    let f = n.lastSon
    if f.kind == nkFinally:
      c.finallys.add f.lastSon

    for i in 0 ..< n.len:
      result[i] = preprocess(c, n[i])

    if f.kind == nkFinally:
      discard c.finallys.pop()

  of nkBlockStmt:
    c.blocks.add((n[0].sym, c.finallys.len))
    result[1] = preprocess(c, n[1])
    discard c.blocks.pop()

  of nkBreakStmt:
    assert c.blocks.len > 0
    if true:
      var fin = -1
      block search:
        for i in countdown(c.blocks.high, 0):
          if c.blocks[i].label == n[0].sym:
            fin = c.blocks[i].fin
            break search

        unreachable("missing break target")

      if true:
        result = newNodeI(nkStmtList, n.info)
        for i in countdown(c.finallys.high, fin):
          var vars = FreshVarsContext(tab: initTable[int, PSym](), config: c.config, info: n.info, idgen: c.idgen)
          result.add freshVars(preprocess(c, c.finallys[i]), vars)
          c.idgen = vars.idgen
        result.add n
  of nkSkip: discard
  else:
    for i in 0 ..< n.len:
      result[i] = preprocess(c, n[i])

proc transformClosureIterator*(g: ModuleGraph; idgen: IdGenerator; fn: PSym, n: PNode): PNode =
  var ctx: Ctx
  ctx.g = g
  ctx.fn = fn
  ctx.idgen = idgen

  ctx.stateLoopLabel = newSym(skLabel, getIdent(ctx.g.cache, ":stateLoop"), nextSymId(idgen), fn, fn.info)
  var pc = PreprocessContext(finallys: @[], config: g.config, idgen: idgen)
  var n = preprocess(pc, n.toStmtList)
  #echo "transformed into ", n
  #var n = n.toStmtList

  discard ctx.newState(n, nil)
  let gotoOut = newTree(nkGotoState, g.newIntLit(n.info, -1))

  var ns = false
  n = ctx.lowerStmtListExprs(n, ns)

  ctx.g.config.internalAssert(not n.hasYieldsInExpressions, "yield in expr not lowered")

  # Splitting transformation
  discard ctx.transformClosureIteratorBody(n, gotoOut)

  # Optimize empty states away
  ctx.deleteEmptyStates()

  # create the dispatcher:
  #
  #   case env.:state
  #   of 0: ...
  #   of 1: ...
  #   ...
  #   else: return
  result = newNodeI(nkCaseStmt, n.info)
  result.add(ctx.newStateAccess())
  for s in ctx.states.items:
    # transform the gotos into state assignments:
    let body = ctx.transformStateAssignments(s.body)
    # then add the block as a branch to the dispatcher:
    result.add:
      newTreeI(nkOfBranch, body.info):
        [g.newIntLit(body.info, s.label), body]

  # add the exit:
  result.add:
    newTreeI(nkElse, n.info):
      newTreeI(nkReturnStmt, n.info, g.emptyNode)

  result = ctx.wrapIntoStateLoop(result)
