## Implements tail call analysis, as well as synthesis of procedures needed for
## portable tail-call elimination (refer to this `document <tailcallelim.html>`_
## for how it works).
##
## The `apply` helper procedure is synthesized during semantic analysis already,
## as `sem` is best equipped to create new global symbols. The synthesized
## procedures are attached to their originating ones via the misc slot.

import
  compiler/ast/[
    ast,
    ast_query,
    ast_types,
    ast_idgen,
    idents,
    lineinfos,
    trees,
    types
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/sem/[
    lowerings,
    semdata
  ],
  compiler/front/[
    msgs
  ],
  compiler/utils/[
    idioms
  ]

from compiler/ast/report_enums import ReportKind
from compiler/ast/reports_sem import SemReport, reportAst, reportSym

# -------------- analysis --------------

proc isValidLvalueExpr(n: PNode): bool =
  ## Computes whether `n` is an lvalue expression that may be passed to a var
  ## or pass-by-reference parameter.
  case n.kind
  of nkSym:
    n.sym.kind == skParam or sfGlobal in n.sym.flags
  of nkDerefExpr:
    if n[0].typ.skipTypes(abstractInst).kind == tyPtr:
      true
    else:
      isValidLvalueExpr(n[0])
  of nkDotExpr, nkBracketExpr, nkHiddenDeref, nkObjUpConv,
     nkObjDownConv, nkCheckedFieldExpr, nkHiddenAddr, nkAddr:
    isValidLvalueExpr(n[0])
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    isValidLvalueExpr(n[1])
  of nkCallKinds:
    getMagic(n) == mSlice and isValidLvalueExpr(n[1])
  else:
    false

proc checkArg(g: ModuleGraph, owner: PSym, n: PNode, i: int, formal: PType) =
  ## Analyses the `i`-th argument `n` for a call to a procedure with type
  ## `formal`. Reports an error for arguments not adhering to the .tailcall
  ## call rules.
  case formal[i].kind
  of tySink:
    discard "all expressions are admitted"
  of tyVar:
    if not isValidLvalueExpr(n):
      g.config.localReport(n.info,
        SemReport(kind: rsemArgumentMustBorrowFromParameter))
  else:
    if isPassByRef(g.config, formal.n[i].sym, formal[0]) or
        hasDestructor(formal[i]):
      # pass-by-value arguments with custom copy behaviour cannot safely be
      # shallow-copied and thus must be borrowed too
      if not isValidLvalueExpr(n):
        g.config.localReport(n.info,
          SemReport(kind: rsemArgumentMustBorrowFromParameter))
    else:
      discard "acts like a sink parameter"

proc verifyTailCalls(g: ModuleGraph, owner: PSym, n, next, problem: PNode) =
  ## Runs the analysis to make sure all .tailcall calls are proper tail calls.
  ## Reports an error for every violation. `next` is following expression/
  ## statement (or nil), `problem` the closest problematic try/except/finally/
  ## defer (or nil).
  template recurse(x: PNode, next = n, p = problem) =
    verifyTailCalls(g, owner, x, next, p)

  case n.kind
  of nkReturnStmt:
    if n[0].kind == nkAsgn:
      recurse(n[0][1], next=nil)
  of nkCallKinds:
    if n[0].kind == nkSym and n[0].sym.magic == mRunnableExamples:
      # the body was not typed properly, nor is it relevant for the
      # analysis; skip
      return

    for it in n.items:
      recurse(it) # arguments are not tailing expressions

    # XXX: unfolded type expressions reach here, making it possible that the
    #      callee's type is missing
    if n[0].typ != nil and
       n[0].typ.skipTypes(abstractInst).callConv == ccTailcall:
      if problem != nil:
        let rep =
          case problem.kind
          of nkDefer: rsemDeferPreventsTailCall
          of nkExceptBranch: rsemExceptPreventsTailCall
          of nkFinally: rsemFinallyPreventsTailCall
          of nkTryStmt: rsemTryPreventsTailCall
          else: unreachable()

        g.config.localReport(n.info, reportAst(rep, problem))
      elif next != nil:
        if n.typ.isEmptyType():
          # it's a void call
          g.config.localReport(n.info,
            reportAst(rsemTrailingStatementPreventsTailCall, next))
        else:
          g.config.localReport(n.info,
            SemReport(kind: rsemNoTailingExpression))
      else:
        # trailing cleanup is detected at a later stage
        discard "all good"

      # make sure the arguments adhere to the rules
      for i in 1..<n.len:
        checkArg(g, owner, n[i], i, n[0].typ.skipTypes(abstractInst))
  of nkStmtList, nkStmtListExpr:
    var problem = problem
    for i in 0..<n.len-1:
      recurse(n[i], n[i + 1], problem)
      if n[i].kind == nkDefer:
        problem = n[i]

    if n.len > 0:
      # the defer also applies to the trailing statement
      recurse(n[^1], next, problem)
  of nkElifBranch, nkElifExpr, nkCaseStmt:
    recurse(n[0])
    for i in 1..<n.len:
      recurse(n[i], next)
  of nkIfStmt, nkIfExpr:
    for it in n.items:
      recurse(it, next)
  of nkDotExpr, nkCheckedFieldExpr:
    # special-cased so that only the first node is scanned
    recurse(n[0])
  of nkCast, nkConv, nkHiddenStdConv, nkHiddenSubConv, nkExprColonExpr,
     nkIdentDefs, nkVarTuple:
    # special-cased so that only the last node is scanned
    recurse(n[^1])
  of nkOfBranch, nkBlockExpr, nkBlockStmt, nkPragmaBlock, nkPragmaExpr,
     nkElse, nkElseExpr:
    # expressions within are tailing expressions
    recurse(n[^1], next)
  of nkExceptBranch, nkFinally:
    # tail calls are not possible within
    recurse(n[^1], next, n)
  of nkTryStmt, nkHiddenTryStmt:
    recurse(n[0], next, n)
    for i in 1..<n.len:
      recurse(n[i], next)
  of nkObjConstr:
    for i in 1..<n.len:
      recurse(n[^1])
  of nkForStmt:
    recurse(n[1])
    recurse(n[2])
  of nkWithoutSons, callableDefs, nkTypeSection, nkConstSection, nkNimNodeLit,
     nkSymChoices, nkBindStmt, nkMixinStmt:
    # don't enter nested routine declarations
    discard "nothing to do"
  else:
    # default processing; all immediate children are not tailing expressions
    for it in n.items:
      recurse(it)

proc verifyTailCalls*(g: ModuleGraph, owner: PSym, body: PNode) =
  # start with no problem and no next expression/statement
  verifyTailCalls(g, owner, body, nil, nil)

# ------------- routine generation -------------

proc genApply*(c: PContext, s: PSym) =
  ## Generates the 'apply' procedure for routine `s`. The apply procedure
  ## adapts `s` to the signature expected by the `Continuation` object.
  ##
  ## Also makes sure that the tuple for storing the parameter doesn't exceed
  ## the maximum allowed space; an error is reported if it does.
  var apply = newSym(s.kind, s.name, nextSymId(c.idgen), s, s.info, nil)
  apply.flags.incl sfInjectDestructors
  apply.flags.incl sfGeneratedOp

  let
    contType    = s.typ.n[0][3].typ # use the hidden type
    tupType     = newParamTuple(c.config, c.idgen, apply, s.typ)
    paramType   = makePtrType(apply, tupType, c.idgen)
    pointerType = c.graph.getSysType(s.info, tyPointer)
    param = newSym(skParam, c.cache.getIdent(":env"), nextSymId(c.idgen),
                   apply, s.info, paramType)
    res   = newSym(skResult, c.cache.getIdent("result"), nextSymId(c.idgen),
                   apply, s.info, contType)

  # check the parameter tuple:
  block:
    let
      size = getSize(c.config, tupType)
      max = getSize(c.config,
        c.graph.systemModuleType(c.cache.getIdent("ParamBlob")))
    if size < 0:
      # some incomplete type; disallowed. Report an error for every
      # problematic parameter
      for i in 0..<tupType.len:
        if getSize(c.config, tupType[i]) < 0:
          c.config.localReport(s.typ.n[i + 1].info,
            reportSym(rsemParameterCannotBeIncomplete, s.typ.n[i + 1].sym))
    elif size > max:
      c.config.localReport(s.info, SemReport(kind: rsemParametersTooLarge))

  # setup the procedure type:
  apply.typ = newProcType(s.info, nextTypeId(c.idgen), s)
  apply.typ.n[0] = s.typ.n[0] # inherit the effects
  apply.typ.n.add newSymNode(param)
  apply.typ[0] = contType
  apply.typ.rawAddSon(paramType, propagateHasAsgn=false)

  var call = newTreeIT(nkCall, s.info, contType, newSymNode(s))
  for i in 1..<s.typ.len:
    let acc = newTreeIT(nkBracketExpr, s.info, tupType[i-1],
      newTreeIT(nkDerefExpr, s.info, paramType.lastSon, newSymNode(param)),
      newIntLit(c.graph, s.info, i-1))
    if s.typ[i].kind == tySink:
      # owning arguments *must* be moved explicitly. The move analyser
      # wouldn't do so automatically, and even if it did, that would still not
      # guarantee a new location being created, which is necessary because the
      # environment's storage may be reused by the called procedure
      call.add newTreeIT(nkCall, s.info, tupType[i - 1],
        newSymNode(createMagic(c.graph, c.idgen, "move", mMove)), acc)
    elif isPassByRef(c.config, s.typ.n[i].sym, s.typ[0]):
      call.add newTreeIT(nkDerefExpr, s.info, tupType[i-1].lastSon, acc)
    elif s.typ[i].skipTypes(abstractInst).kind == tyVar:
      # a deref/addr pair is required
      call.add newTreeIT(nkHiddenAddr, s.info, tupType[i-1],
        newTreeIT(nkHiddenDeref, s.info, tupType[i-1].lastSon,
          acc))
    else:
      call.add acc

  # pass along the type-erased environemnt pointer:
  call.add newTreeIT(nkCast, s.info, pointerType,
    newNodeIT(nkType, s.info, pointerType),
    newSymNode(param))

  apply.ast = newProcNode(nkProcDef, s.info,
    body = newTree(nkAsgn, newSymNode(res), call),
    params = newTree(nkFormalParams, c.graph.emptyNode, newSymNode(param)),
    name = newSymNode(apply),
    c.graph.emptyNode,
    c.graph.emptyNode,
    c.graph.emptyNode,
    c.graph.emptyNode)
  apply.ast.sons.setLen(resultPos + 1)
  apply.ast[resultPos] = newSymNode(res)

  s.ast[miscPos] = newTree(nkBracket,
    newSymNode(apply),
    newNodeIT(nkType, s.info, contType))
