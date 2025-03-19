## Implements both the semantic analysis part of tail-call elimination and
## the actual elimination. Refer to this `document <tailcallelim.html>`_ for
## a high-level overview of how tail-call elimination works.
##
## The `apply` and `trampoline` helper procedures are created during semantic
## analysis already, as it potentially requires instantiating generic types.
## The synthesized procedures are attached to their originating ones via the
## misc slot.

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
    semdata,
    semtypinst
  ],
  compiler/front/[
    msgs,
    options
  ]

from compiler/ast/report_enums import ReportKind
from compiler/ast/reports_sem import SemReport, reportStr

type
  Mode = enum
    emDefer
    emTry
    emExpr
    emLast

proc error(config: ConfigRef, info: TLineInfo, msg: string) =
  # TODO: use dedicated reports
  config.localReport(info, reportStr(rsemUserError, msg))

# -------------- analysis --------------

proc checkArg(g: ModuleGraph, owner: PSym, n: PNode, i: int, formal: PType) =
  ## Analyses the `i`-th argument `n` for an call to a procedure with type
  ## `formal`. Reports an error for arguments not adhering to the .musttail
  ## call rules.
  proc isValid(owner, s: PSym): bool =
    (sfGlobal in s.flags) or (s.kind == skParam and s.owner == owner)

  case formal[i].kind
  of tySink:
    discard "all expressions are admitted"
  of tyVar:
    let root = getRoot(n)
    # globals and our own parameters are okay, everything else is not
    if not isValid(owner, root):
      g.config.error(n.info, "argument must borrow from a parameter or global")
  else:
    if isPassByRef(g.config, formal.n[i].sym, formal[0]) or
        hasDestructor(formal[i]):
      # pass-by-value arguments with custom copy behaviour cannot safely be
      # shallow-copied and thus must be borrowed too
      let root = getRoot(n)
      if root.isNil:
        g.config.error(n.info, "argument must be lvalue expression")
      elif not isValid(owner, root):
        g.config.error(n.info, "argument must borrow from a parameter or global")
    else:
      discard "acts like a sink parameter"

proc supportsSiblingCalls(s: PSym): bool =
  # sibling-calling (necessary for tail-call elimination) requires modifying
  # a caller's signature, therefore, as a rule of thumb: every routine whose
  # signature cannot easily and transparently be changed to something else
  # cannot use tail calls
  if s.typ == nil:
    echo s
  if s.typ != nil and s.typ.callConv == ccMusttail:
    # a .musttail routine can always call another .musttail routine
    true
  else:
    s.kind notin {skIterator, skMethod} and
      sfExportc notin s.flags and
      s.typ.callConv == ccNimCall

proc verifyTailCalls(g: ModuleGraph, owner: PSym, n: PNode, mode: set[Mode]) =
  ## Runs the analysis to make sure all .musttail calls are proper tail calls.
  ## Reports an error for every violation. `mode` is used to describe the
  ## current context.
  template recurse(n: PNode, mode: set[Mode]) =
    verifyTailCalls(g, owner, n, mode)

  # note: the `emExpr` mode only needs to be activated only on the
  # statement -> expression edge
  case n.kind
  of nkReturnStmt:
    if n[0].kind == nkAsgn:
      recurse(n[0][1], mode + {emLast} - {emExpr})
  of nkCallKinds:
    for it in n.items:
      recurse(it, mode + {emExpr})

    if n[0].typ.skipTypes(abstractInst).callConv == ccMusttail:
      if not supportsSiblingCalls(owner):
        g.config.error(n.info, "the enclosing routine cannot call .musttail procedures")
      else:
        owner.flags.incl sfCallsMusttail

      if emDefer in mode:
        g.config.error( n.info, "defer prevents tail call")
      elif emTry in mode:
        g.config.error(n.info, "try prevents tail call")
      elif emExpr in mode:
        g.config.error(n.info, "enclosing expression prevents tail call")
      elif emLast notin mode:
        g.config.error(n.info, "trailing statements prevents tail call")
      else:
        # trailing cleanup errors are detected at a later stage
        discard "all good"

      # make sure the arguments have an acceptable shape
      for i in 1..<n.len:
        checkArg(g, owner, n[i], i, n[0].typ.skipTypes(abstractInst))
  of nkStmtList, nkStmtListExpr:
    var tmp = mode - {emLast}
    for i in 0..<n.len-1:
      recurse(n[i], tmp)
      if n[i].kind == nkDefer:
        tmp = tmp + {emDefer}

    if n.len > 0:
      # the defer also applies to the trailing statement
      recurse(n[^1], mode + tmp)
  of nkDotExpr, nkCheckedFieldExpr:
    recurse(n[0], mode)
  of nkBracketExpr:
    recurse(n[0], mode)
    recurse(n[1], mode)
  of nkAddr, nkDerefExpr, nkHiddenAddr, nkCast, nkConv, nkHiddenStdConv,
     nkHiddenSubConv, nkObjDownConv, nkObjUpConv, nkExprColonExpr, nkBlockExpr,
     nkBlockStmt, nkOfBranch, nkExceptBranch, nkPragmaBlock, nkPragmaExpr:
    recurse(n[^1], mode)
  of nkDiscardStmt, nkRaiseStmt:
    recurse(n[0], mode + {emExpr})
  of nkIdentDefs, nkVarTuple:
    recurse(n[0], mode + {emExpr})
  of nkYieldStmt:
    if n.len > 0:
      recurse(n[0], mode + {emExpr})
  of nkAsgn, nkFastAsgn:
    recurse(n[0], mode + {emExpr})
    recurse(n[1], mode + {emExpr})
  of nkTryStmt, nkHiddenTryStmt:
    recurse(n[0], mode + {emTry})
    for i in 1..<n.len:
      recurse(n[i], mode)
  of nkObjConstr:
    for i in 1..<n.len:
      recurse(n[^1], mode)
  of nkElifBranch, nkElifExpr, nkCaseStmt:
    recurse(n[0], mode + {emExpr})
    for i in 1..<n.len:
      recurse(n[i], mode)
  of nkWithoutSons, callableDefs, nkTypeSection, nkConstSection, nkNimNodeLit, nkSymChoices, nkBindStmt, nkMixinStmt:
    # don't enter nested routine declarations
    discard "nothing to do"
  else:
    for it in n.items:
      recurse(it, mode)

proc verifyTailCalls*(g: ModuleGraph, owner: PSym, body: PNode) =
  # TODO: disallow iterators (both inline and closure ones) from tail calling
  verifyTailCalls(g, owner, body, {emLast})

# ------------- routine generation -------------

proc instContinuationType(c: PContext, with: PType, info: TLineInfo): PType =
  ## Instantiates the generic ``system.Continuation`` type with `with`.
  let invoc = newType(tyGenericInvocation, nextTypeId(c.idgen), nil)
  invoc.rawAddSon(c.graph.systemModuleType(c.cache.getIdent("Continuation")))
  invoc.rawAddSon(with)
  var pt: TIdTable
  result = generateTypeInstance(c, pt, info, invoc)

proc genParamContainer(config: ConfigRef, idgen: IdGenerator, owner: PSym,
                       fntype: PType): PType =
  ## Creates a parameter container tuple. The parameter types are taken from
  ## `fntype`.
  result = newType(tyTuple, nextTypeId(idgen), owner)
  for i in 1..<fntype.len:
    let typ = fntype[i]
    if typ.kind == tySink:
      result.rawAddSon(typ.lastSon)
    elif isPassByRef(config, fntype.n[i].sym, fntype[0]):
      result.rawAddSon(makePtrType(owner, typ, idgen))
    else:
      result.rawAddSon(typ)

proc genApply*(c: PContext, s: PSym) =
  ## Generates the 'apply' procedure for routine `s`. The apply procedure
  ## adapts `s` to the signature expected by the `Continuation` object.
  var apply = newSym(s.kind, s.name, nextSymId(c.idgen), s, s.info, nil)
  apply.flags.incl sfInjectDestructors
  apply.flags.incl sfGeneratedOp

  let
    contType    = s.typ.n[0][3].typ # use the hidden type
    tupType     = genParamContainer(c.config, c.idgen, apply, s.typ)
    paramType   = makePtrType(apply, tupType, c.idgen)
    pointerType = c.graph.getSysType(s.info, tyPointer)
    param = newSym(skParam, c.cache.getIdent(":env"), nextSymId(c.idgen),
                   apply, s.info, paramType)
    res   = newSym(skResult, c.cache.getIdent("result"), nextSymId(c.idgen),
                   apply, s.info, contType)

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
      call.add newTreeIT(nkHiddenAddr, s.info, tupType[i-1], newTreeIT(nkHiddenDeref, s.info, tupType[i-1].lastSon, acc))
    else:
      call.add acc

  # pass along the type-erased environemnt pointer:
  call.add newTreeIT(nkConv, s.info, pointerType,
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

proc genTrampoline*(c: PContext, s: PSym) =
  ## Generates and attaches to `s` the trampoline wrapper procedure for `s`.
  ## "trampolining" refers to the process of repeatedly calling continuations
  ## returned by other continuations until a terminal one storing the final
  ## result is reached.
  let prc = newSym(s.kind, s.name, nextSymId(c.idgen), s, s.info, nil)
  prc.flags.incl sfInjectDestructors
  prc.flags.incl sfGeneratedOp

  let
    blobType    = c.graph.systemModuleType(c.cache.getIdent("ParamBlob"))
    boolTyp     = c.graph.getSysType(s.info, tyBool)
    blobPtrType = makePtrType(prc, blobType, c.idgen)
    contType =
      if s.typ[0].isEmptyType():
        c.instContinuationType(c.voidType, s.info)
      else:
        c.instContinuationType(s.typ[0], s.info)

  # create a duplicate of the type and argument list:
  let typ = copyType(s.typ, nextTypeId(c.idgen), prc)
  typ.n = copyTree(typ.n)
  for i in 1..<typ.n.len:
    typ.n[i] = newSymNode(copySym(typ.n[i].sym, nextTypeId(c.idgen)))
    typ.n[i].sym.owner = prc
    # detach any default values to make sure their AST doesn't end up where
    # it shouldn't
    typ.n[i].sym.ast = nil

  prc.typ = typ

  # copy the parameter list and patch the symbols:
  let params = shallowCopy(s.ast[paramsPos])
  var i = 1
  params[0] = s.ast[paramsPos][0]
  for p in 1..<s.ast[paramsPos].len:
    params[p] = copyNodeWithKids(s.ast[paramsPos][p])
    # patch the symbols:
    for j in 0..<(s.ast[paramsPos][p].len - 2):
      params[p][j] = newSymNode(prc.typ.n[i].sym)
      inc i

  # create the body:
  var
    body = newTree(nkStmtList)
    contSym = newSym(skVar, c.cache.getIdent("cont"), nextSymId(c.idgen),
                     prc, s.info, contType)
    envSym = newSym(skVar, c.cache.getIdent(":env"), nextSymId(c.idgen),
                    prc, s.info, blobType)
    resSym = PSym nil

  # the storage is always initialized when needed, no default value is required
  envSym.flags.incl sfNoInit

  # build the call to the actual procedure:
  var call = newTreeIT(nkCall, s.info, contType, newSymNode(s))
  for i in 1..<prc.typ.n.len:
    if prc.typ[i].kind == tyVar:
      # needs a deref + addr pair
      call.add newTreeIT(nkHiddenAddr, s.info, prc.typ[i],
        newTreeIT(nkHiddenDeref, s.info, prc.typ[i].base,
          newSymNode(prc.typ.n[i].sym)))
    else:
      call.add newSymNode(prc.typ.n[i].sym)
  # also pass the address of the env local:
  call.add newTreeIT(nkAddr, s.info, blobPtrType, newSymNode(envSym))

  # emit the trampoline:
  body.add newTree(nkVarSection,
    newIdentDefs(newSymNode(envSym)),
    newIdentDefs(newSymNode(contSym), call))
  body.add newTree(nkWhileStmt,
    newTreeIT(nkCall, s.info, boolTyp,
      newSymNode(c.graph.operators.opNot),
      rawDirectAccess(contSym, lookupInType(contType, 0))),
    newTree(nkAsgn,
      newSymNode(contSym),
      newTreeIT(nkCall, s.info, contType,
        rawDirectAccess(contSym, lookupInType(contType, 1)),
        newTreeIT(nkAddr, s.info, blobPtrType,
          newSymNode(envSym)))))

  if not s.typ[0].isEmptyType():
    # emit the continuation unpacking and result assignment
    resSym = newSym(skResult, c.cache.getIdent("result"), nextSymId(c.idgen),
                    prc, s.info, s.typ[0])
    body.add newTree(nkAsgn,
      newSymNode(resSym),
      rawDirectAccess(contSym, lookupInType(contType, 2)))

  # assemble the final AST:
  prc.ast = newProcNode(s.ast.kind, s.ast.info,
    body = body,
    params = params,
    name = newSymNode(prc),
    c.graph.emptyNode,
    c.graph.emptyNode,
    c.graph.emptyNode,
    c.graph.emptyNode)

  if resSym != nil:
    prc.ast.sons.setLen(resultPos + 1)
    prc.ast[resultPos] = newSymNode(resSym)

  # attach the trampoline and return type to the procedure via its misc slot,
  # for later lookup:
  s.ast[miscPos] = newTree(nkBracket,
    newSymNode(prc),
    newNodeIT(nkType, s.info, contType))
