## Implements the coroutine-related transformations. Despite being a separate
## module, the coroutine transformation(s) is heavily intertwined with
## `transf <#transf>`_.

import
  compiler/ast/[
    ast,
    idents,
    types
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/sem/[
    closureiters,
    lambdalifting,
    lowerings
  ],
  compiler/utils/[
    idioms,
  ]

proc computeFieldStart(t: PType, pos: var int) =
  ## Recursively traverses `t` and updates `pos` to the field position
  ## of the first field a type inheriting from `t` would have.
  proc aux(n: PNode, p: var int) =
    case n.kind
    of nkRecCase:
      inc p # discriminator
      for i in 1..<n.len:
        aux(n[i], p)
    of nkRecList:
      for it in n.items:
        aux(it, p)
    of nkSym:
      inc p
    else:
      unreachable()

  let t = t.skipTypes(skipPtrs)
  assert t.kind == tyObject
  if t.len > 0 and t.base != nil:
    computeFieldStart(t.base, pos)

  aux(t.n, pos)

proc preTransformConstr*(g: ModuleGraph, idgen: IdGenerator, prc: PSym, body: PNode): PNode =
  ## Transforms the `body` of coroutine constructor `prc` into:
  ##
  ##   .. code-block:: nim
  ##
  ##     discard (proc inner() {.closure.} =
  ##       body
  ##     )
  ##
  ## Since the owner of the locals within `body` is not changed, the lambda-
  ## lifting pass will lift them into the environment type.
  let cache = g.cache
  let base = g.getCompilerProc("CoroutineBase").typ

  # setup the actual coroutine:
  let inner = newSym(skProc, prc.name, nextSymId idgen, prc, prc.info,
                     prc.options)
  inner.flags.incl sfCoroutine
  inner.ast = newProcNode(nkLambda, prc.info, body,
                          newTree(nkFormalParams, newNodeIT(nkType, prc.info, base)),
                          newSymNode(inner),
                          newNode(nkEmpty),
                          newNode(nkEmpty),
                          newNode(nkEmpty),
                          newNode(nkEmpty))
  inner.typ = newProcType(prc.info, nextTypeId idgen, inner)
  inner.typ[0] = base # return type
  # temporarily mark the procedure as a closure procedure, so that the lambda-
  # lifting pass visits it
  inner.typ.callConv = ccClosure

  # temporarily stash the ``self`` symbol node in the inner procedure's
  # dispatcher slot
  inner.ast.sons.setLen(dispatcherPos + 1)
  inner.ast[dispatcherPos] = move prc.ast[dispatcherPos]

  # result symbol for the coroutine:
  inner.ast[resultPos] = newSymNode:
    newSym(skResult, cache.getIdent("result"), nextSymId idgen, inner,
           inner.info, base)

  # place the instance base type in the constructor's dispatcher
  # slot, for the lambda-lifting pass to later fetch it
  prc.ast[dispatcherPos] = newNodeIT(nkType, prc.info, prc.typ[0])

  # fix the result variable for the constructor:
  prc.ast[resultPos] = newSymNode:
    newSym(skResult, cache.getIdent("result"), nextSymId idgen, prc,
           prc.info, prc.typ[0])

  let body = copyNodeWithKids(inner.ast)
  body.typ = inner.typ
  result = nkDiscardStmt.newTree(body)

proc transformCoroutineConstr*(g: ModuleGraph, idgen: IdGenerator, prc: PSym,
                               body: PNode): PNode =
  ## Post-processes the transformed coroutine instance constructor procedure,
  ## completing the body of the constructor.
  ##
  ## The `body` is expected to have undergone the transf pass, including
  ## lambda-lifting.
  let
    cache = g.cache
    base = g.getCompilerProc("CoroutineBase").typ
    # this is a bit brittle. We rely on the exact positions in the AST
    inner = body.lastSon[0][0].sym
    selfSym = move inner.ast[dispatcherPos]
    res = prc.ast[resultPos].sym

  result = body

  # remove the discard statement injected earlier:
  result.delSon(result.len - 1)

  let
    envLocal = body[0][0][0] # the local injected by lambda-lifting
    constr = body[0][0][2] # the env construction expression

  # patch the environment construction:
  constr.add nkExprColonExpr.newTree(
    newSymNode lookupInType(base, cache.getIdent("fn")),
    newSymNode inner
    )
  # the state needs to be initialized to -4, to signal that the instance is
  # suspended:
  constr.add nkExprColonExpr.newTree(
    newSymNode lookupInType(base, cache.getIdent("state")),
    newIntTypeNode(-4, g.getSysType(prc.info, tyInt32))
  )

  # add the result assignment:
  result.add newAsgnStmt(newSymNode(res),
                        newTreeIT(nkObjDownConv, prc.info, res.typ, envLocal))
  # init the lifted ``self`` symbol:
  if getFieldFromObj(envLocal.typ.base, selfSym.sym) != nil:
    result.add newAsgnStmt(indirectAccess(envLocal, selfSym.sym, selfSym.info),
                           newSymNode res)

proc transformCoroutine*(g: ModuleGraph, idgen: IdGenerator, prc: PSym,
                         body: PNode): PNode =
  ## Given the ``trans``formed and lambda-lifted `body`, applies the
  ## transformation for turning the coroutine `prc` into a resumable procedure
  ## (uses `closureiters <#closureiters>`_ underneath).
  ##
  ## Also takes care of fixing the signature of `prc`.
  let
    base = g.getCompilerProc("CoroutineBase").typ
    body = transformClosureIterator(g, idgen, prc, body)
    param = prc.getEnvParam()

  # replace the hidden parameter with one that has the correct type:
  let newParam = copySym(param, nextSymId idgen)
  newParam.typ = base

  # the original parameter is turned into a cursor local and is injected
  # into the body. Reusing the symbol means that body doesn't need
  # to be patched
  param.kind = skLet
  param.flags.incl sfCursor

  # fix the signature. It needs to be ``CoroutineBase -> CoroutineBase``
  prc.typ.callConv = ccNimCall
  prc.typ.rawAddSon(base) # first parameter type
  prc.typ.n.add newSymNode(newParam)
  # replace the hidden parameter (it's no longer hidden):
  prc.ast[paramsPos][^1] = newSymNode(newParam)

  # inject a definition for the local and emit ``result`` initialization
  result = nkStmtList.newTree(
    nkLetSection.newTree(
      newIdentDefs(newSymNode(param),
                   newTreeIT(nkObjDownConv, body.info, param.typ,
                             newSymNode(newParam)))),
    newAsgnStmt(prc.ast[resultPos], newSymNode(newParam)),
    body
  )

  # the fields in the constructed environment are wrong, they need to be
  # patched
  let obj = param.typ.base
  var start = 0
  computeFieldStart(obj.base, start)
  for it in obj.n.items:
    it.sym.position += start
