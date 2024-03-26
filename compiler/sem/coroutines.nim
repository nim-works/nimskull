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
  ##     proc inner() {.closure.} =
  ##       body
  ##     discard inner
  ##
  ## Since the owner of the locals within `body` is not changed, the lambda-
  ## lifting pass will lift them into the environment type. The
  ## ``discard inner`` makes sure ``inner`` is considerd by lambda-
  ## lifting.
  let cache = g.cache
  let base = g.getCompilerProc("CoroutineBase").typ

  # setup the actual coroutine:
  let inner = newSym(skProc, prc.name, nextSymId idgen, prc, prc.info,
                     prc.options)
  inner.flags.incl sfCoroutine
  inner.ast = newProcNode(nkProcDef, prc.info, body,
                          newTree(nkFormalParams, newNodeIT(nkType, prc.info, base)),
                          newSymNode(inner),
                          newNode(nkEmpty),
                          newNode(nkEmpty),
                          newNode(nkEmpty),
                          newNode(nkEmpty))
  inner.typ = newProcType(prc.info, nextTypeId idgen, inner)
  # temporarily mark the procedure as a closure procedure, so that the lambda-
  # lifting pass visits it
  inner.typ.callConv = ccClosure

  # move the selfSym node to the inner procedure's dispacher slot
  inner.ast.sons.setLen(dispatcherPos + 1)
  inner.ast[dispatcherPos] = move prc.ast[dispatcherPos]

  # place the instance base type is stored in the constructors dispatcher
  # slot, for the lambda-lifting pass to later fetch it
  prc.ast[dispatcherPos] = newNodeIT(nkType, prc.info, prc.typ[0])

  # setup the proper result variable:
  let res = newSym(skResult, cache.getIdent("result"), nextSymId idgen, prc,
                   prc.info)
  res.typ = prc.typ[0]
  prc.ast[resultPos] = newSymNode(res)

  result = nkStmtList.newTree(
    inner.ast,
    nkDiscardStmt.newTree(newSymNode(inner))
  )

proc transformCoroutineConstr*(g: ModuleGraph, idgen: IdGenerator, prc: PSym, body: PNode): PNode =
  ## Post-processes the transformed coroutine instance constructor procedure,
  ## completing the body of the constructor (but not of the actual coroutine).
  ##
  ## This pass has to make sure that the signature of the inner procedure
  ## (i.e., the actual coroutine) is final and valid, since the symbol might
  ## be passed to code generation *before* its body is transformed.
  let
    cache = g.cache
    base = g.getCompilerProc("CoroutineBase").typ
    # this is a bit brittle. We rely on the exact positions in the AST
    stmts = body.lastSon
    inner = stmts[0][namePos].sym
    selfSym = move inner.ast[dispatcherPos]

  # the lambda-lifting pass needs to be run early for the inner procedure,
  # since we need to patch the parameter type afterwards
  var param: PSym
  (inner.ast[bodyPos], param) = liftLambdas(g, inner, inner.ast[bodyPos], idgen)

  # replace the hidden parameter with a correctly typed one:
  let newParam = copySym(param, nextSymId idgen)
  newParam.typ = base

  # the original hidden parameter is turned into a cursor local that's later
  # injected into the body. Reusing the symbols means that body doesn't need
  # to be updated
  param.kind = skLet
  param.flags.incl sfCursor
  # the symbols is stashed in the dispatcher slot
  inner.ast[dispatcherPos] = newSymNode(param)

  # now turn the inner procedure into one with the expected signature:
  inner.typ.callConv = ccNimCall
  inner.typ[0] = base # return type
  inner.typ.rawAddSon(base) # first parameter type
  inner.typ.n.add newSymNode(newParam)
  inner.ast[paramsPos][^1] = newSymNode(newParam) # hidden parameter symbol

  block:
    # update the result variable of the coroutine
    let res = newSym(skResult, cache.getIdent("result"), nextSymId idgen,
                     inner, prc.info)
    res.typ = base
    inner.ast[resultPos] = newSymNode res

  # body patching
  # -------------

  let envLocal = body[0][0][0] # the local injected by lambda-lifting
  result = body
  # replace the placeholder ``nkDiscardStmt`` tree with assigning the
  # environment to the result:
  stmts[1] = newAsgnStmt(newSymNode(res),
                         newTreeIT(nkObjDownConv, prc.info, res.typ, envLocal))
  # set the procedure pointer:
  stmts.add newAsgnStmt(indirectAccess(envLocal, "fn", prc.info, cache),
                        newSymNode inner)
  # the state needs to be initialized to -4, to signal that the instance is
  # suspended:
  stmts.add newAsgnStmt(indirectAccess(envLocal, "state", prc.info, cache),
                        newIntTypeNode(-4, g.getSysType(prc.info, tyInt32)))
  # init the lifted ``self`` symbol:
  if getFieldFromObj(envLocal.typ.base, selfSym.sym) != nil:
    stmts.add newAsgnStmt(indirectAccess(envLocal, selfSym.sym, selfSym.info),
                          newSymNode res)

  # the fields in the constructed environment are wrong, they need to be
  # patched
  let obj = envLocal.typ.base
  var start = 0
  computeFieldStart(obj.base, start)
  for it in obj.n.items:
    it.sym.position += start

proc transformCoroutine*(g: ModuleGraph, idgen: IdGenerator, prc: PSym,
                         body: PNode): PNode =
  ## Applies the actual transformation for turning the coroutine `prc` into
  ## a resumable procedure (uses `closureiters <#closureiters>`_ underneath).
  ##
  ## Additional code for completing the coroutine body is also injected here.
  let
    env = prc.typ.n[1].sym
    body = transformClosureIterator(g, idgen, prc, body)
  let s = prc.ast[dispatcherPos] # the cursor local stashed earlier
  # inject a definition for the local and emit ``result`` initialization
  result = nkStmtList.newTree(
    nkLetSection.newTree(
      newIdentDefs(s, newTreeIT(nkObjDownConv, body.info, env.typ,
                                newSymNode(env)))),
    newAsgnStmt(prc.ast[resultPos], newSymNode(env)),
    body
  )
