#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This file implements lambda lifting for the transformator.

import
  std/[
    intsets,
    tables
  ],
  compiler/ast/[
    ast,
    astalgo,
    reports,
    idents,
    renderer,
    types,
    lineinfos
  ],
  compiler/utils/astrepr,
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/sem/[
    typeallowed,
    liftdestructors,
    transf,
    lowerings
  ]

discard """
  The basic approach is that captured vars need to be put on the heap and
  that the calling chain needs to be explicitly modelled. Things to consider:

  proc a =
    var v = 0
    proc b =
      var w = 2

      for x in 0..3:
        proc c = capture v, w, x
        c()
    b()

    for x in 0..4:
      proc d = capture x
      d()

  Needs to be translated into:

  proc a =
    var cl: *
    new cl
    cl.v = 0

    proc b(cl) =
      var bcl: *
      new bcl
      bcl.w = 2
      bcl.up = cl

      for x in 0..3:
        var bcl2: *
        new bcl2
        bcl2.up = bcl
        bcl2.up2 = cl
        bcl2.x = x

        proc c(cl) = capture cl.up2.v, cl.up.w, cl.x
        c(bcl2)

      c(bcl)

    b(cl)

    for x in 0..4:
      var acl2: *
      new acl2
      acl2.x = x
      proc d(cl) = capture cl.x
      d(acl2)

  Closures as interfaces:

  proc outer: T =
    var captureMe: TObject # value type required for efficiency
    proc getter(): int = result = captureMe.x
    proc setter(x: int) = captureMe.x = x

    result = (getter, setter)

  Is translated to:

  proc outer: T =
    var cl: *
    new cl

    proc getter(cl): int = result = cl.captureMe.x
    proc setter(cl: *, x: int) = cl.captureMe.x = x

    result = ((cl, getter), (cl, setter))


  For 'byref' capture, the outer proc needs to access the captured var through
  the indirection too. For 'bycopy' capture, the outer proc accesses the var
  not through the indirection.

  Possible optimizations:

  1) If the closure contains a single 'ref' and this
  reference is not re-assigned (check ``sfAddrTaken`` flag) make this the
  closure. This is an important optimization if closures are used as
  interfaces.
  2) If the closure does not escape, put it onto the stack, not on the heap.
  3) Dataflow analysis would help to eliminate the 'up' indirections.
  4) If the captured var is not actually used in the outer proc (common?),
  put it into an inner proc.

"""

# Important things to keep in mind:
# * Don't base the analysis on nkProcDef et al. This doesn't work for
#   instantiated (formerly generic) procs. The analysis has to look at nkSym.
#   This also means we need to prevent the same proc is processed multiple
#   times via the 'processed' set.
# * Keep in mind that the owner of some temporaries used to be unreliable.
# * For closure iterators we merge the "real" potential closure with the
#   local storage requirements for efficiency. This means closure iterators
#   have slightly different semantics from ordinary closures.

# ---------------- essential helpers -------------------------------------

const
  upName* = ":up" # field name for the 'up' reference
  paramName* = ":envP"
  envName* = ":env"

proc newCall(a: PSym, b: PNode): PNode =
  result = newNodeI(nkCall, a.info)
  result.add newSymNode(a)
  result.add b

proc createClosureIterStateType*(g: ModuleGraph; iter: PSym; idgen: IdGenerator): PType =
  var n = newNodeI(nkRange, iter.info)
  n.add newIntNode(nkIntLit, -1)
  n.add newIntNode(nkIntLit, 0)
  result = newType(tyRange, nextTypeId(idgen), iter)
  result.n = n
  var intType = nilOrSysInt(g)
  if intType.isNil: intType = newType(tyInt, nextTypeId(idgen), iter)
  rawAddSon(result, intType)

proc createStateField(g: ModuleGraph; iter: PSym; idgen: IdGenerator): PSym =
  result = newSym(skField, getIdent(g.cache, ":state"), nextSymId(idgen), iter, iter.info)
  result.typ = createClosureIterStateType(g, iter, idgen)

proc createEnvObj(g: ModuleGraph; idgen: IdGenerator; owner: PSym; info: TLineInfo): PType =
  # YYY meh, just add the state field for every closure for now, it's too
  # hard to figure out if it comes from a closure iterator:
  result = createObj(g, idgen, owner, info, final=false)
  rawAddField(result, createStateField(g, owner, idgen))

proc getClosureIterResult*(g: ModuleGraph; iter: PSym; idgen: IdGenerator): PSym =
  if resultPos < iter.ast.len:
    result = iter.ast[resultPos].sym
  else:
    # XXX a bit hacky:
    result = newSym(skResult, getIdent(g.cache, ":result"), nextSymId(idgen), iter, iter.info, {})
    result.typ = iter.typ[0]
    incl(result.flags, sfUsed)
    iter.ast.add newSymNode(result)

proc addHiddenParam(routine: PSym, param: PSym) =
  assert param.kind == skParam
  var params = routine.ast[paramsPos]
  # -1 is correct here as param.position is 0 based but we have at position 0
  # some nkEffect node:
  param.position = routine.typ.n.len-1
  params.add newSymNode(param)
  #incl(routine.typ.flags, tfCapturesEnv)
  assert sfFromGeneric in param.flags
  #echo "produced environment: ", param.id, " for ", routine.id

proc getHiddenParam(g: ModuleGraph; routine: PSym): PSym =
  let params = routine.ast[paramsPos]
  let hidden = lastSon(params)

  g.config.internalAssert(
    hidden.kind == nkSym and hidden.sym.kind == skParam and hidden.sym.name.s == paramName,
    routine.info, "internal error: could not find env param for " & routine.name.s)

  result = hidden.sym
  assert sfFromGeneric in result.flags

proc getEnvParam*(routine: PSym): PSym =
  let params = routine.ast[paramsPos]
  let hidden = lastSon(params)
  if hidden.kind == nkSym and hidden.sym.name.s == paramName:
    result = hidden.sym
    assert sfFromGeneric in result.flags

proc interestingVar(s: PSym): bool {.inline.} =
  result = s.kind in {skVar, skLet, skTemp, skForVar, skParam, skResult} and
    sfGlobal notin s.flags and
    s.typ.kind notin {tyStatic, tyTypeDesc}

proc illegalCapture(s: PSym): bool {.inline.} =
  result = classifyViewType(s.typ) != noView or s.kind == skResult

proc isInnerProc(s: PSym): bool =
  if s.kind in {skProc, skFunc, skMethod, skConverter, skIterator} and s.magic == mNone:
    result = s.skipGenericOwner.kind in routineKinds

proc newAsgnStmt(le, ri: PNode, info: TLineInfo): PNode =
  # Bugfix: unfortunately we cannot use 'nkFastAsgn' here as that would
  # mean to be able to capture string literals which have no GC header.
  # However this can only happen if the capture happens through a parameter,
  # which is however the only case when we generate an assignment in the first
  # place.
  result = newNodeI(nkAsgn, info, 2)
  result[0] = le
  result[1] = ri

proc makeClosure*(g: ModuleGraph; idgen: IdGenerator; prc: PSym; env: PNode; info: TLineInfo): PNode =
  result = newNodeIT(nkClosure, info, prc.typ)
  result.add(newSymNode(prc))
  if env == nil:
    result.add(newNodeIT(nkNilLit, info, getSysType(g, info, tyNil)))
  else:
    g.config.internalAssert(env.skipConv.kind != nkClosure, info, "taking closure of closure")
    result.add(env)
  #if isClosureIterator(result.typ):
  createTypeBoundOps(g, nil, result.typ, info, idgen)
  if tfHasAsgn in result.typ.flags or optSeqDestructors in g.config.globalOptions:
    prc.flags.incl sfInjectDestructors

proc interestingIterVar(s: PSym): bool {.inline.} =
  # XXX optimization: Only lift the variable if it lives across
  # yield/return boundaries! This can potentially speed up
  # closure iterators quite a bit.
  result = s.kind in {skResult, skVar, skLet, skTemp, skForVar} and sfGlobal notin s.flags

template isIterator*(owner: PSym): bool =
  owner.kind == skIterator and owner.typ.callConv == ccClosure

proc liftingHarmful(conf: ConfigRef; owner: PSym): bool {.inline.} =
  ## lambda lifting can be harmful for JS-like code generators.
  let isCompileTime = sfCompileTime in owner.flags or owner.kind == skMacro
  result = conf.backend == backendJs and not isCompileTime

proc createTypeBoundOpsLL(g: ModuleGraph; refType: PType; info: TLineInfo; idgen: IdGenerator; owner: PSym) =
  if owner.kind != skMacro:
    createTypeBoundOps(g, nil, refType.lastSon, info, idgen)
    createTypeBoundOps(g, nil, refType, info, idgen)
    if tfHasAsgn in refType.flags or optSeqDestructors in g.config.globalOptions:
      owner.flags.incl sfInjectDestructors

proc liftIterSym*(g: ModuleGraph; n: PNode; idgen: IdGenerator; owner: PSym): PNode =
  # transforms  (iter)  to  (let env = newClosure[iter](); (iter, env))
  if liftingHarmful(g.config, owner): return n
  let iter = n.sym
  assert iter.isIterator

  result = newNodeIT(nkStmtListExpr, n.info, n.typ)

  let hp = getHiddenParam(g, iter)
  var env: PNode
  if owner.isIterator:
    let it = getHiddenParam(g, owner)
    addUniqueField(it.typ.skipTypes({tyOwned})[0], hp, g.cache, idgen)
    env = indirectAccess(newSymNode(it), hp, hp.info)
  else:
    let e = newSym(skLet, iter.name, nextSymId(idgen), owner, n.info)
    e.typ = hp.typ
    e.flags = hp.flags
    env = newSymNode(e)
    let v = newTreeI(nkVarSection, n.info):
      newIdentDefs(env)
    result.add(v)
  # add 'new' statement:
  result.add newCall(getSysSym(g, n.info, "internalNew"), env)
  createTypeBoundOpsLL(g, env.typ, n.info, idgen, owner)
  result.add makeClosure(g, idgen, iter, env, n.info)

proc freshVarForClosureIter*(g: ModuleGraph; s: PSym; idgen: IdGenerator; owner: PSym): PNode =
  let envParam = getHiddenParam(g, owner)
  let obj = envParam.typ.skipTypes({tyOwned, tyRef, tyPtr})
  addField(obj, s, g.cache, idgen)

  var access = newSymNode(envParam)
  assert obj.kind == tyObject
  let field = getFieldFromObj(obj, s)
  g.config.internalAssert(field != nil, s.info, "internal error: cannot generate fresh variable")
  result = rawIndirectAccess(access, field, s.info)

# ------------------ new stuff -------------------------------------------

proc markAsClosure(g: ModuleGraph; owner: PSym; n: PNode) =
  let s = n.sym
  if illegalCapture(s):
    localReport(g.config, n.info, reportSymbols(
      rsemIllegalMemoryCapture, @[s, owner]))

  elif not (
    owner.typ.callConv == ccClosure or
    owner.typ.callConv == ccNimCall and
    tfExplicitCallConv notin owner.typ.flags
  ):
    localReport(g.config, n.info, reportSymbols(
      rsemIllegalCallconvCapture, @[s, owner]))

  incl(owner.typ.flags, tfCapturesEnv)
  owner.typ.callConv = ccClosure

type
  DetectionPass = object
    processed, capturedVars: IntSet
    ownerToType: Table[int, PType]
    somethingToDo: bool
    graph: ModuleGraph
    idgen: IdGenerator

proc initDetectionPass(g: ModuleGraph; fn: PSym; idgen: IdGenerator): DetectionPass =
  result.processed = initIntSet()
  result.capturedVars = initIntSet()
  result.ownerToType = initTable[int, PType]()
  result.processed.incl(fn.id)
  result.graph = g
  result.idgen = idgen

discard """
proc outer =
  var a, b: int
  proc innerA = use(a)
  proc innerB = use(b); innerA()
# --> innerA and innerB need to *share* the closure type!
This is why need to store the 'ownerToType' table and use it
during .closure'fication.
"""

proc getEnvTypeForOwner(c: var DetectionPass; owner: PSym;
                        info: TLineInfo): PType =
  result = c.ownerToType.getOrDefault(owner.id)
  if result.isNil:
    result = newType(tyRef, nextTypeId(c.idgen), owner)
    let obj = createEnvObj(c.graph, c.idgen, owner, info)
    rawAddSon(result, obj)
    c.ownerToType[owner.id] = result

proc asOwnedRef(c: var DetectionPass; t: PType): PType =
  if optOwnedRefs in c.graph.config.globalOptions:
    assert t.kind == tyRef
    result = newType(tyOwned, nextTypeId(c.idgen), t.owner)
    result.flags.incl tfHasOwned
    result.rawAddSon t
  else:
    result = t

proc getEnvTypeForOwnerUp(c: var DetectionPass; owner: PSym;
                          info: TLineInfo): PType =
  var r = c.getEnvTypeForOwner(owner, info)
  result = newType(tyPtr, nextTypeId(c.idgen), owner)
  rawAddSon(result, r.skipTypes({tyOwned, tyRef, tyPtr}))

proc createUpField(c: var DetectionPass; dest, dep: PSym; info: TLineInfo) =
  let refObj = c.getEnvTypeForOwner(dest, info) # getHiddenParam(dest).typ
  let obj = refObj.skipTypes({tyOwned, tyRef, tyPtr})
  # The assumption here is that gcDestructors means we cannot deal
  # with cycles properly, so it's better to produce a weak ref (=ptr) here.
  # This seems to be generally correct but since it's a bit risky it's disabled
  # for now.
  # XXX This is wrong for the 'hamming' test, so remove this logic again.
  let fieldType = if isDefined(c.graph.config, "nimCycleBreaker"):
                    c.getEnvTypeForOwnerUp(dep, info) #getHiddenParam(dep).typ
                  else:
                    c.getEnvTypeForOwner(dep, info)
  c.graph.config.internalAssert(refObj != fieldType, dep.info):
    "internal error: invalid up reference computed"

  let upIdent = getIdent(c.graph.cache, upName)
  let upField = lookupInRecord(obj.n, upIdent)
  if upField != nil:
    c.graph.config.internalAssert(
      upField.typ.skipTypes({tyOwned, tyRef, tyPtr}) == fieldType.skipTypes({tyOwned, tyRef, tyPtr}),
      dep.info, "internal error: up references do not agree")

    when false:
      if c.graph.config.selectedGC == gcDestructors and sfCursor notin upField.flags:
        localUnreachable(c.graph.config, dep.info, "internal error: up reference is not a .cursor")
  else:
    let result = newSym(skField, upIdent, nextSymId(c.idgen), obj.owner, obj.owner.info)
    result.typ = fieldType
    when false:
      if c.graph.config.selectedGC == gcDestructors:
        result.flags.incl sfCursor
    rawAddField(obj, result)

discard """
There are a couple of possibilities of how to implement closure
iterators that capture outer variables in a traditional sense
(aka closure closure iterators).

1. Transform iter() to  iter(state, capturedEnv). So use 2 hidden
   parameters.
2. Add the captured vars directly to 'state'.
3. Make capturedEnv an up-reference of 'state'.

We do (3) here because (2) is obviously wrong and (1) is wrong too.
Consider:

  proc outer =
    var xx = 9

    iterator foo() =
      var someState = 3

      proc bar = echo someState
      proc baz = someState = 0
      baz()
      bar()

"""

proc addClosureParam(c: var DetectionPass; fn: PSym; info: TLineInfo) =
  var cp = getEnvParam(fn)
  let owner = if fn.kind == skIterator: fn else: fn.skipGenericOwner
  let t = c.getEnvTypeForOwner(owner, info)
  if cp == nil:
    cp = newSym(skParam, getIdent(c.graph.cache, paramName), nextSymId(c.idgen), fn, fn.info)
    incl(cp.flags, sfFromGeneric)
    cp.typ = t
    addHiddenParam(fn, cp)
  elif cp.typ != t and fn.kind != skIterator:
    internalError(c.graph.config, fn.info, "internal error: inconsistent environment type")
  #echo "adding closure to ", fn.name.s

proc detectCapturedVars(n: PNode; owner: PSym; c: var DetectionPass) =
  case n.kind
  of nkSym:
    let s = n.sym
    if s.kind in {skProc, skFunc, skMethod, skConverter, skIterator} and
        s.typ != nil and s.typ.callConv == ccClosure:
      # this handles the case that the inner proc was declared as
      # .closure but does not actually capture anything:
      addClosureParam(c, s, n.info)
      c.somethingToDo = true

    let innerProc = isInnerProc(s)
    if innerProc:
      if s.isIterator: c.somethingToDo = true
      if not c.processed.containsOrIncl(s.id):
        let body = transformBody(c.graph, c.idgen, s, cache = true)
        detectCapturedVars(body, s, c)
    let ow = s.skipGenericOwner
    if ow == owner:
      if owner.isIterator:
        c.somethingToDo = true
        addClosureParam(c, owner, n.info)
        if interestingIterVar(s):
          if not c.capturedVars.containsOrIncl(s.id):
            let obj = getHiddenParam(c.graph, owner).typ.skipTypes({tyOwned, tyRef, tyPtr})
            #let obj = c.getEnvTypeForOwner(s.owner).skipTypes({tyOwned, tyRef, tyPtr})

            if s.name.id == getIdent(c.graph.cache, ":state").id:
              obj.n[0].sym.itemId = ItemId(module: s.itemId.module, item: -s.itemId.item)
            else:
              addField(obj, s, c.graph.cache, c.idgen)
    # direct or indirect dependency:
    elif (innerProc and s.typ.callConv == ccClosure) or interestingVar(s):
      discard """
        proc outer() =
          var x: int
          proc inner() =
            proc innerInner() =
              echo x
            innerInner()
          inner()
        # inner() takes a closure too!
      """
      # mark 'owner' as taking a closure:
      c.somethingToDo = true
      markAsClosure(c.graph, owner, n)
      addClosureParam(c, owner, n.info)
      #echo "capturing ", n.info
      # variable 's' is actually captured:
      if interestingVar(s) and not c.capturedVars.containsOrIncl(s.id):
        let obj = c.getEnvTypeForOwner(ow, n.info).skipTypes({tyOwned, tyRef, tyPtr})
        #getHiddenParam(owner).typ.skipTypes({tyOwned, tyRef, tyPtr})
        addField(obj, s, c.graph.cache, c.idgen)
      # create required upFields:
      var w = owner.skipGenericOwner
      if isInnerProc(w) or owner.isIterator:
        if owner.isIterator: w = owner
        let last = if ow.isIterator: ow.skipGenericOwner else: ow
        while w != nil and w.kind != skModule and last != w:
          discard """
          proc outer =
            var a, b: int
            proc outerB =
              proc innerA = use(a)
              proc innerB = use(b); innerA()
          # --> make outerB of calling convention .closure and
          # give it the same env type that outer's env var gets:
          """
          let up = w.skipGenericOwner
          #echo "up for ", w.name.s, " up ", up.name.s
          markAsClosure(c.graph, w, n)
          addClosureParam(c, w, n.info) # , ow
          createUpField(c, w, up, n.info)
          w = up
  of nkEmpty..pred(nkSym), succ(nkSym)..nkNilLit,
     nkTemplateDef, nkTypeSection, nkProcDef, nkMethodDef,
     nkConverterDef, nkMacroDef, nkFuncDef, nkCommentStmt,
     nkTypeOfExpr, nkMixinStmt, nkBindStmt:
    discard
  of nkLambdaKinds, nkIteratorDef:
    if n.typ != nil:
      detectCapturedVars(n[namePos], owner, c)
  of nkReturnStmt:
    detectCapturedVars(n[0], owner, c)
  # of nkError:
  #   echo "error node id: ", n.id, " info: ", c.graph.config $ n.info, 
  #        "\n  inst: ", c.graph.config.getReport(n.reportId).reportInst,
  #        "\n  from: ", c.graph.config.getReport(n.reportId).reportFrom
  else:
    for i in 0..<n.len:
      detectCapturedVars(n[i], owner, c)

type
  LiftingPass = object
    processed: IntSet
    envVars: Table[int, PNode]
    inContainer: int
    unownedEnvVars: Table[int, PNode] # only required for --newruntime

proc initLiftingPass(fn: PSym): LiftingPass =
  result.processed = initIntSet()
  result.processed.incl(fn.id)
  result.envVars = initTable[int, PNode]()

proc accessViaEnvParam(g: ModuleGraph; n: PNode; owner: PSym): PNode =
  let s = n.sym
  # Type based expression construction for simplicity:
  let envParam = getHiddenParam(g, owner)
  g.config.internalAssert(not envParam.isNil, n.info, "internal error: environment misses: " & s.name.s)
  var access = newSymNode(envParam)
  while true:
    let obj = access.typ[0]
    assert obj.kind == tyObject
    let field = getFieldFromObj(obj, s)
    if field != nil:
      return rawIndirectAccess(access, field, n.info)
    let upField = lookupInRecord(obj.n, getIdent(g.cache, upName))
    g.config.internalAssert(upField != nil, n.info, "internal error: environment misses: " & s.name.s)
    access = rawIndirectAccess(access, upField, n.info)

proc newEnvVar(cache: IdentCache; owner: PSym; typ: PType; info: TLineInfo; idgen: IdGenerator): PNode =
  var v = newSym(skVar, getIdent(cache, envName), nextSymId(idgen), owner, info)
  v.flags = {sfShadowed, sfGeneratedOp}
  v.typ = typ
  result = newSymNode(v)
  when false:
    if owner.kind == skIterator and owner.typ.callConv == ccClosure:
      let it = getHiddenParam(owner)
      addUniqueField(it.typ[0], v)
      result = indirectAccess(newSymNode(it), v, v.info)
    else:
      result = newSymNode(v)

proc setupEnvVar(owner: PSym; d: var DetectionPass;
                 c: var LiftingPass; info: TLineInfo): PNode =
  if owner.isIterator:
    return getHiddenParam(d.graph, owner).newSymNode
  result = c.envVars.getOrDefault(owner.id)
  if result.isNil:
    let envVarType = d.ownerToType.getOrDefault(owner.id)
    d.graph.config.internalAssert(envVarType != nil, owner.info, "internal error: could not determine closure type")
    result = newEnvVar(d.graph.cache, owner, asOwnedRef(d, envVarType), info, d.idgen)
    c.envVars[owner.id] = result
    if optOwnedRefs in d.graph.config.globalOptions:
      var v = newSym(skVar, getIdent(d.graph.cache, envName & "Alt"), nextSymId d.idgen, owner, info)
      v.flags = {sfShadowed, sfGeneratedOp}
      v.typ = envVarType
      c.unownedEnvVars[owner.id] = newSymNode(v)

proc getUpViaParam(g: ModuleGraph; owner: PSym): PNode =
  let p = getHiddenParam(g, owner)
  result = p.newSymNode
  if owner.isIterator:
    let upField = lookupInRecord(p.typ.skipTypes({tyOwned, tyRef, tyPtr}).n, getIdent(g.cache, upName))
    g.config.internalAssert(upField != nil, owner.info, "could not find up reference for closure iter")
    result = rawIndirectAccess(result, upField, p.info)

proc rawClosureCreation(owner: PSym;
                        d: var DetectionPass; c: var LiftingPass;
                        info: TLineInfo): PNode =
  result = newNodeI(nkStmtList, owner.info)

  var env: PNode
  if owner.isIterator:
    env = getHiddenParam(d.graph, owner).newSymNode
  else:
    env = setupEnvVar(owner, d, c, info)
    if env.kind == nkSym:
      let v = newTreeI(nkVarSection, env.info):
        newIdentDefs(env)
      result.add(v)
      if optOwnedRefs in d.graph.config.globalOptions:
        let unowned = c.unownedEnvVars[owner.id]
        assert unowned != nil
        v.add newIdentDefs(unowned)

    # add 'new' statement:
    result.add(newCall(getSysSym(d.graph, env.info, "internalNew"), env))
    if optOwnedRefs in d.graph.config.globalOptions:
      let unowned = c.unownedEnvVars[owner.id]
      assert unowned != nil
      let env2 = copyTree(env)
      env2.typ = unowned.typ
      result.add newAsgnStmt(unowned, env2, env.info)
      createTypeBoundOpsLL(d.graph, unowned.typ, env.info, d.idgen, owner)

    # add assignment statements for captured parameters:
    for i in 1..<owner.typ.n.len:
      let local = owner.typ.n[i].sym
      if local.id in d.capturedVars:
        let fieldAccess = indirectAccess(env, local, env.info)
        # add ``env.param = param``
        result.add(newAsgnStmt(fieldAccess, newSymNode(local), env.info))
        if owner.kind != skMacro:
          createTypeBoundOps(d.graph, nil, fieldAccess.typ, env.info, d.idgen)
        if tfHasAsgn in fieldAccess.typ.flags or optSeqDestructors in d.graph.config.globalOptions:
          owner.flags.incl sfInjectDestructors

  let upField = lookupInRecord(
    env.typ.skipTypes({tyOwned, tyRef, tyPtr}).n, getIdent(d.graph.cache, upName))

  if upField != nil:
    let up = getUpViaParam(d.graph, owner)
    d.graph.config.internalAssert(
      up != nil and upField.typ.skipTypes({tyOwned, tyRef, tyPtr}) == up.typ.skipTypes({tyOwned, tyRef, tyPtr}),
      env.info, "internal error: cannot create up reference")

    result.add(newAsgnStmt(rawIndirectAccess(env, upField, env.info), up, env.info))
  # we are not in the sem'check phase anymore! so pass 'nil' for the PContext
  # and hope for the best:
  createTypeBoundOpsLL(d.graph, env.typ, owner.info, d.idgen, owner)

proc finishClosureCreation(owner: PSym; d: var DetectionPass; c: LiftingPass;
                           info: TLineInfo; res: PNode) =
  if optOwnedRefs in d.graph.config.globalOptions:
    let unowned = c.unownedEnvVars[owner.id]
    assert unowned != nil
    let nilLit = newNodeIT(nkNilLit, info, unowned.typ)
    res.add newAsgnStmt(unowned, nilLit, info)
    createTypeBoundOpsLL(d.graph, unowned.typ, info, d.idgen, owner)

proc closureCreationForIter(iter: PNode;
                            d: var DetectionPass; c: var LiftingPass): PNode =
  result = newNodeIT(nkStmtListExpr, iter.info, iter.sym.typ)
  let owner = iter.sym.skipGenericOwner
  var v = newSym(skVar, getIdent(d.graph.cache, envName), nextSymId(d.idgen), owner, iter.info)
  incl(v.flags, sfShadowed)
  v.typ = asOwnedRef(d, getHiddenParam(d.graph, iter.sym).typ)
  var vnode: PNode
  if owner.isIterator:
    let it = getHiddenParam(d.graph, owner)
    addUniqueField(it.typ.skipTypes({tyOwned, tyRef, tyPtr}), v, d.graph.cache, d.idgen)
    vnode = indirectAccess(newSymNode(it), v, v.info)
  else:
    vnode = v.newSymNode
    let vs = newTreeI(nkVarSection, iter.info):
      newIdentDefs(vnode)
    result.add(vs)
  result.add(newCall(getSysSym(d.graph, iter.info, "internalNew"), vnode))
  createTypeBoundOpsLL(d.graph, vnode.typ, iter.info, d.idgen, owner)

  let upField = lookupInRecord(v.typ.skipTypes({tyOwned, tyRef, tyPtr}).n, getIdent(d.graph.cache, upName))
  if upField != nil:
    let u = setupEnvVar(owner, d, c, iter.info)

    d.graph.config.internalAssert(
      u.typ.skipTypes({tyOwned, tyRef, tyPtr}) == upField.typ.skipTypes({tyOwned, tyRef, tyPtr}),
      iter.info, "internal error: cannot create up reference for iter")

    result.add(newAsgnStmt(rawIndirectAccess(vnode, upField, iter.info), u, iter.info))
  result.add makeClosure(d.graph, d.idgen, iter.sym, vnode, iter.info)

proc accessViaEnvVar(n: PNode; owner: PSym; d: var DetectionPass;
                     c: var LiftingPass): PNode =
  var access = setupEnvVar(owner, d, c, n.info)
  if optOwnedRefs in d.graph.config.globalOptions:
    access = c.unownedEnvVars[owner.id]
  let obj = access.typ.skipTypes({tyOwned, tyRef, tyPtr})
  let field = getFieldFromObj(obj, n.sym)
  d.graph.config.internalAssert(field != nil, n.info, "internal error: not part of closure object type")
  result = rawIndirectAccess(access, field, n.info)

proc getStateField*(g: ModuleGraph; owner: PSym): PSym =
  getHiddenParam(g, owner).typ.skipTypes({tyOwned, tyRef, tyPtr}).n[0].sym

proc liftCapturedVars(n: PNode; owner: PSym; d: var DetectionPass;
                      c: var LiftingPass): PNode

proc symToClosure(n: PNode; owner: PSym; d: var DetectionPass;
                  c: var LiftingPass): PNode =
  let s = n.sym
  if s == owner:
    # recursive calls go through (lambda, hiddenParam):
    let available = getHiddenParam(d.graph, owner)
    result = makeClosure(d.graph, d.idgen, s, available.newSymNode, n.info)
  elif s.isIterator:
    result = closureCreationForIter(n, d, c)
  elif s.skipGenericOwner == owner:
    # direct dependency, so use the outer's env variable:
    result = makeClosure(d.graph, d.idgen, s, setupEnvVar(owner, d, c, n.info), n.info)
  else:
    let available = getHiddenParam(d.graph, owner)
    let wanted = getHiddenParam(d.graph, s).typ
    # ugh: call through some other inner proc;
    var access = newSymNode(available)
    while true:
      if access.typ == wanted:
        return makeClosure(d.graph, d.idgen, s, access, n.info)
      let obj = access.typ.skipTypes({tyOwned, tyRef, tyPtr})
      let upField = lookupInRecord(obj.n, getIdent(d.graph.cache, upName))
      d.graph.config.internalAssert(upField != nil, n.info, "internal error: no environment found")
      access = rawIndirectAccess(access, upField, n.info)

proc liftCapturedVars(n: PNode; owner: PSym; d: var DetectionPass;
                      c: var LiftingPass): PNode =
  result = n
  case n.kind
  of nkSym:
    let s = n.sym
    if isInnerProc(s):
      if not c.processed.containsOrIncl(s.id):
        #if s.name.s == "temp":
        #  echo renderTree(s.getBody, {renderIds})
        let oldInContainer = c.inContainer
        c.inContainer = 0
        var body = transformBody(d.graph, d.idgen, s, cache = false)
        body = liftCapturedVars(body, s, d, c)
        if c.envVars.getOrDefault(s.id).isNil:
          s.transformedBody = body
        else:
          s.transformedBody = newTree(nkStmtList, rawClosureCreation(s, d, c, n.info), body)
          finishClosureCreation(s, d, c, n.info, s.transformedBody)
        c.inContainer = oldInContainer

      if s.typ.callConv == ccClosure:
        result = symToClosure(n, owner, d, c)

    elif s.id in d.capturedVars:
      if s.owner != owner:
        result = accessViaEnvParam(d.graph, n, owner)
      elif owner.isIterator and interestingIterVar(s):
        result = accessViaEnvParam(d.graph, n, owner)
      else:
        result = accessViaEnvVar(n, owner, d, c)
  of nkEmpty..pred(nkSym), succ(nkSym)..nkNilLit,
     nkTemplateDef, nkTypeSection, nkProcDef, nkMethodDef, nkConverterDef,
     nkMacroDef, nkFuncDef, nkMixinStmt, nkBindStmt:
    discard
  of nkClosure:
    if n[1].kind == nkNilLit:
      n[0] = liftCapturedVars(n[0], owner, d, c)
      let x = n[0].skipConv
      if x.kind == nkClosure:
        #localReport(n.info, "internal error: closure to closure created")
        # now we know better, so patch it:
        n[0] = x[0]
        n[1] = x[1]
  of nkLambdaKinds, nkIteratorDef:
    if n.typ != nil and n[namePos].kind == nkSym:
      let oldInContainer = c.inContainer
      c.inContainer = 0
      let m = newSymNode(n[namePos].sym)
      m.typ = n.typ
      result = liftCapturedVars(m, owner, d, c)
      c.inContainer = oldInContainer
  of nkHiddenStdConv:
    if n.len == 2:
      n[1] = liftCapturedVars(n[1], owner, d, c)
      if n[1].kind == nkClosure: result = n[1]
  of nkReturnStmt:
    if n[0].kind in {nkAsgn, nkFastAsgn}:
      # we have a `result = result` expression produced by the closure
      # transform, let's not touch the LHS in order to make the lifting pass
      # correct when `result` is lifted
      n[0][1] = liftCapturedVars(n[0][1], owner, d, c)
    else:
      n[0] = liftCapturedVars(n[0], owner, d, c)
  of nkTypeOfExpr:
    result = n
  else:
    if owner.isIterator:
      if nfLL in n.flags:
        # special case 'when nimVm' due to bug #3636:
        n[1] = liftCapturedVars(n[1], owner, d, c)
        return

    let inContainer = n.kind in {nkObjConstr, nkBracket}
    if inContainer: inc c.inContainer
    for i in 0..<n.len:
      n[i] = liftCapturedVars(n[i], owner, d, c)
    if inContainer: dec c.inContainer

# ------------------ old stuff -------------------------------------------

proc semCaptureSym*(s, owner: PSym) =
  discard """
    proc outer() =
      var x: int
      proc inner() =
        proc innerInner() =
          echo x
        innerInner()
      inner()
    # inner() takes a closure too!
  """
  proc propagateClosure(start, last: PSym) =
    var o = start
    while o != nil and o.kind != skModule:
      if o == last: break
      o.typ.callConv = ccClosure
      o = o.skipGenericOwner

  if interestingVar(s) and s.kind != skResult:
    if owner.typ != nil and not isGenericRoutine(owner):
      # XXX: is this really safe?
      # if we capture a var from another generic routine,
      # it won't be consider captured.
      var o = owner.skipGenericOwner
      while o != nil and o.kind != skModule:
        if s.owner == o:
          if owner.typ.callConv == ccClosure or owner.kind == skIterator or
             owner.typ.callConv == ccNimCall and tfExplicitCallConv notin owner.typ.flags:
            owner.typ.callConv = ccClosure
            propagateClosure(owner.skipGenericOwner, s.owner)
          else:
            discard "do not produce an error here, but later"
          #echo "computing .closure for ", owner.name.s, " because of ", s.name.s
        o = o.skipGenericOwner
    # since the analysis is not entirely correct, we don't set 'tfCapturesEnv'
    # here

proc liftIterToProc*(g: ModuleGraph; fn: PSym; body: PNode; ptrType: PType;
                     idgen: IdGenerator): PNode =
  var d = initDetectionPass(g, fn, idgen)
  var c = initLiftingPass(fn)
  # pretend 'fn' is a closure iterator for the analysis:
  let oldKind = fn.kind
  let oldCC = fn.typ.callConv
  fn.transitionRoutineSymKind(skIterator)
  fn.typ.callConv = ccClosure
  d.ownerToType[fn.id] = ptrType
  detectCapturedVars(body, fn, d)
  result = liftCapturedVars(body, fn, d, c)
  fn.transitionRoutineSymKind(oldKind)
  fn.typ.callConv = oldCC

proc liftLambdas*(g: ModuleGraph; fn: PSym, body: PNode; tooEarly: var bool;
                  idgen: IdGenerator): PNode =
  # XXX backend == backendJs does not suffice! The compiletime stuff needs
  # the transformation even when compiling to JS ...

  # However we can do lifting for the stuff which is *only* compiletime.
  let isCompileTime = sfCompileTime in fn.flags or fn.kind == skMacro

  if body.kind == nkEmpty or (
      g.config.backend == backendJs and not isCompileTime) or
      fn.skipGenericOwner.kind != skModule:

    # ignore forward declaration:
    result = body
    tooEarly = true
  else:
    var d = initDetectionPass(g, fn, idgen)
    detectCapturedVars(body, fn, d)
    if not d.somethingToDo and fn.isIterator:
      addClosureParam(d, fn, body.info)
      d.somethingToDo = true
    if d.somethingToDo:
      var c = initLiftingPass(fn)
      result = liftCapturedVars(body, fn, d, c)
      # echo renderTree(result, {renderIds})
      if c.envVars.getOrDefault(fn.id) != nil:
        result = newTree(nkStmtList, rawClosureCreation(fn, d, c, body.info), result)
        finishClosureCreation(fn, d, c, body.info, result)
    else:
      result = body
    #if fn.name.s == "get2":
    #  echo "had something to do ", d.somethingToDo
    #  echo renderTree(result, {renderIds})

proc liftLambdasForTopLevel*(module: PSym, body: PNode): PNode =
  # XXX implement it properly
  result = body

# ------------------- iterator transformation --------------------------------

proc liftForLoop*(g: ModuleGraph; body: PNode; idgen: IdGenerator; owner: PSym): PNode =
  # problem ahead: the iterator could be invoked indirectly, but then
  # we don't know what environment to create here:
  #
  # iterator count(): int =
  #   yield 0
  #
  # iterator count2(): int =
  #   var x = 3
  #   yield x
  #   inc x
  #   yield x
  #
  # proc invoke(iter: iterator(): int) =
  #   for x in iter(): echo x
  #
  # --> When to create the closure? --> for the (count) occurrence!
  discard """
      for i in foo(): ...

    Is transformed to:

      cl = createClosure()
      while true:
        let i = foo(cl)
        if (nkBreakState(cl.state)):
          break
        ...
    """
  if liftingHarmful(g.config, owner): return body
  if not (body.kind == nkForStmt and body[^2].kind in nkCallKinds):
    localReport(g.config, body, reportSem rsemIgnoreInvalidForLoop)
    return body
  var call = body[^2]

  result = newNodeI(nkStmtList, body.info)

  # static binding?
  var env: PSym
  let op = call[0]
  if op.kind == nkSym and op.sym.isIterator:
    # createClosure()
    let iter = op.sym

    let hp = getHiddenParam(g, iter)
    env = newSym(skLet, iter.name, nextSymId(idgen), owner, body.info)
    env.typ = hp.typ
    env.flags = hp.flags

    let v = newTreeI(nkVarSection, body.info):
      newIdentDefs(newSymNode(env))
    result.add(v)
    # add 'new' statement:
    result.add(newCall(getSysSym(g, env.info, "internalNew"), env.newSymNode))
    createTypeBoundOpsLL(g, env.typ, body.info, idgen, owner)

  elif op.kind == nkStmtListExpr:
    let closure = op.lastSon
    if closure.kind == nkClosure:
      call[0] = closure
      for i in 0..<op.len-1:
        result.add op[i]

  var loopBody = newNodeI(nkStmtList, body.info, 3)
  var whileLoop = newNodeI(nkWhileStmt, body.info, 2)
  whileLoop[0] = newIntTypeNode(1, getSysType(g, body.info, tyBool))
  whileLoop[1] = loopBody
  result.add whileLoop

  # setup loopBody:
  # gather vars in a tuple:
  var v2 = newNodeI(nkLetSection, body.info)
  var vpart = newNodeI(if body.len == 3: nkIdentDefs else: nkVarTuple, body.info)
  for i in 0..<body.len-2:
    if body[i].kind == nkSym:
      body[i].sym.transitionToLet()
    vpart.add body[i]

  vpart.add newNodeI(nkEmpty, body.info) # no explicit type
  if not env.isNil:
    call[0] = makeClosure(g, idgen, call[0].sym, env.newSymNode, body.info)
  vpart.add call
  v2.add vpart

  loopBody[0] = v2
  var bs = newNodeI(nkBreakState, body.info)
  bs.add call[0]

  let ibs = newNodeI(nkIfStmt, body.info)
  let elifBranch = newNodeI(nkElifBranch, body.info)
  elifBranch.add(bs)

  let br = newNodeI(nkBreakStmt, body.info)
  br.add(g.emptyNode)

  elifBranch.add(br)
  ibs.add(elifBranch)

  loopBody[1] = ibs
  loopBody[2] = body[^1]
