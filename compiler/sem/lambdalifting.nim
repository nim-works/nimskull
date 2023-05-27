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
    idents,
    renderer,
    types,
    lineinfos
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
    liftdestructors,
    lowerings
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport,
  reportSem
from compiler/ast/report_enums import ReportKind

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

proc newObjConstr(typ: PType, info: TLineInfo): PNode =
  ## Creates an object construction node with no arguments for type `typ`,
  ## using `info` for source line information.
  newTreeIT(nkObjConstr, info, typ, [newNodeIT(nkType, info, typ)])

proc createClosureIterStateType(g: ModuleGraph; iter: PSym; idgen: IdGenerator): PType =
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
  result = createObj(g, idgen, owner, info, final=false)

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

  if tfHasAsgn in result.typ.flags or optSeqDestructors in g.config.globalOptions:
    prc.flags.incl sfInjectDestructors

proc interestingIterVar(s: PSym): bool {.inline.} =
  # XXX optimization: Only lift the variable if it lives across
  # yield/return boundaries! This can potentially speed up
  # closure iterators quite a bit.
  result = s.kind in {skResult, skVar, skLet, skTemp, skForVar} and sfGlobal notin s.flags

template isIterator*(owner: PSym): bool =
  owner.kind == skIterator and owner.typ.callConv == ccClosure

proc createTypeBoundOpsLL(g: ModuleGraph; refType: PType; info: TLineInfo; idgen: IdGenerator; owner: PSym) =
  if owner.kind != skMacro:
    # XXX: this breaks the IC implementation. Lambda-lifting happens as part
    #      of the backend processing, at which point the packed modules are
    #      effectively sealed and no more content must be written to them (which
    #      is what happens here).
    createTypeBoundOps(g, nil, refType.lastSon, info, idgen)
    createTypeBoundOps(g, nil, refType, info, idgen)
    if tfHasAsgn in refType.flags or optSeqDestructors in g.config.globalOptions:
      owner.flags.incl sfInjectDestructors

proc freshVarForClosureIter*(g: ModuleGraph; s: PSym; idgen: IdGenerator; owner: PSym): PNode =
  ## Adds a field corresponding to the local `s` to `owner`'s environment
  ## type.
  let envParam = getHiddenParam(g, owner)
  let obj = envParam.typ.base
  addField(obj, s, g.cache, idgen)

  var access = newSymNode(envParam)
  assert obj.kind == tyObject
  let field = getFieldFromObj(obj, s)
  g.config.internalAssert(field != nil, s.info, "internal error: cannot generate fresh variable")
  result = rawIndirectAccess(access, field, s.info)

# ------------------ new stuff -------------------------------------------

type
  DetectionPass = object
    ## Acts as the accumulator for the detection pass. That is, the pass
    ## gathers information into an instance of ``DetectionPass``.
    graph: ModuleGraph
    root: PSym
      ## the symbol of the *root* procedure. It is the one for which we're
      ## gathering the set of locals that need to be lifted

    processed, marker: IntSet
    capturedVars: seq[PSym]
      ## all local variables and parameters that need to be lifted into
      ## the generated environment
    usedInner: seq[PSym]
      ## all used inner closure routines defined directly in root. We cannot
      ## gather these by simply looking for procdef nodes, as we'd miss
      ## instances of generics routines then

    accessOuter: bool
      ## whether the root routine accesses an outer local
    requireUp: bool
      ## whether an 'up' field is required in the environment lifted for the
      ## root. This is the case when a inner routine needs access to some of
      ## the root's parent environment

    closureProcCalled: bool
      ## indicates whether some inner, non-iterator routine that uses the
      ## .closure calling convention and is defined in root is called
      ## somewhere

proc initDetectionPass(g: ModuleGraph; fn: PSym; idgen: IdGenerator): DetectionPass =
  result.graph = g
  result.root = fn
  result.processed = initIntSet()
  result.marker = initIntSet()
  result.processed.incl(fn.id)

proc createUpField(c: DetectionPass; idgen: IdGenerator,
                   dest, dep: PType; info: TLineInfo) =
  # The assumption here is that gcDestructors means we cannot deal
  # with cycles properly, so it's better to produce a weak ref (=ptr) here.
  # This seems to be generally correct but since it's a bit risky it's disabled
  # for now.
  # XXX This is wrong for the 'hamming' test, so remove this logic again.
  let
    fieldType = dep
    upIdent = getIdent(c.graph.cache, upName)

  c.graph.config.internalAssert(
    lookupInRecord(dest.n, upIdent) == nil,
    info, "'up' field already exists")

  let result = newSym(skField, upIdent, nextSymId(idgen), dest.owner, dest.owner.info)
  result.typ = fieldType
  if isDefined(c.graph.config, "nimCycleBreaker"):
    # see the comment regarding the weak ref above
    result.flags.incl sfCursor
  rawAddField(dest, result)

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

proc addClosureParam(graph: ModuleGraph, idgen: IdGenerator; fn: PSym; t: PType, info: TLineInfo) =
  assert fn.typ.callConv == ccClosure
  graph.config.internalAssert(getEnvParam(fn) == nil, fn.info):
    "internal error: environment parameter already added"

  let cp = newSym(skParam, getIdent(graph.cache, paramName), nextSymId(idgen), fn, fn.info)
  incl(cp.flags, sfFromGeneric)
  cp.typ = t
  addHiddenParam(fn, cp)

proc detectCapturedVars(n: PNode; owner: PSym; c: var DetectionPass) =
  ## Traverses the AST `n` and accumulates information required by the later
  ## transformation pass into `c`. Nothing is modified yet.
  case n.kind
  of nkSym:
    let
      s = n.sym
      interestingProc = isInnerProc(s) and s.typ.callConv == ccClosure

    if interestingProc and not c.processed.containsOrIncl(s.id):
      # the inner routine uses the closure calling convention, which
      # indicates that it *might* close over something (note that
      # semantic analysis makes sure that all routines closing over
      # something *must* use the ``closure`` calling convention). Scan the
      # routine for entities of `owner` it closes over, but only if we
      # haven't done so already
      detectCapturedVars(getBody(c.graph, s), s, c)

    let ow = s.skipGenericOwner
    if ow == owner:
      # also mark local variables of the iterator we're analysing (``c.root``)
      # as captured. This is so that they're also lifted into the environment
      # XXX: it's probably a better idea to move this step into the
      #      ``closureiters`` pass, as it would allow for only lifting locals
      #      that survive across yields, and we also wouldn't need
      #      ``freshVarForClosureIter`` then
      if owner == c.root and owner.isIterator and interestingIterVar(s) and
         not c.marker.containsOrIncl(s.id):
        c.capturedVars.add s
    elif interestingProc or interestingVar(s):
      # something eligible for capturing is accessed. Note that creating a
      # closure (by using the symbol of a closure routine) also means that a
      # local is accessed: the local storing the environment
      if ow == c.root:
        if interestingVar(s) and not c.marker.containsOrIncl(s.id):
          # a local defined in the root routine is accessed from an inner
          # routine -> it needs to be lifted
          c.capturedVars.add s
      elif isOwnedBy(c.root, ow):
        # usage of a local that's defined outside the root routine -> the root
        # routine requires access to some outer procedure's environment
        if owner == c.root:
          # the root routine uses a local defined in an outer scope
          c.accessOuter = true
        else:
          # some inner routine uses a local defined in a scope that's
          # outside the root routine
          c.requireUp = true

    if ow == c.root and interestingProc and not c.marker.containsOrIncl(s.id):
      if not s.isIterator:
        # problem: a non-iterator with the closure calling convention doesn't
        # imply that the procedure captures anything. If it doesn't, we still
        # need to add a hidden environment parameter for it. While we could
        # use some type like ``ref int`` to signal that it's a fake environment
        # type, that would further complicate things, so instead, we force
        # the allocation of a possibly empty environment object.
        c.closureProcCalled = true

      c.usedInner.add s

  of nkWithoutSons - nkSym, nkTypeSection, nkCommentStmt,
     nkTypeOfExpr, nkMixinStmt, nkBindStmt, routineDefs - nkIteratorDef:
    discard
  of nkLambdaKinds, nkIteratorDef:
    if n.typ != nil:
      # a lambda expression; it combines *definition* and *usage*
      detectCapturedVars(n[namePos], owner, c)
  of nkReturnStmt:
    detectCapturedVars(n[0], owner, c)
  of nkNimNodeLit:
    discard "skip node literals as they're data not code"
  of nkIdentDefs:
    # don't traverse the type slot
    detectCapturedVars(n[0], owner, c)
    detectCapturedVars(n[2], owner, c)
  else:
    for it in n.items:
      detectCapturedVars(it, owner, c)

proc produceEnvType(c: DetectionPass, idgen: IdGenerator, prc: PSym, info: TLineInfo): PType =
  ## Produces the environment type for routine `prc`, using the information
  ## previously gathered by ``detectCapturedVars``.
  result = newType(tyRef, nextTypeId(idgen), prc)
  let obj = createEnvObj(c.graph, idgen, prc, info)

  if prc.isIterator:
    # iterators require an additional state field, and we add it
    # here already. Note that the "state" field has to be the *first*
    # field; this allows for simple lookup later on
    rawAddField(obj, createStateField(c.graph, prc, idgen))

  if c.requireUp:
    # if an up-reference is required, earlier processing made sure that
    # `prc` has the type already added as the hidden environment parameter,
    # meaning that we can safely query it here
    createUpField(c, idgen, obj, getHiddenParam(c.graph, prc).typ, info)

  # TODO: re-use the 'up' type if it's the only field in the environment
  for it in c.capturedVars.items:
    addField(obj, it, c.graph.cache, idgen)

  rawAddSon(result, obj)

  if not prc.isIterator:
    # do **not** lift the type-bound operators for the environment type of
    # iterators here. It's, unfortunately, still modified past this
    # point, and lifting the operators here would result in fields being
    # skipped
    createTypeBoundOpsLL(c.graph, result, prc.info, idgen, prc)

proc prepareInnerRoutines(d: DetectionPass, idgen: IdGenerator, env: PType, info: TLineInfo) =
  ## Adds `env` as the hidden parameter type to all inner routines defined
  ## directly in `root`. `d` must have been populated by ``detectCapturedVars``
  ## already.
  for it in d.usedInner.items:
    addClosureParam(d.graph, idgen, it, env, info)

type
  LiftingPass = object
    ## Contextual information for the transformation pass.
    owner: PSym  ## the routine the pass is run for
    env: PType   ## the local environment's ``ref`` type
    envSym: PSym ## the symbol of the local (or parameter) to access the local
                 ## environment through

    capturedVars: IntSet
      ## the set of locals variables and parameters that were lifted into the
      ## local environment

func initLiftingPass(d: sink DetectionPass, envSym: PSym): LiftingPass =
  ## Creates an instance of the ``LiftingPass`` from `d` and `envSym`.
  result = LiftingPass(owner: d.root, env: envSym.typ, envSym: envSym,
                       capturedVars: move d.marker)

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

proc newEnvVar(cache: IdentCache; owner: PSym; typ: PType; info: TLineInfo; idgen: IdGenerator): PSym =
  var v = newSym(skVar, getIdent(cache, envName), nextSymId(idgen), owner, info)
  v.flags = {sfShadowed, sfGeneratedOp}
  v.typ = typ
  result = v

proc getUpViaParam(g: ModuleGraph; owner: PSym): PNode =
  let p = getHiddenParam(g, owner)
  result = p.newSymNode
  if owner.isIterator:
    let upField = lookupInRecord(p.typ.skipTypes({tyRef, tyPtr}).n, getIdent(g.cache, upName))
    g.config.internalAssert(upField != nil, owner.info, "could not find up reference for closure iter")
    result = rawIndirectAccess(result, upField, p.info)

proc rawClosureCreation(graph: ModuleGraph, idgen: IdGenerator,
                        c: LiftingPass; info: TLineInfo): PNode =
  ## Generates and returns the AST for allocating and setting up an instance
  ## of `owner`'s lifted local environment.
  result = newNodeI(nkStmtList, c.owner.info)

  var env: PNode
  if c.owner.isIterator:
    env = newSymNode(c.envSym)
  else:
    env = newSymNode(c.envSym)
    let v = newTreeI(nkVarSection, env.info):
      newIdentDefs(env, newObjConstr(env.typ, info))
    result.add(v)

    # add assignment statements for captured parameters:
    for i in 1..<c.owner.typ.n.len:
      let local = c.owner.typ.n[i].sym
      if local.id in c.capturedVars:
        let fieldAccess = indirectAccess(env, local, env.info)
        # add ``env.param = param``
        result.add(newAsgnStmt(fieldAccess, newSymNode(local), env.info))
        if c.owner.kind != skMacro:
          createTypeBoundOps(graph, nil, fieldAccess.typ, env.info, idgen)
        if tfHasAsgn in fieldAccess.typ.flags or optSeqDestructors in graph.config.globalOptions:
          c.owner.flags.incl sfInjectDestructors

  let upField = lookupInRecord(
    env.typ.base.n, getIdent(graph.cache, upName))

  if upField != nil:
    let up = getUpViaParam(graph, c.owner)
    graph.config.internalAssert(
      up != nil and upField.typ.base == up.typ.base,
      env.info, "internal error: cannot create up reference")

    result.add(newAsgnStmt(rawIndirectAccess(env, upField, env.info), up, env.info))

proc accessViaEnvVar(n: PNode; c: LiftingPass): PNode =
  let access = newSymNode(c.envSym)
  let field = getFieldFromObj(access.typ.base, n.sym)
  assert field != nil, "internal error: not part of closure object type"
  result = rawIndirectAccess(access, field, n.info)

proc accessEnv(available: PSym, wanted: PType, info: TLineInfo,
               graph: ModuleGraph): PNode =
  ## Generates the AST for accessing through `available` the environment
  ## identified by `wanted` which must be reachable from the current context.
  # walk up the stack formed by the 'up' fields until we reach one that is of
  # type `wanted`
  result = newSymNode(available)
  while result.typ != wanted:
    let obj = result.typ.base
    assert obj.kind == tyObject
    let upField = lookupInRecord(obj.n, getIdent(graph.cache, upName))
    graph.config.internalAssert(upField != nil, info):
      "internal error: no environment found"
    result = rawIndirectAccess(result, upField, info)

proc getStateField*(g: ModuleGraph; owner: PSym): PSym =
  getHiddenParam(g, owner).typ.base.n[0].sym

proc symToClosure(n: PNode; graph: ModuleGraph; idgen: IdGenerator;
                  c: LiftingPass): PNode =
  let
    s = n.sym
    owner = c.owner

  assert not s.isIterator, "too early"
  if s == owner:
    # recursive calls go through (lambda, hiddenParam):
    let available = getHiddenParam(graph, owner)
    result = makeClosure(graph, idgen, s, newSymNode(available), n.info)
  elif s.skipGenericOwner == owner:
    # direct dependency, so use the outer's env variable:
    result = makeClosure(graph, idgen, s, newSymNode(c.envSym), n.info)
  else:
    # ugh: call through some other inner proc;
    let
      wanted = getHiddenParam(graph, s).typ
      access = accessEnv(getHiddenParam(graph, owner), wanted, n.info, graph)
    result = makeClosure(graph, idgen, s, access, n.info)

proc transformedAccess(n: PNode, graph: ModuleGraph, idgen: IdGenerator, c: LiftingPass): PNode =
  let s = n.sym
  if isInnerProc(s):
    # don't transform closure iterator usages yet; we don't know the
    # proper environment types at this point
    if s.typ.callConv == ccClosure and not s.isIterator:
      result = symToClosure(n, graph, idgen, c)
    else:
      result = n
  elif s.id in c.capturedVars:
    # something that was lifted into the local environment
    if c.owner.isIterator:
      result = accessViaEnvParam(graph, n, c.owner)
    else:
      result = accessViaEnvVar(n, c)
  elif s.owner != c.owner and interestingVar(s):
    # access to some outer local
    result = accessViaEnvParam(graph, n, c.owner)
  else:
    result = n

proc liftCapturedVars(n: PNode, graph: ModuleGraph, idgen: IdGenerator,
                      c: LiftingPass): PNode =
  ## Recursively transforms the AST `n` according to the context provided
  ## with `c`. Usages and definitions of locals are transformed into an
  ## environment field access, and usages of closure routines (except for
  ## iterators) are transformed into closure construction expressions.
  ##
  ## In addition, the current enivornment (which can either be the root
  ## routines local environment or some outer environment) is added as the
  ## hidden parameter to each closure routine defined in the root
  ## routine.
  ##
  ## `n` is assumed to be owned, and is thus modified in-place.
  result = n
  case n.kind
  of nkSym:
    # note that we're ignoring whether we're in a definition or usage
    # context here. This means that code like ``var env.x = 0`` will be
    # generated, but that's okay; these are transformed into normal
    # assignments at a later point
    result = transformedAccess(n, graph, idgen, c)
  of nkWithoutSons - nkSym, nkTypeSection, nkMixinStmt, nkBindStmt,
     routineDefs - {nkIteratorDef}:
    discard
  of nkClosure:
    if n[1].kind == nkNilLit:
      n[0] = liftCapturedVars(n[0], graph, idgen, c)
      let x = n[0].skipConv
      if x.kind == nkClosure:
        #localReport(n.info, "internal error: closure to closure created")
        # now we know better, so patch it:
        n[0] = x[0]
        n[1] = x[1]
  of nkLambdaKinds, nkIteratorDef:
    let s = n[namePos].sym
    if n.typ != nil and s.typ.callConv == ccClosure:
      # a lambda expression that yields a closure
      result = newSymNode(s, n.info)
      # turn into a closure construction expression already
      # (if possible):
      result = transformedAccess(result, graph, idgen, c)

  of nkHiddenStdConv:
    if n.len == 2:
      n[1] = liftCapturedVars(n[1], graph, idgen, c)
      if n[1].kind == nkClosure: result = n[1]
  of nkReturnStmt:
    if n[0].kind in {nkAsgn, nkFastAsgn}:
      # let's not touch the LHS in order to make the lifting pass
      # correct when `result` is lifted
      n[0][1] = liftCapturedVars(n[0][1], graph, idgen, c)
    else:
      n[0] = liftCapturedVars(n[0], graph, idgen, c)
  of nkIdentDefs:
    assert n.len == 3
    n[0] = liftCapturedVars(n[0], graph, idgen, c)
    # don't process the type slot
    n[2] = liftCapturedVars(n[2], graph, idgen, c)
  of nkTypeOfExpr:
    result = n
  else:
    if c.owner.isIterator:
      if nfLL in n.flags:
        # special case 'when nimVm' due to bug #3636:
        # XXX: this means that the ``when nimVm`` branch is not transformed for
        #      closure iterators!
        n[1] = liftCapturedVars(n[1], graph, idgen, c)
        return

    for i in 0..<n.len:
      n[i] = liftCapturedVars(n[i], graph, idgen, c)

# ------------------ old stuff -------------------------------------------

proc liftIterToProc*(g: ModuleGraph; fn: PSym; body: PNode; ptrType: PType;
                     idgen: IdGenerator): PNode =
  var d = initDetectionPass(g, fn, idgen)
  detectCapturedVars(body, fn, d)

  for it in d.capturedVars.items:
    addField(ptrType.base, it, g.cache, idgen)

  addClosureParam(g, idgen, fn, ptrType, fn.info)
  var c = initLiftingPass(d, getHiddenParam(g, fn))
  result = liftCapturedVars(body, g, idgen, c)

proc liftLambdas*(g: ModuleGraph; fn: PSym, body: PNode;
                  idgen: IdGenerator): tuple[body: PNode, env: PSym] =
  ## Performs multiple things:
  ## * produce an object type that contains all local variables and
  ##   parameters of `fn` (with body `body`) that inner routines close
  ##   over
  ## * injects the AST for setting up an instance of the environment
  ## * rewrites definitions and usages of the lifted local variables into
  ##   an environment field access (but only those directly in `body`; the
  ##   bodies of inner routines are not modified)
  ## * rewrites usages of ``.closure`` routine symbols into closure
  ##   construction expressions (i.e., ``nkClosure``). Because of a phase-
  ##   ordering problem, this step is skipped for symbols of closure
  ##   iterators
  ## * adds the produced environment type as a hidden parameter to all
  ##   inner routines defined *directly* in `body` (transitive inner
  ##   routines are not modified)
  ##
  ## If `fn` closes over some outer locals itself, it is required that the
  ## environment type through which all closed-over locals are accessible
  ## is present as `fn`'s hidden parameter.
  ##
  ## In the case of closure iterators, the body of the generated environment
  ## type is not yet final -- both ``transf`` and the ``closureiters`` pass
  ## still append to it.
  if fn.isIterator:
    g.config.internalAssert(sfForward notin fn.flags)

    var d = initDetectionPass(g, fn, idgen)
    detectCapturedVars(body, fn, d)

    # iterators take their own environment as the parameter, so if the iterator
    # accesses some outer local, we also need an 'up' parameter
    d.requireUp = d.requireUp or d.accessOuter

    # we always need to produce an environment type, even if currently empty
    let t = produceEnvType(d, idgen, fn, fn.info)

    # if the iterator has a hidden environment parameter already, then it's
    # of the wrong type -- it currently has the type of some some outer
    # routines's environment, but it has to be the iterator's environment
    # type. This patching has to  happen *before* transforming the body below.
    var param = getEnvParam(fn)
    if param.isNil:
      addClosureParam(g, idgen, fn, t, fn.info)
      param = getHiddenParam(g, fn)
    else:
      param.typ = t # replace with the correct type

    prepareInnerRoutines(d, idgen, t, fn.info)
    # the environment instance is not setup here; that's done at the iterator's
    # callsite
    result.body = liftCapturedVars(body, g, idgen, initLiftingPass(d, param))
    result.env = param
  elif body.kind != nkEmpty:
    assert fn.typ.callConv != ccClosure or getEnvParam(fn) != nil,
      "missing environment parameter"

    var d = initDetectionPass(g, fn, idgen)
    detectCapturedVars(body, fn, d)

    if d.requireUp or d.accessOuter or d.closureProcCalled or
       d.capturedVars.len > 0:
      let t = produceEnvType(d, idgen, fn, fn.info)
      prepareInnerRoutines(d, idgen, t, fn.info)

      let
        c = initLiftingPass(d, newEnvVar(g.cache, fn, t, fn.info, idgen))
        transformed = liftCapturedVars(body, g, idgen, c)

      # XXX: if only the outer environment needs to be passed to inner
      #      routines and nothing in `fn` itself needs to be lifted,
      #      then we don't need a dedicated environment that stores just
      #      the up reference
      if d.requireUp or d.closureProcCalled or d.capturedVars.len > 0:
        result.body = rawClosureCreation(g, idgen, c, body.info)
        result.body.add transformed
      else:
        result.body = transformed

      result.env = c.envSym
    else:
      # nothing to do
      result = (body, nil)
  else:
    # a routine prootype or otherwise empty routine -> nothing to do
    result = (body, nil)

proc liftLambdasForTopLevel*(module: PSym, body: PNode): PNode =
  # XXX implement it properly
  result = body

proc liftIterSym*(g: ModuleGraph; n: PNode; idgen: IdGenerator; owner, currEnv: PSym): PNode =
  ## Transforms  ``(iter)``  to  ``(let env = newClosure[iter](); (iter, env))``.
  ## This cannot happen as part of ``liftLambdas``, as the iterators's
  ## environment type is not available at that point.
  let iter = n.sym
  assert iter.isIterator

  let
    hp = getHiddenParam(g, iter)
    envTyp = hp.typ # the environment's ``ref`` type
    v = newSym(skLet, getIdent(g.cache, envName), nextSymId(idgen), owner, n.info)

  v.typ = envTyp
  incl(v.flags, sfShadowed)

  let vnode = newSymNode(v)

  result = newNodeIT(nkStmtListExpr, n.info, iter.typ)
  result.add newTreeI(nkLetSection, n.info,
    [newIdentDefs(vnode, newObjConstr(envTyp, n.info))])

  let upField = lookupInRecord(envTyp.base.n, getIdent(g.cache, upName))
  if upField != nil:
    # the iterator has an 'up' field, and we have to initialize it here
    let access = accessEnv(currEnv, upField.typ, n.info, g)
    result.add(newAsgnStmt(rawIndirectAccess(vnode, upField, n.info), access, n.info))

  result.add makeClosure(g, idgen, iter, vnode, n.info)

proc ensureEnvParam*(graph: ModuleGraph, idgen: IdGenerator, prc: PSym) =
  ## Problem: top-level anonymous expression can explicitly use the .closure
  ## calling convention (which doesn't make sense, really). All .closure
  ## procedures need a hidden environment paramater, and so ``ensureEnvParam``
  ## adds an empty one.
  assert not prc.isIterator
  assert prc.typ.callConv == ccClosure
  assert prc.skipGenericOwner.kind == skModule

  let t = newType(tyRef, nextTypeId(idgen), prc)
  t.rawAddSon createEnvObj(graph, idgen, prc, prc.info)
  addClosureParam(graph, idgen, prc, t, prc.info)

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

      let cl = createClosure()
      while true:
        let i = cl()
        if (finished(cl)):
          break
        ...
    """
  if not (body.kind == nkForStmt and body[^2].kind in nkCallKinds):
    localReport(g.config, body, reportSem rsemIgnoreInvalidForLoop)
    return body
  var call = body[^2]

  result = newNodeI(nkStmtList, body.info)

  var op = call[0]
  if op.kind == nkStmtListExpr and op.lastSon.kind == nkClosure:
    # this is the symbol of a closure iterator that was transformed into a
    # closure setup expression. We assign the created closure to a new
    # local and replace the callee part with said local
    let
      orig = op
      callee = newSym(skLet, op.lastSon[0].sym.name, nextSymId(idgen), owner, op.info)
    callee.typ = op.typ

    if owner.isIterator:
      # meh, we have to add the local to the environment; it might be used
      # across yields
      op = freshVarForClosureIter(g, callee, idgen, owner)
    else:
      op = newSymNode(callee, orig.info)

    result.add newTree(nkLetSection, [newIdentDefs(op, orig)])

    # update the call expression:
    call[0] = op

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
  vpart.add call
  v2.add vpart

  loopBody[0] = v2
  let
    finishedPrc = getSysMagic(g, body.info, "finished", mFinished)
    bs = newTreeIT(nkCall, body.info, getSysType(g, body.info, tyBool),
                   newSymNode(finishedPrc), call[0])

  let ibs = newNodeI(nkIfStmt, body.info)
  let elifBranch = newNodeI(nkElifBranch, body.info)
  elifBranch.add(bs)

  let br = newNodeI(nkBreakStmt, body.info)
  br.add(g.emptyNode)

  elifBranch.add(br)
  ibs.add(elifBranch)

  loopBody[1] = ibs
  loopBody[2] = body[^1]

proc finishClosureIterator*(g: ModuleGraph, idgen: IdGenerator, iter: PSym) =
  ## Creates the type-bound operators for the iterator's hidden environment
  ## parameter type.
  createTypeBoundOpsLL(g, getHiddenParam(g, iter).typ, iter.info, idgen, iter)