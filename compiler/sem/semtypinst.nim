#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module does the instantiation of generic types.

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast,
    astalgo,
    types,
    renderer,
    lineinfos,
  ],
  compiler/modules/[
    magicsys,
    modulegraphs,
  ],
  compiler/front/[
     msgs,
     options,
  ],
  compiler/sem/[
    semdata,
  ],
  compiler/utils/[
    astrepr
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportAst,
  reportTyp
from compiler/ast/report_enums import ReportKind

const tfInstClearedFlags = {tfHasMeta, tfUnresolved}

proc checkPartialConstructedType(conf: ConfigRef; info: TLineInfo, t: PType) =
  if t.kind in {tyVar, tyLent} and t[0].kind in {tyVar, tyLent}:
    conf.localReport(info, reportTyp(rsemVarVarNotAllowed, t))

proc checkConstructedType*(conf: ConfigRef; info: TLineInfo, typ: PType) =
  var t = typ.skipTypes({tyDistinct})
  if t.kind in tyTypeClasses:
    discard

  elif t.kind in {tyVar, tyLent} and t[0].kind in {tyVar, tyLent}:
    conf.localReport(info, reportTyp(rsemVarVarNotAllowed, t))

  elif computeSize(conf, t) == szIllegalRecursion or isTupleRecursive(t):
    conf.localReport(info, reportTyp(rsemIllegalRecursion, t))

  when false:
    if t.kind == tyObject and t[0] != nil:
      if t[0].kind != tyObject or tfFinal in t[0].flags:
        localReport(info, errInheritanceOnlyWithNonFinalObjects)

proc searchInstTypes*(g: ModuleGraph; key: PType): PType =
  let genericTyp = key[0]
  if not (genericTyp.kind == tyGenericBody and
      genericTyp.sym != nil): return

  for inst in typeInstCacheItems(g, genericTyp.sym):
    if inst.id == key.id: return inst
    if inst.len < key.len:
      # XXX: This happens for prematurely cached
      # types such as Channel[empty]. Why?
      # See the notes for PActor in handleGenericInvocation
      return
    if not sameFlags(inst, key):
      continue

    block matchType:
      for j in 1..high(key.sons):
        # XXX sameType is not really correct for nested generics?
        if not compareTypes(inst[j], key[j],
                            flags = {ExactGenericParams, PickyCAliases}):
          break matchType

      return inst

proc cacheTypeInst(c: PContext; inst: PType) =
  let gt = inst[0]
  let t = if gt.kind == tyGenericBody: gt.lastSon else: gt
  if t.kind in {tyStatic, tyError, tyGenericParam} + tyTypeClasses:
    return
  addToGenericCache(c, gt.sym, inst)

type
  LayeredIdTable* {.acyclic.} = ref object
    topLayer*: TIdTable
    nextLayer*: LayeredIdTable

  TReplTypeVars* = object
    c*: PContext
    typeMap*: LayeredIdTable  # map PType to PType
    symMap*: TIdTable         # map PSym to PSym
    localCache*: TIdTable     # local cache for remembering already replaced
                              # types during instantiation of meta types
                              # (they are not stored in the global cache)
    info*: TLineInfo
    allowMetaTypes*: bool     # allow types such as seq[Number]
                              # i.e. the result contains unresolved generics
    skipTypedesc*: bool       # whether we should skip typeDescs
    isReturnType*: bool
    owner*: PSym              # where this instantiation comes from
    recursionLimit: int

proc replaceTypeVarsTAux(cl: var TReplTypeVars, t: PType): PType
proc replaceTypeVarsS(cl: var TReplTypeVars, s: PSym): PSym
proc replaceTypeVarsN*(cl: var TReplTypeVars, n: PNode; start=0): PNode

proc initLayeredTypeMap*(pt: TIdTable): LayeredIdTable =
  result = LayeredIdTable()
  copyIdTable(result.topLayer, pt)

proc newTypeMapLayer*(cl: var TReplTypeVars): LayeredIdTable =
  result = LayeredIdTable()
  result.nextLayer = cl.typeMap
  initIdTable(result.topLayer)

proc lookup(typeMap: LayeredIdTable, key: PType): PType =
  var tm = typeMap
  while tm != nil:
    result = PType(idTableGet(tm.topLayer, key))
    if result != nil: return
    tm = tm.nextLayer

template put(typeMap: LayeredIdTable, key, value: PType) =
  idTablePut(typeMap.topLayer, key, value)

template checkMetaInvariants(cl: TReplTypeVars, t: PType) = # noop code
  when false:
    if t != nil and tfHasMeta in t.flags and
       cl.allowMetaTypes == false:
      echo "UNEXPECTED META ", t.id, " ", instantiationInfo(-1)
      debug t
      writeStackTrace()

proc replaceTypeVarsT*(cl: var TReplTypeVars, t: PType): PType =
  result = replaceTypeVarsTAux(cl, t)
  checkMetaInvariants(cl, result)

proc isTypeParameterAccess(cl: TReplTypeVars, s: PSym): bool =
  # XXX: this is a workaround used for detecting whether the ``skType``
  #      symbols represents a generic type parameter access
  s.kind == skType and s.typ.kind == tyTypeDesc and
    cl.typeMap.lookup(s.typ[0]) != nil

proc prepareNode*(cl: var TReplTypeVars, n: PNode): PNode =
  ## Replaces unresolved types referenced by the nodes of expression/statement
  ## `n` with the corresponding bound ones.
  ##
  ## Because of how late-bound static parameters are currently implemented,
  ## ``prepareNode`` is also responsible for replacing sub-expression of type
  ## ``tyStatic`` with the computed value.
  template resolveStatic() =
    ## Redirects to the evaluated (or unevaluated) value of the static type, if
    ## `n` resolves to a ``tyStatic``
    let t {.inject.} = replaceTypeVarsT(cl, n.typ)
    # XXX: this logic has to be here because of the ``tyStatic``-related logic
    #      in ``evalAtCompileTime``. Try to find a way to handle late-bound
    #      static parameters differently
    if t != nil and t.kind == tyStatic and t.n != nil:
      # a ``tyStatic`` having a non-nil AST value doesn't mean that it's a
      # resolved ``tyStatic``, see ``sigmatch.paramTypeMatchesAux``
      # XXX: this is dangerous. A lot of places in the compiler depend on
      #      ``TTNode.n != nil`` meaning resolved
      return if tfUnresolved in t.flags: prepareNode(cl, t.n)
              else: t.n

  case n.kind
  of nkSym:
    # only replace type variables for symbols if the symbol belongs to a generic
    # parameter, non-generic parameters, or where it represents an access of
    # generic types parameters. An example of the latter:
    #
    #   type Typ[Param] = object
    #   proc p(x: Typ): seq[Typ.Param]
    #
    # XXX: handle the type parameter access differently; the workaround used
    #      here is shaky
    let s = n.sym
    if s.kind == skParam or
       (s.kind in {skGenericParam, skType} and
        {tfGenericTypeParam, tfImplicitTypeParam} * s.typ.flags != {}) or
       isTypeParameterAccess(cl, s):
      resolveStatic()
      result = copyNode(n)
      result.typ = t
      result.sym = replaceTypeVarsS(cl, s)
    else:
      # important: don't use ``resolveStatic`` here, as the ``replaceTypeVarsT``
      # call would traverse into types not depending on any of the type
      # variables we're replacing here
      result = n
  of nkWithoutSons - {nkSym}:
    resolveStatic()
    result = copyNode(n)
    result.typ = t
  of nkSymChoices:
    # a symbol choice is itself similiar to a symbol
    result = n
  of nkWithSons - nkSymChoices:
    resolveStatic()

    result = shallowCopy(n)
    result.typ = t
    for i, it in n.pairs:
      result[i] = prepareNode(cl, it)

proc isTypeParam(n: PNode): bool =
  # XXX: generic params should use skGenericParam instead of skType
  return n.kind == nkSym and
         (n.sym.kind == skGenericParam or
           (n.sym.kind == skType and sfFromGeneric in n.sym.flags))

proc reResolveCallsWithTypedescParams(cl: var TReplTypeVars, n: PNode): PNode =
  # This is needed for tgenericshardcases
  # It's possible that a generic param will be used in a proc call to a
  # typedesc accepting proc. After generic param substitution, such procs
  # should be optionally instantiated with the correct type. In order to
  # perform this instantiation, we need to re-run the generateInstance path
  # in the compiler, but it's quite complicated to do so at the moment so we
  # resort to a mild hack; the head symbol of the call is temporary reset and
  # overload resolution is executed again (which may trigger generateInstance).
  if n.kind in nkCallKinds and sfFromGeneric in n[0].sym.flags:
    var needsFixing = false
    for i in 1..<n.safeLen:
      if isTypeParam(n[i]): needsFixing = true
    if needsFixing:
      n[0] = newSymNode(n[0].sym.owner)
      return cl.c.semOverloadedCall(cl.c, n, {skProc, skFunc}, {})

  for i in 0..<n.safeLen:
    n[i] = reResolveCallsWithTypedescParams(cl, n[i])

  return n

proc replaceObjBranches(cl: TReplTypeVars, n: PNode): PNode =
  result = n
  case n.kind
  of nkNone..nkNilLit:
    discard
  of nkRecWhen:
    var branch: PNode = nil              # the branch to take
    for i in 0 ..< n.len:
      var it = n[i]
      if it == nil:
        cl.c.config.globalReport(reportAst(
          rsemIllformedAst, n, str = "subnode at idx $1 is 'nil'" % [$i]))

      case it.kind
      of nkElifBranch:
        checkSonsLen(it, 2, cl.c.config)
        var cond = it[0]
        var e = cl.c.semConstExpr(cl.c, cond)
        cl.c.config.internalAssert(e.kind == nkIntLit, e.info):
          "ReplaceTypeVarsN: when condition not a bool"

        if e.intVal != 0 and branch == nil: branch = it[1]
      of nkElse:
        checkSonsLen(it, 1, cl.c.config)
        if branch == nil:
          branch = it[0]
      else:
        cl.c.config.globalReport(reportAst(
          rsemIllformedAst,  n,
          str = "Expected else or elif for subnode at idx $1, but found $2" % [
            $i, $it.kind]))
    if branch != nil:
      result = replaceObjBranches(cl, branch)
    else:
      result = newNodeI(nkRecList, n.info)
  else:
    for i in 0..<n.len:
      n[i] = replaceObjBranches(cl, n[i])

proc replaceTypeVarsN(cl: var TReplTypeVars, n: PNode; start=0): PNode =
  ## Replaces references to unresolved types in AST associated with types (i.e.:
  ## the AST that is stored in the ``TType.n`` field).
  ##
  ## **See also:**
  ## * `prepareNode <#prepareNode,TReplTypeVars,PNode>`_
  if n == nil: return
  result = copyNode(n)
  if n.typ != nil:
    result.typ = replaceTypeVarsT(cl, n.typ)
  case n.kind
  of nkNone..pred(nkSym), succ(nkSym)..nkNilLit:
    discard
  of nkOpenSymChoice, nkClosedSymChoice: result = n
  of nkSym:
    result.sym = replaceTypeVarsS(cl, n.sym)
    if result.sym.typ.kind == tyVoid:
      # don't add the 'void' field
      result = newNodeI(nkRecList, n.info)
  of nkRecWhen:
    var branch: PNode = nil              # the branch to take
    for i in 0..<n.len:
      var it = n[i]
      if it == nil:
        cl.c.config.globalReport(reportAst(
          rsemIllformedAst, n,
          str = "subnode at idx $1 is 'nil'" % [$i]))
      case it.kind
      of nkElifBranch:
        checkSonsLen(it, 2, cl.c.config)
        var cond = prepareNode(cl, it[0])
        var e = cl.c.semConstExpr(cl.c, cond)
        cl.c.config.internalAssert(e.kind == nkIntLit, e.info):
          "ReplaceTypeVarsN: when condition not a bool"
        if e.intVal != 0 and branch == nil: branch = it[1]
      of nkElse:
        checkSonsLen(it, 1, cl.c.config)
        if branch == nil: branch = it[0]
      else:
        cl.c.config.globalReport(reportAst(
          rsemIllformedAst, n,
          str = "Expected else or elif for subnode at idx $1, but found $2" % [
            $i, $it.kind]))
    if branch != nil:
      result = replaceTypeVarsN(cl, branch)
    else:
      result = newNodeI(nkRecList, n.info)
  of nkStaticExpr:
    var n = prepareNode(cl, n)
    n = reResolveCallsWithTypedescParams(cl, n)
    result = if cl.allowMetaTypes: n
             else: cl.c.semExpr(cl.c, n)
    if not cl.allowMetaTypes:
      assert result.kind notin nkCallKinds
  else:
    if n.len > 0:
      newSons(result, n.len)
      if start > 0:
        result[0] = n[0]
      for i in start..<n.len:
        result[i] = replaceTypeVarsN(cl, n[i])

proc replaceTypeVarsS(cl: var TReplTypeVars, s: PSym): PSym =
  if s == nil: return nil
  # symbol is not our business:
  if cl.owner != nil and s.owner != cl.owner:
    return s

  # XXX: Bound symbols in default parameter expressions may reach here.
  # We cannot process them, because `sym.n` may point to a proc body with
  # cyclic references that will lead to an infinite recursion.
  # Perhaps we should not use a black-list here, but a whitelist instead
  # (e.g. skGenericParam and skType).
  # Note: `s.magic` may be `mType` in an example such as:
  # proc foo[T](a: T, b = myDefault(type(a)))
  if s.kind in routineKinds+{skLet, skConst, skVar} or s.magic != mNone:
    return s

  #result = PSym(idTableGet(cl.symMap, s))
  #if result == nil:
  #[

  We cannot naively check for symbol recursions, because otherwise
  object types A, B whould share their fields!

      import tables

      type
        Table[S, T] = object
          x: S
          y: T

        G[T] = object
          inodes: Table[int, T] # A
          rnodes: Table[T, int] # B

      var g: G[string]

  ]#
  result = copySym(s, nextSymId cl.c.idgen)
  incl(result.flags, sfFromGeneric)
  #idTablePut(cl.symMap, s, result)
  result.owner = s.owner
  result.typ = replaceTypeVarsT(cl, s.typ)
  if result.kind != skType:
    result.ast = replaceTypeVarsN(cl, s.ast)

proc lookupTypeVar(cl: var TReplTypeVars, t: PType): PType =
  result = cl.typeMap.lookup(t)
  if result == nil:
    if cl.allowMetaTypes or tfRetType in t.flags: return
    cl.c.config.localReport(t.sym.info, reportTyp(rsemCannotInstantiate, t))
    result = errorType(cl.c)
    # In order to prevent endless recursions, we must remember
    # this bad lookup and replace it with errorType everywhere.
    # These code paths are only active in "nim check"
    cl.typeMap.put(t, result)
  elif result.kind == tyGenericParam and not cl.allowMetaTypes:
    cl.c.config.internalError("substitution with generic parameter")

proc instCopyType*(cl: var TReplTypeVars, t: PType): PType =
  # XXX: relying on allowMetaTypes is a kludge
  if cl.allowMetaTypes:
    result = t.exactReplica
  else:
    result = copyType(t, nextTypeId(cl.c.idgen), t.owner)
    copyTypeProps(cl.c.graph, cl.c.idgen.module, result, t)
    #cl.typeMap.topLayer.idTablePut(result, t)

  if cl.allowMetaTypes: return
  result.flags.incl tfFromGeneric
  if not (t.kind in tyMetaTypes or
         (t.kind == tyStatic and t.n == nil)):
    result.flags.excl tfInstClearedFlags
  else:
    result.flags.excl tfHasAsgn
  when false:
    if newDestructors:
      result.assignment = nil
      result.destructor = nil
      result.sink = nil

proc handleGenericInvocation(cl: var TReplTypeVars, t: PType): PType =
  # tyGenericInvocation[A, tyGenericInvocation[A, B]]
  # is difficult to handle:
  var body = t[0]
  cl.c.config.internalAssert(body.kind == tyGenericBody, cl.info, "no generic body")
  var header = t
  # search for some instantiation here:
  if cl.allowMetaTypes:
    result = PType(idTableGet(cl.localCache, t))
  else:
    result = searchInstTypes(cl.c.graph, t)

  if result != nil and sameFlags(result, t):
    when defined(reportCacheHits):
      echo "Generic instantiation cached ", typeToString(result), " for ", typeToString(t)
    return
  for i in 1..<t.len:
    var x = t[i]
    if x.kind in {tyGenericParam}:
      x = lookupTypeVar(cl, x)
      if x != nil:
        if header == t: header = instCopyType(cl, t)
        header[i] = x
        propagateToOwner(header, x)
    else:
      propagateToOwner(header, x)

  if header != t:
    # search again after first pass:
    result = searchInstTypes(cl.c.graph, header)
    if result != nil and sameFlags(result, t):
      when defined(reportCacheHits):
        echo "Generic instantiation cached ", typeToString(result), " for ",
          typeToString(t), " header ", typeToString(header)
      return
  else:
    header = instCopyType(cl, t)

  result = newType(tyGenericInst, nextTypeId(cl.c.idgen), t[0].owner)
  result.flags = header.flags
  # be careful not to propagate unnecessary flags here (don't use rawAddSon)
  result.sons = @[header[0]]
  # ugh need another pass for deeply recursive generic types (e.g. PActor)
  # we need to add the candidate here, before it's fully instantiated for
  # recursive instantions:
  if not cl.allowMetaTypes:
    cacheTypeInst(cl.c, result)
  else:
    idTablePut(cl.localCache, t, result)

  let oldSkipTypedesc = cl.skipTypedesc
  cl.skipTypedesc = true

  cl.typeMap = newTypeMapLayer(cl)

  for i in 1..<t.len:
    let x = replaceTypeVarsT(cl, header[i])
    assert x.kind != tyGenericInvocation
    header[i] = x
    propagateToOwner(header, x)
    cl.typeMap.put(body[i-1], x)

  for i in 1..<t.len:
    # if one of the params is not concrete, we cannot do anything
    # but we already raised an error!
    rawAddSon(result, header[i], propagateHasAsgn = false)

  if body.kind == tyError:
    return

  let bbody = lastSon body
  var newbody = replaceTypeVarsT(cl, bbody)
  cl.skipTypedesc = oldSkipTypedesc
  newbody.flags = newbody.flags + (t.flags + body.flags - tfInstClearedFlags)
  result.flags = result.flags + newbody.flags - tfInstClearedFlags

  cl.typeMap = cl.typeMap.nextLayer

  # This is actually wrong: tgeneric_closure fails with this line:
  #newbody.callConv = body.callConv
  # This type may be a generic alias and we want to resolve it here.
  # One step is enough, because the recursive nature of
  # handleGenericInvocation will handle the alias-to-alias-to-alias case
  if newbody.isGenericAlias: newbody = newbody.skipGenericAlias
  rawAddSon(result, newbody)
  checkPartialConstructedType(cl.c.config, cl.info, newbody)
  if not cl.allowMetaTypes:
    let dc = cl.c.graph.getAttachedOp(newbody, attachedDeepCopy)
    if dc != nil and sfFromGeneric notin dc.flags:
      # 'deepCopy' needs to be instantiated for
      # generics *when the type is constructed*:
      cl.c.graph.setAttachedOp(cl.c.module.position, newbody, attachedDeepCopy,
          cl.c.instTypeBoundOp(cl.c, dc, result, cl.info, attachedDeepCopy, 1))
    if newbody.typeInst == nil:
      # doAssert newbody.typeInst == nil
      newbody.typeInst = result
      if tfRefsAnonObj in newbody.flags and newbody.kind != tyGenericInst:
        # can come here for tyGenericInst too, see tests/metatype/ttypeor.nim
        # need to look into this issue later
        assert newbody.kind in {tyRef, tyPtr}
        if newbody.lastSon.typeInst != nil:
          #internalError(cl.c.config, cl.info, "ref already has a 'typeInst' field")
          discard
        else:
          newbody.lastSon.typeInst = result
    # DESTROY: adding object|opt for opt[topttree.Tree]
    # sigmatch: Formal opt[=destroy.T] real opt[topttree.Tree]
    # adding myseq for myseq[system.int]
    # sigmatch: Formal myseq[=destroy.T] real myseq[system.int]
    #echo "DESTROY: adding ", typeToString(newbody), " for ", typeToString(result, preferDesc)
    let mm = skipTypes(bbody, abstractPtrs)
    if tfFromGeneric notin mm.flags:
      # bug #5479, prevent endless recursions here:
      incl mm.flags, tfFromGeneric
      for col, meth in methodsForGeneric(cl.c.graph, mm):
        # we instantiate the known methods belonging to that type, this causes
        # them to be registered and that's enough, so we 'discard' the result.
        discard cl.c.instTypeBoundOp(cl.c, meth, result, cl.info,
          attachedAsgn, col)
      excl mm.flags, tfFromGeneric

proc eraseVoidParams*(t: PType) =
  # transform '(): void' into '()' because old parts of the compiler really
  # don't deal with '(): void':
  if t[0] != nil and t[0].kind == tyVoid:
    t[0] = nil

  for i in 1..<t.len:
    # don't touch any memory unless necessary
    if t[i].kind == tyVoid:
      var pos = i
      for j in i+1..<t.len:
        if t[j].kind != tyVoid:
          t[pos] = t[j]
          t.n[pos] = t.n[j]
          inc pos
      setLen t.sons, pos
      setLen t.n.sons, pos
      break

proc skipIntLiteralParams*(t: PType; idgen: IdGenerator) =
  for i in 0..<t.len:
    let p = t[i]
    if p == nil: continue
    let skipped = p.skipIntLit(idgen)
    if skipped != p:
      t[i] = skipped
      if i > 0: t.n[i].sym.typ = skipped

  # when the typeof operator is used on a static input
  # param, the results gets infected with static as well:
  if t[0] != nil and t[0].kind == tyStatic:
    t[0] = t[0].base

proc propagateFieldFlags(t: PType, n: PNode) =
  # This is meant for objects and tuples
  # The type must be fully instantiated!
  if n.isNil:
    return
  #internalAssert n.kind != nkRecWhen
  case n.kind
  of nkSym:
    propagateToOwner(t, n.sym.typ)
  of nkRecList, nkRecCase, nkOfBranch, nkElse:
    for son in n:
      propagateFieldFlags(t, son)
  else: discard

proc replaceTypeVarsTAux(cl: var TReplTypeVars, t: PType): PType =
  template bailout =
    if cl.recursionLimit > 100:
      # bail out, see bug #2509. But note this caching is in general wrong,
      # look at this example where TwoVectors should not share the generic
      # instantiations (bug #3112):

      # type
      #   Vector[N: static[int]] = array[N, float64]
      #   TwoVectors[Na, Nb: static[int]] = (Vector[Na], Vector[Nb])
      result = PType(idTableGet(cl.localCache, t))
      if result != nil: return result
    inc cl.recursionLimit

  result = t
  if t == nil: return

  if t.kind in {tyStatic, tyGenericParam} + tyTypeClasses:
    let lookup = cl.typeMap.lookup(t)
    if lookup != nil: return lookup

  case t.kind
  of tyGenericInvocation:
    result = handleGenericInvocation(cl, t)
    if result.lastSon.kind == tyUserTypeClass:
      result.kind = tyUserTypeClassInst

  of tyGenericBody, tyCompositeTypeClass:
    cl.c.config.localReport(cl.info, reportTyp(rsemCannotInstantiate,t))

    result = errorType(cl.c)
    #result = replaceTypeVarsT(cl, lastSon(t))

  of tyFromExpr:
    if cl.allowMetaTypes: return
    # This assert is triggered when a tyFromExpr was created in a cyclic
    # way. You should break the cycle at the point of creation by introducing
    # a call such as: `n.typ = makeTypeFromExpr(c, n.copyTree)`
    # Otherwise, the cycle will be fatal for the prepareNode call below
    assert t.n.typ != t
    var n = prepareNode(cl, t.n)
    if n.kind != nkEmpty:
      n = cl.c.semConstExpr(cl.c, n)
    if n.typ.kind == tyTypeDesc:
      # XXX: sometimes, chained typedescs enter here.
      # It may be worth investigating why this is happening,
      # because it may cause other bugs elsewhere.
      result = n.typ.skipTypes({tyTypeDesc})
      # result = n.typ.base
    else:
      if n.typ.kind != tyStatic and n.kind != nkType:
        # XXX: In the future, semConstExpr should
        # return tyStatic values to let anyone make
        # use of this knowledge. The patching here
        # won't be necessary then.
        result = newTypeS(tyStatic, cl.c)
        result.sons = @[n.typ]
        result.n = n
      else:
        result = n.typ

  of tyInt, tyFloat:
    result = skipIntLit(t, cl.c.idgen)

  of tyTypeDesc:
    let lookup = cl.typeMap.lookup(t)
    if lookup != nil:
      result = lookup
      if result.kind != tyTypeDesc:
        result = makeTypeDesc(cl.c, result)
      elif tfUnresolved in t.flags or cl.skipTypedesc:
        result = result.base
    elif t[0].kind != tyNone:
      result = makeTypeDesc(cl.c, replaceTypeVarsT(cl, t[0]))

  of tyUserTypeClass, tyStatic:
    result = t

  of tyUserTypeClassInst:
    bailout()
    result = instCopyType(cl, t)
    idTablePut(cl.localCache, t, result)
    for i in 1..<result.len:
      result[i] = replaceTypeVarsT(cl, result[i])
    propagateToOwner(result, result.lastSon)

  of tyGenericInst:
    # do nothing for ``tyGenericInst`` instances. Concrete ones don't need any
    # replacing, and meta ones are handled at the callsite
    doAssert(cl.allowMetaTypes or not containsGenericType(t))
    result = t

  else:
    if containsGenericType(t):
      #if not cl.allowMetaTypes:
      bailout()
      result = instCopyType(cl, t)
      result.size = -1 # needs to be recomputed
      #if not cl.allowMetaTypes:
      idTablePut(cl.localCache, t, result)

      for i in 0..<result.len:
        if result[i] != nil:
          if result[i].kind == tyGenericBody:
            localReport(cl.c.config, if t.sym.isNil: cl.info else: t.sym.info):
              block:
                var r = reportTyp(rsemCannotInstantiate, result[i])
                r.ownerSym = t.owner
                r

          var r = replaceTypeVarsT(cl, result[i])
          if result.kind == tyObject:
            # carefully coded to not skip the precious tyGenericInst:
            let r2 = r.skipTypes({tyAlias, tySink})
            if r2.kind in {tyPtr, tyRef}:
              r = skipTypes(r2, {tyPtr, tyRef})
          result[i] = r
          if result.kind != tyArray or i != 0:
            propagateToOwner(result, r)
      # bug #4677: Do not instantiate effect lists
      result.n = replaceTypeVarsN(cl, result.n, ord(result.kind==tyProc))
      case result.kind
      of tyArray:
        let idx = result[0]
        internalAssert(cl.c.config, idx.kind != tyStatic, "[FIXME]")

      of tyObject, tyTuple:
        propagateFieldFlags(result, result.n)
        if result.kind == tyObject and cl.c.computeRequiresInit(cl.c, result):
          result.flags.incl tfRequiresInit

      of tyProc:
        eraseVoidParams(result)
        skipIntLiteralParams(result, cl.c.idgen)

      of tyRange:
        result[0] = result[0].skipTypes({tyStatic, tyDistinct})

      else: discard
    else:
      # If this type doesn't refer to a generic type we may still want to run it
      # trough replaceObjBranches in order to resolve any pending nkRecWhen nodes
      result = t

      # Slow path, we have some work to do
      if t.kind == tyRef and t.len > 0 and t[0].kind == tyObject and t[0].n != nil:
        discard replaceObjBranches(cl, t[0].n)

      elif result.n != nil and t.kind == tyObject:
        # Invalidate the type size as we may alter its structure
        result.size = -1
        result.n = replaceObjBranches(cl, result.n)

proc initTypeVars*(p: PContext, typeMap: LayeredIdTable, info: TLineInfo;
                   owner: PSym): TReplTypeVars =
  initIdTable(result.symMap)
  initIdTable(result.localCache)
  result.typeMap = typeMap
  result.info = info
  result.c = p
  result.owner = owner

proc replaceTypesInBody*(p: PContext, pt: TIdTable, n: PNode;
                         owner: PSym, allowMetaTypes = false): PNode =
  var typeMap = initLayeredTypeMap(pt)
  var cl = initTypeVars(p, typeMap, n.info, owner)
  cl.allowMetaTypes = allowMetaTypes
  pushInfoContext(p.config, n.info)
  result = replaceTypeVarsN(cl, n)
  popInfoContext(p.config)

when false:
  # deadcode
  proc replaceTypesForLambda*(p: PContext, pt: TIdTable, n: PNode;
                              original, new: PSym): PNode =
    var typeMap = initLayeredTypeMap(pt)
    var cl = initTypeVars(p, typeMap, n.info, original)
    idTablePut(cl.symMap, original, new)
    pushInfoContext(p.config, n.info)
    result = replaceTypeVarsN(cl, n)
    popInfoContext(p.config)

proc recomputeFieldPositions*(t: PType; obj: PNode; currPosition: var int) =
  if t != nil and t.len > 0 and t[0] != nil:
    let b = skipTypes(t[0], skipPtrs)
    recomputeFieldPositions(b, b.n, currPosition)
  case obj.kind
  of nkRecList:
    for i in 0..<obj.len: recomputeFieldPositions(nil, obj[i], currPosition)
  of nkRecCase:
    recomputeFieldPositions(nil, obj[0], currPosition)
    for i in 1..<obj.len:
      recomputeFieldPositions(nil, lastSon(obj[i]), currPosition)
  of nkSym:
    obj.sym.position = currPosition
    inc currPosition
  else: discard "cannot happen"

proc generateTypeInstance*(p: PContext, pt: TIdTable, info: TLineInfo,
                           t: PType): PType =
  ## Produces the instantiated type for the generic type `t`, using the
  ## bindings provided by `pt`. All type variables used by `t` must have
  ## *concrete* types bound to them -- both meta types and missing bindings
  ## are disallowed and will result in an instantiation failure.
  # Given `t` like Foo[T]
  # pt: Table with type mappings: T -> int
  # Desired result: Foo[int]
  # proc (x: T = 0); T -> int ---->  proc (x: int = 0)
  var typeMap = initLayeredTypeMap(pt)
  var cl = initTypeVars(p, typeMap, info, nil)
  pushInfoContext(p.config, info)
  result = replaceTypeVarsT(cl, t)
  popInfoContext(p.config)
  let objType = result.skipTypes(abstractInst)
  if objType.kind == tyObject:
    var position = 0
    recomputeFieldPositions(objType, objType.n, position)

proc prepareMetatypeForSigmatch*(p: PContext, pt: TIdTable, info: TLineInfo,
                                 t: PType): PType =
  var typeMap = initLayeredTypeMap(pt)
  var cl = initTypeVars(p, typeMap, info, nil)
  cl.allowMetaTypes = true
  pushInfoContext(p.config, info)
  result = replaceTypeVarsT(cl, t)
  popInfoContext(p.config)

template generateTypeInstance*(p: PContext, pt: TIdTable, arg: PNode,
                               t: PType): untyped =
  generateTypeInstance(p, pt, arg.info, t)
