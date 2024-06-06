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
    intsets
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
    astrepr,
    debugutils,
    idioms
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportAst,
  reportTyp, SemReport
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

func addInheritedFieldsAux(check: var IntSet, pos: var int, n: PNode) =
  case n.kind
  of nkRecCase:
    addInheritedFieldsAux(check, pos, n[0])
    for _, it in branches(n):
      case it.kind
      of nkOfBranch, nkElse:
        addInheritedFieldsAux(check, pos, it[^1])
      else:
        unreachable()
  of nkRecList:
    for it in n.items:
      addInheritedFieldsAux(check, pos, it)
  of nkSym:
    incl(check, n.sym.name.id)
    inc(pos)
  else:
    unreachable()

func addInheritedFields*(check: var IntSet, obj: PType): int =
  ## Counts the number of fields in `obj` (and its base types) and returns it.
  ## In addition, adds the IDs of all field names in `obj` and its base types
  ## to `check`.
  assert obj.kind == tyObject, $obj.kind
  if (obj.len > 0) and (obj.base != nil):
    result += addInheritedFields(check, obj.base.skipTypes(skipPtrs))
  addInheritedFieldsAux(check, result, obj.n)

proc searchInstTypes(g: ModuleGraph; key: PType): PType =
  let genericTyp = key[0]
  if not (genericTyp.kind == tyGenericBody and
      genericTyp.sym != nil): return

  for inst in typeInstCacheItems(g, genericTyp.sym):
    if inst.id == key.id: return inst
    assert inst.len >= key.len
    if not sameFlags(inst, key):
      continue

    block matchType:
      # compare the arguments. If all are equal, we've got a cache hit,
      # otherwise we don't
      for j in 1..<key.len:
        if not compareTypes(inst[j], key[j],
                            flags = {PickyCAliases}):
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
    info*: TLineInfo
    skipTypedesc*: bool       # whether we should skip typeDescs
    isReturnType*: bool
    owner*: PSym              # where this instantiation comes from
    recursionLimit: int

proc replaceTypeVarsTAux(cl: var TReplTypeVars, t: PType): PType
proc replaceTypeVarsS(cl: var TReplTypeVars, s: PSym): PSym
proc replaceTypeVarsN(cl: var TReplTypeVars, n: PNode; start=0): PNode
proc replaceTypeVarsInBody*(c: PContext, pt: TIdTable, n: PNode): PNode

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
    if t != nil and tfHasMeta in t.flags:
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

proc fixType(c: PContext, s: PSym, t: PType): PType =
  ## Computes the correct type to assign to the symbol node of a replaced
  ## type variable.
  # XXX: ideally, this would be left to ``semSym``, but at the time of writing,
  #      an expression having a type prevents it from being analyzed again in
  #      some cases (e.g., call operands), meaning that we have to already
  #      assign the expected type here
  case s.kind
  of skGenericParam:
    if s.typ.kind == tyStatic:
      t
    else:
      makeTypeDesc(c, t)
  of skType:
    makeTypeDesc(c, t)
  of skParam:
    t
  else:
    unreachable(s.kind)

proc prepareNode*(cl: var TReplTypeVars, n: PNode): PNode =
  ## Replaces unresolved types referenced by the nodes of expression/statement
  ## `n` with the corresponding bound ones.
  ##
  ## Because of how late-bound static parameters are currently implemented,
  ## ``prepareNode`` is also responsible for replacing sub-expression of type
  ## ``tyStatic`` with the computed value.
  addInNimDebugUtils(cl.c.config, "prepareNode", n, result)

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
      result.sym = replaceTypeVarsS(cl, s)
      result.typ = fixType(cl.c, s, t)

      if s.kind == skParam and s.typ.kind == tyTypeDesc:
        # late-bound typedesc parameters act like types when used inside
        # type-attached AST, and to make things easier for further
        # processing, we make this explicit by transitioning the symbol kind
        result.sym.transitionGenericParamToType()
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

proc reResolveCallsWithTypedescParams(c: PContext, n: PNode): PNode =
  # This is needed for tgenericshardcases
  # It's possible that a generic param will be used in a proc call to a
  # typedesc accepting proc. After generic param substitution, such procs
  # should be optionally instantiated with the correct type. In order to
  # perform this instantiation, we need to re-run the generateInstance path
  # in the compiler, but it's quite complicated to do so at the moment so we
  # resort to a mild hack; the head symbol of the call is temporary reset and
  # overload resolution is executed again (which may trigger generateInstance).
  if n.kind in nkCallKinds and n[0].kind == nkSym and
     sfFromGeneric in n[0].sym.flags:
    var needsFixing = false
    for i in 1..<n.safeLen:
      if isTypeParam(n[i]): needsFixing = true
    if needsFixing:
      n[0] = newSymNode(n[0].sym.owner)
      return c.semOverloadedCall(c, n, n, {skProc, skFunc}, {})

  for i in 0..<n.safeLen:
    n[i] = reResolveCallsWithTypedescParams(c, n[i])

  return n

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
  of nkWithoutSons - nkSym:
    discard
  of nkOpenSymChoice, nkClosedSymChoice: result = n
  of nkSym:
    result.sym = replaceTypeVarsS(cl, n.sym)
    if result.sym.typ.kind == tyVoid:
      # don't add the 'void' field
      result = newNodeI(nkRecList, n.info)
  of nkRecWhen, nkRecCase:
    # the nodes are only part of object-type AST
    unreachable(n.kind)

  of nkStaticExpr:
    var n = prepareNode(cl, n)
    n = reResolveCallsWithTypedescParams(cl.c, n)
    result = cl.c.semExpr(cl.c, n)
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

proc instantiateRecord(cl: var TReplTypeVars, names: var IntSet, pos: var int,
                       n: PNode, owner: PSym): PNode =
  ## Traverses the pre-processed AST of a generic object type and produces
  ## the instantiated AST.
  template recurse(n: PNode): PNode =
    instantiateRecord(cl, names, pos, n, owner)

  case n.kind
  of nkSym:
    # a single field
    let t = replaceTypeVarsT(cl, n.sym.typ)
    if t.kind == tyVoid:
      # don't add 'void' fields to objects
      result = newNodeI(nkRecList, n.info)
    else:
      let s = copySym(n.sym, nextSymId cl.c.idgen)
      s.typ = t
      s.flags.incl sfFromGeneric
      s.owner = owner
      s.position = pos
      propagateToOwner(owner.typ, t)
      inc pos

      result = copyNode(n)
      result.sym = s
      result.typ = s.typ

      if containsOrIncl(names, s.name.id):
        # a field with the same name already exists
        localReport(cl.c.config, n.info):
          SemReport(kind: rsemRedefinitionOf, sym: s)

  of nkRecList:
    result = copyNode(n)
    for it in n.items:
      let r = recurse(it)
      if r.kind != nkRecList or r.len > 0:
        # eliminate empty sub nodes by not adding them
        result.add(r)

  of nkRecCase:
    result = shallowCopy(n)
    result[0] = recurse(n[0]) # discriminator
    for i in 1..<n.len:
      let branch = n[i]
      # leave the labels as is, only the last node needs to be
      # instantiated
      result[i] = copyTreeWithoutNode(branch, branch[^1])
      result[i].add recurse(branch[^1])
  of nkRecWhen:
    # select the active branch (or nothing)
    var branch = PNode(nil)
    for i, it in n.pairs:
      case it.kind
      of nkElifBranch:
        # replace the type variables (if any) in the expression and then
        # evaluate it
        let e = cl.c.semConstBoolExpr(cl.c, prepareNode(cl, it[0]))
        # XXX: proper error propagation is missing
        if e.kind == nkIntLit and e.intVal != 0 and branch == nil:
          branch = it[1]
      of nkElse:
        if branch == nil:
          branch = it[0]
      else:
        unreachable(it.kind)

    if branch != nil:
      result = recurse(branch)
    else:
      # no branch was selected; the 'when' is collapsed to nothing
      result = newNodeI(nkRecList, n.info)
  else:
    unreachable()

proc lookupTypeVar(cl: var TReplTypeVars, t: PType): PType =
  result = cl.typeMap.lookup(t)
  if result == nil:
    if tfRetType in t.flags: return
    cl.c.config.localReport(t.sym.info, reportTyp(rsemCannotInstantiate, t))
    result = errorType(cl.c)
    # In order to prevent endless recursions, we must remember
    # this bad lookup and replace it with errorType everywhere.
    # These code paths are only active in "nim check"
    cl.typeMap.put(t, result)
  elif result.kind == tyGenericParam:
    cl.c.config.internalError("substitution with generic parameter")

proc instCopyType*(cl: var TReplTypeVars, t: PType): PType =
  if true:
    result = copyType(t, nextTypeId(cl.c.idgen), t.owner)
    copyTypeProps(cl.c.graph, cl.c.idgen.module, result, t)

  result.flags.incl tfFromGeneric
  if not (t.kind in tyMetaTypes or
         (t.kind == tyStatic and t.n == nil)):
    result.flags.excl tfInstClearedFlags
  else:
    result.flags.excl tfHasAsgn

proc instantiate(cl: var TReplTypeVars, t: PType): PType =
  ## Instatiates the body of a parametric type.
  case t.kind
  of tyRef, tyPtr:
    if tfRefsAnonObj in t.flags:
      # this is a ``ref|ptr object`` type
      result = instCopyType(cl, t)
      result[^1] = instantiate(cl, t[^1])
      propagateToOwner(result, result[^1])
    else:
      # a structural type
      result = replaceTypeVarsT(cl, t)
  of tyObject:
    # create a proper instance of the type. A new instance is created
    # even if the body doesn't contain any type parameters, so that
    # phantom types work properly
    var
      names = initIntSet()
      pos   = 0

    result = instCopyType(cl, t)

    if t.len > 0 and t.base != nil:
      # the base may be something depending on type variables
      var base = replaceTypeVarsT(cl, t.base).skipTypes({tyAlias})
      let skipped = skipToObject(base)

      if skipped.kind == tyError:
        base = nil
      elif skipped.kind != tyObject:
        # the base is not an object type
        localReport(cl.c.config, cl.info):
          SemReport(kind: rsemExpectObjectForBase)
        base = nil
      elif tfFinal in skipped.flags:
        localReport(cl.c.config, cl.info):
          SemReport(kind: rsemExpectNonFinalForBase, typ: base)
        base = nil
      elif base.kind in {tyPtr, tyRef}:
        # if the replaced-with type is a direct ref/ptr, we skip it
        base = base[^1]

      result[0] = base
      if base != nil:
        propagateToOwner(result, base)
        # add the fields to check:
        pos = addInheritedFields(names, skipped)

    # --- setup the symbol:
    result.sym = copySym(t.sym, nextSymId cl.c.idgen)
    result.sym.flags.incl sfFromGeneric
    result.sym.owner = t.sym   # the owner is the generic type
    result.sym.ast = t.sym.ast # points to the original AST
    result.sym.typ = result

    # --- instantiate the record AST:
    result.n = instantiateRecord(cl, names, pos, t.n, result.sym)
    result.size = szUncomputedSize # size needs to be recomputed now

    if cl.c.computeRequiresInit(cl.c, result):
      result.flags.incl tfRequiresInit
  of tyDistinct:
    # same as with object types, create a new type instance even if the body
    # doesn't change during instantiation
    result = instCopyType(cl, t)
    result[0] = replaceTypeVarsT(cl, t[0])

    # always create a new symbol, even if
    result.sym = copySym(t.sym, nextSymId cl.c.idgen)
    result.sym.flags.incl sfFromGeneric
    result.sym.owner = t.sym
    result.sym.ast = t.sym.ast
    result.sym.typ = result
  else:
    # XXX: these types also need new symbols...
    result = replaceTypeVarsT(cl, t)

proc handleGenericInvocation(cl: var TReplTypeVars, t: PType): PType =
  ## Evaluates a type application (a ``tyGenericInvocation``) and returns
  ## the resulting ``tyGenericInst``. The result is cached, and evaluating
  ## the type application again won't create a new instance, but will instead
  ## return the cached instance.
  let body = t.base
  cl.c.config.internalAssert(body.kind == tyGenericBody, cl.info, "no generic body")
  var header = t

  let oldSkipTypedesc = cl.skipTypedesc
  cl.skipTypedesc = true

  # instantiate all invocation arguments:
  for i in 1..<t.len:
    let it = t[i]
    var x =
      if it.kind == tyGenericParam:
        lookupTypeVar(cl, it)
      else:
        replaceTypeVarsT(cl, it)

    if x.kind == tyTypeDesc:
      # we want the unwrapped type in argument positions
      x = x.base

    if x != it:
      if header == t:
        # optimization: only create a copy when something changes. We only
        # use the header for the cache lookup, so an exact replica suffices
        header = exactReplica(t)
      header[i] = x
      propagateToOwner(header, x)

  cl.skipTypedesc = oldSkipTypedesc

  # search the instance cache for the type:
  result = searchInstTypes(cl.c.graph, header)
  if result != nil:
    when defined(reportCacheHits):
      echo "Generic instantiation cached ", typeToString(result), " for ",
        typeToString(t), " header ", typeToString(header)
    return

  # not cached yet
  result = newType(tyGenericInst, nextTypeId(cl.c.idgen), t[0].owner)
  # inherit the flags relevant to type equality before recursing:
  result.flags = header.flags * eqTypeFlags
  # be careful not to propagate unnecessary flags here (don't use rawAddSon)
  result.sons = @[header[0]]

  for i in 1..<t.len:
    # if one of the params is not concrete, we cannot do anything
    # but we already raised an error!
    rawAddSon(result, header[i], propagateHasAsgn = false)

  # cache the incomplete instance *before* instantiating the body, so that
  # recursive instantiation works
  cacheTypeInst(cl.c, result)

  cl.typeMap = newTypeMapLayer(cl)

  for i in 1..<t.len:
    # bind the arguments to the body's parameters
    cl.typeMap.put(body[i-1], header[i])

  let bbody = lastSon body
  var newbody = instantiate(cl, bbody)
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
  if true:
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

    let dc = cl.c.graph.getAttachedOp(newbody, attachedDeepCopy)
    if dc != nil and sfFromGeneric notin dc.flags:
      # 'deepCopy' needs to be instantiated for generics *when the type is
      # constructed* but *after* the type's `typeInst` field is set:
      cl.c.graph.setAttachedOp(cl.c.module.position, newbody, attachedDeepCopy,
          cl.c.instTypeBoundOp(cl.c, dc, result, cl.info, attachedDeepCopy, 1))

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

proc eraseVoidTypes(t: PType; start = 0) =
  ## Removes all ``tyVoid`` items from `t`. If `t` has attached AST, the slots
  ## corresponding to the type items are removed too.
  for i in start..<t.len:
    # don't touch any memory unless necessary
    if t[i].kind == tyVoid:
      var pos = i
      for j in i+1..<t.len:
        if t[j].kind != tyVoid:
          t[pos] = t[j]
          if t.n != nil:
            t.n[pos] = t.n[j]
          inc pos
      setLen t.sons, pos
      if t.n != nil:
        setLen t.n.sons, pos
      break

proc eraseVoidParams*(t: PType) =
  # transform '(): void' into '()' because old parts of the compiler really
  # don't deal with '(): void':
  if t[0] != nil and t[0].kind == tyVoid:
    t[0] = nil

  eraseVoidTypes(t, start=1)

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

proc replaceTypeVarsTAux(cl: var TReplTypeVars, t: PType): PType =
  template bailout =
    if cl.recursionLimit > 100:
      # too nested instantiation
      cl.c.config.localReport(cl.info, reportTyp(rsemCannotInstantiate, t))
      return errorType(cl.c)
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
    for i in 1..<result.len:
      result[i] = replaceTypeVarsT(cl, result[i])
    propagateToOwner(result, result.lastSon)

  of tyGenericInst, tyObject:
    # this must be a fully resolved concrete type
    doAssert(not containsGenericType(t))
    result = t

  else:
    if containsGenericType(t):
      bailout()
      result = instCopyType(cl, t)
      result.size = -1 # needs to be recomputed

      for i in 0..<result.len:
        if result[i] != nil:
          let r = replaceTypeVarsT(cl, result[i])
          result[i] = r
          if result.kind != tyArray or i != 0:
            propagateToOwner(result, r)
      case result.kind
      of tyArray:
        let idx = result[0]
        internalAssert(cl.c.config, idx.kind != tyStatic, "[FIXME]")

      of tyTuple:
        if result.n != nil:
          # update the record description
          result.n = shallowCopy(t.n)
          var pos = 0
          for i, it in t.n.pairs:
            # don't copy void fields, they're removed afterwards
            if result[i].kind != tyVoid:
              # always copy the symbol to make sure we can modify it
              let s = copySym(it.sym, nextSymId cl.c.idgen)
              s.typ = result[i]
              incl(s.flags, sfFromGeneric)
              s.position = pos
              inc pos
              result.n[i] = newSymNode(s, it.info)

        # now erase the void types, which will also eliminate the empty slots
        eraseVoidTypes(result)

      of tyProc:
        # bug #4677: Do not instantiate effect lists
        result.n = replaceTypeVarsN(cl, result.n, 1)
        eraseVoidParams(result)
        skipIntLiteralParams(result, cl.c.idgen)

      of tyRange:
        result.n = replaceTypeVarsN(cl, result.n, 0)
        result[0] = result[0].skipTypes({tyStatic, tyDistinct})

      else: discard

proc replaceTypeParamsInType*(c: PContext, pt: TIdTable, t: PType): PType =
  ## Replaces with their bound type, if a binding exists, type parameters in
  ## the type `t`. Only type parameters inside the *types* and attached AST
  ## of ``tyRange``s and ``tyTypeFrom`` are replaced -- the AST of, for
  ## example, named tuple types or object types is not adjusted.
  ##
  ## `t` is not modified directly. Instead, an exact replica (which includes
  ## the type ID) is first created prior to something being replaced. If
  ## nothing is replaced, the returned type is exactly `t`.
  ##
  ## If something was replaced, the returned type is **not** a proper
  ## instantiated one and thus must not be used in contexts where a
  ## proper type is expected.
  template update(i: int, with: PType) =
    let w = with
    if result[i] != w:
      if result == t: # copy on write
        result = t.exactReplica
      result[i] = w

  const AndOrNot = {tyAnd, tyOr, tyNot}

  case t.kind
  of tyGenericParam, tyStatic:
    # a type variable -> replace it with the currently bound type
    result = PType(idTableGet(pt, t))
    if result == nil:
      result = t
  of tyGenericInvocation:
    # update the arguments:
    result = t
    for i in 1..<t.len:
      update(i): replaceTypeParamsInType(c, pt, t[i])

  of tyTypeDesc, tyArray, tySet, tySink, tyTuple, tyVar, tyLent,
     tyPtr, tyRef, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray,
     tyOrdinal, tyDistinct, AndOrNot:
    # types that can contain type variables and don't require special handling
    result = t
    # update type parameters:
    for i in 0..<t.len:
      update(i): replaceTypeParamsInType(c, pt, t[i])

  of tyProc, tyObject:
    # types that can have 'nil' elements
    result = t
    # update type parameters:
    for i in 0..<t.len:
      if t[i] != nil:
        update(i): replaceTypeParamsInType(c, pt, t[i])

  of tyRange:
    # their attached AST defines range type, meaning that we have to update
    # it
    let
      n    = replaceTypeVarsInBody(c, pt, t.n)
      base = replaceTypeParamsInType(c, pt, t.base)
    if n == t.n and base == t.base:
      result = t # nothing changed
    else:
      result = t.exactReplica
      result[0] = base
      result.n = n
  of tyFromExpr:
    # has no type parameters, but does have AST that contains type
    # variables
    let n = replaceTypeVarsInBody(c, pt, t.n)
    if n == t.n:
      result = t # nothing changed
    else:
      result = t.exactReplica
      result.n = n
  of tyGenericInst, tyInferred, tyTypeClasses - AndOrNot, tyError:
    # resolved types and meta types that don't contain type variables
    result = t
  of IntegralTypes, tyVoid, tyPointer, tyString, tyCstring,
     tyForward, tyNone, tyEmpty, tyAlias, tyNil, tyUntyped, tyTyped:
    # non-parametric types (i.e., types that cannot contain type variables)
    result = t
  of tyGenericBody:
    unreachable("shouldn't reach here")

proc initTypeVars*(p: PContext, typeMap: LayeredIdTable, info: TLineInfo;
                   owner: PSym): TReplTypeVars =
  initIdTable(result.symMap)
  result.typeMap = typeMap
  result.info = info
  result.c = p
  result.owner = owner

proc instantiateTypesInBody*(p: PContext, pt: TIdTable, n: PNode;
                         owner: PSym): PNode =
  ## Instantiates all types referenced in the generic AST `n`. `pt` provides
  ## the bindings for all used type variables (i.e., generic parameters and
  ## type-classes) -- missing bindings result in an error.
  var typeMap = initLayeredTypeMap(pt)
  var cl = initTypeVars(p, typeMap, n.info, owner)
  pushInfoContext(p.config, n.info)
  result = replaceTypeVarsN(cl, n)
  popInfoContext(p.config)

proc replaceTypeVarsInBody*(c: PContext, pt: TIdTable, n: PNode): PNode =
  ## Replaces with their bound types (provided by `pt`) all type variables in
  ## the generic AST `n`, with unbound type variables being ignored. Generic
  ## types are not instantiated and static expression not evaluated.
  ##
  ## Use this procedure instead of ``instantiateTypesInBody`` when it's not
  ## guaranteed whether all type variables have been resolved.
  template lookup(t: PType): PType = PType(idTableGet(pt, t))

  case n.kind
  of nkSym:
    let s = n.sym
    if isUnresolvedSym(s) and (let t = lookup(s.typ); t != nil):
      # we've found a type variable that has something bound to it
      if t.kind == tyStatic and t.n != nil:
        # replace ``static`` types with their computed value or unresolved
        # expression
        if tfUnresolved in t.flags:
          # also process the unresolved expression
          result = replaceTypeVarsInBody(c, pt, t.n)
        else:
          result = t.n
      else:
        # don't modify the existing symbol; create a copy first
        let copy = copySym(s, nextSymId(c.idgen))
        copy.owner = s.owner
        copy.typ = t

        # transition ``skGenericParam`` and ``skParam``s to ``skType``,
        # replicating how they would look like in other normal instantiated
        # generic AST
        if t.kind notin {tyStatic, tyGenericParam}:
          transitionGenericParamToType(copy)

        result = copyNode(n)
        result.sym = copy
        result.typ = fixType(c, s, t)
    else:
      result = n

  of nkWithoutSons - {nkSym}:
    result = n
  of nkWithSons:
    # XXX: can be optimized by only copying `n` when some sub node
    #      changes (i.e., copy on write)
    result = shallowCopy(n)
    # HACK: we de-type the expression here. This is necessary because some
    #       expressions are getting types assigned to them, which would then
    #       prevent the expression from getting properly analyzed when used
    #       as a call operands. Ultimately, the producers of the generic
    #       expressions (e.g., ``semRangeAux``) need to make sure that
    #       the expressions stay untyped (apart from the type
    #       variables)
    result.typ = nil
    for i, it in n.pairs:
      result[i] = replaceTypeVarsInBody(c, pt, it)

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

template generateTypeInstance*(p: PContext, pt: TIdTable, arg: PNode,
                               t: PType): untyped =
  generateTypeInstance(p, pt, arg.info, t)

proc containsUnboundTypeVar(pt: TIdTable, t: PType): bool =
  ## Given the type `t`, answers the question of whether `t` either is or uses
  ## an unbound type variable somewhere. `pt` provides the bindings.
  proc nodeContainsUnbound(pt: TIdTable, n: PNode): bool =
    case n.kind
    of nkSym:
      result = isUnresolvedSym(n.sym) and idTableGet(pt, n.sym.typ) == nil
    of nkWithoutSons - {nkSym}:
      result = false
    of nkWithSons:
      for it in n.items:
        if nodeContainsUnbound(pt, it):
          return true

  case t.kind
  of tyStatic, tyGenericParam, tyTypeDesc, tyTypeClasses:
    # a type variable; it's an unbound one if it's not in the lookup table
    result = idTableGet(pt, t) == nil
  of tyGenericBody:
    # this can only be a composite type-class that wasn't lifted
    result = true
  of tyGenericInvocation:
    # we only look at the arguments and ignore the body. If the body is a
    # meta-type, that's fine; we don't guarantee that the resulting type is
    # not a meta-type
    result = false
    for i in 1..<t.len:
      if containsUnboundTypeVar(pt, t[i]):
        result = true
        break

  of tyDistinct, tyArray, tySet, tySink, tyTuple, tyVar, tyLent, tyPtr, tyRef,
     tySequence, tyOpenArray, tyVarargs, tyUncheckedArray,
     tyProc, tyOrdinal, tyAlias, tyObject:
    # can contain type variables
    for it in t.sons.items:
      # the element can be nil for proc types
      if it != nil and containsUnboundTypeVar(pt, it):
        result = true
        break

  of tyRange:
    result = containsUnboundTypeVar(pt, t) or nodeContainsUnbound(pt, t.n)
  of tyFromExpr:
    result = nodeContainsUnbound(pt, t.n)
  of tyGenericInst, tyInferred:
    result = containsUnboundTypeVar(pt, t[^1])
  of tyNone, tyEmpty, tyError, IntegralTypes, tyNil, tyUntyped, tyTyped,
     tyPointer, tyString, tyCstring, tyForward, tyVoid:
    # neither a type variables nor can it contain one
    result = false

proc tryGenerateInstance*(c: PContext, pt: TIdTable, info: TLineInfo, t: PType): PType =
  ## Tries to resolve the generic type `t` to a non-generic one, using the type
  ## bindings provided by `pt`. Returns `nil` on failure, and the resolved type
  ## on success. Do note that the returned type can still be a meta type.
  ##
  ## XXX: this procedure is only a workaround. ``replaceTypeVarsT`` should
  ##      properly support the case where it's not certain whether all
  ##      referenced type parameters are resolved already
  assert containsGenericType(t)
  if containsUnboundTypeVar(pt, t):
    result = nil
  else:
    result = generateTypeInstance(c, pt, info, t)
