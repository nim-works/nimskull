#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the signature matching for resolving
## the call to overloaded procs, generic procs and operators.
import
  std/[
    intsets,
  ],
  compiler/ast/[
    ast,
    astalgo,
    types,
    renderer,
    idents,
    trees,
    lineinfos,
    errorreporting,
    errorhandling,
    reports,
  ],
  compiler/modules/[
    modulegraphs,
    magicsys,
  ],
  compiler/front/[
    msgs,
    options,
  ],
  compiler/sem/[
    semdata,
    semtypinst,
    lookups,
    lowerings,
    parampatterns,
  ],
  compiler/utils/[
    debugutils,
  ]


type
  TCandidateState* = enum
    csEmpty, csMatch, csNoMatch

  TCandidate* = object
    c*: PContext
    exactMatches*: int       ## also misused to prefer iters over procs
    genericMatches: int      ## also misused to prefer constraints
    subtypeMatches: int
    intConvMatches: int      ## conversions to int are not as expensive
    convMatches: int
    state*: TCandidateState
    callee*: PType           ## may not be nil!
    calleeSym*: PSym         ## may be nil
    calleeScope*: int        ## scope depth:
                             ## is this a top-level symbol or a nested proc?
    call*: PNode             ## modified call
    bindings*: TIdTable      ## maps types to types
    magic*: TMagic           ## magic of operation
    baseTypeMatch: bool      ## needed for conversions from T to openarray[T]
                             ## for example
    fauxMatch*: TTypeKind    ## the match was successful only due to the use
                             ## of error or wildcard (unknown) types.
                             ## this is used to prevent instantiations.
    genericConverter*: bool  ## true if a generic converter needs to
                             ## be instantiated
    coerceDistincts*: bool   ## this is an explicit coercion that can strip away
                             ## a distrinct type
    typedescMatched*: bool
    isNoCall*: bool          ## misused for generic type instantiations C[T]
    inferredTypes: seq[PType] ## inferred types during the current signature
                              ## matching. they will be reset if the matching
                              ## is not successful. may replace the bindings
                              ## table in the future.
    inheritancePenalty: int   ## to prefer closest father object type
    error*: SemCallMismatch

  TTypeRelFlag* = enum
    trDontBind
    trNoCovariance
    trBindGenericParam  ## bind tyGenericParam even with trDontBind

  TTypeRelFlags* = set[TTypeRelFlag]

const
  isNilConversion = isConvertible # maybe 'isIntConv' fits better?

proc toDebugCallableCandidate*(c: TCandidate): DebugCallableCandidate =
  DebugCallableCandidate(
    state: $c.state,
    callee: c.callee,
    calleeSym: c.calleeSym,
    calleeScope: c.calleeScope,
    call: c.call,
    error: c.error
  )

template hasFauxMatch*(c: TCandidate): bool = c.fauxMatch != tyNone

proc put(c: var TCandidate, key, val: PType) {.inline.} =
  ## Given: proc foo[T](x: T); foo(4)
  ## key: 'T'
  ## val: 'int' (typeof(4))
  when false:
    let old = PType(idTableGet(c.bindings, key))
    if old != nil:
      echo "Putting ", typeToString(key), " ", typeToString(val), " and old is ", typeToString(old)
      if typeToString(old) == "float32":
        writeStackTrace()
    if c.c.module.name.s == "temp3":
      echo "binding ", key, " -> ", val
  idTablePut(c.bindings, key, val.skipIntLit(c.c.idgen))

proc initCandidate*(ctx: PContext, c: var TCandidate, callee: PType) =
  c.c = ctx
  c.exactMatches = 0
  c.subtypeMatches = 0
  c.convMatches = 0
  c.intConvMatches = 0
  c.genericMatches = 0
  c.state = csEmpty
  c.callee = callee
  c.calleeSym = nil
  c.call = nil
  c.baseTypeMatch = false
  c.genericConverter = false
  c.inheritancePenalty = 0
  c.error = SemCallMismatch()
  initIdTable(c.bindings)

proc initCandidate*(ctx: PContext, c: var TCandidate, callee: PSym,
                    binding: PNode, calleeScope = -1) =
  initCandidate(ctx, c, callee.typ)
  c.calleeSym = callee
  if callee.kind in skProcKinds and calleeScope == -1:
    if callee.originatingModule == ctx.module:
      c.calleeScope = 2
      var owner = callee
      while true:
        owner = owner.skipGenericOwner
        if owner.kind == skModule: break
        inc c.calleeScope
    else:
      c.calleeScope = 1
  else:
    c.calleeScope = calleeScope
  c.magic = c.calleeSym.magic
  if binding != nil and callee.kind in routineKinds:
    var typeParams = callee.ast[genericParamsPos]
    for i in 1..min(typeParams.safeLen, binding.safeLen-1):
      var formalTypeParam = typeParams[i-1].typ
      var bound = binding[i].typ
      if bound != nil:
        if formalTypeParam.kind == tyTypeDesc:
          if bound.kind != tyTypeDesc:
            bound = makeTypeDesc(ctx, bound)
        else:
          bound = bound.skipTypes({tyTypeDesc})
        put(c, formalTypeParam, bound)

proc newCandidate*(ctx: PContext, callee: PSym,
                   binding: PNode, calleeScope = -1): TCandidate =
  initCandidate(ctx, result, callee, binding, calleeScope)

proc newCandidate*(ctx: PContext, callee: PType): TCandidate =
  initCandidate(ctx, result, callee)

proc copyCandidate(a: var TCandidate, b: TCandidate) =
  a.c = b.c
  a.exactMatches = b.exactMatches
  a.subtypeMatches = b.subtypeMatches
  a.convMatches = b.convMatches
  a.intConvMatches = b.intConvMatches
  a.genericMatches = b.genericMatches
  a.state = b.state
  a.callee = b.callee
  a.calleeSym = b.calleeSym
  a.call = copyTree(b.call)
  a.baseTypeMatch = b.baseTypeMatch
  copyIdTable(a.bindings, b.bindings)

proc typeRel*(c: var TCandidate, f, aOrig: PType,
              flags: TTypeRelFlags = {}): TTypeRelation

proc checkGeneric(a, b: TCandidate): int =
  let c = a.c
  let aa = a.callee
  let bb = b.callee
  var winner = 0
  for i in 1..<min(aa.len, bb.len):
    var ma = newCandidate(c, bb[i])
    let tra = typeRel(ma, bb[i], aa[i], {trDontBind})
    var mb = newCandidate(c, aa[i])
    let trb = typeRel(mb, aa[i], bb[i], {trDontBind})
    if tra == isGeneric and trb == isNone:
      if winner == -1: return 0
      winner = 1
    if trb == isGeneric and tra == isNone:
      if winner == 1: return 0
      winner = -1
  result = winner

proc sumGeneric(t: PType): int =
  # count the "genericness" so that Foo[Foo[T]] has the value 3
  # and Foo[T] has the value 2 so that we know Foo[Foo[T]] is more
  # specific than Foo[T].
  var t = t
  var isvar = 1
  while true:
    case t.kind
    of tyGenericInst, tyArray, tyRef, tyPtr, tyDistinct, tyUncheckedArray,
        tyOpenArray, tyVarargs, tySet, tyRange, tySequence, tyGenericBody,
        tyLent, tyOwned:
      t = t.lastSon
      inc result
    of tyOr:
      var maxBranch = 0
      for branch in t.sons:
        let branchSum = sumGeneric(branch)
        if branchSum > maxBranch: maxBranch = branchSum
      inc result, maxBranch
      break
    of tyVar:
      t = t[0]
      inc result
      inc isvar
    of tyTypeDesc:
      t = t.lastSon
      if t.kind == tyEmpty: break
      inc result
    of tyGenericInvocation, tyTuple, tyProc, tyAnd:
      result += ord(t.kind in {tyGenericInvocation, tyAnd})
      for i in 0..<t.len:
        if t[i] != nil:
          result += sumGeneric(t[i])
      break
    of tyStatic:
      return sumGeneric(t[0]) + 1
    of tyGenericParam, tyUntyped, tyTyped: break
    of tyAlias, tySink: t = t.lastSon
    of tyBool, tyChar, tyEnum, tyObject, tyPointer,
        tyString, tyCstring, tyInt..tyInt64, tyFloat..tyFloat128,
        tyUInt..tyUInt64, tyCompositeTypeClass:
      return isvar
    else:
      return 0

proc complexDisambiguation(a, b: PType): int =
  # 'a' matches better if *every* argument matches better or equal than 'b'.
  var winner = 0
  for i in 1..<min(a.len, b.len):
    let x = a[i].sumGeneric
    let y = b[i].sumGeneric
    if x != y:
      if winner == 0:
        if x > y: winner = 1
        else: winner = -1
      elif x > y:
        if winner != 1:
          # contradiction
          return 0
      else:
        if winner != -1:
          return 0
  result = winner
  when false:
    var x, y: int
    for i in 1..<a.len: x += a[i].sumGeneric
    for i in 1..<b.len: y += b[i].sumGeneric
    result = x - y

proc writeMatches*(c: TCandidate) =
  echo "Candidate '", c.calleeSym.name.s, "' at ", c.c.config $ c.calleeSym.info
  echo "  exact matches: ", c.exactMatches
  echo "  generic matches: ", c.genericMatches
  echo "  subtype matches: ", c.subtypeMatches
  echo "  intconv matches: ", c.intConvMatches
  echo "  conv matches: ", c.convMatches
  echo "  inheritance: ", c.inheritancePenalty

proc cmpCandidates*(a, b: TCandidate): int =
  result = a.exactMatches - b.exactMatches
  if result != 0: return
  result = a.genericMatches - b.genericMatches
  if result != 0: return
  result = a.subtypeMatches - b.subtypeMatches
  if result != 0: return
  result = a.intConvMatches - b.intConvMatches
  if result != 0: return
  result = a.convMatches - b.convMatches
  if result != 0: return
  # the other way round because of other semantics:
  result = b.inheritancePenalty - a.inheritancePenalty
  if result != 0: return
  # check for generic subclass relation
  result = checkGeneric(a, b)
  if result != 0: return
  # prefer more specialized generic over more general generic:
  result = complexDisambiguation(a.callee, b.callee)
  # only as a last resort, consider scoping:
  if result != 0: return
  result = a.calleeScope - b.calleeScope


proc concreteType(c: TCandidate, t: PType; f: PType = nil): PType =
  case t.kind
  of tyTypeDesc:
    if c.isNoCall: result = t
    else: result = nil
  of tySequence, tySet:
    if t[0].kind == tyEmpty: result = nil
    else: result = t
  of tyGenericParam, tyAnything:
    result = t
    while true:
      result = PType(idTableGet(c.bindings, t))
      if result.isNil():
        break # it's ok, no match
        # example code that triggers it:
        # proc sort[T](cmp: proc(a, b: T): int = cmp)
      if result.kind != tyGenericParam: break
  of tyGenericInvocation:
    result = nil
  of tyOwned:
    # bug #11257: the comparison system.`==`[T: proc](x, y: T) works
    # better without the 'owned' type:
    if f != nil and f.len > 0 and f[0].skipTypes({tyBuiltInTypeClass}).kind == tyProc:
      result = t.lastSon
    else:
      result = t
  else:
    result = t                # Note: empty is valid here

proc handleRange(f, a: PType, min, max: TTypeKind): TTypeRelation =
  if a.kind == f.kind:
    result = isEqual
  else:
    let ab = skipTypes(a, {tyRange})
    let k = ab.kind
    if k == f.kind: result = isSubrange
    elif k == tyInt and f.kind in {tyRange, tyInt8..tyInt64,
                                   tyUInt..tyUInt64} and
        isIntLit(ab) and getInt(ab.n) >= firstOrd(nil, f) and
                         getInt(ab.n) <= lastOrd(nil, f):
      # passing 'nil' to firstOrd/lastOrd here as type checking rules should
      # not depend on the target integer size configurations!
      # integer literal in the proper range; we want ``i16 + 4`` to stay an
      # ``int16`` operation so we declare the ``4`` pseudo-equal to int16
      result = isFromIntLit
    elif f.kind == tyInt and k in {tyInt8..tyInt32}:
      result = isIntConv
    elif f.kind == tyUInt and k in {tyUInt8..tyUInt32}:
      result = isIntConv
    elif k >= min and k <= max:
      result = isConvertible
    elif a.kind == tyRange and
      # Make sure the conversion happens between types w/ same signedness
      (f.kind in {tyInt..tyInt64} and a[0].kind in {tyInt..tyInt64} or
       f.kind in {tyUInt8..tyUInt32} and a[0].kind in {tyUInt8..tyUInt32}) and
      a.n[0].intVal >= firstOrd(nil, f) and a.n[1].intVal <= lastOrd(nil, f):
      # passing 'nil' to firstOrd/lastOrd here as type checking rules should
      # not depend on the target integer size configurations!
      result = isConvertible
    else: result = isNone

proc isConvertibleToRange(f, a: PType): bool =
  if f.kind in {tyInt..tyInt64, tyUInt..tyUInt64} and
     a.kind in {tyInt..tyInt64, tyUInt..tyUInt64}:
    case f.kind
    of tyInt8: result = isIntLit(a) or a.kind in {tyInt8}
    of tyInt16: result = isIntLit(a) or a.kind in {tyInt8, tyInt16}
    of tyInt32: result = isIntLit(a) or a.kind in {tyInt8, tyInt16, tyInt32}
    # This is wrong, but seems like there's a lot of code that relies on it :(
    of tyInt, tyUInt, tyUInt64: result = true
    of tyInt64: result = isIntLit(a) or a.kind in {tyInt8, tyInt16, tyInt32, tyInt, tyInt64}
    of tyUInt8: result = isIntLit(a) or a.kind in {tyUInt8}
    of tyUInt16: result = isIntLit(a) or a.kind in {tyUInt8, tyUInt16}
    of tyUInt32: result = isIntLit(a) or a.kind in {tyUInt8, tyUInt16, tyUInt32}
    #of tyUInt: result = isIntLit(a) or a.kind in {tyUInt8, tyUInt16, tyUInt32, tyUInt}
    #of tyUInt64: result = isIntLit(a) or a.kind in {tyUInt8, tyUInt16, tyUInt32, tyUInt, tyUInt64}
    else: result = false
  elif f.kind in {tyFloat..tyFloat128}:
    # `isIntLit` is correct and should be used above as well, see PR:
    # https://github.com/nim-lang/Nim/pull/11197
    result = isIntLit(a) or a.kind in {tyFloat..tyFloat128}

proc handleFloatRange(f, a: PType): TTypeRelation =
  if a.kind == f.kind:
    result = isEqual
  else:
    let ab = skipTypes(a, {tyRange})
    var k = ab.kind
    if k == f.kind: result = isSubrange
    elif isFloatLit(ab): result = isFromIntLit
    elif isIntLit(ab): result = isConvertible
    elif k >= tyFloat and k <= tyFloat128:
      # conversion to "float32" is not as good:
      if f.kind == tyFloat32: result = isConvertible
      else: result = isIntConv
    else: result = isNone

proc genericParamPut(c: var TCandidate; last, fGenericOrigin: PType) =
  if fGenericOrigin != nil and last.kind == tyGenericInst and
     last.len-1 == fGenericOrigin.len:
    for i in 1..<fGenericOrigin.len:
      let x = PType(idTableGet(c.bindings, fGenericOrigin[i]))
      if x.isNil():
        put(c, fGenericOrigin[i], last[i])

proc isObjectSubtype(c: var TCandidate; a, f, fGenericOrigin: PType): int =
  var t = a
  assert t.kind == tyObject
  var depth = 0
  var last = a
  while t != nil and not sameObjectTypes(f, t):
    assert t.kind == tyObject
    t = t[0]
    if t.isNil(): break
    last = t
    t = skipTypes(t, skipPtrs)
    inc depth
  if t != nil:
    genericParamPut(c, last, fGenericOrigin)
    result = depth
  else:
    result = -1

type
  SkippedPtr = enum skippedNone, skippedRef, skippedPtr

proc skipToObject(t: PType; skipped: var SkippedPtr): PType =
  var r = t
  # we're allowed to skip one level of ptr/ref:
  var ptrs = 0
  while r != nil:
    case r.kind
    of tyGenericInvocation:
      r = r[0]
    of tyRef:
      inc ptrs
      skipped = skippedRef
      r = r.lastSon
    of tyPtr:
      inc ptrs
      skipped = skippedPtr
      r = r.lastSon
    of tyGenericBody, tyGenericInst, tyAlias, tySink, tyOwned:
      r = r.lastSon
    else:
      break
  if r.kind == tyObject and ptrs <= 1: result = r

proc isGenericSubtype(c: var TCandidate; a, f: PType, d: var int, fGenericOrigin: PType): bool =
  assert f.kind in {tyGenericInst, tyGenericInvocation, tyGenericBody}
  var askip = skippedNone
  var fskip = skippedNone
  var t = a.skipToObject(askip)
  let r = f.skipToObject(fskip)
  if r.isNil(): return false
  var depth = 0
  var last = a
  # XXX sameObjectType can return false here. Need to investigate
  # why that is but sameObjectType does way too much work here anyway.
  while t != nil and r.sym != t.sym and askip == fskip:
    t = t[0]
    if t.isNil(): break
    last = t
    t = t.skipToObject(askip)
    inc depth
  if t != nil and askip == fskip:
    genericParamPut(c, last, fGenericOrigin)
    d = depth
    result = true

proc minRel(a, b: TTypeRelation): TTypeRelation =
  if a <= b: result = a
  else: result = b

proc recordRel(c: var TCandidate, f, a: PType): TTypeRelation =
  result = isNone
  if sameType(f, a):
    result = isEqual
  elif a.len == f.len:
    result = isEqual
    let firstField = if f.kind == tyTuple: 0
                     else: 1
    for i in firstField..<f.len:
      var m = typeRel(c, f[i], a[i])
      if m < isSubtype: return isNone
      result = minRel(result, m)
    if f.n != nil and a.n != nil:
      for i in 0..<f.n.len:
        # check field names:
        if f.n[i].kind != nkSym: return isNone
        elif a.n[i].kind != nkSym: return isNone
        else:
          var x = f.n[i].sym
          var y = a.n[i].sym
          if f.kind == tyObject and typeRel(c, x.typ, y.typ) < isSubtype:
            return isNone
          if x.name.id != y.name.id: return isNone

proc allowsNil(f: PType): TTypeRelation {.inline.} =
  result = if tfNotNil notin f.flags: isSubtype else: isNone

proc inconsistentVarTypes(f, a: PType): bool {.inline.} =
  result = f.kind != a.kind and
    (f.kind in {tyVar, tyLent, tySink} or a.kind in {tyVar, tyLent, tySink})

proc procParamTypeRel(c: var TCandidate, f, a: PType): TTypeRelation =
  ## For example we have:
  ##
  ## .. code-block:: nim
  ##   proc myMap[T,S](sIn: seq[T], f: proc(x: T): S): seq[S] = ...
  ##   proc innerProc[Q,W](q: Q): W = ...
  ##
  ## And we want to match: myMap(@[1,2,3], innerProc)
  ## This proc (procParamTypeRel) will do the following steps in
  ## three different calls:
  ## - matches f=T to a=Q. Since f is metatype, we resolve it
  ##    to int (which is already known at this point). So in this case
  ##    Q=int mapping will be saved to c.bindings.
  ## - matches f=S to a=W. Both of these metatypes are unknown, so we
  ##    return with isBothMetaConvertible to ask for rerun.
  ## - matches f=S to a=W. At this point the return type of innerProc
  ##    is known (we get it from c.bindings). We can use that value
  ##    to match with f, and save back to c.bindings.
  var
    f = f
    a = a

  if a.isMetaType:
    let aResolved = PType(idTableGet(c.bindings, a))
    if aResolved != nil:
      a = aResolved
  if a.isMetaType:
    if f.isMetaType:
      # We are matching a generic proc (as proc param)
      # to another generic type appearing in the proc
      # signature. There is a chance that the target
      # type is already fully-determined, so we are
      # going to try resolve it
      if c.call != nil:
        f = generateTypeInstance(c.c, c.bindings, c.call.info, f)
      else:
        f = nil
      if f.isNil() or f.isMetaType:
        # no luck resolving the type, so the inference fails
        return isBothMetaConvertible
    # Note that this typeRel call will save a's resolved type into c.bindings
    let reverseRel = typeRel(c, a, f)
    if reverseRel >= isGeneric:
      result = isInferred
      #inc c.genericMatches
  else:
    # Note that this typeRel call will save f's resolved type into c.bindings
    # if f is metatype.
    result = typeRel(c, f, a)

  if result <= isSubrange or inconsistentVarTypes(f, a):
    result = isNone

  #if result == isEqual:
  #  inc c.exactMatches

proc procTypeRel(c: var TCandidate, f, a: PType): TTypeRelation =
  case a.kind
  of tyProc:
    if f.len != a.len: return
    result = isEqual      # start with maximum; also correct for no
                          # params at all

    template checkParam(f, a) =
      result = minRel(result, procParamTypeRel(c, f, a))
      if result == isNone: return

    # Note: We have to do unification for the parameters before the
    # return type!
    for i in 1..<f.len:
      checkParam(f[i], a[i])

    if f[0] != nil:
      if a[0] != nil:
        checkParam(f[0], a[0])
      else:
        return isNone
    elif a[0] != nil:
      return isNone

    result = getProcConvMismatch(c.c.config, f, a, result)[1]

    when useEffectSystem:
      if compatibleEffects(f, a) != efCompat:
        return isNone

  of tyNil:
    result = f.allowsNil
  else: discard

proc typeRangeRel(f, a: PType): TTypeRelation {.noinline.} =
  template checkRange[T](a0, a1, f0, f1: T): TTypeRelation =
    if a0 == f0 and a1 == f1:
      isEqual
    elif a0 >= f0 and a1 <= f1:
      isConvertible
    elif a0 <= f1 and f0 <= a1:
      # X..Y and C..D overlap iff (X <= D and C <= Y)
      isConvertible
    else:
      isNone

  if f.isOrdinalType:
    checkRange(firstOrd(nil, a), lastOrd(nil, a), firstOrd(nil, f), lastOrd(nil, f))
  else:
    checkRange(firstFloat(a), lastFloat(a), firstFloat(f), lastFloat(f))


proc matchUserTypeClass*(m: var TCandidate; ff, a: PType): PType =
  var
    c = m.c
    typeClass = ff.skipTypes({tyUserTypeClassInst})
    body = typeClass.n[3]
    matchedConceptContext: TMatchedConcept
    prevMatchedConcept = c.matchedConcept
    prevCandidateType = typeClass[0][0]

  if prevMatchedConcept != nil:
    matchedConceptContext.prev = prevMatchedConcept
    matchedConceptContext.depth = prevMatchedConcept.depth + 1
    if prevMatchedConcept.depth > 4:
      localReport(m.c.graph.config, body.info, reportAst(
        rsemTooNestedConcept, body))

      return nil

  openScope(c)
  matchedConceptContext.candidateType = a
  typeClass[0][0] = a
  c.matchedConcept = addr(matchedConceptContext)
  defer:
    c.matchedConcept = prevMatchedConcept
    typeClass[0][0] = prevCandidateType
    closeScope(c)

  var typeParams: seq[(PSym, PType)]

  if ff.kind == tyUserTypeClassInst:
    for i in 1..<(ff.len - 1):
      var
        typeParamName = ff.base[i-1].sym.name
        typ = ff[i]
        param: PSym
        alreadyBound = PType(idTableGet(m.bindings, typ))

      if alreadyBound != nil: typ = alreadyBound

      template paramSym(kind): untyped =
        newSym(kind, typeParamName, nextSymId(c.idgen), typeClass.sym, typeClass.sym.info, {})

      block addTypeParam:
        for prev in typeParams:
          if prev[1].id == typ.id:
            param = paramSym prev[0].kind
            param.typ = prev[0].typ
            break addTypeParam

        case typ.kind
        of tyStatic:
          param = paramSym skConst
          param.typ = typ.exactReplica
          #copyType(typ, nextTypeId(c.idgen), typ.owner)
          if typ.n.isNil():
            param.typ.flags.incl tfInferrableStatic
          else:
            param.ast = typ.n
        of tyUnknown:
          param = paramSym skVar
          param.typ = typ.exactReplica
          #copyType(typ, nextTypeId(c.idgen), typ.owner)
        else:
          param = paramSym skType
          param.typ = if typ.isMetaType:
                        c.newTypeWithSons(tyInferred, @[typ])
                      else:
                        makeTypeDesc(c, typ)

        typeParams.add((param, typ))

      addDecl(c, param)

  # When concept substitution is performed we sem a fake body.
  # Reports are collected in `diagnostics` for type mismatch errors,
  # `{.explain.}` or `--explain`.

  if sfExplain in typeClass.sym.flags:
    m.error.diagnosticsEnabled = true

  var diagnostics: seq[SemReport]

  # REFACTOR(nkError) Until nkError reporting is fully implemented in the
  # `sigmatch.matches` we need to rely on the report writer hack that is
  # modified during `semTryExpr`, and then moved over to the candidate
  # match data. When nkError is implemented this needs to be removed.
  let tmpHook = c.config.getReportHook()
  c.config.setReportHook(
    proc(conf: ConfigRef, report: Report): TErrorHandling =
      if report.category == repSem:
        diagnostics.add report.semReport
  )

  var checkedBody = c.semTryExpr(c, body.copyTree, {efExplain})

  c.config.setReportHook(tmpHook)

  # This is disabled because semTryExpr also runs sempass2
  # which will add the error via the report hook
  # if checkedBody != nil:
  #   for e in m.c.config.walkErrors(checkedBody):
  #     diagnostics.add c.config.getReport(e).semReport

  m.error.diag = SemDiagnostics(
    diagnosticsTarget: typeClass.sym,
    reports: diagnostics)

  if checkedBody.isNil() or checkedBody.kind == nkError:
    # xxx: return nil on nkError doesn't seem quite right, but this is a type
    return nil

  # The inferrable type params have been identified during the semTryExpr above.
  # We need to put them in the current sigmatch's binding table in order for them
  # to be resolvable while matching the rest of the parameters
  for p in typeParams:
    put(m, p[1], p[0].typ)

  if ff.kind == tyUserTypeClassInst:
    result = generateTypeInstance(c, m.bindings, typeClass.sym.info, ff)
  else:
    result = ff.exactReplica
    #copyType(ff, nextTypeId(c.idgen), ff.owner)

  result.n = checkedBody

proc shouldSkipDistinct(m: TCandidate; rules: PNode, callIdent: PIdent): bool =
  # xxx: `considerQuotedIdent` can produce an error and is not being handled
  if rules.kind == nkWith:
    for r in rules:
      if considerQuotedIdent(m.c, r) == (callIdent, nil): return true
    return false
  else:
    for r in rules:
      if considerQuotedIdent(m.c, r) == (callIdent, nil): return false
    return true

proc maybeSkipDistinct(m: TCandidate; t: PType, callee: PSym): PType =
  if t != nil and t.kind == tyDistinct and t.n != nil and
     shouldSkipDistinct(m, t.n, callee.name):
    result = t.base
  else:
    result = t

proc tryResolvingStaticExpr(c: var TCandidate, n: PNode,
                            allowUnresolved = false): PNode =
  # Consider this example:
  #   type Value[N: static[int]] = object
  #   proc foo[N](a: Value[N], r: range[0..(N-1)])
  # Here, N-1 will be initially nkStaticExpr that can be evaluated only after
  # N is bound to a concrete value during the matching of the first param.
  # This proc is used to evaluate such static expressions.
  let instantiated = replaceTypesInBody(c.c, c.bindings, n, nil,
                                        allowMetaTypes = allowUnresolved)
  result = c.c.semExpr(c.c, instantiated)

proc inferStaticParam*(c: var TCandidate, lhs: PNode, rhs: BiggestInt): bool =
  ##[

This is a simple integer arithimetic equation solver,
capable of deriving the value of a static parameter in
expressions such as `(N + 5) / 2 = rhs`

Preconditions:

* The input of this proc must be semantized
  - all templates should be expanded
  - aby constant folding possible should already be performed
* There must be exactly one unresolved static parameter

Result:
  The proc will return true if the static types was successfully
  inferred. The result will be bound to the original static type
  in the TCandidate.

  ]##
  if lhs.kind in nkCallKinds and lhs[0].kind == nkSym:
    case lhs[0].sym.magic
    of mAddI, mAddU, mInc, mSucc:
      if lhs[1].kind == nkIntLit:
        return inferStaticParam(c, lhs[2], rhs - lhs[1].intVal)
      elif lhs[2].kind == nkIntLit:
        return inferStaticParam(c, lhs[1], rhs - lhs[2].intVal)

    of mDec, mSubI, mSubU, mPred:
      if lhs[1].kind == nkIntLit:
        return inferStaticParam(c, lhs[2], lhs[1].intVal - rhs)
      elif lhs[2].kind == nkIntLit:
        return inferStaticParam(c, lhs[1], rhs + lhs[2].intVal)

    of mMulI, mMulU:
      if lhs[1].kind == nkIntLit:
        if rhs mod lhs[1].intVal == 0:
          return inferStaticParam(c, lhs[2], rhs div lhs[1].intVal)
      elif lhs[2].kind == nkIntLit:
        if rhs mod lhs[2].intVal == 0:
          return inferStaticParam(c, lhs[1], rhs div lhs[2].intVal)

    of mDivI, mDivU:
      if lhs[1].kind == nkIntLit:
        if lhs[1].intVal mod rhs == 0:
          return inferStaticParam(c, lhs[2], lhs[1].intVal div rhs)
      elif lhs[2].kind == nkIntLit:
        return inferStaticParam(c, lhs[1], lhs[2].intVal * rhs)

    of mShlI:
      if lhs[2].kind == nkIntLit:
        return inferStaticParam(c, lhs[1], rhs shr lhs[2].intVal)

    of mShrI:
      if lhs[2].kind == nkIntLit:
        return inferStaticParam(c, lhs[1], rhs shl lhs[2].intVal)

    of mAshrI:
      if lhs[2].kind == nkIntLit:
        return inferStaticParam(c, lhs[1], ashr(rhs, lhs[2].intVal))

    of mUnaryMinusI:
      return inferStaticParam(c, lhs[1], -rhs)

    of mUnaryPlusI:
      return inferStaticParam(c, lhs[1], rhs)

    else: discard

  elif lhs.kind == nkSym and lhs.typ.kind == tyStatic and lhs.typ.n.isNil():
    var inferred = newTypeWithSons(c.c, tyStatic, lhs.typ.sons)
    inferred.n = newIntNode(nkIntLit, rhs)
    put(c, lhs.typ, inferred)
    if c.c.matchedConcept != nil:
      # inside concepts, binding is currently done with
      # direct mutation of the involved types:
      lhs.typ.n = inferred.n
    return true

  return false

proc failureToInferStaticParam(conf: ConfigRef; n: PNode) =
  let staticParam = n.findUnresolvedStatic
  conf.localReport(n.info, reportSym(
    rsemCannotInferStaticValue, staticParam.sym, str = "unknown"))

proc inferStaticsInRange(c: var TCandidate,
                         inferred, concrete: PType): TTypeRelation =
  let lowerBound = tryResolvingStaticExpr(c, inferred.n[0],
                                          allowUnresolved = true)
  let upperBound = tryResolvingStaticExpr(c, inferred.n[1],
                                          allowUnresolved = true)
  template doInferStatic(e: PNode, r: Int128) =
    var exp = e
    var rhs = r
    if inferStaticParam(c, exp, toInt64(rhs)):
      return isGeneric
    else:
      failureToInferStaticParam(c.c.config, exp)

  if lowerBound.kind == nkIntLit:
    if upperBound.kind == nkIntLit:
      if lengthOrd(c.c.config, concrete) == upperBound.intVal - lowerBound.intVal + 1:
        return isGeneric
      else:
        return isNone
    doInferStatic(upperBound, lengthOrd(c.c.config, concrete) + lowerBound.intVal - 1)
  elif upperBound.kind == nkIntLit:
    doInferStatic(lowerBound, getInt(upperBound) + 1 - lengthOrd(c.c.config, concrete))

template subtypeCheck() =
  if result <= isSubrange and f.lastSon.skipTypes(abstractInst).kind in {
      tyRef, tyPtr, tyVar, tyLent, tyOwned}:
    result = isNone

proc isCovariantPtr(c: var TCandidate, f, a: PType): bool =
  # this proc is always called for a pair of matching types
  assert f.kind == a.kind

  template baseTypesCheck(lhs, rhs: PType): bool =
    lhs.kind notin {tyPtr, tyRef, tyVar, tyLent, tyOwned} and
      typeRel(c, lhs, rhs, {trNoCovariance}) == isSubtype

  case f.kind
  of tyRef, tyPtr, tyOwned:
    return baseTypesCheck(f.base, a.base)
  of tyGenericInst:
    let body = f.base
    return body == a.base and
           a.len == 3 and
           tfWeakCovariant notin body[0].flags and
           baseTypesCheck(f[1], a[1])
  else:
    return false

when false:
  proc maxNumericType(prev, candidate: PType): PType =
    let c = candidate.skipTypes({tyRange})
    template greater(s) =
      if c.kind in s: result = c
    case prev.kind
    of tyInt: greater({tyInt64})
    of tyInt8: greater({tyInt, tyInt16, tyInt32, tyInt64})
    of tyInt16: greater({tyInt, tyInt32, tyInt64})
    of tyInt32: greater({tyInt64})

    of tyUInt: greater({tyUInt64})
    of tyUInt8: greater({tyUInt, tyUInt16, tyUInt32, tyUInt64})
    of tyUInt16: greater({tyUInt, tyUInt32, tyUInt64})
    of tyUInt32: greater({tyUInt64})

    of tyFloat32: greater({tyFloat64, tyFloat128})
    of tyFloat64: greater({tyFloat128})
    else: discard

template skipOwned(a) =
  if a.kind == tyOwned: a = a.skipTypes({tyOwned, tyGenericInst})

proc typeRel(c: var TCandidate, f, aOrig: PType,
             flags: TTypeRelFlags = {}): TTypeRelation =
  ##[

typeRel can be used to establish various relationships between types:

1) When used with concrete types, it will check for type equivalence
    or a subtype relationship.

2) When used with a concrete type against a type class (such as generic
   signature of a proc), it will check whether the concrete type is a member
   of the designated type class.

3) When used with two type classes, it will check whether the types
   matching the first type class are a strict subset of the types matching
   the other. This allows us to compare the signatures of generic procs in
   order to give preferrence to the most specific one:

`seq[seq[any]]` is a strict subset of seq[any] and hence more specific.

  ]##

  result = isNone
  assert(f != nil)

  when declared(deallocatedRefId):
    let corrupt = deallocatedRefId(cast[pointer](f))
    if corrupt != 0:
      c.c.config.quitOrRaise "it's corrupt " & $corrupt

  if f.kind == tyUntyped:
    if aOrig != nil: put(c, f, aOrig)
    return isGeneric

  assert(aOrig != nil)

  var
    useTypeLoweringRuleInTypeClass = c.c.matchedConcept != nil and
                                     not c.isNoCall and
                                     f.kind != tyTypeDesc and
                                     tfExplicit notin aOrig.flags and
                                     tfConceptMatchedTypeSym notin aOrig.flags

    aOrig = if useTypeLoweringRuleInTypeClass:
          aOrig.skipTypes({tyTypeDesc})
        else:
          aOrig

  if aOrig.kind == tyInferred:
    let prev = aOrig.previouslyInferred
    if prev != nil:
      return typeRel(c, f, prev, flags)
    else:
      var candidate = f

      case f.kind
      of tyGenericParam:
        var prev = PType(idTableGet(c.bindings, f))
        if prev != nil:
          candidate = prev
      of tyFromExpr:
        let computedType = tryResolvingStaticExpr(c, f.n).typ

        case computedType.kind
        of tyTypeDesc:
          candidate = computedType.base
        of tyStatic:
          candidate = computedType
        else:
          # XXX What is this non-sense? Error reporting in signature matching?
          discard "localReport(f.n.info, errTypeExpected)"
      else:
        discard

      result = typeRel(c, aOrig.base, candidate, flags)
      if result != isNone:
        c.inferredTypes.add aOrig
        aOrig.add candidate
        result = isEqual
      return

  template doBind: bool = trDontBind notin flags

  # var, sink and static arguments match regular modifier-free types
  var a = maybeSkipDistinct(c, aOrig.skipTypes({tyStatic, tyVar, tyLent, tySink}), c.calleeSym)
  # XXX: Theoretically, maybeSkipDistinct could be called before we even
  # start the param matching process. This could be done in `prepareOperand`
  # for example, but unfortunately `prepareOperand` is not called in certain
  # situation when nkDotExpr are rotated to nkDotCalls

  if aOrig.kind in {tyAlias, tySink}:
    return typeRel(c, f, lastSon(aOrig), flags)

  if a.kind == tyGenericInst and
      skipTypes(f, {tyStatic, tyVar, tyLent, tySink}).kind notin {
        tyGenericBody, tyGenericInvocation,
        tyGenericInst, tyGenericParam} + tyTypeClasses:
    return typeRel(c, f, lastSon(a), flags)

  if a.isResolvedUserTypeClass:
    return typeRel(c, f, a.lastSon, flags)

  template bindingRet(res) =
    if doBind:
      let bound = aOrig.skipTypes({tyRange}).skipIntLit(c.c.idgen)
      put(c, f, bound)
    return res

  template considerPreviousT(body: untyped) =
    var prev = PType(idTableGet(c.bindings, f))
    if prev.isNil(): body
    else: return typeRel(c, prev, a, flags)

  case a.kind
  of tyOr:
    # XXX: deal with the current dual meaning of tyGenericParam
    c.typedescMatched = true
    # seq[int|string] vs seq[number]
    # both int and string must match against number
    # but ensure that '[T: A|A]' matches as good as '[T: A]' (bug #2219):
    result = isGeneric
    for branch in a.sons:
      let x = typeRel(c, f, branch, flags + {trDontBind})
      if x == isNone:
        result = isNone
        break
      elif x < result:
        result = x
    return result

  of tyAnd:
    # XXX: deal with the current dual meaning of tyGenericParam
    c.typedescMatched = true
    # seq[Sortable and Iterable] vs seq[Sortable]
    # only one match is enough
    for branch in a.sons:
      let x = typeRel(c, f, branch, flags + {trDontBind})
      if x != isNone:
        return if x >= isGeneric: isGeneric else: x
    return isNone

  of tyIterable:
    if f.kind != tyIterable:
      return isNone
  of tyNot:
    case f.kind
    of tyNot:
      # seq[!int] vs seq[!number]
      # seq[float] matches the first, but not the second
      # we must turn the problem around:
      # is number a subset of int?
      return typeRel(c, a.lastSon, f.lastSon, flags)

    else:
      # negative type classes are essentially infinite,
      # so only the `any` type class is their superset
      return if f.kind == tyAnything: isGeneric
             else: isNone

  of tyAnything:
    return if f.kind == tyAnything: isGeneric
           else: isNone

  of tyUserTypeClass, tyUserTypeClassInst:
    if c.c.matchedConcept != nil and c.c.matchedConcept.depth <= 4:
      # consider this: 'var g: Node' *within* a concept where 'Node'
      # is a concept too (tgraph)
      inc c.c.matchedConcept.depth
      let x = typeRel(c, a, f, flags + {trDontBind})
      if x >= isGeneric:
        return isGeneric
  else:
    discard

  case f.kind
  of tyEnum:
    if a.kind == f.kind and sameEnumTypes(f, a):
      result = isEqual
    elif sameEnumTypes(f, skipTypes(a, {tyRange})):
      result = isSubtype
  of tyBool, tyChar:
    if a.kind == f.kind:
      result = isEqual
    elif skipTypes(a, {tyRange}).kind == f.kind:
      result = isSubtype
  of tyRange:
    if a.kind == f.kind:
      if f.base.kind == tyNone:
        return isGeneric

      result = typeRel(c, base(f), base(a), flags)
      # bugfix: accept integer conversions here
      #if result < isGeneric: result = isNone
      if result notin {isNone, isGeneric}:
        # resolve any late-bound static expressions
        # that may appear in the range:
        for i in 0..1:
          if f.n[i].kind == nkStaticExpr:
            f.n[i] = tryResolvingStaticExpr(c, f.n[i])
        result = typeRangeRel(f, a)
    else:
      let f = skipTypes(f, {tyRange})
      if f.kind == a.kind and (f.kind != tyEnum or sameEnumTypes(f, a)):
        result = isIntConv
      elif isConvertibleToRange(f, a):
        result = isConvertible  # a convertible to f
  of tyInt:      result = handleRange(f, a, tyInt8, tyInt32)
  of tyInt8:     result = handleRange(f, a, tyInt8, tyInt8)
  of tyInt16:    result = handleRange(f, a, tyInt8, tyInt16)
  of tyInt32:    result = handleRange(f, a, tyInt8, tyInt32)
  of tyInt64:    result = handleRange(f, a, tyInt, tyInt64)
  of tyUInt:     result = handleRange(f, a, tyUInt8, tyUInt32)
  of tyUInt8:    result = handleRange(f, a, tyUInt8, tyUInt8)
  of tyUInt16:   result = handleRange(f, a, tyUInt8, tyUInt16)
  of tyUInt32:   result = handleRange(f, a, tyUInt8, tyUInt32)
  of tyUInt64:   result = handleRange(f, a, tyUInt, tyUInt64)
  of tyFloat:    result = handleFloatRange(f, a)
  of tyFloat32:  result = handleFloatRange(f, a)
  of tyFloat64:  result = handleFloatRange(f, a)
  of tyFloat128: result = handleFloatRange(f, a)
  of tyVar, tyLent:
    result =
      if aOrig.kind == f.kind:
        typeRel(c, f.base, aOrig.base, flags)
      else:
        typeRel(c, f.base, aOrig, flags + {trNoCovariance})

    subtypeCheck()
  of tyArray:
    case a.kind
    of tyArray:
      var
        fRange = f[0]
        aRange = a[0]
      
      if fRange.kind == tyGenericParam:
        let prev = PType(idTableGet(c.bindings, fRange))
        if prev.isNil():
          put(c, fRange, a[0])
          fRange = a
        else:
          fRange = prev
      
      let
        ff = f[1].skipTypes({tyTypeDesc})
        aa = a[1] #.skipTypes({tyTypeDesc})
                  # This typeDesc rule is wrong, see:
                  # https://github.com/nim-lang/nim/issues/7331

      result =
        if f[0].kind != tyGenericParam and aa.kind == tyEmpty:
          isGeneric
        else:
          typeRel(c, ff, aa, flags)

      if result < isGeneric:
        if nimEnableCovariance and
           trNoCovariance notin flags and
           ff.kind == aa.kind and
           isCovariantPtr(c, ff, aa):
          result = isSubtype
        else:
          return isNone

      if fRange.rangeHasUnresolvedStatic:
        return inferStaticsInRange(c, fRange, a)
      elif c.c.matchedConcept != nil and aRange.rangeHasUnresolvedStatic:
        return inferStaticsInRange(c, aRange, f)
      else:
        if lengthOrd(c.c.config, fRange) != lengthOrd(c.c.config, aRange):
          result = isNone
    else:
      discard
  of tyUncheckedArray:
    if a.kind == tyUncheckedArray:
      result = typeRel(c, base(f), base(a), flags)
      if result < isGeneric:
        result = isNone
    else:
      discard
  of tyOpenArray, tyVarargs:
    # varargs[untyped] is special too but handled earlier. So we only need to
    # handle varargs[typed]:
    if f.kind == tyVarargs:
      if tfVarargs in a.flags:
        return typeRel(c, f.base, a.lastSon, flags)
      if f[0].kind == tyTyped:
        return

    template matchArrayOrSeq(aBase: PType) =
      let
        ff = f.base
        aa = aBase
        baseRel = typeRel(c, ff, aa, flags)
      if baseRel >= isGeneric:
        result = isConvertible
      elif nimEnableCovariance and
           trNoCovariance notin flags and
           ff.kind == aa.kind and
           isCovariantPtr(c, ff, aa):
        result = isConvertible

    case a.kind
    of tyOpenArray, tyVarargs:
      result = typeRel(c, base(f), base(a), flags)
      if result < isGeneric:
        result = isNone
    of tyArray:
      if (f[0].kind != tyGenericParam) and (a[1].kind == tyEmpty):
        return isSubtype
      matchArrayOrSeq(a[1])
    of tySequence:
      if (f[0].kind != tyGenericParam) and (a[0].kind == tyEmpty):
        return isConvertible
      matchArrayOrSeq(a[0])
    of tyString:
      if f.kind == tyOpenArray:
        if f[0].kind == tyChar:
          result = isConvertible
        elif f[0].kind == tyGenericParam and a.len > 0 and
            typeRel(c, base(f), base(a), flags) >= isGeneric:
          result = isConvertible
    else:
      discard
  of tySequence:
    case a.kind
    of tySequence:
      if (f[0].kind != tyGenericParam) and (a[0].kind == tyEmpty):
        result = isSubtype
      else:
        let
          ff = f[0]
          aa = a[0]
        result = typeRel(c, ff, aa, flags)
        if result < isGeneric:
          if nimEnableCovariance and
             trNoCovariance notin flags and
             ff.kind == aa.kind and
             isCovariantPtr(c, ff, aa):
            result = isSubtype
          else:
            result = isNone
        elif tfNotNil in f.flags and tfNotNil notin a.flags:
          result = isNilConversion
    of tyNil:
      result = isNone
    else:
      discard
  of tyOrdinal:
    if isOrdinalType(a, allowEnumWithHoles = optNimV1Emulation in c.c.config.globalOptions):
      var x =
        if a.kind == tyOrdinal:
          a[0]
        else:
          a
      
      if f[0].kind == tyNone:
        result = isGeneric
      else:
        result = typeRel(c, f[0], x, flags)
        if result < isGeneric:
          result = isNone
    elif a.kind == tyGenericParam:
      result = isGeneric
  of tyForward:
    #internalError("forward type in typeRel()")
    result = isNone
  of tyNil:
    skipOwned(a)
    if a.kind == f.kind:
      result = isEqual
  of tyTuple:
    if a.kind == tyTuple:
      result = recordRel(c, f, a)
  of tyObject:
    if a.kind == tyObject:
      if sameObjectTypes(f, a):
        result = isEqual
        # elif tfHasMeta in f.flags: result = recordRel(c, f, a)
      else:
        var depth = isObjectSubtype(c, a, f, nil)
        if depth > 0:
          inc(c.inheritancePenalty, depth)
          result = isSubtype
  of tyDistinct:
    a = a.skipTypes({tyOwned, tyGenericInst, tyRange})
    if a.kind == tyDistinct:
      if sameDistinctTypes(f, a):
        result = isEqual
      #elif f.base.kind == tyAnything:
      #  result = isGeneric  # see https://github.com/nim-lang/nim/issues/4435
      elif c.coerceDistincts:
        result = typeRel(c, f.base, a, flags)
    elif a.kind == tyNil and f.base.kind in NilableTypes:
      result = f.allowsNil # XXX remove this typing rule, it is not in the spec
    elif c.coerceDistincts:
      result = typeRel(c, f.base, a, flags)
  of tySet:
    if a.kind == tySet:
      if f[0].kind != tyGenericParam and a[0].kind == tyEmpty:
        result = isSubtype
      else:
        result = typeRel(c, f[0], a[0], flags)
        if result < isGeneric:
          if result <= isConvertible:
            result = isNone
          elif tfIsConstructor notin a.flags:
            # set constructors are a bit special...
            result = isNone

  of tyPtr, tyRef:
    skipOwned(a)
    if a.kind == f.kind:
      # ptr[R, T] can be passed to ptr[T], but not the other way round:
      if a.len < f.len:
        return isNone
      
      for i in 0..<f.len-1:
        if typeRel(c, f[i], a[i], flags) == isNone:
          return isNone
      
      result = typeRel(c, f.lastSon, a.lastSon, flags + {trNoCovariance})
      subtypeCheck()
      
      if result <= isIntConv:
        result = isNone
      elif tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
    elif a.kind == tyNil:
      result = f.allowsNil
    else:
      discard
  of tyProc:
    skipOwned(a)
    result = procTypeRel(c, f, a)
    if result != isNone and tfNotNil in f.flags and tfNotNil notin a.flags:
      result = isNilConversion
  of tyOwned:
    case a.kind
    of tyOwned:
      result = typeRel(c, lastSon(f), lastSon(a), flags)
    of tyNil:
      result = f.allowsNil
    else:
      discard
  of tyPointer:
    skipOwned(a)
    case a.kind
    of tyPointer:
      if tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
      else:
        result = isEqual
    of tyNil:
      result = f.allowsNil
    of tyProc:
      if a.callConv != ccClosure: result = isConvertible
    of tyPtr:
      # 'pointer' is NOT compatible to regionized pointers
      # so 'dealloc(regionPtr)' fails:
      if a.len == 1: result = isConvertible
    of tyCstring:
      result = isConvertible
    else:
      discard
  of tyString:
    case a.kind
    of tyString:
      if tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
      else:
        result = isEqual
    of tyNil:
      result = isNone
    else:
      discard
  of tyCstring:
    # conversion from string to cstring is automatic:
    case a.kind
    of tyCstring:
      if tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
      else:
        result = isEqual
    of tyNil:
      result = f.allowsNil
    of tyString:
      result = isConvertible
    of tyPtr:
      # ptr[Tag, char] is not convertible to 'cstring' for now:
      if a.len == 1:
        let pointsTo = a[0].skipTypes(abstractInst)
        if pointsTo.kind == tyChar:
          result = isConvertible
        elif pointsTo.kind == tyUncheckedArray and pointsTo[0].kind == tyChar:
          result = isConvertible
        elif pointsTo.kind == tyArray and firstOrd(nil, pointsTo[0]) == 0 and
            skipTypes(pointsTo[0], {tyRange}).kind in {tyInt..tyInt64} and
            pointsTo[1].kind == tyChar:
          result = isConvertible
    else:
      discard

  of tyEmpty, tyVoid:
    if a.kind == f.kind:
      result = isEqual

  of tyAlias, tySink:
    result = typeRel(c, lastSon(f), a, flags)

  of tyIterable:
    case a.kind
    of tyIterable:
      if f.len == 1:
        result = typeRel(c, lastSon(f), lastSon(a), flags)
      else:
        # f.len = 3, for some reason
        result = isGeneric
    else:
      result = isNone
  of tyGenericInst:
    var prev = PType(idTableGet(c.bindings, f))
    let origF = f
    var f = if prev.isNil(): f else: prev

    if a.len > 0:
      let skippedA = a.skipTypesOrNil({tyObject}) # Skips if it's inheriting from a generic
      if skippedA != nil:
        a = skippedA
    let roota = a.skipGenericAlias
    let rootf = f.skipGenericAlias

    if a.kind == tyGenericInst:
      if roota.base == rootf.base:
        let nextFlags = flags + {trNoCovariance}
        var hasCovariance = false
        # YYYY
        result = isEqual

        for i in 1..<rootf.len-1:
          let
            ff = rootf[i]
            aa = roota[i]
            res = typeRel(c, ff, aa, nextFlags)
          
          if res != isNone and res != isEqual:
            result = isGeneric
          
          if res notin {isEqual, isGeneric}:
            if trNoCovariance notin flags and ff.kind == aa.kind:
              let paramFlags = rootf.base[i-1].flags
              hasCovariance =
                if tfCovariant in paramFlags:
                  if tfWeakCovariant in paramFlags:
                    isCovariantPtr(c, ff, aa)
                  else:
                    ff.kind notin {tyRef, tyPtr} and res == isSubtype
                else:
                  tfContravariant in paramFlags and
                    typeRel(c, aa, ff, flags) == isSubtype
              if hasCovariance:
                continue

            return isNone
        if prev.isNil():
          put(c, f, a)
      else:
        let fKind = rootf.lastSon.kind
        if fKind in {tyAnd, tyOr}:
          result = typeRel(c, lastSon(f), a, flags)
          if result != isNone:
            put(c, f, a)
          return

        var aAsObject = roota.lastSon

        if fKind in {tyRef, tyPtr}:
          if aAsObject.kind == tyObject:
            # bug #7600, tyObject cannot be passed
            # as argument to tyRef/tyPtr
            return isNone
          elif aAsObject.kind == fKind:
            aAsObject = aAsObject.base

        if aAsObject.kind == tyObject:
          let baseType = aAsObject.base
          if baseType != nil:
            c.inheritancePenalty += 1
            let ret = typeRel(c, f, baseType, flags)
            return if ret in {isEqual,isGeneric}: isSubtype else: ret

        result = isNone
    else:
      assert lastSon(origF) != nil
      result = typeRel(c, lastSon(origF), a, flags)
      if result != isNone and a.kind != tyNil:
        put(c, f, a)

  of tyGenericBody:
    considerPreviousT:
      if a == f or a.kind == tyGenericInst and a.skipGenericAlias[0] == f:
        bindingRet isGeneric
      let ff = lastSon(f)
      if ff != nil:
        result = typeRel(c, ff, a, flags)

  of tyGenericInvocation:
    var x = a.skipGenericAlias
    let concpt = f[0].skipTypes({tyGenericBody})
    var preventHack = false

    if x.kind == tyOwned and f[0].kind != tyOwned:
      preventHack = true
      x = x.lastSon

    # XXX: This is very hacky. It should be moved back into liftTypeParam
    if x.kind in {tyGenericInst, tyArray} and
      c.calleeSym != nil and
      c.calleeSym.kind in {skProc, skFunc} and c.call != nil and not preventHack:
      let inst = prepareMetatypeForSigmatch(c.c, c.bindings, c.call.info, f)
      #echo "inferred ", typeToString(inst), " for ", f
      return typeRel(c, inst, a, flags)

    if x.kind == tyGenericInvocation:
      if f[0] == x[0]:
        for i in 1..<f.len:
          let tr = typeRel(c, f[i], x[i], flags)
          if tr <= isSubtype: return
        result = isGeneric
    elif x.kind == tyGenericInst and f[0] == x[0] and
          x.len - 1 == f.len:
      for i in 1..<f.len:
        c.c.graph.config.internalAssert(x[i].kind != tyGenericParam, "wrong instantiated type!")

        if typeRel(c, f[i], x[i], flags) <= isSubtype:
          # Workaround for https://github.com/nim-lang/nim/issues/4589
          if f[i].kind != tyTypeDesc:
            return

      result = isGeneric
    else:
      var
        askip = skippedNone
        fskip = skippedNone
        depth = -1
      let
        genericBody = f[0]
        aobj = x.skipToObject(askip)
        fobj = genericBody.lastSon.skipToObject(fskip)
      
      if fobj != nil and aobj != nil and askip == fskip:
        depth = isObjectSubtype(c, aobj, fobj, f)
      
      result = typeRel(c, genericBody, x, flags)
      
      if result != isNone:
        # see tests/generics/tgeneric3.nim for an example that triggers this
        # piece of code:
        #
        # proc internalFind[T,D](n: PNode[T,D], key: T): ref TItem[T,D]
        # proc internalPut[T,D](ANode: ref TNode[T,D], Akey: T, Avalue: D,
        #                       Oldvalue: var D): ref TNode[T,D]
        # var root = internalPut[int, int](nil, 312, 312, oldvalue)
        # var it1 = internalFind(root, 312) # cannot instantiate: 'D'
        #
        # we steal the generic parameters from the tyGenericBody:
        for i in 1..<f.len:
          let x = PType(idTableGet(c.bindings, genericBody[i-1]))
          if x.isNil():
            discard "maybe fine (for e.g. a==tyNil)"
          else:
            c.c.graph.config.internalAssert(
              x.kind notin {tyGenericInvocation, tyGenericParam},
              "wrong instantiated type!")
            let key = f[i]
            let old = PType(idTableGet(c.bindings, key))
            if old.isNil():
              put(c, key, x)
            elif typeRel(c, old, x, flags + {trDontBind}) == isNone:
              return isNone

      if result == isNone:
        # Here object inheriting from generic/specialized generic object
        # crossing path with metatypes/aliases, so we need to separate them
        # by checking sym.id
        let genericSubtype = isGenericSubtype(c, x, f, depth, f)
        if not (genericSubtype and aobj.sym.id != fobj.sym.id) and aOrig.kind != tyGenericBody:
          depth = -1

      if depth >= 0:
        c.inheritancePenalty += depth
        # bug #4863: We still need to bind generic alias crap, so
        # we cannot return immediately:
        result = if depth == 0: isGeneric else: isSubtype
  of tyAnd:
    considerPreviousT:
      result = isEqual
      for branch in f.sons:
        let x = typeRel(c, branch, aOrig, flags)
        if x < isSubtype:
          return isNone
        # 'and' implies minimum matching result:
        if x < result:
          result = x
      if result > isGeneric:
        result = isGeneric
      bindingRet result

  of tyOr:
    considerPreviousT:
      result = isNone
      let oldInheritancePenalty = c.inheritancePenalty
      var maxInheritance = 0
      for branch in f.sons:
        c.inheritancePenalty = 0
        let x = typeRel(c, branch, aOrig, flags)
        maxInheritance = max(maxInheritance, c.inheritancePenalty)

        # 'or' implies maximum matching result:
        if x > result:
          result = x
      if result >= isSubtype:
        if result > isGeneric:
          result = isGeneric
        bindingRet result
      else:
        result = isNone
      c.inheritancePenalty = oldInheritancePenalty + maxInheritance

  of tyNot:
    considerPreviousT:
      for branch in f.sons:
        if typeRel(c, branch, aOrig, flags) != isNone:
          return isNone

      bindingRet isGeneric

  of tyAnything:
    considerPreviousT:
      var concrete = concreteType(c, a)
      if concrete != nil and doBind:
        put(c, f, concrete)
      return isGeneric

  of tyBuiltInTypeClass:
    considerPreviousT:
      let
        targetKind = f[0].kind
        effectiveArgType = a.skipTypes({tyRange, tyGenericInst,
                                        tyBuiltInTypeClass, tyAlias,
                                        tySink, tyOwned})
        typeClassMatches = targetKind == effectiveArgType.kind and
                             not effectiveArgType.isEmptyContainer
      if typeClassMatches or
        (targetKind in {tyProc, tyPointer} and effectiveArgType.kind == tyNil):
        put(c, f, a)
        return isGeneric
      else:
        return isNone

  of tyUserTypeClassInst, tyUserTypeClass:
    if f.isResolvedUserTypeClass:
      result = typeRel(c, f.lastSon, a, flags)
    else:
      considerPreviousT:
        if aOrig == f:
          return isEqual
        
        var matched = matchUserTypeClass(c, f, aOrig)
        
        if matched != nil:
          bindConcreteTypeToUserTypeClass(matched, a)
          if doBind:
            put(c, f, matched)
          result = isGeneric
        elif a.len > 0 and a.lastSon == f:
          # Needed for checking `Y` == `Addable` in the following
          #[
            type
              Addable = concept a, type A
                a + a is A
              MyType[T: Addable; Y: static T] = object
          ]#
          result = isGeneric
        else:
          result = isNone

  of tyCompositeTypeClass:
    considerPreviousT:
      let
        roota = a.skipGenericAlias
        rootf = f.lastSon.skipGenericAlias
      if a.kind == tyGenericInst and roota.base == rootf.base:
        for i in 1..<rootf.len-1:
          let
            ff = rootf[i]
            aa = roota[i]
          result = typeRel(c, ff, aa, flags)
          if result == isNone:
            return
          if ff.kind == tyRange and result != isEqual:
            return isNone
      else:
        result = typeRel(c, rootf.lastSon, a, flags)
      if result != isNone:
        put(c, f, a)
        result = isGeneric

  of tyGenericParam:
    let doBindGP = doBind or trBindGenericParam in flags
    var x = PType(idTableGet(c.bindings, f))
    if x.isNil():
      if c.callee.kind == tyGenericBody and not c.typedescMatched:
        # XXX: The fact that generic types currently use tyGenericParam for
        # their parameters is really a misnomer. tyGenericParam means "match
        # any value" and what we need is "match any type", which can be encoded
        # by a tyTypeDesc params. Unfortunately, this requires more substantial
        # changes in semtypinst and elsewhere.
        if tfWildcard in a.flags:
          result = isGeneric
        elif a.kind == tyTypeDesc:
          if f.len == 0:
            result = isGeneric
          else:
            internalAssert c.c.graph.config, a.len > 0, "[FIXME]"
            c.typedescMatched = true
            var aa = a
            while aa.kind in {tyTypeDesc, tyGenericParam} and aa.len > 0:
              aa = lastSon(aa)
            if aa.kind in {tyGenericParam} + tyTypeClasses:
              # If the constraint is a genericParam or typeClass this isGeneric
              return isGeneric
            result = typeRel(c, f.base, aa, flags)
            if result > isGeneric:
              result = isGeneric
        elif c.isNoCall:
          if doBindGP:
            let concrete = concreteType(c, a, f)
            if concrete.isNil(): return isNone
            put(c, f, concrete)
          result = isGeneric
        else:
          result = isNone
      else:
        # check if 'T' has a constraint as in 'proc p[T: Constraint](x: T)'
        if f.len > 0 and f[0].kind != tyNone:
          let oldInheritancePenalty = c.inheritancePenalty
          result = typeRel(c, f[0], a, flags + {trDontBind,trBindGenericParam})
          if doBindGP and result notin {isNone, isGeneric}:
            let concrete = concreteType(c, a, f)
            if concrete.isNil(): return isNone
            put(c, f, concrete)
          # bug #6526
          if result in {isEqual, isSubtype}:
            # 'T: Class' is a *better* match than just 'T'
            # but 'T: Subclass' is even better:
            c.inheritancePenalty = oldInheritancePenalty - c.inheritancePenalty -
                                  100 * ord(result == isEqual)
            result = isGeneric
        elif a.kind == tyTypeDesc:
          # somewhat special typing rule, the following is illegal:
          # proc p[T](x: T)
          # p(int)
          result = isNone
        else:
          result = isGeneric

      if result == isGeneric:
        var concrete = a
        if tfWildcard in a.flags:
          a.sym.transitionGenericParamToType()
          a.flags.excl tfWildcard
        else:
          concrete = concreteType(c, a, f)
          if concrete.isNil():
            return isNone
        if doBindGP:
          put(c, f, concrete)
      elif result > isGeneric:
        result = isGeneric
    elif a.kind == tyEmpty:
      result = isGeneric
    elif x.kind == tyGenericParam:
      result = isGeneric
    else:
      result = typeRel(c, x, a, flags) # check if it fits
      if result > isGeneric: result = isGeneric
  of tyStatic:
    let prev = PType(idTableGet(c.bindings, f))
    if prev.isNil():
      if aOrig.kind == tyStatic:
        if f.base.kind notin {tyNone, tyGenericParam}:
          result = typeRel(c, f.base, a, flags)
          if result != isNone and f.n != nil:
            if not exprStructuralEquivalent(f.n, aOrig.n):
              result = isNone
        elif f.base.kind == tyGenericParam:
          # Handling things like `type A[T; Y: static T] = object`, the reason this is here is the following:
          # The information is lost in the future steps so eventually it gets `T` vs. `T` which can be solved with a change to the tyTypedesc branch,
          # but it then gets typedesc[int] vs. int literal (2) without knowing either side is static.
          # Seems the only place to solve it is here anywhere lower just lacks the knowledge to know to check the children types.
          if f.base.len > 0: # There is a constraint, handle it
            result = typeRel(c, f.base.lastSon, a, flags)
          else:
            # No constraint
            if tfGenericTypeParam in f.flags:
              result = isGeneric
            else:
              # for things like `proc fun[T](a: static[T])`
              result = typeRel(c, f.base, a, flags)
        else:
          result = isGeneric
        if result != isNone: put(c, f, aOrig)
      elif aOrig.n != nil and aOrig.n.typ != nil:
        result = if f.base.kind != tyNone:
                   typeRel(c, f.lastSon, aOrig.n.typ, flags)
                 else: isGeneric
        if result != isNone:
          var boundType = newTypeWithSons(c.c, tyStatic, @[aOrig.n.typ])
          boundType.n = aOrig.n
          put(c, f, boundType)
      else:
        result = isNone
    elif prev.kind == tyStatic:
      if aOrig.kind == tyStatic:
        result = typeRel(c, prev.lastSon, a, flags)
        if result != isNone and prev.n != nil:
          if not exprStructuralEquivalent(prev.n, aOrig.n):
            result = isNone
      else: result = isNone
    else:
      # XXX endless recursion?
      #result = typeRel(c, prev, aOrig, flags)
      result = isNone

  of tyInferred:
    let prev = f.previouslyInferred
    if prev != nil:
      result = typeRel(c, prev, a, flags)
    else:
      result = typeRel(c, f.base, a, flags)
      if result != isNone:
        c.inferredTypes.add f
        f.add a

  of tyTypeDesc:
    var prev = PType(idTableGet(c.bindings, f))
    if prev.isNil():
      # proc foo(T: typedesc, x: T)
      # when `f` is an unresolved typedesc, `a` could be any
      # type, so we should not perform this check earlier
      if a.kind != tyTypeDesc:
        if a.kind == tyGenericParam and tfWildcard in a.flags:
          # TODO: prevent `a` from matching as a wildcard again
          result = isGeneric
        else:
          result = isNone
      elif f.base.kind == tyNone:
        result = isGeneric
      else:
        result = typeRel(c, f.base, a.base, flags)

      if result != isNone:
        put(c, f, a)
    else:
      if tfUnresolved in f.flags:
        result = typeRel(c, prev.base, a, flags)
      elif a.kind == tyTypeDesc:
        result = typeRel(c, prev.base, a.base, flags)
      else:
        result = isNone

  of tyTyped:
    if aOrig != nil:
      put(c, f, aOrig)
    result = isGeneric

  of tyProxy:
    result = isEqual

  of tyFromExpr:
    # fix the expression, so it contains the already instantiated types
    if f.n.isNil() or f.n.kind == nkEmpty: return isGeneric
    let reevaluated = tryResolvingStaticExpr(c, f.n)
    case reevaluated.typ.kind
    of tyTypeDesc:
      result = typeRel(c, a, reevaluated.typ.base, flags)
    of tyStatic:
      result = typeRel(c, a, reevaluated.typ.base, flags)
      if result != isNone and reevaluated.typ.n != nil:
        if not exprStructuralEquivalent(aOrig.n, reevaluated.typ.n):
          result = isNone
    else:
      # bug #14136: other types are just like 'tyStatic' here:
      result = typeRel(c, a, reevaluated.typ, flags)
      if result != isNone and reevaluated.typ.n != nil:
        if not exprStructuralEquivalent(aOrig.n, reevaluated.typ.n):
          result = isNone
  of tyNone:
    if a.kind == tyNone: result = isEqual
  else:
    internalError c.c.graph.config, " unknown type kind " & $f.kind

when false:
  var nowDebug = false
  var dbgCount = 0

  proc typeRel(c: var TCandidate, f, aOrig: PType,
              flags: TTypeRelFlags = {}): TTypeRelation =
    if nowDebug:
      echo f, " <- ", aOrig
      inc dbgCount
      if dbgCount == 2:
        writeStackTrace()
    result = typeRelImpl(c, f, aOrig, flags)
    if nowDebug:
      echo f, " <- ", aOrig, " res ", result

proc cmpTypes*(c: PContext, f, a: PType): TTypeRelation =
  var m = newCandidate(c, f)
  result = typeRel(m, f, a)

proc getInstantiatedType(c: PContext, arg: PNode, m: TCandidate,
                         f: PType): PType =
  result = PType(idTableGet(m.bindings, f))
  if result.isNil():
    result = generateTypeInstance(c, m.bindings, arg, f)
  c.graph.config.internalAssert(result != nil, arg.info, "getInstantiatedType")

proc implicitConv(kind: TNodeKind, f: PType, arg: PNode, m: TCandidate,
                  c: PContext): PNode =
  result = newNodeI(kind, arg.info)
  result.typ =
    if containsGenericType(f):
      if not m.hasFauxMatch:
        getInstantiatedType(c, arg, m, f).skipTypes({tySink})
      else:
        errorType(c)
    else:
      f.skipTypes({tySink})

  c.graph.config.internalAssert(result.typ != nil, arg.info, "implicitConv")

  result.add c.graph.emptyNode
  result.add arg

proc isLValue(c: PContext; n: PNode): bool {.inline.} =
  case isAssignable(nil, n)
  of arLValue, arLocalLValue, arStrange:
    true
  of arDiscriminant:
    c.inUncheckedAssignSection > 0
  else:
    false

proc userConvMatch(c: PContext, m: var TCandidate, f, a: PType,
                   arg: PNode): PNode =
  result = nil

  for i in 0..<c.converters.len:
    var
      src = c.converters[i].typ[1]
      dest = c.converters[i].typ[0]

    # for generic type converters we need to check 'src <- a' before
    # 'f <- dest' in order to not break the unification:
    # see tests/tgenericconverter:
    let srca = typeRel(m, src, a)
    
    if srca notin {isEqual, isGeneric, isSubtype}:
      continue

    # What's done below matches the logic in ``matchesAux``
    let constraint = c.converters[i].typ.n[1].sym.constraint

    if not constraint.isNil and not matchNodeKinds(constraint, arg):
      continue

    if src.kind in {tyVar, tyLent} and not isLValue(c, arg):
      continue

    let destIsGeneric = containsGenericType(dest)

    if destIsGeneric:
      dest = generateTypeInstance(c, m.bindings, arg, dest)

    let fdest = typeRel(m, f, dest)

    if fdest in {isEqual, isGeneric} and
       not (dest.kind == tyLent and f.kind in {tyVar}):
      markUsed(c, arg.info, c.converters[i])

      let conv = c.converters[i]

      # We build the call expression by ourselves in order to avoid passing this
      # expression through the semantic check phase once again so let's make
      # sure it is correct
      result = newTreeIT(nkHiddenCallConv, arg.info, dest,
                # the converter sym
                newSymNodeIT(conv, arg.info, conv.typ),
                # arg for the converter call
                if srca == isSubtype:
                  implicitConv(nkHiddenSubConv, src, copyTree(arg), m, c)
                elif src.kind in {tyVar}:
                  # Analyse the converter return type
                  newTreeIT(nkHiddenAddr, arg.info, conv.typ[1], copyTree(arg))
                else:
                  copyTree(arg))

      if dest.kind in {tyVar, tyLent}:
        dest.flags.incl tfVarIsPtr
        result = newDeref(result)

      inc(m.convMatches)

      if not m.genericConverter:
        m.genericConverter = srca == isGeneric or destIsGeneric

      return result

proc localConvMatch(c: PContext, m: var TCandidate, f, a: PType,
                    arg: PNode): PNode =
  # arg.typ can be nil in 'suggest':
  if isNil(arg.typ):
    return nil

  # sem'checking for 'echo' needs to be re-entrant:
  # xxx: sem idempotency issue
  if f == arg.typ and arg.kind == nkHiddenStdConv:
    return arg

  var call = newNodeI(nkCall, arg.info)
  call.add(f.n.copyTree)
  call.add(arg.copyTree)

  # XXX: This would be much nicer if we don't use `semTryExpr` and
  # instead we directly search for overloads with `resolveOverloads`.
  # but converters are special and that causes all sorts of issues :/
  result = c.semTryExpr(c, call, {efNoSem2Check})

  if result != nil:
    if result.typ.isNil():
      return nil

    # ensure we produce a real generic instantiation, see bug
    # https://github.com/nim-lang/nim/issues/13378:
    result = c.semExpr(c, call)

    # resulting type must be consistent with the other arguments:
    var r = typeRel(m, f[0], result.typ)
    
    if r < isGeneric:
      return nil

    if result.kind == nkCall:
      result.transitionSonsKind(nkHiddenCallConv)

    inc(m.convMatches)

    if r == isGeneric:
      result.typ = getInstantiatedType(c, arg, m, base(f))

    m.baseTypeMatch = true

proc incMatches(m: var TCandidate; r: TTypeRelation; convMatch = 1) =
  case r
  of isConvertible, isIntConv:
    inc(m.convMatches, convMatch)
  of isSubtype, isSubrange:
    inc(m.subtypeMatches)
  of isGeneric, isInferred, isBothMetaConvertible:
    inc(m.genericMatches)
  of isFromIntLit:
    inc(m.intConvMatches, 256)
  of isInferredConvertible:
    inc(m.convMatches)
  of isEqual:
    inc(m.exactMatches)
  of isNone:
    discard

template matchesVoidProc(t: PType): bool =
  (t.kind == tyProc and t.len == 1 and t[0].isNil()) or
    (t.kind == tyBuiltInTypeClass and t[0].kind == tyProc)

proc paramTypesMatchAux(m: var TCandidate, f, a: PType,
                        argSemantized: PNode): PNode =
  var
    fMaybeStatic = f.skipTypes({tyDistinct})
    arg = argSemantized
    a = a
    c = m.c

  if tfHasStatic in fMaybeStatic.flags:
    # XXX: When implicit statics are the default
    # this will be done earlier - we just have to
    # make sure that static types enter here

    # Zahary: weaken tyGenericParam and call it tyGenericPlaceholder
    # and finally start using tyTypedesc for generic types properly.
    # Araq: This would only shift the problems around, in 'proc p[T](x: T)'
    # the T is NOT a typedesc.
    if a.kind == tyGenericParam and tfWildcard in a.flags:
      a.assignType(f)
      # put(m.bindings, f, a)
      result = arg
      return

    if a.kind == tyStatic:
      if m.callee.kind == tyGenericBody and
         a.n.isNil() and
         tfGenericTypeParam notin a.flags:
        result = newNodeIT(nkType, arg.info, makeTypeFromExpr(c, arg))
        return
    else:
      var evaluated = c.semTryConstExpr(c, arg)
      if evaluated != nil:
        # Don't build the type in-place because `evaluated` and `arg` may point
        # to the same object and we'd end up creating recursive types (#9255)
        let typ = newTypeS(tyStatic, c)
        typ.sons = @[evaluated.typ]
        typ.n = evaluated
        arg = copyTree(arg) # fix #12864
        arg.typ = typ
        a = typ
      else:
        if m.callee.kind == tyGenericBody:
          if f.kind == tyStatic and typeRel(m, f.base, a) != isNone:
            result = makeStaticExpr(m.c, arg)
            result.typ.flags.incl tfUnresolved
            result.typ.n = arg
            return

  let oldInheritancePenalty = m.inheritancePenalty
  var r = typeRel(m, f, a)

  # This special typing rule for macros and templates is not documented
  # anywhere and breaks symmetry. It's hard to get rid of though, my
  # custom seqs example fails to compile without this:
  if r != isNone and m.calleeSym != nil and
    m.calleeSym.kind in {skMacro, skTemplate}:
    # XXX: duplicating this is ugly, but we cannot (!) move this
    # directly into typeRel using return-like templates
    incMatches(m, r)
    result =
      case f.kind
      of tyTyped, tyTypeDesc:
        arg
      of tyStatic:
        if arg.typ.n.isNil:  # no value on the type
          argSemantized
        else:                # value on the type
          arg.typ.n
      else:
        argSemantized
    return

  var
    bothMetaCounter = 0
      ## bothMetaCounter is for safety to avoid any infinite loop,
      ## we don't have any example when it is needed.
    lastBindingsLength = -1
      ## lastBindingsLenth use to track whether m.bindings remains the same,
      ## because in that case there is no point in continuing.
  
  # If r == isBothMetaConvertible then we rerun typeRel.
  while r == isBothMetaConvertible and
      lastBindingsLength != m.bindings.counter and
      bothMetaCounter < 100:                        # ensure termination

    lastBindingsLength = m.bindings.counter
    inc(bothMetaCounter)

    case arg.kind
    of nkProcDef, nkFuncDef, nkIteratorDef, nkLambdaKinds:
      result = c.semInferredLambda(c, m.bindings, arg)
    of nkSym:
      let inferred = c.semGenerateInstance(c, arg.sym, m.bindings, arg.info)
      result = newSymNode(inferred, arg.info)
    else:
      result = nil
      return

    inc(m.convMatches)
    arg = result
    r = typeRel(m, f, arg.typ)

  # now check the relation result in `r`
  case r
  of isConvertible:
    inc(m.convMatches)
    result = implicitConv(nkHiddenStdConv, f, arg, m, c)
  of isIntConv:
    # I'm too lazy to introduce another ``*matches`` field, so we conflate
    # ``isIntConv`` and ``isIntLit`` here:
    # xxx: this should likely be disambiguated
    inc(m.intConvMatches)
    result = implicitConv(nkHiddenStdConv, f, arg, m, c)
  of isSubtype:
    inc(m.subtypeMatches)
    result =
      case f.kind
      of tyTypeDesc:
        arg
      else:
        implicitConv(nkHiddenSubConv, f, arg, m, c)
  of isSubrange:
    inc(m.subtypeMatches)
    result =
      case f.kind
      of tyVar:
        arg
      else:
        implicitConv(nkHiddenStdConv, f, arg, m, c)
  of isInferred, isInferredConvertible:
    case arg.kind
    of nkProcDef, nkFuncDef, nkIteratorDef, nkLambdaKinds:
      result = c.semInferredLambda(c, m.bindings, arg)
    of nkSym:
      let inferred = c.semGenerateInstance(c, arg.sym, m.bindings, arg.info)
      result = newSymNode(inferred, arg.info)
    else:
      result = nil
      return

    case r
    of isInferredConvertible:
      inc(m.convMatches)
      result = implicitConv(nkHiddenStdConv, f, result, m, c)
    else:
      inc(m.genericMatches)
  of isGeneric:
    inc(m.genericMatches)
    # xxx: this logic isn't more linear likely because we don't differentiate:
    #      - array/seq/etc as universal type (kind?), vs 
    #      - array/seq/etc as a monomorph, vs
    #      - array/seq/etc instantiated
    if arg.typ.isNil():
      result = arg
    elif skipTypes(arg.typ, abstractVar-{tyTypeDesc}).kind == tyTuple or
         m.inheritancePenalty > oldInheritancePenalty:
      result = implicitConv(nkHiddenSubConv, f, arg, m, c)
    elif arg.typ.isEmptyContainer:
      result = arg.copyTree
      result.typ = getInstantiatedType(c, arg, m, f)
    else:
      result = arg
  of isBothMetaConvertible:
    # This is the result for the 101th time.
    result = nil
  of isFromIntLit:
    # too lazy to introduce another ``*matches`` field, so we conflate
    # ``isIntConv`` and ``isIntLit`` here:
    # xxx: this should likely be disambiguated, subtle impact everywhere
    inc(m.intConvMatches, 256)
    result = implicitConv(nkHiddenStdConv, f, arg, m, c)
  of isEqual:
    inc(m.exactMatches)
    result = arg
    if skipTypes(f, abstractVar-{tyTypeDesc}).kind == tyTuple or
      (arg.typ != nil and skipTypes(arg.typ, abstractVar-{tyTypeDesc}).kind == tyTuple):
      result = implicitConv(nkHiddenSubConv, f, arg, m, c)
  of isNone:
    # do not do this in ``typeRel`` as it then can't infer T in ``ref T``:
    # xxx: likely a latent bug, as tyError is simply a const equal to tyProxy
    if a.kind in {tyProxy, tyUnknown}:
      inc(m.genericMatches)
      m.fauxMatch = a.kind
      result = arg
      return
    elif a.kind == tyVoid and f.matchesVoidProc and arg.kind == nkStmtList:
      # lift do blocks without params to lambdas
      let
        p = c.graph
        lifted = c.semExpr(c, newProcNode(nkDo, arg.info,
                                  name = p.emptyNode,
                                  genericParams = p.emptyNode,
                                  params = nkFormalParams.newTree(p.emptyNode),
                                  pragmas = p.emptyNode,
                                  exceptions = p.emptyNode,
                                  body = arg,
                                  pattern = p.emptyNode))

      if f.kind == tyBuiltInTypeClass:
        inc m.genericMatches
        put(m, f, lifted.typ)

      inc m.convMatches
      result = implicitConv(nkHiddenStdConv, f, lifted, m, c)
      return
    else:
      discard # keep processing below

    result = userConvMatch(c, m, f, a, arg)

    # check for a base type match, which supports varargs[T] without []
    # constructor in a call:
    if result.isNil() and f.kind == tyVarargs:
      if f.n != nil:
        # Forward to the varargs converter
        result = localConvMatch(c, m, f, a, arg)
      else:
        r = typeRel(m, base(f), a)
        case r
        of isGeneric:
          inc(m.convMatches)
          result = copyTree(arg)
          result.typ = getInstantiatedType(c, arg, m, base(f))
          m.baseTypeMatch = true
        of isFromIntLit:
          inc(m.intConvMatches, 256)
          result = implicitConv(nkHiddenStdConv, f[0], arg, m, c)
          m.baseTypeMatch = true
        of isEqual:
          inc(m.convMatches)
          result = copyTree(arg)
          m.baseTypeMatch = true
        of isSubtype: # bug #4799, varargs accepting subtype relation object
          inc(m.subtypeMatches)
          if base(f).kind == tyTypeDesc:
            result = arg
          else:
            result = implicitConv(nkHiddenSubConv, base(f), arg, m, c)
          m.baseTypeMatch = true
        else:
          result = userConvMatch(c, m, base(f), a, arg)
          if result != nil:
            m.baseTypeMatch = true

proc paramTypesMatch*(
    candidate: var TCandidate,
    formal, passed: PType,
    arg: PNode
  ): PNode =
  ## Try to check a single argument for the `candidate` function call using
  ## provided `formal` parmameter and `passed` one. `arg` is a node that
  ## was passed from the callsite

  if arg.isNil() or
     # If argument is not a named one (aka `arg=`) then try to match
     # parameter types directly
     arg.kind notin nkSymChoices
     # Also try to match them if argument is not an open sym choice
     # (QUESTION in which cases it can be one?)
  :
    result = paramTypesMatchAux(candidate, formal, passed, arg)

  else:
    # CAUTION: The order depends on the used hashing scheme. Thus it is
    # incorrect to simply use the first fitting match. However, to implement
    # this correctly is inefficient. We have to copy `m` here to be able to
    # roll back the side effects of the unification algorithm.
    let c = candidate.c
    var
      # Define three candidates 'first', 'second' and look through all
      # possibly symbols that can me matched on their position.
      best = newCandidate(c, candidate.callee)
      second = newCandidate(c, candidate.callee)
      # Keep track of the best argument position as well, it will be used
      # to get back to the selected symbol node.
      bestArg = -1

    best.calleeSym = candidate.calleeSym
    second.calleeSym = candidate.calleeSym

    for i in 0 ..< arg.len:
      # Try all match candidates in the open/closed symbol choice passed as
      # an argument
      let matchCandidate = arg[i]
      if matchCandidate.sym.kind in OverloadableSyms - {skModule}:
        var next = newCandidate(c, candidate.callee)
        copyCandidate(next, candidate)
        next.callee = matchCandidate.typ

        if tfUnresolved in next.callee.flags:
          continue

        next.calleeSym = matchCandidate.sym
        # XXX this is still all wrong: (T, T) should be 2 generic matches
        # and  (int, int) 2 exact matches, etc. Essentially you cannot call
        # typeRel here and expect things to work!
        let rel = typeRel(next, formal, matchCandidate.typ)
        incMatches(next, rel, 2)
        if rel != isNone:
          next.state = csMatch
          case best.state:
            of csEmpty, csNoMatch:
              best = next
              bestArg = i

            of csMatch:
              let cmp = cmpCandidates(best, next)
              if cmp < 0:
                bestArg = i
                best = next

              elif cmp == 0:
                second = next           # next is as good as x

    if best.state == csEmpty:
      result = nil

    elif second.state == csMatch and
         # The 'best' and 'second' candidates match equally well - unless
         # it is a template this means code is ambiguous.
         cmpCandidates(best, second) == 0:

      candidate.c.graph.config.internalAssert(
        best.state == csMatch,
        arg.info,
        "best.state is not csMatch")
      # ambiguous: more than one symbol fits!
      # See tsymchoice_for_expr as an example. 'f.kind == tyUntyped' should match
      # anyway:
      result = if formal.kind in {tyUntyped, tyTyped}: arg else: nil

    else:
      # only one valid interpretation found, executing argument match
      markUsed(candidate.c, arg.info, arg[bestArg].sym)
      onUse(arg.info, arg[bestArg].sym)
      result = paramTypesMatchAux(
        candidate, formal, arg[bestArg].typ, arg[bestArg])

  when false:
    if candidate.calleeSym != nil and
       candidate.calleeSycandidate.name.s == "[]":
      echo(
        candidate.c.config $ arg.info,
        " for ",
        candidate.calleeSycandidate.name.s,
        " ",
        candidate.c.config $ candidate.calleeSym.info)

      writeMatches(candidate)

proc setSon(father: PNode, at: int, son: PNode) =
  let oldLen = father.len
  if oldLen <= at:
    setLen(father.sons, at + 1)
  father[at] = son
  # insert potential 'void' parameters:
  #for i in oldLen..<at:
  #  father[i] = newNodeIT(nkEmpty, son.info, getSysType(tyVoid))

# we are allowed to modify the calling node in the 'prepare*' procs:
proc prepareOperand(c: PContext; formal: PType; a: PNode): PNode =
  if formal.kind == tyUntyped and formal.len != 1:
    result = a

  elif a.typ.isNil:
    if formal.kind == tyIterable:
      let flags = {efDetermineType, efAllowStmt, efWantIterator, efWantIterable}
      result = c.semOperand(c, a, flags)
    else:
      # XXX This is unsound! 'formal' can differ from overloaded routine to
      # overloaded routine!
      let flags = {efDetermineType, efAllowStmt}
                  #if formal.kind == tyIterable: {efDetermineType, efWantIterator}
                  #else: {efDetermineType, efAllowStmt}
                  #elif formal.kind == tyTyped: {efDetermineType, efWantStmt}
                  #else: {efDetermineType}
      result = c.semOperand(c, a, flags)
  else:
    result = a
    considerGenSyms(c, result)
    if result.kind != nkHiddenDeref and result.typ.kind in {tyVar, tyLent} and c.matchedConcept.isNil():
      result = newDeref(result)

proc prepareOperand(c: PContext; a: PNode): PNode =
  if a.typ.isNil:
    result = c.semOperand(c, a, {efDetermineType})
  else:
    result = a
    considerGenSyms(c, result)

proc prepareNamedParam(a: PNode; c: PContext) =
  ## set the correct ident node, or nkError, in the 0th position for this
  ## named param
  doAssert not a.isErrorLike, "named param is error like"

  case a.kind
  of nkExprEqExpr:
    case a[0].kind
    of nkIdent, nkError:
      discard "nothing to do"
    else:
      let
        info = a[0].info
        (i, err) = considerQuotedIdent(c, a[0])
      a[0] =
        if err.isNil():
          newIdentNode(i, info)   # xxx: going "backwards" to an ident, fishy
        else:
          err
  else:
    c.config.internalError("named param must be exprEqExpr, got: " & $a.kind)


proc arrayConstr(c: PContext, n: PNode): PType =
  result = newTypeS(tyArray, c)
  rawAddSon(result, makeRangeType(c, 0, 0, n.info))
  addSonSkipIntLit(result, skipTypes(n.typ,
      {tyGenericInst, tyVar, tyLent, tyOrdinal}), c.idgen)

proc arrayConstr(c: PContext, info: TLineInfo): PType =
  result = newTypeS(tyArray, c)
  rawAddSon(result, makeRangeType(c, 0, -1, info))
  rawAddSon(result, newTypeS(tyEmpty, c)) # needs an empty basetype!

proc incrIndexType(t: PType) =
  assert t.kind == tyArray
  inc t[0].n[1].intVal

template isVarargsUntyped(x): untyped =
  x.kind == tyVarargs and x[0].kind == tyUntyped

proc findFirstArgBlock(m: var TCandidate, n: PNode): int =
  # see https://github.com/nim-lang/RFCs/issues/405
  result = int.high
  for a2 in countdown(n.len - 1, 0):
    # checking `nfBlockArg in n[a2].flags` wouldn't work inside templates
    # xxx: ^^ why???
    case n[a2].kind
    of nkStmtList:
      let formalLast = m.callee.n[m.callee.n.len - (n.len - a2)]

      if formalLast.kind == nkSym and formalLast.sym.ast.isNil():
        result = a2
      else:
        break
    else:
      break


proc matchesAux(c: PContext, n: PNode, m: var TCandidate, marker: var IntSet) =
  ## used to match a call `n` with a candidate `m`, noting matched formal
  ## params in `marker` by position. `m` and `marker` are out parameters and
  ## updated with the produced results.

  assert n.kind in nkCallKinds + {nkBracketExpr},
         "sigmatch.matchesAux wrong kind: " & $n.kind

  template noMatch() =
    c.mergeShadowScope #merge so that we don't have to resem for later overloads
    m.state = csNoMatch
    m.error.firstMismatch.pos = a
    m.error.firstMismatch.arg = n[a]
    m.error.firstMismatch.formal = formal
    return

  template noMatchDueToError() =
    ## found an nkError along the way so wrap the call in an error, do not use
    ## if the legacy `localReport`s etc are being used.
    m.call = wrapError(c.config, m.call)
    noMatch()

  template checkConstraint(n: untyped) {.dirty.} =
    if not formal.constraint.isNil:
      if matchNodeKinds(formal.constraint, n):
        # better match over other routines with no such restriction:
        inc(m.genericMatches, 100)
      else:
        noMatch()

    if formal.typ.kind in {tyVar}:
      let argConverter = if arg.kind == nkHiddenDeref: arg[0] else: arg
      if argConverter.kind == nkHiddenCallConv:
        if argConverter.typ.kind notin {tyVar}:
          m.error.firstMismatch.kind = kVarNeeded
          noMatch()
      elif not isLValue(c, n):
        m.error.firstMismatch.kind = kVarNeeded
        noMatch()

  m.state = csMatch # until proven otherwise
  m.error.firstMismatch = MismatchInfo()
  m.call = newNodeIT(n.kind, n.info, m.callee.base)
  m.call.add n[0]

  let firstArgBlock = findFirstArgBlock(m, n)

  var
    a = 1
      ## index to iterate over the actual given arguments
    f = if m.callee.kind != tyGenericBody: 1
        else: 0
      ## index to iterate over formal parameters
    arg: PNode
      ## current prepared argument
    formalLen = m.callee.n.len
    formal = if formalLen > 1: m.callee.n[1].sym else: nil
      ## current routine parameter
    container: PNode = nil
      ## container (arg list, bracket, ...) to store intermediaries

  while a < n.len:
    c.openShadowScope

    # untyped varargs
    if a >= formalLen - 1 and              # last or finished passing args
       f < formalLen and                   # still have more formal params
       m.callee.n[f].typ.isVarargsUntyped: # current formal is varargs untped
      
      formal = m.callee.n[f].sym
      incl(marker, formal.position)

      case n[a].kind
      of nkHiddenStdConv:
        # if there is a conversion we, "steal" its container and pass it along
        doAssert n[a][0].kind == nkEmpty and
                 n[a][1].kind in {nkBracket, nkArgList}
        # xxx: is reusing the converted call arg's container sound?
        setSon(m.call, formal.position + 1, n[a][1])
      else:
        # make sure we have a container for the varargs
        if container.isNil:
          container = newNodeIT(nkArgList, n[a].info, arrayConstr(c, n.info))
          setSon(m.call, formal.position + 1, container)
        else:
          incrIndexType(container.typ)
        
        container.add n[a]
    elif n[a].kind == nkExprEqExpr:        # named params `foo(bar = "baz")`
      # named param

      # assume it's wrong, then prove it correct
      m.error.firstMismatch.kind = kUnknownNamedParam
      
      # check if m.callee has such a param:
      prepareNamedParam(n[a], c)
      
      if n[a].kind == nkError or n[a][0].kind != nkIdent:
        localReport(c.config, n[a].info, reportAst(
          rsemExpectedIdentifier, n[a],
          str = "named parameter has to be an identifier"
        ))
        noMatch()
      
      formal = getNamedParamFromList(m.callee.n, n[a][0].ident)
      
      if formal.isNil or formal.isError:
        # no error message!
        noMatch()
      
      if containsOrIncl(marker, formal.position):
        m.error.firstMismatch.kind = kAlreadyGiven
        # already in namedParams, so no match
        # we used to produce 'errCannotBindXTwice' here but see
        # bug https://github.com/nim-lang/nim/issues/3836 for why that is not
        # sound (other overload with different parameter names could match
        # later on)
        #
        # xxx: with nkError this is likely no longer an issue
        when false: localReport(n[a].info, errCannotBindXTwice, formal.name.s)
        noMatch()
      
      m.baseTypeMatch = false
      m.typedescMatched = false
      n[a][1] = prepareOperand(c, formal.typ, n[a][1])
      n[a].typ = n[a][1].typ
      arg = paramTypesMatch(m, formal.typ, n[a].typ, n[a][1])
      m.error.firstMismatch.kind = kTypeMismatch
      
      if arg.isNil() or arg.isErrorLike:
        noMatch()
      elif n[a][1].isErrorLike: # named param's value is an error
        noMatchDueToError()
      
      checkConstraint(n[a][1])  # will update `m` with info
      
      if m.baseTypeMatch:
        #assert(container.isNil())
        container = newNodeIT(nkBracket, n[a].info, arrayConstr(c, arg))
        container.add arg
        setSon(m.call, formal.position + 1, container)
        if f != formalLen - 1: # not the last formal param
          container = nil      # xxx: is this more vararg stuff?
      else:
        setSon(m.call, formal.position + 1, arg)
      
      inc f
    else:                                  # unnamed param `foo("baz")`
      if f >= formalLen:
        # too many arguments?
        if tfVarargs in m.callee.flags:
          # is ok... but don't increment any counters...
          # we have no formal here to snoop at:
          n[a] = prepareOperand(c, n[a])

          case skipTypes(n[a].typ, abstractVar-{tyTypeDesc}).kind
          of tyString:
            # implicit conversion string -> cstring
            m.call.add implicitConv(nkHiddenStdConv,
                                    getSysType(c.graph, n[a].info, tyCstring),
                                    copyTree(n[a]), m, c)
          else:
            m.call.add copyTree(n[a])
        elif formal != nil and formal.typ.kind == tyVarargs: # extra varargs
          m.error.firstMismatch.kind = kTypeMismatch
          # beware of the side-effects in 'prepareOperand'! So only do it for
          # varargs matching. See tests/metatype/tstatic_overloading.
          m.baseTypeMatch = false
          m.typedescMatched = false
          incl(marker, formal.position)
          n[a] = prepareOperand(c, formal.typ, n[a])
          arg = paramTypesMatch(m, formal.typ, n[a].typ, n[a])

          if arg != nil and arg.kind != nkError and # valid argument
             m.baseTypeMatch and                    # match type in `varargs[T]`
             container != nil:                      # container must exist
            
            container.add arg
            incrIndexType(container.typ)
            checkConstraint(n[a])
          elif n[a].isErrorLike:                    # invalid operand
            noMatchDueToError()
          else:
            noMatch()
        else:
          m.error.firstMismatch.kind = kExtraArg
          noMatch()
      else:
        c.config.internalAssert(m.callee.n[f].kind == nkSym, n[a].info,
                                "matches")

        if a >= firstArgBlock:
          f = max(f, m.callee.n.len - (n.len - a))

        formal = m.callee.n[f].sym
        m.error.firstMismatch.kind = kTypeMismatch

        # both positional and named argument for the same formal
        if containsOrIncl(marker, formal.position) and container.isNil:
          m.error.firstMismatch.kind = kPositionalAlreadyGiven
          # positional param already in namedParams, see named param handling
          # above for additional remarks about `errCannotBindXTwice`
          #
          # xxx: shouldn't be an issue with nkError
          when false:
            localReport(n[a].info, errCannotBindXTwice, formal.name.s)
          noMatch()

        if formal.typ.isVarargsUntyped: # first untyped varargs param
          if container.isNil:
            container = newNodeIT(nkArgList, n[a].info, arrayConstr(c, n.info))
            setSon(m.call, formal.position + 1, container)
          else:
            incrIndexType(container.typ)

          container.add n[a]
        else:                           # additional varargs or other param
          
          # xxx: this code and the unnamed/varargs handling above are near
          #      identical, should revise the overall logic and deduplicate
          
          m.baseTypeMatch = false
          m.typedescMatched = false
          n[a] = prepareOperand(c, formal.typ, n[a])
          arg = paramTypesMatch(m, formal.typ, n[a].typ, n[a])

          if arg.isNil() or arg.isErrorLike: # invalid arg
            noMatch()
          elif n[a].isErrorLike:            # invalid operand
            noMatchDueToError()

          if m.baseTypeMatch: # var args
            assert formal.typ.kind == tyVarargs
            
            if container.isNil:
              container = newNodeIT(nkBracket, n[a].info, arrayConstr(c, arg))
              container.typ.flags.incl tfVarargs
            else:
              incrIndexType(container.typ)
            
            container.add arg
            setSon(m.call, formal.position + 1,
                   implicitConv(nkHiddenStdConv, formal.typ, container, m, c))

            # pick the formal from the end, so a regular param can follow a
            # varargs: 'foo(x: int, y: varargs[typed], blk: untyped): typed'
            f = max(f, formalLen - n.len + a + 1)
          elif formal.typ.kind != tyVarargs or container.isNil(): # regular arg
            setSon(m.call, formal.position + 1, arg)
            inc f
            container = nil
          else:
            # we end up here if the argument can be converted into the varargs
            # formal (e.g. seq[T] -> varargs[T]) but we have already instantiated
            # a container

            localReport(c.config, n[a].info,
              SemReport(
                kind: rsemCannotConvertTypes,
                typeMismatch: @[c.config.typeMismatch(
                  formal = formal.typ, actual = n[a].typ)]))

            noMatch()

        checkConstraint(n[a])

    if m.state == csMatch and
       not (m.calleeSym != nil and m.calleeSym.kind in {skTemplate, skMacro}):
      c.mergeShadowScope
    else:
      c.closeShadowScope

    inc a

  # xxx: what???
  # for some edge cases (see tdont_return_unowned_from_owned test case)
  m.error.firstMismatch.pos = a
  m.error.firstMismatch.formal = formal

proc semFinishOperands*(c: PContext, n: PNode) =
  # this needs to be called to ensure that after overloading resolution every
  # argument has been sem'checked:
  for i in 1..<n.len:
    n[i] = prepareOperand(c, n[i])

proc partialMatch*(c: PContext, n: PNode, m: var TCandidate) =
  # for 'suggest' support:
  var marker = initIntSet()
  matchesAux(c, n, m, marker)

proc matches*(c: PContext, n: PNode, m: var TCandidate) =
  addInNimDebugUtils(c.config, "matches", n, m)

  if m.magic in {mArrGet, mArrPut}:
    m.state = csMatch
    m.call = n

    # Note the following doesn't work as it would produce ambiguities.
    # We hack system.nim instead: https://github.com/nim-lang/nim/issues/8049.
    # xxx: shouldn't this be a generic match and not an exact one, then?
    when false:
      inc m.genericMatches
      inc m.exactMatches

    return
  
  var marker = initIntSet()
  matchesAux(c, n, m, marker)

  if m.state == csNoMatch:
    return

  # check that every formal parameter got a value:
  for f in 1..<m.callee.n.len:
    let formal = m.callee.n[f].sym
    if not containsOrIncl(marker, formal.position):
      if formal.ast.isNil():            # no default values
        if formal.typ.kind == tyVarargs:
          # For consistency with what happens in `matchesAux` select the
          # container node kind accordingly
          let
            cnKind =
              if formal.typ.isVarargsUntyped:
                nkArgList
              else:
                nkBracket
            container = newNodeIT(cnKind, n.info, arrayConstr(c, n.info))
          
          setSon(m.call, formal.position + 1,
                 implicitConv(nkHiddenStdConv, formal.typ, container, m, c))
        else:
          # no default value
          m.state = csNoMatch
          m.error.firstMismatch.kind = kMissingParam
          m.error.firstMismatch.formal = formal
          break
      else:                            # handle default values
        # given: `proc foo(x: T = 0.0)`, then call `foo()` should work

        let defaultValue =
          case formal.ast.kind
          of nkEmpty:
            # The default param value is set to empty in `instantiateProcType`
            # when the type of the default expression doesn't match the type
            # of the instantiated proc param:
            c.config.newError(m.callee.n[f],
                              reportSym(rsemIncompatibleDefaultExpr, formal))
          of nkNilLit:
            implicitConv(nkHiddenStdConv, formal.typ, copyTree(formal.ast), m, c)
          else:
            copyTree(formal.ast)

        if defaultValue.isError:
          # xxx: change this to propagate
          c.config.localReport(defaultValue)

        if nfDefaultRefsParam in formal.ast.flags:
          m.call.flags.incl nfDefaultRefsParam

        if {tfImplicitTypeParam, tfGenericTypeParam} * formal.typ.flags != {}:
          let existing = PType(idTableGet(m.bindings, formal.typ))

          if existing.isNil() or existing.kind == tyTypeDesc:
            # see bug https://github.com/nim-lang/nim/issues/11600:
            put(m, formal.typ, defaultValue.typ)
        
        defaultValue.flags.incl nfDefaultParam
        setSon(m.call, formal.position + 1, defaultValue)
  
  # forget all inferred types if the overload matching failed
  if m.state == csNoMatch:
    for t in m.inferredTypes:
      if t.len > 1:
        t.sons.setLen 1

proc argtypeMatches*(c: PContext, f, a: PType, fromHlo = false): bool =
  var m = newCandidate(c, f)
  let res = paramTypesMatch(m, f, a, c.graph.emptyNode)
  #instantiateGenericConverters(c, res, m)
  # XXX this is used by patterns.nim too; I think it's better to not
  # instantiate generic converters for that
  if fromHlo:
    # pattern templates do not allow for conversions except from int literal
    res != nil and m.convMatches == 0 and m.intConvMatches in [0, 256]
  else:
    res != nil

when not defined(nimHasSinkInference):
  {.pragma: nosinks.}

proc instTypeBoundOp*(c: PContext; dc: PSym; t: PType; info: TLineInfo;
                      op: TTypeAttachedOp; col: int): PSym {.nosinks.} =
  var m = newCandidate(c, dc.typ)
  if col >= dc.typ.len:
    localReport(c.config, info, reportSym(rsemCannotInstantiate, dc))
    return nil
  var f = dc.typ[col]

  if op == attachedDeepCopy:
    if f.kind in {tyRef, tyPtr}: f = f.lastSon
  else:
    if f.kind in {tyVar}: f = f.lastSon
  if typeRel(m, f, t) == isNone:
    localReport(c.config, info, reportSym(rsemCannotInstantiate, dc))
  else:
    result = c.semGenerateInstance(c, dc, m.bindings, info)
    if op == attachedDeepCopy:
      assert sfFromGeneric in result.flags

when not declared(tests):
  template tests(s: untyped) = discard

tests:
  var dummyOwner = newSym(skModule, getIdent("test_module"), nil, unknownLineInfo)

  proc `|` (t1, t2: PType): PType =
    result = newType(tyOr, dummyOwner)
    result.rawAddSon(t1)
    result.rawAddSon(t2)

  proc `&` (t1, t2: PType): PType =
    result = newType(tyAnd, dummyOwner)
    result.rawAddSon(t1)
    result.rawAddSon(t2)

  proc `!` (t: PType): PType =
    result = newType(tyNot, dummyOwner)
    result.rawAddSon(t)

  proc seq(t: PType): PType =
    result = newType(tySequence, dummyOwner)
    result.rawAddSon(t)

  proc array(x: int, t: PType): PType =
    result = newType(tyArray, dummyOwner)

    var n = newNodeI(nkRange, unknownLineInfo)
    n.add newIntNode(nkIntLit, 0)
    n.add newIntNode(nkIntLit, x)
    let range = newType(tyRange, dummyOwner)

    result.rawAddSon(range)
    result.rawAddSon(t)

  suite "type classes":
    let
      int = newType(tyInt, dummyOwner)
      float = newType(tyFloat, dummyOwner)
      string = newType(tyString, dummyOwner)
      ordinal = newType(tyOrdinal, dummyOwner)
      any = newType(tyAnything, dummyOwner)
      number = int | float

    var TFoo = newType(tyObject, dummyOwner)
    TFoo.sym = newSym(skType, getIdent"TFoo", dummyOwner, unknownLineInfo)

    var T1 = newType(tyGenericParam, dummyOwner)
    T1.sym = newSym(skType, getIdent"T1", dummyOwner, unknownLineInfo)
    T1.sym.position = 0

    var T2 = newType(tyGenericParam, dummyOwner)
    T2.sym = newSym(skType, getIdent"T2", dummyOwner, unknownLineInfo)
    T2.sym.position = 1

    setup:
      var c = newCandidate(nil, nil)

    template yes(x, y) =
      test astToStr(x) & " is " & astToStr(y):
        check typeRel(c, y, x) == isGeneric

    template no(x, y) =
      test astToStr(x) & " is not " & astToStr(y):
        check typeRel(c, y, x) == isNone

    yes seq(any), array(10, int) | seq(any)
    # Sure, seq[any] is directly included

    yes seq(int), seq(any)
    yes seq(int), seq(number)
    # Sure, the int sequence is certainly
    # part of the number sequences (and all sequences)

    no seq(any), seq(float)
    # Nope, seq[any] includes types that are not seq[float] (e.g. seq[int])

    yes seq(int|string), seq(any)
    # Sure

    yes seq(int&string), seq(any)
    # Again

    yes seq(int&string), seq(int)
    # A bit more complicated
    # seq[int&string] is not a real type, but it's analogous to
    # seq[Sortable and Iterable], which is certainly a subset of seq[Sortable]

    no seq(int|string), seq(int|float)
    # Nope, seq[string] is not included in not included in
    # the seq[int|float] set

    no seq(!(int|string)), seq(string)
    # A sequence that is neither seq[int] or seq[string]
    # is obviously not seq[string]

    no seq(!int), seq(number)
    # Now your head should start to hurt a bit
    # A sequence that is not seq[int] is not necessarily a number sequence
    # it could well be seq[string] for example

    yes seq(!(int|string)), seq(!string)
    # all sequnece types besides seq[int] and seq[string]
    # are subset of all sequence types that are not seq[string]

    no seq(!(int|string)), seq(!(string|TFoo))
    # Nope, seq[TFoo] is included in the first set, but not in the second

    no seq(!string), seq(!number)
    # Nope, seq[int] in included in the first set, but not in the second

    yes seq(!number), seq(any)
    yes seq(!int), seq(any)
    no seq(any), seq(!any)
    no seq(!int), seq(!any)

    yes int, ordinal
    no  string, ordinal
