#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/[
    strutils,
    tables,
    intsets,
  ],
  compiler/ast/[
    ast,
    renderer,
    astalgo,
    types,
    idents,
    wordrecg,
    errorreporting,
    errorhandling,
    lineinfos,
    trees,
  ],
  compiler/front/[
   options,
   msgs,
  ],
  compiler/modules/[
    modulegraphs,
    magicsys,
  ],
  compiler/utils/[
    debugutils,
    idioms
  ],
  compiler/sem/[
    varpartitions,
    typeallowed,
    guards,
    semdata,
    nilcheck,
  ]

from compiler/ast/reports_sem import SemReport,
  SemGcUnsafetyKind, # xxx: data type taken over by "reports", unwind this crap
  SemSideEffectCallKind, # xxx: data type taken over by "reports", again...
  reportAst,
  reportSem,
  reportStr,
  reportSym,
  reportSymbols
from compiler/ast/report_enums import ReportKind

when defined(useDfa):
  import dfa

import liftdestructors
include sinkparameter_inference

##[
This module contains the second semantic checking pass over the AST. Necessary
because the old way had some inherent problems. Performs:

- effect+exception tracking
- "usage before definition" checking
- checking of captured entities
- detection of whether a procedure captures something and, if necessary,
  adjusting the calling convention
- also now calls the "lift destructor logic" at strategic positions, this
  is about to be put into the spec:

We treat assignment and sinks and destruction as identical.

In the construct `let/var x = expr()` x's type is marked.

In `x = y` the type of `x` is marked.

For every sink parameter of type `T T` is marked.

For every call `f()` the return type of `f()` is marked.

Captured entities
=================

If a local is used inside procedure X but the local does not belong to X, the
local needs to be captured. Turning the procedure into a top-level procedure
with a hidden environment parameter is later performed by the `lambdalifting`
transformation, but semantic analysis already needs to know which procedures
require the `closure` calling convention, as this information is part of the
procedure's type.

]##

# ------------------------ exception and tag tracking -------------------------

discard """
  exception tracking:

  a() # raises 'x', 'e'
  try:
    b() # raises 'e'
  except e:
    # must not undo 'e' here; hrm
    c()

 --> we need a stack of scopes for this analysis

  # XXX enhance the algorithm to care about 'dirty' expressions:
  lock a[i].L:
    inc i # mark 'i' dirty
    lock a[j].L:
      access a[i], a[j]  # --> reject a[i]
"""

type
  TEffects = object
    exc: PNode  ## stack of exceptions
    tags: PNode ## list of tags
    bottom, inTryStmt, inExceptOrFinallyStmt, leftPartOfAsgn: int
    isReraiseAllowed: int
      ## > 0 if a re-raise statement is allowed
    owner: PSym
    ownerModule: PSym
    init: seq[int] ## list of initialized variables
    guards: TModel ## nested guards
    locked: seq[PNode] ## locked locations
    gcUnsafe, isRecursive, isTopLevel, hasSideEffect, inEnforcedGcSafe: bool
    hasDangerousAssign, isInnerProc: bool
    inEnforcedNoSideEffects: bool
    maxLockLevel, currLockLevel: TLockLevel
    currOptions: TOptions
    config: ConfigRef
    graph: ModuleGraph
    c: PContext
    escapingParams: IntSet
  PEffects = var TEffects

proc `<`(a, b: TLockLevel): bool {.borrow.}
proc `<=`(a, b: TLockLevel): bool {.borrow.}
proc `==`(a, b: TLockLevel): bool {.borrow.}
proc max(a, b: TLockLevel): TLockLevel {.borrow.}

proc createTypeBoundOps(tracked: PEffects, typ: PType; info: TLineInfo) =
  if typ == nil or tfHasMeta in typ.flags: return
  # lift the type-bound operations for the underlying concrete type, not for
  # the wrapper type
  let realType = typ.skipTypes(skipForHooks)
  createTypeBoundOps(tracked.graph, tracked.c, realType, info, tracked.c.idgen)
  if (tfHasAsgn in realType.flags) or
      optSeqDestructors in tracked.config.globalOptions:
    tracked.owner.flags.incl sfInjectDestructors

proc isLocalVar(a: PEffects, s: PSym): bool =
  # and (s.kind != skParam or s.typ.kind == tyOut)
  s.kind in {skVar, skResult} and sfGlobal notin s.flags and
    s.owner == a.owner and s.typ != nil

proc getLockLevel(t: PType): TLockLevel =
  var t = t
  # tyGenericInst(TLock {tyGenericBody}, tyStatic, tyObject):
  if t.kind == tyGenericInst and t.len == 3: t = t[1]
  if t.kind == tyStatic and t.n != nil and t.n.kind in nkIntLiterals:
    assert t.n.kind in nkSIntLiterals
    result = t.n.intVal.TLockLevel

proc lockLocations(a: PEffects; pragma: PNode) =
  if pragma.kind != nkExprColonExpr:
    localReport(a.config, pragma.info, SemReport(kind: rsemLocksRequiresArgs))
    return
  var firstLL = TLockLevel(-1'i16)
  for x in pragma[1]:
    let thisLL = getLockLevel(x.typ)
    if thisLL != 0.TLockLevel:
      if thisLL < 0.TLockLevel or thisLL > MaxLockLevel.TLockLevel:
        localReport(a.config, x.info, reportStr(
          rsemLocksPragmaBadLevel, $thisLL))

      elif firstLL < 0.TLockLevel: firstLL = thisLL
      elif firstLL != thisLL:
        localReport(a.config, x.info, SemReport(kind: rsemMultilockRequiresSameLevel))
      a.maxLockLevel = max(a.maxLockLevel, firstLL)
    a.locked.add x
  if firstLL >= 0.TLockLevel and firstLL != a.currLockLevel:
    if a.currLockLevel > 0.TLockLevel and a.currLockLevel <= firstLL:
      localReport(a.config, pragma.info, SemReport(kind: rsemInvalidNestedLocking))
    a.currLockLevel = firstLL

proc guardGlobal(a: PEffects; n: PNode; guard: PSym) =
  # check whether the corresponding lock is held:
  for L in a.locked:
    if L.kind == nkSym and L.sym == guard: return
  # we allow accesses nevertheless in top level statements for
  # easier initialization:
  #if a.isTopLevel:
  #  message(a.config, n.info, warnUnguardedAccess, renderTree(n))
  #else:
  if not a.isTopLevel:
    localReport(
      a.config, n.info, reportAst(rsemUnguardedAccess, n))

# 'guard*' are checks which are concerned with 'guard' annotations
# (var x{.guard: y.}: int)
proc guardDotAccess(a: PEffects; n: PNode) =
  let ri = n[1]
  if ri.kind != nkSym or ri.sym.kind != skField: return
  var g = ri.sym.guard
  if g.isNil or a.isTopLevel: return
  # fixup guard:
  if g.kind == skUnknown:
    var field: PSym = nil
    var ty = n[0].typ.skipTypes(abstractPtrs)
    if ty.kind == tyTuple and not ty.n.isNil:
      field = lookupInRecord(ty.n, g.name)
    else:
      while ty != nil and ty.kind == tyObject:
        field = lookupInRecord(ty.n, g.name)
        if field != nil: break
        ty = ty[0]
        if ty == nil: break
        ty = ty.skipTypes(skipPtrs)
    if field == nil:
      localReport(a.config, n.info, reportSym(rsemInvalidGuardField, g))
      return
    g = field
    #ri.sym.guard = field
    # XXX unfortunately this is not correct for generic instantiations!
  if g.kind == skField:
    let dot = newNodeI(nkDotExpr, n.info, 2)
    dot[0] = n[0]
    dot[1] = newSymNode(g)
    dot.typ = g.typ
    for L in a.locked:
      #if a.guards.sameSubexprs(dot, L): return
      if guards.sameTree(dot, L): return
    localReport(
      a.config, n.info, reportAst(rsemUnguardedAccess, n))

  else:
    guardGlobal(a, n, g)

proc initVar(a: PEffects, n: PNode) =
  if n.kind != nkSym: return
  let s = n.sym
  if isLocalVar(a, s):
    for x in a.init:
      if x == s.id: return
    a.init.add s.id

proc initVarViaNew(a: PEffects, n: PNode) =
  if n.kind != nkSym: return
  let s = n.sym
  if {tfRequiresInit, tfNotNil} * s.typ.flags <= {tfNotNil}:
    # 'x' is not nil, but that doesn't mean its "not nil" children
    # are initialized:
    initVar(a, n)

proc warnAboutGcUnsafe(n: PNode; conf: ConfigRef) =
  localReport(conf, n.info, reportAst(rsemWarnGcUnsafe, n))

proc markGcUnsafe(a: PEffects; reason: PSym) =
  if not a.inEnforcedGcSafe:
    a.gcUnsafe = true
    if a.owner.kind in routineKinds - {skMacro}:
      a.owner.gcUnsafetyReason = reason

proc markGcUnsafe(a: PEffects; reason: PNode) =
  if not a.inEnforcedGcSafe:
    a.gcUnsafe = true
    if a.owner.kind in routineKinds - {skMacro}:
      if reason.kind == nkSym:
        a.owner.gcUnsafetyReason = reason.sym
      else:
        a.owner.gcUnsafetyReason = newSym(skUnknown, a.owner.name, nextSymId a.c.idgen,
                                          a.owner, reason.info, {})

proc markSideEffect(a: PEffects; reason: PNode | PSym; useLoc: TLineInfo) =
  if not a.inEnforcedNoSideEffects:
    a.hasSideEffect = true
    if a.owner.kind in routineKinds:
      var sym: PSym
      when reason is PNode:
        if reason.kind == nkSym:
          sym = reason.sym
        else:
          let kind = if reason.kind == nkHiddenDeref: skParam else: skUnknown
          sym = newSym(kind, a.owner.name, nextSymId a.c.idgen, a.owner, reason.info, {})
      else:
        sym = reason
      a.c.sideEffects.mgetOrPut(a.owner.id, @[]).add (useLoc, sym)
    when false: markGcUnsafe(a, reason)

proc listGcUnsafety(
    s: PSym; onlyWarning: bool; cycleCheck: var IntSet; conf: ConfigRef) =
  proc aux(
      s: PSym,
      onlyWarning: bool,
      cycleCheck: var IntSet,
      conf: ConfigRef,
    ) =

    let u = s.gcUnsafetyReason
    if u != nil and not cycleCheck.containsOrIncl(u.id):
      var reason: SemGcUnsafetyKind
      case u.kind
      of skLet, skVar:
        if u.typ.skipTypes(abstractInst).kind == tyProc:
          reason = sgcuCallsUnsafe
        else:
          reason = sgcuAccessesGcGlobal
      of routineKinds:
        # recursive call *always* produces only a warning so the full error
        # message is printed:
        aux(u, true, cycleCheck, conf)
        reason = sgcuCallsUnsafe
      of skParam, skForVar:
        reason = sgcuIndirectCallVia
      else:
        reason = sgcuIndirectCallHere

      var report = reportSem:
        if onlyWarning:
          rsemWarnGcUnsafeListing
        else:
          rsemErrGcUnsafeListing

      report.gcUnsafeTrace = (
        isUnsafe: s,
        unsafeVia: u,
        unsafeRelation: reason
      )

      conf.localReport(
        if reason == sgcuIndirectCallHere: u.info else: s.info,
        report)

  aux(s, onlyWarning, cycleCheck, conf)

proc listGcUnsafety(s: PSym; onlyWarning: bool; conf: ConfigRef) =
  var cycleCheck = initIntSet()
  listGcUnsafety(s, onlyWarning, cycleCheck, conf)

proc listSideEffects(
    result: var SemReport,
    s: PSym,
    cycleCheck: var IntSet,
    conf: ConfigRef,
    context: PContext,
    level: int
  ) =

  if context.sideEffects.hasKey(s.id):
    for (useLineInfo, u) in context.sideEffects[s.id]:
      if u != nil and not cycleCheck.containsOrIncl(u.id):
        var trace: SemSideEffectCallKind
        case u.kind
        of skLet, skVar:
          trace = ssefUsesGlobalState
        of routineKinds:
          trace = ssefCallsSideEffect
        of skParam, skForVar:
          trace = ssefCallsViaHiddenIndirection
        else:
          trace = ssefCallsViaIndirection

        result.sideEffectTrace.add((
          isUnsafe: s,
          unsafeVia: u,
          trace: trace,
          location: useLineInfo,
          level: level
        ))

        if u.kind in routineKinds:
          listSideEffects(result, u, cycleCheck, conf, context, level + 1)


proc listSideEffects(result: var SemReport; s: PSym; conf: ConfigRef; context: PContext) =
  var cycleCheck = initIntSet()
  result.sym = s
  listSideEffects(result, s, cycleCheck, conf, context, 1)

proc illegalCapture(s: PSym): bool {.inline.} =
  result = classifyViewType(s.typ) != noView or s.kind == skResult

proc useVarNoInitCheck(a: PEffects; n: PNode; s: PSym) =
  if {sfGlobal, sfThread} * s.flags != {} and s.kind in {skVar, skLet} and
      s.magic != mNimvm:
    if s.guard != nil: guardGlobal(a, n, s.guard)
    if {sfGlobal, sfThread} * s.flags == {sfGlobal} and
        (tfHasGCedMem in s.typ.flags or s.typ.isGCedMem):
      #if a.config.hasWarn(warnGcUnsafe): warnAboutGcUnsafe(n)
      markGcUnsafe(a, s)
    markSideEffect(a, s, n.info)
  if s.owner != a.owner and s.kind in {skVar, skLet, skForVar, skResult, skParam} and
     {sfGlobal, sfThread} * s.flags == {}:
    a.isInnerProc = true

  if s.magic != mNimvm and # XXX: workaround for ``when nimvm`` reaching here
     s.kind notin routineKinds and
     sfCompileTime in s.flags and
     not inCompileTimeOnlyContext(a.c):
    # a .compileTime location is used outside of a compile-time-only
    # context, which is disallowed
    localReport(a.config, n.info, reportSym(rsemIllegalCompileTimeAccess, s))

proc useVar(a: PEffects, n: PNode) =
  let s = n.sym
  if a.inExceptOrFinallyStmt > 0:
    incl s.flags, sfUsedInFinallyOrExcept
  if isLocalVar(a, s):
    if sfNoInit in s.flags:
      # If the variable is explicitly marked as .noinit. do not emit any error
      a.init.add s.id
    elif s.id notin a.init:
      if s.typ.requiresInit:
        localReport(a.config, n.info, reportSym(rsemProveInit, s))
      elif a.leftPartOfAsgn <= 0:
        localReport(a.config, n.info, reportSym(rsemUninit, s))
      # prevent superfluous warnings about the same variable:
      a.init.add s.id

  if s.kind in skLocalVars and
     sfGlobal notin s.flags and       # must not be a global
     a.owner.kind in routineKinds and # only perform the check inside procedures
     s.owner != a.owner and
     illegalCapture(s):
    # XXX: it'd be better if this error would be detected during the first
    #      semantic pass already, but detecting an access to an outer entity
    #      is not easily doable because of typed template/macro parameters
    localReport(a.config, n.info, reportSym(
      rsemIllegalMemoryCapture, s))

  useVarNoInitCheck(a, n, s)


type
  TIntersection = seq[tuple[id, count: int]] # a simple count table

proc addToIntersection(inter: var TIntersection, s: int) =
  for j in 0..<inter.len:
    if s == inter[j].id:
      inc inter[j].count
      return
  inter.add((id: s, count: 1))

proc throws(tracked, n, orig: PNode) =
  if n.typ == nil or n.typ.kind != tyError:
    if orig != nil:
      let x = copyTree(orig)
      x.typ = n.typ
      tracked.add x
    else:
      tracked.add n

proc getEbase(g: ModuleGraph; info: TLineInfo): PType =
  result = g.sysTypeFromName(info, "Exception")

proc excType(g: ModuleGraph; n: PNode): PType =
  # reraise is like raising E_Base:
  let t = if n.kind == nkEmpty or n.typ.isNil: getEbase(g, n.info) else: n.typ
  result = skipTypes(t, skipPtrs)

proc createRaise(g: ModuleGraph; n: PNode): PNode =
  result = newNode(nkType)
  result.typ = getEbase(g, n.info)
  if not n.isNil: result.info = n.info

proc createTag(g: ModuleGraph; n: PNode): PNode =
  result = newNode(nkType)
  result.typ = g.sysTypeFromName(n.info, "RootEffect")
  if not n.isNil: result.info = n.info

proc addRaiseEffect(a: PEffects, e, comesFrom: PNode) =
  #assert e.kind != nkRaiseStmt
  var aa = a.exc
  for i in a.bottom..<aa.len:
    # we only track the first node that can have the effect E in order
    # to safe space and time.
    if sameType(a.graph.excType(aa[i]), a.graph.excType(e)): return

  if e.typ != nil and not isDefectException(e.typ):
    throws(a.exc, e, comesFrom)

proc addTag(a: PEffects, e, comesFrom: PNode) =
  var aa = a.tags
  for i in 0..<aa.len:
    # we only track the first node that can have the effect E in order
    # to safe space and time.
    if sameType(aa[i].typ.skipTypes(skipPtrs), e.typ.skipTypes(skipPtrs)): return
  throws(a.tags, e, comesFrom)

proc mergeRaises(a: PEffects, b, comesFrom: PNode) =
  if b.isNil:
    addRaiseEffect(a, createRaise(a.graph, comesFrom), comesFrom)
  else:
    for effect in items(b): addRaiseEffect(a, effect, comesFrom)

proc mergeTags(a: PEffects, b, comesFrom: PNode) =
  if b.isNil:
    addTag(a, createTag(a.graph, comesFrom), comesFrom)
  else:
    for effect in items(b): addTag(a, effect, comesFrom)

proc listEffects(a: PEffects) =
  var report = reportSem(rsemEffectsListingHint)
  for e in items(a.exc):
    report.effectListing.exceptions.add e.typ

  for e in items(a.tags):
    report.effectListing.tags.add e.typ

  a.config.localReport(report)


  #if a.maxLockLevel != 0:
  #  message(e.info, hintUser, "lockLevel: " & a.maxLockLevel)

proc catches(tracked: PEffects, e: PType) =
  let e = skipTypes(e, skipPtrs)
  var L = tracked.exc.len
  var i = tracked.bottom
  while i < L:
    # r supertype of e?
    if safeInheritanceDiff(e, tracked.graph.excType(tracked.exc[i])) <= 0:
      tracked.exc[i] = tracked.exc[L-1]
      dec L
    else:
      inc i
  if tracked.exc.len > 0:
    setLen(tracked.exc.sons, L)
  else:
    assert L == 0

proc catchesAll(tracked: PEffects) =
  if tracked.exc.len > 0:
    setLen(tracked.exc.sons, tracked.bottom)

proc track(tracked: PEffects, n: PNode)
proc trackTryStmt(tracked: PEffects, n: PNode) =
  let oldBottom = tracked.bottom
  tracked.bottom = tracked.exc.len

  let oldState = tracked.init.len
  var inter: TIntersection = @[]

  inc tracked.inTryStmt
  track(tracked, n[0])
  dec tracked.inTryStmt
  for i in oldState..<tracked.init.len:
    addToIntersection(inter, tracked.init[i])

  var branches = 1
  var hasFinally = false
  inc tracked.inExceptOrFinallyStmt

  # Collect the exceptions caught by the except branches
  for i in 1..<n.len:
    let b = n[i]
    if b.kind == nkExceptBranch:
      inc branches
      if b.len == 1:
        catchesAll(tracked)
      else:
        for j in 0..<b.len - 1:
          if b[j].isInfixAs():
            assert(b[j][1].kind == nkType)
            catches(tracked, b[j][1].typ)
            createTypeBoundOps(tracked, b[j][2].typ, b[j][2].info)
          else:
            assert(b[j].kind == nkType)
            catches(tracked, b[j].typ)
    else:
      assert b.kind == nkFinally
  # Add any other exception raised in the except bodies
  for i in 1..<n.len:
    let b = n[i]
    if b.kind == nkExceptBranch:
      setLen(tracked.init, oldState)
      inc tracked.isReraiseAllowed
      track(tracked, b[^1])
      dec tracked.isReraiseAllowed
      for i in oldState..<tracked.init.len:
        addToIntersection(inter, tracked.init[i])
    else:
      setLen(tracked.init, oldState)
      let prev = tracked.isReraiseAllowed
      # re-raising in a finally clause would allow handling the exception,
      # which is illegal. Therefore, re-raising is disallowed:
      tracked.isReraiseAllowed = 0
      track(tracked, b[^1])
      tracked.isReraiseAllowed = prev
      hasFinally = true

  tracked.bottom = oldBottom
  dec tracked.inExceptOrFinallyStmt
  if not hasFinally:
    setLen(tracked.init, oldState)
  for id, count in items(inter):
    if count == branches: tracked.init.add id

proc isIndirectCall(tracked: PEffects; n: PNode): bool =
  # we don't count f(...) as an indirect call if 'f' is an parameter.
  # Instead we track expressions of type tyProc too. See the manual for
  # details:
  if n.kind != nkSym:
    result = true
  elif n.sym.kind == skParam:
    if strictEffects in tracked.c.features:
      if tracked.owner == n.sym.owner and sfEffectsDelayed in n.sym.flags:
        result = false # it is not a harmful call
      else:
        result = true
    else:
      result = tracked.owner != n.sym.owner or tracked.owner == nil
  elif n.sym.kind notin routineKinds:
    result = true

proc isForwardedProc(n: PNode): bool =
  result = n.kind == nkSym and sfForward in n.sym.flags

proc trackPragmaStmt(tracked: PEffects, n: PNode) =
  for i in 0..<n.len:
    var it = n[i]
    let pragma = whichPragma(it)
    if pragma == wEffects:
      # list the computed effects up to here:
      listEffects(tracked)

template notGcSafe(t): untyped = {tfGcSafe, tfNoSideEffect} * t.flags == {}

proc importedFromC(n: PNode): bool =
  # when imported from C, we assume GC-safety.
  result = n.kind == nkSym and sfImportc in n.sym.flags

proc getLockLevel(s: PSym): TLockLevel =
  result = s.typ.lockLevel
  if result == UnspecifiedLockLevel:
    if {sfImportc, sfNoSideEffect} * s.flags != {} or
       tfNoSideEffect in s.typ.flags:
      result = 0.TLockLevel
    else:
      result = UnknownLockLevel
      #message(??.config, s.info, warnUser, "FOR THIS " & s.name.s)

proc mergeLockLevels(tracked: PEffects, n: PNode, lockLevel: TLockLevel) =
  if lockLevel >= tracked.currLockLevel:
    # if in lock section:
    if tracked.currLockLevel > 0.TLockLevel:
      localReport(tracked.config, n.info,
        SemReport(
          kind: rsemLockLevelMismatch,
          lockMismatch: ($tracked.currLockLevel, $lockLevel)))

    tracked.maxLockLevel = max(tracked.maxLockLevel, lockLevel)

proc propagateEffects(tracked: PEffects, n: PNode, s: PSym) =
  let pragma = s.ast[pragmasPos]
  let spec = effectSpec(pragma, wRaises)
  mergeRaises(tracked, spec, n)

  let tagSpec = effectSpec(pragma, wTags)
  mergeTags(tracked, tagSpec, n)

  if notGcSafe(s.typ) and sfImportc notin s.flags:
    warnAboutGcUnsafe(n, tracked.config)
    markGcUnsafe(tracked, s)

  if tfNoSideEffect notin s.typ.flags:
    markSideEffect(tracked, s, n.info)
  mergeLockLevels(tracked, n, s.getLockLevel)

proc procVarCheck(n: PNode; conf: ConfigRef) =
  if n.kind in nkSymChoices:
    for x in n:
      procVarCheck(x, conf)

  elif n.kind == nkSym and n.sym.magic != mNone and n.sym.kind in routineKinds:
    localReport(conf, n.info, reportSym(rsemCantPassProcvar, n.sym))

proc notNilCheck(tracked: PEffects, n: PNode, paramType: PType) =
  let n = n.skipConv
  if paramType.isNil or paramType.kind != tyTypeDesc:
    procVarCheck skipConvCastAndClosure(n), tracked.config
  #elif n.kind in nkSymChoices:
  #  echo "came here"
  let paramType = paramType.skipTypesOrNil(abstractInst)
  if paramType != nil and tfNotNil in paramType.flags and n.typ != nil:
    let ntyp = n.typ.skipTypesOrNil({tyVar, tyLent, tySink})
    if ntyp != nil and tfNotNil notin ntyp.flags:
      if isAddrNode(n):
        # addr(x[]) can't be proven, but addr(x) can:
        if not containsNode(n, {nkDerefExpr, nkHiddenDeref}): return
      elif (n.kind == nkSym and n.sym.kind in routineKinds) or
          (n.kind in procDefs + {nkObjConstr, nkBracket, nkClosure} + nkStrLiterals) or
          (n.kind in nkCallKinds and n[0].kind == nkSym and n[0].sym.magic == mArrToSeq) or
          n.typ.kind == tyTypeDesc:
        # 'p' is not nil obviously:
        return
      case impliesNotNil(tracked.guards, n)
      of impUnknown:
        localReport(tracked.config, n.info, reportAst(rsemCannotProveNotNil, n))

      of impNo:
        localReport(tracked.config, n.info, reportAst(rsemProvablyNil, n))

      of impYes:
        discard

proc discriminantAsgnCheck(tracked: PEffects, n: PNode) =
  ## Checks whether the assignment `n` is the assignment of a discriminant and,
  ## if it is, whether it's legal. If it's not, an error is reported
  let dest = n[0]
  if not isDiscriminantField(dest):
    return

  let objType =
    case dest.kind
    of nkCheckedFieldExpr: dest[0][0].typ
    of nkDotExpr:          dest[0].typ
    else:                  unreachable(dest.kind)

  if hasDestructor(objType):
    let destructor = getAttachedOp(tracked.graph, objType, attachedDestructor)
    if destructor != nil and sfOverriden in destructor.flags:
      # the destructor is user-provided, which disallows lifting one for the
      # discriminant assignment -- the code is ill-formed
      localReport(tracked.config, n):
        reportSem rsemCannotAssignToDiscriminantWithCustomDestructor

proc assumeTheWorst(tracked: PEffects; n: PNode; op: PType) =
  addRaiseEffect(tracked, createRaise(tracked.graph, n), nil)
  addTag(tracked, createTag(tracked.graph, n), nil)
  let lockLevel = if op.lockLevel == UnspecifiedLockLevel: UnknownLockLevel
                  else: op.lockLevel
  #if lockLevel == UnknownLockLevel:
  #  message(??.config, n.info, warnUser, "had to assume the worst here")
  mergeLockLevels(tracked, n, lockLevel)

proc isOwnedProcVar(tracked: PEffects; n: PNode): bool =
  # XXX prove the soundness of this effect system rule
  result = n.kind == nkSym and n.sym.kind == skParam and
    tracked.owner == n.sym.owner
  #if result and sfPolymorphic notin n.sym.flags:
  #  echo tracked.config $ n.info, " different here!"
  if strictEffects in tracked.c.features:
    result = result and sfEffectsDelayed in n.sym.flags

proc isNoEffectList(n: PNode): bool {.inline.} =
  assert n.kind == nkEffectList
  n.len == 0 or (n[tagEffects] == nil and n[exceptionEffects] == nil)

proc isTrival(caller: PNode): bool {.inline.} =
  result = caller.kind == nkSym and caller.sym.magic in {mEqProc, mIsNil, mMove, mWasMoved, mSwap}

proc trackOperandForIndirectCall(tracked: PEffects, n: PNode, formals: PType; argIndex: int; caller: PNode) =
  let a = skipConvCastAndClosure(n)
  let op = a.typ
  let param =
    if formals != nil and argIndex < formals.len and formals.n != nil:
      formals.n[argIndex].sym
    else:
      nil

  # assume indirect calls are taken here:
  if op != nil and op.kind == tyProc and n.skipConv.kind != nkNilLit and
      not isTrival(caller) and
      ((param != nil and sfEffectsDelayed in param.flags) or strictEffects notin tracked.c.features):

    internalAssert(tracked.config, op.n[0].kind == nkEffectList, "Expected effect list node kind")
    var effectList = op.n[0]
    var s = n.skipConv
    if s.kind == nkCast and s[1].typ.kind == tyProc:
      s = s[1]
    if s.kind == nkSym and s.sym.kind in routineKinds and isNoEffectList(effectList):
      propagateEffects(tracked, n, s.sym)
    elif isNoEffectList(effectList):
      if isForwardedProc(n):
        # we have no explicit effects but it's a forward declaration and so it's
        # stated there are no additional effects, so simply propagate them:
        propagateEffects(tracked, n, n.sym)
      elif not isOwnedProcVar(tracked, a):
        # we have no explicit effects so assume the worst:
        assumeTheWorst(tracked, n, op)
      # assume GcUnsafe unless in its type; 'forward' does not matter:
      if notGcSafe(op) and not isOwnedProcVar(tracked, a):
        warnAboutGcUnsafe(n, tracked.config)
        markGcUnsafe(tracked, a)
      elif tfNoSideEffect notin op.flags and not isOwnedProcVar(tracked, a):
        markSideEffect(tracked, a, n.info)
    else:
      mergeRaises(tracked, effectList[exceptionEffects], n)
      mergeTags(tracked, effectList[tagEffects], n)
      if notGcSafe(op):
        warnAboutGcUnsafe(n, tracked.config)
        markGcUnsafe(tracked, a)

      elif tfNoSideEffect notin op.flags:
        markSideEffect(tracked, a, n.info)
  let paramType = if formals != nil and argIndex < formals.len: formals[argIndex] else: nil
  if paramType != nil and paramType.kind == tyProc and tfGcSafe in paramType.flags:
    let argtype = skipTypes(a.typ, abstractInst)
    # XXX figure out why this can be a non tyProc here. See httpclient.nim for an
    # example that triggers it.
    if argtype.kind == tyProc and notGcSafe(argtype) and not tracked.inEnforcedGcSafe:
      localReport(tracked.config, n.info, reportAst(rsemErrGcUnsafe, n))
  notNilCheck(tracked, n, paramType)

proc breaksBlock(n: PNode): bool =
  # semantic check doesn't allow statements after raise, break, return or
  # call to noreturn proc, so it is safe to check just the last statements
  var it = n
  while it.kind in {nkStmtList, nkStmtListExpr} and it.len > 0:
    it = it.lastSon

  result = it.kind in {nkBreakStmt, nkReturnStmt, nkRaiseStmt} or
    it.kind in nkCallKinds and it[0].kind == nkSym and sfNoReturn in it[0].sym.flags

proc trackCase(tracked: PEffects, n: PNode) =
  track(tracked, n[0])
  let oldState = tracked.init.len
  let oldFacts = tracked.guards.s.len
  let stringCase = n[0].typ != nil and skipTypes(n[0].typ,
        abstractVarRange-{tyTypeDesc}).kind in {tyFloat..tyFloat64, tyString}
  let interesting = not stringCase and interestingCaseExpr(n[0]) and
        tracked.config.hasWarn(rsemProveField)
  var inter: TIntersection = @[]
  var toCover = 0
  for i in 1..<n.len:
    let branch = n[i]
    setLen(tracked.init, oldState)
    if interesting:
      setLen(tracked.guards.s, oldFacts)
      addCaseBranchFacts(tracked.guards, n, i)
    for i in 0..<branch.len:
      track(tracked, branch[i])
    if not breaksBlock(branch.lastSon): inc toCover
    for i in oldState..<tracked.init.len:
      addToIntersection(inter, tracked.init[i])

  setLen(tracked.init, oldState)
  if not stringCase or lastSon(n).kind == nkElse:
    for id, count in items(inter):
      if count >= toCover: tracked.init.add id
    # else we can't merge
  setLen(tracked.guards.s, oldFacts)

proc trackIf(tracked: PEffects, n: PNode) =
  track(tracked, n[0][0])
  let oldFacts = tracked.guards.s.len
  addFact(tracked.guards, n[0][0])
  let oldState = tracked.init.len

  var inter: TIntersection = @[]
  var toCover = 0
  track(tracked, n[0][1])
  if not breaksBlock(n[0][1]): inc toCover
  for i in oldState..<tracked.init.len:
    addToIntersection(inter, tracked.init[i])

  for i in 1..<n.len:
    let branch = n[i]
    setLen(tracked.guards.s, oldFacts)
    for j in 0..i-1:
      addFactNeg(tracked.guards, n[j][0])
    if branch.len > 1:
      addFact(tracked.guards, branch[0])
    setLen(tracked.init, oldState)
    for i in 0..<branch.len:
      track(tracked, branch[i])
    if not breaksBlock(branch.lastSon): inc toCover
    for i in oldState..<tracked.init.len:
      addToIntersection(inter, tracked.init[i])
  setLen(tracked.init, oldState)
  if lastSon(n).len == 1:
    for id, count in items(inter):
      if count >= toCover: tracked.init.add id
    # else we can't merge as it is not exhaustive
  setLen(tracked.guards.s, oldFacts)

proc trackBlock(tracked: PEffects, n: PNode) =
  if n.kind in {nkStmtList, nkStmtListExpr}:
    var oldState = -1
    for i in 0..<n.len:
      if hasSubnodeWith(n[i], nkBreakStmt):
        # block:
        #   x = def
        #   if ...: ... break # some nested break
        #   y = def
        # --> 'y' not defined after block!
        if oldState < 0: oldState = tracked.init.len
      track(tracked, n[i])
    if oldState > 0: setLen(tracked.init, oldState)
  else:
    track(tracked, n)

proc checkLe(c: PEffects; a, b: PNode) =
  case proveLe(c.guards, a, b)
  of impUnknown:
    c.config.localReport(a.info):
      block:
        var r = reportSem(rsemStaticIndexLeqUnprovable)
        r.rangeExpression = (a, b)
        r
  of impYes:
    discard
  of impNo:
    c.config.localReport(a.info):
      block:
        var r = reportSem(rsemStaticIndexGeProvable)
        r.rangeExpression = (a, b)
        r

proc checkBounds(c: PEffects; arr, idx: PNode) =
  checkLe(c, lowBound(c.config, arr), idx)
  checkLe(c, idx, highBound(c.config, arr, c.guards.g.operators))

proc checkRange(c: PEffects; value: PNode; typ: PType) =
  let t = typ.skipTypes(abstractInst - {tyRange})
  if t.kind == tyRange:
    let lowBound = copyTree(t.n[0])
    lowBound.info = value.info
    let highBound = copyTree(t.n[1])
    highBound.info = value.info
    checkLe(c, lowBound, value)
    checkLe(c, value, highBound)

proc objConvCheck(config: ConfigRef, n: PNode) =
  case n.kind
  of nkHiddenSubConv:
    if n.typ.skipTypes({tyGenericInst, tyAlias, tySink}).kind == tyObject:
      # the result of an implicit, narrowing object conversion is read as
      # a whole (e.g., passed to a non-var parameter, assigned somewhere,
      # etc.). Information is lost in this case, so a warning is reported
      localReport(config, n[1], SemReport(
        kind: rsemImplicitObjConv,
        typeMismatch: @[typeMismatch(formal = n.typ, actual = n[1].typ)]))
    objConvCheck(config, n[^1])
  of nkStmtListExpr, nkIfExpr, nkBlockExpr, nkElifExpr, nkElseExpr,
     nkExceptBranch:
    objConvCheck(config, n[^1])
  of nkCaseStmt:
    for i in 1..<n.len:
      objConvCheck(config, n[i][^1])
  of nkTryStmt, nkHiddenTryStmt:
    for it in n.items:
      if it.kind != nkFinally:
        objConvCheck(config, it)
  else:
    discard

proc trackCall(tracked: PEffects; n: PNode) =
  template gcsafeAndSideeffectCheck() =
    if notGcSafe(op) and not importedFromC(a):
      # and it's not a recursive call:
      if not (a.kind == nkSym and a.sym == tracked.owner):
        warnAboutGcUnsafe(n, tracked.config)
        markGcUnsafe(tracked, a)

    if tfNoSideEffect notin op.flags and not importedFromC(a):
      # and it's not a recursive call:
      if not (a.kind == nkSym and a.sym == tracked.owner):
        markSideEffect(tracked, a, n.info)

  # p's effects are ours too:
  var a = n[0]
  #if canRaise(a):
  #  echo "this can raise ", tracked.config $ n.info
  let op = a.typ
  if n.typ != nil:
    if tracked.owner.kind != skMacro and n.typ.skipTypes(abstractVar).kind != tyOpenArray:
      createTypeBoundOps(tracked, n.typ, n.info)
  if true:
    if a.kind == nkCast and a[1].typ.kind == tyProc:
      a = a[1]
    # XXX: in rare situations, templates and macros will reach here after
    # calling getAst(templateOrMacro()). Currently, templates and macros
    # are indistinguishable from normal procs (both have tyProc type) and
    # we can detect them only by checking for attached nkEffectList.
    if op != nil and op.kind == tyProc and op.n[0].kind == nkEffectList:
      if a.kind == nkSym:
        if a.sym == tracked.owner: tracked.isRecursive = true
        # even for recursive calls we need to check the lock levels (!):
        mergeLockLevels(tracked, n, a.sym.getLockLevel)
        if sfSideEffect in a.sym.flags: markSideEffect(tracked, a, n.info)
      else:
        mergeLockLevels(tracked, n, op.lockLevel)
      var effectList = op.n[0]
      if a.kind == nkSym and a.sym.kind == skMethod:
        propagateEffects(tracked, n, a.sym)
      elif isNoEffectList(effectList):
        if isForwardedProc(a):
          propagateEffects(tracked, n, a.sym)
        elif isIndirectCall(tracked, a):
          assumeTheWorst(tracked, n, op)
          gcsafeAndSideeffectCheck()
      else:
        mergeRaises(tracked, effectList[exceptionEffects], n)
        mergeTags(tracked, effectList[tagEffects], n)
        gcsafeAndSideeffectCheck()
    if a.kind != nkSym or a.sym.magic notin {mFinished}:
      for i in 1..<n.len:
        trackOperandForIndirectCall(tracked, n[i], op, i, a)
    if a.kind == nkSym and a.sym.magic == mNewSeq:
      # may not look like an assignment, but it is:
      let arg = n[1]
      initVarViaNew(tracked, arg)
      if arg.typ.len != 0 and {tfRequiresInit} * arg.typ.lastSon.flags != {}:
        if a.sym.magic == mNewSeq and n[2].kind in nkIntLiterals and
            n[2].intVal == 0:
          # var s: seq[notnil];  newSeq(s, 0)  is a special case!
          discard
        else:
          localReport(tracked.config, arg.info, reportAst(rsemProveInit, arg))

      # check required for 'nim check':
      if n[1].typ.len > 0:
        createTypeBoundOps(tracked, n[1].typ.lastSon, n.info)
        createTypeBoundOps(tracked, n[1].typ, n.info)

    elif a.kind == nkSym and a.sym.magic in {mArrGet, mArrPut} and
        optStaticBoundsCheck in tracked.currOptions:
      checkBounds(tracked, n[1], n[2])

    if a.kind != nkSym or a.sym.magic != mRunnableExamples:
      for i in 0..<n.safeLen:
        track(tracked, n[i])
        objConvCheck(tracked.config, n[i])

  if a.kind == nkSym and a.sym.name.s.len > 0 and a.sym.name.s[0] == '=' and
        tracked.owner.kind != skMacro:
    var opKind = find(AttachedOpToStr, a.sym.name.s.normalize)
    if a.sym.name.s == "=": opKind = attachedAsgn.int
    if opKind != -1:
      # rebind type bounds operations after createTypeBoundOps call
      let t = n[1].typ.skipTypes(skipForHooks + {tyVar})
      if a.sym != getAttachedOp(tracked.graph, t, TTypeAttachedOp(opKind)):
        createTypeBoundOps(tracked, t, n.info)
        let op = getAttachedOp(tracked.graph, t, TTypeAttachedOp(opKind))
        if op != nil:
          n[0].sym = op

  if op != nil and op.kind == tyProc:
    for i in 1..<min(n.safeLen, op.len):
      case op[i].kind
      of tySink:
        createTypeBoundOps(tracked,  op[i][0], n.info)
        checkForSink(tracked.config, tracked.c.idgen, tracked.owner, n[i])
      of tyVar:
        tracked.hasDangerousAssign = true
      #of tyOut:
      # consider this case: p(out x, x); we want to remark that 'x' is not
      # initialized until after the call. Since we do this after we analysed the
      # call, this is fine.
      # initVar(tracked, n[i].skipAddr, false)
      else: discard

type
  PragmaBlockContext = object
    oldLocked: int
    oldLockLevel: TLockLevel
    enforcedGcSafety, enforceNoSideEffects: bool
    oldExc, oldTags: int
    exc, tags: PNode

proc createBlockContext(tracked: PEffects): PragmaBlockContext =
  result = PragmaBlockContext(oldLocked: tracked.locked.len,
    oldLockLevel: tracked.currLockLevel,
    enforcedGcSafety: false, enforceNoSideEffects: false,
    oldExc: tracked.exc.len, oldTags: tracked.tags.len)

proc applyBlockContext(tracked: PEffects, bc: PragmaBlockContext) =
  if bc.enforcedGcSafety: tracked.inEnforcedGcSafe = true
  if bc.enforceNoSideEffects: tracked.inEnforcedNoSideEffects = true

proc unapplyBlockContext(tracked: PEffects; bc: PragmaBlockContext) =
  if bc.enforcedGcSafety: tracked.inEnforcedGcSafe = false
  if bc.enforceNoSideEffects: tracked.inEnforcedNoSideEffects = false
  setLen(tracked.locked, bc.oldLocked)
  tracked.currLockLevel = bc.oldLockLevel
  if bc.exc != nil:
    # beware that 'raises: []' is very different from not saying
    # anything about 'raises' in the 'cast' at all. Same applies for 'tags'.
    setLen(tracked.exc.sons, bc.oldExc)
    for e in bc.exc:
      addRaiseEffect(tracked, e, e)
  if bc.tags != nil:
    setLen(tracked.tags.sons, bc.oldTags)
    for t in bc.tags:
      addTag(tracked, t, t)

proc castBlock(tracked: PEffects, pragma: PNode, bc: var PragmaBlockContext) =
  case whichPragma(pragma)
  of wGcSafe:
    bc.enforcedGcSafety = true
  of wNoSideEffect:
    bc.enforceNoSideEffects = true
  of wTags:
    let n = pragma[1]
    if n.kind in {nkCurly, nkBracket}:
      bc.tags = n
    else:
      bc.tags = newNodeI(nkArgList, pragma.info)
      bc.tags.add n
  of wRaises:
    let n = pragma[1]
    if n.kind in {nkCurly, nkBracket}:
      bc.exc = n
    else:
      bc.exc = newNodeI(nkArgList, pragma.info)
      bc.exc.add n
  of wUncheckedAssign:
    discard "handled in sempass1"
  else:
    localReport(tracked.config, pragma.info, reportAst(
      rsemInvalidPragmaBlock, pragma))

proc trackInnerProc(tracked: PEffects, n: PNode) =
  addInNimDebugUtils(tracked.config, "trackInnerProc")
  case n.kind
  of nkSym:
    let s = n.sym
    if s.kind == skParam and s.owner == tracked.owner:
      tracked.escapingParams.incl s.id
  of nkWithoutSons - nkSym:
    discard
  of nkProcDef, nkConverterDef, nkMethodDef, nkIteratorDef, nkLambda, nkFuncDef, nkDo:
    if n[0].kind == nkSym and n[0].sym.ast != nil:
      trackInnerProc(tracked, getBody(tracked.graph, n[0].sym))
  of nkTypeSection, nkMacroDef, nkTemplateDef,
     nkConstSection, nkConstDef, nkIncludeStmt, nkImportStmt,
     nkExportStmt, nkPragma, nkTypeOfExpr, nkMixinStmt,
     nkBindStmt:
    discard
  else:
    for ch in n: trackInnerProc(tracked, ch)

proc allowCStringConv(n: PNode): bool =
  case n.kind
  of nkStrLiterals: result = true
  of nkSym: result = n.sym.kind in {skConst, skParam}
  of nkAddr: result = isCharArrayPtr(n.typ, true)
  of nkCallKinds:
    result = isCharArrayPtr(n.typ, n[0].kind == nkSym and n[0].sym.magic == mAddr)
  else: result = isCharArrayPtr(n.typ, false)

proc reportErrors(c: ConfigRef, n: PNode) =
  ## Reports all errors found in the AST `n`.
  case n.kind
  of nkError:
    localReport(c, n)
  of nkWithSons:
    for it in n.items:
      reportErrors(c, it)
  of nkWithoutSons - {nkError}:
    discard "ignore"

proc track(tracked: PEffects, n: PNode) =
  addInNimDebugUtils(tracked.config, "track")
  case n.kind
  of nkSym:
    useVar(tracked, n)
    if n.sym.typ != nil and tfHasAsgn in n.sym.typ.flags:
      tracked.owner.flags.incl sfInjectDestructors
      # bug #15038: ensure consistency
      if not hasDestructor(n.typ) and sameType(n.typ, n.sym.typ): n.typ = n.sym.typ
  of nkHiddenAddr, nkAddr:
    if n[0].kind == nkSym and isLocalVar(tracked, n[0].sym):
      useVarNoInitCheck(tracked, n[0], n[0].sym)
    else:
      track(tracked, n[0])
  of nkRaiseStmt:
    if n[0].kind != nkEmpty:
      n[0].info = n.info
      #throws(tracked.exc, n[0])
      addRaiseEffect(tracked, n[0], n)
      for i in 0..<n.safeLen:
        track(tracked, n[i])
      createTypeBoundOps(tracked, n[0].typ, n.info)
    elif tracked.isReraiseAllowed > 0:
      # A `raise` with no arguments means we're going to re-raise the exception
      # being handled
      # XXX: using an `Exception` tag is overly conservative. It is statically
      #      known which exceptions an except branch covers, but this
      #      information isn't available here, at the moment.
      addRaiseEffect(tracked, createRaise(tracked.graph, n), nil)
    else:
      localReport(tracked.config, n, reportSem(rsemCannotReraise))
  of nkCallKinds:
    trackCall(tracked, n)
  of nkDotExpr:
    guardDotAccess(tracked, n)
    for i in 0..<n.len: track(tracked, n[i])
  of nkCheckedFieldExpr:
    track(tracked, n[0])
    if tracked.config.hasWarn(rsemProveField):
      checkFieldAccess(tracked.guards, n, tracked.config)
  of nkTryStmt: trackTryStmt(tracked, n)
  of nkPragma: trackPragmaStmt(tracked, n)
  of nkAsgn, nkFastAsgn:
    track(tracked, n[1])
    initVar(tracked, n[0])
    inc tracked.leftPartOfAsgn
    track(tracked, n[0])
    dec tracked.leftPartOfAsgn
    addAsgnFact(tracked.guards, n[0], n[1])
    notNilCheck(tracked, n[1], n[0].typ)
    objConvCheck(tracked.config, n[1])
    discriminantAsgnCheck(tracked, n)
    if tracked.owner.kind != skMacro:
      createTypeBoundOps(tracked, n[0].typ, n.info)
    if n[0].kind != nkSym or not isLocalVar(tracked, n[0].sym):
      checkForSink(tracked.config, tracked.c.idgen, tracked.owner, n[1])
      if not tracked.hasDangerousAssign and n[0].kind != nkSym:
        tracked.hasDangerousAssign = true
  of nkVarSection, nkLetSection:
    for child in n:
      let last = lastSon(child)
      if child.kind == nkIdentDefs and sfCompileTime in child[0].sym.flags:
        # don't analyse the definition of ``.compileTime`` globals. They
        # don't "exist" in the context (i.e., run time) we're analysing
        # in
        continue

      if last.kind != nkEmpty: track(tracked, last)
      if tracked.owner.kind != skMacro:
        if child.kind == nkVarTuple:
          createTypeBoundOps(tracked, child[^1].typ, child.info)
          for i in 0..<child.len-2:
            createTypeBoundOps(tracked, child[i].typ, child.info)
        else:
          createTypeBoundOps(tracked, child[0].typ, child.info)
      if child.kind == nkIdentDefs and last.kind != nkEmpty:
        for i in 0..<child.len-2:
          initVar(tracked, child[i])
          addAsgnFact(tracked.guards, child[i], last)
          notNilCheck(tracked, last, child[i].typ)
          objConvCheck(tracked.config, last)
      elif child.kind == nkVarTuple and last.kind != nkEmpty:
        for i in 0..<child.len-1:
          if child[i].kind == nkEmpty or
            child[i].kind == nkSym and child[i].sym.name.s == "_":
            continue
          initVar(tracked, child[i])
          if last.kind in {nkPar, nkTupleConstr}:
            addAsgnFact(tracked.guards, child[i], last[i])
            notNilCheck(tracked, last[i], child[i].typ)
            objConvCheck(tracked.config, last[i])
      # since 'var (a, b): T = ()' is not even allowed, there is always type
      # inference for (a, b) and thus no nil checking is necessary.
  of nkConstSection:
    for child in n:
      let last = lastSon(child)
      track(tracked, last)
  of nkCaseStmt: trackCase(tracked, n)
  of nkWhen, nkIfStmt, nkIfExpr: trackIf(tracked, n)
  of nkBlockStmt, nkBlockExpr: trackBlock(tracked, n[1])
  of nkWhileStmt:
    # 'while true' loop?
    if isTrue(n[0]):
      trackBlock(tracked, n[1])
    else:
      # loop may never execute:
      let oldState = tracked.init.len
      let oldFacts = tracked.guards.s.len
      addFact(tracked.guards, n[0])
      track(tracked, n[0])
      track(tracked, n[1])
      setLen(tracked.init, oldState)
      setLen(tracked.guards.s, oldFacts)
  of nkForStmt:
    # we are very conservative here and assume the loop is never executed:
    let
      oldState = tracked.init.len
      oldFacts = tracked.guards.s.len
      iterCall = n[n.len-2]
    
    if optStaticBoundsCheck in tracked.currOptions and iterCall.kind in nkCallKinds:
      let op = iterCall[0]
      if op.kind == nkSym and fromSystem(op.sym):
        let iterVar = n[0]
        case op.sym.name.s
        of "..", "countup", "countdown":
          let lower = iterCall[1]
          let upper = iterCall[2]
          # for i in 0..n   means  0 <= i and i <= n. Countdown is
          # the same since only the iteration direction changes.
          addFactLe(tracked.guards, lower, iterVar)
          addFactLe(tracked.guards, iterVar, upper)
        of "..<":
          let lower = iterCall[1]
          let upper = iterCall[2]
          addFactLe(tracked.guards, lower, iterVar)
          addFactLt(tracked.guards, iterVar, upper)
        else: discard

    for i in 0..<n.len-2:
      let it = n[i]
      track(tracked, it)
      if tracked.owner.kind != skMacro:
        if it.kind == nkVarTuple:
          for x in it:
            createTypeBoundOps(tracked, x.typ, x.info)
        else:
          createTypeBoundOps(tracked, it.typ, it.info)

    let loopBody = n[^1]
    if tracked.owner.kind != skMacro and
       iterCall.kind != nkError and iterCall.safeLen > 1:
      # XXX this is a bit hacky:
      if iterCall[1].typ != nil and
         iterCall[1].typ.skipTypes(abstractVar).kind notin {tyVarargs, tyOpenArray}:
        createTypeBoundOps(tracked, iterCall[1].typ, iterCall[1].info)

    if tracked.owner.kind != skMacro and iterCall.kind in nkCallKinds and
       iterCall[0].typ != nil and # XXX: untyped AST can reach here due to
                                  # semTypeNode discarding the typed AST
       iterCall[0].typ.skipTypes(abstractInst).callConv == ccClosure:
      # the loop is a for-loop over a closure iterator. Lift the hooks for
      # the iterator
      createTypeBoundOps(tracked, iterCall[0].typ, iterCall[0].info)

    track(tracked, iterCall)
    track(tracked, loopBody)
    setLen(tracked.init, oldState)
    setLen(tracked.guards.s, oldFacts)
  of nkObjConstr:
    when false: track(tracked, n[0])
    let oldFacts = tracked.guards.s.len
    for i in 1..<n.len:
      let x = n[i]
      track(tracked, x)
      if x[0].kind == nkSym and sfDiscriminant in x[0].sym.flags:
        addDiscriminantFact(tracked.guards, x)
      if tracked.owner.kind != skMacro:
        createTypeBoundOps(tracked, x[1].typ, n.info)

      if x.kind == nkExprColonExpr:
        if x[0].kind == nkSym:
          notNilCheck(tracked, x[1], x[0].sym.typ)
          objConvCheck(tracked.config, x[1])
        checkForSink(tracked.config, tracked.c.idgen, tracked.owner, x[1])
      else:
        checkForSink(tracked.config, tracked.c.idgen, tracked.owner, x)
    setLen(tracked.guards.s, oldFacts)
    if tracked.owner.kind != skMacro:
      # XXX n.typ can be nil in runnableExamples, we need to do something about it.
      if n.typ != nil and n.typ.skipTypes(abstractInst).kind == tyRef:
        createTypeBoundOps(tracked, n.typ.lastSon, n.info)
      createTypeBoundOps(tracked, n.typ, n.info)
  of nkTupleConstr:
    for i in 0..<n.len:
      track(tracked, n[i])
      notNilCheck(tracked, n[i].skipColon, n[i].typ)
      objConvCheck(tracked.config, n[i].skipColon)
      if tracked.owner.kind != skMacro:
        if n[i].kind == nkExprColonExpr:
          createTypeBoundOps(tracked, n[i][0].typ, n.info)
        else:
          createTypeBoundOps(tracked, n[i].typ, n.info)
      checkForSink(tracked.config, tracked.c.idgen, tracked.owner, n[i])
  of nkPragmaBlock:
    let pragmaList = n[0]
    var bc = createBlockContext(tracked)
    for i in 0..<pragmaList.len:
      let pragma = whichPragma(pragmaList[i])
      case pragma
      of wLocks:
        lockLocations(tracked, pragmaList[i])
      of wGcSafe:
        bc.enforcedGcSafety = true
      of wNoSideEffect:
        bc.enforceNoSideEffects = true
      of wCast:
        castBlock(tracked, pragmaList[i][1], bc)
      else:
        discard
    applyBlockContext(tracked, bc)
    track(tracked, n.lastSon)
    unapplyBlockContext(tracked, bc)

  of nkProcDef, nkConverterDef, nkMethodDef, nkIteratorDef, nkLambda, nkFuncDef, nkDo:
    if n[0].kind == nkSym and n[0].sym.ast != nil:
      trackInnerProc(tracked, getBody(tracked.graph, n[0].sym))
  of nkTypeSection, nkMacroDef, nkTemplateDef:
    discard
  of nkCast:
    if n.len == 2:
      track(tracked, n[1])
      if tracked.owner.kind != skMacro:
        createTypeBoundOps(tracked, n.typ, n.info)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    let t = n.typ.skipTypes(abstractInst)
    if n.kind in {nkHiddenStdConv, nkHiddenSubConv} and
        t.kind == tyCstring and
        not allowCStringConv(n[1]):
      localReport(tracked.config, n.info, reportAst(
        rsemImplicitCstringConvert, n[1]))

    if t.kind == tyEnum:
      if tfEnumHasHoles in t.flags:
        localReport(tracked.config, n, reportSem rsemHoleEnumConvert)
      else:
        localReport(tracked.config, n, reportSem rsemAnyEnumConvert)

    if n.len == 2:
      track(tracked, n[1])
      if tracked.owner.kind != skMacro:
        createTypeBoundOps(tracked, n.typ, n.info)
        # This is a hacky solution in order to fix bug #13110. Hopefully
        # a better solution will come up eventually.
        if n[1].typ.kind != tyString:
          createTypeBoundOps(tracked, n[1].typ, n[1].info)
      if optStaticBoundsCheck in tracked.currOptions:
        checkRange(tracked, n[1], n.typ)
  of nkObjUpConv, nkObjDownConv, nkChckRange, nkChckRangeF, nkChckRange64:
    if n.len == 1:
      track(tracked, n[0])
      if tracked.owner.kind != skMacro:
        createTypeBoundOps(tracked, n.typ, n.info)
        createTypeBoundOps(tracked, n[0].typ, n[0].info)
      if optStaticBoundsCheck in tracked.currOptions:
        checkRange(tracked, n[0], n.typ)
  of nkBracket:
    for i in 0..<n.safeLen:
      track(tracked, n[i])
      objConvCheck(tracked.config, n[i])
      checkForSink(tracked.config, tracked.c.idgen, tracked.owner, n[i])
    if tracked.owner.kind != skMacro:
      createTypeBoundOps(tracked, n.typ, n.info)
  of nkBracketExpr:
    if optStaticBoundsCheck in tracked.currOptions and n.len == 2:
      if n[0].typ != nil and skipTypes(n[0].typ, abstractVar).kind != tyTuple:
        checkBounds(tracked, n[0], n[1])
    track(tracked, n[0])
    dec tracked.leftPartOfAsgn
    for i in 1 ..< n.len:
      track(tracked, n[i])

    inc tracked.leftPartOfAsgn
  of nkBindStmt, nkMixinStmt, nkImportStmt, nkImportExceptStmt, nkExportStmt,
     nkExportExceptStmt, nkFromStmt:
    # a declarative statement that is not relevant to the analysis. Report
    # errors part of the AST, but otherwise ignore
    reportErrors(tracked.config, n)
  of nkError:
    localReport(tracked.config, n)
  else:
    for i in 0 ..< n.safeLen:
      track(tracked, n[i])

proc subtypeRelation(g: ModuleGraph; spec, real: PNode): bool =
  if spec.typ.kind == tyOr:
    for t in spec.typ.sons:
      if safeInheritanceDiff(t, g.excType(real)) <= 0:
        return true
  else:
    return safeInheritanceDiff(spec.typ, g.excType(real)) <= 0

proc checkRaisesSpec(
    g: ModuleGraph,
    onFail: ReportKind,
    spec, real: PNode,
    hints: bool,
    effectPredicate: proc (g: ModuleGraph; a, b: PNode): bool {.nimcall.},
    hintsArg: PNode = nil
  ) =
  # check that any real exception is listed in 'spec'; mark those as used;
  # report any unused exception
  var used = initIntSet()
  for r in items(real):
    block search:
      for s in 0..<spec.len:
        if effectPredicate(g, spec[s], r):
          used.incl(s)
          break search
      # XXX call graph analysis would be nice here!
      pushInfoContext(g.config, spec.info)
      var rr = if r.kind == nkRaiseStmt: r[0] else: r
      while rr.kind in {nkStmtList, nkStmtListExpr} and rr.len > 0:
        rr = rr.lastSon

      localReport(g.config, r.info, reportAst(
        onFail, rr, typ = r.typ))

      popInfoContext(g.config)
  # hint about unnecessarily listed exception types:
  if hints:
    for s in 0..<spec.len:
      if not used.contains(s):
        localReport(g.config, spec[s].info, SemReport(
          kind: rsemXCannotRaiseY, ast: hintsArg, raisesList: spec[s]))

proc checkMethodEffects*(g: ModuleGraph; disp, branch: PSym) =
  ## checks for consistent effects for multi methods.
  let actual = branch.typ.n[0]
  if actual.len != effectListLen: return

  let p = disp.ast[pragmasPos]
  let raisesSpec = effectSpec(p, wRaises)
  if not isNil(raisesSpec):
    checkRaisesSpec(
      g,
      rsemUnlistedRaises,
      raisesSpec,
      actual[exceptionEffects],
      hints = off,
      subtypeRelation)

  let tagsSpec = effectSpec(p, wTags)
  if not isNil(tagsSpec):
    checkRaisesSpec(
      g,
      rsemUnlistedEffects,
      tagsSpec,
      actual[tagEffects],
      hints = off,
      subtypeRelation)

  if sfThread in disp.flags and notGcSafe(branch.typ):
    localReport(g.config, branch.info, reportSymbols(
      rsemOverrideSafetyMismatch, @[disp, branch]))

  if branch.typ.lockLevel > disp.typ.lockLevel:
    when true:
      localReport(g.config, branch.info, reportSymbols(
        rsemOverrideLockMismatch, @[disp, branch]))

    else:
      # XXX make this an error after bigbreak has been released:
      localReport(g.config, branch.info,
        "base method has lock level $1, but dispatcher has $2" %
          [$branch.typ.lockLevel, $disp.typ.lockLevel])

proc setEffectsForProcType*(g: ModuleGraph; t: PType, n: PNode; s: PSym = nil) =
  var effects = t.n[0]
  if t.kind != tyProc or effects.kind != nkEffectList: return
  if n.kind != nkEmpty:
    internalAssert(g.config, effects.len == 0, "Starting effects list must be empty")

    newSeq(effects.sons, effectListLen)
    let raisesSpec = effectSpec(n, wRaises)
    if not isNil(raisesSpec):
      effects[exceptionEffects] = raisesSpec
    elif s != nil and (s.magic != mNone or {sfImportc, sfExportc} * s.flags == {sfImportc}):
      effects[exceptionEffects] = newNodeI(nkArgList, effects.info)

    let tagsSpec = effectSpec(n, wTags)
    if not isNil(tagsSpec):
      effects[tagEffects] = tagsSpec
    elif s != nil and (s.magic != mNone or {sfImportc, sfExportc} * s.flags == {sfImportc}):
      effects[tagEffects] = newNodeI(nkArgList, effects.info)

    effects[pragmasEffects] = n
  if s != nil and s.magic != mNone:
    if s.magic != mEcho:
      t.flags.incl tfNoSideEffect

proc rawInitEffects(g: ModuleGraph; effects: PNode) =
  newSeq(effects.sons, effectListLen)
  effects[exceptionEffects] = newNodeI(nkArgList, effects.info)
  effects[tagEffects] = newNodeI(nkArgList, effects.info)
  effects[pragmasEffects] = g.emptyNode

proc initEffects(g: ModuleGraph; effects: PNode; s: PSym; t: var TEffects; c: PContext) =
  rawInitEffects(g, effects)

  t.exc = effects[exceptionEffects]
  t.tags = effects[tagEffects]
  t.owner = s
  t.ownerModule = s.getModule
  t.init = @[]
  t.guards.s = @[]
  t.guards.g = g
  t.currOptions = g.config.options + s.options
  t.guards.beSmart = optStaticBoundsCheck in t.currOptions
  t.locked = @[]
  t.graph = g
  t.config = g.config
  t.c = c

proc hasRealBody(s: PSym): bool =
  ## also handles importc procs with runnableExamples, which requires `=`,
  ## which is not a real implementation, refs #14314
  result = {sfForward, sfImportc} * s.flags == {}

# ------------- routines for capture analysis ------------

proc detectCapture(owner, top: PSym, n: PNode, marker: var IntSet): PNode =
  ## Traverses the imperative code in `n` and searches for entities (such as
  ## locals) that need to be directly or indirectly captured. This is detected
  ## by checking whether an entity is defined (i.e. owned) by a procedure that
  ## is itself the owner of `top`. `owner` is the procedure the currently
  ## processed AST belongs to. The node of the first encountered entity that
  ## needs to be captured is returned.
  ##
  ## In the following case, ``inner`` indirectly captures something:
  ##
  ## .. code-block:: nim
  ##
  ##   proc outer() =
  ##     var x = 0
  ##     proc inner() =
  ##       proc innerInner() =
  ##         x = 1
  ##       innerInner()
  ##     inner()
  ##
  ## On encountering the *usage* of a procedure that uses the ``.closure``
  ## calling convention (e.g. indirect call or assignment), ``detectCapture``
  ## recurses into the body of said procedure and checks if any captured entity
  ## is defined outside of `top`. If that's the case, it means that something
  ## that `top` captures was found.
  template detect(s: PSym): PNode =
    ## Checks if `s` is something that would need to be directly or indirectly
    ## captured by `top`, and evaluates to `n` if yes
    if top.isOwnedBy(s.owner): n
    else: nil

  case n.kind
  of nkTypeSection, nkTypeOfExpr, nkCommentStmt, nkIncludeStmt, nkImportStmt,
     nkImportExceptStmt, nkExportStmt, nkExportExceptStmt, nkFromStmt,
     nkStaticStmt, nkMixinStmt, nkBindStmt:
    # XXX: this set of node kinds is commonly used. It would make sense to make
    #      a `const` out of it
    discard "ignore declarative contexts"
  of routineDefs:
    discard "also ignore routine defs; we're only looking for alive code"
  of nkSym:
    let s = n.sym
    case s.kind
    of skLocalVars:
      # make sure to not check globals, as those don't need to be captured
      if sfGlobal notin s.flags and s.owner.id != owner.id:
        # the local doesn't belong to the procedure we're analysing
        result = detect(s)
    of skProc, skFunc, skIterator:
      # NOTE: a routine using the closure calling convention means that it
      # *may* captures something (it might not). A routine not using the
      # closure calling convention means that it *can't* capture anything,
      # so we don't need to analyse the latter
      if s.typ != nil and s.typ.callConv == ccClosure and
         s.owner.kind != skModule: # don't analyse top-level closure iterators
        if s.owner.id == owner.id:
          # a procedure that's defined directly inside the currently analysed
          # procedure is used as a value. Recurse into it to see if it captures
          # an entity outside of `top`
          result = detectCapture(s, top, s.ast[bodyPos], marker)
        else:
          # procedure A that uses the closure calling convention is used in
          # procedure B, but A is not an inner procedure of B. Because of a
          # limitation of the lambda-lifting implementation, B needs to be
          # treated as capturing something
          # XXX: fixing this requires two things: 1) tracking which routine
          #      really captures something, and 2) fixing ``lambdalifting``
          result = detect(s)
    else:
      discard "not relevant"
  of nkWithoutSons - {nkSym, nkCommentStmt}:
    discard "not relevant"
  of nkConv, nkHiddenStdConv, nkHiddenSubConv:
    # only analyse the imperative part:
    result = detectCapture(owner, top, n[1], marker)
  of nkLambdaKinds:
    result = detectCapture(owner, top, n[namePos], marker)
  of nkNimNodeLit:
    discard "ignore node literals as they're data not code"
  else:
    # TODO: make exhaustive
    for it in n.items:
      result = detectCapture(owner, top, it, marker)
      if result != nil:
        # we've found something that requires capturing, don't continue and
        # unwind
        return

proc canCaptureFrom*(captor, target: PSym): bool =
  ## Tests if the `captor` routine is allowed to capture a local from `target`,
  ## taking the compile-time/run-time boundary into account:
  ## 1) attempting to capture a local defined outside an inner macro from
  ##   inside the macro is illegal
  ## 2) closing over a local defined inside a compile-time-only routine from a
  ##   routine than can also be used at run-time is only valid if the chain of
  ##   enclosing routines leading up to `target` are all compile-time-only
  ## 3) a compile-time-only routine closing over a run-time location is illegal
  template isCompTimeOnly(s: PSym): bool =
    sfCompileTime in s.flags

  result = not captor.isCompTimeOnly or target.isCompTimeOnly    # rule #3
  # check `captor` and all enclosing routines up to, but not including,
  # `target` for rule violations
  var s = captor
  while result and s != target:
    result =
      s.kind != skMacro and                                      # rule #1
      (s == captor or s.isCompTimeOnly == target.isCompTimeOnly) # rule #2
    s = s.skipGenericOwner

# ------------- public interface ----------------

proc trackProc*(c: PContext; s: PSym, body: PNode) =
  addInNimDebugUtils(c.config, "trackProc")
  if body.kind == nkError:
    # the body has an error, don't attempt to analyse it further
    return

  let g = c.graph
  var effects = s.typ.n[0]
  if effects.kind != nkEffectList: return
  # effects already computed?
  if not s.hasRealBody: return
  let emitWarnings = tfEffectSystemWorkaround in s.typ.flags
  if effects.len == effectListLen and not emitWarnings: return

  var inferredEffects = newNodeI(nkEffectList, s.info)

  var t: TEffects
  initEffects(g, inferredEffects, s, t, c)
  rawInitEffects g, effects
  track(t, body)

  if s.kind != skMacro:
    let params = s.typ.n
    for i in 1..<params.len:
      let param = params[i].sym
      let typ = param.typ
      if isSinkTypeForParam(typ) or
          (t.config.selectedGC in {gcArc, gcOrc} and
            (isClosure(typ.skipTypes(abstractInst)) or param.id in t.escapingParams)):
        createTypeBoundOps(t, typ, param.info)
      when false:
        if typ.kind == tyOut and param.id notin t.init:
          message(g.config, param.info, warnProveInit, param.name.s)

  let cap = block:
    var marker: IntSet
    detectCapture(s, s, body, marker)

  if cap != nil:
    # the procedure captures something and thus requires a hidden environment
    # parameter
    if not canCaptureFrom(s, cap.sym.owner):
      # attempting to capture an entity that only exists at run-time in a
      # compile-time context
      localReport(g.config, cap.info, reportSym(
        rsemIllegalCompTimeCapture, cap.sym))
    elif s.typ.callConv != ccClosure and tfExplicitCallConv in s.typ.flags:
      # the analysed procedure is explicitly *not* using the ``.closure``
      # calling convention
      localReport(g.config, cap.info, reportSymbols(
        rsemIllegalCallconvCapture, @[cap.sym, s]))
    else:
      # set the calling convention and mark it as really capturing something:
      s.typ.callConv = ccClosure

  elif s.kind != skIterator and s.typ.callConv == ccClosure:
    # nothing is captured (so no hidden environment parameter is needed), but
    # the procedure was explicitly annotated to use the ``.closure`` calling
    # convention
    assert tfExplicitCallConv in s.typ.flags
    localReport(g.config, s.info, reportSym(rsemClosureWithoutEnv, s))

  if not isEmptyType(s.typ[0]) and
     (s.typ[0].requiresInit or s.typ[0].skipTypes(abstractInst).kind == tyVar) and
     s.kind in {skProc, skFunc, skConverter, skMethod}:
    var res = s.ast[resultPos].sym # get result symbol
    if res.id notin t.init:
      localReport(g.config, body.info, reportSym(rsemProveInit, res))
  let p = s.ast[pragmasPos]
  let raisesSpec = effectSpec(p, wRaises)
  if not isNil(raisesSpec):
    checkRaisesSpec(g, rsemUnlistedRaises, raisesSpec, t.exc,
                    hints=on, subtypeRelation, hintsArg=s.ast[0])
    # after the check, use the formal spec:
    effects[exceptionEffects] = raisesSpec
  else:
    effects[exceptionEffects] = t.exc

  let tagsSpec = effectSpec(p, wTags)
  if not isNil(tagsSpec):
    checkRaisesSpec(g, rsemUnlistedEffects, tagsSpec, t.tags,
                    hints=off, subtypeRelation)
    # after the check, use the formal spec:
    effects[tagEffects] = tagsSpec
  else:
    effects[tagEffects] = t.tags

  # ensure that user-provided hooks have no effects and don't raise
  if sfOverriden in s.flags:
    # if raising was explicitly disabled (i.e., via ``.raises: []``),
    # exceptions, if any, were already reported; don't report errors again in
    # that case
    if raisesSpec.isNil or raisesSpec.len > 0:
      let newSpec = newNodeI(nkArgList, s.info)
      checkRaisesSpec(g, rsemHookCannotRaise, newSpec,
                      t.exc, hints=off, nil)
      # override the raises specification to prevent cascading errors:
      effects[exceptionEffects] = newSpec

    # enforce that no defects escape the routine at run-time:
    s.flags.incl sfNeverRaises

  var mutationInfo = MutationInfo()
  var hasMutationSideEffect = false
  if {strictFuncs, views} * c.features != {}:
    var goals: set[Goal] = {}
    if strictFuncs in c.features: goals.incl constParameters
    if views in c.features: goals.incl borrowChecking
    var partitions = computeGraphPartitions(s, body, g, goals)
    if not t.hasSideEffect and t.hasDangerousAssign:
      t.hasSideEffect = varpartitions.hasSideEffect(partitions, mutationInfo)
      hasMutationSideEffect = t.hasSideEffect
    if views in c.features:
      checkBorrowedLocations(partitions, body, g.config)

  if s.kind != skMacro and sfThread in s.flags and t.gcUnsafe:
    if optThreads in g.config.globalOptions and optThreadAnalysis in g.config.globalOptions:
      #localReport(s.info, "'$1' is not GC-safe" % s.name.s)
      listGcUnsafety(s, onlyWarning=false, g.config)
    else:
      listGcUnsafety(s, onlyWarning=true, g.config)
      #localReport(s.info, warnGcUnsafe2, s.name.s)
  if sfNoSideEffect in s.flags and t.hasSideEffect:
    when false:
      listGcUnsafety(s, onlyWarning=false, g.config)
    else:
      if hasMutationSideEffect:
        var report = reportSym(rsemHasSideEffects, s)

        report.sideEffectTrace.add((
          isUnsafe: s,
          unsafeVia: mutationInfo.param,
          trace: ssefParameterMutation,
          location: mutationInfo.mutatedHere,
          level: 0
        ))

        report.sideEffectMutateConnection = mutationInfo.connectedVia

        localReport(g.config, s.info, report)
      elif c.compilesContextId == 0:
        # don't render extended diagnostic messages in `system.compiles` context
        var report = reportSem(rsemHasSideEffects)
        listSideEffects(report, s, g.config, t.c)
        localReport(g.config, s.info, report)
      else:
        # simple error for `system.compiles` context
        localReport(g.config, s.info, reportSem rsemCompilesHasSideEffects)

  if not t.gcUnsafe:
    s.typ.flags.incl tfGcSafe
  if not t.hasSideEffect and sfSideEffect notin s.flags:
    s.typ.flags.incl tfNoSideEffect
  if s.typ.lockLevel == UnspecifiedLockLevel:
    s.typ.lockLevel = t.maxLockLevel
  elif t.maxLockLevel > s.typ.lockLevel:
    localReport(g.config, s.info, SemReport(
      kind: rsemLockLevelMismatch,
      lockMismatch: ($s.typ.lockLevel, $t.maxLockLevel)))

  when defined(useDfa):
    if s.name.s == "testp":
      dataflowAnalysis(s, body)

      when false: trackWrites(s, body)
  if strictNotNil in c.features and s.kind == skProc:
    # HACK I don't know why there are two different configurations anyway,
    # but without mixing in `c.features` `checkNil` cannot know if nil
    # reports are enabled or not.
    let oldFeatures = g.config.features
    g.config.features = c.features + g.config.features

    checkNil(s, body, g.config, c.idgen)

    g.config.features = oldFeatures

proc trackStmt*(c: PContext; module: PSym; n: PNode, isTopLevel: bool) =
  if n.kind in {nkPragma, nkMacroDef, nkTemplateDef, nkProcDef, nkFuncDef,
                nkTypeSection, nkConverterDef, nkMethodDef, nkIteratorDef}:
    return
  let g = c.graph
  var effects = newNodeI(nkEffectList, n.info)
  var t: TEffects
  initEffects(g, effects, module, t, c)
  t.isTopLevel = isTopLevel
  track(t, n)
