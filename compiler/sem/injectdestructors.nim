#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Injects destructor calls into Nim code as well as
## an optimizer that optimizes copies to moves. This is implemented as an
## AST to AST transformation so that every backend benefits from it.

## See doc/destructors.rst for a spec of the implemented rewrite rules

import
  std/[
    intsets,
    strtabs,
    strutils,
    tables
  ],
  compiler/ast/[
    ast,
    astalgo,
    renderer,
    types,
    typesrenderer,
    idents,
    lineinfos,
    reports
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    dfa,
    lowerings,
    parampatterns,
    sighashes,
    liftdestructors,
    optimizer,
    varpartitions
  ]

from compiler/ast/trees import exprStructuralEquivalent, getRoot

type
  Con = object
    owner: PSym
    g: ControlFlowGraph
    graph: ModuleGraph
    inLoop, inLoopCond: int
    uninit: IntSet # set of uninit'ed vars
    uninitComputed: bool
    idgen: IdGenerator

  Scope = object # we do scope-based memory management.
    # a scope is comparable to an nkStmtListExpr like
    # (try: statements; dest = y(); finally: destructors(); dest)
    vars: seq[PSym]
    wasMoved: seq[PNode]
    final: seq[PNode] # finally section
    needsTry: bool
    parent: ptr Scope

  ProcessMode = enum
    normal
    consumed
    sinkArg

const toDebug {.strdefine.} = ""
when toDebug.len > 0:
  var shouldDebug = false

template dbg(body) =
  when toDebug.len > 0:
    if shouldDebug:
      body

proc hasDestructor(c: Con; t: PType): bool {.inline.} =
  result = ast.hasDestructor(t)
  when toDebug.len > 0:
    # for more effective debugging
    if not result and c.graph.config.selectedGC in {gcArc, gcOrc}:
      assert(not containsGarbageCollectedRef(t))

proc getTemp(c: var Con; s: var Scope; typ: PType; info: TLineInfo): PNode =
  let sym = newSym(skTemp, getIdent(c.graph.cache, ":tmpD"), nextSymId c.idgen, c.owner, info)
  sym.typ = typ
  s.vars.add(sym)
  result = newSymNode(sym)

proc nestedScope(parent: var Scope): Scope =
  Scope(vars: @[], wasMoved: @[], final: @[], needsTry: false, parent: addr(parent))

proc p(n: PNode; c: var Con; s: var Scope; mode: ProcessMode): PNode
proc moveOrCopy(dest, ri: PNode; c: var Con; s: var Scope; isDecl = false): PNode

import sets, hashes

proc hash(n: PNode): Hash = hash(cast[pointer](n))

proc aliasesCached(cache: var Table[(PNode, PNode), AliasKind], obj, field: PNode): AliasKind =
  let key = (obj, field)
  if not cache.hasKey(key):
    cache[key] = aliases(obj, field)
  cache[key]

type
  State = ref object
    lastReads: IntSet
    potentialLastReads: IntSet
    notLastReads: IntSet
    alreadySeen: HashSet[PNode]

proc preprocessCfg(cfg: var ControlFlowGraph) =
  for i in 0..<cfg.len:
    if cfg[i].kind in {goto, fork} and i + cfg[i].dest > cfg.len:
      cfg[i].dest = cfg.len - i

proc mergeStates(a: var State, b: sink State) =
  # Inplace for performance:
  #   lastReads = a.lastReads + b.lastReads
  #   potentialLastReads = (a.potentialLastReads + b.potentialLastReads) - (a.notLastReads + b.notLastReads)
  #   notLastReads = a.notLastReads + b.notLastReads
  #   alreadySeen = a.alreadySeen + b.alreadySeen
  # b is never nil
  if a == nil:
    a = b
  else:
    a.lastReads.incl b.lastReads
    a.potentialLastReads.incl b.potentialLastReads
    a.potentialLastReads.excl a.notLastReads
    a.potentialLastReads.excl b.notLastReads
    a.notLastReads.incl b.notLastReads
    a.alreadySeen.incl b.alreadySeen

proc computeLastReadsAndFirstWrites(cfg: ControlFlowGraph) =
  var cache = initTable[(PNode, PNode), AliasKind]()
  template aliasesCached(obj, field: PNode): AliasKind =
    aliasesCached(cache, obj, field)

  var cfg = cfg
  preprocessCfg(cfg)

  var states = newSeq[State](cfg.len + 1)
  states[0] = State()

  for pc in 0..<cfg.len:
    template state: State = states[pc]
    if state != nil:
      case cfg[pc].kind
      of def:
        var potentialLastReadsCopy = state.potentialLastReads
        for r in potentialLastReadsCopy:
          if cfg[pc].n.aliasesCached(cfg[r].n) == yes:
            # the path leads to a redefinition of 's' --> sink 's'.
            state.lastReads.incl r
            state.potentialLastReads.excl r
          elif cfg[r].n.aliasesCached(cfg[pc].n) != no:
            # only partially writes to 's' --> can't sink 's', so this def reads 's'
            # or maybe writes to 's' --> can't sink 's'
            cfg[r].n.comment = '\n' & $pc
            state.potentialLastReads.excl r
            state.notLastReads.incl r

        var alreadySeenThisNode = false
        for s in state.alreadySeen:
          if cfg[pc].n.aliasesCached(s) != no or s.aliasesCached(cfg[pc].n) != no:
            alreadySeenThisNode = true; break
        if alreadySeenThisNode: cfg[pc].n.flags.excl nfFirstWrite
        else: cfg[pc].n.flags.incl nfFirstWrite

        state.alreadySeen.incl cfg[pc].n

        mergeStates(states[pc + 1], move(states[pc]))
      of use:
        var potentialLastReadsCopy = state.potentialLastReads
        for r in potentialLastReadsCopy:
          if cfg[pc].n.aliasesCached(cfg[r].n) != no or cfg[r].n.aliasesCached(cfg[pc].n) != no:
            cfg[r].n.comment = '\n' & $pc
            state.potentialLastReads.excl r
            state.notLastReads.incl r

        state.potentialLastReads.incl pc

        state.alreadySeen.incl cfg[pc].n

        mergeStates(states[pc + 1], move(states[pc]))
      of goto:
        mergeStates(states[pc + cfg[pc].dest], move(states[pc]))
      of fork:
        var copy = State()
        copy[] = states[pc][]
        mergeStates(states[pc + cfg[pc].dest], copy)
        mergeStates(states[pc + 1], move(states[pc]))

  let lastReads = (states[^1].lastReads + states[^1].potentialLastReads) - states[^1].notLastReads
  var lastReadTable: Table[PNode, seq[int]]
  for position, node in cfg:
    if node.kind == use:
      lastReadTable.mgetOrPut(node.n, @[]).add position
  for node, positions in lastReadTable:
    block checkIfAllPosLastRead:
      for p in positions:
        if p notin lastReads: break checkIfAllPosLastRead
      node.flags.incl nfLastRead

proc isLastRead(n: PNode; c: var Con): bool =
  let m = dfa.skipConvDfa(n)
  (m.kind == nkSym and sfSingleUsedTemp in m.sym.flags) or nfLastRead in m.flags

proc isFirstWrite(n: PNode; c: var Con): bool =
  let m = dfa.skipConvDfa(n)
  nfFirstWrite in m.flags

proc initialized(code: ControlFlowGraph; pc: int,
                 init, uninit: var IntSet; until: int): int =
  ## Computes the set of definitely initialized variables across all code paths
  ## as an IntSet of IDs.
  var pc = pc
  while pc < code.len:
    case code[pc].kind
    of goto:
      pc += code[pc].dest
    of fork:
      var initA = initIntSet()
      var initB = initIntSet()
      var variantA = pc + 1
      var variantB = pc + code[pc].dest
      while variantA != variantB:
        if max(variantA, variantB) > until:
          break
        if variantA < variantB:
          variantA = initialized(code, variantA, initA, uninit, min(variantB, until))
        else:
          variantB = initialized(code, variantB, initB, uninit, min(variantA, until))
      pc = min(variantA, variantB)
      # we add vars if they are in both branches:
      for v in initA:
        if v in initB:
          init.incl v
    of use:
      let v = code[pc].n.sym
      if v.kind != skParam and v.id notin init:
        # attempt to read an uninit'ed variable
        uninit.incl v.id
      inc pc
    of def:
      let v = code[pc].n.sym
      init.incl v.id
      inc pc
  return pc

proc isCursor(n: PNode): bool =
  case n.kind
  of nkSym:
    sfCursor in n.sym.flags
  of nkDotExpr:
    isCursor(n[1])
  of nkCheckedFieldExpr:
    isCursor(n[0])
  else:
    false

template isUnpackedTuple(n: PNode): bool =
  ## we move out all elements of unpacked tuples,
  ## hence unpacked tuples themselves don't need to be destroyed
  (n.kind == nkSym and n.sym.kind == skTemp and n.sym.typ.kind == tyTuple)

proc checkForErrorPragma(c: Con; t: PType; ri: PNode; opname: string) =
  var rep = SemReport(
    kind: rsemUnavailableTypeBound,
    typ: t,
    str: opname,
    ast: ri,
    sym: c.owner
  )

  if (opname == "=" or opname == "=copy") and ri != nil:
    if ri.comment.startsWith('\n'):
      rep.missingTypeBoundElaboration.anotherRead = some(
        c.g[parseInt(ri.comment[1..^1])].n.info )

    elif ri.kind == nkSym and
         ri.sym.kind == skParam and
         not isSinkType(ri.sym.typ):
      rep.missingTypeBoundElaboration.tryMakeSinkParam = true

  localReport(c.graph.config, ri.info, rep)

proc makePtrType(c: var Con, baseType: PType): PType =
  result = newType(tyPtr, nextTypeId c.idgen, c.owner)
  addSonSkipIntLit(result, baseType, c.idgen)

proc genOp(c: var Con; op: PSym; dest: PNode): PNode =
  let addrExp = newTreeIT(nkHiddenAddr, dest.info, makePtrType(c, dest.typ)): dest
  result = newTree(nkCall, newSymNode(op), addrExp)

proc genOp(c: var Con; t: PType; kind: TTypeAttachedOp; dest, ri: PNode): PNode =
  var op = getAttachedOp(c.graph, t, kind)
  if op == nil or op.ast.isGenericRoutine:
    # give up and find the canonical type instead:
    let h = sighashes.hashType(t, {CoType, CoConsiderOwned, CoDistinct})
    let canon = c.graph.canonTypes.getOrDefault(h)
    if canon != nil:
      op = getAttachedOp(c.graph, canon, kind)

  c.graph.config.internalAssert(op != nil, dest.info):
    "internal error: '" & AttachedOpToStr[kind] &
    "' operator not found for type " & typeToString(t)

  c.graph.config.internalAssert(not op.ast.isGenericRoutine, dest.info):
    "internal error: '" & AttachedOpToStr[kind] & "' operator is generic"

  dbg:
    if kind == attachedDestructor:
      echo "destructor is ", op.id, " ", op.ast
  if sfError in op.flags:
    checkForErrorPragma(c, t, ri, AttachedOpToStr[kind])
  c.genOp(op, dest)

proc genDestroy(c: var Con; dest: PNode): PNode =
  let t = dest.typ.skipTypes({tyGenericInst, tyAlias, tySink})
  result = c.genOp(t, attachedDestructor, dest, nil)

proc canBeMoved(c: Con; t: PType): bool {.inline.} =
  let t = t.skipTypes({tyGenericInst, tyAlias, tySink})
  if optOwnedRefs in c.graph.config.globalOptions:
    result = t.kind != tyRef and getAttachedOp(c.graph, t, attachedSink) != nil
  else:
    result = getAttachedOp(c.graph, t, attachedSink) != nil

proc isNoInit(dest: PNode): bool {.inline.} =
  result = dest.kind == nkSym and sfNoInit in dest.sym.flags

proc genSink(c: var Con; dest, ri: PNode, isDecl = false): PNode =
  if (c.inLoopCond == 0 and (isUnpackedTuple(dest) or isDecl or
      (isAnalysableFieldAccess(dest, c.owner) and isFirstWrite(dest, c)))) or
      isNoInit(dest):
    # optimize sink call into a bitwise memcopy
    result = newTree(nkFastAsgn, dest, ri)
  else:
    let t = dest.typ.skipTypes({tyGenericInst, tyAlias, tySink})
    if getAttachedOp(c.graph, t, attachedSink) != nil:
      result = c.genOp(t, attachedSink, dest, ri)
      result.add ri
    else:
      # the default is to use combination of `=destroy(dest)` and
      # and copyMem(dest, source). This is efficient.
      result = newTree(nkStmtList, c.genDestroy(dest), newTree(nkFastAsgn, dest, ri))

proc isCriticalLink(dest: PNode): bool {.inline.} =
  #[
  Lins's idea that only "critical" links can introduce a cycle. This is
  critical for the performance gurantees that we strive for: If you
  traverse a data structure, no tracing will be performed at all.
  ORC is about this promise: The GC only touches the memory that the
  mutator touches too.

  These constructs cannot possibly create cycles::

    local = ...

    new(x)
    dest = ObjectConstructor(field: noalias(dest))

  But since 'ObjectConstructor' is already moved into 'dest' all we really have
  to look for is assignments to local variables.
  ]#
  result = dest.kind != nkSym

proc finishCopy(c: var Con; result, dest: PNode; isFromSink: bool) =
  if c.graph.config.selectedGC == gcOrc:
    let t = dest.typ.skipTypes({tyGenericInst, tyAlias, tySink, tyDistinct})
    if cyclicType(t):
      result.add boolLit(c.graph, result.info, isFromSink or isCriticalLink(dest))

proc genMarkCyclic(c: Con; result, dest: PNode) =
  if c.graph.config.selectedGC == gcOrc:
    let t = dest.typ.skipTypes({tyGenericInst, tyAlias, tySink, tyDistinct})
    if cyclicType(t):
      if t.kind == tyRef:
        result.add callCodegenProc(c.graph, "nimMarkCyclic", dest.info, dest)
      else:
        let xenv = genBuiltin(c.graph, c.idgen, mAccessEnv, "accessEnv", dest)
        xenv.typ = getSysType(c.graph, dest.info, tyPointer)
        result.add callCodegenProc(c.graph, "nimMarkCyclic", dest.info, xenv)

proc genCopyNoCheck(c: var Con; dest, ri: PNode): PNode =
  let t = dest.typ.skipTypes({tyGenericInst, tyAlias, tySink})
  result = c.genOp(t, attachedAsgn, dest, ri)
  assert ri.typ != nil

proc genCopy(c: var Con; dest, ri: PNode): PNode =
  let t = dest.typ
  if tfHasOwned in t.flags and ri.kind != nkNilLit:
    # try to improve the error message here:
    c.checkForErrorPragma(t, ri, "=copy")
  result = c.genCopyNoCheck(dest, ri)
  assert ri.typ != nil

proc genDiscriminantAsgn(c: var Con; s: var Scope; n: PNode): PNode =
  # discriminator is ordinal value that doesn't need sink destroy
  # but fields within active case branch might need destruction

  # tmp to support self assignments
  let tmp = c.getTemp(s, n[1].typ, n.info)

  result = newTree(nkStmtList)
  result.add newTree(nkFastAsgn, tmp, p(n[1], c, s, consumed))
  result.add p(n[0], c, s, normal)

  let le = p(n[0], c, s, normal)
  let leDotExpr = if le.kind == nkCheckedFieldExpr: le[0] else: le
  let objType = leDotExpr[0].typ

  if hasDestructor(c, objType):
    if getAttachedOp(c.graph, objType, attachedDestructor) != nil and
        sfOverriden in getAttachedOp(c.graph, objType, attachedDestructor).flags:
      localReport(c.graph.config, n, reportSem rsemCannotAssignToDiscriminantWithCustomDestructor)
      result.add newTree(nkFastAsgn, le, tmp)
      return

    # generate: if le != tmp: `=destroy`(le)
    let branchDestructor = produceDestructorForDiscriminator(c.graph, objType, leDotExpr[1].sym, n.info, c.idgen)
    let cond = newTreeIT(nkInfix, n.info, getSysType(c.graph, unknownLineInfo, tyBool)):
      [newSymNode(getMagicEqSymForType(c.graph, le.typ, n.info)), le, tmp]
    let notExpr = newTreeIT(nkPrefix, n.info, getSysType(c.graph, unknownLineInfo, tyBool)):
      [newSymNode(createMagic(c.graph, c.idgen, "not", mNot)), cond]
    result.add newTree(nkIfStmt, newTree(nkElifBranch, notExpr, c.genOp(branchDestructor, le)))
  result.add newTree(nkFastAsgn, le, tmp)

proc genWasMoved(c: var Con, n: PNode): PNode =
  result = newTreeI(nkCall, n.info):
    [newSymNode(createMagic(c.graph, c.idgen, "wasMoved", mWasMoved)),
     copyTree(n)] #mWasMoved does not take the address
  #if n.kind != nkSym:
  #  message(c.graph.config, n.info, warnUser, "wasMoved(" & $n & ")")

proc genDefaultCall(t: PType; c: Con; info: TLineInfo): PNode =
  result = newTreeIT(nkCall, info, t):
    newSymNode(createMagic(c.graph, c.idgen, "default", mDefault))

proc destructiveMoveVar(n: PNode; c: var Con; s: var Scope): PNode =
  # generate: (let tmp = v; reset(v); tmp)
  if not hasDestructor(c, n.typ):
    assert n.kind != nkSym or not hasDestructor(c, n.sym.typ)
    result = copyTree(n)
  else:
    var temp = newSym(skLet, getIdent(c.graph.cache, "blitTmp"), nextSymId c.idgen, c.owner, n.info)
    temp.typ = n.typ
    let tempAsNode = newSymNode(temp)

    var vpart = newTreeI(nkIdentDefs, tempAsNode.info):
      [tempAsNode, newNodeI(nkEmpty, tempAsNode.info), n]

    let v = newTreeI(nkLetSection, n.info): vpart

    let nn = skipConv(n)
    result = newNodeIT(nkStmtListExpr, n.info, n.typ)
    result.add v
    c.genMarkCyclic(result, nn)
    result.add c.genWasMoved(nn)
    result.add tempAsNode

proc isCapturedVar(n: PNode): bool =
  let root = getRoot(n)
  if root != nil: result = root.name.s[0] == ':'

proc passCopyToSink(n: PNode; c: var Con; s: var Scope): PNode =
  result = newNodeIT(nkStmtListExpr, n.info, n.typ)
  let tmp = c.getTemp(s, n.typ, n.info)
  if hasDestructor(c, n.typ):
    result.add c.genWasMoved(tmp)
    var m = c.genCopy(tmp, n)
    m.add p(n, c, s, normal)
    c.finishCopy(m, n, isFromSink = true)
    result.add m
    if isLValue(n) and
       not isCapturedVar(n) and
       n.typ.skipTypes(abstractInst).kind != tyRef:

      localReport(c.graph.config, n, reportSem rsemCopiesToSink)

  else:
    if c.graph.config.selectedGC in {gcArc, gcOrc}:
      assert(not containsManagedMemory(n.typ))

    if n.typ.skipTypes(abstractInst).kind in {tyOpenArray, tyVarargs}:
      localReport(c.graph.config, n.info, reportAst(
        rsemCannotCreateImplicitOpenarray, n))

    result.add newTree(nkAsgn, tmp, p(n, c, s, normal))
  # Since we know somebody will take over the produced copy, there is
  # no need to destroy it.
  result.add tmp

proc isDangerousSeq(t: PType): bool {.inline.} =
  let t = t.skipTypes(abstractInst)
  result = t.kind == tySequence and tfHasOwned notin t[0].flags

proc containsConstSeq(n: PNode): bool =
  if n.kind == nkBracket and n.len > 0 and n.typ != nil and isDangerousSeq(n.typ):
    return true
  result = false
  case n.kind
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = containsConstSeq(n[1])
  of nkObjConstr, nkClosure:
    for i in 1..<n.len:
      if containsConstSeq(n[i]): return true
  of nkCurly, nkBracket, nkPar, nkTupleConstr:
    for son in n:
      if containsConstSeq(son): return true
  else: discard

proc ensureDestruction(arg, orig: PNode; c: var Con; s: var Scope): PNode =
  # it can happen that we need to destroy expression contructors
  # like [], (), closures explicitly in order to not leak them.
  if arg.typ != nil and hasDestructor(c, arg.typ):
    # produce temp creation for (fn, env). But we need to move 'env'?
    # This was already done in the sink parameter handling logic.
    let tmp = c.getTemp(s, arg.typ, arg.info)
    result = newTreeIT(nkStmtListExpr, arg.info, arg.typ):
      [c.genSink(tmp, arg, isDecl = true), tmp]
    s.final.add c.genDestroy(tmp)
  else:
    result = arg

proc cycleCheck(n: PNode; c: var Con) =
  if c.graph.config.selectedGC != gcArc: return
  var value = n[1]
  if value.kind == nkClosure:
    value = value[1]
  if value.kind == nkNilLit: return
  let destTyp = n[0].typ.skipTypes(abstractInst)
  if destTyp.kind != tyRef and not (destTyp.kind == tyProc and destTyp.callConv == ccClosure):
    return

  var x = n[0]
  var field: PNode = nil
  while true:
    if x.kind == nkDotExpr:
      field = x[1]
      if field.kind == nkSym and sfCursor in field.sym.flags: return
      x = x[0]
    elif x.kind in {nkBracketExpr, nkCheckedFieldExpr, nkDerefExpr, nkHiddenDeref}:
      x = x[0]
    else:
      break
    if exprStructuralEquivalent(x, value, strictSymEquality = true):
      localReport(c.graph.config, n.info):
        reportAst(rsemUncollectableRefCycle, field).withIt do:
          it.cycleField = field


      break

proc pVarTopLevel(v: PNode; c: var Con; s: var Scope; res: PNode) =
  # move the variable declaration to the top of the frame:
  s.vars.add v.sym
  if isUnpackedTuple(v):
    if c.inLoop > 0:
      # unpacked tuple needs reset at every loop iteration
      res.add newTree(nkFastAsgn, v, genDefaultCall(v.typ, c, v.info))
  elif sfThread notin v.sym.flags and sfCursor notin v.sym.flags:
    # do not destroy thread vars for now at all for consistency.
    if sfGlobal in v.sym.flags and s.parent == nil: #XXX: Rethink this logic (see tarcmisc.test2)
      c.graph.globalDestructors.add c.genDestroy(v)
    else:
      s.final.add c.genDestroy(v)

proc processScope(c: var Con; s: var Scope; ret: PNode): PNode =
  result = newNodeI(nkStmtList, ret.info)
  if s.vars.len > 0:
    let varSection = newNodeI(nkVarSection, ret.info)
    for tmp in s.vars:
      varSection.add newTree(nkIdentDefs,
        [newSymNode(tmp), newNodeI(nkEmpty, ret.info), newNodeI(nkEmpty, ret.info)])
    result.add varSection
  if s.wasMoved.len > 0 or s.final.len > 0:
    let finSection = newNodeI(nkStmtList, ret.info)
    for m in s.wasMoved: finSection.add m
    for i in countdown(s.final.high, 0): finSection.add s.final[i]
    if s.needsTry:
      result.add newTryFinally(ret, finSection)
    else:
      result.add ret
      result.add finSection
  else:
    result.add ret

  if s.parent != nil: s.parent[].needsTry = s.parent[].needsTry or s.needsTry

template processScopeExpr(c: var Con; s: var Scope; ret: PNode, processCall: untyped): PNode =
  assert not ret.typ.isEmptyType
  var result = newNodeI(nkStmtListExpr, ret.info)
  # There is a possibility to do this check: s.wasMoved.len > 0 or s.final.len > 0
  # later and use it to eliminate the temporary when theres no need for it, but its
  # tricky because you would have to intercept moveOrCopy at a certain point
  let tmp = c.getTemp(s.parent[], ret.typ, ret.info)
  tmp.sym.flags.incl sfSingleUsedTemp
  let cpy = if hasDestructor(c, ret.typ):
              s.parent[].final.add c.genDestroy(tmp)
              moveOrCopy(tmp, ret, c, s, isDecl = true)
            else:
              newTree(nkFastAsgn, tmp, p(ret, c, s, normal))

  if s.vars.len > 0:
    let varSection = newNodeI(nkVarSection, ret.info)
    for tmp in s.vars:
      varSection.add newTree(nkIdentDefs, # TODO XXX lowerings.nim????????
        [newSymNode(tmp), newNodeI(nkEmpty, ret.info), newNodeI(nkEmpty, ret.info)])
    result.add varSection
  let finSection = newNodeI(nkStmtList, ret.info)
  for m in s.wasMoved: finSection.add m
  for i in countdown(s.final.high, 0): finSection.add s.final[i]
  if s.needsTry:
    result.add newTryFinally(newTree(nkStmtListExpr, cpy, processCall(tmp, s.parent[])), finSection)
  else:
    result.add cpy
    result.add finSection
    result.add processCall(tmp, s.parent[])

  if s.parent != nil: s.parent[].needsTry = s.parent[].needsTry or s.needsTry

  result

template handleNestedTempl(n, processCall: untyped, willProduceStmt = false) =
  template maybeVoid(child, s): untyped =
    if isEmptyType(child.typ): p(child, c, s, normal)
    else: processCall(child, s)

  case n.kind
  of nkStmtList, nkStmtListExpr:
    # a statement list does not open a new scope
    if n.len == 0: return n
    result = copyNode(n)
    for i in 0..<n.len-1:
      result.add p(n[i], c, s, normal)
    result.add maybeVoid(n[^1], s)

  of nkCaseStmt:
    result = copyNode(n)
    result.add p(n[0], c, s, normal)
    for i in 1..<n.len:
      let it = n[i]
      assert it.kind in {nkOfBranch, nkElse}

      var branch = shallowCopy(it)
      for j in 0 ..< it.len-1:
        branch[j] = copyTree(it[j])
      var ofScope = nestedScope(s)
      branch[^1] = if it[^1].typ.isEmptyType or willProduceStmt:
                     processScope(c, ofScope, maybeVoid(it[^1], ofScope))
                   else:
                     processScopeExpr(c, ofScope, it[^1], processCall)
      result.add branch

  of nkWhileStmt:
    inc c.inLoop
    inc c.inLoopCond
    result = copyNode(n)
    result.add p(n[0], c, s, normal)
    dec c.inLoopCond
    var bodyScope = nestedScope(s)
    let bodyResult = p(n[1], c, bodyScope, normal)
    result.add processScope(c, bodyScope, bodyResult)
    dec c.inLoop

  of nkBlockStmt, nkBlockExpr:
    result = copyNode(n)
    result.add n[0]
    var bodyScope = nestedScope(s)
    result.add if n[1].typ.isEmptyType or willProduceStmt:
                 processScope(c, bodyScope, processCall(n[1], bodyScope))
               else:
                 processScopeExpr(c, bodyScope, n[1], processCall)

  of nkIfStmt, nkIfExpr:
    result = copyNode(n)
    for i in 0..<n.len:
      let it = n[i]
      var branch = shallowCopy(it)
      var branchScope = nestedScope(s)
      if it.kind in {nkElifBranch, nkElifExpr}:
        #Condition needs to be destroyed outside of the condition/branch scope
        branch[0] = p(it[0], c, s, normal)

      branch[^1] = if it[^1].typ.isEmptyType or willProduceStmt:
                     processScope(c, branchScope, maybeVoid(it[^1], branchScope))
                   else:
                     processScopeExpr(c, branchScope, it[^1], processCall)
      result.add branch

  of nkTryStmt:
    result = copyNode(n)
    var tryScope = nestedScope(s)
    result.add if n[0].typ.isEmptyType or willProduceStmt:
                 processScope(c, tryScope, maybeVoid(n[0], tryScope))
               else:
                 processScopeExpr(c, tryScope, n[0], maybeVoid)

    for i in 1..<n.len:
      let it = n[i]
      var branch = copyTree(it)
      var branchScope = nestedScope(s)
      branch[^1] = if it[^1].typ.isEmptyType or willProduceStmt or it.kind == nkFinally:
                     processScope(c, branchScope, if it.kind == nkFinally: p(it[^1], c, branchScope, normal)
                                                  else: maybeVoid(it[^1], branchScope))
                   else:
                     processScopeExpr(c, branchScope, it[^1], processCall)
      result.add branch

  of nkWhen: # This should be a "when nimvm" node.
    result = copyTree(n)
    result[1][0] = processCall(n[1][0], s)
  else: assert(false)

proc pRaiseStmt(n: PNode, c: var Con; s: var Scope): PNode =
  if optOwnedRefs in c.graph.config.globalOptions and n[0].kind != nkEmpty:
    if n[0].kind in nkCallKinds:
      let call = p(n[0], c, s, normal)
      result = copyNode(n)
      result.add call
    else:
      let tmp = c.getTemp(s, n[0].typ, n.info)
      var m = c.genCopyNoCheck(tmp, n[0])
      m.add p(n[0], c, s, normal)
      c.finishCopy(m, n[0], isFromSink = false)
      result = newTree(nkStmtList, c.genWasMoved(tmp), m)
      var toDisarm = n[0]
      if toDisarm.kind == nkStmtListExpr: toDisarm = toDisarm.lastSon
      if toDisarm.kind == nkSym and toDisarm.sym.owner == c.owner:
        result.add c.genWasMoved(toDisarm)
      result.add newTree(nkRaiseStmt, tmp)
  else:
    result = copyNode(n)
    if n[0].kind != nkEmpty:
      result.add p(n[0], c, s, sinkArg)
    else:
      result.add copyNode(n[0])
  s.needsTry = true

proc p(n: PNode; c: var Con; s: var Scope; mode: ProcessMode): PNode =
  if n.kind in {nkStmtList, nkStmtListExpr, nkBlockStmt, nkBlockExpr, nkIfStmt,
                nkIfExpr, nkCaseStmt, nkWhen, nkWhileStmt, nkTryStmt}:
    template process(child, s): untyped = p(child, c, s, mode)
    handleNestedTempl(n, process)
  elif mode == sinkArg:
    if n.containsConstSeq:
      # const sequences are not mutable and so we need to pass a copy to the
      # sink parameter (bug #11524). Note that the string implementation is
      # different and can deal with 'const string sunk into var'.
      result = passCopyToSink(n, c, s)
    elif n.kind in {nkBracket, nkObjConstr, nkTupleConstr, nkClosure, nkNilLit} +
         nkCallKinds + nkLiterals:
      result = p(n, c, s, consumed)
    elif ((n.kind == nkSym and isSinkParam(n.sym)) or isAnalysableFieldAccess(n, c.owner)) and
        isLastRead(n, c) and not (n.kind == nkSym and isCursor(n)):
      # Sinked params can be consumed only once. We need to reset the memory
      # to disable the destructor which we have not elided
      result = destructiveMoveVar(n, c, s)
    elif n.kind in {nkHiddenSubConv, nkHiddenStdConv, nkConv}:
      result = copyTree(n)
      if n.typ.skipTypes(abstractInst-{tyOwned}).kind != tyOwned and
          n[1].typ.skipTypes(abstractInst-{tyOwned}).kind == tyOwned:
        # allow conversions from owned to unowned via this little hack:
        let nTyp = n[1].typ
        n[1].typ = n.typ
        result[1] = p(n[1], c, s, sinkArg)
        result[1].typ = nTyp
      else:
        result[1] = p(n[1], c, s, sinkArg)
    elif n.kind in {nkObjDownConv, nkObjUpConv}:
      result = copyTree(n)
      result[0] = p(n[0], c, s, sinkArg)
    elif n.typ == nil:
      # 'raise X' can be part of a 'case' expression. Deal with it here:
      result = p(n, c, s, normal)
    else:
      # copy objects that are not temporary but passed to a 'sink' parameter
      result = passCopyToSink(n, c, s)
  else:
    case n.kind
    of nkBracket, nkTupleConstr, nkClosure, nkCurly:
      # Let C(x) be the construction, 'x' the vector of arguments.
      # C(x) either owns 'x' or it doesn't.
      # If C(x) owns its data, we must consume C(x).
      # If it doesn't own the data, it's harmful to destroy it (double frees etc).
      # We have the freedom to choose whether it owns it or not so we are smart about it
      # and we say, "if passed to a sink we demand C(x) to own its data"
      # otherwise we say "C(x) is just some temporary storage, it doesn't own anything,
      # don't destroy it"
      # but if C(x) is a ref it MUST own its data since we must destroy it
      # so then we have no choice but to use 'sinkArg'.
      let m = if mode == normal: normal
              else: sinkArg

      result = copyTree(n)
      for i in ord(n.kind == nkClosure)..<n.len:
        if n[i].kind == nkExprColonExpr:
          result[i][1] = p(n[i][1], c, s, m)
        elif n[i].kind == nkRange:
          result[i][0] = p(n[i][0], c, s, m)
          result[i][1] = p(n[i][1], c, s, m)
        else:
          result[i] = p(n[i], c, s, m)
    of nkObjConstr:
      # see also the remark about `nkTupleConstr`.
      let t = n.typ.skipTypes(abstractInst)
      let isRefConstr = t.kind == tyRef
      let m = if isRefConstr: sinkArg
              elif mode == normal: normal
              else: sinkArg

      result = copyTree(n)
      for i in 1..<n.len:
        if n[i].kind == nkExprColonExpr:
          let field = lookupFieldAgain(t, n[i][0].sym)
          if field != nil and sfCursor in field.flags:
            result[i][1] = p(n[i][1], c, s, normal)
          else:
            result[i][1] = p(n[i][1], c, s, m)
        else:
          result[i] = p(n[i], c, s, m)
      if mode == normal and isRefConstr:
        result = ensureDestruction(result, n, c, s)
    of nkCallKinds:
      let parameters = n[0].typ
      let L = if parameters != nil: parameters.len else: 0

      when false:
        var isDangerous = false
        if n[0].kind == nkSym and n[0].sym.magic in {mOr, mAnd}:
          inc c.inDangerousBranch
          isDangerous = true

      result = shallowCopy(n)
      for i in 1..<n.len:
        if i < L and isCompileTimeOnly(parameters[i]):
          result[i] = n[i]
        elif i < L and isSinkTypeForParam(parameters[i]):
          result[i] = p(n[i], c, s, sinkArg)
        else:
          result[i] = p(n[i], c, s, normal)

      when false:
        if isDangerous:
          dec c.inDangerousBranch

      if n[0].kind == nkSym and n[0].sym.magic in {mNew, mNewFinalize}:
        result[0] = copyTree(n[0])
        if c.graph.config.selectedGC in {gcHooks, gcArc, gcOrc}:
          let destroyOld = c.genDestroy(result[1])
          result = newTree(nkStmtList, destroyOld, result)
      else:
        result[0] = p(n[0], c, s, normal)
      if canRaise(n[0]): s.needsTry = true
      if mode == normal:
        result = ensureDestruction(result, n, c, s)
    of nkDiscardStmt: # Small optimization
      result = shallowCopy(n)
      if n[0].kind != nkEmpty:
        result[0] = p(n[0], c, s, normal)
      else:
        result[0] = copyNode(n[0])
    of nkVarSection, nkLetSection:
      # transform; var x = y to  var x; x op y  where op is a move or copy
      result = newNodeI(nkStmtList, n.info)
      for it in n:
        var ri = it[^1]
        if it.kind == nkVarTuple and hasDestructor(c, ri.typ):
          let x = lowerTupleUnpacking(c.graph, it, c.idgen, c.owner)
          result.add p(x, c, s, consumed)
        elif it.kind == nkIdentDefs and hasDestructor(c, it[0].typ):
          for j in 0..<it.len-2:
            let v = it[j]
            if v.kind == nkSym:
              if sfCompileTime in v.sym.flags: continue
              pVarTopLevel(v, c, s, result)
            if ri.kind != nkEmpty:
              result.add moveOrCopy(v, ri, c, s, isDecl = v.kind == nkSym)
            elif ri.kind == nkEmpty and c.inLoop > 0:
              result.add moveOrCopy(v, genDefaultCall(v.typ, c, v.info), c, s, isDecl = v.kind == nkSym)
        else: # keep the var but transform 'ri':
          var v = copyNode(n)
          var itCopy = copyNode(it)
          for j in 0..<it.len-1:
            itCopy.add it[j]
          itCopy.add p(it[^1], c, s, normal)
          v.add itCopy
          result.add v
    of nkAsgn, nkFastAsgn:
      if hasDestructor(c, n[0].typ) and n[1].kind notin {nkProcDef, nkDo, nkLambda}:
        if n[0].kind in {nkDotExpr, nkCheckedFieldExpr}:
          cycleCheck(n, c)
        assert n[1].kind notin {nkAsgn, nkFastAsgn}
        result = moveOrCopy(p(n[0], c, s, mode), n[1], c, s)
      elif isDiscriminantField(n[0]):
        result = c.genDiscriminantAsgn(s, n)
      else:
        result = copyNode(n)
        result.add p(n[0], c, s, mode)
        result.add p(n[1], c, s, consumed)
    of nkRaiseStmt:
      result = pRaiseStmt(n, c, s)
    of nkWhileStmt:
      internalError(c.graph.config, n.info, "nkWhileStmt should have been handled earlier")
      result = n
    of nkNone..nkNilLit, nkTypeSection, nkProcDef, nkConverterDef,
       nkMethodDef, nkIteratorDef, nkMacroDef, nkTemplateDef, nkLambda, nkDo,
       nkFuncDef, nkConstSection, nkConstDef, nkIncludeStmt, nkImportStmt,
       nkExportStmt, nkPragma, nkCommentStmt, nkBreakState,
       nkTypeOfExpr, nkMixinStmt, nkBindStmt:
      result = n

    of nkStringToCString, nkCStringToString, nkChckRangeF, nkChckRange64, nkChckRange, nkPragmaBlock:
      result = shallowCopy(n)
      for i in 0 ..< n.len:
        result[i] = p(n[i], c, s, normal)
      if n.typ != nil and hasDestructor(c, n.typ):
        if mode == normal:
          result = ensureDestruction(result, n, c, s)

    of nkHiddenSubConv, nkHiddenStdConv, nkConv:
      # we have an "ownership invariance" for all constructors C(x).
      # See the comment for nkBracket construction. If the caller wants
      # to own 'C(x)', it really wants to own 'x' too. If it doesn't,
      # we need to destroy 'x' but the function call handling ensures that
      # already.
      result = copyTree(n)
      if n.typ.skipTypes(abstractInst-{tyOwned}).kind != tyOwned and
          n[1].typ.skipTypes(abstractInst-{tyOwned}).kind == tyOwned:
        # allow conversions from owned to unowned via this little hack:
        let nTyp = n[1].typ
        n[1].typ = n.typ
        result[1] = p(n[1], c, s, mode)
        result[1].typ = nTyp
      else:
        result[1] = p(n[1], c, s, mode)

    of nkObjDownConv, nkObjUpConv:
      result = copyTree(n)
      result[0] = p(n[0], c, s, mode)

    of nkDotExpr:
      result = shallowCopy(n)
      result[0] = p(n[0], c, s, normal)
      for i in 1 ..< n.len:
        result[i] = n[i]
      if mode == sinkArg and hasDestructor(c, n.typ):
        if isAnalysableFieldAccess(n, c.owner) and isLastRead(n, c):
          s.wasMoved.add c.genWasMoved(n)
        else:
          result = passCopyToSink(result, c, s)

    of nkBracketExpr, nkAddr, nkHiddenAddr, nkDerefExpr, nkHiddenDeref:
      result = shallowCopy(n)
      for i in 0 ..< n.len:
        result[i] = p(n[i], c, s, normal)
      if mode == sinkArg and hasDestructor(c, n.typ):
        if isAnalysableFieldAccess(n, c.owner) and isLastRead(n, c):
          # consider 'a[(g; destroy(g); 3)]', we want to say 'wasMoved(a[3])'
          # without the junk, hence 'c.genWasMoved(n)'
          # and not 'c.genWasMoved(result)':
          s.wasMoved.add c.genWasMoved(n)
        else:
          result = passCopyToSink(result, c, s)

    of nkDefer, nkRange:
      result = shallowCopy(n)
      for i in 0 ..< n.len:
        result[i] = p(n[i], c, s, normal)

    of nkBreakStmt:
      s.needsTry = true
      result = n
    of nkReturnStmt:
      result = shallowCopy(n)
      for i in 0..<n.len:
        result[i] = p(n[i], c, s, mode)
      s.needsTry = true
    of nkCast:
      result = shallowCopy(n)
      result[0] = n[0]
      result[1] = p(n[1], c, s, mode)
    of nkCheckedFieldExpr:
      result = shallowCopy(n)
      result[0] = p(n[0], c, s, mode)
      for i in 1..<n.len:
        result[i] = n[i]
    of nkGotoState, nkState, nkAsmStmt:
      result = n
    else:
      internalError(c.graph.config, n.info, "cannot inject destructors to node kind: " & $n.kind)

proc sameLocation*(a, b: PNode): bool =
  proc sameConstant(a, b: PNode): bool =
    a.kind in nkLiterals and a.intVal == b.intVal

  const nkEndPoint = {nkSym, nkDotExpr, nkCheckedFieldExpr, nkBracketExpr}
  if a.kind in nkEndPoint and b.kind in nkEndPoint:
    if a.kind == b.kind:
      case a.kind
      of nkSym: a.sym == b.sym
      of nkDotExpr, nkCheckedFieldExpr: sameLocation(a[0], b[0]) and sameLocation(a[1], b[1])
      of nkBracketExpr: sameLocation(a[0], b[0]) and sameConstant(a[1], b[1])
      else: false
    else: false
  else:
    case a.kind
    of nkSym, nkDotExpr, nkCheckedFieldExpr, nkBracketExpr:
      # Reached an endpoint, flip to recurse the other side.
      sameLocation(b, a)
    of nkAddr, nkHiddenAddr, nkDerefExpr, nkHiddenDeref:
      # We don't need to check addr/deref levels or differentiate between the two,
      # since pointers don't have hooks :) (e.g: var p: ptr pointer; p[] = addr p)
      sameLocation(a[0], b)
    of nkObjDownConv, nkObjUpConv: sameLocation(a[0], b)
    of nkHiddenStdConv, nkHiddenSubConv: sameLocation(a[1], b)
    else: false

proc moveOrCopy(dest, ri: PNode; c: var Con; s: var Scope, isDecl = false): PNode =
  if sameLocation(dest, ri):
    # rule (self-assignment-removal):
    result = newNodeI(nkEmpty, dest.info)
  elif isCursor(dest):
    case ri.kind:
    of nkStmtListExpr, nkBlockExpr, nkIfExpr, nkCaseStmt, nkTryStmt:
      template process(child, s): untyped = moveOrCopy(dest, child, c, s, isDecl)
      # We know the result will be a stmt so we use that fact to optimize
      handleNestedTempl(ri, process, willProduceStmt = true)
    else:
      result = newTree(nkFastAsgn, dest, p(ri, c, s, normal))
  else:
    case ri.kind
    of nkCallKinds:
      result = c.genSink(dest, p(ri, c, s, consumed), isDecl)
    of nkBracketExpr:
      if isUnpackedTuple(ri[0]):
        # unpacking of tuple: take over the elements
        result = c.genSink(dest, p(ri, c, s, consumed), isDecl)
      elif isAnalysableFieldAccess(ri, c.owner) and isLastRead(ri, c):
        if aliases(dest, ri) == no:
          # Rule 3: `=sink`(x, z); wasMoved(z)
          var snk = c.genSink(dest, ri, isDecl)
          result = newTree(nkStmtList, snk, c.genWasMoved(ri))
        else:
          result = c.genSink(dest, destructiveMoveVar(ri, c, s), isDecl)
      else:
        result = c.genCopy(dest, ri)
        result.add p(ri, c, s, consumed)
        c.finishCopy(result, dest, isFromSink = false)
    of nkBracket:
      # array constructor
      if ri.len > 0 and isDangerousSeq(ri.typ):
        result = c.genCopy(dest, ri)
        result.add p(ri, c, s, consumed)
        c.finishCopy(result, dest, isFromSink = false)
      else:
        result = c.genSink(dest, p(ri, c, s, consumed), isDecl)
    of nkObjConstr, nkTupleConstr, nkClosure, nkCharLit..nkNilLit:
      result = c.genSink(dest, p(ri, c, s, consumed), isDecl)
    of nkSym:
      if isSinkParam(ri.sym) and isLastRead(ri, c):
        # Rule 3: `=sink`(x, z); wasMoved(z)
        let snk = c.genSink(dest, ri, isDecl)
        result = newTree(nkStmtList, snk, c.genWasMoved(ri))
      elif ri.sym.kind != skParam and ri.sym.owner == c.owner and
          isLastRead(ri, c) and canBeMoved(c, dest.typ) and not isCursor(ri):
        # Rule 3: `=sink`(x, z); wasMoved(z)
        let snk = c.genSink(dest, ri, isDecl)
        result = newTree(nkStmtList, snk, c.genWasMoved(ri))
      else:
        result = c.genCopy(dest, ri)
        result.add p(ri, c, s, consumed)
        c.finishCopy(result, dest, isFromSink = false)
    of nkHiddenSubConv, nkHiddenStdConv, nkConv, nkObjDownConv, nkObjUpConv, nkCast:
      result = c.genSink(dest, p(ri, c, s, sinkArg), isDecl)
    of nkStmtListExpr, nkBlockExpr, nkIfExpr, nkCaseStmt, nkTryStmt:
      template process(child, s): untyped = moveOrCopy(dest, child, c, s, isDecl)
      # We know the result will be a stmt so we use that fact to optimize
      handleNestedTempl(ri, process, willProduceStmt = true)
    of nkRaiseStmt:
      result = pRaiseStmt(ri, c, s)
    else:
      if isAnalysableFieldAccess(ri, c.owner) and isLastRead(ri, c) and
          canBeMoved(c, dest.typ):
        # Rule 3: `=sink`(x, z); wasMoved(z)
        let snk = c.genSink(dest, ri, isDecl)
        result = newTree(nkStmtList, snk, c.genWasMoved(ri))
      else:
        result = c.genCopy(dest, ri)
        result.add p(ri, c, s, consumed)
        c.finishCopy(result, dest, isFromSink = false)

proc computeUninit(c: var Con) =
  if not c.uninitComputed:
    c.uninitComputed = true
    c.uninit = initIntSet()
    var init = initIntSet()
    discard initialized(c.g, pc = 0, init, c.uninit, int.high)

proc injectDefaultCalls(n: PNode, c: var Con) =
  case n.kind
  of nkVarSection, nkLetSection:
    for it in n:
      if it.kind == nkIdentDefs and it[^1].kind == nkEmpty:
        computeUninit(c)
        for j in 0..<it.len-2:
          let v = it[j]
          doAssert v.kind == nkSym
          if c.uninit.contains(v.sym.id):
            it[^1] = genDefaultCall(v.sym.typ, c, v.info)
            break
  of nkNone..nkNilLit, nkTypeSection, nkProcDef, nkConverterDef, nkMethodDef,
      nkIteratorDef, nkMacroDef, nkTemplateDef, nkLambda, nkDo, nkFuncDef:
    discard
  else:
    for i in 0..<n.safeLen:
      injectDefaultCalls(n[i], c)

proc injectDestructorCalls*(g: ModuleGraph; idgen: IdGenerator; owner: PSym; n: PNode): PNode =
  when toDebug.len > 0:
    shouldDebug = toDebug == owner.name.s or toDebug == "always"
  if sfGeneratedOp in owner.flags or (owner.kind == skIterator and isInlineIterator(owner.typ)):
    return n
  var c = Con(owner: owner, graph: g, g: constructCfg(owner, n), idgen: idgen)
  dbg:
    echo "\n### ", owner.name.s, ":\nCFG:"
    echoCfg(c.g)
    echo n

  if optCursorInference in g.config.options:
    computeCursors(owner, n, g)

  computeLastReadsAndFirstWrites(c.g)

  var scope: Scope
  let body = p(n, c, scope, normal)

  if owner.kind in {skProc, skFunc, skMethod, skIterator, skConverter}:
    let params = owner.typ.n
    for i in 1..<params.len:
      let t = params[i].sym.typ
      if isSinkTypeForParam(t) and hasDestructor(c, t.skipTypes({tySink})):
        scope.final.add c.genDestroy(params[i])
  #if optNimV2 in c.graph.config.globalOptions:
  #  injectDefaultCalls(n, c)
  result = optimize processScope(c, scope, body)
  dbg:
    echo ">---------transformed-to--------->"
    echo renderTree(result, {renderIds})

  if g.config.arcToExpand.hasKey(owner.name.s):
    g.config.localReport(SemReport(
      kind: rsemExpandArc,
      ast: n,
      sym: owner,
      expandedAst: result
    ))
