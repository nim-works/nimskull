## This module implements the transformations over the IR.
## There are two kinds of passes:
## 1. linear transform
## 2. static-control-flow-based transforms
##
## A linear transform just walks over each IR node in insertion order and injects, removes or replaces nodes.
##
## A static-control-flow-based transform collects state for each node base on static control-flow and performs modifications based on the gathered data
##

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_types,
    ast,
    idents,
    lineinfos,
    types
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/sem/[
    sighashes
  ],
  compiler/vm/[
    irdbg,
    vmir
  ]

from compiler/ast/astalgo import getModule
from compiler/vm/vmdef import unreachable

type
  PassError* = object of CatchableError
    n*: IRIndex

  LinearPass*[T] = object
    when T is void:
      visit: proc (x: IrNode3, c: var IrCursor)
    else:
      visit: proc (env: T, x: IrNode3, c: var IrCursor)

  LinearPass2*[T] = object
    when T is void:
      discard#visit: proc (x: IrNode3, c: var IrCursor)
    else:
      # XXX: meh, a mutable env is not always necessary
      visit: proc (env: var T, x: IrNode3, ir: IrStore3, c: var IrCursor)

type CfPass*[G; L] = object
  ## Static-control-flow based pass
  visit: proc (gs: var G, ls: var L, c: var IrCursor, irs: IrStore3, n: IrNode3)
  merge: proc (a: var L, b: L)
  onEnd: proc (gs: var G, ls: L)

# TODO: rename
template customAssert(cond: bool, node: IRIndex) =
  if not cond:
    raise (ref PassError)(msg: astToStr(cond), n: node)

proc runPass*[T](irs: var IrStore3, ctx: T, pass: LinearPass[T]) =
  var cursor: IrCursor
  cursor.setup(irs)

  try:
    var i = 0
    for n in irs.nodes:
      cursor.setPos(i)
      pass.visit(ctx, n, cursor)
      inc i
  except PassError as e:
    echo e.getStackTrace()
    echo "Msg: ", e.msg
    echo "IR (error at node: ", e.n, "):"
    printIr(irs, calcStmt(irs))
    echo "Node was added at: "
    for e in irs.traceFor(e.n).items:
      debugEcho e

  irs.update(cursor)

proc runPass*[T](irs: var IrStore3, ctx: var T, pass: LinearPass2[T]) =
  var cursor: IrCursor
  cursor.setup(irs)

  try:
    var i = 0
    for n in irs.nodes:
      cursor.setPos(i)
      pass.visit(ctx, n, irs, cursor)
      inc i
  except PassError as e:
    echo e.getStackTrace()
    echo "Msg: ", e.msg
    echo "IR (error at node: ", e.n, "):"
    printIr(irs, calcStmt(irs))
    echo "Node was added at: "
    for e in irs.traceFor(e.n).items:
      debugEcho e

  irs.update(cursor)


proc runV2*[G, L](s: var IrStore3, gs: var G, pass: CfPass[G, L]) =
  #mixin mergeFrom
  #mixin exec

  # XXX: see if using a diff-based approach to the thread state makes is more
  #      efficient. There's an unfinished attempt of this in `vmir.run`
  var states: seq[(L, bool)]
  var visited: seq[uint8] # visit state for each join point. used to abort loops
  visited.newSeq(s.len)
  var joinPoints: Table[JoinPoint, int] ## join point -> index into `states`
  var nextThreads: seq[IRIndex] # XXX: a circular buffer would make sense here

  var ls: L
  var stateIndex: int

  var cursor: IrCursor
  cursor.setup(s)

  func startThread(): int =
    for i, x in states.mpairs:
      if not x[1]:
        x[1] = true
        return i

  func queue(target: JoinPoint) =
    if visited[target] == 2:
      # each join point must only be visited a maximum amount of two times
      # TODO: wrong! This breaks apart for nested loops. Reset the
      #       visited counter of all nested join points back to zero on iteration
      return

    # TODO: use a binary search here
    var p = -1
    for j, t in nextThreads.pairs:
      if target >= t:
        p = j
        break

    if p == -1:
      nextThreads.add(target)
    elif target != nextThreads[p]:
      nextThreads.insert(target, p)

  states.newSeq(1)
  states[0] = (default(L), true)

  var i = 0
  while i < s.len:
    let n = s.at(i)
    cursor.setPos i
    case n.kind
    of ntkBranch:
      let target = n.target
      # execute the branch not taken case (i.e. fallthrough) first
      queue(target) # queue the branch-taken thread

      if target notin joinPoints:
        # TODO: reuse an unused entry in `states`
        states.add (states[stateIndex][0], true)
        joinPoints[target] = states.high
      else:
        let idx = joinPoints[target]
        pass.merge(states[idx][0], states[stateIndex][0])

    of ntkGoto:
      # TODO: check how loop ends fare
      let target = n.target
      # end of thread
      if target notin joinPoints:
        # re-use the previous thread for the new thread
        discard
        joinPoints[target] = stateIndex
      else:
        let idx = joinPoints[target]
        pass.merge(states[idx][0], states[stateIndex][0])

      # TODO: improve the next == `target` case

      queue(target)

      # goto join point
      let next = nextThreads[0]
      nextThreads.delete(0)
      stateIndex = joinPoints[next]

      i = s.position(next)
      assert s.at(i).kind == ntkJoin # TODO: move this to a separate validation pass

      inc visited[next]

      dec i

    else:
      #exec(gs, states[stateIndex], n)
      pass.visit gs, states[stateIndex][0], cursor, s, n

    inc i

  if pass.onEnd != nil:
    # TODO: what about the exceptional exit path?
    pass.onEnd(gs, states[joinPoints[0]][0])

  s.update(cursor)


# location and value lifetimes are different things. A location can live
# longer than it's value, but accessing a location past it's values' lifetime
# is illegal. A `ntkLocEnd` signifies the end-of-life of a location

# OUTDATED IDEA, mostly obsolete now (some things still apply)
# phase 1:
#   alias analysis
#   side-effect analysis
#   borrow-checking, cursor inference | (`ntkWeakAsgn` is resolved here)
# phase 2 (optional, only when arc/orc are used):
#   with the alias data
#   inject destructors:
#     replace `ntkAsgn:copy` and `ntkAsgn:sink` with calls to `=copy` and
#     `=sink` where applicable (i.e. for types that need it). insert `=destroy`
#     calls before `ntkLocEnd` where needed
#     when orc is enabled: insert the
#
# phase 3: proc inlining (if optimizations are enabled)
#   after all procs are inlined, rerun alias analysis, since we have more
#   context now
#
# phase 4: lower ntkUse, ntkConsume, and (some) ntkAsgn to ntkLoad and ntkWrite
#
# phase 5: VM code gen

type
  DestrCtx = object
    output: IrStore
  DestrLocal = object

  AliasCtx = object
  AliasesLocal = object

func mergeFrom(a: var AliasesLocal, b: AliasesLocal) =
  discard

# There are three similar but different concepts used here: ownership, reachability, and derived-ness
# If there exists a valid path expression from location `a` to `b`, `b` is reachable from `a`. Reachability is strongly linked with observability (for example, it is said that a function can observe all locations reachable from it's parameters and globals)
# If destroying location `a`'s value also always destroys location `b`'s value, `a` owns `b` (ownership)
# Derived currently means "part of the location's memory", which would mean that a location owned by a `seq` is not derived from the owner of the seq (and technically also not from the seq itself since it's basically a pointer/ref)

type AAEvalResult = enum
  asfNone
  asfRef # location is a `ref`
  asfHasRef # location contains a `ref`

func aliasAnalysisEval(c: AliasCtx, loc: LocIndex): AAEvalResult =
  discard

iterator fields(c: AliasCtx, loc: LocIndex): FieldId =
  ## iterates overs all fields relevant for the alias analysis
  discard

func setLoc(ls: AliasesLocal, id: NewNodeId, locId: ValueId) =
  discard

func setHandle(ls: AliasesLocal, id: NewNodeId, locId: ValueId) =
  discard

func setTarget(ls: AliasesLocal, a, locId: ValueId) =
  discard

func uid(c: AliasesLocal, l: LocIndex): ValueId =
  discard

func fieldId(c: AliasesLocal, l: LocIndex, f: FieldId): int =
  discard

func ownedBy(c: AliasesLocal, l: LocIndex): ValueId =
  discard


func computeAliases(c: var AliasCtx, ls: var AliasesLocal, n: NewNode) =
  case n.kind
  of ntkAsgn:
    # tricky case: up- and downconverting of both ref/ptr and object values
    case aliasAnalysisEval(c, n.srcLoc)
    of asfRef:
      ls.setTarget(ls.uid(n.loc), ls.uid(n.srcLoc))
      # the location is a ref/ptr
    of asfHasRef:
      # the location contains a ref/ptr
      for it in c.fields(n.loc):
        # XXX: only the fields which are directly accessed in the current
        #      work item (i.e. function or block) need to be included here
        ls.setTarget(ls.fieldId(n.loc, it), ls.fieldId(n.srcLoc, it))
    of asfNone:
      discard
  of ntkAddr:
    ls.setHandle(n.id, ls.uid(n.srcLoc))
  of ntkDeref:
    ls.setLoc(n.id, ls.uid(n.loc))
  of ntkCall:
    if n.isViewCall:
      # SPEC: the spec says the returned view is _derived_ from the first
      # argument, not only _reachable_. It should probably say that the returned handle needs to borrow from a location _owned_ by the first parameter
      # SPEC: the spec also says two different things in two different places:
      #   - "the [result] location must borrow from a location that is derived from the first parameter"
      #   - "[result] has to be a location derived from the first formal parameter or from a constant location."

      # XXX: we use the "result is _owned_ by first parameter" interpretation here for now

      # since we're not doing inter-procedure analysis, we don't know which location
      ls.setLoc(n.id, ls.ownedBy(ls.uid n.arg(0)))
  of ntkSym, ntkUse, ntkConsume, ntkLocEnd:
    discard "not relevant"
  of ntkWrite, ntkLoad:
    # too early, these shouldn't exist yet
    unreachable(n.kind)
  of ntkGoto, ntkBranch:
    unreachable(n.kind)
  else:
    {.warning: "handle this".}

type Ternary = enum
  No, Maybe, Yes

func aliveState(ls: DestrLocal, loc: LocIndex): Ternary =
  discard

func kill(ls: var DestrLocal, loc: LocIndex) =
  ## Mark the location and all locations owned by it's value as dead

func needsDestroy(c: DestrCtx, loc: LocIndex): bool =
  discard

func insertG*(c: var IrStore, kind: NewNodeKind, id: varargs[NewNodeId]): NewNodeId =
  discard


func insert*(c: var IrStore, kind: NewNodeKind, id: varargs[NewNodeId]) =
  discard

func keep(c: var IrStore) =
  discard

func makeDestroyHook(c: var DestrCtx, loc: LocIndex): int =
  discard

func mergeFrom(a: var DestrLocal, b: DestrLocal) =
  discard


iterator aliveStates(c: DestrLocal): (LocIndex, Ternary) =
  discard

func isParam(c: DestrCtx, loc: LocIndex): Ternary =
  discard

func loc(n: IrNode3): LocIndex =
  try:
    n.srcLoc
  except:
    n.wrLoc

# note: mostly outdated now
func computeDestructors(c: var DestrCtx, ls: var DestrLocal, cr: var IrCursor, ir: IrStore3, n: IrNode3) =
  ## Destructor computation. Also computes alive states. Basically the ``injectdestructors`` pass. Injects call to the destroy, copy and sink hooks for types that have them.

  proc testAlive() =
    case ls.aliveState(n.loc)
    of Yes: discard "all good"
    of Maybe:
      # automatic sinks or moves are only injected where it's safe (otherwise
      # a copy is used). If a location is maybe alive, it means that there's a
      # code-path where the value is dead (it was moved)
      # TODO: collect a warning?
      discard
    of No:
      # definitely a use-after-move
      # TODO: collect a warning
      discard

  case n.kind
  #of ntkAsgn:
  of ntkUse:
    testAlive()

  of ntkConsume:
    testAlive()
    # TODO: insert `wasMoved` call? Or use a special node kind?
    ls.kill(n.loc)

  of ntkAsgn:
    # SPEC: can an assign start a new value lifetime?
    discard

  of ntkLocEnd:
    if c.needsDestroy(n.loc):
      case ls.aliveState(n.loc)
      of Yes, Maybe:
        # TODO: use `ntkUse` instead? consume is currently used since event
        #       though a `=destroy` hook takes a `var` param, the modifications
        #       don't need to be visible
        let i = c.output.insertG(ntkConsume, n.loc)
        let i2 = c.output.insertG(ntkSym, c.makeDestroyHook(n.loc))
        c.output.insert(ntkCall, n.loc)
        c.output.keep() # keep the `ntkLocEnd` node
      of No:
        # XXX: if a location end is reached and the value is not alive anymore
        #      AND the dead value is not observable by anyone else (e.g. in
        #      the case of a local variable), the `wasMoved` after every
        #      consume on the same CF path as this end can be elided
        discard "nothing to do"

  of ntkCall, ntkAddr, ntkDeref, ntkSym:
    discard

  of ntkLoad, ntkWrite:
    unreachable(n.kind)
  of ntkGoto, ntkBranch:
    unreachable(n.kind)
  else:
    {.warning: "handle this".}

func computeDestructorsEnd(c: var DestrCtx, ls: DestrLocal) =
  # Check if a parameter was killed.
  for loc, x in ls.aliveStates:
    if x == Yes: continue

    case c.isParam(loc)
    of Yes:
      case x
      of Maybe: discard# a parameter's value is maybe dead
      of No: discard # a parameter's value is definitely dead
      of Yes: discard
    of Maybe:
      # the value of a location that's potentially reachable from a parameter is:
      case x
      of Maybe: discard # maybe dead
      of No: discard # dead
      of Yes: discard # still alive
    of No:
      discard "don't care"


func nthField(n: PNode, pos: int): PSym =
  case n.kind
  of nkSym:
    if n.sym.position == pos:
      result = n.sym
  of nkRecList:
    for it in n.sons:
      result = nthField(it, pos)
      if result != nil:
        return
  of nkRecCase:
    if n[0].sym.position == pos:
      return n[0].sym

    for i in 1..<n.len:
      result = nthField(n[i].lastSon, pos)
      if result != nil:
        return
  else:
    unreachable(n.kind)

# XXX: I'm very sure there exists a proc that does the same in the compiler
#      code already
func nthField(t: PType, pos: int): PSym =
  # TODO: also traverse base types
  assert t.kind == tyObject

  if t.n != nil:
    result = nthField(t.n, pos)

  if result == nil and t.len > 0 and t[0] != nil:
    result = nthField(t[0].skipTypes(skipPtrs), pos)

func computeTypes*(ir: IrStore3): seq[PType] =
  result.newSeq(ir.len)
  var i = 0
  for n in ir.nodes:
    case n.kind
    of ntkAsgn, ntkJoin, ntkGoto, ntkBranch, ntkContinue:
      discard
    of ntkCall:
      result[i] =
        if n.isBuiltIn:
          # XXX: built-in calls feel wrong. Using magics instead might be better
          n.typ
        else:
          let callee = ir.at(n.callee)
          if callee.kind != ntkSym:
            result[n.callee][0] # the callee's return type
          elif (let s = ir.sym(callee); s.typ != nil):
            s.typ[0]
          else:
            # the symbol for magics created with ``createMagic`` don't have
            # type information
            nil

    of ntkLit:
      result[i] = ir.getLit(n).typ
    of ntkSym:
      let s = ir.sym(n)
      customAssert s != nil, i
      if s.kind notin routineKinds:
        # don't compute the type for routine symbols. This makes it easier to
        # figure out the type dependencies later on.
        result[i] = s.typ
    of ntkUse, ntkConsume:
      result[i] = result[n.srcLoc]
    of ntkLocal:
      result[i] = ir.getLocal(i)[1]
    of ntkAddr:
      # XXX: completely wrong, but we're missing a way to get
      #      the correct type without creating a new one
      result[i] = result[n.addrLoc]
    of ntkDeref:
      let t = result[n.addrLoc].skipTypes(abstractInst)
      customAssert t.kind in {tyPtr, tyRef, tyVar, tyLent}, i
      result[i] = t.elemType
    of ntkPathObj:
      customAssert result[n.srcLoc] != nil, n.srcLoc
      let typ = result[n.srcLoc].skipTypes(abstractInst)
      let idx = n.fieldIdx
      case typ.kind
      of tyObject:
        let f = typ.nthField(n.fieldIdx)
        result[i] = f.typ
      of tyTuple:
        result[i] = typ[idx]
      else:
        customAssert false, n.srcLoc

    of ntkPathArr:
      result[i] = result[n.srcLoc].elemType()

    else:
      debugEcho "computeTypes missing: ", n.kind
    inc i

func getMagic(ir: IrStore3, n: IrNode3): TMagic =
  assert n.kind == ntkCall
  if n.isBuiltIn:
    mNone
  else:
    let callee = ir.at(n.callee)
    if callee.kind == ntkSym:
      ir.sym(callee).magic
    else:
      mNone

func insertLit(cr: var IrCursor, lit: string): IRIndex =
  cr.insertLit newStrNode(nkStrLit, lit)

func insertLit(cr: var IrCursor, i: int): IRIndex =
  cr.insertLit newIntNode(nkIntLit, i)

proc insertMagicCall(cr: var IrCursor, g: ModuleGraph, name: string, m: TMagic, args: varargs[IRIndex]): IRIndex {.discardable.} =
  cr.insertCallExpr(createMagic(g, g.idgen, name, m), args)

proc insertCompProcCall(cr: var IrCursor, g: ModuleGraph, name: string, args: varargs[IRIndex]): IRIndex {.discardable.} =
  cr.insertCallExpr(g.getCompilerProc(name), args)


type RefcPassCtx* = object
  graph: ModuleGraph
  idgen: IdGenerator
  types: seq[PType]

  # XXX: only used for the ``lowerSeqs`` passes, but `RefcPassCtx` is
  #      currently (ab)-used as the context for most passes
  localMap: Table[int, int] # old local-name -> new local-name

func setupRefcPass*(c: var RefcPassCtx, g: ModuleGraph, idgen: IdGenerator, ir: IrStore3) =
  c.types = computeTypes(ir) # XXX: very bad
  c.graph = g
  c.idgen = idgen

func typeof(c: RefcPassCtx, val: IRIndex): PType =
  customAssert c.types[val] != nil, val
  c.types[val]

type StorageLoc = enum
  slUnknown
  slStack
  slHeap
  # TODO: also add `slStatic`, used for constants?

func storageLoc(c: RefcPassCtx, val: IRIndex): StorageLoc =
  # TODO: missing
  slUnknown

proc requestRtti(c: var RefcPassCtx, cr: var IrCursor, t: PType): IRIndex =
  # refc uses the v1 type-info
  cr.insertCallExpr(createMagic(c.graph, c.idgen, "getTypeInfo", mGetTypeInfo), cr.insertLit(newNodeIT(nkType, unknownLineInfo, t))) # TODO: bad; don't create a new mGetTypeInfo sym every time
  # TODO: collect for which types rtti was requested

proc processMagicCall(c: var RefcPassCtx, cr: var IrCursor, ir: IrStore3, m: TMagic, n: IrNode3) =
  ## Lowers calls to various magics into calls to `compilerproc`s
  case getMagic(ir, n)
  of mDestroy:
    # An untransformed `mDestroy` indicates a ref or string. `seq`
    # destructors were lifted into specialized procs already
    let val = n.args(0)
    case c.typeof(val).kind
    of tyString:
      cr.replace()
      cr.insertCompProcCall(c.graph, "genericSeqAssign")
    of tyRef:
      # XXX: only non-injected destroys for refs should be turned
      cr.replace()
      let nilLit = cr.insertLit(newNode(nkNilLit))
      let r = c.storageLoc(val)
      case r
      of slStack:
        # if it's on the stack, we can simply assign 'nil'
        cr.insertAsgn(askShallow, val, nilLit)
      of slHeap:
        cr.insertCompProcCall(c.graph, "asgnRef", val, nilLit)
      of slUnknown:
        cr.insertCompProcCall(c.graph, "unsureAsgnRef", val, nilLit)
    else:
      discard

  of mNew:
    cr.replace()
    # TODO: alignment value missing
    let v = cr.insertCompProcCall(c.graph, "newObjRC1", c.requestRtti(cr, c.typeof(n.args(0))), cr.insertLit(0))
    # XXX: not sure about `askMove` here...
    cr.insertAsgn(askMove, n.args(0), v)

  else:
    discard "ignore"

proc genRefcRefAssign(cr: var IrCursor, g: ModuleGraph, dst, src: IRIndex, sl: StorageLoc) =
  # TODO: document
  case sl
  of slStack:
    cr.insertAsgn(askShallow, dst, src)
  of slHeap:
    cr.insertCompProcCall(g, "asgnRef", dst, src)
  of slUnknown:
    cr.insertCompProcCall(g, "unsureAsgnRef", dst, src)


proc applyRefcPass(c: var RefcPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  case n.kind
  of ntkAsgn:
    case n.asgnKind
    of askMove:
      if c.typeof(n.wrLoc).kind in {tyString, tyRef, tySequence}:
        genRefcRefAssign(cr, c.graph, n.wrLoc, n.srcLoc, c.storageLoc(n.wrLoc))
        # XXX: source needs to be zeroed?
    of askCopy:
      case c.typeof(n.wrLoc).kind
      of tyString:
        cr.replace()
        cr.insertCompProcCall(c.graph, "copyString", n.wrLoc, n.srcLoc)
      of tySequence:
        cr.replace()
        cr.insertCompProcCall(c.graph, "genericSeqAssign", n.wrLoc, n.srcLoc)
      else:
        discard
    of askInit, askShallow, askDiscr:
      # XXX: init might need special handling
      discard

  of ntkCall:
    processMagicCall(c, cr, ir, getMagic(ir, n), n)
  else:
    discard

type HookCtx* = object
  graph: ModuleGraph
  types: seq[PType]

func initHookCtx*(g: ModuleGraph, ir: IrStore3): HookCtx =
  HookCtx(graph: g, types: computeTypes(ir))

func hasAttachedOp*(c: HookCtx, op: TTypeAttachedOp, typ: PType): bool =
  assert typ != nil
  c.graph.getAttachedOp(typ, op) != nil


func typeof(c: HookCtx, n: IRIndex): PType =
  customAssert c.types[n] != nil, n
  c.types[n]

func injectHooks(c: HookCtx, n: IrNode3, cr: var IrCursor) =
  ## Replaces assignments and destroys with calls to the copy, sink, and destroy hooks.
  # TODO: rename. We're not injecting anything, just replacing
  case n.kind
  of ntkAsgn:
    let typ = c.typeof(n.wrLoc)
    case n.asgnKind
    of askInit:
      # TODO: missing
      discard
    of askMove:
      if hasAttachedOp(c, attachedSink, typ):
        cr.replace()
        cr.insertCallStmt(c.graph.getAttachedOp(typ, attachedSink), n.wrLoc, n.srcLoc)
    of askCopy:
      if hasAttachedOp(c, attachedAsgn, typ):
        cr.replace()
        cr.insertCallStmt(c.graph.getAttachedOp(typ, attachedAsgn), n.wrLoc, n.srcLoc)

    of askShallow, askDiscr:
      discard "nothing to do"

  of ntkCall:
    # XXX: the full IR (needed for magic lookup) is missing here
    #[
    if getMagic(irs, n) == mDestroy:
      if hasAttachedOp(c, attachedDestructor, c.typeof(n.args(0))):
        cr.replace()
        cr.insertCall(c.graph.getAttachedOp(typ, attachedDestructor), n.args(0))
    ]#
    discard
  else:
    discard

func insertError(cr: var IrCursor, err: string): IRIndex {.discardable.} =
  cr.insertCallExpr(bcError, nil, cr.insertLit err)


type GenericTransCtx = object
  graph: ModuleGraph
  types: seq[PType]

func setupTransCtx*(g: ModuleGraph, ir: IrStore3): GenericTransCtx =
  result.graph = g
  result.types = computeTypes(ir)

# XXX: the field position is not necessarily 2; the value should be detected
#      during compilation instead
const SeqDataFieldPos = 2

proc requestSeqType(c: var RefcPassCtx, t: PType): PType =
  ## `t` is the original ``tySequence`` type. The resulting type has the following definition:
  ##
  ## .. code:: nim
  ##   type NimSeq = ptr object of TGenericSeq
  ##     data: UncheckedArray[t.elemType]

  let cache = c.graph.cache

  # TODO: use a cache for the instantiations
  # XXX: yeah, this is bad; same as for symbols, it'd probably be a good idea to
  #      introduce a custom type representation for the whole compiler backend
  let
    typSym = newSym(skType, cache.getIdent("NimSeq"), c.idgen.nextSymId(), t.owner, t.owner.info)
    objTyp = newType(tyObject, c.idgen.nextTypeId(), t.owner)
    f =      newSym(skField, cache.getIdent("data"), c.idgen.nextSymId(), typSym, t.owner.info)

  f.position = SeqDataFieldPos
  f.typ = newType(tyUncheckedArray, nextTypeId c.idgen, t.owner)
  f.typ.add t.elemType

  objTyp.add(c.graph.getCompilerProc("TGenericSeq").typ) # base type
  objTyp.n = newTree(nkRecList, [newSymNode(f)])

  result = newType(tyPtr, c.idgen.nextTypeId(), t.owner)
  result.add(objTyp)
  result.linkTo(typSym)

proc lowerSeqsV1(c: var RefcPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Lowers the `seq`-related magic operations into calls to the v1 `seq`
  ## implementation
  case n.kind
  of ntkCall:
    case getMagic(ir, n)
    of mSetLengthStr:
      cr.replace()
      # TODO: is shallow correct here?
      cr.insertAsgn(askShallow, n.args(0), cr.insertCompProcCall(c.graph, "setLengthStr", n.args(0), n.args(1)))
    of mSetLengthSeq:
      cr.replace()
      # TODO: evaluation order might be violated here
      cr.insertAsgn(askShallow, n.args(0), cr.insertCompProcCall(c.graph, "setLengthSeqV2", n.args(0), c.requestRtti(cr, c.typeof(n.args(0))), n.args(1)))

    of mNewSeq:
      cr.replace()

      let val = n.args(0)
      let nilLit = cr.insertLit(newNode(nkNilLit))

      let sl = c.storageLoc(val)
      case sl
      of slHeap, slUnknown:
        # write barrier
        # TODO: document
        let target = cr.newJoinPoint()
        cr.insertBranch(cr.insertMagicCall(c.graph, "isNil", mIsNil), target)
        # TODO: use nimGCunrefNoCylce when applicable
        cr.insertCompProcCall(c.graph, "nimGCunrefRC1", val)
        cr.insertAsgn(askShallow, val, nilLit)
        cr.insertGoto(target)
        cr.insertJoin(target)

        var ns = cr.insertCompProcCall(c.graph, "newSeq", c.requestRtti(cr, c.typeof(val)), n.args(1))
        ns = cr.insertCast(c.typeof(val), ns)
        cr.insertAsgn(askShallow, val, ns)
      of slStack:

        var ns = cr.insertCompProcCall(c.graph, "newSeq", c.requestRtti(cr, c.typeof(val)), n.args(1))
        ns = cr.insertCast(c.typeof(val), ns)
        cr.insertAsgn(askShallow, val, ns)

    of mNewSeqOfCap:
      cr.replace()

      let val = cr.position
      discard cr.insertCast(c.typeof(val), cr.insertCompProcCall(c.graph, "nimNewSeqOfCap", c.requestRtti(cr, c.typeof(val)), n.args(0)))

    of mAppendSeqElem:
      # ``seq &= x`` is transformed into:
      #   ``seq = cast[typeof(seq)](incrSeqV3(seq, getTypeInfo(2)))``
      #   ``seq = ``
      cr.replace()
      let seqVal = n.args(0)
      let typ = c.typeof(seqVal).skipTypes({tyVar})

      # XXX: if the refc pass would be run after the `lowerSeqV1` pass, a
      #      `askMove` assignment could be used here instead
      cr.genRefcRefAssign(c.graph, seqVal, cr.insertCast(typ, cr.insertCompProcCall(c.graph, "incrSeqV3", seqVal, c.requestRtti(cr, typ)) ), c.storageLoc(seqVal))

      # TODO: filling the element and adjusting the seq length is missing
      discard cr.insertCallExpr(bcError, nil, cr.insertLit "Not implemented: lowerSeqsV1.mAppendSeqElem")

    of mAppendStrStr:
      cr.replace()
      var lens: array[2, IRIndex]
      #lens[0] = genIfThanElse() # we `len` call needs to be lowered directly
      discard cr.insertCallExpr(bcError, nil, cr.insertLit "Not implemented: lowerSeqsV1.mAppendStrStr")

    of mLengthStr:
      cr.replace()
      # XXX: might be a good idea to cache the `string` type
      let strTyp = c.graph.getCompilerProc("NimStringDesc")
      #genIfThanElse(cr.insertMagicCall("isNil", mIsNil, a.val))

      discard cr.insertCallExpr(bcError, nil, cr.insertLit "Not implemented: lowerSeqsV1.mLengthStr")

    else:
      discard

  of ntkLocal:
    # replace locals of `seq` and `string` type with locals of the lowered type
    # XXX: there's currently no way to replace an existing local (would be
    #      simpler and more efficient), so the logic here resorts to
    #      introducing a new local and replacing all reference to the old one
    let (lk, origTyp, sym) = ir.getLocal(cr.position)

    let typ = origTyp.skipTypes(abstractInst)

    # TODO: handle ``var`` and ``lent`` wrapped types here
    case typ.kind
    of tySequence:
      # replace seqs with `NimSeq`. The latter is bascially a generic type
      # that were instantiating manually here. The old C backend did this step
      # in the code-generator
      cr.replace()

      let idx = ir.getLocalIdx(cr.position)
      var newName = c.localMap.getOrDefault(idx, -1)
      if newName == -1:
        # XXX: ugly; the whole backend would probably benefit from it's own
        #      symbol representation
        let nt = c.requestSeqType(typ)
        if sym != nil:
          let ns = copySym(sym, c.idgen.nextSymId())
          ns.typ = nt

          newName = cr.newLocal(lk, ns)
        else:
          newName = cr.newLocal(lk, nt)

        c.localMap[idx] = newName

      discard cr.insertLocalRef(newName)

    of tyString:
      # replace `string` with `NimString`

      cr.replace()

      let idx = ir.getLocalIdx(cr.position)
      var newName = c.localMap.getOrDefault(idx, -1)
      if newName == -1:
        # XXX: ugly; the whole backend would probably benefit from it's own
        #      symbol representation

        let nt = c.graph.getCompilerProc("NimString").typ
        if sym != nil:
          let ns = copySym(sym, c.idgen.nextSymId())
          ns.typ = nt
          newName = cr.newLocal(lk, ns)
        else:
          newName = cr.newLocal(lk, nt)

        c.localMap[idx] = newName

      discard cr.insertLocalRef(newName)

    else:
      discard

  of ntkSym:
    # replace `string` and `seq` types of globals and parameters by directly
    # modifying the `PSym`s

    let sym = ir.sym(n)

    # XXX: the symbol patching here won't work out...
    if sym.kind notin {skVar, skLet, skParam}:
      # XXX: ignore constants for now
      return

    var newTyp: PType = nil

    let typ = sym.typ.skipTypes(abstractInst)
    let newType =
      case skipTypes(typ, {tyVar, tyLent}).kind
      of tySequence: c.requestSeqType(typ)
      of tyString:   c.graph.getCompilerProc("NimString").typ
      else: nil

    # this overwrites possibly present ``tyGenericInst``, ``tyDistinct``,
    # etc. but at this point in the backend, we no longer need those
    if newType != nil:
      if typ.kind == tyVar:
        # only ``var seq`` is treated as a pointer-to-pointer, not ``lent``
        sym.typ = newType(tyVar, nextTypeId c.idgen, typ.owner)
        sym.typ.add newType
      else:
        sym.typ = newType

  of ntkPathArr:
    let arrTyp = c.typeof(n.srcLoc).skipTypes(abstractInst)

    # TODO: needs tests
    case skipTypes(arrTyp, {tyVar, tyLent}).kind
    of tyString, tySequence:
      # --> x[].data[idx]

      cr.replace()
      var r = cr.insertDeref(n.srcLoc)
      # a `lent seq` is not a treated as a `ptr NimSeq` but just as `NimSeq`
      # (`NimSeq` itself is a pointer type)
      if arrTyp.kind == tyVar:
        r = cr.insertDeref(r)

      r = cr.insertPathObj(r, SeqDataFieldPos)
      discard cr.insertPathArr(r, n.arrIdx)

    else:
      discard

  else:
    discard "ignore"

func lowerSeqsV2(c: GenericTransCtx, n: IrNode3, cr: var IrCursor) =
  ## Lowers the `seq`-related magic operations into calls to the v2 `seq`
  ## implementation. Enabled by the `optSeqDestructors` toggle
  doAssert false, "missing"

type LiftPassCtx* = object
  graph*: ModuleGraph
  idgen*: IdGenerator
  cache*: IdentCache

  typeInfoMarker*: Table[SigHash, PSym] # sig hash -> type info sym

  syms*: seq[(PSym, PType)] ## all lifted globals

proc liftTypeInfoV1(c: var LiftPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Turns all ``mGetTypeInfo`` calls into globals and collects the newly
  ## created symbols
  # XXX: can this really be considered lifting?
  case n.kind
  of ntkCall:
    if getMagic(ir, n) == mGetTypeInfo:
      cr.replace()

      let
        typ = ir.getLit(ir.at(n.args(0))).typ
        sig = hashType(typ)

      assert typ != nil

      var s = c.typeInfoMarker.getOrDefault(sig)
      if s == nil:
        # TODO: either use a `Rope` here or use a string buffer stored in
        #       `LiftPassCtx` that is reserved for temporary usage like this
        let name = "NTI" & $sig & "_" # XXX: too many short-lived and unnecessary allocations

        # the symbol is owned by the module the type is owned by
        s = newSym(skVar, c.cache.getIdent(name), c.idgen.nextSymId(), typ.owner.getModule(), unknownLineInfo)
        # TODO: cache the `TNimType` type
        s.typ = c.graph.getCompilerProc("TNimType").typ
        s.flags.incl sfGlobal

        c.typeInfoMarker[sig] = s

      # TODO: cache the `pointer` type
      discard cr.insertCast(c.graph.getSysType(unknownLineInfo, tyPointer), cr.insertSym s)

  else:
    discard

const ErrFlagName = "nimError"

proc lowerTestError*(ir: var IrStore3, g: ModuleGraph, cache: IdentCache, idgen: IdGenerator, owner: PSym) =
  ## Lowers ``bcTestError`` builtin calls for the C-like targets. Turns
  ## ``bcTestError`` into ``unlikelyProc(ErrFlagName[])`` and inserts a
  ##
  ## .. code:: nim
  ##   let ErrFlagName = nimErrorFlag()
  ##
  ## at the top, but only if the error flag is actually accessed!
  # XXX: a `LinearPass` can't be used here, since we need to insert
  # XXX: "lower" is the wrong terminology here
  # XXX: maybe this should happen as part of ``irgen`` instead?

  var
    cr: IrCursor
    addedErr: bool
    errFlag: IRIndex

  cr.setup(ir)

  for i in 0..<ir.len:
    cr.setPos i
    let n = ir.at(i)
    case n.kind
    of ntkCall:
      # XXX: maybe introduce a bcNone and return that from `n.builtin` in case
      #      the call is no builtin call? Would simplify some callsites
      if n.isBuiltIn and n.builtin == bcTestError:
        if not addedErr:
          addedErr = true
          # since modifications have to happen in increasing order, we have
          # to first jump back and insert the error flag
          cr.setPos 0
          let
            p = g.getCompilerProc("nimErrorFlag")
            s = newSym(skLet, cache.getIdent(ErrFlagName), idgen.nextSymId(), owner, unknownLineInfo)

          s.typ = p.getReturnType()

          errFlag = cr.insertLocalRef(cr.newLocal(lkLet, s))
          cr.insertAsgn(askInit, errFlag, cr.insertCallExpr(p))

          cr.setPos i # set cursor back to the current position

        cr.replace()
        var up: PSym
        # TODO: `systemModuleSyms` only picks up on the 'unlikelyProc' if it's exported...
        for x in g.systemModuleSyms(cache.getIdent("unlikelyProc")):
          up = x
          break
        assert up != nil

        discard cr.insertCallExpr(up, cr.insertDeref(errFlag))

    else:
      discard

  ir.update(cr)

proc lowerSets*(c: var RefcPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Lowers ``set`` operations into bit operations. Intended for the C-like targets
  # XXX: ideally we'd also lower the ``set`` types used for locals and parameters
  # XXX: some set lowerings could be simplified by adding them as
  #      compiler-procs in ``system.nim`` and then doing something an
  #      `cr.inline` here
  case n.kind
  of ntkCall:
    case getMagic(ir, n)
    of mInSet:
      cr.replace()

      let setType = skipTypes(c.typeof(cr.position), abstractVar)
      let size = getSize(c.graph.config, setType).int
      case size
      of 1, 2, 4, 8:
        # small sets
        cr.insertError("mInSet for small sets missing")
      else:
        let uintTyp = c.graph.getSysType(unknownLineInfo, tyUInt)
        let conv = cr.insertConv(uintTyp, n.args(1))
        cr.insertMagicCall(c.graph, "and", mBitandI, cr.insertMagicCall(c.graph, "shr", mShrI, conv, cr.insertLit 3), cr.insertMagicCall(c.graph, "shl", mShlI, cr.insertLit 1, cr.insertMagicCall(c.graph, "and", mBitandI, conv, cr.insertLit 7)))
        # TODO: unfinished
        #binaryExprIn(p, e, a, b, d, "(($1[(NU)($2)>>3] &(1U<<((NU)($2)&7U)))!=0)")

    else:
      discard
  else:
    discard


proc lowerRangeChecks*(c: var RefcPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Lowers ``bcRangeCheck`` (nkChckRange, nkChckRangeF, etc.) into simple comparisons
  # XXX: the lowering could be simplified by just replacing the range check
  #      with a call to a ``chkRange`` inline function that'd be defined in
  #      ``system.nim`` for the C-like targets

  case n.kind
  of ntkCall:
    if n.isBuiltIn and n.builtin == bcRangeCheck:
      let srcTyp = c.typeof(n.args(0))

      cr.replace()
      var cond: IRIndex
      var raiser: string

      case srcTyp.kind
      of tyUInt, tyUInt64:
        # .. code:: nim
        #   cast[dstTyp](high) < val
        cond = cr.insertMagicCall(c.graph, "<", mLtU, cr.insertCast(srcTyp, n.args(0)), n.args(2))
        raiser = "raiseRangeErrorNoArgs"
      else:
        let dstTyp = skipTypes(c.typeof(cr.position), abstractVarRange)
        case dstTyp.kind
        of tyUInt8..tyUInt32, tyChar:
          raiser = "raiseRangeErrorU"
        of tyFloat..tyFloat128:
          raiser = "raiseRangeErrorF"
          let conv = cr.insertConv(dstTyp, n.args(0))
          # no need to lower the `or` into an `ntkBranch` + `ntkJoin` here; it has no impact on further analysis
          cond = cr.insertMagicCall(c.graph, "or", mOr, cr.insertMagicCall(c.graph, "<", mLtF64, conv, n.args(1)), cr.insertMagicCall(c.graph, "<", mLtF64, n.args(2), conv))

        else:
          cr.insertError("missing chkRange impl")

        raiser =
          case skipTypes(c.typeof(cr.position), abstractVarRange).kind
          of tyFloat..tyFloat128: "raiseRangeErrorF"
          else: "raiseRangeErrorI"

        #[
        let boundaryCast =
          if n0t.skipTypes(abstractVarRange).kind in {tyUInt, tyUInt32, tyUInt64} or
              (n0t.sym != nil and sfSystemModule in n0t.sym.owner.flags and n0t.sym.name.s == "csize"):
            "(NI64)"
          else:
            ""
            ]#

        let target = cr.newJoinPoint()
        cr.insertBranch(cr.insertMagicCall(c.graph, "not", mNot, cond), target)
        cr.insertCompProcCall(c.graph, raiser, n.args(0), n.args(1), n.args(2))
        # XXX: it would be nice if we could also move the following
        #      ``if bcTestError(): goto error`` into the branch here

        cr.insertJoin(target)
        discard cr.insertConv(dstTyp, n.args(0))

  else:
    discard

const hookPass* = LinearPass[HookCtx](visit: injectHooks)
const refcPass* = LinearPass2[RefcPassCtx](visit: applyRefcPass)
const seqV1Pass* = LinearPass2[RefcPassCtx](visit: lowerSeqsV1)
const seqV2Pass* = LinearPass[GenericTransCtx](visit: lowerSeqsV2)
const typeV1Pass* = LinearPass2[LiftPassCtx](visit: liftTypeInfoV1)
const lowerRangeCheckPass* = LinearPass2[RefcPassCtx](visit: lowerRangeChecks)
const lowerSetsPass* = LinearPass2[RefcPassCtx](visit: lowerSets)