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
    nimsets, # for ``toBitSet``
    types
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/sem/[
    sighashes
  ],
  compiler/utils/bitsets,
  compiler/vm/[
    irdbg,
    pass_helpers,
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
      visit*: proc (env: var T, x: IrNode3, ir: IrStore3, c: var IrCursor)

type CfPass*[G; L] = object
  ## Static-control-flow based pass
  visit: proc (gs: var G, ls: var L, c: var IrCursor, irs: IrStore3, n: IrNode3)
  merge: proc (a: var L, b: L)
  onEnd: proc (gs: var G, ls: L)

# TODO: rename
template customAssert(cond: bool, node: IRIndex) =
  if not cond:
    raise (ref PassError)(msg: astToStr(cond), n: node)

type PassEnv* = ref object # XXX: will be a non-`ref` later on
  compilerprocs*: Table[string, ProcId]
  compilertypes*: Table[string, TypeId]

  attachedOps*: array[TTypeAttachedOp, Table[TypeId, ProcId]]

  sysTypes*: array[TTypeKind, TypeId]

func getCompilerProc*(g: PassEnv, name: string): ProcId =
  g.compilerprocs[name]

func getCompilerType*(g: PassEnv, name: string): TypeId =
  g.compilertypes[name]

func getSysType*(g: PassEnv, kind: TTypeKind): TypeId =
  g.sysTypes[kind]

proc runPass*[T](irs: var IrStore3, ctx: T, pass: LinearPass[T]) =
  var cursor: IrCursor
  cursor.setup(irs)

  var i = 0
  for n in irs.nodes:
    cursor.setPos(i)
    pass.visit(ctx, n, cursor)
    inc i

  irs.update(cursor)

proc runPass*[T](irs: var IrStore3, ctx: var T, pass: LinearPass2[T]) =
  var cursor: IrCursor
  cursor.setup(irs)

  var i = 0
  for n in irs.nodes:
    cursor.setPos(i)
    pass.visit(ctx, n, irs, cursor)
    inc i

  irs.update(cursor)


proc runPass2*[T](irs: IrStore3, diff: var Changes, ctx: var T, pass: LinearPass2[T]) =
  ## Applies `pass` to the given `irs` and adds changes to `diff`
  var cr: IrCursor
  cr.setup(irs)

  var i = 0
  for n in irs.nodes:
    cr.setPos(i)
    pass.visit(ctx, n, irs, cr)
    inc i

  diff.merge(cr)

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

type
  UntypedPassCtx* = object
    ## Pass context for passes that neither need typed IR nor other extra data
    graph*: PassEnv
    env*: ptr IrEnv

func initUntypedCtx*(graph: PassEnv, env: ptr IrEnv): UntypedPassCtx =
  result = UntypedPassCtx(graph: graph, env: env)

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


func computeTypes*(ir: IrStore3, env: IrEnv): seq[TypeId] =
  result.newSeq(ir.len)
  var i = 0
  for n in ir.nodes:
    case n.kind
    of ntkAsgn, ntkJoin, ntkGoto, ntkBranch, ntkContinue, ntkProc:
      discard
    of ntkCall:
      result[i] =
        case n.callKind
        of ckBuiltin, ckMagic:
          # XXX: built-in calls feel wrong. Using magics instead might be better
          n.typ
        of ckNormal:
          let callee = ir.at(n.callee)
          if callee.kind != ntkProc:
            env.types.getReturnType(result[n.callee]) # the callee's return type
          else:
            env.procs.getReturnType(callee.procId)

    of ntkLit:
      result[i] = ir.getLit(n).typ
    of ntkSym:
      let s = ir.sym(n)
      customAssert s != NoneSymbol, i
      result[i] = env.syms[s].typ
    of ntkParam:
      result[i] = env.procs.param(ir.owner, n.paramIndex).typ
    of ntkUse, ntkConsume:
      result[i] = result[n.srcLoc]
    of ntkLocal:
      result[i] = ir.getLocal(i).typ
    of ntkAddr:
      # XXX: completely wrong, but we're missing a way to get
      #      the correct type without creating a new one
      result[i] = result[n.addrLoc]
    of ntkDeref:
      let t = result[n.addrLoc]
      customAssert env.types[t].kind in {tnkPtr, tnkRef, tnkVar, tnkLent}, i
      result[i] = env.types.elemType(t)
    of ntkPathObj:
      customAssert result[n.srcLoc] != NoneType, n.srcLoc
      let typ = result[n.srcLoc]
      let idx = n.fieldIdx
      case env.types[typ].kind
      of tnkRecord:
        let f = env.types.nthField(typ, n.fieldIdx)
        result[i] = env.types[f].typ
      else:
        customAssert false, n.srcLoc

    of ntkPathArr:
      let typ = env.types.skipVarOrLent(result[n.srcLoc])
      result[i] = env.types.elemType(typ)

    of ntkConv, ntkCast:
      result[i] = n.typ

    else:
      debugEcho "computeTypes missing: ", n.kind
    inc i

template binaryBoolOp*(cr: var IrCursor, g: PassEnv, op: TMagic; a, b: IRIndex): IRIndex =
  cr.insertCallExpr(op, g.sysTypes[tyBool], a, b)

proc insertMagicCall*(cr: var IrCursor, g: PassEnv, m: TMagic, t: TTypeKind, args: varargs[IRIndex]): IRIndex {.discardable.} =
  cr.insertCallExpr(m, g.sysTypes[t], args)

proc insertCompProcCall*(cr: var IrCursor, g: PassEnv, name: string, args: varargs[IRIndex]): IRIndex {.discardable.} =
  cr.insertCallExpr(g.compilerprocs[name], args)

func insertLoop(cr: var IrCursor): JoinPoint

# TODO: move to ``pass_helpers``
template genForLoop*(cr: var IrCursor, d: var LiteralData, g: PassEnv, len: IRIndex, body: untyped) =
  ## Generates a for-loop with counting from '0' till the given `len` - 1. The
  ## counter is accessible in from `body` via ``counter`` and the loop-exit
  ## point via ``loopExit``
  block:
    let
      lenVal = len
      counter {.inject.} = cr.insertLocalRef(cr.newLocal(lkTemp, g.sysTypes[tyInt]))
      loopExit {.inject.} = cr.newJoinPoint()
      loop = insertLoop(cr)

    # loop condition
    cr.genIfNot(cr.insertCallExpr(mLeI, g.sysTypes[tyBool], counter, lenVal)):
      cr.insertGoto(loopExit)

    body

    # increment the counter
    cr.insertAsgn(askCopy, counter, cr.insertMagicCall(g, mAddI, tyInt, counter, cr.insertLit(d, 1)))
    cr.insertGoto(loop)

    cr.insertJoin(loopExit)

type TypedPassCtx* = object
  ## General context object for passes that require typed IR
  graph*: PassEnv
  env*: ptr IrEnv

  types*: seq[TypeId]

func init*(c: var TypedPassCtx, g: PassEnv, env: ptr IrEnv, ir: IrStore3) =
  c.graph = g
  c.env = env
  c.types = computeTypes(ir, env[])

type RefcPassCtx* = object
  graph: ModuleGraph
  idgen: IdGenerator

  extra: PassEnv

  env: ptr IrEnv # XXX: in order to get to something working, a `ptr` for now
  types: seq[TypeId]

  # XXX: only used for the ``lowerSeqs`` passes, but `RefcPassCtx` is
  #      currently (ab)-used as the context for most passes
  localMap: Table[int, int] # old local-name -> new local-name

func setupRefcPass*(c: var RefcPassCtx, pe: PassEnv, env: ptr IrEnv, g: ModuleGraph, idgen: IdGenerator, ir: IrStore3) =
  c.types = computeTypes(ir, env[]) # XXX: very bad
  c.graph = g
  c.idgen = idgen
  c.extra = pe
  c.env = env

func typeof(c: RefcPassCtx, val: IRIndex): TypeId =
  customAssert c.types[val] != NoneType, val
  c.types[val]

func typeKindOf(c: RefcPassCtx, val: IRIndex): TypeNodeKind =
  customAssert c.types[val] != NoneType, val
  c.env.types[c.types[val]].kind

type StorageLoc = enum
  slUnknown
  slStack
  slHeap
  # TODO: also add `slStatic`, used for constants?

func storageLoc(c: RefcPassCtx, val: IRIndex): StorageLoc =
  # TODO: missing
  slUnknown

proc requestRtti(c: var RefcPassCtx, cr: var IrCursor, t: TypeId): IRIndex =
  # refc uses the v1 type-info
  cr.insertAddr cr.insertCallExpr(mGetTypeInfo, c.extra.getCompilerType("TNimType"), cr.insertTypeLit(t))
  # TODO: collect for which types rtti was requested

func requestRtti2(g: PassEnv, cr: var IrCursor, t: TypeId): IRIndex =
  # refc uses the v1 type-info
  cr.insertAddr cr.insertCallExpr(mGetTypeInfo, g.getCompilerType("TNimType"), cr.insertTypeLit(t))

proc genRefcRefAssign(cr: var IrCursor, e: PassEnv, dst, src: IRIndex, sl: StorageLoc)

proc genNewObj(cr: var IrCursor, g: PassEnv, env: IrEnv, ptrTyp: TypeId,
               dest, sizeExpr: IRIndex; loc: StorageLoc) =
  let
    typ = env.types[ptrTyp].base
    rttiExpr = g.requestRtti2(cr, typ)

  let sizeExpr =
    if sizeExpr == InvalidIndex:
      cr.insertMagicCall(g, mSizeOf, tyInt, cr.insertTypeLit(typ))
    else:
      sizeExpr


  case loc
  of slHeap:
    let v = cr.insertCompProcCall(g, "newObjRC1", rttiExpr, sizeExpr)
    # XXX: not sure about `askMove` here...
    cr.insertAsgn(askMove, dest, cr.insertCast(ptrTyp, v))
  of slStack, slUnknown:
    let v = cr.insertCompProcCall(g, "newObj", rttiExpr, sizeExpr)
    genRefcRefAssign(cr, g, dest, v, loc)

proc processMagicCall(c: var RefcPassCtx, cr: var IrCursor, ir: IrStore3, m: TMagic, n: IrNode3) =
  ## Lowers calls to various magics into calls to `compilerproc`s
  template arg(i: Natural): IRIndex =
    ir.args(cr.position, i)

  case getMagic(ir, c.env[], n)
  of mDestroy:
    # An untransformed `mDestroy` indicates a ref or string. `seq`
    # destructors were lifted into specialized procs already
    let val = arg(0)
    case c.env.types[c.typeof(val)].kind
    of tnkRef, tnkString:
      # XXX: only non-injected destroys for refs should be turned
      cr.replace()
      let nilLit = cr.insertNilLit(c.env.data, c.typeof(val))
      let r = c.storageLoc(val)
      case r
      of slStack:
        # if it's on the stack, we can simply assign 'nil'
        cr.insertAsgn(askShallow, val, nilLit)
      of slHeap:
        cr.insertCompProcCall(c.extra, "asgnRef", val, nilLit)
      of slUnknown:
        cr.insertCompProcCall(c.extra, "unsureAsgnRef", val, nilLit)
    else:
      discard

  of mNew:
    let
      arg = arg(0)
      ptrTyp = c.env.types.skipVarOrLent(c.typeof(arg))
      # ``unsafeNew`` also uses the ``mNew`` magic, so we have to handle
      # that here
      size = if n.argCount > 1: arg(1) else: InvalidIndex

    cr.replace()
    # XXX: not sure about `askMove` here...
    genNewObj(cr, c.extra, c.env[], ptrTyp, arg, size, c.storageLoc(arg))

  of mGCref, mGCunref:
    const op = [mGCref:   "nimGCref",
                mGCunref: "nimGCunref"]

    cr.replace()
    genIfNot(cr, cr.insertMagicCall(c.extra, mIsNil, tyBool, arg(0))):
      cr.insertCompProcCall(c.extra, op[m], arg(0))

  of mDefault:
    # XXX: temporary implementation

    cr.replace()
    let typ = ir.getLit(ir.at(arg(0)))[1]

    case c.env.types[typ].kind
    of tnkBool, tnkChar, tnkInt, tnkUInt:
      # TODO: bool and char need their own literal
      discard cr.insertLit(c.env.data, 0, typ)
    of tnkFloat:
      discard cr.insertLit(c.env.data, 0.0, typ)
    of tnkPtr, tnkRef, tnkProc:
      discard cr.insertNilLit(c.env.data, typ)
    else:
      # a complex type
      type TypeHeaderKind = enum
        none, single, embedded
      var hdr = embedded # XXX: wrong, obviously

      let
        tmp = cr.newLocal(lkTemp, typ)
        tmpAcc = cr.insertLocalRef(tmp)

      # XXX: should IR temporaries be specified to be zero-initialized by
      #      default? Sounds like a good idea
      cr.insertCompProcCall(c.extra, "nimZeroMem", cr.insertAddr tmpAcc, cr.insertMagicCall(c.extra, mSizeOf, tyInt, cr.insertTypeLit(typ)))
      case hdr
      of none: discard
      of single:
        # XXX: ``mAccessTypeField`` returns a ``ptr TNimType``, but we don't
        #      have access to that type here
        cr.insertAsgn(askInit, cr.insertMagicCall(c.extra, mAccessTypeField, tyPointer, tmpAcc), c.requestRtti(cr, typ))
      of embedded:
        cr.insertCompProcCall(c.extra, "objectInit", cr.insertAddr tmpAcc, c.requestRtti(cr, typ))

      discard cr.insertLocalRef(tmp)

  else:
    discard "ignore"

proc genRefcRefAssign(cr: var IrCursor, e: PassEnv, dst, src: IRIndex, sl: StorageLoc) =
  # TODO: document
  case sl
  of slStack:
    cr.insertAsgn(askShallow, dst, src)
  of slHeap:
    cr.insertCompProcCall(e, "asgnRef", dst, src)
  of slUnknown:
    cr.insertCompProcCall(e, "unsureAsgnRef", dst, src)


proc applyRefcPass(c: var RefcPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  case n.kind
  of ntkAsgn:
    case n.asgnKind
    of askMove:
      if c.typeKindOf(n.wrLoc) in {tnkString, tnkRef, tnkSeq}:
        genRefcRefAssign(cr, c.extra, n.wrLoc, n.srcLoc, c.storageLoc(n.wrLoc))
        # XXX: source needs to be zeroed?
    of askCopy:
      case c.typeKindOf(n.wrLoc)
      of tnkString:
        cr.replace()
        # TODO: this is only correct for strings on the stack
        cr.insertAsgn(askShallow, n.wrLoc, cr.insertCompProcCall(c.extra, "copyString", n.srcLoc))
      of tnkSeq:
        cr.replace()
        cr.insertCompProcCall(c.extra, "genericSeqAssign", n.wrLoc, n.srcLoc, c.requestRtti(cr, c.typeof(n.wrLoc)))
      else:
        discard
    of askInit, askShallow, askDiscr:
      # XXX: init might need special handling
      discard

  of ntkCall:
    processMagicCall(c, cr, ir, getMagic(ir, c.env[], n), n)
  else:
    discard

type HookCtx* = object
  graph: PassEnv
  env: ptr IrEnv
  types: seq[TypeId]

func initHookCtx*(g: PassEnv, ir: IrStore3, env: IrEnv): HookCtx =
  HookCtx(graph: g, types: computeTypes(ir, env))

func hasAttachedOp*(c: HookCtx, op: TTypeAttachedOp, typ: TypeId): bool =
  assert typ != NoneType
  typ in c.graph.attachedOps[op]

func getAttachedOp(c: HookCtx, op: TTypeAttachedOp, typ: TypeId): ProcId =
  assert typ != NoneType
  c.graph.attachedOps[op][typ]

func typeof(c: HookCtx, n: IRIndex): TypeId =
  customAssert c.types[n] != NoneType, n
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
        cr.insertCallStmt(c.getAttachedOp(attachedSink, typ), n.wrLoc, n.srcLoc)
    of askCopy:
      if hasAttachedOp(c, attachedAsgn, typ):
        cr.replace()
        cr.insertCallStmt(c.getAttachedOp(attachedAsgn, typ), n.wrLoc, n.srcLoc)

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

type GenericTransCtx = object
  graph: ModuleGraph
  types: seq[TypeId]

func setupTransCtx*(g: ModuleGraph, ir: IrStore3, env: IrEnv): GenericTransCtx =
  result.graph = g
  result.types = computeTypes(ir, env)

proc genTernaryIf(cr: var IrCursor, g: PassEnv, asgn: AssignKind, cond, dest, a, b: IRIndex) =
  # if cond:
  #   dest = a
  # else:
  #   dest = b

  let
    elseP = cr.newJoinPoint()
    endP = cr.newJoinPoint()

  cr.insertBranch(cr.insertMagicCall(g, mNot, tyBool, cond), elseP)
  cr.insertAsgn(asgn, dest, a)
  cr.insertGoto(endP)

  cr.insertJoin(elseP)
  cr.insertAsgn(asgn, dest, b)
  cr.insertGoto(endP)

  cr.insertJoin(endP)

const SeqV1LenField = 0
# XXX: the field position is not necessarily 2; the value should be detected
#      during compilation instead
const SeqV1DataField = 2 # TODO: depends on the compiler flags

proc isConst(ir: IrStore3, env: IrEnv, n: IRIndex): bool =
  ## Returns if `n` refers to a ``const``
  ir.at(n).kind == ntkSym and env.syms[ir.sym(ir.at(n))].kind == skConst

proc isLiteral(ir: IrStore3, n: IRIndex): bool =
  ## Returns if `n` is a literal value
  ir.at(n).kind == ntkLit

proc accessSeq(cr: var IrCursor, ir: IrStore3, env: IrEnv, n: IRIndex, typ: TypeId): IRIndex =
  assert env.types[typ].kind in { tnkSeq, tnkString }
  # XXX: as an alternative to handling this here, we could introduce a
  #      ``bcLoadConst``, which would allow us to move the logic here
  #      to a different pass
  # XXX: yet another one would be to introduce a ``bcAccessSeq``

  if false: #isLiteral(ir, n):
    # seq/string literals are turned into constant seqs/strings. These are of
    # specialized record type (not using an ``tnkUncheckedArray``), so we
    # first have to take the address and then cast
    cr.insertCast(typ, cr.insertAddr(n))

  else:
    # direct access
    n

proc accessSeqField(cr: var IrCursor, ir: IrStore3, src: IRIndex, f: int): IRIndex =
  # a const is stores the underlying objec type directly, but other seqs are pointer types
  let obj =
    if false: src #isLiteral(ir, src): src
    else:                  cr.insertDeref(src)

  cr.insertPathObj(obj, f.int16)

proc genSeqLen(cr: var IrCursor, d: var LiteralData, g: PassEnv, ir: IrStore3, src: IRIndex): IRIndex =
  ## Generates a ``len`` getter expression for a V1 seq-like value
  # result = if src.isNil: 0 else: src[].len

  # idea: instead of manually inserting these predefined sequences, use a
  #       template-like mechanism where the IR-node sequence is pre-generated
  #       and then expanded here. The logic could be directly integrated into
  #       the ``IrCursor`` API so that the expansion of templates only happens
  #       when calling ``IrCursor.update``

  let local = cr.newLocal(lkTemp, g.sysTypes[tyInt])
  let tmp   = cr.insertLocalRef(local)

  if false:#isLiteral(ir, src):
    cr.insertAsgn(askInit, tmp, accessSeqField(cr, ir, src, SeqV1LenField))
  else:
    let cond = cr.insertMagicCall(g, mIsNil, tyBool, src)
    genTernaryIf(cr, g, askInit, cond, tmp, cr.insertLit(d, 0.int), cr.insertPathObj(cr.insertDeref(src), SeqV1LenField))

  result = cr.insertLocalRef(local)

func genSeqAt(cr: var IrCursor, g: PassEnv, ir: IrStore3, src, idx: IRIndex): IRIndex =
  cr.insertPathArr(accessSeqField(cr, ir, src, SeqV1DataField), idx)

proc genStrConcat(cr: var IrCursor, g: PassEnv, tm: seq[TypeId], ir: IrStore3, env: var IrEnv, n: IRIndex): IRIndex =
  # Input:
  #   s = "Abc" & "def" & str & 'g'
  # Transformed:
  #   var tmp = rawNewString(str.len + 7)
  #   appendString(tmp, "Abc")
  #   appendString(tmp, "def")
  #   appendString(tmp, name)
  #   appendChar(tmp, 'g')

  # XXX: with the introduction of the literal IR, the `env` parameter was
  #      changed to be mutable - but that is much too broad! Only the literal
  #      data is mutated

  var staticLen = 0
  var lenExpr = InvalidIndex

  for arg in ir.args(n):
    case env.types[tm[arg]].kind
    of tnkChar:
      inc staticLen
    of tnkString:
      if ir.at(arg).kind == ntkLit:
        staticLen += env.data.getStr(getLit(ir, ir.at(arg)).val).len
      else:
        let v = genSeqLen(cr, env.data, g, ir, arg)
        lenExpr =
          if lenExpr == InvalidIndex:
            v
          else:
            cr.insertMagicCall(g, mAddI, tyInt, lenExpr, v)

    else: unreachable()

  let staticLenExpr = cr.insertLit(env.data, staticLen)
  lenExpr =
    if lenExpr == InvalidIndex:
      staticLenExpr
    else:
      cr.insertMagicCall(g, mAddI, tyInt, lenExpr, staticLenExpr)

  let tmp = cr.newLocal(lkTemp, g.getCompilerType("NimString"))
  cr.insertAsgn(askInit, cr.insertLocalRef(tmp), cr.insertCompProcCall(g, "rawNewString", lenExpr))

  for arg in ir.args(n):
    # BUG: the discard is necessary for the compiler not to crash with an NPE
    case env.types[tm[arg]].kind
    of tnkChar:   discard cr.insertCompProcCall(g, "appendChar", cr.insertLocalRef(tmp), arg)
    of tnkString: discard cr.insertCompProcCall(g, "appendString", cr.insertLocalRef(tmp), arg)
    else: unreachable()

  result = cr.insertLocalRef(tmp)

proc lowerSeqsV1(c: var RefcPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Lowers the `seq`-related magic operations into calls to the v1 `seq`
  ## implementation
  case n.kind
  of ntkCall:
    template arg(i: Natural): IRIndex =
      ir.args(cr.position, i)

    case getMagic(ir, c.env[], n)
    of mSetLengthStr:
      cr.replace()
      # TODO: is shallow correct here?
      cr.insertAsgn(askShallow, arg(0), cr.insertCompProcCall(c.extra, "setLengthStr", arg(0), arg(1)))
    of mSetLengthSeq:
      cr.replace()
      # TODO: evaluation order might be violated here
      cr.insertAsgn(askShallow, arg(0), cr.insertCompProcCall(c.extra, "setLengthSeqV2", arg(0), arg(1), c.requestRtti(cr, c.typeof(arg(0)))))

    of mNewSeq:
      cr.replace()

      let val = arg(0)
      let nilLit = cr.insertNilLit(c.env.data, c.typeof(val))

      let sl = c.storageLoc(val)
      case sl
      of slHeap, slUnknown:
        # write barrier
        # TODO: document
        let target = cr.newJoinPoint()
        cr.insertBranch(cr.insertMagicCall(c.extra, mIsNil, tyBool, val), target)
        # TODO: use nimGCunrefNoCylce when applicable
        cr.insertCompProcCall(c.extra, "nimGCunrefRC1", val)
        cr.insertAsgn(askShallow, val, nilLit)
        cr.insertGoto(target)
        cr.insertJoin(target)

        var ns = cr.insertCompProcCall(c.extra, "newSeq", c.requestRtti(cr, c.typeof(val)), arg(1))
        ns = cr.insertCast(c.typeof(val), ns)
        cr.insertAsgn(askShallow, val, ns)
      of slStack:

        var ns = cr.insertCompProcCall(c.extra, "newSeq", c.requestRtti(cr, c.typeof(val)), arg(1))
        ns = cr.insertCast(c.typeof(val), ns)
        cr.insertAsgn(askShallow, val, ns)

    of mNewSeqOfCap:
      cr.replace()

      let val = cr.position
      discard cr.insertCast(c.typeof(val), cr.insertCompProcCall(c.extra, "nimNewSeqOfCap", c.requestRtti(cr, c.typeof(val)), arg(0)))

    of mArrToSeq:
      # XXX: passing a non-constant array construction expression as the
      #      argument will produce rather inefficient code, but we can't do
      #      much about it here because of how an ``nkBracket`` is lowered
      #      during ``irgen``
      cr.replace()

      let
        seqTyp = c.typeof(cr.position)
        arrTyp = c.typeof(arg(0))

      let
        elemCount = cr.insertLit(c.env.data, c.env.types.length(arrTyp))
        tmp = cr.newLocal(lkTemp, c.typeof(cr.position))

      # TODO: this transformation shoud likely happen in a pass before
      #       seqs are lowered (maybe in ``irgen``)

      # XXX: if we'd know about the destination location, we could use
      #      ``newSeqRC1`` if the destination is on the heap

      # construct the seq
      cr.insertAsgn(askInit, cr.insertLocalRef(tmp), cr.insertCast(seqTyp, cr.insertCompProcCall(c.extra, "newSeq", c.requestRtti(cr, seqTyp), elemCount)))

      # TODO: don't emit a loop if the source array is empty
      # TODO: maybe add back the small loop unrolling?
      cr.genForLoop(c.env.data, c.extra, elemCount):
        cr.insertAsgn(askInit, cr.genSeqAt(c.extra, ir, cr.insertLocalRef(tmp), counter), cr.insertPathArr(arg(0), counter))

      discard cr.insertLocalRef(tmp)

    of mAppendSeqElem:
      # ``seq &= x`` -->:
      #   seq = cast[typeof(seq)](incrSeqV3(seq, getTypeInfo(2)))``
      #   let tmp = seq[].len
      #   inc seq[].len
      #   seq[].data[tmp] = move x
      cr.replace()
      let seqVal = arg(0)
      let typ = c.typeof(seqVal)#.skipTypes({tyVar})

      # XXX: if the refc pass would be run after the `lowerSeqV1` pass, a
      #      `askMove` assignment could be used here instead
      cr.genRefcRefAssign(c.extra, seqVal, cr.insertCast(typ, cr.insertCompProcCall(c.extra, "incrSeqV3", seqVal, c.requestRtti(cr, typ)) ), c.storageLoc(seqVal))

      # TODO: filling the element and adjusting the seq length is missing
      let seqLen = cr.accessSeqField(ir, seqVal, SeqV1LenField)
      let tmp = cr.genTempOf(seqLen, c.extra.sysTypes[tyInt])

      cr.insertAsgn(askCopy, seqLen, cr.insertMagicCall(c.extra, mAddI, tyInt, cr.insertLocalRef(tmp), cr.insertLit(c.env.data, 1)))
      # the value is a sink parameter so we can use a move
      # XXX: we're running after the inject-hook pass, so we either need to
      #      reorder the passes or manually insert a hook call here
      cr.insertAsgn(askMove, cr.insertPathArr(cr.accessSeqField(ir, seqVal, SeqV1DataField), cr.insertLocalRef(tmp)), arg(1))

    of mAppendStrStr:
      # -->
      #   resizeString(lhs, len(rhs))
      #   appendString(lhs, rhs)
      cr.replace()

      let lenTmp = genSeqLen(cr, c.env.data, c.extra, ir, arg(1))
      cr.insertCompProcCall(c.extra, "resizeString", arg(0), lenTmp)
      cr.insertCompProcCall(c.extra, "appendString", arg(0), arg(1))

    of mAppendStrCh:
      # -->
      #   str = addChar(str, c)

      cr.replace()
      let strVal = arg(0)
      let tmp = cr.genTempOf(strVal, c.typeof(strVal))

      cr.genRefcRefAssign(c.extra, strVal, cr.insertCompProcCall(c.extra, "addChar", cr.insertLocalRef(tmp), arg(1)), c.storageLoc(strVal))

    of mConStrStr:
      cr.replace()
      discard genStrConcat(cr, c.extra, c.types, ir, c.env[], cr.position)

    of mEqStr:
      # TODO: move into a common pass, since this shared between v1 and v2
      let
        a = arg(0)
        b = arg(1)

      func isEmptyStr(ir: IrStore3, data: LiteralData, i: IRIndex): bool =
        let n = ir.at(i)
        n.kind == ntkLit and getStr(data, ir.getLit(n).val).len == 0

      cr.replace()
      # optimize the case where either 'a' or 'b' is an empty string
      # literal
      let nonEmptyIdx =
        if   isEmptyStr(ir, c.env.data, a): b
        elif isEmptyStr(ir, c.env.data, b): a
        else: InvalidIndex

      if nonEmptyIdx == InvalidIndex:
        # both operands are not statically empty
        cr.insertCompProcCall(c.extra, "eqStrings", a, b)
      else:
        # we still call ``len`` for an empty string in the case that both
        # operands are empty strings, but since that case is highly unlikely,
        # it doesn't get special handling
        discard cr.binaryBoolOp(c.extra, mEqI, genSeqLen(cr, c.env.data, c.extra, ir, nonEmptyIdx), cr.insertLit(c.env.data, 0))

    of mLeStr, mLtStr:
      # same implementation for v1 and v2
      discard "lowered later"

    of mLengthStr:
      case c.env.types[typeof(c, arg(0))].kind
      of tnkString:
        cr.replace()
        discard genSeqLen(cr, c.env.data, c.extra, ir, arg(0))
      of tnkCString:
        discard "transformed later"
      else:
        unreachable()

    of mLengthSeq:
      cr.replace()
      discard genSeqLen(cr, c.env.data, c.extra, ir, arg(0))

    else:
      discard

  of ntkPathArr:
    let arrTyp = c.typeof(n.srcLoc)

    # TODO: needs tests
    case c.env.types[arrTyp].kind#skipTypes(arrTyp, {tyVar, tyLent}).kind
    of tnkString, tnkSeq:
      # -->
      #   x[].data[idx] # if not a const
      #   x.data[idx]   # if a cosnt

      cr.replace()
      var r = cr.insertDeref(cr.accessSeq(ir, c.env[], n.srcLoc, arrTyp))
      # a `lent seq` is not a treated as a `ptr NimSeq` but just as `NimSeq`
      # (`NimSeq` itself is a pointer type)
      if c.env.types[arrTyp].kind == tnkVar:
        r = cr.insertDeref(r)

      r = cr.insertPathObj(r, SeqV1DataField)
      discard cr.insertPathArr(r, n.arrIdx)

    else:
      discard

  # XXX: rewriting of `nktUse(ntkLit str)` is not done here anymore, but
  #      during `liftSeqConstsV1` instead. This does have the downside of
  #      also replacing string-literals only used in non-argument context with
  #      ``cast[NimString](addr str)``
  #[
  of ntkUse:
    if c.env.types[c.typeof(n.srcLoc)].kind == tnkString and isLiteral(ir, n.srcLoc):
      # XXX: the design of `ntkUse` is not final yet and doing this kind of
      #      rewrite might become a problem. Additionally, this prevents
      #      many-to-one node relationships
      # ideas:
      # * introduce the concept of a virtual-replace (or non-destructive-replace). Basically:
      #   * each node may be referenced by multiple other nodes
      #   * replacing a node requires specifying a usage-site (node)
      #   * if the node to replace only has one total usage site, it is
      #     directly modified
      #   * otherwise a new node is introduced and the given usage-site is
      #     modified to reference the new node
      # * collect usage-type information for each node (e.g. used as an
      #   argument, used in a path-expression, etc.) and take this information
      #   into account when interacting with the node. For example, if a
      #   string-literal is only used in a path-expression context, it is
      #   replaced with only a const-reference, but if it's used in an
      #   argument context (either exclusively or additionally) the literal
      #   is replaced with a ``cast[NimString](addr constString)``. Other
      #   rewrites (e.g. ``accessSeqField``) would also have to take this into
      #   account

      # the literal is going to be replaced by a constant and since procedures
      # taking strings will now expect a ``NimString`` (i.e. pointer type), we
      # have to adjust the literal
      cr.replace()
      discard cr.insertCast(c.extra.getCompilerType("NimString"), cr.insertAddr(n.srcLoc))

      # TODO: insertUse is missing?
      #cr.insertUse()
  ]#

  else:
    discard "ignore"

type TypeTransformCtx* = object
  graph*: PassEnv
  ic*: IdentCache

func lowerSeqTypesV1*(c: var TypeTransformCtx, tenv: var TypeEnv, senv: var SymbolEnv) =
  let
    strTyp = c.graph.getCompilerType("NimString")
    seqTyp = c.graph.getCompilerType("TGenericSeq")
    fieldName = c.ic.getIdent("data")

  var remap: Table[TypeId, TypeId]
  for id, typ in tenv.items:
    case typ.kind
    of tnkString:
      remap[id] = c.graph.getCompilerType("NimString")
    of tnkSeq:
      # replace a ``seq[T]`` with the following:
      #
      # .. code:: nim
      #   type PSeq = ptr object of TGenericSeq # name is just an example
      #     data: UncheckedArray[T]
      #
      let
        arr = tenv.requestGenericType(tnkUncheckedArray, typ.base)
        sym = senv.addDecl(fieldName)
        rec = tenv.requestRecordType(base = seqTyp, [(sym, arr)])
      remap[id] = requestGenericType(tenv, tnkPtr, rec)
    else:
      discard

  commit(tenv, remap)

func lowerSeqsV2(c: GenericTransCtx, n: IrNode3, cr: var IrCursor) =
  ## Lowers the `seq`-related magic operations into calls to the v2 `seq`
  ## implementation. Enabled by the `optSeqDestructors` toggle
  doAssert false, "missing"

type
  ConstCache = Table[(LiteralId, TypeId), SymId]

type LiftPassCtx* = object
  graph*: PassEnv
  idgen*: IdGenerator
  cache*: IdentCache

  env*: ptr IrEnv

  typeInfoMarker*: Table[TypeId, SymId] # sig hash -> type info sym

  syms*: seq[(SymId, TypeId)] ## all lifted globals

  constCache*: ConstCache ## caches the created constants corresponding to
                          ## (literal, type) pairs

func addGlobal*(c: var LiftPassCtx, t: TypeId, name: string): SymId =
  # XXX: temporary helper
  c.env.syms.addSym(skLet, t, c.cache.getIdent(name), {sfGlobal}) # XXX: uh-oh, hidden mutation

func addConst(syms: var SymbolEnv, c: var ConstCache, name: PIdent, t: TypeId, val: LiteralId): SymId =
  ## If there doesn't exists a constant for the ``(val, t)`` pair in `c`, adds
  ## one to `syms` and stores a mapping for it in `c`.
  ## `name` only provides the identifier to use during the creation of the
  ## constant - it has no effect on the caching. E.g. multiple calls to
  ## ``addConst`` with the same ``(val, t)`` pair but a different `name` will
  ## all yield the same symbol
  # XXX: since it not always adds a const, rename?
  let pair = (val, t)
  result = c.getOrDefault(pair, NoneSymbol)
  if result == NoneSymbol:
    # not lifted into a constant yet
    result = syms.addSym(skConst, t, name)
    syms.setData(result, val)

    # XXX: double table lookup
    c[pair] = result

func addConst(c: var LiftPassCtx, t: TypeId, name: string, val: LiteralId): SymId {.inline.} =
  addConst(c.env.syms, c.constCache, c.cache.getIdent(name), t, val)

proc liftSeqConstsV1(c: var LiftPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  # XXX: we reuse the ``LiftPassCtx`` for now, but it's currently not really
  #      meant for our usage here

  case n.kind
  of ntkLit:
    let lit = getLit(ir, n)
    if lit.typ == NoneType:
      # TODO: remove this once all literals have type information
      return

    if lit.val == NoneLit:
      # a type literal
      return

    case c.env.types[lit.typ].kind
    of tnkString:
      let s = c.addConst(lit.typ, "strConst", lit.val)

      cr.replace()
      # see the comment for ``tnkSeq`` below for why the addr + cast is done
      discard cr.insertCast(c.graph.getCompilerType("NimString"), cr.insertAddr(cr.insertSym(s)))

    of tnkSeq:
      let s = c.addConst(lit.typ, "seqConst", lit.val)

      cr.replace()
      # at the current stage of processing, the cast would produce wrong
      # behaviour. However, during later processing, the type of the constant
      # is changed to a specialized fixed-length seq type that is pointer
      # compatible with ``TGenericSeq``
      # XXX: the mentioned later processing doesn't exist yet
      discard cr.insertCast(lit.typ, cr.insertAddr(cr.insertSym(s)))

    else:
      discard

  of ntkSym:
    let
      s = ir.sym(n)
      typ = c.env.syms[s].typ

    case c.env.types.kind(typ)
    of tnkString, tnkSeq:
      # see the documentation of the ``seq``-literal lifting for the reason
      # behind this transformation
      # XXX: strictly speaking, this transformation is not part of the actual
      #      lifting. Moving it to a separate does seem a bit overkill however
      cr.replace()
      discard cr.insertCast(typ, cr.insertAddr(cr.insertSym(s)))
    else:
      discard

  else:
    discard

const ErrFlagName = "nimError"

proc lowerTestError*(ir: var IrStore3, g: PassEnv, ic: IdentCache, types: TypeEnv, procs: ProcedureEnv, syms: var SymbolEnv) =
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

            # TODO: this lookup yields the same across all calls to `lowerTestError`. Cache both the compiler proc and it's return type
            typ = procs.getReturnType(p)
            decl = syms.addDecl(ic.getIdent(ErrFlagName))

          errFlag = cr.insertLocalRef(cr.newLocal(lkLet, typ, decl))
          cr.insertAsgn(askInit, errFlag, cr.insertCallExpr(p))

          cr.setPos i # set cursor back to the current position

        cr.replace()
        discard cr.insertCallExpr(bcUnlikely, NoneType, cr.insertDeref(errFlag)) # TODO: `NoneType` is wrong here

    else:
      discard

  ir.update(cr)

func genSetElemOp(cr: var IrCursor, g: PassEnv, m: TMagic, t: TypeId, a, b: IRIndex): IRIndex =
  case m
  of mEqSet:
    cr.binaryBoolOp(g, mEqI, a, b)
  of mMulSet:
    cr.insertCallExpr(mBitandI, t, a, b)
  of mPlusSet:
    cr.insertCallExpr(mBitorI, t, a, b)
  of mMinusSet:
    cr.insertCallExpr(mBitandI, t, a, cr.insertCallExpr(mBitnotI, t, b))
  else:
    unreachable(m)

func insertLoop(cr: var IrCursor): JoinPoint =
  result = cr.newJoinPoint()
  cr.insertJoin(result)

func genMaskExpr(setType: TypeId, len: uint, val: IRIndex, data: var LiteralData, cr: var IrCursor): IRIndex =
  ## Generates the expression for calculating the bitmask of a set
  ## element (`val`). `setType` is the type to use for the resulting
  ## expression and `len` the number of elements in the set.
  let mask =
    case len
    of 0..8:   7
    of 9..16:  15
    of 17..32: 31
    of 33..64: 63
    else: unreachable()

  # ``1 shl (a and mask)``
  cr.insertCallExpr(mShlI, setType, cr.insertLit(data, 1), cr.insertCallExpr(mBitandI, setType, val, cr.insertLit(data, mask)))

func genSubsetRelOp(setType: TypeId, a, b: IRIndex, testTrue: bool, g: PassEnv, data: var LiteralData, cr: var IrCursor): IRIndex =
  # compute if a is either a subset of or equal to b
  let isSubsetExpr = cr.insertMagicCall(g, mEqI, tyBool, cr.insertCallExpr(mBitandI, setType, a, cr.insertCallExpr(mBitnotI, setType, b)), cr.insertLit(data, 0))

  if testTrue:
    # if the test is for true subset relationship, we also need compare
    # both sets for equality
    # --->
    #   var tmp = isSubsetExpr
    #   if tmp:
    #     tmp = a != b
    #   tmp
    let
      tmp = cr.newLocal(lkTemp, g.sysTypes[tyBool])
      exit = cr.newJoinPoint()

    cr.insertAsgn(askInit, cr.insertLocalRef(tmp), isSubsetExpr)

    cr.insertBranch(cr.insertMagicCall(g, mNot, tyBool, cr.insertLocalRef(tmp)), exit)
    cr.insertAsgn(askCopy, cr.insertLocalRef(tmp), cr.insertMagicCall(g, mEqI, tyBool, a, b))
    cr.insertGoto(exit)

    cr.insertJoin(exit)

    result = cr.insertLocalRef(tmp)
  else:
    result = isSubsetExpr

func genSetOp(c: var RefcPassCtx, ir: IrStore3, m: TMagic, n: IrNode3, cr: var IrCursor) =
  # TODO: sets with ``firstOrd != 0`` aren't taken into account yet.
  #       ``irgen`` has to insert the required adjustment
  let
    setType = c.typeof(ir.argAt(cr, 0))
    len = c.env.types.length(setType)

  case len
  of 0..64:
    case m
    of mEqSet, mMulSet, mPlusSet, mMinusSet:
      discard genSetElemOp(cr, c.extra, m, setType, ir.argAt(cr, 0), ir.argAt(cr, 1))
    of mLeSet, mLtSet:
      discard genSubsetRelOp(setType, ir.argAt(cr, 0), ir.argAt(cr, 1), testTrue=(m == mLtSet), c.extra, c.env.data, cr)
    of mInSet:
      let mask = genMaskExpr(setType, len, ir.argAt(cr, 1), c.env.data, cr)
      discard cr.insertCallExpr(mBitandI, setType, ir.argAt(cr, 0), mask)
    of mIncl, mExcl:
      # mIncl --->
      #   a = a or (1 shl b)
      # mExcl --->
      #   a = a and not(1 shl b)
      let
        a = ir.argAt(cr, 0)
        b = ir.argAt(cr, 1)
        mask = genMaskExpr(setType, len, b, c.env.data, cr)

      let (op, rhs) =
        case m
        of mIncl: (mBitorI, mask)
        of mExcl: (mBitandI, cr.insertCallExpr(mBitnotI, setType, mask))
        else: unreachable()

      cr.insertAsgn(askCopy, a, cr.insertCallExpr(op, setType, a, rhs))

    of mCard:
      let prc =
        case len
        of 0..32: "countBits32"
        of 33..64: "countBits64"
        else: unreachable()

      cr.insertCompProcCall(c.extra, prc, ir.argAt(cr, 0))
    else:
      unreachable()

  else:
    case m
    of mMulSet, mPlusSet, mMinusSet:
      let
        arg0 = ir.argAt(cr, 0)
        arg1 = ir.argAt(cr, 1)
        res = cr.newLocal(lkTemp, setType)

      # apply the set operation to each byte in the array-based ``set``
      cr.genForLoop(c.env.data, c.extra, cr.insertLit(c.env.data, (len + 7) div 8)):
        let v = genSetElemOp(cr, c.extra, m, c.extra.sysTypes[tyUInt8], cr.insertPathArr(arg0, counter), cr.insertPathArr(arg1, counter))
        cr.insertAsgn(askInit, cr.insertPathArr(cr.insertLocalRef(res), counter), v)

      discard cr.insertLocalRef(res)

    of mEqSet, mLeSet, mLtSet:
      let
        arg0 = ir.argAt(cr, 0)
        arg1 = ir.argAt(cr, 1)
        res = cr.newLocal(lkTemp, c.extra.getSysType(tyBool))

      # start with ``res = true``
      cr.insertAsgn(askInit, cr.insertLocalRef(res), cr.insertLit(c.env.data, 1))

      # iterate over all bytes in the set and perform the comparison:
      cr.genForLoop(c.env.data, c.extra, cr.insertLit(c.env.data, (len + 7) div 8)):
        let
          a = cr.insertPathArr(arg0, counter)
          b = cr.insertPathArr(arg1, counter)

        let v =
          case m
          of mEqSet:         cr.binaryBoolOp(c.extra, mEqI, a, b)
          of mLtSet, mLeSet: genSubsetRelOp(c.extra.sysTypes[tyUInt8], a, b, m == mLtSet, c.extra, c.env.data, cr)
          else:              unreachable()

        # if the comparison for the partial sets doesn't succeed, set the result
        # to false and exit the loop
        cr.genIfNot(v):
          cr.insertAsgn(askCopy, cr.insertLocalRef(res), cr.insertLit(c.env.data, 0))
          cr.insertGoto(loopExit)

      discard cr.insertLocalRef(res)

    of mInSet:
        let uintTyp = c.extra.getSysType(tyUInt)
        let conv = cr.insertConv(uintTyp, ir.argAt(cr, 1))
        let
          index = cr.insertCallExpr(mShrI, uintTyp, conv, cr.insertLit(c.env.data, 3))
          mask = cr.insertCallExpr(mShlI, uintTyp, cr.insertLit(c.env.data, 1), cr.insertCallExpr(mBitandI, uintTyp, conv, cr.insertLit(c.env.data, 7)))

        let val = cr.insertCallExpr(mBitandI, c.extra.sysTypes[tyUInt8], cr.insertPathArr(ir.argAt(cr, 0), index), mask)
        discard cr.insertMagicCall(c.extra, mNot, tyBool, cr.binaryBoolOp(c.extra, mEqI, val, cr.insertLit(c.env.data, 0)))

    of mIncl, mExcl:
      # mIncl --->
      #   a[b shr 2] = a[b shr 2] or (1 shl (b and 7))
      # mExcl --->
      #   a[b shr 2] = a[b shr 2] and not(1 shl (b and 7))

      let
        a = ir.argAt(cr, 0)
        b = ir.argAt(cr, 1)
        uintTy = c.extra.sysTypes[tyUInt]
        dest = cr.insertPathArr(a, cr.insertCallExpr(mShrI, uintTy, b, cr.insertLit(c.env.data, 3)))
        bit = cr.insertCallExpr(mShlI, uintTy, cr.insertLit(c.env.data, 1), cr.insertCallExpr(mBitandI, uintTy, b, cr.insertLit(c.env.data, 7)))

      let rhs =
        case m
        of mIncl: cr.insertCallExpr(mBitorI, uintTy, dest, bit)
        of mExcl: cr.insertCallExpr(mBitandI, uintTy, dest, cr.insertCallExpr(mBitnotI, uintTy, bit))
        else:     unreachable()

      cr.insertAsgn(askCopy, dest, rhs)

    of mCard:
      # --->
      #   cardSet2(cast[ptr UncheckedArray[uint8]](set), size)
      let
        size = (len + 7) div 8 # ceil-div by 8
        arrTyp = c.env.types.lookupGenericType(tnkUncheckedArray, c.extra.sysTypes[tyUInt8])
        ptrTyp = c.env.types.lookupGenericType(tnkPtr, arrTyp)

      cr.insertCompProcCall(c.extra, "cardSetPtr", cr.insertCast(ptrTyp, cr.insertAddr(ir.argAt(cr, 0))), cr.insertLit(c.env.data, size))

    else:
      unreachable()

func setAsInt(env: TypeEnv, data: var LiteralData, id: TypeId, lit: LiteralId): LiteralId

proc lowerSets*(c: var RefcPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Lowers ``set`` operations into bit operations. Intended for the C-like targets
  # XXX: some set lowerings could be simplified by adding them as
  #      compiler-procs in ``system.nim`` and then doing something an
  #      `cr.inline` here
  case n.kind
  of ntkCall:
    let m = getMagic(ir, c.env[], n)

    case m
    of mIncl, mExcl, mCard, mEqSet, mLeSet, mLtSet, mMulSet, mPlusSet, mMinusSet, mInSet:
      cr.replace()
      genSetOp(c, ir, m, n, cr)
    else:
      discard

  of ntkLit:
    let lit = getLit(ir, n)

    if lit.val == NoneLit or lit.typ == NoneType:
      # TODO: remove the ``NoneType`` guard once all literals have type
      #       information
      return

    # only transform ``set`` literals that fit into integer types. The ones
    # requiring an array as the representation are lifted into constants in
    # a separate pass
    let typ = c.env.types[lit.typ]
    if typ.kind == tnkSet and typ.length <= 64:
      let newLit = setAsInt(c.env.types, c.env.data, lit.typ, lit.val)

      cr.replace()
      discard cr.insertLit((newLit, NoneType))

  else:
    discard

func liftLargeSets(c: var LiftPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  case n.kind
  of ntkLit:
    let lit = getLit(ir, n)

    if lit.val == NoneLit or lit.typ == NoneType:
      # TODO: remove the ``NoneType`` guard once all literals have type
      #       information
      return

    let typ = c.env.types[lit.typ]
    if typ.kind == tnkSet and typ.length > 64:
      let s = c.addConst(lit.typ, "setConst", lit.val)

      cr.replace()
      discard cr.insertSym(s)

  else:
    discard

func lowerSetTypes*(c: var TypeTransformCtx, tenv: var TypeEnv, senv: SymbolEnv) =
  var remap: Table[TypeId, TypeId]

  for id, typ in tenv.items:
    if typ.kind == tnkSet:
      let L = typ.length

      if L <= 64:
        # sets smaller than 64 bits are turned into fitting uint types
        let r =
          if L <= 8:    c.graph.sysTypes[tyUInt8]
          elif L <= 16: c.graph.sysTypes[tyUInt16]
          elif L <= 32: c.graph.sysTypes[tyUInt32]
          else:         c.graph.sysTypes[tyUInt64]

        remap[id] = r
      else:
        # larget sets are turned into byte arrays
        let numBytes = ((L + 7) and (not 7'u)) div 8'u # round to the next multiple of 8
        remap[id] = tenv.requestArrayType(numBytes, c.graph.sysTypes[tyUInt8])

  commit(tenv, remap)

func liftArrays(c: var LiftPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  case n.kind
  of ntkLit:
    let lit = getLit(ir, n)

    if lit.val == NoneLit or lit.typ == NoneType:
      # TODO: remove the ``NoneType`` guard once all literals have type
      #       information
      return

    if c.env.types[lit.typ].kind == tnkArray:
      cr.replace()
      discard cr.insertSym: c.addConst(lit.typ, "arrConst", lit.val)

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
      let
        val = ir.argAt(cr, 0)
        lower = ir.argAt(cr, 1)
        upper = ir.argAt(cr, 2)

      let srcTyp = c.typeof(val)

      cr.replace()
      var cond: IRIndex
      var raiser: string

      case c.env.types[srcTyp].kind
      of tnkInt: # tyUInt, tyUInt64:
        # .. code:: nim
        #   cast[dstTyp](high) < val
        cond = cr.binaryBoolOp(c.extra, mLtU, cr.insertCast(srcTyp, upper), val)
        raiser = "raiseRangeErrorNoArgs"
      else:
        let dstTyp = c.typeof(cr.position)#skipTypes(c.typeof(cr.position), abstractVarRange)
        case c.env.types[dstTyp].kind
        of tnkInt: #tyUInt8..tyUInt32, tyChar:
          raiser = "raiseRangeErrorU"
        of tnkFloat: #tyFloat..tyFloat128:
          raiser = "raiseRangeErrorF"
          let conv = cr.insertConv(dstTyp, val)
          # no need to lower the `or` into an `ntkBranch` + `ntkJoin` here; it has no impact on further analysis
          cond = cr.binaryBoolOp(c.extra, mOr, cr.binaryBoolOp(c.extra, mLtF64, conv, lower), cr.binaryBoolOp(c.extra, mLtF64, upper, conv))

        else:
          cr.insertError(c.env.data, "missing chkRange impl")

        raiser =
          case c.env.types[c.typeof(cr.position)].kind#skipTypes(c.typeof(cr.position), abstractVarRange).kind
          of tnkFloat: "raiseRangeErrorF"#tyFloat..tyFloat128: "raiseRangeErrorF"
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
        cr.insertBranch(cr.insertMagicCall(c.extra, mNot, tyBool, cond), target)
        cr.insertCompProcCall(c.extra, raiser, val, lower, upper)
        # XXX: it would be nice if we could also move the following
        #      ``if bcTestError(): goto error`` into the branch here

        cr.insertJoin(target)
        discard cr.insertConv(dstTyp, val)

  else:
    discard


const OpenArrayDataField = 0
const OpenArrayLenField = 1

type LowerOACtx = object
  graph: PassEnv
  env: ptr IrEnv

  types: seq[TypeId]
  paramMap: seq[uint32] ## maps the current parameter indices to the new ones

func expandData(c: LowerOACtx, cr: var IrCursor, ir: IrStore3, src: IRIndex): IRIndex =
  #[if ir.at(src).kind == ntkParam:
    let pIdx = ir.at(src).paramIndex
    cr.insertParam(c.paramMap[pIdx] + 0)
  else:
  ]#
    cr.insertPathObj(src, OpenArrayDataField)

func expand(c: LowerOACtx, cr: var IrCursor, ir: IrStore3, src: IRIndex): tuple[dataExpr, lenExpr: IRIndex] =
  # XXX: verify that this doesn't lead to evaluation order issues
  #[
  if ir.at(src).kind == ntkParam:
    let pIdx = ir.at(src).paramIndex
    result.dataExpr = cr.insertParam(c.paramMap[pIdx] + 0)
    result.lenExpr = cr.insertParam(c.paramMap[pIdx] + 1)
  else:
  ]#
    result.dataExpr = cr.insertPathObj(src, OpenArrayDataField)
    result.lenExpr = cr.insertPathObj(src, OpenArrayLenField)

# TODO: the openArray rewriting needs lots of tests (a sign that it's too complex/need to be done differently?)
func lowerOpenArrayVisit(c: var LowerOACtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  case n.kind
  of ntkAsgn:
    #[
    if c.env.types[c.types[n.wrLoc]].kind == tnkOpenArray:
      # the lhs can only be a non-parameter
      if ir.at(n.srcLoc).kind == ntkParam:
        # -->
        #   dst.data = srcData
        #   dst.len = srcLen
        cr.replace()
        let
          dest = expand(c, cr, ir, n.wrLoc)
          src = expand(c, cr, ir, n.srcLoc)
        cr.insertAsgn(n.asgnKind, dest.dataExpr, src.dataExpr)
        cr.insertAsgn(n.asgnKind, dest.lenExpr, src.lenExpr)
    ]#
    discard

  of ntkCall:
    case getMagic(ir, c.env[], n)
    of mLengthOpenArray:
      cr.replace()
      #[
      if ir.at(n.args(0)).kind == ntkParam:
        discard cr.insertParam(c.paramMap[ir.at(n.args(0)).paramIndex] + 1)
      else:
      ]#
      # rewrite to field-access
      discard cr.insertPathObj(ir.argAt(cr, 0), OpenArrayLenField)

      # TODO: this needs to be done differently
      return # prevent the argument patching from running
    of mSlice:
      let
        arr = ir.argAt(cr, 0)
        first = ir.argAt(cr, 1)
        last = ir.argAt(cr, 2)

      cr.replace()
      let tmp = cr.newLocal(lkTemp, c.types[cr.position])
      let arg = c.env.types.skipVarOrLent(c.types[arr])

      let tmpAcc = cr.insertLocalRef(tmp)
      let ex = block:
        (dataExpr: cr.insertPathObj(tmpAcc, OpenArrayDataField),
         lenExpr:  cr.insertPathObj(tmpAcc, OpenArrayLenField))

      let p =
        case c.env.types[arg].kind
        of tnkArray:
          cr.insertAddr(cr.insertPathArr(arr, first))

        of tnkOpenArray:
          cr.insertAddr(cr.insertPathArr(cr.insertDeref(expandData(c, cr, ir, arr)), first))

        of tnkPtr:
          assert c.env.types[c.env.types.baseType(arg)].kind == tnkUncheckedArray
          cr.insertAddr(cr.insertPathArr(cr.insertDeref(arr), first))

        of tnkCString, tnkString, tnkSeq:
          cr.insertAddr(cr.insertPathArr(arr, first))
        else:
          unreachable()

      let lenExpr = cr.insertMagicCall(c.graph, mSubI, tyInt, last, first)

      # XXX: the pointer needs a cast, but we don't know the correct type yet...
      cr.insertAsgn(askInit, ex.dataExpr, p)
      cr.insertAsgn(askInit, ex.lenExpr, lenExpr)

      discard cr.insertLocalRef(tmp)

      return
    else:
      discard

    #[
    var numOaParams = 0

    for it in ir.args(cr.position):
      let t = c.types[it]
      if t != NoneType and c.env.types[c.env.types.skipVarOrLent(c.types[it])].kind == tnkOpenArray:
        inc numOaParams

    # we have to patch calls to procedures taking ``openArray``s
    if numOaParams > 0:
      cr.replace()
      # TODO: re-use the seq
      var newArgs = newSeq[IRIndex](n.argCount + numOaParams) # each openArray arguments is expanded into two arguments
      var i = 0
      for it in ir.args(cr.position):
        if c.env.types[c.env.types.skipVarOrLent(c.types[it])].kind == tnkOpenArray:
          # XXX: verify that this doesn't lead to evaluation order issues.
          #      We're inserting the expansion _after_ the other arguments, but
          #      might be able to get away with it since no more analysis is
          #      performed beyond this point
          let exp = expand(c, cr, ir, it)
          newArgs[i + 0] = exp.dataExpr
          newArgs[i + 1] = exp.lenExpr
          i += 2
        else:
          newArgs[i] = it
          inc i

      # patch the call
      if n.isBuiltIn:
        discard cr.insertCallExpr(n.builtin, n.typ, newArgs)
      else:
        discard cr.insertCallExpr(n.callee, newArgs)
    ]#

  of ntkPathArr:
    # XXX: the amount of workarounds to handle ``var openArray`` parameters
    #      indicates that a different approach for representing mutable
    #      ``openArray``s is needed
    if c.env.types[c.env.types.skipVarOrLent(c.types[n.srcLoc])].kind == tnkOpenArray:
      cr.replace()

      let field =
        #[
        if ir.at(n.srcLoc).kind == ntkParam:
          cr.insertParam(c.paramMap[ir.at(n.srcLoc).paramIndex] + 0)
        else:]#
          # rewrite to field-access
          cr.insertPathObj(n.srcLoc, OpenArrayDataField)

      discard cr.insertPathArr(cr.insertDeref(field), n.arrIdx)

  of ntkConv:
    let typ = c.env.types.skipVarOrLent(n.typ)
    if c.env.types[typ].kind == tnkOpenArray:
      # Transform to:
      #   var tmp: OpenArrayTuple
      #   tmp[0] = addr src[0]
      #   tmp[1] = src.len

      cr.replace()
      let
        tmp = cr.newLocal(lkTemp, n.typ)
        arr = cr.access(c.env.types, n.srcLoc, c.types[n.srcLoc])
        srcTyp = c.env.types.skipVarOrLent(c.types[n.srcLoc])

      let tmpAcc = cr.insertLocalRef(tmp)
      let ex = block:
        (dataExpr: cr.insertPathObj(tmpAcc, OpenArrayDataField),
         lenExpr:  cr.insertPathObj(tmpAcc, OpenArrayLenField))

      assert c.env.types[srcTyp].kind in {tnkArray, tnkString, tnkSeq}, $c.env.types[srcTyp].kind

      # TODO: this will fail if the source has a length of zero
      let p =
        if c.env.types[srcTyp].kind == tnkArray and c.env.types.length(srcTyp) == 0:
          # XXX: the type is wrong, but we don't have access to the correct
          #      one
          cr.insertNilLit(c.env.data, c.graph.sysTypes[tyPointer])
        else:
          cr.insertAddr cr.insertPathArr(arr, cr.insertLit(c.env.data, 0))

      let lenExpr =
        case c.env.types[srcTyp].kind
        of tnkArray:
          cr.insertLit(c.env.data, c.env.types[srcTyp].length)
        of tnkSeq:
          cr.insertMagicCall(c.graph, mLengthSeq, tyInt, arr)
        of tnkString:
          cr.insertMagicCall(c.graph, mLengthStr, tyInt, arr)
        else:
          unreachable(c.env.types[srcTyp].kind)

      # XXX: the pointer needs a cast, but we don't know the correct type yet...
      cr.insertAsgn(askInit, ex.dataExpr, p)
      cr.insertAsgn(askInit, ex.lenExpr, lenExpr)

      discard cr.insertLocalRef(tmp)
  #[
  of ntkParam:
    let orig = n.paramIndex
    if c.paramMap[orig] != orig.uint32:
      # reduce the amount of work by only rewriting parameters for which
      # patching is necessary
      cr.replace()
      discard cr.insertParam(c.paramMap[orig])
  ]#
  else:
    discard


func genTransformedOpenArray(g: PassEnv, tenv: var TypeEnv, typ: Type): TypeId =
  ## .. code-block:: nim
  ##
  ##   type X = object
  ##     data: ptr UncheckedArray[T]
  ##     len: int
  let
    arrTyp = tenv.requestGenericType(tnkUncheckedArray, typ.base)
    ptrTyp = tenv.requestGenericType(tnkPtr, arrTyp)

  result = tenv.requestRecordType(base = NoneType, [(NoneDecl, ptrTyp), (NoneDecl, g.sysTypes[tyInt])])

func lowerOpenArrayTypes*(c: var TypeTransformCtx, tenv: var TypeEnv, senv: SymbolEnv) =
  ## Transforms ``openArray[T]`` types

  var remap: Table[TypeId, TypeId]

  for id, typ in tenv.items:
    case typ.kind
    of tnkVar, tnkLent:
      # ``var/lent openArray[T]`` is lowered too
      if tenv[typ.base].kind == tnkOpenArray:
        remap[id] = genTransformedOpenArray(c.graph, tenv, tenv[typ.base])

    of tnkOpenArray:
      remap[id] = genTransformedOpenArray(c.graph, tenv, tenv[id])
    else:
      discard

  commit(tenv, remap)

proc lowerOpenArray*(g: PassEnv, id: ProcId, ir: var IrStore3, env: var IrEnv) =
  ## * transform ``openArray`` **parameters** (not the types in general) into
  ##  an unpacked ``(ptr T, int)`` pair. That is:
  ##
  ## .. code-block:: nim
  ##   proc a(x: openArray[int]) = ...
  ##   # becomes
  ##   proc a(xData: ptr int, xLen: int) = ...
  ##
  ## * patch ``openArray`` arguments
  ## * transform all ``openArray`` related magics

  # XXX: we don't modify the whole `env`, just the procedures. Passing in
  #      each sub-environment separately won't work however, as we also need
  #      access to the whole `env` for the lowering pass

  var ctx = LowerOACtx(graph: g, env: addr env)
  ctx.types = computeTypes(ir, env)
  #[
  # TODO: don't create a new seq for each procedure we're modifying
  ctx.paramMap.newSeq(env.procs.numParams(id))

  var i, j = 0
  for p in env.procs.params(id):
    ctx.paramMap[i] = j.uint32
    inc i

    if env.types[env.types.skipVarOrLent(p.typ)].kind == tnkOpenArray:
      # an openArray parameter gets expanded and then takes up two parameters
      j += 2
    else:
      j += 1
  ]#

  const pass = LinearPass2[LowerOACtx](visit: lowerOpenArrayVisit)
  runPass(ir, ctx, pass)

  #[
  # only modify the signature if really necessary:
  if j != i:
    var old: typeof(ProcHeader.params)
    old.newSeq(j)
    swap(old, env.procs.mget(id).params)

    i = 0
    j = 0
    for p in old.items:
      let t = env.types.skipVarOrLent(p.typ)
      if env.types[t].kind == tnkOpenArray:
        # an openArray parameter gets expanded and then takes up two parameters
        let dataType = env.types.requestGenericType(tnkPtr):
          env.types.requestGenericType(tnkUncheckedArray, env.types[t].base)

        env.procs.mget(id).params[j + 0] = (old[i].name & "Data_", dataType)
        env.procs.mget(id).params[j + 1] = (old[i].name & "Len_", g.sysTypes[tyInt])
        j += 2
      else:
        env.procs.mget(id).params[j] = old[i]
        j += 1

      inc i
  ]#

proc lowerOfV1(c: var UntypedPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  case n.kind
  of ntkCall:
    case getMagic(ir, c.env[], n)
    of mOf:
      # TODO: ``isObjWithCache`` should be used, but supporting it is non-trivial
      # TODO: a simply equality test can be used if the object is marked as
      #       ``.final``
      # XXX: ``tyPointer`` is not correct, but since ``mAccessTypeField``
      #      gets lowered before reaching code-gen, we should be able to get
      #      away with it
      cr.replace()
      discard cr.insertCompProcCall(c.graph, "isObj", cr.insertMagicCall(c.graph, mAccessTypeField, tyPointer, ir.argAt(cr, 0)), requestRtti2(c.graph, cr, ir.getLit(ir.at(ir.argAt(cr, 1))).typ))

    else:
      discard

  else:
    discard

type LiftAtomProc = proc(typ: TypeId, iter: var DataIter, syms: var SymbolEnv, c: var ConstCache, data: LiteralData, types: TypeEnv): bool

proc liftFrom(typ: TypeId, iter: var DataIter, syms: var SymbolEnv, c: var ConstCache, data: LiteralData, types: TypeEnv, prc: LiftAtomProc)
proc liftFromAux(typ: TypeId, iter: var DataIter, syms: var SymbolEnv, c: var ConstCache, data: LiteralData, types: TypeEnv, prc: LiftAtomProc)

proc liftFromStruct(typ: TypeId, iter: var DataIter, syms: var SymbolEnv, c: var ConstCache, data: LiteralData, types: TypeEnv, prc: LiftAtomProc) =
  ## Traverses the initializer AST `data` and applies `prc` to each sub-node
  ## (via ``liftFrom``)
  case types.kind(typ)
  of tnkArray, tnkSeq:
    assert iter.kind == conArray
    for _ in 0..<iter.len:
      liftFrom(types.base(typ), iter, syms, c, data, types, prc)

  of tnkRecord:
    case iter.kind
    of conRecord:
      for i in 0..<iter.len:
        iter.next() # go to the position entry
        let id = types.nthField(typ, data.getRecPos(iter))
        liftFrom(types[id].typ, iter, syms, c, data, types, prc)

    of conArray:
      assert iter.len.int - types[typ].fieldOffset == types.numFields(typ)
      for i in 0..<iter.len.int:
        liftFrom(types[types.nthField(typ, i)].typ, iter, syms, c, data, types, prc)

    else:
      unreachable(iter.kind)

  else:
    # ``liftFromStruct`` is also used as the entry procedure for constant
    # lifting, where it's not known whether or not the literal is really a
    # structure. Don't raise an error - just do nothing
    discard

proc liftFromAux(typ: TypeId, iter: var DataIter, syms: var SymbolEnv,
                 c: var ConstCache, data: LiteralData, types: TypeEnv,
                 prc: LiftAtomProc) =
  ## Traverses the initializer AST `data` and applies `prc` to each sub-node
  ## (via ``liftFrom``)
  if iter.get(data).kind in {conConst, conConstAddr}:
    # don't follow references to consts
    return

  case types.kind(typ)
  of tnkArray, tnkSeq, tnkRecord:
    var sub = iter.enter(data)
    liftFromStruct(typ, sub, syms, c, data, types, prc)
    close(iter, sub)
  else:
    iter.skipChildren(data)

proc liftFrom(typ: TypeId, iter: var DataIter, syms: var SymbolEnv, c: var ConstCache,
              data: LiteralData, types: TypeEnv, prc: LiftAtomProc) =
  ## Recursively walks the initializer AST `data`, applying `prc` to all
  ## sub-nodes. After all sub-nodes were traversed, `prc` is applied to `data`
  ## itself
  iter.next()
  if not prc(typ, iter, syms, c, data, types):
    # if the item was not lifted/replaced, recurse into it
    liftFromAux(typ, iter, syms, c, data, types, prc)

proc liftSeqConstsV1*(syms: var SymbolEnv, data: var LiteralData,
                      c: var ConstCache, name: PIdent, types: TypeEnv) =
  ## Lifts ``string|seq`` literals used by constant data into their own
  ## constants and replace the lifted-from location with a reference to the
  ## newly created string constant

  # TODO: same issue as with the other ``liftSeqConstsV1`` - we're creating
  #       duplicates

  func liftAtom(typ: TypeId, iter: var DataIter, syms: var SymbolEnv, c: var ConstCache, data: LiteralData, types: TypeEnv): bool =
    let kind = types.kind(typ)
    case kind
    of tnkString, tnkSeq:
      let s = syms.addConst(c, name, typ, data.getLit(iter))

      replaceWithConstAddr(iter, s.uint32)

      result = true
    else:
      discard

  for id, s in syms.msymbols:
    case s.kind
    of skConst:
      let lit = syms.data(id)

      if lit.kind != lkComplex:
        continue

      var iter = initDataIter(data, lit)
      moveInto(iter, data)
      # don't use ``liftFrom``, as that would also lift the data itself
      liftFromStruct(s.typ, iter, syms, c, data, types, liftAtom)

      # TODO: only set the data if it has changed
      syms.setData(id, data.finish(iter))
    else:
      discard

func transformSeqConstsV1*(pe: PassEnv, syms: var SymbolEnv, data: var LiteralData, types: var TypeEnv) =
  ## Transforms the data representation for seq|string constants to what is
  ## expected for v1 seqs. The type used for each the constants is equivalent
  ## to an instantiation of the following high-level type:
  ##
  ## .. code-block::nim
  ##
  ##   type SeqConstTy[I: static int; T] = object of TGenericSeq
  ##     data: array[I, T]
  ##
  ## where ``I`` is the number of elements in the ``string|seq``, and ``T`` is
  ## the element type (``char`` in the case of ``string``).
  ##
  ## For strings, ``array`` also stores a trailing '\0'

  # XXX: temporary hack; the ``strlitFlag`` constant in ``system.nim``
  #      needs to be marked as ``.core|.compilerproc``, so that we can
  #      query it's value here
  const strlitFlag = 1 shl (sizeof(int)*8 - 2)

  let seqType = pe.getCompilerType("TGenericSeq")

  for id, s in syms.msymbols:
    case s.kind
    of skConst:
      case types.kind(s.typ)
      of tnkString:
        let str = getStr(data, syms.data(id))
        s.typ = types.requestRecordType(base = seqType):
          (NoneDecl, types.requestArrayType(str.len.uint + 1,
                                            pe.sysTypes[tyChar]))

        # the two most-significant-bits of ``TGenericSeq.reserved`` are a
        # bitfield. The ``strlitFlag`` bit indicates that the seq in question
        # is a literal
        let tupl = data.startArray(3)
        data.addLit(tupl, data.newLit(str.len)) # length
        data.addLit(tupl, data.newLit(str.len or strlitFlag)) # reserved
        # TODO: don't create a full copy of the string here with just an
        #       additional zero-terminator at the end. Either add a new literal
        #       kind (e.g. `lkZtString`) or introduce the more general concept
        #       of a rope-like data structure into ``LiteralData``
        #       (e.g. ``conRope``)
        data.addLit(tupl, data.newLit(str & '\0')) # data

        syms.setData(id, data.finish(tupl))

      of tnkSeq:
        let
          lit = syms.data(id)
          len = data.len(lit)
          elemTyp = types.base(s.typ)

        s.typ = types.requestRecordType(base = seqType):
          (NoneDecl, types.requestArrayType(len.uint, elemTyp))

        let tupl = data.startArray(3)
        data.addLit(tupl, data.newLit(len)) # length
        data.addLit(tupl, data.newLit(len or strlitFlag)) # reserved
        data.addLit(tupl, lit) # data

        syms.setData(id, data.finish(tupl))

      else:
        discard

    else:
      discard

func incl(dst: var TBitSetView, data: LiteralData, lit: LiteralId) =
  for a, b in sliceListIt(data, lit):
    if a == b: bitSetIncl(dst, data.getInt(a))
    else:      bitSetInclRange(dst, data.getInt(a) .. data.getInt(b))

func setAsInt(env: TypeEnv, data: var LiteralData, id: TypeId, lit: LiteralId): LiteralId =
  ## Returns the set-literal `lit` represented as an unsigned integer if it
  ## fits into one. If it doesn't, `lit` is returned
  if env.length(id) <= 64:
    var arr: array[8, uint8]
    incl(arr, data, lit)

    var val: BiggestUInt
    for i in 0..7:
      val = val or (BiggestUInt(arr[i]) shl (i*8))

    result = data.newLit(val)

  else:
    result = lit

proc liftSetConsts*(syms: var SymbolEnv, data: var LiteralData, c: var ConstCache, name: PIdent, types: TypeEnv) =
  ## Lifts ``set`` literals part of constant data into their own
  ## constants. `name` is the name to use for the produced constants
  proc liftAtom(typ: TypeId, iter: var DataIter, syms: var SymbolEnv, c: var ConstCache, data: LiteralData, types: TypeEnv): bool =
    case types.kind(typ)
    of tnkSet:
      let lit = data.getLit(iter)
      # XXX: the case where the set can be represented by an int was
      #      previously optimized to not use a const. It did require a mutable
      #      `data` object however, which is why it's not done anymore. The
      #      downside is that the memory usage is higher (due to a larger
      #      amount of constants)
      let s = syms.addConst(c, name, typ, data.getLit(iter))

      replaceWithConst(iter, s.uint32)
      #[
      let val = setAsInt(types, data, typ, lit)
      if lit == val:
        # the set requires an array
        let s = syms.addSym(skConst, typ, name)
        syms.setData(s, val)

        replaceWithConst(iter, s.uint32)
      else:
        # the set can be represented via an integer - we don't need an extra
        # constant
        replaceWithLit(iter, val)
      ]#

      result = true
    else:
      discard

  for id, s in syms.msymbols:
    case s.kind
    of skConst:
      let lit = syms.data(id)

      if lit.kind != lkComplex:
        # we are only interested in complex data
        continue

      var iter = initDataIter(data, lit)
      moveInto(iter, data)
      liftFromStruct(s.typ, iter, syms, c, data, types, liftAtom)

      syms.setData(id, data.finish(iter))
    else:
      discard

func transformSetConsts*(pe: PassEnv, syms: var SymbolEnv, data: var LiteralData, types: TypeEnv) =
  ## Transforms the data for ``set`` constants into either byte arrays or - if
  ## they're small enough to fit - unsigned integers
  for id, s in syms.msymbols:
    case s.kind
    of skConst:
      if types.kind(s.typ) == tnkSet:
        let lit = syms.data(id)

        block:
          if (let tfrmed = setAsInt(types, data, s.typ, lit); tfrmed != lit):
            # a small set -> transformed into an unsigned integer
            syms.setData(id, tfrmed)
          else:
            # TODO: the size is wrong! It needs to be a multiple of the size of
            #       an ``int``
            var arr = addPackedArray[uint8](data, (types.length(s.typ) + 7) div 8)
            incl(arr.get, data, lit)

            syms.setData(id, data.finish(arr))

    else:
      discard

const hookPass* = LinearPass[HookCtx](visit: injectHooks)
const refcPass* = LinearPass2[RefcPassCtx](visit: applyRefcPass)
const seqV1Pass* = LinearPass2[RefcPassCtx](visit: lowerSeqsV1)
const seqV2Pass* = LinearPass[GenericTransCtx](visit: lowerSeqsV2)
const ofV1Pass* = LinearPass2[UntypedPassCtx](visit: lowerOfV1)
const seqConstV1Pass* = LinearPass2[LiftPassCtx](visit: liftSeqConstsV1)
const arrayConstPass* = LinearPass2[LiftPassCtx](visit: liftArrays)
const setConstPass* = LinearPass2[LiftPassCtx](visit: liftLargeSets)
const lowerRangeCheckPass* = LinearPass2[RefcPassCtx](visit: lowerRangeChecks)
const lowerSetsPass* = LinearPass2[RefcPassCtx](visit: lowerSets)