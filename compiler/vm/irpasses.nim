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
    bitsetutils,
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

  # XXX: both names are a bit awkward and it's not immediately clear what
  #      their meaning is
  compilerconsts*: Table[string, LiteralId] ## constants marked as ``.core``
                                            ## or ``.compilerproc``
  compilerglobals*: Table[string, SymId] ## globals marked as ``.compilerproc``

  attachedOps*: array[TTypeAttachedOp, Table[TypeId, ProcId]]

  sysTypes*: array[TTypeKind, TypeId]

func getCompilerProc*(g: PassEnv, name: string): ProcId =
  g.compilerprocs[name]

func getCompilerType*(g: PassEnv, name: string): TypeId =
  g.compilertypes[name]

func getSysType*(g: PassEnv, kind: TTypeKind): TypeId =
  g.sysTypes[kind]

type
  TypeFieldStatus* = enum
    tfsNone     ## no type field
    tfsHeader   ## only the object itself has a type header
    tfsEmbedded ## there are one or more sub-objects that have type headers
                ## and the object itself might also have a type header

  TypeFieldInfo* = seq[TypeFieldStatus]
    ## The status of if and in what form type-fields are present for each
    ## object type. ``none`` if the type is not a record or array type.
    # XXX: only 2 out of the 8 bit are used; a ``PackedSeq`` would make sense
    #      here

  TypeContext* = object
    ## Stores the type information for a procedure's body

    # TODO: introduce the ``IdMap[SomeId, T]`` type that's already mentioned
    #       elsewhere and use it here
    orig: seq[TypeId] ## ``IRIndex`` -> unmapped type of the expression
    mapped: seq[TypeId] ## ``IRIndex`` -> mapped type of the expression. A
      ## mapped type is the type after lowering/transformation.

  TypedPass*[T] = object
    # XXX: going just by the name, a ``TypedPass`` would be a pass that is
    #      typed, which doesn't match at all with what the type actually
    #      represents. A better name is needed
    # TODO: the ``IrEnv`` parameter must not be mutable. It currently has to be
    #       because ``IrEnv`` also stores the ``LiteralData`` object, which
    #       needs to be mutable in order for new literal data to be added.
    #       In order to stay forward-compatible, do **not** modify anything
    #       besides ``LiteralData`` in `env`
    when T is void:
      visit*: proc (code: IrStore3, types: TypeContext, env: var IrEnv,
                    cr: var IrCursor)
    else:
      visit*: proc (code: IrStore3, types: TypeContext, env: var IrEnv,
                    extra: T, cr: var IrCursor)

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

proc runPass*[T: not void](code: var IrStore3, types: TypeContext,
                           env: var IrEnv, extra: T, pass: TypedPass[T]) =
  ## Applies `pass` to the given `code`, directly merging the changes into
  ## `code`
  var cr: IrCursor
  cr.setup(code)

  for i in 0..<code.len:
    cr.setPos(i)
    # XXX: the change to not using the `nodes` iterator and looking up the
    #      pointed to node in the visit procedure increased the run time of
    #      the main passes by around 5%. That's quite a lot.
    pass.visit(code, types, env, extra, cr)

  code.update(cr)

proc runPass2*[T: not void](code: IrStore3, types: TypeContext, env: var IrEnv,
                           extra: T, diff: var Changes, pass: TypedPass[T]) =
  ## Applies `pass` to the given `code` and merges the changes into `diff`
  var cr: IrCursor
  cr.setup(code)

  for i in 0..<code.len:
    cr.setPos(i)
    pass.visit(code, types, env, extra, cr)

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


func `[]`*(x: TypeContext, i: IRIndex): TypeId {.inline.} =
  x.orig[i]

func real(x: TypeContext, i: IRIndex): TypeId {.inline.} =
  # TODO: rename
  x.mapped[i]

type
  TypeMap* = Table[TypeId, TypeId]
  # XXX: a specialized table implementation could be used for tables mapping
  #      an ID to something else (especially if we're treating the ID itself
  #      as the hash)

template computeTypesImpl(ir: IrStore3, env: IrEnv) =
  mixin asgnTo, get
  var i = 0
  for n in ir.nodes:
    case n.kind
    of ntkAsgn, ntkJoin, ntkGoto, ntkBranch, ntkContinue, ntkProc:
      discard
    of ntkCall:
      asgnTo(i):
        case n.callKind
        of ckBuiltin, ckMagic:
          # XXX: built-in calls feel wrong. Using magics instead might be better
          n.typ
        of ckNormal:
          let callee = ir.at(n.callee)
          if callee.kind != ntkProc:
            env.types.getReturnType(get(n.callee)) # the callee's return type
          else:
            env.procs.getReturnType(callee.procId)

    of ntkLit:
      asgnTo(i): ir.getLit(n).typ
    of ntkSym:
      let s = ir.sym(n)
      customAssert s != NoneSymbol, i
      asgnTo(i): env.syms[s].typ
    of ntkParam:
      asgnTo(i): env.procs.param(ir.owner, n.paramIndex).typ
    of ntkUse, ntkConsume:
      asgnTo(i): get(n.srcLoc)
    of ntkLocal:
      asgnTo(i): ir.getLocal(i).typ
    of ntkAddr:
      # XXX: completely wrong, but we're missing a way to get
      #      the correct type without creating a new one
      asgnTo(i): get(n.addrLoc)
    of ntkDeref:
      let t = get(n.addrLoc)
      customAssert env.types[t].kind in {tnkPtr, tnkRef, tnkVar, tnkLent}, i
      asgnTo(i): env.types.elemType(t)
    of ntkPathObj:
      let typ = get(n.srcLoc)
      customAssert typ != NoneType, n.srcLoc
      let idx = n.fieldIdx
      case env.types[typ].kind
      of tnkRecord:
        let f = env.types.nthField(typ, n.fieldIdx)
        asgnTo(i): env.types[f].typ
      else:
        customAssert false, n.srcLoc

    of ntkPathArr:
      let typ = env.types.skipVarOrLent(get(n.srcLoc))
      asgnTo(i): env.types.elemType(typ)

    of ntkConv, ntkCast:
      asgnTo(i): n.typ

    else:
      debugEcho "computeTypes missing: ", n.kind
    inc i

func computeTypes*(code: IrStore3, env: IrEnv): seq[TypeId] =
  template asgnTo(i: IRIndex, id: TypeId) =
    result[i] = id

  template get(i: IRIndex): TypeId =
    result[i]

  # TODO: don't return a new sequence but accept a mutable one instead (so
  #       that it's memory can be reused)
  result.newSeq(code.len)
  computeTypesImpl(code, env)

func computeTypes*(code: IrStore3, env: IrEnv, map: TypeMap): seq[TypeId] =
  ## Compute the type for each expression, applying the given `map` to the
  ## type of each sub-expression
  template asgnTo(i: IRIndex, id: TypeId) =
    result[i] = map.getOrDefault(id, id)

  template get(i: IRIndex): TypeId =
    result[i]

  result.newSeq(code.len)
  computeTypesImpl(code, env)

func newSeqReuse[T](x: var seq[T], size: Natural) {.inline.} =
  when defined(nimv2):
    # TODO: verify that this really works. The implementation of ``shrink``
    #       makes it seem like it doesn't
    x.newSeq(size)
  else:
    # zero out the occupied memory:
    x.setLen(0)
    # now resize to the requested size:
    x.setLen(size)

func computeTypes*(res: var TypeContext, code: IrStore3, env: IrEnv,
                   map: TypeMap) =
  ## Computes the type for each expression in `code`, applying the given `map`
  ## to the type of each sub-expression. The previous contents of `res` are
  ## overwritten
  template asgnTo(i: IRIndex, id: TypeId) =
    let
      v = i
      v2 = id
    res.orig[v] = v2
    res.mapped[v] = map.getOrDefault(v2, v2)

  template get(i: IRIndex): TypeId =
    res.mapped[i]

  res.orig.newSeqReuse(code.len)
  res.mapped.newSeqReuse(code.len)

  computeTypesImpl(code, env)


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
  extra: PassEnv

  tfInfo*: TypeFieldInfo
  gcLookup*: BitSet[TypeId]

func initTypeContext*(code: IrStore3, env: IrEnv): TypeContext =
  result.orig = computeTypes(code, env)

func initTypeContext*(code: IrStore3, env: IrEnv, map: TypeMap): TypeContext =
  assert map.len > 0
  result.computeTypes(code, env, map)

func setupRefcPass*(c: var RefcPassCtx, pe: PassEnv) =
  c.extra = pe

type StorageLoc = enum
  slUnknown
  slStack
  slHeap
  # TODO: also add `slStatic`, used for constants?

func storageLoc(c: RefcPassCtx, val: IRIndex): StorageLoc =
  # TODO: missing
  slUnknown

proc requestRtti(c: RefcPassCtx, cr: var IrCursor, t: TypeId): IRIndex =
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

# TODO: add and use a ``LocalId`` for naming locals
proc newTemp(cr: var IrCursor, pe: PassEnv, typ: TypeId): int =
  ## Creates a new zero-initialized temporary variable of the given `typ`
  ## and returns it's name
  result = cr.newLocal(lkTemp, typ)

  # XXX: temporaries should be specified to start as zero-initialized at the
  #      back-end code-IR level. The code-generators (or dedicated
  #      pre-code-generator transformations) are then responsible for
  #      initializing the temporaries to zero, which makes sense from a
  #      modularity perspective
  cr.insertCompProcCall(pe, "nimZeroMem", cr.insertAddr(cr.insertLocalRef(result)), cr.insertMagicCall(pe, mSizeOf, tyInt, cr.insertTypeLit(typ)))

proc processMagicCall(c: RefcPassCtx, cr: var IrCursor, ir: IrStore3, types: TypeContext, env: var IrEnv, m: TMagic, n: IrNode3) =
  ## Lowers calls to various magics into calls to `compilerproc`s
  template arg(i: Natural): IRIndex =
    ir.args(cr.position, i)

  case getMagic(ir, env, n)
  of mDestroy:
    # An untransformed `mDestroy` indicates a ref or string. `seq`
    # destructors were lifted into specialized procs already
    let val = arg(0)
    case env.types[types[val]].kind
    of tnkRef, tnkString:
      # XXX: only non-injected destroys for refs should be turned
      cr.replace()
      let nilLit = cr.insertNilLit(env.data, types[val])
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
      ptrTyp = env.types.skipVarOrLent(types[arg])
      # ``unsafeNew`` also uses the ``mNew`` magic, so we have to handle
      # that here
      size = if n.argCount > 1: arg(1) else: InvalidIndex

    cr.replace()
    # XXX: not sure about `askMove` here...
    genNewObj(cr, c.extra, env, ptrTyp, arg, size, c.storageLoc(arg))

  of mGCref, mGCunref:
    const op = [mGCref:   "nimGCref",
                mGCunref: "nimGCunref"]

    cr.replace()
    genIfNot(cr, cr.insertMagicCall(c.extra, mIsNil, tyBool, arg(0))):
      cr.insertCompProcCall(c.extra, op[m], arg(0))

  of mDefault:
    # TODO: move the lowering of ``mDefault`` to a separate pass
    cr.replace()
    let typ = ir.getLit(ir.at(arg(0)))[1]

    case env.types[typ].kind
    of tnkBool, tnkChar, tnkInt, tnkUInt:
      discard cr.insertLit(env.data, 0, typ)
    of tnkFloat:
      discard cr.insertLit(env.data, 0.0, typ)
    of tnkPtr, tnkRef, tnkProc, tnkCString:
      discard cr.insertNilLit(env.data, typ)
    of tnkString, tnkSeq:
      # XXX: only v1 seqs can be initialized with a 'nil' pointer. The
      #      transformation here should leave ``mDefault`` for them untouched
      #      and let the seq lowering pass take care of it
      discard cr.insertNilLit(env.data, typ)
    of tnkSet, tnkClosure:
      # XXX: the 'default' lowering for these should be moved to their
      #      respective lowering passes (``lowerSets`` and ``lowerClosures``)
      #      too
      discard cr.insertLocalRef(cr.newTemp(c.extra, typ))
    of tnkArray, tnkRecord:
      # a compound type
      let
        hdr = c.tfInfo[typ.toIndex]
        tmp = cr.newTemp(c.extra, typ)
        tmpAcc = cr.insertLocalRef(tmp)

      case hdr
      of tfsNone: discard
      of tfsHeader:
        # XXX: ``mAccessTypeField`` returns a ``ptr TNimType``, but we don't
        #      have access to that type here
        cr.insertAsgn(askInit, cr.insertMagicCall(c.extra, mAccessTypeField, tyPointer, tmpAcc), c.requestRtti(cr, typ))
      of tfsEmbedded:
        cr.insertCompProcCall(c.extra, "objectInit", cr.insertAddr tmpAcc, c.requestRtti(cr, typ))

      discard cr.insertLocalRef(tmp)

    of tnkVar, tnkLent:
      # XXX: allowed for now. Needs some further thought
      discard cr.insertNilLit(env.data, c.extra.sysTypes[tyPointer])

    of tnkOpenArray, tnkTypeDesc, tnkUncheckedArray, tnkVoid, tnkEmpty:
      # these don't have a default representation
      unreachable(env.types[typ].kind)

  else:
    discard "ignore"

proc genRefcRefAssign(cr: var IrCursor, e: PassEnv, dst, src: IRIndex, sl: StorageLoc) =
  # TODO: document
  case sl
  of slStack:
    cr.insertAsgn(askShallow, dst, src)
  of slHeap:
    cr.insertCompProcCall(e, "asgnRef", cr.insertAddr(dst), src)
  of slUnknown:
    cr.insertCompProcCall(e, "unsureAsgnRef", cr.insertAddr(dst), src)

func genAssignmentV1(cr: var IrCursor, c: RefcPassCtx, env: IrEnv, dst, src: IRIndex, typ: TypeId, loc: StorageLoc) =
  ## Generates the assignment logic between `dst` and `src` of the given `typ`,
  ## emitting the RTTI-based ``genericAssign`` if necessary
  # TODO: move the logic for string and seq into the ``lowerSeqsV1`` pass
  case env.types.kind(typ)
  of tnkString:
    cr.replace()
    genRefcRefAssign(cr, c.extra, dst, cr.insertCompProcCall(c.extra, "copyString", src), loc)
    # XXX: if `dst` is on the heap, this will result in the following code:
    #      ``asgnRef(copyString(...))``
    #      Which is inefficient because the string is added to the ZCT just
    #      to have it's refcount set to '1' immediately after. There already
    #      exists a ``copyStringRC1`` procedure specifically for use in this
    #      situation, but whether or not it can be used depends on the
    #      selected GC.
    #      A seperate pass that fuses ``asgnRef``+``copyString|newObj|newSeq``
    #      pairs into just ``copyStringRC1|newObjRC1|newSeqRC1`` might make
    #      sense (it'd be a trade of efficiency for modularity)
  of tnkSeq:
    # it's not obvious from the procedure's signature, but ``genericSeqAssign``
    # expects a ``ptr pointer`` as the first argument
    cr.replace()
    cr.insertCompProcCall(c.extra, "genericSeqAssign", cr.insertAddr(dst), src, c.requestRtti(cr, typ))
  of tnkRef:
    cr.replace()
    genRefcRefAssign(cr, c.extra, dst, src, loc)
  of tnkArray:
    if typ in c.gcLookup:
      # a ``genericAssign`` is only necessary if the array contains GC'ed
      # memory
      cr.replace()
      cr.insertCompProcCall(c.extra, "genericAssign", cr.insertAddr(dst), cr.insertAddr(src), c.requestRtti(cr, typ))

  of tnkRecord:
    # TODO: shallow semantics are not respected (i.e. objects marked as
    #       ``.shallow``)
    if c.tfInfo[typ.toIndex] == tfsHeader or typ in c.gcLookup:
      # we need a ``genericAssign`` if the record contains GC'ed memory or if
      # it has a type-header
      # TODO: the object type check logic should be removed from
      #       ``genericAssign`` and instead be executed separately. With how
      #       it currently is, the check doesn't respect ``optObjCheck`` and a
      #       ``genericAssign`` is forced even if the record doesn't contain
      #       GC'ed memory
      cr.replace()
      cr.insertCompProcCall(c.extra, "genericAssign", cr.insertAddr(dst), cr.insertAddr(src), c.requestRtti(cr, typ))

  of tnkClosure:
    # note: don't replace the assignment - it gets transformed into an
    #       assignment of just the procedure field during a later pass

    # closure objects have reference semantics
    genRefcRefAssign(cr, c.extra,
                     cr.insertMagicCall(c.extra, mAccessEnv, tyPointer, dst),
                     cr.insertMagicCall(c.extra, mAccessEnv, tyPointer, src),
                     loc)

  of tnkBool, tnkChar, tnkInt, tnkUInt, tnkFloat, tnkLent, tnkVar, tnkPtr, tnkProc,
     tnkCString, tnkOpenArray, tnkSet:
    # no special behaviour. A blit copy is enough
    discard
  of tnkEmpty, tnkVoid, tnkUncheckedArray, tnkTypeDesc:
    unreachable()

proc applyRefcPass(ir: IrStore3, types: TypeContext, env: var IrEnv, c: RefcPassCtx, cr: var IrCursor) =
  let n = ir[cr]
  case n.kind
  of ntkAsgn:
    case n.asgnKind
    of askMove:
      if env.types.kind(types[n.wrLoc]) in {tnkString, tnkRef, tnkSeq}:
        genRefcRefAssign(cr, c.extra, n.wrLoc, n.srcLoc, c.storageLoc(n.wrLoc))
        # XXX: source needs to be zeroed?
    of askCopy:
      genAssignmentV1(cr, c, env, n.wrLoc, n.srcLoc, types[n.wrLoc], c.storageLoc(n.wrLoc))
    of askInit, askShallow, askDiscr:
      # XXX: init might need special handling
      discard

  of ntkCall:
    processMagicCall(c, cr, ir, types, env, getMagic(ir, env, n), n)
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

proc genStrConcat(cr: var IrCursor, g: PassEnv, types: TypeContext, ir: IrStore3, env: var IrEnv, n: IRIndex): IRIndex =
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
    case env.types[types[arg]].kind
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
    case env.types[types[arg]].kind
    of tnkChar:   discard cr.insertCompProcCall(g, "appendChar", cr.insertLocalRef(tmp), arg)
    of tnkString: discard cr.insertCompProcCall(g, "appendString", cr.insertLocalRef(tmp), arg)
    else: unreachable()

  result = cr.insertLocalRef(tmp)

proc lowerSeqsV1(ir: IrStore3, types: TypeContext, env: var IrEnv, c: RefcPassCtx, cr: var IrCursor) =
  ## Lowers the `seq`-related magic operations into calls to the v1 `seq`
  ## implementation
  let n = ir[cr]
  case n.kind
  of ntkCall:
    template arg(i: Natural): IRIndex =
      ir.args(cr.position, i)

    case getMagic(ir, env, n)
    of mSetLengthStr:
      let
        dst = arg(0)
        len = arg(1)

      cr.replace()
      # TODO: don't process ``lowerSeqsV1`` in the same batch as the
      #       garbage-collector. An ``askCopy`` could be used here then
      #       instead of the ``genRefcRefAssign``
      genRefcRefAssign(cr, c.extra, dst, cr.insertCompProcCall(c.extra, "setLengthStr", dst, len), c.storageLoc(dst))
    of mSetLengthSeq:
      let
        dst = arg(0)
        len = arg(1)

      # TODO: same as the todo above
      cr.replace()
      genRefcRefAssign(cr, c.extra, dst, cr.insertCompProcCall(c.extra, "setLengthSeqV2", dst, len, c.requestRtti(cr, types[dst])), c.storageLoc(dst))

    of mNewSeq:
      cr.replace()

      let val = arg(0)
      let
        typ = types[val]
        nilLit = cr.insertNilLit(env.data, typ)

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

        var ns = cr.insertCompProcCall(c.extra, "newSeq", c.requestRtti(cr, typ), arg(1))
        ns = cr.insertCast(typ, ns)
        cr.insertAsgn(askShallow, val, ns)
      of slStack:

        var ns = cr.insertCompProcCall(c.extra, "newSeq", c.requestRtti(cr, typ), arg(1))
        ns = cr.insertCast(typ, ns)
        cr.insertAsgn(askShallow, val, ns)

    of mNewSeqOfCap:
      cr.replace()

      let typ = types[cr.position]
      discard cr.insertCast(typ, cr.insertCompProcCall(c.extra, "nimNewSeqOfCap", c.requestRtti(cr, typ), arg(0)))

    of mArrToSeq:
      # XXX: passing a non-constant array construction expression as the
      #      argument will produce rather inefficient code, but we can't do
      #      much about it here because of how an ``nkBracket`` is lowered
      #      during ``irgen``
      cr.replace()

      let
        seqTyp = types[cr.position]
        arrTyp = types[arg(0)]

      let
        elemCount = cr.insertLit(env.data, env.types.length(arrTyp))
        tmp = cr.newLocal(lkTemp, seqTyp)

      # TODO: this transformation shoud likely happen in a pass before
      #       seqs are lowered (maybe in ``irgen``)

      # XXX: if we'd know about the destination location, we could use
      #      ``newSeqRC1`` if the destination is on the heap

      # construct the seq
      cr.insertAsgn(askInit, cr.insertLocalRef(tmp), cr.insertCast(seqTyp, cr.insertCompProcCall(c.extra, "newSeq", c.requestRtti(cr, seqTyp), elemCount)))

      # TODO: don't emit a loop if the source array is empty
      # TODO: maybe add back the small loop unrolling?
      cr.genForLoop(env.data, c.extra, elemCount):
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
      let typ = types[seqVal]#.skipTypes({tyVar})

      # XXX: if the refc pass would be run after the `lowerSeqV1` pass, a
      #      `askMove` assignment could be used here instead
      cr.genRefcRefAssign(c.extra, seqVal, cr.insertCast(typ, cr.insertCompProcCall(c.extra, "incrSeqV3", seqVal, c.requestRtti(cr, typ)) ), c.storageLoc(seqVal))

      # TODO: filling the element and adjusting the seq length is missing
      let seqLen = cr.accessSeqField(ir, seqVal, SeqV1LenField)
      let tmp = cr.genTempOf(seqLen, c.extra.sysTypes[tyInt])

      cr.insertAsgn(askCopy, seqLen, cr.insertMagicCall(c.extra, mAddI, tyInt, cr.insertLocalRef(tmp), cr.insertLit(env.data, 1)))
      # the value is a sink parameter so we can use a move
      # XXX: we're running after the inject-hook pass, so we either need to
      #      reorder the passes or manually insert a hook call here
      cr.insertAsgn(askMove, cr.insertPathArr(cr.accessSeqField(ir, seqVal, SeqV1DataField), cr.insertLocalRef(tmp)), arg(1))

    of mAppendStrStr:
      # -->
      #   resizeString(lhs, len(rhs))
      #   appendString(lhs, rhs)
      cr.replace()

      let lenTmp = genSeqLen(cr, env.data, c.extra, ir, arg(1))
      cr.insertCompProcCall(c.extra, "resizeString", arg(0), lenTmp)
      cr.insertCompProcCall(c.extra, "appendString", arg(0), arg(1))

    of mAppendStrCh:
      # -->
      #   str = addChar(str, c)

      cr.replace()
      let strVal = arg(0)
      let tmp = cr.genTempOf(strVal, types[strVal])

      cr.genRefcRefAssign(c.extra, strVal, cr.insertCompProcCall(c.extra, "addChar", cr.insertLocalRef(tmp), arg(1)), c.storageLoc(strVal))

    of mConStrStr:
      cr.replace()
      discard genStrConcat(cr, c.extra, types, ir, env, cr.position)

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
        if   isEmptyStr(ir, env.data, a): b
        elif isEmptyStr(ir, env.data, b): a
        else: InvalidIndex

      if nonEmptyIdx == InvalidIndex:
        # both operands are not statically empty
        cr.insertCompProcCall(c.extra, "eqStrings", a, b)
      else:
        # we still call ``len`` for an empty string in the case that both
        # operands are empty strings, but since that case is highly unlikely,
        # it doesn't get special handling
        discard cr.binaryBoolOp(c.extra, mEqI, genSeqLen(cr, env.data, c.extra, ir, nonEmptyIdx), cr.insertLit(env.data, 0))

    of mLeStr, mLtStr:
      # same implementation for v1 and v2
      discard "lowered later"

    of mLengthStr:
      case env.types[types[arg(0)]].kind
      of tnkString:
        cr.replace()
        discard genSeqLen(cr, env.data, c.extra, ir, arg(0))
      of tnkCString:
        discard "transformed later"
      else:
        unreachable()

    of mLengthSeq:
      cr.replace()
      discard genSeqLen(cr, env.data, c.extra, ir, arg(0))

    else:
      discard

  of ntkPathArr:
    let arrTyp = types[n.srcLoc]

    # TODO: needs tests
    case env.types[arrTyp].kind#skipTypes(arrTyp, {tyVar, tyLent}).kind
    of tnkString, tnkSeq:
      # -->
      #   x[].data[idx] # if not a const
      #   x.data[idx]   # if a cosnt

      cr.replace()
      var r = cr.insertDeref(cr.accessSeq(ir, env, n.srcLoc, arrTyp))
      # a `lent seq` is not a treated as a `ptr NimSeq` but just as `NimSeq`
      # (`NimSeq` itself is a pointer type)
      if env.types[arrTyp].kind == tnkVar:
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

    if c.env.syms[s].kind == skConst:
      case c.env.types.kind(typ)
      of tnkString, tnkSeq:
        # see the documentation of the ``seq``-literal lifting for the reason
        # behind this transformation
        # XXX: strictly speaking, this transformation is not part of the actual
        #      lifting. Moving it to a separate pass does seem a bit overkill
        #      however
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

func genSetOp(ir: IrStore3, pe: PassEnv, env: var IrEnv, setType: TypeId, m: TMagic, n: IrNode3, cr: var IrCursor) =
  # TODO: sets with ``firstOrd != 0`` aren't taken into account yet.
  #       ``irgen`` has to insert the required adjustment
  let len = env.types.length(setType)

  case len
  of 0..64:
    case m
    of mEqSet, mMulSet, mPlusSet, mMinusSet:
      discard genSetElemOp(cr, pe, m, setType, ir.argAt(cr, 0), ir.argAt(cr, 1))
    of mLeSet, mLtSet:
      discard genSubsetRelOp(setType, ir.argAt(cr, 0), ir.argAt(cr, 1), testTrue=(m == mLtSet), pe, env.data, cr)
    of mInSet:
      let mask = genMaskExpr(setType, len, ir.argAt(cr, 1), env.data, cr)
      discard cr.insertCallExpr(mBitandI, setType, ir.argAt(cr, 0), mask)
    of mIncl, mExcl:
      # mIncl --->
      #   a = a or (1 shl b)
      # mExcl --->
      #   a = a and not(1 shl b)
      let
        a = ir.argAt(cr, 0)
        b = ir.argAt(cr, 1)
        mask = genMaskExpr(setType, len, b, env.data, cr)

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

      cr.insertCompProcCall(pe, prc, ir.argAt(cr, 0))
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
      cr.genForLoop(env.data, pe, cr.insertLit(env.data, (len + 7) div 8)):
        let v = genSetElemOp(cr, pe, m, pe.sysTypes[tyUInt8], cr.insertPathArr(arg0, counter), cr.insertPathArr(arg1, counter))
        cr.insertAsgn(askInit, cr.insertPathArr(cr.insertLocalRef(res), counter), v)

      discard cr.insertLocalRef(res)

    of mEqSet, mLeSet, mLtSet:
      let
        arg0 = ir.argAt(cr, 0)
        arg1 = ir.argAt(cr, 1)
        res = cr.newLocal(lkTemp, pe.getSysType(tyBool))

      # start with ``res = true``
      cr.insertAsgn(askInit, cr.insertLocalRef(res), cr.insertLit(env.data, 1))

      # iterate over all bytes in the set and perform the comparison:
      cr.genForLoop(env.data, pe, cr.insertLit(env.data, (len + 7) div 8)):
        let
          a = cr.insertPathArr(arg0, counter)
          b = cr.insertPathArr(arg1, counter)

        let v =
          case m
          of mEqSet:         cr.binaryBoolOp(pe, mEqI, a, b)
          of mLtSet, mLeSet: genSubsetRelOp(pe.sysTypes[tyUInt8], a, b, m == mLtSet, pe, env.data, cr)
          else:              unreachable()

        # if the comparison for the partial sets doesn't succeed, set the result
        # to false and exit the loop
        cr.genIfNot(v):
          cr.insertAsgn(askCopy, cr.insertLocalRef(res), cr.insertLit(env.data, 0))
          cr.insertGoto(loopExit)

      discard cr.insertLocalRef(res)

    of mInSet:
        let uintTyp = pe.getSysType(tyUInt)
        let conv = cr.insertConv(uintTyp, ir.argAt(cr, 1))
        let
          index = cr.insertCallExpr(mShrI, uintTyp, conv, cr.insertLit(env.data, 3))
          mask = cr.insertCallExpr(mShlI, uintTyp, cr.insertLit(env.data, 1), cr.insertCallExpr(mBitandI, uintTyp, conv, cr.insertLit(env.data, 7)))

        let val = cr.insertCallExpr(mBitandI, pe.sysTypes[tyUInt8], cr.insertPathArr(ir.argAt(cr, 0), index), mask)
        discard cr.insertMagicCall(pe, mNot, tyBool, cr.binaryBoolOp(pe, mEqI, val, cr.insertLit(env.data, 0)))

    of mIncl, mExcl:
      # mIncl --->
      #   a[b shr 2] = a[b shr 2] or (1 shl (b and 7))
      # mExcl --->
      #   a[b shr 2] = a[b shr 2] and not(1 shl (b and 7))

      let
        a = ir.argAt(cr, 0)
        b = ir.argAt(cr, 1)
        uintTy = pe.sysTypes[tyUInt]
        dest = cr.insertPathArr(a, cr.insertCallExpr(mShrI, uintTy, b, cr.insertLit(env.data, 3)))
        bit = cr.insertCallExpr(mShlI, uintTy, cr.insertLit(env.data, 1), cr.insertCallExpr(mBitandI, uintTy, b, cr.insertLit(env.data, 7)))

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
        arrTyp = env.types.lookupGenericType(tnkUncheckedArray, pe.sysTypes[tyUInt8])
        ptrTyp = env.types.lookupGenericType(tnkPtr, arrTyp)

      cr.insertCompProcCall(pe, "cardSetPtr", cr.insertCast(ptrTyp, cr.insertAddr(ir.argAt(cr, 0))), cr.insertLit(env.data, size))

    else:
      unreachable()

func setAsInt(env: TypeEnv, data: var LiteralData, id: TypeId, lit: LiteralId): LiteralId

proc lowerSets*(ir: IrStore3, types: TypeContext, env: var IrEnv, pe: PassEnv,
                cr: var IrCursor) =
  ## Lowers ``set`` operations into bit operations. Intended for the C-like
  ## targets
  # XXX: some set lowerings could be simplified by adding their resulting
  #      logic as ``.compilerprocs`` to ``system`` and then integrating them
  #      via ``cr.inline`` here
  let n = ir[cr]
  case n.kind
  of ntkCall:
    let m = getMagic(ir, env, n)

    case m
    of mIncl, mExcl, mCard, mEqSet, mLeSet, mLtSet, mMulSet, mPlusSet, mMinusSet, mInSet:
      cr.replace()
      genSetOp(ir, pe, env, types[ir.argAt(cr, 0)], m, n, cr)
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
    let typ = env.types[lit.typ]
    if typ.kind == tnkSet and typ.length <= 64:
      let newLit = setAsInt(env.types, env.data, lit.typ, lit.val)

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

proc lowerRangeChecks*(ir: IrStore3, types: TypeContext, env: var IrEnv, pe: PassEnv, cr: var IrCursor) =
  ## Lowers ``bcRangeCheck`` (nkChckRange, nkChckRangeF, etc.) into simple comparisons
  # XXX: the lowering could be simplified by just replacing the range check
  #      with a call to a ``chkRange`` inline function that'd be defined in
  #      ``system.nim`` for the C-like targets
  let n = ir[cr]

  case n.kind
  of ntkCall:
    if n.isBuiltIn and n.builtin == bcRangeCheck:
      let
        val = ir.argAt(cr, 0)
        lower = ir.argAt(cr, 1)
        upper = ir.argAt(cr, 2)

      let srcTyp = types[val]

      cr.replace()
      var cond: IRIndex
      var raiser: string

      case env.types[srcTyp].kind
      of tnkInt: # tyUInt, tyUInt64:
        # .. code:: nim
        #   cast[dstTyp](high) < val
        cond = cr.binaryBoolOp(pe, mLtU, cr.insertCast(srcTyp, upper), val)
        raiser = "raiseRangeErrorNoArgs"
      else:
        let dstTyp = types[cr.position]#skipTypes(c.typeof(cr.position), abstractVarRange)
        case env.types[dstTyp].kind
        of tnkInt: #tyUInt8..tyUInt32, tyChar:
          raiser = "raiseRangeErrorU"
        of tnkFloat: #tyFloat..tyFloat128:
          raiser = "raiseRangeErrorF"
          let conv = cr.insertConv(dstTyp, val)
          # no need to lower the `or` into an `ntkBranch` + `ntkJoin` here; it has no impact on further analysis
          cond = cr.binaryBoolOp(pe, mOr, cr.binaryBoolOp(pe, mLtF64, conv, lower), cr.binaryBoolOp(pe, mLtF64, upper, conv))

        else:
          cr.insertError(env.data, "missing chkRange impl")

        raiser =
          case env.types[types[cr.position]].kind#skipTypes(c.typeof(cr.position), abstractVarRange).kind
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
        cr.insertBranch(cr.insertMagicCall(pe, mNot, tyBool, cond), target)
        cr.insertCompProcCall(pe, raiser, val, lower, upper)
        # XXX: it would be nice if we could also move the following
        #      ``if bcTestError(): goto error`` into the branch here

        cr.insertJoin(target)
        discard cr.insertConv(dstTyp, val)

  else:
    discard


const OpenArrayDataField = 0
const OpenArrayLenField = 1

type LowerOACtx* = object
  graph: PassEnv
  env: ptr IrEnv

  types*: seq[TypeId]
  paramMap: seq[uint32] ## maps the current parameter indices to the new ones

func init*(x: var LowerOACtx, g: PassEnv, env: ptr IrEnv) =
  x.graph = g
  x.env = env

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
const refcPass* = TypedPass[RefcPassCtx](visit: applyRefcPass)
const seqV1Pass* = TypedPass[RefcPassCtx](visit: lowerSeqsV1)
const seqV2Pass* = LinearPass[GenericTransCtx](visit: lowerSeqsV2)
const ofV1Pass* = LinearPass2[UntypedPassCtx](visit: lowerOfV1)
const seqConstV1Pass* = LinearPass2[LiftPassCtx](visit: liftSeqConstsV1)
const arrayConstPass* = LinearPass2[LiftPassCtx](visit: liftArrays)
const setConstPass* = LinearPass2[LiftPassCtx](visit: liftLargeSets)
const lowerRangeCheckPass* = TypedPass[PassEnv](visit: lowerRangeChecks)
const lowerSetsPass* = TypedPass[PassEnv](visit: lowerSets)
const lowerOpenArrayPass* = LinearPass2[LowerOACtx](visit: lowerOpenArrayVisit)