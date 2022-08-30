## A temporary module to hold some currently unused code. The code here is
## for the most part outdated and obsolete.

import std/[tables, intsets]

import compiler/ast/[ast_types, ast_query, reports, lineinfos]
import compiler/front/[msgs]
import compiler/vm/[vmdef, vmtypegen, vmir]


## --------------- code previously located in ``vmir.nim``:

type VmTypeId = uint32

type
  HandleInfo = object
    root: int32
    sub: uint32

  Path = object
    src: IRIndex
    sub: seq[uint32]
    typ: VmTypeId

  PathIndex = #[distinct]# IRIndex

  IrNodeKindTwo = enum
    inktGlobal
    inktConstLoc
    inktProc
    inktConst
    inktImm
    inktCursor
    inktPathArr
    inktPathObj
    inktPathCall
    inktLd
    inktWr
    inktLdDeref
    inktWrDeref
    inktCommit ## links a call expression with barriers
    inktBarrier
    inktOp
    inktStmt
    inktCallExpr
    inktCallStmt

    inktAddr

    inktTemp
    inktLocal
    inktParam
    inktJoinFwd
    inktJoin

    inktBranch
    inktGoto

  # XXX: structure is temporary
  IrNode2 = object
    case kind: IrNodeKindTwo
    of inktLd, inktLdDeref:
      ldSrc: IRIndex
      srcVmTyp: VmTypeId
      srcTyp: TTypeKind
    of inktWr, inktWrDeref:
      wrDst, wrSrc: IRIndex
      dstVmTyp: VmTypeId
      dstTyp: TTypeKind

    of inktPathObj:
      field: uint16
      objSrc: PathIndex
    of inktPathArr:
      arrSrc: PathIndex
      idx: IRIndex
    of inktImm:
      immediate: uint32
    of inktConst:
      litId: uint32
    of inktGlobal, inktProc, inktConstLoc:
      linkIndex: uint32

    of inktOp, inktStmt:
      opc: TOpcode
      opArgs: seq[IRIndex]

    of inktCallExpr, inktCallStmt, inktPathCall:
      hasSideEffect: bool
      name: IRIndex
      args: seq[IRIndex]

    of inktCommit:
      call: IRIndex
      barriers: seq[IRIndex]

    of inktTemp, inktLocal:
      tmpName: uint32
      typ: VmTypeId

    of inktParam:
      param: uint32

    of inktJoinFwd:
      fwd: JoinPoint

    of inktBranch:
      branchOp: TOpcode
      cond: IRIndex
      target: JoinPoint

    of inktGoto:
      gotoOp: TOpcode
      gotoTarget: JoinPoint
    of inktAddr:
      addrLoc: IRIndex

    else:
      discard

  IrStore* = object
    handles: seq[HandleInfo]
    paths: seq[Path]
    ops: seq[IrNode2]

    nextTemp: uint32

  CodeFragment* = object
    code*: seq[TInstr]
    debug*: seq[TLineInfo]

  IrGenError = object of ValueError
    report: SemReport

  InstrInfo = object
    ## Description of an instrunction representing a magic
    isInOut: bool ## whether the output is also an input
    hasResult: bool
    isBx: bool
    hasImm: bool

  GenState* = object
    regs: seq[bool]
    nodeRegs: seq[TRegister]

    types {.cursor.}: seq[PVmType]

const InstrInfos: array[TOpcode, InstrInfo] = default(array[TOpcode, InstrInfo])

func fail(info: TLineInfo, kind: ReportKind;
          loc = instLoc()) {.noreturn, noinline.} =
  var report = SemReport(kind: kind, location: some(info),
                         reportInst: toReportLineInfo(loc))
  raise (ref IrGenError)(report: report)


template missingImpl*() = assert false


# version 1


#func irConst(c: var TCtx, litIdx: int): IRIndex =
#  discard

#func irAddCall(c: var TCtx, i: IRIndex, args: seq[IRIndex]) =
#  discard

#func irAddCallInd(c: var TCtx, i: IRIndex) =
#  discard

func irMoved*(c: var IrStore, i: IRIndex): IRIndex =
  missingImpl()
#[
func irObjAcc*(c: var TCtx, s: IRIndex, f: int): IRIndex = discard
func irArrAcc*(c: var TCtx, s: IRIndex, i: IRIndex): IRIndex = discard
func irWrObj*(c: var TCtx, d: IRIndex, f: int, v: IRIndex): IRIndex = discard
func irWrArr*(c: var TCtx, d, i, v: IRIndex): IRIndex = discard
func irWrLoc*(c: var TCtx, d: IRIndex, s: IRIndex): IRIndex = discard
]#
#func irGlobal(c: var TCtx, linkIndex: uint32): IRIndex = discard


#[
func irDep*(c: var TCtx, val: IRIndex, stmt: IRIndex): IRIndex =
  ## encodes `val` having a control-flow dependency on `stmt`
  discard
]#

#func irOp(c: var TCtx, op: TOpcode, x: varargs[IRIndex]): IRIndex = discard
#func irOpMut*(c: var TCtx, op: TOpcode, x: varargs[IRIndex]): IRIndex = discard


#func irPhony*(c: var TCtx, x: varargs[IRIndex]): IRIndex =
#  discard

#func irFork*(c: var TCtx): IRIndex =
#  discard

#func irPred*(c: var TCtx): IRIndex =
#  discard

#func irJoin*(c: var TCtx, paths: varargs[IRIndex]): IRIndex =
#  discard

#func irSetCf*(c: var TCtx, p: IRIndex) = discard

#func irOfBranch*(c: var TCtx, op: TOpcode, lit: int): IRIndex = discard
#func irBranch*(c: var TCtx, op: TOpcode, i: IRIndex): IRIndex = discard
#func irBranch*(c: var TCtx, op: TOpcode, a, b: IRIndex): IRIndex = missingImpl()


func add(x: var IrStore, n: sink IrNode2): IRIndex =
  result = x.ops.len.IRIndex
  x.ops.add n


proc irGlobal*(c: var IrStore, linkIndex: uint32): IRIndex =
  ## A global
  c.add(IrNode2(kind: inktGlobal, linkIndex: linkIndex))

proc irConstLoc*(c: var IrStore, linkIndex: uint32): IRIndex =
  ## A complex constant
  c.add(IrNode2(kind: inktConstLoc, linkIndex: linkIndex))

func irProc*(c: var IrStore, linkIndex: uint32): IRIndex =
  ## A procedure
  c.add(IrNode2(kind: inktProc, linkIndex: linkIndex))

func irTemp*(c: var IrStore, typ: VmTypeId): IRIndex =
  ## Introduces a new temporary with a unique name
  result = c.add(IrNode2(kind: inktTemp, tmpName: c.nextTemp, typ: typ))
  inc c.nextTemp

proc irConst*(c: var IrStore, i: uint32): IRIndex =
  ## Load a simple constant with the given literal index `i`
  c.add(IrNode2(kind: inktConst, litId: i))

func irCursor*(c: var IrStore, i: IRIndex): IRIndex =
  ## Introduce a cursor (i.e. a shallow copy) of `i`
  missingImpl()

proc irLd*(c: var IrStore, path: PathIndex#[, tk: TTypeKind, typ: VmTypeId]#): IRIndex =
  ## Represents the beginning of lifetime of a loaded location
  c.add(IrNode2(kind: inktLd, ldSrc: path#[, srcTyp: tk, srcVmTyp: typ]#))

proc irLdDeref*(c: var IrStore, handle: IRIndex): IRIndex =
  ## Dereference a pointer, reference or ``var``/``lent``
  missingImpl()


proc irWrDeref*(c: var IrStore, handle: IRIndex): IRIndex =
  ## Write through a dereference of a pointer, ref, or ``var``/``lent``
  missingImpl()

proc irBarrier*(c: var IrStore, path: PathIndex): IRIndex =
  ## Read (maybe also write?) barrier. A load must not be moved across a
  ## barrier
  missingImpl()

proc irOp*(c: var IrStore, opc: TOpcode, args: varargs[IRIndex]): IRIndex =
  ## An operation that takes input and produces an output without side-effects
  c.add(IrNode2(kind: inktOp, opc: opc, opArgs: @args))

proc irStmt*(c: var IrStore, opc: TOpcode, args: varargs[IRIndex]): IRIndex {.discardable.} =
  ## Can't be reordered with other statements. The returned IRIndex must not
  ## be used in a value or location context
  c.add(IrNode2(kind: inktStmt, opc: opc, opArgs: @args))

proc irCallExpr*(c: var IrStore, name: IRIndex, noSideEffect: bool, args: varargs[IRIndex]): IRIndex =
  ## A call producing a value. Can only be reordered with other calls if both have no side-effects
  c.add(IrNode2(kind: inktCallExpr, name: name, hasSideEffect: not noSideEffect, args: @args))

proc irCallPathExpr*(c: var IrStore, name: IRIndex, noSideEffect: bool, args: varargs[IRIndex]): IRIndex =
  ## A call producing a handle. Acts as a path constructor. Can only be
  ## reordered with other calls if both have no side-effects
  c.add(IrNode2(kind: inktPathCall, name: name, hasSideEffect: not noSideEffect, args: @args))

proc irCallStmt*(c: var IrStore, name: IRIndex, noSideEffect: bool, args: varargs[IRIndex]): IRIndex =
  ## A call producing nothing. Acts as a path constructor. Call has no side-effects
  c.add(IrNode2(kind: inktCallStmt, name: name, hasSideEffect: not noSideEffect, args: @args))


func irNull*(c: var IrStore, typ: PType): IRIndex =
  ## The zero representation for `typ`
  missingImpl()

func isGoto*(c: IrStore, p: IRIndex): bool {.inline.} =
  missingImpl()


# TODO: not related to the IR. Might need a better home
proc append*(dst: var CodeFragment, f: CodeFragment) =
  ## Appends the code from `f` to `dst`. The code doesn't need to be adjusted,
  ## since all jumps are relative
  dst.code.add(f.code)
  dst.debug.add(f.debug)


# ------ optimizer

#[
func run*(s: var IrStore) =
  ## Calculate lifetimes and aliases

  type CfgNode = object
    isLoop: bool
    prev: seq[ref CfgNode]
    next: seq[ref CfgNode]

  var start = (ref CfgNode)()
  var curr = start

  var i = 0
  var joins: Table[IRIndex, ref CfgNode]
  var loads: seq[(IRIndex, ref CfgNode, int)]
  var stores: seq[(IRIndex, ref CfgNode, int)]
  var roots: seq[int]
  var locs: seq[tuple[isAlias: bool, others: seq[int], root: int]]
  var uses: seq[(IRIndex, ref CfgNode, int)]
  var locMap: Table[IRIndex, int]

  roots.add(0) # root 0 = params
  roots.add(1) # root 1 = globals
  roots.add(2) # root 2 = constants

  proc addLoc(p: IRIndex, root: int) =
    let loc = locs.len
    locs.add((false, @[], root))
    locMap[p] = loc

  while i < s.ops.len:
    let n = s.ops[i]
    case n.kind
    of inktParam:
      addLoc(i, 0)
    of inktGlobal:
      addLoc(i, 1)
    of inktConstLoc:
      addLoc(i, 2)
    of inktTemp, inktLocal:
      let root = roots.len
      roots.add(0)
      addLoc(i, root)
    of inktLd:
      loads.add((i, curr, locMap[n.ldSrc]))
      uses.add((i, curr, locMap[n.ldSrc]))

    of inktWr:
      stores.add((i, curr, locMap[n.wrDst]))
      uses.add((i, curr, locMap[n.wrDst]))

    of inktJoinFwd:
      joins[n.fwd] = (ref CfgNode)()

    of inktJoin:
      if i notin joins:
        curr = (ref CfgNode)()
        joins[i] = curr
      else:
        curr = joins[i]

    of inktBranch:
      # XXX: is there always a i + 1?
      if s.ops[i+1].kind != inktJoin:
        let newNode = new(CfgNode)
        curr.next.add(newNode)
        newNode.prev.add(curr)
        curr = newNode

    of inktGoto:
      let target = if s.ops[n.gotoTarget].kind == inktJoinFwd: s.ops[n.gotoTarget].fwd else: n.gotoTarget
      if target < i:
        # XXX: meh
        joins[target].isLoop = true

      joins[target].prev.add(curr)
      curr.next.add(joins[target])
    else:
      discard "ignore"

    inc i
]#

func run*(s: var IrStore) =
  # TODO: improve documentation

  var writes: seq[tuple[loc: int]]
  var targets: seq[int] # add targets for handles
  var trustDiff: seq[(int, bool)]
  var loadDiff: seq[(int, bool)]
  var aliasDiff: seq[(IRIndex)]
  var frames: seq[tuple[td, ld: Slice[int]]] # the diff frames

  var tsTD: int # thread-start load diff
  var tsLD: int # thread-start trust diff

  var roots: seq[int]
  var locs: seq[tuple[isAlias: bool, others: seq[int], root: int]]

  var locMap: Table[IRIndex, int] # index -> index into locs
  var handleMap: Table[IRIndex, int] # index -> index into locs

  var trusted: seq[bool] ## root index -> trust state
  # TODO: should be seq:
  var loaded: IntSet ## for each path index -> loaded state

  var joinPoints: Table[IRIndex, seq[int]] ## joint point -> list of indices into `frames`
  var nextThreads: seq[IRIndex] # XXX: a circular buffer would make sense here

  const UniqueRoot = 0 ## the unique root is used for things that can never
                       ## alias. Temporaries use the unique root for example

  var paramRoots: seq[int]

  # TODO: try to make `localRoots` a seq
  var localRoots: Table[IRIndex, int] # local index -> root index

  proc root(s: IrStore, p: IRIndex): int =
    # TODO: better doc
    ## Gets the currently active root for the given location
    let n = s.ops[p]
    case n.kind
    of inktTemp:
      UniqueRoot
    of inktCallExpr, inktOp:
      # the return values of call expressions are temporaries
      UniqueRoot
    of inktPathObj:
      s.root(n.objSrc)
    of inktPathArr:
      s.root(n.arrSrc)
    of inktPathCall:
      # for path calls (functions returning views) the returned handle is always derived from the first argument
      s.root(n.args[0])
    of inktLocal:
      localRoots[p]
    of inktConstLoc:
      # constants can never be written too
      UniqueRoot
    else:
      unreachable(n.kind)

  proc addRoot(trust: bool): int =
    result = roots.len
    trusted.add(trust)

  proc isTrusted(root: int): bool =
    trusted[root]

  var addList: seq[(IRIndex, IrNode2)]
  var delList: seq[IRIndex]

  proc validate(root: int) =
    if not trusted[root]:
      # TODO: insert `irVerify`
      trusted[root] = true
      trustDiff.add((root, false)) # add the inversion to the diff

  var i = 0
  while i < s.ops.len:
    let n = s.ops[i]
    case n.kind
    of inktLocal:
      # setup the root. Locals start as trusted
      localRoots[i] = addRoot(trust=true)
    of inktLd:
      let r = s.root(n.ldSrc)
      if not isTrusted(r):
        # the root is not trusted -> validate the root
        validate(r)

      if n.ldSrc in loaded:
        # the load can be removed
        delList.add(i)
      else:
        # source is not loaded yet
        loadDiff.add((n.ldSrc, false))
        loaded.incl n.ldSrc

    of inktWr:
      let r = s.root(n.wrDst)
      if not isTrusted(r):
        # the root is not trusted
        validate(r)


    of inktAddr:
      # treat addr as an imperative statement
      discard


    of inktBranch:
      let target = if s.ops[n.target].kind == inktJoinFwd: s.ops[n.target].fwd else: n.target
      # execute the branch not taken case (i.e. fallthrough) first
      nextThreads.add(target)

      # start a new thread
      tsTD = trustDiff.len
      tsLD = loadDiff.len

    of inktGoto:
      # FIXME: loops will cause an infinite loop here
      # TODO: check how loop end fare
      let target = if s.ops[n.gotoTarget].kind == inktJoinFwd: s.ops[n.gotoTarget].fwd else: n.gotoTarget
      # end of thread
      # TODO: clean up thread
      frames.add((td: tsTD..trustDiff.high, ld: tsLD..loadDiff.high))
      joinPoints.mgetOrPut(target, @[]).add(frames.high)

      # TODO: improve the next == `target` case

      # TODO: use a binary search here
      var p = -1
      for j, t in nextThreads.pairs:
        if target <= t:
          p = j
          break

      if p == -1:
        nextThreads.add(target)
      elif target != nextThreads[p]:
        nextThreads.insert(target, p)

      debugEcho "next threads: ", target
      # goto join point
      i = nextThreads[0]
      debugEcho "what: ", i
      nextThreads.delete(0)

      # start a new thread
      tsTD = trustDiff.len
      tsLD = loadDiff.len

      debugEcho "goto: ", i
      debugEcho "next threads: ", nextThreads

      dec i

    else:
      discard "not relevant"


    inc i


func optimize(s: var IrStore) =
  discard


# ------- end

#func runAlias*(s: IrStore) =



#[
func populateHandles(s: var IrStore) =
  s.handles.newSeq(s.ops.len)
  for i, x in s.ops.pairs:
    case x.kind
    of irkPath:
      if x.isDyn:
        s.handles[i] = HandleInfo(root: x.parent.int32, sub: high(uint32))
      else:
        s.handles[i] = HandleInfo(root: x.parent.int32, sub: x.sub.uint32)
    of irkDeref:
      s.handles[i] = s.handles[x.ds]
    of irkAddr:
      s.handles[i] = s.handles[x.aso]
    else:
      discard
]#

template getT(c: TCtx, id: VmTypeId): PVmType =
  discard


func freeReg(gs: var GenState): TRegister =
  for i, r in gs.regs.mpairs:
    if not r:
      r = true
      return TRegister(i)

  result = TRegister(gs.regs.len)
  gs.regs.add(true)

func regRange(gs: var GenState, num: int): Slice[TRegister] =
  # TODO: refactor
  for i, r in gs.regs.pairs:
    if not r:
      block search:
        # test if `num` registers starting at `i` are free:
        for j in i+1 ..< i+num:
          if gs.regs[i]:
            # they aren't
            break search

        # claim registers
        for j in i..<i+num:
          gs.regs[j] = true

        return TRegister(i)..TRegister(i+num-1)

  # no available range found

  if gs.regs.len + num > high(uint16).int:
    fail(unknownLineInfo, rsemTooManyRegistersRequired)

  let start = gs.regs.len
  gs.regs.setLen(start + num)
  result = TRegister(start)..TRegister(start+num-1)
  for i in result:
    gs.regs[i] = true

func gABC(ctx: var TCtx; n: PNode; opc: TOpcode; a, b, c: TRegister = 0) =
  ## Takes the registers `b` and `c`, applies the operation `opc` to them, and
  ## stores the result into register `a`
  ## The node is needed for debug information
  assert opc.ord < 255
  let ins = (opc.TInstrType or (a.TInstrType shl regAShift) or
                           (b.TInstrType shl regBShift) or
                           (c.TInstrType shl regCShift)).TInstr
  #[
  when false:
    if ctx.code.len == 43:
      writeStackTrace()
      echo "generating ", opc
  ]#
  ctx.code.add(ins)
  ctx.debug.add(n.info)

proc gABI(c: var TCtx; n: PNode; opc: TOpcode; a, b: TRegister; imm: BiggestInt) =
  # Takes the `b` register and the immediate `imm`, applies the operation `opc`,
  # and stores the output value into `a`.
  # `imm` is signed and must be within [-128, 127]
  c.config.internalAssert(imm in -128..127 , n.info,
    "VM: immediate value does not fit into an int8")

  let ins = (opc.TInstrType or (a.TInstrType shl regAShift) or
                           (b.TInstrType shl regBShift) or
                           (imm+byteExcess).TInstrType shl regCShift).TInstr
  c.code.add(ins)
  c.debug.add(n.info)

proc gABx(c: var TCtx; n: PNode; opc: TOpcode; a: TRegister = 0; bx: int) =
  # Applies `opc` to `bx` and stores it into register `a`
  # `bx` must be signed and in the range [regBxMin, regBxMax]

  #[
  when false:
    if c.code.len == 43:
      writeStackTrace()
      echo "generating ", opc
      ]#

  c.config.internalAssert(bx in regBxMin-1..regBxMax, n.info,
    "VM: immediate value does not fit into regBx")

  let ins = (opc.TInstrType or a.TInstrType shl regAShift or
            (bx+wordExcess).TInstrType shl regBxShift).TInstr
  c.code.add(ins)
  c.debug.add(n.info)


func gABx2(ctx: var CodeFragment, opc: TOpcode, a: TRegister, bx: int; info: TLineInfo = unknownLineInfo) =
  # Applies `opc` to `bx` and stores it into register `a`
  # `bx` must be signed and in the range [regBxMin, regBxMax]

  #c.config.internalAssert(bx in regBxMin-1..regBxMax, n.info,
  #  "VM: immediate value does not fit into regBx")

  let ins = (opc.TInstrType or a.TInstrType shl regAShift or
            (bx+wordExcess).TInstrType shl regBxShift).TInstr
  debugEcho opc
  ctx.code.add(ins)
  ctx.debug.add(info)

func gABC2(ctx: var CodeFragment, opc: TOpcode, a: TRegister; b, c: TRegister = 0; info: TLineInfo = unknownLineInfo) =
  ## Takes the registers `b` and `c`, applies the operation `opc` to them, and
  ## stores the result into register `a`
  ## The node is needed for debug information
  assert opc.ord < 255
  let ins = (opc.TInstrType or (a.TInstrType shl regAShift) or
                           (b.TInstrType shl regBShift) or
                           (c.TInstrType shl regCShift)).TInstr

  ctx.code.add(ins)
  ctx.debug.add(info)

func gABI2(ctx: var CodeFragment, opc: TOpcode, a: TRegister; b: TRegister; imm: int; info: TLineInfo = unknownLineInfo) =
  # Takes the `b` register and the immediate `imm`, applies the operation `opc`,
  # and stores the output value into `a`.
  # `imm` is signed and must be within [-128, 127]
  #c.config.internalAssert(imm in -128..127 , n.info,
  #  "VM: immediate value does not fit into an int8")

  let ins = (opc.TInstrType or (a.TInstrType shl regAShift) or
                           (b.TInstrType shl regBShift) or
                           (imm+byteExcess).TInstrType shl regCShift).TInstr
  debugEcho opc
  ctx.code.add(ins)
  ctx.debug.add(info)

proc xjmp(c: var TCtx; n: PNode; opc: TOpcode; a: TRegister = 0): TPosition =
  #assert opc in {opcJmp, opcFJmp, opcTJmp}
  result = TPosition(c.code.len)
  gABx(c, n, opc, a, 0)

func genLabel(c: TCtx): TPosition =
  result = TPosition(c.code.len)
  #c.jumpTargets.incl(c.code.len)

proc jmpBack(c: var TCtx, n: PNode, p = TPosition(0)) =
  let dist = p.int - c.code.len
  internalAssert(c.config, regBxMin < dist and dist < regBxMax)
  gABx(c, n, opcJmpBack, 0, dist)

proc patch(c: var TCtx, p: TPosition) =
  # patch with current index
  let p = p.int
  let diff = c.code.len - p
  #c.jumpTargets.incl(c.code.len)
  internalAssert(c.config, regBxMin < diff and diff < regBxMax)
  let oldInstr = c.code[p]
  # opcode and regA stay the same:
  c.code[p] = ((oldInstr.TInstrType and regBxMask).TInstrType or
               TInstrType(diff+wordExcess) shl regBxShift).TInstr

const EmptySlot = high(TRegister)


func genPath(c: var CodeFragment, gs: var GenState, s: IrStore, p: IRIndex; isAddr: bool = false): TRegister

func genOp(c: var CodeFragment, gs: GenState, s: IrStore, p: IRIndex, dst: TRegister) =
  let n = s.ops[p]
  assert n.kind == inktOp

func getImm(s: IrStore, p: IRIndex): int =
  ## Returns the immediate value stored at pos `p`
  missingImpl()

func asgnReg(c: var CodeFragment, a, b: TRegister) =
  c.gABC2(opcCpReg, a, b)

func genBuiltinCall(c: var CodeFragment, gs: var GenState, s: IrStore, n: IrNode2): TRegister =
  # TODO: refactor; improve ``InstrInfo``
  let info = InstrInfos[n.opc]
  var inputs: array[5, int]
  var numArgs = n.opArgs.len
  assert numArgs <= 5
  for i, x in n.opArgs.pairs:
    inputs[i] = x

  if info.hasResult:
    result = gs.freeReg()

  if info.isInOut or not info.hasResult:
    result = inputs[0]
    inputs[0] = inputs[1]
    inputs[1] = inputs[2]
    inputs[2] = inputs[3]
    inputs[3] = inputs[4]
    inputs[4] = 0
    dec numArgs

  if info.isBx:
    # TODO: also needs to be asserted when emitting the IR:
    assert numArgs == 1
    c.gABx2(n.opc, result, inputs[0])
  elif info.hasImm:
    assert numArgs == 2
    # TODO: assert that inputs[1] is taken from a literal
    c.gABI2(n.opc, result, inputs[0], inputs[1])
  elif numArgs <= 2:
    c.gABC2(n.opc, result, inputs[0], inputs[2])
  else:
    c.gABC2(n.opc, result, inputs[0], inputs[1])
    c.gABC2(n.opc, inputs[2], inputs[3], inputs[4])


func getType(s: IrStore, types: seq[PVmType], p: IRIndex): PVmType =
  let n = s.ops[p]
  #case n.kind
  #of inktPathObj:
  #  getTyp()


func genValue(c: var CodeFragment, gs: var GenState, s: IrStore, p: IRIndex): TRegister =
  if gs.nodeRegs[p] != EmptySlot:
    return gs.nodeRegs[p]

  result = freeReg(gs) # FIXME: wrong, sometimes not used
  let n = s.ops[p]
  case n.kind
  of inktImm:
    c.gABx2(opcLdImmInt, result, n.immediate.int)
  of inktConst:
    # TODO: maybe rename to opcLdLit?
    c.gABx2(opcLdConst, result, n.litId.int)
  of inktLd:
    # TODO: assert that the register entry is populated
    # TODO: use a proper `genLoad`. `genPath` is wrong here!!
    result = genPath(c, gs, s, n.ldSrc)
    # TODO: emit validate instruction
  of inktOp:
    result = genBuiltinCall(c, gs, s, n)
  of inktCallExpr:
    let regs = regRange(gs, n.args.len+1)
    c.asgnReg(regs.a, genValue(c, gs, s, n.name))
    for i, arg in n.args.pairs:
      c.asgnReg(regs.a + i + 1, genValue(c, gs, s, arg))

    c.gABC2(opcIndCallAsgn, result, regs.a, regs.len)

  of inktProc:
    # XXX: procedural values aren't stored in registers yet and thus require
    #      special handling depending on where they're used. As an alternative
    #      temporary workaround, we could perform a
    #      ``opdLdNull``+``opcWrProc`` here, but that would require a type
    #      lookup (for LdNull) that's hard to pull off here.
    unreachable("inktProc requires special handling")
  else:
    unreachable(n.kind)

  gs.nodeRegs[p] = result

func genLoc(c: var CodeFragment, gs: var GenState, s: IrStore, p: IRIndex): TRegister =
  ## Emits the instructions for the given IR yielding a handle to a location
  result = gs.nodeRegs[p]
  if result != EmptySlot:
    return

  let n = s.ops[p]
  case n.kind
  of inktGlobal:
    result = gs.freeReg()
    c.gABx2(opcLdGlobal, result, n.linkIndex.int)
  else:
    # FIXME: wrong
    result = genPath(c, gs, s, p)

  gs.nodeRegs[p] = result

func genPath(c: var CodeFragment, gs: var GenState, s: IrStore, p: IRIndex; isAddr: bool = false): TRegister =
  # `genPath` implementation and architecture is unacceptable, but works for now

  result = gs.nodeRegs[p]
  if result != EmptySlot:
    return

  let n = s.ops[p]
  result = freeReg(gs) # XXX: due to recursion, order is wrong
  case n.kind
  of inktPathObj:
    let src = genPath(c, gs, s, n.objSrc)
    c.gABC2(opcLdObj, result, src, TRegister(n.field))
  of inktPathArr:
    let src = genPath(c, gs, s, n.objSrc)
    let idx = genValue(c, gs, s, n.idx)

    c.gABC2(opcLdArr, result, src, idx)

  of inktPathCall:
    result = genValue(c, gs, s, p)

  else:
    unreachable(n.kind)

  gs.nodeRegs[p] = result


proc optimizeJumps(c: var CodeFragment; start: int)

proc getReg*(gs: GenState, x: IRIndex): TRegister =
  gs.nodeRegs[x]

func isVoid*(s: IrStore, idx: IRIndex): bool =
  s.ops[idx].kind in {inktStmt, inktCallStmt}

proc genCode*(s: var IrStore, gs: var GenState): (CodeFragment, int) =
  var handles: seq[TRegister]
  handles.newSeq(s.paths.len)

  var code: CodeFragment
  var cl: GenClosure

  gs.nodeRegs.newSeq(s.ops.len)
  for r in gs.nodeRegs.mitems:
    r = EmptySlot # FIXME: sentinel value is a valid register index

  for i, x in s.ops.pairs:
    case x.kind
    of inktLd, inktLdDeref:
      # a load is only generated when it's connected to a statement
      # TODO: maybe a bad idea (requires one to be more vigilant regarding
      #       evaluation order)
      discard
    of inktWr:
      # meh, procs require special handling for now:
      if s.ops[x.wrSrc].kind == inktProc:
        let loc = genLoc(code, gs, s, x.wrDst)
        code.gABx2(opcWrProc, loc, s.ops[x.wrSrc].linkIndex.int)
        continue

      let src = genValue(code, gs, s, x.wrSrc) # TODO: left-to-right violation?

      let dstN = s.ops[x.wrDst]
      case dstN.kind
      of inktPathArr:
        let a = genPath(code, gs, s, dstN.arrSrc)
        let idx = genValue(code, gs, s, dstN.idx)
        code.gABC2(opcWrArr, a, idx, src)
      of inktPathObj:
        let a = genPath(code, gs, s, dstN.objSrc)
        let f = TRegister(dstN.field)
        code.gABC2(opcWrObj, a, f, src)
      of inktPathCall:
        let v = genValue(code, gs, s, i)
        # TODO: merge ``opcAsgnComplex`` and the other 'opcAsgnX' instructions into just ``opcAsgn``
        # a ``lent`` or ``var`` value is currently implemented as a pointer
        code.gABC2(opcWrDeref, v, 0, src)
      of inktGlobal, inktTemp, inktLocal:
        let v = genLoc(code, gs, s, x.wrDst)
        code.gABC2(opcAsgnComplex, v, src)
      else:
        # dst must be a path component
        unreachable(dstN.kind)

    of inktWrDeref:
      let
        # do _NOT_ reorder these two declarations
        dst = genValue(code, gs, s, x.wrDst)
        src = genValue(code, gs, s, x.wrSrc)

      code.gABC2(opcWrDeref, dst, src)

    of inktStmt:
      discard genBuiltinCall(code, gs, s, x)
    of inktCallExpr, inktCallStmt:
      if x.hasSideEffect:
        discard genValue(code, gs, s, i)

    of inktCommit:
      discard genValue(code, gs, s, x.call)

    of inktTemp, inktLocal:
      # XXX: register allocation is a mess, a second, more low-level IR might
      #      be needed
      let reg = gs.freeReg()
      gs.nodeRegs[i] = reg

      code.gABx2(opcLdNull, reg, x.typ.int)

    of inktGoto:
      case x.gotoOp
      of TOpcode(0):
        # TODO: emit simple jump
        echo "Missing jump"
      of opcRet:
        code.gABC2(opcRet, gs.nodeRegs[1]) # FIXME: hardcoded result handling
      else:
        unreachable(x.gotoOp)

    else:
      #echo "ignore: ", x.kind
      # the rest is only generated on use
      discard

  optimizeJumps(code, 0)

  #echo "dump"
  #echo s.ops.len
  #echo code.code.len
  #echo gs.regs.len

  result = (move code, gs.regs.len)

proc finalJumpTarget(c: var CodeFragment; pc, diff: int) =
  #[internalAssert(
    c.config,
    regBxMin < diff and diff < regBxMax,
    "Jump target is not in range of min/max registers - $1 < $2 < $3 failed" % [
      $regBxMin, $diff, $regBxMax])]#

  let oldInstr = c.code[pc]
  # opcode and regA stay the same:
  c.code[pc] = ((
    oldInstr.TInstrType and
    ((regOMask shl regOShift) or (regAMask shl regAShift))).TInstrType or
                TInstrType(diff+wordExcess) shl regBxShift).TInstr


proc optimizeJumps(c: var CodeFragment; start: int) =
  const maxIterations = 10
  for i in start..<c.code.len:
    let opc = c.code[i].opcode
    case opc
    of opcTJmp, opcFJmp:
      var reg = c.code[i].regA
      var d = i + c.code[i].jmpDiff
      for iters in countdown(maxIterations, 0):
        case c.code[d].opcode
        of opcJmp:
          d += c.code[d].jmpDiff
        of opcTJmp, opcFJmp:
          if c.code[d].regA != reg: break
          # tjmp x, 23
          # ...
          # tjmp x, 12
          # -- we know 'x' is true, and so can jump to 12+13:
          if c.code[d].opcode == opc:
            d += c.code[d].jmpDiff
          else:
            # tjmp x, 23
            # fjmp x, 22
            # We know 'x' is true so skip to the next instruction:
            d += 1
        else: break
      if d != i + c.code[i].jmpDiff:
        c.finalJumpTarget(i, d - i)
    of opcJmp, opcJmpBack:
      var d = i + c.code[i].jmpDiff
      var iters = maxIterations
      while c.code[d].opcode == opcJmp and iters > 0:
        d += c.code[d].jmpDiff
        dec iters
      if c.code[d].opcode == opcRet:
        # optimize 'jmp to ret' to 'ret' here
        c.code[i] = c.code[d]
      elif d != i + c.code[i].jmpDiff:
        c.finalJumpTarget(i, d - i)
    else: discard


proc dumpIr*(s: IrStore): string =
  var names: seq[string]
  names.newSeq(s.ops.len)

  var ldName: int

  proc genExpr(p: IRIndex): string =
    proc genProc(p: IRIndex): string =
      if s.ops[p].kind == inktProc:
        result = "proc_" & $s.ops[p].linkIndex
      else:
        result = genExpr(p)

    if names[p].len > 0:
      return names[p]

    let n = s.ops[p]
    case n.kind
    of inktImm:
      result &= $n.immediate
    of inktOp:
      result = $n.opc & "("
      for arg in n.opArgs.items:
        result &= genExpr(arg)
        result &= ", "
      result &= ")"
    of inktCallExpr:
      let name = genProc(n.name)
      result = name & "("
      for arg in n.args.items:
        result &= genExpr(arg)
        result &= ", "
      result &= ")"
    of inktGlobal:
      result &= "global_" & $n.linkIndex
    of inktConst:
      # TODO: print the literal value
      result &= "lit_" & $n.litId
    of inktPathObj:
      result &= genExpr(n.objSrc) & ".field_" & $n.field
    of inktParam:
      result &= "local_" & $n.param
    of inktProc:
      result &= genProc(p)
    of inktAddr:
      result &= "addr " & genExpr(n.addrLoc) # XXX: should use `genLoc` (doesn't matter much)
    of inktPathArr:
      result &= genExpr(n.arrSrc) & "["
      result &= genExpr(n.idx)
      result &= "]"
    else:
      unreachable(n.kind)

  proc genLoc(p: IRIndex): string =
    if names[p].len > 0:
      return names[p]

    result = genExpr(p)
    doAssert result.len > 0

  for i, n in s.ops.pairs:
    case n.kind
    of inktLd:
      names[i] = "loaded_" & $ldName
      inc ldName
      result &= "let " & names[i] & " = " & genExpr(n.ldSrc) & "\n"
    of inktWr:
      result &= genLoc(n.wrDst)
      result &= " = "
      result &= genExpr(n.wrSrc)
      result &= "\n"
    of inktTemp:
      names[i] = "tmp_" & $n.tmpName

      result &= "var " & names[i] & "\n"
    of inktLocal:
      names[i] = "local_" & $i

      result &= "var " & names[i] & "\n"
    of inktCallStmt:
      let name = genExpr(n.name)
      result &= name & "("
      for arg in n.args.items:
        result &= genExpr(arg)
        result &= ", "
      result &= ")\n"
    of inktStmt:
      result &= $n.opc & "("
      for arg in n.opArgs.items:
        result &= genExpr(arg)
        result &= ", "
      result &= ")\n"

    of inktImm, inktConst, inktParam, inktPathObj, inktPathCall, inktPathArr, inktOp, inktProc, inktAddr:
      discard
    of inktJoinFwd:
      names[n.fwd] = "label_" & $i
    of inktJoin:
      if names[i].len == 0:
        names[i] = "label_" & $i

      result &= names[i] & ":\n"
    of inktBranch:
      case n.branchOp
      of opcTJmp:
        result &= "if " & genExpr(n.cond)
      of opcFJmp:
        result &= "if not " & genExpr(n.cond)
      else: unreachable(n.branchOp)

      result &= ":\n"
    else:
      result &= "missing: " & $n.kind & "\n"

type NewNodeKind* = IrNodeKind3


type
  NewNode* = object
    kind*: NewNodeKind
  ExecProc*[A, B] = proc (c: var A, ls: var B, n: NewNode)
  NewIr* = seq[NewNode]

type
  LocIndex* = int
  #FieldId* = int
  NewNodeId* = int
  ValueId* = int

func target(n: NewNode): NewNodeId =
  discard

func gotoTarget(n: NewNode): NewNodeId =
  discard


func id*(n: NewNode): int =
  discard

func loc*(n: NewNode): LocIndex =
  discard

func srcLoc*(n: NewNode): LocIndex =
  discard

func isViewCall*(n: NewNode): bool =
  discard

func arg*(n: NewNode, i: int): LocIndex =
  discard


type ProcInfo = object
  # TODO: merge bools into a bitset
  isInline: bool
  addrTaken: bool # whether the procedure is called indirectly
  deps: Slice[uint32]
  order: int

func merge(a: var ProcInfo, b: ProcInfo) =
  a.addrTaken = a.addrTaken or b.addrTaken

func callDeps(body: PNode, deps: var Table[int, ProcInfo]) =
  # TODO: figuring out the dependencies could and should be done differently,
  #       preferably without the need for recursion
  case body.kind
  of nkSym:
    # XXX: detecting a prodecure symbol use in a non-call-symbol context like
    #      this is brittle (and also wrong when there are meta expressions in `body`)
    deps.mgetOrPut(body.sym.id, ProcInfo()).addrTaken = true
  of nkCallKinds:
    let symNode = body[0]
    if symNode.kind == nkSym:
      # add a dependency if it doesn't exist already
      discard deps.hasKeyOrPut(symNode.sym.id, ProcInfo())

    for i in 1..<body.len:
      callDeps(body[i], deps)
  else:
    # traverse all children
    for i in 0..<body.safeLen:
      callDeps(body[i], deps)


func computeInlining(aliveSet: seq[PSym]) =
  ## `aliveSet` is the set of procedures for which the processing order and lifetimes are to be calculated
  # For inlining we first build a dependency graph between all functions.
  # This is done in order to reduce the peak memory usage. For inlining, we
  # require the post-phase-2 IR of all procedures that are to be inlined in
  # the current inlining context (e.g. procedure). First generating the IR for
  # all existing procedures and then performing inlining could require large amounts of memory.
  # Instead, we first compute the "lifetime" of each procedure.

  var tbl: Table[int, int] # symbol id -> graph index
  var tmp: Table[int, ProcInfo]

  type NodeIndex = int

  var gc: seq[ProcInfo] # graph-control; the graph nodes
  var deps: seq[NodeIndex] # a seq of seqs inlined as a single seq

  gc.newSeq(aliveSet.len)

  for i, it in aliveSet.pairs:
    gc[i].isInline = it.typ.callConv == ccInline
    gc[i].deps = 1'u32..0'u32 # empty slice
    tmp.clear()
    callDeps(it.ast, tmp)
    let start = deps.len
    gc[i].deps = uint32(start)..uint32(start+tmp.len-1)

    #deps.setLen(deps.len + tmp.len)
    # merge the collected proc infos
    for id, info in tmp.pairs:
      let nodeIdx = tbl[id]
      merge(gc[nodeIdx], info)
      # TODO: don't `add`; use `setLen` + `[]=`
      deps.add(nodeIdx)

  # compute the ordering by iteratively propagating the order value
  # TODO: this needs to be done differently
  var modified = true
  while modified:
    modified = false
    for it in gc.mitems:
      let newOrder = it.order + 1
      for d in it.deps:
        if gc[d].order < newOrder:
          gc[d].order = newOrder
          modified = true

  # compute the upper bounds
  discard

func firstPass*(s: var NewIr) =
  ## Runs phase 1 and 2
  # TODO: `s` shouldn't be mutable
  #runV2[AliasCtx, AliasesLocal, int](s, computeAliases)
  #runV2[DestrCtx, DestrLocal, int](s, computeDestructors)


type
  InputState = object
  ModState = object
  Loc = distinct int

  CGLocal = object
  CGGlobal = object
    code: CodeFragment

#[
func requiresAssign(s: ModState, x: Loc): bool =
  s.hasRef(x) and not s.isCursor(x)
]#

func isRegister(c: CGGlobal, n: LocIndex): bool =
  discard

func getFreeReg(ls: var CGLocal): int =
  discard

func setLoaded(ls: var CGLocal, loc: LocIndex, regIdx: int) =
  discard

func reg(s: CGLocal, loc: LocIndex): int =
  discard

func hasSideEffects(c: CGGlobal, n: NewNode): bool =
  discard

iterator loaded(ls: CGLocal): tuple[loc: LocIndex, reg: int] =
  discard

iterator args(n: NewNode): NewNode =
  discard

func hasResult(n: NewNode): bool =
  discard

func resetTrusted(ls: var CGLocal) =
  discard

func handle(ls: CGLocal, loc: LocIndex): int =
  discard

func typ(ls: CGGlobal, loc: LocIndex): PType =
  discard

func trust(ls: var CGLocal, l: LocIndex) =
  discard

func isTrusted(ls: CGLocal, l: LocIndex): bool =
  discard

const opcRegCopy = opcAsgnComplex # XXX: temporary
const opcValidate = opcAsgnComplex # XXX: temporary

func genCodeV3Exec(c: var CGGlobal, ls: var CGLocal, n: NewNode) =
  template verify(l: LocIndex) =
    # TODO: must not be a template
    ls.trust(l)
    c.code.gABC2(opcValidate, ls.handle(l))

  case n.kind
  of ntkWrite:
    # write to a location
    if c.isRegister(n.loc):
      # we can simply do a register copy. Also mark the register as loaded
      assert c.isRegister(n.loc)
      let r = ls.getFreeReg()
      ls.setLoaded(n.loc, r)

      c.code.gABC2(opcRegCopy, ls.reg(n.loc), ls.reg(n.srcLoc))

  of ntkLoad:
    if c.isRegister(n.loc):
      discard # XXX: hm, noop?
    else:
      # TODO: tyProc is wrong; only closures are not loaded
      if c.typ(n.loc).kind in {tySequence, tyProc}:
        # these don't go into registers
        ls.setLoaded(n.loc, ls.handle(n.loc)) # XXX: wrong?
      else:
        if not ls.isTrusted(n.loc):
          verify(n.loc)

        let r = ls.getFreeReg()
        ls.setLoaded(n.loc, r)

        c.code.gABC2(opcNodeToReg, r, ls.handle(n.loc))

  of ntkCall:
    # TODO: maybe the flush analysis should be done in stage 4?

    # Calls are completely opaque, so we have no idea what's happening there. For the location validity analysis, we don't trust the
    # `noSideEffect` attribute of a function since side-effects can be cast away via `{.cast.}`. The `noSideEffect` attribute is only
    # used to decide if writes need to be made visible and some other optimizations

    if c.hasSideEffects(n):
      # across a function call boundary, writes need to be observable and we need to flush all locations temporarily stored in registers back to memory
      for x, r in ls.loaded:
        discard
        #c.gABC2(opcWrLoc, x)
        #freeReg()
    else:
      # SPEC: does noSideEffect also mean that only locations reachable from the parameters are read? the current wording is somewhat ambiguous. Here it's assumed it does

      for arg in n.args:
        discard
        #x.isArg
      #

    # reset all handles to non-locals back to untrusted
    ls.resetTrusted()

    if n.hasResult:
      discard #c.code.gABC2(opcIndCallAsgn, )

  of ntkSym, ntkAddr, ntkDeref:
    discard

  of ntkUse, ntkConsume, ntkAsgn:
    unreachable(n.kind)
  of ntkGoto, ntkBranch:
    unreachable(n.kind)
  else:
    {.warning: "handle this".}

#[
func genCodeV2(ins: InputState) =
  var s: ModState



  for n in ins.nodes.items:
    case n.kind
    of ntkWrite:
      # write to a location
      if s.isRegister(n.loc):
        # we can simply do a register copy
        assert s.isRegister(n.srcLoc)
        c.gABC2(opcRegCopy, s.reg(n.loc), s.reg(n.srcLoc))
      else:
        # target is a location
        if s.isRegister(n.srcLoc):
          assert not s.isComplex(n.loc)
          c.gABC2(opcWrLoc, s.handle(n.loc), s.reg(n.srcLoc))
        else:
          if s.requiresAssign(n.loc):
            # ref-like types are not mutated in registers, so we don't need
            # to flush any aliases
            c.gABC2(opcAsgnComplex, s.handle(n.loc), s.handle(n.srcLoc))
          else:
            # no need for a complex assign, just do a mem copy
            c.gABC2(opcFastAsgn, s.handle(n.loc), s.handle(n.srcLoc))

    of ntkLoad:
      var needsLoad: bool
      if not s.isTrusted(n):
        assert not s.isLoaded(n.loc) # if
        c.gABC2(opcChkHandle, s.handle(n.loc))
]#

## --------------- code previously located in ``irgen.nim``:

func irGotoRaise(c: var IrStore, i: IRIndex): IRIndex =
  missingImpl()

func irGotoLink(c: var IrStore, i: IRIndex): IRIndex =
  missingImpl()

func irFwd(c: var TCtx): IRIndex =
  missingImpl()

func irEnd(c: var TCtx, target, value: IRIndex): IRIndex =
  missingImpl()

func appendCode(c: var vmdef.TCtx, f: CodeFragment) =
  var tmp: CodeFragment
  swap(tmp.code, c.code)
  swap(tmp.debug, c.debug)

  tmp.append(f)

  swap(tmp.code, c.code)
  swap(tmp.debug, c.debug)

## --------------- code previously located in ``irpasses.nim``:


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