## Implements a data-flow graph (=DFG) representation plus algorithms for
## traversing the graph in both forward or backward direction. In the
## abstract, this means visiting the nodes in topological-order (forward)
## or post-order (backward), with special handling for loops.
##
## In the compiler, this is mainly used for control- and/or data-flow
## analysis, meant to propagate properties through the graph or to answer
## questions such as: "is point A connected to point B?", "is X initialized on
## all paths?", etc.
##
## A DFG is built via the `computeDfg <#computeDfg,MirTree>`_ routine. Instead
## of a pointer-based graph structure, the graph is represented as a sequence
## of instructions encoding all data- and control-flow relevant properties
## for a piece of code.

# XXX: with a small to medium amount of work, the algorithms and
#      ``DataFlowGraph`` can be generalized to work independent from the
#      MIR. The current ``computeDfg`` would be moved to somewhere else

import
  std/[
    algorithm,
    options,
    packedsets,
    tables
  ],
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    idioms
  ]

type
  Opcode* = enum
    ## The opcode of a data-/control-flow instruction, representing edges and
    ## nodes in the graph.
    opNone ## no-op
    opFork ## branching control-flow that cannot introduce a cycle
    opGoto ## unconditional jump that cannot introduce a cycle
    opLoop ## unconditional jump to the start of a loop. The start of a cycle
    opJoin ## a join point for control-flow

    opUse         ## use of a value
    opDef         ## definition of a value
    opKill        ## end-of-life for a value
    opInvalidate  ## all information gathered about a value becomes invalid
    opMutate      ## mutation of a value. Can be viewed as a combined 'use' +
                  ## 'def'
    opConsume     ## a value is consumed. This is effectively a 'use' + 'kill'
    opDestroy     ## a location's value is destroyed

    opMutateGlobal ## an unspecified global is mutated

  DataFlowOpcode = range[opUse..opMutateGlobal]

const
  DataFlowOps = {opUse .. opMutateGlobal}

type
  # TODO: make both types distinct
  InstrPos* = int32
  JoinId = uint32

  Subgraph* = Slice[InstrPos]
    ## Represents a sub-graph within the data-flow graph. Internally, this is
    ## a span of instructions.

  Instr = object
    node: NodePosition
    case op: Opcode
    of opFork, opGoto, opLoop:
      dest: JoinId
    of opJoin:
      id: JoinId
    of DataFlowOps:
      val: OpValue
    of opNone:
      discard

  DataFlowGraph* = object
    ## Encodes the data-flow graph of a local program as a sequence of
    ## control- and data-flow instructions.
    instructions: seq[Instr]
      ## sorted in ascending order by attached-to node position
    map: seq[InstrPos]
      ## join ID -> instruction

  TraverseState* = object
    exit*: bool     ## used to communicate to ``traverse`` that the active path
                    ## should be "killed" (not followed further). Reset to `false`
                    ## whenever control is passed back to the iterator.
                    ## When traversal is finished, `exit` is set to 'true'
                    ## if traversal reached the end of the given span, 'false'
                    ## otherwise
    escapes*: bool  ## whether control-flow escapes the given span

  LoopEntry = tuple
    start: JoinId ## the join point that marks the start of the loop
    fin: InstrPos ## the 'loop' instruction
    isRun: bool   ## whether the loop is currently processed

  Time = uint16
    ## Represents the abstract time that a basic block is visited at.
    ## For simplicity, the zero-representation (`0`) means invalid /
    ## uninitialized. Because of this, a higher time value means "visited
    ## earlier"
    ##
    ## Using ``uint16`` the upper limit for the length of the longest chain of
    ## basic blocks is 2^16-1. In practice, a few optimizations are applied in
    ## order to increase the effective upper limit for some situtations.

  ExecState = object
    ## Execution environment state for backward traversal
    visited: seq[Time]
      ## - for loop joins: the `top` time when entering the loop
      ## - for normal joins: the earliest time at which the join point was
      ##   reached
      ## - for both: '0' if the join point was never reached
    loops: seq[LoopEntry]
      ## the loop stack. Remembers the loops the current basic block is
      ## located inside

    time: Time
      ## the current time, or `0`, if the next basic block is not connected to
      ## the start
    top: Time
      ## if `time` is lower or equal to `top`, the next basic block wasn't
      ## visited yet
    bottom: Time
      ## the lowest value `time` had. Provides the time to resume at after
      ## exiting a loop

    pc: InstrPos
      ## the program counter. Points to the CFG instruction that is executed
      ## next

  ClosureEnv = object
    instrs: seq[Instr]
    joins: seq[InstrPos]
      ## the ``JoinId`` -> instruction position mappings
    labelToJoin: Table[LabelId, JoinId]
      ## maps the label ID to the corresponding join ID
    resumeLabel: Option[JoinId]
      ## only setup when used

func incl[T](s: var seq[T], v: sink T) =
  ## If not present already, adds `v` to the sorted ``seq`` `s`
  var i = 0
  while i < s.len and s[i] < v:
    inc i

  if i >= s.len or s[i] != v:
    s.insert(v, i)

func compare(a: Instr, b: NodePosition): int =
  ord(a.node) - ord(b)

template lowerBound(c: DataFlowGraph, node: NodePosition): InstrPos =
  lowerBound(c.instructions, node, compare).InstrPos

template upperBound(c: DataFlowGraph, node: NodePosition): InstrPos =
  upperBound(c.instructions, node, compare).InstrPos

func `[]`(c: DataFlowGraph, pc: SomeInteger): lent Instr {.inline.} =
  c.instructions[pc]

# ---- data-flow graph setup ----

proc firstTarget(tree: MirTree, n: NodePosition): NodePosition =
  ## Returns the first label or resume in the jump target description.
  case tree[n].kind
  of mnkLabel:
    result = n
  of mnkTargetList:
    for p in subNodes(tree, n):
      if tree[p].kind in {mnkResume, mnkLabel}:
        return p
    unreachable("ill-formed target list")
  else:
    unreachable(tree[n].kind)

func map(env: var ClosureEnv, id: LabelId): JoinId =
  if id in env.labelToJoin:
    result = env.labelToJoin[id]
  else:
    result = env.joins.len.JoinId
    env.joins.add 0 # will be patched later
    env.labelToJoin[id] = result

func dfaOp(env: var ClosureEnv, opc: Opcode, n: NodePosition, v: OpValue) =
  {.cast(uncheckedAssign).}:
    env.instrs.add Instr(op: opc, node: n, val: v)

func dfaOp(env: var ClosureEnv, opc: Opcode, tree: MirTree, n: NodePosition,
           v: OpValue) {.inline.} =
  ## Only emits an instruction if the operand is an lvalue.
  if tree[v].kind in LvalueExprKinds:
    dfaOp(env, opc, n, v)

func getResumeLabel(env: var ClosureEnv): JoinId =
  # the join point is allocated when first used
  if env.resumeLabel.isNone:
    env.resumeLabel = some env.joins.len.JoinId
    env.joins.add 0 # will be patched later
  env.resumeLabel.unsafeGet

func raiseExit(env: var ClosureEnv, opc: Opcode, tree: MirTree,
               at, target: NodePosition) =
  let target = firstTarget(tree, target)
  # compute the join ID to use, accounting for the special 'resume' action:
  let join =
    case tree[target].kind
    of mnkLabel:
      map(env, tree[target].label)
    of mnkResume:
      env.getResumeLabel()
    else:
      unreachable()

  {.cast(uncheckedAssign).}:
    env.instrs.add Instr(op: opc, node: at, id: join)

func emitForValue(env: var ClosureEnv, tree: MirTree, at: NodePosition,
                  source: OpValue) =
  ## Emits the 'use' operations for all usages appearing within the value
  ## expression `source`.
  case tree[source].kind
  of mnkPathPos, mnkPathNamed, mnkPathConv, mnkPathVariant:
    emitForValue(env, tree, at, tree.operand(source))
  of mnkPathArray:
    emitForValue(env, tree, at, tree.operand(NodePosition source, 0))
    env.dfaOp(opUse, tree, at, tree.operand(NodePosition source, 1))
  of mnkDeref, mnkDerefView:
    env.dfaOp(opUse, tree, at, tree.operand(source))
  of Atoms:
    discard "handled, or not, at the callsite"
  of AllNodeKinds - LvalueExprKinds - Atoms:
    unreachable(tree[source].kind)

func emitLvalueOp(env: var ClosureEnv, opc: DataFlowOpcode, tree: MirTree,
                  at: NodePosition, source: OpValue) =
  emitForValue(env, tree, at, source)
  env.dfaOp(opc, tree, at, source)

func emitForArg(env: var ClosureEnv, tree: MirTree, at, source: NodePosition) =
  case tree[source].kind
  of mnkArg:
    emitLvalueOp(env, opUse, tree, at, tree.operand(source))
  of mnkConsume:
    emitLvalueOp(env, opConsume, tree, at, tree.operand(source))
  of mnkName:
    emitForValue(env, tree, at, tree.skip(tree.operand(it), mnkTag))
  else:
    unreachable(tree[source].kind)

func emitForArgs(env: var ClosureEnv, tree: MirTree, at, source: NodePosition) =
  for it in subNodes(tree, source):
    case tree[it].kind
    of mnkArg, mnkConsume, mnkName:
      emitForArg(env, tree, at, it)
    of mnkMagic, mnkProc, mnkLabel, mnkTargetList:
      discard
    else:
      emitLvalueOp(env, opUse, tree, at, OpValue it)

func emitForExpr(env: var ClosureEnv, tree: MirTree, at, source: NodePosition) =
  ## Emits the data- and control-flow instructions corresponding to the
  ## expression at `source`.
  template op(o: Opcode, v: OpValue) =
    env.dfaOp(o, tree, at, v)

  case tree[source].kind
  of mnkCall, mnkCheckedCall, mnkArrayConstr, mnkSeqConstr, mnkTupleConstr,
     mnkClosureConstr:
    emitForArgs(env, tree, at, source)
  of mnkObjConstr, mnkRefConstr:
    for it in subNodes(tree, source):
      emitForArg(env, tree, at, tree.child(it, 1))
  of mnkSetConstr:
    for it in subNodes(tree, source):
      case tree[it].kind
      of mnkRange:
        emitLvalueOp(env, opUse, tree, at, tree.operand(it, 0))
        emitLvalueOp(env, opUse, tree, at, tree.operand(it, 1))
      else:
        emitLvalueOp(env, opUse, tree, at, OpValue it)
  of mnkConv, mnkStdConv, mnkCast:
    # a read is performed on the source operand (if it's an lvalue)
    emitLvalueOp(env, opUse, tree, at, tree.operand(source))
  of mnkAddr:
    # ``addr`` doesn't actually read its operand location, rather
    # it create a run-time handle (i.e., pointer) to them. Since those
    # handles aren't tracked however, the operation is conservatively
    # treated as a mutation
    emitLvalueOp(env, opMutate, tree, at, tree.operand(source))
  of mnkView, mnkMutView:
    # if the created view supports mutation, treat the creation as a
    # mutation itself
    let opc =
      if tree[source].kind == mnkView: opUse
      else:                            opMutate
    emitLvalueOp(env, opc, tree, at, tree.operand(source))
  of mnkToSlice, mnkToMutSlice:
    # slices aren't tracked at the moment, so the mere creation of a slice is
    # treated as a usage of the sequence. If the resulting openArray supports
    # mutation, creation of the slice is treated as a mutation. To ensure the
    # correct data-flow operation order for the mutation case, the lower/upper
    # bound operands are treated as being evaluated (i.e., used) first
    if len(tree, source) == 3:
      emitLvalueOp(env, opUse, tree, at, tree.operand(source, 1))
      emitLvalueOp(env, opUse, tree, at, tree.operand(source, 2))

    let opc =
      if tree[source].kind == mnkToSlice: opUse
      else:                               opMutate
    emitLvalueOp(env, opc, tree, at, tree.operand(source, 0))
  of mnkCopy, mnkSink:
    # until it's collapsed, a sink is conservatively treated as only a
    # usage (not a consumption)
    emitLvalueOp(env, opUse, tree, at, tree.operand(source))
  of mnkMove:
    emitLvalueOp(env, opConsume, tree, at, tree.operand(source))
  of UnaryOps:
    emitLvalueOp(env, opUse, tree, at, tree.operand(source, 0))
  of BinaryOps:
    emitLvalueOp(env, opUse, tree, at, tree.operand(source, 0))
    emitLvalueOp(env, opUse, tree, at, tree.operand(source, 1))
  of LvalueExprKinds:
    # raw usage of an lvalue
    emitLvalueOp(env, opUse, tree, at, OpValue source)
  of mnkNone, LiteralDataNodes, mnkProcVal:
    discard "okay, ignore"
  of AllNodeKinds - ExprKinds - {mnkNone} + {mnkType}:
    unreachable(tree[source].kind)

  # the effects on lvalues (if any) take place *within* the called procedure.
  # For the local data-flow, this is represented as taking place after the
  # callsite arguments are used but before the exceptional exit (if any)
  case tree[source].kind
  of mnkCall, mnkCheckedCall:
    # lvalue effects:
    for k, it in arguments(tree, source):
      if tree[it].kind == mnkTag:
        let opr = tree.operand(it)
        case tree[it].effect
        of ekMutate:     op opMutate, opr
        of ekReassign:   op opDef, opr
        of ekKill:       op opKill, opr
        of ekInvalidate: op opInvalidate, opr
      elif k == mnkName:
        # the lvalue may be read from within the procedure
        op opUse, it

    # the potential mutation happens within the procedure, so the data-flow
    # operation has to come before the fork
    if geMutateGlobal in tree[source].effects:
      env.instrs.add Instr(op: opMutateGlobal, node: at)
    if tree[source].kind == mnkCheckedCall:
      # the jump target description is in the last slot
      raiseExit(env, opFork, tree, at, tree.previous(findEnd(tree, source)))
  else:
    discard

func emitForDef(env: var ClosureEnv, tree: MirTree, n: NodePosition) =
  let
    dest   = tree.operand(n, 0)
    source = tree.operand(n, 1)
  emitForValue(env, tree, n, dest)
  emitForExpr(env, tree, n, NodePosition source)
  # defs with an empty initializer have no data- or control-flow properties.
  # Parameter definitions are an exception.
  if tree[dest].kind == mnkParam or tree[source].kind != mnkNone:
    # the value re-assignment of the target takes place after the control-flow
    # effects and other mutation effects
    env.dfaOp opDef, n, dest

func computeDfg*(tree: MirTree): DataFlowGraph =
  ## Computes the data-flow graph for the given `tree`. This is a moderately
  ## expensive operation. The cost is due to having to materialize all data-
  ## flow operation for a given tree.

  template join(pos: NodePosition, label: LabelId) =
    var id: JoinId
    # pop the table entry; it's not needed past this point
    if env.labelToJoin.pop(label, id):
      env.instrs.add Instr(op: opJoin, node: pos, id: id)
      # patch the join-to-instruction mapping:
      env.joins[id] = env.instrs.high.InstrPos
    else:
      discard "label is not used, ignore"

  template goto(pos: NodePosition, label: LabelId) =
    env.instrs.add Instr(op: opGoto, node: pos, dest: map(env, label))

  template fork(pos: NodePosition, label: LabelId) =
    env.instrs.add Instr(op: opFork, node: pos, dest: map(env, label))

  template loop(pos: NodePosition, label: LabelId) =
    var id: JoinId
    discard env.labelToJoin.pop(label, id)
    env.instrs.add Instr(op: opLoop, node: pos, dest: id)

  var
    env = ClosureEnv()
    ifs = newSeq[LabelId]()

  for i, n in tree.pairs:
    case n.kind
    of mnkGoto:
      let first = tree.firstTarget(tree.child(i, 0))
      # the node for the target is guaranteed to be a label
      goto i, tree[first].label
    of mnkLoop:
      loop i, tree[i, 0].label
    of mnkIf:
      emitLvalueOp(env, opUse, tree, i, tree.operand(i, 0))
      fork i, tree[i, 1].label
      ifs.add tree[i, 1].label
    of mnkCase:
      var j = 0
      for it in subNodes(tree, i):
        if j == 0:
          emitLvalueOp(env, opUse, tree, i, OpValue it)
        elif j < tree[i].len.int:
          # all branches up until the final one are forks
          fork it, tree[it, tree[it].len - 1].label
        else:
          # a case dispatcher doesn't fall through (it's a terminator), so the
          # last jump is a goto
          goto it, tree[it, tree[it].len - 1].label

        inc j
    of mnkJoin:
      join i, tree[i, 0].label
    of mnkLoopJoin:
      # special handling for loop joins, as they come before their
      # corresponding jump instruction
      let id = env.joins.len.JoinId
      env.joins.add env.instrs.len.InstrPos
      env.instrs.add Instr(op: opJoin, node: i, id: id)
      env.labelToJoin[tree[i, 0].label] = id
    of mnkExcept:
      join i, tree[i, 0].label
      # fork to the handler that is jumped to when there's no match
      if n.len > 1:
        raiseExit(env, opFork, tree, i, tree.child(i, n.len - 1))
    of mnkFinally:
      join i, tree[i, 0].label
    of mnkContinue:
      var j = 0
      # a continue acts much like a dispatcher
      for it in subNodes(tree, i):
        if j == 0:
          discard "label of the associated finally; ignore"
        elif j < n.len.int - 1:
          fork i, tree[it].label
        else:
          goto i, tree[it].label
        inc j

      if n.len == 1:
        # no follow-up targets means that the finally continues exceptional
        # control-flow in the caller
        let target = env.getResumeLabel()
        env.instrs.add Instr(op: opGoto, node: i, dest: target)
    of mnkRaise:
      # raising an exception consumes it:
      if tree[tree.operand(i)].kind != mnkNone:
        emitLvalueOp(env, opConsume, tree, i, tree.operand(i))

      raiseExit(env, opGoto, tree, i, tree.child(i, 1))
    of mnkEndStruct:
      # emit a join at the end of an 'if'
      if ifs.len > 0 and tree[i, 0].label == ifs[^1]:
        join i, ifs.pop()

    of mnkDef, mnkDefCursor, mnkAsgn, mnkInit:
      emitForDef(env, tree, i)
    of mnkSwitch:
      # the switch statement invalidates the destination rather than
      # reassigning it (i.e., ``opDef``)
      let
        dest   = tree.operand(i, 0)
        source = tree.operand(i, 1)
      emitForValue(env, tree, i, dest)
      emitForExpr(env, tree, i, NodePosition source)
      dfaOp env, opInvalidate, i, dest
    of mnkBindMut, mnkBind:
      emitForValue(env, tree, i, tree.operand(i, 1))
    of mnkVoid:
      emitForExpr(env, tree, i, NodePosition tree.operand(i))
    of mnkDestroy:
      emitLvalueOp(env, opDestroy, tree, i, tree.operand(i))
    of mnkEmit, mnkAsm:
      emitForArgs(env, tree, i, i)

    else:
      discard "not relevant"

  # patch the resume label, if used:
  if env.resumeLabel.isSome:
    let id = env.resumeLabel.unsafeGet
    env.joins[id] = env.instrs.len.InstrPos
    env.instrs.add Instr(op: opJoin, node: tree.len.NodePosition, id: id)

  swap(env.instrs, result.instructions)
  swap(env.joins, result.map)

func subgraphFor*(dfg: DataFlowGraph, span: Slice[NodePosition]): Subgraph =
  ## Computes a reference to the sub-graph encompassing the `span` of MIR
  ## instructions.
  result.a = lowerBound(dfg, span.a)
  result.b = upperBound(dfg, span.b) - 1

func find*(dfg: DataFlowGraph, n: NodePosition): InstrPos =
  ## Returns the first data-/control-flow operation associated with `n`.
  ## If none are associated with `n`, the closest following (in terms of
  ## attached-to node position) operation is returned.
  lowerBound(dfg, n)

func change*(dfg: var DataFlowGraph, instrs: openArray[InstrPos],
             to: Opcode) =
  ## Changes all data-flow instructions identified by `instrs` to use the
  ## `to` opcode.
  for it in instrs.items:
    assert dfg.instructions[it].op in DataFlowOps
    dfg.instructions[it].op = to

iterator instructions*(dfg: DataFlowGraph): (InstrPos, Opcode, OpValue) =
  ## Returns all data-flow operations in order of appearance together with
  ## their position.
  for i, it in dfg.instructions.pairs:
    if it.op in DataFlowOps - {opMutateGlobal}:
      yield (InstrPos i, it.op, it.val)

iterator traverse*(c: DataFlowGraph, span: Subgraph, start: InstrPos,
                   state: var TraverseState): (DataFlowOpcode, OpValue) =
  ## Starts at the data-flow operation closest to `start` and traverses/yields
  ## all data-flow operations inside `span` in control-flow order. Outside of
  ## loops, this means that an operation is visited *before* operations that
  ## have a control-flow dependency on it. If `start` is not part of `span`,
  ## nothing is returned and `state.exit` is set to 'false'.
  ##
  ## The same basic block may be yielded multiple times. This is not a general
  ## limitation, but rather because of a shortcut taken by the implementation.
  ##
  ## `state` is used for bi-directional communication -- see the documentation
  ## of ``TraverseState`` for more information.
  var
    pc =
      if start in span: start
      else:             span.b + 1 # disable execution
    last = span.b
    queue: seq[InstrPos]
    visited: PackedSet[JoinId]

  state = TraverseState()

  template resume() =
    if queue.len > 0:
      pc = queue[0]
      queue.delete(0)

      assert c[pc].op == opJoin
    else:
      # no more threads left -> exit
      break

  template push(target: JoinId) =
    ## If the destination position is inside the active span, adds
    ## it to the execution queue, effectively starting a new thread. Records
    ## an escape otherwise
    let dst = c.map[target]
    if dst in span:
      queue.incl dst
    else:
      state.escapes = true

  template abort() =
    ## Exit the current thread and continue with the next one in the queue
    resume()

  while pc <= last:
      let instr = c[pc]

      case instr.op
      of opGoto:
        push(instr.dest)
        resume()
      of opFork:
        push(instr.dest)
      of opLoop:
        if not visited.containsOrIncl(instr.dest):
          push(instr.dest)

        resume()
      of opJoin:
        if queue.len > 0 and queue[0] == pc:
          # this case happens for the following CFG sequence:
          #   fork 0
          #   0: join
          # which is generated for e.g. an ``else`` branch
          queue.delete(0)
      of DataFlowOps:
        yield (DataFlowOpcode(instr.op), instr.val)
      of opNone:
        discard "ignore"

      if state.exit or pc + 1 == start:
        # abort the current path if we either reached the instruction we
        # started at or the path was manually killed
        state.exit = false
        abort()

      # increment *after* the abort handling, otherwise it wouldn't be
      # possible to detect that the end wasn't reached when an abort is
      # triggered by the very last instruction
      inc pc

  assert queue.len <= 1

  # don't set `exit` to true if nothing was traversed
  state.exit = pc > last and start in span

template active(s: ExecState): bool =
  # if a thread is selected and it's either the or derived from the main
  # thread, execution is active
  s.time != 0 and s.time.uint16 <= s.top

template step(s: var ExecState) =
  dec s.time
  # remember the lowest time value we've reached so far:
  s.bottom = min(s.bottom, s.time)

func processJoin(id: JoinId, s: var ExecState, c: DataFlowGraph) {.inline.} =
  ## Processes a 'join' instruction in the context of reverse traversal

  if s.loops.len > 0 and s.loops[^1].start == id:
    # the join point is the start of a loop
    let (_, p, isRetry) = s.loops[^1]
    # note that at this point, `s.active` can only be true if this is the first
    # we're reaching the loop start while being active. For each following
    # visit, active will always be 'false'
    if s.visited[id] == 0 and s.active:
      # this is the first time we're reaching this loop start -- jump back
      # to the end of the loop and set `isRetry` for this loop-start to
      # true so that we know whether it'a a loop exit the next time we reach it
      s.loops[^1].isRun = true

      # jump to the end of the loop:
      s.pc = p
      # remember the current `top` and prevent blocks already visited during
      # the first pass from being visited again by setting `top` to the current
      # time:
      s.visited[id] = s.top
      # XXX: it might be possible to set `time` to `bottom` here (doing so
      #      didn't cause any test failures), which would increase the amount
      #      basic blocks that can be traversed before exhausting the
      #      ``uint16`` range. Howerver, before applying the this optimization, it must
      #      first be formally proven to be correct
      s.time = s.bottom
      s.top = s.time
    else:
      if isRetry:
        # we finished the loop. Restore the `top` value to what is was when
        # entering the loop and use `bottom` as the time for the next basic
        # block. The latter is important for nested loops
        s.time = s.bottom
        s.top = s.visited[id]
        # XXX: as an optimization, `s.bottom` and `s.top` can both be reset
        #      to ``high(Time)``, increasing the amount of basic blocks that
        #      can be traversed in the situation where there are multiple
        #      top-level loops (i.e. loops not nested in other ones). Do note
        #      that this only if it's guaranteed that forward control-flow
        #      cannot jump *into* a loop (which is the case all NimSkull code
        #      *except* state machines realized via the ``.goto`` pragma)

      # the loop was processed, pop it from the stack:
      s.loops.setLen(s.loops.len - 1)

  else:
    # only remember the earliest time the join point was reached:
    s.visited[id] = max(s.time, s.visited[id])
    s.time = s.visited[id]

iterator traverseReverse*(c: DataFlowGraph, span: Subgraph, start: InstrPos,
                          exit: var bool): (DataFlowOpcode, OpValue) =
  ## Starts at `start - 1` and visits and returns all data-flow operations
  ## inside `span` in post-order.
  ##
  ## `span` being empty is supported: nothing is returned in that case.
  ##
  ## Similar to ``traverse``, `exit` is used, with the same meaning, for
  ## bi-directional communication.
  var s: ExecState

  # simplify further processing by making sure that `span` is something sane
  let span =
    if span.a <= span.b:
      assert start-1 in span, "`start` not inside `span`"
      span
    else:
      start..start-1 # `span` is empty

  let
    fin = span.a
      ## abstract control-flow reaching this instructions means "end reached"

  s.visited.newSeq(c.map.len)
  s.pc = span.b
  s.top = high(Time)
  s.time = s.top
  s.bottom = s.time

  exit = false

  # move the program counter to the DFG instruction coming before start. While
  # doing so, collect the loops the start position is located inside:
  while s.pc >= start:
    let instr = c[s.pc]
    case instr.op
    of opLoop:
      s.loops.add (instr.dest, s.pc, false)
    of opJoin:
      # the start of a loop; pop the previous loop entry:
      if s.loops.len > 0 and s.loops[^1].start == instr.id:
        s.loops.setLen(s.loops.len - 1)
    of opGoto, opFork, DataFlowOps, opNone:
      discard

    dec s.pc

  # perform the traversal:
  while s.pc >= fin:
    # execute all control-flow instructions located at the end of the basic
    # block (if we're at the end of one):
    while s.pc >= fin:
      let instr = c[s.pc]

      case instr.op
      of opGoto:
        # resume with the time from the target
        s.time = s.visited[instr.dest]
      of opFork:
        # time is always shortest distance to the next join point, hence the
        # use of ``max`` (and not ``min``)
        s.time = max(s.time, s.visited[instr.dest])
      of opLoop:
        # remember the loop instruction so that we can jump back to it when
        # reaching the start of the loop
        s.loops.add (instr.dest, s.pc, false)
        s.time = 0 # disable execution
      of opJoin:
        processJoin(instr.id, s, c)
      of DataFlowOps:
        # the end (in our case start) of the basic block is reached
        break
      of opNone:
        discard "ignore"

      dec s.pc

    let prev = s.pc # for detecting whether the `start` is crossed

    if s.active:
      # prevent the first half of the basic block we started inside to be
      # returned:
      let adjusted =
        if prev >= start: start
        else:             fin

      # return all items in the current basic block, with `exit` (which may be
      # set to true by the callsite) aborting the loop
      while s.pc >= adjusted and c[s.pc].op in DataFlowOps and not exit:
        yield (DataFlowOpcode(c[s.pc].op), c[s.pc].val)
        dec s.pc

      step(s)

      if exit:
        exit = false
        s.time = 0 # disable execution

    else:
      # inactive, skip all data-flow operations (i.e., the basic block)
      while s.pc >= fin and c[s.pc].op in DataFlowOps:
        dec s.pc

    if s.pc < start and prev >= start:
      # we've crossed the start position, so set the time to what it was at
      # the start
      s.time = high(Time)

  exit = s.active

iterator traverseFromExits*(c: DataFlowGraph, span: Subgraph,
                            exit: var bool): (DataFlowOpcode, OpValue) =
  ## Similar to ``traverseReverse``, but starts traversal at each unstructured
  ## exit of `span`. Here, unstructured exit means that the control-flow leaves
  ## `span` via a 'goto' or 'fork'.
  ##
  ## For the algorithm to work correctly, it is important that span does not
  ## cross a loop. That is, both start and the end of loop need to be present
  ## in `span`.
  ##
  ## `exit` works the same way as it does for ``traverseReverse``
  const EntryTime = high(Time)
  var s: ExecState

  let fin = span.a
  s.pc = span.b
  s.visited.newSeq(c.map.len)
  s.time = 0 # start as disabled
  s.top = EntryTime
  s.bottom = EntryTime

  exit = false

  template exits(target: JoinId): bool =
    c.map[target] notin span

  # for the most part similar to the loop in ``traverseReverse``, but with
  # special handling for jumps out of `span`
  while s.pc >= fin:
    # execute all instructions located at the end of the basic block (if we're
    # at the end of one):
    while s.pc >= fin:
      let instr = c[s.pc]

      case instr.op
      of opGoto:
        s.time =
          if exits(instr.dest): EntryTime
          else:                 s.visited[instr.dest]
      of opFork:
        s.time =
          if exits(instr.dest): EntryTime
          else:                 max(s.time, s.visited[instr.dest])
      of opLoop:
        # remember the loop instruction so that we can jump back to it when
        # reaching the start of the loop
        s.loops.add (instr.dest, s.pc, false)
        s.time = 0 # disable execution
      of opJoin:
        processJoin(instr.id, s, c)
      of DataFlowOps:
        # the end of a basic block is reached
        break
      of opNone:
        discard "ignore"

      dec s.pc

    # if they weren't visited yet, return all items in the current basic
    # block:
    if s.active:
      # return all data-flow operations from the current basic block:
      while s.pc >= fin and c[s.pc].op in DataFlowOps and not exit:
        yield (DataFlowOpcode(c[s.pc].op), c[s.pc].val)
        dec s.pc

      # perform the time step. This has to happen *before* potentially setting
      # `time` to 0
      step(s)

      if exit:
        exit = false
        s.time = 0 # disable execution

    else:
      # inactive, skip all data-flow operations (i.e., the basic block)
      while s.pc >= fin and c[s.pc].op in DataFlowOps:
        dec s.pc

  exit = s.active


func `$`*(c: DataFlowGraph): string =
  ## Renders the instructions of `c` as a human-readable text representation
  for i, n in c.instructions.pairs:
    case n.op
    of opJoin:
      result.add $n.id & ": join"
    of opGoto, opFork, opLoop:
      result.add $n.op & " " & $n.dest
    of DataFlowOps:
      result.add $n.op & " " & $ord(n.val)
    else:
      result.add "---"

    result.add " -> " & $ord(n.node) & "\n"
