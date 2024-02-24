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
    structStack: seq[InstrPos]
      ## stack of instruction positions for the currently open
      ## structured control-flow blocks (if, loop, and regions).
    blocks: seq[Option[LabelId]]
      ## stack of targets for forward, merging control-flow. No label
      ## means that it's a *hidden* block (such as the one opened by a
      ## ``try`` statement)
    exits: seq[tuple[instr: InstrPos, id: uint32, inTry: uint32]]
      ## unstructured exits. An `id` of ``high(uint32)`` means that it's
      ## exceptional control-flow.

    inTry: uint32
    numJoins: int

const
  RaiseLabel = high(uint32)
  ExitLabel = 0'u32

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

func dfaOp(env: var ClosureEnv, opc: Opcode, n: NodePosition, v: OpValue) =
  {.cast(uncheckedAssign).}:
    env.instrs.add Instr(op: opc, node: n, val: v)

func dfaOp(env: var ClosureEnv, opc: Opcode, tree: MirTree, n: NodePosition,
           v: OpValue) {.inline.} =
  ## Only emits an instruction if the operand is an lvalue.
  if tree[v].kind in LvalueExprKinds:
    dfaOp(env, opc, n, v)

func emit(env: var ClosureEnv, opc: Opcode, n: NodePosition): InstrPos =
  env.instrs.add Instr(op: opc, node: n)
  env.instrs.high.InstrPos

func exit(env: var ClosureEnv, opc: Opcode, pos: NodePosition, blk: uint32) =
  env.exits.add (emit(env, opc, pos), blk, env.inTry)

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

func emitForArgs(env: var ClosureEnv, tree: MirTree, at, source: NodePosition) =
  for it in subNodes(tree, source):
    case tree[it].kind
    of mnkArg:
      emitLvalueOp(env, opUse, tree, at, tree.operand(it))
    of mnkConsume:
      emitLvalueOp(env, opConsume, tree, at, tree.operand(it))
    of mnkName:
      emitForValue(env, tree, at, tree.skip(tree.operand(it), mnkTag))
    of mnkField, mnkMagic:
      discard
    else:
      emitLvalueOp(env, opUse, tree, at, OpValue it)

func emitForExpr(env: var ClosureEnv, tree: MirTree, at, source: NodePosition) =
  ## Emits the data- and control-flow instructions corresponding to the
  ## expression at `source`.
  template op(o: Opcode, v: OpValue) =
    env.dfaOp(o, tree, at, v)

  case tree[source].kind
  of mnkCall, mnkCheckedCall, mnkConstr, mnkObjConstr:
    emitForArgs(env, tree, at, source)
  of mnkConv, mnkStdConv, mnkCast:
    # a read is performed on the source operand (if it's an lvalue)
    emitLvalueOp(env, opUse, tree, at, tree.operand(source))
  of mnkAddr:
    # ``addr`` doesn't actually read its operand location, rather
    # it create a run-time handle (i.e., pointer) to them. Since those
    # handles aren't tracked however, the operation is conservatively
    # treated as a mutation
    emitLvalueOp(env, opMutate, tree, at, tree.operand(source))
  of mnkView:
    # if the created view supports mutation, treat the creation as a
    # mutation itself
    let opc =
      if tree[source].typ.kind == tyVar: opMutate
      else:                              opUse
    emitLvalueOp(env, opc, tree, at, tree.operand(source))
  of mnkToSlice:
    # slices aren't tracked at the moment, so the mere creation of a slice is
    # treated as a usage of the sequence. If the resulting openArray supports
    # mutation, creation of the slice is treated as a mutation. To ensure the
    # correct data-flow operation order for the mutation case, the lower/upper
    # bound operands are treated as being evaluated (i.e., used) first
    if numArgs(tree, source) == 3:
      emitLvalueOp(env, opUse, tree, at, tree.operand(source, 1))
      emitLvalueOp(env, opUse, tree, at, tree.operand(source, 2))

    let opc =
      if tree[source].typ.kind == tyVar: opMutate
      else:                              opUse
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
  of mnkNone, mnkLiteral, mnkProc:
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
      exit env, opFork, at, RaiseLabel
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
  ## Computes the data-flow graph for the given `tree`. This is an
  ## expensive operation! The high cost is due to two essential reasons:
  ##
  ## 1. a control-flow graph needs to materialize all edges for a given `tree`
  ## 2. the amount of allocations/bookkeeping necessary to do so
  ##
  ## The most amount of bookkeeping is required for finalizers and exceptions.

  template emit(opc: Opcode, n: NodePosition): InstrPos =
    env.instrs.add Instr(op: opc, node: n)
    env.instrs.high.InstrPos

  template join(pos: NodePosition): JoinId =
    let id = JoinId env.numJoins
    inc env.numJoins
    env.instrs.add Instr(op: opJoin, node: pos, id: id)
    id

  proc findBlock(env: ClosureEnv, label: Option[LabelId]): uint32 =
    var i = env.blocks.high
    # search for the unstructured control-flow target for `label`
    while i >= 0 and env.blocks[i] != label:
      dec i

    assert i >= 0, "invalid exit"
    result = uint32(i + 1)

  template findHidden(): uint32 = findBlock(env, none(LabelId))

  template exit(opc: Opcode, pos: NodePosition, blk: uint32) =
    env.exits.add (emit(opc, pos), blk, env.inTry)

  template push(opc: Opcode, pos: NodePosition) =
    env.structStack.add emit(opc, pos)

  proc pop(env: var ClosureEnv, p: NodePosition) =
    let start = env.structStack.pop()
    case env.instrs[start].op
    of opJoin:
      let id = env.instrs[start].id
      env.instrs.add Instr(op: opLoop, node: p, dest: id)
    of opFork, opGoto, opLoop:
      let id = join(p)
      env.instrs[start].dest = id
    of DataFlowOps:
      discard

  iterator updateTargets(env: var ClosureEnv, n: NodePosition,
                         update: var bool): auto {.inline.} =
    ## Yields all exits that are part of the active 'try' statement. If
    ## `update` is 'true' when control is given back to the iterator, the exit
    ## is removed from the list and the destination of the associated
    ## instruction set to `n`.
    var
      id = none(JoinId) # only create a 'join' if really needed
      i = 0
    while i < env.exits.len:
      if env.exits[i].inTry >= env.inTry:
        update = false
        yield env.exits[i]

        if update:
          if id.isNone:
            id = some(join n)

          env.instrs[env.exits[i].instr].dest = id.unsafeGet
          del(env.exits, i)
        else:
          inc i
      else:
        inc i

  template open(label: LabelId) =
    env.blocks.add some(label)
  template openHidden() =
    env.blocks.add none(LabelId)

  proc close(env: var ClosureEnv, i: NodePosition) =
    var upd = false
    for exit in updateTargets(env, i, upd):
      upd = exit.id == env.blocks.len.uint32
    discard env.blocks.pop()

  var
    env = ClosureEnv()
    finallyExits: seq[tuple[id: uint32, inTry: uint32]]

  for i, n in tree.pairs:
    case n.kind
    of mnkIf:
      emitLvalueOp(env, opUse, tree, i, tree.operand(i, 0))
      push opFork, i
    of mnkBranch:
      # optimization: the first branch doesn't use a CFG edge
      if tree[i-2].kind != mnkCase and tree[i-1].kind != mnkExcept:
        pop(env, i)
    of mnkRepeat:
      # add a 'join' for the 'loop' that is emitted at the end of the repeat
      discard join(i)
      env.structStack.add env.instrs.high.InstrPos
    of mnkBlock:
      open n.label
    of mnkCase:
      emitLvalueOp(env, opUse, tree, i, tree.operand(i, 0))
      openHidden() # for the branch exits
      # fork to all branches except the the first one:
      for _ in 0..<tree[i].len-1:
        push opFork, i
    of mnkTry:
      openHidden()
      inc env.inTry
    of mnkExcept:
      # first, add a structured exit for the tried statement (which
      # immediately precedes the handler):
      exit opGoto, i-1, findHidden()
      # set the join point for all exits targeting the 'raise' label:
      var upd = false
      for exit in updateTargets(env, i, upd):
        upd = exit.id == RaiseLabel

      # fork to the exception matchers:
      for _ in 0..<tree[i].len-1:
        push opFork, i
    of mnkFinally:
      if not (tree[i-1].kind == mnkEnd and tree[i-1].start == mnkExcept):
        # this is a finally clause for a try statement without an exception
        # handler. We need the structured exit for the tried statement to
        # know where to resume at the end of the clause
        exit opGoto, i-1, findHidden()

      let start = finallyExits.len

      var upd = false
      for exit in updateTargets(env, i, upd):
        upd = true
        # make sure that the list stays sorted while deduplicating exits:
        var ins = start
        while ins < finallyExits.len and finallyExits[ins].id < exit.id:
          inc ins

        if ins == finallyExits.len or finallyExits[ins].id != exit.id:
          # the exit is not part of the surrounding 'try', so subtract 1
          finallyExits.insert((exit.id, env.inTry-1), ins)

      # the code inside the finally clause is control-flow wise not part of
      # the 'try'
      dec env.inTry
    of mnkBreak:
      exit opGoto, i, findBlock(env, some n.label)
    of mnkReturn:
      exit opGoto, i, ExitLabel
    of mnkRaise:
      # raising an exception consumes it:
      if tree[tree.operand(i)].kind != mnkNone:
        emitLvalueOp(env, opConsume, tree, i, tree.operand(i))

      exit opGoto, i, RaiseLabel # go to closest handler
    of mnkEnd:
      case n.start
      of mnkBranch:
        # XXX: the goto is redundant/unnecessary if the body doesn't have a
        #      structured exit
        # only create an edge for branches that are not the last one
        if tree[i+1].kind != mnkEnd:
          exit opGoto, i, findHidden()
      of mnkIf, mnkRepeat:
        pop(env, i)

      # unstructured exits:
      of mnkBlock, mnkCase:
        close(env, i)
      of mnkTry:
        close(env, i)

        if not (tree[i-1].kind == mnkEnd and tree[i-1].start == mnkFinally):
          # if no finally clause exists, we need to patch the ``inTry``
          # values for the exits. Effictively, the exits in a 'try' without
          # a 'finally' clause become part of the surrounding 'try' (if any)
          dec env.inTry
          for exit in env.exits.mitems:
            exit.inTry = min(exit.inTry, env.inTry)

      # handlers and finally:
      of mnkExcept:
        # after a structured exit of the handler, control-flow continues after
        # the code section it is attached to. Node-wise, this is the ``end``
        # node terminating the ``try`` the handler is part of
        # TODO: with this approach, either the last branch needs to fork
        #       control-flow to the next exception handler or ``mirgen`` has to
        #       introduce a catch-all handler
        if tree[i+1].kind == mnkFinally:
          # only add an edge if the there's a finalizer -- no edge is needed
          # if there's none
          exit opGoto, i, findHidden()
      of mnkFinally:
        # search for the start of the relevant exits:
        var start = finallyExits.len
        while start > 0 and finallyExits[start-1].inTry == env.inTry:
          dec start

        # where control-flow continues after exiting the finalizer depends on
        # the jumps it intercepts. We represent this in the CFG by emitting a
        # 'fork' instruction targeting the destination of each intercepted jump
        for x in start..<finallyExits.len-1:
          exit opFork, i, finallyExits[x].id

        # the last target needs to be linked via a 'goto' instruction, as the
        # finalizer must not be exited via fallthrough. The finally clause
        # having no exits is valid: it means that the finalizer is unused
        # (control-flow never enters it)
        if start < finallyExits.len:
          exit opGoto, i, finallyExits[^1].id

        finallyExits.setLen(start)
      else:
        # no control-flow or other effects
        discard

    of mnkDef, mnkDefCursor, mnkDefUnpack, mnkAsgn, mnkInit:
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
      unreachable("not implemented yet")
    of mnkEmit, mnkAsm:
      emitForArgs(env, tree, i, i)

    else:
      discard "not relevant"

  assert env.inTry == 0

  if env.exits.len > 0:
    let id = join tree.len.NodePosition
    # all unhandled exits (including raise exits) that reach here exit the
    # processed tree:
    for it in env.exits.items:
      env.instrs[it.instr].dest = id

  swap(env.instrs, result.instructions)

  # looking up the position of the ``opcJoin`` instruction that defines a given
  # join point is a very common operation, so we cache this information for
  # efficiency
  result.map.newSeq(env.numJoins)
  for i, instr in result.instructions.lpairs:
    if instr.op == opJoin:
      result.map[instr.id] = InstrPos(i)

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

      inc pc

      if state.exit or pc == start:
        # abort the current path if we either reached the instruction we
        # started at or the path was manually killed
        state.exit = false
        abort()

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
    of opGoto, opFork, DataFlowOps:
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

    result.add " -> " & $ord(n.node) & "\n"
