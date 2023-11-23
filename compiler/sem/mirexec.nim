## This module implements algorithms for traversing a control-flow graph (=CFG)
## in forward or backward direction. In the abstract, it means visiting the
## nodes in topological-order (forward) or post-order (backward), with special
## handling for loops.
##
## In the compiler, this is mainly used for control- and/or data-flow
## analysis, meant to propagate properties through the graph or to to answer
## questions such as: "is point A connected to point B?", "is X initialized on
## all paths?", etc.
##
## Before traversing a control-flow graph, one has to be created via
## ``computeCfg`` first. Instead of a pointer-based graph structure, the graph
## is represented via a linear list of instruction.
##
## TODO: expand this section with how the algorithms used by for- and backward
##       traversal work

# XXX: with a small to medium amount of work, the algorithms and
#      ``ControlFlowGraph`` can be generalized to work independent from the
#      MIR. The current ``computeCfg`` would be moved to somewhere else

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

from compiler/ast/ast_query import magicsThatCanRaise

type
  Opcode = enum
    opFork ## branching control-flow that cannot introduce a cycle
    opGoto ## unconditional jump that cannot introduce a cycle
    opLoop ## unconditional jump to the start of a loop. The start of a cycle
    opJoin ## defines a join point

  # TODO: make both types distinct
  InstrPos = int32
  JoinId = uint32

  Instr = object
    node: NodePosition
    case op: Opcode
    of opFork, opGoto, opLoop:
      dest: JoinId
    of opJoin:
      id: JoinId

  ControlFlowGraph* = object
    ## The control-flow graph is represented as a linear sequence of
    ## instructions
    instructions: seq[Instr]
    map: seq[InstrPos] ## joind ID -> instruction

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
    i: NodePosition
      ## points to the next item to yield

func incl[T](s: var seq[T], v: sink T) =
  ## If not present already, adds `v` to the sorted ``seq`` `s`
  var i = 0
  while i < s.len and s[i] < v:
    inc i

  if i >= s.len or s[i] != v:
    s.insert(v, i)

func compare(a: Instr, b: NodePosition): int =
  ord(a.node) - ord(b)

template lowerBound(c: ControlFlowGraph, node: NodePosition): InstrPos =
  lowerBound(c.instructions, node, compare).InstrPos

template upperBound(c: ControlFlowGraph, node: NodePosition): InstrPos =
  upperBound(c.instructions, node, compare).InstrPos

func `[]`(c: ControlFlowGraph, pc: SomeInteger): lent Instr {.inline.} =
  c.instructions[pc]

func computeCfg*(tree: MirTree): ControlFlowGraph =
  ## Computes the control-flow graph for the given `tree`. This is a very
  ## expensive operation! The high cost is due to two essential reasons:
  ##
  ## 1. a control-flow graph needs to materialize all edges for a given `tree`
  ## 2. the amount of allocations/bookkeeping necessary to do so
  ##
  ## The most amount of bookkeeping is required for finalizers and exceptions.
  type
    ClosureEnv = object
      instrs: seq[Instr]
      ifs: seq[InstrPos]
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
    env.ifs.add emit(opc, pos)

  proc pop(env: var ClosureEnv, p: NodePosition) =
    let start = env.ifs.pop()
    case env.instrs[start].op
    of opJoin:
      let id = env.instrs[start].id
      env.instrs.add Instr(op: opLoop, node: p, dest: id)
    of opFork, opGoto, opLoop:
      let id = join(p)
      env.instrs[start].dest = id

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

  # open NoLabel, true # the unstructured exit of the whole tree

  for i, n in tree.pairs:
    case n.kind
    of mnkCall:
      if geRaises in n.effects:
        # the control-flow graph only encodes procedure-local control-flow, so
        # a procedure call that might raise an exception is treated as forking
        # to the closest handler
        exit opFork, i, RaiseLabel

    of mnkMagic:
      if n.magic in magicsThatCanRaise:
        exit opFork, i, RaiseLabel

    of mnkIf:
      push opFork, i
    of mnkBranch:
      # optimization: the first branch doesn't use a CFG edge
      if tree[i-1].kind notin {mnkCase, mnkExcept}:
        pop(env, i)
    of mnkRegion:
      # a region is specified to have no obsersvable control-flow effects, so
      # we effectively skip it. The control-flow instructions for the
      # intra-region control-flow are still generated -- by default, they're
      # just skipped over
      push opGoto, i
    of mnkRepeat:
      # add a 'join' for the 'loop' that is emitted at the end of the repeat
      discard join(i)
      env.ifs.add env.instrs.high.InstrPos
    of mnkBlock:
      open n.label
    of mnkCase:
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
      exit opGoto, i, RaiseLabel # go to closest handler
    of mnkEnd:
      case n.start
      of mnkBranch:
        # XXX: the goto is redundant/unnecessary if the body doesn't have a
        #      structured exit
        # only create an edge for branches that are not the last one
        if tree[i+1].kind != mnkEnd:
          exit opGoto, i, findHidden()
      of mnkIf, mnkRegion, mnkRepeat:
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
        discard

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

iterator traverse*(tree: MirTree, c: ControlFlowGraph,
                   span: Slice[NodePosition], start: NodePosition,
                   state: var TraverseState): (NodePosition, lent MirNode) =
  ## Starts at `start + 1` and traverses/yields all basic blocks inside `span`
  ## in control-flow order. That is, except for in the context of loops, each
  ## basic block is yielded before those having a control-flow dependency on
  ## it. Traversal begins at `start`, which is allowed to point inside a basic
  ## block.
  ##
  ## The same basic block may be yielded multiple times. This is not a general
  ## limitation, but rather because of a shortcut taken by the implementation.
  ##
  ## `state` is used for bi-directional communication -- see the documentation
  ## of ``TraverseState`` for more information.
  assert start in span
  var
    i = start + 1
    pc: InstrPos
    queue: seq[InstrPos]
    visited: PackedSet[JoinId]


  state = TraverseState()

  template resume() =
    if queue.len > 0:
      pc = queue[0]
      queue.delete(0)

      assert c[pc].op == opJoin
      i = c[pc].node - 1
    else:
      # no more threads left -> exit
      i = span.b + 1 # move pc past the end
      break

  template push(target: JoinId) =
    ## If the destination position is inside the active span, adds
    ## it to the execution queue, effectively starting a new thread. Records
    ## an escape otherwise
    let dst = c.map[target]
    if c[dst].node in span:
      queue.incl dst
    else:
      state.escapes = true

  template blockEnd(): NodePosition =
    if pc < c.instructions.len:
      c[pc].node
    else:
      NodePosition(tree.len)

  template abort() =
    ## Exit the current thread and continue with the next one in the queue
    resume()
    next = blockEnd()

  pc = lowerBound(c, i)
  var next = blockEnd()

  # XXX: this loop can be optimized further. Instead of yielding each item
  #      from separately, the basic block could be yielded as a slice instead
  while i <= span.b:
    yield (i, tree[i])

    if state.exit or i == start:
      state.exit = false
      abort()

    # if at the end of a basic block, execute all CFG instructions associated
    # with it:
    while i == next:
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

      inc pc
      next = blockEnd()

    inc i

  assert queue.len <= 1

  state.exit = i == span.b + 1

template active(s: ExecState): bool =
  # if a thread is selected and it's either the or derived from the main
  # thread, execution is active
  s.time != 0 and s.time.uint16 <= s.top

template step(s: var ExecState) =
  dec s.time
  # remember the lowest time value we've reached so far:
  s.bottom = min(s.bottom, s.time)

func processJoin(id: JoinId, s: var ExecState, c: ControlFlowGraph) {.inline.} =
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
      s.i = c[p].node
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

iterator traverseReverse*(tree: MirTree, c: ControlFlowGraph,
                          span: Slice[NodePosition], start: NodePosition,
                          exit: var bool): (NodePosition, lent MirNode) =
  ## Starts at `start - 1` and visits and returns all basic blocks inside
  ## `span` in post-order.
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

  s.visited.newSeq(c.map.len)
  s.pc = upperBound(c, span.b) - 1
  # start activity *after* the start position is reached so that
  # the start node itself is not yielded
  s.i = start - 1
  s.top = high(Time)
  s.time = s.top
  s.bottom = s.time

  exit = false

  # move the program counter to the CFG instruction that marks the start of the
  # basic block `start` is located inside. While doing so, collect the loops
  # the start position is located inside:
  while s.pc >= 0 and c[s.pc].node > s.i:
    let instr = c[s.pc]
    case instr.op
    of opLoop:
      s.loops.add (instr.dest, s.pc, false)
    of opJoin:
      # the start of a loop; pop the previous loop entry:
      if s.loops.len > 0 and s.loops[^1].start == instr.id:
        s.loops.setLen(s.loops.len - 1)
    of opGoto, opFork:
      discard

    dec s.pc

  # perform the traversal:
  while s.i >= span.a:
    # execute all instructions located at the end of the basic block (if we're
    # at the end of one):
    while s.pc >= 0 and c[s.pc].node == s.i:
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

      dec s.pc

    # `next` is the position of the first item in the current basic block,
    # taking the provided `span` into account
    let next =
      if s.pc >= 0: max(c[s.pc].node + 1, span.a)
      else:         span.a

    assert next <= s.i

    let cross = next <= start and s.i >= start
      ## whether `start` is part of the next basic block

    # if they weren't visited yet, return all items in the current basic
    # block:
    if s.active:
      # prevent the first half of the basic block we started inside to be
      # returned:
      let adjusted =
        if cross: start
        else:     next

      # TODO: yield ``next..s.i`` instead and let the callsite do the
      #       iteration. It's much more flexible and we no longer need the
      #       `tree` parameter

      while s.i >= adjusted and not exit:
        yield (s.i, tree[s.i])
        dec s.i

      step(s)

      if exit:
        exit = false
        s.time = 0 # disable execution

    else:
      s.i = next - 1

    if cross:
      # we've reached the start position, so set the time to what it was at
      # the start
      s.time = high(Time)

  exit = s.active

iterator traverseFromExits*(tree: MirTree, c: ControlFlowGraph,
                            span: Slice[NodePosition], exit: var bool
                           ): (NodePosition, lent MirNode) =
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

  s.i = span.b
  s.pc = upperBound(c, s.i) - 1
  s.visited.newSeq(c.map.len)

  s.time = 0 # start as disabled
  s.top = EntryTime
  s.bottom = EntryTime

  exit = false

  template exits(target: JoinId): bool =
    c[c.map[target]].node notin span

  # for the most part similar to the loop in ``traverse``, but with special
  # handling for jumps outside of `span`
  while s.i >= span.a:
    # execute all instructions located at the end of the basic block (if we're
    # at the end of one):
    while s.pc >= 0 and c[s.pc].node == s.i:
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

      dec s.pc

    # `next` is the position of the first item in the current basic block,
    # taking the provided `span` into account
    let next =
      if s.pc >= 0: max(c[s.pc].node + 1, span.a)
      else:         span.a

    assert next <= s.i

    # if they weren't visited yet, return all items in the current basic
    # block:
    if s.active:
      # we want to yield all nodes up to and including `next`
      while s.i >= next and not exit:
        yield (s.i, tree[s.i])
        dec s.i

      # perform the time step. This has to happen *before* potentially setting
      # `time` to 0
      step(s)

      if exit:
        exit = false
        s.time = 0 # disable execution

    else:
      # -1 so that `i` points to the last itme of the next basic block
      s.i = next - 1

  exit = s.active


func `$`*(c: ControlFlowGraph): string =
  ## Renders the instructions of `c` as a human-readable text representation
  for i, n in c.instructions.pairs:
    case n.op
    of opJoin:
      result.add $n.id & ": join"
    of opGoto, opFork, opLoop:
      result.add $n.op & " " & $n.dest

    result.add " -> " & $ord(n.node) & "\n"