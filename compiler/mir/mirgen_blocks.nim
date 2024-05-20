## Subordinate module to `mirgen <#mirgen>`_. Implements the low-level control-
## flow-related parts, as well as destructor injection.

import
  std/[
    options
  ],
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirconstr,
    mirtrees
  ],
  compiler/utils/[
    idioms
  ]

type
  BlockKind* = enum
    bkBlock
    bkScope
    bkTryExcept
    bkTryFinally
    bkFinally
    bkExcept

  Block* = object
    ## Information about a block-like structure. This not only includes |NimSkull|
    ## ``block``s, but also try, finally, etc.
    id*: Option[LabelId]
      ## the block's label. Initialized on-demand, meaning that 'none'
      ## indicates that the block is unused
    case kind*: BlockKind
    of bkBlock:
      label*: PSym
        ## the symbol of the block's label. nil if it's an internal block
    of bkScope:
      numRegistered: int
        ## number of entities registered for the scope in the to-destroy list
      scopeExits: seq[LabelId]
        ## unordered set of follow-up targets
    of bkTryFinally:
      doesntExit*: bool
        ## whether structured control-flow doesn't reach the end of the finally
      exits*: seq[LabelId]
        ## unordered set of follow-up targets
    of bkTryExcept, bkFinally, bkExcept:
      discard

  BlockCtx* = object
    ## Per-procedure block-related state.
    blocks: seq[Block]
      ## stack of enclosing try, finally, etc. blocks
    toDestroy: seq[tuple[entity: Value, label: Option[LabelId]]]
      ## all locals/globals/temporaries that need destruction, together
      ## with the label of the finally that needs to be entered when
      ## destroying
    currScope: int
      ## block index of the current scope

# shorten some common parameter declarations:
using
  c: var BlockCtx
  bu: var MirBuilder

proc requestLabel(bu; b: var Block): LabelId =
  if b.id.isNone:
    b.id = some bu.allocLabel()
  result = b.id.unsafeGet

proc labelNode*(label: LabelId): MirNode =
  MirNode(kind: mnkLabel, label: label)

proc emitDestroy(bu; val: Value) =
  bu.subTree mnkDestroy:
    bu.use val

proc emitFinalizerLabels(c; bu; locs: Slice[int]) =
  ## Emits the labels for all scope finalizers required for cleaning up the
  ## registered entities in `locs`.
  # destruction happens in reverse, so iterate from high to low
  for i in countdown(locs.b, locs.a):
    if c.toDestroy[i].label.isSome:
      bu.add labelNode(c.toDestroy[i].label.unsafeGet)

proc blockLeaveActions(c; bu; targetBlock: int): bool =
  ## Emits the actions for leaving the blocks up until (but not including)
  ## `targetBlock`. Returns false when there's an intercepting
  ## ``finally`` clause that doesn't exit (meaning that `targetBlock` won't
  ## be reached), true otherwise.
  proc incl[T](s: var seq[T], it: T) {.inline.} =
    if it notin s:
      s.add it

  proc inclExit(b: var Block, it: LabelId) {.inline.} =
    case b.kind
    of bkTryFinally: b.exits.incl it
    of bkScope:      b.scopeExits.incl it
    else: unreachable()

  var
    last = c.toDestroy.high
    previous = -1

  for i in countdown(c.blocks.high, targetBlock + 1):
    let b {.cursor.} = c.blocks[i]
    case b.kind
    of bkBlock, bkTryExcept:
      discard "nothing to do"
    of bkExcept, bkFinally:
      # needs a leave action
      bu.add MirNode(kind: mnkLeave, label: b.id.get)
    of bkScope:
      if b.numRegistered > 0:
        # there are some locations that require cleanup
        if c.toDestroy[last].label.isNone:
          c.toDestroy[last].label = some bu.allocLabel()

        if previous != -1:
          c.blocks[previous].inclExit c.toDestroy[last].label.unsafeGet

        previous = i
        # emit the labels for all scope finalizers that need to be run
        emitFinalizerLabels(c, bu, (last-b.numRegistered+1)..last)

        last -= b.numRegistered
    of bkTryFinally:
      let label = bu.requestLabel(c.blocks[i])
      # register as outgoing edge of the preceding finally (if any):
      if previous != -1:
        c.blocks[previous].inclExit label

      previous = i

      # enter the finally clause:
      bu.add labelNode(label)
      if b.doesntExit:
        # structured control-flow doesn't leave the finally; the finally is
        # the final jump target
        return false

  if targetBlock >= 0 and previous != -1 and
     c.blocks[targetBlock].kind in {bkBlock, bkTryExcept}:
    # register the target as the follow-up for the previous finally
    c.blocks[previous].inclExit bu.requestLabel(c.blocks[targetBlock])

  result = true

template add*(c: var BlockCtx; b: Block) =
  c.blocks.add b

template pop*(c: var BlockCtx): Block =
  c.blocks.pop()

proc closest*(c: BlockCtx): int =
  ## Returns the index of the closest block.
  result = c.blocks.high
  while result >= 0 and c.blocks[result].kind != bkBlock:
    dec result
  assert result >= 0, "no enclosing block?"

proc findBlock*(c: BlockCtx, label: PSym): int =
  ## Returns the index of the block with label `label`.
  var i = c.blocks.high
  while i >= 0 and (c.blocks[i].kind != bkBlock or c.blocks[i].label != label):
    dec i
  assert i >= 0, "no enclosing block?"
  result = i

proc blockExit*(c; bu; targetBlock: int) =
  ## Emits the jump target description for a jump to `targetBlock`.
  # XXX: a target list is only necessary if there's more than one jump
  #      target
  bu.subTree mnkTargetList:
    if blockLeaveActions(c, bu, targetBlock):
      bu.add labelNode(bu.requestLabel(c.blocks[targetBlock]))

proc raiseExit*(c; bu) =
  ## Emits the jump target description for a jump to the nearest enclosing
  ## exception handler.
  var i = c.blocks.high
  while i >= 0 and c.blocks[i].kind != bkTryExcept:
    dec i

  bu.subTree mnkTargetList:
    if blockLeaveActions(c, bu, i):
      if i == -1:
        # nothing handles the exception within the current procedure
        bu.add MirNode(kind: mnkResume)
      else:
        bu.add labelNode(bu.requestLabel(c.blocks[i]))

proc closeBlock*(c; bu): bool =
  ## Finishes the current block. If required for the block (because it is a
  ## ``block`` and broken out of), emits a join and returns true, false
  ## otherwise.
  let blk = c.blocks.pop()
  # if there's no label, the exit of the block is never jumped to
  # and the join can be omitted
  if blk.kind == bkBlock and blk.id.isSome:
    bu.join blk.id.unsafeGet
    result = true

func register*(c; loc: Value) =
  ## Registers `loc` for destruction at the end of the current scope.
  ## Destruction happens in the reverse order the locations are registered in.
  inc c.blocks[c.currScope].numRegistered
  c.toDestroy.add (loc, none LabelId)

proc startScope*(c): int =
  ## Starts a new scope and returns the index of the last one.
  result = c.currScope
  c.blocks.add Block(kind: bkScope)
  c.currScope = c.blocks.high

proc earlyExit*(c; bu) =
  ## Emits the destroy operations for when structured control-flow reaches the
  ## current scope's end. All entities for which a destroy operation is
  ## emitted are unregistered already.
  let start = c.toDestroy.len - c.blocks[c.currScope].numRegistered
  var i = c.toDestroy.high

  while i >= start and c.toDestroy[i].label.isNone:
    bu.emitDestroy(c.toDestroy[i].entity)
    dec i

  # unregister the entities for which a destroy operation was emitted:
  c.blocks[c.currScope].numRegistered = i - start + 1
  c.toDestroy.setLen(i + 1)

proc closeScope*(c; bu; nextScope: int, hasStructuredExit: bool) =
  ## Pops the scope from the stack and emits the scope exit actions.
  ## `hasStructuredExit` tells whether structured control-flow reaches
  ## the end of the scope, affecting how the exit looks like.
  ##
  ## `next` is the index of the scope index returns by the previous
  ## `startScope <#startScope,BlockCtx>`_ call.
  # emit all destroy operations that don't need a finally
  earlyExit(c, bu)

  var scope = c.blocks.pop()
  assert scope.kind == bkScope

  let start = c.toDestroy.len - scope.numRegistered

  var next = none LabelId
  if start < c.toDestroy.len and hasStructuredExit:
    # there are destroy operations that need a finally. A goto is required
    # for visiting them
    next = some bu.allocLabel()
    bu.subTree mnkGoto:
      bu.subTree mnkTargetList:
        emitFinalizerLabels(c, bu, start..c.toDestroy.high)
        bu.add labelNode(next.unsafeGet)

    scope.scopeExits.add next.unsafeGet

  # emit all finally sections for the scope. Since not all entities requiring
  # destruction necessarily start their existence at the start of the scope,
  # multiple sections may be required
  var curr = none LabelId
  for i in countdown(c.toDestroy.high, start):
    # if a to-destroy entry has a label, it marks the start of a new finally
    if c.toDestroy[i].label.isSome:
      if curr.isSome:
        # finish the previous finally by emitting the corresponding 'continue':
        bu.subTree MirNode(kind: mnkContinue, len: 2):
          bu.add labelNode(curr.unsafeGet)
          # a finally section that's not the last one always continues with
          # the next finally
          bu.add labelNode(c.toDestroy[i].label.unsafeGet)

      curr = c.toDestroy[i].label
      bu.subTree mnkFinally:
        bu.add labelNode(curr.unsafeGet)

    bu.emitDestroy(c.toDestroy[i].entity)

  if curr.isSome:
    # finish the final finally. `scopeExits` stores all the possible
    bu.subTree MirNode(kind: mnkContinue, len: uint32(1 + scope.scopeExits.len)):
      bu.add labelNode(curr.unsafeGet)
      for it in scope.scopeExits.items:
        bu.add labelNode(it)

  if next.isSome:
    # the join point for the structured scope exit
    bu.join next.unsafeGet

  # unregister all entities registered with the scope:
  c.toDestroy.setLen(start)
  c.currScope = nextScope
