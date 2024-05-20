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
  ]

type
  BlockKind* = enum
    bkBlock
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

proc blockLeaveActions(c; bu; targetBlock: int): bool =
  ## Emits the actions for leaving the blocks up until (but not including)
  ## `targetBlock`. Returns false when there's an intercepting
  ## ``finally`` clause that doesn't exit (meaning that `targetBlock` won't
  ## be reached), true otherwise.
  proc incl[T](s: var seq[T], it: T) {.inline.} =
    if it notin s:
      s.add it

  var previous = -1 # previous finally
  for i in countdown(c.blocks.high, targetBlock + 1):
    let b {.cursor.} = c.blocks[i]
    case b.kind
    of bkBlock, bkTryExcept:
      discard "nothing to do"
    of bkExcept, bkFinally:
      # needs a leave action
      bu.add MirNode(kind: mnkLeave, label: b.id.get)
    of bkTryFinally:
      let label = bu.requestLabel(c.blocks[i])
      # register as outgoing edge the preceding finally (if any):
      if previous != -1:
        c.blocks[previous].exits.incl label

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
    c.blocks[previous].exits.incl bu.requestLabel(c.blocks[targetBlock])

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
