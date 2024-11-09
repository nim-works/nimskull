## Subordinate module to `mirgen <#mirgen>`_. Implements the block and scope
## management required for translating the AST's high-level control-flow
## constructs to the MIR's goto-based ones. Injecting ``mnkDestroy``
## operations is also implemented here, integrated with the scope management.

import
  std/[
    options
  ],
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirconstr,
    mirenv,
    mirtrees,
    mirtypes
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
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
    of bkTryFinally:
      selector*: Option[Value]
        ## the variable to store the destination index in
      exits*: seq[int]
        ## a set of all original target block indices
    of bkFinally:
      selectorVar*: Value
        ## the selector storing the dispatcher's target index
      excState*: int
        ## the selector value representing the "finally entered via exception"
        ## state
    of bkTryExcept, bkExcept:
      discard

  BlockCtx* = object
    ## Per-procedure block-related state.
    blocks: seq[Block]
      ## stack of enclosing try, finally, etc. blocks
    toDestroy: seq[tuple[entity: Value, label: Option[LabelId]]]
      ## all locals/globals/temporaries that need destruction, together
      ## with the label of the finally that the destroy operation is part
      ## of. Only the items where the `label` changes have an initialized
      ## label
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

proc isInFinally*(c: BlockCtx): bool =
  c.blocks.len > 0 and c.blocks[^1].kind == bkFinally

proc blockExit*(c; graph: ModuleGraph; env: var MirEnv; bu; targetBlock: int) =
  ## Emits a goto jumping to the `targetBlock`, together with the necessary scope
  ## and exception cleanup logic. If the jump crosses a try/finally, the
  ## finally is jumped to instead.
  proc incl[T](s: var seq[T], val: T): int =
    for i, it in s.pairs:
      if it == val:
        return i

    result = s.len
    s.add val

  var last = c.toDestroy.high

  for i in countdown(c.blocks.high, targetBlock + 1):
    let b {.cursor.} = c.blocks[i]
    case b.kind
    of bkScope:
      let start = last - b.numRegistered
      var j = last
      while j > start:
        bu.emitDestroy(c.toDestroy[j].entity)
        dec j

      last = start
    of bkTryFinally:
      if b.selector.isSome:
        # add the target as an exit of the try:
        let pos = c.blocks[i].exits.incl(targetBlock)
        bu.subTree mnkAsgn:
          bu.use b.selector.unsafeGet
          bu.use literal(mnkIntLit, env.getOrIncl(pos.BiggestInt), UInt32Type)

      # enter to the intercepting finally
      bu.subTree mnkGoto:
        bu.add labelNode(bu.requestLabel(c.blocks[i]))
      return
    of bkFinally:
      # emit a conditional abort:
      let tmp = bu.wrapTemp BoolType:
        bu.buildMagicCall mEqI, BoolType:
          bu.emitByVal b.selectorVar
          bu.emitByVal:
            literal(mnkIntLit, env.getOrIncl(b.excState.BiggestInt),
                    UInt32Type)

      bu.buildIf (;bu.use tmp):
        bu.subTree mnkVoid:
          let p = graph.getCompilerProc("nimAbortException")
          bu.buildCall env.procedures.add(p), VoidType:
            bu.emitByVal literal(mnkIntLit, env.getOrIncl(0), BoolType)
    of bkExcept:
      bu.subTree mnkVoid:
        let p = graph.getCompilerProc("nimLeaveExcept")
        bu.buildCall env.procedures.add(p), VoidType:
          discard
    of bkBlock, bkTryExcept:
      discard "nothing to do"

  # no intercepting finally exists
  bu.subTree mnkGoto:
    bu.add labelNode(bu.requestLabel(c.blocks[targetBlock]))

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


proc raiseExit*(c; bu) =
  ## Emits the jump target for a jump to the nearest enclosing
  ## exception handler.
  var last = c.toDestroy.high

  for i in countdown(c.blocks.high, 0):
    let b {.cursor.} = c.blocks[i]
    case b.kind
    of bkBlock:
      discard "nothing to do"
    of bkScope:
      if b.numRegistered > 0:
        # there are some locations that require cleanup
        if c.toDestroy[last].label.isNone:
          c.toDestroy[last].label = some bu.allocLabel()

        bu.add labelNode(c.toDestroy[last].label.unsafeGet)
        return
    of bkTryExcept, bkTryFinally, bkFinally, bkExcept:
      # something that intercepts the exceptional control-flow
      bu.add labelNode(bu.requestLabel(c.blocks[i]))
      return

  # no local exception handler exists
  bu.add MirNode(kind: mnkResume)

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
  ## Starts a new scope and returns the index of the previous one.
  result = c.currScope
  c.blocks.add Block(kind: bkScope)
  c.currScope = c.blocks.high


proc closeScope*(c; bu; nextScope: int, hasStructuredExit: bool) =
  ## Pops the scope from the stack and emits the scope exit actions.
  ## `hasStructuredExit` tells whether structured control-flow reaches
  ## the end of the scope, affecting how the exit looks like.
  ##
  ## `next` is the index of the scope index returns by the previous
  ## `startScope <#startScope,BlockCtx>`_ call.
  # emit all destroy operations that don't need a finally
  var scope = c.blocks.pop()
  assert scope.kind == bkScope

  let start = c.toDestroy.len - scope.numRegistered

  if hasStructuredExit:
    var i = c.toDestroy.high
    while i >= start:
      bu.emitDestroy(c.toDestroy[i].entity)
      dec i

  # look for the first destroy that needs a 'finally'
  var i = c.toDestroy.high
  while i >= start and c.toDestroy[i].label.isNone:
    dec i

  if i >= start:
    # some exceptional exits need cleanup
    var next = none LabelId
    if hasStructuredExit:
      # emit a jump over the finalizers:
      next = some bu.allocLabel()
      bu.goto next.unsafeGet

    # emit all finally sections for the scope. Since not all entities requiring
    # destruction necessarily start their existence at the start of the scope,
    # multiple sections may be required
    var curr = none LabelId
    for i in countdown(i, start):
      # if a to-destroy entry has a label, it marks the start of a new finally
      if c.toDestroy[i].label.isSome:
        if curr.isSome:
          # finish the previous finally by emitting the corresponding
          # 'continue':
          bu.subTree mnkContinue:
            bu.add labelNode(c.toDestroy[i].label.unsafeGet)

        curr = c.toDestroy[i].label
        bu.subTree mnkFinally:
          bu.add labelNode(curr.unsafeGet)

      bu.emitDestroy(c.toDestroy[i].entity)

    # unregister all entities registered with the scope. This needs to happen
    # before the ``raiseExitActions`` call below
    c.toDestroy.setLen(start)

    if curr.isSome:
      # continue raising the exception
      bu.subTree mnkContinue:
        raiseExit(c, bu)

    if next.isSome:
      bu.join next.unsafeGet
  else:
    # unregister all entities registered with the scope:
    c.toDestroy.setLen(start)

  c.currScope = nextScope
