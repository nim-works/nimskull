## Implements the translation of CGIR control-flow constructs to JavaScript
## constructs.
##
## While JavaScript doesn't directly support the kind of control flow that
## the MIR supports, it does support 'finally' and 'catch', both which are
## leveraged for the translation. The idea: figure out which statements
## JavaScript blocks, 'finally's, and 'catch's need to enclose and then place
## them in a way such that the behaviour is translated correctly.

import
  std/[
    packedsets
  ],
  compiler/backend/[
    cgir
  ],
  compiler/utils/[
    idioms
  ]

type
  StructKind* = enum
    stkTry   ## start of a 'try' statement
    stkBlock ## start of a labeled block

    stkStructStart ## start of an 'if' or 'while'
    stkCatch   ## a 'catch' clause of a 'try' statement
    stkFinally ## a 'finally' clause of a 'try' statement

    stkEnd     ## end of a catch, finally, or block

    stkTerminator ## a goto or raise statement. Only relevant during analysis
    stkReturn     ## JavaScript return

  Structure* = object
    ## A list of ``Structure`` items describes how the JavaScript control-flow
    ## statements are laid out.
    stmt*: int
      ## the associated CGIR statement
    case kind*: StructKind
    of stkStructStart, stkTry, stkBlock, stkCatch, stkFinally, stkEnd:
      label*: BlockId
        ## the associated CGIR label
    of stkTerminator, stkReturn:
      discard

func finalTarget(n: CgNode): CgNode =
  case n.kind
  of cnkLabel:      n
  of cnkTargetList: n[^1]
  else:
    unreachable()

proc spawnOpens(items: var seq[Structure], pos: int, n: CgNode, isError: bool,
                finallys: PackedSet[BlockId], marker: var PackedSet[BlockId]) =
  ## Using the jump action description `n`, spawns the 'try' or labeled block
  ## openings needed for the jump targets. The set of targets for which
  ## opening was already spawned are tracked by `marker`.
  ##
  ## `pos` is the index of the statement. `isError` indicates whether `n`
  ## describes exceptional control-flow, and is required to interpret what the
  ## final jump target represents (exception handler or ordinary join point).
  let target: range[stkTry..stkBlock] = if isError: stkTry else: stkBlock

  case n.kind
  of cnkLabel:
    # direct jump to something that cannot be a finally section
    if not containsOrIncl(marker, n.label):
      items.add Structure(kind: target, stmt: pos, label: n.label)

  of cnkTargetList:
    for i in 0..<n.len-1:
      case n[i].kind
      of cnkLabel:
        # an intermediate target must be a finally section (so spawn a try)
        let label = n[i].label
        if not containsOrIncl(marker, label):
          items.add Structure(kind: stkTry, stmt: pos, label: label)
      of cnkLeave:
        discard "not handled here"
      else:
        unreachable()

    # special handling for the final target
    case n[^1].kind
    of cnkLabel:
      let label = n[^1].label
      if not containsOrIncl(marker, label):
        # the final target might be a finally section, in which case a
        # 'try' needs to be spawned, always
        if label in finallys:
          items.add Structure(kind: stkTry, stmt: pos, label: label)
        else:
          items.add Structure(kind: target, stmt: pos, label: label)
    of cnkResume:
      discard "no special handling for the 'resume' target"
    else:
      unreachable()

  else:
    unreachable(n.kind)

proc collectRecover(n: CgNode, finallys: PackedSet[BlockId],
                    needsRecover: var PackedSet[BlockId]) =
  ## Given the jump action description `n`, computes, based on the ``Leave``
  ## action, at which jump-targets updating the "current exception" is
  ## required.
  case n.kind
  of cnkLabel:
    discard "nothing to do"
  of cnkTargetList:
    var isOutgoing = false
    for it in n.items:
      case it.kind
      of cnkLabel:
        if isOutgoing:
          # a jump target that's reached after an exception handler is exited.
          # The proper current exception needs to be restored when landing
          needsRecover.incl it.label
          isOutgoing = false
      of cnkLeave:
        if it[0].label notin finallys: # exception handler?
          isOutgoing = true
      of cnkResume:
        discard "recovery is handled in the caller procedure"
      else:
        unreachable(n.kind)

  else:
    unreachable(n.kind)

proc toStructureList*(stmts: openArray[CgNode]): (seq[Structure], PackedSet[BlockId]) =
  ## Creates and returns the JavaScript control-flow-construct-focused
  ## representation for `stmts`. Also returns a set with all join points at
  ## which the current exception needs to be updated/restored.
  var
    structs = newSeq[Structure]()
    needsRecover = initPackedSet[BlockId]()
    finallys = initPackedSet[BlockId]()
    marker = initPackedSet[BlockId]()

  # before doing anything else, we need to know which labels belong to finallys
  for it in stmts.items:
    if it.kind == cnkFinally:
      finallys.incl it[0].label

  # the first step is computing the first statement that opening 'try's and
  # labeled blocks *must* enclose:
  #   def x = 0
  #   def y = f() -> [L0, L1]
  #   goto [L0, L2]
  #   L0: # finalizer
  #     ...
  #     Continue
  #   L1: # exception handler
  #     ...
  #   L2: ...
  #
  # Here, two JavaScript 'try' statements need to start right before the
  # `def y`, the first one for the finalizer, the second one for the exception
  # handler. A labeled JS block must enclose the `goto`.
  for i, it in stmts.pairs:
    template exit(n: CgNode, isError: bool) =
      spawnOpens(structs, i, n, isError, finallys, marker)
      collectRecover(n, finallys, needsRecover)

    template terminator() =
      structs.add Structure(kind: stkTerminator, stmt: i)

    template struct(k: StructKind, lbl: BlockId) =
      structs.add Structure(kind: k, stmt: i, label: lbl)

    case it.kind
    of cnkDef, cnkAsgn, cnkFastAsgn:
      if it[1].kind == cnkCheckedCall:
        exit(it[1][^1], isError=true)
    of cnkCheckedCall:
      exit(it[^1], isError=true)
    of cnkGotoStmt:
      exit(it[0], isError=false)
      let target = finalTarget(it[0])
      # if the goto jumps to a finally, there's no label for the break.
      # Since this can only happen when structured control-flow never
      # leaves the finally, we can use a JavaScript 'return' in that case
      if target.label in finallys:
        structs.add Structure(kind: stkReturn, stmt: i)
      else:
        terminator()
    of cnkRaiseStmt:
      exit(it[^1], isError=true)
      terminator()
    of cnkCaseStmt:
      for j in 1..<it.len:
        exit(it[j][^1], isError=false)
      terminator()
    of cnkLoopJoinStmt:
      # start of a 'while'
      struct(stkStructStart, it[0].label)
    of cnkIfStmt:
      struct(stkStructStart, it[1].label)
    of cnkEnd, cnkContinueStmt, cnkLoopStmt:
      struct(stkEnd, it[0].label)
    of cnkJoinStmt:
      assert it[0].label in marker
      struct(stkEnd, it[0].label)
    of cnkFinally:
      assert it[0].label in marker
      struct(stkFinally, it[0].label)
    of cnkExcept:
      assert it[0].label in marker
      struct(stkCatch, it[0].label)
      if it.len > 1:
        # not a catch-all handler; raising might continue
        exit(it[^1], isError=true)
    else:
      discard "not relevant"

  # the list of openings and closing produced by the first pass will in most
  # cases not be valid JavaScript code. We have to "solve" therepresentation
  # by reordering the openings until they're matched with their corresponding
  # end. ``stkCatch``, ``stkFinally``, ``stkStructStart``, and ``stkEnd`` must
  # keep their relative order and stay attached to the same statements, only
  # ``stkTry`` and ``stkBlock`` can be moved, but only backwards
  var i = structs.high
  while i > 0:
    if structs[i].kind in {stkTry, stkBlock}:
      # compute the difference in nesting between the try/block and its
      # corresponding end:
      var
        depth = 1
        j = i
      while true:
        inc j
        case structs[j].kind
        of stkTry, stkBlock, stkStructStart:
          inc depth
        of stkFinally, stkCatch:
          if structs[j].label == structs[i].label:
            dec depth
            break
        of stkEnd:
          dec depth
          if structs[j].label == structs[i].label:
            break
        of stkTerminator, stkReturn:
          discard "not relevant"

      # depth < 0 means that the try/block start is more nested than its end.
      # In other words, the try or block start is currently too nested. Move
      # it backwards (i.e., associate it with an earlier statement) until it's
      # at the same level as its end
      let moved = depth < 0
      var x = i
      while depth < 0:
        # change the associated statement...
        structs[x].stmt = structs[x - 1].stmt
        # ... then swap
        swap(structs[x], structs[x - 1])

        case structs[x].kind
        of stkEnd:
          dec depth
        of stkBlock, stkTry, stkStructStart:
          inc depth
        of stkTerminator, stkReturn, stkCatch, stkFinally:
          # catch and finally don't change the nesting (the try's body is at
          # the same level as catch/finally's body)
          discard

        dec x

      if moved:
        # a different item than before is in the slot now; it needs to be
        # processed too
        continue # skip the following decrement

    dec i

  # possible improvements:
  # * breaks and the associated block could be eliminated where JavaScript's
  #   structured control-flow would take same route
  # * for ``try{ try{ ... } catch(...) { ... } } finally {...}``, the trys
  #   could be merged
  # * a chain of exception could be merged into a single JavaScript catch

  # lastly, something has to be done about goto (which are translated into
  # ``break``s) and raise statements that jump "over" finally or except
  # sections. Since this cannot be expressed directly in JavaScript, a boolean
  # flag is associated with the affected 'finally'/'except' to indicate
  # whether it's disabled. For the sake of emitting less code, the "enabled"
  # flags for all finalizers within a procedure are bundled into a integer
  discard "not yet implemented"

  result = (structs, needsRecover)
