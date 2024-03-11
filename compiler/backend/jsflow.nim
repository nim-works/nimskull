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
    packedsets,
    tables
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

  StructDesc* = tuple
    ## Describes how a CGIR statement-list translates to JavaScript code. The
    ## focus is on the control-flow constructs, hence the name.
    structs: seq[Structure]
    finallys: PackedSet[BlockId]
      ## labels that denote finally sections
    inline: Table[BlockId, int]
      ## maps all blocks that can be inlined into swith-case statements to
      ## the blocks' 'end' item

const
  Terminators = {stkReturn, stkTerminator}

func finalTarget*(n: CgNode): CgNode =
  ## Given a label or target list, retrieves the target.
  case n.kind
  of cnkLabel:      n
  of cnkTargetList: n[^1]
  else:
    unreachable()

proc spawnOpens(items: var seq[Structure], pos: int, n: CgNode, isError: bool,
                finallys: PackedSet[BlockId], marker: var PackedSet[BlockId]) =
  ## Using the jump action description `n`, spawns the 'try' or labeled block
  ## openings needed for the jump targets. The set of targets for which
  ## openings were already spawned are tracked by `marker`.
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

func endsInTerminator(structs: seq[Structure], start: int): bool =
  ## Computes and returns whether the region starting at `start` ends in a
  ## terminator.
  var
    i = start
    depth = 0
  while depth >= 0 and i < structs.len:
    case structs[i].kind
    of stkBlock, stkTry, stkStructStart:
      inc depth
    of stkEnd:
      dec depth
    of stkCatch, stkFinally:
      if depth == 0:
        # end of scope
        break
      # depth stays the same
    of Terminators:
      if depth == 0:
        # a terminator is reached and it's at the same level as was started
        # at
        return true

    inc i

  result = false # doesn't end in a terminator

proc toStructureList*(stmts: openArray[CgNode]): StructDesc =
  ## Creates and returns the JavaScript control-flow-construct-focused
  ## representation for `stmts`.
  var
    structs = newSeq[Structure]()
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
      # leaves the finally, a JavaScript 'return' can be used
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
  # cases not be valid JavaScript code. We have to "solve" the representation
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
        of Terminators:
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
        of Terminators, stkCatch, stkFinally:
          # catch and finally don't change the nesting (the try's body is at
          # the same level as catch/finally's body)
          discard

        dec x

      if moved:
        # a different item than before is in the slot now; it needs to be
        # processed too
        continue # skip the following decrement

    dec i

  # note: changing what statements a 'try' encloses can alter semantics! That's
  # none of our concern here, however: the code generator is reponsible for
  # addressing/fixing it.

  # if a case dispatcher is the only break targeting a block, and the block is
  # not exited through structured control-flow, the code following the block
  # can be inlined directly at the break within the switch-case statement:
  #   L2: {
  #     L1: {
  #       switch (x) {
  #       case 0:
  #         break L1;
  #       default:
  #         break L2;
  #       }
  #     }
  #     // A
  #     break L2
  #   }
  #
  # Here, the A section plus the ``break L2`` can be inlined directly at
  # the ``break L1``.
  # We perform two passes over the structure list:
  #   1. the first one counts for each block how many breaks target it
  #   2. the second pass removes all ineligible blocks from the table and
  #      replaces the counter with an item index of the blocks' 'end'
  # For efficiency, and thanks to the forward-only control-flow, both
  # steps are performed with a single pass.
  var inline: Table[BlockId, int]
  for i, it in structs.pairs:
    case it.kind
    of stkTerminator:
      let n = stmts[it.stmt]
      case n.kind
      of cnkCaseStmt:
        for j in 1..<n.len:
          inline.mgetOrPut(n[j][^1].label, 0) += 1
      of cnkGotoStmt:
        # we don't inline the target at bare gotos. Mark the block the goto
        # targets as ineligible by incrementing the counter by two
        inline.mgetOrPut(finalTarget(n[0]).label, 0) += 2
      else:
        discard "only gotos are interesting"
    of stkEnd:
      case inline.getOrDefault(it.label, 0)
      of 0:
        discard "must be the end of a finally or catch; ignore"
      of 1:
        # possible candidate. Is the region preceded by a terminator (meaning
        # that structured control-flow doesn't enter it) and ends in one?
        if structs[i - 1].kind in Terminators and
           endsInTerminator(structs, i + 1):
          # can be inlined. Replace the counter value with the index
          inline[it.label] = i
        else:
          # not eligible, remove it from the set of candidates
          inline.del(it.label)
      else:
        # blocks broken out of more than once cannot be inlined. Remove
        # them from the table
        inline.del(it.label)
    of stkTry, stkBlock, stkCatch, stkFinally, stkStructStart, stkReturn:
      discard "not relevant"

  # the `inline` table now contains only the blocks inline-able into swith-case
  # statements

  # possible improvements:
  # * breaks and the associated block could be eliminated where JavaScript's
  #   structured control-flow would take same route
  # * a chain of exception could be merged into a single JavaScript catch

  result = (structs, finallys, inline)
