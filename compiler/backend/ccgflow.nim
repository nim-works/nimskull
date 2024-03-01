## Implements the translation of CGIR to a code listing for an abstract
## machine that focuses on control-flow and exception handling.
##
## This code listing is intended for consumption by the C code generator.

import
  std/[
    options,
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
  COpcode* = enum
    opJump       ## unconditional jump
    opErrJump    ## jump if in error mode
    opDispJump   ## jump part of a dispatcher
    opLabel      ## jump target

    opSetTarget  ## set the value of a dispatcher's discriminator
    opDispatcher ## start of a dispatcher

    opBackup     ## backup the error state in a local variable and clear it
    opRestore    ## restore the error from a local variable

    opStmts      ## slice of statements
    opStmt       ## control-flow relevant single statement. A label
                 ## specifier is passed along
    # future direction: ``CStmt`` should be removed and all unstructured
    # control-flow bits modeled with the other instructions. Checked calls
    # used as an assignment source currently block this, as they might
    # require an assignment-to-temporary

    opAbort
    opPopHandler

  JumpOp = range[opJump..opDispJump]

  CLabelId* = distinct uint32
  CLabelSpecifier* = uint32
    ## used for identifying the extra labels attached to finally sections

  CLabel* = tuple
    ## Name of a label.
    id: CLabelId
    specifier: Option[CLabelSpecifier]

  CInstr* = object
    case op*: COpcode
    of opJump, opErrJump, opLabel, opDispJump:
      label*: CLabel
    of opSetTarget, opDispatcher:
      discr*: uint32 ## ID of the discriminator variable
      value*: int    ## either the value, or number of dispatcher branches
    of opStmts:
      stmts*: Slice[int]
    of opStmt:
      stmt*: int
      specifier*: CLabelSpecifier
    of opBackup, opRestore, opAbort:
      local*: uint32 ## ID of the backup variable
    of opPopHandler:
      discard

  FinallyInfo* = object
    routes: seq[PathIndex]
    numExits: int
      ## number of exits the finally has. Pre-computed for efficiency
    numErr: int
      ## number of exceptional jump paths going through this finalizer
    numNormal: int
      ## number of non-exception jump paths going through this finalizer

    discriminator: uint32
      ## ID of the discriminator variable to use for the dispatcher
    errBackupId: uint32
      ## only valid if the finally is entered by exceptional control-flow

  PathKind = enum
    pkError
    pkNormal
  PathIndex = uint32
    ## Index of a ``PathItem`` within the item storage.
  PathItemTarget = object
    label: CLabelId
    isCleanup: bool
  PathItem = object
    ## Represents a step in a jump path. A jump path is a chain of finally
    ## sections plus final target an intercepted goto visits.
    ##
    ## An item is part of two intrusive linked-lists: one doubly-linked-list
    ## representing a single chain, and one singly-linked-list for the
    ## adjacent chains. The "none" value for a pointer is represented by it
    ## pointing to the node itself.
    prev, next: PathIndex
    sibling: PathIndex

    target: PathItemTarget
      ## the identifier of the jump target
    kinds: set[PathKind]
      ## the kinds of control-flow (exception or normal) reaching the path
      ## item

  Paths = seq[PathItem]
    ## The data storage for multiple jump paths, with the items layed out
    ## "tail first", meaning that the final target of a jump chain comes
    ## *before* the others. The idea is to uniquely identify jump paths
    ## within a body while merging common trailing paths.
    ##
    ## Consider the two jump paths E->D->C->B->A and G->F->C->B->A. If
    ## both are added to the storage, the content would look like this:
    ##
    ##    (0: A) -> (1: B) -> (2: C) -> (3: D) -> (4: E)
    ##                                \ (5: F) -> (6: G)
    ##
    ## The numbers represent the items' index in the sequence. The `sibling`
    ## item of 3 is 5 (all other items have no siblings); the `next` pointer
    ## of 5 points to 2. As can be seen, common trailing paths are merged into
    ## one.

  Context = object
    ## Local state used during the translation bundled into an object for
    ## convenience.
    paths: Paths
    stmtToPath: Table[int, int]
    finallys: Table[CLabelId, FinallyInfo]
    cleanups: Table[CLabelId, FinallyInfo]
      ## cleanup here refers to the exception-related cleanup when
      ## exiting a finally or except section

const
  ExitLabel* = CLabelId(0)
    ## The label of the procedure exit.
  ResumeLabel* = ExitLabel
    ## The C label that a ``cnkResume`` targets.

func `==`*(a, b: CLabelId): bool {.borrow.}

func toCLabel*(n: CgNode): CLabelId =
  ## Returns the ID of the C label the label-like node `n` represents.
  case n.kind
  of cnkResume:
    ResumeLabel
  of cnkLabel:
    CLabelId(ord(n.label) + 2)
  of cnkLeave:
    toCLabel(n[0])
  else:
    unreachable(n.kind)

func toCLabel*(n: CgNode, specifier: Option[CLabelSpecifier]
              ): CLabel {.inline.} =
  (toCLabel(n), specifier)

func toBlockId*(id: CLabelId): BlockId =
  ## If `id` was converted to from a valid CGIR label, converts it back to
  ## the CGIR label.
  BlockId(ord(id) - 2)

func rawAdd(p: var Paths, x: openArray[PathItemTarget]): PathIndex =
  ## Appends the chain `x` to `p` without any deduplication or
  ## linking with the existing items. Returns the index of the
  ## tail item.
  result = p.len.PathIndex
  for i in countdown(x.high, 0):
    let pos = p.len.PathIndex
    p.add PathItem(prev: (if i > 0: pos + 1 else: pos),
                   next: (if i < x.high: pos - 1 else: pos),
                   sibling: pos,
                   target: x[i])

func add(p: var Paths, path: openArray[PathItemTarget]): PathIndex =
  ## Adds `path` to the `p`. Only the sub-path of `path` not yet present in
  ## `p` is added. The index of the *head* item of the added (or existing)
  ## path is returned.
  if p.len == 0:
    discard rawAdd(p, path)
    p[0].next = 0'u32
    return p.high.PathIndex

  var pos = 0'u32 ## the current search position
  for i in countdown(path.len-1, 0):
    # search the sibling list for a matching item:
    while p[pos].target != path[i] and pos != p[pos].sibling:
      pos = p[pos].sibling

    if p[pos].target != path[i]:
      # no item was found, meaning that this is the end of the common paths.
      # Add the remaining items to the storage.
      let next = rawAdd(p, path.toOpenArray(0, i))
      p[pos].sibling = next
      # only set the next pointer if there was a common sub-path (otherwise
      # there's no next item):
      if i != path.high:
        p[next].next = p[pos].next
      return p.high.PathIndex

    # it's a match! continue down the chain
    if i > 0:
      if p[pos].prev == pos:
        # there's no next item, append the remaining new targets to the
        # pre-existing path
        let next = rawAdd(p, path.toOpenArray(0, i-1))
        p[pos].prev = next
        p[next].next = pos
        return p.high.PathIndex
      else:
        pos = p[pos].prev

  # the chain `path` already exists in `p`
  result = pos

func incl(p: var Paths, at: PathIndex, kind: PathKind) =
  ## Marks all items following and including `at` with `kind`.
  var i = at
  while p[i].next != i:
    p[i].kinds.incl kind
    i = p[i].next
  p[i].kinds.incl kind

func needsDispatcher(f: FinallyInfo): bool =
  # a dispatcher is required if re are more than one exits. An exception is
  # the case where one exit is only taken when in error mode and the other is
  # not. If a dispatcher is required, the finally has sub-labels.
  f.numExits > 1 and
    not(f.routes.len == 2 and f.numErr == 1 and f.numNormal == 1)

func needsSpecifier(c: Context, target: PathItemTarget): bool =
  # cleanup sections don't have a unique label themselves, so using a
  # specifier is required
  target.isCleanup or
    ((target.label in c.finallys) and
     needsDispatcher(c.finallys[target.label]))

proc append(targets: var seq[PathItemTarget],
            redirects: Table[BlockId, CgNode],
            exits: PackedSet[BlockId], n: CgNode) =
  ## Appends all jump targets `n` represents to `targets`, following
  ## `redirects` and turning all labels part of `exits` into the
  ## "before return" label.
  template addTarget(t: CLabelId; cleanup = false) =
    targets.add PathItemTarget(label: t, isCleanup: cleanup)

  case n.kind
  of cnkLabel:
    if n.label in redirects:
      append(targets, redirects, exits, redirects[n.label])
    elif n.label in exits:
      addTarget ExitLabel
    else:
      addTarget toCLabel(n)
  of cnkTargetList:
    # only the final target could possibly be redirected
    let hasRedir = n[^1].kind == cnkLabel and n[^1].label in redirects
    for i in 0..<n.len - ord(hasRedir):
      case n[i].kind
      of cnkLeave:
        addTarget toCLabel(n[i][0]), cleanup=true
      of cnkResume:
        addTarget toCLabel(n[i])
      of cnkLabel:
        if n[i].label in exits:
          addTarget ExitLabel
        else:
          addTarget toCLabel(n[i])
      else:
        unreachable()

    if hasRedir:
      append(targets, redirects, exits, redirects[n[^1].label])
  else:
    unreachable()

proc gatherRedirectsAndFinallys(c: var Context, stmts: CgNode
                               ): Table[BlockId, CgNode] =
  #ä First pass: gather the redirects for redundant labels. Consider:
  ##   L1:
  ##   goto L2
  ##
  ## Here, all jumps to L1 can jump to L2 directly. This pattern is
  ## especially common with compiler-generated cleanup sections.
  ##
  ## The table for the finally sections is also populated.
  for i in 0..<stmts.len - 1:
    case stmts[i].kind
    of cnkJoinStmt:
      var j = i + 1
      # skip join and end statements:
      while j < stmts.len and stmts[j].kind in {cnkJoinStmt, cnkEnd}:
        inc j

      # if the label is followed directly by a goto statement, all jumps to
      # the label can jump to the goto's target instead
      if j < stmts.len and stmts[j].kind == cnkGotoStmt:
        result[stmts[i][0].label] = stmts[j][0]
    of cnkFinally:
      # make sure a table entry exists for the finally section:
      c.finallys[toCLabel(stmts[i][0])] = FinallyInfo()
    else:
      discard

proc toInstrList*(stmts: CgNode, isFull: bool): seq[CInstr] =
  ## Turns the statements list `stmts` into an instruction list for the
  ## abstract machine. `isFull` signals whether the end of the statement list
  ## can be considered the end of the procedure, which allows for the merging
  ## of some control-flow paths.
  var c = Context()
  let redirects = gatherRedirectsAndFinallys(c, stmts) # first pass

  # mark the labels of the trailing joins as being the same as the exit label:
  var exits: PackedSet[BlockId]
  for i in countdown(stmts.len - 1, 0):
    if stmts[i].kind == cnkJoinStmt:
      exits.incl stmts[i][0].label
    else:
      break

  # second pass: collect all jump paths, using the table of redirections to
  # eliminate unnecessary breaks in the paths
  var targets: seq[PathItemTarget]
  for i, it in stmts.pairs:
    template exit(x: CgNode; isErr = false) =
      targets.setLen(0)
      targets.append(redirects, exits, x)

      # a single jump is only of relevance if it targets a finally directly
      if targets.len > 1 or targets[0].label in c.finallys:
        let id = c.paths.add(targets)
        if isErr: incl(c.paths, id, pkError)
        else:     incl(c.paths, id, pkNormal)

        # remember the path associated with the statement for later:
        c.stmtToPath[i] = id.int

    case it.kind
    of cnkDef, cnkAsgn, cnkFastAsgn:
      if it[1].kind == cnkCheckedCall:
        exit(it[1][^1], true)
    of cnkRaiseStmt, cnkCheckedCall:
      exit(it[^1], true)
    of cnkGotoStmt:
      exit(it[0])
    of cnkCaseStmt:
      for j in 1..<it.len:
        exit(it[j][^1])
    of cnkExcept:
      if it.len > 1:
        exit(it[^1], true)
    else:
      discard

  # register every path item with the finally section it targets, and compute
  # some statistics that are used during the later code generation:
  for i, it in c.paths.pairs:
    func setup(f: var FinallyInfo, it: PathItem) =
      f.routes.add i.PathIndex
      f.numExits += ord(it.next.int != i)
      f.numErr += ord(pkError in it.kinds)
      f.numNormal += ord(pkNormal in it.kinds)

    if it.target.isCleanup:
      setup(c.cleanups.mgetOrPut(it.target.label, FinallyInfo()), it)
    elif it.target.label in c.finallys:
      setup(c.finallys[it.target.label], it)

  # construction of the instruction list follows

  proc label(code: var seq[CInstr], id: CLabelId;
             spec = none(CLabelSpecifier)) {.nimcall.} =
    # a label must always be preceded by some code, so no length guard is
    # required
    if code[^1].op in {opJump, opErrJump} and code[^1].label.id == id and
       code[^1].label.specifier == spec:
      # optimization: remove the preceding jump if it targets the label
      code.setLen(code.len - 1)
    code.add CInstr(op: opLabel, label: (id, spec))

  proc jump(code: var seq[CInstr], target: CLabelId) {.nimcall.} =
    code.add CInstr(op: opJump, label: (target, none CLabelSpecifier))

  proc jump(code: var seq[CInstr], op: JumpOp, c: Context,
            path: PathIndex) {.nimcall.} =
    let target = c.paths[path].target
    if needsSpecifier(c, target):
      code.add CInstr(op: op, label: (target.label, some path))
    else:
      code.add CInstr(op: op, label: (target.label, none CLabelSpecifier))

  proc stmt(code: var seq[CInstr], c: Context, pos: int) {.nimcall.} =
    if (let path = c.stmtToPath.getOrDefault(pos, -1); path != -1 and
        needsSpecifier(c, c.paths[path].target)):
      # a label specifier, and thus a separate instruction, is needed
      code.add CInstr(op: opStmt, stmt: pos, specifier: CLabelSpecifier path)
    elif code.len > 0 and code[^1].op == opStmts and
         code[^1].stmts.b == pos + 1:
      # append to the sequence
      inc code[^1].stmts.b
    else:
      # start a new sequence
      code.add CInstr(op: opStmts, stmts: pos..pos)

  var
    code: seq[CInstr]
    nextDispId = 0'u32
    nextRecoverID = 0'u32

  for i, it in stmts.pairs:
    case it.kind
    of cnkFinally:
      stmt code, c, i
      let
        clabel = toCLabel(it[0])
        f = addr c.finallys[clabel]

      # allocate and set the ID for the discriminator variable:
      f.discriminator = nextDispId
      inc nextDispId

      # emit the entry-point(s); one for each route
      if needsDispatcher(f[]):
        # an entry point looks like this:
        #   L1_1_:
        #   Target = ...
        #   goto L1_
        for i, entry in f.routes.pairs:
          label code, clabel, some(entry)
          code.add CInstr(op: opSetTarget, discr: f.discriminator, value: i)
          # jump to the main code:
          jump code, clabel

      # the body follows:
      label code, clabel
      if f.numErr > 0:
        # backing up the error state is only needed when the finally is
        # entered by exceptional control-flow
        f.errBackupId = nextRecoverID
        code.add CInstr(op: opBackup, local: nextRecoverID)
        inc nextRecoverID

    of cnkContinueStmt:
      let
        clabel = toCLabel(it[0])
        f {.cursor.} = c.finallys[clabel]

      # no need to restore the error state if control-flow never reaches the
      # end of the finally anyway
      if f.numErr > 0 and f.numExits > 0:
        code.add CInstr(op: opRestore, local: f.errBackupId)

      if f.numExits == 0:
        discard "the end is never reached; nothing to do"
      elif not needsDispatcher(f) and f.routes.len == 2:
        # optimization: if two paths go through a finally, with one of them
        # an exceptional jump path and the other one not, instead of using a
        # full dispatcher we emit:
        #   if err: goto error_exit
        #   goto normal_exit
        let exit = if c.paths[f.routes[0]].kinds == {pkError}: 0 else: 1
        jump code, opErrJump, c, c.paths[f.routes[exit]].next
        jump code, opJump,    c, c.paths[f.routes[1 - exit]].next
      else:
        assert f.routes.len == f.numExits
        # a dispatcher is only required if there is more than one exit
        let op = if f.numExits > 1: opDispJump
                 else:              opJump

        if op == opDispJump:
          code.add CInstr(op: opDispatcher, discr: f.discriminator,
                          value: f.numExits)

        for it in f.routes.items:
          jump code, op, c, c.paths[it].next

      # emit the exception-related cleanup after the dispatcher:
      if clabel in c.cleanups:
        let cleanup {.cursor.} = c.cleanups[clabel]
        # a dispatcher is not worth the overhead, emit an abort instruction
        # for each route
        for entry in cleanup.routes.items:
          label code, clabel, some(entry)
          # TODO: omit the cleanup logic as a whole, if the finally section is
          #       never entered via an exception
          if f.numErr > 0:
            code.add CInstr(op: opAbort, local: f.errBackupId)
          jump code, opJump, c, PathIndex c.paths[entry].next

      stmt code, c, i

    of cnkJoinStmt:
      # XXX: labels that were redirected cannot be eliminated yet, as case
      #      statements (which are handled outside of ccgflow) might still
      #      target them
      label code, toCLabel(it[0])
    of cnkExcept:
      # an except section is a label followed by the filter logic
      label code, toCLabel(it[0])
      stmt code, c, i
    of cnkEnd:
      let clabel = toCLabel(it[0])
      # emit the cleanup for except sections:
      if clabel in c.cleanups:
        let cleanup {.cursor.} = c.cleanups[clabel]
        # a dispatcher is not worth the overhead, emit a pop instruction
        # for each route
        for entry in cleanup.routes.items:
          label code, clabel, some(entry)
          code.add CInstr(op: opPopHandler)
          jump code, opJump, c, PathIndex c.paths[entry].next

      stmt code, c, i

    of cnkGotoStmt:
      let target = it[0]
      if (let path = c.stmtToPath.getOrDefault(i, -1); path != -1):
        jump code, opJump, c, PathIndex path
      elif target.kind == cnkLabel:
        jump code, toCLabel(target)
      else:
        jump code, toCLabel(target[^1])
    of cnkRaiseStmt:
      stmt code, c, i # the statement handles the exception setup part
      # the goto part is the same as for a normal goto
      let target = it[^1]
      if target.kind == cnkLabel:
        jump code, toCLabel(target)
      elif (let path = c.stmtToPath.getOrDefault(i, -1); path != -1):
        jump code, opJump, c, PathIndex path
      else:
        jump code, toCLabel(target[^1])

    else:
      stmt code, c, i

  result = code
