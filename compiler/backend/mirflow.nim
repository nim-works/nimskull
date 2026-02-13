## Implements an analysis pass for creating a structure control-flow
## representation from the unstructured, goto-based MIR.

import
  std/[
    intsets,
    tables
  ],
  compiler/mir/[
    mirtrees
  ]

type
  StmtKind* {.pure.} = enum
    None
    Break
    Return
    Raise
    Stmts
    Scope
    Block
    If
    Dispatch
    Target
    Loop
    Try

  Stmt* = object
    kind*: StmtKind
    sub*: int
      ## for block-like statements, index of the sub statement, or 0 (no
      ## sub statement)
    next*: int
      ## forms a singly-linked list. 0 terminates the list
    n*: NodePosition
      ## meaning depends on the kind

using
  stmts: seq[Stmt]
  tree: MirTree

const
  withSub = {Block, Loop, Scope, If, Dispatch, Target, Try}

iterator statements(tree): (int, NodePosition) =
  ## Iterates over all MIR statement in order of appearance.
  var n = NodePosition(0)
  var i = 0
  while n.ord < tree.len:
    yield (i, n)
    n = tree.sibling(n)
    inc i

proc pretty*(stmts: seq[Stmt]): string =
  ## Meant for debugging. Renders the statement as an indented tree.
  var res = ""
  proc pretty(i, ind: int) =
    proc list(i, ind: int) =
      var i = i
      while i != 0:
        pretty(i, ind)
        i = stmts[i].next

    for _ in 0..<ind:
      res.add "  "
    res.add $stmts[i]
    res.add "\n"
    case stmts[i].kind
    of withSub:
      list(stmts[i].sub, ind + 1)
    else:
      discard

  pretty(0, 0)
  result = res

proc toStructured*(tree): seq[Stmt] =
  ## Computes a control-flow focused representation of `tree`, where all
  ## control-flow is structured, preserving scope information.
  var labels: Table[LabelId, uint32]
    ## maps MIR labels to the index of the associated join statement
  var scopes: Table[uint32, uint32]
    ## maps the index of scope statements to the index of their corresponding
    ## end statement
  var s = 0

  # fill the tables:
  block:
    var stack: seq[uint32]
    for i, it in tree.statements():
      case tree[it].kind
      of mnkJoin, mnkLoop, mnkExcept, mnkFinally:
        labels[tree[it, 0].label] = uint32 i
      of mnkEndStruct:
        # can be the end of either an 'except' or 'if'. If it's the end of an
        # 'except' (which we want to ignore), the label is already part of `labels`
        if tree[it, 0].label notin labels:
          labels[tree[it, 0].label] = uint32 i
      of mnkScope:
        stack.add uint32(i)
      of mnkEndScope:
        scopes[stack.pop()] = uint32 i
      else:
        discard "ignore"
      inc s

  type BlockInfo = tuple[e: uint32, item, prev: int]

  # construct the structured representation
  var stmts: seq[Stmt]
  var stack: seq[BlockInfo]
    ## a top-to-bottom stack of blocks. A block is some construct that encloses
    ## statements and other blocks. Every block has an end (`e`). The stack
    ## must stay sorted such that ``stack[i].e <= stack[i-1].e``

  # translation is easier if there's always something on the stack. A scope is
  # used for this
  stmts.add Stmt(kind: Scope)
  stack.add (high(uint32), 0, -1)

  proc append(stmts: var seq[Stmt], with: var BlockInfo, i: int) =
    if with.prev != -1:
      stmts[with.prev].next = i
      with.prev = i
    else:
      assert stmts[with.item].kind in withSub
      stmts[with.item].sub = i
      with.prev = i

  proc append(s: sink Stmt) =
    append(stmts, stack[^1], stmts.len)
    stmts.add s

  proc push(s: sink Stmt, e: uint32) =
    stack.add (e, stmts.len, -1)
    # the stmts list element is only linked to the `next` list upon
    # completion of the block structure
    stmts.add s

  proc insertBlock(s: sink Stmt, e: uint32) =
    # insert the block `s` while preserving the order of `stack`
    for i in countdown(stack.high, 0):
      if stack[i].e == e and stmts[stack[i].item].kind == s.kind:
        # already registered
        return
      elif stack[i].e > e:
        # found the element to insert after
        stack.insert (e, stmts.len, -1), i+1
        stmts.add s
        return
    unreachable()

  proc insertBlock(n: NodePosition, e: uint32) =
    insertBlock(Stmt(kind: Block, n: n), e)
  proc insertTry(n: NodePosition, e: uint32) =
    insertBlock(Stmt(kind: Try, n: n), e)

  proc popBlock(expect: StmtKind) =
    let s = stack.pop()
    assert stmts[s.item].kind == expect
    append(stmts, stack[^1], s.item)

  for i, it in tree.statements():
    # close the blocks whose target is the current block
    case tree[it].kind
    of mnkIf:
      push(Stmt(kind: If, n: it), labels[tree[it, 1].label])
    of mnkScope:
      push(Stmt(kind: Scope, n: it), scopes[uint32 i])
    of mnkEndScope:
      popBlock(Scope)
    of mnkJoin:
      popBlock(Block)
    of mnkEndStruct:
      if labels[tree[it, 0].label] == uint32 i: # guard against end-of-except
        popBlock(If)
    of mnkExcept:
      popBlock(Try)
      let x = tree.last(it)
      if tree[it].len > 1 and tree[x].kind != mnkUnwind:
        insertTry(x, labels[tree[x].label])
      append(Stmt(kind: Stmts, n: it))
    of mnkFinally:
      popBlock(Try)
      append(Stmt(kind: Stmts, n: it))
    of mnkLoopJoin:
      # start of a loop
      push(Stmt(kind: Loop), labels[tree[it, 0].label])
    of mnkLoop:
      # end of a loop
      popBlock(Loop)
    of mnkCase:
      push(Stmt(kind: Dispatch, n: it), uint32 i)
      for c in tree.subNodes(it, 1):
        insertBlock(tree.last(c), labels[tree[tree.last(c)].label])
        # add a choice + break
        append(Stmt(kind: Target, n: c, sub: stmts.len + 1))
        stmts.add Stmt(kind: Break, n: tree.last(c))
      popBlock(Dispatch)
    of mnkGoto:
      insertBlock(tree.child(it, 0), labels[tree[it, 0].label])
      append(Stmt(kind: Break, n: tree.child(it, 0)))
    of mnkContinue:
      if tree[it, 0].kind != mnkUnwind:
        insertTry(tree.child(it, 0), labels[tree[it, 0].label])
      append(Stmt(kind: Stmts, n: it))
    of mnkDef, mnkDefCursor, mnkAsgn, mnkInit, mnkSwitch, mnkVoid:
      let e = tree.last(it)
      if tree[e].kind == mnkCheckedCall:
        if tree[tree.last(e)].kind != mnkUnwind:
          insertTry(tree.last(e), labels[tree[tree.last(e)].label])

      append(Stmt(kind: Stmts, n: it))
    of mnkRaise:
      if tree[tree.last(it)].kind != mnkUnwind:
        insertTry(tree.last(it), labels[tree[tree.last(it)].label])
      append(Stmt(kind: Raise, n: it))
    of mnkReturn:
      append(Stmt(kind: Return, n: it))
    of mnkDestroy, mnkEmit, mnkAsm, mnkBind, mnkBindMut:
      # statements without control-flow-relevant properties
      append(Stmt(kind: Stmts, n: it))
    of AllNodeKinds - StmtNodes:
      unreachable()

  assert stack.len == 1
  result = stmts

proc optimize*(stmts: var seq[Stmt]) =
  ## Removes the following unecessary constructs:
  ## * scopes in the tailing position of an 'if' or 'loop'
  ## * scopes in the tailing position of other scopes
  ## * empty scopes

  proc removeScopes(stmts: var seq[Stmt], i: int, rem: bool): int =
    proc walk(stmts: var seq[Stmt], i: int, rem: bool): int =
      var i = i
      var prev = 0
      while i != 0:
        let got = removeScopes(stmts, i, rem and stmts[i].next == 0)
        if prev == 0:
          prev = got
          result = got
        else:
          stmts[prev].next = got
          if got != 0:
            prev = got

        i = stmts[i].next

    result = i
    case stmts[i].kind
    of If, Loop:
      # 'if' and 'loop' open a scope
      stmts[i].sub = walk(stmts, stmts[i].sub, rem=true)
    of Scope:
      let got = walk(stmts, stmts[i].sub, rem=true)
      if rem or got == 0 or stmts[got].kind in {Break, Loop}:
        # remove the scope itself
        result = got
      else:
        stmts[i].sub = got
    of Try, Block:
      stmts[i].sub = walk(stmts, stmts[i].sub, rem)
    of Dispatch, Target:
      stmts[i].sub = walk(stmts, stmts[i].sub, rem=false)
    else:
      discard "not a block-like statement; nothing to do"

  discard removeScopes(stmts, 0, false)
  # TODO: inline block continuations into breaks, using the following
  #       semantics- and scoping-preserving heuristic:
  #       1. a single 'break' must target the 'block'
  #       2. the block's body must end in a terminator (break, return, etc.)
  #       3. the block must be in the same scope as the break
  #       4. the statement list following the block must end in a terminator
