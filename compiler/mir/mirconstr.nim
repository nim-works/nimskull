## Implements routines and types that assist in producing MIR code.

import
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    idioms
  ],
  experimental/[
    dod_helpers
  ]

type
  Value* = object
    node: MirNode
    info: opt(SourceId)
      ## the source ID associated with the node, or none

  Fragment* = object
    ## Identifies a fragment (usually a sub-tree) within the staging buffer.
    s: Slice[NodeIndex]
    typ*: PType

  MirBuffer = object
    ## Accumulates in-progress MIR code and keeps track of additional state
    ## needed when building MIR code sequences.
    nodes*: MirNodeSeq
      # XXX: should not be exported
    cursor: int
      ## points to the first node that hasn't had its ``info`` set up yet

  MirBuilder* = object
    ## Holds the state needed for building MIR trees and allocating
    ## temporaries. A double-buffering scheme is used.
    front*: MirBuffer
      ## the front buffer, targeted by all add/subTree operations. By default,
      ## this is the "final" buffer
    back: MirBuffer
      ## by default, this the "staging" buffer
    swapped: bool
      ## whether the buffers were swapped. If 'true', `front` holds the
      ## staging buffer

    currentSourceId: SourceId
      ## the ID of the meta-data to associate with all added nodes (that
      ## don't have an explicitly assigned source ID)

    numTemps*: uint32
      ## tracks the number of existing temporaries. Used for allocating new
      ## IDs.

    # XXX: the internal fields are currently exported for the integration
    #      with changesets to work, but future refactorings should focus
    #      on making them hidden

func typ*(val: Value): PType =
  assert val.node.kind != mnkNone, "uninitialized"
  val.node.typ

func procNode*(s: PSym): MirNode {.inline.} =
  assert s.kind in routineKinds
  MirNode(kind: mnkProc, sym: s)

func endNode*(k: MirNodeKind): MirNode {.inline.} =
  assert k in SubTreeNodes
  MirNode(kind: mnkEnd, start: k)


func procLit*(sym: PSym): Value =
  Value(node: MirNode(kind: mnkProc, typ: sym.typ, sym: sym))

func typeLit*(t: PType): Value =
  Value(node: MirNode(kind: mnkType, typ: t))

func literal*(n: PNode): Value =
  Value(node: MirNode(kind: mnkLiteral, typ: n.typ, lit: n))

func temp*(typ: PType, id: TempId): Value =
  Value(node: MirNode(kind: mnkTemp, typ: typ, temp: id))

func alias*(typ: PType, id: TempId): Value =
  Value(node: MirNode(kind: mnkAlias, typ: typ, temp: id))

func symbol*(kind: range[mnkConst..mnkLocal], sym: PSym): Value =
  Value(node: MirNode(kind: kind, typ: sym.typ, sym: sym))

# --------- MirBuffer interface ----------

func len*(b: MirBuffer): int {.inline.} =
  b.nodes.len

func apply(buf: var MirBuffer, id: SourceId) =
  ## Associates all nodes added since the previous call to ``apply`` with the
  ## origin information identified by `id`.
  for i in buf.cursor..<buf.nodes.len:
    buf.nodes[i].info = id

  buf.cursor = buf.nodes.len

template add(b: var MirBuffer, n: MirNode) =
  b.nodes.add n

func moveTo(`from`, to: var MirBuffer, start: int) =
  ## Moves the nodes starting from `from` starting at `start` into `to`.
  assert to.cursor == to.len
  let
    num = `from`.len - start
    offset = to.len

  assert num > 0, "nothing to commit"
  # first resize the destination buffer, then *move* all elements over
  to.nodes.setLen(offset + num)
  for i in start..<`from`.nodes.len:
    to.nodes[offset + i - start] = move `from`.nodes[i]

  # adjust the destination buffer's cursor:
  to.cursor += max(0, `from`.cursor - start)
  # remove the moved-over nodes from the staging buffer:
  `from`.nodes.setLen(start)
  # adjust the source buffer's cursor:
  `from`.cursor = min(`from`.cursor, `from`.len)

# --------- low-level/internal MirBuilder interface -----------

func hasUnassigned*(bu: MirBuilder): bool =
  ## Returns whether nodes with unassigned ``info`` fields exist in the
  ## buffers.
  bu.front.cursor != bu.front.len or bu.back.cursor != bu.back.len

func staging*(bu: var MirBuilder): var MirNodeSeq {.inline.} =
  ## Provides access to the staging buffer.
  if bu.swapped:
    result = bu.front.nodes
  else:
    result = bu.back.nodes

func popSingle*(bu: var MirBuilder, f: Fragment): Value =
  ## Retrieves the topmost atom node identified by `v` from the staging
  ## buffer.
  func aux(s: var MirBuffer, start: int): Value {.inline.} =
    assert start == s.nodes.high, "doesn't point to the staging buffer's top"
    result = Value(node: s.nodes.pop())
    # adjust the cursor:
    s.cursor = min(s.cursor, s.nodes.len)

  if bu.swapped:
    result = aux(bu.front, f.s.a.int)
  else:
    result = aux(bu.back, f.s.a.int)

  result.info = someOpt bu.currentSourceId

func swap(bu: var MirBuilder, doSwap: bool) =
  if doSwap:
    swap(bu.front, bu.back)
    bu.swapped = not bu.swapped

template push*(bu: var MirBuilder, body: untyped): Fragment =
  ## Makes the staging buffer the active one and runs `body`. The pushed but
  ## not-yet-popped fragment is returned.
  let
    doSwap = not bu.swapped
    start =
      if doSwap: bu.back.len
      else:      bu.front.len

  swap(bu, doSwap)
  body
  swap(bu, doSwap)

  Fragment(s: NodeIndex(start) .. NodeIndex(bu.staging.high),
           typ: bu.staging[start].typ)

func pop*(bu: var MirBuilder, f: Fragment) =
  ## Moves the expression/statement identified by `v` from the top of the
  ## staging buffer to the final buffer.
  if bu.swapped:
    assert f.s.b.int == bu.front.len - 1
    bu.back.apply(bu.currentSourceId)
    bu.front.moveTo(bu.back, f.s.a.int)
  else:
    assert f.s.b.int == bu.back.len - 1
    bu.front.apply(bu.currentSourceId)
    bu.back.moveTo(bu.front, f.s.a.int)

template withFront*(bu: var MirBuilder, body: untyped) =
  ## Runs `body` with the final buffer as the front buffer.
  let doSwap = bu.swapped
  swap(bu, doSwap)
  body
  swap(bu, doSwap)

template buildStmt*(bu: var MirBuilder, body: untyped) =
  ## A shortcut for ``push`` + ``pop``.
  let v = bu.push(body)
  bu.pop(v)

template buildStmt*(bu: var MirBuilder, k: MirNodeKind, body: untyped) =
  ## Similar to `buildStmt <#buildStmt,TCtx,untyped>`_, but also starts a sub-
  ## tree of kind `k`.
  bu.buildStmt:
    bu.subTree MirNode(kind: k):
      body

template pos*(f: Fragment): NodePosition =
  ## Returns the fragment's starting position in the staging buffer.
  NodePosition(f.s.a)

# ------ higher-level MirBuilder interface ------

func initBuilder*(id: SourceId, buf: sink MirNodeSeq = @[]): MirBuilder =
  ## Initializes a ``MirBuilder`` with the given `id` and `buf` as the final
  ## buffer's initial content.
  MirBuilder(currentSourceId: id,
             front: MirBuffer(cursor: buf.len, nodes: buf))

func setSource*(bu: var MirBuilder, id: SourceId) =
  ## Sets `id` as the active source/info ID. All nodes added after a call to
  ## ``setSource`` will use `id` for their ``info`` field.
  let prev = bu.currentSourceId
  if id != prev:
    # flush the buffers:
    bu.front.apply(prev)
    bu.back.apply(prev)
    # now change the active ID
    bu.currentSourceId = id

func add*(bu: var MirBuilder, n: sink MirNode) {.inline.} =
  ## Emits `n` to the node buffers.
  bu.front.add n

func add*(bu: var MirBuilder, id: SourceId, n: sink MirNode) =
  ## Adds `n` to the front buffer and sets the node's source/info ID to
  ## `id`, ignoring the active ID set via ``setSource``.
  # the cursor is moved, so a flush has to be performed first
  bu.front.apply(bu.currentSourceId)
  n.info = id
  bu.front.add n
  inc bu.front.cursor

func emitFrom*(bu: var MirBuilder, tree: MirTree, n: NodePosition) =
  ## Emits the sub-tree at `n` within `tree` into `bu`'s front buffer.
  bu.front.apply(bu.currentSourceId)
  bu.front.nodes.add toOpenArray(tree, int n, int tree.sibling(n)-1)
  bu.front.cursor = bu.front.len

template subTree*(bu: var MirBuilder, n: MirNode, body: untyped) =
  let start = bu.front.len
  bu.add n
  body
  # note: don't use `n.kind` here as that would evaluate `n` twice
  bu.add endNode(bu.front.nodes[start].kind)

template subTree*(bu: var MirBuilder, k: MirNodeKind, body: untyped) =
  bu.subTree MirNode(kind: k):
    body

template stmtList*(bu: var MirBuilder, body: untyped) =
  bu.subTree MirNode(kind: mnkStmtList):
    body

template scope*(bu: var MirBuilder, body: untyped) =
  bu.subTree MirNode(kind: mnkScope):
    body

func allocTemp(bu: MirBuilder, t: PType; id: TempId, alias: bool): Value =
  ## Allocates a new temporary or alias and returns it.
  let kind = if alias: mnkAlias
             else:     mnkTemp
  {.cast(uncheckedAssign).}:
    result = Value(node: MirNode(kind: kind, typ: t, temp: id),
                   info: someOpt bu.currentSourceId)

template allocTemp*(bu: var MirBuilder, t: PType, alias = false): Value =
  # XXX: the only purpose of this is to work around a ``strictFuncs`` bug
  let id = TempId bu.numTemps
  inc bu.numTemps
  allocTemp(bu, t, id, alias)

func use*(bu: var MirBuilder, val: sink Value) {.inline.} =
  ## Emits a use of `val`.
  if val.info.isSome:
    bu.add val.info[], val.node
  else:
    bu.add val.node

template wrapTemp*(bu: var MirBuilder, t: PType,
                  body: untyped): Value =
  ## Emits a definition of a temporary with `body` as the initializer
  ## expression.
  let val = allocTemp(bu, t)
  bu.subTree MirNode(kind: mnkDef):
    bu.use val
    body
  val

template wrapMutAlias*(bu: var MirBuilder, t: PType, body: untyped): Value =
  ## Emits a ``mnkBind`` statement with `body` as the lvalue expression.
  ## Returns the name of the alias.
  let val = allocTemp(bu, t, true)
  bu.subTree mnkBindMut:
    bu.use val
    body
  val

template buildMagicCall*(bu: var MirBuilder, m: TMagic, t: PType,
                         body: untyped) =
  bu.subTree MirNode(kind: mnkMagic, magic: m, typ: t):
    body

template buildCall*(bu: var MirBuilder, prc: PSym, t: PType, d: untyped) =
  bu.subTree MirNode(kind: mnkCall, typ: t):
    bu.use procLit(prc)
    d

func emitByVal*(bu: var MirBuilder, y: Value) =
  bu.subTree mnkArg:
    bu.use y

func emitByName*(bu: var MirBuilder, val: Value, e: EffectKind) =
  bu.subTree mnkName:
    bu.subTree MirNode(kind: mnkTag, effect: e):
      bu.use val

func asgn*(buf: var MirBuilder, a, b: Value) =
  ## Emits an assignment of `b` to `a`.
  buf.subTree MirNode(kind: mnkAsgn):
    buf.use a
    buf.use b

func inline*(bu: var MirBuilder, tree: MirTree, fr: NodePosition): Value =
  ## Inlines the operand for non-mutating use. This is meant to be used for
  ## materialzing immutable arguments when inlining calls / expanding
  ## assignments.
  # TODO: finish the implementation
  case tree[fr].kind
  of Atoms:
    result = Value(node: tree[fr])
  else:
    result = allocTemp(bu, tree[fr].typ)
    bu.subTree mnkDef:
      bu.use result
      bu.emitFrom(tree, fr)

func bindImmutable*(bu: var MirBuilder, tree: MirTree,
                    lval: NodePosition): Value =
  case tree[lval].kind
  of mnkAlias, mnkTemp, mnkLocal, mnkGlobal, mnkParam, mnkConst:
    result = Value(node: tree[lval])
  of LvalueExprKinds - Atoms:
    result = allocTemp(bu, tree[lval].typ, alias=true)
    bu.subTree mnkBind:
      bu.use result
      bu.emitFrom(tree, lval)
  else:
    unreachable("cannot create alias of: " & $tree[lval].kind)

func bindMut*(bu: var MirBuilder, tree: MirTree, lval: NodePosition): Value =
  ## Creates an alias from the lvalue `lval` that supports mutations (e.g.,
  ## using as the destination of an assignment, passing to ``var``.
  ## parameter, etc.).
  case tree[lval].kind
  of mnkAlias, mnkTemp, mnkLocal, mnkGlobal, mnkParam:
    result = Value(node: tree[lval])
  of mnkConst:
    # catch obvious mistakes
    unreachable("cannot create mutable alias with constant")
  of LvalueExprKinds - AtomNodes:
    result = allocTemp(bu, tree[lval].typ, alias=true)
    bu.subTree MirNode(kind: mnkBindMut):
      bu.use result
      bu.emitFrom(tree, lval)
  else:
    unreachable("cannot create mutable alias with: " & $tree[lval].kind)

func materialize*(bu: var MirBuilder, loc: Value): Value =
  ## Captures the value of the location `loc` into a non-owning temporary
  ## and returns the name of the temporary.
  result = allocTemp(bu, loc.typ)
  bu.subTree MirNode(kind: mnkDefCursor):
    bu.use result
    bu.use loc

func finish*(bu: sink MirBuilder): MirTree =
  ## Consumes `bu` and returns the finished tree.
  if bu.swapped:
    swap(bu.front, bu.back)
    bu.swapped = false

  assert bu.back.len == 0, "staging buffer is not empty"
  # make sure all nodes have their info IDs assigned:
  apply(bu.front, bu.currentSourceId)
  result = move bu.front.nodes
