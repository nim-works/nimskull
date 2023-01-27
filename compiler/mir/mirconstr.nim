## This module contains constructor procedures for the different kinds of
## ``MirNode``s plus convenience procedures for generating expressions.

import
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirtrees
  ]

type
  Value* = distinct bool
    ## Used to mark a procedure as generating code that yields a value
  Sink* = distinct bool
    ## Used to mark a procedure as generating code that acts as a sink
  SinkAndValue* = distinct bool

template `=>`*(v: Value, sink: Sink) =
  discard v
  discard sink

template `=>`*(v: Value, sink: SinkAndValue): Value =
  discard v
  discard sink
  Value(false)

template `=>|`*(v: Value, sink: SinkAndValue) =
  discard v
  discard sink

template `|=>`*(sink: Sink) =
  discard sink

template `|=>`*(sink: SinkAndValue): Value =
  discard sink
  Value(false)

template previous*(): Value =
  Value(false)

template follows*(): Sink =
  ## A pseudo-sink used to indicate that the actual output expression is
  ## either generated separately next or is already present and, in the context
  ## where ``follows`` is used, comes next
  Sink(false)

# --------- node constructors:

func procNode*(s: PSym): MirNode {.inline.} =
  assert s.kind in routineKinds
  MirNode(kind: mnkProc, sym: s)

func magic*(m: TMagic, typ: PType; n: PNode = nil): MirNode {.inline.} =
  MirNode(kind: mnkMagic, typ: typ, magic: m)

template endNode*(k: MirNodeKind): MirNode =
  MirNode(kind: mnkEnd, start: k)

# --------- tree generation utilities:

template subTree*(tree: var MirTree, n: MirNode, body: untyped) =
  let start = tree.len
  tree.add n
  body
  # note: don't use `n.kind` here as that would evaluate `n` twice
  tree.add endNode(tree[start].kind)

template stmtList*(tree: var MirTree, body: untyped) =
  tree.subTree MirNode(kind: mnkStmtList):
    body

template scope*(tree: var MirTree, body: untyped) =
  tree.subTree MirNode(kind: mnkScope):
    body

template argBlock*(tree: var MirTree, body: untyped) =
  tree.subTree MirNode(kind: mnkArgBlock):
    body