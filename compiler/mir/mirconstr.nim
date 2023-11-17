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
  ]

type
  Value* = object
    # TODO: document
    node*: MirNode
    # TODO: `node` should be hidden

  EValue* = Value
    ## Legacy alias. Don't use it for new code.
    ## TODO: replace all remaining usages and then remove the type

  MirBuilder* = object
    ## Holds the state needed for building MIR trees and allocating
    ## temporaries.
    buffer*: MirNodeSeq

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

# --------- MirBuilder interface -----------

func add*(bu: var MirBuilder, n: sink MirNode) =
  ## Emits `n` to the node buffers.
  bu.buffer.add n

func emitFrom*(bu: var MirBuilder, tree: MirTree, n: NodePosition) =
  ## Emits the sub-tree at `n` within `tree` into `bu`'s node buffer.
  bu.buffer.add toOpenArray(tree, int n, int tree.sibling(n)-1)

template subTree*(bu: var MirBuilder, n: MirNode, body: untyped) =
  let start = bu.buffer.len
  bu.add n
  body
  # note: don't use `n.kind` here as that would evaluate `n` twice
  bu.add endNode(bu.buffer[start].kind)

template subTree*(bu: var MirBuilder, k: MirNodeKind, body: untyped) =
  bu.subTree MirNode(kind: k):
    body

template stmtList*(bu: var MirBuilder, body: untyped) =
  bu.subTree MirNode(kind: mnkStmtList):
    body

template scope*(bu: var MirBuilder, body: untyped) =
  bu.subTree MirNode(kind: mnkScope):
    body

proc allocTemp*(bu: var MirBuilder, t: PType; alias = false): Value =
  ## Allocates a new temporary or alias and returns it.
  let kind = if alias: mnkAlias
             else: mnkTemp
  {.cast(uncheckedAssign).}:
    result = Value(node: MirNode(kind: kind, typ: t,
                                  temp: TempId bu.numTemps))
  inc bu.numTemps

template use*(bu: var MirBuilder, val: Value) =
  ## Emits a use of `val`.
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
