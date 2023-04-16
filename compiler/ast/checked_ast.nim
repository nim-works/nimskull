## The ``CheckedAst`` type is implemented here, along with its accessor
## routines and routines for creating and manipulating ``CheckedAst``
## instances.
##
## ``CheckedAst`` is a wrapper around ``PNode``, and is intended to make
## propagation less manual, by keeping track of whether there is an error
## *somewhere* in the managed AST.
##
## The idea is to use ``CheckedAst`` as the return type of all semantic
## analysis procedures, eventually removing the need for wrapper errors
## (``adWrappedError``).

import
  compiler/ast/[
    ast,
    ast_types,
    errorhandling,
    lineinfos
  ],
  compiler/front/[
    options
  ]

type
  CheckedAst* = object
    ## Represents either in-progress or fully semanticized AST. It is meant
    ## as a wrapper for semantically *checked* AST, hence the name.
    n: PNode       ## stores the AST. Never nil
    hasError: bool ## tracks whether `n` contains an ``nkError`` node
                   ## somewhere

proc initForMappingChildren*(x: var CheckedAst, n: PNode) =
  ## Initialize `x` as a node of kind ``n.kind``. Space for all children of
  ## `n` is allocated, but the child nodes are not copied. For the flags, only
  ## the persistent ones are copied.
  assert x.n == nil, "already initialized"
  assert n.kind in nkWithSons
  assert n.len > 0
  x.n = shallowCopy(n)

func initWith*(x: var CheckedAst, n: sink PNode) =
  ## Initializes `x`, using `n` as the tree. This is only meant to be used
  ## with the direct result of a ``newNodeX`` or ``newTreeX`` call.
  assert x.n == nil, "already initialized"
  assert n.kind != nkError
  x.n = n

proc newError*(c: ConfigRef, n: sink CheckedAst, diag: PAstDiag): CheckedAst =
  ## Creates an ``CheckedAst`` error node.
  result.n = c.newError(n.n, diag)
  result.hasError = true

# speed up debug builds by not generating stack-trace information for the
# accessors procedures
{.push stacktrace: off.}

func kind*(x: CheckedAst): TNodeKind {.inline.} =
  x.n.kind

func len*(x: CheckedAst): int {.inline.} =
  x.n.len

func info*(x: CheckedAst): TLineInfo {.inline.} =
  x.n.info

proc add*(x: var CheckedAst, n: sink PNode) {.inline.} =
  x.hasError = x.hasError or n.kind == nkError
  x.n.add n

proc add*(x: var CheckedAst, n: sink CheckedAst) {.inline.} =
  x.hasError = x.hasError or n.hasError
  x.n.add n.n

{.pop.}

template idx(len: int, x: Natural): int =
  int(x)

template idx(len: int, x: BackwardsIndex): int =
  len - int(x)

func `[]`*(x: CheckedAst, i: int|BackwardsIndex): PNode =
  x.n[idx(x.n.len, i)]

func `[]=`*(x: var CheckedAst, i: int|BackwardsIndex, n: sink PNode) =
  x.hasError = x.hasError or n.kind == nkError
  x.n[idx(x.n.len, i)] = n

func `[]=`*(x: var CheckedAst, i: int|BackwardsIndex, n: sink CheckedAst) =
  x.hasError = x.hasError or n.hasError
  x.n[idx(x.n.len, i)] = n.n

proc extract*(c: ConfigRef, n: sink CheckedAst): PNode =
  ## Converts `n` to a ``PNode``. If the AST contains an error somewhere, the
  ## resulting node is always an ``nkError`` node.
  ##
  ## This procedure is only meant to be used at the edge to ``CheckedAst``-
  ## unaware code.
  if n.hasError and n.n.kind != nkError:
    result = c.wrapError(n.n)
  else:
    result = n.n

func flattenIf*(n: var CheckedAst, kind: TNodeKind) =
  ## Replace `n` with its first child node, but only if `n` has a single
  ## child node and is of kind `kind`.
  assert kind in nkWithSons
  if n.kind == kind and n.len == 1:
    n.n = n.n[0]

template get*(n: CheckedAst): PNode =
  ## Direct access to the internal node. Should only be used by code meant for
  ## debugging.
  n.n
