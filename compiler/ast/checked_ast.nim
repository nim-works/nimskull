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
##
## A second experiment, ``ElaborateAst``, is also implemented here. Rather
## than being a wrapper around ``PNode``, it acts as sort of "builder"
## during piece-by-piece production of an AST node.

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
    n: PNode       ## stores the AST. Never nil nor an error
    hasError: bool ## tracks whether `n` contains an ``nkError`` node
                   ## somewhere

  ElaborateAst* = object
    ## *Note*: the name is not final and going to change.
    ## Represents an in-progress or finished production of either an AST of the
    ## same kind and number of children as the source, or an error.
    orig: PNode ## the source node

    diag*: PAstDiag       ## the error diagnostic, or nil. If the production
                          ## finishes and the diagnostic is non-nil, an error
                          ## is produced instead
    hasError: bool        ## whether one of the produced children contains an
                          ## error somewhere

    children: seq[PNode]  ## the produced child nodes; has the number of
                          ## elements as `n` has children
    typ*: PType           ## the type of the produced expression

# --------------- ElaborateAst API ------------------

proc initWith*(x: var ElaborateAst, n: PNode) =
  ## Setup the production of a new AST of the kind and with the number of
  ## children that `n` has.
  assert x.orig == nil, "already initialized"
  assert n.kind in nkWithSons
  assert n.len > 0
  x.orig = n
  x.children.setLen(n.len)

template `[]`*(x: var ElaborateAst, i: int|BackwardsIndex): PNode =
  x.children[i]

func `[]=`*(x: var ElaborateAst, i: int|BackwardsIndex, n: PNode) {.inline.} =
  ## Sets the child node at `i`
  x.hasError = x.hasError or n.kind == nkError
  x.children[i] = n

func `[]=`*(x: var ElaborateAst, i: int|BackwardsIndex,
            n: sink CheckedAst) {.inline.} =
  ## Sets the child node at `i`
  x.hasError = x.hasError or n.hasError
  x.children[i] = n.n

proc extract*(c: ConfigRef, n: sink ElaborateAst): PNode =
  ## Finishes the production and returns the result as a ``PNode``.
  result = newNodeI(n.orig.kind, n.orig.info)
  result.flags = n.orig.flags * PersistentNodeFlags
  result.typ   = n.typ # may be nil
  result.sons  = move n.children

  # XXX: the original node could be re-used when the children are the same as
  #      the original ones
  if n.diag != nil:
    result = c.newError(result, n.diag)
  elif n.hasError:
    # at least one of the child nodes contains an error somewhere; use the
    # generic wrapper error
    result = c.wrapError(result)
  else:
    discard "leave as is"

# -------------- CheckedAst API ------------------

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

func assign*(x: var CheckedAst, n: sink PNode) {.inline.} =
  ## Assigns the tree `n` to `x`. The behaviour is undefined if `n` is either
  ## an error error or contains one.
  assert n != nil
  assert n.kind != nkError
  x = CheckedAst(n: n, hasError: false)

# speed up debug builds by not generating stack-trace information for the
# accessors procedures
{.push stacktrace: off.}

func kind*(x: CheckedAst): TNodeKind {.inline.} =
  x.n.kind

func len*(x: CheckedAst): int {.inline.} =
  x.n.len

func info*(x: CheckedAst): TLineInfo {.inline.} =
  x.n.info

func typ*(x: CheckedAst): PType {.inline.} =
  x.n.typ

func `typ=`*(x: var CheckedAst, typ: PType) {.inline.} =
  x.n.typ = typ

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
  if n.hasError:
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
