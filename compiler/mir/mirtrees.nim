## This module contains the main definitions that make up the mid-end IR (MIR).
##
## See `MIR <mir.html>`_ for the grammar plus a high-level overview of the MIR.

import
  compiler/ast/[
    ast_types
  ],
  compiler/utils/[
    idioms
  ]

type
  ## The MIR itself doesn't not care for how each of the following ID types is
  ## interpreted. For example, a ``ParamId`` could be the index of the
  ## parameter or it could be an index into a list of symbols.

  LocalId {.used.} = distinct uint32
    ## Identifies a local inside a code fragment
  GlobalId {.used.} = distinct uint32
    ## Identifies a global inside a code fragment
  ConstId {.used.} = distinct uint32
    ## Identifies a named constant inside a code fragment
  ParamId {.used.} = distinct uint32
    ## Identifies a parameter of the code fragment
  FieldId {.used.} = distinct uint32
    ## Identifies the field of a record type
  ProcedureId {.used.} = distinct uint32
    ## Identifies a procedure
  LiteralId {.used.} = distinct uint32
    ## Identifies a literal

  TypeInstance {.used.} = distinct uint32
    ## Refers to an existing type instance
  TypeId {.used.} = distinct uint32
    ## The ID of a type instance or nil

  SourceId* = distinct range[0'u32 .. high(uint32)-1]
    ## The ID of a source-mapping that's stored separately from the MIR nodes.

# make ``SourceId`` available for use with ``OptIndex``:
template indexLike*(_: typedesc[SourceId]) = discard

type
  ## Different to the ID types above, how and what the following ID types
  ## represent is dictated by the MIR
  TempId* = distinct uint32
    ## ID of a temporary location. A temporary location is created and
    ## inserted by the compiler. The only difference to other named locations
    ## is that temporaries are allowed to be elided (by an optimization pass,
    ## for example) if it's deemed to have no effect on the codes' semantics
  LabelId* = distinct uint32
    ## ID of a label, used to identify a block (``mnkBlock``).

  MirNodeKind* = enum
    ## Users of ``MirNodeKind`` should not depend on the absolute or relative
    ## order between the enum values
    # when adding new enum values, make sure to adjust the sets below
    mnkNone

    # entity names:
    mnkProc   ## procedure
    mnkConst  ## named constant
    mnkGlobal ## global location
    mnkParam  ## parameter
    mnkLocal  ## local location
    mnkTemp   ## temporary introduced during the MIR phase. Has the same
              ## semantics as ``mnkLocal``
    mnkAlias  ## local run-time handle. This is essentially a ``var T`` or
              ## ``lent T`` local

    mnkField  ## declarative node only allowed in special contexts

    mnkLiteral ## literal data. Currently represented via a ``PNode``
    # future direction: split into IntLit, FloatLit, and StrLit and store the
    # values in a separate table (so that MirNode gets smaller)
    mnkType    ## a type literal

    # future direction:
    # store the type of the destination within each def, assignment, etc. and
    # then remove the type field from ``MirNode``

    mnkMagic  ## only allowed in a callee position. Refers to a magic
              ## procedure

    mnkDef       ## marks the start of existence of a local, global, procedure,
                 ## or temporary. Supports an optional intial value (except for
                 ## procedure definitions)
    mnkDefCursor ## marks the start of existence of a non-owning location
    # future direction: remove this distinction and perform all related decision
    # making (e.g., injecting destructors) requiring knowledge of locations'
    # ownership in ``mirgen``. There's only going to be the ``Def`` kind
    mnkDefUnpack ## intermediate hack required by destructor injection. Don't
                 ## use
    mnkBind      ## introduces an alias that may only be used for read access.
                 ## The source expression must not be empty
    mnkBindMut   ## introduces an alias that may be used for write access.
                 ## The source expression must not be empty

    mnkFastAsgn ## assignment that cannot be rewritten into copy, move, or
                ## hook call
    # future direction: same as with DefCursor, remove FastAsgn
    mnkAsgn     ## normal assignment; the destination might store a value
                ## already. Whether the source is copied or moved depends
                ## on the expression
    mnkInit     ## similar to ``mnkAsgn``, but makes the guarantee that the
                ## destination contains no value (i.e., is not initialized).
                ## "Not initialized" doesn't make any guarantees about the
                ## destination's in-memory contents

    mnkSwitch ## sets the value of a discriminator field, changing the active
              ## branch, if necessary. The destination operand must be a
              ## ``mnkPathVariant`` expression

    mnkPathNamed ## access of a named field in a record
    mnkPathPos   ## access of a field in record via its position
    # future direction: merge ``mnkPathPos`` with ``mnkPathNamed``. This first
    # requires a dedicated MIR type representation.
    mnkPathArray ## access of an array-like (both dynamic and static) value
                 ## with an integer index
    mnkPathVariant ## access a tagged union
                   ## XXX: this is likely only a temporary solution. Each
                   ##      record-case part of an object should be its own
                   ##      dedicated object type, which can then be addressed
                   ##      as a normal field
    # future direction: merge ``mnkPathVariant`` into ``mnkPathNamed`` once the
    # MIR's record type structure supports this
    mnkPathConv  ## a handle conversion. That is, a conversion that produces a
                 ## *handle*, and not a new *value*. At present, this operator
                 ## also applies to first-class handles, like ``ref``.

    mnkAddr   ## create a pointer from the provided lvalue
    mnkDeref  ## dereference a ``ptr`` or ``ref`` value

    mnkView      ## create a first-class safe alias from an lvalue
    mnkDerefView ## dereference a first-class safe alias

    mnkStdConv    ## a standard conversion. Produce a new value.
    mnkConv       ## ``conv(x)``; a conversion. Produces a new value.
    # future direction: replace both conversion operators with ``NumberConv``.
    # String-to-cstring conversion, and vice versa, should use magics, pointer
    # conversion should use ``mnkCast``
    mnkCast       ## cast the representation of a value into a different type
    mnkToSlice    ## create an openArray from the full sequence specified as
                  ## the operand
    # future direction: also use to ``ToSlice`` for creating sub-slices (i.e.,
    # ``toOpenArray``)

    mnkCall   ## invoke a procedure and pass along the provided arguments.
              ## Used for both static and dynamic calls
    # future direction: introduce a ``mnkCheckedCall`` node, for
    # representing calls that can start unwinding

    mnkRaise  ## if the operand is an ``mnkNone`` node, reraises the
              ## currently active exception. Otherwise, set the operand value
              ## as the active exception (via a move). Control-flow is
              ## transfered to the closest exception handler. If none exists,
              ## the program terminates

    mnkTag    ## must only appear as the immediate subnode to a ``mnkName``
              ## tree. Describes what kind of mutation is applied to the
              ## lvalue within the called procedure

    mnkConstr     ## constructs a either new aggregate value or set value made
                  ## up of the input values. Whether the resulting value is
                  ## owned depends on whether one the context it's used in
    mnkObjConstr  ## either allocate a new managed heap cell and returns a
                  ## ``ref`` to it, or or constructs a new aggregate value
                  ## with named fields

    mnkArg    ## when used in a call: denotes an argument that may either be
              ## passed by value or by name. Evaluation order is unspecified
              ## when used in a construction: denotes a value that is copied
              ## (shallow) into the aggregate value
    mnkName   ## denotes an argument that is passed by name
    mnkConsume## similar to ``mnkArg``, but moves (non-destructively) the
              ## value into the aggregate or parameter

    mnkVoid   ## either a:
              ## * syntactic statement node for representing void calls
              ## * statement acting as a use of the given lvalue

    mnkStmtList ## a sequence of statements, grouped together as a single
                ## statement
    mnkScope  ## the only way to introduce a scope. Scopes can be nested and
              ## dictate the lifetime of the locals that are directly enclosed
              ## by them

    mnkIf     ## depending on the run-time value of `x`, transfers control-
              ## flow to either the start or the end of the spanned code
    mnkCase   ## dispatches to one the its branches based on the run-time
              ## value of the operand
    mnkRepeat ## repeats the body indefinitely
    mnkTry    ## associates one one or more statements (the first sub-node)
              ## with: an exception handler, a finalizer, or both
    mnkExcept ## defines and attaches an exception handler to a ``try`` block.
              ## Only one handler can be attached to a ``try`` block
    mnkFinally## defines a finalizer in the context of a ``try`` construct. All
              ## control-flow that either leaves the body of the ``try`` and
              ## does not target the exception handler (if one is present) or
              ## that leaves the exception handler is redirected to inside the
              ## finalizer first. Once control-flow reaches the end of a
              ## finalizer, it is transferred to the original destination. Only
              ## one finalizer can be attached to a ``try`` block
    mnkBlock  ## attaches a label to a span of code. If control-flow reaches
              ## this statement, it is transferred to the start of the body.
              ## Once control-flow reaches the end of a ``block``, it is
              ## transferred to the next statement/operation following the
              ## block
    mnkBreak  ## transfers control-flow to the statement/operation following
              ## after the ``block`` with the given label
    mnkReturn ## if the code-fragment represents the body of a procedure,
              ## transfers control-flow back to the caller

    mnkBranch ## defines a branch of an ``mnkExcept`` or ``mnkCase``

    mnkAsm    ## embeds backend-dependent code directly into the output
    mnkEmit   ## embeds backend-dependent code directly into the output

    mnkEnd    ## marks the end of a sub-tree. Has no behaviour associated with
              ## it -- it's only required to know where a sub-tree ends
    # future direction: replace the End node with storing the number of sub-
    # nodes of a sub-tree on the node itself. This will require significant
    # structural changes, as not all node kinds are able to use the length
    # field at the moment

    mnkPNode ## depending on the context, either statement or something else.
             ## If it appears as a statement, it is expected to not have any
             ## obsersvable effects
             ## XXX: eventually, everything that currently requires
             ##      ``mnkPNode`` should be expressable directly in the IR

  EffectKind* = enum
    ekMutate    ## the value in the location is mutated
    ekReassign  ## a new value is assigned to the location
    ekKill      ## the value is removed from the location (without observing
                ## it), leaving the location empty
    ekInvalidate## all knowledge and assumptions about the location and its
                ## value become outdated. The state of it is now completely
                ## unknown

  GeneralEffect* = enum
    geMutateGlobal ## the operation mutates global state
    geRaises       ## the operation is a source of exceptional control-flow

  MirNode* = object
    typ*: PType ## non-nil for all expressions
    info*: SourceId
      ## non-critical meta-data associated with the node (e.g., origin
      ## information)
    case kind*: MirNodeKind
    of mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal:
      sym*: PSym
    of mnkField, mnkPathNamed, mnkPathVariant:
      field*: PSym
    of mnkLiteral:
      lit*: PNode
    of mnkTemp, mnkAlias:
      temp*: TempId
    of mnkPathPos:
      position*: uint32 ## the 0-based position of the field
    of mnkCall:
      effects*: set[GeneralEffect]
    of mnkMagic:
      magic*: TMagic
    of mnkBlock, mnkBreak:
      label*: LabelId ## for a block, the label that identifies the block;
                      ## for a break, the label of the block to break out of
    of mnkEnd:
      start*: MirNodeKind ## the kind of the corresponding start node
    of mnkPNode:
      node*: PNode
    of mnkTag:
      effect*: EffectKind ## the effect that happens when the operator the
                          ## tagged value is passed to is executed
    else:
      len*: int

  MirTree* = seq[MirNode]
  MirNodeSeq* = seq[MirNode]
    ## A buffer of MIR nodes without any further meaning

  NodeIndex* = uint32
  NodePosition* = distinct int32
    ## refers to a ``MirNode`` of which the position relative to other nodes
    ## has meaning. Uses a signed integer as the base
  OpValue* = distinct uint32
    ## refers to an node appearing in an expression/operand position

  ArgKinds = range[mnkArg..mnkConsume]
    ## helper type to make writing exhaustive case statement easier

const
  AllNodeKinds* = {low(MirNodeKind)..high(MirNodeKind)}
    ## Convenience set containing all existing node kinds

  DefNodes* = {mnkDef, mnkDefCursor, mnkDefUnpack, mnkBind, mnkBindMut}
    ## Node kinds that represent definition statements (i.e. something that
    ## introduces a named entity)

  AtomNodes* = {mnkNone..mnkType, mnkMagic, mnkBreak, mnkReturn, mnkPNode}
    ## Nodes that don't support sub nodes.

  SubTreeNodes* = AllNodeKinds - AtomNodes - {mnkEnd}
    ## Nodes that start a sub-tree. They're always matched with an ``mnkEnd``
    ## node.

  SingleOperandNodes* = {mnkPathNamed, mnkPathPos, mnkPathVariant, mnkPathConv,
                         mnkAddr, mnkDeref, mnkView, mnkDerefView, mnkStdConv,
                         mnkConv, mnkCast, mnkToSlice, mnkRaise, mnkTag, mnkArg,
                         mnkName, mnkConsume, mnkVoid}
    ## Nodes that start sub-trees but that always have a single sub node.

  ArgumentNodes* = {mnkArg, mnkName, mnkConsume}
    ## Nodes only allowed in argument contexts.

  SymbolLike* = {mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal}
    ## Nodes for which the `sym` field is available

  # --- semantics-focused sets:

  Atoms* = {mnkNone .. mnkType} - {mnkField}
    ## Nodes that may be appear in atom-expecting slots.

  StmtNodes* = {mnkScope, mnkStmtList, mnkIf, mnkCase, mnkRepeat, mnkTry,
                mnkBlock, mnkBreak, mnkReturn, mnkRaise, mnkPNode, mnkInit,
                mnkAsgn, mnkSwitch, mnkFastAsgn, mnkVoid, mnkRaise, mnkEmit,
                mnkAsm} + DefNodes

  LvalueExprKinds* = {mnkPathPos, mnkPathNamed, mnkPathArray, mnkPathVariant,
                      mnkPathConv, mnkDeref, mnkDerefView, mnkTemp, mnkAlias,
                      mnkLocal, mnkParam, mnkConst, mnkGlobal}
  RvalueExprKinds* = {mnkLiteral, mnkType, mnkProc, mnkConv, mnkStdConv,
                      mnkCast, mnkAddr, mnkView, mnkToSlice}
  ExprKinds* =       {mnkCall, mnkConstr, mnkObjConstr} +
                     LvalueExprKinds + RvalueExprKinds

func `==`*(a, b: SourceId): bool {.borrow.}
func `==`*(a, b: TempId): bool {.borrow.}
func `==`*(a, b: LabelId): bool {.borrow.}

# XXX: ideally, the arithmetic operations on ``NodePosition`` should not be
#      exported. How the nodes are stored should be an implementation detail

template `-`*(a: NodePosition, b: int): NodePosition =
  NodePosition(ord(a) - b)

template `+`*(a: NodePosition, b: int): NodePosition =
  NodePosition(ord(a) + b)

template `dec`*(a: var NodePosition) =
  dec int32(a)

template `inc`*(a: var NodePosition) =
  inc int32(a)

func `<`*(a, b: NodePosition): bool {.borrow, inline.}
func `<=`*(a, b: NodePosition): bool {.borrow, inline.}
func `==`*(a, b: NodePosition): bool {.borrow, inline.}

func `in`*(p: NodePosition, tree: MirTree): bool {.inline.} =
  ord(p) >= 0 and ord(p) < tree.len

template `[]`*(tree: MirTree, i: NodePosition | OpValue): untyped =
  tree[ord(i)]

func parent*(tree: MirTree, n: NodePosition): NodePosition =
  result = n

  var depth = 1
  while depth > 0:
    dec result

    let kind = tree[result].kind
    depth += ord(kind == mnkEnd) - ord(kind in SubTreeNodes)

func parentEnd*(tree: MirTree, n: NodePosition): NodePosition =
  # Computes the position of the ``mnkEnd`` node belonging to the sub-tree
  # enclosing `n`
  result = n

  # start at depth '2' if `n` starts a sub-tree itself. The terminator of said
  # sub-tree would be treated as the parent's end otherwise
  var depth = 1 + ord(tree[n].kind in SubTreeNodes)
  while depth > 0:
    inc result

    let kind = tree[result].kind
    depth += ord(kind in SubTreeNodes) - ord(kind == mnkEnd)

func sibling*(tree: MirTree, n: NodePosition): NodePosition =
  ## Computes the index of the next sibling node of `x`
  # TODO: should return a option. Not all nodes have siblings
  # TODO: since this doesn't consider 'end' nodes, the procedure should
  #       probably be renamed to ``rawSibling``?
  result = n + 1

  var depth = ord(tree[n].kind in SubTreeNodes)
  while depth > 0:
    let kind = tree[result].kind
    # to be more efficient, we don't use branching. We're incrementing
    # `depth` whenever we encounter the start of a sub-tree and decrement
    # it when an 'end' node is encountered
    depth += ord(kind in SubTreeNodes) - ord(kind == mnkEnd)

    inc result

  if result.int == tree.len or tree[result].kind == mnkEnd:
    # no sibling exists
    discard

func previous*(tree: MirTree, n: NodePosition): NodePosition =
  ## Computes the index of `n`'s the preceding sibling node. If there
  ## is none, returns the index of the parent node.
  var i = n - 1

  var depth = ord(tree[i].kind == mnkEnd)
  while depth > 0:
    dec i
    let kind = tree[i].kind

    # to be more efficient, we don't use branching. We're incrementing
    # `depth` whenever we encounter the end of a sub-tree and decrement
    # it when a start of one is encountered
    depth += ord(kind == mnkEnd) - ord(kind in SubTreeNodes)

  assert ord(i) >= 0
  result = i

func computeSpan*(tree: MirTree, n: NodePosition): Slice[NodePosition] =
  ## If `n` refers to a leaf node, returns a span with the `n` as the single
  ## item.
  ## Otherwise, computes and returns the span of nodes part of the sub-tree
  ## at `n`. The 'end' node is included.
  result = n .. (sibling(tree, n) - 1)

func start*(tree: MirTree, n: NodePosition): NodePosition =
  ## Find the corresponding start node for an ``mnkEnd`` node
  assert tree[n].kind == mnkEnd
  result = n

  var depth = 1
  while depth > 0:
    dec result

    let kind = tree[result].kind
    depth += ord(kind == mnkEnd) - ord(kind in SubTreeNodes)

func findEnd*(tree: MirTree, n: NodePosition): NodePosition =
  ## Finds the corresponding ``end`` node for the node `n` that starts a
  ## sub-tree
  assert tree[n].kind in SubTreeNodes
  result = sibling(tree, n) - 1

func child*(tree: MirTree, n: NodePosition, index: Natural): NodePosition =
  ## Returns the position of the child node at index `index`. `index` *must*
  ## refer to a valid sub-node -- no validation is performed
  assert tree[n].kind in SubTreeNodes
  result = n + 1 # point `result` to the first child
  for _ in 0..<index:
    result = sibling(tree, result)

func operand*(tree: MirTree, n: NodePosition, i: Natural): OpValue {.inline.} =
  ## Returns the `i`-th operand to the sub-tree at `n`. It is expected that
  ## the operation has at least `i` + 1 operands.
  OpValue child(tree, n, i)

func `[]`*(tree: MirTree, n: NodePosition, index: Natural): lent MirNode =
  ## Returns the `index`-th child node of sub-tree `n`.
  tree[child(tree, n, index)]

func `[]`*(tree: MirTree, n: OpValue, index: Natural): lent MirNode =
  ## Returns the `index`-th child node of sub-tree `n`.
  tree[child(tree, NodePosition n, index)]

func getStart*(tree: MirTree, n: NodePosition): NodePosition =
  ## If `n` refers to an ``end`` node, returns the corresponding start node --
  ## `n` otherwise
  if tree[n].kind == mnkEnd:
    start(tree, n)
  else:
    n

func findParent*(tree: MirTree, start: NodePosition,
                 kind: MirNodeKind): NodePosition =
  ## Searches for the first enclosing sub-tree node of kind `kind` (which is
  ## *required* to exist). The node at `start` is itself also considered
  ## during the search
  assert kind in SubTreeNodes
  result = start
  while tree[result].kind != kind:
    result = parent(tree, result)

func numArgs*(tree: MirTree, n: NodePosition): int =
  ## Computes the number of arguments in the call tree.
  var n = n + 1
  while tree[n].kind != mnkEnd:
    inc result
    n = tree.sibling(n)

func operand*(tree: MirTree, op: OpValue|NodePosition): OpValue =
  ## Returns the index (``OpValue``) of the operand for the single-input node
  ## at `op`.
  assert tree[op].kind in SingleOperandNodes, $tree[op].kind
  let pos =
    when op is NodePosition: op
    else:                    NodePosition(op)
  result = OpValue(pos + 1)

func argument*(tree: MirTree, n: NodePosition, i: Natural): OpValue =
  ## Returns the `i`-th argument in the call-like tree at `n`, skipping
  ## tag nodes. It is expected that the call has at least `i` + 1
  ## arguments.
  assert tree[n].kind == mnkCall
  var n = tree.sibling(n + 1)
  for _ in 0..<i:
    n = tree.sibling(n)
  n = NodePosition tree.operand(n)
  # skip the tag node if one exists
  if tree[n].kind == mnkTag:
    tree.operand(n)
  else:
    OpValue n

func skip*(tree: MirTree, n: OpValue, kind: MirNodeKind): OpValue =
  ## If `n` is of `kind`, return its operand node, `n` otherwise.
  if tree[n].kind == kind: tree.operand(n)
  else:                    n

iterator pairs*(tree: MirTree): (NodePosition, lent MirNode) =
  var i = 0
  let L = tree.len
  while i < L:
    yield (i.NodePosition, tree[i])
    inc i

iterator subNodes*(tree: MirTree, n: NodePosition): NodePosition =
  ## Iterates over and yields all direct child nodes of `n`
  var r = n + 1
  while tree[r].kind != mnkEnd:
    yield r
    r = sibling(tree, r)

iterator arguments*(tree: MirTree, n: NodePosition): (ArgKinds, OpValue) =
  ## Returns the argument kinds together with the operand node (or tag tree).
  assert tree[n].kind == mnkCall
  var i = tree.sibling(n + 1) # skip the callee
  while tree[i].kind != mnkEnd:
    yield (ArgKinds(tree[i].kind), tree.operand(i))
    i = tree.sibling(i)

func findDef*(tree: MirTree, n: NodePosition): NodePosition =
  ## Finds and returns the first definition for the name of the temporary
  ## at node `n`. No control-flow analysis is performed.
  let expected = tree[n].temp
  # first, unwind until the closest statement
  result = n
  while tree[result].kind notin StmtNodes:
    result = tree.parent(result)

  # then search for the definition statement
  while result > NodePosition 0:
    if tree[result].kind in DefNodes:
      let name = tree.operand(result, 0)
      if tree[name].kind in {mnkTemp, mnkAlias} and tree[name].temp == expected:
        return

    result = tree.previous(result)

  unreachable("no corresponding def found")

# XXX: ``lpairs`` is not at all related to the mid-end IR. The ``pairs``
#      iterator from the stdlib should be changed to use ``lent`` instead
iterator lpairs*[T](x: seq[T]): (int, lent T) =
  var i = 0
  let L = x.len
  while i < L:
    yield (i, x[i])
    inc i