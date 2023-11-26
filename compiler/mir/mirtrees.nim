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
    mnkTemp   ## temporary; works the same as a ``mnkLocal`` but uses a
              ## separate namespace. This currently allows for cheap ad-hoc
              ## introduction of new locals
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

    mnkFastAsgn ## ``fastAsgn(dst, src)``; assigns the `src` value to the location
                ## named by the lvalue `dst`. Neither the previous value in the
                ## destination location nor the source value are mutated in any
                ## way. No transfer of ownership happens.
    # future direction: same as with DefCursor, remove FastAsgn
    mnkAsgn     ## ``asgn(dst, src)``; assigns the `src` value to the location
                ##  named by `dst`, also transferring onwership.
    mnkInit     ## ``init(dst, src)``; similar to `asgn`, but with the
                ## guarantee that the destination contains no value prior

    mnkSwitch ## ``switch(x, y)``; changes the active branch of the record-case
              ## identified by the result of the ``pathVariant`` operation used
              ## as the `x` operand. `y` is the new discriminator value

    mnkPathNamed ## access of a named field in a record
    mnkPathPos   ## access of a field in record via its position
    # future direction: merge ``mnkPathPos`` with ``mnkPathNamed``. This first
    # requires a dedicated MIR type representation.
    mnkPathArray ## ``pathArray(x, i)``; array-like access
    # future direction: separate access of run-time arrays (i.e., strings and
    # seqs) into a dedicated operation
    mnkPathVariant ## access a field inside a tagged union
                   ## XXX: this is likely only a temporary solution. Each
                   ##      record-case part of an object should be its own
                   ##      dedicated object type, which can then be addressed
                   ##      as a normal field
    # future direction: merge ``mnkPathVariant`` into ``mnkPathNamed`` once the
    # MIR's record type structure supports this
    mnkPathConv  ## an handle conversion. That is, a conversion that produces a
                 ## *handle*, and not a new *value*. At present, this operator
                 ## also applies to first-class handles, like ``ref``.

    mnkAddr   ## ``addr(x)``; creates a first-class unsafe alias/handle (i.e.
              ## pointer) from the input lvalue `x`
    mnkDeref  ## ``deref(x)``; dereferences the pointer-like `x` (this
              ## *excludes* views), producing an lvalue that has same identity
              ## as the pointed-to location
              ## XXX: the possibility of invalid pointers (e.g. nil pointer) is
              ##      currently ignored

    # XXX: the exact semantics around views and their related operators are
    #      not yet finalized. One should not rely on them too much at this
    #      point
    mnkView      ## ``view(x)``; creates a safe alias of the l-value 'x'
    mnkDerefView ## ``derefView(x)``; dereferences a view, producing the lvalue
                 ## named by it
    # XXX: ``mnkDerefView`` is not used for ``openArray`` right now, due to
    #      the latter's interactions with ``var`` and ``lent``

    mnkStdConv    ## ``stdConv(x)``; a standard conversion.Produce a new value.
    mnkConv       ## ``conv(x)``; a conversion. Produces a new value.
    # XXX: distinguishing between ``stdConv`` and ``conv`` is only done to
    #      make ``cgirgen`` a bit more efficient. Further progress should focus
    #      on removing the need for it
    mnkCast       ## ``cast(x)``; produces a new *instance* of the input value
                  ## with a different type
    mnkToSlice    ## creates an openArray from the full sequence specified as
                  ## the operand
    # future direction: also use to ``ToSlice`` for creating sub-slices (i.e.,
    # ``toOpenArray``)

    mnkCall   ## ``call(p, ...)``; transfers control-flow (i.e. calls) the
              ## procedure that `p` evaluates to and passes the provided
              ## arguments
    mnkMagic  ## ``magic(...)``; a call to a magic procedure

    mnkRaise  ## ``raise(x)``; if `x` is a ``none`` node, reraises the
              ## currently active exception. If `x` is a value, transfers the
              ## ownership over it to the raise operation and transfers
              ## control-flow to the respective exception handler

    mnkTag    ## ``tag[T](x)``; must only appear directly as the input to
              ## either a ``name`` sink. Marks evaluating the operator that
              ## has the result of the tag operation as input as having the
              ## specified effect on the location named by l-value `x`

    mnkConstr     ## ``constr(...)``; constructs a new aggregate value or set
                  ## value made up of the input values. Whether the resulting
                  ## value is owned depends on whether one the context it's
                  ## used in
    mnkObjConstr  ## ``objConstr(...)``; either heap-allocates and initializes
                  ## a new managed location, or constructs a new aggregate value
                  ## with named fields

    # TODO: update the argument node documentation to reflect reality
    mnkArg    ## binds either an instance of the input value or the value
              ## itself to an argument
    mnkName   ## binds an lvalue to an argument
    mnkConsume## similar to ``arg``, but also transfers ownership over the
              ## value from the source to the operation taking the argument
              ## as input. The source value *must* be an owned value.
              ## **Note**: the transfer of ownership happens when the
              ## value is bound to the argument, not when control-flow reaches
              ## the target operation
    # future direction: prior to the move-analyser pass, ``Consume`` encodes a
    # *request* rather than a *fact*. This needs to be changed; the AST -> MIR
    # translation needs to make that the argument to a ``sink`` parameter can
    # *always* be consumed. ``Consume`` always meaning "consume" will
    # make data-flow analysis significantly simpler

    mnkVoid   ## the 'void' sink. Discards the input value without doing
              ## anything else with it

    mnkStmtList ## groups statements together
    mnkScope  ## the only way to introduce a scope. Scopes can be nested and
              ## dictate the lifetime of the locals that are directly enclosed
              ## by them

    mnkIf     ## ``if(x)``; depending on the runtime value of `x`, transfers
              ## control-flow to either the start or the end of the code, the
              ## ``if`` spans
    mnkCase   ## ``case(x)``; depending on the runtime value of `x`, transfers
              ## control-flow to the start of one of its branches
    mnkRepeat ## once control-flow reaches this statement, control-flow is
              ## transfered to start of its body. Once control-flow reaches
              ## the end of the body, it is transfered back to the start. In
              ## other words, repeats its body an infinite number of times
              # XXX: rename to ``mnkRepeat``?
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

    mnkAsm    ## corresponds to the high-level ``asm`` statement. Takes an
              ## argument block as input, but has itself no meaning at the MIR
              ## level
    mnkEmit   ## corresponds to the ``emit`` directive. In the context of the
              ## MIR, has the same behaviour as ``mnkAsm``

    mnkEnd    ## marks the physical end of a sub-tree. Has no semantic
              ## meaning -- it's only required to know where a sub-tree ends
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
    typ*: PType ## must be non-nil for operators, inputs, and sinks
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
      # XXX: with the current design, a magic call cannot have general effects,
      #      which is a problem, as magic calls can indeed have general effects
      #      (such as raising an exception). The ability to store information
      #      about general effect ouf-of-band is likely required to properly
      #      support this
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
      # XXX: now only used by ``mnkTry``, ``mnkCase``, and ``mnkObjConstr``. In
      #      each case, the information is redundant. That is, the information
      #      it stores can be compute from the tree itself
      len*: int

  MirTree* = seq[MirNode]
  MirNodeSeq* = seq[MirNode]
    ## A buffer of MIR nodes without any further meaning

  # XXX: some of the distinct types below have a super/sub-type relation to
  #      each other, but this can't be expressed in the language right now
  #      (without turning the types into empty ``object``s)

  NodeIndex* = uint32
  NodeInstance* = distinct range[0'u32..high(uint32)-1]
    ## refers to a node as just a node. Used to communicate that only the node
    ## itself is of interest, not what it represents
  NodePosition* = distinct int32
    ## refers to a ``MirNode`` of which the position relative to other nodes
    ## has meaning. Uses a signed integer as the base
  Operation* = distinct uint32
    ## refers to a ``MirNode`` that represents an operation
  OpValue* = distinct uint32
    ## refers to a value an operation produces
  # TODO: some of the distinctions aren't as useful anymore; clean them up

  ArgKinds = range[mnkArg..mnkConsume]
    ## helper type to make writing exhaustive case statement easier

const
  AllNodeKinds* = {low(MirNodeKind)..high(MirNodeKind)}
    ## Convenience set containing all existing node kinds

  DefNodes* = {mnkDef, mnkDefCursor, mnkDefUnpack, mnkBind, mnkBindMut}
    ## Node kinds that represent definition statements (i.e. something that
    ## introduces a named entity)

  AtomNodes* = {mnkNone..mnkType, mnkBreak, mnkReturn, mnkPNode}
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

  StmtNodes* = {mnkScope, mnkStmtList, mnkIf, mnkCase, mnkRepeat, mnkTry,
                mnkBlock, mnkBreak, mnkReturn, mnkRaise, mnkPNode, mnkInit,
                mnkAsgn, mnkSwitch, mnkFastAsgn, mnkVoid, mnkRaise, mnkEmit,
                mnkAsm} + DefNodes
    ## Nodes that always act as statements syntax-wise.

  SymbolLike* = {mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal}
    ## Nodes for which the `sym` field is available

  # --- semantics-focused sets:

  Atoms* = {mnkNone .. mnkType} - {mnkField}
    ## Nodes that may be appear in atom-expecting slots.

  ConsumeCtx* = {mnkConsume, mnkRaise}
    ## if an lvalue is used as an operand to these operators, the value stored
    ## in the named location is considered to be consumed (ownership over it
    ## transfered to the operation)
    ## XXX: possibly not useful anymore
  UseContext* = {mnkArg, mnkDeref, mnkDerefView, mnkStdConv, mnkConv, mnkCast,
                 mnkToSlice, mnkVoid, mnkIf, mnkCase} + ConsumeCtx
    ## using an lvalue as the operand to one of these operators means that
    ## the content of the location is observed (when control-flow reaches the
    ## operator). In other words, applying the operator results in a read
    ## XXX: this set too
  OpsWithEffects* = {mnkCall, mnkMagic, mnkAsgn, mnkFastAsgn, mnkSwitch,
                     mnkInit}
    ## the set of operations that can have lvalue-parameterized or general
    ## effects
    ## XXX: this set too

  LvalueExprKinds* = {mnkPathPos, mnkPathNamed, mnkPathArray, mnkPathVariant,
                      mnkPathConv, mnkDeref, mnkDerefView, mnkTemp, mnkAlias,
                      mnkLocal, mnkParam, mnkConst, mnkGlobal}
  RvalueExprKinds* = {mnkLiteral, mnkType, mnkProc, mnkConv, mnkStdConv,
                      mnkCast, mnkAddr, mnkView, mnkToSlice}
  ExprKinds* =       {mnkCall, mnkMagic, mnkConstr, mnkObjConstr} +
                     LvalueExprKinds + RvalueExprKinds

func `==`*(a, b: SourceId): bool {.borrow.}
func `==`*(a, b: TempId): bool {.borrow.}
func `==`*(a, b: LabelId): bool {.borrow.}

# make ``NodeInstance`` available to be used with ``OptIndex``:
template indexLike*(_: typedesc[NodeInstance]) = discard

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

converter toOp*(x: OpValue): Operation {.inline.} =
  ## For convenience, a converter is provided for this conversion. Each
  ## ``OpValue`` is always backed by an ``Operation``
  Operation(x)

func `<`*(a, b: NodePosition): bool {.borrow, inline.}
func `<=`*(a, b: NodePosition): bool {.borrow, inline.}
func `==`*(a, b: NodePosition): bool {.borrow, inline.}

func `==`*(a, b: Operation): bool {.borrow, inline.}

func `in`*(p: NodePosition, tree: MirTree): bool {.inline.} =
  ord(p) >= 0 and ord(p) < tree.len

template `[]`*(tree: MirTree, i: NodePosition | NodeInstance | Operation | OpValue): untyped =
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

func childIdx*(tree: MirTree, n: NodePosition, index: int): NodePosition =
  ## Returns the position of the child node at index `index`. `index` *must*
  ## refer to a valid sub-node -- no validation is performed
  result = n + 1 # point `result` to the first child
  for _ in 0..<index:
    result = sibling(tree, result)

func `[]`*(tree: MirTree, n: NodePosition, index: int): lent MirNode =
  ## Returns the `index`-th child node of sub-tree `n`.
  tree[childIdx(tree, n, index)]

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

func operand*(tree: MirTree, n: NodePosition, opr: Natural): OpValue =
  ## Returns the `opr`-th operand to the sub-tree at `n`. It is expected that
  ## the operation has at least `opr` + 1 operands.
  assert tree[n].kind in SubTreeNodes
  var n = n + 1
  for i in 0..<opr:
    n = tree.sibling(n)
  result = OpValue n

func numArgs*(tree: MirTree, n: NodePosition): int =
  ## Computes the number of arguments in the call tree.
  var n = n + 1
  while tree[n].kind != mnkEnd:
    inc result
    n = tree.sibling(n)

func operand*(tree: MirTree, op: Operation|OpValue|NodePosition): OpValue =
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
  assert tree[n].kind in {mnkCall, mnkMagic}
  var n = n + 1 + ord(tree[n].kind == mnkCall)
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
  assert tree[n].kind in {mnkCall, mnkMagic}
  # skip the callee for calls
  var n = n + 1 + ord(tree[n].kind == mnkCall)
  while tree[n].kind != mnkEnd:
    yield (ArgKinds(tree[n].kind), tree.operand(n))
    n = tree.sibling(n)

func findDef*(tree: MirTree, n: NodePosition): NodePosition =
  ## Finds and returns the first definition for the name of the temporary
  ## at node `n`. No control-flow analysis is performed.
  let expected = tree[n].temp
  # first, unwind until the closest statement
  var n = n
  while tree[n].kind notin StmtNodes:
    n = tree.parent(n)

  # then search for the definition statement
  while n > NodePosition 0:
    if tree[n].kind in DefNodes:
      let name = tree.operand(n, 0)
      if tree[name].kind in {mnkTemp, mnkAlias} and tree[name].temp == expected:
        return n

    n = tree.previous(n)

  unreachable("no corresponding def found")

# XXX: ``lpairs`` is not at all related to the mid-end IR. The ``pairs``
#      iterator from the stdlib should be changed to use ``lent`` instead
iterator lpairs*[T](x: seq[T]): (int, lent T) =
  var i = 0
  let L = x.len
  while i < L:
    yield (i, x[i])
    inc i