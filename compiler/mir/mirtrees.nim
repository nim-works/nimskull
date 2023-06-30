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

  LocalId = distinct uint32
    ## Identifies a local inside a code fragment
  GlobalId = distinct uint32
    ## Identifies a global inside a code fragment
  ConstId = distinct uint32
    ## Identifies a named constant inside a code fragment
  ParamId = distinct uint32
    ## Identifies a parameter of the code fragment
  FieldId = distinct uint32
    ## Identifies the field of a record type
  ProcedureId = distinct uint32
    ## Identifies a procedure
  LiteralId = distinct uint32
    ## Identifies a literal

  TypeInstance = distinct uint32
    ## Refers to an existing type instance
  TypeId = distinct uint32
    ## The ID of a type instance or nil

type
  ## Different to the ID types above, how and what the following ID types
  ## represent is dictated by the MIR
  TempId* = distinct uint32
    ## ID of a temporary location. A temporary location is created and
    ## inserted by the compiler. The only difference to other named locations
    ## is that temporaries are allowed to be elided (by an optimization pass,
    ## for example) if it's deemed to have no effect on the codes' semantics
  LabelId* = distinct uint32
    ## ID of a label, used to identify a block (``mnkBlock``). The default
    ## value is empty state and means "absence of label"

  MirNodeKind* = enum
    ## Users of ``MirNodeKind`` should not depend on the absolute or relative
    ## order between the enum values
    # when adding new enum values, make sure to adjust the sets below

    # entity names:
    mnkProc   ## procedure
    mnkConst  ## named constant
    mnkGlobal ## global location
    mnkParam  ## parameter
    mnkLocal  ## local location
    mnkTemp   ## temporary; Semantics-wise, a ``mnkTemp`` is the same as a
              ## ``mnkLocal``, but a different namespace is used (``TempId``
              ## vs. ``PSym``). This allows for cheap introduction of locals

    mnkOpParam ## references a parameter of the enclosing region
               ## XXX: see if it's possible to merge this with ``mnkParam``,
               ##      by treating a procedure's body as a region itself

    mnkLiteral ## literal data. Currently represented via a ``PNode``
    mnkType    ## a type literal

    mnkNone    ## the "nothing" value. Represents the absence of a value.
               ## To ease the back-to-``PNode`` translation, this node is
               ## currently allowed to have a non-nil type.

    mnkDef       ## defines an entity and, if the entity is a location, starts
                 ## its lifetime (if the entity is a location). If the entity
                 ## is a location, the ``def`` can be either a *sink* or a
                 ## statement -- it must be a statement otherwise
    mnkDefCursor ## starts the lifetime of a location that is non-owning. The
                 ## location may or may not contain a value and is not
                 ## responsible for destroying it
                 ## inputs: either a single value (no arg-block) or none
    mnkDefUnpack ## starts the lifetime of a location that owns a *tuple*
                 ## value but is not responsible for destroying it, as all
                 ## sub-values are moved out of it
                 ## **Warning**: this is a hack and it should be relied on as
                 ## little as possible. It's a workaround to support the
                 ## temporary tuple values introduced for destructuring.

    mnkFastAsgn ## ``fastAsgn(dst, src)``; assigns the `src` value to the location
                ## named by the lvalue `dst`. Neither the previous value in the
                ## destination location nor the source value are mutated in any
                ## way. No transfer of ownership happens.
    mnkAsgn     ## ``asgn(dst, src)``; assigns the `src` value to the location
                ##  named by `dst`, also transferring onwership.
    mnkInit     ## ``init(dst, src)``; similar to `asgn`, but with the
                ## guarantee that the destination contains no value prior

    mnkSwitch ## ``switch(x, y)``; changes the active branch of the record-case
              ## identified by the result of the ``pathVariant`` operation used
              ## as the `x` operand. `y` is the new discriminator value

    mnkPathNamed ## access of a named field in a record
    mnkPathPos   ## access of a field in record via its position
    mnkPathArray ## ``pathArray(x, i)``; array-like access
    mnkPathVariant ## access a field inside a tagged union
                   ## XXX: this is likely only a temporary solution. Each
                   ##      record-case part of an object should be its own
                   ##      dedicated object type, which can then be addressed
                   ##      as a normal field

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

    mnkStdConv    ## ``stdConv(x)``; a standard conversion. Depending on the
                  ## source and target type, lvalue-ness is preserved
    mnkConv       ## ``conv(x)``; a conversion. Depending on the source and
                  ## target type, lvalue-ness is preserved
    # XXX: distinguishing between ``stdConv`` and ``conv`` is only done to
    #      make ``astgen`` a bit more efficient. Further progress should focus
    #      on removing the need for it
    mnkCast       ## ``cast(x)``; produces a new *instance* of the input value
                  ## with a different type

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

    # XXX: ``mnkObjConstr`` could be implemented as a user-op (which would
    #      remove the need for ``mnkField``) at the cost of a larger amount
    #      of nodes
    mnkConstr     ## ``constr(...)``; constructs a new compound value made up of
                  ## the input values. Whether the resulting value is owned
                  ## depends on whether one the context it's used in
    mnkObjConstr  ## ``objConstr(...)``; either heap-allocates and initializes
                  ## a new managed location, or constructs a new compound value
                  ## with named fields

    # the following three are argument sinks. They must only appear directly
    # inside an ``mnkArgBlock``
    mnkArg    ## binds either an instance of the input value or the value
              ## itself to an argument
    mnkName   ## binds an lvalue to an argument
    mnkConsume## similar to ``arg``, but also transfers ownership over the
              ## value from the source to the operation taking the argument
              ## as input. The source value *must* be an owned value.
              ## **Note**: the transfer of ownership happens when the
              ## value is bound to the argument, not when control-flow reaches
              ## the target operation

    mnkVoid   ## the 'void' sink. Discards the input value without doing
              ## anything else with it

    mnkField  ## may only appear as a sub-node to ``mnkObjConstr``. Identifies
              ## the record field the corresponding argument is assigned to

    mnkArgBlock ## an argument block groups the operands to an operation
                ## together, and is required whenever an operation takes more
                ## than one operand -- whether an argumnet block is required for
                ## when there's only a single operand depends on the
                ## corresponding operation
    mnkRegion ## a region is something that accepts arguments (provided by a
              ## mandatory arg-block) and performs some logic that is required
              ## to only have the effects described by the argument tags. This
              ## also includes control-flow effects, e.g. raising an exception
              ## that is not handled inside the region. Right now, definitions
              ## inside a region are allowed, but this might change in the
              ## future

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
    mnkBreak  ## if a non-nil label is provided, transfers control-flow to the
              ## statement/operation following after the ``block`` with the
              ## given label. If no label is provided, control-flow is
              ## transferred to the exit of the enclosing ``repeat`` (it is
              ## required that there exists one)
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

    mnkPNode ## depending on the context, either statement or something else.
             ## If it appears as a statement, it is expected to not have any
             ## obsersvable effects
             ## XXX: eventually, everything that currently requires
             ##      ``mnkPNode`` (for example, ``nkAsmStmt``, emit, etc.)
             ##      should be expressable directly in the IR

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

    case kind*: MirNodeKind
    of mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal:
      sym*: PSym
    of mnkField, mnkPathNamed, mnkPathVariant:
      field*: PSym
    of mnkLiteral:
      lit*: PNode
    of mnkTemp:
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
    of mnkOpParam:
      param*: uint32 ## the 0-based index of the enclosing region's parameter
    of mnkBlock, mnkBreak:
      label*: LabelId ## for a block, its label. A block always must always
                      ## have a valid label ('none' is disallowed).
                      ## for a break, the label of the block to break out of.
                      ## May be 'none', in which case it means "exit the
                      ## enclosing 'repeat'"
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

const
  AllNodeKinds* = {low(MirNodeKind)..high(MirNodeKind)}
    ## Convenience set containing all existing node kinds

  DefNodes* = {mnkDef, mnkDefCursor, mnkDefUnpack}
    ## Node kinds that represent definition statements (i.e. something that
    ## introduces a named entity)

  SubTreeNodes* = {mnkObjConstr, mnkArgBlock, mnkRegion, mnkStmtList, mnkScope,
                   mnkIf..mnkBlock, mnkBranch } + DefNodes
    ## Nodes that mark the start of a sub-tree. They're always matched with a
    ## corrsponding ``mnkEnd`` node

  AtomNodes* = AllNodeKinds - SubTreeNodes
    ## Nodes that aren't sub-trees

  InputNodes* = {mnkProc..mnkNone, mnkArgBlock}
    ## Nodes that can appear in the position of inputs/operands but that
    ## themselves don't have any operands
  InOutNodes* = {mnkMagic, mnkCall, mnkPathNamed..mnkPathVariant, mnkConstr,
                 mnkObjConstr, mnkView, mnkTag, mnkCast, mnkDeref, mnkAddr,
                 mnkDerefView, mnkStdConv, mnkConv}
    ## Operations that act as both input and output
  SourceNodes* = InputNodes + InOutNodes
    ## Nodes than can appear in the position of inputs/operands

  OutputNodes* = {mnkRaise, mnkFastAsgn..mnkInit, mnkSwitch, mnkVoid, mnkIf,
                  mnkCase, mnkRegion, mnkAsm, mnkEmit} + DefNodes
    ## Node kinds that are allowed in every output context
    # TODO: maybe rename to SinkNodes

  ArgumentNodes* = {mnkArg, mnkName, mnkConsume}
    ## Node kinds only allowed in an output context directly inside an
    ## arg-block

  SingleInputNodes* = {mnkAddr, mnkDeref, mnkDerefView, mnkCast, mnkConv,
                       mnkTag, mnkIf, mnkCase, mnkRaise, mnkVoid} +
                      ArgumentNodes
    ## Operators and statements that must not have argument-blocks as input

  StmtNodes* = {mnkScope, mnkRepeat, mnkTry, mnkBlock, mnkBreak, mnkReturn,
                mnkPNode} + DefNodes
    ## Nodes that act as statements syntax-wise

  SymbolLike* = {mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal}
    ## Nodes for which the `sym` field is available

  NoLabel* = LabelId(0)

func `==`*(a, b: TempId): bool {.borrow.}
func `==`*(a, b: LabelId): bool {.borrow.}

func isSome*(x: LabelId): bool {.inline.} =
  x.uint32 != 0

func isNone*(x: LabelId): bool {.inline.} =
  x.uint32 == 0

template `[]`*(x: LabelId): uint32 =
  assert x.uint32 != 0
  uint32(x) - 1

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
  ## Computes the index of the previous sibling node of `x`
  # TODO: should return a option. Not all nodes have predecessors
  var i = n - 1

  var depth = ord(tree[n].kind == mnkEnd)
  while depth > 0:
    let kind = tree[i].kind
    # to be more efficient, we don't use branching. We're incrementing
    # `depth` whenever we encounter the start of a sub-tree and decrement
    # it when an 'end' node is encountered
    depth += ord(kind == mnkEnd) - ord(kind in SubTreeNodes)

    dec i

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

func child*(tree: MirTree, n: NodePosition, index: int): lent MirNode =
  ## Similar to ``childIndex`` but returns the node directly
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

func operand*(tree: MirTree, op: Operation, opr: Natural): OpValue =
  ## Returns the `opr`th operand to the operation `op`. It is expected that
  ## the operation has at least `opr` + 1 operands
  let prev = NodePosition(op) - 1
  if tree[prev].kind == mnkEnd and tree[prev].start == mnkArgBlock:
    # start at the first sub-node of the argument block:
    var pos = parent(tree, prev) + 1
    # skip the sub-nodes until we've reached the `opr`-th arg node
    var i = 0
    while pos < prev:
      if tree[pos].kind in {mnkArg, mnkName}:
        if i == opr:
          # return the input, not the 'arg' node itself
          return OpValue getStart(tree, pos - 1)
        inc i

      pos = sibling(tree, pos)

    unreachable("argument out of bounds: " & $opr)
  else:
    # there exists only a single operand
    assert opr == 0
    result = OpValue getStart(tree, prev)

func operands*(tree: MirTree, op: Operation, slice: Slice[int],
               result: var openArray[OpValue]) =
  ## Collects the operands of `op` identified by `slice` to `result`
  assert slice.len == result.len
  let prev = NodePosition(op) - 1
  if tree[prev].kind == mnkEnd and tree[prev].start == mnkArgBlock:
    # start at the first sub-node of the argument block:
    var pos = parent(tree, prev) + 1
    # skip the sub-nodes until we've reached the `opr`-th arg node
    var i = 0
    while pos < prev:
      if tree[pos].kind in ArgumentNodes:
        if i >= slice.a:
          # return the input, not the 'arg' node itself
          result[i - slice.a] = OpValue getStart(tree, pos - 1)
        if i == slice.b:
          return

        inc i

      pos = sibling(tree, pos)

    unreachable("argument out of bounds: " & $slice)
  else:
    # there exists only a single operand
    assert slice.a == 0 and slice.b == 0
    result[0] = OpValue getStart(tree, prev)

func fetchArgs*(tree: MirTree, op: Operation, result: var openArray[NodePosition]) =
  ## Collects all operands of `op` to `result`. The length of `result` must
  ## match the number of operands
  let prev = NodePosition(op) - 1
  if tree[prev].kind == mnkEnd and tree[prev].start == mnkArgBlock:
    var pos = prev - 1
    # `pos` now points to the last argument sink inside the arg-block
    var i = result.high
    while tree[pos].kind != mnkArgBlock:
      if tree[pos].kind in ArgumentNodes:
        assert i >= 0, "not enough space for all argument nodes"
        # return the input, not the 'arg' node itself
        result[i] = pos
        dec i

      pos = previous(tree, pos)

    assert i == -1, "not enough arguments"

  else:
    # there exists only a single operand
    assert result.len == 1
    result[0] = prev

func numArgs*(tree: MirTree, op: Operation): int =
  ## Computes the number of arguments in the argument-block used as the input
  ## to `op`
  let prev = NodePosition(op) - 1
  if tree[prev].kind == mnkEnd and tree[prev].start == mnkArgBlock:
    var pos = prev - 1
    while tree[pos].kind != mnkArgBlock:
      if tree[pos].kind in ArgumentNodes:
        inc result

      pos = previous(tree, pos)

  else:
    unreachable("no arg-block is used")

func unaryOperand*(tree: MirTree, op: Operation): OpValue =
  # XXX: a 'def' node is not an operation
  assert tree[op].kind in SingleInputNodes + DefNodes
  result = OpValue getStart(tree, NodePosition(op) - 1)

func hasInput*(tree: MirTree, op: Operation): bool =
  # XXX: a 'def' node is not an operation
  assert tree[op].kind in DefNodes
  let node = tree[NodePosition(op)-1]
  case node.kind
  # exclude sub-tree nodes here so that a dynamic operation appearing as the
  # first child-node of, for example, an arg-block is not treated as having an
  # input
  of SourceNodes - SubTreeNodes: true
  of mnkEnd:      node.start in SourceNodes
  else:           false

iterator pairs*(tree: MirTree): (NodePosition, lent MirNode) =
  var i = 0
  let L = tree.len
  while i < L:
    yield (i.NodePosition, tree[i])
    inc i

iterator subNodes*(tree: MirTree, n: NodePosition): NodePosition =
  ## Iterates over and yields all direct child nodes of `n`
  let L = tree[n].len
  var r = n + 1
  for _ in 0..<L:
    yield r
    r = sibling(tree, r)

# XXX: ``lpairs`` is not at all related to the mid-end IR. The ``pairs``
#      iterator from the stdlib should be changed to use ``lent`` instead
iterator lpairs*[T](x: seq[T]): (int, lent T) =
  var i = 0
  let L = x.len
  while i < L:
    yield (i, x[i])
    inc i