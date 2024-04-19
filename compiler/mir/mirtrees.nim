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
  LocalId* = distinct uint32
    ## Identifies a local inside a code fragment
  GlobalId* = distinct uint32
    ## Identifies a global across all MIR code
  ConstId* = distinct uint32
    ## Identifies a constant across all MIR code. This includes both
    ## user-defined constants as well as anonymous constants
  ParamId {.used.} = distinct uint32
    ## Identifies a parameter of the code fragment
  FieldId {.used.} = distinct uint32
    ## Identifies the field of a record type
  ProcedureId* = distinct uint32
    ## Identifies a procedure
  NumberId* = distinct uint32
    ## Uniquely identifies some numerical value (float, signed int,
    ## unsigned int). Two values with the same bit pattern have the same ID
  StringId* = distinct uint32
    ## Uniquely identifies a string value. Two strings sharing the same
    ## content map to the same ID
  AstId* = distinct uint32
    ## Identifies an AST fragment stored in the MIR environment.
  DataId* = distinct uint32
    ## Identifies a complete constant expression

  TypeId* = distinct uint32
    ## Identifies a type

  SourceId* = distinct range[0'u32 .. high(uint32)-1]
    ## The ID of a source-mapping that's stored separately from the MIR nodes.

# make ``SourceId`` available for use with ``OptIndex``:
template indexLike*(_: typedesc[SourceId]) = discard

type
  LabelId* = distinct uint32
    ## ID of a label, used to identify a block (``mnkBlock``).

  MirNodeKind* = enum
    ## Users of ``MirNodeKind`` should not depend on the absolute or relative
    ## order between the enum values
    # when adding new enum values, make sure to adjust the sets below
    mnkNone

    # entity names:
    mnkProc   ## procedure reference; only allowed in callee slots
    mnkProcVal## procedural value
    mnkConst  ## named constant
    mnkGlobal ## global location
    mnkParam  ## parameter
    mnkLocal  ## local location
    mnkTemp   ## like ``mnkLocal``, but the local was introduced by the
              ## compiler during the MIR phase
    mnkAlias  ## local run-time handle. This is essentially a ``var T`` or
              ## ``lent T`` local

    mnkField  ## declarative node only allowed in special contexts

    mnkNilLit  ## nil literal
    mnkIntLit  ## reference to signed integer literal
    mnkUIntLit ## reference to unsigend integer literal
    mnkFloatLit## reference to float literal
    mnkStrLit  ## reference to a literal string
    mnkAstLit  ## reference to AST fragment
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
    mnkBind      ## introduces an alias that may be used for read/write
                 ## access, but not for direct assignments. The source
                 ## expression must not be empty
    mnkBindMut   ## introduces an alias that may be used for read/write access
                 ## and assignments. The source expression must not be empty

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
    mnkMutView   ## create a safe mutable view from an lvalue
    mnkDerefView ## dereference a first-class safe alias

    mnkStdConv    ## a standard conversion. Produce a new value.
    mnkConv       ## ``conv(x)``; a conversion. Produces a new value.
    # future direction: replace both conversion operators with ``NumberConv``.
    # String-to-cstring conversion, and vice versa, should use magics, pointer
    # conversion should use ``mnkCast``
    mnkCast       ## cast the representation of a value into a different type
    mnkToSlice    ## has to variants:
                  ## * the 1 argument variant creates an openArray from the
                  ##   full sequence specified as the operand
                  ## * the 3 argument variant creates an openArray from the
                  ##   sub-sequence specified by the sequence and lower and
                  ##   upper bound
    # XXX: consider using a separate operator for the slice-from-sub-sequence
    #      operation
    mnkToMutSlice ## version of ``mnkToSlice`` for creating a mutable slice

    mnkCall   ## invoke a procedure and pass along the provided arguments.
              ## Used for both static and dynamic calls
    mnkCheckedCall  ## invoke a magic procedure and pass along the provided arguments

    # unary arithmetic operations:
    mnkNeg ## signed integer and float negation (for ints, overflow is UB)
    # binary arithmetic operations:
    mnkAdd ## signed integer and float addition (for ints, overflow is UB)
    mnkSub ## signed integer and float subtraction (for ints, overflow is UB)
    mnkMul ## signed integer and float multiplication (for ints, overflow is
           ##  UB)
    mnkDiv ## signed integer and float division (for ints, division by zero is
           ## UB)
    mnkModI ## compute the remainder of an integer division (division by zero
            ## is UB)
    # future direction: the arithmetic operations should also apply to
    # unsigned integers

    mnkRaise  ## if the operand is an ``mnkNone`` node, reraises the
              ## currently active exception. Otherwise, set the operand value
              ## as the active exception (via a move). Control-flow is
              ## transfered to the closest exception handler. If none exists,
              ## the program terminates

    mnkTag    ## must only appear as the immediate subnode to a ``mnkName``
              ## tree. Describes what kind of mutation is applied to the
              ## lvalue within the called procedure

    mnkSetConstr  ## constructor for set values
    mnkRange      ## range constructor. May only appear in set constructions
                  ## and as a branch label
    mnkArrayConstr## constructor for array values
    mnkSeqConstr  ## constructor for seq values
    mnkTupleConstr## constructor for tuple values
    mnkClosureConstr## constructor for closure values
    mnkObjConstr  ## constructor for object values
    mnkRefConstr  ## allocates a new managed heap cell and initializes it

    mnkCopy   ## denotes the assignment as copying the source value
    mnkMove   ## denotes the assignment as moving the value. This does
              ## not imply a phyiscal change to the source location
    mnkSink   ## collapses into one of the following:
              ## - a copy (`mnkCopy`)
              ## - a non-destructive move (`mnkMove`)
              ## - a destructive move
              ##
              ## Collapsing ``mnkSink`` is the responsibility of the move
              ## analyzer.

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

    mnkDestroy## destroys the value stored in the given location, leaving the
              ## location in an undefined state

    mnkAsm    ## embeds backend-dependent code directly into the output
    mnkEmit   ## embeds backend-dependent code directly into the output

    mnkEnd    ## marks the end of a sub-tree. Has no behaviour associated with
              ## it -- it's only required to know where a sub-tree ends
    # future direction: replace the End node with storing the number of sub-
    # nodes of a sub-tree on the node itself. This will require significant
    # structural changes, as not all node kinds are able to use the length
    # field at the moment

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

  MirNode* = object
    typ*: TypeId ## valid for all expression, including all calls
    info*: SourceId
      ## non-critical meta-data associated with the node (e.g., origin
      ## information)
    case kind*: MirNodeKind
    of mnkProc, mnkProcVal:
      prc*: ProcedureId
    of mnkGlobal:
      global*: GlobalId
    of mnkConst:
      cnst*: ConstId
    of mnkParam, mnkLocal, mnkTemp, mnkAlias:
      local*: LocalId
    of mnkField, mnkPathNamed, mnkPathVariant:
      field*: int32
        ## field position
    of mnkIntLit, mnkUIntLit, mnkFloatLit:
      number*: NumberId
    of mnkStrLit:
      strVal*: StringId
    of mnkAstLit:
      ast*: AstId
    of mnkPathPos:
      position*: uint32 ## the 0-based position of the field
    of mnkCall, mnkCheckedCall:
      effects*: set[GeneralEffect]
    of mnkMagic:
      magic*: TMagic
    of mnkBlock, mnkBreak:
      label*: LabelId ## for a block, the label that identifies the block;
                      ## for a break, the label of the block to break out of
    of mnkEnd:
      start*: MirNodeKind ## the kind of the corresponding start node
    of mnkTag:
      effect*: EffectKind ## the effect that happens when the operator the
                          ## tagged value is passed to is executed
    else:
      len*: uint32

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

  AtomNodes* = {mnkNone..mnkType, mnkMagic, mnkBreak, mnkReturn}
    ## Nodes that don't support sub nodes.

  SubTreeNodes* = AllNodeKinds - AtomNodes - {mnkEnd}
    ## Nodes that start a sub-tree. They're always matched with an ``mnkEnd``
    ## node.

  SingleOperandNodes* = {mnkPathNamed, mnkPathPos, mnkPathVariant, mnkPathConv,
                         mnkAddr, mnkDeref, mnkView, mnkDerefView, mnkStdConv,
                         mnkConv, mnkCast, mnkRaise, mnkTag, mnkArg,
                         mnkName, mnkConsume, mnkVoid, mnkCopy, mnkMove,
                         mnkSink, mnkDestroy, mnkMutView, mnkToMutSlice}
    ## Nodes that start sub-trees but that always have a single sub node.

  ArgumentNodes* = {mnkArg, mnkName, mnkConsume}
    ## Nodes only allowed in argument contexts.

  ModifierNodes* = {mnkCopy, mnkMove, mnkSink}
    ## Assignment modifiers. Nodes that can only appear directly in the source
    ## slot of assignments.

  LiteralDataNodes* = {mnkNilLit, mnkIntLit, mnkUIntLit, mnkFloatLit,
                       mnkStrLit, mnkAstLit}

  ConstrTreeNodes* = {mnkSetConstr, mnkRange, mnkArrayConstr, mnkSeqConstr,
                      mnkTupleConstr, mnkClosureConstr, mnkObjConstr,
                      mnkRefConstr, mnkProcVal, mnkArg, mnkField,
                      mnkEnd} + LiteralDataNodes
    ## Nodes that can appear in the MIR subset used for constant expressions.

  # --- semantics-focused sets:

  Atoms* = {mnkNone .. mnkType} - {mnkField, mnkProc}
    ## Nodes that may be appear in atom-expecting slots.

  StmtNodes* = {mnkScope, mnkStmtList, mnkIf, mnkCase, mnkRepeat, mnkTry,
                mnkBlock, mnkBreak, mnkReturn, mnkRaise, mnkInit,
                mnkAsgn, mnkSwitch, mnkVoid, mnkRaise, mnkDestroy, mnkEmit,
                mnkAsm} + DefNodes

  UnaryOps*  = {mnkNeg}
    ## All unary operators
  BinaryOps* = {mnkAdd, mnkSub, mnkMul, mnkDiv, mnkModI}
    ## All binary operators

  LvalueExprKinds* = {mnkPathPos, mnkPathNamed, mnkPathArray, mnkPathVariant,
                      mnkPathConv, mnkDeref, mnkDerefView, mnkTemp, mnkAlias,
                      mnkLocal, mnkParam, mnkConst, mnkGlobal}
  RvalueExprKinds* = {mnkType, mnkProcVal, mnkConv, mnkStdConv, mnkCast,
                      mnkAddr, mnkView, mnkMutView, mnkToSlice,
                      mnkToMutSlice} + UnaryOps + BinaryOps + LiteralDataNodes
  ExprKinds* =       {mnkCall, mnkCheckedCall, mnkSetConstr, mnkArrayConstr,
                      mnkSeqConstr, mnkTupleConstr, mnkClosureConstr,
                      mnkObjConstr, mnkRefConstr} + LvalueExprKinds +
                     RvalueExprKinds + ModifierNodes

  CallKinds* = {mnkCall, mnkCheckedCall}

func `==`*(a, b: SourceId): bool {.borrow.}
func `==`*(a, b: LocalId): bool {.borrow.}
func `==`*(a, b: LabelId): bool {.borrow.}
func `==`*(a, b: ConstId): bool {.borrow.}
func `==`*(a, b: GlobalId): bool {.borrow.}
func `==`*(a, b: ProcedureId): bool {.borrow.}
func `==`*(a, b: DataId): bool {.borrow.}
func `==`*(a, b: NumberId): bool {.borrow.}
func `==`*(a, b: StringId): bool {.borrow.}
func `==`*(a, b: AstId): bool {.borrow.}
func `==`*(a, b: TypeId): bool {.borrow.}

func isAnon*(id: ConstId): bool =
  ## Returns whether `id` represents an anonymous constant.
  (uint32(id) and (1'u32 shl 31)) != 0

func extract*(id: ConstId): DataId =
  ## Extracts the ``DataId`` from `id`.
  DataId(uint32(id) and not(1'u32 shl 31))

func toConstId*(id: DataId): ConstId =
  ## Creates the ID for an anonymous constant with `id` as the content.
  ConstId((1'u32 shl 31) or uint32(id))

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
  assert tree[n].kind in CallKinds
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
  assert tree[n].kind in CallKinds
  var i = tree.sibling(n + 1) # skip the callee
  while tree[i].kind != mnkEnd:
    yield (ArgKinds(tree[i].kind), tree.operand(i))
    i = tree.sibling(i)

func findDef*(tree: MirTree, n: NodePosition): NodePosition =
  ## Finds and returns the first definition for the name of the temporary
  ## at node `n`. No control-flow analysis is performed.
  assert tree[n].kind in {mnkTemp, mnkAlias}
  let expected = tree[n].local
  # first, unwind until the closest statement
  result = n
  while tree[result].kind notin StmtNodes:
    result = tree.parent(result)

  # then search for the definition statement
  while result > NodePosition 0:
    if tree[result].kind in DefNodes:
      let name = tree.operand(result, 0)
      if tree[name].kind in {mnkTemp, mnkAlias} and
         tree[name].local == expected:
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