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
    ## ID of a label, used to identify a block (``mnkBlock``).

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

    mnkLiteral ## literal data. Currently represented via a ``PNode``
    mnkType    ## a type literal

    mnkNone    ## the "nothing" value. Represents the absence of a value.
               ## To ease the back-to-``PNode`` translation, this node is
               ## currently allowed to have a non-nil type.

    mnkDef       ## defines an entity. The entity is only accessible past the
                 ## point of the definition and within the surrounding
                 ## ``Scope``. If the entity describes a run-time location,
                 ## the instruction may have a single input.
    mnkDefCursor ## starts the lifetime of a location that is non-owning. The
                 ## location may or may not contain a value and is not
                 ## responsible for destroying it. May have a single input.
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

    mnkPath        ## applies a projection to the input, but doesn't
                   ## materialize either a value or handle. Opens a sub-tree
                   ## that contains a list of ``PathX`` operations describing
                   ## the projection
    mnkPathNamed   ## named record field access (``a.b``)
    mnkPathPos     ## positional record field access (``a[0]``)
    # future direction: once a type IR that allows for faster positional field
    # lookup exists, ``PathNamed`` should be removed
    mnkPathArray   ## array access with run-time index value. This currently
                   ## includes access of dynamically-sized types, like ``seq``
                   ## and ``string``
    mnkPathVariant ## access a field inside a tagged union
                   ## XXX: this is likely only a temporary solution. Each
                   ##      record-case part of an object should be its own
                   ##      dedicated object type, which can then be addressed
                   ##      as a normal field
    mnkPathConv    ## handle conversion. Produces a new handle (same location,
                   ## different type) but no new value

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
    mnkDerefView ## ``derefView(x)``; dereferences a single-location view,
                 ## producing the lvalue named by it

    mnkStdConv    ## ``stdConv(x)``; a standard conversion.Produce a new value.
                  ## Also used for to-slice (``openArray``) conversions, in
                  ## which case the semantics are still fuzzy.
    mnkConv       ## ``conv(x)``; a conversion. Produces a new value.
    # future direction: the ``StdConv`` and ``Conv`` distinction is currently
    # still needed by the code generators, and also for to-``openArray``
    # conversion, but needs to eventually be removed
    mnkCast       ## ``cast(x)``; produces a new *instance* of the input value
                  ## with a different type

    mnkCall   ## ``call(p, ...)``; transfers control-flow (i.e. calls) the
              ## procedure that `p` evaluates to and passes the provided
              ## arguments
    mnkMagic  ## ``magic(...)``; a call to a magic procedure

    mnkRaise  ## if zero arguments are provided: re-raises the currently active
              ## exception
              ## if one argument is provided: consumes the argument (exception)
              ## and transfers control-flow to the closest applicable exception
              ## handler

    mnkMaterialize  ## reads the value from a location. Does not imply copying
    mnkMaterializeL ## computes and returns the *identity* (e.g., address) of
                    ## the location the input expression identifies

    # --- effect nodes
    mnkMutate     ## marks the value as being mutated (unspecific; in-out
                  ## parameters)
    mnkReassign   ## marks the location as being fully re-assigned, without the
                  ## previous value being observed (out parameter)
    mnkKill       ## marks the location as no longer storing a value after the
                  ## associated operation
    mnkInvalidate ## marks the location as being in an unknown state after the
                  ## associated operation
    # --- effect nodes end

    mnkConstr     ## ``constr(...)``; constructs a new compound value made up of
                  ## the input values. Whether the resulting value is owned
                  ## depends on whether one the context it's used in
    mnkObjConstr  ## ``objConstr(...)``; either heap-allocates and initializes
                  ## a new managed location, or constructs a new compound value
                  ## with named fields

    # --- argument nodes
    mnkArg    ## binds a value to a parameter.
    mnkName   ## binds an lvalue to a parameter. The operand must be the result
              ## of a ``mnkMaterializeL`` operation
    mnkConsume## binds a *full* value to a parameter
    # --- argument nodes end

    mnkDrop   ## drops the input value without doing anything else with it

    mnkField  ## syntax-only node that may only appear as a sub-node to
              ## ``mnkObjConstr``. Identifies the record field the
              ## corresponding argument is assigned to

    mnkRegion ## defines an anonymous sub-routine and immediately invokes it.
              ## Supports arguments. Executing the sub-routine must only have
              ## the effects described by the argument tags; control-flow
              ## effects (e.g., raising an exception) are disallowed. Right now,
              ## definitions inside a region are allowed, but this could change
              ## in the future. Within a region, the argument nodes to the
              ## region can be referenced as operands

    mnkStmtList ## groups statements together
    mnkScope  ## encloses statements in a scope, delimiting the livetimes
              ## of all locals defined therein. Can be nested.

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

    mnkAsm    ## corresponds to the high-level ``asm`` statement. Takes one or
              ## more arguments, but has no meaning itself at the MIR level
    mnkEmit   ## corresponds to the ``emit`` directive. In the context of the
              ## MIR, has the same behaviour as ``mnkAsm``

    mnkEnd    ## marks the physical end of a sub-tree. Has no semantic
              ## meaning -- it's only required to know where a sub-tree ends

    mnkPNode ## depending on the context, either a statement or something else.
             ## If it appears as a statement, it is expected to not have any
             ## obsersvable effects
             ## XXX: eventually, everything that currently requires
             ##      ``mnkPNode`` should be expressable directly in the IR

  GeneralEffect* = enum
    geMutateGlobal ## the operation mutates global state
    geRaises       ## the operation is a source of exceptional control-flow

const
  DefNodes* = {mnkDef, mnkDefCursor, mnkDefUnpack}
    ## Node kinds that represent definition statements (i.e. something that
    ## introduces a named entity).

  ArgumentNodes* = {mnkArg, mnkName, mnkConsume}
    ## Nodes that represent. An argument node must be followed by either
    ## another argument node or a node from the ``MultiInputNodes`` set.

  ArgumentListNodes* = {mnkCall, mnkMagic, mnkRegion, mnkConstr, mnkObjConstr,
                        mnkRaise, mnkAsgn, mnkFastAsgn, mnkInit} + DefNodes
    ## Nodes that may be preceded by argument nodes.

  EffectNodes* = {mnkMutate, mnkReassign, mnkKill, mnkInvalidate}
    ## Syntax-nodes that describe the effect executing an operation/instruction
    ## has on an operand. May only referenced by ``ArgumentNodes``.
    ## Representation-wise, an effect-node must come immediately before the
    ## argument list from which it is referenced.

  PathNodes* = {mnkPathNamed, mnkPathPos, mnkPathArray, mnkPathVariant,
                mnkPathConv}
    ## Nodes that may only appear within a ``mnkPath`` sub-tree.

  SymbolLike* = {mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal}
    ## Nodes for which the `sym` field is available.

  OperandNodes* = {mnkAddr, mnkDeref, mnkView, mnkDerefView, mnkConv,
                   mnkMaterialize, mnkMaterializeL, mnkStdConv, mnkCast,
                   mnkPath, mnkPathArray, mnkDrop, mnkIf, mnkCase} +
                  ArgumentNodes + EffectNodes
    ## Nodes for which the ``operand`` field is available.

type
  MirNode* = object
    typ*: PType ## non-nil for all ``ValueNodes``

    case kind*: MirNodeKind
    of SymbolLike:
      sym*: PSym
    of mnkField, mnkPathNamed, mnkPathVariant:
      field*: PSym
    of mnkLiteral:
      lit*: PNode
    of mnkTemp:
      temp*: TempId
    of mnkPathPos:
      position*: uint32 ## the 0-based position of the field
    of OperandNodes:
      operand*: OpValue
    of mnkCall:
      effects*: set[GeneralEffect]
    of mnkMagic:
      magic*: TMagic
      # future direction: magics that can raise an exception should use a
      # dedicated instruction/node kind
    of mnkBlock, mnkBreak:
      label*: LabelId ## for a block, the label that identifies the block;
                      ## for a break, the label of the block to break out of
    of mnkEnd:
      start*: MirNodeKind ## the kind of the corresponding start node
    of mnkPNode:
      node*: PNode
    of mnkTry, mnkBranch, mnkExcept, mnkObjConstr:
      len*: int ## for ``mnkTry``: the number of ``mnkFinally`` and
                ## ``mnkExcept`` nodes
                ## for ``mnkBranch``: the number of branch labels
                ## for ``mnkExcept``: the number of handlers
                ## for ``mnkObjConstr``: the number of ``mnkField`` sub-nodes
    else:
      discard

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

  SubTreeNodes* = {mnkObjConstr, mnkRegion, mnkStmtList, mnkScope,
                   mnkIf..mnkBlock, mnkBranch, mnkPath} + DefNodes
    ## Nodes that mark the start of a sub-tree. They're always matched with a
    ## corrsponding ``mnkEnd`` node

  AtomNodes* = AllNodeKinds - SubTreeNodes
    ## Nodes that aren't sub-trees

  ValueNodes* = {mnkProc..mnkNone, mnkMagic, mnkCall, mnkAddr, mnkDeref,
                 mnkView, mnkDerefView, mnkStdConv, mnkConv, mnkCast, mnkPath,
                 mnkObjConstr, mnkConstr, mnkMaterialize, mnkMaterializeL} +
                ArgumentNodes + EffectNodes
    ## Nodes that may be referenced via the ``operand`` field.

  ImplicitMaterialize* = {mnkProc, mnkLiteral, mnkType, mnkAddr, mnkView,
                          mnkConv, mnkStdConv, mnkCast, mnkMagic, mnkCall,
                          mnkObjConstr, mnkConstr}
    ## Operations that implicitly materialize a *value*.

  StmtNodes* = {mnkScope, mnkRepeat, mnkTry, mnkBlock, mnkBreak, mnkReturn,
                mnkRaise, mnkDrop, mnkPNode, mnkIf, mnkCase} + DefNodes
    ## Nodes that act as statements syntax-wise

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

template operand*(tree: MirTree, op: NodePosition|Operation|OpValue): OpValue =
  ## Returns the operand for the single-input operation `op`. Prefer this
  ## routine over directly accessing the `operand` field of a node.
  tree[op].operand

func operand*(tree: MirTree, op: Operation, opr: Natural): OpValue =
  ## Returns the `opr`th operand to the operation `op`. It is expected that
  ## the operation has at least `opr` + 1 operands.
  var i = NodePosition(op) - 1
  while tree[i].kind in ArgumentNodes:
    dec i

  assert i + 1 + opr < NodePosition(op), "operand out of bounds"
  result = tree.operand(i + 1 + opr)

func numArgs*(tree: MirTree, op: Operation): int =
  ## Computes the number of argument nodes belonging to the `op` node.
  var i = NodePosition(op) - 1
  while tree[i].kind in ArgumentNodes:
    inc result
    dec i

proc user*(body: MirTree, val: OpValue): NodePosition =
  ## Returns the node that uses `val` as its operand.
  assert body[val].kind in ValueNodes - ArgumentNodes
  var p = int(val) + 1
  # all nodes representing an operation yielding a value *must* be referenced
  # exactly once, meaning we only need to find the first occurrence
  while p < body.len and
        not (body[p].kind in OperandNodes and body[p].operand == val):
    inc p

  assert p < body.len, "missing user"
  result = NodePosition p

proc consumer*(body: MirTree, arg: NodePosition): NodePosition =
  ## Given the an `arg`ument node, returns the node of the operation that the
  ## argument node belongs to.
  assert body[arg].kind in ArgumentNodes
  var p = arg
  while body[p].kind in ArgumentNodes:
    inc p

  result = p

func hasInput*(tree: MirTree, op: Operation): bool =
  ## Returns whether the operation `op` has at least one operand.
  assert tree[op].kind in ArgumentListNodes
  tree[NodePosition(op) - 1].kind in ArgumentNodes

iterator pairs*(tree: MirTree): (NodePosition, lent MirNode) =
  var i = 0
  let L = tree.len
  while i < L:
    yield (i.NodePosition, tree[i])
    inc i

iterator subNodes*(tree: MirTree, n: NodePosition): NodePosition =
  ## Iterates over and yields all direct child nodes of `n`
  var n = n + 1
  while tree[n].kind != mnkEnd:
    yield n
    n = sibling(tree, n)

# XXX: ``lpairs`` is not at all related to the mid-end IR. The ``pairs``
#      iterator from the stdlib should be changed to use ``lent`` instead
iterator lpairs*[T](x: seq[T]): (int, lent T) =
  var i = 0
  let L = x.len
  while i < L:
    yield (i, x[i])
    inc i