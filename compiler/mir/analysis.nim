## This module implements various data-flow related analysis for MIR code.
## They're based on the ``mirexec`` traversal algorithms and require a
## ``Values`` dictionary and a ``ControlFlowGraph`` object, both
## corresponding to the code fragment (i.e. ``MirTree``) that is analysed.
##
## A ``Values`` dictionary stores information about the result of operations,
## namely, whether the value is owned and, for lvalues, the root. It also
## stores the lvalue effects of operations. An instance of the dictionary is
## created and initialized via the ``computeValuesAndEffects`` procedure.
##
## Each location that is not allocated via ``new`` or ``alloc`` is owned by a
## single handle (the name of a local, global, etc.), but can be aliased
## through both pointers and views. Once the owning handle goes out of scope,
## the lifetime of the corresponding locatins ends, irrespective of whether an
## unsafe alias (pointer) of it still exists.
##
## Instead of assigning a unique ID to each value/lvalue, they're identified
## via the operation sequence that produces them (stored as a ``NodePosition``
## tuple). While the comparision is not as efficient as an equality test
## between two integers, it is still relatively cheap, and, in addition, also
## allows for part-of analysis without requiring complex algorithms or
## data-structures.
##
## Do note that it is assumed that there only exists one handle for each
## location -- pointers or views are not tracked. Reads or writes through
## aliases are not detected.
##
## ..note:: implementing this is possible. A second step after
##          ``computeValuesAndEffects`` could perform an abstract execution of
##          the MIR code to produce a conservative set of possible handles for
##          each pointer-like dereferencing operation. The analysis routines
##          would then compare the analysed handle with each set element,
##          optionally taking types into account in order to reduce the number
##          of comparisons (i.e. by not comparing handles of differing type)
##
## When a "before" or "after" relationship is mentioned in the context of
## operations, it doesn't refer to the relative memory location of the
## nodes representing the operations, but rather to the operations'
## control-flow relationship. If control-flow visits A first and then B, A is
## said to come before B and B to come after A. Not all operations are
## connected to each other through control-flow however, in which case the
## aforementioned relationship doesn't exist.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_types,
    ast_query
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/sem/[
    aliasanalysis,
    mirexec,
    typeallowed
  ],
  compiler/utils/[
    containers
  ],
  experimental/[
    dod_helpers
  ]

import std/packedsets

type
  Owned* {.pure.} = enum
    no
    yes
    weak ## values derived from compound values (e.g. ``object``, ``tuple``,
         ## etc.) that are weakly owned decay to no ownership. Rvalues are
         ## weakly owned -- they can be consumed directly, but sub-values of
         ## them can't
    unknown

  ValueInfo = object
    root: opt(NodeInstance) ## the root of the value (or 'none' if the
                            ## ``ValueInfo`` is invalid)
    owns: Owned             ## whether the handle owns its value

  Values* = object
    ## Stores information about the produced values plus the lvalue effects of
    ## operations
    values: OrdinalSeq[OpValue, ValueInfo]
    # XXX: `values` currently stores an entry for each *node*. Not every node
    #      represents an operation and we're also not interested in the value
    #      of every operation, only of those that appear in specific contexts.
    #      A ``Table`` could be used, but that would make lookup less
    #      efficient (although less used memory could also mean better memory
    #      locality)

  AliveState = enum
    unchanged
    dead
    alive

  ComputeAliveProc[T] =
    proc(tree: MirTree, values: Values, loc: T, op: Opcode,
         n: OpValue): AliveState {.nimcall, noSideEffect.}

func skipConversions(tree: MirTree, val: OpValue): OpValue =
  ## Returns the producing operation after skipping handle-only
  ## conversions.
  var p = NodePosition(val)
  while tree[p].kind == mnkPathConv:
    p = previous(tree, p)

  result = OpValue(p)

template getRoot*(v: Values, val: OpValue): OpValue =
  OpValue v.values[val].root[]

template owned*(v: Values, val: OpValue): Owned =
  v.values[val].owns

func setOwned*(v: var Values, val: OpValue, owns: Owned) {.inline.} =
  v.values[val].owns = owns

func toLvalue*(v: Values, val: OpValue): LvalueExpr {.inline.} =
  (NodePosition v.values[val].root[],
   NodePosition val)

func decayed(x: ValueInfo): ValueInfo {.inline.} =
  ## Turns 'weak' ownership into 'no' ownership
  result = x
  if result.owns == Owned.weak:
    result.owns = Owned.no

func computeValuesAndEffects*(body: MirTree): Values =
  ## Creates a ``Values`` dictionary with all operation effects collected and
  ## (static) value roots computed. Value ownership is already computed where it
  ## is possible to do so by just taking the static operation sequences into
  ## account (i.e. no control- or data-flow analysis is performed)
  result.values.newSeq(body.len)

  template inherit(i, source: NodePosition) =
    result.values[OpValue i] = result.values[OpValue source]

  template inheritDecay(i, source: NodePosition) =
    result.values[OpValue i] = decayed result.values[OpValue source]

  # we're doing three things here:
  # 1. propagate the value root
  # 2. propagate ownership status
  #
  # This is done in a single forward iteration over all nodes in the code
  # fragment -- nodes that don't represent operations are ignored.
  # XXX: this is all going to change.

  for i, n in body.pairs:
    template start(owned: Owned) =
      result.values[OpValue i] =
        ValueInfo(root: someOpt(NodeInstance i), owns: owned)

    case n.kind
    of mnkOpParam:
      # XXX: the body of regions are not yet analysed (they're skipped over).
      #      Once they are, the ownership status of an `opParam` depends on
      #      the corresponding argument. Values coming from 'name' and 'arg'
      #      arguments are not owned, but for those coming from 'consume'
      #      arguments, it depends (i.e. ``unknown``)
      start: Owned.no
    of mnkDeref, mnkDerefView, mnkConst, mnkType, mnkNone, mnkCast:
      start: Owned.no
    of mnkLiteral, mnkProc:
      # literals are always owned (each instance can be mutated without
      # impacting the others). Because of their copy-on-write mechanism,
      # this also includes string literals
      start: Owned.yes
    of mnkTemp, mnkLocal, mnkGlobal, mnkParam:
      # more context is required to know whether the value is owned
      start: Owned.unknown
    of mnkConstr:
      # the result of a ``seq`` construction via ``constr`` is essentially a
      # non-owning view into constant data
      start:
        if n.typ.skipTypes(abstractInst).kind == tySequence: Owned.no
        else: Owned.weak
    of mnkObjConstr:
      start:
        if n.typ.skipTypes(abstractInst).kind == tyRef: Owned.yes
        else: Owned.weak
      # ``mnkObjConstr`` is a sub-tree, so in order to keep the inheriting
      # logic simple, the 'end' node for the sub-tree uses the same
      # ``ValueInfo`` as the start node
      inherit(findEnd(body, i), i)
    of mnkCall, mnkMagic:
      # we currently can't reason about which location(s) views alias, so
      # we always treat values accessed through them as not owned
      start:
        if directViewType(n.typ) != noView: Owned.no
        else: Owned.weak
    of mnkStdConv, mnkConv:
      # non-lvalue conversions produces a new unique value, meaning that
      # the result is always owned
      start: Owned.yes
    of mnkAddr, mnkView, mnkPathPos, mnkPathVariant:
      inheritDecay(i, NodePosition body.operand(i))
    of mnkPathArray:
      # inherit from the first operand (i.e. the array-like value)
      inheritDecay(i, NodePosition operand(body, Operation(i), 0))
    of mnkPathNamed:
      inheritDecay(i, NodePosition body.operand(i))
      if sfCursor in n.field.flags:
        # any lvalue derived from a cursor location is non-owning
        result.values[OpValue i].owns = Owned.no
    of mnkPathConv:
      inherit(i, NodePosition body.operand(i))

    of AllNodeKinds - InOutNodes - InputNodes + {mnkTag, mnkArgBlock}:
      discard "leave uninitialized"

func isAlive*(tree: MirTree, cfg: ControlFlowGraph, v: Values,
             span: Slice[NodePosition], loc: LvalueExpr,
             pos: NodePosition): bool =
  ## Computes if the location named by `loc` does contain a value at `pos`
  ## (i.e. is alive). The performed data-flow analysis only considers code
  ## inside `span`
  template toLvalue(val: OpValue): LvalueExpr =
    toLvalue(v, val)

  template overlaps(val: OpValue): bool =
    overlaps(tree, loc, toLvalue val) != no

  # this is a reverse data-flow problem. We follow all control-flow paths from
  # `pos` backwards until either there's no path left to follow or one of them
  # reaches a potential mutation of `loc`, in which case the underlying location
  # is considered to be alive. A path is not followed further if it reaches an
  # operation that "kills" the `loc` (removes its value, e.g. by moving it
  # somewhere else)

  var exit = false
  for op, n in traverseReverse(cfg, span, pos, exit):
    case op
    of opDef, opMutate:
      if overlaps(n):
        # consider ``a.b = x`` (A) and ``a.b.c.d.e = y`` (B). If the
        # analysed l-value expression is ``a.b.c`` then both A and B mutate
        # it (either fully or partially). If traversal reaches what's
        # possibly a mutation of the analysed location, it means that the
        # location needs to be treated as being alive at `pos`, so we can
        # return already
        return true

    of opKill:
      if isPartOf(tree, loc, toLvalue n) == yes:
        exit = true

    of opInvalidate:
      discard

    of opMutateGlobal:
      if tree[loc.root].kind == mnkGlobal:
        # an unspecified global is mutated and we're analysing a location
        # derived from a global -> assume the analysed global is mutated
        return true

    of opConsume:
      if v.owned(n) == Owned.yes:
        if isPartOf(tree, loc, toLvalue n) == yes:
          # the location's value is consumed and it becomes empty. No operation
          # coming before the current one can change that, so we can stop
          # traversing the current path
          exit = true

        # partially consuming the location does *not* change the alive state

    else:
      discard "not relevant"

  # no mutation is directly connected to `pos`. The location is not alive
  result = false

func isLastRead*(tree: MirTree, cfg: ControlFlowGraph, values: Values,
                 span: Slice[NodePosition], loc: LvalueExpr, pos: NodePosition
                ): bool =
  ## Performs data-flow analysis to compute whether the value in `loc`
  ## is observed on any paths starting from and including `pos`. If it is
  ## observed, 'false' is returned, 'true' otherwise. Only operations within
  ## `span` are considered.
  ## It's important to note that this analysis does not test whether the
  ## underlying *location* is accessed, but rather the *value* it stores. If a
  ## new value is assigned to the underlying location which is then accessed
  ## after, it won't cause the analysis to return false
  template toLvalue(val: OpValue): LvalueExpr =
    toLvalue(values, val)

  var state: TraverseState
  for op, n in traverse(cfg, span, pos, state):
    case op
    of opDef:
      let cmp = compareLvalues(tree, loc, toLvalue n)
      if isAPartOfB(cmp) == yes:
        # the location is reassigned -> all operations coming after will
        # observe a different value
        state.exit = true
      elif isBPartOfA(cmp) != no:
        # the location is partially written to -> the relevant values is
        # observed
        return false

    of opMutate:
      if overlaps(tree, loc, toLvalue n) != no:
        # the location is partially written to
        return false

    of opKill:
      let cmp = compareLvalues(tree, loc, toLvalue n)
      if isAPartOfB(cmp) == yes:
        # the location is definitely killed, it no longer stores the value
        # we're interested in
        state.exit = true

    of opInvalidate:
      discard

    of opMutateGlobal:
      if tree[loc.root].kind == mnkGlobal:
        # an unspecified global is mutated and we're analysing a location
        # derived from a global -> assume that it's a read/use
        return false

    of opUse, opConsume:
      if overlaps(tree, loc, toLvalue n) != no:
        # value is observed -> not the last read
        return false

    else:
      discard

  # no further read of the value is connected to `pos`
  result = true

func isLastWrite*(tree: MirTree, cfg: ControlFlowGraph, values: Values,
                  span: Slice[NodePosition], loc: LvalueExpr, pos: NodePosition
                 ): tuple[result, exits, escapes: bool] =
  ## Computes whether the location `loc` is reassigned or modified on any paths
  ## starting from and including `pos`, returning 'false' if yes and 'true' if
  ## not. In other words, computes whether a reassignment or mutation that has
  ## a control-flow dependency on `pos` and is located inside `span` observes
  ## the current value.
  ##
  ## In addition, whether the `pos` is connected to a structured or
  ## unstructured exit of `span` is also returned
  template toLvalue(val: OpValue): LvalueExpr =
    toLvalue(values, val)

  var state: TraverseState
  for op, n in traverse(cfg, span, pos, state):
    case op
    of opDef, opMutate, opInvalidate:
      # note: since we don't know what happens to the location when it is
      # invalidated, the ``opInvalidate`` is also included here
      if overlaps(tree, loc, toLvalue n) != no:
        return (false, false, false)

    of opKill:
      let cmp = compareLvalues(tree, loc, toLvalue n)
      if isAPartOfB(cmp) == yes:
        state.exit = true

      # partially killing the analysed location is not considered to be a
      # write

    of opMutateGlobal:
      if tree[loc.root].kind == mnkGlobal:
        # an unspecified global is mutated and we're analysing a location
        # derived from a global
        return (false, false, false)

    else:
      discard

  result = (true, state.exit, state.escapes)

func computeAliveOp*[T: PSym | TempId](
  tree: MirTree, values: Values, loc: T, op: Opcode, n: OpValue): AliveState =
  ## Computes the state of `loc` at the *end* of the given operation. The
  ## operands are expected to *not* alias with each other. The analysis
  ## result will be wrong if they do

  func isAnalysedLoc[T](n: MirNode, loc: T): bool =
    when T is TempId:
      n.kind == mnkTemp and n.temp == loc
    elif T is PSym:
      n.kind in {mnkLocal, mnkParam, mnkGlobal} and n.sym.id == loc.id
    else:
      {.error.}

  template isRootOf(val: OpValue): bool =
    isAnalysedLoc(tree[values.getRoot(val)], loc)

  template sameLocation(val: OpValue): bool =
    isAnalysedLoc(tree[skipConversions(tree, val)], loc)

  case op
  of opMutate, opDef:
    if isRootOf(n):
      # the analysed location or one derived from it is mutated
      return alive

  of opKill:
    if sameLocation(n):
      # the location is killed
      return dead

  of opInvalidate:
    discard "cannot be reasoned about here"

  of opMutateGlobal:
    when T is PSym:
      # XXX: testing the symbol's flags is okay for now, but a different
      #      approach has to be used once moving away from storing ``PSym``s
      #      in ``MirNodes``
      if sfGlobal in loc.flags:
        # the operation mutates global state and we're analysing a global
        result = alive

  of opConsume:
    if values.owned(n) == Owned.yes and sameLocation(n):
      # the location's value is consumed
      result = dead

  else:
    discard

func computeAlive*[T](tree: MirTree, cfg: ControlFlowGraph, values: Values,
                      span: Slice[NodePosition], loc: T,
                      op: static ComputeAliveProc[T]
                     ): tuple[alive, escapes: bool] =
  ## Computes whether the location is alive when `span` is exited via either
  ## structured or unstructured control-flow. A location is considered alive
  ## if it contains a value

  # assigning to or mutating the analysed location makes it become alive,
  # because it then stores a value. Consuming its value or using ``wasMoved``
  # on it "kills" it (it no longer contains a value)

  var exit = false
  for opc, n in traverseFromExits(cfg, span, exit):
    case op(tree, values, loc, opc, n)
    of dead:
      exit = true
    of alive:
      # the location is definitely alive when leaving the span via
      # unstructured control-flow
      return (true, true)
    of unchanged:
      discard

  # check if the location is alive at the structured exit of the span
  for opc, n in traverseReverse(cfg, span, span.b + 1, exit):
    case op(tree, values, loc, opc, n)
    of dead:
      exit = true
    of alive:
      # the location is definitely alive when leaving the span via
      # structured control-flow
      return (true, false)
    of unchanged:
      discard

  result = (false, false)

proc doesGlobalEscape*(tree: MirTree, scope: Slice[NodePosition],
                       start: NodePosition, s: PSym): bool =
  ## Computes if the global `s` potentially "escapes". A global escapes if it
  ## is not declared at module scope and is used inside a procedure that is
  ## then called outside the analysed global's scope. Example:
  ##
  ## .. code-block:: nim
  ##
  ##   # a.nim
  ##   var p: proc()
  ##   block:
  ##     var x = Resource(...)
  ##     proc prc() =
  ##       echo x
  ##
  ##     p = prc # `x` "escapes" here
  ##     # uncommenting the below would make `x` not escape
  ##     # p = nil
  ##
  ##   p()
  ##
  # XXX: to implement this, one has to first collect side-effectful procedures
  #      defined inside either the same or nested scopes and their
  #      address taken (``sfSideEffect`` and ``sfAddrTaken``). The
  #      ``sfSideEffect`` flag only indicates whether a procedure accesses
  #      global state, not if the global in question (`s`) is modified /
  #      observed -- recursively applying the analysis to the procedures'
  #      bodies would be necessary for that.
  #      Then look for all assignments with one of the collect procedures as
  #      the source operand and perform an analysis similar to the one
  #      performed by ``isLastRead`` to check if the destination still
  #      contains the procedural value at the of the scope. If it does, the
  #      global escapes
  # XXX: as an escaping global is a semantic error, it would make more sense
  #      to detect and report it during semantic analysis instead -- the
  #      required DFA is not as simple there as it is with the MIR however
  result = false

func isConsumed*(tree: MirTree, val: OpValue): bool =
  ## Computes if `val` is definitely consumed. This is the case if it's
  ## directly used in a consume context, ignoring handle conversions.
  var dest = NodePosition(val)
  while true:
    dest = sibling(tree, dest)

    case tree[dest].kind
    of mnkPathConv:
      discard "skip conversions"
    of ConsumeCtx:
      return true
    else:
      return false