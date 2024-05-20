## This module implements various data-flow related analysis for MIR code.
## They're based on the ``mirexec`` traversal algorithms and a
## ``DataFlowGraph`` object corresponding to the code fragment (i.e.,
## ``MirTree``) that is analysed.
##
## All analysis implemented here establish the relationship between two lvalues
## purely by inspecting their paths. For example, ``a.b`` and ``a.b`` are
## treated as equals, whereas ``a.b``/``c.d`` and ``a.b`` and ``x[].b`` are
## not. This means that run-time aliasing (e.g., through pointers) is **not**
## considered.
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
    packedsets,
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
  ],
  compiler/utils/[
    containers
  ]

type
  AliveState = enum
    unchanged
    dead
    alive

  ComputeAliveProc[T] =
    proc(tree: MirTree, loc: T, op: Opcode,
         n: OpValue): AliveState {.nimcall, noSideEffect.}

func skipConversions*(tree: MirTree, val: OpValue): OpValue =
  ## Returns the expression after skipping handle-only conversions.
  result = val
  while tree[result].kind == mnkPathConv:
    result = tree.operand(result)

func isAlive*(tree: MirTree, cfg: DataFlowGraph,
             span: Subgraph, loc: Path, start: InstrPos): bool =
  ## Computes whether the location named by `loc` does contain a value (i.e.,
  ## is alive) when the data-flow operation at `start` is reached (but not
  ## executed). Only the `span` sub-graph is considered by the analysis.
  template path(val: OpValue): Path =
    computePath(tree, NodePosition val)

  template overlaps(val: OpValue): bool =
    overlaps(tree, loc, path(val)) != no

  # this is a reverse data-flow problem. We follow all control-flow paths from
  # `start` backwards until either there's no path left to follow or one of them
  # reaches a potential mutation of `loc`, in which case the underlying location
  # is considered to be alive. A path is not followed further if it reaches an
  # operation that "kills" the `loc` (removes its value, e.g. by moving it
  # somewhere else)

  var exit = false
  for op, n in traverseReverse(cfg, span, start, exit):
    case op
    of opDef, opMutate:
      if overlaps(n):
        # consider ``a.b = x`` (A) and ``a.b.c.d.e = y`` (B). If the
        # analysed l-value expression is ``a.b.c`` then both A and B mutate
        # it (either fully or partially). If traversal reaches what's
        # possibly a mutation of the analysed location, it means that the
        # location needs to be treated as being alive at `start`, so we can
        # return already
        return true

    of opKill, opConsume, opDestroy:
      if isPartOf(tree, loc, path n) == yes:
        # the location's value is consumed or the location is killed. No
        # operation coming before the current one can change that, so we can
        # stop traversing the current path
        exit = true

      # partially consuming the value, or killing the location, does *not*
      # change the alive state

    of opInvalidate:
      discard

    of opMutateGlobal:
      if tree[loc.root].kind == mnkGlobal:
        # an unspecified global is mutated and we're analysing a location
        # derived from a global -> assume the analysed global is mutated
        return true

    of opUse:
      discard "not relevant"

  # no mutation is directly connected to `start`. The location is not alive
  result = false

func isLastRead*(tree: MirTree, cfg: DataFlowGraph, span: Subgraph,
                 loc: Path, start: InstrPos): bool =
  ## Performs data-flow analysis to compute whether the value that `loc`
  ## evaluates to when `start` is reached (but not executed) is *not*
  ## observed by operations that have a control-flow dependency on the
  ## operation/statement at `start` and are located inside `span`.
  ## It's important to note that this analysis does not test whether the
  ## underlying *location* is accessed, but rather the *value* it stores. If a
  ## new value is assigned to the underlying location which is then accessed
  ## after, it won't cause the analysis to return false
  template path(val: OpValue): Path =
    computePath(tree, NodePosition val)

  var state: TraverseState
  for op, n in traverse(cfg, span, start, state):
    case op
    of opDef:
      let cmp = compare(tree, loc, path n)
      if isAPartOfB(cmp) == yes:
        # the location is reassigned -> all operations coming after will
        # observe a different value
        state.exit = true
      elif isBPartOfA(cmp) != no:
        # the location is partially written to -> the relevant values is
        # observed
        return false

    of opMutate:
      if overlaps(tree, loc, path n) != no:
        # the location is partially written to
        return false

    of opKill, opDestroy:
      let cmp = compare(tree, loc, path n)
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
      if overlaps(tree, loc, path n) != no:
        # value is observed -> not the last read
        return false

  # no further read of the value is connected to `start`
  result = true

func isLastWrite*(tree: MirTree, cfg: DataFlowGraph, span: Subgraph, loc: Path,
                  start: InstrPos): bool =
  ## Computes whether the location `loc` is reassigned or modified on any paths
  ## starting from and including `start`, returning 'false' if yes and 'true'
  ## if not. In other words, computes whether a reassignment or mutation that
  ## has a control-flow dependency on `start` and is located inside `span`
  ## observes the current value.
  template path(val: OpValue): Path =
    computePath(tree, NodePosition val)

  var state: TraverseState
  for op, n in traverse(cfg, span, start, state):
    case op
    of opDef, opMutate, opInvalidate, opDestroy:
      # note: since we don't know what happens to the location when it is
      # invalidated, the ``opInvalidate`` is also included here
      if overlaps(tree, loc, path n) != no:
        return false

    of opKill:
      let cmp = compare(tree, loc, path n)
      if isAPartOfB(cmp) == yes:
        state.exit = true

      # partially killing the analysed location is not considered to be a
      # write

    of opMutateGlobal:
      if tree[loc.root].kind == mnkGlobal:
        # an unspecified global is mutated and we're analysing a location
        # derived from a global
        return false

    of opUse, opConsume:
      discard

  result = true

func computeAliveOp*[T: LocalId | GlobalId](
  tree: MirTree, loc: T, op: Opcode, n: OpValue): AliveState =
  ## Computes the state of `loc` at the *end* of the given operation. The
  ## operands are expected to *not* alias with each other. The analysis
  ## result will be wrong if they do

  func isAnalysedLoc[T](n: MirNode, loc: T): bool =
    when T is GlobalId:
      n.kind == mnkGlobal and n.global == loc
    elif T is LocalId:
      n.kind in {mnkLocal, mnkParam, mnkTemp} and n.local == loc
    else:
      {.error.}

  template isRootOf(val: OpValue): bool =
    isAnalysedLoc(tree[getRoot(tree, val)], loc)

  template sameLocation(val: OpValue): bool =
    isAnalysedLoc(tree[skipConversions(tree, val)], loc)

  case op
  of opMutate, opDef:
    if isRootOf(n):
      # the analysed location or one derived from it is mutated
      return alive

  of opKill, opConsume, opDestroy:
    if sameLocation(n):
      # the location is killed or its value is consumed (i.e., moved somewhere
      # else)
      return dead

  of opInvalidate:
    discard "cannot be reasoned about here"

  of opMutateGlobal:
    when T is GlobalId:
      # the operation mutates global state and we're analysing a global
      result = alive

  of opUse:
    discard

func computeAlive*[T](tree: MirTree, cfg: DataFlowGraph,
                      span: Subgraph, loc: T, op: static ComputeAliveProc[T]
                     ): tuple[alive, escapes: bool] =
  ## Computes whether the location is alive when `span` is exited via either
  ## structured or unstructured control-flow. A location is considered alive
  ## if it contains a value

  # assigning to or mutating the analysed location makes it become alive,
  # because it then stores a value. Consuming its value or using ``wasMoved``
  # on it "kills" it (it no longer contains a value)

  var exit = false
  for opc, n in traverseFromExits(cfg, span, exit):
    case op(tree, loc, opc, n)
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
    case op(tree, loc, opc, n)
    of dead:
      exit = true
    of alive:
      # the location is definitely alive when leaving the span via
      # structured control-flow
      return (true, false)
    of unchanged:
      discard

  result = (false, false)

proc doesGlobalEscape*(tree: MirTree, scope: Subgraph, start: InstrPos,
                       s: GlobalId): bool =
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
