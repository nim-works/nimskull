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
## the lifetime of the corresponding locatins ends, regardless of whether an
## unsafe alias (pointer) of it still exists.
##
## Instead of assigning a unique ID to each expression, they're identified
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
##
## TODO: update the doc comment, large parts of it are outdated

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
  ],
  compiler/utils/[
    containers
  ]

import std/packedsets

type
  Owned* {.pure.} = enum
    # TODO: remove ``Owned`` and use a ``PackedSet`` instead
    no
    yes
    weak ## values derived from compound values (e.g. ``object``, ``tuple``,
         ## etc.) that are weakly owned decay to no ownership. Rvalues are
         ## weakly owned -- they can be consumed directly, but sub-values of
         ## them can't
    unknown

  Values* = object
    ## Stores information about MIR expressions.
    status: Table[OpValue, Owned]

  AliveState = enum
    unchanged
    dead
    alive

  ComputeAliveProc[T] =
    proc(tree: MirTree, values: Values, loc: T, op: Opcode,
         n: OpValue): AliveState {.nimcall, noSideEffect.}

func skipConversions*(tree: MirTree, val: OpValue): OpValue =
  ## Returns the expression after skipping handle-only conversions.
  result = val
  while tree[result].kind == mnkPathConv:
    result = tree.operand(result)

template owned*(v: Values, val: OpValue): Owned =
  v.status[val]

func setOwned*(v: var Values, val: OpValue, owns: Owned) {.inline.} =
  v.status[val] = owns

func isAlive*(tree: MirTree, cfg: ControlFlowGraph, v: Values,
             span: Slice[InstrPos], loc: Path,
             start: InstrPos): bool =
  ## Computes if the location named by `loc` does contain a value (i.e., is
  ## alive) when the DFG instruction at `start` is reached (but not executed).
  ## The performed data-flow analysis only considers DFG instructions inside
  ## `span`.
  template path(val: OpValue): Path =
    computePath(tree, NodePosition val)

  template overlaps(val: OpValue): bool =
    overlaps(tree, loc, path(val)) != no

  # this is a reverse data-flow problem. We follow all control-flow paths from
  # `pos` backwards until either there's no path left to follow or one of them
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
        # location needs to be treated as being alive at `pos`, so we can
        # return already
        return true

    of opKill:
      if isPartOf(tree, loc, path n) == yes:
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
        if isPartOf(tree, loc, path n) == yes:
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
                 span: Slice[InstrPos], loc: Path, start: InstrPos
                ): bool =
  ## Performs data-flow analysis to compute whether the value that `loc`
  ## evaluates to at `pos` is *not* observed by operations that have a
  ## control-flow dependency on the operation/statement at `pos` and
  ## are located inside `span`.
  ## It's important to note that this analysis does not test whether the
  ## underlying *location* is accessed, but rather the *value* it stores. If a
  ## new value is assigned to the underlying location which is then accessed
  ## after, it won't cause the analysis to return false
  template path(val: OpValue): Path =
    computePath(tree, NodePosition val)

  var state: TraverseState
  for op, n in traverse(tree, cfg, span, start, state):
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

    of opKill:
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

    else:
      discard

  # no further read of the value is connected to `pos`
  result = true

func isLastWrite*(tree: MirTree, cfg: ControlFlowGraph, values: Values,
                  span: Slice[InstrPos], loc: Path, start: InstrPos
                 ): tuple[result, exits, escapes: bool] =
  ## Computes if the location `loc` is not reassigned to or modified while it
  ## still contains the value it contains at `pos`. In other words, computes
  ## whether a reassignment or mutation that has a control-flow dependency on
  ## `pos` and is located inside `span` observes the current value.
  ##
  ## In addition, whether the `pos` is connected to a structured or
  ## unstructured exit of `span` is also returned
  template path(val: OpValue): Path =
    computePath(tree, NodePosition val)

  var state: TraverseState
  for op, n in traverse(tree, cfg, span, start, state):
    case op
    of opDef, opMutate, opInvalidate:
      # note: since we don't know what happens to the location when it is
      # invalidated, the ``opInvalidate`` is also included here
      if overlaps(tree, loc, path n) != no:
        return (false, false, false)

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
    isAnalysedLoc(tree[getRoot(tree, NodePosition val)], loc)

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
                      span: Slice[InstrPos], loc: T,
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

proc doesGlobalEscape*(tree: MirTree, scope: Slice[InstrPos],
                       start: InstrPos, s: PSym): bool =
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
