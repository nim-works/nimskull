#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the following MIR passes:
## - the 'switch' operation lowering (``lowerBranchSwitch``)
## - the pass for collapsing sink assignments into copies, moves, and
##   destrutive moves
## - the pass for injected ``wasMoved`` calls for consumed lvalues
## - the destructor (i.e. ``=destroy`` hook) injection
##
## Overview
## ========
##
## An analysis pass is performed that collects all entities that require
## destruction into an ``EntityDict``. These are: locals, temporaries, ``sink``
## parameters, and globals (with some exceptions). If a location has no
## type-bound ``=destroy`` hook (both user-provided and lifted), it is not
## included.
##
## .. note: for globals, only those that are not defined at module or
##          procedure scope and are not thread-local variables are collected.
##          Except for thread-local variables, the others are destroyed at the
##          end of the program.
##
## ``collapseSink`` then computes for all lvalue expression appearing as
## source operands to sink assignments whether it's the last use of the
## value currently stored in the location identified by the lvalue. All sinks
## where this is the case are remembered, and their corresponding data-flow
## operation is turned from a 'use' into a 'consume'.
##
## With all sink assignments either collapsed into copy or move assignments,
## the next analysis step computes which locations need to be destroyed via a
## destructor call (see ``computeDestructors``).
##
## As the last step, the assignment rewriting and destructor injection is
## performed, using the previously gathered data.
##
## Ownership analysis
## ==================
##
## Reassigning or reading from a location through a handle that is not the
## owning one is **not** detected by the analysis. In the following case
## (assuming no cursor inference):
##
## .. code-block::nim
##
##   var a = @[1, 2]
##   let p = addr a
##
##   var b = a
##   b.add 3
##   doAssert p[][0] == 1
##
##   a = ... # force the earlier move to be destructive
##
## the analysis will detect `var b = a` to be the last usage of `a`,
## subsequently turning the assignment into a move and thus making the
## assertion fail with an ``IndexDefect``.

# XXX: there exists an effect-related problem with the lifetime-tracking hooks
#      (i.e. ``=copy``, ``=sink``, ``=destroy``). The assignment rewriting and,
#      to some degree, the destructor injection can be seen as a
#      refinement/expansion/lowering and should thus not introduce (observable)
#      side-effects (mutation of global state, exceptional control-flow, etc.) --
#      it also violates the MIR specification. All three hooks are currently
#      allowed to have side-effects, which violates the aforementioned rules.
#      It also causes the concrete issue of cyclic dependencies: for example,
#      the move analyser uses data-flow analysis (which requires a control-flow
#      graph) in order to decide where to move and where to copy. If whether a
#      copy or move is used affects the control-flow graph, the move analyser
#      depends on its own output, which while possible to make work, would
#      likely introduce a large amount of complexity.
#      There are two possible solutions:
#      1. disallow lifetime-tracking hooks from having any side-effects
#      2. at least for the ``=copy`` and ``=sink`` hooks, each assignment
#         could be said to have the union of the effects from both hooks.
#         Those can be computed when generating the MIR code, as types and
#         their type-bound operations are already figured out at that point.
#         It's more complicated for ``=destroy`` hooks, since they are
#         injected rather than being the result of an expansion. The current
#         plan is to introduce the MIR concept of dedicated "scope finalizers",
#         which could be used to attach the effects gathered from all possible
#         destructor calls to

# XXX: not being able to rewrite an assignment into a call to the copy hook
#      because it is disabled is a semantic error, meaning that it should
#      be detected and reported during semantic analysis, not as part of
#      mid-end processing. Implementing this is not easily possible, however,
#      as it would either require duplicating the logic from ``analysis.nim``
#      for ``PNode`` AST (this is non-trivial) or somehow running the analysis
#      part of this module at the end of the second semantic pass
#      (``sempass2``).

import
  std/[
    algorithm,
    hashes,
    packedsets,
    tables
  ],
  compiler/ast/[
    ast,
    lineinfos,
    types
  ],
  compiler/mir/[
    analysis,
    mirbodies,
    mirchangesets,
    mirconstr,
    mirenv,
    mirtrees,
    sourcemaps
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    options
  ],
  compiler/sem/[
    aliasanalysis,
    liftdestructors,
    mirexec,
    sighashes
  ],
  compiler/utils/[
    cursors,
    idioms
  ]

type
  AnalyseCtx = object
    cfg: DataFlowGraph
    graph: ModuleGraph

  EntityName = object
    ## A unique identifier for an entity in the context of a ``MirTree``,
    ## which is meant to be used as a ``Table`` or ``HashSet`` key.
    ##
    ## Internally, two integers are used: the first integer is the integer
    ## value of a name's node kind, while what the second integer represents
    ## depends on the entity kind.
    a: array[2, int]

  EntityInfo = object
    ## Information about a lifetime of an entity. The lifetime of an entity is
    ## the time during which it can be *live* (i.e., store a value).
    def: NodePosition ## the position of the 'def' for the entity
    scope: Subgraph   ## the data-flow subgraph during which the entity exists

  EntityDict = Table[EntityName, seq[EntityInfo]]
    ## Entity dictionary. Stores all entities relevant to destructor
    ## injection and the move analyser. A location may have more than one
    ## lifetimes.

  DestroyEntry = tuple
    scope: NodePosition ## the position of the enclosing 'scope' node
    pos: NodePosition   ## the position of the 'def' belonging to the entity
                        ## that requires destruction
    needsFinally: bool  ## whether the destructor needs to be placed in a
                        ## 'finally' clause

  Moves = PackedSet[OpValue]
    ## A set storing the operands of all sinks that were collapsed into
    ## moves.

  AnalysisResults = object
    ## Bundled-up immutable state needed for assignment rewriting. Since
    ## they're immutable, ``Cursor``s are used in order to not copy
    # XXX: ideally, view types (i.e. ``lent``) would be used here
    moves: Cursor[Moves]
    entities: Cursor[EntityDict]
    destroy: Cursor[seq[DestroyEntry]]

iterator ritems[T](x: openArray[T]): lent T =
  ## Iterates and yields the items from the container `x` in reverse
  var i = x.high
  while i >= 0:
    yield x[i]
    dec i

func hash(x: EntityName): int =
  result = 0 !& x.a[0] !& x.a[1]
  result = !$result

func toName(n: MirNode): EntityName =
  ## Creates a unique representation for the entity the name node `n`
  ## references
  result.a[0] = n.kind.int
  result.a[1] =
    case n.kind
    of SymbolLike: n.sym.id
    of mnkGlobal:  n.global.int
    of mnkTemp:    n.temp.int
    else:          unreachable(n.kind)

func findScope(entities: EntityDict, name: EntityName, at: InstrPos,
               exists: var bool): EntityInfo =
  ## Returns the ``EntityInfo`` for `name` that encloses the data-flow
  ## instruction at `at`. If `name` is present in `entities` but `at` is not
  ## directly part of any lifetime, the ``EntityInfo`` for the lifetime
  ## preceding `at` is returned.
  ##
  ## `exists` is updated to indicate whether a scope was found.
  if name in entities:
    let lifetimes {.cursor.} = entities[name]
    # search for the upper bound:
    var i = 0
    while i < lifetimes.len and at >= lifetimes[i].scope.a:
      inc i

    if i - 1 >= 0:
      result = lifetimes[i - 1]
      exists = true
    else:
      exists = false
  else:
    exists = false


proc getVoidType(g: ModuleGraph): PType {.inline.} =
  g.getSysType(unknownLineInfo, tyVoid)

proc getOp*(g: ModuleGraph, t: PType, kind: TTypeAttachedOp): PSym =
  let t = t.skipTypes(skipForHooks)
  result = getAttachedOp(g, t, kind)
  if result == nil or result.ast.isGenericRoutine:
    # give up and find the canonical type instead:
    let h = sighashes.hashType(t, {CoType, CoDistinct})
    let canon = g.canonTypes.getOrDefault(h)
    if canon != nil:
      result = getAttachedOp(g, canon, kind)

func isNamed(tree: MirTree, val: OpValue): bool =
  ## Returns whether `val` is the projection of a named location (or refers to
  ## the named location itself).
  tree[tree.getRoot(val)].kind in {mnkLocal, mnkGlobal, mnkParam, mnkTemp}

func getDefEntity(tree: MirTree, n: NodePosition): NodePosition =
  assert tree[n].kind in DefNodes
  n + 1

# --------- compute routines ---------------

iterator nodesWithScope(tree: MirTree): (NodePosition, lent MirNode, Slice[NodePosition]) =
  ## Iterates over all nodes in `tree` and yields them together with the span
  ## of their enclosing scope
  var scopeStack: seq[Slice[NodePosition]]
  # the logic relies on the assumption that there exists a scope around
  # every 'def'

  # XXX: profiling showed that a significant amount of time is spent in
  #      ``computeSpan`` and adding elements to the `scopeStack`. An approach
  #      where a scope's span is only computed when needed might be better
  for i, n in tree.pairs:
    case n.kind
    of mnkScope:
      # start a new scope. The start and end node/token are not included in
      # the span
      let span = computeSpan(tree, i)
      scopeStack.add (span.a + 1)..(span.b - 1)
    of mnkEnd:
      if n.start == mnkScope:
        # leave the current scope:
        scopeStack.setLen(scopeStack.len - 1)

    else:
      yield (i, n, scopeStack[^1])

  #result.pos = p

func initEntityDict(tree: MirTree, dfg: DataFlowGraph): EntityDict =
  ## Collects the names of all analysable locations relevant to destructor
  ## injection and the move analyser. This includes: locals, temporaries, sink
  ## parameters and, with some restrictions, globals.
  ##
  ## Only owning locations that store values representing *resources* are
  ## relevant, so locations with no destructor for their type (not a resource)
  ## and cursor locations (non-owning) are not include in the dictionary.
  for i, n, scope in nodesWithScope(tree):
    case n.kind
    of mnkDef, mnkDefUnpack:
      let entity = tree[getDefEntity(tree, i)]

      let t =
        case entity.kind
        of mnkParam:
          assert isSinkTypeForParam(entity.sym.typ)
          entity.sym.typ
        of mnkLocal:
          assert sfCursor notin entity.sym.flags
          entity.sym.typ
        of mnkTemp, mnkGlobal:
          entity.typ
        else:
          unreachable()

      if hasDestructor(t):
        result.mgetOrPut(toName(entity), @[]).add:
          # don't include the data-flow operations preceding the def
          EntityInfo(def: i, scope: subgraphFor(dfg, i .. scope.b))

    else:
      discard

func computeOwnership(tree: MirTree, cfg: DataFlowGraph, entities: EntityDict,
                      lval: Path, start: InstrPos): bool =
  ## Computes for `lval` whether it can be moved from (i.e., ownership of the
  ## value transferred) at the program position `start`.
  case tree[lval.root].kind
  of mnkLocal, mnkParam, mnkGlobal, mnkTemp:
    # only entities that are relevant for destructor injection have an entry in
    # `entities`. Those that don't also can't be consumed (because we either
    # can't reason about them or they're non-owning locations), so values
    # derived from them are treated as non-owning
    # TODO: this currently also includes the ``result`` variable. It's possible
    #       to analyse it too -- we just need to make sure to treat an
    #       otherwise last-read as not a last-read if it is connected to a
    #       procedure exit. A slightly different approach would be to add a
    #       pseudo-use at the end of the body and make all procedure exits
    #       visit it first
    var exists = false
    let info = entities.findScope(toName(tree[lval.root]), start, exists)
    exists and not isCursor(tree, lval) and
      isLastRead(tree, cfg, info.scope, lval, start)
  else:
    unreachable()

func collapseSink(tree: MirTree, cfg: var DataFlowGraph,
                  entities: EntityDict): Moves =
  ## Computes for every ``mnkSink`` node what operation (copy or move) it has
  ## to collapse to, returning a set with the operands of all sinks that are
  ## collapsed into moves.
  ##
  ## In addition, the DFG instructions in `cfg` for sinks-turned-into-moves
  ## are updated to ``opConsume`` instructions.
  var update: seq[InstrPos]
    ## tracks the DFG instructions that need to be updated

  # search for all 'use' instructions representing sinks, and compute whether
  # they have to be turned into a move or copy
  for i, op, opr in cfg.instructions:
    if op == opUse and tree[tree.parent(NodePosition opr)].kind == mnkSink:
      # it's the DFG instruction for a sink
      if hasDestructor(tree[opr].typ) and
         computeOwnership(tree, cfg, entities,
                          computePath(tree, NodePosition opr), i + 1):
        update.add i
        result.incl opr

      # for the moment, sinks are always turned into copies for values without
      # custom destroy/copy/sink behaviour

  # change all 'use' instructions corresponding to sinks to 'consume'
  # instructions. This is more efficient than changing the node kinds and then
  # recomputing the graph
  cfg.change(update, opConsume)

type DestructionMode = enum
  demNone    ## location doesn't need to be destroyed because it contains no
             ## value when control-flow exits the enclosing scope
  demNormal  ## the location contains a value when the scope is exited via
             ## structured control-flow
  demFinally ## the location contains a value when the scope is exited via
             ## unstructured control-flow

func requiresDestruction(tree: MirTree, cfg: DataFlowGraph,
                         span: Subgraph, def: NodePosition, entity: MirNode
                        ): DestructionMode =
  template computeAlive(loc, op: untyped): untyped =
    computeAlive(tree, cfg, span, loc, op)

  let r =
    case entity.kind
    of mnkParam, mnkLocal:
      computeAlive(entity.sym, computeAliveOp[PSym])
    of mnkGlobal:
      computeAlive(entity.global, computeAliveOp[GlobalId])
    of mnkTemp:
      # unpacked tuples don't need to be destroyed because all elements are
      # moved out of them
      if tree[def].kind != mnkDefUnpack:
        computeAlive(entity.temp, computeAliveOp[TempId])
      else:
        (alive: false, escapes: false)
    else:
      unreachable(entity.kind)

  result =
    if r.escapes: demFinally
    elif r.alive: demNormal
    else:         demNone

func computeDestructors(tree: MirTree, cfg: DataFlowGraph,
                        entities: EntityDict): seq[DestroyEntry] =
  ## Computes and collects which locations present in `entities` need to be
  ## destroyed at the exit of their enclosing scope in order to prevent the
  ## values they still store from staying alive.
  ##
  ## Special handling is required if the scope is exited via unstructured
  ## control-flow while the location is still alive (its value is then said
  ## to "escape")
  var needsFinally: PackedSet[NodePosition]

  iterator items(x: EntityDict): lent EntityInfo =
    for _, infos in x.pairs:
      for it in infos.items:
        yield it

  for info in entities.items:
    let
      def = info.def ## the position of the entity's definition
      entity = tree[getDefEntity(tree, def)]
      scopeStart = findParent(tree, def, mnkScope)

    if entity.kind == mnkGlobal and
       doesGlobalEscape(tree, info.scope, info.scope.a, entity.global):
      # TODO: handle escaping globals. Either report a warning, an error, or
      #       defer destruction of the global to the end of the program
      discard

    case requiresDestruction(tree, cfg, info.scope, def, entity)
    of demNormal:
      result.add (scopeStart, def, false)
    of demFinally:
      needsFinally.incl scopeStart
      result.add (scopeStart, def, true)
    of demNone:
      discard

  # second pass: if at least one destructor call in a scope needs to use a
  # finalizer, all do. Update the entries accordingly
  for it in result.mitems:
    if it.scope in needsFinally:
      it.needsFinally = true

# --------- analysis routines --------------

func isAlive(tree: MirTree, cfg: DataFlowGraph,
             entities: EntityDict, val: Path, at: InstrPos): bool =
  ## Computes if `val` refers to a location that contains a value when
  ## `at` in the DFG is reached.
  let root = val.root

  case tree[root].kind
  of mnkLocal, mnkParam, mnkGlobal, mnkTemp:
    let scope =
      # XXX: the way the ``result`` variable is detected here is a hack. It
      #      should be treated as any other local in the context of the MIR
      if tree[root].kind in SymbolLike and tree[root].sym.kind == skResult:
        cfg.subgraphFor(NodePosition(0) .. NodePosition(tree.high))
      else:
        var exists: bool
        let info = entities.findScope(toName(tree[root]), at, exists)
        if exists: info.scope
        else:      return true # not something we can analyse -> assume alive

    # if the location is not assigned an initial value on definition, `start`
    # may come before the alive subgraph
    if at <= scope.a:
      false # the location cannot be alive
    else:
      isAlive(tree, cfg, scope, val, at)
  else:
    # something that we can't analyse (e.g. a dereferenced pointer). We have
    # to be conservative and assume that the location the lvalue names already
    # stores a value
    true

func needsReset(tree: MirTree, cfg: DataFlowGraph, ar: AnalysisResults,
                src: Path, at: InstrPos): bool =
  ## Computes whether a reset needs to be injected for `src` in order to
  ## prevent the current value the underlying location contains from being
  ## observed. `at` is the DFG position to compute this information at.
  ##
  ## This is relevant for when ownership of a value is transferred, as the
  ## transferral doesn't imply a change to neither the previous owner
  ## (location) nor the value itself. As long as the location is not observed
  ## to still contain the value it now no longer owns, this is not a problem.
  ## If it can't be proven that the unowned value is observed (which could
  ## cause problems like, for example, double-frees), the location is
  ## explicitly reset (i.e. the value removed from it).
  let root = src.root
  # XXX: the way the ``result`` variable is detected here is a hack. It
  #      should be treated as any other local in the context of MIR. The
  #      fact that the result variable is potentially used outside the
  #      procedure's body should be encoded by inserting a special 'use'
  #      operation that has a control-flow dependency on *all* other
  #      operations
  if tree[root].kind in SymbolLike and tree[root].sym.kind == skResult:
    return true

  var exists: bool
  let info = findScope(ar.entities[], toName(tree[root]), at, exists)

  if not exists:
    # the location is not local to the current context -> assume that it needs
    # to be reset
    return true

  let res = isLastWrite(tree, cfg, info.scope, src, at)

  if res.result:
    if res.escapes or res.exits:
      let def = info.def
      assert tree[def].kind in DefNodes

      # check if there exists a destructor call that would observe the
      # location's value:
      for it in ar.destroy[].items:
        if def == it.pos:
          if (it.needsFinally and res.escapes) or res.exits:
            # there exists a destructor call for the location -> the current
            # value is observed
            return true

          # no need to continue searching
          break

    # no mutation nor destructor call observes the current value -> no reset
    # is needed
    result = false
  else:
    # the presence of the value is observed -> a reset is required
    result = true

# ------- code generation routines --------

template buildVoidCall*(bu: var MirBuilder, env: var MirEnv, p: PSym,
                       body: untyped) =
  let prc = p # prevent multi evaluation
  bu.subTree mnkVoid:
    let kind =
      if canRaise(optPanics in graph.config.globalOptions, prc.ast[namePos]):
        mnkCheckedCall
      else:
        mnkCall

    # XXX: injected procedures should not introduce new control-flow paths
    bu.subTree MirNode(kind: kind, typ: getVoidType(graph)):
      bu.use toValue(env.procedures.add(prc), prc.typ)
      body

proc genWasMoved(bu: var MirBuilder, graph: ModuleGraph, target: Value) =
  bu.subTree MirNode(kind: mnkVoid):
    bu.buildMagicCall mWasMoved, getVoidType(graph):
      bu.emitByName(target, ekKill)

proc genDestroy*(bu: var MirBuilder, graph: ModuleGraph, env: var MirEnv,
                 target: Value) =
  let destr = getOp(graph, target.typ, attachedDestructor)

  bu.buildVoidCall(env, destr):
    bu.emitByName(target, ekMutate)

func destructiveMoveOperands(bu: var MirBuilder, tree: MirTree,
                             src: NodePosition
                            ): tuple[src, clear: Value] =
  ## Creates the bindings for the operands to use for a destructive move.
  let x = NodePosition skipConversions(tree, OpValue src)
  if x == src:
    # nothing was skipped, the same binding can be used
    let r = bu.bindMut(tree, x)
    (r, r)
  else:
    # use the skipped expression for clearing, the original one as
    # the assignment source
    (bu.bindImmutable(tree, src), bu.bindMut(tree, x))

proc specializeAsgn(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
                    stmt: NodePosition, pos: InstrPos, c: var Changeset) =
  ## Specializes the modifier-using assignment at `stmt` using the analysis
  ## results:
  ## * guaranteed self-assignments are eliminated (i.e., the assignment is
  ##   removed)
  ## * sink assignments are turned into copy, move, or destructive move
  ##   assignments
  ## * normal assignments are turned into initializing assignments (if
  ##   possible)
  ##
  ## `pos` is the 'def' data-flow instruction corresponding to the assignment.
  let
    dest       = tree.child(stmt, 0)
    operator   = tree.child(stmt, 1)
    source     = tree.child(operator, 0)
    sourcePath = computePath(tree, source)
    destPath   = computePath(tree, dest)
    relation   = compare(tree, sourcePath, destPath)

  if relation.isSame:
    # a self-assignment -> elide
    c.remove(tree, stmt)
  elif tree[operator].kind == mnkSink:
    let isAlive = tree[stmt].kind == mnkAsgn and
                  isAlive(tree, ctx.cfg, ar.entities[], destPath, pos)
    if tree.operand(operator) in ar.moves[]:
      # turn the sink into a move
      if isAlive and isAPartOfB(relation) != no:
        # it's potentially a part-to-whole assignment, e.g.: ``x = move x.y``,
        # and the destination contains a value. The value must first be moved
        # into a temporary, since a move destination must not overlap with
        # the source
        var tmp: Value
        c.insert(tree, stmt, source, bu):
          let b = bu.bindMut(tree, source)
          tmp = bu.wrapTemp b.typ:
            bu.move b
          genWasMoved(bu, ctx.graph, b)

        c.replaceMulti(tree, operator, bu):
          bu.move tmp
      elif needsReset(tree, ctx.cfg, ar, sourcePath, pos):
        # the value can be moved, but the source location needs to be cleared
        # afterwards. Turn ``a = sink x.y`` into:
        #   bind_mut _1 = x.y
        #   a = move _1
        #   wasMoved(name _1)
        var b, clear: Value
        c.insert(tree, stmt, source, bu):
          (b, clear) = bu.destructiveMoveOperands(tree, source)
        c.replaceMulti(tree, operator, bu):
          bu.move b
        c.insert(tree, tree.sibling(stmt), source, bu):
          genWasMoved(bu, ctx.graph, clear)
      else:
        # the value can be moved without the source location having to be
        # cleared
        c.changeTree(tree, operator):
          MirNode(kind: mnkMove, typ: tree[operator].typ)
    else:
      # the value cannot be moved, turn the sink into a copy
      c.changeTree(tree, operator):
        MirNode(kind: mnkCopy, typ: tree[operator].typ)

    if tree[stmt].kind == mnkAsgn and not isAlive:
      # the assignment initializes the location
      c.changeTree(tree, stmt): MirNode(kind: mnkInit)
  else:
    # it's a move or move already, so nothing to change there
    if tree[stmt].kind == mnkAsgn and
       not isAlive(tree, ctx.cfg, ar.entities[], destPath, pos):
      # the assignment initializes the location
      c.changeTree(tree, stmt): MirNode(kind: mnkInit)

proc consumeArg(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
                expr: NodePosition, src: OpValue, pos: InstrPos,
                c: var Changeset) =
  ## Injects the reset logic for the underlying location of lvalues passed to
  ## sink parameters or the ``raise`` statement. This is only necessary if
  ## there's a destructor call that needs to be disarmed -- if there's nothing
  ## to disarm, no reset logic is emitted.
  ##
  ## `expr` is the call, construction, or ``raise`` argument expression that
  ## the consume is part of; `src` is the consumed lvalue; and `pos` is the
  ## data-flow instruction correspondig to the consume operation.
  assert tree[expr].kind in ExprKinds
  if isNamed(tree, src) and
     needsReset(tree, ctx.cfg, ar, computePath(tree, NodePosition src),
                pos + 1):
    let stmt = tree.parent(expr)

    if tree[expr].kind == mnkCheckedCall:
      # the consumer raises, meaning that resetting the consumed-from location
      # cannot happen *after* the statement. The source location's value is
      # first assigned to a temporary and then the source is reset
      var tmp: Value
      c.insert(tree, stmt, NodePosition src, bu):
        let v = bu.bindMut(tree, NodePosition src)
        tmp = bu.materializeMove(v)
        genWasMoved(bu, ctx.graph, v)

      # replace the argument with the injected temporary:
      c.replaceMulti(tree, NodePosition src, bu):
        bu.use tmp
    else:
      # the reset can happen after the statement
      c.insert(tree, tree.sibling(stmt), NodePosition src, bu):
        let v = bu.bindMut(tree, NodePosition src)
        genWasMoved(bu, ctx.graph, v)

proc rewriteAssignments(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
                        c: var Changeset) =
  ## Rewrites assignments to locations into calls to either the ``=copy``
  ## or ``=sink`` hook (see ``expandAsgn`` for more details).
  ##
  ## Also injects the necessary location reset logic for lvalues passed to
  ## 'consume' argument sinks.
  for i, opc, val in ctx.cfg.instructions:
    if opc == opConsume and hasDestructor(tree[val].typ):
      # disarm the destructors for locations of which the value is consumed
      # but that are reassigned or destroyed after
      let parent = tree.parent(NodePosition val)

      case tree[parent].kind
      of mnkConsume:
        # we must be processing a call/construction argument
        consumeArg(tree, ctx, ar, tree.parent(parent), val, i, c)
      of mnkRaise:
        consumeArg(tree, ctx, ar, NodePosition val, val, i, c)
      of mnkMove, mnkSink:
        # assignments are handled separately
        discard
      else:
        unreachable(tree[parent].kind)
    elif opc == opDef and (let stmt = tree.parent(NodePosition val);
          tree[stmt, 1].kind in {mnkCopy, mnkMove, mnkSink}):
      # specialize the modifier-using assignment
      assert tree[stmt].kind in {mnkDef, mnkDefUnpack, mnkAsgn, mnkInit}
      specializeAsgn(tree, ctx, ar, stmt, i, c)

# --------- destructor injection -------------

proc injectDestructorsInner(bu: var MirBuilder, orig: MirTree,
                            graph: ModuleGraph, env: var MirEnv,
                            entries: openArray[DestroyEntry]) =
  ## Generates a destructor call for each item in `entries`, using `buf` as the
  ## output.
  for it in ritems(entries):
    let def = getDefEntity(orig, it.pos)
    let t =
      case orig[def].kind
      of SymbolLike: orig[def].sym.typ
      of mnkGlobal:  orig[def].typ
      of mnkTemp:    orig[def].typ
      else:          unreachable()

    bu.buildVoidCall(env, getOp(graph, t, attachedDestructor)):
      bu.emitByName(Value(node: orig[def]), ekMutate)

proc injectDestructors(tree: MirTree, graph: ModuleGraph,
                       destroy: seq[DestroyEntry], env: var MirEnv,
                       c: var Changeset) =
  ## Injects a destructor call for each entity in the `destroy` list, in the
  ## entities reverse order they are defined. That is the entity defined last
  ## is destroyed first
  if destroy.len == 0:
    # nothing to do
    return

  var
    entries = destroy
    needsFinally: PackedSet[NodePosition]

  # first pass: gather which scopes need to be wrapped in a ``finally``
  for it in destroy.items:
    assert tree[it.scope].kind == mnkScope
    if it.needsFinally:
      needsFinally.incl it.scope

  # sort the entries by scope (first-order) and position (second-order) in
  # ascending order. Do this before moving the definitions, as `entries` would
  # have no defined order otherwise (which could change the relative order
  # of the moved definitions)
  sort(entries, proc(x, y: auto): int =
    result = ord(x.scope) - ord(y.scope)
    if result == 0:
      result = ord(x.pos) - ord(y.pos)
  )

  iterator scopeItems(e: seq[DestroyEntry]): Slice[int] {.inline.} =
    ## Partitions `e` using the `scope` field and yields the slice of each
    ## partition
    var
      scopePos = e[0].scope
      start = 0

    # the loop is written in such a way as that ``yield`` is only needed once
    for i in 1..e.len:
      if i == e.len or e[i].scope != scopePos:
        yield start .. (i - 1)
        if i < e.len:
          scopePos = e[i].scope
          start = i

  # second pass: inject the destructors and place them inside a ``finally``
  # clause if necessary
  for s in scopeItems(entries):
    let
      scopeStart = entries[s.a].scope
      useFinally = scopeStart in needsFinally
      source = scopeStart
        ## the node to inherit the origin information from

    if useFinally:
      # start a 'finally' at the beginning of the scope:
      c.insert(tree, scopeStart + 1, source, buf):
        buf.add MirNode(kind: mnkTry, len: 1)
        buf.add MirNode(kind: mnkStmtList)

    # insert at the scope's end node
    c.insert(tree, findEnd(tree, scopeStart), source, buf):
      if useFinally:
        buf.add endNode(mnkStmtList) # close the body of the 'try' clause
        buf.subTree MirNode(kind: mnkFinally):
          # there's no need for opening a new scope -- we use a statement-list
          # instead
          buf.subTree MirNode(kind: mnkStmtList):
            injectDestructorsInner(buf, tree, graph, env,
                                   toOpenArray(entries, s.a, s.b))

        buf.add endNode(mnkTry)
      else:
        injectDestructorsInner(buf, tree, graph, env,
                               toOpenArray(entries, s.a, s.b))

proc lowerBranchSwitch(bu: var MirBuilder, body: MirTree, graph: ModuleGraph,
                       idgen: IdGenerator, env: var MirEnv,
                       stmt: NodePosition) =
  ## Lowers a 'switch' operation into a simple discriminant assignment plus
  ## the logic for destroying the previous branch (if necessary)
  assert body[stmt].kind == mnkSwitch

  let
    target = body.operand(stmt, 0)
    objType = body[target].typ
    typ = body[target].field.typ

  assert body[target].kind == mnkPathVariant
  assert body[stmt, 1].kind in ModifierNodes

  let
    a = bu.wrapMutAlias(typ):
      # bind the discriminator lvalue, not the variant lvalue
      bu.subTree MirNode(kind: mnkPathNamed, typ: typ, field: body[target].field):
        bu.emitFrom(body, NodePosition body.operand(target))
    b = bu.wrapTemp typ:
      bu.emitFrom(body, body.child(stmt, 1))

  # check if the object contains fields requiring destruction:
  if hasDestructor(objType):
    # XXX: we are only interested in if the *record-case* contains fields
    #      requiring destruction, not the whole *object*. If none of the
    #      branches requires destruction, but the enclosing object does,
    #      we're creating an empty destructor here
    # XXX: in general, I think it'd make sense to change how variant objects
    #      are represented in the compiler. An early idea:
    #      - lift the record-case into its own object, using a dedicated
    #        "variant object" type
    #      - also, lift each branch that has more than one field into it's own
    #        ``object`` type
    #      together, these steps would greatly simplify both interacting with
    #      variant objects and attaching extra data to them (such as a
    #      destructor).
    #      Depending on how it's implemented, this approach has issues with
    #      field alignment, however.
    let branchDestructor = produceDestructorForDiscriminator(
                            graph, objType,
                            body[target].field,
                            unknownLineInfo, idgen
                           )

    let
      boolTyp = graph.getSysType(unknownLineInfo, tyBool)

    # XXX: comparing the discrimant values here means that the branch is
    #      destroyed even if the branch doesn't change. This differs from
    #      the VM's behaviour. There, the branch is only reset if it's
    #      actually changed
    var val = bu.wrapTemp(boolTyp):
      bu.buildMagicCall(getMagicEqForType(typ), boolTyp):
        bu.emitByVal a
        bu.emitByVal b

    val = bu.wrapTemp(boolTyp):
      bu.buildMagicCall mNot, boolTyp:
         bu.emitByVal val

    bu.subTree mnkIf:
      bu.use val
      # ``=destroy`` call:
      bu.buildVoidCall(env, branchDestructor):
        # pass the original variant access to the destroy call
        bu.subTree mnkName:
          bu.subTree MirNode(kind: mnkTag, effect: ekInvalidate):
            bu.emitFrom(body, NodePosition target)

  else:
    # the object doesn't need destruction, which means that neither does one
    # of the branches. We can just change the discriminant
    # XXX: this differs from what the VM does. For the VM, switching branches
    #      resets the memory of the record-case back to zero, independent of
    #      whether a branch contains managed memory or not
    discard

  # generate the ``discriminant = newValue`` assignment:
  bu.asgn(a, b)

func shouldInjectDestructorCalls*(owner: PSym): bool =
  # only inject destructor calls if the owner is not a generated OP (e.g. a
  # generated ``=destroy``) and also not an ``.inline`` iterator
  result =
     {sfInjectDestructors, sfGeneratedOp} * owner.flags == {sfInjectDestructors} and
     (owner.kind != skIterator or not isInlineIterator(owner.typ))

proc injectDestructorCalls*(g: ModuleGraph, idgen: IdGenerator,
                            env: var MirEnv, owner: PSym,
                            body: var MirBody) =
  ## The ``injectdestructors`` pass entry point. The pass is made up of
  ## multiple sub-passes, hence the mutable `body` (as opposed
  ## to returning a ``Changeset``).
  ##
  ## For now, semantic errors and other diagnostics related to lifetime-hook
  ## usage are also reported here.

  template apply(c: Changeset) =
    ## Applies the changeset `c` to `body`.
    apply(body.code, prepare(c))

  # apply the first batch of passes:
  block:
    var changes = initChangeset(body.code)
    # the VM implements branch switching itself - performing the lowering for
    # code meant to run in it would be harmful
    # FIXME: discriminant assignment lowering also needs to be disabled for
    #        when generating code running at compile-time (e.g. inside a
    #        macro)
    # XXX: the lowering is *always* necessary, as the destructors for
    #      fields inside switched-away-from branches won't be called
    #      otherwise
    # TODO: make the branch-switch lowering a separate and standalone pass --
    #       it's not directly related to the rest of the processing here
    if g.config.backend != backendNimVm:
      for i, n in body.code.pairs:
        if n.kind == mnkSwitch:
          changes.replaceMulti(body.code, i, buf):
            lowerBranchSwitch(buf, body.code, g, idgen, env, i)

    apply(changes)

  # apply the second batch of passes:
  block:
    var
      changes = initChangeset(body.code)
      actx = AnalyseCtx(graph: g, cfg: computeDfg(body.code))

    let
      entities = initEntityDict(body.code, actx.cfg)
      moves = collapseSink(body.code, actx.cfg, entities)

    let destructors = computeDestructors(body.code, actx.cfg, entities)

    rewriteAssignments(
      body.code, actx,
      AnalysisResults(moves: cursor(moves),
                      entities: cursor(entities),
                      destroy: cursor(destructors)),
      changes)

    injectDestructors(body.code, g, destructors, env, changes)

    apply(changes)
