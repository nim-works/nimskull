#
#
#           The Nim Compiler
#        (c) Copyright 2017 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the following MIR passes:
## - the pass for injecting temporaries for unconsumed rvalues that have
##   destructors (``injectTemporaries``)
## - the 'switch' operation lowering (``lowerBranchSwitch``)
## - the pass for rewriting assignments into call to the respective
##   lifetime-tracking hooks
## - the pass for introducing copies for unowned values passed to ``sink``
##   parameters
## - the destructor (i.e. ``=destroy`` hook) injection
##
## Overview
## ========
##
## The injection of temporaries is required to prevent leaks. Only locations
## can be destroyed, so if the result of a procedure call is a resource that
## requires cleanup and is not directly consumed (by assigning it to a
## location or passing it to a ``sink`` argument), it is materialized into a
## temporary. The analysis of what requires destruction only takes entities
## (globals, locals, temporaries, etc.) into account that are explicitly
## defined in the code fragment (``MirTree``), so the changes performed by the
## temporary injection have to be visible to it.
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
## As an optimization, only entities for which it can't be statically proven
## that they don't contain a value at the end of their scope are collected.
##
## Next, an instance of a ``Values`` dictionary corresponding to the input
## code-fragment is created and initialized. For all arguments that appear in
## a consume context (e.g. passed to ``sink`` argument, assignment source)
## and for which the ownership status could not be resolved to either 'yes' or
## 'no' by ``analysis.computeValuesAndEffects``, a data-flow analysis is
## performed to figure out the status (see ``solveOwnership``).
##
## Using the now resolved ownership status of all expressions, the next
## analysis step computes which locations need to be destroyed via a destructor
## call (see ``computeDestructors``).
##
## As the last step, the assignment rewriting and destructor injection is
## performed, using the previously gathered data.
##
## For the assignment rewriting, if the source operand of an assignment is
## owned, a move is used instead of a copy.
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
##
## Escaping temporaries
## ====================
##
## TODO: This problem is fixed now. Create tests and then remove this section
##
## There exists the general problem of both temporaries and rvalues escaping
## in the context of consumed arguments. Consider:
##
## .. code-block::nim
##
##   proc f_sink(x: sink Obj, y: int) = discard
##
##   f_sink(create(), callThatRaises()) # 1
##
##   var x = Obj()
##   f_sink(notLastUseOf x, callThatRaises()) # 2
##
##   var y = Obj()
##   f_sink(lastUseOf y, callThatRaises()) # 3
##
##   var z = Obj()
##   f_sink(lastUseOf z, callThatRaises()) # 4
##   z = Obj()
##
## For #1, the temporary injection pass recognizes that the result of
## ``create()`` is used in a consume context and thus doesn't inject a
## temporary. This then causes the value to leak when ``callThatRaises()``
## raises an exception. Note that the raising of an exception is only used
## as an example -- the same issue is present with all other unstructured
## control-flow (``return``, ``break``, etc.).
##
## Fixing this would require for the temporary injection pass to check if
## the consume is connected to the call on all control-flow paths and only
## then omit the temporary. A clean solution that introduces no duplication of
## logic would be to use the ``ControlFlowGraph`` for this, but it is not yet
## available at that point.
##
## #2, #3, and #4 are variations of the same problem. Consume-argument handling
## happens concurrently to destructor injection and a communication channel
## between the two would be required in order to notify the destructor
## injection pass about the introduced temporaries.

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
    strtabs,
    tables
  ],
  compiler/ast/[
    ast,
    lineinfos,
    types
  ],
  compiler/mir/[
    analysis,
    mirchangesets,
    mirconstr,
    mirtrees,
    sourcemaps,
    utils
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    options,
    msgs
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

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport
from compiler/ast/report_enums import ReportKind

type
  AnalyseCtx = object
    cfg: ControlFlowGraph
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
    def: NodePosition ## the position of the 'def' for the entity
    scope: Slice[InstrPos]
      ## the span of instruction covering the location's alive range (but not
      ## more)

  EntityDict = Table[EntityName, EntityInfo]
    ## Entity dictionary. Stores all entities relevant to destructor
    ## injection and the move analyser

  AnalysisResults = object
    ## Bundled-up immutable state needed for assignment rewriting. Since
    ## they're immutable, ``Cursor``s are used in order to not copy
    # XXX: ideally, view types (i.e. ``lent``) would be used here
    v: Cursor[Values]
    entities: Cursor[EntityDict]
    destroy: Cursor[seq[(NodePosition, bool)]]

  LocalDiagKind = enum
    ldkPassCopyToSink       ## a copy is introduced in a consume context
    ldkUnavailableTypeBound ## a type-bound operator is requested but not
                            ## available

  LocalDiag = object
    ## A temporary diagnostic representation that is later turned into a
    ## ``SemReport``
    pos: NodePosition ## the location of the report
    case kind: LocalDiagKind
    of ldkUnavailableTypeBound:
      op: TTypeAttachedOp
    of ldkPassCopyToSink:
      discard

const
  skipAliases = {tyGenericInst, tyAlias, tySink}
    ## the set of types to not consider when looking up a type-bound operator

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
    of mnkTemp:    n.temp.int
    else:          unreachable(n.kind)

func getAliveRange(entities: EntityDict, name: EntityName, exists: var bool
                  ): Slice[InstrPos] =
  ## Returns the maximum span of data-/control-flow instructions that happen
  ## during the existence of the entity with the given `name`.
  ## `exists` is set to whether an entity with the given `name` is present
  ## in `entities`.
  let info =
    entities.getOrDefault(name, EntityInfo(def: NodePosition(-1)))

  exists = info.def != NodePosition(-1)
  if exists:
    result = info.scope

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
  ## Returns whether `val` is an lvalue that names a location derived from
  ## a named entity. For example, ``local.a.b`` is such a location.
  ## TODO: update the doc comment
  tree[tree.getRoot(NodePosition val)].kind in {mnkLocal, mnkGlobal, mnkParam,
                                                mnkTemp}

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

func initEntityDict(tree: MirTree, cfg: ControlFlowGraph): EntityDict =
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
        of mnkLocal, mnkGlobal:
          assert sfCursor notin entity.sym.flags
          entity.sym.typ
        of mnkTemp:
          entity.typ
        else:
          nil # not a location (e.g. a procedure)

      if t != nil and hasDestructor(t):
        let re = toName(entity)
        # XXX: a ``doAssert`` is only used here in order to always catch
        #      duplicate symbols incorrectly getting past ``transf``
        doAssert re notin result, "entity appears in a 'def' multiple times"
        # don't include the def statement itself in the scope, so that the
        # statement's control-flow effects don't interfere...
        var scope = cfg.rangeFor((i+1) .. scope.b)
        if entity.kind == mnkParam or tree[tree.operand(i, 1)].kind != mnkNone:
          # ... but make sure that the 'def' instruction is included
          # XXX: not pretty, but works for now. Explicit start and end
          #      instruction delimiting the lifetime of locations in the
          #      DFG are going to allow for a cleaner solution
          scope.a -= 1
        result[re] = EntityInfo(def: i, scope: scope)

    else:
      discard

func computeOwnership(tree: MirTree, cfg: ControlFlowGraph, values: Values,
                      entities: EntityDict, lval: Path, start: InstrPos
                     ): Owned =
  case tree[lval.root].kind
  of mnkDeref, mnkDerefView, mnkConst:
    # * derefs reaching here means that they couldn't be resolved
    # * handles to constant locations are never owning
    Owned.no
  of mnkLiteral:
    # literals can be moved (although not destructively)
    Owned.yes
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
    let aliveRange = entities.getAliveRange(toName(tree[lval.root]), exists)
    if exists and isLastRead(tree, cfg, values, aliveRange, lval, start):
      Owned.yes
    else:
      Owned.no
  else:
    unreachable()

func solveOwnership(tree: MirTree, cfg: ControlFlowGraph, values: var Values,
                    entities: EntityDict) =
  ## Computes for all lvalues used in consume context whether they're owning
  ## or not. `values` is updated with the results.
  # search for 'consume' instructions and compute for their operands whether
  # it's a handle that owns the location's value
  for i, op, opr in cfg.instructions:
    if op == opConsume:
      if hasDestructor(tree[opr].typ):
        # unresolved onwership status and has a destructors
        values.setOwned(opr):
          computeOwnership(tree, cfg, values, entities,
                           computePath(tree, NodePosition opr), i+1)
      else:
        # later analysis expects the status to be set for all lvalues
        # appearing in consume contexts. Use `no`, but `yes` would work
        # too
        values.setOwned(opr): Owned.no

    else:
      discard "nothing to do"

type DestructionMode = enum
  demNone    ## location doesn't need to be destroyed because it contains no
             ## value when control-flow exits the enclosing scope
  demNormal  ## the location contains a value when the scope is exited via
             ## structured control-flow
  demFinally ## the location contains a value when the scope is exited via
             ## unstructured control-flow

func requiresDestruction(tree: MirTree, cfg: ControlFlowGraph, values: Values,
                         span: Slice[InstrPos], def: NodePosition,
                         entity: MirNode): DestructionMode =
  template computeAlive(loc, op: untyped): untyped =
    computeAlive(tree, cfg, values, span, loc, op)

  let r =
    case entity.kind
    of mnkParam, mnkLocal, mnkGlobal:
      computeAlive(entity.sym, computeAliveOp[PSym])
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

func computeDestructors(tree: MirTree, cfg: ControlFlowGraph, values: Values,
                        entities: EntityDict): seq[(NodePosition, bool)] =
  ## Computes and collects which locations present in `entities` need to be
  ## destroyed at the exit of their enclosing scope in order to prevent the
  ## values they still store from staying alive.
  ##
  ## Special handling is required if the scope is exited via unstructured
  ## control-flow while the location is still alive (its value is then said
  ## to "escape")
  for _, info in entities.pairs:
    let
      def = info.def ## the position of the entity's definition
      entity = tree[getDefEntity(tree, def)]

    if entity.kind == mnkGlobal and
       doesGlobalEscape(tree, info.scope, info.scope.a, entity.sym):
      # TODO: handle escaping globals. Either report a warning, an error, or
      #       defer destruction of the global to the end of the program
      discard

    case requiresDestruction(tree, cfg, values, info.scope, def, entity)
    of demNormal:
      result.add (def, false)
    of demFinally:
      result.add (def, true)
    of demNone:
      discard

# --------- analysis routines --------------

func isAlive(tree: MirTree, cfg: ControlFlowGraph, v: Values,
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
        cfg.rangeFor(NodePosition(0) .. NodePosition(tree.high))
      else:
        var exists: bool
        let s = entities.getAliveRange(toName(tree[root]), exists)
        if exists: s
        else:      return true # not something we can analyse -> assume alive

    if at-1 in scope: isAlive(tree, cfg, v, scope, val, at)
    else:             false
  else:
    # something that we can't analyse (e.g. a dereferenced pointer). We have
    # to be conservative and assume that the location the lvalue names already
    # stores a value
    true

func needsReset(tree: MirTree, cfg: ControlFlowGraph, ar: AnalysisResults,
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

  let info = getOrDefault(ar.entities[], toName(tree[root]),
                          EntityInfo(def: NodePosition -1))

  if info.def == NodePosition(-1):
    # the entity needs can't be reasoned about in the current context -> assume
    # that it needs to be reset
    return true

  let res = isLastWrite(tree, cfg, ar.v[], info.scope, src, at)

  if res.result:
    if res.escapes or res.exits:
      let def = info.def
      assert tree[def].kind in DefNodes

      # check if there exists a destructor call that would observe the
      # location's value:
      for it in ar.destroy[].items:
        if def == it[0]:
          if (it[1] and res.escapes) or res.exits:
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

func isOwned(tree: MirTree, v: Values, n: NodePosition): Owned =
  ## Returns whether expression `n` is owning.
  case tree[n].kind:
  of LvalueExprKinds - {mnkDeref, mnkDerefView}:
    v.owned(OpValue n)
  of mnkDeref, mnkDerefView:
    Owned.no
  of mnkLiteral, mnkProc, mnkType:
    Owned.yes
  of mnkConv, mnkStdConv, mnkCast:
    # the result of these operations is not an owned value
    Owned.no
  of mnkConstr, mnkCall, mnkMagic, mnkObjConstr:
    Owned.yes
  of AllNodeKinds - ExprKinds:
    unreachable(tree[n].kind)

# ------- code generation routines --------

template buildVoidCall(bu: var MirBuilder, prc: PSym, body: untyped) =
  bu.subTree mnkVoid:
    bu.buildCall prc, getVoidType(graph):
      body

proc genWasMoved(bu: var MirBuilder, graph: ModuleGraph, target: Value) =
  bu.subTree MirNode(kind: mnkVoid):
    bu.buildMagicCall mWasMoved, getVoidType(graph):
      bu.emitByName(target, ekKill)

proc genDestroy*(bu: var MirBuilder, graph: ModuleGraph, target: Value) =
  let destr = getOp(graph, target.typ, attachedDestructor)

  bu.buildVoidCall(destr):
    bu.emitByName(target, ekMutate)

proc genInjectedSink(bu: var MirBuilder, graph: ModuleGraph,
                     dest, source: Value) =
  ## Generates either a call to the ``=sink`` hook, or (if none exists), a
  ## sink emulated via a destructor-call + bitwise-copy.
  let op = getOp(graph, dest.typ, attachedSink)
  if op != nil:
    bu.buildVoidCall(op):
      bu.emitByName(dest, ekMutate)
      bu.emitByVal source
  else:
    # without a sink hook, a ``=destroy`` + blit-copy is used
    genDestroy(bu, graph, dest)
    bu.asgn dest, source

proc genSinkFromTemporary(bu: var MirBuilder, graph: ModuleGraph,
                          dest, source: Value) =
  ## Similar to ``genInjectedSink`` but generates code for destructively
  ## moving the source operand into a temporary first.
  let tmp = bu.materialize(source)
  genWasMoved(bu, graph, source)
  genInjectedSink(bu, graph, dest, tmp)

proc genCopy(bu: var MirBuilder, graph: ModuleGraph,
             dst, src: Value, maybeCyclic: bool) =
  ## Emits a ``=copy`` hook call with `dst`, `src`, and (if necessary)
  ## `maybeCyclic` as the arguments
  let
    t  = dst.typ
    op = getOp(graph, t, attachedAsgn)
  assert op != nil

  bu.buildVoidCall(op):
    bu.emitByName(dst, ekMutate)
    bu.emitByVal src

    if graph.config.selectedGC == gcOrc and
        cyclicType(t.skipTypes(skipAliases + {tyDistinct}), graph):
      # pass whether the copy can potentially introduce cycles as the third
      # parameter:
      bu.emitByVal literal(boolLit(graph, unknownLineInfo, maybeCyclic))

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

proc expandAsgn(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
                stmt: NodePosition, pos: InstrPos,
                c: var Changeset) =
  ## Expands an assignment into either a copy, move, or destructive move.
  ## `stmt` is the assignment statement node and `pos` is the 'def' data-flow
  ## instruction corresponding to it.
  let
    dest = NodePosition operand(tree, stmt, 0)
    source = NodePosition operand(tree, stmt, 1)
    sourcePath = computePath(tree, source)
    destPath   = computePath(tree, dest)
    relation   = compare(tree, sourcePath, destPath)

  if relation.isSame:
    # a self-assignment -> elide
    c.remove(tree, stmt)
  elif isOwned(tree, ar.v[], source) == Owned.yes:
    # we own the source value -> sink
    if true:
      let fromLvalue = isNamed(tree, OpValue source)

      if tree[stmt].kind != mnkInit and
         isAlive(tree, ctx.cfg, ar.v[], ar.entities[], destPath, pos):
        # there already exists a value in the destination location -> use the
        # sink operation
        if fromLvalue:
          c.replaceMulti(tree, stmt, bu):
            let a = bu.bindMut(tree, dest)
            if isAPartOfB(relation) != no:
              # this is a potential part-to-whole assignment, e.g.: ``x = x.y``.
              # We need to move the source value into a temporary first, as
              # ``=sink`` would otherwise destroy ``x`` first, also destroying
              # ``x.y`` in the process
              let b = bu.bindImmutable(tree, source)
              genSinkFromTemporary(bu, ctx.graph, a, b)
            elif needsReset(tree, ctx.cfg, ar, sourcePath, pos):
              # a sink from a location that needs to be reset after the move
              # (i.e., a destructive move)
              let (b, clear) = bu.destructiveMoveOperands(tree, source)
              genInjectedSink(bu, ctx.graph, a, b)
              genWasMoved(bu, ctx.graph, clear)
            else:
              # a sink from a location that doesn't need to be cleared after
              let b = bu.bindImmutable(tree, source)
              genInjectedSink(bu, ctx.graph, a, b)

        else:
          # this is a bit hack-y, but in order to support changes within the
          # second operand's tree, the assignment is not replaced as a whole
          # but rather turned into a def statement. ``a.x = f(arg 1)`` becomes:
          #   def _1 = f(arg 1)
          #   bind_mut _2 = a.x
          #   =sink(name _2, arg _1)
          # XXX: this is going to become cleaner once `mirgen` handles most of
          #      the sink-related transformations
          var tmp: Value
          c.replaceSingle(stmt, MirNode(kind: mnkDef))
          c.replaceMulti(tree, dest, bu):
            # replace the destination operand with the name of a newly
            # allocated temporary
            tmp = bu.allocTemp(tree[source].typ)
            bu.use tmp

          c.insert(tree, tree.sibling(stmt), stmt, bu):
            # the value is only accessible through the source expression, a
            # destructive move is not required
            let a = bu.bindMut(tree, dest)
            genInjectedSink(bu, ctx.graph, a, tmp)

      else:
        # the destination location doesn't contain a value yet (which would
        # need to be destroyed first otherwise) -> a bitwise copy can be used
        if fromLvalue and needsReset(tree, ctx.cfg, ar, sourcePath, pos):
          # we don't need to check for part-to-whole assignments here, because
          # if the destination location has no value, so don't locations derived
          # from it, in which case it doesn't matter when the reset happens
          # XXX: the reset could be omitted for part-to-whole assignments
          c.replaceMulti(tree, stmt, bu):
            let
              a          = bu.bindMut(tree, dest)
              (b, clear) = bu.destructiveMoveOperands(tree, source)
            bu.asgn a, b
            genWasMoved(bu, ctx.graph, clear)

        else:
          # no hook call nor destructive move is required
          discard

  else:
    # the source value isn't owned -> copy
    c.replaceMulti(tree, stmt, bu):
      # copies to locals or globals can't introduce cyclic structures, as
      # those are standlone and not part of any other structure
      let maybeCyclic =
        tree[dest].kind notin {mnkLocal, mnkTemp, mnkParam, mnkGlobal}
      let
        a = bu.bindMut(tree, dest)
        b = bu.inline(tree, source)

      genCopy(bu, ctx.graph, a, b, maybeCyclic)

proc expandDef(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
               at: NodePosition, pos: InstrPos, c: var Changeset) =
  ## Depending on whether the source can be moved out of, either rewrites the
  ## 'def' at `at` into a call to the ``=copy`` hook or into a destructive
  ## move. If the source can be moved out of non-destructively, nothing is
  ## changed. `src` is the data-flow instruction
  let
    dest   = NodePosition tree.operand(at, 0)
    source = NodePosition tree.operand(at, 1)
  case isOwned(tree, ar.v[], source)
  of Owned.no:
    # a copy is required. Transform ``def x = a.b`` into:
    #   def x
    #   bind _1 = a.b
    #   =copy(name x, arg _1)
    c.replace(tree, source): MirNode(kind: mnkNone)
    c.insert(tree, tree.sibling(at), source, bu):
      let
        a = bu.bindMut(tree, dest)
        b = bu.inline(tree, source)
      # the destination can only be a cell-like location (local, global,
      # etc.), no cycle can possibly be introduced
      genCopy(bu, ctx.graph, a, b, false)
  of Owned.yes:
    if isNamed(tree, OpValue source) and
       needsReset(tree, ctx.cfg, ar, computePath(tree, source), pos):
      # the value can be moved, but the location needs to be reset. Transform
      # ``def x = a.b`` into:
      #   bind_mut _1 = a.b
      #   def x = _1
      #   wasMoved(name x)
      var tmp, clear: Value
      c.insert(tree, at, source, bu):
        (tmp, clear) = bu.destructiveMoveOperands(tree, source)
      c.replace(tree, source): tmp.node
      c.insert(tree, tree.sibling(at), source, bu):
        genWasMoved(bu, ctx.graph, clear)
  else:
    unreachable()

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

    if (tree[expr].kind == mnkCall and geRaises in tree[expr].effects) or
       (tree[expr].kind == mnkMagic and tree[expr].magic in magicsThatCanRaise):
      # the consumer raises, meaning that resetting the consumed-from location
      # cannot happen *after* the statement. The source location's value is
      # first assigned to a temporary and then the source is reset
      var tmp: Value
      c.insert(tree, stmt, NodePosition src, bu):
        let v = bu.bindMut(tree, NodePosition src)
        tmp = bu.materialize(v)
        genWasMoved(bu, ctx.graph, v)

      # replace the argument with the injected temporary:
      c.replaceMulti(tree, NodePosition src, bu):
        bu.use tmp
    else:
      # the reset can happen after the statement
      c.insert(tree, tree.sibling(stmt), NodePosition src, bu):
        let v = bu.bindMut(tree, NodePosition src)
        genWasMoved(bu, ctx.graph, v)

proc isUsedForSink(tree: MirTree, stmt: NodePosition): bool =
  ## Tests whether the definition statement is something produced for sink
  ## parameter handling.
  assert tree[stmt].kind in {mnkDef, mnkDefUnpack}
  let def = tree.operand(stmt, 0)
  if tree[def].kind != mnkTemp:
    # only temporaries are used for sink handling
    return

  # look for whether the temporary is used as a 'consume' node's operand,
  # but do reduce the amount of work by not searching beyond the
  # temporary's lifetime
  # HACK: this detection relies on the code shapes ``mirgen`` currently
  #       emits for sink parameters and is thus very brittle. The proper
  #       solution is to mark through a side channel the statement as being
  #       generated for a sink parameter
  var
    n = tree.sibling(stmt)
    depth = 0
  while n < NodePosition tree.len:
    case tree[n].kind
    of mnkConsume:
      let x = tree.operand(n)
      if tree[x].kind == mnkTemp and tree[x].temp == tree[def].temp:
        # the temporary is used for sink parameter passing
        result = true
        break
    of mnkScope:
      inc depth
    of mnkEnd:
      if tree[n].kind == mnkScope:
        dec depth
        if depth < 0:
          # the end of the temporary's surrounding scope is reached
          break
    else:
      discard

    inc n

proc checkCopy(graph: ModuleGraph, tree: MirTree, expr: NodePosition,
               diags: var seq[LocalDiag]) =
  let op = getOp(graph, tree[expr].typ, attachedAsgn)
  if sfError in op.flags:
    diags.add LocalDiag(pos: expr,
                        kind: ldkUnavailableTypeBound,
                        op: attachedAsgn)

proc rewriteAssignments(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
                        diags: var seq[LocalDiag], c: var Changeset) =
  ## Rewrites assignments to locations into calls to either the ``=copy``
  ## or ``=sink`` hook (see ``expandAsgn`` for more details), using the
  ## previously computed ownership information to decide.
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
      of mnkAsgn, mnkInit, mnkDef, mnkDefUnpack:
        # assignments are handled separately
        discard
      else:
        unreachable(tree[parent].kind)
    elif opc == opDef and hasDestructor(tree[val].typ):
      # where necessary, rewrite assignments into moves, destructive moves,
      # and copies
      let stmt = tree.parent(NodePosition val)

      case tree[stmt].kind
      of mnkDef, mnkDefUnpack:
        let src = NodePosition tree.operand(stmt, 1)
        # ignore definitions with no initializer
        if tree[src].kind != mnkNone:
          if isOwned(tree, ar.v[], src) == Owned.no:
            checkCopy(ctx.graph, tree, src, diags)
            # emit a warning for copies-to-sink
            if isUsedForSink(tree, stmt):
              diags.add LocalDiag(kind: ldkPassCopyToSink,
                                  pos: src)
          expandDef(tree, ctx, ar, stmt, i, c)
      of mnkAsgn, mnkInit:
        let src = NodePosition tree.operand(stmt, 1)
        if isOwned(tree, ar.v[], src) == Owned.no:
          checkCopy(ctx.graph, tree, src, diags)
        expandAsgn(tree, ctx, ar, stmt, i, c)
      else:
        # e.g., output arguments to procedures
        discard "ignore"

# --------- destructor injection -------------

type DestroyEntry = tuple
  pos: NodePosition   ## the position of the 'def' belonging to the entity that
                      ## requires destruction
  scope: NodePosition ## the position of the enclosing 'scope'

proc injectDestructorsInner(bu: var MirBuilder, orig: MirTree, graph: ModuleGraph,
                            entries: openArray[DestroyEntry]) =
  ## Generates a destructor call for each item in `entries`, using `buf` as the
  ## output.
  for pos, _ in ritems(entries):
    let def = getDefEntity(orig, pos)
    let t =
      case orig[def].kind
      of SymbolLike: orig[def].sym.typ
      of mnkTemp:    orig[def].typ
      else:          unreachable()

    bu.buildVoidCall(getOp(graph, t, attachedDestructor)):
      bu.emitByName(Value(node: orig[def]), ekMutate)

proc injectDestructors(tree: MirTree, graph: ModuleGraph,
                       destroy: seq[(NodePosition, bool)], c: var Changeset) =
  ## Injects a destructor call for each entity in the `destroy` list, in the
  ## entities reverse order they are defined. That is the entity defined last
  ## is destroyed first
  if destroy.len == 0:
    # nothing to do
    return

  var
    needsFinally: PackedSet[NodePosition]
    entries: seq[DestroyEntry]

  # first pass: setup the `entries` list and collect the scopes that need to
  # be wrapped in a ``finally``
  for pos, escapes in destroy.items:
    assert tree[pos].kind == mnkDef
    let scopeStart = findParent(tree, pos, mnkScope)

    if escapes:
      needsFinally.incl scopeStart

    entries.add (pos: pos, scope: scopeStart)

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
            injectDestructorsInner(buf, tree, graph,
                                   toOpenArray(entries, s.a, s.b))

        buf.add endNode(mnkTry)
      else:
        injectDestructorsInner(buf, tree, graph,
                               toOpenArray(entries, s.a, s.b))

proc lowerBranchSwitch(bu: var MirBuilder, body: MirTree, graph: ModuleGraph,
                       idgen: IdGenerator, op: Operation) =
  ## Lowers a 'switch' operation into a simple discriminant assignment plus
  ## the logic for destroying the previous branch (if necessary)
  assert body[op].kind == mnkSwitch

  let
    target = operand(body, NodePosition op, 0)
    objType = body[target].typ
    typ = body[target].field.typ

  let a = bu.bindMut(body, NodePosition target)
  let b = bu.inline(body, NodePosition body.operand(NodePosition op, 1))

  assert body[target].kind == mnkPathVariant

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
      bu.buildVoidCall(branchDestructor):
        bu.emitByName(a, ekInvalidate)

  else:
    # the object doesn't need destruction, which means that neither does one
    # of the branches. We can just change the discriminant
    # XXX: this differs from what the VM does. For the VM, switching branches
    #      resets the memory of the record-case back to zero, independent of
    #      whether a branch contains managed memory or not
    discard

  # generate the ``discriminant = newValue`` assignment:
  bu.asgn(a, b)

proc reportDiagnostics(g: ModuleGraph, tree: MirTree, sourceMap: SourceMap,
                       owner: PSym, diags: var seq[LocalDiag]) =
  ## Reports all diagnostics in `diags` as ``SemReport``s and clear the list
  for diag in diags.items:
    let ast = sourceMap[tree[diag.pos].info]
    let rep =
      case diag.kind
      of ldkUnavailableTypeBound:
        SemReport(kind: rsemUnavailableTypeBound,
                  typ: tree[diag.pos].typ,
                  str: AttachedOpToStr[diag.op],
                  ast: ast,
                  sym: owner)
      of ldkPassCopyToSink:
        SemReport(kind: rsemCopiesToSink, ast: ast)

    localReport(g.config, ast.info, rep)

func shouldInjectDestructorCalls*(owner: PSym): bool =
  # only inject destructor calls if the owner is not a generated OP (e.g. a
  # generated ``=destroy``) and also not an ``.inline`` iterator
  result =
     {sfInjectDestructors, sfGeneratedOp} * owner.flags == {sfInjectDestructors} and
     (owner.kind != skIterator or not isInlineIterator(owner.typ))

proc injectDestructorCalls*(g: ModuleGraph; idgen: IdGenerator; owner: PSym;
                            tree: var MirTree, sourceMap: var SourceMap) =
  ## The ``injectdestructors`` pass entry point. The pass is made up of
  ## multiple sub-passes, hence the mutable `tree` and `sourceMap` (as opposed
  ## to returning a ``Changeset``).
  ##
  ## For now, semantic errors and other diagnostics related to lifetime-hook
  ## usage are also reported here.

  template apply(c: Changeset) =
    ## Applies the changeset `c` to `tree`.
    apply(tree, prepare(c))

  # apply the first batch of passes:
  block:
    var changes = initChangeset(tree)
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
      for i, n in tree.pairs:
        if n.kind == mnkSwitch:
          changes.replaceMulti(tree, i, buf):
            lowerBranchSwitch(buf, tree, g, idgen, Operation i)

    apply(changes)

  # apply the second batch of passes:
  block:
    var
      changes = initChangeset(tree)
      diags: seq[LocalDiag]

    let
      actx = AnalyseCtx(graph: g, cfg: computeCfg(tree))
      entities = initEntityDict(tree, actx.cfg)

    var values: Values
    solveOwnership(tree, actx.cfg, values, entities)

    let destructors = computeDestructors(tree, actx.cfg, values, entities)

    rewriteAssignments(
      tree, actx,
      AnalysisResults(v: cursor(values),
                      entities: cursor(entities),
                      destroy: cursor(destructors)),
      diags, changes)

    # turn the collected diagnostics into reports and report them:
    reportDiagnostics(g, tree, sourceMap, owner, diags)

    injectDestructors(tree, g, destructors, changes)

    apply(changes)

  if g.config.arcToExpand.hasKey(owner.name.s):
    g.config.msgWrite("--expandArc: " & owner.name.s & "\n")
    g.config.msgWrite(render(tree))
    g.config.msgWrite("\n-- end of expandArc ------------------------\n")