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
    sourcemaps
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

# XXX: we shouldn't need to be concerned with rendering backend-
#      IR to text here
from compiler/backend/cgirutils import render
from compiler/backend/cgirgen import generateIR

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
    scope: Slice[NodePosition] ## the scope the entity is defined in

  EntityDict = Table[EntityName, EntityInfo]
    ## Entity dictionary. Stores all entities relevant to destructor
    ## injection and the move analyser

  AnalysisResults = object
    ## Bundled-up immutable state needed for assignment rewriting. Since
    ## they're immutable, ``Cursor``s are used in order to not copy
    # XXX: ideally, views types (i.e. ``lent``) would be used here
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

  Lvalue = distinct OpValue
    ## An ``OpValue`` that names a location

const
  skipAliases = {tyGenericInst, tyAlias, tySink}
    ## the set of types to not consider when looking up a type-bound operator

iterator ritems[T](x: openArray[T]): lent T =
  ## Iterates and yields the items from the container `x` in reverse
  var i = x.high
  while i >= 0:
    yield x[i]
    dec i

func conv[A, B](x: Slice[A], _: typedesc[B]): Slice[B] {.inline.} =
  B(x.a) .. B(x.b)

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
                  ): Slice[NodePosition] =
  ## Returns the maximum lifespan of the entity with the given `name`.
  ## `exists` is used to output whether there exists an entity with the given
  ## `name` in `entities`
  let info =
    entities.getOrDefault(name, EntityInfo(scope: conv(1..0, NodePosition)))

  exists = info.scope.a <= info.scope.b
  if exists:
    # the entity is not alive before its definition, hence the usage of
    # ``info.def`` for the start and not ``info.scope.b``
    result = info.def .. info.scope.b

func paramType(p: PSym, i: Natural): PType =
  assert p.kind in routineKinds
  p.typ[1 + i]

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

proc needsMarkCyclic(graph: ModuleGraph, typ: PType): bool =
  # skip distinct types too so that a ``distinct ref`` also gets marked as
  # cyclic at runtime
  graph.config.selectedGC == gcOrc and
  cyclicType(typ.skipTypes(skipAliases + {tyDistinct}), graph)

func isNamed(tree: MirTree, v: Values, val: OpValue): bool =
  ## Returns whether `val` is an lvalue that names a location derived from
  ## a named entity. For example, ``local.a.b`` is such a location.
  tree[v.getRoot(val)].kind in {mnkLocal, mnkGlobal, mnkParam, mnkTemp}

func getDefEntity(tree: MirTree, n: NodePosition): NodePosition =
  assert tree[n].kind in DefNodes
  n + 1

func skipTag(tree: MirTree, n: Operation): OpValue =
  ## Returns the input to the tag operation `n`
  assert tree[n].kind == mnkTag
  tree.operand(n)

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

func initEntityDict(tree: MirTree): EntityDict =
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
        result[re] = EntityInfo(def: i, scope: scope)

    else:
      discard

func computeOwnership(tree: MirTree, cfg: ControlFlowGraph, values: Values,
                      entities: EntityDict, lval: LvalueExpr, pos: NodePosition
                     ): Owned =
  case tree[lval.root].kind
  of mnkObjConstr, mnkConstr, mnkMagic, mnkCall:
    # all values derived from a constructor-operation that reach here are
    # guaranteed to own (see ``analyser.computeValuesAndEffects``).
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

    if exists and isLastRead(tree, cfg, values, aliveRange, lval, pos):
      Owned.yes
    else:
      Owned.no
  else:
    unreachable()

func solveOwnership(tree: MirTree, cfg: ControlFlowGraph, values: var Values,
                    entities: EntityDict) =
  ## Ensures that the ownership status of values used in a consume context is
  ## certain (i.e. either owned or not owned)
  # we're only interested about the ownership status of values used in a consume
  # context
  for i, n in tree.pairs:
    case n.kind
    of ConsumeCtx:
      let opr = tree.operand(i)

      if values.owned(opr) in {unknown, weak} and hasDestructor(tree[opr].typ):
        # unresolved onwership status and has a destructors
        values.setOwned(opr):
          computeOwnership(tree, cfg, values, entities,
                           values.toLvalue(opr), i)

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
                         span: Slice[NodePosition], def: Operation,
                         entity: MirNode): DestructionMode =
  template computeAlive(loc: untyped, hasInit: bool, op: untyped): untyped =
    computeAlive(tree, cfg, values, span, loc, hasInit, op)

  # XXX: a 'def' is not an operation. It defines an entity, optionally with a
  #      starting value, but doesn't produce a value itself

  let r =
    case entity.kind
    of mnkParam, mnkLocal, mnkGlobal:
      # ``sink`` parameter locations always start with an initial value
      computeAlive(entity.sym, (entity.kind == mnkParam or hasInput(tree, def)),
                   computeAliveOp[PSym])

    of mnkTemp:
      # unpacked tuples don't need to be destroyed because all elements are
      # moved out of them
      if tree[def].kind != mnkDefUnpack:
        computeAlive(entity.temp, hasInput(tree, def),
                     computeAliveOp[TempId])
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
      start = sibling(tree, def)
      entity = tree[getDefEntity(tree, def)]
      scope = start .. info.scope.b

    if entity.kind == mnkGlobal and
       doesGlobalEscape(tree, scope, start, entity.sym):
      # TODO: handle escaping globals. Either report a warning, an error, or
      #       defer destruction of the global to the end of the program
      discard

    case requiresDestruction(tree, cfg, values, scope, Operation def, entity)
    of demNormal:
      result.add (def, false)
    of demFinally:
      result.add (def, true)
    of demNone:
      discard

# --------- analysis routines --------------

func isAlive(tree: MirTree, cfg: ControlFlowGraph, v: Values,
             entities: EntityDict, val: Lvalue): bool =
  ## Computes if `val` refers to a location that contains a value at the point
  ## in time where `val` is computed
  let
    pos = NodePosition(val)
    root = v.getRoot(OpValue val)

  case tree[root].kind
  of mnkLocal, mnkParam, mnkGlobal, mnkTemp:
    let scope =
      # XXX: the way the ``result`` variable is detected here is a hack. It
      #      should be treated as any other local in the context of the MIR
      if tree[root].kind in SymbolLike and tree[root].sym.kind == skResult:
        NodePosition(0) .. NodePosition(tree.high)
      else:
        var exists: bool
        let s = entities.getAliveRange(toName(tree[root]), exists)
        if exists: s
        else:      return true # not something we can analyse -> assume alive

    isAlive(tree, cfg, v, scope, (NodePosition root, pos), pos)
  else:
    # something that we can't analyse (e.g. a dereferenced pointer). We have
    # to be conservative and assume that the location the lvalue names already
    # stores a value
    true

func needsReset(tree: MirTree, cfg: ControlFlowGraph, ar: AnalysisResults,
                src: Lvalue): bool =
  ## Computes whether a reset needs to be injected for `src` in order to
  ## prevent the current value the underlying location contains from being
  ## observed.
  ##
  ## This is relevant for when ownership of a value is transferred, as the
  ## transferral doesn't imply a change to neither the previous owner
  ## (location) nor the value itself. As long as the location is not observed
  ## to still contain the value it now no longer owns, this is not a problem.
  ## If it can't be proven that the unowned value is observed (which could
  ## cause problems like, for example, double-frees), the location is
  ## explicitly reset (i.e. the value removed from it).
  let root = ar.v[].getRoot(OpValue src)
  # XXX: the way the ``result`` variable is detected here is a hack. It
  #      should be treated as any other local in the context of MIR. The
  #      fact that the result variable is potentially used outside the
  #      procedure's body should be encoded by inserting a special 'use'
  #      operation that has a control-flow dependency on *all* other
  #      operations
  if tree[root].kind in SymbolLike and tree[root].sym.kind == skResult:
    return true

  var exists = false
  let aliveRange = ar.entities[].getAliveRange(toName(tree[root]), exists)

  if not exists:
    # the entity needs can't be reasoned about in the current context -> assume
    # that it needs to be reset
    return true

  let res = isLastWrite(tree, cfg, ar.v[], aliveRange,
                        toLvalue(ar.v[], OpValue src), NodePosition(src) + 1)

  if res.result:
    if res.escapes or res.exits:
      let def = aliveRange.a
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

# ------- code generation routines --------

func undoConversions(buf: var MirNodeSeq, tree: MirTree, src: OpValue) =
  ## When performing a destructive move for ``ref`` values, it's possible for
  ## the source to be an lvalue conversion -- in that case, we want pass the
  ## uncoverted root location to the ``wasMoved`` operation. To do so, we apply
  ## the conversions in *reverse*. ``cgirgen`` detects this pattern and removes
  ## the conversions that cancel each other out.
  var p = NodePosition(src)
  while tree[p].kind == mnkPathConv:
    p = previous(tree, p)
    buf.add MirNode(kind: mnkPathConv, typ: tree[p].typ)

template voidCallWithArgs(buf: var MirNodeSeq, body: untyped) =
  argBlock(buf):
    body
  buf.add MirNode(kind: mnkCall, typ: getVoidType(graph))
  buf.add MirNode(kind: mnkVoid)

func genDefTemp(buf: var MirNodeSeq, id: TempId, typ: PType) =
  buf.subTree MirNode(kind: mnkDef):
    buf.add MirNode(kind: mnkTemp, typ: typ, temp: id)

template genWasMoved(buf: var MirNodeSeq, graph: ModuleGraph, target: EValue) =
  argBlock(buf):
    chain(buf): target => tag(ekKill) => arg()
  chain(buf): magicCall(mWasMoved, getVoidType(graph)) => voidOut()

proc genDestroy*(buf: var MirNodeSeq, graph: ModuleGraph, t: PType,
                 target: sink MirNode) =
  let destr = getOp(graph, t, attachedDestructor)

  voidCallWithArgs(buf):
    chain(buf): procLit(destr) => arg()
    chain(buf): emit(target) => tag(ekMutate) => arg()

proc genInjectedSink(buf: var MirNodeSeq, graph: ModuleGraph, t: PType,
                     source: sink MirNode) =
  ## Generates either a call to the ``=sink`` hook, or (if none exists), a
  ## sink emulated via a destructor-call + bitwise-copy. The output is meant
  ## to be placed inside a region.
  let op = getOp(graph, t, attachedSink)
  if op != nil:
    voidCallWithArgs(buf):
      chain(buf): procLit(op) => arg()
      chain(buf): opParam(0, t) => tag(ekMutate) => arg()
      chain(buf): emit(source) => arg()
  else:
    # without a sink hook, a ``=destroy`` + blit-copy is used
    genDestroy(buf, graph, t, opParamNode(0, t))

    argBlock(buf):
      chain(buf): opParam(0, t) => arg()
      chain(buf): emit(source) => arg()
    buf.add MirNode(kind: mnkFastAsgn)

proc genInjectedSink(buf: var MirNodeSeq, graph: ModuleGraph, t: PType) =
  genInjectedSink(buf, graph, t):
    opParamNode(1, t)

proc genSinkFromTemporary(buf: var MirNodeSeq, graph: ModuleGraph, t: PType,
                          tmp: TempId) =
  ## Similar to ``genInjectedSink`` but generates code for destructively
  ## moving the source operand into a temporary first
  buf.add opParamNode(1, t)
  buf.genDefTemp(tmp, t)

  genWasMoved(buf, graph):
    opParam(buf, 1, t)

  genInjectedSink(buf, graph, t):
    MirNode(kind: mnkTemp, typ: t, temp: tmp)

proc genCopy(buf: var MirTree, graph: ModuleGraph, t: PType,
             dst, src: sink MirNode, maybeCyclic: bool) =
  ## Emits a ``=copy`` hook call from with `dst`, `src`, and (if necessary)
  ## `maybeCyclic` as the arguments
  let op = getOp(graph, t, attachedAsgn)
  assert op != nil

  voidCallWithArgs(buf):
    chain(buf): procLit(op) => arg()
    chain(buf): emit(dst) => tag(ekMutate) => arg()
    chain(buf): emit(src) => arg()

    if graph.config.selectedGC == gcOrc and
        cyclicType(t.skipTypes(skipAliases + {tyDistinct}), graph):
      # pass whether the copy can potentially introduce cycles as the third
      # parameter:
      chain(buf): literal(boolLit(graph, unknownLineInfo, maybeCyclic)) => arg()

proc genMarkCyclic(buf: var MirTree, graph: ModuleGraph, typ: PType,
                   dest: sink MirNode) =
  ## Emits a call to ``nimMarkCyclic`` for `dest` if required by its `typ`
  if graph.config.selectedGC == gcOrc:
    # also skip distinct types so that a ``distinct ref`` gets marked as
    # cyclic too
    let t = typ.skipTypes(skipAliases + {tyDistinct})
    if cyclicType(t, graph):
      voidCallWithArgs(buf):
        chain(buf): procLit(getCompilerProc(graph, "nimMarkCyclic")) => arg()
        # for closures, only the environment needs to be marked as potentially
        # cyclic
        chain(buf): emit(dest) => predicate(t.kind == tyProc) =>
          unaryMagicCall(mAccessEnv,
                         getSysType(graph, unknownLineInfo, tyPointer)) =>
          arg()

proc expandAsgn(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
                typ: PType, source: OpValue, asgn: Operation,
                c: var Changeset) =
  ## Expands an assignment into either a copy or move
  let
    dest = skipTag(tree, operand(tree, asgn, 0))
    relation = compareLvalues(tree, toLvalue(ar.v[], source),
                              toLvalue(ar.v[], dest))

  c.seek(NodePosition asgn)

  if relation.isSame:
    # a self-assignment. We can't remove the arg-block (it might have
    # side-effects), so the assignment is replaced with a
    # no-op instead
    c.replaceMulti(buf):
      buf.subTree MirNode(kind: mnkRegion): discard

  elif owned(ar.v[], source) == Owned.yes:
    # we own the source value -> sink
    c.replaceMulti(buf):
      let fromLvalue = isNamed(tree, ar.v[], source)

      if tree[asgn].kind != mnkInit and
         isAlive(tree, ctx.cfg, ar.v[], ar.entities[], Lvalue dest):
        # there already exists a value in the destination location -> use the
        # sink operation
        buf.subTree MirNode(kind: mnkRegion):
          if fromLvalue:
            if isAPartOfB(relation) != no:
              # this is a potential part-to-whole assignment, e.g.: ``x = x.y``.
              # We need to move the source value into a temporary first, as
              # ``=sink`` would otherwise destroy ``x`` first,  also destroying
              # ``x.y`` in the process
              genSinkFromTemporary(buf, ctx.graph, typ, c.getTemp())
            else:
              genInjectedSink(buf, ctx.graph, typ)

              if needsReset(tree, ctx.cfg, ar, Lvalue source):
                genWasMoved(buf, ctx.graph):
                  buf.add opParamNode(1, typ)
                  undoConversions(buf, tree, source)
                  EValue(typ: buf[^1].typ)

          else:
            # the value is only accessible through the source expression, a
            # destructive move is not required
            genInjectedSink(buf, ctx.graph, typ)

      else:
        # the destination location doesn't contain a value yet (which would
        # need to be destroyed first otherwise) -> a bitwise copy can be used
        if fromLvalue:
          # we don't need to check for part-to-whole assignments here, because
          # if the destination location has no value, so don't locations derived
          # from it, in which case it doesn't matter when the reset happens
          buf.subTree MirNode(kind: mnkRegion):
            argBlock(buf):
              chain(buf): opParam(0, typ) => arg()
              chain(buf): opParam(1, typ) => arg()

            buf.add MirNode(kind: mnkFastAsgn)

            # XXX: the reset could be omitted for part-to-whole assignments
            if needsReset(tree, ctx.cfg, ar, Lvalue source):
              genWasMoved(buf, ctx.graph):
                buf.add opParamNode(1, typ)
                undoConversions(buf, tree, source)
                EValue(typ: buf[^1].typ)

        else:
          # no hook call nor destructive move is required
          buf.add MirNode(kind: mnkFastAsgn)

  else:
    # we don't own the source value -> copy
    c.replaceMulti(buf):
      # copies to locals or globals can't introduce cyclic structures, as
      # those are standlone and not part of any other structure
      let maybeCyclic =
        tree[dest].kind notin {mnkLocal, mnkTemp, mnkParam, mnkGlobal}

      buf.subTree MirNode(kind: mnkRegion):
        genCopy(buf, ctx.graph, typ,
                opParamNode(0, typ),
                opParamNode(1, typ),
                maybeCyclic)

proc consumeArg(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
                typ: PType, src: OpValue, c: var Changeset) =
  ## Injects the ownership-transfer related logic needed for when a value is
  ## consumed. Since the value is not passed by reference to the ``sink``
  ## parameter, the source location has to be reset, as it'd otherwise contain
  ## a value that it no longer owns, while the rest of the code still operates
  ## under the assumption that it owns the value.
  if isNamed(tree, ar.v[], src):
    let
      markCyclic = needsMarkCyclic(ctx.graph, typ)
      reset = needsReset(tree, ctx.cfg, ar, Lvalue src)

    if not markCyclic and not reset:
      # if the value is not something that needs to be marked as cyclic
      # nor is the source a location that needs to be reset, we skip
      # injecting a temporary and pass the argument directly
      return

    let tmp = c.getTemp()

    c.insert(NodeInstance src, buf):
      buf.subTree MirNode(kind: mnkRegion):
        buf.add opParamNode(0, typ)
        buf.genDefTemp(tmp, typ)

        genMarkCyclic(buf, ctx.graph, typ, opParamNode(0, typ))
        if reset:
          genWasMoved(buf, ctx.graph):
            buf.add opParamNode(0, typ)
            undoConversions(buf, tree, src)
            EValue(typ: buf[^1].typ)

      buf.add MirNode(kind: mnkTemp, typ: typ, temp: tmp)

proc insertCopy(tree: MirTree, graph: ModuleGraph, typ: PType,
                maybeCyclic: bool, c: var Changeset) =
  ## Generates a call to the `typ`'s ``=copy`` hook that uses the contextual
  ## input as the source value
  let tmp = c.getTemp()
  c.insert(NodeInstance c.position, buf):
    buf.subTree MirNode(kind: mnkRegion):
      argBlock(buf): discard
      buf.add MirNode(kind: mnkMagic, typ: typ, magic: mDefault)
      buf.genDefTemp(tmp, typ)

      genCopy(buf, graph, typ,
              MirNode(kind: mnkTemp, typ: typ, temp: tmp),
              opParamNode(0, typ),
              maybeCyclic)

    buf.add MirNode(kind: mnkTemp, typ: typ, temp: tmp)

proc rewriteAssignments(tree: MirTree, ctx: AnalyseCtx, ar: AnalysisResults,
                        diags: var seq[LocalDiag], c: var Changeset) =
  ## Rewrites assignments to relevant locations into calls to either the
  ## ``=copy`` or ``=sink`` hook (see ``expandAsgn`` for more details),
  ## using the previously computed ownership information to decide.
  ##
  ## Also injects the necessary callsite logic for arguments passed to
  ## 'consume' argument sinks. The argument can only be consumed if it is
  ## *owned* -- if it isn't, a temporary copy is introduced and passed to the
  ## parameter instead. If a copy is required, a diagnostic is added to
  ## `msgs`.
  ##
  ## If the ``=copy`` hook is requested but not available because it's
  ## disabled, a diagnostic is added to `msgs`.
  # XXX: the procedure does too much and is thus too complex. Splitting the
  #      'consume' handling into a separate procedure would makes sense, but
  #      would likely also be less efficient due to the required extra
  #      (linear) tree traversal.
  #      Another possible improvement is moving the actual hook injection
  #      to a follow-up pass. The pass here would only inject ``mCopyAsgn`` and
  #      ``mSinkAsgn`` magics, which the aforementioned follow-up pass then
  #      expands into the hook calls. This would simplify the logic here a bit
  for i, n in tree.pairs:
    case n.kind
    of mnkRaise, mnkDefUnpack:
      # passing a value to the ``raise`` operation or as the initial value of
      # a temporary used for tuple unpacking also requires consuming it
      let
        opr = tree.operand(i)
        typ = tree[opr].typ

      if tree[opr].kind == mnkNone or not hasDestructor(typ):
        # for values without destructors 'consume' is a no-op
        continue

      c.seek(i)
      case ar.v[].owned(opr)
      of Owned.yes:
        consumeArg(tree, ctx, ar, typ, opr, c)
      of Owned.no:
        insertCopy(tree, ctx.graph, tree[opr].typ, maybeCyclic = true, c)
      of Owned.unknown, Owned.weak:
        unreachable()

    of mnkConsume:
      let typ = n.typ
      if not hasDestructor(typ):
        continue

      let
        user = Operation(findEnd(tree, parent(tree, i)) + 1) ## the consumer
        # XXX: 'consume' is not an operation -- it's an argument sink. It
        #      might make sense to introduce a new type for those
        val = tree.operand(i)

      case tree[user].kind
      of mnkConstr, mnkObjConstr, mnkCall, mnkMagic:
        # a consume in a non-assignment context:
        c.seek(i)
        case ar.v[].owned(val)
        of Owned.yes:
          consumeArg(tree, ctx, ar, typ, val, c)
        of Owned.no:
          let op = getOp(ctx.graph, typ, attachedAsgn)
          if sfError in op.flags:
            diags.add LocalDiag(pos: NodePosition val,
                                kind: ldkUnavailableTypeBound,
                                op: attachedAsgn)
            # report a diagnostic but still insert the copy
          else:
            diags.add LocalDiag(pos: NodePosition val,
                                kind: ldkPassCopyToSink)

          insertCopy(tree, ctx.graph, typ, maybeCyclic = true, c)
        else:
          unreachable("un-collapsed ownership status")

      of mnkInit, mnkAsgn:
        # a consume in an assignment context. They're handled separately
        case ar.v[].owned(val)
        of Owned.yes: discard
        of Owned.no:
          let op = getOp(ctx.graph, typ, attachedAsgn)
          if sfError in op.flags:
            diags.add LocalDiag(pos: NodePosition val,
                                kind: ldkUnavailableTypeBound,
                                op: attachedAsgn)
            continue

        else:
          unreachable()

        expandAsgn(tree, ctx, ar, typ, source=val, asgn=user, c)
      else:
        unreachable("not a valid consume context")

    of AllNodeKinds - ConsumeCtx:
      discard "not relevant"

# --------- destructor injection -------------

type DestroyEntry = tuple
  pos: NodePosition   ## the position of the 'def' belonging to the entity that
                      ## requires destruction
  scope: NodePosition ## the position of the enclosing 'scope'

proc injectDestructorsInner(buf: var MirTree, orig: MirTree, graph: ModuleGraph,
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

    genDestroy(buf, graph, t.skipTypes(skipAliases), orig[def])

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

  # second pass: if at least one entity in a scope needs its destructor call
  # placed in a ``finally`` clause, all others in the same scope do too, as the
  # order-of-destruction would be violated otherwise
  for pos, scope in entries.items:
    if scope in needsFinally:
      # if the destroy call has to be placed inside a ``finally`` clause, we
      # first need to move the 'def' from its current position to the start of
      # the scope, as it'd be otherwise located inside the ``try``'s body
      # (which would render the entity unavailable inside the ``finally``
      # clause)
      assert tree[pos].kind == mnkDef
      c.seek(pos)
      if hasInput(tree, Operation pos):
        # replace the 'def' with an initializing assignment if it has an
        # input:
        c.replaceMulti(buf):
          buf.subTree MirNode(kind: mnkRegion):
            argBlock(buf):
              let e = getDefEntity(tree, pos) # the entity (e.g. local)
              chain(buf): emit(tree[e]) => name()
              chain(buf): opParam(0, tree[e].typ) => arg()
            buf.add MirNode(kind: mnkInit)

      else:
        c.remove()

      # insert the 'def' at the start of the scope:
      c.seek(scope + 1)
      c.insert(NodeInstance pos, buf):
        buf.add toOpenArray(tree, pos.int, pos.int+2)

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

  # third pass: inject the destructors and place them inside a ``finally``
  # clause if necessary
  for s in scopeItems(entries):
    let
      scopeStart = entries[s.a].scope
      useFinally = scopeStart in needsFinally
      source = NodeInstance scopeStart
        ## the node to inherit the origin information from

    if useFinally:
      # at the start of the scope (after the 'def's previously moved there),
      # insert the start nodes of a 'try' and a 'stmtList':
      c.seek(scopeStart + 1)
      c.insert(source, buf):
        buf.add MirNode(kind: mnkTry, len: 1)
        buf.add MirNode(kind: mnkStmtList)

    c.seek findEnd(tree, scopeStart) # seek to the scope's end node
    c.insert(source, buf):
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

proc injectTemporaries(tree: MirTree, c: var Changeset) =
  ## Injects temporaries for all unnamed values requiring destruction (they
  ## have a destructor) that escape. An unnamed value value escapes if there
  ## exists a control-flow path where it is not consumed

  # XXX: there is an issue with the current implementation that causes leaks,
  #      see the module's doc comment
  for i, n in tree.pairs:
    let isMangedRValue =
      case n.kind
      of mnkCall, mnkMagic:
        hasDestructor(n.typ)
      of mnkObjConstr:
        # there's no need to skip ``tyDistinct`` here - a ``distinct ref``
        # can't be constructed. We also don't need to consider non-ref
        # constructors, as the resulting value is non-owning outside of
        # consume context.
        # XXX: should this behaviour be explicitly put in the
        #      specification?
        n.typ.skipTypes(skipAliases).kind == tyRef
      else:
        false

    if isMangedRValue and not isConsumed(tree, OpValue i):
      # only locations can be destroyed, so we assign the value to a
      # temporary. The destructor injection pass takes care of the rest then
      let tmp = c.getTemp()
      c.seek(i)
      c.skip(1)
      c.insert(NodeInstance i, buf):
        buf.genDefTemp(tmp, n.typ)
        buf.add MirNode(kind: mnkTemp, typ: n.typ, temp: tmp)


proc lowerBranchSwitch(buf: var MirNodeSeq, body: MirTree, graph: ModuleGraph,
                       idgen: IdGenerator, op: Operation) =
  ## Lowers a 'switch' operation into a simple discriminant assignment plus
  ## the logic for destroying the previous branch (if necessary)
  assert body[op].kind == mnkSwitch

  let
    target = skipTag(body, operand(body, op, 0))
    objType = body[target].typ
    typ = body[target].field.typ

  assert body[target].kind == mnkPathVariant

  # ``subTree`` is not used for the region in order to reduce indentation
  # a bit
  buf.add MirNode(kind: mnkRegion)

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
      voidTyp = graph.getSysType(unknownLineInfo, tyVoid)

    # XXX: comparing the discrimant values here means that the branch is
    #      destroyed even if the branch doesn't change. This differs from
    #      the VM's behaviour. There, the branch is only reset if it's
    #      actually changed
    argBlock(buf):
      chain(buf): opParam(0, typ) => arg()
      chain(buf): opParam(1, typ) => arg()

    forward(buf): magicCall(getMagicEqForType(typ), boolTyp) =>
                  unaryMagicCall(mNot, boolTyp)
    buf.subTree MirNode(kind: mnkIf):
      stmtList(buf):
        # ``=destroy`` call:
        voidCallWithArgs(buf):
          chain(buf): procLit(buf, branchDestructor) => arg()
          chain(buf): opParam(buf, 0, typ) => tag(ekInvalidate) => arg()

  else:
    # the object doesn't need destruction, which means that neither does one
    # of the branches. We can just change the discriminant
    # XXX: this differs from what the VM does. For the VM, switching branches
    #      resets the memory of the record-case back to zero, independent of
    #      whether a branch contains managed memory or not
    discard

  # generate the ``discriminant = newValue`` assignment:
  argBlock(buf):
    chain(buf): opParam(0, typ) => tag(ekReassign) => name()
    chain(buf): opParam(1, typ) => arg()
  buf.add MirNode(kind: mnkFastAsgn)

  buf.add endNode(mnkRegion)

proc reportDiagnostics(g: ModuleGraph, tree: MirTree, sourceMap: SourceMap,
                       owner: PSym, diags: var seq[LocalDiag]) =
  ## Reports all diagnostics in `diags` as ``SemReport``s and clear the list
  for diag in diags.items:
    let ast = sourceMap.sourceFor(diag.pos.NodeInstance)
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
    ## Applies the changeset to both the
    let prepared = prepare(c, sourceMap)
    updateSourceMap(sourceMap, prepared)
    apply(tree, prepared)

  # apply the first batch of passes:
  block:
    var changes = initChangeset(tree)

    injectTemporaries(tree, changes)

    # the VM implements branch switching itself - performing the lowering for
    # code meant to run in it would be harmful
    # FIXME: discriminant assignment lowering also needs to be disabled for
    #        when generating code running at compile-time (e.g. inside a
    #        macro)
    # XXX: the lowering *is* always necessary, because the destructors for
    #      fields inside switched-away-from branches won't be called
    #      otherwise
    # TODO: make the branch-switch lowering a separate and standalone pass --
    #       it's not directly related to the rest of the processing here
    if g.config.backend != backendNimVm:
      for i, n in tree.pairs:
        if n.kind == mnkSwitch:
          changes.seek(i)
          changes.replaceMulti(buf):
            lowerBranchSwitch(buf, tree, g, idgen, Operation i)

    apply(changes)

  # apply the second batch of passes:
  block:
    var
      changes = initChangeset(tree)
      diags: seq[LocalDiag]

    let
      actx = AnalyseCtx(graph: g, cfg: computeCfg(tree))
      entities = initEntityDict(tree)

    var values = computeValuesAndEffects(tree)
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
    # due to some parts of it being very declarative, rendering and echoing
    # the MIR code wouldn't be very useful, so we turn it into backend IR
    # first, which we then render to text
    # XXX: this needs a deeper rethink
    let n = generateIR(g, idgen, owner, tree, sourceMap)
    g.config.msgWrite("--expandArc: " & owner.name.s & "\n")
    g.config.msgWrite(render(n))
    g.config.msgWrite("\n-- end of expandArc ------------------------\n")