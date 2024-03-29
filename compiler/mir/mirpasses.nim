## Implements the various MIR-based passes. The entry point is the
## `applyPasses <#applyPasses>`_ procedure.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    lineinfos,
    types
  ],
  compiler/front/[
    in_options
  ],
  compiler/mir/[
    analysis,
    datatables,
    mirbodies,
    mirenv,
    mirchangesets,
    mirconstr,
    mirtrees,
    sourcemaps
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/sem/[
    aliasanalysis,
    mirexec
  ],
  compiler/utils/[
    idioms
  ]

# for type-based alias analysis
from compiler/sem/aliases import isPartOf, TAnalysisResult

from compiler/front/options import ConfigRef

type
  TargetBackend* = enum
    ## The backend that is going to consume the MIR code. Used to select what
    ## passes to run.
    # XXX: this is the wrong abstraction. Application of the MIR passes doesn't
    #      care about what backend is the target -- it cares about what the
    #      targeted *language level* is (which is what the backend implies)
    targetC
    targetJs
    targetVm

const
  LocSkip = abstractRange + tyUserTypeClasses
    ## types to skip to arrive at the underlying concrete value type

iterator search(tree: MirTree, kinds: static set[MirNodeKind]): NodePosition =
  ## Returns in order of appearance the positions of all nodes matching the
  ## given `kinds`.
  var i = 0
  while i < tree.len:
    if tree[i].kind in kinds:
      yield NodePosition(i)
    inc i

iterator search(tree: MirTree, magic: static TMagic): NodePosition =
  ## Returns in appearance order the positions of all magic-call nodes
  ## where the magic matches `magic`.
  var i = 0
  while i < tree.len:
    if tree[i].kind == mnkMagic and tree[i].magic == magic:
      yield tree.parent(NodePosition(i))
    inc i

proc overlapsConservative(tree: MirTree, a, b: Path, typA, typB: PType): bool =
  ## Computes whether the lvalues `a` and `b` potentially name overlapping
  ## mutable memory locations. The analysis is based on the paths, with
  ## type-based analysis used if either path involves a pointer dereference.
  if mnkConst in {tree[a.root].kind, tree[b.root].kind}:
    # while two locations derived from constants can overlap, mutating they're
    # not mutable lvalues, meaning that we can ignore them
    return false

  # use type-based alias analysis for derefs:
  if tree[a.root].kind == mnkDeref:
    # is 'b's type potentially part of 'a's type?
    if isPartOf(typB, typA) != arNo:
      return true
    elif tree[b.root].kind == mnkDeref:
      return isPartOf(typA, typB) != arNo
    else:
      # the type of 'b' is not part of 'a's type and 'b' is not an lvalue
      # coming from a deref -> the locations cannot possibly overlap (when
      # the dynamic and static types are compatible, that is)
      return false
  elif tree[b.root].kind == mnkDeref:
    # is 'a's type potentially part of 'b's type?
    return isPartOf(typA, typB) != arNo

  # use path-based analysis:
  result = overlaps(tree, a, b) != no

proc preventRvo(tree: MirTree, changes: var Changeset) =
  ## Injects intermediate temporaries for assignments where the source is an
  ## RVO-using call rvalue and the destination potentially aliases with a
  ## location accessible witin the call through one of the arguments.
  proc eligibleForRvo(t: PType): bool =
    # keep synchronized with ``ccgtypes.isInvalidReturnType``
    # XXX: this needs a bigger rethink. When and how the return-value
    #      optimization is applied should be independent of the used
    #      backend
    let t = t.skipTypes(LocSkip)
    result = t.kind in {tySet, tyArray} or
             containsGarbageCollectedRef(t) or
             (t.kind == tyObject and not isObjLackingTypeField(t))

  # we don't need to consider defs or initializing assignments (``mnkInit``)
  # here, because there it is guaranteed that the destination does not appear
  # anywhere in the source expression
  for i in search(tree, {mnkAsgn}):
    let source = tree.operand(i, 1)
    if tree[source].kind notin CallKinds or tree[source, 0].kind == mnkMagic or
       not eligibleForRvo(tree[source].typ):
      # the return-value optimization is not used
      continue

    let
      dest = tree.operand(i, 0)
      path = computePath(tree, NodePosition dest)
    var needsTemp = false
    for kind, it in arguments(tree, NodePosition source):
      let (check, arg) =
        case kind
        of mnkArg:
          # special handling for openArrays: they are also able to observe the
          # result location
          if tree[it].kind == mnkTemp and
             tree[it].typ.skipTypes(abstractVar).kind == tyOpenArray:
            # find the lvalue expression (if any) that the slice was created
            # from and use that for the overlap analysis
            let def = tree.child(findDef(tree, NodePosition it), 1)
            if tree[def].kind == mnkToSlice:
              (true, tree.operand(def, 0))
            else:
              (false, OpValue 0)
          else:
            (false, OpValue 0)
        of mnkName:
          (true, tree.skip(it, mnkTag))
        of mnkConsume:
          (false, OpValue 0)

      if check and overlapsConservative(tree, path,
                                        computePath(tree, NodePosition arg),
                                        tree[dest].typ, tree[arg].typ):
        needsTemp = true
        break

    if needsTemp:
      # rewrite the assignment into a definition-of-temporary and then assign
      # the temporary to the correct location
      let pos = tree.child(i, 0)
      changes.changeTree(tree, i): MirNode(kind: mnkDef)
      var tmp: Value
      changes.replaceMulti(tree, pos, bu):
        tmp = bu.allocTemp(tree[source].typ)
        bu.use tmp
      changes.insert(tree, tree.sibling(i), i, bu):
        bu.subTree mnkAsgn:
          bu.emitFrom(tree, pos)
          bu.move tmp

proc lowerSwap(tree: MirTree, changes: var Changeset) =
  ## Lowers a ``swap(a, b)`` call into:
  ##
  ## ..code-block:: nim
  ##
  ##   let tmp = a
  ##   a = b
  ##   b = tmp
  ##
  ## where all assignments are shallow.
  # XXX: consider lowering swap in ``mirgen`` already, it's a transformation
  #      that's needed by every backend
  for i in search(tree, mSwap):
    changes.replaceMulti(tree, tree.parent(i), bu):
      let
        a = bu.bindMut(tree, NodePosition tree.argument(i, 0))
        b = bu.bindMut(tree, NodePosition tree.argument(i, 1))
        temp = bu.materializeMove(a)
      # we're just swapping the values, no full copy is needed
      bu.asgnMove a, b
      bu.asgnMove b, temp

proc eliminateTemporaries(tree: MirTree, changes: var Changeset) =
  ## Where safe (i.e., observable program behaviour does not change), elides
  ## temporaries in a backend-agnostice way. This is an optimization.
  ##
  ## For example:
  ##
  ##   def _1 = a.b.c
  ##   call(arg _1)
  ##
  ## would be transformed into:
  ##
  ##   call(arg a.b.c)
  var ct = initCountTable[uint32]()

  proc isDangerous(tree: MirTree, n: NodePosition): bool =
    # HACK: this is a tremendous hack to detect whether `n` is part of a
    #       loose expression, which are currently required by expression
    #       support for ``vmjit``. Remove as soon as no longer needed
    var i = int n
    while i < tree.len and tree[i].kind notin StmtNodes:
      inc i

    result = i >= tree.len

  # first pass: gather all single-use temporaries that are created from
  # lvalues and are eligible for elimination.
  var i = NodePosition 0
  while i.int < tree.len:
    case tree[i].kind
    of mnkDef, mnkDefCursor:
      let e = tree.operand(i, 1)
      if tree[i, 0].kind == mnkTemp and
         tree[e].kind in LvalueExprKinds and
         tree[getRoot(tree, e)].kind != mnkTemp:
        # definition of a temporary into which an lvalue is assigned. Elision
        # is disabled for projections of temporaries; the projected temporary
        # might be elided itself, which could lead to evaluation order issues
        ct[tree[i, 0].temp.uint32] = 1

      i = NodePosition e # skip to the source expression
    of mnkTemp:
      # treat as usage
      # XXX: this is brittle. Usages should be detected through DFA, not by
      #      looking for names
      let id = tree[i].temp
      if hasKey(ct, id.uint32):
        if isDangerous(tree, i):
          ct.del(id.uint32)
        else:
          ct.inc(id.uint32)

      inc i
    of mnkDeref, mnkDerefView:
      # a non-name lvalue expression cannot be placed into deref slots. All
      # analysed temporaries are assumed to have been initialized with a non-
      # name lvalue expression, so if a temporary appears in a deref slot,
      # elision of said temporary is disabled
      if tree[i, 0].kind == mnkTemp:
        ct.del(tree[i, 0].temp.uint32) # treat as not eligible
      i = tree.sibling(i) # skip the deref
    of mnkPathArray:
      # for array index slots, the above also applies
      let index = tree.child(i, 1)
      if tree[index].kind == mnkTemp:
        ct.del(tree[index].temp.uint32)
      inc i
    else:
      inc i

  if ct.len == 0:
    # no temporaries are used at all -> nothing to do
    return

  template overlaps(a: Path, typ: PType, b: OpValue): bool =
    let x = NodePosition b
    overlapsConservative(tree, a, computePath(tree, x), typ, tree[x].typ)

  proc findUse(tree: MirTree, dfg: DataFlowGraph, p: Path, typ: PType,
               start: InstrPos, e: TempId): NodePosition {.nimcall.} =
    ## Conservative data-flow analysis that computes whether the `p` might be
    ## modified. If there are no modifications of `p` between `start`
    ## (inclusive) and the use of `e`, the the usage of `e` is returned --
    ## -1 otherwise.
    let all = dfg.subgraphFor(NodePosition(0) .. NodePosition(tree.len))
    var s: TraverseState
    # XXX: the analysis is using the wrong direction! We want to know
    #      whether `p` is mutated on any paths connecting `start` and
    #      the use of `e`, not whether a mutation of `p` is connected
    #      to `start` -- the analysis needs to be rewritten to use
    #      ``traverseReverse``.
    for op, n in traverse(dfg, all, start, s):
      case op
      of opUse:
        if tree[n].kind == mnkTemp and tree[n].temp == e:
          # the searched-for temporary is used and there was no mutation of
          # `p` so far -> not modified
          return NodePosition(n)
      of opConsume, opDef, opMutate, opKill, opInvalidate:
        if (tree[n].kind == mnkTemp and tree[n].temp == e) or
           overlaps(p, typ, n):
          # either the searched-for temporary is mutated or consumed itself,
          # or the lvalue is mutated/consumed
          return NodePosition(-1)
      of opMutateGlobal:
        if tree[p.root].kind == mnkGlobal:
          return NodePosition(-1)

    # either the data-flow graph creation logic is wrong or there's a bug in
    # the optimizer
    unreachable("temporary is not a single-use temporary")

  # second pass: find the point-of-definition for each single-use temporary,
  # check whether their source lvalue is mutated prior to the only usage of
  # the temporary, and if it's not, elide the temporary.
  let dfg = computeDfg(tree)
  for i, op, n in instructions(dfg):
    if op == opDef and tree[n].kind == mnkTemp and
       ct.getOrDefault(tree[n].temp.uint32, 0) == 2:
      # definition of a single-use temporary that might be elidable. Look for
      # potential mutations of the lvalue
      let
        n   = NodePosition n
        def = tree.parent(n)
        p   = computePath(tree, tree.child(def, 1))
        typ = tree[n].typ
        pos = findUse(tree, dfg, p, typ, i + 1, tree[n].temp)

      if pos == NodePosition(-1):
        # the copy is necessary
        continue

      var expr = pos # the expression the usage is part of
      while (let p = tree.parent(expr); tree[p].kind notin StmtNodes):
        expr = p

      var elide = false
      case tree[expr].kind
      of LvalueExprKinds:
        # usage in an lvalue expression -> the temporary can be elided
        elide = true
      of RvalueExprKinds:
        elide = true
      of mnkConstr, mnkObjConstr:
        # if the lvalue doesn't overlap with the assignment destination, the
        # temporary can be elided
        let stmt = tree.parent(expr)
        elide = tree[stmt].kind in {mnkInit, mnkDef, mnkDefCursor} or
                not overlaps(p, typ, tree.operand(stmt, 0))
      of CallKinds:
        # the lvalue overlapping with a mutable argument disable the elision,
        # as eliding the temporary would be obersvable when the backend decides
        # to use pass-by-reference for the immutable parameter
        elide = true # unless proven otherwise
        for k, arg in arguments(tree, expr):
          if tree[arg].kind == mnkTag and overlaps(p, typ, tree.operand(arg)):
            elide = false
            break

        if elide:
          # the lvalue must also not overlap with the call-result destination
          elide = not overlaps(p, typ, tree.operand(tree.parent(expr), 0))
      else:
        unreachable(tree[expr].kind)

      if elide:
        # remove the definition of the temporary:
        changes.remove(tree, def)
        # replace the temporary's only usage with the lvalue expression it was
        # created from:
        changes.replaceMulti(tree, pos, bu):
          bu.emitFrom(tree, tree.child(def, 1))

proc extractStringLiterals(tree: MirTree, env: var MirEnv,
                           changes: var Changeset) =
  ## Extracts all string literals and promotes them to anonymous constants,
  ## replacing the string literals with a usage of the constants they were
  ## promoted to.
  for i in search(tree, {mnkLiteral}):
    # note: both normal string *and* cstring literals are currently included
    if tree[i].lit.kind in nkStrLiterals:
      # create an anonymous constant from the literal:
      let c = toConstId env.data.getOrPut(@[tree[i]])
      # replace the usage of the literal with the anonymous constant:
      changes.replaceMulti(tree, i, bu):
        bu.use toValue(c, tree[i].typ)

proc injectResultInit(tree: MirTree, resultVar: PSym, changes: var Changeset) =
  ## Injects a default-initialization for the result variable, if deemed
  ## necessary by data-flow analysis.
  ##
  ## For targets that don't default-initialize locals automatically,
  ## default-initialization is necessary for the result variable if:
  ## * it is partially modified, read from, or otherwise used without having
  ##   been fully assigned first
  ## * a procedure exit is reached and the variable is not definitely
  ##   initialized
  # future direction: once possible, extend this pass to apply to all local
  # variables
  func isResult(tree: MirTree, n: OpValue): bool =
    tree[n].kind == mnkLocal and tree[n].sym.kind == skResult

  func requiresInit(tree: MirTree): bool =
    let
      dfg = computeDfg(tree)
      all = dfg.subgraphFor(NodePosition(0) .. NodePosition(tree.len))
    var s: TraverseState

    for op, n in traverse(dfg, all, 0, s):
      case op
      of opDef, opKill:
        if isResult(tree, skipConversions(tree, n)):
          # the result variable is fully assigned or reset -> quit the
          # path
          s.exit = true

      of opUse, opConsume, opMutate, opInvalidate:
        if isResult(tree, getRoot(tree, n)):
          # the result variable is read from or modified before it was
          # initialized
          return true

      of opMutateGlobal:
        discard "not relevant"

    # the exit flag indicates that traversal reached the end of the body
    # (without ``result`` being an initialized). The a > b check makes sure
    # an empty procedure body also requires initialization of the result
    # var
    result = s.exit or all.a > all.b

  if requiresInit(tree):
    assert tree[0].kind == mnkScope
    let at = tree.child(NodePosition 0, 0)
    changes.insert(tree, at, at, bu):
      bu.subTree mnkInit:
        bu.use toValue(mnkLocal, resultVar)
        bu.buildMagicCall mDefault, resultVar.typ:
          discard

proc injectProfilerCalls(tree: MirTree, graph: ModuleGraph, env: var MirEnv,
                         changes: var Changeset) =
  ## Instruments the body with calls to the ``nimProfile`` compiler runtime
  ## procedure. Profiler calls are placed:
  ## * at the beginning of a procedure's body
  ## * at the end of a loop's body
  let
    voidType = graph.getSysType(unknownLineInfo, tyVoid)
    prc = graph.getCompilerProc("nimProfile")
    prcId = env.procedures.add(prc)

  # insert the entry call within the outermost scope:
  changes.insert(tree, tree.child(NodePosition 0, 0), NodePosition 0, bu):
    bu.subTree mnkVoid:
      bu.buildCall prcId, prc.typ, voidType:
        discard "no arguments"

  for i in search(tree, {mnkEnd}):
    if tree[i].start == mnkRepeat:
      # insert the call before the end node:
      changes.insert(tree, i - 1, i, bu):
        bu.subTree mnkVoid:
          bu.buildCall prcId, prc.typ, voidType:
            discard "no arguments"

proc applyPasses*(body: var MirBody, prc: PSym, env: var MirEnv,
                  graph: ModuleGraph, target: TargetBackend) =
  ## Applies all applicable MIR passes to the body (`tree` and `source`) of
  ## `prc`. `target` is the targeted backend and is used to enable/disable
  ## certain passes. Passes may register new entities with `env`.
  template batch(b: untyped) =
    block:
      var c {.inject.} = initChangeset(body)
      b
      apply(body, c)

  if target == targetC:
    batch:
      # only the C code generator employs the return-value optimization (=RVO)
      # at the moment
      preventRvo(body.code, c)

  batch:
    if target == targetC and (prc.kind in routineKinds) and
       (sfNoInit notin prc.flags) and not prc.typ[0].isEmptyType():
      # the procedure has a result variable and initialization of it is
      # allowed
      injectResultInit(body.code, prc.ast[resultPos].sym, c)

    lowerSwap(body.code, c)
    if target == targetVm:
      # only the C and VM targets need the extraction, and only the VM
      # requires the extraction for cstring literals
      extractStringLiterals(body.code, env, c)

  # instrument the body with profiler calls after all lowerings, but before
  # optimization
  if (sfPure notin prc.flags) and (optProfiler in prc.options):
    batch:
      injectProfilerCalls(body.code, graph, env, c)

  # eliminate temporaries after all other passes
  batch:
    eliminateTemporaries(body.code, c)
