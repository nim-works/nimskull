## Implements the various MIR-based passes. The entry point is the
## `applyPasses <#applyPasses>`_ procedure.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    types
  ],
  compiler/mir/[
    mirchangesets,
    mirconstr,
    mirtrees,
    sourcemaps
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

func skipTag(tree: MirTree, n: NodePosition): NodePosition {.inline.} =
  if tree[n].kind == mnkTag: NodePosition tree.operand(n)
  else:                      n
iterator search(tree: MirTree, kinds: static set[MirNodeKind]): NodePosition =
  ## Returns in order of appearance the positions of all nodes matching the
  ## given `kinds`.
  var i = 0
  while i < tree.len:
    if tree[i].kind in kinds:
      yield NodePosition(i)
    inc i

iterator search(tree: MirTree, magic: static TMagic): NodePosition =
  ## Returns in appearance order the positions of all ``mnkMagic`` nodes
  ## that match `magic`.
  var i = 0
  while i < tree.len:
    if tree[i].kind == mnkMagic and tree[i].magic == magic:
      yield NodePosition(i)
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
  for i in search(tree, {mnkFastAsgn, mnkAsgn}):
    let source = tree.operand(i, 1)
    if tree[source].kind != mnkCall or not eligibleForRvo(tree[source].typ):
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
            let def = tree.operand(findDef(tree, NodePosition it), 1)
            if tree[def].kind == mnkToSlice:
              (true, NodePosition tree.operand(def))
            else:
              (false, NodePosition 0)
          else:
            (false, NodePosition 0)
        of mnkName:
          (true, tree.skipTag(NodePosition it))
        of mnkConsume:
          (false, NodePosition 0)

      if check and overlapsConservative(tree, path, computePath(tree, arg),
                                        tree[dest].typ, tree[arg].typ):
        needsTemp = true
        break

    if needsTemp:
      # rewrite the assignment into a definition-of-temporary and then assign
      # the temporary to the correct location
      let pos = NodePosition tree.operand(i, 0)
      changes.changeTree(tree, i): MirNode(kind: mnkDef)
      var tmp: Value
      changes.replaceMulti(tree, pos, bu):
        tmp = bu.allocTemp(tree[source].typ)
        bu.use tmp
      changes.insert(tree, tree.sibling(i), i, bu):
        bu.subTree tree[i].kind:
          bu.emitFrom(tree, pos)
          bu.use tmp

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
        temp = bu.materialize(a)
      # we're just swapping the values, no full copy is needed
      bu.asgn a, b
      bu.asgn b, temp

proc eliminateTemporaries(tree: MirTree, changes: var Changeset) =
  ## Where safe (i.e., observable program behaviour does not change), elides
  ## temporaries in a backend-agnostice way. This is an optimization.
  const Ignore = IntegralTypes + {tyPtr, tyPointer, tyRef, tyVar, tyLent,
                                  tyOpenArray, tyProc}
    ## ignored by the optimization pass. These are types where a copy is
    ## faster than creating a reference
  var ct = initCountTable[uint32]()

  # first pass: gather all single-use temporaries that are created from
  # lvalues and are eligible for elimination.
  var i = NodePosition 0
  while i.int < tree.len:
    case tree[i].kind
    of mnkDef, mnkDefCursor:
      let e = NodePosition tree.operand(i, 1)
      if tree[i, 0].kind == mnkTemp and
         tree[i, 0].typ.skipTypes(abstractInst).kind notin Ignore and
         tree[e].kind in LvalueExprKinds and
         tree[getRoot(tree, e)].kind notin {mnkConst, mnkTemp}:
        # definition of a temporary into which an lvalue is assigned. Elision
        # is disabled for:
        # * projections of temporaries; the projected temporary might be
        #   elided itself, which could lead to evaluation order issues
        # * constants; works around code generator issues
        ct[tree[i, 0].temp.uint32] = 1

      i = e # skip to the source expression
    of mnkTemp:
      let id = tree[i].temp
      if hasKey(ct, id.uint32):
        ct.inc(id.uint32)
      inc i
    else:
      inc i

  if ct.len == 0:
    # no temporaries are used at all -> nothing to do
    return

  proc overlaps(tree: MirTree, a: Path, b: OpValue): auto {.nimcall.} =
    overlaps(tree, a, computePath(tree, NodePosition b))

  proc findUse(tree: MirTree, cfg: ControlFlowGraph, p: Path, start: InstrPos,
               e: TempId): NodePosition {.nimcall.} =
    ## Conservative data-flow analysis that computes whether the `p` might be
    ## modified. If there are no modifications of `p` between `start`
    ## (inclusive) and the use of `e`, the the usage of `e` is returned --
    ## -1 otherwise.
    let all = cfg.rangeFor(NodePosition(0) .. NodePosition(tree.len))
    var s: TraverseState
    for op, n in traverse(tree, cfg, all, start, s):
      case op
      of opUse:
        if tree[n].kind == mnkTemp and tree[n].temp == e:
          # the searched-for temporary is used and there was no mutation of
          # `p` so far -> not modified
          return NodePosition(n)
      of opConsume, opDef, opMutate, opKill, opInvalidate:
        if tree[n].kind == mnkTemp and tree[n].temp == e:
          # the searched-for temporary is mutated or consumed itself
          return NodePosition(-1)

        case tree[p.root].kind
        of mnkDeref:
          # a location is mutated through a pointer indirection. Assume that
          # the pointer points into the location that `p` identifies
          return NodePosition(-1)
        of mnkDerefView:
          # view dereferences are fine, as a mutable view cannot alias
          # a path that's used for reading
          discard
        elif overlaps(tree, p, n) != no:
          return NodePosition(-1)
      of opMutateGlobal:
        if tree[p.root].kind == mnkGlobal:
          return NodePosition(-1)
      else:
        discard

    # either the data-flow graph creation logic is wrong or there's a bug in
    # the optimizer
    unreachable("temporary is not a single-use temporary")

  # second pass: find the point-of-definition for each single-use temporary,
  # check whether their source lvalue is mutated prior to the only usage of
  # the temporary, and if it's not, elide the temporary.
  let cfg = computeCfg(tree)
  for i, op, n in instructions(cfg):
    if op == opDef and tree[n].kind == mnkTemp and
       ct.getOrDefault(tree[n].temp.uint32, 0) == 2:
      # definition of a single-use temporary that might be elidable. Look for
      # potential mutations of the lvalue
      let
        n   = NodePosition n
        def = tree.parent(n)
        p   = computePath(tree, NodePosition tree.operand(def, 1))
        pos = findUse(tree, cfg, p, i + 1, tree[n].temp)

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
                overlaps(tree, p, tree.operand(stmt, 0)) == no
      of mnkMagic, mnkCall:
        # the lvalue overlapping with a mutable argument disable the elision,
        # as eliding the temporary would be obersvable when the backend decides
        # to use pass-by-reference for the immutable parameter
        elide = true # unless proven otherwise
        for k, arg in arguments(tree, expr):
          if tree[arg].kind == mnkTag and
             overlaps(tree, p, tree.operand(arg)) != no:
            elide = false
            break

        if elide:
          # the lvalue must also not overlap with the call-result destination
          elide = overlaps(tree, p, tree.operand(tree.parent(expr), 0)) == no
      else:
        unreachable(tree[expr].kind)

      if elide:
        # XXX: lvalue expression can currently have side-effects, so
        #      forwarding the expression would change behaviour. Instead, the
        #      temporary is turned into an alias
        let
          alias = MirNode(kind: mnkAlias, typ: tree[n].typ, temp: tree[n].temp)
          def = tree.parent(n)

        changes.changeTree(tree, def): MirNode(kind: mnkBind)
        changes.replace(tree, n): alias
        changes.replace(tree, pos): alias

proc applyPasses*(tree: var MirTree, source: var SourceMap, prc: PSym,
                  config: ConfigRef, target: TargetBackend) =
  ## Applies all applicable MIR passes to the body (`tree` and `source`) of
  ## `prc`. `target` is the targeted backend and is used to enable/disable
  ## certain passes.
  template batch(body: untyped) =
    block:
      var c {.inject.} = initChangeset(tree)
      body
      apply(tree, prepare(c))

  if target == targetC:
    batch:
      # only the C code generator employs the return-value optimization (=RVO)
      # at the moment
      preventRvo(tree, c)

  batch:
    lowerSwap(tree, c)

  # eliminate temporaries after all other passes
  batch:
    eliminateTemporaries(tree, c)