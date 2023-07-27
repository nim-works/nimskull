## Implements the various MIR-based passes. The entry point is the
## `applyPasses <#applyPasses>`_ procedure.

import
  compiler/ast/[
    ast_query,
    ast_types,
    types
  ],
  compiler/mir/[
    analysis,
    mirchangesets,
    mirconstr,
    mirtrees,
    sourcemaps
  ],
  compiler/sem/[
    aliasanalysis
  ],
  compiler/utils/[
    idioms
  ]

# for type-based alias analysis
from compiler/sem/aliases import isPartOf, TAnalysisResult

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

  ArgIter = object
    ## State for an iterator over an argument block. For efficiency, the inputs
    ## to the arguments are returned in *reverse*
    pos: NodePosition ## node position

const
  LocSkip = abstractRange + tyUserTypeClasses
    ## types to skip to arrive at the underlying concrete value type

func isRvalue(tree: MirTree, a: OpValue): bool {.inline.} =
  ## Returns whether `a` represents an r-value, that is, something that
  ## doesn't have a name and cannot be assigned to. Only checks the
  ## immediate operator that yield the value.
  tree[a].kind in { mnkNone, mnkProc, mnkType, mnkLiteral, mnkConstr,
                    mnkObjConstr, mnkCall, mnkStdConv, mnkCast, mnkAddr,
                    mnkView }

func isArgBlock(tree: MirTree, n: NodePosition): bool =
  ## Returns whether the node `n` is the end-node of an arg-block.
  tree[n].kind == mnkEnd and tree[n].start == mnkArgBlock

func getRoot(tree: MirTree, n: OpValue): NodePosition =
  ## Returns the root of a value. The root is either:
  ## - the first operation yielding an lvalue (e.g., a pointer dereference)
  ## - the name of a location (e.g., ``a`` in ``a.b.c``)
  ## - an r-value (e.g., ``call()`` in ``call().b.c``)
  const PathNodes = { mnkPathArray, mnkPathNamed, mnkPathPos, mnkPathVariant,
                      mnkConv }
    ## all operations that (can) take an lvalue as input and produce
    ## lvalue

  var i = n
  while tree[i].kind in PathNodes:
    case tree[i].kind
    of mnkPathNamed, mnkPathPos, mnkPathVariant:
      i = OpValue(NodePosition(i) - 1)
    of mnkConv:
      i = unaryOperand(tree, Operation(i))
    of mnkPathArray:
      i = operand(tree, Operation(i), 0)
    of AllNodeKinds - PathNodes:
      unreachable()

  result = i.NodePosition

func getOpChain(tree: MirTree, a: OpValue): LvalueExpr {.inline.} =
  ## Returns the chain of operations that produce `a`.
  (getRoot(tree, a), NodePosition a)

func skipTag(tree: MirTree, a: OpValue): OpValue {.inline.} =
  if tree[a].kind == mnkTag: OpValue(NodePosition(a) - 1)
  else:                      a

func initArgIter*(tree: MirTree, call: NodePosition): ArgIter =
  assert tree[call].kind in { mnkCall, mnkMagic, mnkRegion, mnkAsgn,
                              mnkFastAsgn, mnkInit, mnkPathArray, mnkAsm,
                              mnkEmit }
  assert isArgBlock(tree, call-1)

  var pos = call - 2
  if tree[pos].kind == mnkArgBlock:
    # it's an empty arg-block, there's nothing to iterate
    pos = NodePosition(-1)

  result = ArgIter(pos: pos)

func next(iter: var ArgIter, tree: MirTree): OpValue =
  assert iter.pos.int >= 0, "no more arguments"
  result = OpValue(iter.pos - 1) # return the operand, not the argument node itself
  # move to the next argument node:
  var i = iter.pos - 1
  while tree[i].kind notin ArgumentNodes + {mnkArgBlock}:
    i = previous(tree, i)

  if tree[i].kind == mnkArgBlock:
    # no more argument nodes left; mark the iterator as finished
    iter.pos = NodePosition(-1)
  else:
    iter.pos = i

func hasNext(iter: ArgIter): bool {.inline.} =
  iter.pos != NodePosition(-1)

iterator search(tree: MirTree, kinds: static set[MirNodeKind]): NodePosition =
  ## Returns in order of appearance the positions of all nodes matching the
  ## given `kinds`.
  var i = 0
  while i < tree.len:
    if tree[i].kind in kinds:
      yield NodePosition(i)
    inc i

iterator uses(tree: MirTree, start, last: NodePosition): OpValue =
  ## Returns in an unspecified order all values used for reads/writes
  ## in the code range ``start..last``. Tags are already skipped.
  # for efficiency, we iterate from last to start
  var i = last
  while i >= start:
    let kind = tree[i].kind
    if kind in UseContext + ArgumentNodes or
       (kind in DefNodes and i.int > 0 and hasInput(tree, Operation i)):
      yield skipTag(tree, unaryOperand(tree, Operation(i)))

    dec i

proc overlapsConservative(tree: MirTree, a, b: LvalueExpr): bool =
  ## Computes whether the lvalues `a` and `b` potentially name overlapping
  ## mutable memory locations. The analysis is based on the operator
  ## application chain that yields the values, with type-based analysis used
  ## if either chain involves a pointer/view dereferences.
  if mnkConst in {tree[a.root].kind, tree[b.root].kind}:
    # while two locations derived from constants can overlap, mutating they're
    # not mutable lvalues, meaning that we can ignore them
    return false

  # use type-based alias analysis for derefs:
  if tree[a.root].kind == mnkDeref:
    # is 'b's type potentially part of 'a's type?
    if isPartOf(tree[b.last].typ, tree[a.last].typ) != arNo:
      return true
    elif tree[b.root].kind == mnkDeref:
      return isPartOf(tree[a.last].typ, tree[b.last].typ) != arNo
    else:
      # the type of 'b' is not part of 'a's type and 'b' is not an lvalue
      # coming from a deref -> the locations cannot possibly overlap (when
      # the dynamic and static types are compatible, that is)
      return false
  elif tree[b.root].kind == mnkDeref:
    # is 'a's type potentially part of 'b's type?
    return isPartOf(tree[a.last].typ, tree[b.last].typ) != arNo

  # we don't follow op params (a.k.a. named r-values) and thus have to treat
  # the values as overlapping if 'a' or 'b' is coming from one:
  if mnkOpParam in {tree[a.root].kind, tree[b.root].kind}:
    return true

  # use path-based analysis:
  result = overlaps(tree, a, b) != no

proc preventRvo(tree: MirTree, changes: var Changeset) =
  ## Injects write-to-temporaries for call and value construction rvalues. The
  ## goal is that the code generators can always use RVO and in-place aggregate
  ## construction when the source operand of an assignment is a call r-value
  ## or construction r-value, respectively.
  ##
  ## Input language:
  ## - no complex assignments nor cursors
  ## - copies are injected for consumed arguments (e.g. ``sink``)
  ##
  ## Changes:
  ## - write-to-temporaries inserted after some ``mnkObjConstr``,
  ##   ``mnkConstr``, and ``mnkCall`` nodes
  proc eligibleForRvo(t: PType): bool =
    # keep synchronized with ``ccgtypes.isInvalidReturnType``
    # XXX: this needs a bigger rethink. When and how the return-value
    #      optimization is applied should be independent of the used
    #      backend
    let t = t.skipTypes(LocSkip)
    result = t.kind in {tySet, tyArray} or
             containsGarbageCollectedRef(t) or
             (t.kind == tyObject and not isObjLackingTypeField(t))

  proc skipStdConv(tree: MirTree, x: OpValue): OpValue =
    # HACK: this is a hack so that ``a = call(p = a)`` works when 'p'
    #       is an ``openArray`` parameter. We need a more general
    #       solution that considers *all* indirection-like rvalues
    #       (pointers, refs, openArrays).
    if tree[x].kind == mnkStdConv and
       tree[x].typ.skipTypes(LocSkip + {tyVar}).kind == tyOpenArray:
      OpValue(NodePosition(x) - 1)
    else:
      x

  # we don't need to consider defs or initializing assignments (``mnkInit``)
  # here, because there it is guaranteed that the destination does not appear
  # anwhere in the source expression
  # XXX: no fast-assignments should exist at this point, but currently they
  #      do
  for i in search(tree, {mnkFastAsgn, mnkAsgn}):
    let source = operand(tree, Operation(i), 1)

    var useTemp = false
    case tree[source].kind
    of mnkCall:
      if not eligibleForRvo(tree[source].typ):
        # the return-value optimization is not used
        continue

      # the source is a call r-value
      let dest = getOpChain(tree, skipTag(tree, operand(tree, Operation(i), 0)))
      var iter = initArgIter(tree, NodePosition source)

      # check all arguments (including the callee):
      while iter.hasNext and not useTemp:
        let arg = getOpChain(tree,
                             skipStdConv(tree, skipTag(tree, next(iter, tree))))
        useTemp = not isRvalue(tree, arg.root.OpValue) and
                  overlapsConservative(tree, dest, arg)

    of mnkObjConstr, mnkConstr:
      # the source is a construction r-value. The logic is slightly more
      # complex compared to call rvalues, since we also need to consider the
      # following:
      #
      #   a = (1, (let x = a[0]; x))
      #
      # the tuple cannot be constructed in-place here, as ``a[0]`` would then
      # be mutated before it is read, causing an obsevable difference in
      # behaviour
      # XXX: ``ref object`` constructions are also considered here, and while
      #      that is not incorrect, it is inefficient; they can be treated
      #      like calls
      let destPos = operand(tree, Operation(i), 0).NodePosition
      let dest = getOpChain(tree, skipTag(tree, OpValue(destPos)))
      # search for potential uses of the destination in the construction
      # expression:
      for use in uses(tree, destPos + 2, i - 2):
        let val = getOpChain(tree, use)
        if not isRvalue(tree, val.root.OpValue) and
           overlapsConservative(tree, dest, val):
          useTemp = true
          break

    else:
      discard "nothing to do"

    if useTemp:
      # inject the write-to-temporary:
      let insert =
        if tree[source].kind == mnkObjConstr:
          findEnd(tree, NodePosition source)
        else:
          NodePosition(source)

      let temp = MirNode(kind: mnkTemp, typ: tree[source].typ,
                         temp: changes.getTemp())
      changes.seek(insert + 1)
      changes.insert(NodeInstance source, buf):
        buf.subTree MirNode(kind: mnkDef):
          buf.add temp
        buf.add temp

proc applyPasses*(tree: var MirTree, source: var SourceMap, prc: PSym,
                  target: TargetBackend) =
  ## Applies all applicable MIR passes to the body (`tree` and `source`) of
  ## `prc`. `target` is the targeted backend and is used to enable/disable
  ## certain passes.
  template batch(body: untyped) =
    block:
      var c {.inject.} = initChangeset(tree)
      body
      let p = prepare(c, source)
      updateSourceMap(source, p)
      apply(tree, p)

  if target == targetC:
    # only the C code generator employs the RVO and in-place construction
    # at the moment
    batch:
      preventRvo(tree, c)