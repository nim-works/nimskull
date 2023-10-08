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

# XXX: required for computing the size of types. Ideally, the size would
#      be fully computed already at this stage of compilation...
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
                    mnkObjConstr, mnkCall, mnkStdConv, mnkConv, mnkCast,
                    mnkAddr, mnkView }

func isArgBlock(tree: MirTree, n: NodePosition): bool =
  ## Returns whether the node `n` is the end-node of an arg-block.
  tree[n].kind == mnkEnd and tree[n].start == mnkArgBlock

func getRoot(tree: MirTree, n: OpValue): NodePosition =
  ## Returns the root of a value. The root is either:
  ## - the first operation yielding an lvalue (e.g., a pointer dereference)
  ## - the name of a location (e.g., ``a`` in ``a.b.c``)
  ## - an r-value (e.g., ``call()`` in ``call().b.c``)
  const PathNodes = { mnkPathArray, mnkPathNamed, mnkPathPos, mnkPathVariant,
                      mnkPathConv }
    ## all operations that (can) take an lvalue as input and produce
    ## an lvalue

  var i = n
  while tree[i].kind in PathNodes:
    case tree[i].kind
    of mnkPathNamed, mnkPathPos, mnkPathVariant, mnkPathConv:
      i = tree.operand(i)
    of mnkPathArray:
      i = operand(tree, Operation(i), 0)
    of AllNodeKinds - PathNodes:
      unreachable()

  result = i.NodePosition

func getOpChain(tree: MirTree, a: OpValue): LvalueExpr {.inline.} =
  ## Returns the chain of operations that produce `a`.
  (getRoot(tree, a), NodePosition a)

func skipTag(tree: MirTree, a: OpValue): OpValue {.inline.} =
  if tree[a].kind == mnkTag: tree.operand(a)
  else:                      a

func skipOpParam(tree: MirTree, n: OpValue): OpValue =
  ## Returns the value that was passed to the surrounding region's argument
  ## corresponding to the given ``mnkOpParam``.
  assert tree[n].kind == mnkOpParam
  let p = findParent(tree, NodePosition n, mnkRegion)
  if isArgBlock(tree, p - 1):
    result = operand(tree, Operation(p), tree[n].param)
  else:
    # it's a single argument region
    result = getStart(tree, p - 1).OpValue

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

func rawNext(iter: var ArgIter, tree: MirTree): NodePosition =
  ## Returns the next argument node.
  assert iter.pos.int >= 0, "no more arguments"
  result = iter.pos
  # move to the next argument node:
  var i = iter.pos - 1
  while tree[i].kind notin ArgumentNodes + {mnkArgBlock}:
    i = previous(tree, i)

  if tree[i].kind == mnkArgBlock:
    # no more argument nodes left; mark the iterator as finished
    iter.pos = NodePosition(-1)
  else:
    iter.pos = i

func next(iter: var ArgIter, tree: MirTree): OpValue {.inline.} =
  ## Returns the next operand node.
  tree.operand(rawNext(iter, tree))

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

iterator search(tree: MirTree, magic: static TMagic): NodePosition =
  ## Returns in appearance order the positions of all ``mnkMagic`` nodes
  ## that match `magic`.
  var i = 0
  while i < tree.len:
    if tree[i].kind == mnkMagic and tree[i].magic == magic:
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
      yield skipTag(tree, tree.operand(i))

    dec i

iterator potentialMutations(tree: MirTree, start, last: NodePosition): OpValue =
  ## Returns in an unspecified order all values that are:
  ## - passed to parameters that allow for mutations
  ## - turned into pointer rvalues (i.e., have their address taken)
  ## - have mutable views created of them
  ## None of the above means that the used value is really mutated in the
  ## specified range, rather it means that the value *could* be mutated.
  var i = start
  while i <= last:
    # all ``mnkTag`` nodes currently imply some sort of mutation/change
    if tree[i].kind in {mnkTag, mnkAddr} or
       (tree[i].kind == mnkView and tree[i].typ.kind == tyVar):
      yield tree.operand(i)

    inc i

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
  # anywhere in the source expression
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

proc fixupCallArguments(tree: MirTree, config: ConfigRef,
                        changes: var Changeset) =
  ## Injects temporaries in order to ensure that left-to-right evaluation
  ## order is respected for call arguments. This is necessary because of
  ## the implicit pass-by-reference semantics of some arguments. Consider:
  ##
  ##   f(a, (a = x; b))
  ##
  ## Where the first parameter uses pass-by-reference. At run-time, it has
  ## to look like the first argument is *evaluated* (not just have its identity
  ## computed, that is, address taken) before evaluating the second argument,
  ## and this is what "fixing up" the arguments is about.
  ##
  ## There is another, somewhat related, problem: the underlying location of an
  ## immutable pass-by-reference parameter can overlap with that of a ``var``
  ## parameter, in which case changes through the ``var`` parameter are
  ## observable on the immutable parameter. This problem is also addressed by
  ## the fixup.
  ##
  ## Do note that due to the placement of this pass (it happens after the
  ## ``injectdestructors`` pass), only *shallow*, non-owning copies of the
  ## affected arguments are made, meaning that there's the issue of resource-
  ## like values (refs, seqs, strings, everything else that has non-trivial
  ## copy behaviour) not being duplicated properly.

  proc maybeSameMutableLocation(tree: MirTree, a, b: OpValue): bool {.nimcall.} =
    ## Compares the path of both `a` and `b` to check whether they could
    ## potentially refer to overlapping mutable memory locations.
    proc path(tree: MirTree, x: OpValue): seq[NodePosition] =
      # gather all path-like operations that contribute to `x` into a
      # list
      var x = x
      while true:
        case tree[x].kind
        of mnkAddr, mnkView, mnkDeref, mnkDerefView, mnkConv, mnkStdConv,
           mnkTag, mnkPathConv:
          x = tree.operand(x)
        of mnkPathNamed, mnkPathPos, mnkPathVariant:
          result.add NodePosition(x)
          x = tree.operand(x)
        of mnkPathArray:
          result.add NodePosition(x)
          x = operand(tree, x, 0)
        of mnkTemp, mnkParam, mnkLocal, mnkGlobal:
          # found a location root
          result.add NodePosition(x)
          break
        of mnkOpParam:
          x = skipOpParam(tree, x)
        of mnkNone, mnkType, mnkLiteral, mnkProc, mnkConst, mnkCall, mnkMagic,
           mnkObjConstr, mnkConstr, mnkCast:
          # either an rvalue or something that's never mutable
          result = @[]
          break
        of AllNodeKinds - SourceNodes + {mnkArgBlock}:
          unreachable(tree[x].kind)

    let
      an = path(tree, a)
      bn = path(tree, b)

    if an.len == 0 or bn.len == 0:
      # at least one of the values is unique or known to be immutable
      return false

    # we now compare the two paths, and if one ends while they haven't
    # diverged, we treat the values as overlapping in memory. Since derefs are
    # skipped, there can be both false positives and false negatives, as the
    # underlying location cannot be statically known when dereferences are
    # involved. For the immediate use case here, the heuristic works okay

    result = true # until proven otherwise
    for i in 1..min(an.len, bn.len):
      let a {.cursor.} = tree[an[^i]]
      let b {.cursor.} = tree[bn[^i]]

      template check(cond: bool) =
        if cond:
          # bail out once the paths diverge
          result = false
          break

      check(a.kind != b.kind)

      case a.kind
      of SymbolLike:
        check(a.sym.id != b.sym.id)
      of mnkTemp:
        check(a.temp != b.temp)
      of mnkPathNamed, mnkPathVariant:
        check(a.field.id != b.field.id)
      of mnkPathPos:
        check(a.position != b.position)
      of mnkPathArray:
        check(sameIndex(tree[an[^i] - 3], tree[bn[^i] - 3]) == no)
      else:
        unreachable()

  # we follow op-params in the analysis, meaning that we can ignore
  # ``mnkRegion``s here
  for i in search(tree, {mnkCall}):
    # regions are allowed to not use an arg-block, and we don't want/need to
    # analyse those that don't
    if not isArgBlock(tree, i - 1):
      continue

    var iter = initArgIter(tree, i)
    while iter.hasNext:
      let arg = rawNext(iter, tree)
      case tree[arg].kind
      of mnkArg:
        # check if a value that is potentially the same as the analysed
        # one is:
        # - potentially mutated while evaluating following arguments
        # - passed to a parameter of the analysed call that allows for
        #   mutations
        # If either is the case, we need to introduce a shallow copy and use
        # that as the argument
        let val = skipTag(tree, tree.operand(arg))

        # 1. an r-value is unique, meaning that we know that a temporary is
        #    not needed
        # 2. aliasing is preferred over stack overflows
        # XXX: ^^ it shouldn't be. A static error would be better...
        if isRvalue(tree, getRoot(tree, val).OpValue) or
           getSize(config, tree[val].typ) >= 1024:
          continue

        var needsTemp = false
        block checkIfArgNeedsTemp:
          # first, check whether the argument value is passed to an earlier
          # ``var`` parameter of the procedure (e.g., given the call
          # ``f(a, b, c, d)`` and analysing 'c', check 'a' and 'b'. 'd' is
          # analysed as part of the ``potentialMutations`` loop below)
          var iter2 = iter
          while iter2.hasNext:
            let
              arg2   = rawNext(iter2, tree)
              argVal = tree.operand(arg2)
            if tree[arg2].kind == mnkName and tree[argVal].kind == mnkTag:
              if maybeSameMutableLocation(tree, val, tree.operand(argVal)):
                needsTemp = true
                break checkIfArgNeedsTemp

          # look for potential mutations of the argument that happen after
          # the argument is bound
          for mut in potentialMutations(tree, arg + 1, i - 2):
            if maybeSameMutableLocation(tree, val, mut):
              needsTemp = true
              break checkIfArgNeedsTemp

        if needsTemp:
          let temp = MirNode(kind: mnkTemp, typ: tree[arg].typ,
                             temp: changes.getTemp())
          changes.seek(arg)
          changes.insert(NodeInstance arg, buf):
            buf.subTree MirNode(kind: mnkDef):
              buf.add temp
            buf.add temp

      of mnkName:
        discard "the argument is explicitly passed by reference"
      of mnkConsume:
        discard "the value is guaranteed to be unique"
      of AllNodeKinds - ArgumentNodes:
        unreachable()

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
  for i in search(tree, mSwap):
    let
      typ = tree[operand(tree, Operation i, 0)].typ
      temp = MirNode(kind: mnkTemp, typ: typ, temp: changes.getTemp())
    changes.seek(i)
    changes.replaceMulti(buf):
      buf.subTree MirNode(kind: mnkRegion):
        # the temporary doesn't need to own the value, so use ``DefCursor``
        buf.add opParamNode(0, typ)
        buf.subTree MirNode(kind: mnkDefCursor):
          buf.add temp
        # we're just swapping the values, no full copy is needed
        argBlock(buf):
          chain(buf): opParam(0, typ) => tag(ekReassign) => name()
          chain(buf): opParam(1, typ) => arg()
        buf.add MirNode(kind: mnkFastAsgn)
        argBlock(buf):
          chain(buf): opParam(1, typ) => tag(ekReassign) => name()
          chain(buf): emit(temp) => arg()
        buf.add MirNode(kind: mnkFastAsgn)
    changes.remove() # remove the 'void' sink

proc applyPasses*(tree: var MirTree, source: var SourceMap, prc: PSym,
                  config: ConfigRef, target: TargetBackend) =
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
    batch:
      # only the C code generator employs the RVO and in-place construction
      # at the moment
      preventRvo(tree, c)
      # XXX: use the fixup pass for all targets. Both the VM and JavaScript
      #      targets are also affected
      fixupCallArguments(tree, config, c)

  batch:
    lowerSwap(tree, c)