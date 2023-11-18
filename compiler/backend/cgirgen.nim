## Implements generating ``CgNode`` code from MIR code. Together with the
## ``CgNode`` IR, this module is a work-in-progress.
##
## The translation is implemented via recursive application of the correct
## translation procedure, where each procedure processes a sub-tree either
## directly or via further recursion. Instead of one uber procedure, multiple
## smaller ones that closely match the grammer are used. This allows for
## validating that the input MIR code is grammatically correct with effectively
## no overhead and without requiring extra contextual data or a separate pass.
##
## .. note::
##   The `tb` prefix is an abbreviation of "translate back", but with the
##   introduction of the code-generator IR, this doesn't make much sense
##   anymore.
##
## ===========
## MIR regions
## ===========
##
## There exists no equivalent to MIR regions in the ``CgNode`` IR, so a more
## complex translation has to be used. At the start of the region, each
## region argument is assigned to a temporary, using either a ``var`` /
## ``lent`` view or shallow copy depending on the argument's mode and
## type. A region parameter reference (``mnkOpParam``) is then translated to
## accessing the temporary introduce for the parameter's argument.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_types,
    ast_idgen,
    ast_query,
    lineinfos,
    types
  ],
  compiler/backend/[
    cgir
  ],
  compiler/front/[
    options
  ],
  compiler/mir/[
    mirtrees,
    sourcemaps
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/utils/[
    containers,
    idioms,
    int128
  ]

from compiler/ast/ast import newSym, newType, rawAddSon
from compiler/ast/idents import whichKeyword
from compiler/sem/semdata import makeVarType

# TODO: move the procedure somewhere common
from compiler/vm/vmaux import findRecCase

type
  ValuesKind = enum
    vkNone, vkSingle, vkMulti

  ArgumentMode = enum
    amValue
    amName
    amConsume

  ValueTag = enum
    ## ``ValueTag``s are used to propgate some information forward to the
    ## value's consumer (e.g. a procedure call)
    vtMutable ## the value is a mutable lvalue meant to be passed to a ``var``
              ## parameter
    vtVariant

  ValueTags = set[ValueTag]

  Values = object
    ## Represents the inputs to an operation. A container of zero-or-more
    ## values, where each value is represented by a ``CgNode`` expression
    case kind: ValuesKind
    of vkNone:
      discard
    of vkSingle:
      single: CgNode
      tag: ValueTags
    of vkMulti:
      list: seq[CgNode]
      modeAndTags: seq[tuple[mode: ArgumentMode, tags: ValueTags]]
        ## a separate sequence is used so that the whole ``CgNode`` list can
        ## be moved into the destination node at once

  TranslateCl = object
    graph: ModuleGraph
    idgen: IdGenerator

    owner: PSym

    tempMap: SeqMap[TempId, LocalId]
      ## maps a ``TempId`` to the ID of the local created for it
    localsMap: Table[int, LocalId]
      ## maps a sybmol ID to the corresponding local. Needed because normal
      ## local variables reach here as ``PSym``s
    blocks: seq[LabelId]
      ## the stack of enclosing blocks for the currently processed node

    locals: Store[LocalId, Local]
      ## the in-progress list of all locals in the translated body

    params: Values

    # a 'def' in the MIR means that the the local starts to exists and that it
    # is accessible in all connected basic blocks part of the enclosing
    # ``mnkScope``. The ``CgNode`` IR doesn't use same notion of scope,
    # so for now, all 'def's (without the initial values) within nested
    # control-flow-related trees are moved to the start of the enclosing
    # ``mnkScope``.
    inUnscoped: bool
      ## whether the currently proceesed statement/expression is part of an
      ## unscoped control-flow context
    defs: seq[CgNode]
      ## the stack of locals/globals for which the ``cnkDef``/assignemnt needs
      ## to be inserted later

  TreeWithSource = object
    ## Combines a ``MirTree`` with its associated ``SourceMap`` for
    ## convenience. It's only meant to be used as parameter type
    # TODO: the fields don't need ownership and should thus be turned into
    #       views as soon as possible
    tree: MirTree
    map: SourceMap

  TreeCursor = object
    ## A cursor into a ``TreeWithSource``
    pos: uint32 ## the index of the currently pointed to node
    origin {.cursor.}: PNode ## the source node

template isFilled(x: LocalId): bool =
  # '0' is a valid ID, but this procedure is only used for
  # temporaries, which can never map to the result variable
  x.int != 0

template `^^`(s, i: untyped): untyped =
  # XXX: copied from ``system.nim`` because it's not exported
  (when i is BackwardsIndex: s.len - int(i) else: int(i))

func toValues(x: sink CgNode): Values {.inline.} =
  # note: having ``toValues`` be an implicit converter lead to an overload
  # resolution issue where the converter was incorrectly chosen, making
  # otherwise correct code not compile
  assert x != nil
  Values(kind: vkSingle, single: x)

func `[]`(v: Values, i: Natural): CgNode =
  if i > 0 or v.kind == vkMulti:
    v.list[i]
  else:
    v.single

func len(v: Values): int =
  case v.kind
  of vkNone:   0
  of vkSingle: 1
  of vkMulti:  v.list.len

func add(v: var Values, n: sink CgNode, tag: ValueTags, mode: ArgumentMode) =
  v.list.add n
  v.modeAndTags.add (mode, tag)

func getCalleeMagic(n: CgNode): TMagic =
  if n.kind == cnkSym: n.sym.magic
  else:                mNone

func newMagicNode(magic: TMagic, info: TLineInfo): CgNode =
  CgNode(kind: cnkMagic, info: info, magic: magic)

func get(t: TreeWithSource, cr: var TreeCursor): lent MirNode {.inline.} =
  cr.origin = t.map[t.tree[cr.pos].info]
  result = t.tree[cr.pos]

  inc cr.pos

func enter(t: TreeWithSource, cr: var TreeCursor): lent MirNode {.inline.} =
  assert t.tree[cr.pos].kind in SubTreeNodes, "not a sub-tree"
  result = get(t, cr)

func leave(t: TreeWithSource, cr: var TreeCursor) =
  assert t.tree[cr.pos].kind == mnkEnd, "not at the end of sub-tree"
  inc cr.pos

template info(cr: TreeCursor): TLineInfo =
  cr.origin.info

template `[]`(t: TreeWithSource, cr: TreeCursor): untyped =
  t.tree[cr.pos]

template hasNext(cr: TreeCursor, t: TreeWithSource): bool =
  cr.pos.int < t.tree.len

func toMode(kind: range[mnkArg..mnkConsume]): ArgumentMode =
  case kind
  of mnkArg:     amValue
  of mnkName:    amName
  of mnkConsume: amConsume

template `[]=`(x: CgNode, i: Natural, n: CgNode) =
  x.kids[i] = n

template `[]=`(x: CgNode, i: BackwardsIndex, n: CgNode) =
  x.kids[i] = n

template add(x: CgNode, y: CgNode) =
  x.kids.add y

proc copyTree(n: CgNode): CgNode =
  case n.kind
  of cnkAtoms:
    new(result)
    result[] = n[]
  of cnkWithOperand:
    result = CgNode(kind: n.kind, info: n.info, typ: n.typ)
    result.operand = copyTree(n.operand)
  of cnkWithItems:
    result = CgNode(kind: n.kind, info: n.info, typ: n.typ)
    result.kids.setLen(n.kids.len)
    for i, it in n.pairs:
      result[i] = copyTree(it)

proc newEmpty(info = unknownLineInfo): CgNode =
  CgNode(kind: cnkEmpty, info: info)

proc newTree(kind: CgNodeKind, info: TLineInfo, kids: varargs[CgNode]): CgNode =
  ## For node kinds that don't represent standalone statements.
  result = CgNode(kind: kind, info: info)
  result.kids = @kids

func newTypeNode(info: TLineInfo, typ: PType): CgNode =
  CgNode(kind: cnkType, info: info, typ: typ)

func newSymNode(s: PSym; info = unknownLineInfo): CgNode =
  CgNode(kind: cnkSym, info: info, typ: s.typ, sym: s)

func newLabelNode(blk: BlockId; info = unknownLineInfo): CgNode =
  CgNode(kind: cnkLabel, info: info, label: blk)

proc newExpr(kind: CgNodeKind, info: TLineInfo, typ: PType,
             kids: sink seq[CgNode]): CgNode =
  ## Variant of ``newExpr`` optimized for passing a pre-existing child
  ## node sequence.
  result = CgNode(kind: kind, info: info, typ: typ)
  result.kids = kids

proc newStmt(kind: CgNodeKind, info: TLineInfo,
             kids: sink seq[CgNode]): CgNode =
  ## Variant of ``newStmt`` optimized for passing a pre-existing child
  ## node sequence.
  result = CgNode(kind: kind, info: info)
  result.kids = kids

proc translateLit*(val: PNode): CgNode =
  ## Translates an ``mnkLiteral`` node to a ``CgNode``.
  ## Note that the MIR not only uses ``mnkLiteral`` for "real" literals, but
  ## also for pushing other raw ``PNode``s through the MIR phase.
  template node(k: CgNodeKind, field, value: untyped): CgNode =
    CgNode(kind: k, info: val.info, typ: val.typ, field: value)

  case val.kind
  of nkIntLiterals:
    # use the type for deciding what whether it's a signed or unsigned value
    case val.typ.skipTypes(abstractRange + {tyEnum}).kind
    of tyInt..tyInt64, tyBool:
      node(cnkIntLit, intVal, val.intVal)
    of tyUInt..tyUInt64, tyChar:
      node(cnkUIntLit, intVal, val.intVal)
    of tyPtr, tyPointer, tyProc:
      # XXX: consider adding a dedicated node for pointer-like-literals
      #      to both ``PNode`` and ``CgNode``
      node(cnkUIntLit, intVal, val.intVal)
    else:
      unreachable(val.typ.skipTypes(abstractRange).kind)
  of nkFloatLiterals:
    case val.typ.skipTypes(abstractRange).kind
    of tyFloat, tyFloat64:
      node(cnkFloatLit, floatVal, val.floatVal)
    of tyFloat32:
      # all code-generators need to do this at one point, so we help them out
      # by narrowing the value to a float32 value
      node(cnkFloatLit, floatVal, val.floatVal.float32.float64)
    else:
      unreachable()
  of nkStrKinds:
    node(cnkStrLit, strVal, val.strVal)
  of nkNilLit:
    newNode(cnkNilLit, val.info, val.typ)
  of nkNimNodeLit:
    node(cnkAstLit, astLit, val[0])
  of nkRange:
    node(cnkRange, kids, @[translateLit(val[0]), translateLit(val[1])])
  of nkBracket:
    assert val.len == 0
    # XXX: ``mirgen`` having to generate ``mnkLiteral``s for empty seq
    #      construction expressions is bad design. Fully constant
    #      construction expresssion should probably be lifted into proper
    #      constants during ``transf``
    newNode(cnkArrayConstr, val.info, val.typ)
  of nkSym:
    # special case for raw symbols used with emit and asm statements
    assert val.sym.kind == skField
    node(cnkSym, sym, val.sym)
  else:
    unreachable("implement: " & $val.kind)

proc copySubTree[A, B](source: PNode, slice: HSlice[A, B], to: var CgNode) =
  ## Translates all sub-nodes from the `slice` in `source` to ``CgNode`` and
  ## appends them to the end of `to`.
  let
    a = source ^^ slice.a
    b = source ^^ slice.b

  if a > b:
    return

  # resize the node list first:
  let start = to.len
  to.kids.setLen(start + (b - a) + 1)

  # then copy all nodes:
  for i in a..b:
    to[start + (i - a)] = translateLit(source[i])

func addIfNotEmpty(stmts: var seq[CgNode], n: sink CgNode) =
  ## Only adds the node to the list if it's not an empty node. Used to prevent
  ## the creation of statement-list expression that only consist of empty
  ## nodes + the result-expression (a statement-list expression is unnecessary
  ## in that case)
  if n.kind != cnkEmpty:
    stmts.add n

func toSingleNode(stmts: sink seq[CgNode]): CgNode =
  ## Creates a single ``CgNode`` from a list of *statements*
  case stmts.len
  of 0:
    result = newEmpty()
  of 1:
    result = move stmts[0]
  else:
    result = newNode(cnkStmtList)
    result.kids = stmts

proc wrapArg(stmts: sink seq[CgNode], info: TLineInfo, val: sink CgNode): CgNode =
  ## If there are extra statements (i.e. `stmts` is not empty), wraps the
  ## statements + result-expression into an ``cnkStmtListExpr``. Otherwise,
  ## returns `val` as is
  if stmts.len == 0:
    result = val
  else:
    assert val.kind != cnkStmtListExpr
    result = newExpr(cnkStmtListExpr, info, val.typ, stmts)
    result.add val

func newTemp(cl: var TranslateCl, typ: PType): LocalId =
  ## Creates and returns a new ``skTemp`` symbol
  cl.locals.add(Local(typ: typ, isImmutable: false))

proc newDefaultCall(info: TLineInfo, typ: PType): CgNode =
  ## Produces the tree for a ``default`` magic call.
  newExpr(cnkCall, info, typ, [newMagicNode(mDefault, info)])

proc initLocal(s: PSym): Local =
  ## Inits a ``Local`` with the data from `s`.
  result = Local(typ: s.typ, flags: s.flags, isImmutable: (s.kind == skLet),
                 name: s.name)
  if s.kind in {skVar, skLet, skForVar}:
    result.alignment = s.alignment.uint32

func findBranch(c: ConfigRef, rec: PNode, field: PIdent): int =
  ## Computes the 0-based position of the branch that `field` is part of. Only
  ## the direct child nodes of `rec` are searched, nested record-cases are
  ## ignored
  assert rec.kind == nkRecCase
  template cmpSym(s: PSym): bool =
    s.name.id == field.id

  for i, b in branches(rec):
    assert b.kind in {nkOfBranch, nkElse}
    case b.lastSon.kind
    of nkSym:
      if cmpSym(b.lastSon.sym):
        return i

    of nkRecList:
      for it in b.lastSon.items:
        let sym =
          case it.kind
          of nkSym: it.sym
          of nkRecCase: it[0].sym
          else: nil

        if sym != nil and cmpSym(sym):
          return i

    of nkRecCase:
      if cmpSym(b[0].sym):
        return i

    else:
      unreachable()

  unreachable("field is not part of the record-case")

proc buildCheck(cl: var TranslateCl, recCase: PNode, pos: Natural,
                info: TLineInfo): CgNode =
  ## Builds the boolean expression testing if `discr` is part of the branch
  ## with position `pos`
  assert recCase.kind == nkRecCase
  let
    discr = recCase[0] ## the node holding the discriminant symbol
    branch = recCase[1 + pos]
    setType = newType(tySet, nextTypeId(cl.idgen), cl.owner)

  rawAddSon(setType, discr.typ) # the set's element type

  var
    lit = newExpr(cnkSetConstr, info, setType)
    invert = false

  case branch.kind
  of nkOfBranch:
    # use the branch labels as the set to test against
    copySubTree(branch, 0..^2, lit)
  of nkElse:
    # iterate over all branches except the ``else`` branch and collect their
    # labels
    for i in 1..<recCase.len-1:
      let b = recCase[i]
      copySubTree(b, 0..^2, lit)

    invert = true
  else:
    unreachable()

  # create a ``contains(lit, discr)`` expression:
  let inExpr =
    newExpr(cnkCall, info, getSysType(cl.graph, info, tyBool), [
      newMagicNode(mInSet, info),
      lit,
      newSymNode(discr.sym)
    ])

  if invert:
    result =
      newExpr(cnkCall, info, getSysType(cl.graph, info, tyBool), [
        newMagicNode(mNot, info),
        inExpr
      ])
  else:
    result = inExpr

proc addToVariantAccess(cl: var TranslateCl, dest: CgNode, field: PSym,
                        info: TLineInfo): CgNode =
  ## Appends a field access for a field inside a record branch to `dest`
  ## (transforming it into a ``cnkCheckedFieldAccess`` if it isn't one already)
  ## and returns the resulting expression
  let node =
    case dest.kind
    of cnkFieldAccess: dest
    of cnkCheckedFieldAccess: dest[0]
    else: unreachable()

  # TODO: generating a field check (i.e. ``cnkCheckedFieldAccess``) should not
  #       be done by the code-generators, but instead happen at the MIR level as
  #       a MIR pass. In other words, a MIR pass should insert an 'if' +
  #       'raise' for each access to a field inside a record branch (but only
  #       if ``optFieldCheck`` is enabled) and no ``cnkCheckedFieldAccess`` should
  #       be generated here

  assert node.kind == cnkFieldAccess

  let
    # the symbol of the discriminant is on the right-side of the dot-expr
    discr = node[1].sym
    recCase = findRecCase(node[0].typ.skipTypes(abstractInst+tyUserTypeClasses), discr)
    check = buildCheck(cl, recCase, findBranch(cl.graph.config, recCase, field.name),
                       info)

  node[1] = newSymNode(field)
  node.typ = field.typ

  case dest.kind
  of cnkFieldAccess:
    newExpr(cnkCheckedFieldAccess, info, field.typ, [node, check])
  of cnkCheckedFieldAccess:
    # a field is accessed that is inside a nested record-case. Don't wrap the
    # ``cnkCheckedFieldAccess`` in another one -- append the check instead.
    # While the order of the checks *should* be irrelevant, we still emit them
    # in the order they were generated originally (i.e. innermost to outermost)
    dest.kids.insert(check, 1)
    # update the type of the expression:
    dest.typ = field.typ
    dest
  else:
    unreachable()

func isSimple(n: CgNode): bool =
  ## Computes if the l-value expression `n` always names the same valid
  ## location
  var n = n
  while true:
    case n.kind
    of cnkSym, cnkLiterals, cnkLocal:
      return true
    of cnkFieldAccess, cnkTupleAccess:
      # ``cnkCheckedFieldAccess`` is deliberately not included here because it
      # means the location is part of a variant-object-branch
      n = n[0]
    of cnkArrayAccess:
      if n[0].typ.skipTypes(abstractInst).kind == tyArray and
          n[1].kind in cnkLiterals:
        # arrays indexed by a constant value are allowed -- they always name
        # the same location
        n = n[0]
      else:
        return false
    else:
      return false

func underlyingLoc(n: CgNode): tuple[underlying: CgNode, firstConv: CgNode] =
  ## Returns the lvalue expression stripped from any trailing lvalue
  ## conversion. For convenience, the node representing the first
  ## applied conversion is also returned. If no conversion exists, `firstConv`
  ## is equal to `underlying`
  var
    n = n
    orig = n

  while n.kind in {cnkObjDownConv, cnkObjUpConv}:
    orig = n
    n = n.operand

  result = (n, orig)

proc useLvalueRef(n: CgNode, mutable: bool, cl: var TranslateCl,
                  stmts: var seq[CgNode]): CgNode =
  ## Generates a temporary view into the location named by the lvalue
  ## expression `n` and returns the deref expression for accessing the
  ## location
  let
    (locExpr, conv) = underlyingLoc(n)
    typ = makeVarType(cl.owner, locExpr.typ, cl.idgen,
                      (if mutable: tyVar else: tyLent))

    tmp = newTemp(cl, typ)

  # for the "undo conversion" logic to work, the expression needs to end in a
  # conversion. Creating a view from the location *after* lvalue conversion
  # would break this, so instead, a view is created from the unconverted
  # location and the conversion is applied at each usage site
  stmts.add newStmt(cnkDef, n.info,
                     [newLocalRef(tmp, n.info, typ),
                      newOp(cnkHiddenAddr, n.info, typ, locExpr)]
                   )

  if locExpr != conv:
    # a conversion exists. Rewrite the conversion operation to apply to the
    # dereferenced view
    conv.operand = newOp(cnkDerefView, n.info, locExpr.typ,
                         newLocalRef(tmp, n.info, typ))
    result = n
  else:
    result = newOp(cnkDerefView, n.info, n.typ,
                   newLocalRef(tmp, n.info, typ))

proc useTemporary(n: CgNode, cl: var TranslateCl, stmts: var seq[CgNode]): CgNode =
  let tmp = newTemp(cl, n.typ)

  stmts.add newStmt(cnkDef, n.info, [newLocalRef(tmp, n.info, n.typ), n])
  result = newLocalRef(tmp, n.info, n.typ)

proc flattenExpr*(expr: CgNode, stmts: var seq[CgNode]): CgNode =
  ## A copy of `flattenExpr <ast/trees.html#PNode,seq[PNode]>`_ adjusted for
  ## ``CgNode``.
  proc forward(n: CgNode, sub: var CgNode): CgNode =
    ## Performs transformation #1
    if sub.kind == cnkStmtListExpr:
      result = sub
      sub = sub[^1]
      result[^1] = n
    else:
      result = n

  var it = expr
  while true:
    # we're looking for expression nodes that represent side-effect free
    # operations
    case it.kind
    of cnkFieldAccess, cnkCheckedFieldAccess, cnkArrayAccess, cnkTupleAccess:
      it = forward(it, it[0])
    of cnkHiddenAddr, cnkAddr, cnkDeref, cnkDerefView, cnkCStringToString,
       cnkStringToCString, cnkObjDownConv, cnkObjUpConv, cnkConv,
       cnkHiddenConv, cnkCast:
      it = forward(it, it.operand)
    else:
      # no IR to which transform #1 applies
      discard

    if it.kind == cnkStmtListExpr:
      # transformation #2:
      for i in 0..<it.len-1:
        stmts.add it[i]

      it = it[^1]
    else:
      # we're done transforming
      break

  result = it

proc canUseView*(n: CgNode): bool =
  ## A copy of `canUseView <ast/ast_analysis.html#PNode,seq[PNode]>`_ adjusted for
  ## ``CgNode``.
  var n {.cursor.} = n
  while true:
    case n.kind
    of cnkArrayAccess, cnkTupleAccess, cnkCheckedFieldAccess, cnkFieldAccess:
      n = n[0]
    of cnkAddr, cnkHiddenAddr, cnkObjUpConv, cnkObjDownConv:
      n = n.operand
    of cnkHiddenConv, cnkConv:
      if skipTypes(n.typ, abstractVarRange).kind in {tyOpenArray, tyTuple, tyObject} or
         compareTypes(n.typ, n.operand.typ, dcEqIgnoreDistinct):
        # lvalue conversion
        n = n.operand
      else:
        return false

    of cnkSym:
      # don't use a view if the location is part of a constant
      return n.sym.kind != skConst
    of cnkLocal:
      return true
    of cnkDerefView, cnkDeref:
      return true
    of cnkCall:
      # if the call yields a view, use an lvalue reference (view) -- otherwise,
      # do not
      return classifyBackendView(n.typ) != bvcNone
    else:
      return false

proc prepareParameter(expr: CgNode, tag: ValueTags, mode: ArgumentMode,
                      cl: var TranslateCl, stmts: var seq[CgNode]): CgNode =
  let expr = flattenExpr(expr, stmts)
  if isSimple(expr):
    # if it's an independent expression with no side-effects, a temporary is
    # not needed and the expression can be used directly
    result = expr
  elif mode == amName or
       (skipTypes(expr.typ, abstractVarRange).kind notin IntegralTypes and
        canUseView(expr)):
    # using an lvalue reference (view) is preferred for complex values
    result = useLvalueRef(expr, vtMutable in tag, cl, stmts)
  elif mode == amConsume and canUseView(expr):
    # changes to the consumed value inside the region must be visible at the
    # source location, so if the source is an lvalue, we need to use an lvalue
    # reference
    result = useLvalueRef(expr, true, cl, stmts)
  else:
    # assign to a temporary first
    result = useTemporary(expr, cl, stmts)

proc prepareParameters(params: var Values, stmts: var seq[CgNode],
                       cl: var TranslateCl) =
  ## Pre-processes the given arguments so that they can be used (referenced)
  ## as region parameters. A region can be seen as an inlined procedure
  ## call, where each reference to a parameter is replaced with the
  ## corresponding argument. Argument expressions that have side-effects or
  ## depend on mutable state are first assigned to a temporary.
  case params.kind
  of vkNone:
    unreachable()
  of vkSingle:
    # arguments passed without an arg-block use the 'consume' argument mode
    params.single = prepareParameter(params.single, {}, amConsume, cl, stmts)
  of vkMulti:
    for i, param in params.list.mpairs:
      let (mode, tags) = params.modeAndTags[i]
      param = prepareParameter(param, tags, mode, cl, stmts)

proc wrapInHiddenAddr(cl: TranslateCl, n: CgNode): CgNode =
  ## Restores the ``cnkHiddenAddr`` around lvalue expressions passed to ``var``
  ## parameters. The code-generators operating on ``CgNode``-IR depend on the
  ## hidden addr to be present
  if n.typ.skipTypes(abstractInst).kind != tyVar:
    newOp(cnkHiddenAddr, n.info, makeVarType(cl.owner, n.typ, cl.idgen), n)
  else:
    # XXX: is this case ever reached? It should not be. Raw ``var`` values
    #      must never be passed directly to ``var`` parameters at the MIR
    #      level
    n

proc genObjConv(n: CgNode, a, b, t: PType): CgNode =
  ## Depending on the relationship between `a` and `b`, wraps `n` in either an
  ## up- or down-conversion. `t` is the type to use for the resulting
  ## expression
  let diff = inheritanceDiff(b, a)
  #echo "a: ", a.sym.name.s, "; b: ", b.sym.name.s
  #assert diff != 0 and diff != high(int), "redundant or illegal conversion"
  if diff == 0:
    return nil
  result = newOp(
    if diff < 0: cnkObjUpConv else: cnkObjDownConv,
    n.info, t): n

# forward declarations:
proc tbSeq(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): Values

proc tbStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
            cr: var TreeCursor): CgNode
proc tbList(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): CgNode

proc tbScope(tree: TreeWithSource, cl: var TranslateCl, n: MirNode, cr: var TreeCursor): CgNode

proc tbRegion(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
              cr: var TreeCursor): CgNode

proc handleSpecialConv(c: ConfigRef, n: CgNode, info: TLineInfo,
                       dest: PType): CgNode =
  ## Checks if a special conversion operator is required for a conversion
  ## between the source type (i.e. that of `n`) and the destination type.
  ## If it is, generates the conversion operation IR and returns it -- nil
  ## otherwise
  let
    orig = dest
    source = n.typ.skipTypes(abstractVarRange)
    dest = dest.skipTypes(abstractVarRange)

  case dest.kind
  of tyObject:
    assert source.kind == tyObject
    result = genObjConv(n, source, dest, orig)
  of tyRef, tyPtr, tyVar, tyLent:
    assert source.kind == dest.kind

    if source.base.kind == tyObject:
      if n.kind in {cnkObjUpConv, cnkObjDownConv} and
         sameType(dest, n.operand.typ.skipTypes(abstractInst)):
        # this one and the previous conversion cancel each other out. Both
        # ``cnkObjUpConv`` and ``cnkObjDownConv`` are not treated as lvalue
        # conversions when the source/dest operands are pointer/reference-like,
        # so the collapsing here is required in order to generate correct
        # code
        result = n.operand
      else:
        result = genObjConv(n, source.base, dest.base, orig)
  else:
    result = nil

proc tbConv(cl: TranslateCl, n: CgNode, info: TLineInfo, dest: PType): CgNode =
  ## Generates the ``CgNode`` IR for an ``mnkPathConv`` operation (handle
  ## conversion).
  result = handleSpecialConv(cl.graph.config, n, info, dest)
  if result == nil:
    # no special conversion is used
    result = newOp(cnkConv, info, dest, n)

proc tbSingle(n: MirNode, cl: TranslateCl, info: TLineInfo): CgNode =
  case n.kind
  of mnkProc, mnkConst, mnkGlobal:
    newSymNode(n.sym, info)
  of mnkLocal, mnkParam:
    # paramaters are treated like locals in the code generators
    assert n.sym.id in cl.localsMap
    newLocalRef(cl.localsMap[n.sym.id], info, n.sym.typ)
  of mnkTemp:
    newLocalRef(cl.tempMap[n.temp], info, n.typ)
  of mnkLiteral:
    translateLit(n.lit)
  of mnkType:
    newTypeNode(info, n.typ)
  else:
    unreachable("not an atom: " & $n.kind)

proc tbExceptItem(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor
                 ): CgNode =
  let n {.cursor.} = get(tree, cr)
  case n.kind
  of mnkPNode:
    assert n.node.kind == nkInfix
    assert n.node[1].kind == nkType
    assert n.node[2].kind == nkSym
    # the infix expression (``type as x``) signals that the except-branch is
    # a matcher for an imported exception. We translate the infix to a
    # ``cnkBinding`` node and let the code generators take care of it
    let id = cl.locals.add initLocal(n.node[2].sym)
    cl.localsMap[n.node[2].sym.id] = id
    newTree(cnkBinding, cr.info):
      [newNode(cnkType, n.node[1].info, n.node[1].typ),
       newLocalRef(id, n.node[2].info, n.node[2].typ)]
  of mnkType:  newTypeNode(cr.info, n.typ)
  else:        unreachable()


proc tbDef(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
           n: MirNode, cr: var TreeCursor): CgNode =
  ## Translates a 'def'-like construct
  assert n.kind in DefNodes
  let
    entity {.cursor.} = get(tree, cr) # the name of the defined entity
    info = cr.info

  var def: CgNode

  case entity.kind
  of mnkLocal:
    # translate the ``PSym`` to a ``Local`` and establish a mapping
    let
      sym = entity.sym
      id = cl.locals.add initLocal(sym)

    assert sym.id notin cl.localsMap, "re-definition of local"
    cl.localsMap[sym.id] = id

    def = newLocalRef(id, info, entity.typ)
  of mnkParam, mnkProc:
    # ignore 'def's for both parameters and procedures
    def = newEmpty()
  of mnkGlobal:
    def = newSymNode(entity.sym, info)
  of mnkTemp:
    # MIR temporaries are like normal locals, with the difference that they
    # are created ad-hoc and don't have any extra information attached
    assert entity.typ != nil
    let tmp = cl.locals.add Local(typ: entity.typ)

    assert entity.temp notin cl.tempMap, "re-definition of temporary"
    cl.tempMap[entity.temp] = tmp

    def = newLocalRef(tmp, info, entity.typ)
  else:
    unreachable()

  leave(tree, cr)

  case def.kind
  of cnkLocal:
    if cl.inUnscoped:
      # add the local to the list of moved definitions and only emit
      # an assignment
      cl.defs.add copyTree(def)
      result =
        case prev.kind
        of vkNone:   newEmpty(info)
        of vkSingle: newStmt(cnkAsgn, info, [def, prev.single])
        of vkMulti:  unreachable()
    elif def.kind == cnkLocal:
      result = newStmt(cnkDef, info):
        case prev.kind
        of vkNone:   [def, newEmpty()]
        of vkSingle: [def, prev.single]
        of vkMulti:  unreachable()
  of cnkSym:
    # there are no defs for globals in the ``CgNode`` IR, so we
    # emit an assignment that has the equivalent behaviour (in
    # terms of initialization)
    case prev.kind
    of vkNone:
      if sfImportc in def.sym.flags:
        # for imported globals, the 'def' only means that the symbol becomes
        # known to us, not that it starts its lifetime here -> don't
        # initialize or move it
        result = newEmpty()
      elif cl.inUnscoped:
        # move the default initialization to the start of the scope
        cl.defs.add def
        result = newEmpty()
      else:
        result = newStmt(cnkAsgn, info, [def, newDefaultCall(info, def.typ)])
    of vkSingle:
      result = newStmt(cnkAsgn, info, [def, prev.single])
    of vkMulti:
      unreachable()
  of cnkEmpty:
    result = def
  else:
    unreachable()

proc translateNode(n: PNode): CgNode =
  ## Translates the content of a ``mnkPNode`` node to a ``CgNode``.
  case n.kind
  of nkPragma:
    # XXX: consider adding a dedicated ``mnkPragma`` MIR node
    # only simple pragmas reach here
    assert n.len == 1
    assert n[0].kind == nkIdent
    CgNode(kind: cnkPragmaStmt, info: n.info, pragma: whichKeyword(n[0].ident))
  else:
    # cannot reach here
    unreachable(n.kind)

proc tbBody(tree: TreeWithSource, cl: var TranslateCl,
            cr: var TreeCursor): CgNode =
  ## Generates the ``CgNode`` tree for the body of a construct that implies
  ## some form of control-flow.
  let prev = cl.inUnscoped
  # assume the body is unscoped until stated otherwise
  cl.inUnscoped = true
  result = tbStmt(tree, cl, get(tree, cr), cr)
  cl.inUnscoped = prev

proc tbSingleStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
                  cr: var TreeCursor): CgNode =
  template body(): CgNode =
    tbBody(tree, cl, cr)

  let info = cr.info ## the source information of `n`

  case n.kind
  of DefNodes:
    # a definition of an entity with no initial value
    result = tbDef(tree, cl, Values(kind: vkNone), n, cr)
  of mnkScope:
    result = tbScope(tree, cl, n, cr)
    leave(tree, cr)
  of mnkRepeat:
    result = newStmt(cnkRepeatStmt, info, body())
    leave(tree, cr)
  of mnkBlock:
    cl.blocks.add n.label # push the label to the stack
    result = newStmt(cnkBlockStmt, info,
                     newLabelNode(cl.blocks.high.BlockId, info),
                     body())
    cl.blocks.setLen(cl.blocks.len - 1) # pop block from the stack
    leave(tree, cr)
  of mnkTry:
    result = newStmt(cnkTryStmt, info, [body()])
    assert n.len <= 2

    for _ in 0..<n.len:
      let it {.cursor.} = enter(tree, cr)

      case it.kind
      of mnkExcept:
        for _ in 0..<it.len:
          let br {.cursor.} = enter(tree, cr)
          assert br.kind == mnkBranch

          let excpt = newNode(cnkExcept, cr.info)
          for j in 0..<br.len:
            excpt.add tbExceptItem(tree, cl, cr)

          excpt.add body()
          result.add excpt

          leave(tree, cr)

      of mnkFinally:
        result.add newTree(cnkFinally, cr.info, body())
      else:
        unreachable(it.kind)

      leave(tree, cr)

    leave(tree, cr)
  of mnkBreak:
    # find the stack index of the enclosing 'block' identified by the break's
    # label; we use the index as the ID
    var idx = cl.blocks.high
    while idx >= 0 and cl.blocks[idx] != n.label:
      dec idx
    result = newStmt(cnkBreakStmt, info, [newLabelNode(BlockId idx, info)])
  of mnkReturn:
    result = newNode(cnkReturnStmt, info)
  of mnkPNode:
    result = translateNode(n.node)
  of AllNodeKinds - StmtNodes:
    unreachable(n.kind)

proc tbStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
            cr: var TreeCursor): CgNode =
  case n.kind
  of mnkStmtList:
    result = tbList(tree, cl, cr)
    leave(tree, cr)
  else:
    result = tbSingleStmt(tree, cl, n, cr)

proc tbSingleStmt(tree: TreeWithSource, cl: var TranslateCl,
                  cr: var TreeCursor): CgNode {.inline.} =
  tbSingleStmt(tree, cl, get(tree, cr), cr)

proc tbCaseStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
                prev: sink Values, cr: var TreeCursor): CgNode =
  result = newStmt(cnkCaseStmt, cr.info, [prev.single])
  for j in 0..<n.len:
    let br {.cursor.} = enter(tree, cr)

    result.add newTree(cnkBranch, cr.info)
    if br.len > 0:
      for x in 0..<br.len:
        result[^1].add translateLit(get(tree, cr).lit)

    result[^1].add tbBody(tree, cl, cr)
    leave(tree, cr)

  leave(tree, cr)

proc tbOut(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
           cr: var TreeCursor): CgNode =
  let n {.cursor.} = get(tree, cr)
  case n.kind
  of DefNodes:
    tbDef(tree, cl, prev, n, cr)
  of mnkRegion:
    tbRegion(tree, cl, prev, cr)
  of mnkFastAsgn:
    assert prev.list.len == 2
    newStmt(cnkFastAsgn, cr.info, [prev[0], prev[1]])
  of mnkInit, mnkAsgn:
    assert prev.list.len == 2
    newStmt(cnkAsgn, cr.info, [prev[0], prev[1]])
  of mnkSwitch:
    assert prev.list.len == 2
    # XXX: should be lowered into either a magic or normal assignment via
    #      a MIR pass
    newStmt(cnkFastAsgn, cr.info, [prev[0], prev[1]])
  of mnkIf:
    assert prev.kind == vkSingle
    let n = newStmt(cnkIfStmt, cr.info, [prev.single, tbBody(tree, cl, cr)])
    leave(tree, cr)

    n
  of mnkVoid:
    # it's a void sink
    assert prev.kind == vkSingle
    if prev.single.typ.isEmptyType():
      # a void call doesn't need to be discarded
      prev.single
    else:
      newStmt(cnkVoidStmt, cr.info, [prev.single])

  of mnkRaise:
    newStmt(cnkRaiseStmt, cr.info, [prev.single])
  of mnkCase:
    tbCaseStmt(tree, cl, n, prev, cr)
  of mnkAsm:
    newStmt(cnkAsmStmt, cr.info, move prev.list)
  of mnkEmit:
    newStmt(cnkEmitStmt, cr.info, move prev.list)
  of AllNodeKinds - OutputNodes:
    unreachable(n.kind)


proc tbArgBlock(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor
               ): Values =
  var stmts: seq[CgNode]
  result = Values(kind: vkMulti)

  while true:
    case tree[cr].kind
    of InputNodes:
      let v = tbSeq(tree, cl, cr)
      case tree[cr].kind
      of ArgumentNodes:
        let n {.cursor.} = get(tree, cr)
        # bundle the statements (if any) and the direct expression together,
        # and reset the collected statements:
        let expr = wrapArg(move stmts, cr.info, v.single)
        result.add(expr, v.tag, toMode(n.kind))
      of OutputNodes:
        stmts.add tbOut(tree, cl, v, cr)
      else:
        unreachable()

    of StmtNodes:
      stmts.addIfNotEmpty tbSingleStmt(tree, cl, cr)
    of mnkEnd:
      break
    else:
      unreachable(tree[cr].kind)

  leave(tree, cr)

  assert stmts.len == 0, "argument block has trailing statements"

proc tbInput(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor
            ): Values =
  let n {.cursor.} = get(tree, cr)
  case n.kind
  of mnkProc..mnkTemp, mnkLiteral, mnkType:
    toValues tbSingle(n, cl, cr.info)
  of mnkOpParam:
    # we need a full copy since the parameter may be referenced multiple times
    let node = copyTree(cl.params[n.param])
    node.info = cr.info
    toValues node
  of mnkNone:
    # it's a 'none' (i.e. empty) input
    let node = newEmpty(cr.info)
    node.typ = n.typ
    toValues node
  of mnkArgBlock:
    tbArgBlock(tree, cl, cr)
  of AllNodeKinds - InputNodes:
    unreachable(n.kind)

proc tbArgs(v: var Values, m: TMagic, cl: TranslateCl) =
  ## The operands to some magics (those in the ``FakeVarParams`` set) must
  ## not be wrapped in ``cnkHiddenAddr`` nodes.
  if m notin FakeVarParams:
    case v.kind
    of vkSingle:
      if vtMutable in v.tag:
        v.single = wrapInHiddenAddr(cl, v.single)

    of vkMulti:
      for i, n in v.list.mpairs:
        if vtMutable in v.modeAndTags[i].tags:
          n = wrapInHiddenAddr(cl, n)

    of vkNone:
      discard "nothing to do"

proc tbInOut(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
             cr: var TreeCursor): Values =
  ## Handles the translation of operations that accept input and produce
  ## a value (a 'in-out-op' in the grammar)
  let
    n {.cursor.} = get(tree, cr)
    info = cr.info

  case n.kind
  of mnkMagic:
    assert n.typ != nil
    tbArgs(prev, n.magic, cl)

    var node = newExpr(cnkCall, info, n.typ)
    node.kids.newSeq(1 + prev.len)
    node.kids[0] = newMagicNode(n.magic, info)

    case prev.kind
    of vkNone: discard
    of vkSingle: node.kids[1] = move prev.single
    of vkMulti:
      for i, v in prev.list.mpairs:
        node.kids[1 + i] = move v

    toValues node
  of mnkCall:
    assert n.typ != nil
    var node = newExpr(cnkCall, info, n.typ)
    case prev.kind
    of vkMulti:
      # pre-process the argument expressions:
      tbArgs(prev, getCalleeMagic(prev.list[0]), cl)

      node.kids = move prev.list
    of vkSingle:
      # the procedure is called with no arguments
      node.kids = @[prev.single]
    of vkNone:
      unreachable()

    toValues node
  of mnkCast:
    toValues newOp(cnkCast, info, n.typ, prev.single)
  of mnkConv:
    toValues newOp(cnkConv, info, n.typ, prev.single)
  of mnkStdConv:
    let
      opr = prev.single
      source = opr.typ.skipTypes(abstractVarRange)
      dest = n.typ.skipTypes(abstractVarRange)

    var adjusted: CgNode

    case dest.kind
    of tyCstring:
      if source.kind == tyString:
        adjusted = newOp(cnkStringToCString, info, n.typ): opr

    of tyString:
      if source.kind == tyCstring:
        adjusted = newOp(cnkCStringToString, info, n.typ): opr

    of tyOpenArray, tyVarargs:
      # the old code-generators depend on conversions to ``openArray`` to be
      # omitted
      adjusted = opr
    else:
      discard

    if adjusted == nil:
      # no special conversion is used
      adjusted = newOp(cnkHiddenConv, info, n.typ, opr)

    toValues adjusted
  of mnkPathVariant:
    var node: CgNode
    if vtVariant in prev.tag:
      node = addToVariantAccess(cl, prev.single, n.field, info)
    else:
      # the node's ``typ`` is the type of the enclosing object not of the
      # discriminant, so we have to explicitly use the field's type here
      node = newExpr(cnkFieldAccess, info, n.field.typ, [prev.single, newSymNode(n.field)])

    # mark the value as being a variant object. Depending on which context the
    # resulting value is used, it's either kept as is, turned  into a
    # ``cnkCheckedFieldAccess``, or, if it already is one, appended to
    Values(kind: vkSingle, single: node, tag: {vtVariant})
  of mnkPathNamed:
    if vtVariant in prev.tag:
      toValues addToVariantAccess(cl, prev.single, n.field, info)
    else:
      toValues newExpr(cnkFieldAccess, info, n.typ, [prev.single, newSymNode(n.field)])

  of mnkPathPos:
    toValues newExpr(cnkTupleAccess, info, n.typ,
      [prev.single, CgNode(kind: cnkIntLit, intVal: n.position.BiggestInt)])

  of mnkPathArray:
    toValues newExpr(cnkArrayAccess, info, n.typ, move prev.list)
  of mnkPathConv:
    toValues tbConv(cl, prev.single, info, n.typ)
  of mnkAddr:
    toValues newOp(cnkAddr, info, n.typ, prev.single)
  of mnkDeref:
    toValues newOp(cnkDeref, info, n.typ, prev.single)
  of mnkView:
    toValues newOp(cnkHiddenAddr, info, n.typ, prev.single)
  of mnkDerefView:
    toValues newOp(cnkDerefView, info, n.typ, prev.single)
  of mnkObjConstr:
    assert n.typ.skipTypes(abstractVarRange).kind in {tyObject, tyRef}
    var node = newExpr(cnkObjConstr, info, n.typ)
    for j in 0..<n.len:
      let f {.cursor.} = get(tree, cr)
      node.add newTree(cnkBinding, cr.info, [newSymNode(f.field), prev[j]])

    leave(tree, cr)
    toValues node
  of mnkConstr:
    let typ = n.typ.skipTypes(abstractVarRange)

    let kind =
      case typ.kind
      of tySet:               cnkSetConstr
      of tyArray, tySequence: cnkArrayConstr
      of tyTuple:             cnkTupleConstr
      of tyProc:
        assert typ.callConv == ccClosure
        cnkClosureConstr
      else:
        unreachable(typ.kind)

    toValues newExpr(kind, info, n.typ, move prev.list)
  of mnkTag:
    if n.effect in {ekMutate, ekReassign, ekInvalidate, ekKill}:
      prev.tag.incl vtMutable

    prev
  of AllNodeKinds - InOutNodes:
    unreachable(n.kind)

proc tbSeq(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): Values =
  ## Translate a 'sequence' MIR syntax construct
  result = tbInput(tree, cl, cr)
  while tree[cr].kind notin OutputNodes + ArgumentNodes:
    result = tbInOut(tree, cl, result, cr)

  assert result.kind != vkNone


proc tbList(tree: TreeWithSource, cl: var TranslateCl, stmts: var seq[CgNode],
            cr: var TreeCursor) =
  while true:
    case tree[cr].kind
    of InputNodes:
      let v = tbSeq(tree, cl, cr)
      stmts.add tbOut(tree, cl, v, cr)
    of StmtNodes:
      stmts.addIfNotEmpty tbSingleStmt(tree, cl, cr)
    of mnkEnd:
      # don't consume the end node
      break
    else:
      unreachable(tree[cr].kind)

proc tbList(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): CgNode =
  ## Translates a 'stmt-list' MIR structure into ``CgNode`` IR.
  var stmts: seq[CgNode]
  tbList(tree, cl, stmts, cr)
  result = toSingleNode(stmts)

proc genDefFor(sym: sink CgNode): CgNode =
  ## Produces the statement tree of a definition for the given symbol-like
  ## node. Globals use an assignment.
  case sym.kind
  of cnkLocal:
    newStmt(cnkDef, sym.info, [sym, newEmpty()])
  of cnkSym:
    # emulate the default-initialization behaviour
    newStmt(cnkAsgn, sym.info, [sym, newDefaultCall(sym.info, sym.typ)])
  else:
    unreachable()

proc tbScope(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
             cr: var TreeCursor): CgNode =
  let
    prev = cl.defs.len
    prevInUnscoped = cl.inUnscoped

  # a scope is entered, meaning that we're no longer in an unscoped context
  cl.inUnscoped = false

  var stmts: seq[CgNode]
  tbList(tree, cl, stmts, cr)

  if cl.defs.len > prev:
    # insert all the lifted defs at the start
    for i in countdown(cl.defs.high, prev):
      stmts.insert genDefFor(move cl.defs[i])

    # "pop" the elements that were added as part of this scope:
    cl.defs.setLen(prev)

  cl.inUnscoped = prevInUnscoped

  result = toSingleNode(stmts)

proc tbRegion(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
              cr: var TreeCursor): CgNode =
  var stmts: seq[CgNode]
  prepareParameters(prev, stmts, cl)

  swap(cl.params, prev)
  # `cl.params` now stores the prepared parameters (and `prev` the ones of the
  # enclosing region, if any)

  # translate the body of the region:
  tbList(tree, cl, stmts, cr)
  leave(tree, cr)

  # restore the parameters of the enclosing region (if any):
  swap(cl.params, prev)

  result = toSingleNode(stmts)


proc tbExpr(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor
           ): tuple[node: CgNode, atEnd: bool] =
  ## Translates the expression located at the current cursor position `cr` to
  ## ``CgNode`` IR
  template hasNext(): bool =
    cr.pos.int < tree.tree.len

  # translate the operation sequence while taking into account that we might
  # reach the end of the tree:
  var values = tbInput(tree, cl, cr)
  while hasNext() and tree[cr].kind notin OutputNodes + ArgumentNodes:
    values = tbInOut(tree, cl, values, cr)

  # also translate the output (if one exists):
  if not hasNext() or tree[cr].kind in ArgumentNodes:
    (values.single, true)
  elif tree[cr].kind in OutputNodes:
    (tbOut(tree, cl, values, cr), false)
  else:
    unreachable("illformed MIR")

proc tbMulti(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): CgNode =
  ## Translates expressions/statements until the cursor either reaches the end
  ## or a top-level argument node is encountered
  var nodes: seq[CgNode]
  while cr.hasNext(tree):
    case tree[cr].kind
    of InputNodes:
      let (n, atEnd) = tbExpr(tree, cl, cr)
      nodes.add n

      if atEnd:
        # we also abort if we reach an argument node, so the loop condition
        # alone is not enough
        break
    of StmtNodes:
      nodes.addIfNotEmpty tbSingleStmt(tree, cl, cr)
    else:
      unreachable("illformed MIR code")

  # insert the var section for the collected defs at the start:
  if cl.defs.len > 0:
    for i in countdown(cl.defs.high, 0):
      nodes.insert genDefFor(move cl.defs[i])

  case nodes.len
  of 0: newEmpty()
  of 1: nodes[0]
  else:
    if nodes[^1].typ.isEmptyType():
      # it's a statement list
      newStmt(cnkStmtList, unknownLineInfo, nodes)
    else:
      newExpr(cnkStmtListExpr, unknownLineInfo, nodes[^1].typ, nodes)

proc tb(tree: TreeWithSource, cl: var TranslateCl, start: NodePosition): CgNode =
  ## Translate `tree` back to a ``CgNode`` IR
  var cr = TreeCursor(pos: start.uint32)
  assert tree[cr].kind in InputNodes + StmtNodes,
         "start must point to the start of expression or statement"
  tbMulti(tree, cl, cr)

proc generateIR*(graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                  tree: sink MirTree, sourceMap: sink SourceMap): Body =
  ## Generates the ``CgNode`` IR corresponding to the input MIR code (`tree`),
  ## using `idgen` for provide new IDs when creating symbols. `sourceMap`
  ## must be the ``SourceMap`` corresponding to `tree` and is used as the
  ## provider for source position information
  var cl = TranslateCl(graph: graph, idgen: idgen, owner: owner)
  if owner.kind in routineKinds:
    # setup the locals and associated mappings for the parameters
    template add(v: PSym) =
      let s = v
      cl.localsMap[s.id] = cl.locals.add initLocal(s)

    let sig =
      if owner.kind == skMacro: owner.internal
      else:                     owner.typ

    # result variable:
    if sig[0].isEmptyType():
      # always reserve a slot for the result variable, even if the latter is
      # not present
      discard cl.locals.add(Local())
    else:
      add(owner.ast[resultPos].sym)

    # normal parameters:
    for i in 1..<sig.len:
      add(sig.n[i].sym)

    if sig.callConv == ccClosure:
      # environment parameter
      add(owner.ast[paramsPos][^1].sym)

  result = Body()
  result.code = tb(TreeWithSource(tree: tree, map: sourceMap), cl,
                  NodePosition 0)
  result.locals = cl.locals
