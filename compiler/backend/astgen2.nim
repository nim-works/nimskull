## This module implements the MIR -> ``CgNode`` AST translation. The translation
## preserves the semantics and uses the smallest ``CgNode`` constructs that best
## match the respective MIR construct. Note that not all constructs in the MIR
## have a direct ``CgNode`` counterpart: those require more complex translation
## logic.
##
## The translation is implemented via recursive application of the correct
## translation procedure, where each procedure processes a sub-tree either
## directly or via further recursion. Instead of one uber procedure, multiple
## smaller ones that closely match the grammer are used. This allows for
## validating that the input MIR code is grammatically correct with effectively
## no overhead and without requiring extra contextual data or a separate pass.
##
## ============
## MIR sections
## ============
##
## There exists no equivalent to MIR sections in the ``CgNode`` AST, so a more
## complex translation has to be used. At the start of the section, each
## section argument is assigning to a temporary, using either a ``var`` /
## ``lent`` view or shallow copy depending on the argument's mode and
## type. A section parameter reference (``mnkOpParam``) is then translated to
## accessing the temporary introduce for the parameter's argument.

import
  compiler/ast/[
    ast,
    ast_types,
    ast_idgen,
    ast_query,
    idents,
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
    modulegraphs
  ],
  compiler/utils/[
    containers,
    idioms
  ]

from compiler/sem/semdata import makeVarType
from compiler/sem/typeallowed import directViewType, noView

# TODO: move the procedure somewhere common
from compiler/vm/vmaux import findRecCase

type
  OriginInfo = PNode

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
    cache: IdentCache
    idgen: IdGenerator

    owner: PSym

    params: Values
    nextTemp: uint32

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
    origin {.cursor.}: PNode ## the node

template isFilled(x: ref): bool = not x.isNil

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

proc `[]=`(n: CgNode, i: Natural, child: sink CgNode) =
  n.childs[i] = child

proc `[]`(n: CgNode, i: Natural): CgNode =
  n.childs[i]

func add(n: CgNode, child: sink CgNode) =
  n.childs.add child

func newNode(kind: CgNodeKind, origin: OriginInfo = nil): CgNode =
  CgNode(kind: kind, origin: origin)

func newTree(kind: CgNodeKind, origin: OriginInfo, childs: varargs[CgNode]): CgNode =
  {.cast(uncheckedAssign).}:
    CgNode(kind: kind, origin: origin, childs: @childs)

func newExpr(kind: CgNodeKind, origin: OriginInfo; typ: PType, childs: varargs[CgNode]): CgNode =
  {.cast(uncheckedAssign).}:
    CgNode(kind: kind, origin: origin, typ: typ, childs: @childs)

func newStmt(kind: CgNodeKind, origin: OriginInfo; childs: varargs[CgNode]): CgNode =
  {.cast(uncheckedAssign).}:
    CgNode(kind: kind, origin: origin, childs: @childs)

func get(t: TreeWithSource, cr: var TreeCursor): lent MirNode {.inline.} =
  cr.origin = sourceFor(t.map, cr.pos.NodeInstance)
  result = t.tree[cr.pos]

  inc cr.pos

func enter(t: TreeWithSource, cr: var TreeCursor): lent MirNode {.inline.} =
  assert t.tree[cr.pos].kind in SubTreeNodes, "not a sub-tree"
  result = get(t, cr)

func leave(t: TreeWithSource, cr: var TreeCursor) =
  assert t.tree[cr.pos].kind == mnkEnd, "not at the end of sub-tree"
  inc cr.pos

template info(cr: TreeCursor): OriginInfo =
  cr.origin

template `[]`(t: TreeWithSource, cr: TreeCursor): untyped =
  t.tree[cr.pos]

template hasNext(cr: TreeCursor, t: TreeWithSource): bool =
  cr.pos.int < t.tree.len

func toMode(kind: range[mnkArg..mnkConsume]): ArgumentMode =
  case kind
  of mnkArg:     amValue
  of mnkName:    amName
  of mnkConsume: amConsume

proc copySubTree[A, B](source: PNode, slice: HSlice[A, B], to: PNode) =
  ## Copies all sub-nodes from the `slice` in `source` to the end of `to`,
  ## using a full sub-tree copy (i.e. ``copyTree``)
  # XXX: using an ``openArray`` instead of a ``PNode`` + ``HSlice`` pair
  #      would simplify this procedure a lot. As of the time of this comment
  #      being written, creating an openArray from a node is rather cumbersome
  #      however
  let
    a = source ^^ slice.a
    b = source ^^ slice.b

  if a > b:
    return

  # resize the node list first:
  let start = to.len
  to.sons.setLen(start + (b - a) + 1)

  # then copy all nodes:
  for i in a..b:
    to[start + (i - a)] = source[i]

func copy(source: CgNode): CgNode =
  result = CgNode()
  result[] = source[]

func copyTree(source: CgNode): CgNode =
  result = CgNode()
  result[] = source[]
  # TODO: fixme

func newLiteral(n: PNode): CgNode =
  CgNode(kind: cgnkLiteralData, typ: n.typ, data: n)

func newField(sym: PSym): CgNode =
  CgNode(kind: cgnkField, sym: sym)

func addIfNotEmpty(stmts: var seq[CgNode], n: sink CgNode) =
  ## Only adds the node to the list if it's not an empty node. Used to prevent
  ## the creation of statement-list expressions that only consist of empty
  ## nodes + the result-expression (a statement-list expression is unnecessary
  ## in that case)
  if n.kind != cgnkEmpty:
    stmts.add n

func toSingleNode(stmts: sink seq[CgNode]): CgNode =
  ## Creates a single ``CgNode`` from a list of *statements*
  case stmts.len
  of 0:
    result = newNode(cgnkEmpty)
  of 1:
    result = move stmts[0]
  else:
    result = newNode(cgnkStmtList)
    result.childs = move stmts

proc wrapArg(stmts: seq[CgNode], info: OriginInfo, val: CgNode): CgNode =
  ## If there are extra statements (i.e. `stmts` is not empty), wraps the
  ## statements + result-expression into an ``nkStmtListExpr``. Otherwise,
  ## returns `val` as is
  if stmts.len == 0:
    result = val
  else:
    assert val.kind != cgnkStmtListExpr
    result = newExpr(cgnkStmtListExpr, info, val.typ, stmts)
    result.add val

func unwrap(expr: CgNode, stmts: var seq[CgNode]): CgNode =
  if expr.kind == cgnkStmtListExpr:
    for it in expr.stmts:
      stmts.add it

    result = expr.source
  else:
    result = expr

proc newTemp(cl: var TranslateCl, info: OriginInfo, typ: PType): CgNode =
  ## Creates and returns a new ``skTemp`` symbol
  result = CgNode(kind: cgnkTemp, origin: info, typ: typ, temp: TempId cl.nextTemp)
  inc cl.nextTemp

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

proc genBranchLabels(n: PNode, dest: CgNode) =
  template lit(x: PNode): CgNode =
    CgNode(kind: cgnkLiteralData, typ: x.typ, data: x)

  for i, it in branchLabels(n):
    if it.kind == nkRange:
      dest.add newTree(cgnkRange, it, [lit(it[0]), lit(it[1])])
    else:
      dest.add CgNode(kind: cgnkLiteralData, typ: it.typ, data: it)

proc buildCheck(cl: var TranslateCl, recCase: PNode, pos: Natural,
                info: OriginInfo): CgNode =
  ## Builds the boolean expression testing if `discr` is part of the branch
  ## with position `pos`
  assert recCase.kind == nkRecCase
  let
    discr = recCase[0] ## the node holding the discriminant symbol
    branch = recCase[1 + pos]
    setType = newType(tySet, nextTypeId(cl.idgen), cl.owner)

  rawAddSon(setType, discr.typ) # the set's element type

  var
    lit = newExpr(cgnkSetConstr, info, setType)
    invert = false

  case branch.kind
  of nkOfBranch:
    # use the branch labels as the set to test against
    genBranchLabels(branch, lit)
  of nkElse:
    # iterate over all branches except the ``else`` branch and collect their
    # labels
    for i in 1..<recCase.len-1:
      let b = recCase[i]
      genBranchLabels(b, lit)

    invert = true
  else:
    unreachable()

  # create a ``contains(lit, discr)`` expression:
  let inExpr = lit

  if invert:
    result =
      newExpr(cgnkCall, info, setType, [
        CgNode(kind: cgnkMagic, magic: mNot),
        lit
      ])
  else:
    result = lit

proc addToVariantAccess(cl: var TranslateCl, dest: CgNode, field: PSym,
                        info: OriginInfo): CgNode =
  ## Appends a field access for a field inside a record branch to `dest`
  ## (transforming it into a ``nkCheckedFieldExpr`` if it isn't one already)
  ## and returns the resulting expression
  var node = dest

  # TODO: generating a field check (i.e. ``nkCheckedFieldExpr``) should not
  #       done by the code-generators, but instead happen at the MIR level as
  #       a MIR pass. In other words, a MIR pass should insert an 'if' +
  #       'raise' for each access to a field inside a record branch (but only
  #       if ``optFieldCheck`` is enabled) and no ``nkCheckedFieldExpr`` should
  #       be generated here

  assert node.kind == cgnkObjAccess

  let
    # the symbol of the discriminant is on the right-side of the dot-expr
    discr = node.field
    recCase = findRecCase(node.typ.skipTypes(abstractInst+tyUserTypeClasses), discr)
    check = buildCheck(cl, recCase, findBranch(cl.graph.config, recCase, field.name),
                       info)

  result = newExpr(cgnkVariantAccess, info, node.source.typ, node[0], node[1], check)

func isSimple(n: CgNode): bool =
  ## Computes whether the l-value expression `n` always names the same valid
  ## location
  var n = n
  while true:
    case n.kind
    of cgnkGlobal, cgnkLocal, cgnkConst, cgnkProc, cgnkTemp, cgnkLiteralData:
      return true
    of cgnkObjAccess, cgnkTupleAccess:
      # ``nkCheckedFieldExpr`` is deliberately not included here because it
      # means the location is part of a variant-object-branch
      n = n.source
    of cgnkArrayAccess:
      if n.index.kind in {cgnkLiteralData, cgnkConst}:
        # an array access with a constant index value is simple -- it always
        # names the same location
        # TODO: this is not true if it's an array-like with a dynamic size
        #       (e.g. ``seq``)
        n = n.source
      else:
        return false
    else:
      return false

func underlyingLoc(n: CgNode): tuple[underlying, firstConv: CgNode] =
  ## Returns the lvalue expression stripped from any trailing lvalue
  ## conversion. For convenience, the node representing the first
  ## applied conversion is also returned. If no conversion exists, `firstConv`
  ## is equal to `underlying`
  var
    n = n
    orig = n

  while n.kind in {cgnkObjDownConv, cgnkObjUpConv}:
    orig = n
    n = n.source

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

    temp = newTemp(cl, n.origin, typ)

  # for the "undo conversion" logic to work, the expression needs to end in a
  # conversion. Creating a view from the location *after* lvalue conversion
  # would break this, so instead, a view is created from the unconverted
  # location and the conversion is applied at each usage site
  stmts.add newStmt(cgnkDef, n.origin, [temp, newExpr(cgnkView, n.origin, typ, locExpr)])

  if locExpr != conv:
    # a conversion exists. Rewrite the conversion operation to apply to the
    # dereferenced view
    conv[0] = newExpr(cgnkDerefView, n.origin, locExpr.typ, [copy(temp)])
    result = n
  else:
    result = newExpr(cgnkDerefView, n.origin, n.typ, [copy(temp)])

proc useTemporary(n: CgNode, cl: var TranslateCl, stmts: var seq[CgNode]): CgNode =
  let temp = newTemp(cl, n.origin, n.typ)
  stmts.add newStmt(cgnkDef, n.origin, [temp, n])
  result = copy(temp)

proc canUseView(n: CgNode): bool =
  ## Computes whether the expression `n` computes to something for which a
  ## view can be created
  var n {.cursor.} = n
  while true:
    case n.kind
    of cgnkAddr, cgnkView, cgnkObjAccess, cgnkTupleAccess, cgnkArrayAccess, cgnkVariantAccess,
      cgnkObjUpConv, cgnkObjDownConv, cgnkLConv:
      n = n.source
    of cgnkConv:
      return false
    of cgnkLocal, cgnkGlobal, cgnkTemp:
      # don't use a view if the location is part of a constant
      return true
    of cgnkDeref, cgnkDerefView:
      return true
    of cgnkCall:
      # if the call yields a view, use an lvalue reference (view) -- otherwise,
      # do not
      return directViewType(n.typ) != noView
    else:
      return false

proc prepareParameter(expr: CgNode, tag: ValueTags, mode: ArgumentMode,
                      cl: var TranslateCl, stmts: var seq[CgNode]): CgNode =
  let expr = unwrap(expr, stmts)
  if isSimple(expr):
    # if it's an independent expression with no side-effects, a temporary is
    # not needed and the expression can be used directly
    result = expr
  elif mode == amName or
       (skipTypes(expr.typ, abstractVarRange).kind notin IntegralTypes and
        canUseView(expr)):
    # using an lvalue reference (view) is preferred for complex values
    result = useLvalueRef(expr, vtMutable in tag, cl, stmts)
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

proc genObjConv(n: CgNode, a, b, t: PType): CgNode =
  ## Depending on the relationship between `a` and `b`, wraps `n` in either an
  ## up- or down-conversion. `t` is the type to use for the resulting
  ## expression
  let diff = inheritanceDiff(b, a)
  #echo "a: ", a.sym.name.s, "; b: ", b.sym.name.s
  #assert diff != 0 and diff != high(int), "redundant or illegal conversion"
  if diff == 0:
    return nil
  result = newExpr(
    if diff < 0: cgnkObjUpConv else: cgnkObjDownConv,
    n.origin, t): n

# forward declarations:
proc tbSeq(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): Values

proc tbStmt(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): CgNode {.inline.}
proc tbList(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): CgNode

proc tbScope(tree: TreeWithSource, cl: var TranslateCl, n: MirNode, cr: var TreeCursor): CgNode

proc tbRegion(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
              cr: var TreeCursor): CgNode

proc handleSpecialConv(c: ConfigRef, n: CgNode, info: OriginInfo,
                       dest: PType): CgNode =
  ## Checks if a special conversion operator is required for a conversion
  ## between the source type (i.e. that of `n`) and the destination type.
  ## If it is, generates the conversion operation AST and returns it -- nil
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
      if n.kind in {cgnkObjUpConv, cgnkObjDownConv} and sameType(dest, n.source.typ):
        # two conversions that cancel each other out. We eliminate them here,
        # so that the code-generators don't have to
        result = n.source
      else:
        result = genObjConv(n, source.base, dest.base, orig)

  of tyInt..tyInt64, tyEnum, tyChar, tyUInt8..tyUInt32:
    # TODO: including ``tyUInt64`` here causes rvmIllegalConv errors for code
    #       that is run in the VM. ``transf`` (from where the logic was copied
    #       from) also doesn't include it. Find out what the underlying problem
    #       is, fix it, and include ``tyUInt64`` here
    # TODO: introducing and lowering range checks into an if + raise should
    #       happen at the MIR level as a MIR pass (maybe even earlier) instead
    #       of requiring the code-generators to implement this logic
    if isOrdinalType(source) and               # is it a float-to-int conversion?
       (firstOrd(c, orig) > firstOrd(c, n.typ) or
        lastOrd(c, n.typ) > lastOrd(c, orig)): # is dest not a sub-range of source?
      # generate a range check:
      let
        rangeDest = skipTypes(orig, abstractVar)
        kind =
          if tyInt64 in {dest.kind, source.kind}: cgnkChckRange64
          else:                                   cgnkChckRange

      result = newExpr(kind, info, orig):
        [n,
         newLiteral newIntTypeNode(firstOrd(c, rangeDest), rangeDest),
         newLiteral newIntTypeNode(lastOrd(c, rangeDest), rangeDest)]
  of tyFloat..tyFloat128:
    let rangeDest = skipTypes(orig, abstractVar)
    if rangeDest.kind == tyRange:
      # a conversion to a float range (e.g. ``range[0.0 .. 1.0]``)
      result = newExpr(cgnkChckRangeF, info, orig):
        [n, newLiteral(rangeDest.n[0]), newLiteral(rangeDest.n[1])]

  else:
    result = nil

proc tbConv(cl: TranslateCl, n: CgNode, info: OriginInfo, dest: PType): CgNode =
  ## Generates the AST for an expression that performs a type conversion for
  ## `n` to type `dest`
  result = handleSpecialConv(cl.graph.config, n, info, dest)
  if result == nil:
    # no special conversion is used
    result = newExpr(cgnkConv, info, dest): n

proc tbSingle(n: MirNode, cl: TranslateCl, info: OriginInfo): CgNode =
  case n.kind
  of mnkProc, mnkConst, mnkParam, mnkGlobal, mnkLocal:
    CgNode(kind: cgnkProc, origin: info, typ: n.sym.typ, sym: n.sym)
  of mnkTemp:
    CgNode(kind: cgnkTemp, origin: info, temp: n.temp)
  of mnkLiteral:
    CgNode(kind: cgnkLiteralData, origin: info, typ: n.lit.typ, data: n.lit)
  of mnkType:
    CgNode(kind: cgnkType, origin: info, typ: n.typ)
  else:
    unreachable("not an atom: " & $n.kind)

proc tbExceptItem(tree: TreeWithSource, cl: TranslateCl, cr: var TreeCursor
                 ): CgNode =
  let n {.cursor.} = get(tree, cr)
  case n.kind
  of mnkPNode: unreachable("imported exception; not implemented yet")
  of mnkType:  newExpr(cgnkType, cr.info, n.typ)
  else:        unreachable()


proc tbDef(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
           n: MirNode, cr: var TreeCursor): CgNode =
  ## Translates a 'def'-like construct
  assert n.kind in DefNodes
  let
    entity {.cursor.} = get(tree, cr) # the name of the defined entity
    info = cr.info

  var ent: CgNode

  case entity.kind
  of mnkLocal, mnkGlobal:
    ent = tbSingle(entity, cl, info)
  of mnkParam:
    # not relevant to the code-generators; ignore
    discard
  of mnkProc:
    # skip the definition
    discard get(tree, cr)
  of mnkTemp:
    ent = CgNode(kind: cgnkTemp, origin: info, typ: n.typ, temp: entity.temp)
  else:
    unreachable()

  leave(tree, cr)

  if ent != nil:
    let value =
      case prev.kind
      of vkNone:   newNode(cgnkEmpty, cr.info)
      of vkSingle: prev.single
      of vkMulti:  unreachable()
    result = newStmt(cgnkDef, info, ent, value)
  else:
    # return an 'empty' node; it gets eliminated later
    result = newNode(cgnkEmpty, info)

proc translate(n: PNode): CgNode =
  case n.kind
  of nkConstSection:
    # placed in MIR code for compatibility with the IC back-end
    # TODO: remove this branch once all code-generators use ``CgNode``
    result = CgNode(kind: cgnkEmpty)
  of nkPragma:
    unreachable("missing")
  of nkAsmStmt:
    # TODO: we need support from ``mirgen`` here
    # for it in t.sons:
    #   case it.kind
    #   of nkStrLit..nkTripleStrLit:
    #   of nkSym:
    #     if sym.kind in {skProc, skFunc, skIterator, skMethod}:
    #     elif sym.kind == skType:
    #     else:
    #   of nkTypeOfExpr:
    #   else:
    #result = CgNode(kind: cgnkAsm, )
    discard
  else:
    unreachable(n.kind)

proc tbSingleStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
                  cr: var TreeCursor): CgNode =
  template body(): CgNode =
    tbStmt(tree, cl, cr)

  let info = cr.info ## the source information of `n`

  case n.kind
  of DefNodes:
    # a definition of an entity with no initial value
    result = tbDef(tree, cl, Values(kind: vkNone), n, cr)
  of mnkScope:
    result = tbScope(tree, cl, n, cr)
    leave(tree, cr)
  of mnkRepeat:
    result = newStmt(cgnkRepeat, info, body())
    leave(tree, cr)
  of mnkBlock:
    result = newStmt(cgnkBlock, info,
                     CgNode(kind: cgnkLabel, lbl: n.label), # the label
                     body())
    leave(tree, cr)
  of mnkTry:
    result = newStmt(cgnkTryStmt, info, [body()])
    assert n.len <= 2

    for _ in 0..<n.len:
      let it {.cursor.} = enter(tree, cr)

      case it.kind
      of mnkExcept:
        for _ in 0..<it.len:
          let br {.cursor.} = enter(tree, cr)
          assert br.kind == mnkBranch

          let excpt = newNode(cgnkExcept, cr.info)
          for j in 0..<br.len:
            excpt.add tbExceptItem(tree, cl, cr)

          excpt.add body()
          result.add excpt

          leave(tree, cr)

      of mnkFinally:
        result.add body()
      else:
        unreachable(it.kind)

      leave(tree, cr)

    leave(tree, cr)
  of mnkBreak:
    let label =
      if n.label.isSome: CgNode(kind: cgnkLabel, lbl: n.label)
      else:              newNode(cgnkEmpty)

    result = newStmt(cgnkBreak, info, [label])
  of mnkReturn:
    result = newStmt(cgnkReturn, info)
  of mnkPNode:
    result = translate(n.node)
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

proc tbStmt(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor
           ): CgNode {.inline.} =
  tbStmt(tree, cl, get(tree, cr), cr)

proc tbCaseStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
                prev: sink Values, cr: var TreeCursor): CgNode =
  result = newStmt(cgnkCase, cr.info, [prev.single])
  for j in 0..<n.len:
    let br {.cursor.} = enter(tree, cr)

    let branch = newTree(cgnkBranch, cr.info)
    for x in 0..<br.len:
      branch.add translate(get(tree, cr).lit)

    branch.add tbStmt(tree, cl, cr)
    result.add branch
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
    newStmt(cgnkBlitCopy, cr.info, [prev[0], prev[1]])
  of mnkInit, mnkAsgn:
    assert prev.list.len == 2
    newStmt(cgnkAsgn, cr.info, [prev[0], prev[1]])
  of mnkSwitch:
    assert prev.list.len == 2
    newStmt(cgnkSwitch, cr.info, [prev[0], prev[1]])
  of mnkIf:
    assert prev.kind == vkSingle
    let n = newStmt(cgnkIf, cr.info, [prev.single, tbStmt(tree, cl, cr)])
    leave(tree, cr)

    n
  of mnkVoid:
    # it's a void sink
    newStmt(cgnkVoidStmt, cr.info, [prev.single])
  of mnkRaise:
    newStmt(cgnkRaise, cr.info, [prev.single])
  of mnkCase:
    tbCaseStmt(tree, cl, n, prev, cr)
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
        let expr = wrapArg(stmts, cr.info, v.single)
        stmts.setLen(0)

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
    node.origin = cr.info
    toValues node
  of mnkNone:
    # it's a 'none' (i.e. empty) input
    toValues newNode(cgnkEmpty, cr.info)
  of mnkArgBlock:
    tbArgBlock(tree, cl, cr)
  of AllNodeKinds - InputNodes:
    unreachable(n.kind)

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
    var node = newExpr(cgnkCall, info, n.typ)
    node.childs.newSeq(1 + prev.len)
    node[0] = newNode(cgnkMagic, info)

    case prev.kind
    of vkNone: discard
    of vkSingle: node[1] = move prev.single
    of vkMulti:
      for i, v in prev.list.mpairs:
        node[1 + i] = move v

    toValues node
  of mnkCall:
    assert n.typ != nil
    var node = newExpr(cgnkCall, info, n.typ)
    case prev.kind
    of vkMulti:
      # pre-process the argument expressions:
      node.childs = move prev.list
    of vkSingle:
      # the procedure is called with no arguments
      node.childs = @[prev.single]
    of vkNone:
      unreachable()

    toValues node
  of mnkCast:
    toValues newExpr(cgnkCast, info, n.typ, prev.single)
  of mnkConv:
    toValues tbConv(cl, prev.single, info, n.typ)
  of mnkStdConv:
    let
      opr = prev.single
      source = opr.typ.skipTypes(abstractVarRange)
      dest = n.typ.skipTypes(abstractVarRange)

    var adjusted = CgNode(nil)

    case dest.kind
    of tyCstring:
      if source.kind == tyString:
        adjusted = newExpr(cgnkStringToCString, info, n.typ): opr

    of tyString:
      if source.kind == tyCstring:
        adjusted = newExpr(cgnkCStringToString, info, n.typ): opr

    else:
      discard

    if adjusted == nil:
      # no special conversion is used
      adjusted = newExpr(cgnkConv, info, n.typ, opr)

    toValues adjusted
  of mnkPathVariant:
    var node: CgNode
    if vtVariant in prev.tag:
      node = addToVariantAccess(cl, prev.single, n.field, info)
    else:
      # the node's ``typ`` is the type of the enclosing object not of the
      # discriminant, so we have to explicitly use the field's type here
      node = newExpr(cgnkObjAccess, info, n.field.typ, [prev.single, newField(n.field)])

    # mark the value as being a variant object. Depending on which context the
    # resulting value is used, it's either kept as is, turned  into a
    # ``nkCheckedFieldExpr``, or, if it already is one, appended to
    Values(kind: vkSingle, single: node, tag: {vtVariant})
  of mnkPathNamed:
    if vtVariant in prev.tag:
      toValues addToVariantAccess(cl, prev.single, n.field, info)
    else:
      toValues newExpr(cgnkObjAccess, info, n.typ, [prev.single, newField(n.field)])

  of mnkPathPos:
    # it's a named tuple
    toValues newExpr(cgnkTupleAccess, info, n.typ, CgNode(kind: cgnkIntLit, intVal: n.position.BiggestInt))
  of mnkPathArray:
    let node = newExpr(cgnkArrayAccess, info, n.typ)
    node.childs = move prev.list
    toValues node
  of mnkAddr:
    toValues newExpr(cgnkAddr, info, n.typ, [prev.single])
  of mnkDeref:
    toValues newExpr(cgnkDeref, info, n.typ, [prev.single])
  of mnkView:
    toValues newExpr(cgnkView, info, n.typ, [prev.single])
  of mnkDerefView:
    toValues newExpr(cgnkDerefView, info, n.typ, [prev.single])
  of mnkObjConstr:
    assert n.typ.skipTypes(abstractVarRange).kind in {tyObject, tyRef}
    var node = newExpr(cgnkObjConstr, info, n.typ)
    for j in 0..<n.len:
      let f {.cursor.} = get(tree, cr)
      node.add newTree(cgnkMap, cr.info, [newField(f.field), prev[j]])

    leave(tree, cr)
    toValues node
  of mnkConstr:
    let typ = n.typ.skipTypes(abstractVarRange)

    let kind =
      case typ.kind
      of tySet:               cgnkSetConstr
      of tyArray, tySequence: cgnkArrayConstr
      of tyTuple:             cgnkTupleConstr
      of tyProc:
        assert typ.callConv == ccClosure
        cgnkTupleConstr
      else:
        unreachable(typ.kind)

    var node = newExpr(kind, info, n.typ)
    node.childs = move prev.list

    toValues node
  of mnkTag:
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
  ## Translates a 'stmt-list' MIR structure into AST
  var stmts: seq[CgNode]
  tbList(tree, cl, stmts, cr)
  result = toSingleNode(stmts)

proc tbScope(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
             cr: var TreeCursor): CgNode =
  let info = cr.info

  var stmts: seq[CgNode]
  tbList(tree, cl, stmts, cr)

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
  ## ``CgNode`` AST
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

  case nodes.len
  of 0: newNode(cgnkEmpty)
  of 1: nodes[0]
  else:
    let r =
      if nodes[^1].typ.isEmptyType():
        # it's a statement list
        newNode(cgnkStmtList)
      else:
        newExpr(cgnkStmtListExpr, nil, nodes[^1].typ)

    r.childs = move nodes
    r

proc tb(tree: TreeWithSource, cl: var TranslateCl, start: NodePosition): CgNode =
  ## Translate `tree` back to a ``CgNode`` AST
  var cr = TreeCursor(pos: start.uint32)
  assert tree[cr].kind in InputNodes + StmtNodes,
         "start must point to the start of expression or statement"
  tbMulti(tree, cl, cr)


proc generateAST*(graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                  tree: sink MirTree, sourceMap: sink SourceMap): CgNode =
  ## Generates a ``CgNode`` tree that is semantically equivalent to `tree`,
  ## using the `idgen` to provide new IDs when creating symbols. `sourceMap`
  ## must be the ``SourceMap`` corresponding to `tree` and is used as the
  ## provider for source position information
  # TODO: `tree` and `sourceMap` are only consumed because of efficiency
  #       reasons (to get around a full copy for both). Remove ``sink`` for
  #       both once the fields of ``TreeWithSource`` are views
  var cl = TranslateCl(graph: graph, idgen: idgen, cache: graph.cache,
                       owner: owner)
  tb(TreeWithSource(tree: tree, map: sourceMap), cl, NodePosition 0)