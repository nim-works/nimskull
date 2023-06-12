## This module implements the MIR -> ``PNode`` AST translation. The translation
## preserves the semantics and uses the smallest ``PNode`` constructs that best
## match the respective MIR construct. Note that not all constructs in the MIR
## have a direct ``PNode`` counterpart: those require more complex translation
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
## There exists no equivalent to MIR sections in the ``PNode`` AST, so a more
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
    trees,
    types
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
  compiler/sem/[
    ast_analysis,
    lowerings
  ],
  compiler/utils/[
    containers,
    idioms
  ]

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
    ## values, where each value is represented by a ``PNode`` expression
    case kind: ValuesKind
    of vkNone:
      discard
    of vkSingle:
      single: PNode
      tag: ValueTags
    of vkMulti:
      list: seq[PNode]
      modeAndTags: seq[tuple[mode: ArgumentMode, tags: ValueTags]]
        ## a separate sequence is used so that the whole ``PNode`` list can
        ## be moved into the destination node at once

  TranslateCl = object
    graph: ModuleGraph
    cache: IdentCache
    idgen: IdGenerator

    owner: PSym

    tempMap: SeqMap[TempId, PSym]
      ## maps a ``TempId`` to ``PSym`` created for it
    labelMap: SeqMap[uint32, PSym]
      ## maps a block-label name to the ``PSym`` created for it

    params: Values

    # While in the MIR only a ``mnkScope`` opens a new scope, in ``PNode``-AST
    # both ``nkStmtList`` and ``nkStmtListExpr`` do - the latter being used by
    # the arg-block translation. A 'def'-like can appear inside an arg-block
    # and the defined entity be used outside of it, which would thus result
    # in the definition being placed in an ``nkStmtListExpr``, producing
    # semantically invalid code that later results in code-gen errors.
    # To solve the problem, if a 'def'-like appears nested inside an arg-block,
    # only an assignment (if necessary) is produced and the symbol node is
    # added to the `def` list, which is then used to create a var section that
    # is prepended to the statement list produced for the current enclosing
    # ``mnkScope``
    inArgBlock: int ## keeps track of the current arg-block nesting
    defs: seq[PSym]

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

func toValues(x: sink PNode): Values {.inline.} =
  # note: having ``toValues`` be an implicit converter lead to an overload
  # resolution issue where the converter was incorrectly chosen, making
  # otherwise correct code not compile
  assert x != nil
  Values(kind: vkSingle, single: x)

func `[]`(v: Values, i: Natural): PNode =
  if i > 0 or v.kind == vkMulti:
    v.list[i]
  else:
    v.single

func len(v: Values): int =
  case v.kind
  of vkNone:   0
  of vkSingle: 1
  of vkMulti:  v.list.len

func add(v: var Values, n: sink PNode, tag: ValueTags, mode: ArgumentMode) =
  v.list.add n
  v.modeAndTags.add (mode, tag)

func getCalleeMagic(n: PNode): TMagic =
  if n.kind == nkSym: n.sym.magic
  else:               mNone

proc createMagic(cl: var TranslateCl, magic: TMagic): PSym =
  createMagic(cl.graph, cl.idgen, "op", magic)

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

func addIfNotEmpty(stmts: var seq[PNode], n: sink PNode) =
  ## Only adds the node to the list if it's not an empty node. Used to prevent
  ## the creation of statement-list expression that only consist of empty
  ## nodes + the result-expression (a statement-list expression is unnecessary
  ## in that case)
  if n.kind != nkEmpty:
    stmts.add n

func toSingleNode(stmts: sink seq[PNode]): PNode =
  ## Creates a single ``PNode`` from a list of *statements*
  case stmts.len
  of 0:
    result = newNode(nkEmpty)
  of 1:
    result = move stmts[0]
  else:
    result = newNode(nkStmtList)
    result.sons = move stmts

proc wrapArg(stmts: seq[PNode], info: TLineInfo, val: PNode): PNode =
  ## If there are extra statements (i.e. `stmts` is not empty), wraps the
  ## statements + result-expression into an ``nkStmtListExpr``. Otherwise,
  ## returns `val` as is
  if stmts.len == 0:
    result = val
  else:
    assert val.kind != nkStmtListExpr
    result = newTreeIT(nkStmtListExpr, info, val.typ, stmts)
    result.add val

proc makeVarSection(syms: openArray[PSym], info: TLineInfo): PNode =
  ## Creates a var section with all symbols from `syms`
  result = newNodeI(nkVarSection, info)
  result.sons.newSeq(syms.len)
  for i, s in syms.pairs:
    result[i] = newIdentDefs(newSymNode(s))

proc newTemp(cl: var TranslateCl, info: TLineInfo, typ: PType): PSym =
  ## Creates and returns a new ``skTemp`` symbol
  newSym(skTemp, cl.cache.getIdent(genPrefix),
         cl.idgen.nextSymId(), cl.owner, info, typ)

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
                info: TLineInfo): PNode =
  ## Builds the boolean expression testing if `discr` is part of the branch
  ## with position `pos`
  assert recCase.kind == nkRecCase
  let
    discr = recCase[0] ## the node holding the discriminant symbol
    branch = recCase[1 + pos]
    setType = newType(tySet, nextTypeId(cl.idgen), cl.owner)

  rawAddSon(setType, discr.typ) # the set's element type

  var
    lit = newNodeIT(nkCurly, info, setType)
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
    newTreeIT(nkCall, info, getSysType(cl.graph, info, tyBool), [
      newSymNode(getSysMagic(cl.graph, info, "contains", mInSet), info),
      lit,
      copyTree(discr)
    ])

  if invert:
    result =
      newTreeIT(nkCall, info, getSysType(cl.graph, info, tyBool), [
        newSymNode(getSysMagic(cl.graph, info, "not", mNot), info),
        inExpr
      ])
  else:
    result = inExpr

proc addToVariantAccess(cl: var TranslateCl, dest: PNode, field: PSym,
                        info: TLineInfo): PNode =
  ## Appends a field access for a field inside a record branch to `dest`
  ## (transforming it into a ``nkCheckedFieldExpr`` if it isn't one already)
  ## and returns the resulting expression
  let node =
    case dest.kind
    of nkDotExpr: dest
    of nkCheckedFieldExpr: dest[0]
    else: unreachable()

  # TODO: generating a field check (i.e. ``nkCheckedFieldExpr``) should not
  #       done by the code-generators, but instead happen at the MIR level as
  #       a MIR pass. In other words, a MIR pass should insert an 'if' +
  #       'raise' for each access to a field inside a record branch (but only
  #       if ``optFieldCheck`` is enabled) and no ``nkCheckedFieldExpr`` should
  #       be generated here

  assert node.kind == nkDotExpr

  let
    # the symbol of the discriminant is on the right-side of the dot-expr
    discr = node[1].sym
    recCase = findRecCase(node[0].typ.skipTypes(abstractInst+tyUserTypeClasses), discr)
    check = buildCheck(cl, recCase, findBranch(cl.graph.config, recCase, field.name),
                       info)

  node[1] = newSymNode(field)
  node.typ = field.typ

  case dest.kind
  of nkDotExpr:
    newTreeIT(nkCheckedFieldExpr, info, field.typ, [node, check])
  of nkCheckedFieldExpr:
    # a field is accessed that is inside a nested record-case. Don't wrap the
    # ``nkCheckedFieldExpr`` in another one -- append the check instead.
    # While the order of the checks *should* be irrelevant, we still emit them
    # in the order they were generated originally (i.e. innermost to outermost)
    dest.sons.insert(check, 1)
    # update the type of the expression:
    dest.typ = field.typ
    dest
  else:
    unreachable()

# FIXME: duplicated from mirgen
func isSimple(n: PNode): bool =
  ## Computes if the l-value expression `n` always names the same valid
  ## location
  var n = n
  while true:
    case n.kind
    of nkSym, nkLiterals:
      return true
    of nkDotExpr:
      # ``nkCheckedFieldExpr`` is deliberately not included here because it
      # means the location is part of a variant-object-branch
      n = n[0]
    of nkBracketExpr:
      if n[0].typ.skipTypes(abstractVarRange).kind in {tyTuple, tyArray} and
          n[1].kind in nkLiterals:
        # tuple access and arrays indexed by a constant value are
        # allowed -- they always name the same location
        n = n[0]
      else:
        return false
    else:
      return false

func underlyingLoc(n: PNode): tuple[underlying: PNode, firstConv: PNode] =
  ## Returns the lvalue expression stripped from any trailing lvalue
  ## conversion. For convenience, the node representing the first
  ## applied conversion is also returned. If no conversion exists, `firstConv`
  ## is equal to `underlying`
  var
    n = n
    orig = n

  while n.kind in {nkObjDownConv, nkObjUpConv}:
    orig = n
    n = n.lastSon

  result = (n, orig)

proc useLvalueRef(n: PNode, mutable: bool, cl: var TranslateCl,
                  stmts: var seq[PNode]): PNode =
  ## Generates a temporary view into the location named by the lvalue
  ## expression `n` and returns the deref expression for accessing the
  ## location
  let
    (locExpr, conv) = underlyingLoc(n)
    typ = makeVarType(cl.owner, locExpr.typ, cl.idgen,
                      (if mutable: tyVar else: tyLent))

    sym = newTemp(cl, n.info, typ)

  # for the "undo conversion" logic to work, the expression needs to end in a
  # conversion. Creating a view from the location *after* lvalue conversion
  # would break this, so instead, a view is created from the unconverted
  # location and the conversion is applied at each usage site
  stmts.add newTreeI(nkVarSection, n.info,
                     [newIdentDefs(newSymNode(sym),
                                   newTreeIT(nkHiddenAddr, n.info, typ, [locExpr]))
                     ])

  if locExpr != conv:
    # a conversion exists. Rewrite the conversion operation to apply to the
    # dereferenced view
    conv[0] = newTreeIT(nkHiddenDeref, n.info, locExpr.typ, [newSymNode(sym)])
    result = n
  else:
    result = newTreeIT(nkHiddenDeref, n.info, n.typ, [newSymNode(sym)])

proc useTemporary(n: PNode, cl: var TranslateCl, stmts: var seq[PNode]): PNode =
  let sym = newTemp(cl, n.info, n.typ)

  stmts.add newTreeI(nkVarSection, n.info, [newIdentDefs(newSymNode(sym), n)])
  result = newSymNode(sym)

proc prepareParameter(expr: PNode, tag: ValueTags, mode: ArgumentMode,
                      cl: var TranslateCl, stmts: var seq[PNode]): PNode =
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

proc prepareParameters(params: var Values, stmts: var seq[PNode],
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

proc wrapInHiddenAddr(cl: TranslateCl, n: PNode): PNode =
  ## Restores the ``nkHiddenAddr`` around lvalue expressions passed to ``var``
  ## parameters. The code-generators operating on ``PNode``-AST depend on the
  ## hidden addr to be present
  let inner =
    if n.kind == nkStmtListExpr: n.lastSon else: n

  result =
    if n.typ.skipTypes(abstractInst).kind != tyVar:
      newTreeIT(nkHiddenAddr, n.info, makeVarType(cl.owner, n.typ, cl.idgen), n)
    elif inner.kind == nkObjDownConv and
         inner[0].typ.kind != tyVar:
      # TODO: ``nkHiddenSubConv`` nodes for objects (which are later
      #       transformed into ``nkObjDownConv`` nodes) are in some cases
      #       incorrectly typed as ``var`` somewhere in the compiler
      #       (presumably during sem). Fix the underlying problem and remove
      #       the special case here
      newTreeIT(nkHiddenAddr, n.info, n.typ, n)
    else:
      n

proc genObjConv(n: PNode, a, b, t: PType): PNode =
  ## Depending on the relationship between `a` and `b`, wraps `n` in either an
  ## up- or down-conversion. `t` is the type to use for the resulting
  ## expression
  let diff = inheritanceDiff(b, a)
  #echo "a: ", a.sym.name.s, "; b: ", b.sym.name.s
  #assert diff != 0 and diff != high(int), "redundant or illegal conversion"
  if diff == 0:
    return nil
  result = newTreeIT(
    if diff < 0: nkObjUpConv else: nkObjDownConv,
    n.info, t): n

# forward declarations:
proc tbSeq(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): Values

proc tbStmt(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): PNode {.inline.}
proc tbList(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): PNode

proc tbScope(tree: TreeWithSource, cl: var TranslateCl, n: MirNode, cr: var TreeCursor): PNode

proc tbRegion(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
              cr: var TreeCursor): PNode

proc handleSpecialConv(c: ConfigRef, n: PNode, info: TLineInfo,
                       dest: PType): PNode =
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
      if n.kind in {nkObjUpConv, nkObjDownConv} and
         sameType(dest, n[0].typ.skipTypes(abstractInst)):
        # this one and the previous conversion cancel each other out. Both
        # ``nkObjUpConv`` and ``nkObjDownConv`` are not treated as lvalue
        # conversions when the source/dest operands are pointer/reference-like,
        # so the collapsing here is required in order to generate correct
        # code
        result = n[0]
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
          if tyInt64 in {dest.kind, source.kind}: nkChckRange64
          else:                                   nkChckRange

      result = newTreeIT(kind, info, orig):
        [n,
         newIntTypeNode(firstOrd(c, rangeDest), rangeDest),
         newIntTypeNode(lastOrd(c, rangeDest), rangeDest)]
  of tyFloat..tyFloat128:
    let rangeDest = skipTypes(orig, abstractVar)
    if rangeDest.kind == tyRange:
      # a conversion to a float range (e.g. ``range[0.0 .. 1.0]``)
      result = newTreeIT(nkChckRangeF, info, orig):
        [n, copyTree(rangeDest.n[0]), copyTree(rangeDest.n[1])]

  else:
    result = nil

proc tbConv(cl: TranslateCl, n: PNode, info: TLineInfo, dest: PType): PNode =
  ## Generates the AST for an expression that performs a type conversion for
  ## `n` to type `dest`
  result = handleSpecialConv(cl.graph.config, n, info, dest)
  if result == nil:
    # no special conversion is used
    result = newTreeIT(nkConv, info, dest): [newNodeIT(nkType, info, dest), n]

proc tbSingle(n: MirNode, cl: TranslateCl, info: TLineInfo): PNode =
  case n.kind
  of mnkProc, mnkConst, mnkParam, mnkGlobal, mnkLocal:
    newSymNodeIT(n.sym, info, n.sym.typ)
  of mnkTemp:
    newSymNode(cl.tempMap[n.temp], info)
  of mnkLiteral:
    n.lit
  of mnkType:
    newNodeIT(nkType, info, n.typ)
  else:
    unreachable("not an atom: " & $n.kind)

proc tbExceptItem(tree: TreeWithSource, cl: TranslateCl, cr: var TreeCursor
                 ): PNode =
  let n {.cursor.} = get(tree, cr)
  case n.kind
  of mnkPNode: n.node
  of mnkType:  newNodeIT(nkType, cr.info, n.typ)
  else:        unreachable()


proc tbDef(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
           n: MirNode, cr: var TreeCursor): PNode =
  ## Translates a 'def'-like construct
  assert n.kind in DefNodes
  let
    entity {.cursor.} = get(tree, cr) # the name of the defined entity
    info = cr.info

  var def: PNode

  case entity.kind
  of SymbolLike:
    def = tbSingle(entity, cl, info)
    case def.sym.kind
    of skVar, skLet, skForVar:
      discard "pass through"
    of skParam:
      # has no ``PNode`` counterpart
      def = newNode(nkEmpty)
    of routineKinds:
      # the original procdef is stored as the second sub-node
      def = get(tree, cr).node
    else:
      unreachable()

  of mnkTemp:
    # for temporaries, we create an ``skTemp`` symbol and associate it with
    # the ``TempId`` so that it can be looked up later
    assert entity.typ != nil
    let sym = newTemp(cl, info, entity.typ)

    assert entity.temp notin cl.tempMap, "re-definition of temporary"
    cl.tempMap[entity.temp] = sym

    def = newSymNode(sym, info)
  else:
    unreachable()

  leave(tree, cr)

  if def.kind == nkSym:
    assert def.sym.kind in {skVar, skLet, skForVar, skTemp}
    # it's a definition that needs to be put into a var section
    if cl.inArgBlock > 0:
      # if we're inside an arg-block, the var section is generated later and
      # placed at an earlier position. We just produce an assignment to the
      # entity here (if the def has an input)
      cl.defs.add def.sym

      result =
        case prev.kind
        of vkNone:   newNodeI(nkEmpty, info)
        of vkSingle: newTreeI(nkAsgn, info, def, prev.single)
        of vkMulti:  unreachable()

    else:
      result = newTreeI(nkVarSection, info):
        case prev.kind
        of vkNone:   newIdentDefs(def)
        of vkSingle: newIdentDefs(def, prev.single)
        of vkMulti:  unreachable()

  else:
    result = def

proc tbSingleStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
                  cr: var TreeCursor): PNode =
  template body(): PNode =
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
    # translated into ``while true: body``
    result =
      newTreeI(nkWhileStmt, info,
               newIntTypeNode(1, cl.graph.getSysType(info, tyBool)), # condition
               body()) # body

    leave(tree, cr)
  of mnkBlock:
    let sym = newSym(skLabel, cl.cache.getIdent("label"), cl.idgen.nextSymId(),
                     cl.owner, info)
    cl.labelMap[n.label[]] = sym

    result = newTreeI(nkBlockStmt, info,
                      newSymNode(sym), # the label
                      body())
    leave(tree, cr)
  of mnkTry:
    result = newTreeIT(nkTryStmt, info, n.typ, [body()])
    assert n.len <= 2

    for _ in 0..<n.len:
      let it {.cursor.} = enter(tree, cr)

      case it.kind
      of mnkExcept:
        for _ in 0..<it.len:
          let br {.cursor.} = enter(tree, cr)
          assert br.kind == mnkBranch

          let excpt = newTreeI(nkExceptBranch, cr.info)
          for j in 0..<br.len:
            excpt.add tbExceptItem(tree, cl, cr)

          excpt.add body()
          result.add excpt

          leave(tree, cr)

      of mnkFinally:
        result.add newTreeI(nkFinally, cr.info, body())
      else:
        unreachable(it.kind)

      leave(tree, cr)

    leave(tree, cr)
  of mnkBreak:
    let label =
      if n.label.isSome: newSymNode(cl.labelMap[n.label[]])
      else:              newNode(nkEmpty)

    result = newTreeI(nkBreakStmt, info, [label])
  of mnkReturn:
    result = newTreeI(nkReturnStmt, info, [newNode(nkEmpty)])
  of mnkPNode:
    result = n.node
  of AllNodeKinds - StmtNodes:
    unreachable(n.kind)

proc tbStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
            cr: var TreeCursor): PNode =
  case n.kind
  of mnkStmtList:
    result = tbList(tree, cl, cr)
    leave(tree, cr)
  else:
    result = tbSingleStmt(tree, cl, n, cr)

proc tbSingleStmt(tree: TreeWithSource, cl: var TranslateCl,
                  cr: var TreeCursor): PNode {.inline.} =
  tbSingleStmt(tree, cl, get(tree, cr), cr)

proc tbStmt(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor
           ): PNode {.inline.} =
  tbStmt(tree, cl, get(tree, cr), cr)

proc tbCaseStmt(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
                prev: sink Values, cr: var TreeCursor): PNode =
  result = newTreeI(nkCaseStmt, cr.info, [prev.single])
  for j in 0..<n.len:
    let br {.cursor.} = enter(tree, cr)

    if br.len == 0:
      result.add newTreeI(nkElse, cr.info)
    else:
      result.add newTreeI(nkOfBranch, cr.info)
      for x in 0..<br.len:
        result[^1].add get(tree, cr).lit

    result[^1].add tbStmt(tree, cl, cr)
    leave(tree, cr)

  leave(tree, cr)

proc tbOut(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
           cr: var TreeCursor): PNode =
  let n {.cursor.} = get(tree, cr)
  case n.kind
  of DefNodes:
    tbDef(tree, cl, prev, n, cr)
  of mnkRegion:
    tbRegion(tree, cl, prev, cr)
  of mnkFastAsgn:
    assert prev.list.len == 2
    newTreeI(nkFastAsgn, cr.info, [prev[0], prev[1]])
  of mnkInit, mnkAsgn:
    assert prev.list.len == 2
    newTreeI(nkAsgn, cr.info, [prev[0], prev[1]])
  of mnkSwitch:
    assert prev.list.len == 2
    newTreeI(nkFastAsgn, cr.info, [prev[0], prev[1]])
    # TODO: use a MIR pass to lower 'switch' operations into assignments and
    #       make reaching here an error
    #unreachable("unlowered switch")
  of mnkIf:
    assert prev.kind == vkSingle
    let n = newTreeI(nkIfStmt, cr.info):
      [newTreeI(nkElifBranch, cr.info, [prev.single, tbStmt(tree, cl, cr)])]
    leave(tree, cr)

    n
  of mnkVoid:
    # it's a void sink
    assert prev.kind == vkSingle
    if prev.single.typ.isEmptyType():
      # a void call doesn't need to be discarded
      prev.single
    else:
      newTreeI(nkDiscardStmt, cr.info, [prev.single])

  of mnkRaise:
    newTreeI(nkRaiseStmt, cr.info, [prev.single])
  of mnkCase:
    tbCaseStmt(tree, cl, n, prev, cr)
  of AllNodeKinds - OutputNodes:
    unreachable(n.kind)


proc tbArgBlock(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor
               ): Values =
  var stmts: seq[PNode]
  result = Values(kind: vkMulti)

  inc cl.inArgBlock

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
  dec cl.inArgBlock

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
    toValues newNodeIT(nkEmpty, cr.info, n.typ)
  of mnkArgBlock:
    tbArgBlock(tree, cl, cr)
  of AllNodeKinds - InputNodes:
    unreachable(n.kind)

proc tbArgs(v: var Values, m: TMagic, cl: TranslateCl) =
  ## The operands to some magics (those in the ``FakeVarParams`` set) must
  ## not be wrapped in ``nkHiddenAddr`` nodes.
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

    var node = newTreeIT(nkCall, info, n.typ)
    node.sons.newSeq(1 + prev.len)
    # the ``PNode`` AST requires a symbol for magics, so we have to create one
    node.sons[0] = newSymNode(createMagic(cl, n.magic))

    case prev.kind
    of vkNone: discard
    of vkSingle: node.sons[1] = move prev.single
    of vkMulti:
      for i, v in prev.list.mpairs:
        node.sons[1 + i] = move v

    toValues node
  of mnkCall:
    assert n.typ != nil
    var node = newNodeIT(nkCall, info, n.typ)
    case prev.kind
    of vkMulti:
      # pre-process the argument expressions:
      tbArgs(prev, getCalleeMagic(prev.list[0]), cl)

      node.sons = move prev.list
    of vkSingle:
      # the procedure is called with no arguments
      node.sons = @[prev.single]
    of vkNone:
      unreachable()

    toValues node
  of mnkCast:
    toValues newTreeIT(nkCast, info, n.typ, newNodeIT(nkType, info, n.typ), prev.single)
  of mnkConv:
    toValues tbConv(cl, prev.single, info, n.typ)
  of mnkStdConv:
    let
      opr = prev.single
      source = opr.typ.skipTypes(abstractVarRange)
      dest = n.typ.skipTypes(abstractVarRange)

    var adjusted = PNode(nil)

    case dest.kind
    of tyCstring:
      if source.kind == tyString:
        adjusted = newTreeIT(nkStringToCString, info, n.typ): opr

    of tyString:
      if source.kind == tyCstring:
        adjusted = newTreeIT(nkCStringToString, info, n.typ): opr

    of tyOpenArray, tyVarargs:
      # the old code-generators depend on conversions to ``openArray`` to be
      # omitted
      adjusted = opr
    else:
      discard

    if adjusted == nil:
      # no special conversion is used
      adjusted = newTreeIT(nkHiddenStdConv, info, n.typ,
                           [newNodeIT(nkType, info, n.typ), opr])

    toValues adjusted
  of mnkPathVariant:
    var node: PNode
    if vtVariant in prev.tag:
      node = addToVariantAccess(cl, prev.single, n.field, info)
    else:
      # the node's ``typ`` is the type of the enclosing object not of the
      # discriminant, so we have to explicitly use the field's type here
      node = newTreeIT(nkDotExpr, info, n.field.typ, [prev.single, newSymNode(n.field)])

    # mark the value as being a variant object. Depending on which context the
    # resulting value is used, it's either kept as is, turned  into a
    # ``nkCheckedFieldExpr``, or, if it already is one, appended to
    Values(kind: vkSingle, single: node, tag: {vtVariant})
  of mnkPathNamed:
    if vtVariant in prev.tag:
      toValues addToVariantAccess(cl, prev.single, n.field, info)
    else:
      toValues newTreeIT(nkDotExpr, info, n.typ, [prev.single, newSymNode(n.field)])

  of mnkPathPos:
    # try to use a dot access where possible
    # TODO: this is done so that ``sizeof(tup.f)`` works for the C back-end
    #       where ``tup`` is an imported tuple type, because ``cgen``'s
    #       implementation of the ``mSizeOf`` magic doesn't support
    #       bracket-access. Make the implementation support bracket-access and
    #       then only emit ``nkBracketExpr`` here
    let t = prev.single.typ.skipTypes(abstractInst + tyUserTypeClasses)
    if t.n != nil:
      # it's a named tuple
      toValues newTreeIT(nkDotExpr, info, n.typ,
        [prev.single, t.n.sons[n.position]])
    else:
      # a tuple with unnamed fields
      toValues newTreeIT(nkBracketExpr, info, n.typ,
        [prev.single, newIntNode(nkIntLit, n.position.BiggestInt)])

  of mnkPathArray:
    let node = newNodeIT(nkBracketExpr, info, n.typ)
    node.sons = move prev.list
    toValues node
  of mnkAddr:
    toValues newTreeIT(nkAddr, info, n.typ, [prev.single])
  of mnkDeref:
    toValues newTreeIT(nkDerefExpr, info, n.typ, [prev.single])
  of mnkView:
    toValues newTreeIT(nkHiddenAddr, info, n.typ, [prev.single])
  of mnkDerefView:
    toValues newTreeIT(nkHiddenDeref, info, n.typ, [prev.single])
  of mnkObjConstr:
    assert n.typ.skipTypes(abstractVarRange).kind in {tyObject, tyRef}
    var node = newTreeIT(nkObjConstr, info, n.typ, newNodeIT(nkType, info, n.typ))
    for j in 0..<n.len:
      let f {.cursor.} = get(tree, cr)
      node.add newTreeI(nkExprColonExpr, cr.info, [newSymNode(f.field), prev[j]])

    leave(tree, cr)
    toValues node
  of mnkConstr:
    let typ = n.typ.skipTypes(abstractVarRange)

    let kind =
      case typ.kind
      of tySet:               nkCurly
      of tyArray, tySequence: nkBracket
      of tyTuple:             nkTupleConstr
      of tyProc:
        assert typ.callConv == ccClosure
        nkClosure
      else:
        unreachable(typ.kind)

    var node = newNodeIT(kind, info, n.typ)
    node.sons = move prev.list

    toValues node
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


proc tbList(tree: TreeWithSource, cl: var TranslateCl, stmts: var seq[PNode],
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

proc tbList(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): PNode =
  ## Translates a 'stmt-list' MIR structure into AST
  var stmts: seq[PNode]
  tbList(tree, cl, stmts, cr)
  result = toSingleNode(stmts)

proc tbScope(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
             cr: var TreeCursor): PNode =
  let
    prev = cl.defs.len
    info = cr.info

  var stmts: seq[PNode]
  tbList(tree, cl, stmts, cr)

  if cl.defs.len > prev:
    # create a var section for the collected symbols:
    stmts.insert(
      makeVarSection(cl.defs.toOpenArray(prev, cl.defs.high), info), 0)

    # "pop" the elements that were added as part of this scope:
    cl.defs.setLen(prev)

  result = toSingleNode(stmts)

proc tbRegion(tree: TreeWithSource, cl: var TranslateCl, prev: sink Values,
              cr: var TreeCursor): PNode =
  var stmts: seq[PNode]
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
           ): tuple[node: PNode, atEnd: bool] =
  ## Translates the expression located at the current cursor position `cr` to
  ## ``PNode`` AST
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

proc tbMulti(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): PNode =
  ## Translates expressions/statements until the cursor either reaches the end
  ## or a top-level argument node is encountered
  var nodes: seq[PNode]
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
    nodes.insert(makeVarSection(cl.defs, unknownLineInfo), 0)

  case nodes.len
  of 0: newNode(nkEmpty)
  of 1: nodes[0]
  else:
    let r =
      if nodes[^1].typ.isEmptyType():
        # it's a statement list
        newNode(nkStmtList)
      else:
        newNodeIT(nkStmtListExpr, unknownLineInfo, nodes[^1].typ)

    r.sons = move nodes
    r

proc tb(tree: TreeWithSource, cl: var TranslateCl, start: NodePosition): PNode =
  ## Translate `tree` back to a ``PNode`` AST
  var cr = TreeCursor(pos: start.uint32)
  assert tree[cr].kind in InputNodes + StmtNodes,
         "start must point to the start of expression or statement"
  tbMulti(tree, cl, cr)


proc generateAST*(graph: ModuleGraph, idgen: IdGenerator, owner: PSym,
                  tree: sink MirTree, sourceMap: sink SourceMap): PNode =
  ## Generates a ``PNode`` AST that is semantically equivalent to `tree`,
  ## using the `idgen` to provide new IDs when creating symbols. `sourceMap`
  ## must be the ``SourceMap`` corresponding to `tree` and is used as the
  ## provider for source position information
  # TODO: `tree` and `sourceMap` are only consumed because of efficiency
  #       reasons (to get around a full copy for both). Remove ``sink`` for
  #       both once the fields of ``TreeWithSource`` are views
  var cl = TranslateCl(graph: graph, idgen: idgen, cache: graph.cache,
                       owner: owner)
  tb(TreeWithSource(tree: tree, map: sourceMap), cl, NodePosition 0)