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

func getCalleeMagic(n: CgNode): TMagic =
  case n.kind
  of cnkSym:   n.sym.magic
  of cnkMagic: n.magic
  else:        mNone

func newMagicNode(magic: TMagic, info: TLineInfo): CgNode =
  CgNode(kind: cnkMagic, info: info, magic: magic)

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
proc stmtToIr(tree: TreeWithSource, cl: var TranslateCl,
              cr: var TreeCursor): CgNode
proc scopeToIr(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor,
               allowExpr=false): seq[CgNode]

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
    genObjConv(n, source, dest, orig)
  of tyRef, tyPtr, tyVar, tyLent:
    assert source.kind == dest.kind
    if source.base.kind == tyObject:
      genObjConv(n, source.base, dest.base, orig)
    else:
      nil
  else:
    nil

proc convToIr(cl: TranslateCl, n: CgNode, info: TLineInfo, dest: PType): CgNode =
  ## Generates the ``CgNode`` IR for an ``mnkPathConv`` operation (handle
  ## conversion).
  result = handleSpecialConv(cl.graph.config, n, info, dest)
  if result == nil:
    # no special conversion is used
    result = newOp(cnkConv, info, dest, n)

proc atomToIr(n: MirNode, cl: TranslateCl, info: TLineInfo): CgNode =
  case n.kind
  of mnkProc, mnkConst, mnkGlobal:
    newSymNode(n.sym, info)
  of mnkLocal, mnkParam:
    # paramaters are treated like locals in the code generators
    assert n.sym.id in cl.localsMap
    newLocalRef(cl.localsMap[n.sym.id], info, n.sym.typ)
  of mnkTemp:
    newLocalRef(cl.tempMap[n.temp], info, n.typ)
  of mnkAlias:
    # the type of the node doesn't match the real one
    let id = cl.tempMap[n.temp]
    let typ = cl.locals[id].typ
    # the view is auto-dereferenced here for convenience
    newOp(cnkDerefView, info, typ.base, newLocalRef(id, info, typ))
  of mnkLiteral:
    translateLit(n.lit)
  of mnkType:
    newTypeNode(info, n.typ)
  of mnkNone:
    # type arguments do use `mnkNone` in some situtations, so keep
    # the type
    CgNode(kind: cnkEmpty, info: info, typ: n.typ)
  else:
    unreachable("not an atom: " & $n.kind)

proc atomToIr(tree: TreeWithSource, cl: var TranslateCl,
              cr: var TreeCursor): CgNode {.inline.} =
  atomToIr(get(tree, cr), cl, cr.info)

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


proc lvalueToIr(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
                cr: var TreeCursor): CgNode =
  ## Translates a MIR lvalue expression to the corresponding CG IR.
  let info = cr.info

  template recurse(): CgNode =
    lvalueToIr(tree, cl, tree.get(cr), cr)

  case n.kind
  of SymbolLike, mnkTemp, mnkAlias:
    return atomToIr(n, cl, info)
  of mnkPathVariant:
    let arg = recurse()
    if arg.kind == cnkCheckedFieldAccess:
      result = addToVariantAccess(cl, arg, n.field, info)
    else:
      # the node's ``typ`` is the type of the enclosing object not of the
      # discriminant, so we have to explicitly use the field's type here
      result = newExpr(cnkFieldAccess, info, n.field.typ,
                       [arg, newSymNode(n.field)])
  of mnkPathNamed:
    let arg = recurse()
    if arg.kind == cnkCheckedFieldAccess:
      result = addToVariantAccess(cl, arg, n.field, info)
    else:
      result = newExpr(cnkFieldAccess, info, n.typ, [arg, newSymNode(n.field)])
  of mnkPathPos:
    result = newExpr(cnkTupleAccess, info, n.typ,
                     [recurse(),
                      CgNode(kind: cnkIntLit, intVal: n.position.BiggestInt)])
  of mnkPathArray:
    # special case in order to support string literal access
    # XXX: this needs to be removed once there is a dedicated run-time-
    #      sequence access operator
    let arg =
      if tree[cr].kind == mnkLiteral:
        atomToIr(tree, cl, cr)
      else:
        recurse()

    result = newExpr(cnkArrayAccess, info, n.typ, [arg, atomToIr(tree, cl, cr)])
  of mnkPathConv:
    result = convToIr(cl, recurse(), info, n.typ)
  # dereferences are allowed at the end of a path tree
  of mnkDeref:
    result = newOp(cnkDeref, info, n.typ, atomToIr(tree, cl, cr))
  of mnkDerefView:
    result = newOp(cnkDerefView, info, n.typ, atomToIr(tree, cl, cr))
  else:
    unreachable(n.kind)

  leave(tree, cr)

proc lvalueToIr(tree: TreeWithSource, cl: var TranslateCl,
                cr: var TreeCursor): CgNode {.inline.} =
  lvalueToIr(tree, cl, tree.get(cr), cr)

proc valueToIr(tree: TreeWithSource, cl: var TranslateCl,
               cr: var TreeCursor): CgNode =
  let n {.cursor.} = tree.get(cr)
  case n.kind
  of SymbolLike, mnkTemp, mnkAlias, mnkLiteral:
    atomToIr(n, cl, cr.info)
  of mnkPathPos, mnkPathNamed, mnkPathArray, mnkPathConv, mnkPathVariant,
     mnkDeref, mnkDerefView:
    lvalueToIr(tree, cl, n, cr)
  else:
    unreachable("not a value: " & $n.kind)

proc argToIr(tree: TreeWithSource, cl: var TranslateCl,
             cr: var TreeCursor): (bool, CgNode) =
  ## Translates a MIR argument tree to the corresponding CG IR tree.
  ## Returns both the tree and whether the argumnet was wrapped in a tag
  ## operator (which indicates that the parameter is a ``var`` parameter).
  var n {.cursor.} = tree.get(cr)
  assert n.kind in ArgumentNodes, "argument node expected: " & $n.kind
  # the inner node may be a tag node
  n = tree.get(cr)
  if n.kind == mnkTag:
    # it is one
    result = (true, atomToIr(tree, cl, cr))
    leave(tree, cr)
  else:
    # it is not, only an atom node follows
    result = (false, atomToIr(n, cl, cr.info))

  leave(tree, cr)

proc callToIr(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode =
  ## Translate a valid call-like tree to the CG IR.
  let info = cr.info
  case n.kind
  of mnkMagic:
    assert n.typ != nil
    result = newExpr(cnkCall, info, n.typ)
    result.add newMagicNode(n.magic, info)
  of mnkCall:
    result = newExpr(cnkCall, info, n.typ)
    result.add atomToIr(tree, cl, cr) # the callee
  else:
    unreachable(n.kind)

  # the code generators currently require some magics to not have any
  # arguments wrapped in ``cnkHiddenAddr`` nodes
  let noAddr = getCalleeMagic(result[0]) in FakeVarParams

  # translate the arguments:
  while tree[cr].kind != mnkEnd:
    var (mutable, arg) = argToIr(tree, cl, cr)
    if noAddr:
      if arg.typ.kind == tyVar:
        # auto-dereference the view
        # XXX: prevent this case from happening
        arg = newOp(cnkDerefView, arg.info, arg.typ.base, arg)
    elif mutable:
      arg = wrapInHiddenAddr(cl, arg)

    result.add arg

  leave(tree, cr)

proc exprToIr(tree: TreeWithSource, cl: var TranslateCl, cr: var TreeCursor): CgNode

proc defToIr(tree: TreeWithSource, cl: var TranslateCl,
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
  of mnkAlias:
    # MIR aliases are translated to var/lent views
    assert n.kind in {mnkBind, mnkBindMut}, "alias can only be defined by binds"
    assert entity.typ != nil
    let
      typ = makeVarType(cl.owner, entity.typ, cl.idgen,
                        if n.kind == mnkBind: tyLent else: tyVar)
      tmp = cl.locals.add Local(typ: typ)

    assert entity.temp notin cl.tempMap, "re-definition of temporary"
    cl.tempMap[entity.temp] = tmp

    def = newLocalRef(tmp, info, typ)
  else:
    unreachable()

  var arg = exprToIr(tree, cl, cr)
  leave(tree, cr)
  if n.kind in {mnkBind, mnkBindMut} and arg.typ.kind notin {tyVar, tyLent}:
    # wrap the operand in an address-of operation
    arg = newOp(cnkHiddenAddr, info, def.typ, arg)

  case def.kind
  of cnkLocal:
    if cl.inUnscoped:
      # add the local to the list of moved definitions and only emit
      # an assignment
      cl.defs.add copyTree(def)
      result =
        case arg.kind
        of cnkEmpty: arg
        else:        newStmt(cnkAsgn, info, [def, arg])
    else:
      result = newStmt(cnkDef, info, [def, arg])
  of cnkSym:
    # there are no defs for globals in the ``CgNode`` IR, so we
    # emit an assignment that has the equivalent behaviour (in
    # terms of initialization)
    case arg.kind
    of cnkEmpty:
      if sfImportc in def.sym.flags:
        # for imported globals, the 'def' only means that the symbol becomes
        # known to us, not that it starts its lifetime here -> don't
        # initialize or move it
        result = arg
      elif cl.inUnscoped:
        # move the default initialization to the start of the scope
        cl.defs.add def
        result = arg
      else:
        result = newStmt(cnkAsgn, info, [def, newDefaultCall(info, def.typ)])
    else:
      if sfImportc notin def.sym.flags and cl.inUnscoped:
        # default intialization is required at the start of the scope
        cl.defs.add def
      result = newStmt(cnkAsgn, info, [def, arg])
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

proc bodyToIr(tree: TreeWithSource, cl: var TranslateCl,
              cr: var TreeCursor): CgNode =
  ## Generates the ``CgNode`` tree for the body of a construct that implies
  ## some form of control-flow.
  let prev = cl.inUnscoped
  # assume the body is unscoped until stated otherwise
  cl.inUnscoped = true
  result = stmtToIr(tree, cl, cr)
  cl.inUnscoped = prev

proc caseToIr(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode

proc stmtToIr(tree: TreeWithSource, cl: var TranslateCl,
              cr: var TreeCursor): CgNode =
  let n {.cursor.} = tree.get(cr)
  let info = cr.info ## the source information of `n`

  template body(): CgNode =
    bodyToIr(tree, cl, cr)

  # TODO: reduce the amount of boilerplate a bit

  case n.kind
  of DefNodes:
    # a definition of an entity with no initial value
    result = defToIr(tree, cl, n, cr)
  of mnkAsgn, mnkInit, mnkSwitch:
    result = newStmt(cnkAsgn, info, [lvalueToIr(tree, cl, cr), exprToIr(tree, cl, cr)])
    leave(tree, cr)
  of mnkFastAsgn:
    result = newStmt(cnkFastAsgn, info, [lvalueToIr(tree, cl, cr), exprToIr(tree, cl, cr)])
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
  of mnkVoid:
    result = exprToIr(tree, cl, cr)
    if result.typ.isEmptyType():
      # a void call doesn't need to be discarded
      discard
    else:
      result = newStmt(cnkVoidStmt, cr.info, [result])
    leave(tree, cr)
  of mnkIf:
    result = newStmt(cnkIfStmt, cr.info, [valueToIr(tree, cl, cr), body()])
    leave(tree, cr)
  of mnkRaise:
    # the operand can either be empty or an lvalue expression
    let arg {.cursor.} = tree.get(cr)
    result = newStmt(cnkRaiseStmt, cr.info):
      case arg.kind
      of mnkNone: atomToIr(arg, cl, cr.info)
      else:       lvalueToIr(tree, cl, arg, cr)
    leave(tree, cr)
  of mnkCase:
    result = caseToIr(tree, cl, n, cr)
  of mnkAsm:
    let n = newStmt(cnkAsmStmt, cr.info)
    while tree[cr].kind != mnkEnd:
      n.add atomToIr(tree, cl, cr)
    leave(tree, cr)
    result = n
  of mnkEmit:
    let n = newStmt(cnkEmitStmt, cr.info)
    while tree[cr].kind != mnkEnd:
      n.add atomToIr(tree, cl, cr)
    leave(tree, cr)
    result = n
  of mnkCall, mnkMagic:
    result = callToIr(tree, cl, n, cr)
  of mnkStmtList:
    var list = newStmt(cnkStmtList, cr.info)
    while tree[cr].kind != mnkEnd:
      list.kids.addIfNotEmpty stmtToIr(tree, cl, cr)
    leave(tree, cr)
    result = list
  of mnkScope:
    result = toSingleNode scopeToIr(tree, cl, cr)
  else:
    unreachable(n.kind)

proc caseToIr(tree: TreeWithSource, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode =
  assert n.kind == mnkCase
  result = newStmt(cnkCaseStmt, cr.info, [valueToIr(tree, cl, cr)])
  for j in 0..<n.len:
    let br {.cursor.} = enter(tree, cr)

    result.add newTree(cnkBranch, cr.info)
    if br.len > 0:
      for x in 0..<br.len:
        result[^1].add translateLit(get(tree, cr).lit)

    result[^1].add bodyToIr(tree, cl, cr)
    leave(tree, cr)

  leave(tree, cr)

proc exprToIr(tree: TreeWithSource, cl: var TranslateCl,
              cr: var TreeCursor): CgNode =
  ## Translates a MIR expression to the corresponding CG IR representation.
  ## Moves the cursor to the next tree item.
  let n {.cursor.} = get(tree, cr)
  let info = cr.info
  case n.kind
  of Atoms:
    atomToIr(n, cl, cr.info)
  of mnkCall, mnkMagic:
    callToIr(tree, cl, n, cr)
  of mnkCast:
    let res = newOp(cnkCast, info, n.typ, valueToIr(tree, cl, cr))
    leave(tree, cr)
    res
  of mnkConv:
    let res = newOp(cnkConv, info, n.typ, valueToIr(tree, cl, cr))
    leave(tree, cr)
    res
  of mnkStdConv:
    let
      opr = valueToIr(tree, cl, cr)
      source = opr.typ.skipTypes(abstractVarRange)
      dest = n.typ.skipTypes(abstractVarRange)

    leave(tree, cr)

    var adjusted: CgNode

    case dest.kind
    of tyCstring:
      if source.kind == tyString:
        adjusted = newOp(cnkStringToCString, info, n.typ): opr

    of tyString:
      if source.kind == tyCstring:
        adjusted = newOp(cnkCStringToString, info, n.typ): opr

    else:
      discard

    if adjusted == nil:
      # no special conversion is used
      adjusted = newOp(cnkHiddenConv, info, n.typ, opr)

    adjusted
  of mnkToSlice:
    # the old code-generators depend on conversions to ``openArray`` to be
    # omitted
    let arg = valueToIr(tree, cl, cr)
    leave(tree, cr)
    arg
  of mnkPathVariant, mnkPathArray, mnkPathConv, mnkPathNamed, mnkPathPos:
    lvalueToIr(tree, cl, n, cr)
  of mnkAddr:
    let res = newOp(cnkAddr, info, n.typ, lvalueToIr(tree, cl, cr))
    leave(tree, cr)
    res
  of mnkDeref:
    let res = newOp(cnkDeref, info, n.typ, atomToIr(tree, cl, cr))
    leave(tree, cr)
    res
  of mnkView:
    let res = newOp(cnkHiddenAddr, info, n.typ, lvalueToIr(tree, cl, cr))
    leave(tree, cr)
    res
  of mnkDerefView:
    let res = newOp(cnkDerefView, info, n.typ, atomToIr(tree, cl, cr))
    leave(tree, cr)
    res
  of mnkObjConstr:
    assert n.typ.skipTypes(abstractVarRange).kind in {tyObject, tyRef}
    var node = newExpr(cnkObjConstr, info, n.typ)
    for j in 0..<n.len:
      let f {.cursor.} = get(tree, cr)
      node.add newTree(cnkBinding, cr.info,
                       [newSymNode(f.field), argToIr(tree, cl, cr)[1]])

    leave(tree, cr)
    node
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

    let res = newExpr(kind, info, n.typ)
    while tree[cr].kind != mnkEnd:
      res.add argToIr(tree, cl, cr)[1]
    leave(tree, cr)
    res
  else:
    unreachable(n.kind)

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

proc scopeToIr(tree: TreeWithSource, cl: var TranslateCl,
               cr: var TreeCursor, allowExpr = false): seq[CgNode] =
  let
    ends =
      if allowExpr: {mnkEnd} + Atoms
      else:         {mnkEnd}
    prev = cl.defs.len
    prevInUnscoped = cl.inUnscoped

  # a scope is entered, meaning that we're no longer in an unscoped context
  cl.inUnscoped = false

  var stmts: seq[CgNode]
  # translate all statements:
  while cr.hasNext(tree) and tree[cr].kind notin ends:
    stmts.addIfNotEmpty stmtToIr(tree, cl, cr)

  if cr.hasNext(tree) and tree[cr].kind == mnkEnd:
    leave(tree, cr) # close the sub-tree

  if cl.defs.len > prev:
    # insert all the lifted defs at the start
    for i in countdown(cl.defs.high, prev):
      stmts.insert genDefFor(move cl.defs[i])

    # "pop" the elements that were added as part of this scope:
    cl.defs.setLen(prev)

  cl.inUnscoped = prevInUnscoped

  result = stmts

proc tb(tree: TreeWithSource, cl: var TranslateCl, start: NodePosition): CgNode =
  ## Translate `tree` back to a ``CgNode`` IR
  var cr = TreeCursor(pos: start.uint32)
  var nodes = scopeToIr(tree, cl, cr, allowExpr=true)
  if cr.hasNext(tree):
    # the tree must be an expression; the last node is required to be an atom
    let x = atomToIr(tree, cl, cr)
    if nodes.len == 0:
      x
    else:
      nodes.add x
      newExpr(cnkStmtListExpr, unknownLineInfo, nodes[^1].typ, nodes)
  else:
    # it's a statement list
    toSingleNode nodes

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
