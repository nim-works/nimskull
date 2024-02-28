## Implements the translation from the MIR to the ``CgNode`` IR. All code
## reaching the code generation phase passes through here.
##
## .. note::
##   The `tb` prefix that's still used in some places is an abbreviation of
##   "translate back"
##
## .. note::
##   The ``CgNode`` IR is slated for removal, with the MIR intended to take
##   its place as the code-generator input.

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
    mirbodies,
    mirenv,
    mirtrees,
    sourcemaps
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/utils/[
    containers,
    idioms,
    int128
  ]

from compiler/ast/ast import newSym, newType, rawAddSon
from compiler/sem/semdata import makeVarType

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

  TreeCursor = object
    ## A cursor into a ``MirBody``.
    pos: uint32 ## the index of the currently pointed to node
    origin {.cursor.}: PNode ## the source node

template isFilled(x: LocalId): bool =
  # '0' is a valid ID, but this procedure is only used for
  # temporaries, which can never map to the result variable
  x.int != 0

func newMagicNode(magic: TMagic, info: TLineInfo): CgNode =
  CgNode(kind: cnkMagic, info: info, magic: magic)

func get(t: MirBody, cr: var TreeCursor): lent MirNode {.inline.} =
  cr.origin = t.sourceFor(cr.pos.NodePosition)
  result = t.code[cr.pos]

  inc cr.pos

func enter(t: MirBody, cr: var TreeCursor): lent MirNode {.inline.} =
  assert t.code[cr.pos].kind in SubTreeNodes, "not a sub-tree"
  result = get(t, cr)

func leave(t: MirBody, cr: var TreeCursor) =
  assert t.code[cr.pos].kind == mnkEnd, "not at the end of sub-tree"
  inc cr.pos

template info(cr: TreeCursor): TLineInfo =
  cr.origin.info

template `[]`(t: MirBody, cr: TreeCursor): untyped =
  t.code[cr.pos]

template hasNext(cr: TreeCursor, t: MirBody): bool =
  cr.pos.int < t.code.len

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

func newFieldNode(s: PSym; info = unknownLineInfo): CgNode =
  CgNode(kind: cnkField, info: info, typ: s.typ, field: s)

func newLabelNode(blk: BlockId; info = unknownLineInfo): CgNode =
  CgNode(kind: cnkLabel, info: info, label: blk)

proc newExpr(kind: CgNodeKind, info: TLineInfo, typ: PType,
             kids: sink seq[CgNode]): CgNode =
  ## Variant of ``newExpr`` optimized for passing a pre-existing child
  ## node sequence.
  result = CgNode(kind: kind, info: info, typ: typ)
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
  of nkSym:
    # special case for raw symbols used with emit and asm statements
    assert val.sym.kind == skField
    node(cnkField, field, val.sym)
  else:
    unreachable("implement: " & $val.kind)

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
proc stmtToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
              cr: var TreeCursor): CgNode
proc scopeToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
               cr: var TreeCursor, allowExpr=false): seq[CgNode]

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
    result = newOp(cnkLvalueConv, info, dest, n)

proc atomToIr(n: MirNode, cl: TranslateCl, info: TLineInfo): CgNode =
  case n.kind
  of mnkProc:
    CgNode(kind: cnkProc, info: info, typ: n.typ, prc: n.prc)
  of mnkGlobal:
    CgNode(kind: cnkGlobal, info: info, typ: n.typ, global: n.global)
  of mnkConst:
    CgNode(kind: cnkConst, info: info, typ: n.typ, cnst: n.cnst)
  of mnkLocal, mnkParam:
    # paramaters are treated like locals in the code generators
    assert n.sym.id in cl.localsMap
    newLocalRef(cl.localsMap[n.sym.id], info, n.sym.typ)
  of mnkTemp:
    newLocalRef(cl.tempMap[n.temp], info, n.typ)
  of mnkAlias:
    # the type of the node doesn't match the real one
    let
      id = cl.tempMap[n.temp]
      typ = cl.locals[id].typ
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

proc atomToIr(tree: MirBody, cl: var TranslateCl,
              cr: var TreeCursor): CgNode {.inline.} =
  atomToIr(get(tree, cr), cl, cr.info)

proc tbExceptItem(tree: MirBody, cl: var TranslateCl, cr: var TreeCursor
                 ): CgNode =
  let n {.cursor.} = get(tree, cr)
  case n.kind
  of mnkLocal:
    # the 'except' branch acts as a definition for the local
    let id = cl.locals.add initLocal(n.sym)
    cl.localsMap[n.sym.id] = id
    newLocalRef(id, cr.info, n.typ)
  of mnkType:  newTypeNode(cr.info, n.typ)
  else:        unreachable()


proc lvalueToIr(tree: MirBody, cl: var TranslateCl, n: MirNode,
                cr: var TreeCursor; preferField = true): CgNode =
  ## Translates a MIR lvalue expression to the corresponding CG IR.
  ## Due to tagged unions (currently) not being addressable at the type-
  ## representation level, the exact meaning of ``mnkPathVariant`` is
  ## context-dependent -- `preferField` disambiguates whether it should be
  ## turned into a field access rather than a (pseudo) access of the tagged
  ## union.
  let info = cr.info

  template recurse(): CgNode =
    lvalueToIr(tree, cl, tree.get(cr), cr, false)

  case n.kind
  of mnkLocal, mnkGlobal, mnkParam, mnkTemp, mnkAlias, mnkConst, mnkProc:
    return atomToIr(n, cl, info)
  of mnkPathNamed:
    result = newExpr(cnkFieldAccess, info, n.typ,
                     [recurse(), newFieldNode(n.field)])
  of mnkPathVariant:
    if preferField:
      result = newExpr(cnkFieldAccess, cr.info, n.field.typ,
                      [recurse(), newFieldNode(n.field)])
    else:
      # variant access itself has no ``CgNode`` counterpart at the moment
      result = recurse()
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
  of AllNodeKinds - LvalueExprKinds - {mnkProc}:
    unreachable(n.kind)

  leave(tree, cr)

proc lvalueToIr(tree: MirBody, cl: var TranslateCl,
                cr: var TreeCursor; preferField=true): CgNode {.inline.} =
  lvalueToIr(tree, cl, tree.get(cr), cr, preferField)

proc valueToIr(tree: MirBody, cl: var TranslateCl,
               cr: var TreeCursor): CgNode =
  case tree[cr].kind
  of mnkProc, mnkConst, mnkGlobal, mnkParam, mnkLocal, mnkTemp, mnkAlias,
     mnkLiteral, mnkType:
    atomToIr(tree, cl, cr)
  of mnkPathPos, mnkPathNamed, mnkPathArray, mnkPathConv, mnkPathVariant,
     mnkDeref, mnkDerefView:
    lvalueToIr(tree, cl, cr)
  else:
    unreachable("not a value: " & $tree[cr].kind)

proc argToIr(tree: MirBody, cl: var TranslateCl,
             cr: var TreeCursor): (bool, CgNode) =
  ## Translates a MIR argument tree to the corresponding CG IR tree.
  ## Returns both the tree and whether the argumnet was wrapped in a tag
  ## operator (which indicates that the parameter is a ``var`` parameter).
  var n {.cursor.} = tree.get(cr)
  assert n.kind in ArgumentNodes, "argument node expected: " & $n.kind
  # the inner node may be a tag node
  n = tree.get(cr)
  case n.kind
  of mnkTag:
    # it is one, the expression must be an lvalue
    result = (true, lvalueToIr(tree, cl, cr))
    leave(tree, cr)
  of mnkLiteral, mnkType, mnkProc, mnkNone:
    # not a tag but an atom
    result = (false, atomToIr(n, cl, cr.info))
  of LvalueExprKinds:
    result = (false, lvalueToIr(tree, cl, n, cr))
  else:
    unreachable("not a valid argument expression")

  leave(tree, cr)

proc callToIr(tree: MirBody, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode =
  ## Translate a valid call-like tree to the CG IR.
  let info = cr.info
  result = newExpr((if n.kind == mnkCall: cnkCall else: cnkCheckedCall),
                   info, n.typ)
  result.add: # the callee
    case tree[cr].kind
    of mnkMagic: newMagicNode(tree.get(cr).magic, info)
    else:        valueToIr(tree, cl, cr)

  # the code generators currently require some magics to not have any
  # arguments wrapped in ``cnkHiddenAddr`` nodes
  let noAddr = result[0].kind == cnkMagic and
               result[0].magic in FakeVarParams

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

proc exprToIr(tree: MirBody, cl: var TranslateCl, cr: var TreeCursor): CgNode

proc sourceExprToIr(tree: MirBody, cl: var TranslateCl,
                    cr: var TreeCursor): tuple[n: CgNode, useFast: bool] =
  ## Translates the MIR expression appearing in an assignment's source
  ## slot. Assignment modifiers are dropped, and whether a fast assignment or
  ## normal assignment should be used is computed and returned.
  case tree[cr].kind
  of mnkCopy, mnkSink:
    # requires a full assignment
    discard enter(tree, cr)
    result = (valueToIr(tree, cl, cr), false)
    leave(tree, cr)
  of mnkMove:
    # an ``x = move y`` assignment can be turned into a fast assignment
    discard enter(tree, cr)
    result = (valueToIr(tree, cl, cr), true)
    leave(tree, cr)
  of LvalueExprKinds:
    # a fast assignment is correct for all raw lvalues
    result = (lvalueToIr(tree, cl, cr), true)
  else:
    # rvalue expressions require a full assignment
    result = (exprToIr(tree, cl, cr), false)

proc defToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
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
  of mnkParam:
    # ignore 'def's for parameters
    def = newEmpty()
  of mnkGlobal:
    def = CgNode(kind: cnkGlobal, info: info, typ: entity.typ,
                 global: entity.global)
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

  var arg =
    if n.kind in {mnkBind, mnkBindMut} and tree[cr].kind in LvalueExprKinds:
      # don't use the field interperation for variant access
      lvalueToIr(tree, cl, cr, preferField=false)
    else:
      sourceExprToIr(tree, cl, cr)[0]
  leave(tree, cr)
  if n.kind in {mnkBind, mnkBindMut} and arg.typ.kind notin {tyVar, tyLent}:
    # wrap the operand in an address-of operation
    arg = newOp(cnkHiddenAddr, info, def.typ, arg)

  let isLet = (entity.kind == mnkTemp and n.kind == mnkDefCursor) or
              (entity.kind == mnkTemp and not hasDestructor(def.typ)) or
              (entity.kind == mnkAlias)
  # to reduce the pressure on the code generator, locals that never cross
  # structured control-flow boundaries are not lifted. As a temporary
  # measure, cursor temporaries and aliases are treated as such, but
  # do note that this is not guaranteed and relies on how `mirgen`
  # produces MIR code

  case def.kind
  of cnkLocal:
    if cl.inUnscoped and not isLet:
      # add the local to the list of moved definitions and only emit
      # an assignment
      cl.defs.add copyTree(def)
      result =
        case arg.kind
        of cnkEmpty: arg
        else:        newStmt(cnkAsgn, info, [def, arg])
    else:
      result = newStmt(cnkDef, info, [def, arg])
  of cnkGlobal:
    # there are no defs for globals in the ``CgNode`` IR, so we
    # emit an assignment that has the equivalent behaviour (in
    # terms of initialization)
    case arg.kind
    of cnkEmpty:
      if sfImportc in env.globals[def.global].flags:
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
      if sfImportc notin env.globals[def.global].flags and cl.inUnscoped:
        # default intialization is required at the start of the scope
        cl.defs.add def
      result = newStmt(cnkAsgn, info, [def, arg])
  of cnkEmpty:
    result = def
  else:
    unreachable()

proc bodyToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
              cr: var TreeCursor): CgNode =
  ## Generates the ``CgNode`` tree for the body of a construct that implies
  ## some form of control-flow.
  let prev = cl.inUnscoped
  # assume the body is unscoped until stated otherwise
  cl.inUnscoped = true
  result = stmtToIr(tree, env, cl, cr)
  cl.inUnscoped = prev

proc caseToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode

proc stmtToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
              cr: var TreeCursor): CgNode =
  let n {.cursor.} = tree.get(cr)
  let info = cr.info ## the source information of `n`

  template body(): CgNode =
    bodyToIr(tree, env, cl, cr)

  template to(kind: CgNodeKind, args: varargs[untyped]): CgNode =
    let r = newStmt(kind, info, args)
    leave(tree, cr)
    r

  template toList(k: CgNodeKind, body: untyped): CgNode =
    let res {.inject.} = newStmt(k, info)
    while tree[cr].kind != mnkEnd:
      body
    leave(tree, cr)
    res

  case n.kind
  of DefNodes:
    defToIr(tree, env, cl, n, cr)
  of mnkAsgn, mnkInit, mnkSwitch:
    let
      dst = lvalueToIr(tree, cl, cr)
      (src, useFast) = sourceExprToIr(tree, cl, cr)
    to (if useFast: cnkFastAsgn else: cnkAsgn), dst, src
  of mnkRepeat:
    to cnkRepeatStmt, body()
  of mnkBlock:
    cl.blocks.add n.label # push the label to the stack
    let body = body()
    cl.blocks.setLen(cl.blocks.len - 1) # pop block from the stack
    to cnkBlockStmt, newLabelNode(cl.blocks.len.BlockId, info), body
  of mnkTry:
    let res = newStmt(cnkTryStmt, info, [body()])
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
          res.add excpt

          leave(tree, cr)

      of mnkFinally:
        res.add newTree(cnkFinally, cr.info, body())
      else:
        unreachable(it.kind)

      leave(tree, cr)

    leave(tree, cr)
    res
  of mnkBreak:
    # find the stack index of the enclosing 'block' identified by the break's
    # label; we use the index as the ID
    var idx = cl.blocks.high
    while idx >= 0 and cl.blocks[idx] != n.label:
      dec idx
    newStmt(cnkBreakStmt, info, [newLabelNode(BlockId idx, info)])
  of mnkReturn:
    newNode(cnkReturnStmt, info)
  of mnkPNode:
    discard
  of mnkVoid:
    var res = exprToIr(tree, cl, cr)
    if res.typ.isEmptyType():
      # a void expression doesn't need to be discarded
      discard
    else:
      res = newStmt(cnkVoidStmt, info, [res])
    leave(tree, cr)
    res
  of mnkIf:
    to cnkIfStmt, valueToIr(tree, cl, cr), body()
  of mnkRaise:
    # the operand can either be empty or an lvalue expression
    to cnkRaiseStmt:
      case tree[cr].kind
      of mnkNone: atomToIr(tree, cl, cr)
      else:       lvalueToIr(tree, cl, cr)
  of mnkCase:
    caseToIr(tree, env, cl, n, cr)
  of mnkAsm:
    toList cnkAsmStmt:
      res.add valueToIr(tree, cl, cr)
  of mnkEmit:
    toList cnkEmitStmt:
      res.add valueToIr(tree, cl, cr)
  of mnkStmtList:
    toList cnkStmtList:
      res.kids.addIfNotEmpty stmtToIr(tree, env, cl, cr)
  of mnkScope:
    toSingleNode scopeToIr(tree, env, cl, cr)
  of mnkDestroy:
    unreachable("a 'destroy' that wasn't lowered")
  of AllNodeKinds - StmtNodes:
    unreachable(n.kind)

proc caseToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode =
  assert n.kind == mnkCase
  result = newStmt(cnkCaseStmt, cr.info, [valueToIr(tree, cl, cr)])
  for j in 0..<n.len:
    let br {.cursor.} = enter(tree, cr)

    result.add newTree(cnkBranch, cr.info)
    if br.len > 0:
      for x in 0..<br.len:
        assert tree[cr].kind in {mnkConst, mnkLiteral}
        result[^1].add atomToIr(tree, cl, cr)

    result[^1].add bodyToIr(tree, env, cl, cr)
    leave(tree, cr)

  leave(tree, cr)

proc exprToIr(tree: MirBody, cl: var TranslateCl,
              cr: var TreeCursor): CgNode =
  ## Translates a MIR expression to the corresponding CG IR representation.
  ## Moves the cursor to the next tree item.
  let n {.cursor.} = get(tree, cr)
  let info = cr.info

  template op(kind: CgNodeKind, e: CgNode): CgNode =
    let r = newOp(kind, info, n.typ, e)
    leave(tree, cr)
    r

  template treeOp(k: CgNodeKind, body: untyped): CgNode =
    let res {.inject.} = newExpr(k, info, n.typ)
    while tree[cr].kind != mnkEnd:
      body
    leave(tree, cr)
    res

  case n.kind
  of Atoms:
    atomToIr(n, cl, info)
  of mnkPathVariant, mnkPathArray, mnkPathConv, mnkPathNamed, mnkPathPos:
    lvalueToIr(tree, cl, n, cr)
  of mnkCast:
    op cnkCast, valueToIr(tree, cl, cr)
  of mnkConv:
    op cnkConv, valueToIr(tree, cl, cr)
  of mnkStdConv:
    op cnkHiddenConv, valueToIr(tree, cl, cr)
  of mnkToSlice:
    treeOp cnkToSlice:
      res.add valueToIr(tree, cl, cr)
  of mnkAddr:
    op cnkAddr, lvalueToIr(tree, cl, cr)
  of mnkDeref:
    op cnkDeref, atomToIr(tree, cl, cr)
  of mnkView:
    op cnkHiddenAddr, lvalueToIr(tree, cl, cr)
  of mnkDerefView:
    op cnkDerefView, atomToIr(tree, cl, cr)
  of mnkObjConstr:
    assert n.typ.skipTypes(abstractVarRange).kind in {tyObject, tyRef}
    treeOp cnkObjConstr:
      let f = newFieldNode(get(tree, cr).field)
      res.add newTree(cnkBinding, cr.info, [f, argToIr(tree, cl, cr)[1]])
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

    treeOp kind:
      res.add argToIr(tree, cl, cr)[1]
  of mnkCall, mnkCheckedCall:
    callToIr(tree, cl, n, cr)
  of UnaryOps:
    const Map = [mnkNeg: cnkNeg]
    treeOp Map[n.kind]:
      res.add valueToIr(tree, cl, cr)
  of BinaryOps:
    const Map = [mnkAdd: cnkAdd, mnkSub: cnkSub,
                 mnkMul: cnkMul, mnkDiv: cnkDiv, mnkModI: cnkModI]
    treeOp Map[n.kind]:
      res.kids = @[valueToIr(tree, cl, cr), valueToIr(tree, cl, cr)]
  of mnkCopy, mnkMove, mnkSink:
    # translation of assignments needs to handle all modifiers
    unreachable("loose assignment modifier")
  of AllNodeKinds - ExprKinds - {mnkNone}:
    unreachable(n.kind)

proc genDefFor(sym: sink CgNode): CgNode =
  ## Produces the statement tree of a definition for the given symbol-like
  ## node. Globals use an assignment.
  case sym.kind
  of cnkLocal:
    newStmt(cnkDef, sym.info, [sym, newEmpty()])
  of cnkGlobal:
    # emulate the default-initialization behaviour
    newStmt(cnkAsgn, sym.info, [sym, newDefaultCall(sym.info, sym.typ)])
  else:
    unreachable()

proc scopeToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
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
    stmts.addIfNotEmpty stmtToIr(tree, env, cl, cr)

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

proc tb(tree: MirBody, env: MirEnv, cl: var TranslateCl,
        start: NodePosition): CgNode =
  ## Translate `tree` to the corresponding ``CgNode`` representation.
  var cr = TreeCursor(pos: start.uint32)
  var nodes = scopeToIr(tree, env, cl, cr, allowExpr=true)
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

proc generateIR*(graph: ModuleGraph, idgen: IdGenerator, env: MirEnv,
                 owner: PSym,
                 body: sink MirBody): Body =
  ## Generates the ``CgNode`` IR corresponding to the input MIR `body`,
  ## using `idgen` to provide new IDs when creating symbols.
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
  result.code = tb(body, env, cl, NodePosition 0)
  result.locals = cl.locals
