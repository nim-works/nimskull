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
    mirtypes,
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

import std/options as std_options

from compiler/ast/ast import newSym, newType, rawAddSon
from compiler/sem/semdata import makeVarType

type
  TranslateCl = object
    graph: ModuleGraph
    idgen: IdGenerator
    env: ptr MirEnv
      ## read/write reference to the MirEnv. Stored here to prevent excessive
      ## parameter passing. Only the type environment is potentially modified

    owner: PSym

    locals: Store[LocalId, Local]
      ## the list of all locals in the body, taken from the ``MirBody``.
      ## Only needed for updating the type for alias locals

    inUnscoped: int
      ## whether the currently proceesed statement/expression is part of an
      ## unscoped control-flow context. Used to move definitions to the start
      ## of the enclosing scope, which is currently required for temporaries
      ## requiring destruction that are spawned as part of the right-hand
      ## operand of ``and``/``or``
    defs: seq[CgNode]
      ## the stack of locals/globals for which the ``cnkDef``/assignemnt needs
      ## to be inserted later

  TreeCursor = object
    ## A cursor into a ``MirBody``.
    pos: uint32 ## the index of the currently pointed to node
    origin {.cursor.}: PNode ## the source node

func newMagicNode(magic: TMagic, info: TLineInfo): CgNode =
  CgNode(kind: cnkMagic, info: info, magic: magic)

func get(t: MirBody, cr: var TreeCursor): lent MirNode {.inline.} =
  cr.origin = t.sourceFor(cr.pos.NodePosition)
  result = t.code[cr.pos]

  inc cr.pos

func skip(body: MirBody, cr: var TreeCursor) =
  ## Skips over the node or sub-tree at the cursor.
  let next = uint32 body.code.sibling(NodePosition cr.pos)
  assert next > cr.pos
  cr.pos = next

func enter(t: MirBody, cr: var TreeCursor): lent MirNode {.inline.} =
  assert t.code[cr.pos].kind in SubTreeNodes, "not a sub-tree"
  result = get(t, cr)

func leave(t: MirBody, cr: var TreeCursor) =
  discard "obsolete; a no-op for backwards compatibility"

template info(cr: TreeCursor): TLineInfo =
  cr.origin.info

template `[]`(t: MirBody, cr: TreeCursor): untyped =
  t.code[cr.pos]

template hasNext(cr: TreeCursor, t: MirBody): bool =
  cr.pos.int < t.code.len

template `[]=`(x: CgNode, i: Natural, n: CgNode) =
  x.kids[i] = n

template add(x: CgNode, y: CgNode) =
  x.kids.add y

template map(cl: TranslateCl, id: TypeId): lent PType =
  cl.env.types[id]

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

func newLabelNode(label: LabelId; info = unknownLineInfo): CgNode =
  CgNode(kind: cnkLabel, info: info, label: BlockId(label))

proc newExpr(kind: CgNodeKind, info: TLineInfo, typ: PType,
             kids: sink seq[CgNode]): CgNode =
  ## Variant of ``newExpr`` optimized for passing a pre-existing child
  ## node sequence.
  result = CgNode(kind: kind, info: info, typ: typ)
  result.kids = kids

func addIfNotEmpty(stmts: var seq[CgNode], n: sink CgNode) =
  ## Only adds the node to the list if it's not an empty node. Used to prevent
  ## the creation of statement-list expression that only consist of empty
  ## nodes + the result-expression (a statement-list expression is unnecessary
  ## in that case)
  if n.kind != cnkEmpty:
    stmts.add n

proc newDefaultCall(info: TLineInfo, typ: PType): CgNode =
  ## Produces the tree for a ``default`` magic call.
  newExpr(cnkCall, info, typ, [newMagicNode(mDefault, info)])

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

proc genObjConv(n: CgNode, to: PType, info: TLineInfo): CgNode =
  ## Depending on the type relationship between `n` and `to`, wraps `n` in
  ## either an up- or down-conversion. Returns `nil` if no up- or down-
  ## conversion is needed.
  let diff = inheritanceDiff(to.skipTypes(skipPtrs), n.typ.skipTypes(skipPtrs))
  if diff == 0:
    return nil
  result = newOp(
    if diff < 0: cnkObjUpConv else: cnkObjDownConv,
    info, to): n

# forward declarations:
proc stmtToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
              cr: var TreeCursor, stmts: var seq[CgNode])
proc scopeToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
               cr: var TreeCursor, stmts: var seq[CgNode])

proc handleSpecialConv(c: ConfigRef, n: CgNode, info: TLineInfo,
                       dest: PType): CgNode =
  ## Checks if a special conversion operator is required for a conversion
  ## between the source type (i.e. that of `n`) and the destination type.
  ## If it is, generates the conversion operation IR and returns it -- nil
  ## otherwise
  if dest.skipTypes(skipPtrs - {tyDistinct}).kind == tyObject:
    # if the destination is an object (or ptr/ref object), it must be an
    # object conversion
    genObjConv(n, dest, info)
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
  let typ = cl.map(n.typ)
  case n.kind
  of mnkProcVal:
    CgNode(kind: cnkProc, info: info, typ: typ, prc: n.prc)
  of mnkGlobal:
    CgNode(kind: cnkGlobal, info: info, typ: typ, global: n.global)
  of mnkConst:
    CgNode(kind: cnkConst, info: info, typ: typ, cnst: n.cnst)
  of mnkLocal, mnkParam, mnkTemp:
    newLocalRef(n.local, info, cl.map(cl.locals[n.local].typ))
  of mnkAlias:
    # the type of the node doesn't match the real one
    let
      id = n.local
      typ = cl.map(cl.locals[id].typ)
    # the view is auto-dereferenced here for convenience
    newOp(cnkDerefView, info, typ.base, newLocalRef(id, info, typ))
  of mnkNilLit:
    CgNode(kind: cnkNilLit, info: info, typ: typ)
  of mnkIntLit:
    CgNode(kind: cnkIntLit, info: info, typ: typ,
           intVal: cl.env[].getInt(n.number))
  of mnkUIntLit:
    CgNode(kind: cnkUIntLit, info: info, typ: typ,
           intVal: cl.env[].getInt(n.number))
  of mnkFloatLit:
    CgNode(kind: cnkFloatLit, info: info, typ: typ,
           floatVal: cl.env[].getFloat(n.number))
  of mnkStrLit:
    CgNode(kind: cnkStrLit, info: info, typ: typ, strVal: n.strVal)
  of mnkAstLit:
    CgNode(kind: cnkAstLit, info: info, typ: typ, astLit: cl.env[][n.ast])
  of mnkType:
    newTypeNode(info, typ)
  of mnkNone:
    # type arguments do use `mnkNone` in some situtations, so keep
    # the type
    CgNode(kind: cnkEmpty, info: info, typ: typ)
  of AllNodeKinds - Atoms:
    unreachable("not an atom: " & $n.kind)

proc atomToIr(tree: MirBody, cl: var TranslateCl,
              cr: var TreeCursor): CgNode {.inline.} =
  atomToIr(get(tree, cr), cl, cr.info)

proc tbExceptItem(tree: MirBody, cl: var TranslateCl, cr: var TreeCursor
                 ): CgNode =
  let n {.cursor.} = get(tree, cr)
  case n.kind
  of mnkLocal: newLocalRef(n.local, cr.info, cl.map(n.typ))
  of mnkType:  newTypeNode(cr.info, cl.map(n.typ))
  else:        unreachable()


proc lvalueToIr(tree: MirBody, cl: var TranslateCl, n: MirNode,
                cr: var TreeCursor; preferField = true): CgNode =
  ## Translates a MIR lvalue expression to the corresponding CG IR.
  ## Due to tagged unions (currently) not being addressable at the type-
  ## representation level, the exact meaning of ``mnkPathVariant`` is
  ## context-dependent -- `preferField` disambiguates whether it should be
  ## turned into a field access rather than a (pseudo) access of the tagged
  ## union.
  let
    info = cr.info
    typ = cl.map(n.typ)

  template recurse(): CgNode =
    lvalueToIr(tree, cl, tree.get(cr), cr, false)

  case n.kind
  of mnkLocal, mnkGlobal, mnkParam, mnkTemp, mnkAlias, mnkConst, mnkProcVal:
    return atomToIr(n, cl, info)
  of mnkPathNamed:
    let obj = recurse()
    result = newExpr(cnkFieldAccess, info, typ,
                     [obj, newFieldNode(lookupInType(obj.typ, n.field.int))])
  of mnkPathVariant:
    if preferField:
      let
        obj = recurse()
        field = lookupInType(obj.typ, n.field.int)
      result = newExpr(cnkFieldAccess, info, field.typ,
                      [obj, newFieldNode(field)])
    else:
      # variant access itself has no ``CgNode`` counterpart at the moment
      result = recurse()
  of mnkPathPos:
    result = newExpr(cnkTupleAccess, info, typ,
                     [recurse(),
                      CgNode(kind: cnkIntLit, intVal: n.position.BiggestInt)])
  of mnkPathArray:
    # special case in order to support string literal access
    # XXX: this needs to be removed once there is a dedicated run-time-
    #      sequence access operator
    let arg =
      if tree[cr].kind in LiteralDataNodes:
        atomToIr(tree, cl, cr)
      else:
        recurse()

    result = newExpr(cnkArrayAccess, info, typ,
                     [arg, atomToIr(tree, cl, cr)])
  of mnkPathConv:
    result = convToIr(cl, recurse(), info, typ)
  # dereferences are allowed at the end of a path tree
  of mnkDeref:
    result = newOp(cnkDeref, info, typ, atomToIr(tree, cl, cr))
  of mnkDerefView:
    result = newOp(cnkDerefView, info, typ, atomToIr(tree, cl, cr))
  of AllNodeKinds - LvalueExprKinds - {mnkProcVal}:
    unreachable(n.kind)

  leave(tree, cr)

proc lvalueToIr(tree: MirBody, cl: var TranslateCl,
                cr: var TreeCursor; preferField=true): CgNode {.inline.} =
  lvalueToIr(tree, cl, tree.get(cr), cr, preferField)

proc valueToIr(tree: MirBody, cl: var TranslateCl,
               cr: var TreeCursor): CgNode =
  case tree[cr].kind
  of mnkProcVal, mnkConst, mnkGlobal, mnkParam, mnkLocal, mnkTemp, mnkAlias,
     mnkType, LiteralDataNodes:
    atomToIr(tree, cl, cr)
  of mnkPathPos, mnkPathNamed, mnkPathArray, mnkPathConv, mnkPathVariant,
     mnkDeref, mnkDerefView:
    lvalueToIr(tree, cl, cr)
  else:
    unreachable("not a value: " & $tree[cr].kind)

proc labelToIr(tree: MirBody, cr: var TreeCursor): CgNode =
  ## Translates a MIR label to a CGIR label.
  assert tree[cr].kind == mnkLabel
  newLabelNode(tree.get(cr).label)

proc targetToIr(tree: MirBody, cr: var TreeCursor): CgNode =
  ## Translates a MIR target list to its CGIR counterpart. Both share the same
  ## structure, so the translation is straightforward.
  proc actionToIr(tree: MirBody, n: MirNode, info: TLineInfo): CgNode =
    case n.kind
    of mnkLabel:  newLabelNode(n.label)
    of mnkLeave:  newTree(cnkLeave, info, newLabelNode(n.label))
    of mnkResume: CgNode(kind: cnkResume, info: info)
    else:
      unreachable(n.kind)

  let n {.cursor.} = tree.get(cr)
  case n.kind
  of mnkLabel:
    result = actionToIr(tree, n, cr.info)
  of mnkTargetList:
    result = newTree(cnkTargetList, cr.info)
    for _ in 0..<n.len:
      result.add actionToIr(tree, tree.get(cr), cr.info)
    leave(tree, cr)
  else:
    unreachable(n.kind)

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
  of LiteralDataNodes, mnkType, mnkProcVal, mnkNone:
    # not a tag but an atom
    result = (false, atomToIr(n, cl, cr.info))
  of LvalueExprKinds:
    result = (false, lvalueToIr(tree, cl, n, cr))
  else:
    unreachable("not a valid argument expression")

  leave(tree, cr)

proc calleeToIr(tree: MirBody, cl: var TranslateCl, cr: var TreeCursor): CgNode =
  case tree[cr].kind
  of mnkMagic:
    newMagicNode(tree.get(cr).magic, cr.info)
  of mnkProc:
    let prc = tree.get(cr).prc
    # assign a type for the CGIR node, the code generators currently need it
    CgNode(kind: cnkProc, typ: cl.env[][prc].typ, info: cr.info, prc: prc)
  else:
    valueToIr(tree, cl, cr)

proc callToIr(tree: MirBody, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode =
  ## Translate a valid call-like tree to the CG IR.
  let info = cr.info
  result = newExpr((if n.kind == mnkCall: cnkCall else: cnkCheckedCall),
                   info, cl.map(n.typ))
  result.add calleeToIr(tree, cl, cr)

  # the code generators currently require some magics to not have any
  # arguments wrapped in ``cnkHiddenAddr`` nodes
  let noAddr = result[0].kind == cnkMagic and
               result[0].magic in FakeVarParams

  # translate the arguments:
  while tree[cr].kind in ArgumentNodes:
    var (mutable, arg) = argToIr(tree, cl, cr)
    if noAddr:
      if arg.typ.kind == tyVar:
        # auto-dereference the view
        # XXX: prevent this case from happening
        arg = newOp(cnkDerefView, arg.info, arg.typ.base, arg)
    elif mutable:
      arg = wrapInHiddenAddr(cl, arg)

    result.add arg

  if n.kind == mnkCheckedCall:
    result.add targetToIr(tree, cr)

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
    typ {.cursor.} = cl.map(entity.typ)

  var def: CgNode

  case entity.kind
  of mnkLocal, mnkTemp:
    let id = entity.local
    def = newLocalRef(id, info, typ)
  of mnkParam:
    # ignore 'def's for parameters
    def = newEmpty()
  of mnkGlobal:
    def = CgNode(kind: cnkGlobal, info: info, typ: typ,
                 global: entity.global)
  of mnkAlias:
    # MIR aliases are translated to var/lent views
    assert n.kind in {mnkBind, mnkBindMut}, "alias can only be defined by binds"
    let
      typ = makeVarType(cl.owner, typ, cl.idgen,
                        if n.kind == mnkBind: tyLent else: tyVar)
    # override the original type
    cl.locals[entity.local].typ = cl.env.types.add(typ)

    def = newLocalRef(entity.local, info, typ)
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
    if cl.inUnscoped > 0 and not isLet:
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
      elif cl.inUnscoped > 0:
        # move the default initialization to the start of the scope
        cl.defs.add def
        result = arg
      else:
        result = newStmt(cnkAsgn, info, [def, newDefaultCall(info, def.typ)])
    else:
      if sfImportc notin env.globals[def.global].flags and cl.inUnscoped > 0:
        # default intialization is required at the start of the scope
        cl.defs.add def
      result = newStmt(cnkAsgn, info, [def, arg])
  of cnkEmpty:
    result = def
  else:
    unreachable()

proc caseToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode

proc stmtToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl,
              cr: var TreeCursor, stmts: var seq[CgNode]) =
  let n {.cursor.} = tree.get(cr)
  let info = cr.info ## the source information of `n`

  template to(kind: CgNodeKind, args: varargs[untyped]) =
    let r = newStmt(kind, info, args)
    leave(tree, cr)
    stmts.add r

  template toList(k: CgNodeKind, body: untyped) =
    let res {.inject.} = newStmt(k, info)
    for _ in 0..<n.len:
      body
    leave(tree, cr)
    stmts.add res

  case n.kind
  of DefNodes:
    stmts.addIfNotEmpty defToIr(tree, env, cl, n, cr)
  of mnkAsgn, mnkInit, mnkSwitch:
    let
      dst = lvalueToIr(tree, cl, cr)
      (src, useFast) = sourceExprToIr(tree, cl, cr)
    to (if useFast: cnkFastAsgn else: cnkAsgn), dst, src
  of mnkGoto:
    to cnkGotoStmt, targetToIr(tree, cr)
  of mnkLoop:
    to cnkLoopStmt, targetToIr(tree, cr)
  of mnkLoopJoin:
    to cnkLoopJoinStmt, targetToIr(tree, cr)
  of mnkJoin:
    to cnkJoinStmt, labelToIr(tree, cr)
  of mnkExcept:
    let excpt = newTree(cnkExcept, info, labelToIr(tree, cr))
    if n.len > 1:
      # not a catch-all handler. Translate the filter items:
      for j in 1..<n.len-1:
        excpt.add tbExceptItem(tree, cl, cr)

      # then the jump target of the next handler:
      excpt.add targetToIr(tree, cr)

    leave(tree, cr)
    stmts.add excpt
    # XXX: temporary workaround, refer to ``inUnscoped`` doc comment
    inc cl.inUnscoped
  of mnkFinally:
    to cnkFinally, labelToIr(tree, cr)
  of mnkContinue:
    stmts.add newStmt(cnkContinueStmt, info, labelToIr(tree, cr))
    # skip the candidate list, it's not relevant to code generation:
    for _ in 1..<n.len:
      tree.skip(cr)
    leave(tree, cr)
  of mnkVoid:
    var res = exprToIr(tree, cl, cr)
    if res.typ.isEmptyType():
      # a void expression doesn't need to be discarded
      discard
    else:
      res = newStmt(cnkVoidStmt, info, [res])
    leave(tree, cr)
    stmts.add res
  of mnkIf:
    to cnkIfStmt, valueToIr(tree, cl, cr), labelToIr(tree, cr)
    # XXX: temporary workaround, refer to ``inUnscoped`` doc comment
    inc cl.inUnscoped
  of mnkEndStruct:
    # XXX: temporary workaround, refer to ``inUnscoped`` doc comment
    dec cl.inUnscoped
    to cnkEnd, labelToIr(tree, cr)
  of mnkRaise:
    # the operand can either be empty or an lvalue expression
    let
      arg {.cursor.} = tree.get(cr)
      res = newStmt(cnkRaiseStmt, info):
        case arg.kind
        of mnkNone: newEmpty()
        else:       lvalueToIr(tree, cl, arg, cr)

    res.add targetToIr(tree, cr)
    stmts.add res
    leave(tree, cr)
  of mnkCase:
    stmts.add caseToIr(tree, env, cl, n, cr)
  of mnkAsm:
    toList cnkAsmStmt:
      res.add valueToIr(tree, cl, cr)
  of mnkEmit:
    toList cnkEmitStmt:
      res.add valueToIr(tree, cl, cr)
  of mnkScope:
    leave(tree, cr)
    scopeToIr(tree, env, cl, cr, stmts)
  of mnkDestroy:
    unreachable("a 'destroy' that wasn't lowered")
  of AllNodeKinds - StmtNodes + {mnkEndScope}:
    unreachable(n.kind)

proc setElementToIr(tree: MirBody, cl: var TranslateCl,
                    cr: var TreeCursor): CgNode =
  ## Translates a sub-tree appearing as a branch label or in a set
  ## construction to the CGIR.
  case tree[cr].kind
  of LvalueExprKinds, LiteralDataNodes:
    result = valueToIr(tree, cl, cr)
  of mnkRange:
    discard enter(tree, cr)
    result = newTree(cnkRange, unknownLineInfo,
                     [valueToIr(tree, cl, cr), valueToIr(tree, cl, cr)])
    leave(tree, cr)
  else:
    unreachable()

proc caseToIr(tree: MirBody, env: MirEnv, cl: var TranslateCl, n: MirNode,
              cr: var TreeCursor): CgNode =
  assert n.kind == mnkCase
  result = newStmt(cnkCaseStmt, cr.info, [valueToIr(tree, cl, cr)])

  # translate the branches:
  for _ in 1..<n.len:
    let br {.cursor.} = enter(tree, cr)

    let branch = newTree(cnkBranch, cr.info)
    for _ in 0..<br.len-1:
      branch.add setElementToIr(tree, cl, cr)

    # the jump target is in the last slot:
    branch.add labelToIr(tree, cr)

    result.add branch
    leave(tree, cr)

  leave(tree, cr)

proc exprToIr(tree: MirBody, cl: var TranslateCl,
              cr: var TreeCursor): CgNode =
  ## Translates a MIR expression to the corresponding CG IR representation.
  ## Moves the cursor to the next tree item.
  let n {.cursor.} = get(tree, cr)
  let info = cr.info

  template op(kind: CgNodeKind, e: CgNode): CgNode =
    let r = newOp(kind, info, cl.map(n.typ), e)
    leave(tree, cr)
    r

  template treeOp(k: CgNodeKind, body: untyped): CgNode =
    let res {.inject.} = newExpr(k, info, cl.map(n.typ))
    for _ in 0..<n.len:
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
  of mnkToSlice, mnkToMutSlice:
    treeOp cnkToSlice:
      res.add valueToIr(tree, cl, cr)
  of mnkAddr:
    op cnkAddr, lvalueToIr(tree, cl, cr)
  of mnkDeref:
    op cnkDeref, atomToIr(tree, cl, cr)
  of mnkView, mnkMutView:
    op cnkHiddenAddr, lvalueToIr(tree, cl, cr)
  of mnkDerefView:
    op cnkDerefView, atomToIr(tree, cl, cr)
  of mnkSetConstr:
    treeOp cnkSetConstr:
      res.add setElementToIr(tree, cl, cr)
  of mnkArrayConstr, mnkSeqConstr:
    treeOp cnkArrayConstr:
      res.add argToIr(tree, cl, cr)[1]
  of mnkTupleConstr:
    treeOp cnkTupleConstr:
      res.add argToIr(tree, cl, cr)[1]
  of mnkClosureConstr:
    treeOp cnkClosureConstr:
      res.add argToIr(tree, cl, cr)[1]
  of mnkObjConstr, mnkRefConstr:
    let typ = cl.map(n.typ)
    assert typ.skipTypes(abstractVarRange).kind in {tyObject, tyRef}
    treeOp cnkObjConstr:
      discard enter(tree, cr) # enter the binding tree
      let f = newFieldNode(lookupInType(typ, get(tree, cr).field))
      res.add newTree(cnkBinding, cr.info, [f, argToIr(tree, cl, cr)[1]])
      leave(tree, cr)
  of mnkCall, mnkCheckedCall:
    callToIr(tree, cl, n, cr)
  of UnaryOps:
    const Map = [mnkNeg: cnkNeg]
    newExpr(Map[n.kind], info, cl.map(n.typ), valueToIr(tree, cl, cr))
  of BinaryOps:
    const Map = [mnkAdd: cnkAdd, mnkSub: cnkSub,
                 mnkMul: cnkMul, mnkDiv: cnkDiv, mnkModI: cnkModI]
    newExpr(Map[n.kind], info, cl.map(n.typ)):
      @[valueToIr(tree, cl, cr), valueToIr(tree, cl, cr)]
  of mnkCopy, mnkMove, mnkSink:
    # translation of assignments needs to handle all modifiers
    unreachable("loose assignment modifier")
  of AllNodeKinds - ExprKinds - {mnkNone} + {mnkEndScope}:
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
               cr: var TreeCursor, stmts: var seq[CgNode]) =
  let
    prev = cl.defs.len
    prevInUnscoped = cl.inUnscoped
    start = stmts.len

  # a scope is entered, meaning that we're no longer in an unscoped context
  cl.inUnscoped = 0

  # translate all statements:
  while cr.hasNext(tree) and tree[cr].kind != mnkEndScope:
    stmtToIr(tree, env, cl, cr, stmts)

  if cr.hasNext(tree) and tree[cr].kind == mnkEndScope:
    skip(tree, cr)

  if cl.defs.len > prev:
    # insert all the lifted defs at the start of the scope
    for i in countdown(cl.defs.high, prev):
      stmts.insert genDefFor(move cl.defs[i]), start

    # "pop" the elements that were added as part of this scope:
    cl.defs.setLen(prev)

  cl.inUnscoped = prevInUnscoped

proc tb(tree: MirBody, env: MirEnv, cl: var TranslateCl,
        start: NodePosition): CgNode =
  ## Translate `tree` to the corresponding ``CgNode`` representation.
  var cr = TreeCursor(pos: start.uint32)
  var stmts: seq[CgNode]
  scopeToIr(tree, env, cl, cr, stmts)

  # XXX: the list of statements is still wrapped in a node for now, but
  #      this needs to change once all code generators use the new CGIR
  result = newStmt(cnkStmtList, unknownLineInfo)
  result.kids = move stmts

proc generateIR*(graph: ModuleGraph, idgen: IdGenerator, env: var MirEnv,
                 owner: PSym,
                 body: sink MirBody): Body =
  ## Generates the ``CgNode`` IR corresponding to the input MIR `body`,
  ## using `idgen` to provide new IDs when creating symbols.
  var cl = TranslateCl(graph: graph, idgen: idgen, env: addr env,
                       owner: owner, locals: move body.locals)

  result = Body()
  result.code = tb(body, env, cl, NodePosition 0)
  result.locals = cl.locals
