## Implements the translation from AST to the MIR. The input AST is expected
## to have already been transformed by ``transf``.
##
## In terms of operation, the input AST is traversed via recursion, with
## declarative constructs not relevant to the MIR being ignored.
##
## None of the MIR operations that imply structured control-flow produce a
## value, so the input expression that do (``if``, ``case``, ``block``, and
## ``try``) are translated into statements where the last expression in each
## clause is turned into an assignment to, depending on the context where
## they're used, either a temporary or existing lvalue expression. The latter
## are forwarded to the generation procedures via ``Destination``.
##
## For efficiency, ``MirBuilder`` double-buffering functionality is used for
## emitting the trees. Statements are directly emitted into the final buffer
## (after all their operands were emitted), while expressions are first
## emitted into the staging buffer, with the callsite then deciding what to
## do with them.
##
## The values of rvalue operations, calls, and construction are first captured
## in temporaries (as the MIR doesn't support them being used as, e.g., call
## arguments directly). In general, all call arguments are first assigned to a
## temporary, except if the expression is stable (in case of by-name
## arguments) or pure (in case of non-sink by-value arguments).
##
## Pure expression are those that don't have side-effect and always refer to
## the exact same value, whereas stable expression are those that don't have
## side-effects and always refer to the same *location*.
##
## Whether a temporary is owning (that is, it needs to be destroyed later)
## depends on both the value it captures and how its used. If the captured
## value is coming from a call or construction of destructible value, the
## temporary, otherwise its non-owning, except if used in a consuming
## context (`sink` parameter or aggregate construction).
##
## Origin information
## ==================
##
## Each produced ``MirNode`` is associated with the ``PNode`` it originated
## from (referred to as "source information"). The ``PNode`` is registered in
## the ``SourceMap``, with the resulting ``SourceId`` then assigned to the
## nodes associated with the AST.
##
## In order to reduce the amount manual bookkeeping and to improve the
## ergonomics of producing MIR node sequences, assigning the ``SourceId``
## is decoupled from the initial production. ``SourceProvider`` keeps track of
## the currently processed AST node and manages setting the ``info`` field on
## nodes.
##
## Changing the active AST node is done via calling the ``useSource`` routine,
## which will apply the previous AST node as the origin to all MIR nodes
## added to a ``MirBuffer`` since the last call to ``useSource``. When
## the scope ``useSource`` is called in is exited, the previous AST node is
## restored as the active origin, allowing for arbitrary nesting.
##

import
  std/[
    tables
  ],
  compiler/ast/[
    ast,
    astmsgs, # for generating the field error message
    lineinfos,
    trees,
    types,
    wordrecg
  ],
  compiler/mir/[
    mirbodies,
    mirconstr,
    mirenv,
    mirtrees,
    sourcemaps
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/front/[
    options
  ],
  compiler/sem/[
    ast_analysis
  ],
  compiler/utils/[
    containers,
    idioms
  ]

type
  DestFlag = enum
    ## Extra information about an assignment destination. The flags are used to
    ## decide which kind of assignment to use
    dfEmpty ## the destination doesn't store a value yet
    dfOwns  ## the destination is an owning location

  Destination = object
    ## Stores the information necessary to generate the code for an assignment
    ## to some destination.
    case isSome: bool
    of false: discard
    of true:  val: Value

    flags: set[DestFlag]

  Block = object
    ## Information about a ``block``
    label: PSym ## the symbol of the block's label. 'nil' if the block has no
                ## label
    id: LabelId ## the block's internal label ID

  SourceProvider = object
    ## Stores the active origin and the in-progress database of origin
    ## ``PNode``s. Both are needed together in most cases, hence their bundling
    ## into an object
    active: tuple[n: PNode, id: SourceId]
      ## the ``PNode`` to use as the origin for emitted ``MirNode``s (if none
      ## is explicitly provided). If `id` is 'none', no database entry exists
      ## for the ``PNode`` yet
    map: SourceMap
      ## the in-progress database of origin ``PNode``s

  GenOption* = enum
    goIsNimvm     ## choose the ``nimvm`` branch for ``when nimvm`` statements
    goGenTypeExpr ## don't omit type expressions
    goIsCompileTime ## whether the code is meant to be run at compile-time.
                    ## Affects handling of ``.compileTime`` globals

  TranslationConfig* = object
     ## Extra configuration for the AST -> MIR translation.
     options*: set[GenOption]
     magicsToKeep*: set[TMagic]
      ## magic procedures that need to be referenced via their symbols, either
      ## because they're not really magic or because the symbol has
      ## additional information

  TCtx = object
    # working state:
    env: MirEnv
      ## for convenience and safety, `TCtx` temporarily assumes full
      ## ownership of the environment
    builder: MirBuilder ## the builder for generating the MIR trees

    blocks: seq[Block] ## the stack of active ``block``s. Used for looking up
                       ## break targets

    sp: SourceProvider

    numLabels: int ## provides the ID to use for the next label

    # input:
    context: TSymKind ## what entity the input AST is part of (e.g. procedure,
                      ## macro, module, etc.). Used to allow or change how the
                      ## AST is interpreted in some places
    userOptions: set[TOption]
    graph: ModuleGraph

    config: TranslationConfig

  ExprKind = enum
    Literal
    Lvalue
    Rvalue         # non-owning rvalue that can only be copied
    OwnedRvalue    # rvalue that requires destruction

const
  abstractInstTypeClass = abstractInst + tyUserTypeClasses
  # TODO: this set shouldn't be needed. ``tyUserTypeClass`` and
  #       ``tyUserTypeClassInst`` should be turned into either aliases or
  #       ``tyGenericInst`` types when they're resolved
  ComplexExprs = {nkIfExpr, nkCaseStmt, nkBlockExpr, nkTryStmt}
    ## The expression that are treated as complex and which are transformed
    ## into assignments-to-temporaries

func isHandleLike(t: PType): bool =
  t.skipTypes(abstractInst).kind in {tyPtr, tyRef, tyLent, tyVar, tyOpenArray}

# XXX: copied from ``injectdestructors``. Move somewhere common
proc isCursor(n: PNode): bool =
  ## Computes whether the expression `n` names a location that is a cursor
  case n.kind
  of nkSym:
    sfCursor in n.sym.flags
  of nkDotExpr:
    isCursor(n[1])
  of nkCheckedFieldExpr:
    isCursor(n[0])
  else:
    false

func endsInNoReturn(n: PNode): bool =
  ## Tests and returns whether the simple or compound statement `n` ends in a
  ## no-return statement

  # TODO: this is a patched version of ``sem.endsInNoReturn`` that also
  #       considers ``nkPragmaBlock``. Move this procedure somewhere common and
  #       replace ``sem.endsInNoReturn`` with it
  const SkipSet = {nkStmtList, nkStmtListExpr, nkPragmaBlock}
  var it {.cursor.} = n
  while it.kind in SkipSet and it.len > 0:
    it = it.lastSon

  result = it.kind in nkLastBlockStmts or
    (it.kind in nkCallKinds and it[0].kind == nkSym and
     sfNoReturn in it[0].sym.flags)

func selectWhenBranch(n: PNode, isNimvm: bool): PNode =
  assert n.kind == nkWhen
  if isNimvm: n[0][1]
  else:       n[1][0]

func selectDefKind(s: PSym): range[mnkDef..mnkDefCursor] =
  ## Returns the kind of definition to use for the given entity
  if sfCursor in s.flags: mnkDefCursor
  else:                   mnkDef

func initDestination(v: sink Value, isFirst, sink: bool): Destination =
  var flags: set[DestFlag]
  if isFirst:
    flags.incl dfEmpty
  if sink:
    flags.incl dfOwns

  Destination(isSome: true, val: v, flags: flags)

proc typeOrVoid(g: ModuleGraph, t: PType): PType =
  ## Returns `t` if it's not 'nil' - the ``void`` type otherwise
  if t != nil: t
  else:        g.getSysType(unknownLineInfo, tyVoid)

proc typeOrVoid(c: TCtx, t: PType): PType {.inline.} =
  ## Returns `t` if it's not 'nil' - the ``void`` type otherwise
  # TODO: cache the void type
  typeOrVoid(c.graph, t)

func nextLabel(c: var TCtx): LabelId =
  result = LabelId(c.numLabels)
  inc c.numLabels

func isPure(tree: MirTree, n: NodePosition): bool =
  ## Returns whether the expression at `n` is a pure expression.
  case tree[n].kind
  of mnkParam:
    # sink parameters are mutable and thus not pure
    tree[n].typ.kind != tySink
  of mnkTemp, mnkConst, mnkLiteral, mnkProc, mnkType:
    # note: during the translation phase, the name of a temporary is a pure
    # expression
    true
  of mnkLocal:
    # let bindings are pure, but only if they don't have a destructor (in
    # which case they're movable)
    tree[n].sym.kind in {skLet, skForVar} and not hasDestructor(tree[n].typ)
  of mnkPathNamed, mnkPathPos, mnkPathConv, mnkPathVariant:
    isPure(tree, NodePosition tree.operand(n))
  of mnkPathArray:
    let arr = NodePosition tree.operand(n, 0)
    case tree[arr].typ.skipTypes(abstractVar).kind
    of tyArray, tyUncheckedArray, tyString, tySequence, tyOpenArray, tyVarargs,
       tyCstring:
      # pure when the both the array and index operands are pure
      isPure(tree, tree.child(n, 1)) and isPure(tree, arr)
    else:
      unreachable(tree[arr].kind)
  else:
    # TODO: make exhaustive
    false

func isStable(tree: MirTree, n: NodePosition): bool =
  ## Returns whether the run-time address of the lvalue expression `n` is
  ## always the same, regardless of when the address is computed.
  case tree[n].kind
  of mnkConst, mnkGlobal, mnkParam, mnkLocal, mnkTemp, mnkAlias:
    true
  of mnkPathArray:
    let arr = NodePosition tree.operand(n, 0)
    case tree[arr].typ.skipTypes(abstractVar).kind
    of tyArray, tyUncheckedArray:
      # static arrays
      isPure(tree, tree.child(n, 1)) and isStable(tree, arr)
    of tyString, tySequence, tyOpenArray, tyVarargs, tyCstring:
      # run-time arrays; stable when neither the array nor index operand
      # depends on mutable state
      isPure(tree, tree.child(n, 1)) and isPure(tree, arr)
    else:
      unreachable()
  of mnkPathNamed, mnkPathPos, mnkPathConv, mnkPathVariant:
    isStable(tree, NodePosition tree.operand(n))
  of mnkDeref, mnkDerefView:
    # a pure target means that the pointer is always the same
    isPure(tree, NodePosition tree.operand(n))
  of AllNodeKinds - LvalueExprKinds:
    unreachable(tree[n].kind)

# ----------- SourceProvider API -------------

template useSource(bu: var MirBuilder, sp: var SourceProvider,
                   origin: PNode) =
  ## Pushes `origin` to be used as the source for the rest of the scope that
  ## ``useSource`` is used inside. When the scope is exited, the previous
  ## origin is restored.
  let x = origin
  var prev =
    if sp.active.n != x: (x, sp.map.add(x))
    else:                (x, sp.active.id)

  # set the new source information on the builder and make it the
  # active one:
  bu.setSource(prev[1])
  swap(prev, sp.active)

  defer:
    # switch back to the previous source information:
    bu.setSource(prev[1])
    swap(prev, sp.active)

# -------------- builder/convenience routines -------------

template add(c: var TCtx, n: MirNode) =
  c.builder.add n

template subTree(c: var TCtx, k: MirNodeKind, body: untyped) =
  c.builder.subTree MirNode(kind: k):
    body

template subTree(c: var TCtx, n: MirNode, body: untyped) =
  c.builder.subTree n:
    body

template scope(c: var TCtx, body: untyped) =
  c.builder.subTree mnkScope:
    body

template use(c: var TCtx, val: Value) =
  c.builder.use(val)

template emitByVal(c: var TCtx, val: Value) =
  ## Emits a pass-by-value argument sub-tree with `val`.
  c.builder.emitByVal(val)

template emitByName(c: var TCtx, eff: EffectKind, body: untyped) =
  ## Emits a pass-by-name argument sub-tree with `val`.
  c.subTree mnkName:
    c.subTree MirNode(kind: mnkTag, effect: eff):
      body

proc empty(c: var TCtx, n: PNode): MirNode =
  MirNode(kind: mnkNone, typ: n.typ)

func intLiteral(val: Int128, typ: PType): Value =
  literal(newIntTypeNode(val, typ))

func nameNode(c: var TCtx, s: PSym): MirNode =
  case s.kind
  of skTemp:
    # temporaries are always locals, even if marked with the ``sfGlobal``
    # flag
    MirNode(kind: mnkLocal, typ: s.typ, sym: s)
  of skConst:
    MirNode(kind: mnkConst, typ: s.typ, cnst: c.env.constants.add(s))
  of skParam:
    MirNode(kind: mnkParam, typ: s.typ, sym: s)
  of skResult:
    MirNode(kind: mnkLocal, typ: s.typ, sym: s)
  of skVar, skLet, skForVar:
    if sfGlobal in s.flags:
      MirNode(kind: mnkGlobal, typ: s.typ, global: c.env.globals.add(s))
    else:
      MirNode(kind: mnkLocal, typ: s.typ, sym: s)
  else:
    unreachable(s.kind)

func genLocation(c: var TCtx, n: PNode): Value =
  let f = c.builder.push: c.builder.add(nameNode(c, n.sym))
  c.builder.popSingle(f)

template allocTemp(c: var TCtx, typ: PType; alias=false): Value =
  ## Allocates a new ID for a temporary and returns the name.
  c.builder.allocTemp(typ, alias)

proc gen(c: var TCtx; n: PNode)
proc genx(c: var TCtx; n: PNode, consume: bool = false)
proc genComplexExpr(c: var TCtx, n: PNode, dest: Destination)

proc genAsgn(c: var TCtx, dest: Destination, rhs: PNode)
proc genWithDest(c: var TCtx; n: PNode; dest: Destination)

func getTemp(c: var TCtx, typ: PType): Value =
  ## Allocates a new temporary and emits a definition for it into the
  ## final buffer.
  assert typ != nil
  result = c.allocTemp(typ)
  withFront c.builder:
    c.subTree mnkDef:
      c.use result
      c.add MirNode(kind: mnkNone)

template buildStmt(c: var TCtx, k: MirNodeKind, body: untyped) =
  c.builder.buildStmt(k, body)

template buildMagicCall(c: var TCtx, m: TMagic, t: PType, body: untyped) =
  c.builder.buildMagicCall(m, t, body)

template buildCheckedMagicCall(c: var TCtx, m: TMagic, t: PType,
                               body: untyped) =
  c.subTree MirNode(kind: mnkCheckedCall, typ: t):
    c.add MirNode(kind: mnkMagic, magic: m)
    body

template buildDefectMagicCall(c: var TCtx, m: TMagic, t: PType,
                              body: untyped) =
  ## Builds and emits a call to the `m` magic with return type `t`. The call
  ## is only marked as potentially raising if panics are not enabled.
  ##
  ## This template is meant to be used for ``Defect``-raising magic
  ## procedures.
  let kind =
    if optPanics in c.graph.config.globalOptions:
      mnkCall
    else:
      mnkCheckedCall

  c.subTree MirNode(kind: kind, typ: t):
    c.add MirNode(kind: mnkMagic, magic: m)
    body

func detectKind(tree: MirTree, n: NodePosition, sink: bool): ExprKind =
  ## Detects the kind of expression `n` (with the originating from AST `e`)
  ## represents. `sink` informs whether expression is used in a sink context.
  case tree[n].kind
  of mnkCall, mnkCheckedCall:
    if hasDestructor(tree[n].typ):
      OwnedRvalue
    else:
      Rvalue
  of mnkConv, mnkStdConv, mnkCast, mnkAddr, mnkView, mnkToSlice:
    Rvalue
  of mnkObjConstr:
    if tree[n].typ.skipTypes(abstractInst).kind == tyRef or
       (sink and hasDestructor(tree[n].typ)):
      # the result of ref constructions always needs to be destroyed
      OwnedRvalue
    else:
      Rvalue
  of mnkConstr:
    # XXX: sequence constructions (i.e., constant ``seq``s) are allowed to be
    #      lifted into constants at a later stage, which needs to be accounted
    #      here by treating the values as non-owning. Performing all lifting-
    #      into-constants here in ``mirgen`` is going to render this special-
    #      casing obsolete
    if sink and hasDestructor(tree[n].typ) and
       tree[n].typ.skipTypes(abstractInst).kind != tySequence:
      OwnedRvalue
    else:
      Rvalue
  of LvalueExprKinds:
    Lvalue
  of mnkLiteral, mnkProc, mnkType:
    Literal
  of AllNodeKinds - ExprKinds:
    unreachable(tree[n].kind)

func captureInTemp(c: var TCtx, f: Fragment, sink: bool): Value =
  ## Pops the expression `f` from the staging buffer and wraps it in a new
  ## temporary. `sink` signals whether the temporary is intended for use
  ## in a sink context.
  let def =
    if sink or detectKind(c.builder.staging, f.pos, sink) == OwnedRvalue:
      mnkDef
    else:
      mnkDefCursor

  result = c.allocTemp(f.typ)
  withFront c.builder:
    c.subTree def:
      c.use result
      c.builder.pop(f)

func captureName(c: var TCtx, f: Fragment, mutable: bool): Value =
  ## Pops the expression `f` from the staging buffer and assigns it to an
  ## alias. The alias is returned.
  let res = c.allocTemp(f.typ, alias=true)
  withFront c.builder:
    c.subTree (if mutable: mnkBindMut else: mnkBind):
      c.add res.node
      c.builder.pop(f)
  res

proc genUse(c: var TCtx, n: PNode): Value =
  ## Generates the MIR tree for expression `n`. If the expression is an
  ## atom, the atom is returned, otherwise the value is captured in a
  ## temporary and the name of the temporary is returned.
  c.builder.useSource(c.sp, n)
  # emit the expression into the staging buffer:
  let f = c.builder.push: genx(c, n)

  if c.builder.staging[f.pos].kind in Atoms:
    result = c.builder.popSingle(f)
  else:
    result = captureInTemp(c, f, sink=false)

proc genRd(c: var TCtx, n: PNode; consume=false): Value =
  ## Generates the MIR code for the expression `n`. Makes sure that the run-
  ## time value of the expression is *captured* by assigning it to a
  ## temporary.
  c.builder.useSource(c.sp, n)
  let f = c.builder.push: genx(c, n)

  if c.builder.staging[f.pos].kind in Atoms and
     isPure(c.builder.staging, f.pos):
    result = c.builder.popSingle(f)
  else:
    # either an rvalue or some non-pure lvalue -> capture
    result = captureInTemp(c, f, consume)

proc genAlias(c: var TCtx, n: PNode, mutable: bool): Value =
  ## `mutable` indicates whether the alias needs to support direct assignments
  ## through it.
  let f = c.builder.push: genx(c, n)
  if c.builder.staging[f.pos].kind in Atoms:
    # names are always stable, so no capture is needed
    c.builder.popSingle(f)
  else:
    captureName(c, f, mutable)

proc capture(c: var TCtx, n: PNode; sink=false): Value =
  ## If not a stable lvalue expression, captures the result of the
  ## expression `n` in a temporary.
  ## * rvalue expression are captured in temporaries (with `sink` indicating
  ##   whether an owned value is preferred)
  ## * lvalue expressions are captured as non-assignable aliases
  ## * literals are not captured
  const Skip = abstractInstTypeClass + {tyVar}
  let f = c.builder.push: genx(c, n)
  case detectKind(c.builder.staging, f.pos, sink)
  of Rvalue, OwnedRvalue:
    captureInTemp(c, f, sink)
  of Literal:
    c.builder.popSingle(f)
  of Lvalue:
    if c.builder.staging[f.pos].kind in Atoms:
      # atoms are always stable and can thus be used directly
      c.builder.popSingle(f)
    elif n.typ.skipTypes(Skip).kind in {tyOpenArray, tyVarargs}:
      # don't create an alias for openArrays, a copy of the view suffices
      captureInTemp(c, f, sink=false)
    else:
      # if used in a sink context, the alias must support potentially
      # destructive moves through it, which requires a ``BindMut``
      captureName(c, f, mutable=sink)

proc genOperand(c: var TCtx, n: PNode; sink = false) =
  ## Translates expression AST `n` to a MIR operand expression. `sink` signals
  ## whether the result is used in a sink context -- the flag is propagated
  ## through conversions such that in ``A(B(C(x: ...)))`` the object
  ## construction produces an owning value.
  c.builder.useSource(c.sp, n)
  let f = c.builder.push: genx(c, n, sink)
  case detectKind(c.builder.staging, f.pos, sink)
  of OwnedRvalue, Rvalue:
    let tmp = captureInTemp(c, f, sink)
    c.use tmp
  of Lvalue, Literal:
    # nothing to do, the nodes are already in the staging buffer
    # note: literals are not valid in a context where an lvalue is required
    #       (e.g., as the operand to an ``addr`` operation)
    discard f

proc genOp(c: var TCtx, k: MirNodeKind, t: PType, n: PNode) =
  assert t != nil
  c.subTree MirNode(kind: k, typ: t):
    genOperand(c, n)

template buildOp(c: var TCtx, k: MirNodeKind, t: PType, body: untyped) =
  assert t != nil
  c.subTree MirNode(kind: k, typ: t):
    body

template wrapTemp(c: var TCtx, t: PType, body: untyped): Value =
  ## Assigns the expression emitted by `body` to a temporary and
  ## returns the name of the latter.
  assert t != nil
  let res = c.allocTemp(t)
  c.buildStmt mnkDef:
    c.use res
    body

  res

template wrapAndUse(c: var TCtx, t: PType, body: untyped) =
  ## Assigns the expression emitted by `body` to a temporary
  ## and immediately emits a use thereof.
  let tmp = c.wrapTemp(t):
    body
  c.use tmp

template buildTree(c: var TCtx, k: MirNodeKind, t: PType, body: untyped) =
  c.subTree MirNode(kind: k, typ: t):
    body

proc genAndOr(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates the code for an ``and|or`` operation:
  ##
  ## .. code-block:: nim
  ##
  ##   dest = a
  ##
  ##   # for `or`:
  ##   if not dest:
  ##     dest = b
  ##
  ##   # for `and`:
  ##   if dest:
  ##     dest = b
  ##
  # TODO: inefficient code is generated for nested ``and|or`` operations, e.g.
  #       ``a or b or c``. Sequences of the same operation should be merged
  #       into a single one before and the logic here adjusted to handle them.
  #       With the aforementioned transformation, the previously mentioned
  #       example would become: ``or(a, b, c)``
  genAsgn(c, dest, n[1]) # the left-hand side

  # condition:
  var v = dest.val
  if n[0].sym.magic == mOr:
    v = c.wrapTemp n.typ:
      c.buildMagicCall mNot, n.typ:
        c.emitByVal v

  c.subTree mnkIf:
    c.use v
    c.subTree mnkStmtList:
      genAsgn(c, dest, n[2]) # the right-hand side

proc genBracketExpr(c: var TCtx, n: PNode) =
  let typ = n[0].typ.skipTypes(abstractInstTypeClass - {tyTypeDesc})
  case typ.kind
  of tyTuple:
    let i = n[1].intVal.int
    # due to all the type-related modifications done by ``transf``, it's safer
    # to lookup query the type from the tuple instead of using `n.typ`
    c.subTree MirNode(kind: mnkPathPos, typ: typ[i], position: i.uint32):
      genOperand(c, n[0])
  of tyArray, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray, tyString,
     tyCstring:
    if optBoundsCheck in c.userOptions and
       needsIndexCheck(c.graph.config, n[0], n[1]):
      let
        arr = capture(c, n[0])
        idx = genRd(c, n[1])

      c.buildStmt mnkVoid:
        c.buildDefectMagicCall mChckIndex, typeOrVoid(c, nil):
          c.emitByVal arr
          c.emitByVal idx

      c.buildOp mnkPathArray, elemType(typ):
        c.use arr
        c.use idx
    else:
      c.buildOp mnkPathArray, elemType(typ):
        genOperand(c, n[0])
        c.use genRd(c, n[1])
  else: unreachable()

proc genFieldCheck(c: var TCtx, access: Value, call: PNode, inverted: bool,
                   field: string) =
  ## Generates and emits a field check.
  let
    conf = c.graph.config
    discr = call[2].sym
  c.buildStmt mnkVoid:
    c.buildDefectMagicCall mChckField, typeOrVoid(c, nil):
      # set operand:
      c.emitByVal c.genRd(call[1])
      # discriminator value operand:
      c.subTree mnkArg:
        c.subTree MirNode(kind: mnkPathNamed, typ: discr.typ,
                          field: discr):
          c.use access
      # inverted flag:
      c.emitByVal literal(newIntTypeNode(ord(inverted), call.typ))
      # error message operand:
      c.emitByVal literal(newStrNode(genFieldDefect(conf, field, discr),
                                     call.info))

proc genVariantAccess(c: var TCtx, n: PNode) =
  ## Generates and emits the MIR code for an object variant access, but
  ## **without** the final field access. That is, for `x.b` - where `b` is part
  ## of a variant - the `.b` part is **not** emitted. If field checks are
  ## enabled, they are emitted too.
  ##
  ## There'S no notion of "access variant" in the AST while in the MIR there
  ## is. This requires first materializing each variant access.
  assert n.kind == nkCheckedFieldExpr

  let access = n[0]
  assert access.kind == nkDotExpr

  var x = capture(c, access[0])
  # iterate the checks in reverse, as the outermost discriminator check is
  # rightmost
  # XXX: the "rightmost" part is an implementation detail of how
  #      ``nkCheckedFieldExpr`` nodes are generated by ``sem``,
  #      depending on it here is brittle
  for i in countdown(n.len - 1, 1):
    let check = n[i]
    assert check.kind in nkCallKinds
    let
      inverted = check[0].sym.magic == mNot
      call =
        if inverted: check[1]
        else:        check
      discr = call[2].sym ## the discriminator's symbol

    if optFieldCheck in c.userOptions:
      genFieldCheck(c, x, call, inverted, access[1].sym.name.s)

    if i == 1:
      # last access in the chain
      c.subTree MirNode(kind: mnkPathVariant, typ: x.typ, field: discr):
        c.use x
    else:
      # capture the access in an alias, so that it can be used for the
      # following check
      withFront c.builder:
        x = c.builder.wrapAlias x.typ:
          c.subTree MirNode(kind: mnkPathVariant, typ: x.typ, field: discr):
            c.use x

proc genObjConv(c: var TCtx, n: PNode, sink, dest: bool) =
  ## Given an ``nkObjDownConv`` or ``nkObjUpConv`` AST, folding away all
  ## immediately nested up- and down-conversion nodes. That is, only one
  ## ``mnkPathConv`` is emitted.
  ##
  ## If conversion checks are enabled and required, at most one check is
  ## emitted. The type used for the check is either the type most nested in
  ## the hierarchy, or a sibling type (if a sibling conversion is present).
  let
    skipped = n.typ.skipTypes(abstractInst)
    # only ref and ptr types are checked during conversions, normal objects
    # are not
    needsCheck = (optObjCheck in c.userOptions) and
                 (skipped.kind in {tyPtr, tyRef}) and
                 not isObjLackingTypeField(skipped.lastSon)

  # find the first non-conversion node:
  var start {.cursor.} = n
  while start.kind in {nkObjDownConv, nkObjUpConv}:
    start = start[0]

  var deepest = start.typ.skipTypes(skipPtrs)
    ## the type most nested in the type hierarchy that's certain to be valid.
    ## If a sibling conversion exists in the chain, this is the first
    ## encountered sibling type
  if needsCheck:
    # walk the conversions again and look for the type deepest in the
    # hierarchy
    var x {.cursor.} = n
    while x.kind in {nkObjDownConv, nkObjUpConv}:
      let typ = x.typ.skipTypes(skipPtrs)
      if (let rel = inheritanceDiff(typ, deepest); rel > 0):
        deepest = typ
        if rel == high(typeof(rel)):
          # whatever other types there are, a sibling conversion will always
          # result in a run-time error
          break

      x = x[0]

  if needsCheck and deepest != start.typ.skipTypes(skipPtrs):
    # we do need a check, but only for the most deeply nested type that is
    # converted to
    let val =
      if dest:
        genAlias(c, start, mutable=true)
      elif sink:
        capture(c, start, true)
      else:
        # if the result neither needs to be moved from nor assigned to, a
        # cursor is enough
        let f = c.builder.push: genx(c, start, false)
        captureInTemp(c, f, false)

    let boolType = c.graph.getSysType(n.info, tyBool)

    c.buildStmt mnkIf:
      # the ``x != nil`` condtion:
      c.wrapAndUse(boolType):
        c.buildMagicCall mNot, boolType:
          c.subTree mnkArg:
            c.wrapAndUse(boolType):
              c.buildMagicCall mIsNil, boolType:
                c.emitByVal val
      # the check:
      c.subTree mnkVoid:
        c.buildDefectMagicCall mChckObj, typeOrVoid(c, nil):
          c.emitByVal val
          c.emitByVal typeLit(deepest)

    c.buildOp mnkPathConv, n.typ:
      c.use val
  else:
    # skip all down- and up-conversion, the final conversion is all that
    # matters
    c.buildOp mnkPathConv, n.typ:
      genOperand(c, start)

proc genTypeExpr(c: var TCtx, n: PNode): Value =
  ## Generates the code for an expression that yields a type. These are only
  ## valid in metaprogramming contexts. If it's a static type expression, we
  ## evaluate it directly and store the result as a type literal in the MIR
  assert n.typ.kind == tyTypeDesc
  c.builder.useSource(c.sp, n)
  case n.kind
  of nkStmtListType, nkStmtListExpr:
    # FIXME: a ``nkStmtListExpr`` shouldn't reach here, but it does. See
    #        ``tests/lang_callable/generics/t18859.nim`` for a case where it
    #        does
    if n[^1].typ.kind == tyTypeDesc:
      genTypeExpr(c, n.lastSon)
    else:
      # HACK: this is big hack. Consider the following case:
      #
      #       .. code-block:: nim
      #
      #         type Obj[T] = object
      #           p: T not nil
      #
      #         var x: Obj[ref int]
      #
      #       The ``T not nil`` is a ``nkStmtListType`` node with a single
      #       ``nkInfix`` sub-node, where the latter doesn't use a
      #       ``tyTypeDesc`` type but a ``tyRef`` instead. For now, we work
      #       around this by using the type of the ``nkStmtListType``
      typeLit(n.typ)
  of nkBlockType:
    genTypeExpr(c, n.lastSon)
  of nkSym:
    case n.sym.kind
    of skType:
      typeLit(n.sym.typ)
    of skVar, skLet, skForVar, skTemp, skParam:
      # a first-class type value stored in a location
      genLocation(c, n)
    else:
      unreachable()
  of nkBracketExpr:
    # the type description of a generic type, e.g. ``seq[int]``
    typeLit(n.typ)
  of nkTupleTy, nkStaticTy, nkRefTy, nkPtrTy, nkVarTy, nkDistinctTy, nkProcTy,
     nkIteratorTy, nkSharedTy, nkTupleConstr:
    typeLit(n.typ)
  of nkTypeOfExpr, nkType:
    typeLit(n.typ)
  else:
    unreachable("not a type expression")

proc genArgExpression(c: var TCtx, n: PNode, sink: bool) =
  ## Generates and emits the code for an expression appearing in a call or
  ## construction argument position.
  c.builder.useSource(c.sp, n)

  let f = c.builder.push: genx(c, n, consume = sink)
  let needsTemp =
    if sink:
      # only literals can be passed directly to sink parameters
      c.builder.staging[f.pos].kind notin {mnkProc, mnkLiteral}
    else:
      # all non-pure expressions need to be captured
      not isPure(c.builder.staging, f.pos)

  if needsTemp:
    c.use captureInTemp(c, f, sink)
  else:
    discard f

proc emitOperandTree(c: var TCtx, n: PNode, sink: bool) =
  ## Generates and emits the MIR tree for a call or construction argument.
  c.subTree (if sink: mnkConsume else: mnkArg):
    genArgExpression(c, n, sink)

proc genLvalueOperand(c: var TCtx, n: PNode; mutable = true) =
  ## Generates the code for lvalue expression `n`. If the expression is either
  ## not pure or has side-effects, its address/name is captured, with
  ## `mutable` denoting whether the address is going to be used for mutation
  ## of the underlying location.
  let
    n = if n.kind == nkHiddenAddr: n[0] else: n
    f = c.builder.push:
      if n.kind in {nkObjDownConv, nkObjUpConv}:
        # XXX: super hacky, but currently required so that object conversions
        #      pick the correct capture method when used as lvalue operands
        genObjConv(c, n, sink=false, mutable)
      else:
        genx(c, n)

  if isStable(c.builder.staging, f.pos):
    # the expression can be used directly. We're already in a staging
    # buffer context, so do nothing
    discard f
  else:
    # capture the address via an alias
    c.use captureName(c, f, mutable)

proc genCallee(c: var TCtx, n: PNode) =
  ## Generates and emits the code for a callee expression.
  if n.kind == nkSym and n.sym.kind in routineKinds:
    c.builder.useSource(c.sp, n)
    let s = n.sym
    if s.magic == mNone or s.magic in c.config.magicsToKeep:
      # reference the procedure by symbol
      c.use toValue(c.env.procedures.add(s), s.typ)
    else:
      # don't use a symbol
      c.add MirNode(kind: mnkMagic, magic: s.magic)
  else:
    # an indirect call
    genArgExpression(c, n, false)

proc genArg(c: var TCtx, formal: PType, n: PNode) =
  ## Generates and emits the MIR code for an argument expression, with the
  ## MIR expression being wrapped in the correct argument node. The `formal`
  ## type is needed for figuring out how the argument is passed.
  case formal.skipTypes(abstractRange-{tySink}).kind
  of tyVar:
    if formal.base.kind in {tyOpenArray, tyVarargs}:
      # it's not a pass-by-name parameter
      c.emitOperandTree n, false
    else:
      c.emitByName ekMutate, genLvalueOperand(c, n, true)
  of tySink:
    c.emitOperandTree n, true
  else:
    c.emitOperandTree n, false

proc genArgs(c: var TCtx, n: PNode) =
  ## Emits the MIR code for the argument expressions (including the
  ## argument node), but without a wrapping ``mnkArgBlock``.
  let fntyp = skipTypes(n[0].typ, abstractInst)

  for i in 1..<n.len:
    # for procedures with unsafe varargs, the type of the argument expression
    # is used as the formal type (because it's the only type-related
    # information about the argument we have access to here)
    let t =
      if i < fntyp.len: fntyp[i]
      else:             n[i].typ

    if t.kind == tyTypeDesc and goGenTypeExpr in c.config.options:
      # generation of type expressions is requested. It's important that this
      # branch comes before the ``isCompileTimeOnly`` one, as a ``tyTypeDesc``
      # is treated as a compile-time-only type and would be omitted then
      # FIXME: some argument expressions seem to reach here incorrectly
      #        typed (i.e., not as a typedesc). Figure out why, resolve
      #        the issues, and then remove the workaround here
      if n[i].typ.kind == tyTypeDesc:
        c.emitByVal genTypeExpr(c, n[i])
      else:
        c.emitByVal typeLit(n[i].typ)
    elif t.isCompileTimeOnly:
      # don't translate arguments to compile-time-only parameters. To ease the
      # translation to ``CgNode``, we don't omit them completely but only
      # replace them with a node holding their type
      c.subTree mnkArg:
        c.add empty(c, n[i])
    elif t.kind == tyVoid:
      # a ``void`` argument. We can't just generate an ``mnkNone`` node, as the
      # statement used as the argument can still have side-effects
      withFront c.builder:
        gen(c, n[i])
      c.subTree mnkArg:
        c.add empty(c, n[i])
    elif i == 1 and not fntyp[0].isEmptyType() and
         not isHandleLike(t) and
         classifyBackendView(fntyp[0]) != bvcNone:
      # the procedure returns a view, but the first parameter is not something
      # that resembles a handle. We need to make sure that the first argument
      # (which the view could be created from), is passed by reference
      c.subTree mnkName:
        let x = c.builder.push: genx(c, n[i])
        case detectKind(c.builder.staging, x.pos, false)
        of OwnedRvalue, Rvalue, Literal:
          c.use captureInTemp(c, x, false)
        of Lvalue:
          if isStable(c.builder.staging, x.pos):
            discard x
          else:
            c.use captureName(c, x, false)

    else:
      genArg(c, t, n[i])

proc genCall(c: var TCtx, n: PNode) =
  ## Generates and emits the MIR code for a call expression.
  let fntyp = n[0].typ.skipTypes(abstractInst)
  let kind: range[mnkCall..mnkCheckedCall] =
    if canRaise(optPanics in c.graph.config.globalOptions, n[0]):
      mnkCheckedCall
    else:
      mnkCall

  var effects: set[GeneralEffect]
  if tfNoSideEffect notin fntyp.flags:
    effects.incl geMutateGlobal

  c.subTree MirNode(kind: kind, typ: typeOrVoid(c, fntyp[0]),
                    effects: effects):
    genCallee(c, n[0])
    genArgs(c, n)

proc genMacroCallArgs(c: var TCtx, n: PNode, kind: TSymKind, fntyp: PType) =
  ## Generates the arguments for a macro/template call expression. `n` is
  ## expected to be a ``getAst`` expression that has been transformed to the
  ## internal representation. `kind` is the meta-routine's kind, and `fntyp`
  ## its signature.
  case kind
  of skMacro:
    genCallee(c, n[1])
  of skTemplate:
    # for late template invocations, the callee template is an argument
    c.subTree mnkArg:
      genCallee(c, n[1])
  else:
    unreachable(kind)

  for i in 2..<n.len:
    let
      it = n[i]
      argTyp = it.typ.skipTypes(abstractInst - {tyTypeDesc})

    if argTyp.kind == tyTypeDesc:
      # the expression is a type expression, explicitly handle it there so that
      # ``genx`` doesn't have to
      c.emitByVal genTypeExpr(c, it)
    elif kind == skMacro:
      # we can extract the formal types from the signature
      genArg(c, fntyp[i - 1], it)
    elif kind == skTemplate:
      # we have to treat the arguments as normal expressions
      c.emitByVal genRd(c, it)
    else:
      unreachable()

proc genInSetOp(c: var TCtx, n: PNode) =
  ## Generates and emits the IR for the ``mInSet`` magic call `n`. If
  ## the element operand is a range check, it is integrated into the
  ## operation, meaning that no defect will be raised if the operand is
  ## not in the expected range.
  case n[2].kind
  of nkChckRange, nkChckRange64:
    # turn
    #   chkRange(a, b, c) in d
    # into
    #   b <= a and a <= c and a in d
    # but make sure that 'd' is still always evaluated
    let
      se = n[1]
      x  = n[2]
      elemTyp = x.typ.skipTypes(abstractRange)
      leOp = getMagicLeForType(elemTyp) # less-equal op
      res = getTemp(c, n.typ) # the temporary to write the result to

    # the evaluation order is reversed here: the second operand comes
    # first
    let
      val = genRd(c, x[0])
      a   = genRd(c, x[1])
      b   = genRd(c, x[2])

    c.buildStmt mnkIf:
      # condition: ``a <= x:``
      c.wrapAndUse(n.typ):
        c.buildMagicCall leOp, n.typ:
          c.emitByVal a
          c.emitByVal val
      # the outer body:
      c.subTree mnkStmtList:
        # condition: ``x <= b:``
        c.subTree mnkIf:
          c.wrapAndUse(n.typ):
            c.buildMagicCall leOp, n.typ:
              c.emitByVal val
              c.emitByVal b
          c.subTree mnkStmtList:
            var sv: Value
            if se.kind == nkCurly:
              sv = c.allocTemp(se.typ)
              c.subTree mnkDef:
                c.use sv
                c.subTree MirNode(kind: mnkConstr, typ: se.typ):
                  for it in se.items:
                    c.emitOperandTree it, false
            else:
              sv = genRd(c, se)

            c.subTree mnkInit:
              c.use res
              c.buildMagicCall mInSet, n.typ:
                c.emitByVal sv
                c.emitByVal val

    c.use res
  else:
    # the operation is not eligible for being turned into an ``if`` chain. Emit a
    # generic magic call
    genCall(c, n)

proc genMagic(c: var TCtx, n: PNode; m: TMagic) =
  ## Generates the MIR code for the magic call expression/statement `n`. `m` is
  ## the magic's enum value and must match with that of the callee.
  ##
  ## Some magics are inserted by the compiler, in which case the corresponding
  ## symbols are incomplete: only the ``magic`` and ``name`` field can be
  ## treated as valid. These magic calls are manually translated and don't go
  ## through ``genCall``
  c.builder.useSource(c.sp, n)

  template arg(n: PNode) =
    c.emitOperandTree n, false

  case m
  of mAnd, mOr:
    let tmp = getTemp(c, n.typ)
    withFront c.builder:
      genAndOr(c, n, Destination(isSome: true, val: tmp))
    c.use tmp
  of mDefault:
    # use the canonical form:
    c.buildMagicCall mDefault, n.typ:
      discard
  of mNew:
    # ``new`` has 2 variants. The standard one with zero arguments, and the
    # unsafe version that takes a ``size`` argument
    assert n.len == 1 or n.len == 2
    c.buildMagicCall m, typeOrVoid(c, n.typ):
      if n.len == 2:
        # the size argument
        arg n[1]

  of mWasMoved:
    # ``wasMoved`` has an effect that is not encoded by the parameter's type
    # (it kills the location), so we need to manually translate it
    c.buildMagicCall m, typeOrVoid(c, n.typ):
      c.emitByName ekKill, genLvalueOperand(c, n[1])
  of mConStrStr:
    # the `mConStrStr` magic is very special. Nested calls to it are flattened
    # into a single call in ``transf``. It can't be passed on to ``genCall``
    # since the number of arguments doesn't match with the number of parameters
    c.buildMagicCall m, n.typ:
      for i in 1..<n.len:
        arg n[i]
  of mInSet:
    genInSetOp(c, n)
  of mEcho:
    # forward the wrapped arguments to the call; don't emit the intermediate array
    let x = n[1].skipConv
    assert x.kind == nkBracket
    c.buildCheckedMagicCall m, typeOrVoid(c, n.typ):
      # for the convenience of later transformations, the type of the would-be
      # array is passed along as the first argument
      if x.len > 0:
        c.emitByVal typeLit(x.typ)
      for it in x.items:
        arg it
  of mAccessEnv:
    c.subTree MirNode(kind: mnkPathPos, typ: n.typ, position: 1):
      genOperand(c, n[1])
  of mOffsetOf:
    # an offsetOf call that has to be evaluated by the backend
    c.buildMagicCall mOffsetOf, n.typ:
      c.subTree mnkName:
        # prevent all checks and make sure that the original lvalue
        # expression reaches the code generators
        # XXX: this is a brittle and problematic hack. The type plus field
        #      index should be passed as the arguments instead
        let orig = c.userOptions
        c.userOptions = {}
        genx(c, n[1])
        c.userOptions = orig
  of mSlice:
    # XXX: a HiddenStdConv erroneously ends up in the array position
    #      sometimes, which would, if kept, lead to index errors when the
    #      array doesn't start at index 0
    let arr = skipConv(n[1])
    if optBoundsCheck in c.userOptions and needsBoundCheck(arr, n[2], n[3]):
      let
        arr = capture(c, arr)
        lo = genRd(c, n[2])
        hi = genRd(c, n[3])
      c.buildStmt mnkVoid:
        c.buildDefectMagicCall mChckBounds, typeOrVoid(c, nil):
          c.emitByVal arr
          c.emitByVal lo
          c.emitByVal hi

      c.buildTree mnkToSlice, n.typ:
        c.use arr
        c.use lo
        c.use hi
    else:
      c.buildTree mnkToSlice, n.typ:
        genOperand(c, arr)
        genArgExpression(c, n[2], sink=false)
        genArgExpression(c, n[3], sink=false)

  # arithmetic operations:
  of mInc, mDec:
    # ``inc a, b`` -> ``a = a + b``
    let
      typ = n[1].typ
      dest = genAlias(c, n[1], true)
    c.buildStmt mnkAsgn:
      c.use dest
      if isUnsigned(typ):
        const magic = [mInc: mAddU, mDec: mSubU]
        c.buildMagicCall magic[m], typ:
          c.emitByVal dest
          arg n[2]
      else:
        const magic = [mInc: mAddI, mDec: mSubI]
        if optRangeCheck in c.userOptions and
           typ.skipTypes(abstractInst).kind in {tyRange, tyEnum}:
          # needs an additional range check in order to ensure that the value
          # is in range
          let val = c.wrapTemp(typ):
            c.buildMagicCall magic[m], typ:
              c.emitByVal dest
              arg n[2]
          c.buildDefectMagicCall mChckRange, typ:
            c.emitByVal val
            c.emitByVal intLiteral(firstOrd(c.graph.config, typ), typ)
            c.emitByVal intLiteral(lastOrd(c.graph.config, typ), typ)
        else:
          # no range check is needed
          c.buildMagicCall magic[m], n[1].typ:
            c.emitByVal dest
            arg n[2]

  # magics that use incomplete symbols (most of them are generated by
  # ``liftdestructors``):
  of mDestroy:
    # ``mDestroy`` magic calls might be incomplete symbols, so we have to
    # translate them manually
    c.buildMagicCall m, typeOrVoid(c, n.typ):
      c.emitByName ekMutate, genLvalueOperand(c, n[1])
  of mNewSeq:
    # XXX: the first parameter is actually an ``out`` parameter -- the
    #      ``ekReassign`` effect could be used
    if n[0].typ == nil:
      c.buildMagicCall m, typeOrVoid(c, n.typ):
        c.emitByName ekMutate, genLvalueOperand(c, n[1])
        arg n[2]
    else:
      genCall(c, n)
  of mSetLengthStr, mCopyInternal:
    if n[0].typ == nil:
      c.buildMagicCall m, typeOrVoid(c, n.typ):
        c.emitByName ekMutate, genLvalueOperand(c, n[1])
        arg n[2]
    else:
      genCall(c, n)
  of mNot, mLtI, mSubI, mLengthSeq, mLengthStr, mSamePayload:
    if n[0].typ == nil:
      # simple translation. None of the arguments need to be passed by lvalue
      c.buildMagicCall m, n.typ:
        for i in 1..<n.len:
          arg n[i]

    else:
      genCall(c, n)
  of mAlignOf:
    # instances of the magic inserted by ``liftdestructors`` and ``alignof(x)``
    # calls where ``x`` is of an imported type with unknown alignment reach
    # here. The code-generators only care about the types in both cases, so
    # that's what we emit
    c.buildMagicCall m, n.typ:
      # skip the surrounding typedesc
      c.emitByVal typeLit(n[1].typ.skipTypes({tyTypeDesc}))
  of mGetTypeInfoV2:
    if n[0].typ == nil:
      # the compiler-generated version always uses a type as the argument
      c.buildMagicCall m, n.typ:
        c.emitByVal typeLit(n[1].typ)
    else:
      # only the compiler-generated version of the magic has a type parameter.
      # The normal one doesn't (see ``cyclebreaker.getDynamicTypeInfo``), so we
      # can safely use ``genCall``
      genCall(c, n)

  # special macro related magics:
  of mExpandToAst:
    # the transformation pass already flattened the call expression for us and
    # made it a bit easier to process
    let callee = n[1] # the meta-routine to evaluate
    case callee.sym.kind
    of skTemplate:
      # a ``getAst`` call taking a template call expression. The arguments
      # need special handling, but the shape stays as is
      c.buildMagicCall m, n.typ:
        genMacroCallArgs(c, n, skTemplate, callee.sym.typ)
    of skMacro:
      # rewrite ``getAst(macro(a, b, c))`` -> ``macro(a, b, c)``
      # treat a macro call as potentially raising and as modifying global
      # data. While not wrong, it is pessimistic
      c.subTree MirNode(kind: mnkCheckedCall, typ: n.typ,
                        effects: {geMutateGlobal}):
        # we can use the internal signature
        genMacroCallArgs(c, n, skMacro, callee.sym.internal)
    else:
      unreachable()

  of mSwap:
    # turn calls to magic procedures that don't require symbols into MIR
    # magic calls
    c.buildMagicCall m, n.typ:
      genArgs(c, n)
  else:
    # no special transformation for the other magics:
    genCall(c, n)

proc genCallOrMagic(c: var TCtx, n: PNode) =
  if n[0].kind == nkSym and (let s = n[0].sym; s.magic != mNone):
    genMagic(c, n, s.magic)
  else:
    genCall(c, n)

proc genSetConstr(c: var TCtx, n: PNode) =
  c.buildTree mnkConstr, n.typ:
    for it in n.items:
      c.emitOperandTree it, false

proc genArrayConstr(c: var TCtx, n: PNode, isConsume: bool) =
  c.buildTree mnkConstr, n.typ:
    for it in n.items:
      c.emitOperandTree it, isConsume

proc genTupleConstr(c: var TCtx, n: PNode, isConsume: bool) =
  assert n.typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind == tyTuple
  c.buildTree mnkConstr, n.typ:
    for it in n.items:
      c.emitOperandTree skipColon(it), isConsume

proc genClosureConstr(c: var TCtx, n: PNode, isConsume: bool) =
  c.buildTree mnkConstr, n.typ:
    c.emitOperandTree n[0].skipConv, false # the procedural value
    # transf wraps the procedure operand in a conversion that we don't
    # need

    c.subTree (if isConsume: mnkConsume else: mnkArg): # the environment
      if n[1].kind == nkNilLit:
        # it can happen that a ``nkNilLit`` has no type (i.e. its typ is nil) -
        # we ensure that the nil literal has the correct type
        # TODO: prevent a ``nkNilLit`` with no type information from being
        #       created instead
        c.use literal(newNodeIT(nkNilLit, n[1].info,
                                c.graph.getSysType(n[1].info, tyNil)))
      else:
        genArgExpression(c, n[1], isConsume)

proc genObjConstr(c: var TCtx, n: PNode, isConsume: bool) =
  let isRef = n.typ.skipTypes(abstractInst).kind == tyRef

  c.subTree MirNode(kind: mnkObjConstr, typ: n.typ, len: n.len-1):
    for i in 1..<n.len:
      let it = n[i]
      let field = lookupFieldAgain(n.typ.skipTypes(abstractInst), it[0].sym)
      assert it.kind == nkExprColonExpr

      # only require require a unique value when constructing a ``ref`` and the
      # destination is not a ``.cursor`` field
      let useConsume =
        (isRef or isConsume) and
        sfCursor notin field.flags

      c.add MirNode(kind: mnkField, field: field)
      c.emitOperandTree it[1], useConsume

proc genRaise(c: var TCtx, n: PNode) =
  assert n.kind == nkRaiseStmt
  c.buildStmt mnkRaise:
    if n[0].kind != nkEmpty:
      genArgExpression(c, n[0], sink=true)
    else:
      c.add MirNode(kind: mnkNone)

proc genReturn(c: var TCtx, n: PNode) =
  assert n.kind == nkReturnStmt
  if n[0].kind != nkEmpty:
    gen(c, n[0])

  c.add MirNode(kind: mnkReturn)

proc genAsgnSource(c: var TCtx, e: PNode, sink: bool) =
  ## Generates the MIR code for the right-hand side of an assignment.
  ## The value is captured in a temporary if necessary for proper
  ## destruction.
  c.builder.useSource(c.sp, e)
  let f = c.builder.push: genx(c, e, sink)

  if not sink and
     detectKind(c.builder.staging, f.pos, false) == OwnedRvalue:
    # the expression produces some value that requires ownership being
    # taken of but the receiver doesn't support holding those. Assign the
    # value to an owning temporary (which can be destroyed later) first
    let tmp = c.allocTemp(e.typ)
    withFront c.builder:
      c.subTree mnkDef:
        c.add tmp.node
        c.builder.pop(f)
    c.use tmp

proc genAsgn(c: var TCtx, dest: Destination, rhs: PNode) =
  assert dest.isSome
  let owns = dfOwns in dest.flags
  let kind =
    if owns:
      if dfEmpty in dest.flags: mnkInit
      else:                     mnkAsgn
    else:                       mnkFastAsgn
  c.buildStmt kind:
    c.use dest.val
    c.genAsgnSource(rhs, sink = owns)

proc unwrap(c: var TCtx, n: PNode): PNode =
  ## If `n` is a statement-list expression, generates the code for all
  ## statements and returns the unwrapped expression. The unchanged `n` is
  ## returned otherwise.
  result = n
  if result.kind == nkStmtListExpr:
    withFront c.builder:
      for i in 0..<(result.len-1):
        gen(c, result[i])

    result = result.lastSon
    assert result.kind != nkStmtListExpr

proc genAsgn(c: var TCtx, isFirst, sink: bool, lhs, rhs: PNode) =
  ## Generates the code for an assignment. `isFirst` indicates if this is the
  ## first assignment to the location named by `lhs`.
  ##
  ## If the expression on the right is complex and the location the left-hand
  ## names might be invalidated by the expression on the right, the value
  ## resulting from the expression on the right is first stored in a temporary

  # generate everything part of the left-hand-side that is not the relevant
  # l-value expression:
  let
    lhs = unwrap(c, lhs)
    sink = sink and not isCursor(lhs)

  case rhs.kind
  of ComplexExprs, nkStmtListExpr:
    # optimization: forward the destination. For example:
    #   x = if cond: a else: b
    # becomes:
    #   if cond: x = a
    #   else:    x = b
    let
      f = c.builder.push: genLvalueOperand(c, lhs, true)
      dest =
        if c.builder.staging[f.pos].kind in Atoms:
          c.builder.popSingle(f)
        else:
          captureName(c, f, true)

    genWithDest(c, rhs, initDestination(dest, isFirst, sink))
  else:
    let kind =
      if sink:
        if isFirst: mnkInit
        else:       mnkAsgn
      else:         mnkFastAsgn

    c.buildStmt kind:
      # ``genLvalueOperand`` ensures that unstable lvalue
      # expressions are captured
      genLvalueOperand(c, lhs, true)
      genAsgnSource(c, rhs, sink)

proc genLocDef(c: var TCtx, n: PNode, val: PNode) =
  ## Generates the 'def' construct for the entity provided by the symbol node
  ## `n`
  let s = n.sym

  c.builder.useSource(c.sp, n)
  c.buildStmt selectDefKind(s):
    c.add nameNode(c.env, s)
    # the initializer is optional
    if val.kind != nkEmpty:
      # XXX: some closure types are erroneously reported as having no
      #      destructor, which would lead to memory leaks if the
      #      expression is a closure construction. As a work around,
      #      a missing destructor disables the sink context
      genAsgnSource(c, val, (sfCursor notin s.flags) and hasDestructor(s.typ))
    else:
      c.add MirNode(kind: mnkNone)

proc genLocInit(c: var TCtx, symNode: PNode, initExpr: PNode) =
  ## Generates the code for a location definition. `sym` is the symbol of the
  ## location and `initExpr` the initializer expression
  let
    sym = symNode.sym

  assert sym.kind in {skVar, skLet, skTemp, skForVar}

  if sfCompileTime in sym.flags and goIsCompileTime notin c.config.options:
    # compile-time-only locations don't exist outside of compile-time
    # contexts, so omit their definitions
    return

  genLocDef(c, symNode, initExpr)

proc genVarTuple(c: var TCtx, n: PNode) =
  ## Generates the code for a ``let/var (a, b) = c`` statement
  assert n.kind == nkVarTuple
  c.builder.useSource(c.sp, n)

  let
    numDefs = n.len - 2
    initExpr = n[^1]

  # then, generate the initialization
  case initExpr.kind
  of nkEmpty:
    unreachable("missing initializer")
  of nkPar, nkTupleConstr:
    # skip constructing a temporary and assign directly
    assert numDefs == initExpr.len
    for i in 0..<numDefs:
      let
        lhs = n[i]
        rhs = initExpr[i].skipColon

      case lhs.kind
      of nkSym:     genLocInit(c, lhs, rhs)
      of nkDotExpr: genAsgn(c, true, true, lhs, rhs) # closure field
      else:         unreachable(lhs.kind)

  else:
    # generate the definition for the temporary:
    let val = c.allocTemp(initExpr.typ)
    c.buildStmt mnkDefUnpack:
      c.use val
      genx(c, initExpr, consume = true)

    # generate the unpack logic:
    for i in 0..<numDefs:
      let lhs = n[i]

      if lhs.kind == nkSym:
        genLocDef(c, lhs, c.graph.emptyNode)

      # generate the assignment:
      c.buildStmt mnkInit:
        genOperand(c, lhs)
        c.subTree MirNode(kind: mnkPathPos, typ: lhs.sym.typ,
                          position: i.uint32):
          c.use val

proc genVarSection(c: var TCtx, n: PNode) =
  for a in n:
    case a.kind
    of nkCommentStmt: discard
    of nkVarTuple:
      genVarTuple(c, a)
    of nkIdentDefs:
      c.builder.useSource(c.sp, a)
      case a[0].kind
      of nkSym:
        genLocInit(c, a[0], a[2])
      of nkDotExpr:
        # initialization of a variable that was lifted into a closure
        # environment
        if a[2].kind != nkEmpty:
          genAsgn(c, false, true, a[0], a[2])
        else:
          # no intializer expression -> assign the default value
          c.buildStmt mnkInit:
            genOperand(c, a[0])
            c.buildMagicCall mDefault, a[0].typ:
              discard
      else:
        unreachable()

    else:
      unreachable(a.kind)


proc genWhile(c: var TCtx, n: PNode) =
  ## Generates the code for a ``nkWhile`` node.
  assert isTrue(n[0]), "`n` wasn't properly transformed"
  c.subTree MirNode(kind: mnkRepeat):
    c.scope:
      c.gen(n[1])

proc genBlock(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates and emits the MIR code for a ``block`` expression or statement
  if sfUsed notin n[0].sym.flags:
    # if the label is never used, it means that the block is only used for
    # scoping. Omit emitting an ``mnkBlock`` and just use a scope
    c.scope: c.genWithDest(n[1], dest)
    return

  let id = nextLabel(c)

  # push the block to the stack:
  var oldLen = c.blocks.len
  c.blocks.add Block(label: n[0].sym, id: id)

  # generate the body:
  c.subTree MirNode(kind: mnkBlock, label: id):
    c.scope:
      c.genWithDest(n[1], dest)

  # pop the block:
  assert c.blocks.len == oldLen + 1
  c.blocks.setLen(oldLen)

proc genBranch(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates the body of a branch. Here, a branch refers to either an
  ## ``if|elif|else``, ``of``, or ``except`` clause

  # if the branch ends in a no-return statement, it has no type. We generate a
  # normal statement (without an assignment to `dest`) in that case
  if dest.isSome and not n.typ.isEmptyType():
    genWithDest(c, n, dest)
  else:
    gen(c, n)

proc genIf(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates the code for an ``if`` statement (``nkIf(Stmt|Expr)``). It's
  ## translated to the ``mnkIf`` MIR construct (which can be seen as a
  ## predicate over a region of code).
  ##
  ## The translation works as follows:
  ##
  ## .. code-block:: nim
  ##
  ##   if (let a = x; a == 0):
  ##     stmt
  ##   elif (let b = x; b == 1):
  ##     stmt2
  ##   else:
  ##     stmt3
  ##
  ## is mapped to MIR code that is equivalent to
  ##
  ## .. code-block:: nim
  ##
  ##   block label:
  ##     let a = x
  ##     if a == 0:
  ##       stmt1
  ##       break label
  ##     let b = x
  ##     if x == 1:
  ##       stmt2
  ##       break label
  ##     stmt3
  ##
  let hasValue = not isEmptyType(n.typ)
  assert hasValue == dest.isSome

  template genElifBranch(branch: PNode, extra: untyped) =
    ## Generates the code for a single ``nkElif(Branch|Expr)``
    let v = genUse(c, branch[0])
    c.subTree mnkIf:
      c.use v
      c.scope:
        genBranch(c, branch.lastSon, dest)
        extra

  if n.len == 1:
    # an ``if`` statement/expression with a single branch. Don't emit the
    # unnecessary 'block' and 'break'
    genElifBranch(n[0]):
      discard

  else:
    # a multi-clause ``if`` statement/expression
    let label = nextLabel(c)
    c.subTree MirNode(kind: mnkBlock, label: label):
      c.subTree mnkStmtList:
        for it in n.items:
          case it.kind
          of nkElifBranch, nkElifExpr:
            genElifBranch(it):
              # don't emit the 'break' if the branch doesn't have a structured
              # exit
              if not endsInNoReturn(it.lastSon):
                c.add MirNode(kind: mnkBreak, label: label)

          of nkElse, nkElseExpr:
            c.scope:
              genBranch(c, it[0], dest)

            # since this is the last branch, a 'break' is not needed
          else:
            unreachable(it.kind)

proc genCase(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates the MIR code for an ``nkCaseStmt`` node. Since the ``mnkCase``
  ## MIR construct works in a very similar way, the translation logic is
  ## straightforward
  assert isEmptyType(n.typ) == not dest.isSome

  # the number of processed branches is not necessarily equal to the amount
  # we're going to emit (in case we skip some), so we have to count them
  # manually
  var num = 0
  for (_, branch) in branches(n):
    # an 'of' branch with no labels (e.g. ``of {}:``) is dropped, no code is
    # generated for it
    num += ord(branch.kind != nkOfBranch or branch.len > 1)

  let v = genUse(c, n[0])
  c.add MirNode(kind: mnkCase, len: num)
  c.use v

  # iterate of/else branches:
  for (_, branch) in branches(n):
    if branch.len == 1 and branch.kind == nkOfBranch:
      continue

    c.add MirNode(kind: mnkBranch, len: branch.len - 1)

    case branch.kind
    of nkElse:
      discard
    of nkOfBranch:
      # emit the lables:
      for (_, lit) in branchLabels(branch):
        c.add MirNode(kind: mnkLiteral, lit: lit)
    else:
      unreachable(branch.kind)

    # the branch's body:
    c.scope:
      genBranch(c, branch.lastSon, dest)

    c.add endNode(mnkBranch)
    inc num

  c.add endNode(mnkCase)

proc genExceptBranch(c: var TCtx, n: PNode, dest: Destination) =
  assert n.kind == nkExceptBranch
  c.builder.useSource(c.sp, n)

  c.subTree MirNode(kind: mnkBranch, len: n.len - 1):
    # emit the exception types the branch covers:
    for _, tn in branchLabels(n):
      case tn.kind
      of nkType:
        c.add MirNode(kind: mnkType, typ: tn.typ)
      of nkInfix:
        # ``x as T`` doesn't get transformed to just ``T`` if ``T`` is the
        # type of an imported exception. We don't care about the type of
        # exceptions at the MIR-level, so we just use carry it along as is
        c.add MirNode(kind: mnkPNode, node: tn)
      else:
        unreachable()

    # generate the body of the branch:
    c.scope:
      genBranch(c, n.lastSon, dest)

proc genTry(c: var TCtx, n: PNode, dest: Destination) =
  let
    hasFinally = n.lastSon.kind == nkFinally
    hasExcept = n[1].kind == nkExceptBranch

  c.add MirNode(kind: mnkTry, len: ord(hasFinally) + ord(hasExcept))
  c.scope:
    c.genBranch(n[0], dest)

  let len =
    if hasFinally: n.len-1
    else: n.len
    ## the number of sub-nodes excluding ``nkFinally``

  if hasExcept:
    c.subTree MirNode(kind: mnkExcept, len: len-1):
      for i in 1..<len:
        genExceptBranch(c, n[i], dest)

  if hasFinally:
    c.builder.useSource(c.sp, n.lastSon)
    c.subTree MirNode(kind: mnkFinally):
      c.scope:
        c.gen(n.lastSon[0])

  c.add endNode(mnkTry)

proc genAsmOrEmitStmt(c: var TCtx, kind: range[mnkAsm..mnkEmit], n: PNode) =
  ## Generates and emits the MIR code for an emit directive or ``asm``
  ## statement.
  c.buildStmt kind:
    for it in n.items:
      # both asm and emit statements support arbitrary expressions
      # (including type expressions) ...
      if it.typ != nil and it.typ.kind == tyTypeDesc:
        c.use genTypeExpr(c, it)
      elif it.kind == nkSym and it.sym.kind == skField:
        # emit and asm support using raw symbols. So that we don't
        # have to allow ``skField``s in general, we special case them
        # here (by pushing them through the MIR phase boxed as
        # ``mnkLiteral``s)
        c.add MirNode(kind: mnkLiteral, lit: it, typ: it.sym.typ)
      else:
        # emit and asm statements support lvalue operands
        genOperand(c, it)

proc genComplexExpr(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates and emits the MIR code for assigning the value resulting from
  ## the complex expression `n` to destination `dest`
  assert not n.typ.isEmptyType()
  assert dest.isSome
  c.builder.useSource(c.sp, n)

  case n.kind
  of nkIfExpr:
    genIf(c, n, dest)
  of nkCaseStmt:
    genCase(c, n, dest)
  of nkTryStmt:
    genTry(c, n, dest)
  of nkBlockExpr:
    genBlock(c, n, dest)
  else:
    # not a complex expression
    unreachable(n.kind)

proc genx(c: var TCtx, n: PNode, consume: bool) =
  ## Generate and emits the raw MIR code for the given expression `n` into
  ## the active buffer.
  ##
  ## `consume` signals whether aggregate constructions have to create owned
  ## value (by consuming their elements) or not (by shallow-copying their
  ## element).
  c.builder.useSource(c.sp, n)

  case n.kind
  of nkSym:
    let s = n.sym
    case s.kind
    of skVar, skForVar, skLet, skResult, skParam, skConst, skTemp:
      c.add nameNode(c, s)
    of skProc, skFunc, skConverter, skMethod, skIterator:
      c.use toValue(c.env.procedures.add(s), s.typ)
    else:
      unreachable(s.kind)

  of nkCallKinds:
    genCallOrMagic(c, n)
  of nkCharLit..nkNilLit, nkRange, nkNimNodeLit:
    c.use literal(n)
  of nkCheckedFieldExpr:
    c.subTree MirNode(kind: mnkPathNamed, typ: n.typ, field: n[0][1].sym):
      genVariantAccess(c, n)
  of nkDotExpr:
    let
      typ = n[0].typ.skipTypes(abstractInstTypeClass)
      sym = n[1].sym

    assert sym.kind == skField

    case typ.kind
    of tyObject:
      c.subTree MirNode(kind: mnkPathNamed, typ: n.typ, field: sym):
        genOperand(c, n[0])
    of tyTuple:
      # always use lookup-by-position for tuples, even when they're accessed
      # with via name
      c.subTree MirNode(kind: mnkPathPos, typ: typ[sym.position],
                        position: sym.position.uint32):
        genOperand(c, n[0])
    else:
      unreachable(typ.kind)
  of nkBracketExpr:
    genBracketExpr(c, n)
  of nkObjDownConv, nkObjUpConv:
    genObjConv(c, n, consume, dest=false)
  of nkAddr:
    c.genOp mnkAddr, n.typ, n[0]
  of nkHiddenAddr:
    case classifyBackendView(n.typ)
    of bvcSingle:
      # a view into the source operand is created
      c.genOp mnkView, n.typ, n[0]
    of bvcSequence:
      # the addr operation itself is a no-op, but the operation needs to be
      # re-typed
      let start = c.builder.staging.len
      genx(c, n[0])
      c.builder.staging[start].typ = n.typ
    of bvcNone:
      # a normal address-of operation
      c.genOp mnkAddr, n.typ, n[0]

  of nkDerefExpr:
    # it's not know where the path is going to be used, so the target is
    # always captured when not pure
    c.buildOp mnkDeref, n.typ:
      c.use genRd(c, n[0])
  of nkHiddenDeref:
    case classifyBackendView(n[0].typ)
    of bvcSingle:
      # it's a deref of a view
      c.buildOp mnkDerefView, n.typ:
        c.use genRd(c, n[0])
    of bvcSequence:
      # it's a no-op
      genx(c, n[0])
    of bvcNone:
      # it's a ``ref`` or ``ptr`` deref
      c.buildOp mnkDeref, n.typ:
        c.use genRd(c, n[0])

  of nkHiddenStdConv:
    case n.typ.skipTypes(abstractVar).kind
    of tyOpenArray:
      c.genOp mnkToSlice, n.typ, n[1]
    else:
      c.genOp mnkStdConv, n.typ, n[1]
  of nkHiddenSubConv, nkConv:
    if compareTypes(n.typ, n[1].typ, dcEqIgnoreDistinct, {IgnoreTupleFields}):
      # it's an lvalue-preserving conversion
      c.buildOp mnkPathConv, n.typ:
        genOperand(c, n[1], consume)
    elif n.typ.skipTypes(abstractVar).kind == tyOpenArray:
      # to-openArray conversion also reach here as ``nkHiddenSubConv``
      # sometimes
      c.genOp mnkToSlice, n.typ, n[1]
    else:
      # it's a conversion that produces a new rvalue
      c.genOp mnkConv, n.typ, n[1]
  of nkLambdaKinds:
    let s = n[namePos].sym
    c.use toValue(c.env.procedures.add(s), n.typ)
  of nkChckRangeF, nkChckRange64, nkChckRange:
    # XXX: only produce range-check nodes where range checks should take
    #      place, and then remove the conditional logic here -- ``mirgen``
    #      should only do what it's told to and not make program-semantics-
    #      related descisions on its own
    if optRangeCheck notin c.userOptions or
       skipTypes(n.typ, abstractVar).kind in {tyUInt..tyUInt64}:
      # unsigned types should be range checked, see: https://github.com/nim-works/nimskull/issues/574
      c.genOp mnkConv, n.typ, n[0]
    else:
      c.buildDefectMagicCall mChckRange, n.typ:
        c.emitOperandTree n[0], false
        c.emitOperandTree n[1], false
        c.emitOperandTree n[2], false
  of nkStringToCString:
    c.buildMagicCall mStrToCStr, n.typ:
      c.emitOperandTree n[0], false
  of nkCStringToString:
    c.buildMagicCall mCStrToStr, n.typ:
      c.emitOperandTree n[0], false
  of nkBracket:
    let consume =
      if n.typ.skipTypes(abstractVarRange).kind == tySequence:
        # don't propagate the `consume` flag through sequence constructors.
        # A sequence constructor is only used for constant seqs
        false
      else:
        consume

    genArrayConstr(c, n, consume)
  of nkCurly:
    # a ``set``-constructor never owns
    genSetConstr(c, n)
  of nkObjConstr:
    genObjConstr(c, n, consume)
  of nkTupleConstr:
    genTupleConstr(c, n, consume)
  of nkClosure:
    genClosureConstr(c, n, consume)
  of nkCast:
    c.genOp mnkCast, n.typ, n[1]
  of nkWhenStmt:
    # a ``when nimvm`` expression
    genx(c, selectWhenBranch(n, goIsNimvm in c.config.options), consume)
  of nkPragmaBlock:
    genx(c, n.lastSon, consume)
  of nkStmtListExpr:
    withFront c.builder:
      for i in 0..<n.len-1:
        gen(c, n[i])

    genx(c, n[^1], consume)
  of ComplexExprs:
    # attempting to generate the code for a complex expression without a
    # destination specified -> assign the value resulting from it to a
    # temporary
    let tmp = getTemp(c, n.typ)

    withFront c.builder:
      genComplexExpr(c, n):
        Destination(isSome: true, val: tmp, flags: {dfOwns, dfEmpty})

    c.use tmp
  else:
    unreachable(n.kind)

proc gen(c: var TCtx, n: PNode) =
  ## Generates and emits the MIR code for the statement `n`
  c.builder.useSource(c.sp, n)

  # because of ``.discardable`` calls, we can't require `n` to be of void
  # type
  #assert n.typ.isEmptyType()

  # for the same reason as mentioned above, we also have to allow
  # ``nkIfExpr|nkStmtListExpr`` here
  case n.kind
  of nkStmtList, nkStmtListExpr:
    # statement-list expressions (``nkStmtListExpr``) reach here if they end
    # in a no-return statement
    for it in n.items:
      gen(c, it)

  of nkBlockStmt:
    genBlock(c, n, Destination())
  of nkIfStmt, nkIfExpr:
    genIf(c, n, Destination())
  of nkCaseStmt:
    genCase(c, n, Destination())
  of nkTryStmt:
    genTry(c, n, Destination())
  of nkWhileStmt:
    genWhile(c, n)
  of nkReturnStmt:
    genReturn(c, n)
  of nkRaiseStmt:
    genRaise(c, n)
  of nkPragmaBlock:
    gen(c, n.lastSon)
  of nkBreakStmt:
    var id: LabelId
    block search:
      let sym = n[0].sym
      # find the block with the matching label and use its ``LabelId``:
      for b in c.blocks.items:
        if b.label.id == sym.id:
          id = b.id
          break search

      unreachable "break target missing"

    c.add MirNode(kind: mnkBreak, label: id)
  of nkVarSection, nkLetSection:
    genVarSection(c, n)
  of nkAsgn:
    # get the unwrapped ``nkDotExpr`` on the left (if one exists):
    let x =
      case n[0].kind
      of nkCheckedFieldExpr: n[0][0]
      else: n[0]

    if x.kind == nkDotExpr and sfDiscriminant in x[1].sym.flags:
      # an assignment to a discriminant. In other words: a branch switch (but
      # only if the new value refers to a different branch than the one that's
      # currently active)
      c.buildStmt mnkSwitch:
        # the 'switch' operations expects a variant access as the first
        # operand
        c.subTree MirNode(kind: mnkPathVariant, typ: x[0].typ, field: x[1].sym):
          case n[0].kind
          of nkCheckedFieldExpr:
            # for nested record-cases, the discriminant access is wrapped in a
            # ``nkCheckedFieldExpr``, in which case it needs to be unwrapped
            # first
            genVariantAccess(c, n[0])
          else:
            genOperand(c, x[0])

        genAsgnSource(c, n[1], false) # the source operand
    else:
      # a normal assignment
      genAsgn(c, false, true, n[0], n[1])

  of nkFastAsgn:
    # for non-destructor-using types, ``nkFastAsgn`` means bitwise copy
    # (i.e. ``mnkFastAsgn``), but for types having a destructor attached, it's
    # a normal assignment
    # XXX: this is confusing and unintuitive behaviour. ``transf`` shouldn't
    #      insert ``nkFastAsgn`` as aggresively as it does now and instead
    #      let the move-analyser and cursor-inference take care of optimizing
    #      the copies away
    let sink = hasDestructor(n[0].typ)
    genAsgn(c, false, sink, n[0], n[1])
  of nkCallKinds:
    # calls are expressions, the void statement allows using them as
    # statements. The call might be a magic that expands to a statement,
    # however
    let f = c.builder.push: genCallOrMagic(c, n)
    if f.len > 0:
      withFront c.builder:
        c.subTree mnkVoid:
          c.builder.pop(f)
  of nkDiscardStmt:
    if n[0].kind != nkEmpty:
      let f = c.builder.push: genx(c, n[0])
      case detectKind(c.builder.staging, f.pos, false)
      of Rvalue, OwnedRvalue:
        # extend the lifetime of the value
        # XXX: while not not possible at the moment, in the future, the
        #      discard statement could destroy the temporary right away
        discard captureInTemp(c, f, false)
      of Lvalue:
        c.subTree mnkVoid:
          c.builder.pop(f)
      of Literal:
        # the expression no side-effects nor does it constitute as use
        # of a location, omit
        discard c.builder.popSingle(f)

  of nkNilLit:
    # a 'nil' literals can be used as a statement, in which case it is treated
    # as a ``discard``
    assert n.typ.isEmptyType()
  of routineDefs, nkCommentStmt, nkImportStmt,
     nkImportExceptStmt, nkFromStmt, nkIncludeStmt, nkStaticStmt,
     nkExportStmt, nkExportExceptStmt, nkTypeSection, nkMixinStmt,
     nkBindStmt, nkConstSection, nkEmpty:
    discard "ignore"
  of nkPragma:
    # traverse the pragma statement and look for and extract directives we're
    # interested in. Everything else is discarded
    for it in n:
      case whichPragma(it)
      of wEmit:
        c.builder.useSource(c.sp, it)
        genAsmOrEmitStmt(c, mnkEmit, it[1])
      of wComputedGoto:
        # the MIR doesn't handle this directive, but the code generators
        # might. As such, we need to keep it via a ``mnkPNode``. Since the
        # directive might be combined with some other directive in a
        # single statement, we split it out into a standalone pragma statement
        # first
        # XXX: ideally, sem or transf would split pragma statement up
        c.builder.useSource(c.sp, it)
        c.add MirNode(kind: mnkPNode, node: newTree(nkPragma, [it]))
      else:     discard

  of nkAsmStmt:
    genAsmOrEmitStmt(c, mnkAsm, n)
  of nkWhenStmt:
    # a ``when nimvm`` statement
    gen(c, selectWhenBranch(n, goIsNimvm in c.config.options))
  else:
    unreachable(n.kind)

proc genWithDest(c: var TCtx, n: PNode; dest: Destination) =
  ## Generates and emits the MIR code for an expression plus the code for
  ## assigning the resulting value to the given destination `dest`. `dest` can
  ## be 'none', in which case `n` is required to be a statement
  if dest.isSome:
    assert not endsInNoReturn(n)

    case n.kind
    of ComplexExprs:
      genComplexExpr(c, n, dest)
    of nkStmtListExpr:
      for i in 0..<n.len-1:
        gen(c, n[i])

      genAsgn(c, dest, n[^1])
    else:
      genAsgn(c, dest, n)

  else:
    gen(c, n)

proc generateCode*(graph: ModuleGraph, env: var MirEnv,
                   config: TranslationConfig, n: PNode,
                   builder: var MirBuilder, source: var SourceMap) =
  ## Generates MIR code that is semantically equivalent to the expression or
  ## statement `n`, appending the resulting code and the corresponding origin
  ## information to `code` and `source`, respectively.
  var c = TCtx(context: skUnknown, graph: graph, config: config)

  template swapState() =
    swap(c.sp.map, source)
    swap(c.builder, builder)
    swap(c.env, env)

  # for the duration of ``generateCode`` we move the state into ``TCtx``
  swapState()

  if n.typ.isEmptyType:
    withFront c.builder:
      gen(c, n)
  elif n.typ.kind == tyTypeDesc:
    # FIXME: this shouldn't happen, but type expressions are sometimes
    #        evaluated with the VM, such as a ``typeof(T.x)`` appearing as
    #        a field type within a generic object definition. While it makes
    #        sense to allow evaluating type expression with the VM, in simple
    #        situtations like the example above, it's simpler, faster, and more
    #        intuitive to either evaluate them directly when analying the type
    #        expression or during ``semfold``
    c.builder.useSource(c.sp, n)
    c.use genTypeExpr(c, n)
  else:
    c.builder.useSource(c.sp, n)
    # XXX: restructure the ``mirgen`` API to use a dedicated procedure for
    #      generating expression code
    let v = genUse(c, n)
    c.use v

  # move the state back into the output parameters:
  swapState()

proc generateCode*(graph: ModuleGraph, env: var MirEnv, owner: PSym,
                   config: TranslationConfig,  body: PNode): MirBody =
  ## Generates the full MIR body for the given AST `body`.
  ##
  ## `owner` it the symbol of the entity (module or procedure) that `body`
  ## belongs to. If the owner is a procedure, `body` is expected to be the
  ## full body of the procedure.
  ##
  ## `config` provides additional configuration options that alter how some
  ## AST is translated.
  # XXX: this assertion can currently not be used, as the ``nfTransf`` flag
  #      might no longer be present after the lambdalifting pass
  #assert nfTransf in body.flags, "transformed AST is expected as input"

  var c = TCtx(context: owner.kind, graph: graph, config: config,
               userOptions: owner.options)
  c.sp.active = (body, c.sp.map.add(body))

  swap(c.env, env)

  c.add MirNode(kind: mnkScope)
  if owner.kind in routineKinds:
    # add a 'def' for each ``sink`` parameter. This simplifies further
    # processing and analysis
    let params = owner.typ.n
    for i in 1..<params.len:
      let s = params[i].sym
      if s.typ.isSinkTypeForParam():
        c.subTree mnkDef:
          c.add MirNode(kind: mnkParam, typ: s.typ, sym: s)
          c.add MirNode(kind: mnkNone)

  gen(c, body)
  c.add endNode(mnkScope)

  swap(c.env, env) # swap back

  # move the buffers into the result body
  MirBody(source: move c.sp.map,
          code: finish(move c.builder))
