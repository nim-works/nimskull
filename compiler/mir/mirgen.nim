## Implements the translation from AST to the MIR. The input AST is expected
## to have already been transformed by ``transf``.
##
## How It Works
## ------------
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
## For efficiency, ``MirBuilder``'s double-buffering functionality is used for
## emitting the trees. Statements are directly emitted into the final buffer
## (after all their operands were emitted), while expressions are first
## emitted into the staging buffer, with the callsite then deciding what to
## do with them.
##
## When translating expressions, they're first translated to the `proto-MIR <proto_mir.html>`_,
## and then the proto-MIR expression is translated to the MIR. This allows
## the translation of expressions (besides calls) to focus only on syntax,
## leaving the semantics-related decision-making to the proto-MIR construction.
##
## For arguments, the translation uses temporaries (both owning and non-owning)
## to make sure that the following invariants are true:
## * normal argument expressions are pure (the expression always evaluates to
##   the same value)
## * lvalue argument expressions are stable (the expression always has the
##   same address)
## * sink arguments are always moveable
## * index and dereference targets are always pure
##
## These guarantees make the following analysis and transformation a lot
## easier.
##
## Origin information
## ------------------
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
    astalgo,
    astmsgs, # for generating the field error message
    lineinfos,
    trees,
    types,
    wordrecg
  ],
  compiler/mir/[
    datatables,
    mirbodies,
    mirconstr,
    mirenv,
    mirtrees,
    proto_mir,
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
    localsMap: Table[int, LocalId]
      ## maps symbol IDs of locals to the corresponding ``LocalId``

    sp: SourceProvider

    numLabels: int ## provides the ID to use for the next label
    scopeDepth: int ## the current amount of scope nesting
    inLoop: int
      ## > 0 if the current statement/expression is part of a loop

    # input:
    context: TSymKind ## what entity the input AST is part of (e.g. procedure,
                      ## macro, module, etc.). Used to allow or change how the
                      ## AST is interpreted in some places
    userOptions: set[TOption]
    graph: ModuleGraph

    config: TranslationConfig

  PMirExpr = seq[ProtoItem]
    ## Convenience alias.

const
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

# -------------- Symbol translation --------------

func localToMir(s: PSym): Local =
  Local(typ: s.typ, flags: s.flags,
        isImmutable: s.kind in {skLet, skForVar},
        name: s.name,
        alignment:
          if s.kind in {skVar, skLet, skForVar}:
            s.alignment.uint32
          else:
            0
        )

template paramToMir(s: PSym): Local =
  localToMir(s)

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
  inc c.scopeDepth
  c.builder.subTree mnkScope:
    body
  dec c.scopeDepth

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

template addLocal(c: var TCtx, local: Local): LocalId =
  c.builder.addLocal(local)

func addLocal(c: var TCtx, s: PSym): LocalId =
  ## Translates `s` to its MIR representation, registers it with body, and
  ## establishes a mapping.
  assert s.id notin c.localsMap
  result = c.addLocal(localToMir(s))
  c.localsMap[s.id] = result

proc empty(c: var TCtx, n: PNode): MirNode =
  MirNode(kind: mnkNone, typ: n.typ)

func intLiteral(env: var MirEnv, val: BiggestInt, typ: PType): Value =
  literal(mnkIntLit, env.getOrIncl(val), typ)

func uintLiteral(env: var MirEnv, val: BiggestUInt, typ: PType): Value =
  literal(mnkUIntLit, env.getOrIncl(val), typ)

func floatLiteral(env: var MirEnv, val: BiggestFloat, typ: PType): Value =
  literal(mnkFloatLit, env.getOrIncl(val), typ)

func astLiteral(env: var MirEnv, val: PNode, typ: PType): Value =
  literal(env.asts.add(val), typ)

proc toIntLiteral(env: var MirEnv, val: Int128, typ: PType): Value =
  ## Interprets `val` based on `typ`.
  if isUnsigned(typ):
    uintLiteral(env, val.toUInt, typ)
  else:
    intLiteral(env, val.toInt, typ)

proc toIntLiteral(env: var MirEnv, n: PNode): Value =
  ## Translates an integer value (represented by `n`) to its MIR
  ## counterpart.
  assert n.kind in nkIntLiterals
  # use the type for deciding what whether it's a signed or unsigned value
  case n.typ.skipTypes(abstractRange + {tyEnum}).kind
  of tyInt..tyInt64, tyBool:
    intLiteral(env, n.intVal, n.typ)
  of tyUInt..tyUInt64, tyChar, tyPtr, tyPointer, tyProc:
    uintLiteral(env, cast[BiggestUInt](n.intVal), n.typ)
  else:
    unreachable()

proc toFloatLiteral(env: var MirEnv, n: PNode): Value =
  ## Translates a float value (represented by `n`) to its MIR
  ## counterpart.
  assert n.kind in nkFloatLiterals
  var val = n.floatVal
  case n.typ.skipTypes(abstractRange).kind
  of tyFloat, tyFloat64:
    discard "nothing to adjust"
  of tyFloat32:
    # all code-generators would have to narrow the value at some point, so we
    # help them by doing it here
    val = val.float32.float64
  else:
    unreachable()

  floatLiteral(env, val, n.typ)

func strLiteral(env: var MirEnv, str: string, typ: PType): Value =
  literal(env.getOrIncl(str), typ)

func nameNode(c: var TCtx, s: PSym): MirNode =
  case s.kind
  of skTemp:
    # temporaries are always locals, even if marked with the ``sfGlobal``
    # flag
    MirNode(kind: mnkLocal, typ: s.typ, local: c.localsMap[s.id])
  of skConst:
    MirNode(kind: mnkConst, typ: s.typ, cnst: c.env.constants.add(s))
  of skParam:
    MirNode(kind: mnkParam, typ: s.typ, local: LocalId(1 + s.position))
  of skResult:
    MirNode(kind: mnkLocal, typ: s.typ, local: resultId)
  of skVar, skLet, skForVar:
    if sfGlobal in s.flags:
      MirNode(kind: mnkGlobal, typ: s.typ, global: c.env.globals.add(s))
    else:
      MirNode(kind: mnkLocal, typ: s.typ, local: c.localsMap[s.id])
  else:
    unreachable(s.kind)

func genLocation(c: var TCtx, n: PNode): Value =
  let f = c.builder.push: c.builder.add(nameNode(c, n.sym))
  c.builder.popSingle(f)

template allocTemp(c: var TCtx, typ: PType; alias=false): Value =
  ## Allocates a new ID for a temporary and returns the name.
  c.builder.allocTemp(typ, alias)

proc gen(c: var TCtx; n: PNode)
proc genx(c: var TCtx; e: PMirExpr, i: int)
proc genComplexExpr(c: var TCtx, n: PNode, dest: Destination)

proc genAsgn(c: var TCtx, dest: Destination, rhs: PNode)
proc genWithDest(c: var TCtx; n: PNode; dest: Destination)

proc exprToPmir(c: var TCtx, n: PNode, sink, mutable: bool): PMirExpr =
  exprToPmir(c.userOptions, c.graph.config, goIsNimvm in c.config.options,
             n, sink, mutable)

proc genx(c: var TCtx, n: PNode; consume: bool = false) =
  let e = exprToPmir(c, n, consume, false)
  genx(c, e, e.high)

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

proc singleToValue(c: var TCtx, e: PMirExpr, i: int): Value =
  c.builder.useSource(c.sp, e[i].orig)
  let f = c.builder.push: genx(c, e, i)
  result = c.builder.popSingle(f)

proc toValue(c: var TCtx, e: PMirExpr, i: int, def: MirNodeKind): Value =
  ## Generates the MIR code for the given expression and turns it into a
  ## ``Value``, using a `def` statement for creating the necessary
  ## temporary.
  c.builder.useSource(c.sp, e[i].orig)
  let f = c.builder.push: genx(c, e, i)
  if c.builder.staging[f.pos].kind in Atoms:
    # can be turned into a ``Value`` directly
    result = c.builder.popSingle(f)
  else:
    # needs a temporary
    result = c.allocTemp(e[i].typ, def in {mnkBind, mnkBindMut})
    withFront c.builder:
      c.subTree def:
        c.use result
        c.builder.pop(f)

proc toValue(c: var TCtx, e: PMirExpr, i: int): Value =
  ## Generates the MIR code for the given expression and turns it into a
  ## ``Value``.
  case classify(e, i)
  of Lvalue:
    case e[i].keep
    of kDontCare:  toValue(c, e, i, mnkDefCursor)
    of kLvalue:    toValue(c, e, i, mnkBind)
    of kMutLvalue: toValue(c, e, i, mnkBindMut)
  of Rvalue:       toValue(c, e, i, mnkDefCursor)
  of OwnedRvalue:  toValue(c, e, i, mnkDef)
  of Literal:      singleToValue(c, e, i)

proc genUse(c: var TCtx, n: PNode): Value =
  ## Generates the MIR code for expression `n` and returns it as a ``Value``.
  ## The expression is not guaranteed to be pure.
  var e = exprToPmir(c, n, false, false)
  toValue(c, e, e.high)

proc genRd(c: var TCtx, n: PNode): Value =
  ## Generates the MIR code for expression `n` and returns it as a pure
  ## ``Value``.
  var e = exprToPmir(c, n, false, false)
  wantPure(e)
  toValue(c, e, e.high)

proc genAlias(c: var TCtx, n: PNode, mutable: bool): Value =
  ## Generates the MIR code for lvalue expression `n`, and creates an alias
  ## (run-time reference) for it. `mutable` indicates whether the alias
  ## needs to support direct assignments through it.
  var e = exprToPmir(c, n, false, mutable)
  toValue(c, e, e.high)

proc genOperand(c: var TCtx, n: PNode) =
  ## Generates and emits the MIR code for expression `n`, using temporaries to
  ## make sure the emitted MIR expression is valid in an operand position.
  var e = exprToPmir(c, n, false, false)
  wantValue(e)
  genx(c, e, e.high)

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
                          field: discr.position.int32):
          c.use access
      # inverted flag:
      c.emitByVal intLiteral(c.env, ord(inverted), call.typ)
      # error message operand:
      c.emitByVal strLiteral(c.env, genFieldDefect(conf, field, discr),
                             c.graph.getSysType(call.info, tyString))

proc genCheckedVariantAccess(c: var TCtx, variant: Value, name: PIdent,
                             check: PNode): PSym =
  ## Generates and emits the field check. `variant` is the variant-object
  ## value the discriminator field is part of, `name` is the name to
  ## put into the error message, and `check` is the check-AST coming from an
  ## ``nkCheckedFieldExpr`` expression.
  ## The symbol of the discriminator field, as taken from `check`, is
  ## returned.
  assert check.kind in nkCallKinds
  let
    inverted = check[0].sym.magic == mNot
    call =
      if inverted: check[1]
      else:        check

  genFieldCheck(c, variant, call, inverted, name.s)
  result = call[2].sym

proc genTypeExpr(c: var TCtx, n: PNode): Value =
  ## Generates the code for an expression that yields a type. These are only
  ## valid in metaprogramming contexts. If it's a static type expression, we
  ## evaluate it directly and store the result as a type literal in the MIR
  assert n.typ.kind == tyTypeDesc
  c.builder.useSource(c.sp, n)
  case n.kind
  of nkStmtListExpr:
    # FIXME: a ``nkStmtListExpr`` shouldn't reach here, but it does. See
    #        ``tests/lang_callable/generics/t18859.nim`` for a case where it
    #        does
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
  var e = exprToPmir(c, n, sink, false)

  if sink:
    wantConsumeable(e)
  else:
    wantValue(e)
    wantPure(e)

  genx(c, e, e.high)

proc emitOperandTree(c: var TCtx, n: PNode, sink: bool) =
  ## Generates and emits the MIR tree for a call or construction argument.
  c.subTree (if sink: mnkConsume else: mnkArg):
    genArgExpression(c, n, sink)

proc genLvalueOperand(c: var TCtx, n: PNode; mutable = true) =
  ## Generates the code for lvalue expression `n`. If the expression is either
  ## not pure or has side-effects, its address/name is captured, with
  ## `mutable` denoting whether the address is going to be used for mutation
  ## of the underlying location.
  let n = if n.kind == nkHiddenAddr: n[0] else: n
  var e = exprToPmir(c, n, false, mutable)
  wantStable(e)
  genx(c, e, e.high)

proc genCallee(c: var TCtx, n: PNode) =
  ## Generates and emits the code for a callee expression.
  if n.kind == nkSym and n.sym.kind in routineKinds:
    c.builder.useSource(c.sp, n)
    let s = n.sym
    if s.magic == mNone or s.magic in c.config.magicsToKeep:
      # reference the procedure by symbol
      c.add procNode(c.env.procedures.add(s))
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
        var e = exprToPmir(c, n[i], false, false)
        wantStable(e)
        genx(c, e, e.high)

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
    c.emitByVal astLiteral(c.env, n[1], n[1].typ)
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

proc genSetConstr(c: var TCtx, n: PNode)

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
            if se.kind == nkCurly and not isDeepConstExpr(se):
              sv = c.allocTemp(se.typ)
              c.subTree mnkDef:
                c.use sv
                genSetConstr(c, se)
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
      genAndOr(c, n, Destination(isSome: true, val: tmp, flags: {dfOwns}))
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

  # arithmetic operations:
  of mAddI, mSubI, mMulI, mDivI, mModI, mPred, mSucc:
    # the `pred` and `succ` magic are lowered to a normal subtraction and
    # addition, respectively. Depending on whether overflow checks are
    # enabled, either magics or the dedicated MIR operators are used
    if optOverflowCheck in c.userOptions:
      const Map = [mAddI: mAddI, mSubI, mMulI, mDivI, mModI,
                   mSucc: mAddI, mPred: mSubI]
      c.buildDefectMagicCall Map[m], n.typ:
        arg n[1]
        arg n[2]
    else:
      const Map = [mAddI: mnkAdd, mSubI: mnkSub,
                   mMulI: mnkMul, mDivI: mnkDiv, mModI: mnkModI,
                   mSucc: mnkAdd, mPred: mnkSub]
      c.buildTree Map[m], n.typ:
        genArgExpression(c, n[1], sink=false)
        genArgExpression(c, n[2], sink=false)

  of mUnaryMinusI, mUnaryMinusI64:
    # negation can cause overflows too
    if optOverflowCheck in c.userOptions:
      c.buildDefectMagicCall m, n.typ:
        arg n[1]
    else:
      c.genOp(mnkNeg, n.typ, n[1])

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
        proc op(c: var TCtx, dest: Value, n: PNode, m: TMagic) =
          if optOverflowCheck in c.userOptions:
            const magic = [mInc: mAddI, mDec: mSubI]
            # use a magic call that can potentially raise
            c.buildDefectMagicCall magic[m], dest.typ:
              c.emitByVal dest
              arg n[2]
          else:
            const kind = [mInc: mnkAdd, mDec: mnkSub]
            # the unchecked arithmetic operators can be used directly
            c.buildTree kind[m], dest.typ:
              c.use dest
              genArgExpression(c, n[2], sink=false)

        if optRangeCheck in c.userOptions and
           typ.skipTypes(abstractInst).kind in {tyRange, tyEnum}:
          # needs an additional range check in order to ensure that the value
          # is in range
          let val = c.wrapTemp(typ): op(c, dest, n, m)
          c.buildDefectMagicCall mChckRange, typ:
            c.emitByVal val
            c.emitByVal toIntLiteral(c.env, firstOrd(c.graph.config, typ), typ)
            c.emitByVal toIntLiteral(c.env, lastOrd(c.graph.config, typ), typ)
        else:
          # no range check is needed
          op(c, dest, n, m)
  of mAbsI:
    # special handling for the ``abs`` magic: if overflow checks are enabled
    # and panics are disabled, the call must be a checked call
    if optOverflowCheck in n[0].sym.options and
       optPanics notin c.graph.config.globalOptions:
      c.buildTree mnkCheckedCall, n.typ:
        c.genCallee(n[0])
        arg n[1]
    else:
      genCall(c, n)

  # float arithmetic operations:
  of mAddF64, mSubF64, mMulF64, mDivF64:
    proc op(c: var TCtx, m: TMagic, a, b: PNode) =
      if optInfCheck in c.userOptions:
        # needs an overflow check
        c.buildDefectMagicCall m, n.typ:
          arg a
          arg b
      else:
        # the unchecked version can be used
        const Map = [mAddF64: mnkAdd, mSubF64: mnkSub,
                     mMulF64: mnkMul, mDivF64: mnkDiv]
        c.buildTree Map[m], n.typ:
          c.genArgExpression(a, sink=false)
          c.genArgExpression(b, sink=false)

    if optNaNCheck in c.userOptions:
      let tmp = c.wrapTemp n.typ:
        op(c, m, n[1], n[2])

      c.buildStmt mnkVoid:
        c.buildDefectMagicCall mChckNaN, typeOrVoid(c, nil):
          c.emitByVal tmp
      c.use tmp
    else:
      op(c, m, n[1], n[2])
  of mUnaryMinusF64:
    c.genOp mnkNeg, n.typ, n[1]

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
  of mNot, mLtI, mLengthSeq, mLengthStr, mSamePayload:
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
  c.buildTree mnkSetConstr, n.typ:
    for it in n.items:
      if it.kind == nkRange:
        # watch out! the operands don't have to be literal values
        c.subTree mnkRange:
          c.genArgExpression(it[0], sink=false)
          c.genArgExpression(it[1], sink=false)
      else:
        c.genArgExpression(it, sink=false)

proc genArrayConstr(c: var TCtx, n: PNode, isConsume: bool) =
  c.buildTree mnkArrayConstr, n.typ:
    for it in n.items:
      c.emitOperandTree it, isConsume

proc genSeqConstr(c: var TCtx, n: PNode) =
  c.buildTree mnkSeqConstr, n.typ:
    for it in n.items:
      c.emitOperandTree it, true

proc genTupleConstr(c: var TCtx, n: PNode, isConsume: bool) =
  assert n.typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind == tyTuple
  c.buildTree mnkTupleConstr, n.typ:
    for it in n.items:
      c.emitOperandTree skipColon(it), isConsume

proc genClosureConstr(c: var TCtx, n: PNode, isConsume: bool) =
  c.buildTree mnkClosureConstr, n.typ:
    c.emitOperandTree n[0].skipConv, false # the procedural value
    # transf wraps the procedure operand in a conversion that we don't
    # need

    c.subTree (if isConsume: mnkConsume else: mnkArg): # the environment
      if n[1].kind == nkNilLit:
        # it can happen that a ``nkNilLit`` has no type (i.e. its typ is nil) -
        # we ensure that the nil literal has the correct type
        # TODO: prevent a ``nkNilLit`` with no type information from being
        #       created instead
        c.add MirNode(kind: mnkNilLit,
                      typ: c.graph.getSysType(n[1].info, tyNil))
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

      c.add MirNode(kind: mnkField, field: field.position.int32)
      c.emitOperandTree it[1], useConsume

proc genRaise(c: var TCtx, n: PNode) =
  assert n.kind == nkRaiseStmt
  if n[0].kind != nkEmpty:
    # the raise operand slot is a sink context, and it behaves much like a
    # ``sink`` parameter
    var e = exprToPmir(c, n[0], true, false)
    wantConsumeable(e)
    let tmp = toValue(c, e, e.high)

    # emit the preparation code:
    let
      typ = skipTypes(n[0].typ, abstractPtrs)
      cp = c.graph.getCompilerProc("prepareException")
    c.buildStmt mnkVoid:
      c.buildTree mnkCall, typeOrVoid(c, nil):
        c.add procNode(c.env.procedures.add(cp))
        c.subTree mnkArg:
          # lvalue conversion to the base ``Exception`` type:
          c.buildTree mnkPathConv, cp.typ[1]:
            c.use tmp
        c.emitByVal strLiteral(c.env, typ.sym.name.s,
                               c.graph.getSysType(n.info, tyCstring))

    # emit the raise statement:
    c.buildStmt mnkRaise:
      c.use tmp
  else:
    # a re-raise statement
    c.buildStmt mnkRaise:
      c.add MirNode(kind: mnkNone)

proc genReturn(c: var TCtx, n: PNode) =
  assert n.kind == nkReturnStmt
  if n[0].kind != nkEmpty:
    gen(c, n[0])

  c.add MirNode(kind: mnkReturn)

proc genAsgnSource(c: var TCtx, e: PNode, status: set[DestFlag]) =
  ## Generates the MIR code for the right-hand side of an assignment.
  ## `status` provides the information necessary to decide what assignment
  ## modifiers to use and whether a temporary is required.
  ##
  ## If not an initial assignment, and lifetime hooks are present, a temporary
  ## is introduced for rvalue expressions that return owning values:
  ##
  ##   def _1 = get()
  ##   dest = move _1
  ##
  ## This is necessary for the later hook injection, which triggers on
  ## assignment modifiers, to work.
  var e = exprToPmir(c, e, dfOwns in status, false)
  if dfOwns in status:
    wantOwning(e, dfEmpty notin status and hasDestructor(e.typ))
  else:
    wantShallow(e)

  genx(c, e, e.high)

proc genAsgn(c: var TCtx, dest: Destination, rhs: PNode) =
  assert dest.isSome
  let kind =
    if dfEmpty in dest.flags: mnkInit
    else:                     mnkAsgn
  c.buildStmt kind:
    c.use dest.val
    c.genAsgnSource(rhs, dest.flags)

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
  of ComplexExprs:
    # optimization: forward the destination. For example:
    #   x = if cond: a else: b
    # becomes:
    #   if cond: x = a
    #   else:    x = b
    let dest = genAlias(c, lhs, true)
    genWithDest(c, rhs, initDestination(dest, isFirst, sink))
  else:
    let kind =
      if isFirst: mnkInit
      else:       mnkAsgn

    var status: set[DestFlag]
    if sink:
      status.incl dfOwns
    if isFirst:
      status.incl dfEmpty

    c.buildStmt kind:
      # ``genLvalueOperand`` ensures that unstable lvalue
      # expressions are captured
      genLvalueOperand(c, lhs, true)
      genAsgnSource(c, rhs, status)

proc genLocDef(c: var TCtx, n: PNode, val: PNode) =
  ## Generates the 'def' construct for the entity provided by the symbol node
  ## `n`
  let
    s = n.sym
    hasInitializer = val.kind != nkEmpty
    sink = sfCursor notin s.flags
    kind = symbolToPmir(s)

  c.builder.useSource(c.sp, n)
  if kind == pirGlobal and c.scopeDepth == 1:
    # no 'def' statement is emitted for top-level globals
    if hasInitializer:
      genAsgn(c, true, sink, n, val)
    elif {sfImportc, sfNoInit} * s.flags == {} and
         {exfDynamicLib, exfNoDecl} * s.extFlags == {}:
      # XXX: ^^ re-think this condition from first principles. Right now,
      #      it's just meant to make some tests work
      # the location doesn't have an explicit starting value. Initialize
      # it to the type's default value.
      c.buildStmt mnkInit:
        c.add nameNode(c, s)
        c.buildMagicCall mDefault, s.typ:
          discard
    else:
      # the definition doesn't imply default intialization
      discard
  else:
    if kind == pirLocal:
      # translate the symbol of the local:
      discard c.addLocal(s)

    c.buildStmt (if sfCursor in s.flags: mnkDefCursor else: mnkDef):
      c.add nameNode(c, s)
      if hasInitializer:
        genAsgnSource(c, val):
          if sink: {dfEmpty, dfOwns}
          else:    {dfEmpty}
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
    isInit = c.inLoop == 0
      ## for lifted locals, whether an 'init' assignment can be used

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
      of nkDotExpr: genAsgn(c, isInit, true, lhs, rhs) # closure field
      else:         unreachable(lhs.kind)

  else:
    # generate the definition for the temporary:
    let val = c.allocTemp(initExpr.typ)
    c.buildStmt mnkDefUnpack:
      c.use val
      # ensure that the temporary owns the tuple value:
      genAsgnSource(c, initExpr, {dfEmpty, dfOwns})

    # generate the unpack logic:
    for i in 0..<numDefs:
      let lhs = n[i]

      if lhs.kind == nkSym:
        genLocDef(c, lhs, c.graph.emptyNode)

      # generate the assignment:
      c.buildStmt (if isInit: mnkInit else: mnkAsgn):
        genOperand(c, lhs)
        # the temporary tuple is ensured to own (see the emission of the
        # definition above), and it's only used for unpacking; it can always be
        # moved out of. The temporary tuple is not destroyed, so no
        # destructive move is required
        c.buildTree mnkMove, lhs.typ:
          c.subTree MirNode(kind: mnkPathPos, typ: lhs.typ,
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
        let isInit = c.inLoop == 0
        if a[2].kind != nkEmpty:
          genAsgn(c, isInit, true, a[0], a[2])
        elif isInit or not hasDestructor(a[0].typ):
          # the default value can be assigned in-place
          c.buildStmt mnkInit:
            genOperand(c, a[0])
            c.buildMagicCall mDefault, a[0].typ:
              discard
        else:
          # a 'move' modifier is required for the assignment to later be
          # rewritten
          c.buildStmt mnkAsgn:
            genOperand(c, a[0])
            c.buildTree mnkMove, a[0].typ:
              c.wrapAndUse a[0].typ:
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
      inc c.inLoop
      c.gen(n[1])
      dec c.inLoop

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
    c.scope:
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

  let v = genUse(c, n[0])
  c.add MirNode(kind: mnkCase, len: n.len - 1)
  c.use v

  # iterate of/else branches:
  for (_, branch) in branches(n):
    c.add MirNode(kind: mnkBranch, len: branch.len - 1)

    case branch.kind
    of nkElse:
      discard
    of nkOfBranch:
      # emit the lables:
      for (_, label) in branchLabels(branch):
        if label.kind == nkRange:
          c.subTree mnkRange:
            genx(c, label[0])
            genx(c, label[1])
        else:
          genx(c, label)
    else:
      unreachable(branch.kind)

    # the branch's body:
    c.scope:
      genBranch(c, branch.lastSon, dest)

    c.add endNode(mnkBranch)

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
        # ``T as a`` doesn't get transformed to just ``T`` if ``T`` is the
        # type of an imported exception -- the local's name is used at the
        # MIR level
        let id = c.addLocal(tn[2].sym)
        c.add MirNode(kind: mnkLocal, typ: tn[2].typ, local: id)
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
        # emit and asm support using raw field symbols. For pushing them
        # through to the code generators, they're quoted (i.e., boxed into
        # an AST literal)
        c.use astLiteral(c.env, it, it.sym.typ)
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

proc constDataToMir*(env: var MirEnv, n: PNode): MirTree

proc toConstant(c: var TCtx, n: PNode): Value =
  ## Creates an anonymous constant from the constant expression `n`
  ## and returns the ``Value`` for it.
  let con = toConstId c.env.data.getOrPut(constDataToMir(c.env, n))
  toValue(con, n.typ)

proc genx(c: var TCtx, e: PMirExpr, i: int) =
  ## Translates the proto-MIR expression to MIR code and emits it into the
  ## current front buffer.
  let n {.cursor.} = e[i]
  c.builder.useSource(c.sp, n.orig)

  template recurse() =
    genx(c, e, i - 1)

  proc viewOp(kind: MirNodeKind, typ: PType): MirNodeKind {.nimcall.} =
    # pick the correct kind based on the var-ness
    let isMutable = typ.skipTypes(abstractInst).kind == tyVar
    case kind
    of mnkView:
      if isMutable: mnkMutView    else: mnkView
    of mnkToSlice:
      if isMutable: mnkToMutSlice else: mnkToSlice
    else: unreachable()

  case n.kind
  of pirProc:
    c.use toValue(c.env.procedures.add(n.sym), n.sym.typ)
  of pirLiteral:
    case n.orig.kind
    of nkNilLit:
      c.add MirNode(kind: mnkNilLit, typ: n.typ)
    of nkIntLiterals:
      c.use toIntLiteral(c.env, n.orig)
    of nkFloatLiterals:
      c.use toFloatLiteral(c.env, n.orig)
    of nkStrLiterals:
      c.use strLiteral(c.env, n.orig.strVal, n.typ)
    of nkNimNodeLit:
      c.use astLiteral(c.env, n.orig[0], n.typ)
    else:
      unreachable(n.orig.kind)
  of pirLocal, pirGlobal, pirParam, pirConst:
    c.add nameNode(c, n.sym)
  of pirDeref:
    c.buildOp mnkDeref, n.typ:
      c.use toValue(c, e, i - 1)
  of pirViewDeref:
    c.buildOp mnkDerefView, n.typ:
      c.use toValue(c, e, i - 1)
  of pirTupleAccess:
    c.subTree MirNode(kind: mnkPathPos, typ: n.typ, position: n.pos):
      recurse()
  of pirFieldAccess:
    c.subTree MirNode(kind: mnkPathNamed, typ: n.typ,
                      field: n.field.position.int32):
      recurse()
  of pirArrayAccess, pirSeqAccess:
    c.buildOp mnkPathArray, n.typ:
      recurse()
      c.use toValue(c, e, n.index)
  of pirVariantAccess:
    c.subTree MirNode(kind: mnkPathVariant, typ: n.typ,
                      field: n.field.position.int32):
      recurse()
  of pirLvalueConv:
    c.buildOp mnkPathConv, n.typ:
      recurse()
  of pirCheckedArrayAccess, pirCheckedSeqAccess:
    let
      arr = toValue(c, e, i - 1)
      idx = toValue(c, e, n.index)

    c.buildStmt mnkVoid:
      c.buildDefectMagicCall mChckIndex, typeOrVoid(c, nil):
        c.emitByVal arr
        c.emitByVal idx

    c.buildOp mnkPathArray, n.typ:
      c.use arr
      c.use idx
  of pirCheckedVariantAccess:
    let
      variant = toValue(c, e, i - 1)
      discr = genCheckedVariantAccess(c, variant, n.orig[0][1].sym.name,
                                      n.orig[n.nodeIndex])
    c.subTree MirNode(kind: mnkPathVariant, typ: n.typ,
                      field: discr.position.int32):
      c.use variant
  of pirCheckedObjConv:
    let
      val = toValue(c, e, i - 1)
      boolType = c.graph.getSysType(n.orig.info, tyBool)

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
          c.emitByVal typeLit(n.check)

    c.buildOp mnkPathConv, n.typ:
      c.use val

  of pirAddr:
    c.buildOp mnkAddr, n.typ:
      recurse()
  of pirView:
    c.buildOp viewOp(mnkView, n.typ), n.typ:
      recurse()
  of pirCast:
    c.buildOp mnkCast, n.typ:
      recurse()
  of pirConv:
    c.buildOp mnkConv, n.typ:
      recurse()
  of pirStdConv:
    c.buildOp mnkStdConv, n.typ:
      recurse()
  of pirToSlice:
    c.buildOp viewOp(mnkToSlice, n.typ), n.typ:
      recurse()
  of pirToSubSlice:
    # the array operand is a PMIR expression already, but the operands
    # specifying the bounds are not
    let
      op = viewOp(mnkToSlice, n.typ)
      a = n.orig[2]
      b = n.orig[3]
    if optBoundsCheck in c.userOptions and needsBoundCheck(n.orig[1], a, b):
      let
        arr = toValue(c, e, i - 1)
        lo = genRd(c, a)
        hi = genRd(c, b)
      c.buildStmt mnkVoid:
        c.buildDefectMagicCall mChckBounds, typeOrVoid(c, nil):
          c.emitByVal arr
          c.emitByVal lo
          c.emitByVal hi

      c.buildTree op, n.typ:
        c.use arr
        c.use lo
        c.use hi
    else:
      c.buildTree op, n.typ:
        recurse()
        genArgExpression(c, a, sink=false)
        genArgExpression(c, b, sink=false)
  of pirCall:
    genCallOrMagic(c, n.orig)
  of pirChckRange:
    c.buildDefectMagicCall mChckRange, n.typ:
      c.emitOperandTree n.orig[0], false
      c.emitOperandTree n.orig[1], false
      c.emitOperandTree n.orig[2], false
  of pirStringToCString:
    c.buildMagicCall mStrToCStr, n.typ:
      c.emitOperandTree n.orig[0], false
  of pirCStringToString:
    c.buildMagicCall mCStrToStr, n.typ:
      c.emitOperandTree n.orig[0], false
  of pirArrayConstr:
    genArrayConstr(c, n.orig, n.owning)
  of pirSeqConstr:
    genSeqConstr(c, n.orig)
  of pirSetConstr:
    genSetConstr(c, n.orig)
  of pirRefConstr:
    genObjConstr(c, n.orig, true)
  of pirObjConstr:
    genObjConstr(c, n.orig, n.owning)
  of pirTupleConstr:
    genTupleConstr(c, n.orig, n.owning)
  of pirClosureConstr:
    genClosureConstr(c, n.orig, n.owning)
  of pirConstExpr:
    # lift a constant from the expression and emit a use of the constant
    c.use toConstant(c, n.orig)
  of pirStmtList:
    let orig = n.orig
    assert orig.kind == nkStmtListExpr
    withFront c.builder:
      for i in 0..<orig.len-1:
        gen(c, orig[i])

    recurse()
  of pirComplex:
    # attempting to generate the code for a complex expression without a
    # destination specified -> assign the value resulting from it to a
    # temporary
    let tmp = getTemp(c, n.typ)

    withFront c.builder:
      genComplexExpr(c, n.orig):
        Destination(isSome: true, val: tmp, flags: {dfOwns, dfEmpty})

    c.use tmp
  of pirCopy:
    c.buildOp mnkCopy, n.typ:
      recurse()
  of pirMove:
    c.buildOp mnkMove, n.typ:
      recurse()
  of pirSink, pirDestructiveMove:
    # a destructive move is currently not translated into a move + wasMoved,
    # but rather into a sink, which is then, if necessary, later turned into
    # a destructive move
    c.buildOp mnkSink, n.typ:
      recurse()
  of pirMat, pirMatCursor:
    let f = c.builder.push: recurse()
    # only materialize a temporary if the expression is not already a
    # temporary introduced by the PMIR-to-MIR translation
    if c.builder.staging[f.pos].kind != mnkTemp:
      let tmp = c.allocTemp(n.typ)
      withFront c.builder:
        c.subTree (if n.kind == pirMat: mnkDef else: mnkDefCursor):
          c.use tmp
          c.builder.pop(f)
      c.use tmp
  of pirMatLvalue:
    let tmp = c.allocTemp(n.typ, true)
    # make sure to create an alias that supports assignment, if requested
    c.buildStmt (if e[i-1].keep == kMutLvalue: mnkBindMut else: mnkBind):
      c.use tmp
      recurse()
    c.use tmp

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
    if isDiscriminantField(n[0]):
      # an assignment to a discriminant. In other words: a branch switch (but
      # only if the new value refers to a different branch than the one that's
      # currently active)
      let dest = exprToPmir(c, n[0], false, true)
      c.buildStmt mnkSwitch:
        # the 'switch' operations expects a variant access as the first
        # operand
        c.subTree MirNode(kind: mnkPathVariant, typ: dest[^2].typ,
                          field: dest[^1].field.position.int32):
          genx(c, dest, dest.len - 2)

        genAsgnSource(c, n[1], {dfOwns}) # the source operand
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
      let e = exprToPmir(c, unwrap(c, n[0]), false, false)
      case classify(e)
      of Rvalue:
        discard toValue(c, e, e.high, mnkDefCursor)
      of OwnedRvalue:
        # extend the lifetime of the value
        # XXX: while not possible at the moment, in the future, the
        #      discard statement could destroy the temporary right away
        discard toValue(c, e, e.high, mnkDef)
      of Lvalue:
        c.buildStmt mnkVoid:
          genx(c, e, e.high)
      of Literal:
        # the expression has no side-effects nor does it constitute as use
        # of a location; drop it
        discard

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

proc generateAssignment*(graph: ModuleGraph, env: var MirEnv,
                   config: TranslationConfig, n: PNode,
                   builder: var MirBuilder, source: var SourceMap) =
  ## Translates an `nkIdentDefs` AST into MIR and emits the result into
  ## `builder`'s currently selected buffer.
  assert n.kind == nkIdentDefs and n.len == 3
  var c = TCtx(context: skUnknown, graph: graph, config: config,
               env: move env)
  # treat the code as top-level code so that no 'def' is generated for
  # assignments to globals
  c.scopeDepth = 1

  template swapState() =
    swap(c.sp.map, source)
    swap(c.builder, builder)

  swapState()
  genLocInit(c, n[0], n[2])
  swapState()
  env = move c.env # move back

proc generateCode*(graph: ModuleGraph, env: var MirEnv,
                   config: TranslationConfig, n: PNode,
                   builder: var MirBuilder, source: var SourceMap) =
  ## Generates MIR code that is semantically equivalent to the expression or
  ## statement `n`, appending the resulting code and the corresponding origin
  ## information to `code` and `source`, respectively.
  var c = TCtx(context: skUnknown, graph: graph, config: config, env: move env)
  c.scopeDepth = 2 # assume that this is not top-level code

  template swapState() =
    swap(c.sp.map, source)
    swap(c.builder, builder)

  # for the duration of ``generateCode`` we move the state into ``TCtx``
  swapState()

  if n.typ.isEmptyType:
    withFront c.builder:
      gen(c, n)
  else:
    c.builder.useSource(c.sp, n)
    # XXX: restructure the ``mirgen`` API to use a dedicated procedure for
    #      generating expression code
    let v = genUse(c, n)
    c.use v

  # move the state back into the output parameters:
  swapState()
  env = move c.env

proc addParams(c: var TCtx, prc: PSym, signature: PType) =
  ## Translates the result variable and the parameters (taken from `signature`)
  ## to their MIR representation and adds them to the list of locals.
  template add(x: Local) =
    discard c.addLocal(x)

  # result variable:
  if signature[0].isEmptyType():
    # always reserve a slot for the result variable, even if the latter is
    # not present
    add Local()
  else:
    add localToMir(prc.ast[resultPos].sym)

  # parameters:
  let params = signature.n
  for i in 1..<params.len:
    add paramToMir(params[i].sym)

  if signature.callConv == ccClosure:
    # environment parameter
    add paramToMir(prc.ast[paramsPos][^1].sym)

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
               userOptions: owner.options, env: move env)
  c.sp.active = (body, c.sp.map.add(body))

  c.scopeDepth = 1
  c.add MirNode(kind: mnkScope)
  if sfNeverRaises in owner.flags:
    c.add MirNode(kind: mnkTry, len: 1)
    c.add MirNode(kind: mnkStmtList)

  if owner.kind in routineKinds:
    # the procedure backing a macro has its own internal signature; use that
    # beyond this point
    let signature =
      if owner.kind == skMacro:
        owner.internal
      else:
        owner.typ

    addParams(c, owner, signature)
    # add a 'def' for each ``sink`` parameter. This simplifies further
    # processing and analysis
    let params = signature.n
    for i in 1..<params.len:
      let s = params[i].sym
      if s.typ.isSinkTypeForParam():
        c.subTree mnkDef:
          c.add nameNode(c, s)
          c.add MirNode(kind: mnkNone)
  else:
    # reserve the result slot:
    discard c.addLocal(Local())

  gen(c, body)

  if sfNeverRaises in owner.flags:
    # if it's enforced that the procedure never raises, exceptions escaping
    # the procedure terminate the program. This is achieved by wrapping the
    # body in a catch-all exception handler
    c.add endNode(mnkStmtList)
    c.subTree MirNode(kind: mnkExcept, len: 1):
      c.subTree mnkBranch:
        c.subTree mnkVoid:
          let p = c.graph.getCompilerProc("nimUnhandledException")
          c.builder.buildCall c.env.procedures.add(p), typeOrVoid(c, p.typ[0]):
            discard
    c.add endNode(mnkTry)

  c.add endNode(mnkScope)

  env = c.env

  # move the buffers into the result body
  let (code, locals) = finish(move c.builder, default(Store[LocalId, Local]))
  MirBody(locals: locals, source: move c.sp.map, code: code)

proc exprToMir*(graph: ModuleGraph, env: var MirEnv,
                config: TranslationConfig, e: PNode): MirBody =
  ## Only meant to be used by `vmjit <#vmjit>`_. Produces a MIR body for a
  ## standalone expression. The result of the expression is assigned to the
  ## special local with ID 0.
  var c = TCtx(context: skUnknown, graph: graph, config: config, env: move env)
  c.sp.active = (e, c.sp.map.add(e))

  let res = c.addLocal(Local(typ: e.typ)) # the result variable
  c.scope:
    c.buildStmt mnkDef:
      c.use toValue(mnkLocal, res, e.typ)
      if e.typ.kind == tyTypeDesc:
        # FIXME: this shouldn't happen, but type expressions are sometimes
        #        evaluated with the VM, such as a ``typeof(T.x)`` appearing as
        #        a field type within a generic object definition. While it
        #        makes sense to allow evaluating type expression with the VM,
        #        in simple situtations like the example above, it's simpler,
        #        faster, and more intuitive to either evaluate them directly
        #        when analyzing the type expression, or during ``semfold``
        c.use genTypeExpr(c, e)
      else:
        c.genAsgnSource(e, {dfOwns, dfEmpty})

  env = move c.env

  let (code, locals) = finish(move c.builder, default(Store[LocalId, Local]))
  MirBody(locals: locals, source: move c.sp.map, code: code)

proc constDataToMir*(env: var MirEnv, n: PNode): MirTree =
  ## Translates the construction expression AST `n` representing some
  ## constant data to its corresponding MIR representation.
  proc constToMirAux(bu: var MirBuilder, env: var MirEnv, n: PNode) =
    case n.kind
    of nkObjConstr:
      # no normalization/canonicalization takes place here, meaning that
      # ``Obj(a: 0, b: 1)`` and ``Obj(b: 1, a: 0)`` will result in two data
      # table entries, even though the values they represent are equivalent
      bu.subTree MirNode(kind: mnkObjConstr, typ: n.typ, len: n.len-1):
        for i in 1..<n.len:
          bu.add MirNode(kind: mnkField, field: n[i][0].sym.position.int32)
          bu.subTree mnkArg:
            constToMirAux(bu, env, n[i][1])
    of nkCurly:
      # similar to object construction, no normalization means that ``{1, 2}``
      # and ``{2, 1}`` results in two data table entries
      bu.subTree MirNode(kind: mnkSetConstr, typ: n.typ, len: n.len):
        for it in n.items:
          constToMirAux(bu, env, it)
    of nkBracket, nkTupleConstr, nkClosure:
      let kind: range[mnkArrayConstr..mnkClosureConstr] =
        case n.typ.skipTypes(abstractInst).kind
        of tyArray:                 mnkArrayConstr
        of tyOpenArray, tySequence: mnkSeqConstr
        of tyTuple:                 mnkTupleConstr
        of tyProc:                  mnkClosureConstr
        else:                       unreachable()

      bu.subTree MirNode(kind: kind, typ: n.typ, len: n.len):
        for it in n.items:
          bu.subTree mnkArg:
            constToMirAux(bu, env, it.skipColon)

    of nkSym:
      # must either be another constant or a procedural value
      case n.sym.kind
      of skProc, skFunc, skConverter, skIterator:
        bu.use toValue(env.procedures.add(n.sym), n.typ)
      of skConst:
        bu.use toValue(env.constants.add(n.sym), n.sym.typ)
      else:
        unreachable()
    of nkRange:
      bu.subTree MirNode(kind: mnkRange, len: 2):
        constToMirAux(bu, env, n[0])
        constToMirAux(bu, env, n[1])
    of nkNilLit:
      bu.add MirNode(kind: mnkNilLit, typ: n.typ)
    of nkIntLiterals:
      bu.use toIntLiteral(env, n)
    of nkFloatLiterals:
      bu.use toFloatLiteral(env, n)
    of nkStrLiterals:
      bu.use strLiteral(env, n.strVal, n.typ)
    of nkHiddenStdConv, nkHiddenSubConv:
      # doesn't translate to a MIR node itself, but the type overrides
      # that of the sub-expression
      let top = bu.staging.len
      constToMirAux(bu, env, n[1])
      # patch the type:
      bu.staging[top].typ = n.typ
    else:
      unreachable(n.kind)

  var bu = initBuilder(SourceId 0)
  # push and pop the content so that ``constToMirAux`` places the nodes into
  # the staging buffer, which is necessary for after-the-fact type patching
  bu.pop(bu.push(constToMirAux(bu, env, n)))
  bu.finish()[0]
