## ``mirgen`` is responsible for generating MIR code from ``PNode`` AST. The
## input AST is expected to have already been transformed by ``transf``.
##
## Operation wise, the input AST is traversed via recursing into statements and
## expressions, ignoring declarative constructs that are not relevant to nor
## representable with the MIR.
##
## None of the MIR operations that imply structured control-flow produce a
## value, so the input expression that do (``if``, ``case``, ``block``, and
## ``try``) are translated into statements where the last expression in each
## clause is turned into an assignment to, depending on the context where
## they're used, either a temporary or existing lvalue expression. The latter
## are forwarded to the generation procedures via ``Destination``.
##
## Origin information
## ==================
##
## Each generated ``MirNode`` is associated with the ``PNode`` it originated
## (referred to as "source information") from. Setting the ``PNode`` to use
## as the origin is done by using the ``useSource`` template. It overrides the
## active source information until the end of the scope it is called inside --
## all nodes without explicitly attached source information added till
## the end of the scope will store the provided ``PNode`` as their origin.
##
## The source information is set/attached lazily because:
## 1. it allows for adding the attachments in a batch, reducing the amount of
##    checks and potentially also ``seq`` resizings
## 2. it decouples adding nodes from attaching source information to them,
##    making changes to how source information is stored/attached a bit
##    simpler
##
## A downside of the current implementation is that it's easy to forget
## explicitly overriding the used source information. While this won't impact
## the generated code, it'll impact the AST / line-information shown in e.g.
## reports.
##

import
  std/[
    tables
  ],
  compiler/ast/[
    ast,
    lineinfos,
    trees,
    types,
    wordrecg
  ],
  compiler/mir/[
    mirconstr,
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
  compiler/utils/[
    containers,
    idioms
  ],
  experimental/[
    dod_helpers
  ]

type
  DestinationKind = enum
    dkNone ## no destination
    dkFrag ## an lvalue expression
    dkGen  ## an lvalue expression that is generated in-place

  DestFlag = enum
    ## Extra information about an assignment destination. The flags are used to
    ## decide which kind of assignment to use
    dfEmpty ## the destination doesn't store a value yet
    dfOwns  ## the destination is an owning location

  Destination = object
    ## Stores the information necessary to generate the code for an assignment
    ## to some lvalue expression
    case kind: DestinationKind
    of dkNone:
      discard
    of dkFrag:
      mnode: MirNode
      source: PNode
    of dkGen:
      node {.cursor.}: PNode

    flags: set[DestFlag]

  Block = object
    ## Information about a ``block``
    label: PSym ## the symbol of the block's label. 'nil' if the block has no
                ## label
    id: LabelId ## the block's internal label ID

  CodeFragment = object
    ## A MIR code fragment that doesn't imply any further semantic meaning. The
    ## code is not required to be complete (i.e. grammatically valid).
    nodes: MirNodeSeq
    source: seq[SourceId]
      ## the ``SourceId`` for each ``MirNode` in `nodes`. The association
      ## happens via their respective index in the ``seq``s. During
      ## construction, `source` is allowed to temporarily have less items
      ## than `nodes`

  SourceProvider = object
    ## Stores the active origin and the in-progress database of origin
    ## ``PNode``s. Both are needed together in most cases, hence their bundling
    ## into an object
    active: tuple[n: PNode, id: opt(SourceId)]
      ## the ``PNode`` to use as the origin for emitted ``MirNode``s (if none
      ## is explicitly provided). If `id` is 'none', no database entry exists
      ## for the ``PNode`` yet
    store: Store[SourceId, PNode]
      ## the in-progress database of origin ``PNode``s

  GenOption* = enum
    goIsNimvm     ## choose the ``nimvm`` branch for ``when nimvm`` statements
    goGenTypeExpr ## don't omit type expressions
    goIsCompileTime ## whether the code is meant to be run at compile-time.
                    ## Affects handling of ``.compileTime`` globals

  TCtx = object
    # output:
    stmts: CodeFragment

    # working state:
    staging: CodeFragment ## intermediate buffer for partially generated MIR
                          ## code

    blocks: seq[Block] ## the stack of active ``block``s. Used for looking up
                       ## break targets

    sp: SourceProvider

    numTemps: int  ## provides the ID for temporaries
    numLabels: int ## provides the ID to use for the next label

    # input:
    context: TSymKind ## what entity the input AST is part of (e.g. procedure,
                      ## macro, module, etc.). Used to allow or change how the
                      ## AST is interpreted in some places
    userOptions: set[TOption]
    graph: ModuleGraph

    options: set[GenOption]

  Value = EValue
  # TODO: move this alias to ``mirconstr``

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

func canonicalExpr(n: PNode): PNode =
  ## Returns the canonical expression, i.e. the expression without leading
  ## pragma blocks or empty statement lists
  func skipped(n: PNode): PNode =
    case n.kind
    of nkPragmaBlock:
      n.lastSon
    of nkStmtListExpr:
      if stupidStmtListExpr(n):
        # the statement-list expression is redundant (i.e. only has a single
        # item or only leading empty nodes) -> skip it
        n.lastSon
      else:
        # the list needs to be kept
        n
    else:
      n

  var n {.cursor.} = n
  while (let it = skipped(n); it != n):
    n = it

  result = n

func selectWhenBranch(n: PNode, isNimvm: bool): PNode =
  assert n.kind == nkWhen
  if isNimvm: n[0][1]
  else:       n[1][0]

func selectDefKind(s: PSym): range[mnkDef..mnkDefCursor] =
  ## Returns the kind of definition to use for the given entity
  if sfCursor in s.flags: mnkDefCursor
  else:                   mnkDef

func isSome(x: Destination): bool {.inline.} =
  x.kind != dkNone

func initDestination(n: PNode, isFirst: bool): Destination =
  var flags: set[DestFlag]
  if isFirst:
    flags.incl dfEmpty
  if not isCursor(n):
    flags.incl dfOwns

  Destination(kind: dkGen, node: n, flags: flags)

proc typeOrVoid(g: ModuleGraph, t: PType): PType =
  ## Returns `t` if it's not 'nil' - the ``void`` type otherwise
  if t != nil: t
  else:        g.getSysType(unknownLineInfo, tyVoid)

proc typeOrVoid(c: TCtx, t: PType): PType {.inline.} =
  ## Returns `t` if it's not 'nil' - the ``void`` type otherwise
  # TODO: cache the void type
  typeOrVoid(c.graph, t)

func nextTempId(c: var TCtx): TempId =
  result = TempId(c.numTemps)
  inc c.numTemps

func nextLabel(c: var TCtx): LabelId =
  result = LabelId(c.numLabels)
  inc c.numLabels

# ----------- CodeFragment API -------------
# XXX: the ``CodeFragment`` API is going to be removed

func len(c: CodeFragment): int {.inline.} =
  c.nodes.len

template add(f: var CodeFragment, n: MirNode) =
  f.nodes.add n

template subTree(f: var MirNodeSeq, n: MirNode, body: untyped) =
  ## Convenience template for emitting a sub-tree around `body`.
  let tmp = n
  f.add tmp
  body
  f.add endNode(tmp.kind)

template subTree(f: var CodeFragment, n: MirNode, body: untyped) =
  f.nodes.subTree n:
    body

func apply(frag: var CodeFragment, id: SourceId) =
  ## Associates all nodes added since the previous call to ``apply`` with the
  ## origin information identified by `id`
  assert frag.nodes.len > frag.source.len
  let start = frag.source.len
  frag.source.setLen(frag.nodes.len)

  for i in start..<frag.source.len:
    frag.source[i] = id

func prepareForUse(sp: var SourceProvider): SourceId =
  if sp.active.id.isNone:
    sp.active.id = someOpt sp.store.add(sp.active.n)

  result = sp.active.id[]

func applySource(frag: var CodeFragment, sp: var SourceProvider) =
  ## Associates all ``MirNode``s that don't yet have a source association with
  ## the currently active source information
  if frag.nodes.len > frag.source.len:
    apply(frag, prepareForUse(sp))

template useSource(frag: var CodeFragment, sp: var SourceProvider,
                   origin: PNode) =
  ## Pushes `origin` to be used as the source for the rest of the scope that
  ## ``useSource`` is used inside. When the scope is left, the previous origin
  ## is restored
  applySource(frag, sp)

  # setup the new source information and swap it with the active one
  var prev = (origin, noneOpt(SourceId))
  swap(prev, sp.active)

  defer:
    # apply the still active source information first and then restore the
    # the source information that was previously active:
    applySource(frag, sp)
    swap(prev, sp.active)

# -------------- builder routines -------------

func commit(c: var TCtx, start: int) =
  ## Commits nodes from the staging buffer into the final buffer.
  let
    num = c.staging.nodes.len - start
    offset = c.stmts.nodes.len

  assert num > 0, "nothing to commit"
  # first resize the destination buffer, then *move* all elements over
  c.stmts.nodes.setLen(offset + num)
  for i in start..<c.staging.nodes.len:
    c.stmts.nodes[offset + i - start] = move c.staging.nodes[i]

  # remove the moved-over nodes from the staging buffer:
  c.staging.nodes.setLen(start)

template subTree(f: var CodeFragment, k: MirNodeKind, body: untyped) =
  f.subTree MirNode(kind: k):
    body

template subTree(f: var TCtx, k: MirNodeKind, body: untyped) =
  f.staging.subTree MirNode(kind: k):
    body

template subTree(f: var TCtx, n: MirNode, body: untyped) =
  f.staging.subTree n:
    body

template scope(f: var CodeFragment, body: untyped) =
  f.subTree mnkScope:
    body

func use(f: var CodeFragment, val: EValue) =
  assert val.node.kind != mnkNone
  f.add val.node

func use(c: var TCtx, val: EValue) {.inline.} =
  use(c.staging, val)

proc emitByVal(c: var TCtx, val: EValue) =
  ## Emits a pass-by-value argument sub-tree with `val`.
  c.subTree MirNode(kind: mnkArg):
    c.use val

proc emitByName(c: var TCtx, val: EValue) =
  ## Emits a pass-by-name argument sub-tree with `val`.
  c.subTree MirNode(kind: mnkName):
    c.use val

proc emitByConsume(c: var TCtx, val: EValue) =
  ## Emits a pass-by-consume argument sub-tree with `val`.
  c.subTree MirNode(kind: mnkConsume):
    c.use val

proc emitByName(c: var TCtx, val: EValue, eff: EffectKind) =
  ## Emits a pass-by-name argument sub-tree with `val` and `eff`.
  c.subTree MirNode(kind: mnkName):
    c.subTree MirNode(kind: mnkTag, effect: eff):
      c.use val

proc empty(c: var TCtx, n: PNode): MirNode =
  MirNode(kind: mnkNone, typ: n.typ)

func nameNode(s: PSym): MirNode =
  if s.kind == skTemp:
    # temporaries are always locals, even if marked with the ``sfGlobal``
    # flag
    MirNode(kind: mnkLocal, typ: s.typ, sym: s)
  elif sfGlobal in s.flags:
    MirNode(kind: mnkGlobal, typ: s.typ, sym: s)
  elif s.kind == skParam:
    MirNode(kind: mnkParam, typ: s.typ, sym: s)
  elif s.kind == skConst:
    MirNode(kind: mnkConst, typ: s.typ, sym: s)
  elif s.kind in {skVar, skLet, skForVar, skResult}:
    MirNode(kind: mnkLocal, typ: s.typ, sym: s)
  else:
    unreachable(s.kind)

template genLocation(c: var TCtx, n: PNode): EValue =
  EValue(node: nameNode(n.sym))

func allocTemp(c: var TCtx, typ: PType; alias=false): Value =
  ## Allocates a new ID for a temporary and returns the name.
  let id = nextTempId(c)
  if alias: alias(typ, id)
  else:     temp(typ, id)

proc gen(c: var TCtx; n: PNode)
proc genx(c: var TCtx; n: PNode, consume: bool = false)
proc genComplexExpr(c: var TCtx, n: PNode, dest: Destination)

proc genAsgn(c: var TCtx, dest: Destination, rhs: PNode)
proc genWithDest(c: var TCtx; n: PNode; dest: Destination)

func getTemp(c: var TCtx, typ: PType): EValue =
  ## Allocates a new temporary and emits a definition for it into the
  ## final buffer.
  assert typ != nil
  result = c.allocTemp(typ)
  c.stmts.subTree mnkDef:
    c.stmts.add result.node
    c.stmts.add MirNode(kind: mnkNone)

template buildStmt(c: var TCtx, body: untyped) =
  let start = c.staging.nodes.len
  body
  commit(c, start)

template buildStmt(c: var TCtx, k: MirNodeKind, body: untyped) =
  c.buildStmt:
    c.staging.subTree MirNode(kind: k):
      body

template buildMagicCall(c: var TCtx, m: TMagic, t: PType, body: untyped) =
  c.subTree MirNode(kind: mnkMagic, magic: m, typ: t):
    body

func needsOwningLocation(tree: MirTree, n: NodePosition, sink: bool): bool =
  ## Computes for the expression at `n` whether it need to be assigned to a
  ## full location.
  case tree[n].kind
  of mnkCall, mnkMagic, mnkView, mnkToSlice:
    # slices and view don't strictly need a full location
    true
  of mnkObjConstr:
    if tree[n].typ.skipTypes(abstractInst).kind == tyRef:
      # ref constructions always need to be destroyed
      true
    else:
      sink
  else:
    # depends on whether it's used in a sink context
    sink

func captureInTemp(c: var TCtx, start: int, sink: bool): Value =
  ## Wraps the expression starting at `start` in the staging buffer in a
  ## temporary, removing the expression from the buffer. `sink` identifies
  ## whether the temporary is intended for use in a sink context.
  let def =
    if needsOwningLocation(c.staging.nodes, NodePosition start, sink):
      mnkDef
    else:
      mnkDefCursor

  result = c.allocTemp(c.staging.nodes[start].typ)
  c.stmts.subTree def:
    c.stmts.use result
    c.commit(start)

proc genUse(c: var TCtx, n: PNode): EValue =
  # TODO: document
  let start = c.staging.len
  # emit the expression into the staging buffer:
  genx(c, n)

  if start + 1 == c.staging.len:
    # only a single node is in the buffer. It must be an atom,
    # meaning that it can be used directly
    result = EValue(node: move c.staging.nodes[start])
    c.staging.nodes.setLen(start)
  else:
    result = captureInTemp(c, start, sink=false)

proc genRd(c: var TCtx, n: PNode; consume=false): EValue =
  ## Generates the MIR code for the expression `n`. Makes sure that the run-
  ## time value of the expression is *captured* by assigning it to a
  ## temporary.
  let start = c.staging.len
  genx(c, n)

  let kind = c.staging.nodes[start].kind

  let needsTemp =
    start + 1 > c.staging.len or
      kind notin {mnkParam, mnkConst, mnkTemp, mnkLiteral, mnkProc}
  # parameters and constants are true immutable locations, so they don't
  # need to be captured. 'let' symbols are excluded here, as while their
  # not directly mutable, they're allowed to be moved out of (which is
  # a mutation) by later optimization passes

  if needsTemp:
    result = captureInTemp(c, start, consume)
  else:
    result = Value(node: c.staging.nodes[start])
    c.staging.nodes.setLen(start)

proc genPath(c: var TCtx, n: PNode; sink = false)

proc genOp(c: var TCtx, k: MirNodeKind, t: PType, n: PNode) =
  assert t != nil
  c.subTree MirNode(kind: k, typ: t):
    # XXX: we don't want a path, we want an lvalue (which could be a path)
    genPath(c, n)

template buildOp(c: var TCtx, k: MirNodeKind, t: PType, body: untyped) =
  assert t != nil
  c.subTree MirNode(kind: k, typ: t):
    body

template wrapTemp(c: var TCtx, t: PType, body: untyped): EValue =
  ## Assigns the expression emitted by `body` to a temporary and
  ## returns the name of the latter.
  assert t != nil
  let
    res   = temp(t, nextTempId(c))
    start = c.staging.nodes.len

  c.staging.subTree MirNode(kind: mnkDef):
    c.use res
    body

  commit(c, start)
  res

template wrapAndUse(c: var TCtx, t: PType, body: untyped) =
  ## Assigns the expression emitted by `body` to a temporary
  ## and immediately emits a use thereof.
  let tmp = c.wrapTemp(t):
    body
  c.use tmp

template buildTree(c: var TCtx, k: MirNodeKind, t: PType, body: untyped) =
  c.staging.subTree MirNode(kind: k, typ: t):
    body

template withSource(c: var TCtx, src: PNode, body: untyped) =
  {.warning: "missing implementation".}
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
  var v = EValue(node: dest.mnode)
  if n[0].sym.magic == mOr:
    v = c.wrapTemp n.typ:
      c.buildMagicCall mNot, n.typ:
        c.withSource(dest.source):
          c.emitByVal v

  c.stmts.subTree MirNode(kind: mnkIf):
    c.stmts.use v
    c.stmts.subTree mnkStmtList:
      genAsgn(c, dest, n[2]) # the right-hand side

proc genBracketExpr(c: var TCtx, n: PNode) =
  let typ = n[0].typ.skipTypes(abstractInstTypeClass - {tyTypeDesc})
  case typ.kind
  of tyTuple:
    let i = n[1].intVal.int
    # due to all the type-related modifications done by ``transf``, it's safer
    # to lookup query the type from the tuple instead of using `n.typ`
    c.subTree MirNode(kind: mnkPathPos, typ: typ[i], position: i.uint32):
      genPath(c, n[0])
  of tyArray, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray, tyString,
     tyCstring:
    c.buildOp mnkPathArray, elemType(typ):
      genPath(c, n[0])
      c.use genRd(c, n[1])
  else: unreachable()

proc genVariantAccess(c: var TCtx, n: PNode) =
  assert n.kind == nkCheckedFieldExpr

  let access = n[0]
  assert access.kind == nkDotExpr

  # iterate the checks in reverse, as the outermost discriminator check is
  # rightmost
  # XXX: the "rightmost" part is an implementation detail of how
  #      ``nkCheckedFieldExpr`` nodes are generated by ``sem.nim``,
  #      depending on it here is brittle. It's okay for now and
  #      ``nkCheckedFieldExpr`` should ideally be removed anyways
  for i in 1..<n.len:
    let check = n[i]
    assert check.kind in nkCallKinds
    # the check may be wrapped in a ``not`` call -- unwrap it first
    let inCall =
      if check[0].sym.magic == mNot: check[1]
      else:                          check

    c.staging.add MirNode(kind: mnkPathVariant, typ: access[0].typ,
                          field: inCall[2].sym)

  genPath(c, access[0])

  # close the open sub trees:
  for i in 1..<n.len:
    c.staging.add endNode(mnkPathVariant)

proc genTypeExpr(c: var TCtx, n: PNode): EValue =
  ## Generates the code for an expression that yields a type. These are only
  ## valid in metaprogramming contexts. If it's a static type expression, we
  ## evaluate it directly and store the result as a type literal in the MIR
  assert n.typ.kind == tyTypeDesc
  c.stmts.useSource(c.sp, n)
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

proc genCallee(c: var TCtx, n: PNode): EValue =
  ## Generates and emits the code for a callee expression
  if n.kind == nkSym and n.sym.kind in routineKinds:
    c.stmts.useSource(c.sp, n)
    procLit(n.sym)
  else:
    # an indirect call
    genRd(c, n)

proc genArgExpression(c: var TCtx, n: PNode, sink: bool): EValue =
  ## Generates the code for an expression appearing in the context of an
  ## argument
  c.stmts.useSource(c.sp, n)

  let start = c.staging.len
  genx(c, n, consume = sink)

  let k = c.staging.nodes[start].kind
  var needsTemp = start + 1 > c.staging.len
  if not needsTemp:
    # XXX: this condition is quite hard to read and parse, find a way
    #      to make it clearer
    if (k in {mnkParam, mnkConst} and sink) or
        k notin {mnkParam, mnkConst, mnkTemp, mnkLiteral, mnkProc}:
      # we do want an owned value, so a copy by way of assignment is
      # also introduced for parameters and constants
      needsTemp = true

  if needsTemp:
    result = captureInTemp(c, start, sink)
  else:
    result = Value(node: move c.staging.nodes[start])
    c.staging.nodes.setLen(start)

proc genByNameArg(c: var TCtx, n: PNode; mutable = true): EValue =
  ## Generates the code for expression `n`. The address/name of the expression
  ## is captured (but not its value), with `mutable` denoting whether the
  ## address is going to be used for mutation of the underlying location.
  case n.kind
  of nkSym:
    # emit the symbol directly, without going through a temporary
    EValue(node: nameNode(n.sym))
  of nkHiddenAddr:
    genByNameArg(c, n[0], mutable)
  else:
    # something more complex, bind the address to an alias and return
    # that
    let res = c.allocTemp(n.typ, alias=true)
    c.buildStmt (if mutable: mnkBindMut else: mnkBind):
      c.staging.add res.node
      genx(c, n)
    res

proc genArg(c: var TCtx, formal: PType, n: PNode) =
  ## Generates and emits the MIR code for an argument expression plus the
  ## required argument sink. The `formal` type is needed for figuring out
  ## how the argument is passed.
  case formal.skipTypes(abstractRange-{tySink}).kind
  of tyVar:
    if formal.base.kind in {tyOpenArray, tyVarargs}:
      # it's not a pass-by-name parameter
      c.emitByVal genArgExpression(c, n, sink=false)
    else:
      c.emitByName genByNameArg(c, n, true), ekMutate
  of tySink:
    c.emitByConsume genArgExpression(c, n, sink=true)
  else:
    c.emitByVal genArgExpression(c, n, sink=false)

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

    if t.kind == tyTypeDesc:
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
      # translation back to ``PNode``, we don't omit them completely but only
      # replace them with a node holding their type
      c.subTree mnkArg:
        c.staging.add empty(c, n[i])
    elif t.kind == tyVoid:
      # a ``void`` argument. We can't just generate an ``mnkNone`` node, as the
      # statement used as the argument can still have side-effects
      gen(c, n[i])
      c.subTree mnkArg:
        c.staging.add empty(c, n[i])
    elif i == 1 and not fntyp[0].isEmptyType() and
         not isHandleLike(t) and
         classifyBackendView(fntyp[0]) != bvcNone:
      # the procedure returns a view, but the first parameter is not something
      # that resembles a handle. We need to make sure that the first argument
      # (which the view could be created from), is passed by reference in that
      # case.
      c.emitByName genByNameArg(c, n[i], false)
    else:
      genArg(c, t, n[i])

proc genCall(c: var TCtx, n: PNode) =
  ## Generates and emits the MIR code for a call expression.
  let fntyp = n[0].typ
  var effects: set[GeneralEffect]
  if canRaiseConservative(n[0]):
    effects.incl geRaises

  if tfNoSideEffect notin fntyp.flags:
    effects.incl geMutateGlobal

  c.subTree MirNode(kind: mnkCall, typ: typeOrVoid(c, fntyp[0]),
                    effects: effects):
    c.use genCallee(c, n[0])
    genArgs(c, n)

proc genMacroCallArgs(c: var TCtx, n: PNode, kind: TSymKind, fntyp: PType) =
  ## Generates the arguments for a macro/template call expression. `n` is
  ## expected to be a ``getAst`` expression that has been transformed to the
  ## internal representation. `kind` is the meta-routine's kind, and `fntyp`
  ## its signature.
  assert kind in {skMacro, skTemplate}
  if kind == skMacro:
    c.use genCallee(c, n[1])
  else:
    c.emitByVal genCallee(c, n[1])

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
            var sv: EValue
            if se.kind == nkCurly:
              sv = c.allocTemp(se.typ)
              c.subTree mnkDef:
                c.use sv
                c.subTree MirNode(kind: mnkConstr, typ: se.typ):
                  for it in se.items:
                    c.emitByVal genRd(c, it)
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
  c.stmts.useSource(c.sp, n)

  proc argExpr(c: var TCtx, n: PNode): EValue =
    ## Generates an argument expression in a context where information about
    ## the formal type is missing -- the type of the expression is used as the
    ## formal type instead. This only works for arguments where the parameter is
    ## known to neither be of ``var`` nor ``sink`` type
    # make sure to skip types such as ``sink``, as the expression would be
    # erroneously treated as used in a consume context otherwise
    genRd(c, n)

  case m
  of mAnd, mOr:
    let tmp = getTemp(c, n.typ)
    genAndOr(c, n, Destination(kind: dkFrag, mnode: tmp.node, source: n))
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
        c.emitByVal genRd(c, n[1])

  of mWasMoved:
    # ``wasMoved`` has an effect that is not encoded by the parameter's type
    # (it kills the location), so we need to manually translate it
    c.buildMagicCall m, typeOrVoid(c, n.typ):
      c.emitByName genByNameArg(c, n[1]), ekKill
  of mConStrStr:
    # the `mConStrStr` magic is very special. Nested calls to it are flattened
    # into a single call in ``transf``. It can't be passed on to ``genCall``
    # since the number of arguments doesn't match with the number of parameters
    c.buildMagicCall m, n.typ:
      for i in 1..<n.len:
        c.emitByVal argExpr(c, n[i])
  of mRunnableExamples:
    # omit the ``runnableExamples`` call. The callsite of ``genMagic`` expects
    # that we emit something, so we emit an ``mnkEmpty`` node
    # TODO: call to ``runnableExamples`` shouldn't reach into ``mirgen``. If
    #       they do, it means that simple expression might sometimes not be
    #       detected as such, because ``canonicalExpr`` doesn't consider
    #       ``runnableExamples``
    discard
  of mInSet:
    genInSetOp(c, n)
  of mEcho:
    # forward the wrapped arguments to the call; don't emit the intermediate array
    let x = n[1].skipConv
    assert x.kind == nkBracket
    c.buildMagicCall m, typeOrVoid(c, n.typ):
      # for the convenience of later transformations, the type of the would-be
      # array is passed along as the first argument
      if x.len > 0:
        c.emitByVal typeLit(x.typ)
      for it in x.items:
        c.emitByVal argExpr(c, it)

  # magics that use incomplete symbols (most of them are generated by
  # ``liftdestructors``):
  of mDestroy:
    # ``mDestroy`` magic calls might be incomplete symbols, so we have to
    # translate them manually
    c.buildMagicCall m, typeOrVoid(c, n.typ):
      c.emitByName genByNameArg(c, n[1]), ekMutate
  of mNewSeq:
    # XXX: the first parameter is actually an ``out`` parameter -- the
    #      ``ekReassign`` effect could be used
    if n[0].typ == nil:
      c.buildMagicCall m, typeOrVoid(c, n.typ):
        c.emitByName genByNameArg(c, n[1]), ekMutate
        c.emitByVal  argExpr(c, n[2])
    else:
      genCall(c, n)
  of mInc, mSetLengthStr, mCopyInternal:
    if n[0].typ == nil:
      c.buildMagicCall m, typeOrVoid(c, n.typ):
        c.emitByName genByNameArg(c, n[1]), ekMutate
        c.emitByVal  argExpr(c, n[2])
    else:
      genCall(c, n)
  of mNot, mLtI, mSubI, mLengthSeq, mLengthStr, mAccessEnv, mSamePayload:
    if n[0].typ == nil:
      # simple translation. None of the arguments need to be passed by lvalue
      c.buildMagicCall m, n.typ:
        for i in 1..<n.len:
          c.emitByVal argExpr(c, n[i])

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
      c.subTree MirNode(kind: mnkCall, typ: n.typ,
                        effects: {geMutateGlobal, geRaises}):
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

proc emitArg(c: var TCtx, consume: bool, v: EValue) =
  let kind =
    if consume: mnkConsume
    else:       mnkArg
  c.subTree MirNode(kind: kind):
    c.use v

proc genSetConstr(c: var TCtx, n: PNode) =
  c.buildTree mnkConstr, n.typ:
    for it in n.items:
      c.emitByVal genRd(c, it)

proc genArrayConstr(c: var TCtx, n: PNode, isConsume: bool) =
  c.buildTree mnkConstr, n.typ:
    for it in n.items:
      c.emitArg isConsume, genArgExpression(c, it, isConsume)

proc genTupleConstr(c: var TCtx, n: PNode, isConsume: bool) =
  assert n.typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind == tyTuple
  c.buildTree mnkConstr, n.typ:
    for it in n.items:
      c.emitArg isConsume, genArgExpression(c, it.skipColon, isConsume)

proc genClosureConstr(c: var TCtx, n: PNode, isConsume: bool) =
  c.buildTree mnkConstr, n.typ:
    c.emitByVal genRd(c, n[0].skipConv) # the procedure
    # transf wraps the procedure operand in a conversion that we don't
    # need

    let v =
      if n[1].kind == nkNilLit:
        # it can happen that a ``nkNilLit`` has no type (i.e. its typ is nil) -
        # we ensure that the nil literal has the correct type
        # TODO: prevent a ``nkNilLit`` with no type information from being
        #       created instead
        literal(newNodeIT(nkNilLit, n[1].info, c.graph.getSysType(n[1].info, tyNil)))
      else:
        genArgExpression(c, n[1], isConsume)

    c.emitArg isConsume, v # the environment

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

      c.staging.add MirNode(kind: mnkField, field: field)
      c.emitArg useConsume, genArgExpression(c, it[1], useConsume)

proc genRaise(c: var TCtx, n: PNode) =
  assert n.kind == nkRaiseStmt
  c.buildStmt mnkRaise:
    if n[0].kind != nkEmpty:
      genPath(c, n[0], true)
    else:
      c.staging.add MirNode(kind: mnkNone)

proc genReturn(c: var TCtx, n: PNode) =
  assert n.kind == nkReturnStmt
  if n[0].kind != nkEmpty:
    gen(c, n[0])

  c.stmts.add MirNode(kind: mnkReturn)

proc genAsgnSource(c: var TCtx, e: PNode, sink: bool) =
  ## Generates the MIR code for the right-hand side of an assignment.
  ## The value is captured in a temporary if necessary for proper
  ## destruction.
  let start = c.staging.len
  genx(c, e, sink)
  if not sink and
     needsOwningLocation(c.staging.nodes, NodePosition start, false):
    # the expression produces some value that requires ownership being
    # taken of but the receiver doesn't support holding those. Assign the
    # value to an owning temporary (which can be destroyed later) first
    let tmp = c.allocTemp(e.typ)
    c.stmts.subTree mnkDef:
      c.stmts.add tmp.node
      c.commit(start)
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
    # left-hand side:
    case dest.kind
    of dkGen:  genPath(c, dest.node)
    of dkFrag: c.use EValue(node: dest.mnode)
    else:      unreachable()

    c.genAsgnSource(rhs, sink = owns) # rhs

proc unwrap(c: var TCtx, n: PNode): PNode =
  ## If `n` is a statement-list expression, generates the code for all
  ## statements and returns the unwrapped expression. Returns the canonicalized
  ## `n` otherwise
  result = canonicalExpr(n)
  if result.kind == nkStmtListExpr:
    for i in 0..<(result.len-1):
      gen(c, result[i])

    result = canonicalExpr(result.lastSon)
    assert result.kind != nkStmtListExpr

proc genAsgn(c: var TCtx, isFirst: bool, lhs, rhs: PNode) =
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
    isCursor = isCursor(lhs)

  func isSimple(n: PNode): bool =
    ## Computes if the l-value expression `n` always names the same valid
    ## location
    var n {.cursor.} = n
    while true:
      case n.kind
      of nkSym:
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

  if isSimple(lhs):
    # the rhs cannot change the location the lhs names, so we can assign
    # directly
    genWithDest(c, rhs, initDestination(lhs, isFirst))
  else:
    let kind =
      if isCursor:  mnkFastAsgn
      else:
        if isFirst: mnkInit
        else:       mnkAsgn

    c.buildStmt kind:
      genPath(c, lhs)
      genAsgnSource(c, rhs, sink=not isCursor)

proc genFastAsgn(c: var TCtx, lhs, rhs: PNode) =
  c.buildStmt mnkFastAsgn:
    genPath(c, lhs)
    genAsgnSource(c, rhs, sink=false)

proc genLocDef(c: var TCtx, n: PNode, val: PNode) =
  ## Generates the 'def' construct for the entity provided by the symbol node
  ## `n`
  let s = n.sym

  c.stmts.useSource(c.sp, n)
  c.buildStmt selectDefKind(s):
    case s.kind
    of skTemp:
      c.staging.add MirNode(kind: mnkLocal, typ: s.typ, sym: s)
    else:
      let kind =
        if sfGlobal in s.flags: mnkGlobal
        else:                   mnkLocal

      {.cast(uncheckedAssign).}:
        c.staging.add MirNode(kind: kind, typ: s.typ, sym: s)

    # the initializer is optional
    if val.kind != nkEmpty:
      # XXX: some closure types are erroneously reported as having no
      #      destructor, which would lead to memory leaks if the
      #      expression is a closure construction. As a work around,
      #      a missing destructor disables the sink context
      genAsgnSource(c, val, (sfCursor notin s.flags) and hasDestructor(s.typ))
    else:
      c.staging.add MirNode(kind: mnkNone)

proc genLocInit(c: var TCtx, symNode: PNode, initExpr: PNode) =
  ## Generates the code for a location definition. `sym` is the symbol of the
  ## location and `initExpr` the initializer expression
  let
    sym = symNode.sym

  assert sym.kind in {skVar, skLet, skTemp, skForVar}

  if sfCompileTime in sym.flags and goIsCompileTime notin c.options:
    # compile-time-only locations don't exist outside of compile-time
    # contexts, so omit their definitions
    return

  genLocDef(c, symNode, initExpr)

proc genVarTuple(c: var TCtx, n: PNode) =
  ## Generates the code for a ``let/var (a, b) = c`` statement
  assert n.kind == nkVarTuple
  c.stmts.useSource(c.sp, n)

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
      of nkDotExpr: genAsgn(c, true, lhs, rhs) # part of a closure environment
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
        genPath(c, lhs)
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
      c.stmts.useSource(c.sp, a)
      case a[0].kind
      of nkSym:
        genLocInit(c, a[0], a[2])
      of nkDotExpr:
        # initialization of a variable that was lifted into a closure
        # environment
        if a[2].kind != nkEmpty:
          genAsgn(c, false, a[0], a[2])
        else:
          # no intializer expression -> assign the default value
          c.buildStmt mnkInit:
            genPath(c, a[0])
            c.buildMagicCall mDefault, a[0].typ:
              discard
      else:
        unreachable()

    else:
      unreachable(a.kind)


proc genWhile(c: var TCtx, n: PNode) =
  ## Generates the code for a ``nkWhile`` node.
  assert isTrue(n[0]), "`n` wasn't properly transformed"
  c.stmts.subTree MirNode(kind: mnkRepeat):
    scope(c.stmts):
      c.gen(n[1])

proc genBlock(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates and emits the MIR code for a ``block`` expression or statement
  if sfUsed notin n[0].sym.flags:
    # if the label is never used, it means that the block is only used for
    # scoping. Omit emitting an ``mnkBlock`` and just use a scope
    scope(c.stmts): c.genWithDest(n[1], dest)
    return

  let id = nextLabel(c)

  # push the block to the stack:
  var oldLen = c.blocks.len
  c.blocks.add Block(label: n[0].sym, id: id)

  # generate the body:
  c.stmts.subTree MirNode(kind: mnkBlock, label: id):
    scope(c.stmts):
      c.genWithDest(n[1], dest)

  # pop the block:
  assert c.blocks.len == oldLen + 1
  c.blocks.setLen(oldLen)

proc genBranch(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates the body of a branch. Here, a branch refers to either an
  ## ``if|elif|else``, ``of``, or ``except`` clause

  # if the branch ends in a no-return statement, it has no type. We generate a
  # normal statement (without an assignment to `dest`) in that case
  if dest.kind != dkNone and not n.typ.isEmptyType():
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
    c.stmts.subTree MirNode(kind: mnkIf):
      c.stmts.use v
      scope(c.stmts):
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
    c.stmts.subTree MirNode(kind: mnkBlock, label: label):
      c.stmts.subTree mnkStmtList:
        for it in n.items:
          case it.kind
          of nkElifBranch, nkElifExpr:
            genElifBranch(it):
              # don't emit the 'break' if the branch doesn't have a structured
              # exit
              if not endsInNoReturn(it.lastSon):
                c.stmts.add MirNode(kind: mnkBreak, label: label)

          of nkElse, nkElseExpr:
            scope(c.stmts):
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

  let start = c.stmts.nodes.len
  c.stmts.add MirNode(kind: mnkCase)
  c.stmts.use v

  # the number of processed branches is not necessarily equal to the amount
  # we're going to emit (in case we skip some), so we have to count them
  # manually
  var num = 0
  # iterate of/else branches:
  for (_, branch) in branches(n):
    if branch.len == 1 and branch.kind == nkOfBranch:
      # an 'of' branch that has no labels (e.g. ``of {}:``). We omit the whole
      # branch and don't generate any code for it
      continue

    c.stmts.add MirNode(kind: mnkBranch, len: branch.len - 1)

    case branch.kind
    of nkElse:
      discard
    of nkOfBranch:
      # emit the lables:
      for (_, lit) in branchLabels(branch):
        c.stmts.add MirNode(kind: mnkLiteral, lit: lit)
    else:
      unreachable(branch.kind)

    # the branch's body:
    scope(c.stmts):
      genBranch(c, branch.lastSon, dest)

    c.stmts.add endNode(mnkBranch)
    inc num

  c.stmts.add endNode(mnkCase)

  # set the number of actually emitted branches:
  c.stmts.nodes[start].len = num

proc genExceptBranch(c: var TCtx, n: PNode, dest: Destination) =
  assert n.kind == nkExceptBranch
  c.stmts.useSource(c.sp, n)

  c.stmts.subTree MirNode(kind: mnkBranch, len: n.len - 1):
    # emit the exception types the branch covers:
    for _, tn in branchLabels(n):
      case tn.kind
      of nkType:
        c.stmts.add MirNode(kind: mnkType, typ: tn.typ)
      of nkInfix:
        # ``x as T`` doesn't get transformed to just ``T`` if ``T`` is the
        # type of an imported exception. We don't care about the type of
        # exceptions at the MIR-level, so we just use carry it along as is
        c.stmts.add MirNode(kind: mnkPNode, node: tn)
      else:
        unreachable()

    # generate the body of the branch:
    scope(c.stmts):
      genBranch(c, n.lastSon, dest)

proc genTry(c: var TCtx, n: PNode, dest: Destination) =
  let
    hasFinally = n.lastSon.kind == nkFinally
    hasExcept = n[1].kind == nkExceptBranch

  c.stmts.add MirNode(kind: mnkTry, len: ord(hasFinally) + ord(hasExcept))
  scope(c.stmts):
    c.genBranch(n[0], dest)

  let len =
    if hasFinally: n.len-1
    else: n.len
    ## the number of sub-nodes excluding ``nkFinally``

  if hasExcept:
    c.stmts.subTree MirNode(kind: mnkExcept, len: len-1):
      for i in 1..<len:
        genExceptBranch(c, n[i], dest)

  if hasFinally:
    c.stmts.useSource(c.sp, n.lastSon)
    c.stmts.subTree MirNode(kind: mnkFinally):
      scope(c.stmts):
        c.gen(n.lastSon[0])

  c.stmts.add endNode(mnkTry)

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
        c.stmts.add MirNode(kind: mnkLiteral, lit: it, typ: it.sym.typ)
      else:
        # XXX: we treat the operands as using pass-by-value. This is not
        #      really correct, but it makes the logic here simpler, and
        #      the whole facility is unsafe enough that one should not depend
        #      on these kind of details
        # don't capture values of symbols, emit and asm both expect the symbol
        # as-written to be embedded
        c.use genUse(c, it)

proc genComplexExpr(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates and emits the MIR code for assigning the value resulting from
  ## the complex expression `n` to destination `dest`
  assert not n.typ.isEmptyType()
  assert dest.isSome
  c.stmts.useSource(c.sp, n)

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


proc genPath(c: var TCtx, n: PNode; sink = false) =
  ## Prefers a name or path. `sink` tells whether the result is used in a
  ## sink context -- the flag is propagated through conversions such that
  ## in ``A(B(C(x: ...)))`` the object construction produces an owning
  ## value.
  case n.kind
  of nkSym:
    let s = n.sym
    case s.kind
    of skVar, skForVar, skLet, skResult, skParam, skConst, skTemp:
      c.staging.add nameNode(s)
    else:
      unreachable()
  of nkBracketExpr:
    genBracketExpr(c, n)
  of nkDotExpr:
    let
      typ = n[0].typ.skipTypes(abstractInstTypeClass)
      sym = n[1].sym

    assert sym.kind == skField

    case typ.kind
    of tyObject:
      c.subTree MirNode(kind: mnkPathNamed, typ: n.typ, field: sym):
        genPath(c, n[0])
    of tyTuple:
      # always use lookup-by-position for tuples, even when they're accessed
      # with via name
      c.subTree MirNode(kind: mnkPathPos, typ: n.typ,
                        position: sym.position.uint32):
        genPath(c, n[0])
    else:
      unreachable(typ.kind)
  of nkObjUpConv:
    # discard conversions in the same direction that are used as the operand
    var arg = n[0]
    while arg.kind == nkObjUpConv:
      arg = arg[0]

    c.buildOp mnkPathConv, n.typ:
      genPath(c, arg, sink)
  of nkObjDownConv:
    c.buildOp mnkPathConv, n.typ:
      genPath(c, n[0], sink)
  of nkCheckedFieldExpr:
    {.warning: "missing implementation".}
    genPath(c, n[0])
  of nkDerefExpr:
    # the dereference ends the path/projection. We don't know
    # where the path is going to be used, so a read is always
    # performed on the operand
    c.buildOp mnkDeref, n.typ:
      c.use genRd(c, n[0])
  of nkHiddenDeref:
    # TODO: this needs a rethink. Duplicating the hidden deref code is
    #       not acceptable, but using a procedure also doesn't seem
    #       right
    case classifyBackendView(n[0].typ)
    of bvcSingle:
      # it's a deref of a view
      c.subTree MirNode(kind: mnkDerefView, typ: n.typ):
        c.use genRd(c, n[0])
    of bvcSequence:
      c.use genUse(c, n[0])
    of bvcNone:
      # it's a ``ref`` or ``ptr`` deref
      c.subTree MirNode(kind: mnkDeref, typ: n.typ):
        c.use genRd(c, n[0])
  of nkStmtListExpr:
    let x = unwrap(c, n)
    genPath(c, x)
  else:
    # expected a path element but got something else, e.g., ``call().x``
    c.use genRd(c, n, sink)

proc genx(c: var TCtx, n: PNode, consume: bool) =
  ## Generate and emits the raw MIR code for the given expression `n` into
  ## the staging buffer.
  ##
  ## `consume` indicates whether the expression is used in a 'consume' context,
  ## that is, whether ownership is requested over the resulting value. This
  ## information is used to decide whether or not constructor expressions yield
  ## a *unique* value (one that has single ownership over its content).
  ## TODO: update the doc comment
  c.stmts.useSource(c.sp, n)

  case n.kind
  of nkSym:
    let s = n.sym
    case s.kind
    of skVar, skForVar, skLet, skResult, skParam, skConst, skTemp:
      c.staging.add nameNode(s)
    of skProc, skFunc, skConverter, skMethod, skIterator:
      c.use procLit(s)
    else:
      unreachable(s.kind)

  of nkCallKinds:
    genCallOrMagic(c, n)
  of nkCharLit..nkNilLit, nkRange, nkNimNodeLit:
    c.use literal(n)
  of nkCheckedFieldExpr:
    # XXX: emitting the field check is currently delayed until ``cgirgen``,
    #      but this is wrong:
    #      1. field checks are always emitted in the CG IR, leaving
    #         omitting them again to the code generators
    #      2. the exceptional control-flow is hidden to the MIR
    #      3. the code generator emitting the checks relies on the
    #         structured control-flow constructs that are currently in use
    #      The field check logic needs to be emitted here.
    genPath(c, n)
  of nkDotExpr, nkObjUpConv, nkObjDownConv, nkBracketExpr:
    genPath(c, n)
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
      let start = c.staging.len
      genx(c, n[0])
      c.staging.nodes[start].typ = n.typ
    of bvcNone:
      # a normal address-of operation
      c.genOp mnkAddr, n.typ, n[0]

  of nkDerefExpr:
    c.buildOp mnkDeref, n.typ:
      c.use genUse(c, n[0])
  of nkHiddenDeref:
    case classifyBackendView(n[0].typ)
    of bvcSingle:
      # it's a deref of a view
      c.buildOp mnkDerefView, n.typ:
        c.use genUse(c, n[0])
    of bvcSequence:
      # it's a no-op
      genx(c, n[0])
    of bvcNone:
      # it's a ``ref`` or ``ptr`` deref
      c.buildOp mnkDeref, n.typ:
        c.use genUse(c, n[0])

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
        genPath(c, n[1], consume)
    elif n.typ.skipTypes(abstractVar).kind == tyOpenArray:
      # to-openArray conversion also reach here as ``nkHiddenSubConv``
      # sometimes
      c.genOp mnkToSlice, n.typ, n[1]
    else:
      # it's a conversion that produces a new rvalue
      c.genOp mnkConv, n.typ, n[1]
  of nkLambdaKinds:
    c.use procLit(n[namePos].sym)
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
      c.buildMagicCall mChckRange, n.typ:
        c.emitByVal genRd(c, n[0])
        c.emitByVal genRd(c, n[1])
        c.emitByVal genRd(c, n[2])
  of nkStringToCString, nkCStringToString:
    # undo the transformation done by ``transf``
    c.genOp mnkStdConv, n.typ, n[0]
  of nkBracket:
    let consume =
      if n.typ.skipTypes(abstractVarRange).kind == tySequence:
        # don't propagate the `consume` flag through sequence constructors.
        # A sequence constructor is only used for constant seqs
        false
      else:
        consume

    if n.typ.skipTypes(abstractRange).kind == tySequence and n.len == 0:
      c.use literal(n)
    else:
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
    genx(c, selectWhenBranch(n, goIsNimvm in c.options), consume)
  of nkPragmaBlock:
    genx(c, n.lastSon, consume)
  of nkStmtListExpr:
    for i in 0..<n.len-1:
      gen(c, n[i])

    genx(c, n[^1], consume)
  of ComplexExprs:
    # attempting to generate the code for a complex expression without a
    # destination specified -> assign the value resulting from it to a
    # temporary
    let tmp = getTemp(c, n.typ)

    genComplexExpr(c, n):
      Destination(kind: dkFrag, mnode: tmp.node, source: n, flags: {dfOwns, dfEmpty})

    c.use tmp
  else:
    unreachable(n.kind)

proc gen(c: var TCtx, n: PNode) =
  ## Generates and emits the MIR code for the statement `n`
  c.stmts.useSource(c.sp, n)

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

    c.stmts.add MirNode(kind: mnkBreak, label: id)
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
            genPath(c, x[0])

        c.use genUse(c, n[1])
        # c.staging.add MirNode(kind: mnkField, field: x[1].sym)
    else:
      # a normal assignment
      genAsgn(c, false, n[0], n[1])

  of nkFastAsgn:
    # for non-destructor-using types, ``nkFastAsgn`` means bitwise copy
    # (i.e. ``mnkFastAsgn``), but for types having a destructor attached, it's
    # a normal assignment
    # XXX: this is confusing and unintuitive behaviour. ``transf`` shouldn't
    #      insert ``nkFastAsgn`` as aggresively as it does now and instead
    #      let the move-analyser and cursor-inference take care of optimizing
    #      the copies away
    if hasDestructor(n[0].typ):
      genAsgn(c, false, n[0], n[1])
    else:
      genFastAsgn(c, n[0], n[1])

  of nkCallKinds:
    # calls are expressions, the void statement allows using them as
    # statements
    c.buildStmt mnkVoid:
      genCallOrMagic(c, n)
  of nkProcDef, nkFuncDef, nkIteratorDef, nkMethodDef, nkConverterDef:
    c.stmts.subTree MirNode(kind: mnkDef):
      c.stmts.add procNode(n[namePos].sym)
      c.stmts.add MirNode(kind: mnkNone)

  of nkDiscardStmt:
    if n[0].kind != nkEmpty:
      # XXX: the current meaning of discard is "use a value and extend the
      #      lifetime of a temporary", but its meaning could be changed to
      #      "forcefully end the lifetime of a value"
      let val = genUse(c, n[0])
      if val.node.kind in {mnkLocal, mnkGlobal, mnkParam}:
        # only emit a discard if the expression is something useable
        c.buildStmt mnkVoid:
          c.use val

  of nkNilLit:
    # a 'nil' literals can be used as a statement, in which case it is treated
    # as a ``discard``
    assert n.typ.isEmptyType()
  of nkCommentStmt, nkTemplateDef, nkMacroDef, nkImportStmt,
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
        c.stmts.useSource(c.sp, it)
        genAsmOrEmitStmt(c, mnkEmit, it[1])
      of wComputedGoto:
        # the MIR doesn't handle this directive, but the code generators
        # might. As such, we need to keep it via a ``mnkPNode``. Since the
        # directive might be combined with some other directive in a
        # single statement, we split it out into a standalone pragma statement
        # first
        # XXX: ideally, sem or transf would split pragma statement up
        c.stmts.useSource(c.sp, it)
        c.stmts.add MirNode(kind: mnkPNode, node: newTree(nkPragma, [it]))
      else:     discard

  of nkAsmStmt:
    genAsmOrEmitStmt(c, mnkAsm, n)
  of nkWhenStmt:
    # a ``when nimvm`` statement
    gen(c, selectWhenBranch(n, goIsNimvm in c.options))
  else:
    unreachable(n.kind)

proc genWithDest(c: var TCtx, n: PNode; dest: Destination) =
  ## Generates and emits the MIR code for an expression plus the code for
  ## assigning the resulting value to the given destination `dest`. `dest` can
  ## be 'none', in which case `n` is required to be a statement
  if dest.isSome:
    let n = canonicalExpr(n)
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


proc generateCode*(graph: ModuleGraph, options: set[GenOption], n: PNode,
                   code: var MirTree, source: var SourceMap) =
  ## Generates MIR code that is semantically equivalent to the expression or
  ## statement `n`, appending the resulting code and the corresponding origin
  ## information to `code` and `source`, respectively.
  assert code.len == source.map.len, "source map doesn't match with code"
  var c = TCtx(context: skUnknown, graph: graph, options: options)

  template swapState() =
    swap(c.sp.store, source.source)
    swap(c.stmts.source, source.map)
    swap(c.stmts.nodes, code)

  # for the duration of ``generateCode`` we move the state into ``TCtx``
  swapState()

  if n.typ.isEmptyType:
    gen(c, n)
  elif n.typ.kind == tyTypeDesc:
    # FIXME: this shouldn't happen, but type expressions are sometimes
    #        evaluated with the VM, such as the ``int`` in the type expression
    #        ``static int``. While it makes to allow evaluating type expression
    #        with the VM, in simple situtations like the example above, it's
    #        simpler, faster, and more intuitive to either evaluate them directly
    #        when analying the type expression or during ``semfold``
    c.stmts.useSource(c.sp, n)
    c.stmts.use genTypeExpr(c, n)
  else:
    c.stmts.useSource(c.sp, n)
    # TODO: restructure the ``mirgen`` API to use a dedicated procedure for
    #       generating expression code
    let v = genUse(c, n)
    c.stmts.use v

  assert c.stmts.nodes.len == c.stmts.source.len
  assert c.staging.nodes.len == 0, "nodes were not commited"

  # move the state back into the output parameters:
  swapState()

proc generateCode*(graph: ModuleGraph, owner: PSym, options: set[GenOption],
                   body: PNode): tuple[code: MirTree, source: SourceMap] =
  ## Generates MIR code that is semantically equivalent to `body` plus the
  ## ``SourceMap`` that associates each ``MirNode`` with the ``PNode`` it
  ## originated from.
  ##
  ## `owner` it the symbol of the entity (module or procedure) that `body`
  ## belongs to. If the owner is a procedure, `body` is expected to be the
  ## full body of the procedure.
  ##
  ## `isNimvm` indicates the branch of a ``when nimvm`` statement that code
  ## should be generated code for
  # XXX: this assertion can currently not be used, as the ``nfTransf`` flag
  #      might no longer be present after the lambdalifting pass
  #assert nfTransf in body.flags, "transformed AST is expected as input"

  var c = TCtx(context: owner.kind, graph: graph, options: options,
               userOptions: owner.options)
  c.sp = SourceProvider(active: (body, noneOpt(SourceId)))

  c.stmts.add MirNode(kind: mnkScope)
  if owner.kind in routineKinds:
    # add a 'def' for each ``sink`` parameter. This simplifies further
    # processing and analysis
    let params = owner.typ.n
    for i in 1..<params.len:
      let s = params[i].sym
      if s.typ.isSinkTypeForParam():
        c.stmts.subTree MirNode(kind: mnkDef):
          c.stmts.add MirNode(kind: mnkParam, typ: s.typ, sym: s)
          c.stmts.add MirNode(kind: mnkNone)

  gen(c, body)

  assert c.staging.nodes.len == 0
  c.stmts.add endNode(mnkScope)

  # set the origin information for the 'end' node added above:
  apply(c.stmts, prepareForUse(c.sp))

  assert c.stmts.nodes.len == c.stmts.source.len

  result[0] = move c.stmts.nodes
  result[1] = SourceMap(source: move c.sp.store, map: move c.stmts.source)