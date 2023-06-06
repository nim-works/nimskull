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
## To shorten the emit code and also make it a bit easier to read and
## understand, a mini DSL realised through templates is used.
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
  compiler/sem/[
    typeallowed
  ],
  compiler/utils/[
    containers,
    idioms
  ],
  experimental/[
    dod_helpers
  ]

type
  EValue = object
    ## Encapsulates information about the abstract value that results from
    ## an operation sequence. It's used as a way to transport said information
    ## between the generator procedures for operators
    # XXX: maybe rename to ``Computation``?
    typ {.cursor.}: PType

  NodeSpan = HOslice[NodeIndex]
    ## Refers to a span of nodes in a ``Fragment``

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
      nodes: NodeSpan ## references a slice of nodes in the staging buffer
    of dkGen:
      node {.cursor.}: PNode

    flags: set[DestFlag]

  ChainEnd = distinct bool
    ## Sentinel type used to mark an operator in a chain as ending the chain.
    ## No further operators can be chained after an operator marked as
    ## ``ChainEnd``

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

  TCtx = object
    # output:
    stmts: CodeFragment

    # working state:
    staging: CodeFragment ## intermediate buffer for partially generated MIR
                          ## code

    blocks: seq[Block] ## the stack of active ``block``s. Used for looking up
                       ## break targets

    sp: SourceProvider

    tmpMap: Table[ItemId, TempId]
      ## maps the ID of ``skTemp`` symbols to the ``TempId`` used to refer to
      ## them in output

    numTemps: int  ## provides the ID for temporaries
    numLabels: int ## provides the ID to use for the next label

    # input:
    context: TSymKind ## what entity the input AST is part of (e.g. procedure,
                      ## macro, module, etc.). Used to allow or change how the
                      ## AST is interpreted in some places
    graph: ModuleGraph

    options: set[GenOption]

const
  abstractInstTypeClass = abstractInst + tyUserTypeClasses
  # TODO: this set shouldn't be needed. ``tyUserTypeClass`` and
  #       ``tyUserTypeClassInst`` should be turned into either aliases or
  #       ``tyGenericInst`` types when they're resolved
  ComplexExprs = {nkIfExpr, nkCaseStmt, nkBlockExpr, nkTryStmt}
    ## The expression that are treated as complex and which are transformed
    ## into assignments-to-temporaries

func halfOpen[T](x: Slice[T]): HOslice[T] {.inline.} =
  ## Constructs a half-open slice from a normal slice. `x` is expected to
  ## already represent a half-open slice
  HOslice[T](a: x.a, b: x.b)

template toOpenArray[T, I](x: seq[T], slice: Slice[I]): openArray[T] =
  let s = slice
  toOpenArray(x, s.a.int, s.b.int)

template toOpenArray[T, I](x: seq[T], slice: HOslice[I]): openArray[T] =
  let s = slice
  toOpenArray(x, s.a.int, s.b.int - 1)

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

func tempNode(typ: PType, id: TempId): MirNode {.inline.} =
  MirNode(kind: mnkTemp, typ: typ, temp: id)


func nextTempId(c: var TCtx): TempId =
  result = TempId(c.numTemps)
  inc c.numTemps

func nextLabel(c: var TCtx): LabelId =
  result = LabelId(c.numLabels + 1)
  inc c.numLabels

# ----------- CodeFragment API -------------

template add(f: var CodeFragment, n: MirNode) =
  f.nodes.add n

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

func addWithSource(frag: var CodeFragment, sp: var SourceProvider,
                   n: sink MirNode, src: PNode) =
  ## Adds `n` to `frag` and associates the node with `src`
  applySource(frag, sp)
  frag.nodes.add n
  frag.source.add sp.store.add(src)

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

template buildSlice(frag: var CodeFragment, sp: var SourceProvider,
                    body: untyped): NodeSpan =
  ## Creates a ``NodeSpan`` containing all code emitted into `frag` during the
  ## execution of `body`
  let nodeStart = frag.nodes.len.NodeIndex

  body

  applySource(frag, sp)
  halfOpen(nodeStart .. frag.nodes.len.NodeIndex)

func popSlice(frag: var CodeFragment, span: NodeSpan) =
  ## Pops (removes) the nodes part of `span` from `frag`
  assert span.b == frag.nodes.len.NodeIndex
  assert span.b == frag.source.len.NodeIndex

  frag.nodes.setLen(span.a)
  frag.source.setLen(span.a)

# -------------- DSL routines -------------

template genValueAdapter(name: untyped) {.dirty.} =
  template `name`(c: var TCtx): SinkAndValue =
    mixin value
    `name`(c, value)
    SinkAndValue(false)

template genValueAdapter1(name, arg1: untyped) {.dirty.} =
  template `name`(c: var TCtx, arg1: untyped): SinkAndValue =
    mixin value
    `name`(c, arg1, value)
    SinkAndValue(false)

template genValueAdapter2(name, arg1, arg2: untyped) {.dirty.} =
  template `name`(c: var TCtx, arg1, arg2: untyped): SinkAndValue =
    mixin value
    `name`(c, arg1, arg2, value)
    SinkAndValue(false)

template genSinkAdapter(name: untyped) {.dirty.} =
  template `name`(c: var TCtx): Sink =
    mixin value
    `name`(c, value)
    Sink(false)

template `=>`(a: EValue, b: SinkAndValue): Value =
  mixin value
  value = a
  discard b
  Value(false)

template `=>`(a: Value, b: Sink): ChainEnd =
  discard a
  discard b
  ChainEnd(false)

template `=>`(a: EValue, b: Sink): ChainEnd =
  mixin value
  value = a
  discard b
  ChainEnd(false)

template discardTypeCheck[T](x: T) =
  ## Helper to discard the expression `x` while still requiring it to be of
  ## the given type. The templates making use of this helper can't use typed
  ## parameters directly, as the arguments (which require a ``value`` symbol
  ## to exist) would be sem'-checked before the `value` symbol is injected
  discard x

template chain(x: untyped) =
  ## Provides the context for chaining operators together via ``=>`` that end
  ## in a value sink
  block:
    var value {.inject, used.}: EValue
    discardTypeCheck[ChainEnd](x)

template forward(x: untyped) =
  ## Provides the context for chaining operators together via ``=>`` that end
  ## in *value*. This is used in places where the consuming operator can't be
  ## expressed as part of the chain and is expected to be emitted immediately
  ## after
  block:
    var value {.inject, used.}: EValue

    type T = Value or EValue
    discardTypeCheck[T](x)

template eval(x: untyped): EValue =
  ## Similar to ``forward``, but returns the resulting ``EValue`` to be used
  ## as the input to another generator chain
  block:
    var value {.inject, used.}: EValue
    discardTypeCheck[Value](x)
    value


template argBlock(f: var CodeFragment, body: untyped) =
  f.nodes.argBlock:
    body

template scope(f: var CodeFragment, body: untyped) =
  f.nodes.scope:
    body

template stmtList(f: var CodeFragment, body: untyped) =
  f.nodes.stmtList:
    body

func arg(c: var TCtx, val: EValue) =
  c.stmts.add MirNode(kind: mnkArg, typ: val.typ)

func consume(c: var TCtx, val: EValue) =
  c.stmts.add MirNode(kind: mnkConsume, typ: val.typ)

func name(c: var TCtx, val: EValue) =
  c.stmts.add MirNode(kind: mnkName, typ: val.typ)

func genVoid(c: var TCtx, val: EValue) =
  c.stmts.add MirNode(kind: mnkVoid)

func tag(c: var TCtx, effect: EffectKind, val: var EValue) =
  c.stmts.add MirNode(kind: mnkTag, effect: effect, typ: val.typ)

func modify(c: var TCtx, val: var EValue) =
  tag(c, ekMutate, val)

func outOp(c: var TCtx, val: var EValue) =
  tag(c, ekReassign, val)

func castOp(c: var TCtx, typ: PType, val: var EValue) =
  c.stmts.add MirNode(kind: mnkCast, typ: typ)
  val.typ = typ

func stdConvOp(c: var TCtx, typ: PType, val: var EValue) =
  c.stmts.add MirNode(kind: mnkStdConv, typ: typ)
  val.typ = typ

func convOp(c: var TCtx, typ: PType, val: var EValue) =
  c.stmts.add MirNode(kind: mnkConv, typ: typ)
  val.typ = typ

func addrOp(c: var TCtx, typ: PType, val: var EValue) =
  c.stmts.add MirNode(kind: mnkAddr, typ: typ)
  # don't change ownership. If the l-value is owned so is the resulting
  # pointer
  val.typ = typ

func viewOp(c: var TCtx, typ: PType, val: var EValue) =
  c.stmts.add MirNode(kind: mnkView, typ: typ)
  val.typ = typ

func derefOp(c: var TCtx, typ: PType, val: var EValue) =
  c.stmts.add MirNode(kind: mnkDeref, typ: typ)
  val.typ = typ

func derefViewOp(c: var TCtx, typ: PType, val: var EValue) =
  c.stmts.add MirNode(kind: mnkDerefView, typ: typ)
  val.typ = typ

func pathObj(c: var TCtx, field: PSym, val: var EValue) =
  assert field.kind == skField
  c.stmts.add MirNode(kind: mnkPathNamed, typ: field.typ, field: field)
  val.typ = field.typ

func pathPos(c: var TCtx, elemType: PType, position: uint32, val: var EValue) =
  c.stmts.add MirNode(kind: mnkPathPos, typ: elemType, position: position)
  val.typ = elemType

func pathVariant(c: var TCtx, objType: PType, field: PSym, val: var EValue) =
  let objType = objType.skipTypes(abstractInstTypeClass)
  c.stmts.add MirNode(kind: mnkPathVariant,
                      typ: objType,
                      field: field)
  val.typ = objType

func unaryMagicCall(c: var TCtx, m: TMagic, typ: PType, val: var EValue) =
  assert typ != nil
  c.stmts.add MirNode(kind: mnkMagic, typ: typ, magic: m)
  val.typ = typ

func magicCall(c: var TCtx, m: TMagic, typ: PType): EValue =
  assert typ != nil
  c.stmts.add MirNode(kind: mnkMagic, typ: typ, magic: m)
  result = EValue(typ: typ)

proc notOp(c: var TCtx, val: var EValue) =
  unaryMagicCall(c, mNot, getSysType(c.graph, c.sp.active.n.info, tyBool), val)

func tupleAccess(c: var TCtx, pos: uint32, typ: PType, val: var EValue) =
  c.stmts.add MirNode(kind: mnkPathPos, typ: typ, position: pos)
  val.typ = typ

# generate the adapters:
genSinkAdapter(arg)
genSinkAdapter(name)
genSinkAdapter(consume)
genSinkAdapter(genVoid)

genValueAdapter(modify)
genValueAdapter(outOp)
genValueAdapter(notOp)

genValueAdapter1(castOp, typ)
genValueAdapter1(stdConvOp, typ)
genValueAdapter1(convOp, typ)
genValueAdapter1(addrOp, typ)
genValueAdapter1(viewOp, typ)
genValueAdapter1(derefOp, typ)
genValueAdapter1(derefViewOp, typ)
genValueAdapter1(pathObj, field)
genValueAdapter1(tag, effect)

genValueAdapter2(pathPos, typ, pos)
genValueAdapter2(pathVariant, typ, field)
genValueAdapter2(tupleAccess, pos, typ)
# currently unused:
#genValueAdapter2(unaryMagicCall, m, typ)

func constr(c: var TCtx, typ: PType): EValue =
  c.stmts.add MirNode(kind: mnkConstr, typ: typ)
  result = EValue(typ: typ)

func tempNode(c: var TCtx, typ: PType, id: TempId): EValue =
  c.stmts.add tempNode(typ, id)
  result = EValue(typ: typ)

func procLit(c: var TCtx, s: PSym): EValue =
  c.stmts.add MirNode(kind: mnkProc, typ: s.typ, sym: s)
  result = EValue(typ: s.typ)

func genTypeLit(c: var TCtx, t: PType): EValue =
  c.stmts.add MirNode(kind: mnkType, typ: t)
  result = EValue(typ: t)

proc genEmpty(c: var TCtx, n: PNode): EValue =
  c.stmts.add MirNode(kind: mnkNone, typ: n.typ)
  result = EValue(typ: c.graph.getSysType(n.info, tyVoid))

func nameNode(s: PSym, n: PNode): MirNode =
  if sfGlobal in s.flags:
    MirNode(kind: mnkGlobal, typ: s.typ, sym: s)
  elif s.kind == skParam:
    MirNode(kind: mnkParam, typ: s.typ, sym: s)
  elif s.kind == skConst:
    MirNode(kind: mnkConst, typ: s.typ, sym: s)
  elif s.kind in {skVar, skLet, skForVar, skResult}:
    MirNode(kind: mnkLocal, typ: s.typ, sym: s)
  else:
    unreachable(s.kind)

func genLocation(c: var TCtx; n: PNode): EValue =
  let mn = nameNode(n.sym, n)
  c.stmts.add mn

  result = EValue(typ: mn.typ)

proc genLit(c: var TCtx; n: PNode): EValue =
  c.stmts.add MirNode(kind: mnkLiteral, typ: n.typ, lit: n)
  result = EValue(typ: n.typ)

func emit(dest: var CodeFragment, sp: var SourceProvider, src: CodeFragment,
          span: NodeSpan): EValue =
  ## Copies the nodes denoted by `span` from `src` to the end of `dest`
  assert span.len > 0, "empty source fragment"

  # make sure that the source information in `dest` is up-to-date before
  # appending
  applySource(dest, sp)

  dest.nodes.add toOpenArray(src.nodes, span)
  dest.source.add toOpenArray(src.source, span)

  result = EValue(typ: dest.nodes[^1].typ)

func genDefault(c: var TCtx, typ: PType): EValue =
  ## Emits a call to the ``default`` operator for the given `typ`
  argBlock(c.stmts): discard
  magicCall(c, mDefault, typ)

proc gen(c: var TCtx; n: PNode)
proc genx(c: var TCtx; n: PNode, consume: bool = false): EValue
proc genComplexExpr(c: var TCtx, n: PNode, dest: Destination)

proc genAsgn(c: var TCtx, dest: Destination, rhs: PNode)
proc genWithDest(c: var TCtx; n: PNode; dest: Destination)

template selectArg(c: var TCtx, useConsume: bool): Sink {.dirty.} =
  if useConsume: consume(c)
  else:          arg(c)

func getTemp(c: var TCtx, typ: PType): TempId =
  ## Allocates a new name for a temporary and emits a definition for it
  result = nextTempId(c)
  c.stmts.subTree MirNode(kind: mnkDef):
    c.stmts.add MirNode(kind: mnkTemp, temp: result, typ: typ)

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
  let label = nextLabel(c)
  genAsgn(c, dest, n[1]) # the left-hand side

  # condition:
  let v = emit(c.stmts, c.sp, c.staging, dest.nodes)
  if n[0].sym.magic == mOr:
    forward: v => notOp(c)

  c.stmts.subTree MirNode(kind: mnkIf):
    stmtList(c.stmts):
      genAsgn(c, dest, n[2]) # the right-hand side

proc genBracketExpr(c: var TCtx, n: PNode): EValue =
  let typ = n[0].typ.skipTypes(abstractInstTypeClass - {tyTypeDesc})
  case typ.kind
  of tyTuple:
    let i = n[1].intVal.int
    # due to all the type-related modifications done by ``transf``, it's safer
    # to lookup query the type from the tuple instead of using `n.typ`
    eval: genx(c, n[0]) => pathPos(c, typ[i], i.uint32)
  of tyArray, tySequence, tyOpenArray, tyVarargs, tyUncheckedArray, tyString,
     tyCstring:
    argBlock(c.stmts):
      chain: genx(c, n[0]) => name(c)
      chain: genx(c, n[1]) => arg(c)
    c.stmts.add MirNode(kind: mnkPathArray, typ: elemType(typ))

    EValue(typ: elemType(typ))
  else: unreachable()

proc genVariantAccess(c: var TCtx, n: PNode): (EValue, PNode) =
  assert n.kind == nkCheckedFieldExpr

  let access = n[0]
  assert access.kind == nkDotExpr

  var val = genx(c, access[0])
  # iterate the checks in reverse, as the outermost discriminator check is
  # rightmost
  # XXX: the "rightmost" part is an implementation detail of how
  #      ``nkCheckedFieldExpr`` nodes are generated by ``sem.nim``,
  #      depending on it here is brittle. It's okay for now and
  #      ``nkCheckedFieldExpr`` should ideally be removed anyways
  for i in countdown(n.len-1, 1):
    let check = n[i]
    assert check.kind in nkCallKinds
    # the check can be wrapped in a ``not`` call -- unwrap it first
    let inCall =
      if check[0].sym.magic == mNot: check[1]
      else:                          check

    pathVariant(c, access[0].typ, inCall[2].sym, val)

  result = (val, access[1])

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
      genTypeLit(c, n.typ)
  of nkBlockType:
    genTypeExpr(c, n.lastSon)
  of nkSym:
    case n.sym.kind
    of skType:
      genTypeLit(c, n.sym.typ)
    of skVar, skLet, skForVar, skTemp, skParam:
      # a first-class type value stored in a location
      genLocation(c, n)
    else:
      unreachable()
  of nkBracketExpr:
    # the type description of a generic type, e.g. ``seq[int]``
    genTypeLit(c, n.typ)
  of nkTupleTy, nkStaticTy, nkRefTy, nkPtrTy, nkVarTy, nkDistinctTy, nkProcTy,
     nkIteratorTy, nkSharedTy, nkTupleConstr:
    genTypeLit(c, n.typ)
  of nkTypeOfExpr, nkType:
    genTypeLit(c, n.typ)
  else:
    unreachable("not a type expression")

proc genCallee(c: var TCtx, n: PNode): EValue =
  ## Generates and emits the code for a callee expression
  if n.kind == nkSym and n.sym.kind in routineKinds:
    c.stmts.useSource(c.sp, n)
    procLit(c, n.sym)
  else:
    # an indirect call
    genx(c, n)

proc genArgExpression(c: var TCtx, formal: PType, n: PNode): EValue =
  ## Generates the code for an expression appearing in the context of an
  ## argument
  const OpenArrayLike = {tyOpenArray, tyVarargs}
  let
    t = formal.skipTypes(abstractVarRange)
    # skip the hidden addr - it's not needed by the IR
    n = if n.kind == nkHiddenAddr: n[0] else: n

  c.stmts.useSource(c.sp, n)

  result = c.genx(n, consume = formal.kind == tySink)

  if t.kind in OpenArrayLike and
     n.typ.skipTypes(abstractVarRange).kind notin OpenArrayLike:
    # restore the conversion that was eliminated by ``transf``
    result = eval: result => stdConvOp(c, formal.skipTypes(abstractVar))

proc genArg(c: var TCtx, formal: PType, n: PNode) =
  ## Generates and emits the MIR code for an argument expression plus the
  ## required argument sink. The `formal` type is needed for figuring out
  ## which sink to use
  let v = genArgExpression(c, formal, n)

  case formal.skipTypes(abstractRange-{tySink}).kind
  of tyVar:
    chain: v => modify(c) => name(c)
  of tySink:
    chain: v => consume(c)
  else:
    chain: v => arg(c)

proc finishCall(c: var TCtx, callee: PNode, fntyp: PType): EValue =
  # compute the effects the call has:
  var effects: set[GeneralEffect]
  if canRaiseConservative(callee):
    effects.incl geRaises

  if tfNoSideEffect notin fntyp.flags:
    effects.incl geMutateGlobal

  # to ease further return-type queries, a non-existing return-type (i.e. the
  # type is 'nil') is replaced with the ``void`` type
  let rtyp = typeOrVoid(c, fntyp[0])
  c.stmts.add MirNode(kind: mnkCall, typ: rtyp, effects: effects)

  result = EValue(typ: rtyp)

proc genCall(c: var TCtx, n: PNode): EValue =
  ## Generates and emits the MIR code for a call expression
  let fntyp = skipTypes(n[0].typ, abstractInst)

  c.stmts.add MirNode(kind: mnkArgBlock)

  chain: genCallee(c, n[0]) => arg(c)

  for i in 1..<n.len:
    # for procedures with unsafe varargs, the type of the argument expression
    # is used as the formal type (because it's the only type-related
    # information about the argument we have access to here)
    let t =
      if i < fntyp.len: fntyp[i]
      else:             n[i].typ

    if t.kind == tyTypeDesc and goGenTypeExpr in c.options:
      # generation of type expressions is requested. It's important that this
      # branch comes before the ``isCompileTimeOnly`` one, as a ``tyTypeDesc``
      # is treated as a compile-time-only type and would be omitted then
      chain: genTypeExpr(c, n[i]) => arg(c)
    elif t.isCompileTimeOnly:
      # don't translate arguments to compile-time-only parameters. To ease the
      # translation back to ``PNode``, we don't omit them completely but only
      # replace them with a node holding their type
      chain: genEmpty(c, n[i]) => arg(c)
    elif t.kind == tyVoid:
      # a ``void`` argument. We can't just generate an ``mnkNone`` node, as the
      # statement used as the argument can still have side-effects
      gen(c, n[i])
      chain: genEmpty(c, n[i]) => arg(c)
    elif i == 1 and not fntyp[0].isEmptyType() and
         not isHandleLike(t) and
         directViewType(fntyp[0]) != noView:
      # the procedure returns a view, but the first parameter is not something
      # that resembles a handle. We need to make sure that the first argument
      # (which the view could be created from), is passed by reference in that
      # case.
      chain: genArgExpression(c, t, n[i]) => name(c)
    else:
      genArg(c, t, n[i])

  c.stmts.add endNode(mnkArgBlock)

  finishCall(c, n[0], fntyp)

proc genMacroCallArgs(c: var TCtx, n: PNode, kind: TSymKind, fntyp: PType) =
  ## Generates the arguments for a macro/template call expression. `n` is
  ## expected to be a ``getAst`` expression that has been transformed to the
  ## internal representation. `kind` is the meta-routine's kind, and `fntyp`
  ## its signature.
  assert kind in {skMacro, skTemplate}
  chain: genCallee(c, n[1]) => arg(c)

  for i in 2..<n.len:
    let
      it = n[i]
      argTyp = it.typ.skipTypes(abstractInst - {tyTypeDesc})

    if argTyp.kind == tyTypeDesc:
      # the expression is a type expression, explicitly handle it there so that
      # ``genx`` doesn't have to
      chain: genTypeExpr(c, it) => arg(c)
    elif kind == skMacro:
      # we can extract the formal types from the signature
      genArg(c, fntyp[i - 1], it)
    elif kind == skTemplate:
      # we have to treat the arguments as normal expressions
      chain: genx(c, it) => arg(c)
    else:
      unreachable()

proc genMagic(c: var TCtx, n: PNode; m: TMagic): EValue =
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
    genArgExpression(c, n.typ.skipTypes(abstractVar), n)

  case m
  of mAnd, mOr:
    let tmp = getTemp(c, n.typ)
    let slice = buildSlice(c.staging, c.sp):
      c.staging.add tempNode(n.typ, tmp)

    genAndOr(c, n, Destination(kind: dkFrag, nodes: slice))

    c.staging.popSlice(slice)

    tempNode(c, n.typ, tmp)
  of mDefault:
    # use the canonical form:
    genDefault(c, n.typ)
  of mNew:
    # ``new`` has 2 variants. The standard one with a single argument, and the
    # unsafe version that also takes an extra ``size`` argument
    assert n.len == 3 or n.len == 2
    argBlock(c.stmts):
      # the first argument is the location storing the ``ref``. A new value is
      # assigned to it by ``new``, so the 'out' tag is used
      chain: genArgExpression(c, n[0].typ[1], n[1]) => outOp(c) => name(c)
      if n.len == 3:
        # the size argument
        chain: genArgExpression(c, n[0].typ[2], n[2]) => arg(c)

    magicCall(c, m, typeOrVoid(c, n.typ))
  of mWasMoved:
    # ``wasMoved`` has an effect that is not encoded by the parameter's type
    # (it kills the location), so we need to manually translate it
    argBlock(c.stmts):
      chain: argExpr(c, n[1]) => tag(c, ekKill) => name(c)

    magicCall(c, mWasMoved, typeOrVoid(c, n.typ))
  of mConStrStr:
    # the `mConStrStr` magic is very special. Nested calls to it are flattened
    # into a single call in ``transf``. It can't be passed on to ``genCall``
    # since the number of arguments doesn't match with the number of parameters
    argBlock(c.stmts):
      for i in 1..<n.len:
        chain: argExpr(c, n[i]) => arg(c)

    magicCall(c, mConStrStr, n.typ)
  of mRunnableExamples:
    # omit the ``runnableExamples`` call. The callsite of ``genMagic`` expects
    # that we emit something, so we emit an ``mnkEmpty`` node
    # TODO: call to ``runnableExamples`` shouldn't reach into ``mirgen``. If
    #       they do, it means that simple expression might sometimes not be
    #       detected as such, because ``canonicalExpr`` doesn't consider
    #       ``runnableExamples``
    genEmpty(c, n)

  # magics that use incomplete symbols (most of them are generated by
  # ``liftdestructors``):
  of mDestroy:
    # ``mDestroy`` magic calls might be incomplete symbols, so we have to
    # translate them manually
    argBlock(c.stmts):
      chain: argExpr(c, n[1]) => tag(c, ekMutate) => name(c)
    magicCall(c, m, typeOrVoid(c, n.typ))
  of mMove:
    # there exist two different types of ``mMove`` magic calls:
    # 1. normal move calls: ``move(x)``
    # 2. special move calls inserted by ``liftdestructors``, used for ``seq``s
    #    and ``string``s: ``move(dst, src, stmt)``
    case n.len
    of 2:
      # the first version
      genCall(c, n)
    of 4:
      # HACK: the ``stmt`` statement is not always evaluated, so treating it as
      #       a ``void`` argument is wrong. We also can't lower the call here
      #       already, since we don't have access to the ``seq``s
      #       implementation details and the lowering is also only required for
      #       the C target.
      #       Treating the statement as a ``void`` argument only works because,
      #       at the time of writing this comment, there are no MIR passes that
      #       inspect code in which a special ``move`` occurs.
      #       A more proper solution is to add a new magic (something like
      #       ``mMoveSeq``) that then gets lowered into the expected
      #       comparision + destructor call by a MIR pass that is only enabled
      #       for the C target

      argBlock(c.stmts):
        chain: argExpr(c, n[1]) => tag(c, ekReassign) => name(c)
        chain: argExpr(c, n[2]) => arg(c)

        gen(c, n[3])
        chain: genEmpty(c, n[3]) => arg(c)
      magicCall(c, m, typeOrVoid(c, n.typ))
    else:
      unreachable()
  of mNewSeq:
    # XXX: the first parameter is actually an ``out`` parameter -- the
    #      ``ekReassign`` effect could be used
    if n[0].typ == nil:
      argBlock(c.stmts):
        chain: argExpr(c, n[1]) => tag(c, ekMutate) => name(c)
        chain: argExpr(c, n[2]) => arg(c)
      magicCall(c, m, typeOrVoid(c, n.typ))
    else:
      genCall(c, n)
  of mInc, mSetLengthStr:
    if n[0].typ == nil:
      argBlock(c.stmts):
        chain: argExpr(c, n[1]) => tag(c, ekMutate) => name(c)
        chain: argExpr(c, n[2]) => arg(c)
      magicCall(c, m, typeOrVoid(c, n.typ))
    else:
      genCall(c, n)
  of mLtI, mSubI, mLengthSeq, mLengthStr, mAccessEnv, mAccessTypeField:
    if n[0].typ == nil:
      # simple translation. None of the arguments need to be passed by lvalue
      argBlock(c.stmts):
        for i in 1..<n.len:
          chain: argExpr(c, n[i]) => arg(c)

      magicCall(c, m, n.typ)
    else:
      genCall(c, n)
  of mAlignOf:
    # instances of the magic inserted by ``liftdestructors`` and ``alignof(x)``
    # calls where ``x`` is of an imported type with unknown alignment reach
    # here. The code-generators only care about the types in both cases, so
    # that's what we emit
    argBlock(c.stmts):
      # skip the surrounding typedesc
      chain: genTypeLit(c, n[1].typ.skipTypes({tyTypeDesc})) => arg(c)
    magicCall(c, m, n.typ)
  of mGetTypeInfoV2:
    if n[0].typ == nil:
      # the compiler-generated version always uses a type as the argument
      argBlock(c.stmts):
        chain: genTypeLit(c, n[1].typ) => arg(c)
      magicCall(c, m, n.typ)
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
      argBlock(c.stmts):
        genMacroCallArgs(c, n, skTemplate, callee.sym.typ)

      magicCall(c, m, n.typ)
    of skMacro:
      # rewrite ``getAst(macro(a, b, c))`` -> ``macro(a, b, c)``
      argBlock(c.stmts):
        # we can use the internal signature
        genMacroCallArgs(c, n, skMacro, callee.sym.internal)

      # treat a macro call as potentially raising and as modifying global
      # data. While not wrong, it is pessimistic
      c.stmts.add MirNode(kind: mnkCall, typ: n.typ,
                          effects: {geMutateGlobal, geRaises})
      EValue(typ: n.typ)
    else:
      unreachable()

  else:
    # no special transformation for the other magics:
    genCall(c, n)

proc genCallOrMagic(c: var TCtx, n: PNode): EValue =
  if n[0].kind == nkSym and (let s = n[0].sym; s.magic != mNone):
    genMagic(c, n, s.magic)
  else:
    genCall(c, n)

proc genSetConstr(c: var TCtx, n: PNode): EValue =
  argBlock(c.stmts):
    for it in n.items:
      chain: genx(c, it) => arg(c)

  result = constr(c, n.typ)

proc genArrayConstr(c: var TCtx, n: PNode, isConsume: bool): EValue =
  argBlock(c.stmts):
    for it in n.items:
      chain: genx(c, it, isConsume) => selectArg(c, isConsume)

  result = constr(c, n.typ)

proc genTupleConstr(c: var TCtx, n: PNode, isConsume: bool): EValue =
  assert n.typ.skipTypes(abstractVarRange-{tyTypeDesc}).kind == tyTuple

  argBlock(c.stmts):
    for it in n.items:
      chain: genx(c, skipColon(it), isConsume) => selectArg(c, isConsume)

  result = constr(c, n.typ)

proc genClosureConstr(c: var TCtx, n: PNode, isConsume: bool): EValue =
  argBlock(c.stmts):
    chain: genx(c, n[0]) => arg(c) # the procedure

    let v =
      if n[1].kind == nkNilLit:
        # it can happen that a ``nkNilLit`` has no type (i.e. its typ is nil) -
        # we ensure that the nil literal has the correct type
        # TODO: prevent a ``nkNilLit`` with no type information from being
        #       created instead
        genLit(c, newNodeIT(nkNilLit, n[1].info, c.graph.getSysType(n[1].info, tyNil)))
      else:
        genx(c, n[1], isConsume)

    chain: v => selectArg(c, isConsume) # the environment

  result = constr(c, n.typ)

proc genObjConstr(c: var TCtx, n: PNode, isConsume: bool): EValue =
  let isRef = n.typ.skipTypes(abstractInst).kind == tyRef

  # generate the argument block:
  argBlock(c.stmts):
    for i in 1..<n.len:
      let it = n[i]
      assert it.kind == nkExprColonExpr

      # only require require a unique value when constructing a ``ref`` and the
      # destination is not a ``.cursor`` field
      let useConsume =
        (isRef or isConsume) and
        sfCursor notin lookupFieldAgain(n.typ.skipTypes(abstractInst), it[0].sym).flags

      chain: genx(c, it[1], useConsume) => selectArg(c, useConsume)

  # emit the constructor:
  c.stmts.subTree MirNode(kind: mnkObjConstr, typ: n.typ, len: n.len-1):
    for i in 1..<n.len:
      let it = n[i]

      c.stmts.add MirNode(kind: mnkField, field: it[0].sym)

  result = EValue(typ: n.typ)


proc genRaise(c: var TCtx, n: PNode) =
  assert n.kind == nkRaiseStmt
  forward:
    if n[0].kind != nkEmpty: c.genx(n[0])
    else:                    c.genEmpty(n[0])

  c.stmts.add MirNode(kind: mnkRaise)

proc genReturn(c: var TCtx, n: PNode) =
  assert n.kind == nkReturnStmt
  if n[0].kind != nkEmpty:
    gen(c, n[0])

  c.stmts.add MirNode(kind: mnkReturn)

proc genAsgn(c: var TCtx, dest: Destination, rhs: PNode) =
  assert dest.isSome
  let owns = dfOwns in dest.flags
  argBlock(c.stmts):
    let v =
      case dest.kind
      of dkGen:  genx(c, dest.node)
      of dkFrag: emit(c.stmts, c.sp, c.staging, dest.nodes)
      else:      unreachable()

    chain: v => outOp(c) => name(c) # lhs
    chain: c.genx(rhs, consume = owns) => selectArg(c, owns) # rhs

  let kind =
    if owns:
      if dfEmpty in dest.flags: mnkInit
      else:                     mnkAsgn
    else:                       mnkFastAsgn

  c.stmts.add MirNode(kind: kind)

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
    argBlock(c.stmts):
      chain: genx(c, lhs) => outOp(c) => name(c)
      # only use 'consume' if the we're not assigning to a cursor
      chain: genx(c, rhs, consume=not isCursor) => selectArg(c, not isCursor)

    let kind =
      if isCursor:  mnkFastAsgn
      else:
        if isFirst: mnkInit
        else:       mnkAsgn

    c.stmts.add MirNode(kind: kind)

proc genFastAsgn(c: var TCtx, lhs, rhs: PNode) =
  argBlock(c.stmts):
    chain: genx(c, lhs) => outOp(c) => name(c)
    chain: genx(c, rhs) => arg(c)

  c.stmts.add MirNode(kind: mnkFastAsgn)

proc genLocDef(c: var TCtx, n: PNode) =
  ## Generates the 'def' construct for the entity provided by the symbol node
  ## `n`
  let s = n.sym

  c.stmts.useSource(c.sp, n)
  c.stmts.subTree MirNode(kind: selectDefKind(s)):
    case s.kind
    of skTemp:
      let id = nextTempId(c)
      c.tmpMap[s.itemId] = id

      c.stmts.add MirNode(kind: mnkTemp, typ: s.typ, temp: id)
    else:
      let kind =
        if sfGlobal in s.flags: mnkGlobal
        else:                   mnkLocal

      {.cast(uncheckedAssign).}:
        c.stmts.add MirNode(kind: kind, sym: s)

proc genLocInit(c: var TCtx, symNode: PNode, initExpr: PNode) =
  ## Generates the code for a location definition. `sym` is the symbol of the
  ## location and `initExpr` the initializer expression
  let
    sym = symNode.sym
    hasInitExpr = initExpr.kind != nkEmpty
    wantsOwnership = sfCursor notin sym.flags and
                     hasDestructor(sym.typ)

  assert sym.kind in {skVar, skLet, skTemp, skForVar}

  if sfCompileTime in sym.flags and goIsNimvm notin c.options:
    # XXX: ``.compileTime`` variables are currently lazily generated/emitted
    #      (that is, only when they're actually used in alive code). This
    #      approach has issues and needs to eventually be revisited, but for
    #      now, we omit their definitions in non-compile-time contexts in order
    #      to make the lazy-generation approach work
    return

  # if there's an initial value and the destination is non-owning, we pass the
  # value directly to the def
  if hasInitExpr and not wantsOwnership:
    # TODO: add a test for using a constructor expression for initializing a
    #       cursor
    forward: genx(c, initExpr, consume = false)

  genLocDef(c, symNode)

  if hasInitExpr and wantsOwnership:
    # a copy or sink can't be expressed via a pass-to-def -- we need to use
    # the assignment operator
    genAsgn(c, true, symNode, initExpr)

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
    let
      typ = initExpr.typ
      tmp = nextTempId(c)

    # XXX: ``mnkDefUnpack`` was meant as a workaround and shouldn't be used
    #      here. Passing each destination plus the tuple expression as
    #      arguments to a region that performs the unpacking would be a much
    #      cleaner solution and also speed up the move analysis a bit, because
    #      the lifetime of the temporary ends much earlier. The problem: the
    #      move analyser is not capable of analysing section parameters yet.
    #      Using a region would also mean an increase in the amount of nodes --
    #      ``mnkDefUnpack``, while a hack, is much more concise
    # generate the definition for the temporary:
    forward: genx(c, initExpr, consume = true)
    c.stmts.subTree MirNode(kind: mnkDefUnpack):
      c.stmts.add MirNode(kind: mnkTemp, typ: typ, temp: tmp)

    # generate the unpack logic:
    for i in 0..<numDefs:
      let lhs = n[i]

      if lhs.kind == nkSym:
        genLocDef(c, lhs)

      # generate the assignment:
      argBlock(c.stmts):
        chain: genx(c, lhs) => outOp(c) => name(c)
        chain: tempNode(c, typ, tmp) => tupleAccess(c, i.uint32, lhs.sym.typ) => consume(c)
      c.stmts.add MirNode(kind: mnkInit)

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
          argBlock(c.stmts):
            chain: genx(c, a[0]) => tag(c, ekReassign) => name(c)
            chain: genDefault(c, a[0].typ) => arg(c)
          c.stmts.add MirNode(kind: mnkInit)
      else:
        unreachable()

    else:
      unreachable(a.kind)


proc genWhile(c: var TCtx, n: PNode) =
  ## Generates the code for a ``nkWhile`` node. Because no conditional loops
  ## exist in the MIR, the the condition is translated to a ``break`` statement
  ## inside an ``if``
  c.stmts.add MirNode(kind: mnkRepeat)

  scope(c.stmts):
    # don't emit a branch for ``while true: ...``
    if not isTrue(n[0]):
      forward: genx(c, n[0]) => notOp(c) # condition
      c.stmts.subTree MirNode(kind: mnkIf):
        c.stmts.add MirNode(kind: mnkBreak)

    # use a nested scope for the body. This is important for scope finalizers,
    # as exiting the loop via the ``break`` emitted above (i.e. the loop
    # condition is false) must not run the finalizers (if present) for the
    # loop's body
    scope(c.stmts):
      c.gen(n[1])

  c.stmts.add endNode(mnkRepeat)

proc genBlock(c: var TCtx, n: PNode, dest: Destination) =
  ## Generates and emits the MIR code for a ``block`` expression or statement
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
    forward: genx(c, branch[0])
    c.stmts.subTree MirNode(kind: mnkIf):
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
      stmtList(c.stmts):
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

  forward: genx(c, n[0])

  let start = c.stmts.nodes.len
  c.stmts.add MirNode(kind: mnkCase)

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


proc genx(c: var TCtx, n: PNode, consume: bool): EValue =
  ## Generate and emits the MIR for the given expression `n`.
  ##
  ## `consume` indicates whether the expression is used in a 'consume' context,
  ## that is, whether ownership is requested over the resulting value. This
  ## information is used to decide whether or not constructor expressions yield
  ## a *unique* value (one that has single ownership over its content)
  c.stmts.useSource(c.sp, n)

  case n.kind
  of nkSym:
    let s = n.sym
    case s.kind
    of skVar, skForVar, skLet, skResult, skParam, skConst:
      genLocation(c, n)
    of skTemp:
      tempNode(c, s.typ, c.tmpMap[s.itemId])
    of skProc, skFunc, skConverter, skMethod, skIterator:
      procLit(c, s)
    of skGenericParam:
      case c.context
      of skUnknown:
        # HACK: during parameter type matching, sigmatch (``paramTypesMatchAux``)
        #       uses ``tryConstExpr`` in order to find out whether the argument
        #       expression to a ``static`` parameter is evaluatable at
        #       compile-time. If the expression contains a reference to an
        #       unresolved generic parameter, ``vmgen`` is expected to fail.
        #       The problem: ``mirgen`` is unable to report any errors, so we
        #       have to push the original ``skGenericParam`` symbol through
        #       the MIR stage to ``vmgen``
        #       The likely best solution is to introduce a dedicated analysis
        #       layer that makes sure that the AST passed to it is valid in the
        #       context of compile-time execution (i.e. does the checks that
        #       ``vmgen`` currently has to do). It would take place after
        #       ``semfold``.
        genLit(c, n)
      else:
        unreachable()

    else:
      unreachable(s.kind)

  of nkCallKinds:
    genCallOrMagic(c, n)
  of nkCharLit..nkNilLit, nkRange, nkNimNodeLit:
    genLit(c, n)
  of nkCheckedFieldExpr:
    let (val, rhs) = genVariantAccess(c, n)
    eval: val => pathObj(c, rhs.sym)
  of nkDotExpr:
    let
      v = genx(c, n[0])
      typ = n[0].typ.skipTypes(abstractInstTypeClass)
      sym = n[1].sym

    assert sym.kind == skField

    case typ.kind
    of tyObject:
      eval: v => pathObj(c, sym)
    of tyTuple:
      # always use lookup-by-position for tuples, even when they're accessed
      # with via name
      eval: v => pathPos(c, typ[sym.position], sym.position.uint32)
    else:
      unreachable(typ.kind)

  of nkBracketExpr:
    genBracketExpr(c, n)
  of nkObjDownConv, nkObjUpConv:
    eval: genx(c, n[0], consume) => convOp(c, n.typ)
  of nkAddr:
    eval: genx(c, n[0]) => addrOp(c, n.typ)
  of nkHiddenAddr:
    let v = genx(c, n[0])
    if directViewType(n.typ) != noView:
      # a view into the source operand is created
      eval: v => viewOp(c, n.typ)
    else:
      # a normal address-of operation
      eval: v => addrOp(c, n.typ)

  of nkDerefExpr:
    eval: genx(c, n[0]) => derefOp(c, n.typ)
  of nkHiddenDeref:
    let v = genx(c, n[0])
    if directViewType(n[0].typ) != noView:
      # it's a deref of a view
      eval: v => derefViewOp(c, n.typ)
    else:
      # it's a ``ref`` or ``ptr`` deref
      eval: v => derefOp(c, n.typ)

  of nkHiddenStdConv:
    eval: genx(c, n[1], consume) => stdConvOp(c, n.typ)
  of nkHiddenSubConv, nkConv:
    eval: genx(c, n[1], consume) => convOp(c, n.typ)
  of nkLambdaKinds:
    procLit(c, n[namePos].sym)
  of nkChckRangeF, nkChckRange64, nkChckRange:
    # turn the conversion back into an unchecked conversion
    # TODO: remove the ``nkConv|nkHiddenStdConv|nkHiddenSubConv`` ->
    #       ``nkChckRange`` transformation logic from ``transf`` -- it's
    #       unnecessary now
    eval: genx(c, n[0]) => convOp(c, n.typ)
  of nkStringToCString, nkCStringToString:
    # undo the transformation done by ``transf``
    # TODO: turn both of them into operators
    eval: genx(c, n[0]) => stdConvOp(c, n.typ)
  of nkBracket:
    let consume =
      if n.typ.skipTypes(abstractVarRange).kind == tySequence:
        # don't propagate the `consume` flag through sequence constructors.
        # A sequence constructor is only used for constant seqs
        false
      else:
        consume

    if n.typ.skipTypes(abstractRange).kind == tySequence and n.len == 0:
      genLit(c, n)
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
    eval: c.genx(n[1]) => castOp(c, n.typ)
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
    let slice = buildSlice(c.staging, c.sp):
      c.staging.add MirNode(kind: mnkTemp, typ: n.typ, temp: tmp)

    genComplexExpr(c, n):
      Destination(kind: dkFrag, nodes: slice, flags: {dfOwns, dfEmpty})

    c.staging.popSlice(slice)

    tempNode(c, n.typ, tmp)
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
    case n[0].kind
    of nkSym:
      let sym = n[0].sym
      # find the block with the matching label and use its ``LabelId``:
      for b in c.blocks.items:
        if b.label.id == sym.id:
          id = b.id
          break

      assert id.isSome, "break target missing"
    of nkEmpty:
      id = NoLabel
    else:
      unreachable()

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
      argBlock(c.stmts):
        # for nested record-cases, the discriminant access is wrapped in a
        # ``nkCheckedFieldExpr``, in which case it needs to be unwrapped
        # first
        let (val, rhs) =
          case n[0].kind
          of nkCheckedFieldExpr: genVariantAccess(c, n[0])
          else:                  (genx(c, x[0]), x[1])

        # the 'switch' operations expects a variant access as its input, so
        # using ``genx(c, x)`` would be wrong
        chain: val => pathVariant(c, x[0].typ, rhs.sym) => tag(c, ekInvalidate) => name(c)
        chain: genx(c, n[1]) => arg(c)

      c.stmts.add MirNode(kind: mnkSwitch)
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
    chain: genCallOrMagic(c, n) => genVoid(c)
  of nkProcDef, nkFuncDef, nkIteratorDef, nkMethodDef, nkConverterDef:
    c.stmts.subTree MirNode(kind: mnkDef):
      c.stmts.add procNode(n[namePos].sym)
      # XXX: this is a temporary solution. The current code-generators
      #      require procdef nodes in some cases, and instead of
      #      recreating them during ``astgen``, we carry the original nodes
      #      over
      c.stmts.add MirNode(kind: mnkPNode, node: n)

  of nkDiscardStmt:
    if n[0].kind != nkEmpty:
      chain: genx(c, n[0]) => genVoid(c)

  of nkNilLit:
    # a 'nil' literals can be used as a statement, in which case it is treated
    # as a ``discard``
    assert n.typ.isEmptyType()
  of nkCommentStmt, nkTemplateDef, nkMacroDef, nkImportStmt,
     nkIncludeStmt, nkStaticStmt, nkExportStmt, nkExportExceptStmt,
     nkTypeSection, nkMixinStmt, nkBindStmt, nkEmpty:
    discard "ignore"
  of nkConstSection:
    # const sections are not relevant for the MIR, but the IC implementation
    # needs them to be present
    c.stmts.add MirNode(kind: mnkPNode, node: n)
  of nkPragma:
    # if the pragma statement contains an ``.emit`` or ``.computeGoto``, it is
    # stored in the MIR as an ``mnkPNode`` -- otherwise it's discarded
    var hasInteresting = false
    for it in n:
      case whichPragma(it)
      of wEmit, wComputedGoto: hasInteresting = true; break
      else:     discard

    if hasInteresting:
      c.stmts.add MirNode(kind: mnkPNode, node: n)

  of nkAsmStmt:
    # these don't have a direct MIR counterpart
    c.stmts.add MirNode(kind: mnkPNode, node: n)
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
    discard genTypeExpr(c, n)
  elif n.typ.kind == tyFromExpr:
    assert goGenTypeExpr in options
    # a type expression that uses unresolved generic parameters. As we're unable
    # to report errors here, we push the expression through to ``vmgen`` as an
    # ``mnkLiteral``
    # TODO: we shouldn't have to do this here. ``paramTypesMatchAux`` should
    #       only try to run compile-time evaluation for expressions with no
    #       unknowns (e.g. unresolved generic parameters)
    c.stmts.useSource(c.sp, n)
    discard genLit(c, n)
  else:
    discard genx(c, n)

  assert c.stmts.nodes.len == c.stmts.source.len

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

  var c = TCtx(context: owner.kind, graph: graph, options: options)
  c.sp = SourceProvider(active: (body, noneOpt(SourceId)))

  c.stmts.add MirNode(kind: mnkScope)
  if owner.kind in routineKinds:
    # add a 'def' for each ``sink`` parameter. This simplifies further
    # processing and analysis
    let params = owner.typ.n
    for i in 1..<params.len:
      if params[i].sym.typ.isSinkTypeForParam():
        c.stmts.subTree MirNode(kind: mnkDef):
          c.stmts.add MirNode(kind: mnkParam, sym: params[i].sym)

  gen(c, body)

  c.stmts.add endNode(mnkScope)

  # set the origin information for the 'end' node added above:
  apply(c.stmts, prepareForUse(c.sp))

  assert c.stmts.nodes.len == c.stmts.source.len

  result[0] = move c.stmts.nodes
  result[1] = SourceMap(source: move c.sp.store, map: move c.stmts.source)