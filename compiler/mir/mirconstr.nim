## This module contains constructor procedures for the different kinds of
## ``MirNode``s plus convenience procedures for generating expressions.

import
  compiler/ast/[
    ast_types
  ],
  compiler/mir/[
    mirtrees
  ]

type
  Value* = distinct bool
    ## Used to mark a procedure as generating code that yields a value
  Sink* = distinct bool
    ## Used to mark a procedure as generating code that acts as a sink
  SinkAndValue* = distinct bool

  Cond = distinct bool
    ## Marks a procedure as being a meta-operand that acts as a condition.
  Predicate = distinct bool
    ## Depending on the boolean value, excludes the next item in the chain.

  ChainEnd = distinct bool
    ## Sentinel type used to mark an operator in a chain as ending the chain.
    ## No further operators can be chained after an operator marked as
    ## ``ChainEnd``

  EValue* = object
    ## Encapsulates information about the abstract value that results from
    ## an operation sequence. It's used as a way to transport said information
    ## between the generator procedures for operators.
    typ* {.cursor.}: PType

# -------- chain templates:
# the templates that provide the context for the mini DSL

template discardTypeCheck[T](x: T) =
  ## Helper to discard the expression `x` while still requiring it to be of
  ## the given type. The templates making use of this helper can't use typed
  ## parameters directly, as the arguments (which require a ``value`` symbol
  ## to exist) would be sem'-checked before the `value` symbol is injected
  discard x

template chain*(buf: var MirNodeSeq, x: untyped) =
  ## Provides the context for chaining operators together via ``=>`` that end
  ## in a value sink
  block:
    var value {.inject, used.}: EValue
    template buffer: untyped {.inject, used.} = buf
    discardTypeCheck[ChainEnd](x)

template forward*(buf: var MirNodeSeq, x: untyped) =
  ## Provides the context for chaining operators together via ``=>`` that end
  ## in a *value*. This is used in places where the consuming operator can't be
  ## expressed as part of the chain and is expected to be emitted immediately
  ## after
  block:
    var value {.inject, used.}: EValue
    template buffer: untyped {.inject, used.} = buf

    type T = Value or EValue
    discardTypeCheck[T](x)

template eval*(buf: var MirNodeSeq, x: untyped): EValue =
  ## Similar to ``forward``, but returns the resulting ``EValue`` to be used
  ## as the input to another generator chain
  block:
    var value {.inject, used.}: EValue
    template buffer: untyped {.inject, used.} = buf
    discardTypeCheck[Value](x)
    value

template `=>`*(a: EValue, b: SinkAndValue): Value =
  mixin value
  value = a
  discard b
  Value(false)

template `=>`*(a: Value, b: Sink): ChainEnd =
  discard a
  discard b
  ChainEnd(false)

template `=>`*(a: EValue, b: Sink): ChainEnd =
  mixin value
  value = a
  discard b
  ChainEnd(false)

template `=>`*(v: Value, c: Cond): Predicate =
  discard v
  Predicate(c)

template `=>`*(v: EValue, c: Cond): Predicate =
  value = v
  Predicate(c)

template `=>`*(v: Predicate, sink: SinkAndValue): Value =
  if bool(v):
    discard sink
  Value(false)

template `=>`*(v: Value, sink: SinkAndValue): Value =
  discard v
  discard sink
  Value(false)

template follows*(): Sink =
  ## A pseudo-sink used to indicate that the actual output expression is
  ## either generated separately next or is already present and, in the context
  ## where ``follows`` is used, comes next
  Sink(false)

# ----------- atoms:
# The routines representing DSL operands that cannot be decomposed further.
# They append to a provided node buffer and update the associated ``EValue``
# instance. The routines are not very useful on their on own -- their
# integration into the chain DSL is what makes them ergonomic to use.

{.push inline.}

func emit*(s: var MirNodeSeq; n: sink MirNode): EValue =
  ## Adds `n` to the node sequences, with its type providing
  ## the initial type information for the result.
  result = EValue(typ: n.typ)
  s.add n

func procLit*(s: var MirNodeSeq, sym: PSym): EValue =
  s.add MirNode(kind: mnkProc, typ: sym.typ, sym: sym)
  result = EValue(typ: sym.typ)

func genTypeLit*(s: var MirNodeSeq, t: PType): EValue =
  s.add MirNode(kind: mnkType, typ: t)
  result = EValue(typ: t)

func genLit*(s: var MirNodeSeq; n: PNode): EValue =
  s.add MirNode(kind: mnkLiteral, typ: n.typ, lit: n)
  result = EValue(typ: n.typ)

func constr*(s: var MirNodeSeq, typ: PType): EValue =
  s.add MirNode(kind: mnkConstr, typ: typ)
  result = EValue(typ: typ)

func tempNode*(s: var MirNodeSeq, typ: PType, id: TempId): EValue =
  s.add MirNode(kind: mnkTemp, typ: typ, temp: id)
  result = EValue(typ: typ)

func symbol*(s: var MirNodeSeq, kind: range[mnkConst..mnkLocal],
             sym: PSym): EValue =
  s.add MirNode(kind: kind, typ: sym.typ, sym: sym)
  result = EValue(typ: sym.typ)

func opParam*(s: var MirNodeSeq, i: uint32, typ: PType): EValue =
  assert typ != nil
  s.add MirNode(kind: mnkOpParam, param: i, typ: typ)
  result = EValue(typ: typ)

func magicCall*(s: var MirNodeSeq, m: TMagic, typ: PType): EValue =
  assert typ != nil
  s.add MirNode(kind: mnkMagic, typ: typ, magic: m)
  result = EValue(typ: typ)

# input/output:

func tag*(s: var MirNodeSeq, effect: EffectKind, val: var EValue) =
  s.add MirNode(kind: mnkTag, effect: effect, typ: val.typ)

func castOp*(s: var MirNodeSeq, typ: PType, val: var EValue) =
  s.add MirNode(kind: mnkCast, typ: typ)
  val.typ = typ

func stdConvOp*(s: var MirNodeSeq, typ: PType, val: var EValue) =
  s.add MirNode(kind: mnkStdConv, typ: typ)
  val.typ = typ

func convOp*(s: var MirNodeSeq, typ: PType, val: var EValue) =
  s.add MirNode(kind: mnkConv, typ: typ)
  val.typ = typ

func addrOp*(s: var MirNodeSeq, typ: PType, val: var EValue) =
  s.add MirNode(kind: mnkAddr, typ: typ)
  val.typ = typ

func viewOp*(s: var MirNodeSeq, typ: PType, val: var EValue) =
  s.add MirNode(kind: mnkView, typ: typ)
  val.typ = typ

func derefOp*(s: var MirNodeSeq, typ: PType, val: var EValue) =
  s.add MirNode(kind: mnkDeref, typ: typ)
  val.typ = typ

func derefViewOp*(s: var MirNodeSeq, typ: PType, val: var EValue) =
  s.add MirNode(kind: mnkDerefView, typ: typ)
  val.typ = typ

func pathObj*(s: var MirNodeSeq, field: PSym, val: var EValue) =
  assert field.kind == skField
  s.add MirNode(kind: mnkPathNamed, typ: field.typ, field: field)
  val.typ = field.typ

func pathPos*(s: var MirNodeSeq, elemType: PType, position: uint32, val: var EValue) =
  s.add MirNode(kind: mnkPathPos, typ: elemType, position: position)
  val.typ = elemType

func pathVariant*(s: var MirNodeSeq, objType: PType, field: PSym, val: var EValue) =
  s.add MirNode(kind: mnkPathVariant, typ: objType, field: field)
  val.typ = objType

func unaryMagicCall*(s: var MirNodeSeq, m: TMagic, typ: PType, val: var EValue) =
  assert typ != nil
  s.add MirNode(kind: mnkMagic, typ: typ, magic: m)
  val.typ = typ

# sinks:

func arg*(s: var MirNodeSeq, val: EValue) =
  s.add MirNode(kind: mnkArg, typ: val.typ)

func consume*(s: var MirNodeSeq, val: EValue) =
  s.add MirNode(kind: mnkConsume, typ: val.typ)

func name*(s: var MirNodeSeq, val: EValue) =
  s.add MirNode(kind: mnkName, typ: val.typ)

func genVoid*(s: var MirNodeSeq, val: EValue) =
  s.add MirNode(kind: mnkVoid)

# special operators:

template predicate*(val: bool): Cond =
  ## If `val` evaluates to 'false', the operand following next in the chain
  ## is not evaluated.
  Cond(val)

{.pop.} # inline

# ----------- chain DSL adapters:

template genInputAdapter1(name, arg1: untyped) =
  template `name`*(arg1: untyped): EValue =
    mixin buffer
    name(buffer, arg1)

template genInputAdapter2(name, arg1, arg2: untyped) =
  template `name`*(arg1, arg2: untyped): EValue =
    mixin buffer
    name(buffer, arg1, arg2)

template genValueAdapter1(name, arg1: untyped) =
  template `name`*(arg1: untyped): SinkAndValue =
    mixin value, buffer
    name(buffer, arg1, value)
    SinkAndValue(false)

template genValueAdapter2(name, arg1, arg2: untyped) =
  template `name`*(arg1, arg2: untyped): SinkAndValue =
    mixin value, buffer
    name(buffer, arg1, arg2, value)
    SinkAndValue(false)

template genSinkAdapter(name: untyped) =
  template `name`*(): Sink =
    mixin value, buffer
    name(buffer, value)
    Sink(false)

# generate the adapters:
genInputAdapter1(emit, n)
genInputAdapter1(procLit, sym)
genInputAdapter1(genTypeLit, n)
genInputAdapter1(genLit, n)
genInputAdapter1(constr, typ)
genInputAdapter2(tempNode, typ, id)
genInputAdapter2(symbol, kind, sym)
genInputAdapter2(opParam, i, typ)
genInputAdapter2(magicCall, typ, id)

genValueAdapter1(tag, effect)
genValueAdapter1(castOp, typ)
genValueAdapter1(stdConvOp, typ)
genValueAdapter1(convOp, typ)
genValueAdapter1(addrOp, typ)
genValueAdapter1(viewOp, typ)
genValueAdapter1(derefOp, typ)
genValueAdapter1(derefViewOp, typ)
genValueAdapter1(pathObj, field)
genValueAdapter2(pathPos, typ, pos)
genValueAdapter2(pathVariant, typ, field)
genValueAdapter2(unaryMagicCall, m, typ)

genSinkAdapter(arg)
genSinkAdapter(name)
genSinkAdapter(consume)
genSinkAdapter(genVoid)

# --------- node constructors:

func procNode*(s: PSym): MirNode {.inline.} =
  assert s.kind in routineKinds
  MirNode(kind: mnkProc, sym: s)

func magic*(m: TMagic, typ: PType; n: PNode = nil): MirNode {.inline.} =
  MirNode(kind: mnkMagic, typ: typ, magic: m)

template endNode*(k: MirNodeKind): MirNode =
  MirNode(kind: mnkEnd, start: k)

# --------- tree generation utilities:

template subTree*(tree: var MirTree, n: MirNode, body: untyped) =
  let start = tree.len
  tree.add n
  body
  # note: don't use `n.kind` here as that would evaluate `n` twice
  tree.add endNode(tree[start].kind)

template stmtList*(tree: var MirTree, body: untyped) =
  tree.subTree MirNode(kind: mnkStmtList):
    body

template scope*(tree: var MirTree, body: untyped) =
  tree.subTree MirNode(kind: mnkScope):
    body

template argBlock*(tree: var MirTree, body: untyped) =
  tree.subTree MirNode(kind: mnkArgBlock):
    body