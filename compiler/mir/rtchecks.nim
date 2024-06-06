## Implements the MIR passes for lowering the various run-time check magic
## calls into comparisons + calls to runtime procedures. The lowerings are only
## applicable for backends using the runtime for C-like targets.

import
  std/[
    options
  ],
  compiler/ast/[
    ast_types,
    ast_query,
    lineinfos,
    types
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/mir/[
    mirbodies,
    mirchangesets,
    mirconstr,
    mirenv,
    mirtrees,
    mirtypes,
    sourcemaps
  ],
  compiler/utils/[
    int128,
    idioms
  ]

import compiler/front/options as comp_options
# XXX: no source position inspection should take place here
from compiler/front/msgs import toFileLineCol

# shorten some common parameter declarations:
using
  body: MirBody
  tree: MirTree
  call: NodePosition
  graph: ModuleGraph
  bu: var MirBuilder
  env: var MirEnv

template subTree(bu; k: MirNodeKind, t: TypeId, body: untyped) =
  bu.subTree MirNode(kind: k, typ: t):
    body

template buildIf(bu; cond: Value, body: untyped) =
  bu.buildIf (;bu.use(cond)):
    bu.scope:
      body

template buildIfNot(bu; cond: Value, body: untyped) =
  let c = bu.wrapTemp BoolType:
    bu.buildMagicCall mNot, BoolType:
      bu.emitByVal cond

  bu.buildIf(c):
    body

template emitCall(bu; tree; call; prc: ProcedureId, arguments: untyped) =
  ## Emits a void call of `prc`, inherting the checked-ness from `call`.
  bu.subTree mnkVoid:
    # if the input call is a checked call, so is the new call
    bu.subTree tree[call].kind, VoidType:
      bu.add MirNode(kind: mnkImmediate, imm: 0)
      bu.add procNode(prc) # callee
      arguments # custom arguments

      if tree[call].kind == mnkCheckedCall:
        # copy the jump target
        bu.emitFrom(tree, tree.last(call))

proc addCompilerProc(env; graph; name: string): ProcedureId =
  env.procedures.add(graph.getCompilerProc(name))

proc makeLiteral(env; kind: MirNodeKind, val: Int128, typ: TypeId): Value =
  literal(kind, env.getOrIncl(val.toUInt.BiggestUInt), typ)

proc getInt(env: MirEnv, n: MirNode): Int128 =
  case n.kind
  of mnkIntLit:  toInt128 env.getInt(n.number)
  of mnkUIntLit: toInt128 env.getUInt(n.number)
  else:          unreachable()

proc emitRangeCheck(tree, call, graph, env, bu): Value =
  ## Emits the lowered version of range check `call`. A range check amounts to
  ## ``if val < a or b < val: raise``, but with comparisons always yielding
  ## false optimized away.
  proc comparison(bu; magic: TMagic, val: Value;
                  lo, hi: Option[Value]): Value {.nimcall.} =
    if lo.isSome:
      # ``cond = val < lo``
      result = bu.wrapTemp BoolType:
        bu.buildMagicCall magic, BoolType:
          bu.emitByVal val
          bu.emitByVal lo.unsafeGet

    if hi.isSome:
      if lo.isSome:
        # ``if not cond: cond = hi < val``
        bu.buildIfNot result:
          bu.subTree mnkAsgn:
            bu.use result
            bu.buildMagicCall magic, BoolType:
              bu.emitByVal hi.unsafeGet
              bu.emitByVal val
      else:
        # ``cond = hi < val``
        result = bu.wrapTemp BoolType:
          bu.buildMagicCall magic, BoolType:
            bu.emitByVal hi.unsafeGet
            bu.emitByVal val

  let
    input   = tree.argument(call, 0)
    lowVal  = tree.argument(call, 1)
    highVal = tree.argument(call, 2)
    outType = env[tree[call].typ].skipTypes(abstractRange)

  var inType = env[tree[input].typ]
  if inType.sym != nil and sfSystemModule in inType.sym.owner.flags and
     inType.sym.name.s == "csize":
    # redirect the underlying type of csize to uint
    # HACK: this works around the ``system.csize`` definition using the wrong
    #       underlying type (int instead of uint)
    inType = graph.getSysType(unknownLineInfo, tyUInt)

  result = bu.inline(tree, NodePosition input)

  let
    lo  = bu.inline(tree, NodePosition lowVal)
    hi  = bu.inline(tree, NodePosition highVal)

  var cond: Value
  if outType.kind in {tyFloat, tyFloat32, tyFloat64}:
    # float range checks are a bit special -- the operand is converted to the
    # target type *first*
    result = bu.wrapTemp tree[call].typ:
      bu.subTree mnkConv, tree[call].typ:
        bu.use result

    cond = comparison(bu, mLtF64, result, some(lo), some(hi))
  else:
    # only the bounds where a range error is possible need to be checked. For
    # the comparisons, the literal values representing the bounds need to be
    # converted to the input type first. Since the guaranteed-to-be-false
    # comparisons are omitted, this conversion is guaranteed to be safe (no
    # out-of-range error is possible)
    let
      lo =
        if firstOrd(graph.config, inType) < env.getInt(tree[lowVal]):
          some literal(tree[lowVal].kind, tree[lowVal].number, result.typ)
        else:
          none Value # low(in) >= lo -> no check needed
      hi =
        if lastOrd(graph.config, inType) > env.getInt(tree[highVal]):
          some literal(tree[highVal].kind, tree[highVal].number, result.typ)
        else:
          none Value # high(in) <= hi -> no check needed

    # if both checks can be omitted, no range check call should have been
    # emitted in the first place
    assert not(lo.isNone and hi.isNone)

    let magic = getMagicLessForType(inType.skipTypes(abstractRange)).lt
    cond = comparison(bu, magic, result, lo, hi)

  bu.buildIf cond:
    if inType.skipTypes(abstractRange).kind in {tyUInt, tyUInt64}:
      # the value could be outside the representable range for the raise
      # procedure's parameter, so use the no-args one
      bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseRangeErrorNoArgs")):
        discard
    else:
      let raiser =
        case outType.kind
        of tyUInt..tyUInt64, tyChar: "raiseRangeErrorU"
        of tyFloat..tyFloat64:       "raiseRangeErrorF"
        else:                        "raiseRangeErrorI"

      # XXX: result isn't necessarily correctly typed for the call argument...
      bu.emitCall(tree, call, env.addCompilerProc(graph, raiser)):
        bu.emitByVal result
        bu.emitByVal lo
        bu.emitByVal hi

proc emitNanCheck(tree; call; graph; env; bu) =
  ## For ``chckNaN(val)`` emit:
  ##   def _1 = eqF64(arg val, arg val)
  ##   def _2 = not(arg _1)
  ##   if _2:
  ##     raiseFloatInvalidOp()
  let cmp = bu.wrapTemp BoolType:
    bu.buildMagicCall mEqF64, BoolType:
      bu.subTree mnkArg:
        bu.emitFrom(tree, NodePosition tree.argument(call, 0))
      bu.subTree mnkArg:
        bu.emitFrom(tree, NodePosition tree.argument(call, 0))

  # if a float value is not equal to itself, it is not a number (=NaN)
  bu.buildIfNot cmp:
    bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseFloatInvalidOp")):
      discard

proc emitIndexCheck(tree; call; graph; env; bu) =
  ## Emits the lowered index check for `call`.
  let
    arrOperand = NodePosition tree.argument(call, 0)
    ty = env[tree[arrOperand].typ].skipTypes(abstractInst +
      tyUserTypeClasses + {tyLent, tyVar})

  if ty.kind == tyCstring:
    # no index check needed
    # XXX: index checks should be omitted for cstrings when using the C
    #      backend
    return

  let
    idxOperand = NodePosition tree.argument(call, 1)
    sizeType   = env.types.sizeType
    usizeType  = env.types.usizeType
    conf       = graph.config

  if ty.kind == tyArray and (firstOrd(conf, ty) != Zero or
     lengthOrd(conf, ty) > lastOrd(conf, env[sizeType])):
    # we cannot check against just the length; the lower and upper bound need
    # to be checked against separately
    let
      idxTyp = env[tree[idxOperand].typ]
      litKind = if isUnsigned(idxTyp): mnkUIntLit else: mnkIntLit
      lo     = firstOrd(conf, ty)
      hi     = lastOrd(conf, ty)
      first  = env.makeLiteral(litKind, lo, tree[idxOperand].typ)
      last   = env.makeLiteral(litKind, hi, tree[idxOperand].typ)
      ltOp   = getMagicLessForType(idxTyp.skipTypes(abstractRange +
                                                    tyUserTypeClasses)).lt

    var cond: Value
    if firstOrd(conf, idxTyp) < lo:
      # lower bound needs to be checked against
      cond = bu.wrapTemp BoolType:
        bu.buildMagicCall ltOp, BoolType:
          bu.subTree mnkArg:
            bu.emitFrom(tree, idxOperand)
          bu.emitByVal first

    if lastOrd(conf, idxTyp) > hi:
      # upper bound needs to be checked against
      proc check(bu; tree; m: TMagic, src: NodePosition, val: Value) {.nimcall.} =
        bu.buildMagicCall m, BoolType:
          bu.emitByVal val
          bu.subTree mnkArg:
            bu.emitFrom(tree, src)

      if firstOrd(conf, idxTyp) < lo:
        # the upper bound only needs to be tested when the lower bound check
        # succeeded
        bu.buildIfNot cond:
          bu.subTree mnkAsgn:
            bu.use cond
            check(bu, tree, ltOp, idxOperand, last)
      else:
        cond = bu.wrapTemp BoolType:
          check(bu, tree, ltOp, idxOperand, last)

    bu.buildIf cond:
      bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseIndexError3")):
        bu.subTree mnkArg:
          bu.emitFrom(tree, idxOperand)
        bu.emitByVal first
        bu.emitByVal last

  else:
    # if the first index is at 0 and the length is guaranteed to be in range
    # 0..high(int), an optimization is used: the operand is coverted to a
    # first, and then only a single comparison against the length is used.
    # This works because a negative index operand becomes a value > high(int)
    # after to-uint conversion
    var len: Value
    if ty.kind == tyArray:
      # the length is static
      len = env.makeLiteral(mnkUIntLit, lengthOrd(graph.config, ty), usizeType)
    else:
      # the length is dynamic
      let tmp = bu.wrapTemp sizeType:
        bu.buildMagicCall mLengthOpenArray, sizeType:
          bu.subTree mnkArg:
            bu.emitFrom(tree, arrOperand)

      # convert the length:
      len = bu.wrapTemp usizeType:
        bu.subTree mnkConv, usizeType:
          bu.use tmp

    # convert the index:
    let idx = bu.wrapTemp usizeType:
      bu.subTree mnkConv, usizeType:
        bu.emitFrom(tree, idxOperand)

    # compare the values:
    let cond = bu.wrapTemp BoolType:
      bu.buildMagicCall mLeU, BoolType:
        bu.emitByVal len
        bu.emitByVal idx

    bu.buildIf cond:
      let val = bu.wrapTemp usizeType:
        bu.subTree mnkSub, usizeType:
          bu.use len
          bu.use literal(mnkUIntLit, env.getOrIncl(1), usizeType)

      bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseIndexError2")):
        bu.subTree mnkArg:
          bu.emitFrom(tree, idxOperand)
        bu.emitByVal val

proc emitFieldCheck(tree; source: SourceMap; call; graph; env; bu) =
  ## For ``chckField(set, discr, invert, msg)`` emits the MIR equivalent of:
  ##   if not contains(set, discr):
  ##     raiseFieldError(msg, ...)
  let
    setVal   = bu.inline(tree, NodePosition tree.argument(call, 0))
    discrVal = bu.inline(tree, NodePosition tree.argument(call, 1))

  var cond = bu.wrapTemp BoolType:
    bu.buildMagicCall mInSet, BoolType:
      bu.emitByVal setVal
      bu.emitByVal discrVal

  # the third argument is a boolean indicating whether the test is inverted
  if env.getInt(tree[tree.argument(call, 2)]) == Zero:
    cond = bu.wrapTemp BoolType:
      bu.buildMagicCall mNot, BoolType:
        bu.emitByVal cond

  var msgVal: Value
  if optDeclaredLocs in graph.config.globalOptions:
    # XXX: this an inadequate hack for supporting showing the source line
    #      information as part of the error message, even when stack-traces are
    #      disabled. This needs to be replaced with a general solution that
    #      applies to all run-time checks
    # fetch the line information of the call, render it, and prepend it to the
    # message
    var msg = toFileLineCol(graph.config, source[tree[call].info].info)
    msg.add " "
    msg.add env[tree[tree.argument(call, 3)].strVal]
    msgVal = literal(env.getOrIncl(msg), StringType)
  else:
    # use the original message as-is
    msgVal = bu.inline(tree, NodePosition tree.argument(call, 3))

  bu.buildIf cond:
    let typ = env[discrVal.typ].skipTypes(abstractRange)
    var
      raiseProc: string
      extra = discrVal # the extra value to pass to the raise procedure

    case typ.kind
    of tyEnum:
      # turn the run-time enum value into a string using the compiler-generated
      # enum-to-string procedure for the type
      let prc = graph.getToStringProc(typ)
      extra = bu.wrapTemp StringType:
        bu.buildCall env.procedures.add(prc), StringType:
          bu.emitByVal discrVal

      raiseProc = "raiseFieldErrorStr"
    of tyChar:
      # XXX: needs to use a dedicated raise procedure, once the runtime
      #      supports it
      raiseProc = "raiseFieldErrorUInt"
    of tyBool:
      raiseProc = "raiseFieldErrorBool"
    of tyInt..tyInt64:
      raiseProc = "raiseFieldErrorInt"
    of tyUInt..tyUInt64:
      raiseProc = "raiseFieldErrorUInt"
    else:
      unreachable(typ.kind)

    bu.emitCall(tree, call, env.addCompilerProc(graph, raiseProc)):
      bu.emitByVal msgVal
      bu.emitByVal extra

proc emitObjectCheck(tree; call; graph; env; bu) =
  ## For ``chckObj(o, typ)`` emits:
  ##   def _1 = of(arg o[], arg typ)
  ##   def _2 = not(arg _1)
  ##   if _2:
  ##     raiseObjectConversionError()
  let
    arg = bu.inline(tree, NodePosition tree.argument(call, 0))
    typ = env[arg.typ].skipTypes(abstractInst + tyUserTypeClasses)

  let cond = bu.wrapTemp BoolType:
    bu.buildMagicCall mOf, BoolType:
      # dereference first. Object checks are always guarded by an ``!= nil``
      # check, so the pointer/ref is guaranteed to be non-nil
      bu.subTree mnkArg:
        bu.subTree mnkDeref, env.types.add(typ[^1]):
          bu.use arg
      bu.subTree mnkArg:
        bu.emitFrom(tree, NodePosition tree.argument(call, 1))

  bu.buildIfNot cond:
    bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseObjectConversionError")):
      discard

# XXX: currently cannot be moved to within ``emitCheckedBinaryIntOp`` due to a
#      csource compiler bug
const ArithChcks: array[mAddI..mModI, array[tyInt..tyInt64, string]] = block:
  # compile the names of the procedure at compile-time
  var res: array[mAddI..mModI, array[tyInt..tyInt64, string]]
  for op, it in res.mpairs:
    let base = case op
      of mAddI: "nimAddInt"
      of mSubI: "nimSubInt"
      of mMulI: "nimMulInt"
      of mDivI: "nimDivInt"
      of mModI: "nimModInt"

    # the inner array stores the per-width names, which is the base name
    # suffixed with the width
    const intKinds = [tyInt8, tyInt16, tyInt32, tyInt64]
    for i, k in intKinds.pairs:
      it[k] = base & $(8 shl i)

    # for the moment, there's still a generic-width version
    it[tyInt] = base

  res

proc emitCheckedBinaryIntOp(tree; call; graph; env; bu): Value =
  ## Emits the lowered version of a checked binary arithmetic operation.
  ## Usually, it looks like this:
  ##   def _1
  ##   def _2 = nimAddInt(arg a, arg b, name _1)
  ##   if _2:
  ##     raiseOverflow()
  ##   result = _1
  let
    magic = tree[call + 1].magic
    t = env[tree[call].typ].skipTypes(abstractRange)
    x = NodePosition tree.argument(call, 0)
    y = NodePosition tree.argument(call, 1)

  # divison-by-zero is checked for separately, so that a dedicated error can
  # be reported:
  if magic in {mDivI, mModI}:
    let cond = bu.wrapTemp BoolType:
      bu.buildMagicCall mEqI, BoolType:
        bu.subTree mnkArg:
          bu.emitFrom(tree, y)
        bu.emitByVal literal(mnkIntLit, env.getOrIncl(0), tree[x].typ)

    bu.buildIf cond:
      bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseDivByZero")):
        discard

  result = bu.allocTemp(tree[call].typ)
  bu.subTree mnkDef:
    bu.use result
    bu.add MirNode(kind: mnkNone)

  let kind = t.skipTypes({tyEnum}).kind
  if kind in {tyUInt..tyUInt64, tyBool}:
    # enums using an unsigned integer as the underlying type can reach here
    # (due to ``succ`` and ``pred`` lowering), and no integer overflow
    # check is performed for them -- the same goes for the bool typ
    # XXX: no checked operation should be emitted for bool and unsigned enum
    #      types in the first place
    bu.subTree mnkInit:
      bu.use result
      # only 'add' and 'sub' operations can reach here
      const Map = [mAddI: mnkAdd, mSubI: mnkSub]
      bu.subTree Map[magic], tree[call].typ:
        bu.emitFrom(tree, x)
        bu.emitFrom(tree, y)

  else:
    # emit a call to the checked arithmethic operation:
    let cond = bu.wrapTemp BoolType:
      bu.buildCall env.addCompilerProc(graph, ArithChcks[magic][kind]), BoolType:
        bu.subTree mnkArg:
          bu.emitFrom(tree, x)
        bu.subTree mnkArg:
          bu.emitFrom(tree, y)
        bu.emitByName(result, ekReassign)

    bu.buildIf cond:
      bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseOverflow")):
        discard

  # for enum types, it's checked that they stay within their valid range
  # XXX: this is unprincipled. Range and bool types are not considered, but
  #      without any clear reason as to why. Using range checks instead might
  #      be a better solution
  if t.kind == tyEnum:
    let litKind = if isUnsigned(t): mnkUIntLit else: mnkIntLit
    let cond = bu.wrapTemp BoolType:
      bu.buildMagicCall mLtEnum, BoolType:
        bu.emitByVal result
        bu.emitByVal env.makeLiteral(litKind, firstOrd(graph.config, t),
                                     result.typ)

    bu.buildIfNot cond:
      bu.subTree mnkAsgn:
        bu.use cond
        bu.buildMagicCall mLtEnum, BoolType:
          bu.emitByVal env.makeLiteral(litKind, lastOrd(graph.config, t),
                                       result.typ)
          bu.emitByVal result

    bu.buildIf cond:
      bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseOverflow")):
        discard

proc emitUnaryOverflowCheck(tree; call; graph; env; bu) =
  ## Emits the overflow check for an integer negation operation:
  ##   if x == low(x):
  ##     raiseOverflow()
  let
    typ = tree[call].typ
    min = firstOrd(graph.config, env[typ])
    cond = bu.wrapTemp BoolType:
      bu.buildMagicCall mEqI, BoolType:
        bu.subTree mnkArg:
          bu.emitFrom(tree, NodePosition tree.argument(call, 0))
        bu.emitByVal env.makeLiteral(mnkIntLit, min, typ)

  bu.buildIf cond:
    bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseOverflow")):
      discard

proc emitCheckedFloatOp(tree; call; graph; env; bu): Value =
  ## Emits the lowered version of a checked float arithmetic operation.
  ## Checked means that the result is tested for infinity.
  let typ = tree[call].typ
  const Map = [mAddF64: mnkAdd, mSubF64: mnkSub, mMulF64: mnkMul, mDivF64: mnkDiv]
  result = bu.wrapTemp typ:
    bu.subTree Map[tree[call + 1].magic], typ:
      bu.emitFrom(tree, NodePosition tree.argument(call, 0))
      bu.emitFrom(tree, NodePosition tree.argument(call, 1))

  # test for infinity by multiplying the result with 0.5 and comparing it
  # against the original result. If equal and the result was not 0, the result
  # must be +inf or -inf
  let cond = bu.wrapTemp BoolType:
    bu.buildMagicCall mEqF64, BoolType:
      bu.emitByVal result
      bu.emitByVal literal(mnkFloatLit, env.getOrIncl(0.0), typ)
  bu.buildIfNot cond:
    let cmp = bu.wrapTemp typ:
      bu.subTree mnkMul, typ:
        bu.use result
        bu.use literal(mnkFloatLit, env.getOrIncl(0.5), typ)
    bu.subTree mnkAsgn:
      bu.use cond
      bu.buildMagicCall mEqF64, BoolType:
        bu.emitByVal result
        bu.emitByVal cmp
    bu.buildIf cond:
      bu.emitCall(tree, call, env.addCompilerProc(graph, "raiseFloatOverflow")):
        bu.emitByVal result

proc lowerChecks*(body; graph; env; changes: var Changeset) =
  ## Lowers all magic calls implementing the run-time checks.
  template tree: MirTree = body.code

  for i, n in tree.pairs:
    if n.kind == mnkMagic:
      case n.magic
      of mChckRange:
        let call = tree.parent(i)
        var tmp: Value
        # insert the range check before the statement:
        changes.insert(tree, tree.parent(call), call, bu):
          tmp = emitRangeCheck(tree, call, graph, env, bu)
        # replace the original call expression with a conversion:
        changes.replaceMulti(tree, call, bu):
          bu.subTree mnkConv, tree[call].typ:
            bu.use tmp
      of mChckNaN:
        let call = tree.parent(i)
        # make sure to take the ``mnkVoid`` wrapper into account
        changes.replaceMulti(tree, tree.parent(call), bu):
          emitNanCheck(tree, call, graph, env, bu)
      of mChckIndex:
        let call = tree.parent(i)
        changes.replaceMulti(tree, tree.parent(call), bu):
          emitIndexCheck(tree, call, graph, env, bu)
      of mChckField:
        let call = tree.parent(i)
        # make sure to take the ``mnkVoid`` wrapper into account
        changes.replaceMulti(tree, tree.parent(call), bu):
          emitFieldCheck(tree, body.source, call, graph, env, bu)
      of mChckObj:
        let call = tree.parent(i)
        changes.replaceMulti(tree, tree.parent(call), bu):
          emitObjectCheck(tree, call, graph, env, bu)

      of mAddI, mSubI, mMulI, mModI, mDivI:
        let call = tree.parent(i)
        var tmp: Value
        changes.insert(tree, tree.parent(call), call, bu):
          tmp = emitCheckedBinaryIntOp(tree, call, graph, env, bu)
        changes.replaceMulti(tree, call, bu):
          bu.use tmp
      of mUnaryMinusI, mUnaryMinusI64:
        let call = tree.parent(i)
        changes.insert(tree, tree.parent(call), call, bu):
          emitUnaryOverflowCheck(tree, call, graph, env, bu)
        # replace with built-in negation operation:
        changes.replaceMulti(tree, call, bu):
          bu.subTree MirNode(kind: mnkNeg, typ: tree[call].typ):
            bu.emitFrom(tree, NodePosition tree.argument(call, 0))
      of mAddF64, mSubF64, mMulF64, mDivF64:
        let call = tree.parent(i)
        var tmp: Value
        changes.insert(tree, tree.parent(call), call, bu):
          tmp = emitCheckedFloatOp(tree, call, graph, env, bu)
        changes.replaceMulti(tree, call, bu):
          bu.use tmp
      else:
        discard "not relevant"
