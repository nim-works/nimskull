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
  bu.subTree mnkIf:
    bu.use cond
    bu.subTree mnkScope:
      body

template buildIfNot(bu; cond: Value, body: untyped) =
  let c = bu.wrapTemp BoolType:
    bu.buildMagicCall mNot, BoolType:
      bu.emitByVal cond

  bu.subTree mnkIf:
    bu.use c
    body

template emitCall(bu; tree; call; prc: ProcedureId, arguments: untyped) =
  ## Emits a void call of `prc`, inherting the checked-ness from `call`.
  bu.subTree mnkVoid:
    # if the input call is a checked call, so is the new call
    bu.subTree tree[call].kind, VoidType:
      bu.add procNode(prc) # callee
      arguments # custom arguments

proc addCompilerProc(env; graph; name: string): ProcedureId =
  env.procedures.add(graph.getCompilerProc(name))

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
      of mChckField:
        let call = tree.parent(i)
        # make sure to take the ``mnkVoid`` wrapper into account
        changes.replaceMulti(tree, tree.parent(call), bu):
          emitFieldCheck(tree, body.source, call, graph, env, bu)
      else:
        discard "not relevant"
