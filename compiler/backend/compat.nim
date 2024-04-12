## This module contains:
## 1. routines duplicated from elsewhere and adjusted to work with ``CgNode``
##    instead of ``PNode``
## 2. routines for translating ``PNode`` to ``CgNode``
## 3. utility routines for ``CgNode`` of which usage should either be phased
##    out or that should be moved somewhere else
##
## The plan is to eventually replace all usages of the routines part of #1 and
## #2 with something else (e.g., by running analysis in an earlier phase,
## lowering earlier, etc.) and then to phase all of them out.

import
  compiler/ast/[
    ast_types,
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
    mirenv,
    mirtrees
  ],
  compiler/utils/[
    bitsets,
    idioms,
    int128
  ]

func lastSon*(n: CgNode): CgNode {.inline.} =
  # XXX: replace usages with `n[^1]`
  {.cast(noSideEffect).}:
    n.kids[^1]

proc skipConv*(n: CgNode): CgNode {.inline.} =
  result = n
  while result.kind in {cnkConv, cnkHiddenConv, cnkLvalueConv}:
    result = result.operand

func getInt*(n: CgNode): Int128 =
  case n.kind
  of cnkUIntLit: toInt128(cast[BiggestUInt](n.intVal))
  of cnkIntLit:  toInt128(n.intVal)
  else:          unreachable(n.kind)

proc getOrdValue*(n: CgNode): Int128 =
  case n.kind
  of cnkUIntLit:    toInt128(cast[BiggestUInt](n.intVal))
  of cnkIntLit:     toInt128(n.intVal)
  of cnkHiddenConv: getOrdValue(n.operand)
  else:             unreachable()

func getCalleeMagic*(env: MirEnv, callee: CgNode): TMagic {.inline.} =
  case callee.kind
  of cnkProc:  env[callee.prc].magic
  of cnkMagic: callee.magic
  else:        mNone

proc getMagic*(env: MirEnv, op: CgNode): TMagic {.inline.}  =
  case op.kind
  of cnkCall, cnkCheckedCall:
    getCalleeMagic(env, op[0])
  else:
    mNone

proc isDiscriminantField*(n: CgNode): bool =
  case n.kind
  of cnkFieldAccess:        sfDiscriminant in n[1].field.flags
  else:                     false

func isOfBranch*(n: CgNode): bool {.inline.} =
  n.kind == cnkBranch and n.len > 1


proc isDeepConstExpr*(n: CgNode): bool =
  case n.kind
  of cnkLiterals, cnkNilLit:
    result = true
  of cnkSetConstr, cnkArrayConstr, cnkClosureConstr, cnkTupleConstr, cnkRange:
    result = true
    for it in n.items:
      if not isDeepConstExpr(it):
        result = false
        break
  of cnkObjConstr:
    let t = n.typ.skipTypes({tyGenericInst, tyDistinct, tyAlias, tySink})
    if t.kind == tyRef:
      # ref-constructions are never constant
      return false

    result = true
    for it in n.items:
      if not isDeepConstExpr(it[1]):
        result = false
        break
  else:
    result = false

proc canRaiseConservative*(env: MirEnv, fn: CgNode): bool =
  ## Duplicate of `canRaiseConservative <ast_query.html#canRaiseConservative,PNode>`_.
  # ``mNone`` is also included in the set, therefore this check works even for
  # non-magic calls
  getCalleeMagic(env, fn) in magicsThatCanRaise

proc toBitSet*(conf: ConfigRef; s: CgNode): TBitSet =
  ## Duplicate of `toBitSet <nimsets.html#toBitSet,ConfigRef,PNode>`_
  bitSetInit(result, int(getSize(conf, s.typ)))

  var first, j: Int128
  first = firstOrd(conf, s.typ[0])
  for it in s.items:
    if it.kind == cnkRange:
      j = getOrdValue(it[0])
      while j <= getOrdValue(it[1]):
        bitSetIncl(result, toInt64(j - first))
        inc(j)
    else:
      bitSetIncl(result, toInt64(getOrdValue(it) - first))

proc newSymNode*(env: MirEnv, s: PSym): CgNode {.inline.} =
  case s.kind
  of skConst:
    CgNode(kind: cnkConst, info: s.info, typ: s.typ, cnst: env.constants[s])
  of skVar, skLet, skForVar:
    CgNode(kind: cnkGlobal, info: s.info, typ: s.typ, global: env.globals[s])
  of skProcKinds:
    CgNode(kind: cnkProc, info: s.info, typ: s.typ, prc: env.procedures[s])
  of skField:
    CgNode(kind: cnkField, info: s.info, typ: s.typ, field: s)
  else:
    unreachable(s.kind)

proc translate*(t: MirTree, env: MirEnv): CgNode =
  ## Compatibility routine for translating a MIR constant-expression (`t`) to
  ## a ``CgNode`` tree. Obsolete once the code generators use the MIR
  ## directly.
  proc translateAux(t: MirTree, i: var int, env: MirEnv): CgNode =
    template recurse(): CgNode =
      translateAux(t, i, env)

    template tree(k: CgNodeKind, body: untyped): CgNode =
      ## Convenience template for setting up the tree node and iterating the
      ## input node's child nodes.
      let res {.inject.} = newExpr(k, unknownLineInfo, n.typ)
      res.kids.newSeq(t[i - 1].len)
      for j in 0..<res.len:
        res.kids[j] = body

      inc i # consume the end node
      res

    let n {.cursor.} = t[i]
    inc i # advance to the first child node
    case n.kind
    of mnkObjConstr:
      tree cnkObjConstr:
        let field = lookupInType(n.typ, t[i].field.int)
        inc i # advance to the arg node
        CgNode(kind: cnkBinding, info: unknownLineInfo,
               kids: @[CgNode(kind: cnkField, field: field),
                       recurse()])
    of mnkArrayConstr, mnkSeqConstr:
      tree cnkArrayConstr:
        recurse()
    of mnkTupleConstr:
      tree cnkTupleConstr:
        recurse()
    of mnkClosureConstr:
      tree cnkClosureConstr:
        recurse()
    of mnkSetConstr:
      tree cnkSetConstr:
        recurse()
    of mnkRange:
      tree cnkRange:
        recurse()
    of mnkArg:
      let x = recurse()
      inc i # skip the end node
      x
    of mnkNilLit:
      CgNode(kind: cnkNilLit, info: unknownLineInfo, typ: n.typ)
    of mnkIntLit:
      CgNode(kind: cnkIntLit, info: unknownLineInfo, typ: n.typ,
             intVal: env.getInt(n.number))
    of mnkUIntLit:
      CgNode(kind: cnkUIntLit, info: unknownLineInfo, typ: n.typ,
             intVal: env.getInt(n.number))
    of mnkFloatLit:
      CgNode(kind: cnkFloatLit, info: unknownLineInfo, typ: n.typ,
             floatVal: env.getFloat(n.number))
    of mnkStrLit:
      CgNode(kind: cnkStrLit, info: unknownLineInfo, typ: n.typ,
             strVal: n.strVal)
    of mnkAstLit:
      CgNode(kind: cnkAstLit, info: unknownLineInfo, typ: n.typ,
             astLit: env[n.ast])
    of mnkProcVal:
      CgNode(kind: cnkProc, info: unknownLineInfo, prc: n.prc, typ: n.typ)
    of AllNodeKinds - ConstrTreeNodes + {mnkEnd, mnkField}:
      # 'end' nodes are skipped manually
      unreachable(n.kind)

  var i = 0
  translateAux(t, i, env)

proc pick*[T](n: CgNode, forInt, forFloat: T): T =
  ## Returns either `forInt` or `forFloat` depending on the type of `n`.
  case n.typ.skipTypes(abstractRange + tyUserTypeClasses + {tyEnum}).kind
  of tyInt..tyInt64, tyBool:   forInt
  of tyUInt..tyUInt64, tyChar: forInt
  of tyFloat..tyFloat64:       forFloat
  else:
    unreachable("not an integer or float type")

func numArgs*(n: CgNode): int {.inline.} =
  ## Returns the number of arguments for a call-like node. The callee
  ## is excluded.
  n.len - 1 - ord(n.kind == cnkCheckedCall)

func callLen*(n: CgNode): int {.inline.} =
  ## The number of sub-nodes in a call-like node, excluding the trailing jump
  ## target description.
  n.len - ord(n.kind == cnkCheckedCall)
