#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#
#

## included from cgen.nim

proc exitCall(p: BProc, call: CgNode) =
  ## Emits the exceptional control-flow related post-call logic.
  let isNoReturn = call[0].kind == cnkProc and
                   sfNoReturn in p.env[call[0].prc].flags
  if call.kind == cnkCheckedCall:
    if isNoReturn:
      # the callee raises and doesn't have a normal exit -> testing the error
      # flag is unnecessary
      if nimErrorFlagDisabled in p.flags:
        # don't jump to the error target. Both exception handlers and
        # finalizers require disabling error mode, but due to the flag being
        # inaccessible, that's not going to work
        # XXX: as an interim solution, skipping handlers is safer than
        #      attempting to execute them. Ultimately, the error flag needs
        #      to be available everywhere
        p.flags.incl beforeRetNeeded
        lineF(p, cpsStmts, "goto BeforeRet_;$n", [])
      else:
        # jump to the handler/finalizer
        lineF(p, cpsStmts, "$1$n", [raiseInstr(p, call[^1])])
    else:
      raiseExit(p, call[^1])
  elif isNoReturn:
    # mark the control-flow path following the call as unreachable
    if hasAssume in CC[p.config.cCompiler].props:
      lineF(p, cpsStmts, "__assume(0);$n", [])

proc fixupCall(p: BProc, le, ri: CgNode, d: var TLoc,
               callee, params: Rope) =
  genLineDir(p, ri)
  var pl = callee & ~"(" & params
  # getUniqueType() is too expensive here:
  var typ = skipTypes(ri[0].typ, abstractInst)
  if typ[0] != nil:
    if isInvalidReturnType(p.config, typ[0]):
      if params != "": pl.add(~", ")
      # the destination is guaranteed to be either a temporary or an lvalue
      # that can be modified in-place
      if true:
        # resetting the result location is the responsibility of the called
        # procedure
        if d.k == locNone:
          getTemp(p, typ[0], d)
        pl.add(addrLoc(p.module, d))
        pl.add(~");$n")
        line(p, cpsStmts, pl)
        exitCall(p, ri)
    else:
      pl.add(~")")
      if true:
        if d.k == locNone: getTemp(p, typ[0], d)
        assert(d.t != nil)        # generate an assignment to d:
        var list: TLoc
        initLoc(list, locCall, d.lode, OnUnknown)
        list.r = pl
        genAssignment(p, d, list)
        exitCall(p, ri)
  else:
    pl.add(~");$n")
    line(p, cpsStmts, pl)
    exitCall(p, ri)

proc reifiedOpenArray(p: BProc, n: CgNode): bool {.inline.} =
  # all non-parameter openArrays are reified
  not(n.kind == cnkLocal and p.locals[n.local].k == locParam)

proc genOpenArraySlice(p: BProc; q: CgNode; formalType, destType: PType): (Rope, Rope) =
  var a, b, c: TLoc
  initLocExpr(p, q[0], a)
  initLocExpr(p, q[1], b)
  initLocExpr(p, q[2], c)
  let ty = skipTypes(a.t, abstractVar+{tyPtr, tyRef, tyLent})
  let dest = getTypeDesc(p.module, destType)
  let lengthExpr = "($1)-($2)+1" % [rdLoc(c), rdLoc(b)]
  case ty.kind
  of tyArray:
    let first = toInt64(firstOrd(p.config, ty))
    if first == 0:
      result = ("($3*)(($1)+($2))" % [rdLoc(a), rdLoc(b), dest],
                lengthExpr)
    else:
      result = ("($4*)($1)+(($2)-($3))" %
        [rdLoc(a), rdLoc(b), intLiteral(first), dest],
        lengthExpr)
  of tyOpenArray, tyVarargs:
    if reifiedOpenArray(p, q[0]):
      result = ("($3*)($1.Field0)+($2)" % [rdLoc(a), rdLoc(b), dest],
                lengthExpr)
    else:
      result = ("($3*)($1)+($2)" % [rdLoc(a), rdLoc(b), dest],
                lengthExpr)
  of tyUncheckedArray, tyCstring:
    result = ("($3*)($1)+($2)" % [rdLoc(a), rdLoc(b), dest],
              lengthExpr)
  of tyString, tySequence:
    let atyp = skipTypes(a.t, abstractInst)
    if atyp.kind in {tyVar}:
      result = ("((*$1).p != NIM_NIL ? ($4*)(*$1)$3+$2 : NIM_NIL)" %
                  [rdLoc(a), rdLoc(b), dataField(p), dest],
                lengthExpr)
    else:
      result = ("($1.p != NIM_NIL ? ($4*)$1$3+$2 : NIM_NIL)" %
                  [rdLoc(a), rdLoc(b), dataField(p), dest],
                lengthExpr)
  else:
    internalError(p.config, "openArrayLoc: " & typeToString(a.t))

proc literalsNeedsTmp(p: BProc, a: TLoc): TLoc =
  getTemp(p, a.lode.typ, result)
  genAssignment(p, result, a)

proc genArg(p: BProc, n: CgNode, param: PSym; call: CgNode): Rope =
  var a: TLoc
  if skipTypes(param.typ, abstractVar).kind in {tyOpenArray, tyVarargs}:
    # openArray parameters are translated to two C parameter: one for the
    # pointer, another for the length
    initLocExpr(p, n, a)
    if n.typ.skipTypes(abstractInst + tyUserTypeClasses).kind == tyArray:
      # FIXME: array values must not be passed to openArray parameters
      #        directly, but the required ``nkHiddenStdConv``/
      #        ``nkHiddenSubConv`` is not produced by sem in the following
      #        case:
      #          proc f(x: varargs[int]) = ...
      #          f(x = 0)
      result = "$1, $2" % [rdLoc(a), rope(lengthOrd(p.config, a.t))]
    elif reifiedOpenArray(p, n):
      result = "$1.Field0, $1.Field1" % [rdLoc(a)]
    else:
      result = "$1, $1Len_0" % [rdLoc(a)]
  elif ccgIntroducedPtr(p.config, param, call[0].typ[0]):
    initLocExpr(p, n, a)
    if n.kind in cnkLiterals + {cnkNilLit}:
      result = addrLoc(p.module, literalsNeedsTmp(p, a))
    else:
      result = addrLoc(p.module, a)
  else:
    initLocExprSingleUse(p, n, a)
    result = rdLoc(a)
  #assert result != nil

proc genArgNoParam(p: BProc, n: CgNode, needsTmp = false): Rope =
  var a: TLoc
  initLocExprSingleUse(p, n, a)
  result = rdLoc(a)

proc genParams(p: BProc, ri: CgNode, typ: PType): Rope =
  for i in 1..<(1 + numArgs(ri)):
    if i < typ.len:
      assert(typ.n[i].kind == nkSym)
      let paramType = typ.n[i]
      if not paramType.typ.isCompileTimeOnly:
        if result != "": result.add(", ")
        result.add(genArg(p, ri[i], paramType.sym, ri))
    else:
      if result != "": result.add(", ")
      result.add(genArgNoParam(p, ri[i]))

proc genPrefixCall(p: BProc, le, ri: CgNode, d: var TLoc) =
  var op: TLoc
  # this is a hotspot in the compiler
  initLocExpr(p, ri[0], op)
  # getUniqueType() is too expensive here:
  var typ = skipTypes(ri[0].typ, abstractInst)
  assert(typ.kind == tyProc)
  assert(typ.len == typ.n.len)

  let
    params = genParams(p, ri, typ)
    callee = rdLoc(op)

  fixupCall(p, le, ri, d, callee, params)

proc genClosureCall(p: BProc, le, ri: CgNode, d: var TLoc) =

  proc addComma(r: Rope): Rope =
    if r == "": r else: r & ", "

  const PatProc = "$1.ClE_0? $1.ClP_0($3$1.ClE_0):(($4)($1.ClP_0))($2)"
  const PatIter = "$1.ClP_0($3$1.ClE_0)" # we know the env exists

  var op: TLoc
  initLocExpr(p, ri[0], op)

  # getUniqueType() is too expensive here:
  var typ = skipTypes(ri[0].typ, abstractInst)
  assert(typ.kind == tyProc)
  assert(typ.len == typ.n.len)

  var pl = genParams(p, ri, typ)

  template genCallPattern {.dirty.} =
    if tfIterator in typ.flags:
      lineF(p, cpsStmts, PatIter & ";$n", [rdLoc(op), pl, pl.addComma, rawProc])
    else:
      lineF(p, cpsStmts, PatProc & ";$n", [rdLoc(op), pl, pl.addComma, rawProc])

  let rawProc = getClosureType(p.module, typ, clHalf)
  if typ[0] != nil:
    if isInvalidReturnType(p.config, typ[0]):
      if numArgs(ri) > 0: pl.add(~", ")
      # the destination is guaranteed to be either a temporary or an lvalue
      # that can be modified in-place
      if true:
        # resetting the result location is the responsibility of the called
        # procedure
        if d.k == locNone:
          getTemp(p, typ[0], d)
        pl.add(addrLoc(p.module, d))
        genCallPattern()
        exitCall(p, ri)
    else:
      if d.k == locNone: getTemp(p, typ[0], d)
      assert(d.t != nil)        # generate an assignment to d:
      var list: TLoc
      initLoc(list, locCall, d.lode, OnUnknown)
      if tfIterator in typ.flags:
        list.r = PatIter % [rdLoc(op), pl, pl.addComma, rawProc]
      else:
        list.r = PatProc % [rdLoc(op), pl, pl.addComma, rawProc]
      genAssignment(p, d, list)
      exitCall(p, ri)
  else:
    genCallPattern()
    exitCall(p, ri)

proc genAsgnCall(p: BProc, le, ri: CgNode, d: var TLoc) =
  if ri[0].typ.skipTypes({tyGenericInst, tyAlias, tySink}).callConv == ccClosure:
    genClosureCall(p, le, ri, d)
  else:
    genPrefixCall(p, le, ri, d)

proc genCall(p: BProc, e: CgNode, d: var TLoc) = genAsgnCall(p, nil, e, d)
