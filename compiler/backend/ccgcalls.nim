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

proc reportObservableStore(p: BProc; le, ri: CgNode) =
  ## Reports the ``rsemObservableStores`` hint when the called procedure can
  ## exit with an exception and `le` is something to which an assignment is
  ## observable in the exception-raised case.
  proc locationEscapes(p: BProc; le: CgNode; inTryStmt: bool): bool =
    var n = le
    while true:
      # do NOT follow ``cnkDerefView`` here!
      case n.kind
      of cnkGlobal:
        # mutation of a global -> the mutation escapes
        return true
      of cnkLocal:
        # if the local is used within an 'except' or 'finally', a mutation of
        # it through a procedure that eventually raises is also an observable
        # store
        return inTryStmt and sfUsedInFinallyOrExcept in p.body[n.local].flags
      of cnkFieldAccess, cnkArrayAccess, cnkTupleAccess:
        n = n[0]
      of cnkObjUpConv, cnkObjDownConv, cnkLvalueConv:
        n = n.operand
      else:
        # cannot analyse the location; assume the worst
        return true

  if le != nil and locationEscapes(p, le, p.nestedTryStmts.len > 0):
    localReport(p.config, le.info, reportSem rsemObservableStores)

proc isHarmlessStore(p: BProc; canRaise: bool; d: TLoc): bool =
  if d.k in {locTemp, locNone} or not canRaise:
    result = true
  elif d.k == locLocalVar and p.withinTryWithExcept == 0:
    # we cannot observe a store to a local variable if the current proc
    # has no error handler:
    result = true
  else:
    result = false

proc exitCall(p: BProc, call: CgNode) =
  ## Emits the exceptional control-flow related post-call logic.
  if call.kind == cnkCheckedCall:
    if nimErrorFlagDisabled in p.flags:
      if call[0].kind == cnkProc and sfNoReturn in p.env[call[0].prc].flags and
         canRaiseConservative(p.env, call[0]):
        # when using goto-exceptions, noreturn doesn't map to "doesn't return"
        # at the C-level. In order to still support dispatching to wrapper
        # procedures around ``raise`` from inside ``.compilerprocs``, we emit
        # an exit after the call
        p.flags.incl beforeRetNeeded
        lineF(p, cpsStmts, "goto BeforeRet_;$n", [])
    else:
      raiseExit(p, call[^1])

proc fixupCall(p: BProc, le, ri: CgNode, d: var TLoc,
               callee, params: Rope) =
  let canRaise = ri.kind == cnkCheckedCall
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
        if d.k notin {locTemp, locNone} and canRaise:
          reportObservableStore(p, le, ri)

        # resetting the result location is the responsibility of the called
        # procedure
        if d.k == locNone:
          getTemp(p, typ[0], d)
        pl.add(addrLoc(p.config, d))
        pl.add(~");$n")
        line(p, cpsStmts, pl)
        exitCall(p, ri)
    else:
      pl.add(~")")
      if isHarmlessStore(p, canRaise, d):
        if d.k == locNone: getTemp(p, typ[0], d)
        assert(d.t != nil)        # generate an assignment to d:
        var list: TLoc
        initLoc(list, locCall, d.lode, OnUnknown)
        list.r = pl
        genAssignment(p, d, list)
        exitCall(p, ri)
      else:
        var tmp: TLoc
        getTemp(p, typ[0], tmp)
        var list: TLoc
        initLoc(list, locCall, d.lode, OnUnknown)
        list.r = pl
        genAssignment(p, tmp, list)
        exitCall(p, ri)
        genAssignment(p, d, tmp)
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
    if formalType.skipTypes(abstractInst).kind in {tyVar} and atyp.kind == tyString:
      linefmt(p, cpsStmts, "#nimPrepareStrMutationV2($1);$n", [byRefLoc(p, a)])
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
      result = addrLoc(p.config, literalsNeedsTmp(p, a))
    else:
      result = addrLoc(p.config, a)
  else:
    initLocExprSingleUse(p, n, a)
    result = rdLoc(a)
  #assert result != nil

proc genArgNoParam(p: BProc, n: CgNode, needsTmp = false): Rope =
  var a: TLoc
  initLocExprSingleUse(p, n, a)
  result = rdLoc(a)

proc genParams(p: BProc, ri: CgNode, typ: PType): Rope =
  for i in 1..<ri.len:
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
  let canRaise = ri.kind == cnkCheckedCall
  if typ[0] != nil:
    if isInvalidReturnType(p.config, typ[0]):
      if ri.len > 1: pl.add(~", ")
      # the destination is guaranteed to be either a temporary or an lvalue
      # that can be modified in-place
      if true:
        if d.k notin {locTemp, locNone} and canRaise:
          reportObservableStore(p, le, ri)

        # resetting the result location is the responsibility of the called
        # procedure
        if d.k == locNone:
          getTemp(p, typ[0], d)
        pl.add(addrLoc(p.config, d))
        genCallPattern()
        exitCall(p, ri)
    elif isHarmlessStore(p, canRaise, d):
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
      var tmp: TLoc
      getTemp(p, typ[0], tmp)
      assert(d.t != nil)        # generate an assignment to d:
      var list: TLoc
      initLoc(list, locCall, d.lode, OnUnknown)
      if tfIterator in typ.flags:
        list.r = PatIter % [rdLoc(op), pl, pl.addComma, rawProc]
      else:
        list.r = PatProc % [rdLoc(op), pl, pl.addComma, rawProc]
      genAssignment(p, tmp, list)
      exitCall(p, ri)
      genAssignment(p, d, tmp)
  else:
    genCallPattern()
    exitCall(p, ri)

proc genAsgnCall(p: BProc, le, ri: CgNode, d: var TLoc) =
  if ri[0].typ.skipTypes({tyGenericInst, tyAlias, tySink}).callConv == ccClosure:
    genClosureCall(p, le, ri, d)
  else:
    genPrefixCall(p, le, ri, d)

proc genCall(p: BProc, e: CgNode, d: var TLoc) = genAsgnCall(p, nil, e, d)
