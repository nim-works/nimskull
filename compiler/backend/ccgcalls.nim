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

proc canRaiseDisp(p: BProc; n: PNode): bool =
  ## 'true' if calling the callee expression `n` can exit via exceptional
  ## control-flow, otherwise 'false'. If panics are disabled, this also
  ## includes all routines that are not certain magics, compiler procs, or
  ## imported.
  if n.kind == nkSym and {sfNeverRaises, sfImportc, sfCompilerProc} * n.sym.flags != {}:
    result = false
  elif optPanics in p.config.globalOptions:
    # we know we can be strict:
    result = canRaise(n)
  else:
    # we have to be *very* conservative:
    result = canRaiseConservative(n)

proc reportObservableStore(p: BProc; le, ri: PNode) =
  ## Reports the ``rsemObservableStores`` hint when the called procedure can
  ## exit with an exception and `le` is something to which an assignment is
  ## observable in the exception-raised case.
  proc locationEscapes(p: BProc; le: PNode; inTryStmt: bool): bool =
    var n = le
    while true:
      # do NOT follow nkHiddenDeref here!
      case n.kind
      of nkSym:
        # we don't own the location so it escapes:
        if n.sym.owner != p.prc:
          return true
        elif inTryStmt and sfUsedInFinallyOrExcept in n.sym.flags:
          # it is also an observable store if the location is used
          # in 'except' or 'finally'
          return true
        return false
      of nkDotExpr, nkBracketExpr, nkObjUpConv, nkObjDownConv,
          nkCheckedFieldExpr:
        n = n[0]
      of nkHiddenStdConv, nkHiddenSubConv, nkConv:
        n = n[1]
      else:
        # cannot analyse the location; assume the worst
        return true

  # we use the weaker 'canRaise' here in order to prevent too many
  # annoying warnings, see #14514
  if le != nil and canRaise(ri[0]) and
     locationEscapes(p, le, p.nestedTryStmts.len > 0):
    localReport(p.config, le, reportSem rsemObservableStores)

proc hasNoInit(call: PNode): bool {.inline.} =
  result = call[0].kind == nkSym and sfNoInit in call[0].sym.flags

proc isHarmlessStore(p: BProc; canRaise: bool; d: TLoc): bool =
  if d.k in {locTemp, locNone} or not canRaise:
    result = true
  elif d.k == locLocalVar and p.withinTryWithExcept == 0:
    # we cannot observe a store to a local variable if the current proc
    # has no error handler:
    result = true
  else:
    result = false

proc exitCall(p: BProc, callee: PNode, canRaise: bool) =
  ## Emits the exceptional control-flow related post-call logic.
  if p.config.exc == excGoto:
    if nimErrorFlagDisabled in p.flags:
      if callee.kind == nkSym and sfNoReturn in callee.sym.flags and
         canRaiseConservative(callee):
        # when using goto-exceptions, noreturn doesn't map to "doesn't return"
        # at the C-level. In order to still support dispatching to wrapper
        # procedures around ``raise`` from inside ``.compilerprocs``, we emit
        # an exit after the call
        p.flags.incl beforeRetNeeded
        lineF(p, cpsStmts, "goto BeforeRet_;$n", [])
    elif canRaise:
      raiseExit(p)

proc fixupCall(p: BProc, le, ri: PNode, d: var TLoc,
               callee, params: Rope) =
  let canRaise = canRaiseDisp(p, ri[0])
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
        if d.k notin {locTemp, locNone}:
          reportObservableStore(p, le, ri)

        if d.k == locNone: getTemp(p, typ[0], d, needsInit=true)
        elif d.k notin {locTemp} and not hasNoInit(ri):
          # reset before pass as 'result' var:
          discard "resetLoc(p, d)"
        pl.add(addrLoc(p.config, d))
        pl.add(~");$n")
        line(p, cpsStmts, pl)
        exitCall(p, ri[0], canRaise)
    else:
      pl.add(~")")
      if isHarmlessStore(p, canRaise, d):
        if d.k == locNone: getTemp(p, typ[0], d)
        assert(d.t != nil)        # generate an assignment to d:
        var list: TLoc
        initLoc(list, locCall, d.lode, OnUnknown)
        list.r = pl
        genAssignment(p, d, list)
        exitCall(p, ri[0], canRaise)
      else:
        var tmp: TLoc
        getTemp(p, typ[0], tmp, needsInit=true)
        var list: TLoc
        initLoc(list, locCall, d.lode, OnUnknown)
        list.r = pl
        genAssignment(p, tmp, list)
        exitCall(p, ri[0], canRaise)
        genAssignment(p, d, tmp)
  else:
    pl.add(~");$n")
    line(p, cpsStmts, pl)
    exitCall(p, ri[0], canRaise)

proc genBoundsCheck(p: BProc; arr, a, b: TLoc)

proc reifiedOpenArray(n: PNode): bool {.inline.} =
  var x = n
  while x.kind in {nkAddr, nkHiddenAddr, nkHiddenStdConv, nkHiddenDeref}:
    x = x[0]
  if x.kind == nkSym and x.sym.kind == skParam:
    result = false
  else:
    result = true

proc genOpenArraySlice(p: BProc; q: PNode; formalType, destType: PType): (Rope, Rope) =
  var a, b, c: TLoc
  initLocExpr(p, q[1], a)
  initLocExpr(p, q[2], b)
  initLocExpr(p, q[3], c)
  # but first produce the required index checks:
  if optBoundsCheck in p.options:
    genBoundsCheck(p, a, b, c)
  let ty = skipTypes(a.t, abstractVar+{tyPtr})
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
    if reifiedOpenArray(q[1]):
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
      result = ("($4*)(*$1)$3+($2)" % [rdLoc(a), rdLoc(b), dataField(p), dest],
                lengthExpr)
    else:
      result = ("($4*)$1$3+($2)" % [rdLoc(a), rdLoc(b), dataField(p), dest],
                lengthExpr)
  else:
    internalError(p.config, "openArrayLoc: " & typeToString(a.t))

proc openArrayLoc(p: BProc, formalType: PType, n: PNode): Rope =
  var q = skipConv(n)
  var skipped = false
  while q.kind == nkStmtListExpr and q.len > 0:
    skipped = true
    q = q.lastSon
  if getMagic(q) == mSlice:
    # magic: pass slice to openArray:
    if skipped:
      q = skipConv(n)
      while q.kind == nkStmtListExpr and q.len > 0:
        for i in 0..<q.len-1:
          genStmts(p, q[i])
        q = q.lastSon
    let (x, y) = genOpenArraySlice(p, q, formalType, n.typ[0])
    result = x & ", " & y
  else:
    var a: TLoc
    initLocExpr(p, if n.kind == nkHiddenStdConv: n[1] else: n, a)
    case skipTypes(a.t, abstractVar+{tyStatic}).kind
    of tyOpenArray, tyVarargs:
      if reifiedOpenArray(n):
        result = "$1.Field0, $1.Field1" % [rdLoc(a)]
      else:
        result = "$1, $1Len_0" % [rdLoc(a)]
    of tyString, tySequence:
      let ntyp = skipTypes(n.typ, abstractInst)
      if formalType.skipTypes(abstractInst).kind in {tyVar} and ntyp.kind == tyString:
        linefmt(p, cpsStmts, "#nimPrepareStrMutationV2($1);$n", [byRefLoc(p, a)])
      if ntyp.kind in {tyVar}:
        var t: TLoc
        t.r = "(*$1)" % [a.rdLoc]
        result = "(*$1)$3, $2" % [a.rdLoc, lenExpr(p, t), dataField(p)]
      else:
        result = "$1$3, $2" % [a.rdLoc, lenExpr(p, a), dataField(p)]
    of tyArray:
      result = "$1, $2" % [rdLoc(a), rope(lengthOrd(p.config, a.t))]
    of tyPtr, tyRef:
      case lastSon(a.t).kind
      of tyString, tySequence:
        var t: TLoc
        t.r = "(*$1)" % [a.rdLoc]
        result = "(*$1)$3, $2" % [a.rdLoc, lenExpr(p, t), dataField(p)]
      of tyArray:
        result = "$1, $2" % [rdLoc(a), rope(lengthOrd(p.config, lastSon(a.t)))]
      else:
        internalError(p.config, "openArrayLoc: " & typeToString(a.t))
    else: internalError(p.config, "openArrayLoc: " & typeToString(a.t))

proc literalsNeedsTmp(p: BProc, a: TLoc): TLoc =
  getTemp(p, a.lode.typ, result, needsInit=false)
  genAssignment(p, result, a)

proc genArgStringToCString(p: BProc, n: PNode): Rope {.inline.} =
  var a: TLoc
  initLocExpr(p, n[0], a)
  ropecg(p.module, "#nimToCStringConv($1)", [rdLoc(a)])

proc genArg(p: BProc, n: PNode, param: PSym; call: PNode): Rope =
  var a: TLoc
  if n.kind == nkStringToCString:
    result = genArgStringToCString(p, n)
  elif skipTypes(param.typ, abstractVar).kind in {tyOpenArray, tyVarargs}:
    var n = if n.kind != nkHiddenAddr: n else: n[0]
    result = openArrayLoc(p, param.typ, n)
  elif ccgIntroducedPtr(p.config, param, call[0].typ[0]):
    initLocExpr(p, n, a)
    if n.kind in {nkCharLit..nkNilLit}:
      result = addrLoc(p.config, literalsNeedsTmp(p, a))
    else:
      result = addrLoc(p.config, a)
  else:
    initLocExprSingleUse(p, n, a)
    result = rdLoc(a)
  #assert result != nil

proc genArgNoParam(p: BProc, n: PNode, needsTmp = false): Rope =
  var a: TLoc
  if n.kind == nkStringToCString:
    result = genArgStringToCString(p, n)
  else:
    initLocExprSingleUse(p, n, a)
    result = rdLoc(a)

proc genParams(p: BProc, ri: PNode, typ: PType): Rope =
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

proc genPrefixCall(p: BProc, le, ri: PNode, d: var TLoc) =
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

proc genClosureCall(p: BProc, le, ri: PNode, d: var TLoc) =

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
  let canRaise = canRaiseDisp(p, ri[0])
  if typ[0] != nil:
    if isInvalidReturnType(p.config, typ[0]):
      if ri.len > 1: pl.add(~", ")
      # the destination is guaranteed to be either a temporary or an lvalue
      # that can be modified in-place
      if true:
        if d.k notin {locTemp, locNone}:
          reportObservableStore(p, le, ri)

        if d.k == locNone:
          getTemp(p, typ[0], d, needsInit=true)
        elif d.k notin {locTemp} and not hasNoInit(ri):
          # reset before pass as 'result' var:
          discard "resetLoc(p, d)"
        pl.add(addrLoc(p.config, d))
        genCallPattern()
        exitCall(p, ri[0], canRaise)
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
      exitCall(p, ri[0], canRaise)
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
      exitCall(p, ri[0], canRaise)
      genAssignment(p, d, tmp)
  else:
    genCallPattern()
    exitCall(p, ri[0], canRaise)

proc notYetAlive(n: PNode): bool {.inline.} =
  let r = getRoot(n)
  result = r != nil and r.locId == 0

proc isInactiveDestructorCall(p: BProc, e: PNode): bool =
  #[ Consider this example.

    var :tmpD_3281815
    try:
      if true:
        return
      let args_3280013 =
        wasMoved_3281816(:tmpD_3281815)
        `=_3280036`(:tmpD_3281815, [1])
        :tmpD_3281815
    finally:
      `=destroy_3280027`(args_3280013)

  We want to return early but the 'finally' section is traversed before
  the 'let args = ...' statement. We exploit this to generate better
  code for 'return'. ]#
  result = e.len == 2 and e[0].kind == nkSym and
    e[0].sym.name.s == "=destroy" and notYetAlive(e[1].skipAddr)

proc genAsgnCall(p: BProc, le, ri: PNode, d: var TLoc) =
  if p.withinBlockLeaveActions > 0 and isInactiveDestructorCall(p, ri):
    return
  if ri[0].typ.skipTypes({tyGenericInst, tyAlias, tySink}).callConv == ccClosure:
    genClosureCall(p, le, ri, d)
  else:
    genPrefixCall(p, le, ri, d)

proc genCall(p: BProc, e: PNode, d: var TLoc) = genAsgnCall(p, nil, e, d)
