#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from cgen.nim

proc getNullValueAuxT(p: BProc; orig, t: PType; obj: PNode, constOrNil: CgNode,
                      result: var Rope; count: var int;
                      info: TLineInfo)

# -------------------------- constant expressions ------------------------

proc rdSetElemLoc(conf: ConfigRef; a: TLoc, typ: PType): Rope

proc int64Literal(i: BiggestInt): Rope =
  if i > low(int64):
    result = "IL64($1)" % [rope(i)]
  else:
    result = ~"(IL64(-9223372036854775807) - IL64(1))"

proc uint64Literal(i: uint64): Rope = rope($i & "ULL")

proc intLiteral(i: BiggestInt): Rope =
  if i > low(int32) and i <= high(int32):
    result = rope(i)
  elif i == low(int32):
    # Nim has the same bug for the same reasons :-)
    result = ~"(-2147483647 -1)"
  elif i > low(int64):
    result = "IL64($1)" % [rope(i)]
  else:
    result = ~"(IL64(-9223372036854775807) - IL64(1))"

proc intLiteral(i: Int128): Rope =
  intLiteral(toInt64(i))

proc intLiteral(p: BProc, i: Int128, ty: PType): Rope =
  assert ty != nil
  case skipTypes(ty, abstractVarRange).kind
  of tyChar:      intLiteral(i)
  of tyBool:
    if i != Zero: "NIM_TRUE"
    else:         "NIM_FALSE"
  of tyInt64:     int64Literal(toInt64(i))
  of tyUInt64:    uint64Literal(toUInt64(i))
  else:
    "(($1) $2)" % [getTypeDesc(p.module, ty), intLiteral(i)]

proc genLiteral(p: BProc, n: CgNode, ty: PType): Rope =
  case n.kind
  of cnkIntLit, cnkUIntLit:
    result = intLiteral(p, getInt(n), ty)
  of cnkNilLit:
    let k = if ty == nil: tyPointer else: skipTypes(ty, abstractVarRange).kind
    if k == tyProc and skipTypes(ty, abstractVarRange).callConv == ccClosure:
      let id = getOrPut(p.module.dataCache, n, p.module.labels)
      result = p.module.tmpBase & rope(id)
      if id == p.module.labels:
        # not found in cache:
        inc(p.module.labels)
        p.module.s[cfsData].addf(
             "static NIM_CONST $1 $2 = {NIM_NIL,NIM_NIL};$n",
             [getTypeDesc(p.module, ty), result])
    elif k in {tyPointer, tyNil, tyProc}:
      result = rope("NIM_NIL")
    else:
      result = "(($1) NIM_NIL)" % [getTypeDesc(p.module, ty)]
  of cnkStrLit:
    let k = if ty == nil: tyString
            else: skipTypes(ty, abstractVarRange + {tyStatic, tyUserTypeClass, tyUserTypeClassInst}).kind
    case k
    of tyNil:
      result = genNilStringLiteral(p.module, n.info)
    of tyString:
      # with the new semantics for not 'nil' strings, we can map "" to nil and
      # save tons of allocations:
      result = genStringLiteral(p.module, n)
    else:
      result = makeCString(n.strVal)
  of cnkFloatLit:
    if ty.kind == tyFloat32:
      result = rope(n.floatVal.float32.toStrMaxPrecision)
    else:
      result = rope(n.floatVal.toStrMaxPrecision)
  else:
    internalError(p.config, n.info, "genLiteral(" & $n.kind & ')')
    result = ""

proc genLiteral(p: BProc, n: CgNode): Rope =
  result = genLiteral(p, n, n.typ)

proc bitSetToWord(s: TBitSet, size: int): BiggestUInt =
  result = 0
  for j in 0..<size:
    if j < s.len: result = result or (BiggestUInt(s[j]) shl (j * 8))

proc genRawSetData(cs: TBitSet, size: int): Rope =
  if size > 8:
    var res = "{\n"
    for i in 0..<size:
      res.add "0x"
      res.add "0123456789abcdef"[cs[i] div 16]
      res.add "0123456789abcdef"[cs[i] mod 16]
      if i < size - 1:
        # not last iteration
        if i mod 8 == 7:
          res.add ",\n"
        else:
          res.add ", "
      else:
        res.add "}\n"

    result = rope(res)
  else:
    result = intLiteral(cast[BiggestInt](bitSetToWord(cs, size)))

proc genSetNode(p: BProc, n: CgNode): Rope =
  var size = int(getSize(p.config, n.typ))
  let cs = toBitSet(p.config, n)
  if size > 8:
    let id = getOrPut(p.module.dataCache, n, p.module.labels)
    result = p.module.tmpBase & rope(id)
    if id == p.module.labels:
      # not found in cache:
      inc(p.module.labels)
      p.module.s[cfsData].addf("static NIM_CONST $1 $2 = $3;$n",
           [getTypeDesc(p.module, n.typ), result, genRawSetData(cs, size)])
  else:
    result = genRawSetData(cs, size)

proc genOpenArrayConv(p: BProc; d: TLoc; a: TLoc) =
  assert d.k != locNone

  case a.t.skipTypes(abstractVar + tyUserTypeClasses).kind
  of tyOpenArray, tyVarargs:
    if reifiedOpenArray(p, a.lode):
      linefmt(p, cpsStmts, "$1.Field0 = $2.Field0; $1.Field1 = $2.Field1;$n",
        [rdLoc(d), a.rdLoc])
    else:
      linefmt(p, cpsStmts, "$1.Field0 = $2; $1.Field1 = $2Len_0;$n",
        [rdLoc(d), a.rdLoc])
  of tySequence:
    linefmt(p, cpsStmts, "$1.Field0 = $2$3; $1.Field1 = $4;$n",
      [rdLoc(d), a.rdLoc, dataField(p), lenExpr(p, a)])
  of tyArray:
    linefmt(p, cpsStmts, "$1.Field0 = $2; $1.Field1 = $3;$n",
      [rdLoc(d), rdLoc(a), rope(lengthOrd(p.config, a.t))])
  of tyString:
    if skipTypes(d.t, abstractInst).kind in {tyVar}:
      linefmt(p, cpsStmts, "#nimPrepareStrMutationV2($1);$n", [byRefLoc(p, a)])

    linefmt(p, cpsStmts, "$1.Field0 = $2$3; $1.Field1 = $4;$n",
      [rdLoc(d), a.rdLoc, dataField(p), lenExpr(p, a)])
  else:
    internalError(p.config, a.lode.info, "cannot handle " & $a.t.kind)

proc genAssignment(p: BProc, dest, src: TLoc) =
  # This function replaces all other methods for generating
  # the assignment operation in C.
  case mapType(p.config, dest.t, skVar)
  of ctChar, ctBool, ctInt, ctInt8, ctInt16, ctInt32, ctInt64,
     ctFloat, ctFloat32, ctFloat64,
     ctUInt, ctUInt8, ctUInt16, ctUInt32, ctUInt64,
     ctStruct, ctPtrToArray, ctPtr, ctNimStr, ctNimSeq, ctProc,
     ctCString:
    linefmt(p, cpsStmts, "$1 = $2;$n", [rdLoc(dest), rdLoc(src)])
  of ctArray:
    assert dest.t.skipTypes(irrelevantForBackend + abstractInst).kind != tyOpenArray
    linefmt(p, cpsStmts, "#nimCopyMem((void*)$1, (NIM_CONST void*)$2, $3);$n",
            [rdLoc(dest), rdLoc(src), getSize(p.config, dest.t)])
  of ctNimOpenArray:
    # HACK: ``cgirgen`` elides to-openArray-conversion operations, so we
    #       need to reconstruct that information here. Remove this case
    #       once ``cgirgen`` no longer elides the operations
    if reifiedOpenArray(p, dest.lode):
      genOpenArrayConv(p, dest, src)
    else:
      linefmt(p, cpsStmts, "$1 = $2;$n", [rdLoc(dest), rdLoc(src)])
  of ctVoid:
    unreachable("not a valid location type")

  if optMemTracker in p.options and dest.storage in {OnHeap, OnUnknown}:
    #writeStackTrace()
    #echo p.currLineInfo, " requesting"
    linefmt(p, cpsStmts, "#memTrackerWrite((void*)$1, $2, $3, $4);$n",
            [addrLoc(p.config, dest), getSize(p.config, dest.t),
            makeCString(toFullPath(p.config, p.currLineInfo)),
            p.currLineInfo.safeLineNm])

proc genDeepCopy(p: BProc; dest, src: TLoc) =
  template addrLocOrTemp(a: TLoc): Rope =
    if a.k == locExpr:
      var tmp: TLoc
      getTemp(p, a.t, tmp)
      genAssignment(p, tmp, a)
      addrLoc(p.config, tmp)
    else:
      addrLoc(p.config, a)

  var ty = skipTypes(dest.t, abstractVarRange + {tyStatic})
  case ty.kind
  of tyPtr, tyRef, tyProc, tyTuple, tyObject, tyArray:
    # XXX optimize this
    linefmt(p, cpsStmts, "#genericDeepCopy((void*)$1, (void*)$2, $3);$n",
            [addrLoc(p.config, dest), addrLocOrTemp(src),
            genTypeInfoV1(p.module, dest.t, dest.lode.info)])
  of tySequence, tyString:
    linefmt(p, cpsStmts, "#genericDeepCopy((void*)$1, (void*)$2, $3);$n",
            [addrLoc(p.config, dest), addrLocOrTemp(src),
            genTypeInfoV1(p.module, dest.t, dest.lode.info)])
  of tyOpenArray, tyVarargs:
    linefmt(p, cpsStmts,
         "#genericDeepCopyOpenArray((void*)$1, (void*)$2, $1Len_0, $3);$n",
         [addrLoc(p.config, dest), addrLocOrTemp(src),
         genTypeInfoV1(p.module, dest.t, dest.lode.info)])
  of tySet:
    if mapSetType(p.config, ty) == ctArray:
      linefmt(p, cpsStmts, "#nimCopyMem((void*)$1, (NIM_CONST void*)$2, $3);$n",
              [rdLoc(dest), rdLoc(src), getSize(p.config, dest.t)])
    else:
      linefmt(p, cpsStmts, "$1 = $2;$n", [rdLoc(dest), rdLoc(src)])
  of tyPointer, tyChar, tyBool, tyEnum, tyCstring,
     tyInt..tyUInt64, tyRange, tyVar, tyLent:
    linefmt(p, cpsStmts, "$1 = $2;$n", [rdLoc(dest), rdLoc(src)])
  else: internalError(p.config, "genDeepCopy: " & $ty.kind)

proc putLocIntoDest(p: BProc, d: var TLoc, s: TLoc) =
  if d.k != locNone:
    genAssignment(p, d, s)
  else:
    d = s # ``d`` is free, so fill it with ``s``

proc putDataIntoDest(p: BProc, d: var TLoc, n: CgNode, r: Rope) =
  var a: TLoc
  if d.k != locNone:
    # need to generate an assignment here
    initLoc(a, locData, n, OnStatic)
    a.r = r
    genAssignment(p, d, a)
  else:
    # we cannot call initLoc() here as that would overwrite
    # the flags field!
    d.k = locData
    d.lode = n
    d.r = r

proc putIntoDest(p: BProc, d: var TLoc, n: CgNode, r: Rope; s=OnUnknown) =
  var a: TLoc
  if d.k != locNone:
    # need to generate an assignment here
    initLoc(a, locExpr, n, s)
    a.r = r
    genAssignment(p, d, a)
  else:
    # we cannot call initLoc() here as that would overwrite
    # the flags field!
    d.k = locExpr
    d.lode = n
    d.r = r

proc binaryStmtAddr(p: BProc, e: CgNode, d: var TLoc, cpname: string) =
  var a, b: TLoc
  if d.k != locNone: internalError(p.config, e.info, "binaryStmtAddr")
  initLocExpr(p, e[1], a)
  initLocExpr(p, e[2], b)
  lineCg(p, cpsStmts, "#$1($2, $3);$n", [cpname, byRefLoc(p, a), rdLoc(b)])

template unaryStmt(p: BProc, e: CgNode, d: var TLoc, frmt: string) =
  var a: TLoc
  if d.k != locNone: internalError(p.config, e.info, "unaryStmt")
  initLocExpr(p, e[1], a)
  lineCg(p, cpsStmts, frmt, [rdLoc(a)])

template binaryExpr(p: BProc, e: CgNode, d: var TLoc, frmt: string) =
  var a, b: TLoc
  assert(e[1].typ != nil)
  assert(e[2].typ != nil)
  initLocExpr(p, e[1], a)
  initLocExpr(p, e[2], b)
  putIntoDest(p, d, e, ropecg(p.module, frmt, [rdLoc(a), rdLoc(b)]))

template binaryExprChar(p: BProc, e: CgNode, d: var TLoc, frmt: string) =
  var a, b: TLoc
  assert(e[1].typ != nil)
  assert(e[2].typ != nil)
  initLocExpr(p, e[1], a)
  initLocExpr(p, e[2], b)
  putIntoDest(p, d, e, ropecg(p.module, frmt, [a.rdCharLoc, b.rdCharLoc]))

template unaryExpr(p: BProc, e: CgNode, d: var TLoc, frmt: string) =
  var a: TLoc
  initLocExpr(p, e[1], a)
  putIntoDest(p, d, e, ropecg(p.module, frmt, [rdLoc(a)]))

template unaryExprChar(p: BProc, e: CgNode, d: var TLoc, frmt: string) =
  var a: TLoc
  initLocExpr(p, e[1], a)
  putIntoDest(p, d, e, ropecg(p.module, frmt, [rdCharLoc(a)]))

template binaryArithOverflowRaw(p: BProc, t: PType, a, b: TLoc;
                            cpname: string): Rope =
  var size = getSize(p.config, t)
  let storage = if size < p.config.target.intSize: rope("NI")
                else: getTypeDesc(p.module, t)
  var result = getTempName(p.module)
  linefmt(p, cpsLocals, "$1 $2;$n", [storage, result])
  lineCg(p, cpsStmts, "if (#$2($3, $4, &$1)) { #raiseOverflow(); $5};$n",
      [result, cpname, rdCharLoc(a), rdCharLoc(b), raiseInstr(p)])
  if size < p.config.target.intSize or t.kind in {tyRange, tyEnum}:
    linefmt(p, cpsStmts, "if ($1 < $2 || $1 > $3){ #raiseOverflow(); $4}$n",
            [result, intLiteral(firstOrd(p.config, t)), intLiteral(lastOrd(p.config, t)),
            raiseInstr(p)])
  result

proc binaryArithOverflow(p: BProc, e: CgNode, d: var TLoc, m: TMagic) =
  const
    prc: array[mAddI..mPred, string] = [
      "nimAddInt", "nimSubInt",
      "nimMulInt", "nimDivInt", "nimModInt",
      "nimAddInt", "nimSubInt"
    ]
    prc64: array[mAddI..mPred, string] = [
      "nimAddInt64", "nimSubInt64",
      "nimMulInt64", "nimDivInt64", "nimModInt64",
      "nimAddInt64", "nimSubInt64"
    ]
    opr: array[mAddI..mPred, string] = ["+", "-", "*", "/", "%", "+", "-"]
  var a, b: TLoc
  assert(e[1].typ != nil)
  assert(e[2].typ != nil)
  initLocExpr(p, e[1], a)
  initLocExpr(p, e[2], b)
  # skipping 'range' is correct here as we'll generate a proper range check
  # later via 'chckRange'
  let t = e.typ.skipTypes(abstractRange)
  if optOverflowCheck notin p.options:
    let res = "($1)($2 $3 $4)" % [getTypeDesc(p.module, e.typ), rdLoc(a), rope(opr[m]), rdLoc(b)]
    putIntoDest(p, d, e, res)
  else:
    # we handle div by zero here so that we know that the compilerproc's
    # result is only for overflows.
    if m in {mDivI, mModI}:
      linefmt(p, cpsStmts, "if ($1 == 0){ #raiseDivByZero(); $2}$n",
              [rdLoc(b), raiseInstr(p)])

    let res = binaryArithOverflowRaw(p, t, a, b,
      if t.kind == tyInt64: prc64[m] else: prc[m])
    putIntoDest(p, d, e, "($#)($#)" % [getTypeDesc(p.module, e.typ), res])

proc unaryArithOverflow(p: BProc, e: CgNode, d: var TLoc, m: TMagic) =
  var
    a: TLoc
    t: PType
  assert(e[1].typ != nil)
  initLocExpr(p, e[1], a)
  t = skipTypes(e.typ, abstractRange)
  if optOverflowCheck in p.options:
    linefmt(p, cpsStmts, "if ($1 == $2){ #raiseOverflow(); $3}$n",
            [rdLoc(a), intLiteral(firstOrd(p.config, t)), raiseInstr(p)])
  case m
  of mUnaryMinusI:
    putIntoDest(p, d, e, "((NI$2)-($1))" % [rdLoc(a), rope(getSize(p.config, t) * 8)])
  of mUnaryMinusI64:
    putIntoDest(p, d, e, "-($1)" % [rdLoc(a)])
  of mAbsI:
    putIntoDest(p, d, e, "($1 > 0? ($1) : -($1))" % [rdLoc(a)])
  else:
    assert(false, $m)

proc binaryArith(p: BProc, e: CgNode, d: var TLoc, op: TMagic) =
  var
    a, b: TLoc
    s, k: BiggestInt
  assert(e[1].typ != nil)
  assert(e[2].typ != nil)
  initLocExpr(p, e[1], a)
  initLocExpr(p, e[2], b)
  # BUGFIX: cannot use result-type here, as it may be a boolean
  s = max(getSize(p.config, a.t), getSize(p.config, b.t)) * 8
  k = getSize(p.config, a.t) * 8

  template applyFormat(frmt: untyped) =
    putIntoDest(p, d, e, frmt % [
      rdLoc(a), rdLoc(b), rope(s),
      getSimpleTypeDesc(p.module, e.typ), rope(k)]
    )

  case op
  of mAddF64: applyFormat("(($4)($1) + ($4)($2))")
  of mSubF64: applyFormat("(($4)($1) - ($4)($2))")
  of mMulF64: applyFormat("(($4)($1) * ($4)($2))")
  of mDivF64: applyFormat("(($4)($1) / ($4)($2))")
  of mShrI: applyFormat("($4)((NU$5)($1) >> (NU$3)($2))")
  of mShlI: applyFormat("($4)((NU$3)($1) << (NU$3)($2))")
  of mAshrI: applyFormat("($4)((NI$3)($1) >> (NU$3)($2))")
  of mBitandI: applyFormat("($4)($1 & $2)")
  of mBitorI: applyFormat("($4)($1 | $2)")
  of mBitxorI: applyFormat("($4)($1 ^ $2)")
  of mMinI: applyFormat("(($1 <= $2) ? $1 : $2)")
  of mMaxI: applyFormat("(($1 >= $2) ? $1 : $2)")
  of mAddU: applyFormat("($4)((NU$3)($1) + (NU$3)($2))")
  of mSubU: applyFormat("($4)((NU$3)($1) - (NU$3)($2))")
  of mMulU: applyFormat("($4)((NU$3)($1) * (NU$3)($2))")
  of mDivU: applyFormat("($4)((NU$3)($1) / (NU$3)($2))")
  of mModU: applyFormat("($4)((NU$3)($1) % (NU$3)($2))")
  of mEqI: applyFormat("($1 == $2)")
  of mLeI: applyFormat("($1 <= $2)")
  of mLtI: applyFormat("($1 < $2)")
  of mEqF64: applyFormat("($1 == $2)")
  of mLeF64: applyFormat("($1 <= $2)")
  of mLtF64: applyFormat("($1 < $2)")
  of mLeU: applyFormat("((NU$3)($1) <= (NU$3)($2))")
  of mLtU: applyFormat("((NU$3)($1) < (NU$3)($2))")
  of mEqEnum: applyFormat("($1 == $2)")
  of mLeEnum: applyFormat("($1 <= $2)")
  of mLtEnum: applyFormat("($1 < $2)")
  of mEqCh: applyFormat("((NU8)($1) == (NU8)($2))")
  of mLeCh: applyFormat("((NU8)($1) <= (NU8)($2))")
  of mLtCh: applyFormat("((NU8)($1) < (NU8)($2))")
  of mEqB: applyFormat("($1 == $2)")
  of mLeB: applyFormat("($1 <= $2)")
  of mLtB: applyFormat("($1 < $2)")
  of mEqRef: applyFormat("($1 == $2)")
  of mLePtr: applyFormat("($1 <= $2)")
  of mLtPtr: applyFormat("($1 < $2)")
  of mXor: applyFormat("($1 != $2)")
  else:
    assert(false, $op)

proc genEqProc(p: BProc, e: CgNode, d: var TLoc) =
  var a, b: TLoc
  assert(e[1].typ != nil)
  assert(e[2].typ != nil)
  initLocExpr(p, e[1], a)
  initLocExpr(p, e[2], b)
  if a.t.skipTypes(abstractInst).callConv == ccClosure:
    putIntoDest(p, d, e,
      "($1.ClP_0 == $2.ClP_0 && $1.ClE_0 == $2.ClE_0)" % [rdLoc(a), rdLoc(b)])
  else:
    putIntoDest(p, d, e, "($1 == $2)" % [rdLoc(a), rdLoc(b)])

proc genIsNil(p: BProc, e: CgNode, d: var TLoc) =
  let t = skipTypes(e[1].typ, abstractRange)
  if t.kind == tyProc and t.callConv == ccClosure:
    unaryExpr(p, e, d, "($1.ClP_0 == 0)")
  else:
    unaryExpr(p, e, d, "($1 == 0)")

proc unaryArith(p: BProc, e: CgNode, d: var TLoc, op: TMagic) =
  var
    a: TLoc
    t: PType
  assert(e[1].typ != nil)
  initLocExpr(p, e[1], a)
  t = skipTypes(e.typ, abstractRange)

  template applyFormat(frmt: untyped) =
    putIntoDest(p, d, e, frmt % [rdLoc(a), rope(getSize(p.config, t) * 8),
                getSimpleTypeDesc(p.module, e.typ)])
  case op
  of mNot:
    applyFormat("!($1)")
  of mUnaryPlusI:
    applyFormat("$1")
  of mBitnotI:
    applyFormat("($3)((NU$2) ~($1))")
  of mUnaryPlusF64:
    applyFormat("$1")
  of mUnaryMinusF64:
    applyFormat("-($1)")
  else:
    assert false, $op

proc genDeref(p: BProc, e: CgNode, d: var TLoc) =
  let
    src = e.operand
    mt = mapType(p.config, src.typ, mapTypeChooser(p, src))
  if mt in {ctArray, ctPtrToArray} and lfEnforceDeref notin d.flags:
    # XXX the amount of hacks for C's arrays is incredible, maybe we should
    # simply wrap them in a struct? --> Losing auto vectorization then?
    expr(p, src, d)
    if src.typ.skipTypes(abstractInst).kind == tyRef:
      d.storage = OnHeap
  else:
    var a: TLoc
    var typ = src.typ
    if typ.kind in {tyUserTypeClass, tyUserTypeClassInst} and typ.isResolvedUserTypeClass:
      typ = typ.lastSon
    typ = typ.skipTypes(abstractInst)
    initLocExprSingleUse(p, src, a)
    if d.k == locNone:
      # dest = *a;  <-- We do not know that 'dest' is on the heap!
      # It is completely wrong to set 'd.storage' here, unless it's not yet
      # been assigned to.
      case typ.kind
      of tyRef:
        d.storage = OnHeap
      of tyVar, tyLent:
        d.storage = OnUnknown
      of tyPtr:
        d.storage = OnUnknown         # BUGFIX!
      else:
        internalError(p.config, e.info, "genDeref " & $typ.kind)

    if mt == ctPtrToArray and lfEnforceDeref in d.flags:
      # we lie about the type for better C interop: 'ptr array[3,T]' is
      # translated to 'ptr T', but for deref'ing this produces wrong code.
      # See tmissingderef. So we get rid of the deref instead. The codegen
      # ends up using 'memcpy' for the array assignment,
      # so the '&' and '*' cancel out:
      putIntoDest(p, d, e, rdLoc(a), a.storage)
    else:
      # in C89, dereferencing a pointer requires a pointer to complete type.
      # Make sure that the element type is fully defined by querying its name:
      discard getTypeDesc(p.module, e.typ, skVar)
      putIntoDest(p, d, e, "(*$1)" % [rdLoc(a)], a.storage)

proc genAddr(p: BProc, e: CgNode, mutate: bool, d: var TLoc) =
  if mapType(p.config, e.operand.typ, mapTypeChooser(p, e.operand)) == ctArray:
    expr(p, e.operand, d)
  else:
    var a: TLoc
    initLoc(a, locNone, e.operand, OnUnknown)
    a.flags.incl lfWantLvalue
    if mutate:
      a.flags.incl lfPrepareForMutation

    expr(p, e.operand, a)
    putIntoDest(p, d, e, addrLoc(p.config, a), a.storage)

template inheritLocation(d: var TLoc, a: TLoc) =
  if d.k == locNone: d.storage = a.storage

proc genRecordFieldAux(p: BProc, e: CgNode, d, a: var TLoc) =
  initLocExpr(p, e[0], a)
  internalAssert(p.config, e[1].kind == cnkField, e.info, "genRecordFieldAux")
  d.inheritLocation(a)
  discard getTypeDesc(p.module, a.t) # fill the record's fields.loc

proc genTupleElem(p: BProc, e: CgNode, d: var TLoc) =
  var
    a: TLoc
  initLocExpr(p, e[0], a)
  let tupType = a.t.skipTypes(abstractInst+{tyVar})
  assert tupType.kind == tyTuple
  d.inheritLocation(a)
  discard getTypeDesc(p.module, a.t) # fill the record's fields.loc
  var r = rdLoc(a)
  internalAssert(p.config, e[1].kind == cnkIntLit, e.info)
  r.addf(".Field$1", [rope(e[1].intVal)])
  putIntoDest(p, d, e, r, a.storage)

proc lookupFieldAgain(p: BProc, ty: PType; field: PSym; r: var Rope;
                      resTyp: ptr PType = nil): PSym =
  var ty = ty
  assert r != ""
  while ty != nil:
    ty = ty.skipTypes(skipPtrs)
    assert ty.kind == tyObject
    result = lookupInRecord(ty.n, field.name)
    if result != nil:
      if resTyp != nil: resTyp[] = ty
      break
    r.add(".Sup")
    ty = ty[0]
  if result == nil: internalError(p.config, field.info, "genCheckedRecordField")

proc genRecordField(p: BProc, e: CgNode, d: var TLoc) =
  var a: TLoc
  genRecordFieldAux(p, e, d, a)
  var r = rdLoc(a)
  var f = e[1].sym
  let ty = skipTypes(a.t, abstractInst + tyUserTypeClasses)
  p.config.internalAssert(ty.kind == tyObject, e[0].info)
  if true:
    var rtyp: PType
    let field = lookupFieldAgain(p, ty, f, r, addr rtyp)
    ensureObjectFields(p.module, field, rtyp)
    r.addf(".$1", [p.fieldName(field)])
    putIntoDest(p, d, e, r, a.storage)

proc genInExprAux(p: BProc, e: CgNode, a, b, d: var TLoc)

proc genFieldCheck(p: BProc, e: CgNode) =
  var test, u, v: TLoc
  if true:
    initLocExpr(p, e[1], u)
    initLocExpr(p, e[2], v)
    genInExprAux(p, e, u, v, test)
    var msg = ""
    if optDeclaredLocs in p.config.globalOptions:
      # xxx this should be controlled by a separate flag, and
      # used for other similar defects so that location information is shown
      # even without the expensive `--stacktrace`; binary size could be optimized
      # by encoding the file names separately from `file(line:col)`, essentially
      # passing around `TLineInfo` + the set of files in the project.
      msg.add toFileLineCol(p.config, e.info) & " "
    msg.add e[4].strVal
    let strLit = genStringLiteral(p.module, newStrNode(msg))

    ## discriminant check
    template fun(code) = linefmt(p, cpsStmts, code, [rdLoc(test)])
    if e[3].intVal == 1:
      # the third operand indicates whether the result needs to be inverted
      fun("if ($1) ")
    else:
      fun("if (!($1)) ")

    let base = v.t.skipTypes(abstractRange)
    var raiseProc, toStr: string
    # generate and emit the code for the failure case:
    case base.kind
    of tyEnum:
      # use the compiler-generated enum-to-string procedure
      let prc = p.module.g.graph.getToStringProc(v.t)
      p.module.extra.add prc # late dependency

      var tmp: TLoc
      expr(p, newSymNode(prc), tmp)
      toStr = "$1($2)" % [rdLoc(tmp), rdLoc(v)]
      raiseProc = "raiseFieldErrorStr"

    of tyChar:
      # XXX: rendering as a character is supported by the runtime
      #raiseProc = "raiseFieldErrorChar"
      toStr = rdCharLoc(v)
      raiseProc = "raiseFieldErrorUInt"
    of tyBool:
      raiseProc = "raiseFieldErrorBool"
    of tyInt..tyInt64:
      raiseProc = "raiseFieldErrorInt"
    of tyUInt..tyUInt64:
      raiseProc = "raiseFieldErrorUInt"
    else:
      discard
      # unreachable()

    if toStr == "":
      toStr = rdLoc(v)

    discard cgsym(p.module, raiseProc) # make sure the compilerproc is generated
    linefmt(p, cpsStmts, "{ $1($3, $4); $2} $n",
            [raiseProc, raiseInstr(p), strLit, toStr])

proc genUncheckedArrayElem(p: BProc, n, x, y: CgNode, d: var TLoc) =
  var a, b: TLoc
  initLocExpr(p, x, a)
  initLocExpr(p, y, b)
  d.inheritLocation(a)
  putIntoDest(p, d, n, ropecg(p.module, "$1[$2]", [rdLoc(a), rdCharLoc(b)]),
              a.storage)

proc genArrayElem(p: BProc, n, x, y: CgNode, d: var TLoc) =
  var a, b: TLoc
  initLocExpr(p, x, a)
  initLocExpr(p, y, b)
  var ty = skipTypes(a.t, abstractVarRange + abstractPtrs + tyUserTypeClasses)
  var first = intLiteral(firstOrd(p.config, ty))

  d.inheritLocation(a)
  putIntoDest(p, d, n,
              ropecg(p.module, "$1[($2)- $3]", [rdLoc(a), rdCharLoc(b), first]), a.storage)

proc genCStringElem(p: BProc, n, x, y: CgNode, d: var TLoc) =
  var a, b: TLoc
  initLocExpr(p, x, a)
  initLocExpr(p, y, b)
  inheritLocation(d, a)
  putIntoDest(p, d, n,
              ropecg(p.module, "$1[$2]", [rdLoc(a), rdCharLoc(b)]), a.storage)

proc genBoundsCheck(p: BProc; arr, a, b: TLoc) =
  # types that map to C pointers need to be skipped here too, since no
  # dereference is generated for ``ptr array`` and the like
  let ty = skipTypes(arr.t, abstractVarRange + {tyPtr, tyRef, tyLent})
  case ty.kind
  of tyOpenArray, tyVarargs:
    if reifiedOpenArray(p, arr.lode):
      linefmt(p, cpsStmts,
        "if ($2-$1 != -1 && " &
        "((NU)($1) >= (NU)($3.Field1) || (NU)($2) >= (NU)($3.Field1))){ #raiseIndexError(); $4}$n",
        [rdLoc(a), rdLoc(b), rdLoc(arr), raiseInstr(p)])
    else:
      linefmt(p, cpsStmts,
        "if ($2-$1 != -1 && " &
        "((NU)($1) >= (NU)($3Len_0) || (NU)($2) >= (NU)($3Len_0))){ #raiseIndexError(); $4}$n",
        [rdLoc(a), rdLoc(b), rdLoc(arr), raiseInstr(p)])
  of tyArray:
    let first = intLiteral(firstOrd(p.config, ty))
    linefmt(p, cpsStmts,
      "if ($2-$1 != -1 && " &
      "($2-$1 < -1 || $1 < $3 || $1 > $4 || $2 < $3 || $2 > $4)){ #raiseIndexError(); $5}$n",
      [rdCharLoc(a), rdCharLoc(b), first, intLiteral(lastOrd(p.config, ty)), raiseInstr(p)])
  of tySequence, tyString:
    linefmt(p, cpsStmts,
      "if ($2-$1 != -1 && " &
      "((NU)($1) >= (NU)$3 || (NU)($2) >= (NU)$3)){ #raiseIndexError(); $4}$n",
      [rdLoc(a), rdLoc(b), lenExpr(p, arr), raiseInstr(p)])
  of tyUncheckedArray, tyCstring:
    discard "no checks are used"
  else:
    unreachable(ty.kind)

proc genIndexCheck(p: BProc; x: CgNode, arr, idx: TLoc) =
  ## Emits the index check logic + subsequent raise operation. `x` is
  ## the array expression the `arr` loc resulted from from.
  let ty = arr.t.skipTypes(abstractVar + tyUserTypeClasses +
                           {tyPtr, tyRef, tyLent, tyVar})
  case ty.kind
  of tyArray:
    var first = intLiteral(firstOrd(p.config, ty))
    if firstOrd(p.config, ty) == 0 and lastOrd(p.config, ty) >= 0:
      linefmt(p, cpsStmts, "if ((NU)($1) > (NU)($2)){ #raiseIndexError2($1, $2); $3}$n",
              [rdCharLoc(idx), intLiteral(lastOrd(p.config, ty)),
               raiseInstr(p)])
    else:
      linefmt(p, cpsStmts, "if ($1 < $2 || $1 > $3){ #raiseIndexError3($1, $2, $3); $4}$n",
              [rdCharLoc(idx), first, intLiteral(lastOrd(p.config, ty)),
               raiseInstr(p)])
  of tySequence, tyString:
    linefmt(p, cpsStmts,
            "if ((NU)($1) >= (NU)$2){ #raiseIndexError2($1,$2-1); $3}$n",
            [rdCharLoc(idx), lenExpr(p, arr), raiseInstr(p)])
  of tyOpenArray, tyVarargs:
    if reifiedOpenArray(p, x):
      linefmt(p, cpsStmts, "if ((NU)($1) >= (NU)($2.Field1)){ #raiseIndexError2($1,$2.Field1-1); $3}$n",
              [rdCharLoc(idx), rdLoc(arr), raiseInstr(p)])
    else:
      linefmt(p, cpsStmts, "if ((NU)($1) >= (NU)($2Len_0)){ #raiseIndexError2($1,$2Len_0-1); $3}$n",
              [rdCharLoc(idx), rdLoc(arr), raiseInstr(p)])
  of tyCstring:
    discard "no bound checks"
  else:
    unreachable()

proc genOpenArrayElem(p: BProc, n, x, y: CgNode, d: var TLoc) =
  var a, b: TLoc
  initLocExpr(p, x, a)
  initLocExpr(p, y, b)
  if not reifiedOpenArray(p, x):
    inheritLocation(d, a)
    putIntoDest(p, d, n,
                ropecg(p.module, "$1[$2]", [rdLoc(a), rdCharLoc(b)]), a.storage)
  else:
    inheritLocation(d, a)
    putIntoDest(p, d, n,
                ropecg(p.module, "$1.Field0[$2]", [rdLoc(a), rdCharLoc(b)]), a.storage)

proc genSeqElem(p: BProc, n, x, y: CgNode, d: var TLoc) =
  var a, b: TLoc
  initLocExpr(p, x, a)
  initLocExpr(p, y, b)
  var ty = skipTypes(a.t, abstractVarRange)
  if ty.kind in {tyRef, tyPtr}:
    ty = skipTypes(ty.lastSon, abstractVarRange)
  if d.k == locNone: d.storage = OnHeap
  if skipTypes(a.t, abstractVar).kind in {tyRef, tyPtr}:
    a.r = ropecg(p.module, "(*$1)", [a.r])

  if lfPrepareForMutation in d.flags and ty.kind == tyString:
    linefmt(p, cpsStmts, "#nimPrepareStrMutationV2($1);$n", [byRefLoc(p, a)])
  putIntoDest(p, d, n,
              ropecg(p.module, "$1$3[$2]", [rdLoc(a), rdCharLoc(b), dataField(p)]), a.storage)

proc genArrayLikeElem(p: BProc; n: CgNode; d: var TLoc) =
  let ty = skipTypes(n[0].typ, abstractVar + tyUserTypeClasses)
  case ty.kind
  of tyUncheckedArray: genUncheckedArrayElem(p, n, n[0], n[1], d)
  of tyArray: genArrayElem(p, n, n[0], n[1], d)
  of tyOpenArray, tyVarargs: genOpenArrayElem(p, n, n[0], n[1], d)
  of tySequence, tyString: genSeqElem(p, n, n[0], n[1], d)
  of tyCstring: genCStringElem(p, n, n[0], n[1], d)
  else: internalError(p.config, n.info, "expr(nkBracketExpr, " & $ty.kind & ')')
  discard getTypeDesc(p.module, n.typ)

proc genEcho(p: BProc, n: CgNode) =
  ## Generates and emits the code for the magic echo call.
  if n.len == 1:
    linefmt(p, cpsStmts, "#echoBinSafe(NIM_NIL, 0);$n", [])
  else:
    # allocate a temporary array and fill it with the arguments:
    var tmp: TLoc
    getTemp(p, n[1].typ, tmp) # the first argument stores the type to use
    for i in 2..<n.len:
      var a: TLoc
      initLocExpr(p, n[i], a)
      linefmt(p, cpsStmts, "$1[$2] = $3;$n", [rdLoc(tmp), i-2, rdLoc(a)])

    linefmt(p, cpsStmts, "#echoBinSafe($1, $2);$n", [rdLoc(tmp), n.len-2])

proc strLoc(p: BProc; d: TLoc): Rope =
  result = byRefLoc(p, d)

proc genStrConcat(p: BProc, e: CgNode, d: var TLoc) =
  #   <Nim code>
  #   s = 'Hello ' & name & ', how do you feel?' & 'z'
  #
  #   <generated C code>
  #  {
  #    string tmp0;
  #    ...
  #    tmp0 = rawNewString(6 + 17 + 1 + s2->len);
  #    // we cannot generate s = rawNewString(...) here, because
  #    // ``s`` may be used on the right side of the expression
  #    appendString(tmp0, strlit_1);
  #    appendString(tmp0, name);
  #    appendString(tmp0, strlit_2);
  #    appendChar(tmp0, 'z');
  #    asgn(s, tmp0);
  #  }
  var a, tmp: TLoc
  getTemp(p, e.typ, tmp)
  var L = 0
  var appends = ""
  var lens = ""
  for i in 0..<e.len - 1:
    # compute the length expression:
    initLocExpr(p, e[i + 1], a)
    if skipTypes(e[i + 1].typ, abstractVarRange).kind == tyChar:
      inc(L)
      appends.add(ropecg(p.module, "#appendChar($1, $2);$n", [strLoc(p, tmp), rdLoc(a)]))
    else:
      if e[i + 1].kind == cnkStrLit:
        inc(L, e[i + 1].strVal.len)
      else:
        lens.add(lenExpr(p, a))
        lens.add(" + ")
      appends.add(ropecg(p.module, "#appendString($1, $2);$n", [strLoc(p, tmp), rdLoc(a)]))
  linefmt(p, cpsStmts, "$1 = #rawNewString($2$3);$n", [tmp.r, lens, L])
  p.s(cpsStmts).add appends
  if d.k == locNone:
    d = tmp
  else:
    genAssignment(p, d, tmp)

proc genStrAppend(p: BProc, e: CgNode, d: var TLoc) =
  #  <Nim code>
  #  s &= 'Hello ' & name & ', how do you feel?' & 'z'
  #  // BUG: what if s is on the left side too?
  #  <generated C code>
  #  {
  #    s = resizeString(s, 6 + 17 + 1 + name->len);
  #    appendString(s, strlit_1);
  #    appendString(s, name);
  #    appendString(s, strlit_2);
  #    appendChar(s, 'z');
  #  }
  var
    a, dest: TLoc
    appends, lens: Rope
  assert(d.k == locNone)
  var L = 0
  initLocExpr(p, e[1], dest)
  for i in 0..<e.len - 2:
    # compute the length expression:
    initLocExpr(p, e[i + 2], a)
    if skipTypes(e[i + 2].typ, abstractVarRange).kind == tyChar:
      inc(L)
      appends.add(ropecg(p.module, "#appendChar($1, $2);$n",
                        [strLoc(p, dest), rdLoc(a)]))
    else:
      if e[i + 2].kind == cnkStrLit:
        inc(L, e[i + 2].strVal.len)
      else:
        lens.add(lenExpr(p, a))
        lens.add(" + ")
      appends.add(ropecg(p.module, "#appendString($1, $2);$n",
                        [strLoc(p, dest), rdLoc(a)]))

  linefmt(p, cpsStmts, "#prepareAdd($1, $2$3);$n",
          [byRefLoc(p, dest), lens, L])
  p.s(cpsStmts).add appends

proc genDefault(p: BProc; n: CgNode; d: var TLoc) =
  if d.k == locNone:
    getTemp(p, n.typ, d)
  resetLoc(p, d)

proc rawGenNew(p: BProc, a: var TLoc, sizeExpr: Rope; needsInit: bool; doInitObj = true) =
  var sizeExpr = sizeExpr
  let typ = a.t
  var b: TLoc
  initLoc(b, locExpr, a.lode, OnHeap)
  let refType = typ.skipTypes(abstractInst)
  assert refType.kind == tyRef
  let bt = refType.lastSon
  if sizeExpr == "":
    sizeExpr = "sizeof($1)" % [getTypeDesc(p.module, bt)]

  block:
    if needsInit:
      b.r = ropecg(p.module, "($1) #nimNewObj($2, NIM_ALIGNOF($3))",
          [getTypeDesc(p.module, typ), sizeExpr, getTypeDesc(p.module, bt)])
    else:
      b.r = ropecg(p.module, "($1) #nimNewObjUninit($2, NIM_ALIGNOF($3))",
          [getTypeDesc(p.module, typ), sizeExpr, getTypeDesc(p.module, bt)])
    genAssignment(p, a, b)

  if doInitObj:
    # set the object type:
    genObjectInit(p, cpsStmts, bt, a, constructRefObj)

proc genNew(p: BProc, e: CgNode, a: var TLoc) =
  # 'genNew' also handles 'unsafeNew':
  if e.len == 2:
    var se: TLoc
    initLocExpr(p, e[1], se)
    rawGenNew(p, a, se.rdLoc, needsInit = true)
  else:
    rawGenNew(p, a, "", needsInit = true)

proc genNewSeqOfCap(p: BProc; e: CgNode; d: var TLoc) =
  let seqtype = skipTypes(e.typ, abstractVarRange)
  var a: TLoc
  initLocExpr(p, e[1], a)
  block:
    if d.k == locNone: getTemp(p, e.typ, d)
    linefmt(p, cpsStmts, "$1.len = 0; $1.p = ($4*) #newSeqPayload($2, sizeof($3), NIM_ALIGNOF($3));$n",
      [d.rdLoc, a.rdLoc, getTypeDesc(p.module, seqtype.lastSon),
      getSeqPayloadType(p.module, seqtype),
    ])

proc rawConstExpr(p: BProc, n: CgNode; d: var TLoc) =
  let t = n.typ
  discard getTypeDesc(p.module, t) # so that any fields are initialized
  let id = getOrPut(p.module.dataCache, n, p.module.labels)
  fillLoc(d, locData, n, p.module.tmpBase & rope(id), OnStatic)
  if id == p.module.labels:
    # expression not found in the cache:
    inc(p.module.labels)
    p.module.s[cfsData].addf("static NIM_CONST $1 $2 = $3;$n",
          [getTypeDesc(p.module, t), d.r, genBracedInit(p, n, t)])

proc handleConstExpr(p: BProc, n: CgNode, d: var TLoc): bool =
  if d.k == locNone and n.len > 0 and n.isDeepConstExpr:
    rawConstExpr(p, n, d)
    result = true
  else:
    result = false

proc specializeInitObject(p: BProc, accessor: Rope, typ: PType,
                          info: TLineInfo)

proc specializeInitObjectN(p: BProc, accessor: Rope, n: PNode, typ: PType) =
  ## Generates type field initialization code for the record node

  # XXX: this proc shares alot of code with `specializeResetN` (it's based on
  #      a copy of it, after all)
  if n == nil: return
  case n.kind
  of nkRecList:
    for i in 0..<n.len:
      specializeInitObjectN(p, accessor, n[i], typ)
  of nkRecCase:
    p.config.internalAssert(n[0].kind == nkSym, n.info,
                            "specializeInitObjectN")
    let disc = n[0].sym
    ensureObjectFields(p.module, disc, typ)
    lineF(p, cpsStmts, "switch ($1.$2) {$n", [accessor, p.fieldName(disc)])
    for i in 1..<n.len:
      let branch = n[i]
      assert branch.kind in {nkOfBranch, nkElse}
      if branch.kind == nkOfBranch:
        genCaseRange(p, branch)
      else:
        lineF(p, cpsStmts, "default:$n", [])
      specializeInitObjectN(p, accessor, lastSon(branch), typ)
      lineF(p, cpsStmts, "break;$n", [])
    lineF(p, cpsStmts, "} $n", [])
  of nkSym:
    let field = n.sym
    if field.typ.kind == tyVoid: return
    ensureObjectFields(p.module, field, typ)
    specializeInitObject(p, "$1.$2" % [accessor, p.fieldName(field)],
                         field.typ, n.info)
  else: internalError(p.config, n.info, "specializeInitObjectN()")

proc specializeInitObject(p: BProc, accessor: Rope, typ: PType,
                          info: TLineInfo) =
  ## Generates type field (if there are any) initialization code for a
  ## location of type `typ`, where `accessor` is the path of the
  ## location.
  if typ == nil:
    return

  let typ = typ.skipTypes(abstractInst)

  # XXX: this function trades compililation time for run-time efficiency by
  #      potentially performing lots of redundant walks over the same types,
  #      in order to not generate code for records that don't need it. The
  #      better solution would be to run type field analysis only once for
  #      each record type and then cache the result. `cgen` lacks a general
  #      mechanism for caching type related info.
  #      A further improvement would be to emit the code into a separate
  #      function and then just call that

  case typ.kind
  of tyArray:
    # To not generate an empty `for` loop, first check if the array contains
    # any type fields. This optimizes for the case where there are none,
    # making the case where type fields exist slower (compile time)
    if analyseObjectWithTypeField(typ) == frNone:
      return

    let arraySize = lengthOrd(p.config, typ[0])
    var i: TLoc
    getTemp(p, getSysType(p.module.g.graph, info, tyInt), i)
    linefmt(p, cpsStmts, "for ($1 = 0; $1 < $2; $1++) {$n",
            [i.r, arraySize])
    specializeInitObject(p, ropecg(p.module, "$1[$2]", [accessor, i.r]),
                         typ[1], info)
    lineF(p, cpsStmts, "}$n", [])
  of tyObject:
    proc pred(t: PType): bool =
      t.kind == tyObject and not isObjLackingTypeField(t)

    var
      t = typ
      a = accessor

    # walk the type hierarchy and generate object initialization code for
    # all bases that contain type fields
    while t != nil:
      t = t.skipTypes(skipPtrs)

      if t.n != nil and searchTypeNodeFor(t.n, pred):
        specializeInitObjectN(p, a, t.n, t)

      a.add ".Sup"
      t = t.base

    # type header:
    if pred(typ):
      genObjectInitHeader(p, cpsStmts, typ, accessor, info)

  of tyTuple:
    let typ = getUniqueType(typ)
    for i in 0..<typ.len:
      specializeInitObject(p, ropecg(p.module, "$1.Field$2", [accessor, i]),
                           typ[i], info)

  else:
    discard

proc genObjConstr(p: BProc, e: CgNode, d: var TLoc) =
  #echo renderTree e, " ", e.isDeepConstExpr
  when false:
    # disabled optimization: see bug https://github.com/nim-lang/nim/issues/13240
    #[
      var box: seq[Thing]
      for i in 0..3:
        box.add Thing(s1: "121") # pass by sink can mutate Thing.
    ]#
    # TODO: verify whether this is still an issue
    if handleConstExpr(p, e, d): return
  var t = e.typ.skipTypes(abstractInst)
  let isRef = t.kind == tyRef

  # a temporary was injected if in-place construction cannot be used,
  # meaning that we can always construct in-place here (we still have
  # to consider uninitialized and expression locs)
  let useTemp = isRef or d.k == locNone

  # if the object has a record-case, don't initialize type fields before but
  # after initializing discriminators. Otherwise, the type fields in the
  # default branch would be filled, leading to uninitialized fields in other
  # branches not being empty (or having their type fields not set) in case the
  # default branch is not the active one
  let hasCase = block:
    var v = false
    var obj = t
    while obj != nil and not(v):
      obj = obj.skipTypes(abstractPtrs)
      v = isCaseObj(obj.n)
      obj = obj.base

    v

  var tmp: TLoc
  var r: Rope
  if useTemp:
    getTemp(p, t, tmp)
    r = rdLoc(tmp)
    if isRef:
      rawGenNew(p, tmp, "",
                needsInit = true,
                doInitObj = not hasCase)
      t = t.lastSon.skipTypes(abstractInst)
      r = "(*$1)" % [r]
    else:
      constructLoc(p, tmp, doInitObj = not hasCase)
  else:
    resetLoc(p, d, doInitObj = not hasCase)
    r = rdLoc(d)
  discard getTypeDesc(p.module, t)
  let ty = getUniqueType(t)
  for it in e.items:
    var tmp2: TLoc
    tmp2.r = r
    let field = lookupFieldAgain(p, ty, it[0].sym, tmp2.r)
    ensureObjectFields(p.module, field, ty)
    tmp2.r.add(".")
    tmp2.r.add(p.fieldName(field))
    if useTemp:
      tmp2.k = locTemp
      tmp2.storage = if isRef: OnHeap else: OnStack
    else:
      tmp2.k = d.k
      tmp2.storage = if isRef: OnHeap else: d.storage
    tmp2.lode = it[1]
    expr(p, it[1], tmp2)
  if useTemp:
    if d.k == locNone:
      d = tmp
    else:
      genAssignment(p, d, tmp)

  if hasCase:
    # initialize the object's type fields, if there are any

    # XXX: for some discriminators, the value is known at compile-time, so
    #      their switch-case stmt emitted by `specializeInitObject` could be
    #      elided
    var r = rdLoc(d)
    if isRef: r = "(*$1)" % [r]

    specializeInitObject(p, r, t, e.info)

proc genSeqConstr(p: BProc, n: CgNode, d: var TLoc) =
  var arr, tmp: TLoc
  # bug #668
  getTemp(p, n.typ, tmp)

  let l = intLiteral(n.len)
  block:
    let seqtype = n.typ
    linefmt(p, cpsStmts, "$1.len = $2; $1.p = ($4*) #newSeqPayload($2, sizeof($3), NIM_ALIGNOF($3));$n",
      [rdLoc tmp, l, getTypeDesc(p.module, seqtype.lastSon),
      getSeqPayloadType(p.module, seqtype)])

  for i in 0..<n.len:
    initLoc(arr, locExpr, n[i], OnHeap)
    arr.r = ropecg(p.module, "$1$3[$2]", [rdLoc(tmp), intLiteral(i), dataField(p)])
    arr.storage = OnHeap            # we know that sequences are on the heap
    expr(p, n[i], arr)

  if d.k == locNone:
    d = tmp
  else:
    genAssignment(p, d, tmp)

proc genArrToSeq(p: BProc, n: CgNode, d: var TLoc) =
  var elem, a, arr: TLoc
  if n[1].kind == cnkArrayConstr:
    # XXX: dead code, but kept as a reminder
    n[1].typ = n.typ
    genSeqConstr(p, n[1], d)
    return
  if d.k == locNone:
    getTemp(p, n.typ, d)
  # generate call to newSeq before adding the elements per hand:
  let L = toInt(lengthOrd(p.config, n[1].typ))
  block:
    let seqtype = n.typ
    linefmt(p, cpsStmts, "$1.len = $2; $1.p = ($4*) #newSeqPayload($2, sizeof($3), NIM_ALIGNOF($3));$n",
      [rdLoc d, L, getTypeDesc(p.module, seqtype.lastSon),
      getSeqPayloadType(p.module, seqtype)])

  initLocExpr(p, n[1], a)
  # bug #5007; do not produce excessive C source code:
  if L < 10:
    for i in 0..<L:
      initLoc(elem, locExpr, lodeTyp elemType(skipTypes(n.typ, abstractInst)), OnHeap)
      elem.r = ropecg(p.module, "$1$3[$2]", [rdLoc(d), intLiteral(i), dataField(p)])
      elem.storage = OnHeap # we know that sequences are on the heap
      initLoc(arr, locExpr, lodeTyp elemType(skipTypes(n[1].typ, abstractInst)), a.storage)
      arr.r = ropecg(p.module, "$1[$2]", [rdLoc(a), intLiteral(i)])
      genAssignment(p, elem, arr)
  else:
    var i: TLoc
    getTemp(p, getSysType(p.module.g.graph, unknownLineInfo, tyInt), i)
    linefmt(p, cpsStmts, "for ($1 = 0; $1 < $2; $1++) {$n",  [i.r, L])
    initLoc(elem, locExpr, lodeTyp elemType(skipTypes(n.typ, abstractInst)), OnHeap)
    elem.r = ropecg(p.module, "$1$3[$2]", [rdLoc(d), rdLoc(i), dataField(p)])
    elem.storage = OnHeap # we know that sequences are on the heap
    initLoc(arr, locExpr, lodeTyp elemType(skipTypes(n[1].typ, abstractInst)), a.storage)
    arr.r = ropecg(p.module, "$1[$2]", [rdLoc(a), rdLoc(i)])
    genAssignment(p, elem, arr)
    lineF(p, cpsStmts, "}$n", [])

proc genOfHelper(p: BProc; dest: PType; a: Rope; info: TLineInfo): Rope =
  result = ropecg(p.module, "#isObj($1.m_type, $2)",
    [a, genTypeInfo2Name(p.module, dest)])

proc genOf(p: BProc, x: CgNode, typ: PType, d: var TLoc) =
  var a: TLoc
  initLocExpr(p, x, a)
  var dest = skipTypes(typ, typedescPtrs)
  var r = rdLoc(a)
  var nilCheck = ""
  var t = skipTypes(a.t, abstractInst)
  while t.kind in {tyVar, tyLent, tyPtr, tyRef}:
    if t.kind notin {tyVar, tyLent}:
      nilCheck = r
    r = ropecg(p.module, "(*$1)", [r])
    t = skipTypes(t.lastSon, typedescInst)
  discard getTypeDesc(p.module, t)
  while t.kind == tyObject and t[0] != nil:
    r.add(~".Sup")
    t = skipTypes(t[0], skipPtrs)

  if isObjLackingTypeField(t):
    localReport(p.config, x.info, reportSem rsemDisallowedOfForPureObjects)

  if nilCheck != "":
    r = ropecg(p.module, "(($1) && ($2))", [nilCheck, genOfHelper(p, dest, r, x.info)])
  else:
    r = ropecg(p.module, "($1)", [genOfHelper(p, dest, r, x.info)])
  putIntoDest(p, d, x, r, a.storage)

proc genOf(p: BProc, n: CgNode, d: var TLoc) =
  genOf(p, n[1], n[2].typ, d)

proc rdMType(p: BProc; a: TLoc; nilCheck: var Rope; enforceV1 = false): Rope =
  result = rdLoc(a)
  var t = skipTypes(a.t, abstractInst)
  while t.kind in {tyVar, tyLent, tyPtr, tyRef}:
    if t.kind notin {tyVar, tyLent}:
      nilCheck = result
    result = "(*$1)" % [result]
    t = skipTypes(t.lastSon, abstractInst)
  discard getTypeDesc(p.module, t)
  while t.kind == tyObject and t[0] != nil:
    result.add(".Sup")
    t = skipTypes(t[0], skipPtrs)
  result.add ".m_type"
  if enforceV1:
    result.add "->typeInfoV1"

proc genGetTypeInfo(p: BProc, e: CgNode, d: var TLoc) =
  discard cgsym(p.module, "TNimType")
  let t = e[1].typ
  # ordinary static type information
  putIntoDest(p, d, e, genTypeInfoV1(p.module, t, e.info))

proc genGetTypeInfoV2(p: BProc, e: CgNode, d: var TLoc) =
  let t = e[1].typ
  if isFinal(t) or e[0].sym.name.s != "getDynamicTypeInfo":
    # ordinary static type information
    putIntoDest(p, d, e, genTypeInfoV2(p.module, t, e.info))
  else:
    var a: TLoc
    initLocExpr(p, e[1], a)
    var nilCheck = ""
    # use the dynamic type stored at offset 0:
    putIntoDest(p, d, e, rdMType(p, a, nilCheck))

proc genAccessTypeField(p: BProc; e: CgNode; d: var TLoc) =
  var a: TLoc
  initLocExpr(p, e[1], a)
  var nilCheck = ""
  # use the dynamic type stored at offset 0:
  putIntoDest(p, d, e, rdMType(p, a, nilCheck))

template genDollar(p: BProc, n: CgNode, d: var TLoc, frmt: string) =
  var a: TLoc
  initLocExpr(p, n[1], a)
  a.r = ropecg(p.module, frmt, [rdLoc(a)])
  a.flags.excl lfIndirect # this flag should not be propagated here (not just for HCR)
  if d.k == locNone: getTemp(p, n.typ, d)
  genAssignment(p, d, a)

proc genArrayLen(p: BProc, e: CgNode, d: var TLoc, op: TMagic) =
  let a = e[1]
  var typ = skipTypes(a.typ, abstractVar + tyUserTypeClasses)
  case typ.kind
  of tyOpenArray, tyVarargs:
    if true:
      if not reifiedOpenArray(p, a):
        if op == mHigh: unaryExpr(p, e, d, "($1Len_0-1)")
        else: unaryExpr(p, e, d, "$1Len_0")
      else:
        if op == mHigh: unaryExpr(p, e, d, "($1.Field1-1)")
        else: unaryExpr(p, e, d, "$1.Field1")
  of tyCstring:
    if op == mHigh: unaryExpr(p, e, d, "($1 ? (#nimCStrLen($1)-1) : -1)")
    else: unaryExpr(p, e, d, "($1 ? #nimCStrLen($1) : 0)")
  of tyString:
    var a: TLoc
    initLocExpr(p, e[1], a)
    var x = lenExpr(p, a)
    if op == mHigh: x = "($1-1)" % [x]
    putIntoDest(p, d, e, x)
  of tySequence:
    # we go through a temporary here because people write bullshit code.
    var a, tmp: TLoc
    initLocExpr(p, e[1], a)
    getIntTemp(p, tmp)
    var x = lenExpr(p, a)
    if op == mHigh: x = "($1-1)" % [x]
    lineCg(p, cpsStmts, "$1 = $2;$n", [tmp.r, x])
    putIntoDest(p, d, e, tmp.r)
  of tyArray:
    # YYY: length(sideeffect) is optimized away incorrectly?
    if op == mHigh: putIntoDest(p, d, e, rope(lastOrd(p.config, typ)))
    else: putIntoDest(p, d, e, rope(lengthOrd(p.config, typ)))
  else: internalError(p.config, e.info, "genArrayLen()")

proc genSetLengthStr(p: BProc, e: CgNode, d: var TLoc) =
  binaryStmtAddr(p, e, d, "setLengthStrV2")

proc rdSetElemLoc(conf: ConfigRef; a: TLoc, typ: PType): Rope =
  # read a location of an set element; it may need a subtraction operation
  # before the set operation
  result = rdCharLoc(a)
  let setType = typ.skipTypes(abstractPtrs)
  assert(setType.kind == tySet)
  if firstOrd(conf, setType) != 0:
    result = "($1- $2)" % [result, rope(firstOrd(conf, setType))]

proc fewCmps(conf: ConfigRef; s: CgNode): bool =
  # this function estimates whether it is better to emit code
  # for constructing the set or generating a bunch of comparisons directly
  if s.kind != cnkSetConstr: return false
  if (getSize(conf, s.typ) <= conf.target.intSize) and isDeepConstExpr(s):
    result = false            # it is better to emit the set generation code
  elif elemType(s.typ).kind in {tyInt, tyInt16..tyInt64}:
    result = true             # better not emit the set if int is basetype!
  else:
    result = s.len <= 8  # 8 seems to be a good value

template binaryExprIn(p: BProc, e: CgNode, a, b, d: var TLoc, frmt: string) =
  putIntoDest(p, d, e, frmt % [rdLoc(a), rdSetElemLoc(p.config, b, a.t)])

proc genInExprAux(p: BProc, e: CgNode, a, b, d: var TLoc) =
  case int(getSize(p.config, skipTypes(e[1].typ, abstractVar)))
  of 1: binaryExprIn(p, e, a, b, d, "(($1 &((NU8)1<<((NU)($2)&7U)))!=0)")
  of 2: binaryExprIn(p, e, a, b, d, "(($1 &((NU16)1<<((NU)($2)&15U)))!=0)")
  of 4: binaryExprIn(p, e, a, b, d, "(($1 &((NU32)1<<((NU)($2)&31U)))!=0)")
  of 8: binaryExprIn(p, e, a, b, d, "(($1 &((NU64)1<<((NU)($2)&63U)))!=0)")
  else: binaryExprIn(p, e, a, b, d, "(($1[(NU)($2)>>3] &(1U<<((NU)($2)&7U)))!=0)")

template binaryStmtInExcl(p: BProc, e: CgNode, d: var TLoc, frmt: string) =
  var a, b: TLoc
  assert(d.k == locNone)
  initLocExpr(p, e[1], a)
  initLocExpr(p, e[2], b)
  lineF(p, cpsStmts, frmt, [rdLoc(a), rdSetElemLoc(p.config, b, a.t)])

proc genInOp(p: BProc, e: CgNode, d: var TLoc) =
  var a, b, x, y: TLoc
  if (e[1].kind == cnkSetConstr) and fewCmps(p.config, e[1]):
    # a set constructor but not a constant set:
    # do not emit the set, but generate a bunch of comparisons
    # XXX: this is currently dead code, but it can be restored once set
    #      literals are passed to the code generators as constants
    let ea = e[2]
    initLocExpr(p, ea, a)
    initLoc(b, locExpr, e, OnUnknown)
    if e[1].len > 0:
      b.r = rope("(")
      for i in 0..<e[1].len:
        let it = e[1][i]
        if it.kind == cnkRange:
          initLocExpr(p, it[0], x)
          initLocExpr(p, it[1], y)
          b.r.addf("$1 >= $2 && $1 <= $3",
               [rdCharLoc(a), rdCharLoc(x), rdCharLoc(y)])
        else:
          initLocExpr(p, it, x)
          b.r.addf("$1 == $2", [rdCharLoc(a), rdCharLoc(x)])
        if i < e[1].len - 1: b.r.add(" || ")
      b.r.add(")")
    else:
      # handle the case of an empty set
      b.r = rope("0")
    putIntoDest(p, d, e, b.r)
  else:
    assert(e[1].typ != nil)
    assert(e[2].typ != nil)
    initLocExpr(p, e[1], a)
    initLocExpr(p, e[2], b)
    genInExprAux(p, e, a, b, d)

proc genSetOp(p: BProc, e: CgNode, d: var TLoc, op: TMagic) =
  const
    lookupOpr: array[mLeSet..mMinusSet, string] = [
      "for ($1 = 0; $1 < $2; $1++) { $n" &
      "  $3 = (($4[$1] & ~ $5[$1]) == 0);$n" &
      "  if (!$3) break;}$n",
      "for ($1 = 0; $1 < $2; $1++) { $n" &
      "  $3 = (($4[$1] & ~ $5[$1]) == 0);$n" &
      "  if (!$3) break;}$n" &
      "if ($3) $3 = (#nimCmpMem($4, $5, $2) != 0);$n",
      "&",
      "|",
      "& ~"]
  var a, b, i: TLoc
  var setType = skipTypes(e[1].typ, abstractVar)
  var size = int(getSize(p.config, setType))
  case size
  of 1, 2, 4, 8:
    case op
    of mIncl:
      case size
      of 1: binaryStmtInExcl(p, e, d, "$1 |= ((NU8)1)<<(($2) & 7);$n")
      of 2: binaryStmtInExcl(p, e, d, "$1 |= ((NU16)1)<<(($2) & 15);$n")
      of 4: binaryStmtInExcl(p, e, d, "$1 |= ((NU32)1)<<(($2) & 31);$n")
      of 8: binaryStmtInExcl(p, e, d, "$1 |= ((NU64)1)<<(($2) & 63);$n")
      else: assert(false, $size)
    of mExcl:
      case size
      of 1: binaryStmtInExcl(p, e, d, "$1 &= ~(((NU8)1) << (($2) & 7));$n")
      of 2: binaryStmtInExcl(p, e, d, "$1 &= ~(((NU16)1) << (($2) & 15));$n")
      of 4: binaryStmtInExcl(p, e, d, "$1 &= ~(((NU32)1) << (($2) & 31));$n")
      of 8: binaryStmtInExcl(p, e, d, "$1 &= ~(((NU64)1) << (($2) & 63));$n")
      else: assert(false, $size)
    of mCard:
      if size <= 4: unaryExprChar(p, e, d, "#countBits32($1)")
      else: unaryExprChar(p, e, d, "#countBits64($1)")
    of mLtSet: binaryExprChar(p, e, d, "((($1 & ~ $2)==0)&&($1 != $2))")
    of mLeSet: binaryExprChar(p, e, d, "(($1 & ~ $2)==0)")
    of mEqSet: binaryExpr(p, e, d, "($1 == $2)")
    of mMulSet: binaryExpr(p, e, d, "($1 & $2)")
    of mPlusSet: binaryExpr(p, e, d, "($1 | $2)")
    of mMinusSet: binaryExpr(p, e, d, "($1 & ~ $2)")
    of mInSet:
      genInOp(p, e, d)
    else: internalError(p.config, e.info, "genSetOp()")
  else:
    case op
    of mIncl: binaryStmtInExcl(p, e, d, "$1[(NU)($2)>>3] |=(1U<<($2&7U));$n")
    of mExcl: binaryStmtInExcl(p, e, d, "$1[(NU)($2)>>3] &= ~(1U<<($2&7U));$n")
    of mCard:
      var a: TLoc
      initLocExpr(p, e[1], a)
      putIntoDest(p, d, e, ropecg(p.module, "#cardSet($1, $2)", [rdCharLoc(a), size]))
    of mLtSet, mLeSet:
      getTemp(p, getSysType(p.module.g.graph, unknownLineInfo, tyInt), i) # our counter
      initLocExpr(p, e[1], a)
      initLocExpr(p, e[2], b)
      if d.k == locNone: getTemp(p, getSysType(p.module.g.graph, unknownLineInfo, tyBool), d)
      if op == mLtSet:
        linefmt(p, cpsStmts, lookupOpr[mLtSet],
           [rdLoc(i), size, rdLoc(d), rdLoc(a), rdLoc(b)])
      else:
        linefmt(p, cpsStmts, lookupOpr[mLeSet],
           [rdLoc(i), size, rdLoc(d), rdLoc(a), rdLoc(b)])
    of mEqSet:
      var a, b: TLoc
      assert(e[1].typ != nil)
      assert(e[2].typ != nil)
      initLocExpr(p, e[1], a)
      initLocExpr(p, e[2], b)
      putIntoDest(p, d, e, ropecg(p.module, "(#nimCmpMem($1, $2, $3)==0)", [a.rdCharLoc, b.rdCharLoc, size]))
    of mMulSet, mPlusSet, mMinusSet:
      # we inline the simple for loop for better code generation:
      getTemp(p, getSysType(p.module.g.graph, unknownLineInfo, tyInt), i) # our counter
      initLocExpr(p, e[1], a)
      initLocExpr(p, e[2], b)
      if d.k == locNone: getTemp(p, setType, d)
      lineF(p, cpsStmts,
           "for ($1 = 0; $1 < $2; $1++) $n" &
           "  $3[$1] = $4[$1] $6 $5[$1];$n", [
          rdLoc(i), rope(size), rdLoc(d), rdLoc(a), rdLoc(b),
          rope(lookupOpr[op])])
    of mInSet: genInOp(p, e, d)
    else: internalError(p.config, e.info, "genSetOp")

proc genOrd(p: BProc, e: CgNode, d: var TLoc) =
  unaryExprChar(p, e, d, "$1")

proc genSomeCast(p: BProc, e: CgNode, d: var TLoc) =
  const
    ValueTypes = {tyTuple, tyObject, tyArray, tyOpenArray, tyVarargs, tyUncheckedArray}

  let src =
    case e.kind
    of cnkCast, cnkConv, cnkHiddenConv: e.operand
    of cnkCall:                         e[1]
    else:                               unreachable()
  # we use whatever C gives us. Except if we have a value-type, we need to go
  # through its address:
  var a: TLoc
  initLocExpr(p, src, a)
  let etyp = skipTypes(e.typ, abstractRange)
  let srcTyp = skipTypes(src.typ, abstractRange)
  if etyp.kind in ValueTypes and lfIndirect notin a.flags:
    putIntoDest(p, d, e, "(*($1*) ($2))" %
        [getTypeDesc(p.module, e.typ), addrLoc(p.config, a)], a.storage)
  elif etyp.kind == tyProc and etyp.callConv == ccClosure and srcTyp.callConv != ccClosure:
    putIntoDest(p, d, e, "(($1) ($2))" %
        [getClosureType(p.module, etyp, clHalfWithEnv), rdCharLoc(a)], a.storage)
  else:
    # C++ does not like direct casts from pointer to shorter integral types
    # QUESTION: should we keep this as a matter of hygiene?
    if srcTyp.kind in {tyPtr, tyPointer} and etyp.kind in IntegralTypes:
      putIntoDest(p, d, e, "(($1) (ptrdiff_t) ($2))" %
          [getTypeDesc(p.module, e.typ), rdCharLoc(a)], a.storage)
    elif etyp.kind in {tySequence, tyString}:
      putIntoDest(p, d, e, "(*($1*) (&$2))" %
          [getTypeDesc(p.module, e.typ), rdCharLoc(a)], a.storage)
    elif etyp.kind == tyBool and srcTyp.kind in IntegralTypes:
      putIntoDest(p, d, e, "(($1) != 0)" % [rdCharLoc(a)], a.storage)
    else:
      putIntoDest(p, d, e, "(($1) ($2))" %
          [getTypeDesc(p.module, e.typ), rdCharLoc(a)], a.storage)

proc genCast(p: BProc, e: CgNode, d: var TLoc) =
  const ValueTypes = {tyFloat..tyFloat64, tyTuple, tyObject, tyArray}
  let
    src = e.operand
    destt = skipTypes(e.typ, abstractRange)
    srct = skipTypes(src.typ, abstractRange)
  if destt.kind in ValueTypes or srct.kind in ValueTypes:
    # 'cast' and some float type involved? --> use a union.
    inc(p.labels)
    var lbl = p.labels.rope
    var tmp: TLoc
    tmp.r = "LOC$1.source" % [lbl]
    linefmt(p, cpsLocals, "union { $1 source; $2 dest; } LOC$3;$n",
      [getTypeDesc(p.module, src.typ), getTypeDesc(p.module, e.typ), lbl])
    tmp.k = locExpr
    tmp.lode = lodeTyp srct
    tmp.storage = OnStack
    tmp.flags = {}
    expr(p, src, tmp)
    putIntoDest(p, d, e, "LOC$#.dest" % [lbl], tmp.storage)
  else:
    # I prefer the shorter cast version for pointer types -> generate less
    # C code; plus it's the right thing to do for closures:
    genSomeCast(p, e, d)

proc genRangeChck(p: BProc, n: CgNode, d: var TLoc) =
  var a: TLoc
  var dest = skipTypes(n.typ, abstractVar)
  initLocExpr(p, n[1], a)
  if true:
    let n0t = n[1].typ

    # emit range check:
    if n0t.kind in {tyUInt, tyUInt64}:
      linefmt(p, cpsStmts, "if ($1 > ($6)($3)){ #raiseRangeErrorNoArgs(); $5}$n",
        [rdCharLoc(a), genLiteral(p, n[2], dest), genLiteral(p, n[3], dest),
        raiser, raiseInstr(p), getTypeDesc(p.module, n0t)])
    else:
      let raiser =
        case skipTypes(n.typ, abstractVarRange).kind
        of tyUInt..tyUInt64, tyChar: "raiseRangeErrorU"
        of tyFloat..tyFloat64: "raiseRangeErrorF"
        else: "raiseRangeErrorI"
      discard cgsym(p.module, raiser)

      let boundaryCast =
        if n0t.skipTypes(abstractVarRange).kind in {tyUInt, tyUInt32, tyUInt64} or
            (n0t.sym != nil and sfSystemModule in n0t.sym.owner.flags and n0t.sym.name.s == "csize"):
          "(NI64)"
        else:
          ""
      linefmt(p, cpsStmts, "if ($6($1) < $2 || $6($1) > $3){ $4($1, $2, $3); $5}$n",
        [rdCharLoc(a), genLiteral(p, n[2], dest), genLiteral(p, n[3], dest),
        raiser, raiseInstr(p), boundaryCast])
  putIntoDest(p, d, n, "(($1) ($2))" %
      [getTypeDesc(p.module, dest), rdCharLoc(a)], a.storage)

proc genConv(p: BProc, e: CgNode, d: var TLoc) =
  let destType = e.typ.skipTypes({tyVar, tyLent, tyGenericInst, tyAlias, tySink})
  if sameBackendType(destType, e.operand.typ):
    expr(p, e.operand, d)
  else:
    genSomeCast(p, e, d)

proc convStrToCStr(p: BProc, n: CgNode, d: var TLoc) =
  var a: TLoc
  initLocExpr(p, n.operand, a)
  putIntoDest(p, d, n,
              ropecg(p.module, "#nimToCStringConv($1)", [rdLoc(a)]),
#                "($1 ? $1->data : (NCSTRING)\"\")" % [a.rdLoc],
              a.storage)

proc convCStrToStr(p: BProc, n: CgNode, d: var TLoc) =
  var a: TLoc
  initLocExpr(p, n.operand, a)
  putIntoDest(p, d, n,
              ropecg(p.module, "#cstrToNimstr($1)", [rdLoc(a)]),
              a.storage)

proc genStrEquals(p: BProc, e: CgNode, d: var TLoc) =
  var x: TLoc
  var a = e[1]
  var b = e[2]
  if a.kind == cnkStrLit and a.strVal == "":
    initLocExpr(p, e[2], x)
    putIntoDest(p, d, e,
      ropecg(p.module, "($1 == 0)", [lenExpr(p, x)]))
  elif b.kind == cnkStrLit and b.strVal == "":
    initLocExpr(p, e[1], x)
    putIntoDest(p, d, e,
      ropecg(p.module, "($1 == 0)", [lenExpr(p, x)]))
  else:
    binaryExpr(p, e, d, "#eqStrings($1, $2)")

proc binaryFloatArith(p: BProc, e: CgNode, d: var TLoc, m: TMagic) =
  if {optNaNCheck, optInfCheck} * p.options != {}:
    const opr: array[mAddF64..mDivF64, string] = ["+", "-", "*", "/"]
    var a, b: TLoc
    assert(e[1].typ != nil)
    assert(e[2].typ != nil)
    initLocExpr(p, e[1], a)
    initLocExpr(p, e[2], b)
    putIntoDest(p, d, e, ropecg(p.module, "(($4)($2) $1 ($4)($3))",
                              [opr[m], rdLoc(a), rdLoc(b),
                              getSimpleTypeDesc(p.module, e[1].typ)]))
    if optNaNCheck in p.options:
      linefmt(p, cpsStmts, "if ($1 != $1){ #raiseFloatInvalidOp(); $2}$n", [rdLoc(d), raiseInstr(p)])
    if optInfCheck in p.options:
      linefmt(p, cpsStmts, "if ($1 != 0.0 && $1*0.5 == $1) { #raiseFloatOverflow($1); $2}$n", [rdLoc(d), raiseInstr(p)])
  else:
    binaryArith(p, e, d, m)

proc skipAddr(n: CgNode): CgNode =
  if n.kind == cnkHiddenAddr: n.operand
  else:                       n

proc genWasMoved(p: BProc; n: CgNode) =
  var a: TLoc
  let n1 = n[1].skipAddr
  if true:
    initLocExpr(p, n1, a, {lfWantLvalue})
    resetLoc(p, a)
    #linefmt(p, cpsStmts, "#nimZeroMem((void*)$1, sizeof($2));$n",
    #  [addrLoc(p.config, a), getTypeDesc(p.module, a.t)])

proc genMove(p: BProc; n: CgNode; d: var TLoc) =
  var a: TLoc
  initLocExpr(p, n[1], a, {lfWantLvalue})
  if true:
    if d.k == locNone: getTemp(p, n.typ, d)
    genAssignment(p, d, a)
    resetLoc(p, a)

proc genDestroy(p: BProc; n: CgNode) =
    let arg = n[1].skipAddr
    let t = arg.typ.skipTypes(abstractInst)
    case t.kind
    of tyString:
      var a: TLoc
      initLocExpr(p, arg, a)
      if optThreads in p.config.globalOptions:
        linefmt(p, cpsStmts, "if ($1.p && !($1.p->cap & NIM_STRLIT_FLAG)) {$n" &
          " #deallocShared($1.p);$n" &
          "}$n", [rdLoc(a)])
      else:
        linefmt(p, cpsStmts, "if ($1.p && !($1.p->cap & NIM_STRLIT_FLAG)) {$n" &
          " #dealloc($1.p);$n" &
          "}$n", [rdLoc(a)])
    of tySequence:
      var a: TLoc
      initLocExpr(p, arg, a)
      linefmt(p, cpsStmts, "if ($1.p && !($1.p->cap & NIM_STRLIT_FLAG)) {$n" &
        " #alignedDealloc($1.p, NIM_ALIGNOF($2));$n" &
        "}$n",
        [rdLoc(a), getTypeDesc(p.module, t.lastSon)])
    else: discard "nothing to do"

proc genSlice(p: BProc; e: CgNode; d: var TLoc) =
  let (x, y) = genOpenArraySlice(p, e, e.typ,
                                 e.typ.skipTypes(abstractVar).base)
  if d.k == locNone: getTemp(p, e.typ, d)
  linefmt(p, cpsStmts, "$1.Field0 = $2; $1.Field1 = $3;$n", [rdLoc(d), x, y])
  when false:
    localReport(p.config, e.info, "invalid context for 'toOpenArray'; " &
      "'toOpenArray' is only valid within a call expression")

proc genBreakState(p: BProc, n: CgNode, d: var TLoc) =
  ## Generates the code for the ``mFinished`` magic, which tests if a
  ## closure iterator is in the "finished" state (i.e. the internal
  ## ``state`` field has a value < 0).
  var
    a: TLoc
    r: string

  let arg = n[1]
  if arg.kind == cnkClosureConstr:
    # XXX: dead code, but kept as a reminder on what to eventually restore
    initLocExpr(p, arg[1], a)
    r = "(((NI*) $1)[1] < 0)" % [rdLoc(a)]
  else:
    initLocExpr(p, arg, a)
    # the environment is guaranteed to contain the 'state' field at offset 1:
    r = "((((NI*) $1.ClE_0)[1]) < 0)" % [rdLoc(a)]

  putIntoDest(p, d, n, r)

proc genMagicExpr(p: BProc, e: CgNode, d: var TLoc, op: TMagic) =
  case op
  of mNot..mUnaryMinusF64: unaryArith(p, e, d, op)
  of mUnaryMinusI..mAbsI: unaryArithOverflow(p, e, d, op)
  of mAddF64..mDivF64: binaryFloatArith(p, e, d, op)
  of mShrI..mXor: binaryArith(p, e, d, op)
  of mEqProc: genEqProc(p, e, d)
  of mAddI..mPred: binaryArithOverflow(p, e, d, op)
  of mGetTypeInfo: genGetTypeInfo(p, e, d)
  of mGetTypeInfoV2: genGetTypeInfoV2(p, e, d)
  of mConStrStr: genStrConcat(p, e, d)
  of mAppendStrCh:
    binaryStmtAddr(p, e, d, "nimAddCharV1")
  of mAppendStrStr: genStrAppend(p, e, d)
  of mAppendSeqElem, mNewSeq, mSetLengthSeq:
    genCall(p, e, d)
  of mEqStr: genStrEquals(p, e, d)
  of mLeStr: binaryExpr(p, e, d, "(#cmpStrings($1, $2) <= 0)")
  of mLtStr: binaryExpr(p, e, d, "(#cmpStrings($1, $2) < 0)")
  of mIsNil: genIsNil(p, e, d)
  of mBoolToStr: genDollar(p, e, d, "#nimBoolToStr($1)")
  of mCharToStr: genDollar(p, e, d, "#nimCharToStr($1)")
  of mCStrToStr: genDollar(p, e, d, "#cstrToNimstr($1)")
  of mStrToStr: expr(p, e[1], d)
  of mIsolate: genCall(p, e, d)
  of mFinished: genBreakState(p, e, d)
  of mEnumToStr: genCall(p, e, d)
  of mOf: genOf(p, e, d)
  of mNew: genNew(p, e, d)
  of mNewSeqOfCap: genNewSeqOfCap(p, e, d)
  of mSizeOf:
    let t = e[1].typ.skipTypes({tyTypeDesc})
    putIntoDest(p, d, e, "((NI)sizeof($1))" % [getTypeDesc(p.module, t, skVar)])
  of mAlignOf:
    let t = e[1].typ.skipTypes({tyTypeDesc})
    putIntoDest(p, d, e, "((NI)NIM_ALIGNOF($1))" % [getTypeDesc(p.module, t, skVar)])
  of mOffsetOf:
    var dotExpr: CgNode
    case e[1].kind
    of cnkFieldAccess, cnkTupleAccess:
      dotExpr = e[1]
    else:
      internalError(p.config, e.info, "unknown ast")
    let t = dotExpr[0].typ.skipTypes({tyTypeDesc})
    let tname = getTypeDesc(p.module, t, skVar)
    let member =
      if dotExpr.kind == cnkTupleAccess:
        "Field" & rope(dotExpr[1].intVal)
      else: p.fieldName(dotExpr[1].sym)
    putIntoDest(p,d,e, "((NI)offsetof($1, $2))" % [tname, member])
  of mChr: genSomeCast(p, e, d)
  of mOrd: genOrd(p, e, d)
  of mLengthArray, mHigh, mLengthStr, mLengthSeq, mLengthOpenArray:
    genArrayLen(p, e, d, op)
  of mGCref: unaryStmt(p, e, d, "if ($1) { #nimGCref($1); }$n")
  of mGCunref: unaryStmt(p, e, d, "if ($1) { #nimGCunref($1); }$n")
  of mSetLengthStr: genSetLengthStr(p, e, d)
  of mIncl, mExcl, mCard, mLtSet, mLeSet, mEqSet, mMulSet, mPlusSet, mMinusSet,
     mInSet:
    genSetOp(p, e, d, op)
  of mNewString, mNewStringOfCap, mExit, mParseBiggestFloat:
    var opr = e[0].sym
    # Why would anyone want to set nodecl to one of these hardcoded magics?
    # - not sure, and it wouldn't work if the symbol behind the magic isn't
    #   somehow forward-declared from some other usage, but it is *possible*
    if exfNoDecl notin opr.extFlags:
      let prc = magicsys.getCompilerProc(p.module.g.graph, opr.extname)
      assert prc != nil, opr.extname
      # Make the function behind the magic get actually generated
      discard cgsym(p.module, opr.extname)

    genCall(p, e, d)
  of mDefault: genDefault(p, e, d)
  of mEcho: genEcho(p, e)
  of mArrToSeq: genArrToSeq(p, e, d)
  of mNLen..mNError, mStatic..mQuoteAst:
    localReport(p.config, e.info, reportSym(
      rsemConstExpressionExpected, e[0].sym))

  of mDeepCopy:
    if p.config.selectedGC in {gcArc, gcOrc} and optEnableDeepCopy notin p.config.globalOptions:
      localReport(p.config, e.info, reportSem rsemRequiresDeepCopyEnabled)

    var a, b: TLoc
    let x = if e[1].kind == cnkHiddenAddr: e[1].operand else: e[1]
    initLocExpr(p, x, a)
    initLocExpr(p, e[2], b)
    genDeepCopy(p, a, b)
  of mDotDot, mEqCString: genCall(p, e, d)
  of mWasMoved: genWasMoved(p, e)
  of mMove: genMove(p, e, d)
  of mDestroy: genDestroy(p, e)
  of mAccessTypeField: genAccessTypeField(p, e, d)
  of mTrace: discard "no code to generate"
  of mAsgnDynlibVar:
    # initialize the internal pointer for a dynlib global/procedure
    var a, b: TLoc
    initLocExpr(p, e[1].operand, a)
    initLocExpr(p, e[2], b)
    var typ = getTypeDesc(p.module, a.t)
    # dynlib variables are stored as pointers
    if lfIndirect in a.flags:
      typ.add "*"

    linefmt(p, cpsStmts, "$1 = ($2)($3);$n", [a.r, typ, rdLoc(b)])
  of mChckRange:
    genRangeChck(p, e, d)
  of mChckIndex:
    var arr, a: TLoc
    initLocExpr(p, e[1], arr)
    initLocExpr(p, e[2], a)
    genIndexCheck(p, e[1], arr, a)
  of mChckBounds:
    var arr, a, b: TLoc
    initLocExpr(p, e[1], arr)
    initLocExpr(p, e[2], a)
    initLocExpr(p, e[3], b)
    genBoundsCheck(p, arr, a, b)
  of mChckField:
    genFieldCheck(p, e)
  of mSamePayload:
    var a, b: TLoc
    initLocExpr(p, e[1], a)
    initLocExpr(p, e[2], b)
    # compare the payloads:
    putIntoDest(p, d, e, "($1.p == $2.p)" % [rdLoc(a), rdLoc(b)])
  of mCopyInternal:
    # copy the content of the type field from b to a
    var a, b: TLoc
    var check: Rope
    initLocExpr(p, e[1].operand, a)
    initLocExpr(p, e[2], b)
    linefmt(p, cpsStmts, "$1 = $2;$n", [rdMType(p, a, check),
                                        rdMType(p, b, check)])
  else:
    when defined(debugMagics):
      echo p.prc.name.s, " ", p.prc.id, " ", p.prc.flags, " ", p.prc.ast[genericParamsPos].kind
    internalError(p.config, e.info, "genMagicExpr: " & $op)

proc genSetConstr(p: BProc, e: CgNode, d: var TLoc) =
  # example: { a..b, c, d, e, f..g }
  # we have to emit an expression of the form:
  # nimZeroMem(tmp, sizeof(tmp)); inclRange(tmp, a, b); incl(tmp, c);
  # incl(tmp, d); incl(tmp, e); inclRange(tmp, f, g);
  var
    a, b, idx: TLoc
  if true:
    if d.k == locNone: getTemp(p, e.typ, d)
    if getSize(p.config, e.typ) > 8:
      # big set:
      linefmt(p, cpsStmts, "#nimZeroMem($1, sizeof($2));$n",
          [rdLoc(d), getTypeDesc(p.module, e.typ)])
      for it in e.items:
        if it.kind == cnkRange:
          getTemp(p, getSysType(p.module.g.graph, unknownLineInfo, tyInt), idx) # our counter
          initLocExpr(p, it[0], a)
          initLocExpr(p, it[1], b)
          lineF(p, cpsStmts, "for ($1 = $3; $1 <= $4; $1++) $n" &
              "$2[(NU)($1)>>3] |=(1U<<((NU)($1)&7U));$n", [rdLoc(idx), rdLoc(d),
              rdSetElemLoc(p.config, a, e.typ), rdSetElemLoc(p.config, b, e.typ)])
        else:
          initLocExpr(p, it, a)
          lineF(p, cpsStmts, "$1[(NU)($2)>>3] |=(1U<<((NU)($2)&7U));$n",
               [rdLoc(d), rdSetElemLoc(p.config, a, e.typ)])
    else:
      # small set
      var ts = "NU" & $(getSize(p.config, e.typ) * 8)
      lineF(p, cpsStmts, "$1 = 0;$n", [rdLoc(d)])
      for it in e.items:
        if it.kind == cnkRange:
          getTemp(p, getSysType(p.module.g.graph, unknownLineInfo, tyInt), idx) # our counter
          initLocExpr(p, it[0], a)
          initLocExpr(p, it[1], b)
          lineF(p, cpsStmts, "for ($1 = $3; $1 <= $4; $1++) $n" &
              "$2 |=(($5)(1)<<(($1)%(sizeof($5)*8)));$n", [
              rdLoc(idx), rdLoc(d), rdSetElemLoc(p.config, a, e.typ),
              rdSetElemLoc(p.config, b, e.typ), rope(ts)])
        else:
          initLocExpr(p, it, a)
          lineF(p, cpsStmts,
               "$1 |=(($3)(1)<<(($2)%(sizeof($3)*8)));$n",
               [rdLoc(d), rdSetElemLoc(p.config, a, e.typ), rope(ts)])

proc genTupleConstr(p: BProc, n: CgNode, d: var TLoc) =
  var rec: TLoc
  if not handleConstExpr(p, n, d):
    let t = n.typ
    discard getTypeDesc(p.module, t) # so that any fields are initialized
    if d.k == locNone: getTemp(p, t, d)
    for i, it in n.pairs:
      initLoc(rec, locExpr, it, d.storage)
      rec.r = "$1.Field$2" % [rdLoc(d), rope(i)]
      rec.flags.incl(lfEnforceDeref)
      expr(p, it, rec)

proc isConstClosure(n: CgNode): bool {.inline.} =
  n[0].kind == cnkProc and n[1].kind == cnkNilLit

proc genClosure(p: BProc, n: CgNode, d: var TLoc) =
  assert n.kind == cnkClosureConstr

  if isConstClosure(n):
    inc(p.module.labels)
    var tmp = "CNSTCLOSURE" & rope(p.module.labels)
    p.module.s[cfsData].addf("static NIM_CONST $1 $2 = $3;$n",
        [getTypeDesc(p.module, n.typ), tmp, genBracedInit(p, n, n.typ)])
    putIntoDest(p, d, n, tmp, OnStatic)
  else:
    var tmp, a, b: TLoc
    initLocExpr(p, n[0], a)
    initLocExpr(p, n[1], b)
    internalAssert(p.config, n[0].skipConv.kind != cnkClosureConstr, n.info):
      "closure to closure created"
    # tasyncawait.nim breaks with this optimization:
    when false:
      if d.k != locNone:
        linefmt(p, cpsStmts, "$1.ClP_0 = $2; $1.ClE_0 = $3;$n",
                [d.rdLoc, a.rdLoc, b.rdLoc])
    else:
      getTemp(p, n.typ, tmp)
      linefmt(p, cpsStmts, "$1.ClP_0 = $2; $1.ClE_0 = $3;$n",
              [tmp.rdLoc, a.rdLoc, b.rdLoc])
      putLocIntoDest(p, d, tmp)

proc genArrayConstr(p: BProc, n: CgNode, d: var TLoc) =
  var arr: TLoc
  if not handleConstExpr(p, n, d):
    if d.k == locNone: getTemp(p, n.typ, d)
    for i in 0..<n.len:
      initLoc(arr, locExpr, lodeTyp elemType(skipTypes(n.typ, abstractInst)), d.storage)
      arr.r = "$1[$2]" % [rdLoc(d), intLiteral(i)]
      expr(p, n[i], arr)

proc genStmtList(p: BProc, n: CgNode) =
  for i in 0..<n.len:
    genStmts(p, n[i])

proc downConv(p: BProc, n: CgNode, d: var TLoc) =
  ## Generates and emits the code for the ``cnkObjDownConv`` (conversion to
  ## sub-type) expression `n`.
  var a: TLoc
  initLocExpr(p, n.operand, a, d.flags * {lfWantLvalue})
  let dest = skipTypes(n.typ, abstractPtrs)
  if optObjCheck in p.options and not isObjLackingTypeField(dest):
    var nilCheck = ""
    let r = rdMType(p, a, nilCheck)
    if nilCheck != "":
      # We only need to do a conversion check if it's a ref object.
      # Since with non refs either a copy is done or a ptr to the element is passed,
      # there is nothing dynamic with them and the compiler knows the error happens at semantic analysis.
      linefmt(p, cpsStmts, "if ($1 && !#isObj($2, $3)){ #raiseObjectConversionError(); $4}$n",
              [nilCheck, r, genTypeInfo2Name(p.module, dest), raiseInstr(p)])

  if n.operand.typ.skipTypes(abstractInst).kind != tyObject:
    if lfWantLvalue in d.flags:
      putIntoDest(p, d, n,
                "(($1*) ($2))" % [getTypeDesc(p.module, n.typ),
                                  addrLoc(p.config, a)], a.storage)
      d.flags.incl lfIndirect
    else:
      putIntoDest(p, d, n,
                "(($1) ($2))" % [getTypeDesc(p.module, n.typ), rdLoc(a)], a.storage)
  else:
    putIntoDest(p, d, n, "(*($1*) ($2))" %
                        [getTypeDesc(p.module, dest), addrLoc(p.config, a)], a.storage)

proc upConv(p: BProc, n: CgNode, d: var TLoc) =
  ## Generates and emits the code for the ``cnkObjUpConv`` (conversion to
  ## super-type/base-type) expression `n`.
  var a: TLoc
  initLocExpr(p, n.operand, a, d.flags * {lfWantLvalue})

  let dest = skipTypes(n.typ, abstractPtrs)
  let src = skipTypes(n.operand.typ, abstractPtrs)
  discard getTypeDesc(p.module, src)
  let isRef = skipTypes(n.typ, abstractInst).kind in {tyRef, tyPtr}
  if isRef and d.k == locNone and lfWantLvalue in d.flags:
    # the address of the converted reference (i.e., pointer) is requested,
    # and since ``&&x->Sup`` is not valid, we take the address of the source
    # expression and then cast the pointer:
    putIntoDest(p, d, n,
                "(($1*) ($2))" % [getTypeDesc(p.module, n.typ),
                                  addrLoc(p.config, a)],
                a.storage)
    # an indirection is used:
    d.flags.incl lfIndirect
  else:
    var r = rdLoc(a) & (if isRef: "->Sup" else: ".Sup")
    for i in 2..inheritanceDiff(src, dest): r.add(".Sup")
    putIntoDest(p, d, n, if isRef: "&" & r else: r, a.storage)

proc exprComplexConst(p: BProc, n: CgNode, d: var TLoc) =
  let t = n.typ
  discard getTypeDesc(p.module, t) # so that any fields are initialized
  let id = getOrPut(p.module.dataCache, n, p.module.labels)
  let tmp = p.module.tmpBase & rope(id)

  if id == p.module.labels:
    # expression not found in the cache:
    inc(p.module.labels)
    p.module.s[cfsData].addf("static NIM_CONST $1 $2 = $3;$n",
         [getTypeDesc(p.module, t, skConst), tmp, genBracedInit(p, n, t)])

  if d.k == locNone:
    fillLoc(d, locData, n, tmp, OnStatic)
  else:
    putDataIntoDest(p, d, n, tmp)
    # This fixes bug #4551, but we really need better dataflow
    # analysis to make this 100% safe.
    if t.kind notin {tySequence, tyString}:
      d.storage = OnStatic

proc useConst*(m: BModule; sym: PSym) =
  useHeader(m, sym)
  if exfNoDecl in sym.extFlags:
    return

  let q = findPendingModule(m, sym)
  # only emit a declaration if the constant is used in a module that is not the
  # one the constant is part of
  if q != m and not containsOrIncl(m.declaredThings, sym.id):
    let headerDecl = "extern NIM_CONST $1 $2;$n" %
        [getTypeDesc(m, sym.typ, skVar), q.consts[sym].r]
    m.s[cfsData].add(headerDecl)

proc genConstDefinition*(q: BModule; sym: PSym) =
  let name = mangleName(q.g.graph, sym)
  if exfNoDecl notin sym.extFlags:
    let p = newProc(nil, q)
    q.s[cfsData].addf("N_LIB_PRIVATE NIM_CONST $1 $2 = $3;$n",
        [getTypeDesc(q, sym.typ), name,
        genBracedInit(p, translate(sym.ast), sym.typ)])

  # all constants need a loc:
  q.consts.put(sym, initLoc(locData, newSymNode(sym), name, OnStatic))

proc expr(p: BProc, n: CgNode, d: var TLoc) =
  when defined(nimCompilerStacktraceHints):
    frameMsg(p.config, n)
  p.currLineInfo = n.info

  case n.kind
  of cnkProc:
    let sym = n.sym
    if sfCompileTime in sym.flags:
      localReport(p.config, n.info, reportSym(
        rsemCannotCodegenCompiletimeProc, sym))

    useProc(p.module, sym)
    putIntoDest(p, d, n, p.module.procs[sym].name, OnStack)
  of cnkConst:
    let sym = n.sym
    if isSimpleConst(sym.typ):
      putIntoDest(p, d, n, genLiteral(p, translate(sym.ast), sym.typ), OnStatic)
    else:
      useConst(p.module, sym)
      putLocIntoDest(p, d, p.module.consts[sym])
  of cnkGlobal:
    let sym = n.sym
    assert sfGlobal in sym.flags
    genVarPrototype(p.module, n)

    if sfThread in sym.flags:
      accessThreadLocalVar(p, sym)
      if emulatedThreadVars(p.config):
        let loc {.cursor.} = p.module.globals[sym]
        putIntoDest(p, d, loc.lode, "NimTV_->" & loc.r)
      else:
        putLocIntoDest(p, d, p.module.globals[sym])
    else:
      putLocIntoDest(p, d, p.module.globals[sym])
  of cnkLocal:
    putLocIntoDest(p, d, p.locals[n.local])
  of cnkStrLit:
    putDataIntoDest(p, d, n, genLiteral(p, n))
  of cnkIntLit, cnkUIntLit, cnkFloatLit, cnkNilLit:
    putIntoDest(p, d, n, genLiteral(p, n))
  of cnkCall, cnkCheckedCall:
    genLineDir(p, n) # may be redundant, it is generated in fixupCall as well
    let m = getCalleeMagic(n[0])
    if n.typ.isNil:
      # discard the value:
      var a: TLoc
      if m != mNone:
        genMagicExpr(p, n, a, m)
      else:
        genCall(p, n, a)
    else:
      # load it into 'd':
      if m != mNone:
        genMagicExpr(p, n, d, m)
      else:
        genCall(p, n, d)
  of cnkSetConstr:
    if isDeepConstExpr(n) and n.len != 0:
      putIntoDest(p, d, n, genSetNode(p, n))
    else:
      genSetConstr(p, n, d)
  of cnkArrayConstr:
    # XXX: constructions of empty seqs should be lifted into C constants too,
    #      but that currently causes collisions and thus C compiler errors  
    if isDeepConstExpr(n) and n.len != 0:
      exprComplexConst(p, n, d)
    elif skipTypes(n.typ, abstractVarRange).kind == tySequence:
      genSeqConstr(p, n, d)
    else:
      genArrayConstr(p, n, d)
  of cnkTupleConstr:
    if n.typ != nil and n.typ.kind == tyProc and n.len == 2:
      genClosure(p, n, d)
    elif isDeepConstExpr(n) and n.len != 0:
      exprComplexConst(p, n, d)
    else:
      genTupleConstr(p, n, d)
  of cnkObjConstr: genObjConstr(p, n, d)
  of cnkCast: genCast(p, n, d)
  of cnkHiddenConv, cnkConv: genConv(p, n, d)
  of cnkToSlice:
    if n.len == 1:
      # treated as a no-op here; the conversion is handled in ``genAssignment``
      expr(p, n[0], d)
    else:
      genSlice(p, n, d)
  of cnkHiddenAddr, cnkAddr:
    if n.operand.kind in {cnkDerefView, cnkDeref}:
      # views and ``ref``s also map to pointers at the C level. We collapse
      # ``&(*x)`` to just ``x``
      expr(p, n.operand.operand, d)
    else:
      let mutate = n.kind == cnkHiddenAddr and n.typ.kind == tyVar
      genAddr(p, n, mutate, d)
  of cnkArrayAccess: genArrayLikeElem(p, n, d)
  of cnkTupleAccess:
    if n[0].typ.skipTypes(abstractInst).kind == tyProc:
      # XXX: temporary workaround. Closures should be normal tuples at this
      #      stage
      var a: TLoc
      initLocExpr(p, n[0], a)
      putIntoDest(p, d, n, "$1.ClE_0" % [rdLoc(a)])
    else:
      genTupleElem(p, n, d)
  of cnkDeref, cnkDerefView: genDeref(p, n, d)
  of cnkFieldAccess: genRecordField(p, n, d)
  of cnkBlockStmt: genBlock(p, n)
  of cnkStmtList: genStmtList(p, n)
  of cnkIfStmt: genIf(p, n)
  of cnkObjDownConv: downConv(p, n, d)
  of cnkObjUpConv: upConv(p, n, d)
  of cnkStringToCString: convStrToCStr(p, n, d)
  of cnkCStringToString: convCStrToStr(p, n, d)
  of cnkClosureConstr: genClosure(p, n, d)
  of cnkEmpty: discard
  of cnkRepeatStmt: genRepeatStmt(p, n)
  of cnkDef: genSingleVar(p, n[0], n[1])
  of cnkCaseStmt: genCase(p, n)
  of cnkReturnStmt: genReturnStmt(p, n)
  of cnkBreakStmt: genBreakStmt(p, n)
  of cnkAsgn, cnkFastAsgn:
    genAsgn(p, n)
  of cnkVoidStmt:
    genLineDir(p, n)
    var a: TLoc
    initLocExprSingleUse(p, n[0], a)
    line(p, cpsStmts, "(void)(" & a.r & ");\L")
  of cnkAsmStmt: genAsmStmt(p, n)
  of cnkEmitStmt: genEmit(p, n)
  of cnkTryStmt:
    assert p.config.exc == excGoto
    genTryGoto(p, n)
  of cnkRaiseStmt: genRaiseStmt(p, n)
  of cnkPragmaStmt: discard
  of cnkInvalid, cnkType, cnkAstLit, cnkMagic, cnkRange, cnkBinding, cnkExcept,
     cnkFinally, cnkBranch, cnkLabel, cnkStmtListExpr, cnkField:
    internalError(p.config, n.info, "expr(" & $n.kind & "); unknown node kind")

proc getDefaultValue(p: BProc; typ: PType; info: TLineInfo): Rope =
  var t = skipTypes(typ, abstractRange-{tyTypeDesc})
  case t.kind
  of tyBool: result = rope"NIM_FALSE"
  of tyEnum, tyChar, tyInt..tyInt64, tyUInt..tyUInt64: result = rope"0"
  of tyFloat..tyFloat64: result = rope"0.0"
  of tyCstring, tyVar, tyLent, tyPointer, tyPtr, tyUntyped,
     tyTyped, tyTypeDesc, tyStatic, tyRef, tyNil:
    result = rope"NIM_NIL"
  of tyString, tySequence:
    result = rope"{0, NIM_NIL}"
  of tyProc:
    if t.callConv != ccClosure:
      result = rope"NIM_NIL"
    else:
      result = rope"{NIM_NIL, NIM_NIL}"
  of tyObject:
    var count = 0
    result.add "{"
    getNullValueAuxT(p, t, t, t.n, nil, result, count, info)
    result.add "}"
  of tyTuple:
    result = rope"{"
    for i in 0..<t.len:
      if i > 0: result.add ", "
      result.add getDefaultValue(p, t[i], info)
    result.add "}"
  of tyArray:
    result = rope"{"
    for i in 0..<toInt(lengthOrd(p.config, t.sons[0])):
      if i > 0: result.add ", "
      result.add getDefaultValue(p, t.sons[1], info)
    result.add "}"
    #result = rope"{}"
  of tyOpenArray, tyVarargs:
    result = rope"{NIM_NIL, 0}"
  of tySet:
    if mapSetType(p.config, t) == ctArray: result = rope"{}"
    else: result = rope"0"
  else:
    internalError(
      p.config, info, "cannot create null element for: " & $t.kind)

proc caseObjDefaultBranch(obj: PNode; branch: Int128): int =
  for i in 1 ..< obj.len:
    for j in 0 .. obj[i].len - 2:
      if obj[i][j].kind == nkRange:
        let x = getOrdValue(obj[i][j][0])
        let y = getOrdValue(obj[i][j][1])
        if branch >= x and branch <= y:
          return i
      elif getOrdValue(obj[i][j]) == branch:
        return i
    if obj[i].len == 1:
      # else branch
      return i
  assert(false, "unreachable")

proc getNullValueAux(p: BProc; t: PType; obj: PNode, constOrNil: CgNode,
                     result: var Rope; count: var int;
                     info: TLineInfo) =
  case obj.kind
  of nkRecList:
    for it in obj.sons:
      getNullValueAux(p, t, it, constOrNil, result, count, info)
  of nkRecCase:
    getNullValueAux(p, t, obj[0], constOrNil, result, count, info)
    if count > 0: result.add ", "
    var branch = Zero
    if constOrNil != nil:
      ## find kind value, default is zero if not specified
      for it in constOrNil.items:
        assert it.kind == cnkBinding
        if it[0].sym.name.id == obj[0].sym.name.id:
          branch = getOrdValue(it[1])
          break

    let selectedBranch = caseObjDefaultBranch(obj, branch)
    result.add "{"
    var countB = 0
    let b = lastSon(obj[selectedBranch])
    # designated initilization is the only way to init non first element of unions
    # branches are allowed to have no members (b.len == 0), in this case they don't need initializer
    if b.kind == nkRecList and b.len > 0:
      result.add "._" & mangleRecFieldName(p.module, obj[0].sym) & "_" & $selectedBranch & " = {"
      getNullValueAux(p, t,  b, constOrNil, result, countB, info)
      result.add "}"
    elif b.kind == nkSym:
      result.add "." & mangleRecFieldName(p.module, b.sym) & " = "
      getNullValueAux(p, t,  b, constOrNil, result, countB, info)
    result.add "}"
  of nkSym:
    if count > 0: result.add ", "
    inc count
    let field = obj.sym
    if constOrNil != nil:
      for it in constOrNil.items:
        assert it.kind == cnkBinding
        if it[0].sym.name.id == field.name.id:
          result.add genBracedInit(p, it[1], field.typ)
          return
    # not found, produce default value:
    result.add getDefaultValue(p, field.typ, info)
  else:
    internalError(p.config, info, "cannot create null element for: " & $obj)

proc getNullValueAuxT(p: BProc; orig, t: PType; obj: PNode, constOrNil: CgNode,
                      result: var Rope; count: var int;
                      info: TLineInfo) =
  var base = t[0]
  let oldRes = result
  let oldcount = count
  if base != nil:
    result.add "{"
    base = skipTypes(base, skipPtrs)
    getNullValueAuxT(p, orig, base, base.n, constOrNil, result, count, info)
    result.add "}"
  elif not isObjLackingTypeField(t):
    result.add genTypeInfoV2(p.module, orig, obj.info)
    inc count
  getNullValueAux(p, t, obj, constOrNil, result, count, info)
  # do not emit '{}' as that is not valid C:
  if oldcount == count: result = oldRes

proc genConstObjConstr(p: BProc; n: CgNode): Rope =
  result = ""
  let t = n.typ.skipTypes(abstractInst)
  var count = 0
  if t.kind == tyObject:
    getNullValueAuxT(p, t, t, t.n, n, result, count, n.info)
  result = "{$1}$n" % [result]

proc genConstSimpleList(p: BProc, n: CgNode): Rope =
  result = rope("{")
  for i in 0..<n.len:
    let it = n[i]
    if i > 0: result.add ",\n"
    result.add genBracedInit(p, it, it.typ)
  result.add("}\n")

proc genConstTuple(p: BProc, n: CgNode; tup: PType): Rope =
  result = rope("{")
  for i in 0..<n.len:
    let it = n[i]
    if i > 0: result.add ",\n"
    result.add genBracedInit(p, it, tup[i])
  result.add("}\n")

proc genConstSeqV2(p: BProc, n: CgNode, t: PType): Rope =
  let base = t.skipTypes(abstractInst)[0]
  var data = rope"{"
  for i in 0..<n.len:
    if i > 0: data.addf(",$n", [])
    data.add genBracedInit(p, n[i], base)
  data.add("}")

  let payload = getTempName(p.module)

  appcg(p.module, cfsData,
    "static $5 struct {$n" &
    "  NI cap; $1 data[$2];$n" &
    "} $3 = {$2 | NIM_STRLIT_FLAG, $4};$n", [
    getTypeDesc(p.module, base), n.len, payload, data, "const"])
  result = "{$1, ($2*)&$3}" % [rope(n.len), getSeqPayloadType(p.module, t), payload]

proc genBracedInit(p: BProc, n: CgNode; optionalType: PType): Rope =
  case n.kind
  of cnkHiddenConv:
    result = genBracedInit(p, n.operand, n.typ)
  else:
    var ty = tyNone
    var typ: PType = nil
    if optionalType == nil:
      if n.kind == cnkStrLit:
        ty = tyString
      else:
        internalError(p.config, n.info, "node has no type")
    else:
      typ = skipTypes(optionalType, abstractInst + {tyStatic})
      ty = typ.kind
    case ty
    of tySet:
      let cs = toBitSet(p.config, n)
      result = genRawSetData(cs, int(getSize(p.config, n.typ)))
    of tySequence:
      result = genConstSeqV2(p, n, typ)
    of tyProc:
      if typ.callConv == ccClosure:
        var symNode: CgNode

        case n.kind
        of cnkNilLit, cnkProc:
          # XXX: a cnkProc shouldn't reach here, but it does. Example that
          #      triggers it:
          #      .. code-block:: nim
          #        proc p() = discard
          #        type Proc = proc()
          #        const c = p
          #
          #      `semConst` removes the `nkHiddenStdConv` around `p` prior to
          #      passing the expression to evaluation
          symNode = n
        of cnkClosureConstr:
          p.config.internalAssert(n[0].kind == cnkProc, n.info)
          p.config.internalAssert(n[1].kind == cnkNilLit, n.info)
          symNode = n[0]
        else:
          p.config.internalError(n.info, "not a closure node: " & $n.kind)

        case symNode.kind
        of cnkNilLit:
          result = ~"{NIM_NIL,NIM_NIL}"
        of cnkProc:
          var d: TLoc
          initLocExpr(p, symNode, d)
          result = "{(($1) $2),NIM_NIL}" % [getClosureType(p.module, typ, clHalfWithEnv), rdLoc(d)]
        else:
          assert false # unreachable

      else:
        var d: TLoc
        initLocExpr(p, n, d)
        result = rdLoc(d)
    of tyArray, tyVarargs:
      result = genConstSimpleList(p, n)
    of tyTuple:
      result = genConstTuple(p, n, typ)
    of tyOpenArray:
      if n.kind != cnkArrayConstr:
        internalError(
          p.config, n.info, "const openArray expression is not an array construction")

      let data = genConstSimpleList(p, n)

      let payload = getTempName(p.module)
      let ctype = getTypeDesc(p.module, typ[0])
      let arrLen = n.len
      appcg(p.module, cfsData,
        "static const $1 $3[$2] = $4;$n", [
        ctype, arrLen, payload, data])
      result = "{($1*)&$2, $3}" % [ctype, payload, rope arrLen]

    of tyObject:
      result = genConstObjConstr(p, n)
    of tyString, tyCstring:
      if n.kind != cnkNilLit and ty == tyString:
        result = genStringLiteralV2Const(p.module, n, true)
      else:
        var d: TLoc
        initLocExpr(p, n, d)
        result = rdLoc(d)
    else:
      var d: TLoc
      initLocExpr(p, n, d)
      result = rdLoc(d)
