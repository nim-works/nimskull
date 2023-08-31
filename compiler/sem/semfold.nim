#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## this module folds constants; used by semantic checking phase
## and evaluation phase

import
  std/[
    strutils,
    strtabs,
    math,
  ],
  compiler/ast/[
    renderer,
    types,
    nimsets,
    ast,
    trees,
    lineinfos,
    errorhandling,
  ],
  compiler/modules/[
    magicsys,
    modulegraphs,
  ],
  compiler/front/[
    options,
  ],
  compiler/utils/[
    platform,
    idioms,
  ]

# xxx: legacy reports cruft
from compiler/front/msgs import internalError

from system/memory import nimCStrLen

type
  Folded = object
    ## Helper type for tracking/passing contextual state during constant
    ## folding.
    node: PNode     ## the AST of the processed expression/statement
    i: int          ## where to insert the next node
    wasCopied: bool ## whether a copy was created. If it was, the node can
                    ## be modified
    hasError: bool

const
  ExpressionNodes = nkCallKinds + nkLiterals + {
    nkSym, nkEmpty, nkNimNodeLit, nkNilLit,

    nkRange, nkBracket, nkCurly, nkObjConstr, nkTupleConstr,

    nkDotExpr, nkCheckedFieldExpr, nkBracketExpr, nkHiddenDeref, nkDerefExpr,
    nkAddr, nkHiddenAddr,

    nkCast, nkConv, nkHiddenStdConv, nkHiddenSubConv,
    nkTypeOfExpr,

    nkIfExpr, nkBlockExpr, nkStmtListExpr, nkError}
    ## apart from the call node kinds, nodes that appear exclusively in
    ## expression contexts

func fromAst(n: PNode, num: int): Folded =
  result = Folded(node: n, i: num, hasError: false)

proc prepareWrite(f: var Folded) =
  if not f.wasCopied:
    let orig = f.node
    f.node = shallowCopy(orig)
    f.wasCopied = true
    # only copy the nodes up-to `num`, the rest
    # is copied manually
    for i in 0..<f.i:
      f.node[i] = orig[i]

proc add(x: var Folded, y: sink Folded) =
  ## Appends `y` to `x` and propagates the const-ness.
  if x.node[x.i] != y.node:
    prepareWrite(x)
    x.node[x.i] = y.node

  inc x.i
  # propagate the error flag
  x.hasError = x.hasError or y.hasError

proc add(f: var Folded, n: PNode) =
  ## Appends `n` to `f`, where `n` is known to be constant.
  if f.node[f.i] != n:
    prepareWrite(f)
    f.node[f.i] = n

  inc f.i
  # propagate the error flag
  f.hasError = f.hasError or n.isError

proc errorType*(g: ModuleGraph): PType =
  ## creates a type representing an error state
  result = newType(tyError, nextTypeId(g.idgen), g.owners[^1])
  result.flags.incl tfCheckedForDestructor

proc getIntLitTypeG(g: ModuleGraph; literal: PNode; idgen: IdGenerator): PType =
  # we cache some common integer literal types for performance:
  let ti = getSysType(g, literal.info, tyInt)
  result = copyType(ti, nextTypeId(idgen), ti.owner)
  result.n = literal

proc newIntNodeT*(intVal: Int128, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  result = newIntTypeNode(intVal, n.typ)
  # See bug #6989. 'pred' et al only produce an int literal type if the
  # original type was 'int', not a distinct int etc.
  if n.typ.kind == tyInt:
    # access cache for the int lit type
    result.typ = getIntLitTypeG(g, result, idgen)
  result.info = n.info

proc newFloatNodeT*(floatVal: BiggestFloat, n: PNode; g: ModuleGraph): PNode =
  if n.typ.skipTypes(abstractInst).kind == tyFloat32:
    result = newFloatNode(nkFloat32Lit, floatVal)
  else:
    result = newFloatNode(nkFloatLit, floatVal)
  result.typ = n.typ
  result.info = n.info

proc newStrNodeT*(strVal: string, n: PNode; g: ModuleGraph): PNode =
  result = newStrNode(nkStrLit, strVal)
  result.typ = n.typ
  result.info = n.info

proc getConstExpr*(m: PSym, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode
  # evaluates the constant expression or returns nil if it is no constant
  # expression

proc checkInRange(conf: ConfigRef; n: PNode, res: Int128): bool =
  res in firstOrd(conf, n.typ)..lastOrd(conf, n.typ)

proc foldAdd(a, b: Int128, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  let res = a + b
  if checkInRange(g.config, n, res):
    result = newIntNodeT(res, n, idgen, g)

proc foldSub(a, b: Int128, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  let res = a - b
  if checkInRange(g.config, n, res):
    result = newIntNodeT(res, n, idgen, g)

proc foldUnarySub(a: Int128, n: PNode; idgen: IdGenerator, g: ModuleGraph): PNode =
  if a != firstOrd(g.config, n.typ):
    result = newIntNodeT(-a, n, idgen, g)

proc foldAbs(a: Int128, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  if a != firstOrd(g.config, n.typ):
    result = newIntNodeT(abs(a), n, idgen, g)

proc foldMul(a, b: Int128, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  let res = a * b
  if checkInRange(g.config, n, res):
    return newIntNodeT(res, n, idgen, g)

proc ordinalValToString(a: PNode; g: ModuleGraph): string =
  # because $ has the param ordinal[T], `a` is not necessarily an enum, but an
  # ordinal
  case a.kind
  of nkIntLiterals:
    let
      x = getInt(a)
      t = skipTypes(a.typ, abstractRange)
    case t.kind
    of tyChar:
      result = $chr(toInt64(x) and 0xff)
    of tyEnum:
      let n = t.n
      for i in 0..<n.len:
        case n[i].kind
        of nkSym:
          let field = n[i].sym
          if field.position == x:
            if field.ast == nil:
              return field.name.s
            else:
              return field.ast.strVal
        of nkError:
          g.config.internalError(n[i].info, "ordinalValToString")
        else:
          unreachable("ordinalValToString")
      if n.len > 0:
        unreachable("range checking `semConv` eliminates this case")
    else:
      result = $x
  of nkError:
    g.config.internalError(a.info, "ordinalValToString")
  else:
    unreachable("non-ordinals never make it here")

proc isFloatRange(t: PType): bool {.inline.} =
  result = t.kind == tyRange and t[0].kind in {tyFloat..tyFloat128}

proc isIntRange(t: PType): bool {.inline.} =
  result = t.kind == tyRange and t[0].kind in {
      tyInt..tyInt64, tyUInt8..tyUInt32}

proc pickIntRange(a, b: PType): PType =
  if isIntRange(a): result = a
  elif isIntRange(b): result = b
  else: result = a

proc isIntRangeOrLit(t: PType): bool =
  result = isIntRange(t) or isIntLit(t)

proc evalOp*(m: TMagic, n, a, b, c: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  # b and c may be nil
  result = nil
  case m
  of mOrd: result = newIntNodeT(getOrdValue(a), n, idgen, g)
  of mChr: result = newIntNodeT(getInt(a), n, idgen, g)
  of mUnaryMinusI, mUnaryMinusI64: result = foldUnarySub(getInt(a), n, idgen, g)
  of mUnaryMinusF64: result = newFloatNodeT(-getFloat(a), n, g)
  of mNot: result = newIntNodeT(One - getInt(a), n, idgen, g)
  of mCard: result = newIntNodeT(toInt128(nimsets.cardSet(g.config, a)), n, idgen, g)
  of mBitnotI:
    if n.typ.isUnsigned:
      result = newIntNodeT(bitnot(getInt(a)).maskBytes(int(n.typ.size)), n, idgen, g)
    else:
      result = newIntNodeT(bitnot(getInt(a)), n, idgen, g)
  of mLengthArray: result = newIntNodeT(lengthOrd(g.config, a.typ), n, idgen, g)
  of mLengthSeq, mLengthOpenArray, mLengthStr:
    if a.kind == nkNilLit:
      result = newIntNodeT(Zero, n, idgen, g)
    elif a.kind in {nkStrLit..nkTripleStrLit}:
      if a.typ.kind == tyString:
        result = newIntNodeT(toInt128(a.strVal.len), n, idgen, g)
      elif a.typ.kind == tyCstring:
        result = newIntNodeT(toInt128(nimCStrLen(a.strVal.cstring)), n, idgen, g)
    else:
      result = newIntNodeT(toInt128(a.len), n, idgen, g)
  of mUnaryPlusI, mUnaryPlusF64: result = a # throw `+` away
  # XXX: Hides overflow/underflow
  of mAbsI: result = foldAbs(getInt(a), n, idgen, g)
  of mSucc: result = foldAdd(getOrdValue(a), getInt(b), n, idgen, g)
  of mPred: result = foldSub(getOrdValue(a), getInt(b), n, idgen, g)
  of mAddI: result = foldAdd(getInt(a), getInt(b), n, idgen, g)
  of mSubI: result = foldSub(getInt(a), getInt(b), n, idgen, g)
  of mMulI: result = foldMul(getInt(a), getInt(b), n, idgen, g)
  of mMinI:
    let argA = getInt(a)
    let argB = getInt(b)
    result = newIntNodeT(if argA < argB: argA else: argB, n, idgen, g)
  of mMaxI:
    let argA = getInt(a)
    let argB = getInt(b)
    result = newIntNodeT(if argA > argB: argA else: argB, n, idgen, g)
  of mShlI:
    case skipTypes(n.typ, abstractRange).kind
    of tyInt8: result = newIntNodeT(toInt128(toInt8(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyInt16: result = newIntNodeT(toInt128(toInt16(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyInt32: result = newIntNodeT(toInt128(toInt32(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyInt64: result = newIntNodeT(toInt128(toInt64(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyInt:
      if g.config.target.intSize == 4:
        result = newIntNodeT(toInt128(toInt32(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
      else:
        result = newIntNodeT(toInt128(toInt64(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyUInt8: result = newIntNodeT(toInt128(toUInt8(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyUInt16: result = newIntNodeT(toInt128(toUInt16(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyUInt32: result = newIntNodeT(toInt128(toUInt32(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyUInt64: result = newIntNodeT(toInt128(toUInt64(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    of tyUInt:
      if g.config.target.intSize == 4:
        result = newIntNodeT(toInt128(toUInt32(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
      else:
        result = newIntNodeT(toInt128(toUInt64(getInt(a)) shl toInt64(getInt(b))), n, idgen, g)
    else:
      unreachable("constant folding for shl")
  of mShrI:
    var a = cast[uint64](getInt(a))
    let b = cast[uint64](getInt(b))
    # To support the ``-d:nimOldShiftRight`` flag, we need to mask the
    # signed integers to cut off the extended sign bit in the internal
    # representation.
    if 0'u64 < b: # do not cut off the sign extension, when there is
              # no bit shifting happening.
      case skipTypes(n.typ, abstractRange).kind
      of tyInt8: a = a and 0xff'u64
      of tyInt16: a = a and 0xffff'u64
      of tyInt32: a = a and 0xffffffff'u64
      of tyInt:
        if g.config.target.intSize == 4:
          a = a and 0xffffffff'u64
      else:
        # unsigned and 64 bit integers don't need masking
        discard
    let c = cast[BiggestInt](a shr b)
    result = newIntNodeT(toInt128(c), n, idgen, g)
  of mAshrI:
    case skipTypes(n.typ, abstractRange).kind
    of tyInt8: result =  newIntNodeT(toInt128(ashr(toInt8(getInt(a)), toInt8(getInt(b)))), n, idgen, g)
    of tyInt16: result = newIntNodeT(toInt128(ashr(toInt16(getInt(a)), toInt16(getInt(b)))), n, idgen, g)
    of tyInt32: result = newIntNodeT(toInt128(ashr(toInt32(getInt(a)), toInt32(getInt(b)))), n, idgen, g)
    of tyInt64, tyInt:
      result = newIntNodeT(toInt128(ashr(toInt64(getInt(a)), toInt64(getInt(b)))), n, idgen, g)
    else:
      unreachable("constant folding for ashr")
  of mDivI:
    let argA = getInt(a)
    let argB = getInt(b)
    if argB != Zero and (argA != firstOrd(g.config, n.typ) or argB != NegOne):
      result = newIntNodeT(argA div argB, n, idgen, g)
  of mModI:
    let argA = getInt(a)
    let argB = getInt(b)
    if argB != Zero and (argA != firstOrd(g.config, n.typ) or argB != NegOne):
      result = newIntNodeT(argA mod argB, n, idgen, g)
  of mAddF64: result = newFloatNodeT(getFloat(a) + getFloat(b), n, g)
  of mSubF64: result = newFloatNodeT(getFloat(a) - getFloat(b), n, g)
  of mMulF64: result = newFloatNodeT(getFloat(a) * getFloat(b), n, g)
  of mDivF64:
    result = newFloatNodeT(getFloat(a) / getFloat(b), n, g)
  of mIsNil: result = newIntNodeT(toInt128(ord(a.kind == nkNilLit)), n, idgen, g)
  of mLtI, mLtB, mLtEnum, mLtCh:
    result = newIntNodeT(toInt128(ord(getOrdValue(a) < getOrdValue(b))), n, idgen, g)
  of mLeI, mLeB, mLeEnum, mLeCh:
    result = newIntNodeT(toInt128(ord(getOrdValue(a) <= getOrdValue(b))), n, idgen, g)
  of mEqI, mEqB, mEqEnum, mEqCh:
    result = newIntNodeT(toInt128(ord(getOrdValue(a) == getOrdValue(b))), n, idgen, g)
  of mLtF64: result = newIntNodeT(toInt128(ord(getFloat(a) < getFloat(b))), n, idgen, g)
  of mLeF64: result = newIntNodeT(toInt128(ord(getFloat(a) <= getFloat(b))), n, idgen, g)
  of mEqF64: result = newIntNodeT(toInt128(ord(getFloat(a) == getFloat(b))), n, idgen, g)
  of mLtStr: result = newIntNodeT(toInt128(ord(getStr(a) < getStr(b))), n, idgen, g)
  of mLeStr: result = newIntNodeT(toInt128(ord(getStr(a) <= getStr(b))), n, idgen, g)
  of mEqStr: result = newIntNodeT(toInt128(ord(getStr(a) == getStr(b))), n, idgen, g)
  of mLtU:
    result = newIntNodeT(toInt128(ord(`<%`(toInt64(getOrdValue(a)), toInt64(getOrdValue(b))))), n, idgen, g)
  of mLeU:
    result = newIntNodeT(toInt128(ord(`<=%`(toInt64(getOrdValue(a)), toInt64(getOrdValue(b))))), n, idgen, g)
  of mBitandI, mAnd: result = newIntNodeT(bitand(a.getInt, b.getInt), n, idgen, g)
  of mBitorI, mOr: result = newIntNodeT(bitor(getInt(a), getInt(b)), n, idgen, g)
  of mBitxorI, mXor: result = newIntNodeT(bitxor(getInt(a), getInt(b)), n, idgen, g)
  of mAddU:
    let val = maskBytes(getInt(a) + getInt(b), int(n.typ.size))
    result = newIntNodeT(val, n, idgen, g)
  of mSubU:
    let val = maskBytes(getInt(a) - getInt(b), int(n.typ.size))
    result = newIntNodeT(val, n, idgen, g)
    # echo "subU: ", val, " n: ", n, " result: ", val
  of mMulU:
    let val = maskBytes(getInt(a) * getInt(b), int(n.typ.size))
    result = newIntNodeT(val, n, idgen, g)
  of mModU:
    let argA = maskBytes(getInt(a), int(a.typ.size))
    let argB = maskBytes(getInt(b), int(a.typ.size))
    if argB != Zero:
      result = newIntNodeT(argA mod argB, n, idgen, g)
  of mDivU:
    let argA = maskBytes(getInt(a), int(a.typ.size))
    let argB = maskBytes(getInt(b), int(a.typ.size))
    if argB != Zero:
      result = newIntNodeT(argA div argB, n, idgen, g)
  of mLeSet: result = newIntNodeT(toInt128(ord(containsSets(g.config, b, a))), n, idgen, g)
  of mEqSet: result = newIntNodeT(toInt128(ord(equalSets(g.config, a, b))), n, idgen, g)
  of mLtSet:
    result = newIntNodeT(toInt128(ord(
      containsSets(g.config, b, a) and not equalSets(g.config, a, b))), n, idgen, g)
  of mMulSet:
    result = nimsets.intersectSets(g.config, a, b)
    result.info = n.info
  of mPlusSet:
    result = nimsets.unionSets(g.config, a, b)
    result.info = n.info
  of mMinusSet:
    result = nimsets.diffSets(g.config, a, b)
    result.info = n.info
  of mConStrStr: result = newStrNodeT(getStrOrChar(a) & getStrOrChar(b), n, g)
  of mInSet: result = newIntNodeT(toInt128(ord(inSet(a, b))), n, idgen, g)
  of mRepr:
    # BUGFIX: we cannot eval mRepr here for reasons that I forgot.
    discard
  of mIntToStr, mInt64ToStr: result = newStrNodeT($(getOrdValue(a)), n, g)
  of mBoolToStr:
    if getOrdValue(a) == 0: result = newStrNodeT("false", n, g)
    else: result = newStrNodeT("true", n, g)
  of mFloatToStr: result = newStrNodeT($getFloat(a), n, g)
  of mCStrToStr, mCharToStr:
    result = newStrNodeT(getStrOrChar(a), n, g)
  of mStrToStr: result = newStrNodeT(getStrOrChar(a), n, g)
  of mEnumToStr: result = newStrNodeT(ordinalValToString(a, g), n, g)
  of mArrToSeq:
    result = copyTree(a)
    result.typ = n.typ
  of mEqProc:
    result = newIntNodeT(toInt128(ord(
        exprStructuralEquivalent(a, b, strictSymEquality=true))), n, idgen, g)
  else: discard

proc getConstIfExpr(c: PSym, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  result = nil
  for i in 0..<n.len:
    var it = n[i]
    if it.len == 2:
      var e = getConstExpr(c, it[0], idgen, g)
      if e == nil: return nil
      if getOrdValue(e) != 0:
        if result == nil:
          result = getConstExpr(c, it[1], idgen, g)
          if result == nil: return
    elif it.len == 1:
      if result == nil: result = getConstExpr(c, it[0], idgen, g)
    else:
      unreachable("getConstIfExpr()")

proc leValueConv*(a, b: PNode): bool =
  result = false
  case a.kind
  of nkCharLit..nkUInt64Lit:
    case b.kind
    of nkCharLit..nkUInt64Lit: result = a.getInt <= b.getInt
    of nkFloatLit..nkFloat128Lit: result = a.intVal <= round(b.floatVal).int
    else: result = false #internalError(a.info, "leValueConv")
  of nkFloatLit..nkFloat128Lit:
    case b.kind
    of nkFloatLit..nkFloat128Lit: result = a.floatVal <= b.floatVal
    of nkCharLit..nkUInt64Lit: result = a.floatVal <= toFloat64(b.getInt)
    else: result = false # internalError(a.info, "leValueConv")
  else: result = false # internalError(a.info, "leValueConv")

proc magicCall(m: PSym, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  # TODO: make `magicCall1`, `magicCall2` etc for different arg variants and
  #       drop all these silly conditionals
  if n.len <= 1: return
  let
    s = n[0].sym
    a = getConstExpr(m, n[1], idgen, g)
    b = if n.len > 2: getConstExpr(m, n[2], idgen, g) else: nil
    c = if n.len > 3: getConstExpr(m, n[3], idgen, g) else: nil

  template makeError(toCopy, a: PNode; b, c: PNode = nil) =
    result = shallowCopy(toCopy)
    result.flags = toCopy.flags # make an "exact" copy
    result[0] = toCopy[0]
    result[1] = if a != nil: a else: toCopy[1]
    if toCopy.len > 2:
      result[2] = if b != nil: b else: toCopy[2]
    if toCopy.len > 3:
      result[3] = if c != nil: c else: toCopy[3]
    result = g.config.wrapError(result)

  # Next we have to check for errors prior to nil because `semfold` is used to
  # do semantic analysis (including error detection).
  case n.len
  of 0, 1: unreachable("early return above eliminates this case")
  of 2:
    if a.isError:
      makeError(n, a)
      return
    elif a.isNil:
      return
  of 3:
    if a.isError or b.isError:
      makeError(n, a, b)
      return
    elif a.isNil or b.isNil:
      return
  else:
    if a.isError or b.isError or c.isError:
      makeError(n, a, b, c)
      return
    elif a.isNil or b.isNil or c.isNil:
      return

  evalOp(s.magic, n, a, b, c, idgen, g)

proc getAppType(n: PNode; g: ModuleGraph): PNode =
  if g.config.globalOptions.contains(optGenDynLib):
    result = newStrNodeT("lib", n, g)
  elif g.config.globalOptions.contains(optGenStaticLib):
    result = newStrNodeT("staticlib", n, g)
  elif g.config.globalOptions.contains(optGenGuiApp):
    result = newStrNodeT("gui", n, g)
  else:
    result = newStrNodeT("console", n, g)

proc foldConv(n, a: PNode; idgen: IdGenerator; g: ModuleGraph; check = false): PNode =
  let dstTyp = skipTypes(n.typ, abstractRange - {tyTypeDesc})
  let srcTyp = skipTypes(a.typ, abstractRange - {tyTypeDesc})

  template rangeCheck(n: PNode, value: Int128, g: ModuleGraph): bool =
    value >= firstOrd(g.config, n.typ) and value <= lastOrd(g.config, n.typ)

  template rangeError(n, a: PNode, g: ModuleGraph): PNode =
    newError(g.config, n,
             PAstDiag(kind: adSemFoldRangeCheckForLiteralConversionFailed,
                      inputLit: a))

  const
    IntegerLike = {tyInt..tyInt64, tyUInt..tyUInt64, tyChar}
    FloatLike   = {tyFloat..tyFloat64}

  case dstTyp.kind
  of tyBool:
    case srcTyp.kind
    of FloatLike:
      result = newIntNodeT(toInt128(getFloat(a) != 0.0), n, idgen, g)
    of IntegerLike:
      result = newIntNodeT(toInt128(a.getOrdValue != 0), n, idgen, g)
    of tyBool, tyEnum: # xxx shouldn't we disallow `tyEnum`?
      result = a
      result.typ = n.typ
    else: unreachable(srcTyp.kind)
  of IntegerLike:
    let val =
      case srcTyp.kind
      of IntegerLike, tyEnum, tyBool: getOrdValue(a)
      of FloatLike:                   toInt128(getFloat(a))
      else:                           unreachable(srcTyp.kind)

    if check and not rangeCheck(n, val, g):
      result = rangeError(n, a, g)
    else:
      result = newIntNodeT(val, n, idgen, g)
  of FloatLike:
    case srcTyp.kind
    of IntegerLike, tyEnum, tyBool:
      result = newFloatNodeT(toFloat64(getOrdValue(a)), n, g)
    of FloatLike:
      result = newFloatNodeT(a.floatVal, n, g)
    else:
      unreachable(srcTyp.kind)
  of tyOpenArray, tyVarargs, tyProc, tyPointer:
    discard
  else:
    # FIXME: conversion-to-enum is missing checks
    result = a
    result.typ = n.typ

proc getArrayConstr(m: PSym, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  if n.kind == nkBracket:
    result = n
  else:
    result = getConstExpr(m, n, idgen, g)
    if result == nil: result = n

proc foldArrayAccess(m: PSym, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  let x = getConstExpr(m, n[0], idgen, g)
  if x.isNil or
      x.typ.skipTypes({tyGenericInst, tyAlias, tySink}).kind == tyTypeDesc:
    return

  let y = getConstExpr(m, n[1], idgen, g)
  if y.isNil: return

  template outOfBounds(n, x, y: PNode, c: ConfigRef): PNode =
    c.newError(n, PAstDiag(kind: adSemIndexOutOfBoundsStatic,
                           staticCollection: x,
                           staticIndex: y))

  var idx = toInt64(getOrdValue(y))
  case x.kind
  of nkPar, nkTupleConstr:
    if 0 <= idx and idx < x.len:
      result = x.sons[idx]
      if result.kind == nkExprColonExpr: result = result[1]
    else:
      result = outOfBounds(n, x, y, g.config)

  of nkBracket:
    idx -= toInt64(firstOrd(g.config, x.typ))
    if 0 <= idx and idx < x.len:
      result = x[int(idx)]
    else:
      result = outOfBounds(n, x, y, g.config)

  of nkStrLit..nkTripleStrLit:
    if 0 <= idx and idx < x.strVal.len:
      result = newNodeIT(nkCharLit, x.info, n.typ)
      result.intVal = ord(x.strVal[int(idx)])
    else:
      result = outOfBounds(n, x, y, g.config)

  else:
    discard

proc foldFieldAccess(m: PSym, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  # a real field access; proc calls have already been transformed
  let x = getConstExpr(m, n[0], idgen, g)
  if x == nil or x.kind notin {nkObjConstr, nkPar, nkTupleConstr}: return

  let field = n[1].sym
  for i in ord(x.kind == nkObjConstr)..<x.len:
    var it = x[i]
    if it.kind != nkExprColonExpr:
      # lookup per index:
      result = x[field.position]
      if result.kind == nkExprColonExpr: result = result[1]
      return
    if it[0].sym.name.id == field.name.id:
      result = x[i][1]
      return

  g.config.newError(n, PAstDiag(kind: adSemStaticFieldNotFound,
                                unknownSym: field))


proc foldConStrStr(m: PSym, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  result = newNodeIT(nkStrLit, n.info, n.typ)
  result.strVal = ""
  for i in 1..<n.len:
    let a = getConstExpr(m, n[i], idgen, g)
    if a == nil: return nil
    result.strVal.add(getStrOrChar(a))

proc newSymNodeTypeDesc*(s: PSym; idgen: IdGenerator; info: TLineInfo): PNode =
  result = newSymNode(s, info)
  if s.typ.kind != tyTypeDesc:
    result.typ = newType(tyTypeDesc, idgen.nextTypeId, s.owner)
    result.typ.addSonSkipIntLit(s.typ, idgen)
  else:
    result.typ = s.typ

proc getConstExpr(m: PSym, n: PNode; idgen: IdGenerator; g: ModuleGraph): PNode =
  result = nil
  case n.kind
  of nkSym:
    var s = n.sym
    case s.kind
    of skEnumField:
      result = newIntNodeT(toInt128(s.position), n, idgen, g)
    of skConst:
      case s.magic
      of mIsMainModule: result = newIntNodeT(toInt128(ord(sfMainModule in m.flags)), n, idgen, g)
      of mCompileDate: result = newStrNodeT(getDateStr(), n, g)
      of mCompileTime: result = newStrNodeT(getClockStr(), n, g)
      of mCpuEndian: result = newIntNodeT(toInt128(ord(CPU[g.config.target.targetCPU].endian)), n, idgen, g)
      of mHostOS: result = newStrNodeT(toLowerAscii(platform.OS[g.config.target.targetOS].name), n, g)
      of mHostCPU: result = newStrNodeT(platform.CPU[g.config.target.targetCPU].name.toLowerAscii, n, g)
      of mBuildOS: result = newStrNodeT(toLowerAscii(platform.OS[g.config.target.hostOS].name), n, g)
      of mBuildCPU: result = newStrNodeT(platform.CPU[g.config.target.hostCPU].name.toLowerAscii, n, g)
      of mAppType: result = getAppType(n, g)
      of mStrDefine, mIntDefine, mBoolDefine:
        result =
          if isDefined(g.config, s.name.s):
            let define = g.config.symbols[s.name.s]
            case s.magic
            of mStrDefine:
              newStrNodeT(define, n, g)
            of mIntDefine:
              try:
                newIntNodeT(toInt128(define.parseInt), n, idgen, g)
              except ValueError:
                g.config.newError(n, PAstDiag(kind: adSemInvalidIntDefine,
                                              invalidDef: define))
            of mBoolDefine:
              try:
                newIntNodeT(toInt128(define.parseBool.int), n, idgen, g)
              except ValueError:
                g.config.newError(n, PAstDiag(kind: adSemInvalidBoolDefine,
                                              invalidDef: define))
            else:
              unreachable("guarded by outer case branch")
          else:
            copyTree(s.ast)
      else:
        result = copyTree(s.ast)
    of skProc, skFunc, skMethod:
      result = n
    of skParam:
      if s.typ != nil and s.typ.kind == tyTypeDesc:
        result = newSymNodeTypeDesc(s, idgen, n.info)
    of skType:
      # XXX gensym'ed symbols can come here and cannot be resolved. This is
      # dirty, but correct.
      if s.typ != nil:
        result = newSymNodeTypeDesc(s, idgen, n.info)
    of skGenericParam:
      if s.typ.kind == tyStatic:
        if s.typ.n != nil and tfUnresolved notin s.typ.flags:
          result = s.typ.n
          result.typ = s.typ.base
      elif s.typ.isIntLit:
        result = s.typ.n
      else:
        result = newSymNodeTypeDesc(s, idgen, n.info)
    else: discard
  of nkCharLit..nkNilLit:
    result = copyNode(n)
  of nkIfExpr:
    result = getConstIfExpr(m, n, idgen, g)
  of nkCallKinds:
    if n[0].kind != nkSym: return
    var s = n[0].sym
    if s.kind != skProc and s.kind != skFunc: return
    try:
      case s.magic
      of mNone:
        # If it has no sideEffect, `evalAtCompileTime` should evaluate it
        return
      of mLow:
        if skipTypes(n[1].typ, abstractVarRange).kind in tyFloat..tyFloat64:
          result = newFloatNodeT(firstFloat(n[1].typ), n, g)
        else:
          result = newIntNodeT(firstOrd(g.config, n[1].typ), n, idgen, g)
      of mHigh:
        if skipTypes(n[1].typ, abstractVar+{tyUserTypeClassInst}).kind notin
            {tySequence, tyString, tyCstring, tyOpenArray, tyVarargs}:
          if skipTypes(n[1].typ, abstractVarRange).kind in tyFloat..tyFloat64:
            result = newFloatNodeT(lastFloat(n[1].typ), n, g)
          else:
            result = newIntNodeT(lastOrd(g.config, skipTypes(n[1].typ, abstractVar)), n, idgen, g)
        else:
          let a = getArrayConstr(m, n[1], idgen, g)
          if a.kind == nkBracket:
            # we can optimize it away:
            result = newIntNodeT(toInt128(a.len-1), n, idgen, g)
      of mLengthOpenArray:
        let a = getArrayConstr(m, n[1], idgen, g)
        result =
          if a.kind == nkBracket:
            # we can optimize it away! This fixes the bug ``len(134)``.
            newIntNodeT(toInt128(a.len), n, idgen, g)
          else:
            magicCall(m, n, idgen, g)
      of mLengthArray:
        # It doesn't matter if the argument is const or not for mLengthArray.
        # This fixes bug #544.
        result = newIntNodeT(lengthOrd(g.config, n[1].typ), n, idgen, g)
      of mSizeOf:
        result = foldSizeOf(g.config, n, nil)
      of mAlignOf:
        result = foldAlignOf(g.config, n, nil)
      of mOffsetOf:
        result = foldOffsetOf(g.config, n, nil)
      of mAstToStr:
        result = newStrNodeT(renderTree(n[1], {renderNoComments}), n, g)
      of mConStrStr:
        result = foldConStrStr(m, n, idgen, g)
      of mIs:
        # The only kind of mIs node that comes here is one depending on some
        # generic parameter
        let
          t1 = n[1].typ.skipTypes({tyTypeDesc})
          t2 = n[2].typ

        # FIXME: this mirrors how ``vmgen`` previously folded the ``mIs``
        #        operation, and before that, how the VM implemented it --
        #        but it's incorrect. A ``mIs`` only reaches here because
        #        expression ``T is B`` where ``T`` depends on a generic
        #        parameter is assigned a ``bool`` type when it's first
        #        encountered, leading to the expression not reaching ``semIs``
        #        again (which would have otherwise folded it into a bool
        #        literal correctly)

        let match = if t2.kind == tyUserTypeClass: true
                    else: sameType(t1, t2)

        result = newIntNodeT(toInt128(ord(match)), n, idgen, g)
      else:
        result = magicCall(m, n, idgen, g)
    except OverflowDefect:
      result = g.config.newError(n, PAstDiag(kind: adSemFoldOverflow))
    except DivByZeroDefect:
      result = g.config.newError(n, PAstDiag(kind: adSemFoldDivByZero))
  of nkAddr:
    let a = getConstExpr(m, n[0], idgen, g)
    if a != nil:
      result = n
      n[0] = a
  of nkBracket, nkCurly:
    result = copyNode(n)
    for i, son in n.pairs:
      let a = getConstExpr(m, son, idgen, g)
      if a == nil: return nil
      result.add a
    incl(result.flags, nfAllConst)
  of nkRange:
    let a = getConstExpr(m, n[0], idgen, g)
    if a == nil: return
    let b = getConstExpr(m, n[1], idgen, g)
    if b == nil: return
    result = copyNode(n)
    result.add a
    result.add b
  #of nkObjConstr:
  #  result = copyTree(n)
  #  for i in 1..<n.len:
  #    var a = getConstExpr(m, n[i][1])
  #    if a == nil: return nil
  #    result[i][1] = a
  #  incl(result.flags, nfAllConst)
  of nkTupleConstr:
    # tuple constructor
    result = copyNode(n)
    if (n.len > 0) and (n[0].kind == nkExprColonExpr):
      for i, expr in n.pairs:
        let exprNew = copyNode(expr) # nkExprColonExpr
        exprNew.add expr[0]
        let a = getConstExpr(m, expr[1], idgen, g)
        if a == nil: return nil
        exprNew.add a
        result.add exprNew
    else:
      for i, expr in n.pairs:
        let a = getConstExpr(m, expr, idgen, g)
        if a == nil: return nil
        result.add a
    incl(result.flags, nfAllConst)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    let a = getConstExpr(m, n[1], idgen, g)
    if a == nil: return
    result = foldConv(n, a, idgen, g, check=true)
  of nkDerefExpr, nkHiddenDeref:
    let a = getConstExpr(m, n[0], idgen, g)
    if a != nil and a.kind == nkNilLit:
      result = nil  # xxx: `nkNilLit` should generate an error; fix the code
                    #      that generates the obviously wrong deref
  of nkCast:
    let a = getConstExpr(m, n[1], idgen, g)
    if a == nil: return
    if n.typ != nil and n.typ.kind in NilableTypes:
      # we allow compile-time 'cast' for pointer types:
      result = a
      result.typ = n.typ
  of nkBracketExpr: result = foldArrayAccess(m, n, idgen, g)
  of nkDotExpr: result = foldFieldAccess(m, n, idgen, g)
  of nkCheckedFieldExpr:
    assert n[0].kind == nkDotExpr
    result = foldFieldAccess(m, n[0], idgen, g)
  of nkStmtListExpr:
    var i = 0
    while i <= n.len - 2:
      if n[i].kind in {nkCommentStmt, nkEmpty}: i.inc
      else: break
    if i == n.len - 1:
      result = getConstExpr(m, n[i], idgen, g)
  else:
    discard # xxx: should we return nil for nkError?

proc foldInAstAux(m: PSym, n: PNode, idgen: IdGenerator, g: ModuleGraph): Folded

proc foldConstExprAux(m: PSym, n: PNode, idgen: IdGenerator, g: ModuleGraph): Folded =
  ## Folds the sub-expressions of `n` and, if possible, `n` itself. If
  ## `wantValue` is 'true', statements part of the expression are not
  ## processed and constants are always inlined, regardless of complexity.
  ##
  ## For simplicity, `n` doesn't have to be an expression.
  result = Folded(node: n, i: 0)

  # first step: fold the sub-expressions
  case n.kind
  of nkEmpty, nkLiterals, nkNimNodeLit, nkNilLit:
    # short-circuit the following ``getConstExpr`` call, so that no
    # unnecessary copy of the node is created
    return
  of nkSym:
    discard "may be folded away"
  of nkTypeOfExpr:
    # XXX: could be folded into an ``nkType`` here...
    discard
  of nkBracket, nkCurly, nkTupleConstr, nkRange, nkAddr, nkHiddenAddr,
     nkHiddenDeref, nkDerefExpr, nkBracketExpr, nkCallKinds, nkIfExpr,
     nkElifExpr, nkElseExpr, nkElse, nkElifBranch:
    for it in n.items:
      result.add foldConstExprAux(m, it, idgen, g)
  of nkCast, nkConv, nkHiddenStdConv, nkHiddenSubConv, nkBlockExpr:
    # the first slot only holds the type/label, which we don't need to traverse
    # into / fold
    result.add n[0]
    result.add foldConstExprAux(m, n[1], idgen, g)
  of nkDotExpr:
    # don't traverse into the field node
    result.add foldConstExprAux(m, n[0], idgen, g)
    result.add n[1]
  of nkCheckedFieldExpr:
    # only the first sub-node is relevant
    let x = foldConstExprAux(m, n[0], idgen, g)
    if x.node.kind notin {nkDotExpr, nkError}:
      # collapse away checked-field expressions where the wrapped
      # node is not a dot-expression anymore
      return x

    result.add x
    for i in 1..<n.len:
      result.add n[i]
  of nkObjConstr:
    result.add n[0] # skip the type slot
    for i in 1..<n.len:
      result.add foldConstExprAux(m, n[i], idgen, g)
  of nkStmtListExpr:
    for i in 0..<n.len-1:
      result.add foldInAstAux(m, n[i], idgen, g)
    # the last node is an expression
    result.add foldConstExprAux(m, n[^1], idgen, g)
  of nkExprColonExpr:
    # comes here from tuple/object constructions
    result.add n[0]
    result.add foldConstExprAux(m, n[1], idgen, g)
    return
  of nkError:
    # XXX: this should become an internal error once errors are properly
    #      wrapped everywhere
    result.hasError = true
  else:
    # this is either something statement-like (coming from ``void`` arguments
    # to procedures) or type AST (coming from ``typedesc`` arguments)
    result = foldInAstAux(m, n, idgen, g)
    return

  if result.hasError:
    # don't attempt folding if the AST contains errors
    return

  # constants can be inlined here, but only if they cannot result in a cast
  # in the back-end (e.g. ``cast[pointer](someProc)``). In addition, so as to
  # not interfere with the documentation generator, statement-list expressions
  # are not folded if they have a comment in the first position
  let exprIsPointerCast = n.kind in {nkCast, nkConv, nkHiddenStdConv} and
                          n.typ != nil and
                          n.typ.kind == tyPointer
  if not exprIsPointerCast and
     not (n.kind == nkStmtListExpr and n[0].kind == nkCommentStmt):
    var cnst = getConstExpr(m, result.node, idgen, g)
    # we inline constants if they are not complex constants:
    if cnst != nil and not dontInlineConstant(n, cnst):
      result.node = cnst
      result.hasError = cnst.kind == nkError

proc foldInAstAux(m: PSym, n: PNode, idgen: IdGenerator, g: ModuleGraph): Folded =
  ## Folds all constant expression in the statement or expression `n`.
  case n.kind
  of ExpressionNodes:
    result = foldConstExprAux(m, n, idgen, g)
  of callableDefs, nkTypeSection, nkConstSection, nkMixinStmt, nkBindStmt,
     nkPragma, nkImportStmt, nkExportStmt, nkFromStmt, nkImportExceptStmt,
     nkBreakStmt, nkContinueStmt:
    # skip declarative nodes and control-flow statements that don't contain
    # fold-able expressions
    result = Folded(node: n)
  of nkStmtList, nkIfStmt, nkCaseStmt, nkWhileStmt, nkWhenStmt, nkTryStmt,
     nkDefer, nkLetSection, nkVarSection, nkYieldStmt, nkReturnStmt,
     nkDiscardStmt, nkRaiseStmt, nkAsmStmt, nkAsgn, nkFastAsgn, nkFinally,
     nkElifBranch, nkElse, nkOfBranch:
    # something that's either a statement or not evaluatable. We still
    # want to process the sub-nodes
    result = Folded(node: n)
    for it in n.items:
      result.add foldInAstAux(m, it, idgen, g)

  of nkPragmaBlock, nkBlockStmt:
    result = fromAst(n, n.len - 1)
    result.add foldInAstAux(m, n[^1], idgen, g)
  of nkForStmt:
    # don't process the symbol slots
    result = fromAst(n, n.len - 2)
    result.add foldInAstAux(m, n[^2], idgen, g)
    result.add foldInAstAux(m, n[^1], idgen, g)
  of nkIdentDefs, nkVarTuple, nkExceptBranch:
    # don't process the symbol slots
    result = fromAst(n, n.len - 1)
    result.add foldInAstAux(m, n[^1], idgen, g)
  else:
    # XXX: type AST reaches here, but ideally this catch-all branch
    #      wouldn't be needed
    result = Folded(node: n)

proc foldInAst*(m: PSym, n: PNode, idgen: IdGenerator, g: ModuleGraph): PNode =
  ## Performs constant folding on all expressions appearing in the statement or
  ## expression `n`. If nothing changed, `n` is returned, a new AST otherwise.
  if n.kind == nkError:
    return n # already an error -> nothing to do

  let f = foldInAstAux(m, n, idgen, g)
  if f.hasError:
    result = g.config.wrapError(f.node)
  else:
    result = f.node

proc getConstExpr2*(m: PSym, n: PNode, idgen: IdGenerator, g: ModuleGraph): PNode =
  ## If `n` is an error, returns the error. Otherwise returns the folded
  ## value, or nil, if `n` isn't constant.
  if n.kind == nkError: n
  else:                 getConstExpr(m, n, idgen, g)