type
  TRegisterKind* = enum
    rkNone
    rkNode
    rkInt
    rkFloat
    rkRegisterAddr
    rkNodeAddr

  TOpcode* = enum
    opcEof,         # end of code
    opcRet,         # return
    opcYldYoid,     # yield with no value
    opcYldVal,      # yield with a value

    opcAsgnInt,
    opcAsgnFloat,
    opcAsgnRef,
    opcAsgnComplex,
    opcCastIntToFloat32,    # int and float must be of the same byte size
    opcCastIntToFloat64,    # int and float must be of the same byte size
    opcCastFloatToInt32,    # int and float must be of the same byte size
    opcCastFloatToInt64,    # int and float must be of the same byte size
    opcCastPtrToInt,
    opcCastIntToPtr,
    opcFastAsgnComplex,
    opcNodeToReg,

    opcLdArr,  # a = b[c]
    opcLdArrAddr, # a = addr(b[c])
    opcWrArr,  # a[b] = c
    opcLdObj,  # a = b.c
    opcLdObjAddr, # a = addr(b.c)
    opcWrObj,  # a.b = c
    opcAddrReg,
    opcAddrNode,
    opcLdDeref,
    opcWrDeref,
    opcWrStrIdx,
    opcLdStrIdx, # a = b[c]
    opcLdStrIdxAddr,  # a = addr(b[c])

    opcAddInt,
    opcAddImmInt,
    opcSubInt,
    opcSubImmInt,
    opcLenSeq,
    opcLenStr,
    opcLenCstring,

    opcIncl, opcInclRange, opcExcl, opcCard, opcMulInt, opcDivInt, opcModInt,
    opcAddFloat, opcSubFloat, opcMulFloat, opcDivFloat,
    opcShrInt, opcShlInt, opcAshrInt,
    opcBitandInt, opcBitorInt, opcBitxorInt, opcAddu, opcSubu, opcMulu,
    opcDivu, opcModu, opcEqInt, opcLeInt, opcLtInt, opcEqFloat,
    opcLeFloat, opcLtFloat, opcLeu, opcLtu,
    opcEqRef, opcEqNimNode, opcSameNodeType,
    opcXor, opcNot, opcUnaryMinusInt, opcUnaryMinusFloat, opcBitnotInt,
    opcEqStr, opcLeStr, opcLtStr, opcEqSet, opcLeSet, opcLtSet,
    opcMulSet, opcPlusSet, opcMinusSet, opcConcatStr,
    opcContainsSet, opcRepr, opcSetLenStr, opcSetLenSeq,
    opcIsNil, opcOf, opcIs,
    opcSubStr, opcParseFloat, opcConv, opcCast,
    opcQuit, opcInvalidField,
    opcNarrowS, opcNarrowU,
    opcSignExtend,

    opcAddStrCh,
    opcAddStrStr,
    opcAddSeqElem,
    opcRangeChck,

    opcNAdd,
    opcNAddMultiple,
    opcNKind,
    opcNSymKind,
    opcNIntVal,
    opcNFloatVal,
    opcNSymbol,
    opcNIdent,
    opcNGetType,
    opcNStrVal,
    opcNSigHash,
    opcNGetSize,

    opcNSetIntVal,
    opcNSetFloatVal, opcNSetSymbol, opcNSetIdent, opcNSetStrVal,
    opcNNewNimNode, opcNCopyNimNode, opcNCopyNimTree, opcNDel, opcGenSym,

    opcNccValue, opcNccInc, opcNcsAdd, opcNcsIncl, opcNcsLen, opcNcsAt,
    opcNctPut, opcNctLen, opcNctGet, opcNctHasNext, opcNctNext, opcNodeId,

    opcSlurp,
    opcGorge,
    opcParseExprToAst,
    opcParseStmtToAst,
    opcQueryErrorFlag,
    opcNError,
    opcNWarning,
    opcNHint,
    opcNGetLineInfo, opcNSetLineInfo,
    opcEqIdent,
    opcStrToIdent,
    opcGetImpl,
    opcGetImplTransf

    opcEcho,
    opcIndCall, # dest = call regStart, n; where regStart = fn, arg1, ...
    opcIndCallAsgn, # dest = call regStart, n; where regStart = fn, arg1, ...

    opcRaise,
    opcNChild,
    opcNSetChild,
    opcCallSite,
    opcNewStr,

    opcTJmp,  # jump Bx if A != 0
    opcFJmp,  # jump Bx if A == 0
    opcJmp,   # jump Bx
    opcJmpBack, # jump Bx; resulting from a while loop
    opcBranch,  # branch for 'case'
    opcTry,
    opcExcept,
    opcFinally,
    opcFinallyEnd,
    opcNew,
    opcNewSeq,
    opcLdNull,    # dest = nullvalue(types[Bx])
    opcLdNullReg,
    opcLdConst,   # dest = constants[Bx]
    opcAsgnConst, # dest = copy(constants[Bx])
    opcLdGlobal,  # dest = globals[Bx]
    opcLdGlobalAddr, # dest = addr(globals[Bx])

    opcLdImmInt,  # dest = immediate value
    opcNBindSym, opcNDynBindSym,
    opcSetType,   # dest.typ = types[Bx]
    opcTypeTrait,
    opcSymOwner,
    opcSymIsInstantiationOf

const
  firstABxInstr* = opcTJmp
  largeInstrs* = { # instructions which use 2 int32s instead of 1:
    opcSubStr, opcConv, opcCast, opcNewSeq, opcOf}
  relativeJumps* = {opcTJmp, opcFJmp, opcJmp, opcJmpBack}
