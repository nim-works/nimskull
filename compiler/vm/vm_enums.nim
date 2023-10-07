type
  TRegisterKind* = enum
    rkNone
    rkInt
    rkFloat
    rkAddress ## Register stores an address and optional type. The address is
              ## not required to point to a valid location
    rkLocation ## Register stores an owning handle to a location. Once the
               ## register transitions to a different kind, the location is
               ## cleaned up and the memory freed
    rkHandle ## Register stores a handle to a location

    rkNimNode

  # TODO: reorder the enum fields so that they're either grouped by topic or
  #       usage (or both)
  TOpcode* = enum
    opcEof,         # end of code
    opcRet,         # return
    opcYldYoid,     # yield with no value
    opcYldVal,      # yield with a value

    opcAsgnInt,
    opcAsgnFloat,
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
    opcWrLoc,  ## ``a = b``; writes the value represented by register `b`
               ## (either directly or indirectly) to the location identified
               ## by register `a`. A full copy is performed.

    opcAddr,    ## a = addr b
    opcLdDeref,
    opcWrDeref,
    opcWrStrIdx,
    opcLdStrIdx, # a = b[c]
    opcLdStrIdxAddr,  # a = addr(b[c])

    opcInitDisc # init discriminant (a.b = c)
    opcSetDisc # set discriminant (a.b = c)

    opcWrProc # deref(a) = functions[Bx]

    # TODO: instead of the a == c hack, use the surplus 8bit of the instruction
    #       word that are currently unused as a boolean
    opcWrClosure # a = (b, c)
    # If a == c, treat the env as nil
    opcAccessEnv ## a = b.env[]; loads a handle to the cell holding the
                 ## environment

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
    opcIsNil, opcOf,
    opcParseFloat, opcConv, opcNumConv, opcObjConv, opcCast
    opcQuit, opcInvalidField,
    opcNarrowS, opcNarrowU,
    opcSignExtend,

    opcAddStrCh,
    opcAddStrStr,
    opcAddSeqElem,
    opcRangeChck,

    opcArrCopy,

    # NimNode manipulation opcodes

    opcNAdd,
    opcNAddMultiple,
    opcNKind,
    opcNSymKind,
    opcNIntVal,
    opcNFloatVal,
    opcNGetType,
    opcNStrVal,
    opcNSigHash,
    opcNGetSize,

    # Direct NimNode setters and NimNode creation

    opcNSetIntVal,
    opcNSetFloatVal, opcNSetStrVal,
    opcNNewNimNode, opcNCopyNimNode, opcNCopyNimTree, opcNDel, opcGenSym,

    # Macro cache operations

    opcNccValue, opcNccInc, opcNcsAdd, opcNcsIncl, opcNcsLen, opcNcsAt,
    opcNctPut, opcNctLen, opcNctGet, opcNctHasNext, opcNctNext, opcNodeId,

    opcSlurp,          ## deprecated to be removed
    opcParseExprToAst,
    opcParseStmtToAst,
    opcNGetLineInfo, opcNSetLineInfo,
    opcEqIdent,
    opcStrToIdent,
    opcGetImpl,
    opcGetImplTransf,

    opcDataToAst,   ## deserialize data to NimNode AST
    opcExpandToAst,

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

    opcLdCmplxConst, # dest = complexConsts[Bx]

    opcLdImmInt,  # dest = immediate value
    opcSetType,   # dest.typ = types[Bx]
    opcNSetType,  # dest.nimNode.typ = types[Bx]
    opcTypeTrait,
    opcSymOwner,
    opcSymIsInstantiationOf

  AccessViolationReason* = enum
    avrNoError      ## No violation happened
    avrOutOfBounds  ## Access to an address not owned by the VM
    avrTypeMismatch ## Dynamic type is not compatible with static type
    avrNoLocation   ## Address points to valid VM memory. but not to the start
                    ## of a location


const
  firstABxInstr* = opcTJmp
  largeInstrs* = { # instructions which use 2 int32s instead of 1:
    opcConv, opcObjConv, opcCast, opcNewSeq, opcOf}
  relativeJumps* = {opcTJmp, opcFJmp, opcJmp, opcJmpBack}
