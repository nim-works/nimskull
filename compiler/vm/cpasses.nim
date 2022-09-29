## IR passes that are meant for the C-like targets

import
  std/[
    tables
  ],
  compiler/ast/[
    ast,
    idents
  ],
  compiler/vm/[
    irpasses,
    pass_helpers,
    vmir
  ]

from compiler/vm/vmdef import unreachable

func transformNrvo*(g: PassEnv, procs: var ProcedureEnv) =
  ## Performs the named-return-value-optimization (NRVO)
  discard

type CTransformEnv* = object
  ## State and artifacts for/of type transformation; not modified during
  ## procedure transformation
  rawProcs: Table[TypeId, TypeId] ## for each closure type the procedure type
    ## without the environment parameter
  remap: Table[TypeId, TypeId]

type CTransformCtx* {.requiresInit.} = object
  graph*: PassEnv
  transEnv*: ptr CTransformEnv


func wrapNot(cr: var IrCursor, g: PassEnv, val: IRIndex): IRIndex =
  cr.insertCallExpr(mNot, g.sysTypes[tyBool], val)

const IntLikeTypes = {tnkBool, tnkChar, tnkInt, tnkUInt}

func insertReset(cr: var IrCursor, g: PassEnv, env: var IrEnv, typ: TypeId, target: IRIndex) =
  ## Inserts a low-level reset. Depending on the target type, this is either
  ## an assignment or a call to ``nimZeroMem``
  # XXX: a mutable `env` is too broad. Only literal data is mutated here
  case env.types[typ].kind
  of IntLikeTypes:
    cr.insertAsgn(askShallow, target, cr.insertLit(env.data, 0))
  of tnkFloat:
    cr.insertAsgn(askShallow, target, cr.insertLit(env.data, 0.0))
  of tnkPtr, tnkRef:
    # XXX: ``tnkRef`` seems wrong to handle here? Instead, it should be
    #      handled during the GC transforms
    cr.insertAsgn(askShallow, target, cr.insertNilLit(env.data, typ))
  else:
    # TODO: handle the case where `target` is a ``var`` type
    cr.insertCompProcCall(g, "nimZeroMem", cr.insertAddr(target), cr.insertMagicCall(g, mSizeOf, tyInt, cr.insertTypeLit(typ)))

func visit(ir: IrStore3, types: TypeContext, env: var IrEnv, c: CTransformCtx, cr: var IrCursor) =
  template arg(i: Natural): IRIndex =
    ir.args(cr.position, i)

  let n = ir[cr]
  case n.kind
  of ntkCall:
    case n.callKind
    of ckBuiltin:
      case n.builtin
      of bcRaise:
        cr.replace()
        # XXX: cache the compiler procs
        if argCount(n) == 0:
          # re-raise
          cr.insertCompProcCall(c.graph, "reraiseException")
        else:
          assert argCount(n) == 2
          let typ = env.types.lookupGenericType(tnkRef, c.graph.getCompilerType("Exception"))
          let nilLit = cr.insertNilLit(env.data, typ)
          cr.insertCompProcCall(c.graph, "raiseExceptionEx",
                                arg(0), arg(1), nilLit, nilLit, cr.insertLit(env.data, 0))

      of bcExitRaise:
        cr.replace()
        cr.insertCompProcCall(c.graph, "popCurrentException")

      of bcOverflowCheck:
        let
          orig = ir.at(arg(0))
          lhs = ir.args(arg(0), 0)
          rhs = ir.args(arg(0), 1)

        let m = getMagic(ir, env, orig)
        assert m in mAddI..mPred

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

        func is64bit(t: Type): bool {.inline.} =
          t.kind == tnkInt and t.size == 0

        let name = if is64bit(env.types[types[lhs]]): prc64[m] else: prc[m]

        cr.replace()

        # perform the div-by-zero test first
        if m in {mDivI, mModI}:
          cr.genIfNot(cr.wrapNot(c.graph, cr.insertMagicCall(c.graph, mEqI, tyBool, rhs, cr.insertLit(env.data, 0)))):
            cr.insertCompProcCall(c.graph, "raiseDivByZero")

        let tmp = cr.newLocal(lkTemp, types[lhs])
        cr.genIfNot(cr.wrapNot(c.graph, cr.insertCompProcCall(c.graph, name, lhs, rhs, cr.insertAddr(cr.insertLocalRef(tmp))))):
          cr.insertCompProcCall(c.graph, "raiseOverflow")

        discard cr.insertLocalRef(tmp)

      of bcInitLoc:
        cr.replace()
        cr.insertReset(c.graph, env, types[arg(0)], arg(0))

      of bcStrToCStr:
        cr.replace()
        cr.insertCompProcCall(c.graph, "nimToCStringConv", arg(0))
      of bcCStrToStr:
        cr.replace()
        cr.insertCompProcCall(c.graph, "cstrToNimstr", arg(0))
      else:
        discard

    of ckMagic:
      let m = n.magic
      case m
      of mWasMoved:
        cr.replace()
        cr.insertReset(c.graph, env, types[arg(0)], arg(0))
      of mCharToStr..mInt64ToStr:
        # XXX: the ``mInt64ToStr`` magic could be replaced with the usage
        #      of ``mIntToStr``
        const Prc = [mCharToStr: "nimCharToStr", mBoolToStr: "nimBoolToStr",
                     mIntToStr: "nimIntToStr", mInt64ToStr: "nimInt64ToStr"]
        cr.replace()
        cr.insertCompProcCall(c.graph, Prc[m], arg(0))
      of mFloatToStr:
        let prc =
          if env.types.getSize(types[arg(0)]) == 32:
            "nimFloat32ToStr"
          else:
            "nimFloatToStr"

        cr.replace()
        cr.insertCompProcCall(c.graph, prc, arg(0))
      of mCStrToStr:
        cr.replace()
        cr.insertCompProcCall(c.graph, "cstrToNimstr", arg(0))
      of mAccessTypeField:
        # replace with accessing the field at position '-1' (the type field)
        cr.replace()

        let src =
          case env.types[types[arg(0)]].kind
          of tnkRef, tnkPtr, tnkVar, tnkLent:
            cr.insertDeref(arg(0))
          of tnkRecord:
            arg(0)
          else:
            unreachable()

        discard cr.insertPathObj(src, -1)
      of mLtStr, mLeStr:
        # --->
        #   cmpStrings(a, b) (< | <=) 0
        cr.replace()
        let
          val = cr.insertCompProcCall(c.graph, "cmpStrings", arg(0), arg(1))
          prc = (if m == mLtStr: mLtI else: mLeI)

        cr.insertMagicCall(c.graph, prc, tyInt, val, cr.insertLit(env.data, 0))

      of mLengthStr:
        # must be the ``len`` operator for a cstring. The one for normal
        # strings was already lowered earlier
        # --->
        #   var tmp: int
        #   if str != nil:
        #     tmp = nimCStrLen(str)
        #   else:
        #     tmp = 0
        #   tmp
        let str = arg(0)
        assert env.types.kind(types[str]) == tnkCString
        cr.replace()

        let
          elsePart = cr.newJoinPoint()
          exit = cr.newJoinPoint()
          tmp = cr.newLocal(lkTemp, c.graph.sysTypes[tyInt])
          tmpAcc = cr.insertLocalRef(tmp)

        cr.insertBranch(cr.insertMagicCall(c.graph, mIsNil, tyBool, str), elsePart)
        cr.insertAsgn(askInit, tmpAcc, cr.insertCompProcCall(c.graph, "nimCStrLen", str))
        cr.insertGoto(exit)

        cr.insertJoin(elsePart)
        cr.insertAsgn(askInit, tmpAcc, cr.insertLit(env.data, 0))
        cr.insertGoto(exit)

        cr.insertJoin(exit)

        discard cr.insertLocalRef(tmp)

      of mParseBiggestFloat:
        # problem: ``parseBiggestFloat`` is only a forward declaration --
        # the actual implementation being a ``.compilerproc``
        # (``nimParseBiggestFloat``). The new back-end doesn't support this
        # kind of indirection, but there already exists a solution: the
        # ``.importCompilerProc`` pragma, which is supported by ``irgen``.
        # The problem with the pragma: ``cgen`` doesn't properly support it, so
        # using it on ``parseBiggestFloat`` breaks bootstrapping.
        # XXX: fix the issue with ``cgen`` and use the ``importCompilerProc``
        #      pragma instead of working around the issue here
        cr.replace()
        cr.insertCompProcCall(c.graph, "nimParseBiggestFloat", arg(0), arg(1), arg(2))

      else:
        discard

    of ckNormal:
      discard

  of ntkCast:
    # TODO: needs to be revisited for compatibility with ``cgen``

    const PtrLike = {tnkPtr, tnkRef, tnkProc, tnkCString}

    var useBlit = false
    case env.types.kind(n.typ):
    of PtrLike, tnkInt, tnkUInt, tnkBool, tnkChar:
      let srcTyp = env.types.kind(types[n.srcLoc])
      case srcTyp
      of tnkInt, tnkUInt, tnkBool, tnkChar, PtrLike:
        useBlit = false
      else:
        # TODO: we need to work around the type of an ``addr x`` expression
        #       being wrong (see ``computeTypes``). Remove the test here once
        #       the type of 'addr' expressions is computed correctly
        useBlit = ir.at(n.srcLoc).kind != ntkAddr
    else:
      useBlit = true

    if useBlit:
      # for the C-like targets, a value-type cast is implemented as a
      # ``memcpy``. Compared to the type-punning-via-union approach, this is
      # correct even when strict-aliasing is enabled

      cr.replace()
      let
        tmp = cr.newLocal(lkTemp, n.typ)
        tmpAcc = cr.insertLocalRef(tmp)

      when true:
        # TODO: ``cgen2`` currently doesn't respect the IR's semantics (i.e.
        #       each node represents an lvalue) and thus generates faulty code
        #       if the src expression doesn't represent an lvalue expression in
        #       the C code.
        #       We work around that here by introducing a temporary - but this
        #       should be removed once ``cgen2`` works properly
        let rhs = cr.insertLocalRef(cr.newLocal(lkTemp, types[n.srcLoc]))
        cr.insertAsgn(askInit, rhs, n.srcLoc)

      # XXX: casting from smaller to larger types will access invalid memory
      # SPEC: the exact behaviour when casting between types of different size
      #       is not specified
      # XXX: what to do if one of the types' size is not known (e.g. if it's an
      #      imported type)?
      cr.insertCompProcCall(c.graph, "nimCopyMem", cr.insertAddr(tmpAcc), cr.insertAddr(rhs), cr.insertMagicCall(c.graph, mSizeOf, tyInt, cr.insertTypeLit(n.typ)))

      discard cr.insertLocalRef(tmp)

  of ntkConv:
    # we need to guard against closures since that conversion is handled
    # separately by the closure lowering pass
    if env.types.kind(n.typ) != tnkClosure and
       env.types.resolve(n.typ) == env.types.resolve(types[n.srcLoc]):
      # conversion to itself. This happens for conversion between distinct
      # types and their base or when previously different types become the
      # same after lowering

      # XXX: maybe we need a way to distinguish between *value* conversions
      #      (i.e. yielding a value) and *lvalue* conversions (i.e. yielding
      #      an lvalue with the same identity but different type as the source
      #      operand)

      # note: do not introduce a temporary here. If the conversion is an lvalue
      # conversion, the result must have the same identity as the input
      cr.redirect(n.srcLoc)

  else:
    discard

const
  ClosureProcField = 0
  ClosureEnvField = 1

func lowerClosuresVisit(ir: IrStore3, types: TypeContext, env: var IrEnv, c: CTransformCtx, cr: var IrCursor) =
  let n = ir[cr]
  case n.kind
  of ntkAsgn:
    if env.types[types[n.wrLoc]].kind == tnkClosure:
      case n.asgnKind
      of askCopy:
        # an earlier pass is responsible for lowering the environment
        # assignment part
        cr.replace()
        cr.insertAsgn(askCopy, cr.insertPathObj(n.wrLoc, ClosureProcField),
                      cr.insertPathObj(n.srcLoc, ClosureProcField))

      of askMove, askBlit:
        # TODO: it's not clear yet what these two should do for closure
        #       objects
        discard

  of ntkCall:
    case n.callKind
    of ckBuiltin:
      case n.builtin
      of bcNewClosure:
        cr.replace()
        let
          tmp = cr.newLocal(lkTemp, n.typ)
          tmpAcc = cr.insertLocalRef(tmp)

        cr.insertAsgn(askInit, cr.insertPathObj(tmpAcc, ClosureProcField), ir.argAt(cr, 0))
        cr.insertAsgn(askInit, cr.insertPathObj(tmpAcc, ClosureEnvField), ir.argAt(cr, 1))

        discard cr.insertLocalRef(tmp)
      else:
        discard

    of ckMagic:
      let m = n.magic
      case m
      of mIsNil:
        let arg0 = ir.argAt(cr, 0)
        if env.types[types[arg0]].kind == tnkClosure:
          cr.replace()
          discard cr.insertMagicCall(c.graph, mIsNil, tyBool, cr.insertPathObj(arg0, ClosureProcField))

      of mEqProc:
        let
          arg0 = ir.argAt(cr, 0)
          arg1 = ir.argAt(cr, 1)

        # only the equality operator for closures is lowered here - the one
        # for non-closure procedures is translated in ``cgen2``
        if env.types[types[arg0]].kind == tnkClosure:
          # --->
          #   var tmp: bool
          #   if a.prc == b.prc:
          #     tmp = a.env == b.env
          #   tmp
          cr.replace()
          let
            tmp = cr.newLocal(lkTemp, c.graph.sysTypes[tyBool])
            tmpAcc = cr.insertLocalRef(tmp)
            exit = cr.newJoinPoint()

          cr.insertBranch(cr.insertMagicCall(c.graph, mNot, tyBool, cr.insertMagicCall(c.graph, mEqRef, tyBool, cr.insertPathObj(arg0, ClosureProcField), cr.insertPathObj(arg1, ClosureProcField))), exit)
          cr.insertAsgn(askInit, tmpAcc, cr.insertMagicCall(c.graph, mEqRef, tyBool, cr.insertPathObj(arg0, ClosureEnvField), cr.insertPathObj(arg1, ClosureEnvField)))

          cr.insertJoin(exit)
          discard cr.insertLocalRef(tmp)

      of mAccessEnv:
        cr.replace()
        discard cr.insertPathObj(ir.argAt(cr, 0), ClosureEnvField)

      else:
        discard

    # rewrite calls using closures as the callee
    elif ir.at(n.callee).kind != ntkProc and env.types[types[n.callee]].kind == tnkClosure:
      # --->
      #   if cl.env != nil:
      #     cl.prc(args, cl.env)
      #   else:
      #     cast[NonClosurePrcTyp](cl.prc)(args)
      let cl = n.callee

      cr.replace()

      let cond = cr.insertMagicCall(c.graph, mIsNil, tyBool, cr.insertPathObj(cl, ClosureEnvField))
      let
        elseP = cr.newJoinPoint()
        endP = cr.newJoinPoint()

      let (tmp, tmpAcc) =
        if env.types[types[cr.position]].kind == tnkVoid:
          (0, InvalidIndex)
        else:
          let l = cr.newLocal(lkTemp, types[cr.position])
          (l, cr.insertLocalRef(l))

      # XXX: meh, unnecessary allocation. The ``IrCursor`` API needs to
      #      expose low-level call generation.
      var args = newSeq[IRIndex](n.argCount)
      block:
        var i = 0
        for arg in ir.args(cr.position):
          args[i] = arg
          inc i

      template asgnResult(val: IRIndex) =
        let v = val
        if tmpAcc != InvalidIndex:
          cr.insertAsgn(askInit, tmpAcc, v)

      cr.insertBranch(cond, elseP)
      block:
        # "env is present"-branch
        args.add cr.insertPathObj(cl, ClosureEnvField)

        asgnResult cr.insertCallExpr(cr.insertPathObj(cl, ClosureProcField), args)
        cr.insertGoto(endP)

      cr.insertJoin(elseP)
      block:
        # the closure procedure needs to be cast to the non-closure
        # counterpart first
        let prc = cr.insertCast(c.transEnv.rawProcs[types[n.callee]],
                                cr.insertPathObj(cl, ClosureProcField))

        # remove the env arg
        args.del(args.high)

        asgnResult cr.insertCallExpr(prc, args)
        cr.insertGoto(endP)

      cr.insertJoin(endP)

      if tmpAcc != InvalidIndex:
        discard cr.insertLocalRef(tmp)

  of ntkConv:
    if env.types[n.typ].kind == tnkClosure:
      # ``sigmatch`` introduces a ``nkHiddenSubConv`` when assigning a
      # procedural value of non-closure calling convention to a closure. It
      # subsequently gets translated into a 'conv' by ``irgen`` and
      # then reaches here.
      # Since all proc types using the ``ccClosure`` calling-convention are
      # translated into ``tnkClosure`` (which renders the conversion wrong) we
      # have to rewrite it to use the correct type here
      let prcTyp = env.types[env.types.nthField(c.transEnv.remap[n.typ], ClosureProcField)].typ

      cr.replace()
      discard cr.insertCast(prcTyp, n.srcLoc)

  else:
    discard

func lowerEchoVisit(c: var UntypedPassCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  ## Doesn't need the IR to be typed
  case n.kind
  of ntkCall:
    case getMagic(ir, c.env[], n)
    of mEcho:
      cr.replace()

      # construct an array from the arguments
      let
        arr = cr.newLocal(lkTemp, c.env.types.lookupArrayType(n.argCount.uint, c.graph.sysTypes[tyString]))
        arrAcc = cr.insertLocalRef(arr)

      for i in 0..<n.argCount:
        cr.insertAsgn(askInit, cr.insertPathArr(arrAcc, cr.insertLit(c.env.data, i)), ir.argAt(cr, i))

      let prc = c.graph.getCompilerProc("echoBinSafe")
      cr.insertCallStmt(prc, cr.insertConv(c.env.procs.param(prc, 0).typ, arrAcc))

    else:
      discard

  else:
    discard

func lowerAccessEnv(ir: var IrStore3, envTyp: TypeId, envSym: DeclId, param: Natural) =
  assert envTyp != NoneType

  var cr: IrCursor
  cr.setup(ir)

  # setup the correctly typed environment local
  cr.setPos(0)
  let envParam = cr.newLocal(lkLet, envTyp, envSym)
  cr.insertAsgn(askInit, cr.insertLocalRef(envParam), cr.insertCast(envTyp, cr.insertParam(param)))

  # replace all usages of ``bcAccessEnv``
  var i = 0
  for n in ir.nodes:
    cr.setPos(i)

    case n.kind
    of ntkCall:
      if n.isBuiltIn and n.builtin == bcAccessEnv:
        cr.replace()
        discard cr.insertLocalRef(envParam)

    else:
      discard

    inc i

  ir.update(cr)

func genSliceListMatch(val: IRIndex; eq, lt: TMagic, data: LiteralData, ofBranch: LiteralId; typ, boolTy: TypeId, exit: JoinPoint, cr: var IrCursor) =
  ## Generates the statements for matching `val` against the slice-list
  ## represented by `ofBranch`.
  ## `exit` is the join-point to branch to in case of a match.
  ## `eq` and `lt` are the magics to use for comparisons.
  for (a, b) in sliceListIt(data, ofBranch):
    if a != b:
      let next = cr.newJoinPoint()
      cr.insertBranch(cr.insertCallExpr(lt, boolTy, val, cr.insertLit((a, typ))), next)
      cr.insertBranch(cr.insertCallExpr(lt, boolTy, cr.insertLit((b, typ)), val), next)
      cr.insertGoto(exit) # a match was found
      cr.insertJoin(next)
    else:
      # if the element matches, we're finished
      cr.insertBranch(cr.insertCallExpr(eq, boolTy, val, cr.insertLit((a, typ))), exit)

func genMatch*(val: IRIndex, typ: TypeId, ofBranch: LiteralId, data: var LiteralData, env: TypeEnv, g: PassEnv, cr: var IrCursor): IRIndex

func lowerMatch(ir: IrStore3, types: TypeContext, env: var IrEnv, pe: PassEnv, cr: var IrCursor) =
  ## Lowers ``bcMatch`` into compare + 'branch' instructions
  # XXX: the pass is also relevant for targets languages not part of the
  #      C-family - it should be located somewhere else
  let n = ir[cr]
  case n.kind
  of ntkCall:
    if n.isBuiltIn and n.builtin == bcMatch:
      let
        val = ir.argAt(cr, 0)
      # XXX: the ``PNode`` of the original ``nkOfBranch`` is used as the
      #      literal here for now, but once literals get their own IR, this
      #      will become a slice-list.
      let ofBranch = ir.getLit(ir.at(ir.argAt(cr, 1))).val

      cr.replace()
      discard genMatch(val, types[val], ofBranch, env.data, env.types, pe, cr)

  else:
    discard

func genMatch*(val: IRIndex, typ: TypeId, ofBranch: LiteralId, data: var LiteralData, env: TypeEnv, g: PassEnv, cr: var IrCursor): IRIndex =
      let
        boolTy = g.sysTypes[tyBool]
        tk = env[typ].kind

      case ofBranch.kind:
      of lkNumber, lkString:
        # a single comparison
        let lit = cr.insertLit((ofBranch, typ))
        let prc =
          case tk
          of tnkInt, tnkUInt: mEqI
          of tnkChar: mEqCh
          of tnkBool: mEqB
          of tnkFloat: mEqF64
          of tnkString: mEqStr
          of tnkCString: mEqCString
          else:
            unreachable(tk)

        result = cr.insertCallExpr(prc, boolTy, val, lit)

      of lkComplex, lkPacked:
        # match the slice-list against the value -->
        #   var tmp = true
        #   if condA or condB or ...:
        #     tmp = false
        #   tmp
        #
        # for single items the condition is calculated via:
        #   val != elem
        #
        # for ranges, it's the following:
        #   val < range.a or range.b < val

        let
          tmp = cr.newLocal(lkTemp, boolTy)
          exit = cr.newJoinPoint()

        # TODO: add an ``insertLit`` overload that accepts a boolean
        cr.insertAsgn(askInit, cr.insertLocalRef(tmp), cr.insertLit(data, 1)) # true

        case tk
        of tnkInt, tnkUInt, tnkChar, tnkBool, tnkFloat:
          let (eq, lt) =
            case tk
            of tnkInt:   (mEqI,  mLtI)
            of tnkUInt:  (mEqI,  mLtU)
            of tnkChar:  (mEqCh, mLtCh)
            of tnkBool:  (mEqB,  mEqB)
            of tnkFloat: (mEqI,  mLtF64)
            else:
              unreachable(tk)

          genSliceListMatch(val, eq, lt, data, ofBranch, typ, boolTy, exit, cr)

        of tnkString:
          # TODO: implement the hash-table optimization present used by
          #       ``ccgstmts``
          genSliceListMatch(val, mEqStr, mLtStr, data, ofBranch, typ, boolTy, exit, cr)

        else:
          unreachable(tk)

        cr.insertAsgn(askCopy, cr.insertLocalRef(tmp), cr.insertLit(data, 0)) # false
        cr.insertJoin(exit)

        result = cr.insertLocalRef(tmp)

const ctransformPass* = TypedPass[CTransformCtx](visit: visit)
const lowerClosuresPass* = TypedPass[CTransformCtx](visit: lowerClosuresVisit)
const lowerMatchPass* = TypedPass[PassEnv](visit: lowerMatch)
const lowerEchoPass* = LinearPass2[UntypedPassCtx](visit: lowerEchoVisit)

func transformClosureProc*(g: PassEnv, paramName: PIdent, localName: DeclId,
                           id: ProcId, procs: var ProcedureEnv,
                           code: var IrStore3) =
  ## If the procedure named by `id` has the calling convention ``ccClosure``,
  ## adds an additional paramter with the name `paramName` used for passing
  ## the closure's environment, and also lowers ``bcAccessEnv`` magic calls
  ## present in the body (`code`).
  ## `localName` is the name to use for the local storing the correctly typed
  ## environment reference.

  # XXX: is it really necessary for the local to have a name? It only helps
  #      with reading the C code and doesn't serve any other purpose

  if procs[id].callConv == ccClosure:
    # add the env param
    procs.mget(id).params.add (paramName, g.sysTypes[tyPointer])

    # only perform the transformation if the procedure really captures an
    # environment - no ``bcAccessEnv`` is present otherwise
    let envTyp = procs[id].envType
    if envTyp != NoneType:
      lowerAccessEnv(code, envTyp, localName, procs.numParams(id) - 1)

func applyCTypeTransforms*(c: var CTransformEnv, g: PassEnv, env: var TypeEnv, senv: var SymbolEnv) =
  # lower closures to a ``tuple[prc: proc, env: pointer]`` pair
  let pointerType = g.sysTypes[tyPointer]
  for id, typ in env.items:
    if typ.kind == tnkClosure:
      let prcTyp = env.requestProcType(id, ccNimCall, params=[pointerType])

      c.remap[id] = env.requestRecordType(base = NoneType):
        [(NoneDecl, prcTyp),
         (NoneDecl, pointerType)]

      # needed for the lowering of closure invocations
      c.rawProcs[id] = env.requestProcType(id, ccNimCall)

func finish*(c: var CTransformEnv, env: var TypeEnv) =
  ## Commit the type modifications
  commit(env, c.remap)

func transformContinue*(code: IrStore3, pe: PassEnv, data: var LiteralData, changes: var Changes) =
  ## Transforms ``ntkGotoLink`` and ``ntkContinue`` into simple gotos.
  # TODO: improved the documentation. Explain what a "linked section" is, how
  #       they're detected, and how the transformation works
  var cr: IrCursor
  cr.setup(code)

  type SecInfo = object
    ## Information about a linked section
    tmp: IRIndex ## the reference of the temporary used for remembering the
                  ## section
    items: seq[JoinPoint] ## possible targets for the continue

  var
    sections: Table[JoinPoint, SecInfo]
    stack: seq[JoinPoint] ## the stack of active linked sections. An item is
      ## pushed when the entry point (i.e. ``ntkJoin``) of a linked section is
      ## encountered, and popped when an ``ntkContinue`` is encountered
      # XXX: use recursion instead? It would requires less heap allocations -
      #      but also make the code more complex

  # TODO: move `sections` and `stack` into an object of which an isntance is
  #       then passed to ``transformContinue`` in order to reuse the memory?
  #       I attempted this when building the compiler itself, and it brought a
  #       measurable (but very likely insignificant) performance improvement.
  #       ``finally`` is only very seldomly used in the compiler code, so it's
  #       likely that the improvement will be much more pronounced with
  #       code-bases that make heavy use of ``finally``

  # XXX: the ordering requirements of ``IrCursor`` (i.e. changes are recorded
  #      in ascending order) are violated here. The resulting changeset thus
  #      can't be merged via ``update``.

  for i, n in code.pairs:
    case n.kind
    of ntkGotoLink:
      let sec = addr sections.mgetOrPut(n.target, default(SecInfo))
      let tmp = block:
        # if `sec.items` is empty, the sections was just added to the table
        if sec.items.len > 0:
          sec.tmp
        else:
          sec.tmp = cr.insertLocalRef cr.newLocal(lkTemp, pe.sysTypes[tyInt])
          sec.tmp

      let join = cr.newJoinPoint()
      cr.setPos(i)
      cr.replace()
      cr.insertAsgn(askCopy, tmp, cr.insertLit(data, sec.items.len))
      cr.insertGoto(n.target)
      cr.insertJoin(join)

      # record the join point
      sec.items.add(join)

    of ntkJoin:
      if n.joinPoint in sections:
        # the start of a linked sections
        stack.add(n.joinPoint)

    of ntkContinue:
      let
        item = stack.pop()
        sec = unsafeAddr sections[item]

      cr.setPos(i)
      cr.replace()

      # generate the dispatcher logic
      let tmp = sec.tmp
      for i, it in sec.items.pairs:
        let join = cr.newJoinPoint()
        cr.insertBranch(wrapNot(cr, pe, cr.insertMagicCall(pe, mEqI, tyBool, tmp, cr.insertLit(data, i))), join)
        cr.insertGoto(it)

        cr.insertJoin(join)

      # a ``ntkGotoLink`` must never be a jump backwards. We take advantage of
      # this knowledge to reduce memory usage by removing the section's table
      # entry once the ``ntkContinue`` (the end of the linked section) is
      # encountered
      sections.del(item)

    else:
      discard

  assert stack.len == 0
  assert sections.len == 0

  changes.merge(cr)
