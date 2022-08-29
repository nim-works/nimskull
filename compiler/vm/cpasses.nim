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

type CTransformCtx = object
  # immutable external state shared across all ``applyCTransforms``
  # invocations
  graph: PassEnv
  env: ptr IrEnv
  transEnv*: ptr CTransformEnv

  # immutable state local to the current procedure being transformed
  types: seq[TypeId]
  paramMap: seq[uint32] ## maps the current parameter indices to the new ones


func insertLit(cr: var IrCursor, i: int): IRIndex =
  cr.insertLit (newIntNode(nkIntLit, i), NoneType)

func insertLit(cr: var IrCursor, f: float): IRIndex =
  cr.insertLit (newFloatNode(nkFloatLit, f), NoneType)

func wrapNot(cr: var IrCursor, g: PassEnv, val: IRIndex): IRIndex =
  cr.insertMagicCall(g, mNot, val)

const IntLikeTypes = {tnkBool, tnkChar, tnkInt, tnkUInt}

func insertReset(cr: var IrCursor, g: PassEnv, env: IrEnv, typ: TypeId, target: IRIndex) =
  ## Inserts a low-level reset. Depending on the target type, this is either
  ## an assignment or a call to ``nimZeroMem``
  case env.types[typ].kind
  of IntLikeTypes:
    cr.insertAsgn(askShallow, target, cr.insertLit(0))
  of tnkFloat:
    cr.insertAsgn(askShallow, target, cr.insertLit(0.0))
  of tnkPtr, tnkRef:
    # XXX: ``tnkRef`` seems wrong to handle here? Instead, it should be
    #      handled during the GC transforms
    cr.insertAsgn(askShallow, target, cr.insertLit((newNode(nkNilLit), NoneType)))
  else:
    # TODO: handle the case where `target` is a ``var`` type
    cr.insertCompProcCall(g, "nimZeroMem", cr.insertAddr(target), cr.insertCallExpr(g.magics[mSizeOf], cr.insertLit((nil, typ))))

func visit(c: var CTransformCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  template arg(i: Natural): IRIndex =
    ir.args(cr.position, i)

  case n.kind
  of ntkCall:
    if n.isBuiltIn:
      case n.builtin
      of bcRaise:
        cr.replace()
        # XXX: cache the compiler procs
        if argCount(n) == 0:
          # re-raise
          cr.insertCompProcCall(c.graph, "reraiseException")
        else:
          # TODO: fix the empty arguments
          let nilLit = cr.insertLit((newNode(nkNilLit), NoneType))
          cr.insertCompProcCall(c.graph, "raiseExceptionEx",
                                arg(0), arg(1), nilLit, nilLit, cr.insertLit(0))

      of bcOverflowCheck:
        let
          orig = ir.at(arg(0))
          lhs = ir.args(arg(0), 0)
          rhs = ir.args(arg(0), 1)

        let m = c.env.procs[ir.at(orig.callee).procId].magic
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

        let name = if is64bit(c.env.types[c.types[lhs]]): prc64[m] else: prc[m]

        cr.replace()

        # perform the div-by-zero test first
        if m in {mDivI, mModI}:
          cr.genIfNot(cr.wrapNot(c.graph, cr.insertMagicCall(c.graph, mEqI, rhs, cr.insertLit(0)))):
            cr.insertCompProcCall(c.graph, "raiseDivByZero")

        let tmp = cr.newLocal(lkTemp, c.types[lhs])
        cr.genIfNot(cr.wrapNot(c.graph, cr.insertCompProcCall(c.graph, name, lhs, rhs, cr.insertAddr(cr.insertLocalRef(tmp))))):
          cr.insertCompProcCall(c.graph, "raiseOverflow")

        discard cr.insertLocalRef(tmp)

      else:
        discard

    elif ir.at(n.callee).kind == ntkProc:
      let m = c.env.procs[ir.at(n.callee).procId].magic
      case m
      of mWasMoved:
        cr.replace()
        cr.insertReset(c.graph, c.env[], c.types[arg(0)], arg(0))
      of mCharToStr..mInt64ToStr:
        # XXX: the ``mInt64ToStr`` magic could be replaced with the usage
        #      of ``mIntToStr``
        const Prc = [mCharToStr: "nimCharToStr", mBoolToStr: "nimBoolToStr",
                     mIntToStr: "nimIntToStr", mInt64ToStr: "nimInt64ToStr"]
        cr.replace()
        cr.insertCompProcCall(c.graph, Prc[m], arg(0))
      of mFloatToStr:
        let prc =
          if c.env.types.getSize(c.types[arg(0)]) == 32:
            "#imFloat32ToStr"
          else:
            "nimFloatToStr"

        cr.replace()
        cr.insertCompProcCall(c.graph, prc, arg(0))
      of mCStrToStr:
        cr.replace()
        cr.insertCompProcCall(c.graph, "cstrToNimstr", arg(0))
      else:
        discard

  of ntkCast:
    # TODO: needs to be revisited for compatibility with ``cgen``

    const PtrLike = {tnkPtr, tnkRef, tnkProc, tnkCString}

    var useBlit = false
    case c.env.types.kind(n.typ):
    of PtrLike, tnkInt, tnkUInt, tnkBool, tnkChar:
      let srcTyp = c.env.types.kind(c.types[n.srcLoc])
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
        let rhs = cr.insertLocalRef(cr.newLocal(lkTemp, c.types[n.srcLoc]))
        cr.insertAsgn(askInit, rhs, n.srcLoc)

      # XXX: casting from smaller to larger types will access invalid memory
      # SPEC: the exact behaviour when casting between types of different size
      #       is not specified
      # XXX: what to do if one of the types' size is not known (e.g. if it's an
      #      imported type)?
      cr.insertCompProcCall(c.graph, "nimCopyMem", cr.insertAddr(tmpAcc), cr.insertAddr(rhs), cr.insertCallExpr(c.graph.magics[mSizeOf], cr.insertTypeLit(n.typ)))

      discard cr.insertLocalRef(tmp)

  of ntkConv:
    # we need to guard against closures since that conversion is handled
    # separately by the closure lowering pass
    if c.env.types.kind(n.typ) != tnkClosure and
       c.env.types.resolve(n.typ) == c.env.types.resolve(c.types[n.srcLoc]):
      # conversion to itself. This happens for conversion between distinct
      # types and their base or when previously different types become the
      # same after lowering

      # XXX: the ``IrCursor`` API currently doesn't support replacing a node
      #      with an existing one, so we have to work around that here
      cr.replace()
      let tmp = cr.newLocal(lkTemp, n.typ)
      cr.insertAsgn(askInit, cr.insertLocalRef(tmp), n.srcLoc)
      discard cr.insertLocalRef(tmp)

  else:
    discard

const
  ClosureProcField = 0
  ClosureEnvField = 1

func lowerClosuresVisit(c: var CTransformCtx, n: IrNode3, ir: IrStore3, cr: var IrCursor) =
  case n.kind
  of ntkCall:
    if n.isBuiltIn:
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

    elif ir.at(n.callee).kind == ntkProc and (let m = c.env.procs[ir.at(n.callee).procId].magic; m != mNone):
      case m
      of mIsNil:
        let arg0 = ir.argAt(cr, 0)
        if c.env.types[c.types[arg0]].kind == tnkClosure:
          cr.replace()
          discard cr.insertCallExpr(c.graph.magics[mIsNil], cr.insertPathObj(arg0, ClosureProcField))

      else:
        discard

    # rewrite calls using closures as the callee
    elif ir.at(n.callee).kind != ntkProc and c.env.types[c.types[n.callee]].kind == tnkClosure:
      # --->
      #   if cl.env != nil:
      #     cl.prc(args, cl.env)
      #   else:
      #     cast[NonClosurePrcTyp](cl.prc)(args)
      let cl = n.callee

      cr.replace()

      let cond = cr.insertMagicCall(c.graph, mIsNil, cr.insertPathObj(cl, ClosureEnvField))
      let
        elseP = cr.newJoinPoint()
        endP = cr.newJoinPoint()

      let (tmp, tmpAcc) =
        if c.env.types[c.types[cr.position]].kind == tnkVoid:
          (0, InvalidIndex)
        else:
          let l = cr.newLocal(lkTemp, c.types[cr.position])
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
        let prc = cr.insertCast(c.transEnv.rawProcs[c.types[n.callee]],
                                cr.insertPathObj(cl, ClosureProcField))

        # remove the env arg
        args.del(args.high)

        asgnResult cr.insertCallExpr(prc, args)
        cr.insertGoto(endP)

      cr.insertJoin(endP)

      if tmpAcc != InvalidIndex:
        discard cr.insertLocalRef(tmp)

  of ntkConv:
    if c.env.types[n.typ].kind == tnkClosure:
      # ``sigmatch`` introduces a ``nkHiddenSubConv`` when assigning a
      # procedural value of non-closure calling convention to a closure. It
      # subsequently gets translated into a 'conv' by ``irgen`` and
      # then reaches here.
      # Since all proc types using the ``ccClosure`` calling-convention are
      # translated into ``tnkClosure`` (which renders the conversion wrong) we
      # have to rewrite it to use the correct type here
      let prcTyp = c.env.types[c.env.types.nthField(c.transEnv.remap[n.typ], ClosureProcField)].typ

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
        cr.insertAsgn(askInit, cr.insertPathArr(arrAcc, cr.insertLit(i)), ir.argAt(cr, i))

      let prc = c.graph.getCompilerProc("echoBinSafe")
      cr.insertCallStmt(prc, cr.insertConv(c.env.procs.param(prc, 0).typ, arrAcc))

    else:
      discard

  else:
    discard

func lowerAccessEnv(ir: var IrStore3, env: IrEnv, envTyp: TypeId, envSym: SymId, param: Natural) =
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

const transformPass = LinearPass2[CTransformCtx](visit: visit)
const lowerClosuresPass = LinearPass2[CTransformCtx](visit: lowerClosuresVisit)
const lowerEchoPass* = LinearPass2[UntypedPassCtx](visit: lowerEchoVisit)

proc applyCTransforms*(c: CTransformEnv, ic: IdentCache, g: PassEnv, id: ProcId, ir: var IrStore3, env: var IrEnv) =
  ## Applies lowerings to the IR that are specific to the C-like targets:
  ## * turn ``bcRaise`` into calls to ``raiseExceptionEx``/``reraiseException``
  ## * transform overflow checks
  ## * lower ``mWasMoved`` and ``mCStrToStr``

  # XXX: only the procedure env is modified. Requiring the whole `env` to be
  #      mutable is a bit meh...

  var ctx = CTransformCtx(graph: g, env: addr env, transEnv: addr c)
  ctx.types = computeTypes(ir, env)

  runPass(ir, ctx, transformPass)
  ctx.types = computeTypes(ir, env)

  runPass(ir, ctx, lowerClosuresPass)

  if env.procs[id].callConv == ccClosure:
    # add the env param
    let name = ic.getIdent("ClE")
    env.procs.mget(id).params.add (name, g.sysTypes[tyPointer])

    # only perform the transformation if the procedure really captures an
    # environment, since no ``bcAccessEnv`` is present otherwise
    let envTyp = env.procs[id].envType
    if envTyp != NoneType:
      let sym = env.syms.addSym(skLet, envTyp, ic.getIdent(":env"))
      lowerAccessEnv(ir, env, envTyp, sym, env.procs.numParams(id) - 1)

func applyCTypeTransforms*(c: var CTransformEnv, g: PassEnv, env: var TypeEnv, senv: var SymbolEnv) =
  # lower closures to a ``tuple[prc: proc, env: pointer]`` pair
  let pointerType = g.sysTypes[tyPointer]
  for id, typ in env.items:
    if typ.kind == tnkClosure:
      let prcTyp = env.requestProcType(id, ccNimCall, params=[pointerType])

      c.remap[id] = env.requestRecordType(base = NoneType):
        [(NoneSymbol, prcTyp),
         (NoneSymbol, pointerType)]

      # needed for the lowering of closure invocations
      c.rawProcs[id] = env.requestProcType(id, ccNimCall)

func finish*(c: var CTransformEnv, env: var TypeEnv) =
  ## Commit the type modifications
  commit(env, c.remap)