## IR passes that are meant for the C-like targets

import
  std/[
    tables
  ],
  compiler/ast/[
    ast
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

type CTransformCtx = object
  graph: PassEnv
  env: ptr IrEnv

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
      case c.env.procs[ir.at(n.callee).procId].magic
      of mWasMoved:
        cr.replace()
        cr.insertReset(c.graph, c.env[], c.types[arg(0)], arg(0))
      of mCStrToStr:
        cr.replace()
        cr.insertCompProcCall(c.graph, "cstrToNimstr", arg(0))
      else:
        discard

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
        # TODO: a cast is needed here, but we don't know the destination type yet...
        # XXX: this problem has come up multiple times now and it's clear that
        #      a different approach to type patching/transforming is needed.
        #      An approach that looks promising is to not mutate the types in
        #      place but create new types and then store original->new
        #      mappings. Type lowering/patching would then happen *before* the
        #      IR pass so that the latter is able to lookup the new types.
        #      After all IR passes still requiring access to the original type
        #      are done, the new types are "committed". For this to work a
        #      general type indirection facility is needed, but this is
        #      planned anyways.
        let prc = cr.insertPathObj(cl, ClosureProcField)

        # remove the env arg
        args.del(args.high)

        asgnResult cr.insertCallExpr(prc, args)
        cr.insertGoto(endP)

      cr.insertJoin(endP)

      if tmpAcc != InvalidIndex:
        discard cr.insertLocalRef(tmp)

  else:
    discard


const transformPass = LinearPass2[CTransformCtx](visit: visit)
const lowerClosuresPass = LinearPass2[CTransformCtx](visit: lowerClosuresVisit)

proc applyCTransforms*(g: PassEnv, id: ProcId, ir: var IrStore3, env: IrEnv) =
  ## Applies lowerings to the IR that are specific to the C-like targets:
  ## * turn ``bcRaise`` into calls to ``raiseExceptionEx``/``reraiseException``
  ## * transform overflow checks
  ## * lower ``mWasMoved`` and ``mCStrToStr``

  var ctx = CTransformCtx(graph: g, env: addr env)
  ctx.types = computeTypes(ir, env)

  runPass(ir, ctx, transformPass)
  ctx.types = computeTypes(ir, env)

  runPass(ir, ctx, lowerClosuresPass)

proc applyCTypeTransforms*(g: PassEnv, env: var TypeEnv, senv: var SymbolEnv) =
  # lower closures to a ``tuple[prc: proc, env: pointer]`` pair
  let pointerType = g.sysTypes[tyPointer]
  for id, typ in env.mtypes:
    if typ.kind == tnkClosure:
      let prcTyp = env.requestProcType(id, ccNimCall, params=[pointerType])

      typ = env.genRecordType(base = NoneType):
        [(NoneSymbol, prcTyp),
         (NoneSymbol, pointerType)]