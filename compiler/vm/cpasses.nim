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

const transformPass = LinearPass2[CTransformCtx](visit: visit)

proc applyCTransforms*(g: PassEnv, id: ProcId, ir: var IrStore3, env: IrEnv) =
  ## Applies lowerings to the IR that are specific to the C-like targets:
  ## * turn ``bcRaise`` into calls to ``raiseExceptionEx``/``reraiseException``
  ## * transform overflow checks
  ## * lower ``mWasMoved`` and ``mCStrToStr``

  var ctx = CTransformCtx(graph: g, env: addr env)
  ctx.types = computeTypes(ir, env)

  runPass(ir, ctx, transformPass)