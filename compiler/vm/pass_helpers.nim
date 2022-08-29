
import compiler/vm/vmir
import compiler/ast/ast

func insertNilLit*(cr: var IrCursor, typ: TypeId): IRIndex =
  cr.insertLit (newNode(nkNilLit), typ)

func insertTypeLit*(cr: var IrCursor, typ: TypeId): IRIndex =
  cr.insertLit (nil, typ)

func insertLit*(cr: var IrCursor, lit: string): IRIndex =
  cr.insertLit (newStrNode(nkStrLit, lit), NoneType)

func insertLit*(cr: var IrCursor, i: int): IRIndex =
  cr.insertLit (newIntNode(nkIntLit, i), NoneType)

func insertLit*(cr: var IrCursor, i: uint): IRIndex =
  cr.insertLit (newIntNode(nkUIntLit, cast[BiggestInt](i)), NoneType)

func insertError*(cr: var IrCursor, err: string): IRIndex {.discardable.} =
  cr.insertCallExpr(bcError, NoneType, cr.insertLit err)

template genIfNot*(cr: var IrCursor, cond: IRIndex, code: untyped) =
  let condVal = cond

  let j = cr.newJoinPoint()
  cr.insertBranch(condVal, j)
  code
  cr.insertGoto(j)
  cr.insertJoin(j)

func genTempOf*(cr: var IrCursor, src: IRIndex, typ: TypeId): int =
  result = cr.newLocal(lkTemp, typ)
  cr.insertAsgn(askInit, cr.insertLocalRef(result), src) # XXX: should be both init and shallow

template argAt*(ir: IrStore3, cr: IrCursor, i: Natural): IRIndex =
  ## Temporary helper until ``IRIndex`` is used in more places
  {.line.}:
    ir.args(cr.position, i)

func access*(cr: var IrCursor, env: TypeEnv, val: IRIndex, typ: TypeId): IRIndex =
  # XXX: including lent here might lead to problems. When removing redundant
  #      ``lent`` types (e.g. transformed seqs/strings) for example
  # don't use a 'deref' for ``var openArray``
  if env[typ].kind in {tnkVar, tnkLent} and
     env[env[typ].base].kind != tnkOpenArray:
    cr.insertDeref(val)
  else:
    val

func getMagic*(ir: IrStore3, env: IrEnv, n: IrNode3): TMagic =
  assert n.kind == ntkCall
  if n.isBuiltIn:
    mNone
  else:
    let callee = ir.at(n.callee)
    if callee.kind == ntkProc:
      env.procs[callee.procId].magic
    else:
      mNone