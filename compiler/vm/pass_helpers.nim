
import compiler/vm/vmir
import compiler/ast/ast

func insertNilLit*(cr: var IrCursor, typ: TypeId): IRIndex =
  cr.insertLit (newNode(nkNilLit), typ)

func insertTypeLit*(cr: var IrCursor, typ: TypeId): IRIndex =
  cr.insertLit (nil, typ)

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
