
import compiler/vm/[vmir, irliterals]
import compiler/ast/ast

func insertNilLit*(cr: var IrCursor, d: var LiteralData, typ: TypeId): IRIndex =
  assert typ != NoneType
  cr.insertLit (d.newLit(0'u), typ)

func insertTypeLit*(cr: var IrCursor, typ: TypeId): IRIndex =
  cr.insertLit (NoneLit, typ)

func insertLit*(cr: var IrCursor, d: var LiteralData, v: SomeInteger|float|string, typ = NoneType): IRIndex {.inline.} =
  cr.insertLit (d.newLit(v), typ)

func insertError*(cr: var IrCursor, d: var LiteralData, err: string): IRIndex {.discardable.} =
  cr.insertCallExpr(bcError, NoneType, cr.insertLit(d, err))

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
  case n.callKind
  of ckBuiltin, ckNormal: mNone
  of ckMagic:             n.magic