## Implements a validation pass for code running at compile-time or in
## NimScript/REPL contexts.
##
## Future direction: this validation needs to be decoupled from the JIT and
## moved into the semantic analyisis layer.

import
  std/[
    intsets
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    lineinfos
  ],
  compiler/mir/[
    mirenv
  ],
  compiler/vm/[
    vmdef
  ],
  experimental/[
    results
  ]

type
  CheckError = object of CatchableError
    diag: VmGenDiag

proc fail(info: TLineInfo, diag: sink VmGenDiag;
          loc: InstantiationInfo = instLoc()) {.noreturn.} =
  diag.location = info
  diag.instLoc = loc
  raise (ref CheckError)(diag: diag)

proc check(defs: var IntSet, env: MirEnv, n: PNode) =
  template recurse(n: PNode) =
    check(defs, env, n)

  case n.kind
  of nkSym:
    let s = n.sym
    case s.kind
    of skVar, skLet, skForVar:
      if sfGlobal in s.flags and s.id notin defs and s notin env.globals:
        fail n.info, VmGenDiag(kind: vmGenDiagCannotEvaluateAtComptime, ast: n)
    of skProcKinds - {skMethod}:
      # XXX: instead of erroring when referencing forwarded procedures,
      #      it might be better to error only when actually *entering* a
      #      forwarded procedure
      if sfForward in s.flags:
        fail n.info, VmGenDiag(kind: vmGenDiagCannotEvaluateAtComptime, ast: n)
    of skMethod:
      # report an error for all method usages, including its usage as a
      # value
      fail n.info, VmGenDiag(kind: vmGenDiagCannotCallMethod, sym: s)
    else:
      discard "nothing to do"

  of nkIdentDefs, nkVarTuple:
    for it in names(n):
      # guard against ``nkDotExpr`` (local lifted into environment)
      if it.kind == nkSym and sfGlobal in it.sym.flags:
        defs.incl it.sym.id

    recurse(n[^1])
  of nkWhenStmt:
    # a 'when nimvm' statement; choose the first branch
    recurse(n[0][1])
  of nkCast, nkConv, nkHiddenStdConv, nkHiddenSubConv:
    recurse(n[1])
  of callableDefs, nkNimNodeLit, nkTypeSection, nkConstSection, nkTypeOfExpr,
     nkMixinStmt, nkBindStmt, nkImportStmt, nkImportExceptStmt, nkFromStmt,
     nkExportStmt:
    discard "ignore declarative nodes"
  else:
    for it in n.items:
      recurse(it)

proc validate*(env: MirEnv, n: PNode): Result[void, VmGenDiag] =
  ## Checks that the transformed AST `n`:
  ## * doesn't use inaccessible globals
  ## * doesn't use methods (either calling them or using them as value)
  ## * doesn't use forwarded procedures
  ## If the AST violates these, the ``VmGenDiag`` for the first error is
  ## returned.
  try:
    var defs = initIntSet()
    check(defs, env, n)
    result.initSuccess()
  except CheckError as e:
    result.initFailure(move e.diag)
