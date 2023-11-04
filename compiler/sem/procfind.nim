#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the searching for procs and iterators.
## This is needed for proper handling of forward declarations.

import
  compiler/ast/[
    ast,
    astalgo,
    types,
    trees,
  ],
  compiler/front/[
     msgs,
  ],
  compiler/sem/[
     semdata,
     lookups,
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportSym
from compiler/ast/report_enums import ReportKind

proc equalGenericParams(procA, procB: PNode): bool {.used.} =
  # currently unused, but might be used again
  if procA.len != procB.len: return false
  for i in 0..<procA.len:
    if procA[i].kind != nkSym:
      return false
    if procB[i].kind != nkSym:
      return false
    let a = procA[i].sym
    let b = procB[i].sym
    if a.name.id != b.name.id or
        not sameTypeOrNil(a.typ, b.typ, {ExactTypeDescValues}): return
    if a.ast != nil and b.ast != nil:
      if not exprStructuralEquivalent(a.ast, b.ast): return
  result = true

proc searchForProcAux(c: PContext, scope: PScope, fn: PSym): PSym =
  var it: TIdentIter
  result = initIdentIter(it, scope.symbols, fn.name)
  while result != nil:
    if result.kind == fn.kind:
      case equalParams(result.typ.n, fn.typ.n)
      of paramsEqual:
        if (sfExported notin result.flags) and (sfExported in fn.flags):
          localReport(c.config, fn.info, reportSym(
            rsemDeclarationVisibilityMismatch, result))
        return
      of paramsIncompatible:
        localReport(c.config, fn.info, reportSym(rsemAmbiguousCall, fn))

        return
      of paramsNotEqual:
        discard
    result = nextIdentIter(it, scope.symbols)

proc searchForProc*(c: PContext, scope: PScope, fn: PSym): tuple[proto: PSym, comesFromShadowScope: bool] =
  var scope = scope
  result.proto = searchForProcAux(c, scope, fn)
  while result.proto == nil and scope.isShadowScope:
    scope = scope.parent
    result.proto = searchForProcAux(c, scope, fn)
    result.comesFromShadowScope = true

when false:
  proc paramsFitBorrow(child, parent: PNode): bool =
    result = false
    if child.len == parent.len:
      for i in 1..<child.len:
        var m = child[i].sym
        var n = parent[i].sym
        assert((m.kind == skParam) and (n.kind == skParam))
        if not compareTypes(m.typ, n.typ, dcEqOrDistinctOf): return
      if not compareTypes(child[0].typ, parent[0].typ,
                          dcEqOrDistinctOf): return
      result = true

  proc searchForBorrowProc*(c: PContext, startScope: PScope, fn: PSym): PSym =
    # Searches for the fn in the symbol table. If the parameter lists are suitable
    # for borrowing the sym in the symbol table is returned, else nil.
    var it: TIdentIter
    for scope in walkScopes(startScope):
      result = initIdentIter(it, scope.symbols, fn.Name)
      while result != nil:
        # watchout! result must not be the same as fn!
        if (result.Kind == fn.kind) and (result.id != fn.id):
          if equalGenericParams(result.ast[genericParamsPos],
                                fn.ast[genericParamsPos]):
            if paramsFitBorrow(fn.typ.n, result.typ.n): return
        result = NextIdentIter(it, scope.symbols)
