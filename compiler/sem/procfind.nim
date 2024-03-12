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
          # Should be rsemDefaultParamIsIncompatible

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

