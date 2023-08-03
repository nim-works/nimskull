## Implements various analysis routines for *transformed* AST.

import
  compiler/ast/[
    ast_query,
    ast_types,
    types
  ]

proc canUseView*(n: PNode): bool =
  ## Computes whether the expression `n` evaluates to something of which a
  ## view can be created.
  var n {.cursor.} = n
  while true:
    case n.kind
    of nkAddr, nkHiddenAddr, nkBracketExpr, nkObjUpConv, nkObjDownConv,
       nkCheckedFieldExpr, nkDotExpr:
      n = n[0]
    of nkHiddenStdConv, nkHiddenSubConv, nkConv:
      if skipTypes(n.typ, abstractVarRange).kind in {tyOpenArray, tyTuple, tyObject} or
         compareTypes(n.typ, n[1].typ, dcEqIgnoreDistinct):
        # lvalue conversion
        n = n[1]
      else:
        return false

    of nkSym:
      # don't use a view if the location is part of a constant
      return n.sym.kind in {skVar, skLet, skForVar, skResult, skParam, skTemp}
    of nkHiddenDeref, nkDerefExpr:
      return true
    of nkCallKinds:
      # if the call yields a view, use an lvalue reference (view) -- otherwise,
      # do not
      return classifyBackendView(n.typ) != bvcNone
    else:
      return false