## Implements various analysis routines for *transformed* AST.

import
  compiler/ast/[
    ast_query,
    ast_types,
    types
  ],
  compiler/utils/[
    idioms
  ]

const instTypes = {tyGenericInst, tyAlias, tySink} + tyUserTypeClasses

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

proc needsIndexCheck*(arr, idx: PNode): bool =
  ## Uses the expressions' type and shape to infer whether index checks are
  ## required for an ``arr[idx]`` access.
  case arr.typ.skipTypes(instTypes + {tyDistinct, tyVar}).kind
  of tyArray:
    # statement-list expressions are, at present, not checked at compile-
    # time, so they're not skipped here
    idx.kind notin nkIntLiterals
  of tyUncheckedArray:
    false
  of tyString, tySequence, tyOpenArray, tyVarargs:
    true
  of tyCstring:
    # XXX: depends on the targeted backend (i.e., index checks are used with
    #      the JS and VM backend, but not with the C one)
    true
  else:
    unreachable()

proc needsBoundCheck*(arr, lo, hi: PNode): bool =
  ## Uses the expression's type and shape to infer whether bound checks are
  ## required for a ``toOpenArray(arr, lo, hi)`` call.
  case arr.typ.skipTypes(instTypes + {tyDistinct, tyVar}).kind
  of tyPtr, tyUncheckedArray:
    false
  of tyArray, tyString, tySequence, tyOpenArray, tyVarargs:
    true
  of tyCstring:
    # XXX: depends on the targeted backend
    true
  else:
    unreachable()