#
#
#           The Nim Compiler
#        (c) Copyright 2020 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains 'typeAllowed' and friends which check
## for invalid types like 'openArray[var int]'.

import
  std/[
    intsets,
  ],
  compiler/ast/[
    ast,
    renderer,
    types,
    errorhandling,
  ],
  compiler/front/[
    options,
  ],
  compiler/sem/[
    semdata,
  ]

export TTypeAllowedFlag, TTypeAllowedFlags

proc typeAllowedAux(marker: var IntSet, typ: PType, kind: TSymKind;
                    c: PContext; flags: TTypeAllowedFlags = {}): PType

proc typeAllowedNode(marker: var IntSet, n: PNode, kind: TSymKind,
                     c: PContext; flags: TTypeAllowedFlags = {}): PType =
  if n != nil:
    result = typeAllowedAux(marker, n.typ, kind, c, flags)
    if result == nil:
      case n.kind
      of nkNone..nkNilLit:
        discard
      else:
        #if n.kind == nkRecCase and kind in {skProc, skFunc, skConst}:
        #  return n[0].typ
        for i in 0..<n.len:
          let it = n[i]
          result = typeAllowedNode(marker, it, kind, c, flags)
          if result != nil: break

proc typeAllowedAux(marker: var IntSet, typ: PType, kind: TSymKind,
                    c: PContext; flags: TTypeAllowedFlags = {}): PType =
  assert(kind in {skVar, skLet, skConst, skProc, skFunc, skParam, skResult})
  # if we have already checked the type, return true, because we stop the
  # evaluation if something is wrong:
  result = nil
  if typ == nil: return nil
  if containsOrIncl(marker, typ.id): return nil
  var t = skipTypes(typ, abstractInst-{tyTypeDesc, tySink})
  case t.kind
  of tyVar, tyLent:
    if kind in {skProc, skFunc, skConst} and (views notin c.features):
      result = t
    elif taIsOpenArray in flags:
      result = t
    elif t.kind == tyLent and ((kind != skResult and views notin c.features) or
                              kind == skParam): # lent can't be used as parameters.
      result = t
    else:
      var t2 = skipTypes(t[0], abstractInst-{tyTypeDesc, tySink})
      case t2.kind
      of tyVar, tyLent:
        if taHeap notin flags: result = t2 # ``var var`` is illegal on the heap
      of tyOpenArray:
        if (kind != skParam and views notin c.features) or taIsOpenArray in flags: result = t
        else: result = typeAllowedAux(marker, t2[0], kind, c, flags+{taIsOpenArray})
      of tyUncheckedArray:
        if kind != skParam and views notin c.features: result = t
        else: result = typeAllowedAux(marker, t2[0], kind, c, flags)
      of tySink:
        result = t
      else:
        if kind notin {skParam, skResult} and views notin c.features: result = t
        else: result = typeAllowedAux(marker, t2, kind, c, flags)
  of tyProc:
    if kind in {skVar, skLet, skConst} and taIsTemplateOrMacro in flags:
      result = t
    else:
      if isInlineIterator(typ) and kind in {skVar, skLet, skConst, skParam, skResult}:
        # only closure iterators may be assigned to anything.
        result = t
      let f = if kind in {skProc, skFunc}: flags+{taNoUntyped} else: flags
      for i in 1..<t.len:
        if result != nil: break
        result = typeAllowedAux(marker, t[i], skParam, c, f-{taIsOpenArray})
      if result.isNil and t[0] != nil:
        result = typeAllowedAux(marker, t[0], skResult, c, flags)
  of tyTypeDesc:
    if kind in {skVar, skLet, skConst} and taProcContextIsNotMacro in flags:
      result = t
    else:
      # XXX: This is still a horrible idea...
      result = nil
  of tyUntyped, tyTyped:
    if kind notin {skParam, skResult} or taNoUntyped in flags: result = t
  of tyStatic:
    if kind notin {skParam}: result = t
  of tyVoid:
    if taField notin flags: result = t
  of tyTypeClasses:
    if tfGenericTypeParam in t.flags or taConcept in flags: #or taField notin flags:
      discard
    elif t.isResolvedUserTypeClass:
      result = typeAllowedAux(marker, t.lastSon, kind, c, flags)
    elif kind notin {skParam, skResult}:
      result = t
  of tyGenericBody, tyGenericParam, tyGenericInvocation,
     tyNone, tyForward, tyFromExpr:
    result = t
  of tyNil:
    if kind != skConst and kind != skParam: result = t
  of tyString, tyBool, tyChar, tyEnum, tyInt..tyUInt64, tyCstring, tyPointer:
    result = nil
  of tyOrdinal:
    if kind != skParam: result = t
  of tyGenericInst, tyDistinct, tyAlias, tyInferred:
    result = typeAllowedAux(marker, lastSon(t), kind, c, flags)
  of tyRange:
    if skipTypes(t[0], abstractInst-{tyTypeDesc}).kind notin
      {tyChar, tyEnum, tyInt..tyFloat64, tyInt..tyUInt64, tyRange}: result = t
  of tyOpenArray:
    # you cannot nest openArrays/sinks/etc.
    if (kind != skParam or taIsOpenArray in flags) and views notin c.features:
      result = t
    else:
      result = typeAllowedAux(marker, t[0], kind, c, flags+{taIsOpenArray})
  of tyVarargs:
    # you cannot nest openArrays/sinks/etc.
    if kind != skParam or taIsOpenArray in flags:
      result = t
    else:
      result = typeAllowedAux(marker, t[0], kind, c, flags+{taIsOpenArray})
  of tySink:
    # you cannot nest openArrays/sinks/etc.
    if kind != skParam or taIsOpenArray in flags or t[0].kind in {tySink, tyLent, tyVar}:
      result = t
    else:
      result = typeAllowedAux(marker, t[0], kind, c, flags)
  of tyUncheckedArray:
    if kind != skParam and taHeap notin flags:
      result = t
    else:
      result = typeAllowedAux(marker, lastSon(t), kind, c, flags-{taHeap})
  of tySequence:
    if t[0].kind != tyEmpty:
      result = typeAllowedAux(marker, t[0], kind, c, flags+{taHeap})
    elif kind in {skVar, skLet}:
      result = t[0]
  of tyArray:
    if t[1].kind == tyTypeDesc:
      result = t[1]
    elif t[1].kind != tyEmpty:
      result = typeAllowedAux(marker, t[1], kind, c, flags)
    elif kind in {skVar, skLet}:
      result = t[1]
  of tyRef:
    if kind == skConst:
      # allow ``ref`` types for ``skConst``, as whether they're allowed in that
      # context depends on the *value* (i.e. nil is okay, but everything else
      # is not)
      result = nil
    else:
      result = typeAllowedAux(marker, t.lastSon, kind, c, flags+{taHeap})
  of tyPtr:
    result = typeAllowedAux(marker, t.lastSon, kind, c, flags+{taHeap})
  of tySet:
    for i in 0..<t.len:
      result = typeAllowedAux(marker, t[i], kind, c, flags)
      if result != nil: break
  of tyObject, tyTuple:
    if kind in {skProc, skFunc, skConst} and
        t.kind == tyObject and t[0] != nil:
      result = t
    else:
      let flags = flags+{taField}
      for i in 0..<t.len:
        result = typeAllowedAux(marker, t[i], kind, c, flags)
        if result != nil: break
      if result.isNil and t.n != nil:
        result = typeAllowedNode(marker, t.n, kind, c, flags)
  of tyEmpty:
    if kind in {skVar, skLet}: result = t
  of tyProxy:
    # for now same as error node; we say it's a valid type as it should
    # prevent cascading errors:
    result = nil

proc typeAllowed*(t: PType, kind: TSymKind; c: PContext; flags: TTypeAllowedFlags = {}): PType =
  ## returns 'nil' on success and otherwise the part of the type that is wrong!
  var marker = initIntSet()
  result = typeAllowedAux(marker, t, kind, c, flags)

proc typeAllowedOrError*(t: PType, kind: TSymKind, c: PContext,
                         def: PNode, flags: TTypeAllowedFlags = {}): PType =
  ## returns the original `t` if allowed, otherwise a type not allowed error
  let temp = typeAllowed(t, kind, c, flags)
  if temp.isNil:
    t
  else:
    newTypeError(t, nextTypeId(c.idgen)):
              c.config.newError(def,
                                PAstDiag(
                                  kind: adSemTypeNotAllowed,
                                  allowedType: (
                                    allowed: temp,
                                    actual: t,
                                    kind: kind,
                                    allowedFlags: flags)))

type
  ViewTypeKind* = enum
    noView, immutableView, mutableView

proc combine(dest: var ViewTypeKind, b: ViewTypeKind) {.inline.} =
  case dest
  of noView, mutableView:
    dest = b
  of immutableView:
    if b == mutableView: dest = b

proc classifyViewTypeAux(marker: var IntSet, t: PType): ViewTypeKind

proc classifyViewTypeNode(marker: var IntSet, n: PNode): ViewTypeKind =
  case n.kind
  of nkSym:
    result = classifyViewTypeAux(marker, n.typ)
  of nkOfBranch:
    result = classifyViewTypeNode(marker, n.lastSon)
  else:
    result = noView
    for child in n:
      result.combine classifyViewTypeNode(marker, child)
      if result == mutableView: break

proc classifyViewTypeAux(marker: var IntSet, t: PType): ViewTypeKind =
  if containsOrIncl(marker, t.id): return noView
  case t.kind
  of tyVar:
    result = mutableView
  of tyLent, tyOpenArray, tyVarargs:
    result = immutableView
  of tyGenericInst, tyDistinct, tyAlias, tyInferred, tySink,
     tyUncheckedArray, tySequence, tyArray, tyRef, tyStatic:
    result = classifyViewTypeAux(marker, lastSon(t))
  of tyFromExpr:
    if t.len > 0:
      result = classifyViewTypeAux(marker, lastSon(t))
    else:
      result = noView
  of tyTuple:
    result = noView
    for i in 0..<t.len:
      result.combine classifyViewTypeAux(marker, t[i])
      if result == mutableView: break
  of tyObject:
    result = noView
    if t.n != nil:
      result = classifyViewTypeNode(marker, t.n)
    if t[0] != nil:
      result.combine classifyViewTypeAux(marker, t[0])
  else:
    # it doesn't matter what these types contain, 'ptr openArray' is not a
    # view type!
    result = noView

proc classifyViewType*(t: PType): ViewTypeKind =
  var marker = initIntSet()
  result = classifyViewTypeAux(marker, t)

proc directViewType*(t: PType): ViewTypeKind =
  # does classify 't' without looking recursively into 't'.
  case t.kind
  of tyVar:
    result = mutableView
  of tyLent, tyOpenArray:
    result = immutableView
  of abstractInst-{tyTypeDesc}:
    result = directViewType(t.lastSon)
  else:
    result = noView

proc requiresInit*(t: PType): bool =
  (t.flags * {tfRequiresInit, tfNeedsFullInit, tfNotNil} != {}) or
  classifyViewType(t) != noView
