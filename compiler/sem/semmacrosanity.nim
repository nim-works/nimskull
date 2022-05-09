#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Implements type sanity checking for ASTs resulting from macros. Lots of
## room for improvement here.
import
  std/[
    strutils
  ],
  compiler/ast/[
    ast,
    types,
    typesrenderer,
    reports,
  ],
  compiler/front/[
    msgs,
    options,
  ]

proc ithField(n: PNode, field: var int): PSym =
  result = nil
  case n.kind
  of nkRecList:
    for i in 0..<n.len:
      result = ithField(n[i], field)
      if result != nil: return
  of nkRecCase:
    if n[0].kind != nkSym: return
    result = ithField(n[0], field)
    if result != nil: return
    for i in 1..<n.len:
      case n[i].kind
      of nkOfBranch, nkElse:
        result = ithField(lastSon(n[i]), field)
        if result != nil: return
      else: discard
  of nkSym:
    if field == 0: result = n.sym
    else: dec(field)
  else: discard

proc ithField(t: PType, field: var int): PSym =
  var base = t[0]
  while base != nil:
    let b = skipTypes(base, skipPtrs)
    result = ithField(b.n, field)
    if result != nil: return result
    base = b[0]
  result = ithField(t.n, field)

proc annotateType*(n: PNode, t: PType; conf: ConfigRef) =
  let x = t.skipTypes(abstractInst + {tyRange})
  # Note: x can be unequal to t and we need to be careful to use 't'
  # to not to skip tyGenericInst

  proc malformedType(msg: string, expected: set[TTypeKind]) =
    globalReport(conf, n.info, SemReport(
      kind: rsemTypeKindMismatch,
      typeMismatch: @[conf.typeMismatch(formal = expected, actual = n.typ)],
      ast: n,
      str: msg))

  case n.kind
  of nkObjConstr:
    let x = t.skipTypes(abstractPtrs)
    n.typ = t
    for i in 1..<n.len:
      var j = i-1
      let field = x.ithField(j)
      if field.isNil:
        globalReport(conf, n.info, reportAst(
          rsemIllformedAst, n,
          str = "'nil' field at index" & $i))

      else:
        internalAssert(
          conf,
          n[i].kind == nkExprColonExpr,
          "Object constructor expects exprColornExpr, but n[$1] has kind $2" % [
            $i, $n[i].kind])

        annotateType(n[i][1], field.typ, conf)
  of nkPar, nkTupleConstr:
    if x.kind == tyTuple:
      n.typ = t
      for i in 0..<n.len:
        if i >= x.len:
          globalReport(conf, n.info, reportAst(
            rsemIllformedAst, n,
            str = "Unexpected field at index $1 - type $2 is expected to have $3 fields." % [
              $i, $x, $x.len]))

        else:
          annotateType(n[i], x[i], conf)

    elif x.kind == tyProc and x.callConv == ccClosure:
      n.typ = t

    else:
      malformedType("() must have a tuple or closure proc type", {tyTuple, tyProc})

  of nkBracket:
    if x.kind in {tyArray, tySequence, tyOpenArray}:
      n.typ = t
      for m in n:
        annotateType(m, x.elemType, conf)

    else:
      malformedType(
        "[] must have some form of array type",
        {tyArray, tySequence, tyOpenArray})

  of nkCurly:
    if x.kind in {tySet}:
      n.typ = t
      for m in n:
        annotateType(m, x.elemType, conf)
    else:
      malformedType("{} must have the set type", {tySet})

  of nkFloatLit..nkFloat128Lit:
    if x.kind in {tyFloat..tyFloat128}:
      n.typ = t

    else:
      malformedType(
        "float literal must have some float type", {tyFloat..tyFloat128})

  of nkCharLit..nkUInt64Lit:
    if x.kind in {tyInt..tyUInt64, tyBool, tyChar, tyEnum}:
      n.typ = t

    else:
      malformedType(
        "integer literal must have some int type",
        {tyInt..tyUInt64, tyBool, tyChar, tyEnum})

  of nkStrLit..nkTripleStrLit:
    if x.kind in {tyString, tyCstring}:
      n.typ = t

    else:
      malformedType(
        "string literal must be of some string type",
        {tyString, tyCstring})

  of nkNilLit:
    if x.kind in NilableTypes + {tyString, tySequence}:
      n.typ = t

    else:
      malformedType(
        "nil literal must be of some pointer type", NilableTypes + {tyString, tySequence})

  else:
    discard
