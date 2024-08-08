#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the pattern matching features for term rewriting
## macro support.

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast,
    types,
    renderer,
    wordrecg,
    trees,
  ],
  compiler/front/[
    msgs,
    options
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportAst
from compiler/ast/report_enums import ReportKind

# we precompile the pattern here for efficiency into some internal
# stack based VM :-) Why? Because it's fun; I did no benchmarks to see if that
# actually improves performance.
type
  TAliasRequest* = enum ## first byte of the bytecode determines alias checking
    aqNone = 1,         ## no alias analysis requested
    aqShouldAlias,      ## with some other param
    aqNoAlias           ## request noalias
  TOpcode = enum
    ppEof = 1, ## end of compiled pattern
    ppOr,      ## we could short-cut the evaluation for 'and' and 'or',
    ppAnd,     ## but currently we don't
    ppNot,
    ppSym,
    ppAtom,
    ppLit,
    ppIdent,
    ppCall,
    ppSymKind,
    ppNodeKind,
    ppLValue,
    ppLocal,
    ppSideEffect,
    ppNoSideEffect
  TPatternCode = string

const
  MaxStackSize* = 64 ## max required stack size by the VM

proc add(code: var TPatternCode, op: TOpcode) {.inline.} =
  code.add chr(ord(op))

proc whichAlias*(p: PSym): TAliasRequest =
  if p.constraint != nil:
    result = TAliasRequest(p.constraint.strVal[0].ord)
  else:
    result = aqNone

proc compileConstraints(p: PNode, result: var TPatternCode; conf: ConfigRef) =
  case p.kind
  of nkCallKinds:
    if p[0].kind != nkIdent:
      conf.localReport(p[0].info, reportAst(
        rsemIllformedAst, p,
        str = "Expected ident for a first subnode, but found '$1'" % [$p[0].kind]))
      return
    let op = p[0].ident
    if p.len == 3:
      if op.s == "|" or op.id == ord(wOr):
        compileConstraints(p[1], result, conf)
        compileConstraints(p[2], result, conf)
        result.add(ppOr)
      elif op.s == "&" or op.id == ord(wAnd):
        compileConstraints(p[1], result, conf)
        compileConstraints(p[2], result, conf)
        result.add(ppAnd)
      else:
        conf.localReport(p.info, reportAst(
          rsemIllformedAst, p,
          str = "Expected any of '|', 'or', '&', 'and' for TRM pattern, but found '$1' ($2)" % [
            $op.s, $op.id]))

    elif p.len == 2 and (op.s == "~" or op.id == ord(wNot)):
      compileConstraints(p[1], result, conf)
      result.add(ppNot)
    else:
      conf.localReport(p.info, reportAst(
        rsemIllformedAst, p,
        str = "Unexpected trm patern - wanted negation or and/or infix"))

  of nkAccQuoted, nkPar:
    if p.len == 1:
      compileConstraints(p[0], result, conf)
    else:
      conf.localReport(p.info, reportAst(
        rsemIllformedAst, p,
        str = "Unexpected number of nodes for node - " &
          "wanted $1, but found $2" % [$1, $p.len]))

  of nkIdent:
    let spec = p.ident.s.normalize
    case spec
    of "atom": result.add(ppAtom)
    of "lit": result.add(ppLit)
    of "sym": result.add(ppSym)
    of "ident": result.add(ppIdent)
    of "call": result.add(ppCall)
    of "alias": result[0] = chr(aqShouldAlias.ord)
    of "noalias": result[0] = chr(aqNoAlias.ord)
    of "lvalue": result.add(ppLValue)
    of "local": result.add(ppLocal)
    of "sideeffect": result.add(ppSideEffect)
    of "nosideeffect": result.add(ppNoSideEffect)
    else:
      # check all symkinds:
      internalAssert conf, int(high(TSymKind)) < 255, "[FIXME]"
      for i in TSymKind:
        if cmpIgnoreStyle(i.toHumanStr, spec) == 0:
          result.add(ppSymKind)
          result.add(chr(i.ord))
          return
      # check all nodekinds:
      internalAssert conf, int(high(TNodeKind)) < 255, "[FIXME]"
      for i in TNodeKind:
        if cmpIgnoreStyle($i, spec) == 0:
          result.add(ppNodeKind)
          result.add(chr(i.ord))
          return
      conf.localReport(p.info, reportAst(rsemIllformedAst, p))
  else:
    conf.localReport(p.info, reportAst(rsemIllformedAst, p))

proc semNodeKindConstraints*(n: PNode; conf: ConfigRef; start: Natural): PNode =
  ## does semantic checking for a node kind pattern and compiles it into an
  ## efficient internal format.
  result = newNodeI(nkStrLit, n.info)
  result.strVal = newStringOfCap(10)
  result.strVal.add(chr(aqNone.ord))
  if n.len >= 2:
    for i in start..<n.len:
      compileConstraints(n[i], result.strVal, conf)

    conf.internalAssert(result.strVal.len < MaxStackSize, n.info, "parameter pattern too complex")

  else:
    conf.localReport(n.info, reportAst(rsemIllformedAst, n))
  result.strVal.add(ppEof)

type
  TSideEffectAnalysis* = enum
    seUnknown, seSideEffect, seNoSideEffect

proc checkForSideEffects*(n: PNode): TSideEffectAnalysis =
  case n.kind
  of nkCallKinds:
    # only calls can produce side effects:
    let op = n[0]
    if op.kind == nkSym and isRoutine(op.sym):
      let s = op.sym
      if sfSideEffect in s.flags:
        return seSideEffect
      elif tfNoSideEffect in op.typ.flags:
        result = seNoSideEffect
      else:
        # assume side effect:
        result = seSideEffect
    elif tfNoSideEffect in op.typ.flags:
      # indirect call without side effects:
      result = seNoSideEffect
    else:
      # indirect call: assume side effect:
      return seSideEffect
    # we need to check n[0] too: (FwithSideEffectButReturnsProcWithout)(args)
    for i in 0..<n.len:
      let ret = checkForSideEffects(n[i])
      if ret == seSideEffect: return ret
      elif ret == seUnknown and result == seNoSideEffect:
        result = seUnknown
  of nkWithoutSons:
    # an atom cannot produce a side effect:
    result = seNoSideEffect
  else:
    # assume no side effect:
    result = seNoSideEffect
    for i in 0..<n.len:
      let ret = checkForSideEffects(n[i])
      if ret == seSideEffect: return ret
      elif ret == seUnknown and result == seNoSideEffect:
        result = seUnknown

type
  TAssignableResult* = enum
    arNone,                   ## no l-value and no discriminant
    arLValue,                 ## is an l-value
    arLocalLValue,            ## is an l-value, but local var; must not escape
                              ## its stack frame!
    arDiscriminant,           ## is a discriminant
    arLentValue,              ## lent value
    arStrange                 ## it is a strange beast like 'typedesc[var T]'

proc exprRoot*(n: PNode): PSym =
  var it = n
  while true:
    case it.kind
    of nkSym: return it.sym
    of nkHiddenDeref, nkDerefExpr:
      if it[0].typ.skipTypes(abstractInst).kind in {tyPtr, tyRef}:
        # 'ptr' is unsafe anyway and 'ref' is always on the heap,
        # so allow these derefs:
        break
      else:
        it = it[0]
    of nkDotExpr, nkBracketExpr, nkHiddenAddr,
       nkObjUpConv, nkObjDownConv, nkCheckedFieldExpr:
      it = it[0]
    of nkHiddenStdConv, nkHiddenSubConv, nkConv:
      it = it[1]
    of nkStmtList, nkStmtListExpr:
      if it.len > 0 and it.typ != nil: it = it.lastSon
      else: break
    of nkCallKinds:
      if it.typ != nil and it.typ.kind in {tyVar, tyLent} and it.len > 1:
        # See RFC #7373, calls returning 'var T' are assumed to
        # return a view into the first argument (if there is one):
        it = it[1]
      else:
        break
    else:
      break

proc isAssignable*(owner: PSym, n: PNode; isUnsafeAddr=false): TAssignableResult =
  ## 'owner' can be nil!
  result = arNone
  case n.kind
  of nkEmpty:
    if n.typ != nil and n.typ.kind in {tyVar}:
      result = arLValue
  of nkSym:
    let kinds = if isUnsafeAddr: {skVar, skResult, skTemp, skParam, skLet, skForVar}
                else: {skVar, skResult, skTemp}
    if n.sym.kind == skParam and n.sym.typ.kind in {tyVar, tySink}:
      result = arLValue
    elif isUnsafeAddr and n.sym.kind == skParam:
      result = arLValue
    elif isUnsafeAddr and n.sym.kind == skConst and dontInlineConstant(n, n.sym.ast):
      result = arLValue
    elif n.sym.kind in kinds:
      if owner != nil and owner == n.sym.owner and
          sfGlobal notin n.sym.flags:
        result = arLocalLValue
      else:
        result = arLValue
    elif n.sym.kind == skType:
      let t = n.sym.typ.skipTypes({tyTypeDesc})
      if t.kind in {tyVar}: result = arStrange
  of nkDotExpr:
    let t = skipTypes(n[0].typ, abstractInst-{tyTypeDesc})
    if t.kind in {tyVar, tySink, tyPtr, tyRef}:
      result = arLValue
    elif isUnsafeAddr and t.kind == tyLent:
      result = arLValue
    else:
      result = isAssignable(owner, n[0], isUnsafeAddr)
    if result != arNone and n[1].kind == nkSym and
        sfDiscriminant in n[1].sym.flags:
      result = arDiscriminant
  of nkBracketExpr:
    let t = skipTypes(n[0].typ, abstractInst-{tyTypeDesc})
    if t.kind in {tyVar, tySink, tyPtr, tyRef}:
      result = arLValue
    elif isUnsafeAddr and t.kind == tyLent:
      result = arLValue
    else:
      result = isAssignable(owner, n[0], isUnsafeAddr)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    # Object and tuple conversions are still addressable, so we skip them
    # XXX why is 'tyOpenArray' allowed here?
    if skipTypes(n.typ, abstractPtrs-{tyTypeDesc}).kind in
        {tyOpenArray, tyTuple, tyObject}:
      result = isAssignable(owner, n[1], isUnsafeAddr)
    elif compareTypes(n.typ, n[1].typ, dcEqIgnoreDistinct):
      # types that are equal modulo distinction preserve l-value:
      result = isAssignable(owner, n[1], isUnsafeAddr)
  of nkHiddenDeref:
    let n0 = n[0]
    if n0.typ.kind == tyLent:
      if isUnsafeAddr or (n0.kind == nkSym and n0.sym.kind == skResult):
        result = arLValue
      else:
        result = arLentValue
    else:
      result = arLValue
  of nkDerefExpr, nkHiddenAddr:
    result = arLValue
  of nkObjUpConv, nkObjDownConv, nkCheckedFieldExpr:
    result = isAssignable(owner, n[0], isUnsafeAddr)
  of nkCallKinds:
    # builtin slice keeps lvalue-ness:
    if getMagic(n) in {mArrGet, mSlice}:
      result = isAssignable(owner, n[1], isUnsafeAddr)
    elif n.typ != nil and n.typ.kind in {tyVar}:
      result = arLValue
    elif isUnsafeAddr and n.typ != nil and n.typ.kind == tyLent:
      result = arLValue
  of nkStmtList, nkStmtListExpr:
    if n.typ != nil:
      result = isAssignable(owner, n.lastSon, isUnsafeAddr)
  of nkVarTy:
    # XXX: The fact that this is here is a bit of a hack.
    # The goal is to allow the use of checks such as "foo(var T)"
    # within concepts. Semantically, it's not correct to say that
    # nkVarTy denotes an lvalue, but the example above is the only
    # possible code which will get us here
    result = arLValue
  else:
    discard

proc isLValue*(n: PNode): bool =
  isAssignable(nil, n) in {arLValue, arLocalLValue, arStrange}

proc matchNodeKinds*(p, n: PNode): bool =
  # matches the parameter constraint 'p' against the concrete AST 'n'.
  # Efficiency matters here.
  var stack {.noinit.}: array[0..MaxStackSize, bool]
  # empty patterns are true:
  stack[0] = true
  var sp = 1

  template push(x: bool) =
    stack[sp] = x
    inc sp

  let code = p.strVal
  var pc = 1
  while true:
    case TOpcode(code[pc])
    of ppEof: break
    of ppOr:
      stack[sp-2] = stack[sp-1] or stack[sp-2]
      dec sp
    of ppAnd:
      stack[sp-2] = stack[sp-1] and stack[sp-2]
      dec sp
    of ppNot: stack[sp-1] = not stack[sp-1]
    of ppSym: push n.kind == nkSym
    of ppAtom: push isAtom(n)
    of ppLit: push n.kind in nkLiterals
    of ppIdent: push n.kind == nkIdent
    of ppCall: push n.kind in nkCallKinds
    of ppSymKind:
      let kind = TSymKind(code[pc+1])
      push n.kind == nkSym and n.sym.kind == kind
      inc pc
    of ppNodeKind:
      let kind = TNodeKind(code[pc+1])
      push n.kind == kind
      inc pc
    of ppLValue: push isAssignable(nil, n) in {arLValue, arLocalLValue}
    of ppLocal: push isAssignable(nil, n) == arLocalLValue
    of ppSideEffect: push checkForSideEffects(n) == seSideEffect
    of ppNoSideEffect: push checkForSideEffects(n) != seSideEffect
    inc pc
  result = stack[sp-1]
