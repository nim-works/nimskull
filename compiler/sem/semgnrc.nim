#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This implements the first pass over the generic body; it resolves some
## symbols. Thus for generics there is a two-phase symbol lookup.
##
## A two-phase lookup as today is templates and not generics, AKA mistake.
##
## A problem is that it cannot be detected if the symbol is introduced
## as in ``var x = ...`` or used because macros/templates can hide this!
## So we have to eval templates/macros right here so that symbol
## lookup can be accurate.

## included from sem.nim

proc getIdentNode(c: PContext; n: PNode): PNode =
  case n.kind
  of nkPostfix: result = getIdentNode(c, n[1])
  of nkPragmaExpr: result = getIdentNode(c, n[0])
  of nkIdent, nkAccQuoted, nkSym: result = n
  of nkError:
    # Passing it through; considerQuotedIdent in
    # newSymS will handle and output this error
    result = n
  else:
    semReportIllformedAst(c.config, n, {
      nkPostfix, nkPragmaExpr, nkIdent, nkAccQuoted, nkSym})

    result = n

type
  GenericCtx = object
    toMixin, toBind: IntSet
    cursorInBody: bool # only for nimsuggest
    bracketExpr: PNode

  TSemGenericFlag = enum
    withinBind,
    withinTypeDesc,
    withinMixin,
    withinConcept

  TSemGenericFlags = set[TSemGenericFlag]

proc semGenericStmt(c: PContext, n: PNode,
                    flags: TSemGenericFlags, ctx: var GenericCtx): PNode

proc semGenericStmtScope(c: PContext, n: PNode,
                         flags: TSemGenericFlags,
                         ctx: var GenericCtx): PNode =
  openScope(c)
  result = semGenericStmt(c, n, flags, ctx)
  closeScope(c)

template macroToExpand(s): untyped =
  s.kind in {skMacro, skTemplate} and (s.typ.len == 1 or sfAllUntyped in s.flags)

template macroToExpandSym(s): untyped =
  sfCustomPragma notin s.flags and s.kind in {skMacro, skTemplate} and
    (s.typ.len == 1) and not fromDotExpr

template isMixedIn(sym): bool =
  let s = sym
  s.name.id in ctx.toMixin or (withinConcept in flags and
                               s.magic == mNone and
                               s.kind in OverloadableSyms)

proc semGenericStmtSymbol(c: PContext, n: PNode, s: PSym,
                          ctx: var GenericCtx; flags: TSemGenericFlags,
                          fromDotExpr=false): PNode =
  semIdeForTemplateOrGenericCheck(c.config, n, ctx.cursorInBody)
  incl(s.flags, sfUsed)
  case s.kind
  of skUnknown:
    # Introduced in this pass! Leave it as an identifier.
    result = n
  of skProc, skFunc, skMethod, skIterator, skConverter, skModule:
    result = symChoice(c, n, s, scOpen)
  of skTemplate:
    if macroToExpandSym(s):
      result = semTemplateExpr(c, n, s, {efNoSemCheck})
      result = semGenericStmt(c, result, {}, ctx)
    else:
      result = symChoice(c, n, s, scOpen)
  of skMacro:
    if macroToExpandSym(s):
      result = semMacroExpr(c, n, s, {efNoSemCheck})
      result = semGenericStmt(c, result, {}, ctx)
    else:
      result = symChoice(c, n, s, scOpen)
  of skGenericParam:
    if s.typ != nil and s.typ.kind == tyStatic:
      if s.typ.n != nil:
        result = s.typ.n
      else:
        result = n
    else:
      result = newSymNodeTypeDesc(s, c.idgen, n.info)
  of skParam:
    result = n
  of skType:
    if (s.typ != nil) and
       (s.typ.flags * {tfGenericTypeParam, tfImplicitTypeParam} == {}):
      result = newSymNodeTypeDesc(s, c.idgen, n.info)
    else:
      result = n
  of skEnumField:
    if overloadableEnums in c.features:
      result = symChoice(c, n, s, scOpen)
    else:
      result = newSymNode(s, n.info)
  else:
    result = newSymNode(s, n.info)

proc lookup(c: PContext, n: PNode, flags: TSemGenericFlags,
            ctx: var GenericCtx): PNode =
  result = n
  let (ident, err) = considerQuotedIdent(c, n)
  if err != nil:
    result = err
    return
  var amb = false
  var s = searchInScopes(c, ident, amb)
  if s == nil:
    s = strTableGet(c.pureEnumFields, ident)
    #if s != nil and contains(c.ambiguousSymbols, s.id):
    #  s = nil
  if s == nil:
    if ident.id notin ctx.toMixin and withinMixin notin flags:
      result = c.createUndeclaredIdentifierError(n, ident.s)
  else:
    if withinBind in flags or s.id in ctx.toBind:
      result = symChoice(c, n, s, scClosed)
    elif s.isMixedIn:
      result = symChoice(c, n, s, scForceOpen)
    else:
      result = semGenericStmtSymbol(c, n, s, ctx, flags)
  # else: leave as nkIdent

proc newDot(n, b: PNode): PNode =
  newTreeI(nkDotExpr, n.info, [n[0], b])

proc fuzzyLookup(c: PContext, n: PNode, flags: TSemGenericFlags,
                 ctx: var GenericCtx; isMacro: var bool): PNode =
  assert n.kind == nkDotExpr
  semIdeForTemplateOrGenericCheck(c.config, n, ctx.cursorInBody)

  let luf = if withinMixin notin flags: {checkUndeclared, checkModule} else: {checkModule}

  var s = qualifiedLookUp(c, n, luf)
  if s.isError:
    result = s.ast
  elif s != nil:
    result = semGenericStmtSymbol(c, n, s, ctx, flags)
  else:
    n[0] = semGenericStmt(c, n[0], flags, ctx)
    if n[0].isError:
      result = c.config.wrapError(n)
      return
    result = n
    let
      n = n[1]
      (ident, err) = considerQuotedIdent(c, n)
    if err != nil:
      result[1] = err
      result = c.config.wrapError(result)
      return
    let candidates = searchInScopesFilterBy(c, ident, routineKinds)
    if candidates.len > 0:
      let s = candidates[0] # XXX take into account the other candidates!
      isMacro = s.kind in {skTemplate, skMacro}
      if withinBind in flags or s.id in ctx.toBind:
        result = newDot(result, symChoice(c, n, s, scClosed))
      elif s.isMixedIn:
        result = newDot(result, symChoice(c, n, s, scForceOpen))
      else:
        let syms = semGenericStmtSymbol(c, n, s, ctx, flags, fromDotExpr=true)
        if syms.kind == nkSym:
          let choice = symChoice(c, n, s, scForceOpen)
          choice.transitionSonsKind(nkClosedSymChoice)
          result = newDot(result, choice)
        else:
          result = newDot(result, syms)
          if syms.isError:
            result = c.config.wrapError(result)

proc addTempDecl(c: PContext; n: PNode; kind: TSymKind) =
  # newSymS doesn't use nkError/skError currently, so we
  # don't need to check and wrap here.
  let s = newSymS(skUnknown, getIdentNode(c, n), c)
  addPrelimDecl(c, s)
  styleCheckDef(c.config, n.info, s, kind)

template captureError(conf: ConfigRef, n: PNode, body) =
  # Should this pattern arise more often, perhaps
  # consider moving this to errorhandling
  var hasError = false
  template checkError(potErr: PNode): PNode =
    let x = potErr
    if x.isError: hasError = true
    x
  body
  if hasError:
    n = conf.wrapError(n)

proc semGenericStmt(c: PContext, n: PNode,
                    flags: TSemGenericFlags, ctx: var GenericCtx): PNode =
  result = n

  when defined(nimsuggest):
    if withinTypeDesc in flags: inc c.inTypeContext

  #if conf.cmd == cmdIdeTools: suggestStmt(c, n)
  semIdeForTemplateOrGenericCheck(c.config, n, ctx.cursorInBody)

  addInNimDebugUtils(c.config, "semGenericStmt", n, result)

  case n.kind
  of nkIdent, nkAccQuoted:
    result = lookup(c, n, flags, ctx)
    if result != nil and result.kind == nkSym:
      assert result.sym != nil
      markUsed(c, n.info, result.sym)
  of nkDotExpr:
    # XXX for example: ``result.add`` -- ``add`` needs to be looked up here...
    var dummy: bool
    result = fuzzyLookup(c, n, flags, ctx, dummy)
  of nkSym:
    let a = n.sym
    let b = getGenSym(c, a)
    if b != a: n.sym = b
  of nkWithoutSons - {nkIdent, nkSym}:
    # see tests/compile/tgensymgeneric.nim:
    # We need to open the gensym'ed symbol again so that the instantiation
    # creates a fresh copy; but this is wrong the very first reason for gensym
    # is that scope rules cannot be used! So simply removing 'sfGenSym' does
    # not work. Copying the symbol does not work either because we're already
    # the owner of the symbol! What we need to do is to copy the symbol
    # in the generic instantiation process...
    discard
  of nkBind:
    result = semGenericStmt(c, n[0], flags+{withinBind}, ctx)
  of nkMixinStmt:
    result = semMixinStmt(c, n, ctx.toMixin)
  of nkBindStmt:
    result = semBindStmt(c, n, ctx.toBind)
  of nkCall, nkHiddenCallConv, nkInfix, nkPrefix, nkCommand, nkCallStrLit:
    # check if it is an expression macro:
    checkMinSonsLen(n, 1, c.config)
    let fn = n[0]
    var s = qualifiedLookUp(c, fn, {})
    if s.isError:
      result[0] = s.ast
      result = wrapError(c.config, result)
      return
    if s == nil and
        {withinMixin, withinConcept}*flags == {} and
        fn.kind in {nkIdent, nkAccQuoted} and
        legacyConsiderQuotedIdent(c, fn, nil).id notin ctx.toMixin:
      errorUndeclaredIdentifier(c, n.info, fn.renderTree)

    var first = int ord(withinConcept in flags)
    var mixinContext = false
    if s != nil:
      incl(s.flags, sfUsed)
      mixinContext = s.magic in {mDefined, mDeclared, mDeclaredInScope, mCompiles, mAstToStr}
      let whichChoice = if s.id in ctx.toBind: scClosed
                        elif s.isMixedIn: scForceOpen
                        else: scOpen
      let sc = symChoice(c, fn, s, whichChoice)
      case s.kind
      of skMacro:
        if macroToExpand(s) and sc.safeLen <= 1:
          result = semMacroExpr(c, n, s, {efNoSemCheck})
          result =
            if result.kind == nkError: n # an error means a "mismatch"
            else:                      semGenericStmt(c, result, flags, ctx)
          result = semGenericStmt(c, result, flags, ctx)
          if result.isError: return
        else:
          n[0] = sc
          result = n
        mixinContext = true
      of skTemplate:
        if macroToExpand(s) and sc.safeLen <= 1:
          result = semTemplateExpr(c, n, s, {efNoSemCheck})
          result =
            if result.kind == nkError: n # an error means a "mismatch"
            else:                      semGenericStmt(c, result, flags, ctx)
          if result.isError: return
        else:
          n[0] = sc
          result = n
        # BUGFIX: we must not return here, we need to do first phase of
        # symbol lookup. Also since templates and macros can do scope injections
        # we need to put the ``c`` in ``t(c)`` in a mixin context to prevent
        # the famous "undeclared identifier: it" bug:
        mixinContext = true
      of skParam:
        # Leave it as an identifier.
        discard
      of skProc, skFunc, skMethod, skIterator, skConverter, skModule:
        result[0] = sc
        first = 1
        # We're not interested in the example code during this pass so let's
        # skip it
        if s.magic == mRunnableExamples:
          first = result.safeLen # see trunnableexamples.fun3
      of skGenericParam:
        result[0] = newSymNodeTypeDesc(s, c.idgen, fn.info)
        first = 1
      of skType:
        # bad hack for generics:
        if (s.typ != nil) and (s.typ.kind != tyGenericParam):
          result[0] = newSymNodeTypeDesc(s, c.idgen, fn.info)
          first = 1
      of skError:
        if s.isError: # has the error ast
          result[0] = s.ast
          result = wrapError(c.config, result)
          return
        else: # legacy nimsuggest skUnknown usage          
          # Leave it as an identifier.
          discard
      else:
        result[0] = newSymNode(s, fn.info)
        first = 1
    elif fn.kind == nkDotExpr:
      result[0] = fuzzyLookup(c, fn, flags, ctx, mixinContext)
      if result[0].isError:
        result = c.config.wrapError(result)
      first = 1
    # Consider 'when declared(globalsSlot): ThreadVarSetValue(globalsSlot, ...)'
    # in threads.nim: the subtle preprocessing here binds 'globalsSlot' which
    # is not exported and yet the generic 'threadProcWrapper' works correctly.
    if not result.isError:
      captureError c.config, result:
        let flags = if mixinContext: flags+{withinMixin} else: flags
        for i in first..<result.safeLen:
          result[i] = checkError semGenericStmt(c, result[i], flags, ctx)
  of nkCurlyExpr:
    result = newNodeI(nkCall, n.info)
    result.add newIdentNode(getIdent(c.cache, "{}"), n.info)
    for i in 0..<n.len: result.add(n[i])
    result = semGenericStmt(c, result, flags, ctx)
    if result.isError: return
  of nkBracketExpr:
    # xxx: screwing up `nkBracketExpr` nodes like this does no one any favours.
    #      instead, just pass them on and figure out what to do _later_ when
    #      there is more context to make a decision. Instead, `semExpr` now has
    #      to crudely recreate this information.
    result = newNodeI(nkCall, n.info)
    result.add newIdentNode(getIdent(c.cache, "[]"), n.info)
    for i in 0..<n.len: result.add(n[i])
    result = semGenericStmt(c, result, flags, ctx)
    if result.isError: return
  of nkAsgn, nkFastAsgn:
    checkSonsLen(n, 2, c.config)
    let a = n[0]
    let b = n[1]

    let k = a.kind
    case k
    of nkCurlyExpr:
      result = newNodeI(nkCall, n.info)
      result.add newIdentNode(getIdent(c.cache, "{}="), n.info)
      for i in 0..<a.len: result.add a[i]
      result.add b
      result = semGenericStmt(c, result, flags, ctx)
      if result.isError: return
    of nkBracketExpr:
      result = newNodeI(nkCall, n.info)
      result.add newIdentNode(getIdent(c.cache, "[]="), n.info)
      for i in 0..<a.len: result.add a[i]
      result.add b
      result = semGenericStmt(c, result, flags, ctx)
      if result.isError: return
    else:
      captureError c.config, result:
        for i in 0..<n.len:
          result[i] = checkError semGenericStmt(c, n[i], flags, ctx)
  of nkIfStmt:
    captureError c.config, result:
      for i in 0..<n.len:
        n[i] = checkError semGenericStmtScope(c, n[i], flags, ctx)
  of nkWhenStmt:
    captureError c.config, result:
      for i in 0..<n.len:
        # bug #8603: conditions of 'when' statements are not
        # in a 'mixin' context:
        let it = n[i]
        if it.kind in {nkElifExpr, nkElifBranch}:
          n[i][0] = checkError semGenericStmt(c, it[0], flags, ctx)
          n[i][1] = checkError semGenericStmt(c, it[1], flags+{withinMixin}, ctx)
        else:
          n[i] = checkError semGenericStmt(c, it, flags+{withinMixin}, ctx)
  of nkWhileStmt:
    captureError c.config, result:
      openScope(c)
      for i in 0..<n.len:
        n[i] = checkError semGenericStmt(c, n[i], flags, ctx)
      closeScope(c)
  of nkCaseStmt:
    captureError c.config, result:
      openScope(c)
      n[0] = checkError semGenericStmt(c, n[0], flags, ctx)
      for i in 1..<n.len:
        var a = n[i]
        checkMinSonsLen(a, 1, c.config)
        for j in 0..<a.len-1:
          a[j] = checkError semGenericStmt(c, a[j], flags, ctx)
        a[^1] = checkError semGenericStmtScope(c, a[^1], flags, ctx)
      closeScope(c)
  of nkForStmt:
    captureError c.config, result:
      openScope(c)
      n[^2] = checkError semGenericStmt(c, n[^2], flags, ctx)
      for i in 0..<n.len - 2:
        if (n[i].kind == nkVarTuple):
          for s in n[i]:
            if (s.kind == nkIdent):
              addTempDecl(c,s,skForVar)
        else:
          addTempDecl(c, n[i], skForVar)
      openScope(c)
      n[^1] = checkError semGenericStmt(c, n[^1], flags, ctx)
      closeScope(c)
      closeScope(c)
  of nkBlockStmt, nkBlockExpr:
    checkSonsLen(n, 2, c.config)
    openScope(c)
    if n[0].kind != nkEmpty:
      addTempDecl(c, n[0], skLabel)
    n[1] = semGenericStmt(c, n[1], flags, ctx)
    closeScope(c)
    if n[1].isError:
      result = c.config.wrapError(n)
  of nkTryStmt, nkHiddenTryStmt:
    checkMinSonsLen(n, 2, c.config)
    captureError c.config, result:
      n[0] = checkError semGenericStmtScope(c, n[0], flags, ctx)
      for i in 1..<n.len:
        var a = n[i]
        checkMinSonsLen(a, 1, c.config)
        openScope(c)
        for j in 0..<a.len-1:
          if a[j].isInfixAs():
            addTempDecl(c, a[j][2], skLet)
            a[j][1] = checkError semGenericStmt(c, a[j][1], flags+{withinTypeDesc}, ctx)
          else:
            a[j] = checkError semGenericStmt(c, a[j], flags+{withinTypeDesc}, ctx)
        a[^1] = checkError semGenericStmtScope(c, a[^1], flags, ctx)
        closeScope(c)
  of nkVarSection, nkLetSection, nkConstSection:
    captureError c.config, result:
      let varKind =
        case n.kind
        of nkVarSection: skVar
        of nkLetSection: skLet
        else: skConst
      for i in 0..<n.len:
        var a = n[i]
        case a.kind:
        of nkCommentStmt: continue
        of nkIdentDefs, nkVarTuple, nkConstDef:
          checkMinSonsLen(a, 3, c.config)
          a[^2] = checkError semGenericStmt(c, a[^2], flags+{withinTypeDesc}, ctx)
          a[^1] = checkError semGenericStmt(c, a[^1], flags, ctx)
          for j in 0..<a.len-2:
            addTempDecl(c, a[j], varKind)
        else:
          semReportIllformedAst(c.config, a, {
            nkCommentStmt, nkIdentDefs, nkVarTuple, nkConstDef})
  of nkGenericParams:
    captureError c.config, result:
      for i in 0..<n.len:
        var a = n[i]
        if (a.kind != nkIdentDefs):
          semReportIllformedAst(c.config, a, {nkIdentDefs})

        checkMinSonsLen(a, 3, c.config)
        a[^2] = checkError semGenericStmt(c, a[^2], flags+{withinTypeDesc}, ctx)
        # do not perform symbol lookup for default expressions
        for j in 0..<a.len-2:
          addTempDecl(c, a[j], skType)
  of nkTypeSection:
    captureError c.config, result:
      for i in 0..<n.len:
        var a = n[i]
        if a.kind == nkCommentStmt: continue
        if (a.kind != nkTypeDef):
          semReportIllformedAst(c.config, a, {nkTypeDef})
        checkSonsLen(a, 3, c.config)
        addTempDecl(c, a[0], skType)
      for i in 0..<n.len:
        var a = n[i]
        if a.kind == nkCommentStmt: continue
        if (a.kind != nkTypeDef):
          semReportIllformedAst(c.config, a, {nkTypeDef})
        checkSonsLen(a, 3, c.config)
        if a[1].kind != nkEmpty:
          openScope(c)
          a[1] = checkError semGenericStmt(c, a[1], flags, ctx)
          a[2] = checkError semGenericStmt(c, a[2], flags+{withinTypeDesc}, ctx)
          closeScope(c)
        else:
          a[2] = checkError semGenericStmt(c, a[2], flags+{withinTypeDesc}, ctx)
  of nkEnumTy:
    captureError c.config, result:
      if n.len > 0:
        if n[0].kind != nkEmpty:
          n[0] = checkError semGenericStmt(c, n[0], flags+{withinTypeDesc}, ctx)

        for i in 1..<n.len:
          var a: PNode
          case n[i].kind
          of nkEnumFieldDef: a = n[i][0]
          of nkIdent: a = n[i]
          else:
            semReportIllformedAst(c.config, n[i], {nkEnumFieldDef, nkIdent})

          addDecl(c, newSymS(skUnknown, getIdentNode(c, a), c))
  of nkObjectTy, nkTupleTy, nkTupleClassTy:
    discard
  of nkFormalParams:
    checkMinSonsLen(n, 1, c.config)
    captureError c.config, result:
      for i in 1..<n.len:
        var a = n[i]
        if (a.kind != nkIdentDefs):
          semReportIllformedAst(c.config, a, {nkIdentDefs})

        checkMinSonsLen(a, 3, c.config)
        a[^2] = checkError semGenericStmt(c, a[^2], flags+{withinTypeDesc}, ctx)
        a[^1] = checkError semGenericStmt(c, a[^1], flags, ctx)
        for j in 0..<a.len-2:
          addTempDecl(c, a[j], skParam)
      # XXX: last change was moving this down here, search for "1.." to keep
      #      going from this file onward
      if n[0].kind != nkEmpty:
        n[0] = checkError semGenericStmt(c, n[0], flags+{withinTypeDesc}, ctx)
  of nkProcDef, nkMethodDef, nkConverterDef, nkMacroDef, nkTemplateDef,
     nkFuncDef, nkIteratorDef, nkLambdaKinds:
    checkSonsLen(n, bodyPos + 1, c.config)
    captureError c.config, result:
      if n[namePos].kind != nkEmpty:
        addTempDecl(c, n[0], skProc)
      openScope(c)
      n[genericParamsPos] = checkError semGenericStmt(c, n[genericParamsPos],
                                                      flags, ctx)
      if n[paramsPos].kind != nkEmpty:
        if n[paramsPos][0].kind != nkEmpty:
          addPrelimDecl(c, newSym(skUnknown, getIdent(c.cache, "result"), nextSymId c.idgen, nil, n.info))
        n[paramsPos] = checkError semGenericStmt(c, n[paramsPos], flags, ctx)
      n[pragmasPos] = checkError semGenericStmt(c, n[pragmasPos], flags, ctx)
      var body: PNode
      if n[namePos].kind == nkSym:
        let s = n[namePos].sym
        if sfGenSym in s.flags and s.ast == nil:
          body = n[bodyPos]
        else:
          body = getBody(c.graph, s)
      else: body = n[bodyPos]
      n[bodyPos] = checkError semGenericStmtScope(c, body, flags, ctx)
      closeScope(c)
  of nkPragma, nkPragmaExpr: discard
  of nkExprColonExpr, nkExprEqExpr:
    checkMinSonsLen(n, 2, c.config)
    result[1] = semGenericStmt(c, n[1], flags, ctx)
    if result[1].isError:
      result = c.config.wrapError(result)
  else:
    captureError c.config, result:
      for i in 0..<n.len:
        result[i] = checkError semGenericStmt(c, n[i], flags, ctx)
  when defined(nimsuggest):
    if withinTypeDesc in flags: dec c.inTypeContext

proc semGenericStmt(c: PContext, n: PNode): PNode =
  var ctx: GenericCtx
  ctx.toMixin = initIntSet()
  ctx.toBind = initIntSet()
  result = semGenericStmt(c, n, {}, ctx)
  semIdeForTemplateOrGeneric(c, result, ctx.cursorInBody)

proc semConceptBody(c: PContext, n: PNode): PNode =
  var ctx: GenericCtx
  ctx.toMixin = initIntSet()
  ctx.toBind = initIntSet()
  result = semGenericStmt(c, n, {withinConcept}, ctx)
  semIdeForTemplateOrGeneric(c, result, ctx.cursorInBody)
