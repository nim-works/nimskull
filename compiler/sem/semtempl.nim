#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from sem.nim

## The current implementation of templates might not yet conform to the
## description that follows.
## 
## Template Basics
## ===============
##
## Given a basic template as follows:
## ..code::
##   template identity(arg: untyped): untyped =
##     arg
##
## The template's output type is `untyped`, meaning the body (`arg`) is treated
## as an `untyped` AST fragment, that substitution of parameters will evaluate
## per the rules of `untyped` templates, and finally evaluation and insertion
## of the template at the callsite will be hygeinic. The template parameter
## `arg` will be captured as `untyped`, meaning no attempt will be made to
## semantically analyse the parameter prior to substitution.
## 
## Template Taxonomy
## =================
## 
## There are at least four types of templates across two categories:
## - AST templates:
##   - `untyped`
##     - `dirty` a non-hygienic sub-variant
##   - `typed`
## - expression templates (all types that are not `untyped` or `typed`)
## 
## Substitution Positions
## ----------------------
## Templates are ultimately AST level constructs regardless of output type,
## they must be valid syntax. There are two types of positions in a template
## body, one is `definition` and the other is `usage`. A `definition` is any
## position where the grammar construct is intended to introduce a new symbol,
## i.e.: the name of a routine, including its parameters; names of variables
## (`const`, `let`, `var`), and so on. All other sites are `usage` sites, where
## a symbol referring to a "chunk" of AST might be used.
## 
## This is a draft of subsitution rules:
## - `untyped` template bodies accept `untyped` params in definition or usage
##   positions; and all other params are usage only
## - `typed` template bodies accept `untyped` params in definition or usage
##   positions; and all other params are usage only
## - non-ast template bodies only allow subsitutions within usage positions

discard """
  hygienic templates:

    template `||` (a, b: untyped): untyped =
      let aa = a
      if aa: aa else: b

    var
      a, b: T

    echo a || b || a

  Each evaluation context has to be different and we need to perform
  some form of preliminary symbol lookup in template definitions. Hygiene is
  a way to achieve lexical scoping at compile time.
"""

type
  TSymBinding = enum
    spNone, spGenSym, spInject

proc symBinding(n: PNode): TSymBinding =
  result = spNone
  for it in n:
    let key = if it.kind == nkExprColonExpr: it[0] else: it

    case key.kind
    of nkIdent:
      case whichKeyword(key.ident)
      of wGensym: return spGenSym
      of wInject: return spInject
      else: discard
    else:
      discard

type
  TSymChoiceRule = enum
    scClosed, scOpen, scForceOpen

proc symChoice(c: PContext, n: PNode, s: PSym, r: TSymChoiceRule;
               isField = false): PNode =
  var
    a: PSym
    o: TOverloadIter
  var i = 0
  a = initOverloadIter(o, c, n)
  while a != nil:
    if a.kind != skModule:
      inc(i)
      if i > 1: break
    elif a.isError:
      localReport(c.config, a.ast)
    a = nextOverloadIter(o, c, n)
  let info = getCallLineInfo(n)
  if i <= 1 and r != scForceOpen:
    # XXX this makes more sense but breaks bootstrapping for now:
    # (s.kind notin routineKinds or s.magic != mNone):
    # for instance 'nextTry' is both in tables.nim and astalgo.nim ...
    if not(isField and sfGenSym in s.flags):
      result = newSymNode(s, info)
      markUsed(c, info, s)
    else:
      result = n
  else:
    # semantic checking requires a type; `fitNode` deals with it
    # appropriately
    let kind = if r == scClosed or n.kind == nkDotExpr: nkClosedSymChoice
               else: nkOpenSymChoice
    result = newNodeIT(kind, info, newTypeS(tyNone, c))
    a = initOverloadIter(o, c, n)
    while a != nil:
      if a.kind != skModule and not(isField and sfGenSym in s.flags):
        incl(a.flags, sfUsed)
        markOwnerModuleAsUsed(c, a)
        result.add newSymNode(a, info)
      elif a.isError:
        localReport(c.config, a.ast)
      a = nextOverloadIter(o, c, n)

proc semBindStmt(c: PContext, n: PNode, toBind: var IntSet): PNode =
  ## Analyses a bind statement `n` in a template body and procduces a checked
  ## node, otherwise an `nkErrror` on failure. Additionally, tracks idents
  ## `toBind`.
  result = copyNode(n)
  var hasError = false
  for a in n.items:
    # If `a` is an overloaded symbol, we used to use the first symbol
    # as a 'witness' and use the fact that subsequent lookups will yield
    # the same symbol!
    # This is however not true anymore for hygienic templates as semantic
    # processing for them changes the symbol table...
    let s = qualifiedLookUp(c, a, {checkUndeclared})
    
    if s.isNil:
      result.add c.config.newError(a, PAstDiag(kind: adSemIllformedAst))
      hasError = true
    elif s.isError:
      result.add s.ast
      hasError = true
    else:
      # we need to mark all symbols:
      let sc = symChoice(c, n, s, scClosed)

      if sc.kind == nkSym:
        toBind.incl(sc.sym.id)
        result.add sc
      else:
        for x in items(sc):
          toBind.incl(x.sym.id)
          result.add x
  
  if hasError and result.kind != nkError:
    result = c.config.wrapError(result)

proc semMixinStmt(c: PContext, n: PNode, toMixin: var IntSet): PNode =
  ## Analyses a mixin statment in a template body and produces a checked node,
  ## otherwise an `nkError` on failure. Additionally, tracks idents `toMixin`.
  result = copyNode(n)
  
  var
    count = 0
    hasError = false

  for it in n.items:
    let (ident, err) = considerQuotedIdent(c, it)
  
    if err.isNil:
      toMixin.incl(ident.id)
      let x = symChoice(c, it, nil, scForceOpen)
      inc count, x.len
      result.add x
    else:
      result.add err
      hasError = true
  
  if hasError:
    result = c.config.wrapError(result)
  else:
    if count == 0:
      result = newNodeI(nkEmpty, n.info)

proc replaceIdentBySym(c: PContext; n: var PNode, s: PNode) =
  ## Replaces the symbol node in `n` (postfix, pragmaExpr, ident, accQuoted, or
  ## sym) with node `s` via in-place mutation, otherwise mutates `n` into an
  ## `nkError` node.
  case n.kind
  of nkPostfix: replaceIdentBySym(c, n[1], s)
  of nkPragmaExpr: replaceIdentBySym(c, n[0], s)
  of nkIdent, nkAccQuoted, nkSym: n = s
  else:
    n = newError(c.config, n):
      PAstDiag(kind: adSemIllformedAstExpectedPragmaOrIdent)

type
  TemplCtx = object
    ## Context used during template definition evaluation
    c: PContext
    toBind, toMixin, toInject: IntSet
    owner: PSym
    cursorInBody: bool # only for nimsuggest
    scopeN: int
    inTemplateHeader: int

proc getIdentNode(c: var TemplCtx, n: PNode): PNode =
  ## gets the ident node, will mutate `n` if it's an `nkPostfix` or
  ## `nkPragmaExpr` and there is an error (return an nkError).
  case n.kind
  of nkPostfix:
    result = getIdentNode(c, n[1])
    if result.isError:
      n[1] = result
      result = c.c.config.wrapError(n)
  of nkPragmaExpr:
    result = getIdentNode(c, n[0])
    if result.isError:
      n[0] = result
      result = c.c.config.wrapError(n)
  of nkIdent:
    let s = qualifiedLookUp(c.c, n, {})
    if s.isNil:
      result = n
    elif s.isError:
      result = s.ast
    elif s.owner == c.owner and s.kind == skParam:
      result = newSymNode(s, n.info)
    else:
      result = n
  of nkAccQuoted, nkSym, nkError:
    result = n
  else:
    result = newError(c.c.config, n,
                PAstDiag(
                  kind: adSemIllformedAstExpectedOneOf,
                  expectedKinds: {nkPostfix, nkPragmaExpr, nkIdent,
                                  nkAccQuoted}))

func isTemplParam(c: TemplCtx, s: PSym): bool {.inline.} =
  ## True if `s` is a parameter symbol of the current template.
  s.kind == skParam and s.owner == c.owner and sfTemplateParam in s.flags

func isTemplParam(c: TemplCtx, n: PNode): bool {.inline.} =
  ## True if `n` is a parameter symbol of the current template.
  n.kind == nkSym and isTemplParam(c, n.sym)

func definitionTemplParam(c: TemplCtx, n: PNode): bool {.inline.} =
  ## True if `n` is an `untyped` parameter symbol of the current template.
  isTemplParam(c, n) and n.sym.typ.kind in {tyUntyped}

proc semTemplBody(c: var TemplCtx, n: PNode): PNode

proc openScope(c: var TemplCtx) =
  openScope(c.c)

proc closeScope(c: var TemplCtx) =
  closeScope(c.c)

proc semTemplBodyScope(c: var TemplCtx, n: PNode): PNode =
  ## Same as `semTemplBody` except wrapped in a scope.
  openScope(c)
  result = semTemplBody(c, n)
  closeScope(c)

proc onlyReplaceParams(c: var TemplCtx, n: PNode): PNode =
  ## Produces a node with `n`'s parameter symbols replaced based on the current
  ## template `c`ontext, otherwise an `nkError`.
  result = n
  case n.kind
  of nkIdent:
    let s = qualifiedLookUp(c.c, n, {})
    if s.isNil:
      discard    # simply return n?
    elif s.isError:
      result = s.ast
    else:
      if s.owner == c.owner and s.kind == skParam:
        incl(s.flags, sfUsed)
        result = newSymNode(s, n.info)
  of nkError:
    result = n
  else:
    var hasError = false
    for i in 0..<n.safeLen:
      result[i] = onlyReplaceParams(c, n[i])

      if result[i].isError:
        hasError = true
    
    if hasError:
      result = c.c.config.wrapError(result)

proc newGenSym(kind: TSymKind, n: PNode, c: var TemplCtx): PSym =
  let (ident, err) = considerQuotedIdent(c.c, n)
  if err.isNil:
    result = newSym(kind, ident, nextSymId c.c.idgen, c.owner, n.info)
    result.flags.incl {sfGenSym, sfShadowed}
  else:
    result = newSym(skError, ident, nextSymId c.c.idgen, c.owner, n.info)
    result.typ = c.c.errorType
    result.ast = err

proc addLocalDecl(c: var TemplCtx, n: var PNode, k: TSymKind) =
  ## Adds local declarations to a template body `n` via in-place mutation, note
  ## upon error `n` is replaced with an `nkError` node.
  # locals default to 'gensym':
  if n.kind == nkPragmaExpr and symBinding(n[1]) == spInject:
    # even if injected, don't produce a sym choice here:
    var x = n[0]
    while true:
      case x.kind
      of nkPostfix: x = x[1]
      of nkPragmaExpr: x = x[0]
      of nkIdent: break
      of nkAccQuoted:
        # consider:  type `T TemplParam` {.inject.}
        # it suffices to return to treat it like 'inject':
        n = onlyReplaceParams(c, n)
        return
      of nkError: break
      else:
        x = newError(c.c.config, x,
                     PAstDiag(kind: adSemIllformedAstExpectedOneOf,
                              expectedKinds: {nkPostfix, nkPragmaExpr, nkIdent,
                                              nkAccQuoted}))

    let ident = getIdentNode(c, x)

    case ident.kind:
    of nkError:
      n = ident
    else:
      if definitionTemplParam(c, ident):
        replaceIdentBySym(c.c, n, ident)
      else:
        c.toInject.incl(x.ident.id)

  else:
    var hasError = false

    if (n.kind == nkPragmaExpr and n.len >= 2 and n[1].kind == nkPragma):

      let pragmaNode = n[1]
      for i in 0..<pragmaNode.len:
        let ni = pragmaNode[i]

        # see D20210801T100514
        var found = false
        if ni.kind == nkIdent:
          for a in templatePragmas:
            if ni.ident == getIdent(c.c.cache, $a):
              found = true
              break

        if not found:
          openScope(c)
          pragmaNode[i] = semTemplBody(c, pragmaNode[i])
          closeScope(c)

          if pragmaNode[i].isError:
            hasError = true

    let ident = getIdentNode(c, n)

    case ident.kind:
    of nkError:
      n = ident
    else:
      if definitionTemplParam(c, ident):
        replaceIdentBySym(c.c, n, ident)
      else:
        if n.kind != nkSym:
          let local = newGenSym(k, ident, c)

          addPrelimDecl(c.c, local)
          styleCheckDef(c.c.config, n.info, local)
          replaceIdentBySym(c.c, n):
            if local.isError:
              hasError = true
              local.ast
            else:
              newSymNode(local, n.info)

          if k == skParam and c.inTemplateHeader > 0:
            local.flags.incl sfTemplateParam

    if hasError and n.kind != nkError:
      n = c.c.config.wrapError(n)


proc semTemplSymbol(c: PContext, n: PNode, s: PSym; isField: bool): PNode =
  ## Analyses a symbol occurring in a template body and produces a checked node
  ## or an `nkError`, the `isField` parameter tailors analysis for fields of
  ## genSyms.
  incl(s.flags, sfUsed)
  # bug #12885; ideally sem'checking is performed again afterwards marking
  # the symbol as used properly, but the nfSem mechanism currently prevents
  # that from happening, so we mark the module as used here already:
  markOwnerModuleAsUsed(c, s)
  case s.kind
  of skUnknown:
    result =
      if s.isError:
        s.ast       # error symbols have an nkError ast
      else:
        n           # Introduced in this pass! Leave it as an identifier.
  of OverloadableSyms-{skEnumField}:
    result = symChoice(c, n, s, scOpen, isField)
  of skType, skGenericParam:
    result = newSymNodeTypeDesc(s, c.idgen, n.info)
  of skParam:
    result = n
  else:
    if s.kind == skEnumField and overloadableEnums in c.features:
      result = symChoice(c, n, s, scOpen, isField)
    else:
      result = newSymNode(s, n.info)
    # Issue #12832
    when defined(nimsuggest):
      suggestSym(c.graph, n.info, s, c.graph.usageSym, false)
    # field access (dot expr) will be handled by builtinFieldAccess
    if not isField and {optStyleHint, optStyleError} * c.config.globalOptions != {}:
      styleCheckUse(c.config, n.info, s)

proc templBindSym(c: TemplCtx, s: PSym, n: PNode, isField: bool): PNode =
  if contains(c.toBind, s.id):
    result = symChoice(c.c, n, s, scClosed, isField)
  elif contains(c.toMixin, s.name.id):
    result = symChoice(c.c, n, s, scForceOpen, isField)
  elif s.owner == c.owner and sfGenSym in s.flags:
    # XXX: this is a tremendous hack. Symbol choices cannot contain
    #      template-introduced gensyms, since gensym'ed symbols are turned
    #      into identifiers during template evaluation. To at least support
    #      basic usage of gensyms, they're bound directly. As a consequence,
    #      overloads part of the definition scope won't be considered
    incl(s.flags, sfUsed)
    result = newSymNode(s, n.info)
  else:
    result = semTemplSymbol(c.c, n, s, isField)

proc semRoutineInTemplName(c: var TemplCtx, n: PNode): PNode =
  ## Analyses the `namePos` in a routine-like occurring in a template body,
  ## producing a checked name node or an `nkError`.
  result = n
  var hasError = false
  case n.kind
  of nkIdent:
    let s = qualifiedLookUp(c.c, n, {})
    if s.isNil:
      discard
    elif s.isError:
      result = s.ast
    else:
      if s.owner == c.owner and (s.kind == skParam or sfGenSym in s.flags):
        incl(s.flags, sfUsed)
        result = newSymNode(s, n.info)
  of nkError:
    discard    # return the error
  else:
    for i in 0..<n.safeLen:
      result[i] = semRoutineInTemplName(c, n[i])

      if result[i].isError:
        hasError = true
  
  if hasError:
    result = c.c.config.wrapError(result)

proc semRoutineInTemplBody(c: var TemplCtx, n: PNode, k: TSymKind): PNode =
  ## Analyses a routine-like occurring in a template body producing a checked
  ## node or an `nkError`.
  result = n
  checkSonsLen(n, bodyPos + 1, c.c.config)
  
  var hasError = false
  
  # routines default to 'inject':
  if n.kind notin nkLambdaKinds and symBinding(n[pragmasPos]) == spGenSym:
    let ident = getIdentNode(c, n[namePos])

    if ident.isError:
      n[namePos] = ident
    elif definitionTemplParam(c, ident):
      n[namePos] = ident
    else:
      var s = newGenSym(k, ident, c)
      s.ast = n
      addPrelimDecl(c.c, s)
      styleCheckDef(c.c.config, n.info, s)
      n[namePos] = newSymNode(s, n[namePos].info)
  else:
    n[namePos] = semRoutineInTemplName(c, n[namePos])

  if n[namePos].isError:
    hasError = true

  # open scope for parameters
  openScope(c)
  for i in patternPos..paramsPos-1:
    n[i] = semTemplBody(c, n[i])

    if n[i].isError:
      hasError = true

  # xxx: special handling for templates within `untyped` output templates
  #      doesn't make sense, it's just untyped AST. For `typed` or `expression`
  #      templates they should be analysed.
  if k == skTemplate: inc(c.inTemplateHeader)
  n[paramsPos] = semTemplBody(c, n[paramsPos])
  if k == skTemplate: dec(c.inTemplateHeader)
  
  if n[paramsPos].isError:
    hasError = true

  for i in paramsPos+1..miscPos:
    n[i] = semTemplBody(c, n[i])

    if n[i].isError:
      hasError = true
  
  # open scope for locals
  inc c.scopeN
  openScope(c)
  n[bodyPos] = semTemplBody(c, n[bodyPos])
  if n[bodyPos].isError:
    hasError = true

  # close scope for locals
  closeScope(c)
  dec c.scopeN
  # close scope for parameters
  closeScope(c)

  if hasError:
    result = c.c.config.wrapError(result)

proc semTemplSomeDecl(c: var TemplCtx, n: PNode, symKind: TSymKind,
                      start = 0): PNode =
  ## Analyses a const/let/var section occuring in a template body, producing a
  ## checked section or an `nkError`. A `start` parameter can be provided to
  ## allow for incremental processing of such sections.
  assert n.kind in {nkConstSection, nkFormalParams, nkLetSection, nkVarSection}

  result = n

  var hasError = false

  for i in start..<n.len:
    var a = n[i]
    case a.kind:
    of nkCommentStmt: continue
    of nkError:
      hasError = true
    of nkIdentDefs, nkVarTuple, nkConstDef:
      checkMinSonsLen(a, 3, c.c.config)
      
      when defined(nimsuggest):
        inc c.c.inTypeContext
      
      a[^2] = semTemplBody(c, a[^2])

      if a[^2].isError:
        hasError = true
      
      when defined(nimsuggest):
        dec c.c.inTypeContext
      
      a[^1] = semTemplBody(c, a[^1])

      if a[^1].isError:
        hasError = true
      
      for j in 0..<a.len-2:
        addLocalDecl(c, a[j], symKind)

        if a[j].isError:
          hasError = true
    else:
      n[i] = newError(c.c.config, a,
                      PAstDiag(kind: adSemIllformedAstExpectedOneOf,
                               expectedKinds: {nkCommentStmt, nkIdentDefs,
                                               nkVarTuple, nkConstDef}))

  if hasError:
    result = c.c.config.wrapError(result)

proc semPattern(c: PContext, n: PNode): PNode

proc semTemplBodySons(c: var TemplCtx, n: PNode): PNode =
  ## Analyses child nodes of `n` with the production being `n` updated in-place
  ## otherwise an `nkError` wrapping `n` for non-dirty and non-immediate
  ## templates.
  var hasError = false

  result = n
  case n.kind
  of nkError:
    discard   # return the error in result
  of nkWithoutSons - nkError:
    unreachable("compiler bug, got kind: " & $n.kind)
  of nkWithSons:
    for i in 0..<n.len:
      result[i] = semTemplBody(c, n[i])

      if result[i].isError:
        hasError = true
  
  if hasError:
    result = c.c.config.wrapError(result)

proc semTemplBody(c: var TemplCtx, n: PNode): PNode =
  ## Analyses a template body `n` for a template producing a semantically
  ## checked body otherwise and `nkError`. This is for non-dirty and
  ## non-immediate template bodies.
  var hasError = false

  result = n
  semIdeForTemplateOrGenericCheck(c.c.config, n, c.cursorInBody)
  case n.kind
  of nkIdent:
    if n.ident.id in c.toInject: return n
    let s = qualifiedLookUp(c.c, n, {})
    if s.isNil:
      discard     # result is already set to n
    elif s.isError:
      result = s.ast
    elif isTemplParam(c, s):
      incl(s.flags, sfUsed)
      result = newSymNode(s, n.info)
    else:
      result = templBindSym(c, s, n, isField=false)

  of nkBind:
    result = semTemplBody(c, n[0])
  of nkBindStmt:
    result = semBindStmt(c.c, n, c.toBind)
  of nkMixinStmt:
    if c.scopeN > 0: result = semTemplBodySons(c, n)
    else: result = semMixinStmt(c.c, n, c.toMixin)
  of nkWithoutSons - nkIdent:
    discard
  of nkIfStmt:
    for i in 0..<n.len:
      var it = n[i]
      if it.len == 2:
        openScope(c)
        it[0] = semTemplBody(c, it[0])
        it[1] = semTemplBody(c, it[1])
        closeScope(c)
        if nkError in {it[0].kind, it[1].kind}:
          hasError = true
      else:
        n[i] = semTemplBodyScope(c, it)
        if n[i].isError:
          hasError = true
  of nkWhileStmt:
    openScope(c)
    for i in 0..<n.len:
      n[i] = semTemplBody(c, n[i])
      if n[i].isError:
          hasError = true
    closeScope(c)
  of nkCaseStmt:
    openScope(c)
    n[0] = semTemplBody(c, n[0])
    
    if n[0].isError:
      hasError = true
    
    for i in 1..<n.len:
      var a = n[i]
      checkMinSonsLen(a, 1, c.c.config)
      
      for j in 0..<a.len-1:
        a[j] = semTemplBody(c, a[j])

        if a[j].isError:
          hasError = true
      
      a[^1] = semTemplBodyScope(c, a[^1])
      
      if a[^1].isError:
        hasError = true
    closeScope(c)
  of nkForStmt:
    openScope(c)
    n[^2] = semTemplBody(c, n[^2])
    if n[^2].isError:
      hasError = true

    for i in 0..<n.len - 2:
      if n[i].kind == nkVarTuple:
        for j in 0..<n[i].len-1:
          addLocalDecl(c, n[i][j], skForVar)

          if n[i][j].isError:
            hasError = true
      else:
        addLocalDecl(c, n[i], skForVar)

        if n[i].isError:
          hasError = true
    
    openScope(c)
    n[^1] = semTemplBody(c, n[^1])
    closeScope(c)

    if n[^1].isError:
      hasError = true

    closeScope(c)
  of nkBlockStmt, nkBlockExpr:
    checkSonsLen(n, 2, c.c.config)
    openScope(c)
    case n[0].kind
    of nkError:
      hasError = true
    of nkEmpty:
      discard
    else:
      addLocalDecl(c, n[0], skLabel)

      if n[0].isError:
        hasError = true
      
      when false:
        # labels are always 'gensym'ed:
        let s = newGenSym(skLabel, n[0], c)
        addPrelimDecl(c.c, s)
        styleCheckDef(c.c.config, s)
        n[0] = newSymNode(s, n[0].info)
    
    n[1] = semTemplBody(c, n[1])

    if n[1].isError:
      hasError = true

    closeScope(c)
  of nkTryStmt, nkHiddenTryStmt:
    checkMinSonsLen(n, 2, c.c.config)
    n[0] = semTemplBodyScope(c, n[0])

    if n[0].isError:
      hasError = true

    for i in 1..<n.len:
      var a = n[i]
      checkMinSonsLen(a, 1, c.c.config)
      openScope(c)

      for j in 0..<a.len-1:
        if a[j].isInfixAs():
          addLocalDecl(c, a[j][2], skLet)

          if a[j][2].isError:
            hasError = true

          a[j][1] = semTemplBody(c, a[j][1])

          if a[j][1].isError:
            hasError = true
        else:
          a[j] = semTemplBody(c, a[j])

          if a[j].isError:
            hasError = true

      a[^1] = semTemplBodyScope(c, a[^1])

      if a[^1].isError:
        hasError = true

      closeScope(c)
  of nkConstSection: result = semTemplSomeDecl(c, n, skConst)
  of nkLetSection:   result = semTemplSomeDecl(c, n, skLet)
  of nkVarSection:   result = semTemplSomeDecl(c, n, skVar)
  of nkFormalParams:
    checkMinSonsLen(n, 1, c.c.config)
    result = semTemplSomeDecl(c, n, skParam, 1)
    n[0] = semTemplBody(c, n[0])
    if n[0].isError and result.kind != nkError:
      result = c.c.config.wrapError(result)
  of nkTypeSection:
    for i in 0..<n.len:
      var a = n[i]
      if a.kind == nkCommentStmt:
        continue

      if (a.kind != nkTypeDef):
        semReportIllformedAst(c.c.config, a, {nkTypeDef})

      checkSonsLen(a, 3, c.c.config)
      addLocalDecl(c, a[0], skType)

      if a[0].isError:
        hasError = true

    for i in 0..<n.len:
      var a = n[i]

      if a.kind == nkCommentStmt:
        continue
      
      if (a.kind != nkTypeDef):
        semReportIllformedAst(c.c.config, a, {nkTypeDef})

      checkSonsLen(a, 3, c.c.config)
      if a[1].kind != nkEmpty:
        openScope(c)
        a[1] = semTemplBody(c, a[1])
        a[2] = semTemplBody(c, a[2])
        closeScope(c)
      else:
        a[2] = semTemplBody(c, a[2])

      if nkError in {a[1].kind, a[2].kind}:
        hasError = true
  of nkProcDef, nkLambdaKinds:
    result = semRoutineInTemplBody(c, n, skProc)
  of nkFuncDef:
    result = semRoutineInTemplBody(c, n, skFunc)
  of nkMethodDef:
    result = semRoutineInTemplBody(c, n, skMethod)
  of nkIteratorDef:
    result = semRoutineInTemplBody(c, n, skIterator)
  of nkTemplateDef:
    result = semRoutineInTemplBody(c, n, skTemplate)
  of nkMacroDef:
    result = semRoutineInTemplBody(c, n, skMacro)
  of nkConverterDef:
    result = semRoutineInTemplBody(c, n, skConverter)
  of nkPragmaExpr:
    result[0] = semTemplBody(c, n[0])

    if result[0].isError:
      hasError = true
  of nkPostfix:
    result[1] = semTemplBody(c, n[1])

    if result[1].isError:
      hasError = true
  of nkPragma:
    for x in n:
      case x.kind
      of nkExprColonExpr:
        x[1] = semTemplBody(c, x[1])

        if x[1].isError:
          hasError = true
      of nkError:
        hasError = true
      else:
        discard
  of nkBracketExpr:
    # xxx: screwing up `nkBracketExpr` nodes like this does no one any favours.
    #      instead, just pass them on and figure out what to do _later_ when
    #      there is more context to make a decision. Instead, `semExpr` now has
    #      to crudely recreate this information.
    result = newNodeI(nkCall, n.info)
    result.add newIdentNode(getIdent(c.c.cache, "[]"), n.info)
    for i in 0..<n.len: result.add(n[i])
    result = semTemplBodySons(c, result)
  of nkCurlyExpr:
    result = newNodeI(nkCall, n.info)
    result.add newIdentNode(getIdent(c.c.cache, "{}"), n.info)
    for i in 0..<n.len: result.add(n[i])
    result = semTemplBodySons(c, result)
  of nkAsgn, nkFastAsgn:
    checkSonsLen(n, 2, c.c.config)
    let a = n[0]
    let b = n[1]

    let k = a.kind
    case k
    of nkBracketExpr:
      result = newNodeI(nkCall, n.info)
      result.add newIdentNode(getIdent(c.c.cache, "[]="), n.info)
      for i in 0..<a.len: result.add(a[i])
      result.add(b)
      result = semTemplBodySons(c, result)
    of nkCurlyExpr:
      result = newNodeI(nkCall, n.info)
      result.add newIdentNode(getIdent(c.c.cache, "{}="), n.info)
      for i in 0..<a.len: result.add(a[i])
      result.add(b)
      result = semTemplBodySons(c, result)
    else:
      result = semTemplBodySons(c, n)
  of nkCallKinds-{nkPostfix}:
    # do not transform runnableExamples (bug #9143)
    if not isRunnableExamples(n[0]):
      result = semTemplBodySons(c, n)
  of nkAccQuoted:
    if n.len == 1:
      # quoted identifier, resolve and unquote it
      result = semTemplBody(c, n[0])
    else:
      # identifier construction
      result = semTemplBodySons(c, n)
  of nkDotExpr:
    # dotExpr is ambiguous: note that we explicitly allow 'x.TemplateParam',
    # so we use the generic code for nkDotExpr too
    let s = qualifiedLookUp(c.c, n, {})

    if s.isNil:
      # a normal dot expression
      result[0] = semTemplBody(c, n[0])
      var
        iter: TOverloadIter
        s = initOverloadIter(iter, c.c, n[1])

      block resolve:
        # only routines and types are eligible for the right-hand side, look
        # for such a symbol:
        while s != nil:
          if s.isError:
            localReport(c.c.config, s.ast)
          elif isTemplParam(c, s):
            # template parameters are bound eagerly
            incl(s.flags, sfUsed)
            result[1] = newSymNode(s, n[1].info)
            break resolve
          elif s.kind in routineKinds + {skType, skGenericParam}:
            break # found a symbol that fits
          s = nextOverloadIter(iter, c.c, n[1])

        if s != nil:
          var field = templBindSym(c, s, n[1], isField=true)
          if field.kind == nkSym and sfGenSym notin field.sym.flags and
             field.sym.kind in OverloadableSyms:
            # ``semexprs.dotTransformation`` ignores single symbols, so we
            # need to wrap the symbol in a sym-choice, to preserve the bound
            # symbol
            field = newTreeIT(nkOpenSymChoice, n[1].info,
                              newTypeS(tyNone, c.c), field)
            # XXX: should be a closed symbol choice, like it works for
            #      generics, but code relies on the symbol being open...
            # XXX: ``dotTransformation`` should not ignore symbols in the
            #      first place

          result[1] = field

      hasError = nkError in {result[0].kind, result[1].kind}:
    elif s.isError:
      result = s.ast
    else:
      if contains(c.toBind, s.id):
        result = symChoice(c.c, n, s, scClosed)
      elif contains(c.toMixin, s.name.id):
        result = symChoice(c.c, n, s, scForceOpen)
      else:
        # FIXME: ``semTemplSymbol`` needs to be used here to ensure correct
        #        typing for type symbols
        result = symChoice(c.c, n, s, scOpen)

  of nkExprColonExpr, nkExprEqExpr:
    let s = qualifiedLookUp(c.c, n[0], {})
    # template parameters can be substituted into the name position of a
    # ``a: b`` or ``a = b`` construct
    if s != nil and isTemplParam(c, s):
      result[0] = newSymNode(s, n[0].info)
    elif n[0].kind == nkAccQuoted and n[0].len > 1:
      # make sure to also process identifier constructions
      result[0] = semTemplBody(c, n[1])

    result[1] = semTemplBody(c, n[1])
    hasError = nkError in {result[0].kind, result[1].kind}
  of nkTableConstr:
    # also transform the keys (bug #12595)
    for i in 0..<n.len:
      result[i] = semTemplBodySons(c, n[i])

      if result[i].isError:
        hasError = true
  else:
    result = semTemplBodySons(c, n)
  
  if hasError:
    result = c.c.config.wrapError(result)

proc semTemplBodyDirty(c: var TemplCtx, n: PNode): PNode

proc semTemplBodyDirtyKids(c: var TemplCtx, n: PNode): PNode =
  ## Analyses child nodes of `n` with the production being `n` updated in-place
  ## otherwise an `nkError` wrapping `n` for dirty templates.
  var hasError = false
  
  result = n
  case n.kind
  of nkError:
    discard    # result is already assigned n
  of nkWithoutSons - nkError:
    unreachable("compiler bug, got kind: " & $n.kind)
  of nkWithSons:
    for i in 0..<n.len:
      result[i] = semTemplBodyDirty(c, n[i])
      
      if result[i].isError:
        hasError = true
  
  if hasError:
    result = c.c.config.wrapError(result)

proc semTemplBodyDirty(c: var TemplCtx, n: PNode): PNode =
  ## Analyses a template body `n` for a `dirty` template producing a
  ## semantically checked body otherwise and `nkError`.
  result = n

  semIdeForTemplateOrGenericCheck(c.c.config, n, c.cursorInBody)

  case n.kind
  of nkIdent:
    let s = qualifiedLookUp(c.c, n, {})

    if s.isNil:
      discard
    elif s.isError:
      result = s.ast
    else:
      if s.owner == c.owner and s.kind == skParam:
        result = newSymNode(s, n.info)
      elif contains(c.toBind, s.id):
        result = symChoice(c.c, n, s, scClosed)
  of nkBind:
    result = semTemplBodyDirty(c, n[0])
  of nkBindStmt:
    result = semBindStmt(c.c, n, c.toBind)
  of nkWithoutSons - nkIdent:
    discard
  of nkDotExpr, nkAccQuoted:
    # dotExpr is ambiguous: note that we explicitly allow 'x.TemplateParam',
    # so we use the generic code for nkDotExpr too
    let s = qualifiedLookUp(c.c, n, {})

    if s.isNil:
      result = semTemplBodyDirtyKids(c, n)
    elif s.isError:
      result = s.ast
    elif contains(c.toBind, s.id):
      result = symChoice(c.c, n, s, scClosed)
    else:
      result = semTemplBodyDirtyKids(c, n)
  else:
    result = semTemplBodyDirtyKids(c, n)

proc semRoutineParams(c: PContext, routine, formal, generic: PNode, kind: TSymKind): PType
# from semstmts

proc semTemplateDef(c: PContext, n: PNode): PNode =
  ## Analyse `n` a template definition producing a callable template.
  addInNimDebugUtils(c.config, "semTemplateDef", n, result)

  assert n.kind == nkTemplateDef, "template def expected, got: " & $n.kind

  # setup node for production
  result = copyNode(n)      # not all flags copied, let's see how that works
  result.sons.newSeq(n.len) # make space for the kids

  result[namePos] = n[namePos]
  var hasError = result[namePos].kind == nkError

  let s = result[namePos].getDefNameSymOrRecover()

  assert s.kind == skTemplate

  if s.owner != nil:
    const names = ["!=", ">=", ">", "incl", "excl", "in", "notin", "isnot"]
    if sfSystemModule in s.owner.flags and s.name.s in names or
       s.owner.name.s == "vm" and s.name.s == "stackTrace":
      incl(s.flags, sfCallsite)

  # check parameter list:
  pushOwner(c, s)
  openScope(c)

  result[pragmasPos] = n[pragmasPos]

  if n[pragmasPos].kind != nkEmpty:
    result[pragmasPos] = pragmaDeclNoImplicit(c, s, n[pragmasPos], templatePragmas)

  result[pragmasPos] = implicitPragmas(c, s, result[pragmasPos], templatePragmas)

  if result[pragmasPos].kind == nkError:
    hasError = true

  s.typ = semRoutineParams(c, result, n[paramsPos], n[genericParamsPos], skTemplate)

  # process parameters:
  var allUntyped = true
  if s.typ != nil:
    # a template's parameters are not gensym'ed even if that was originally the
    # case as we determine whether it's a template parameter in the template
    # body by the absence of the sfGenSym flag:
    for i in 1..<s.typ.n.len:
      let param = s.typ.n[i].sym
      param.flags.incl sfTemplateParam
      param.flags.excl sfGenSym
      if param.typ.kind != tyUntyped:
        allUntyped = false
  else:
    # an empty parameter list was provided, default to returning ``typed``
    s.typ = newTypeS(tyProc, c)
    # XXX why do we need tyTyped as a return type again?
    s.typ.n = newNodeI(nkFormalParams, n.info)
    rawAddSon(s.typ, newTypeS(tyTyped, c))
    s.typ.n.add newNodeIT(nkType, n.info, s.typ[0])

  if allUntyped:
    incl(s.flags, sfAllUntyped)
  
  result[patternPos] = semPattern(c, n[patternPos])
  if result[patternPos].isError:
    hasError = true

  var ctx = TemplCtx(c: c, toBind: initIntSet(), toMixin: initIntSet(),
                     toInject: initIntSet(), owner: s)
  result[bodyPos] =
    if sfDirty in s.flags:
      semTemplBodyDirty(ctx, n[bodyPos])
    else:
      semTemplBody(ctx, n[bodyPos])
  
  # only parameters are resolve, no type checking is performed
  semIdeForTemplateOrGeneric(c, result[bodyPos], ctx.cursorInBody)
  closeScope(c)
  popOwner(c)

  # set the symbol AST after pragmas, at least. This stops pragma that have
  # been pushed (implicit) to be explicitly added to the template definition
  # and misapplied to the body.
  s.ast = result

  case result[bodyPos].kind
  of nkError:
    hasError = true
  of nkEmpty:
    if sfCustomPragma notin s.flags:
      result[bodyPos] = newError(c.config, result[bodyPos],
                                  PAstDiag(kind: adSemImplementationExpected,
                                           routineSym: s,
                                           routineDefStartPos: result.info))
      hasError = true
  else:
    if sfCustomPragma in s.flags:      
      result[bodyPos] = newError(c.config, result[bodyPos],
                                  PAstDiag(kind: adSemImplementationNotAllowed,
                                           symWithImpl: s))
      hasError = true

  let (proto, comesFromShadowScope) = searchForProc(c, c.currentScope, s)

  if proto.isNil:
    addInterfaceOverloadableSymAt(c, c.currentScope, s)
  elif not comesFromShadowScope:
    symTabReplace(c.currentScope.symbols, proto, s)
  
  case result[patternPos].kind
  of nkEmpty:
    discard # no pattern, nothing to do
  else:
    addPattern(c, LazySym(sym: s))

  if hasError:
    result = c.config.wrapErrorAndUpdate(result, s)

proc semPatternBody(c: var TemplCtx, n: PNode): PNode =
  ## Analyse `n` a term rewriting pattern body producing an instantiable
  ## template, otherwise an error.
  ##
  ## The body itself will be applied when the associated pattern is matched see
  ## `semPattern`.
  var hasError = false

  template templToExpand(s: untyped): untyped =
    s.kind == skTemplate and (s.typ.len == 1 or sfAllUntyped in s.flags)

  proc semPatternBodyKids(c: var TemplCtx, n: PNode): PNode {.inline.} =
    var hasError = false
  
    result = n
    case n.kind
    of nkError:
      discard    # result is already assigned n
    else:
      for i in 0..<n.len:
        result[i] = semPatternBody(c, n[i])
        
        if result[i].isError:
          hasError = true
    
    if hasError:
      result = c.c.config.wrapError(result)

  proc newParam(c: var TemplCtx, n: PNode, s: PSym): PNode =
    # the param added in the current scope is actually wrong here for
    # macros because they have a shadowed param of type 'PNimNode' (see
    # semtypes.addParamOrResult). Within the pattern we have to ensure
    # to use the param with the proper type though:
    incl(s.flags, sfUsed)
    let x = c.owner.typ.n[s.position+1].sym
    assert x.name == s.name
    result = newSymNode(x, n.info)

  proc expectParam(c: var TemplCtx, n: PNode): PNode =
    let s = qualifiedLookUp(c.c, n, {})

    if s != nil and s.owner == c.owner and s.kind == skParam:
      result = newParam(c, n, s)
    else:
      if s.isError:
        result = s.ast
      else:
        result = n

      result = c.c.config.newError(result, PAstDiag(kind: adSemInvalidExpression))

  result = n
  case n.kind
  of nkIdent:
    let s = qualifiedLookUp(c.c, n, {})
    result =
      if s.isNil:
        n
      elif s.isError:
        s.ast
      else:
        if s.owner == c.owner and s.kind == skParam:
          newParam(c, n, s)
        elif contains(c.toBind, s.id):
          symChoice(c.c, n, s, scClosed)
        elif templToExpand(s):
          semPatternBody(c, semTemplateExpr(c.c, n, s, {efNoSemCheck}))
        else:
          n
          # we keep the ident unbound for matching instantiated symbols and
          # more flexibility
  of nkBindStmt:
    result = semBindStmt(c.c, n, c.toBind)
  of nkWithoutSons - nkIdent:
    discard
  of nkCurlyExpr:
    # we support '(pattern){x}' to bind a subpattern to a parameter 'x';
    # '(pattern){|x}' does the same but the matches will be gathered in 'x'
    if n.len != 2:
      result = c.c.config.newError(n, PAstDiag(kind: adSemInvalidExpression))
    elif n[1].kind == nkIdent:
      n[0] = semPatternBody(c, n[0])
      n[1] = expectParam(c, n[1])

      if nkError in {n[0].kind, n[1].kind}:
        hasError = true
    elif n[1].kind == nkPrefix and n[1][0].kind == nkIdent:
      let opr = n[1][0]
      
      if opr.ident.s == "|":
        n[0] = semPatternBody(c, n[0])
        n[1][1] = expectParam(c, n[1][1])

        if nkError in {n[0].kind, n[1][1].kind}:
          hasError = true
      else:
        result = c.c.config.newError(n, PAstDiag(kind: adSemInvalidExpression))
    else:
      result = c.c.config.newError(n, PAstDiag(kind: adSemInvalidExpression))
  of nkStmtList, nkStmtListExpr:
    result =
      if stupidStmtListExpr(n):
        semPatternBody(c, n.lastSon)
      else:
        semPatternBodyKids(c, n)
  of nkCallKinds:
    let s = qualifiedLookUp(c.c, n[0], {})
    if s.isNil:
      discard  # n is already assigned
    elif s.isError:
      # XXX: move to propagating nkError, skError, and tyError
      result = s.ast
    else:
      if s.owner == c.owner and s.kind == skParam: discard
      elif contains(c.toBind, s.id): discard
      elif templToExpand(s):
        return semPatternBody(c, semTemplateExpr(c.c, n, s, {efNoSemCheck}))

    if n.kind == nkInfix and
      (let id = legacyConsiderQuotedIdent(c.c, n[0], nil); id != nil):
      # we interpret `*` and `|` only as pattern operators if they occur in
      # infix notation, so that '`*`(a, b)' can be used for verbatim matching:
      if id.s == "*" or id.s == "**":
        result = newTreeI(nkPattern, n.info):
          [newIdentNode(id, n.info),
           semPatternBody(c, n[1]),
           expectParam(c, n[2])]

        if nkError in {result[1].kind, result[2].kind}:
          result = c.c.config.wrapError(result)

        return
      elif id.s == "|":
        result = newTreeI(nkPattern, n.info):
          [newIdentNode(id, n.info),
           semPatternBody(c, n[1]),
           semPatternBody(c, n[2])]

        if nkError in {result[1].kind, result[2].kind}:
          result = c.c.config.wrapError(result)

        return

    if n.kind == nkPrefix and
      (let id = legacyConsiderQuotedIdent(c.c, n[0], nil); id != nil):
      if id.s == "~":
        result = newTreeI(nkPattern, n.info):
          [newIdentNode(id, n.info),
           semPatternBody(c, n[1])]

        if result[1].isError:
          result = c.c.config.wrapError(result)

        return

    result = semPatternBodyKids(c, n)
  of nkDotExpr, nkAccQuoted:
    # dotExpr is ambiguous: note that we explicitly allow 'x.TemplateParam',
    # so we use the generic code for nkDotExpr too
    let s = qualifiedLookUp(c.c, n, {})

    result =
      if s.isNil:
        semPatternBodyKids(c, n)
      elif s.isError:
        s.ast
      elif contains(c.toBind, s.id):
        symChoice(c.c, n, s, scClosed)
      else:
        newIdentNode(s.name, n.info)
  of nkPar:
    result =
      if n.len == 1:
        semPatternBody(c, n[0])
      else:
        semPatternBodyKids(c, n)
  else:
    result = semPatternBodyKids(c, n)

  if hasError and result.kind != nkError:
    result = c.c.config.wrapError(result)

proc semPattern(c: PContext, n: PNode): PNode =
  ## Analyses `n` and produces an analysed pattern or `nkError` upon failure.
  ##
  ## `n` should be the AST from a `patternPos` of a template definition. The
  ## analysed pattern is addd to the current `c`ontext for term matching during
  ## `hlo` (high level optimization) phase.
  
  case n.kind
  of nkError, nkEmpty: # treated as no-ops
    result = n
  else:
    openScope(c)
    
    var ctx = TemplCtx(
      toBind: initIntSet(),
      toMixin: initIntSet(),
      toInject: initIntSet(),
      c: c,
      owner: getCurrOwner(c)
    )
    
    result = flattenStmts(semPatternBody(ctx, n))
    
    if result.kind in {nkStmtList, nkStmtListExpr}:
      result =
        case result.len
        of 1:
          result[0] # unwrap
        of 0:
          c.config.newError(n, PAstDiag(kind: adSemExpectedNonemptyPattern))
        else:
          result    # leave as is
    closeScope(c)
