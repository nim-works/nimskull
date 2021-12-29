#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module implements lookup helpers.
import std/[algorithm, strutils]
import
  intsets, ast, astalgo, idents, semdata, types, msgs, options,
  renderer, nimfix/prettybase, lineinfos, modulegraphs, astmsgs,
  errorhandling, reports

proc ensureNoMissingOrUnusedSymbols(c: PContext; scope: PScope)

type
  PIdentResult* = tuple
    ident: PIdent      # found ident, otherwise `IdentCache.notFoundIdent`
    errNode: PNode     # if ident is notFoundIdent, node where error occurred
                       # use with original PNode for better error reporting

proc noidentError2(conf: ConfigRef; n, origin: PNode): PNode =
  ## generate an error node when no ident was found in `n`, with `origin` being
  ## the the expression within which `n` resides, if `origin` is the same then
  ## a simplified error is generated.
  assert n != nil, "`n` must be provided"

  let bad = if origin.isNil: n else: origin
  conf.newError(bad, SemReport(
    kind: rsemIdentExpected,
    expression: bad
  ))


proc considerQuotedIdent2*(c: PContext; n: PNode): PIdentResult =
  ## Retrieve a PIdent from a PNode, taking into account accent nodes.
  ##
  ## If none found, returns a `idents.IdentCache.identNotFound`
  let ic = c.cache

  result =
    case n.kind
    of nkIdent: (ident: n.ident, errNode: nil)
    of nkSym: (ident: n.sym.name, errNode: nil)
    of nkAccQuoted:
      case n.len
      of 0: (ident: ic.getNotFoundIdent(), errNode: n)
      of 1: considerQuotedIdent2(c, n[0])
      else:
        var
          id = ""
          error = false
        for i in 0..<n.len:
          let x = n[i]
          case x.kind
          of nkIdent: id.add(x.ident.s)
          of nkSym: id.add(x.sym.name.s)
          of nkLiterals - nkFloatLiterals: id.add(x.renderTree)
          else:
            error = true
            break
        if error:
          (ident: ic.getNotFoundIdent(), errNode: n)
        else:
          (ident: getIdent(c.cache, id), errNode: nil)
    of nkOpenSymChoice, nkClosedSymChoice:
      if n[0].kind == nkSym:
        (ident: n[0].sym.name, errNode: nil)
      else:
        (ident: ic.getNotFoundIdent(), errNode: n)
    else:
      (ident: ic.getNotFoundIdent(), errNode: n)

  # this handles the case where in an `nkAccQuoted` node we "dig"
  if result[1] != nil:
    result[1] = noidentError2(c.config, result[1], n)

proc noidentError(conf: ConfigRef; n, origin: PNode) =
  let bad = if origin.isNil: n else: origin
  conf.localError(n.info, SemReport(
    kind: rsemIdentExpected,
    expression: bad
  ))

proc considerQuotedIdent*(c: PContext; n: PNode, origin: PNode = nil): PIdent =
  ## Retrieve a PIdent from a PNode, taking into account accent nodes.
  ## ``origin`` can be nil. If it is not nil, it is used for a better
  ## error message.
  ##
  ## XXX: legacy, deprecate and replace with `considerQuotedIdent2`
  template handleError(n, origin: PNode) =
    noidentError(c.config, n, origin)
    result = getIdent(c.cache, "<Error>")

  case n.kind
  of nkIdent: result = n.ident
  of nkSym: result = n.sym.name
  of nkAccQuoted:
    case n.len
    of 0: handleError(n, origin)
    of 1: result = considerQuotedIdent(c, n[0], origin)
    else:
      var id = ""
      for i in 0..<n.len:
        let x = n[i]
        case x.kind
        of nkIdent: id.add(x.ident.s)
        of nkSym: id.add(x.sym.name.s)
        of nkLiterals - nkFloatLiterals: id.add(x.renderTree)
        else: handleError(n, origin)
      result = getIdent(c.cache, id)
  of nkOpenSymChoice, nkClosedSymChoice:
    if n[0].kind == nkSym:
      result = n[0].sym.name
    else:
      handleError(n, origin)
  else:
    handleError(n, origin)

template addSym*(scope: PScope, s: PSym) =
  strTableAdd(scope.symbols, s)

proc addUniqueSym*(scope: PScope, s: PSym): PSym =
  result = strTableInclReportConflict(scope.symbols, s)

proc openScope*(c: PContext): PScope {.discardable.} =
  result = PScope(parent: c.currentScope,
                  symbols: newStrTable(),
                  depthLevel: c.scopeDepth + 1)
  c.currentScope = result

proc rawCloseScope*(c: PContext) =
  c.currentScope = c.currentScope.parent

proc closeScope*(c: PContext) =
  ensureNoMissingOrUnusedSymbols(c, c.currentScope)
  rawCloseScope(c)

iterator allScopes*(scope: PScope): PScope =
  var current = scope
  while current != nil:
    yield current
    current = current.parent

iterator localScopesFrom*(c: PContext; scope: PScope): PScope =
  for s in allScopes(scope):
    if s == c.topLevelScope: break
    yield s

proc skipAlias*(s: PSym; n: PNode; conf: ConfigRef): PSym =
  if s == nil or s.kind != skAlias:
    result = s
  else:
    result = s.owner
    if conf.cmd == cmdNimfix:
      prettybase.replaceDeprecated(conf, n.info, s, result)
    else:
      conf.localError(n.info, SemReport(
        kind: rsemDeprecated, psym: s, alternative: result))

proc isShadowScope*(s: PScope): bool {.inline.} =
  s.parent != nil and s.parent.depthLevel == s.depthLevel

proc localSearchInScope*(c: PContext, s: PIdent): PSym =
  var scope = c.currentScope
  result = strTableGet(scope.symbols, s)
  while result == nil and scope.isShadowScope:
    # We are in a shadow scope, check in the parent too
    scope = scope.parent
    result = strTableGet(scope.symbols, s)

proc initIdentIter(ti: var ModuleIter; marked: var IntSet; im: ImportedModule; name: PIdent;
                   g: ModuleGraph): PSym =
  result = initModuleIter(ti, g, im.m, name)
  while result != nil:
    let b =
      case im.mode
      of importAll: true
      of importSet: result.id in im.imported
      of importExcept: name.id notin im.exceptSet
    if b and not containsOrIncl(marked, result.id):
      return result
    result = nextModuleIter(ti, g)

proc nextIdentIter(ti: var ModuleIter; marked: var IntSet; im: ImportedModule;
                   g: ModuleGraph): PSym =
  while true:
    result = nextModuleIter(ti, g)
    if result == nil: return nil
    case im.mode
    of importAll:
      if not containsOrIncl(marked, result.id):
        return result
    of importSet:
      if result.id in im.imported and not containsOrIncl(marked, result.id):
        return result
    of importExcept:
      if result.name.id notin im.exceptSet and not containsOrIncl(marked, result.id):
        return result

iterator symbols(im: ImportedModule; marked: var IntSet; name: PIdent; g: ModuleGraph): PSym =
  var ti: ModuleIter
  var candidate = initIdentIter(ti, marked, im, name, g)
  while candidate != nil:
    yield candidate
    candidate = nextIdentIter(ti, marked, im, g)

iterator importedItems*(c: PContext; name: PIdent): PSym =
  var marked = initIntSet()
  for im in c.imports.mitems:
    for s in symbols(im, marked, name, c.graph):
      yield s

proc allPureEnumFields(c: PContext; name: PIdent): seq[PSym] =
  var ti: TIdentIter
  result = @[]
  var res = initIdentIter(ti, c.pureEnumFields, name)
  while res != nil:
    result.add res
    res = nextIdentIter(ti, c.pureEnumFields)

iterator allSyms*(c: PContext): (PSym, int, bool) =
  # really iterate over all symbols in all the scopes. This is expensive
  # and only used by suggest.nim.
  var isLocal = true
  var scopeN = 0
  for scope in allScopes(c.currentScope):
    if scope == c.topLevelScope: isLocal = false
    dec scopeN
    for item in scope.symbols:
      yield (item, scopeN, isLocal)

  dec scopeN
  isLocal = false
  for im in c.imports.mitems:
    for s in modulegraphs.allSyms(c.graph, im.m):
      assert s != nil
      yield (s, scopeN, isLocal)

proc someSymFromImportTable*(c: PContext; name: PIdent; ambiguous: var bool): PSym =
  var marked = initIntSet()
  var symSet = OverloadableSyms
  if overloadableEnums notin c.features:
    symSet.excl skEnumField
  result = nil
  block outer:
    for im in c.imports.mitems:
      for s in symbols(im, marked, name, c.graph):
        if result == nil:
          result = s
        elif s.kind notin symSet or result.kind notin symSet:
          ambiguous = true
          break outer

proc searchInScopes*(c: PContext, s: PIdent; ambiguous: var bool): PSym =
  for scope in allScopes(c.currentScope):
    result = strTableGet(scope.symbols, s)
    if result != nil: return result
  result = someSymFromImportTable(c, s, ambiguous)

proc debugScopes*(c: PContext; limit=0, max = int.high) {.deprecated.} =
  var i = 0
  var count = 0
  for scope in allScopes(c.currentScope):
    echo "scope ", i
    for h in 0..high(scope.symbols.data):
      if scope.symbols.data[h] != nil:
        if count >= max: return
        echo count, ": ", scope.symbols.data[h].name.s
        count.inc
    if i == limit: return
    inc i

proc searchInScopesFilterBy*(c: PContext, s: PIdent, filter: TSymKinds): seq[PSym] =
  result = @[]
  block outer:
    for scope in allScopes(c.currentScope):
      var ti: TIdentIter
      var candidate = initIdentIter(ti, scope.symbols, s)
      while candidate != nil:
        if candidate.kind in filter:
          result.add candidate
          # Break here, because further symbols encountered would be shadowed
          break outer
        candidate = nextIdentIter(ti, scope.symbols)

  if result.len == 0:
    var marked = initIntSet()
    for im in c.imports.mitems:
      for s in symbols(im, marked, s, c.graph):
        if s.kind in filter:
          result.add s

proc errorSym*(c: PContext, n: PNode): PSym =
  ## creates an error symbol to avoid cascading errors (for IDE support)
  var m = n
  # ensure that 'considerQuotedIdent' can't fail:
  if m.kind == nkDotExpr: m = m[1]
  let ident = if m.kind in {nkIdent, nkSym, nkAccQuoted}:
      considerQuotedIdent(c, m)
    else:
      getIdent(c.cache, "err:" & renderTree(m))
  result = newSym(skError, ident, nextSymId(c.idgen), getCurrOwner(c), n.info, {})
  result.typ = errorType(c)
  incl(result.flags, sfDiscardable)
  # pretend it's from the top level scope to prevent cascading errors:
  if c.config.cmd != cmdInteractive and c.compilesContextId == 0:
    c.moduleScope.addSym(result)

type
  TOverloadIterMode* = enum
    oimDone, oimNoQualifier, oimSelfModule, oimOtherModule, oimSymChoice,
    oimSymChoiceLocalLookup
  TOverloadIter* = object
    it*: TIdentIter
    mit*: ModuleIter
    m*: PSym
    mode*: TOverloadIterMode
    symChoiceIndex*: int
    currentScope: PScope
    importIdx: int
    marked: IntSet

proc getSymRepr*(conf: ConfigRef; s: PSym, getDeclarationPath = true): string =
  case s.kind
  of routineKinds, skType:
    result = getProcHeader(conf, s, getDeclarationPath = getDeclarationPath)
  else:
    result = "'$1'" % s.name.s
    if getDeclarationPath:
      result.addDeclaredLoc(conf, s)

proc ensureNoMissingOrUnusedSymbols(c: PContext; scope: PScope) =
  # check if all symbols have been used and defined:
  var it: TTabIter
  var s = initTabIter(it, scope.symbols)
  var missingImpls = 0
  var unusedSyms: seq[tuple[sym: PSym, key: string]]
  while s != nil:
    if sfForward in s.flags and s.kind notin {skType, skModule}:
      # too many 'implementation of X' errors are annoying
      # and slow 'suggest' down:
      if missingImpls == 0:
        c.config.localError(s.info, SemReport(
          kind: rsemImplementationExpected, psym: s))

      inc missingImpls
    elif {sfUsed, sfExported} * s.flags == {}:
      if s.kind notin {skForVar, skParam, skMethod, skUnknown, skGenericParam, skEnumField}:
        # XXX: implicit type params are currently skTypes
        # maybe they can be made skGenericParam as well.
        if s.typ != nil and tfImplicitTypeParam notin s.typ.flags and
           s.typ.kind != tyGenericParam:
          unusedSyms.add (s, toFileLineCol(c.config, s.info))
    s = nextIter(it, scope.symbols)
  for (s, _) in sortedByIt(unusedSyms, it.key):
    c.config.localError(s.info, SemReport(
      kind: rsemXDeclaredButNotUsed, psym: s))

proc wrongRedefinition*(
    c: PContext; info: TLineInfo, s: PSym;
                        conflictsWith: PSym) =
  ## Emit a redefinition error if in non-interactive mode
  if c.config.cmd != cmdInteractive:
    c.config.localError(info, SemReport(
      kind: rsemRedefinitionOf, psym: s, alternative: conflictsWith))

# xxx pending bootstrap >= 1.4, replace all those overloads with a single one:
# proc addDecl*(c: PContext, sym: PSym, info = sym.info, scope = c.currentScope) {.inline.} =
proc addDeclAt*(c: PContext; scope: PScope, sym: PSym, info: TLineInfo) =
  let conflict = scope.addUniqueSym(sym)
  if conflict != nil:
    if sym.kind == skModule and conflict.kind == skModule and sym.owner == conflict.owner:
      # e.g.: import foo; import foo
      # xxx we could refine this by issuing a different hint for the case
      # where a duplicate import happens inside an include.
      c.config.localError(info, SemReport(
        kind: rsemDuplicateModuleImport,
        psym: sym,
        previous: c.config.toReportLinePoint(conflict.info)))
    else:
      wrongRedefinition(c, info, sym, conflict)

proc addDeclAt*(c: PContext; scope: PScope, sym: PSym) {.inline.} =
  addDeclAt(c, scope, sym, sym.info)

proc addDecl*(c: PContext, sym: PSym, info: TLineInfo) {.inline.} =
  addDeclAt(c, c.currentScope, sym, info)

proc addDecl*(c: PContext, sym: PSym) {.inline.} =
  addDeclAt(c, c.currentScope, sym)

proc addPrelimDecl*(c: PContext, sym: PSym) =
  discard c.currentScope.addUniqueSym(sym)

from ic / ic import addHidden

proc addInterfaceDeclAux(c: PContext, sym: PSym) =
  ## adds symbol to the module for either private or public access.
  if sfExported in sym.flags:
    # add to interface:
    if c.module != nil:
      exportSym(c, sym)
    else:
      c.config.internalUnreachable("addInterfaceDeclAux")

  elif sym.kind in ExportableSymKinds and c.module != nil and isTopLevelInsideDeclaration(c, sym):
    strTableAdd(semtabAll(c.graph, c.module), sym)
    if c.config.symbolFiles != disabledSf:
      addHidden(c.encoder, c.packedRepr, sym)

proc addInterfaceDeclAt*(c: PContext, scope: PScope, sym: PSym) =
  ## adds a symbol on the scope and the interface if appropriate
  addDeclAt(c, scope, sym)
  if not scope.isShadowScope:
    # adding into a non-shadow scope, we need to handle exports, etc
    addInterfaceDeclAux(c, sym)

proc addInterfaceDecl*(c: PContext, sym: PSym) {.inline.} =
  ## adds a decl and the interface if appropriate
  addInterfaceDeclAt(c, c.currentScope, sym)

proc addOverloadableSymAt*(c: PContext; scope: PScope, fn: PSym) =
  ## adds an symbol to the given scope, will check for and raise errors if it's
  ## a redefinition as opposed to an overload.
  if fn.kind notin OverloadableSyms:
    c.config.internalUnreachable(fn.info, "addOverloadableSymAt")
    return
  let check = strTableGet(scope.symbols, fn.name)
  if check != nil and check.kind notin OverloadableSyms:
    wrongRedefinition(c, fn.info, fn, check)
  else:
    scope.addSym(fn)

proc addInterfaceOverloadableSymAt*(c: PContext, scope: PScope, sym: PSym) =
  ## adds an overloadable symbol on the scope and the interface if appropriate
  addOverloadableSymAt(c, scope, sym)
  if not scope.isShadowScope:
    # adding into a non-shadow scope, we need to handle exports, etc
    addInterfaceDeclAux(c, sym)

proc openShadowScope*(c: PContext) =
  ## opens a shadow scope, just like any other scope except the depth is the
  ## same as the parent -- see `isShadowScope`.
  c.currentScope = PScope(parent: c.currentScope,
                          symbols: newStrTable(),
                          depthLevel: c.scopeDepth)

proc closeShadowScope*(c: PContext) =
  ## closes the shadow scope, but doesn't merge any of the symbols
  ## Does not check for unused symbols or missing forward decls since a macro
  ## or template consumes this AST
  rawCloseScope(c)

proc mergeShadowScope*(c: PContext) =
  ## close the existing scope and merge in all defined symbols, this will also
  ## trigger any export related code if this is into a non-shadow scope.
  ##
  ## Merges:
  ## shadow -> shadow: add symbols to the parent but check for redefinitions etc
  ## shadow -> non-shadow: the above, but also handle exports and all that
  let shadowScope = c.currentScope
  c.rawCloseScope
  for sym in shadowScope.symbols:
    if sym.kind in OverloadableSyms:
      c.addInterfaceOverloadableSymAt(c.currentScope, sym)
    else:
      c.addInterfaceDecl(sym)

when false:
  # `nimfix` used to call `altSpelling` and prettybase.replaceDeprecated(n.info, ident, alt)
  proc altSpelling(c: PContext, x: PIdent): PIdent =
    case x.s[0]
    of 'A'..'Z': result = getIdent(c.cache, toLowerAscii(x.s[0]) & x.s.substr(1))
    of 'a'..'z': result = getIdent(c.cache, toLowerAscii(x.s[0]) & x.s.substr(1))
    else: result = x

import std/editdistance, heapqueue

func `<`(a, b: SemSpellCandidate): bool =
  a.dist < b.dist and a.depth < b.depth and a.sym.name.s < b.sym.name.s

proc mustFixSpelling(c: PContext): bool {.inline.} =
  result = c.config.spellSuggestMax != 0 and c.compilesContextId == 0
    # don't slowdown inside compiles()

proc fixSpelling(
    c: PContext, n: PNode, ident: PIdent): seq[SemSpellCandidate] =
  ## when we cannot find the identifier, suggest nearby spellings
  var list = initHeapQueue[SemSpellCandidate]()
  let name0 = ident.s.nimIdentNormalize

  for (sym, depth, isLocal) in allSyms(c):
    let depth = -depth - 1
    let dist = editDistance(name0, sym.name.s.nimIdentNormalize)
    list.push SemSpellCandidate(
      dist: dist, depth: depth, sym: sym, isLocal: isLocal)

  if list.len == 0:
    return

  let e0 = list[0]
  var count = 0
  while true:
    if list.len == 0:
      break

    let e = list.pop()
    if c.config.spellSuggestMax == spellSuggestSecretSauce:
      const
        smallThres = 2
        maxCountForSmall = 4
        # avoids ton of operator matches when mis-matching short symbols
        # such as `i` other heuristics could be devised, such as only
        # suggesting operators if `name0` is an operator (likewise with
        # non-operators).

      if e.dist > e0.dist or
         (name0.len <= smallThres and count >= maxCountForSmall):
        break

    elif count >= c.config.spellSuggestMax:
      break

    result.add e
    inc count

proc errorUseQualifier(
    c: PContext; info: TLineInfo; s: PSym; amb: var bool): PSym =
  var
    i = 0
    ignoredModules = 0
    rep = SemReport(kind: rsemAmbiguous, psym: s)

  for candidate in importedItems(c, s.name):
    rep.candidates.add candidate
    if candidate.kind == skModule:
      inc ignoredModules
    else:
      result = candidate
    inc i

  if ignoredModules != i - 1:
    c.config.localError(info, rep)
    result = nil
  else:
    amb = false

proc errorUseQualifier*(c: PContext; info: TLineInfo; s: PSym) =
  var amb: bool
  discard errorUseQualifier(c, info, s, amb)

proc errorUseQualifier(c: PContext; info: TLineInfo; candidates: seq[PSym]) =
  var
    i = 0
    rep = SemReport(kind: rsemAmbiguous, psym: candidates[0])

  for candidate in candidates:
    rep.candidates.add candidate
    inc i

  c.config.localError(info, rep)

proc errorUndeclaredIdentifier*(
    c: PContext; info: TLineInfo; name: string,
    candidates: seq[SemSpellCandidate] = @[]
  ) =

  c.config.localError(info, SemReport(
    kind: rsemUndeclaredIdentifier,
    msg: name,
    spellingCandidates: candidates,
    potentiallyRecursive: c.recursiveDep.len > 0
  ))

  if c.recursiveDep.len > 0:
    # prevent excessive errors for 'nim check'
    c.recursiveDep = ""

proc errorUndeclaredIdentifierHint*(c: PContext; n: PNode, ident: PIdent): PSym =
  var candidates: seq[SemSpellCandidate]
  if c.mustFixSpelling:
    candidates = fixSpelling(c, n, ident)

  errorUndeclaredIdentifier(c, n.info, ident.s, candidates)

  result = errorSym(c, n)

proc lookUp*(c: PContext, n: PNode): PSym =
  # Looks up a symbol. Generates an error in case of nil.
  var amb = false
  case n.kind
  of nkIdent:
    result = searchInScopes(c, n.ident, amb).skipAlias(n, c.config)
    if result == nil: result = errorUndeclaredIdentifierHint(c, n, n.ident)
  of nkSym:
    result = n.sym
  of nkAccQuoted:
    var ident = considerQuotedIdent(c, n)
    result = searchInScopes(c, ident, amb).skipAlias(n, c.config)
    if result == nil: result = errorUndeclaredIdentifierHint(c, n, ident)
  else:
    c.config.internalUnreachable("lookUp")
    return
  if amb:
    #contains(c.ambiguousSymbols, result.id):
    result = errorUseQualifier(c, n.info, result, amb)
  when false:
    if result.kind == skStub: loadStub(result)

proc newQualifiedLookUpError(c: PContext, ident: PIdent, info: TLineInfo, err: PNode): PSym =
  ## create an error symbol for `qualifiedLookUp` related errors
  result = newSym(skError, ident, nextSymId(c.idgen), getCurrOwner(c), info)
  result.typ = c.errorType
  result.flags.incl(sfDiscardable)
  # result.flags.incl(sfError)
  result.ast = err

proc errorExpectedIdentifier(
    c: PContext, ident: PIdent, n: PNode, exp: PNode = nil
  ): PSym {.inline.} =
  ## create an error symbol for non-identifier in identifier position within an
  ## expression (`exp`). non-nil `exp` leads to better error messages.
  let ast =
    if exp.isNil:
      c.config.newError(n, SemReport(kind: rsemExpectedIdentifier))
    else:
      c.config.newError(n, SemReport(
        kind: rsemExpectedIdentifierInExpr, expression: exp))

  result = newQualifiedLookUpError(c, ident, n.info, ast)

proc errorSym2*(c: PContext, n, err: PNode): PSym =
  ## creates an error symbol to avoid cascading errors (for IDE support), with
  ## `n` as the node with the error and `err` with the desired `nkError`
  var m = n
  # ensure that 'considerQuotedIdent2' can't fail:
  if m.kind == nkDotExpr: m = m[1]
  let ident = if m.kind in {nkIdent, nkSym, nkAccQuoted}:
      let (i, e) = considerQuotedIdent2(c, m)
      doAssert e.isNil, "unexpected failure to retrieve ident: " & renderTree(m)
      i
    else:
      getIdent(c.cache, "err:" & renderTree(m))
  result = newQualifiedLookUpError(c, ident, n.info, err)
  # pretend it's from the top level scope to prevent cascading errors:
  if c.config.cmd != cmdInteractive and c.compilesContextId == 0:
    c.moduleScope.addSym(result)

proc errorUndeclaredIdentifierWithHint(
    c: PContext; n: PNode; name: string,
    candidates: seq[SemSpellCandidate] = @[]
  ): PSym =
  ## creates an error symbol with hints as to what it might be eg: recursive
  ## imports
  result = errorSym2(c, n, c.config.newError(
    n,
    SemReport(
      kind: rsemUndeclaredIdentifier,
      potentiallyRecursive: c.recursiveDep.len > 0,
      spellingCandidates: candidates,
      msg: name)))

  if c.recursiveDep.len > 0:
    c.recursiveDep = ""

proc errorAmbiguousUseQualifier(
    c: PContext; ident: PIdent, n: PNode, candidates: seq[PSym]
  ): PSym =
  ## create an error symbol for an ambiguous unqualified lookup
  var rep = SemReport(kind: rsemAmbiguous, psym: candidates[0])
  for i, candidate in candidates.pairs:
    rep.candidates.add candidate

  newQualifiedLookUpError(
    c, ident, n.info, c.config.newError(n, rep))

type
  TLookupFlag* = enum
    checkAmbiguity, checkUndeclared, checkModule, checkPureEnumFields

proc qualifiedLookUp2*(c: PContext, n: PNode, flags: set[TLookupFlag]): PSym =
  ## updated version of `qualifiedLookUp`, takes an identifier (ident, accent
  ## quoted, dot expression qualified, etc), finds the associated symbol or
  ## reports errors based on the `flags` configuration (allow ambiguity, etc).
  ##
  ## this new version returns an error symbol rather than issuing errors
  ## directly. The symbol's `ast` field will contain an nkError, and the `typ`
  ## field on the symbol will be the errorType
  ##
  ## XXX: currently skError is just a const for skUnknown which has many uses,
  ##      once things are cleaner, create a proper skError and use that instead
  ##      of a tuple return.
  ##
  ## XXX: maybe remove the flags for ambiguity and undeclared and let the call
  ##      sites figure it out instead?
  const allExceptModule = {low(TSymKind)..high(TSymKind)} - {skModule, skPackage}

  proc symFromCandidates(
    c: PContext, candidates: seq[PSym], ident: PIdent, n: PNode,
    flags: set[TLookupFlag], amb: var bool
  ): PSym =
    ## helper to find a sym in `candidates` from a scope or enums search
    case candidates.len
    of 0: nil
    of 1: candidates[0]
    else:
      amb = true
      if checkAmbiguity in flags:
        errorAmbiguousUseQualifier(c, ident, n, candidates)
      else:
        candidates[0]

  case n.kind
  of nkIdent, nkAccQuoted:
    var
      amb = false
      (ident, errNode) = considerQuotedIdent2(c, n)
    if not errNode.isNil:
      let errExprCtx = if errNode != n: n else: nil
        ## expression within which the error occurred
      result = errorExpectedIdentifier(c, ident, errNode, errExprCtx)
    elif checkModule in flags:
      result = searchInScopes(c, ident, amb).skipAlias(n, c.config)
      # search in scopes can return an skError
      if not result.isNil and result.kind == skError and not amb:
        result.ast = c.config.newError(
          n,
          SemReport(kind: rsemUndeclaredIdentifier),
          @[newStrNode(ident.s, n.info)])
    else:
      let candidates = searchInScopesFilterBy(c, ident, allExceptModule) #.skipAlias(n, c.config)
      result = symFromCandidates(c, candidates, ident, n, flags, amb)

    if result.isNil:
      # XXX: this might be a bug in that we only do this search if there are no
      #      results in scopes, but there could well be ambiguity across the
      #      two searches
      let candidates = allPureEnumFields(c, ident)
      result = symFromCandidates(c, candidates, ident, n, flags, amb)

    if result.isNil and checkUndeclared in flags:
      var candidates: seq[SemSpellCandidate]
      if c.mustFixSpelling:
        candidates = fixSpelling(c, n, ident)

      result = errorUndeclaredIdentifierWithHint(c, n, ident.s, candidates)

    elif checkAmbiguity in flags and result != nil and amb:
      var
        i = 0
        ignoredModules = 0
        candidates: seq[PSym]
      for candidate in importedItems(c, result.name):
        candidates.add(candidate)
        if candidate.kind == skModule:
          inc ignoredModules
        else:
          result = candidate
        inc i
      if ignoredModules == i-1: # left with exactly one unignored module
        # we're down to one, so we recovered from the error
        amb = false
      elif candidates.len == 0:
        discard
      else:
        result = errorAmbiguousUseQualifier(c, ident, n, candidates)

    if result == nil:
      if checkUndeclared in flags:
        result = errorUndeclaredIdentifierWithHint(c, n, ident.s, @[])
      else:
        discard
    elif result.kind == skError and result.typ.isNil:
      # XXX: legacy calls above can return an `skError` without the `typ` set
      result.typ = c.errorType

    c.isAmbiguous = amb
  of nkSym:
    result = n.sym
  of nkDotExpr:
    result = nil
    var m = qualifiedLookUp2(c, n[0], (flags * {checkUndeclared}) + {checkModule})
    if m != nil and m.kind == skModule:
      var
        ident: PIdent = nil
        errNode: PNode = nil
      if n[1].kind == nkIdent:
        ident = n[1].ident
      elif n[1].kind == nkAccQuoted:
        (ident, errNode) = considerQuotedIdent2(c, n[1])

      if ident != nil and errNode.isNil:
        if m == c.module:
          result = strTableGet(c.topLevelScope.symbols, ident).skipAlias(n, c.config)

        else:
          result = someSym(c.graph, m, ident).skipAlias(n, c.config)

        if result == nil and checkUndeclared in flags:
          result = errorUndeclaredIdentifierWithHint(c, n[1], ident.s, @[])

      elif n[1].kind == nkSym:
        result = n[1].sym
      elif checkUndeclared in flags and
          n[1].kind notin {nkOpenSymChoice, nkClosedSymChoice}:
        result = errorSym2(c, n[1],
          c.config.newError(
            n[1], SemReport(kind: rsemExpectedIdentifier)))
  else:
    result = nil
  when false:
    if result != nil and result.kind == skStub: loadStub(result)

proc qualifiedLookUp*(c: PContext, n: PNode, flags: set[TLookupFlag]): PSym =
  ## updated version of `qualifiedLookUp`, takes an identifier (ident, accent
  ## quoted, dot expression qualified, etc), finds the associated symbol or
  ## reports errors based on the `flags` configuration (allow ambiguity, etc).
  ##
  ## XXX: legacy, deprecate and replace with `qualifiedLookup2`
  const allExceptModule = {low(TSymKind)..high(TSymKind)} - {skModule, skPackage}
  case n.kind
  of nkIdent, nkAccQuoted:
    var amb = false
    var ident = considerQuotedIdent(c, n)
    if checkModule in flags:
      result = searchInScopes(c, ident, amb).skipAlias(n, c.config)
    else:
      let candidates = searchInScopesFilterBy(c, ident, allExceptModule) #.skipAlias(n, c.config)
      if candidates.len > 0:
        result = candidates[0]
        amb = candidates.len > 1
        if amb and checkAmbiguity in flags:
          errorUseQualifier(c, n.info, candidates)
    if result == nil:
      let candidates = allPureEnumFields(c, ident)
      if candidates.len > 0:
        result = candidates[0]
        amb = candidates.len > 1
        if amb and checkAmbiguity in flags:
          errorUseQualifier(c, n.info, candidates)

    if result == nil and checkUndeclared in flags:
      result = errorUndeclaredIdentifierHint(c, n, ident)
    elif checkAmbiguity in flags and result != nil and amb:
      result = errorUseQualifier(c, n.info, result, amb)
    c.isAmbiguous = amb
  of nkSym:
    result = n.sym
  of nkDotExpr:
    result = nil
    var m = qualifiedLookUp(c, n[0], (flags * {checkUndeclared}) + {checkModule})
    if m != nil and m.kind == skModule:
      var ident: PIdent = nil
      if n[1].kind == nkIdent:
        ident = n[1].ident
      elif n[1].kind == nkAccQuoted:
        ident = considerQuotedIdent(c, n[1])
      if ident != nil:
        if m == c.module:
          result = strTableGet(c.topLevelScope.symbols, ident).skipAlias(n, c.config)
        else:
          result = someSym(c.graph, m, ident).skipAlias(n, c.config)
        if result == nil and checkUndeclared in flags:
          result = errorUndeclaredIdentifierHint(c, n[1], ident)
      elif n[1].kind == nkSym:
        result = n[1].sym
      elif checkUndeclared in flags and
           n[1].kind notin {nkOpenSymChoice, nkClosedSymChoice}:
        c.config.localError(n[1].info, SemReport(
          kind: rsemExpectedIdentifier, expression: n[1]))

        result = errorSym(c, n[1])
  else:
    result = nil
  when false:
    if result != nil and result.kind == skStub: loadStub(result)

proc initOverloadIter*(o: var TOverloadIter, c: PContext, n: PNode): PSym =
  o.importIdx = -1
  o.marked = initIntSet()
  case n.kind
  of nkIdent, nkAccQuoted:
    var ident = considerQuotedIdent(c, n)
    var scope = c.currentScope
    o.mode = oimNoQualifier
    while true:
      result = initIdentIter(o.it, scope.symbols, ident).skipAlias(n, c.config)
      if result != nil:
        o.currentScope = scope
        break
      else:
        scope = scope.parent
        if scope == nil:
          for i in 0..c.imports.high:
            result = initIdentIter(o.mit, o.marked, c.imports[i], ident, c.graph).skipAlias(n, c.config)
            if result != nil:
              o.currentScope = nil
              o.importIdx = i
              return result
          return nil

  of nkSym:
    result = n.sym
    o.mode = oimDone
  of nkDotExpr:
    o.mode = oimOtherModule
    o.m = qualifiedLookUp(c, n[0], {checkUndeclared, checkModule})
    if o.m != nil and o.m.kind == skModule:
      var ident: PIdent = nil
      if n[1].kind == nkIdent:
        ident = n[1].ident
      elif n[1].kind == nkAccQuoted:
        ident = considerQuotedIdent(c, n[1], n)
      if ident != nil:
        if o.m == c.module:
          # a module may access its private members:
          result = initIdentIter(o.it, c.topLevelScope.symbols,
                                 ident).skipAlias(n, c.config)
          o.mode = oimSelfModule
        else:
          result = initModuleIter(o.mit, c.graph, o.m, ident).skipAlias(n, c.config)
      else:
        noidentError(c.config, n[1], n)
        result = errorSym(c, n[1])
  of nkClosedSymChoice, nkOpenSymChoice:
    o.mode = oimSymChoice
    if n[0].kind == nkSym:
      result = n[0].sym
    else:
      o.mode = oimDone
      return nil
    o.symChoiceIndex = 1
    o.marked = initIntSet()
    incl(o.marked, result.id)
  else: discard
  when false:
    if result != nil and result.kind == skStub: loadStub(result)

proc lastOverloadScope*(o: TOverloadIter): int =
  case o.mode
  of oimNoQualifier:
    result = if o.importIdx >= 0: 0
             elif o.currentScope.isNil: -1
             else: o.currentScope.depthLevel
  of oimSelfModule:  result = 1
  of oimOtherModule: result = 0
  else: result = -1

proc nextOverloadIterImports(o: var TOverloadIter, c: PContext, n: PNode): PSym =
  assert o.currentScope == nil
  var idx = o.importIdx+1
  o.importIdx = c.imports.len # assume the other imported modules lack this symbol too
  while idx < c.imports.len:
    result = initIdentIter(o.mit, o.marked, c.imports[idx], o.it.name, c.graph).skipAlias(n, c.config)
    if result != nil:
      # oh, we were wrong, some other module had the symbol, so remember that:
      o.importIdx = idx
      break
    inc idx

proc symChoiceExtension(o: var TOverloadIter; c: PContext; n: PNode): PSym =
  assert o.currentScope == nil
  while o.importIdx < c.imports.len:
    result = initIdentIter(o.mit, o.marked, c.imports[o.importIdx], o.it.name, c.graph).skipAlias(n, c.config)
    #while result != nil and result.id in o.marked:
    #  result = nextIdentIter(o.it, o.marked, c.imports[o.importIdx])
    if result != nil:
      #assert result.id notin o.marked
      return result
    inc o.importIdx

proc nextOverloadIter*(o: var TOverloadIter, c: PContext, n: PNode): PSym =
  case o.mode
  of oimDone:
    result = nil
  of oimNoQualifier:
    if o.currentScope != nil:
      assert o.importIdx < 0
      result = nextIdentIter(o.it, o.currentScope.symbols).skipAlias(n, c.config)
      while result == nil:
        o.currentScope = o.currentScope.parent
        if o.currentScope != nil:
          result = initIdentIter(o.it, o.currentScope.symbols, o.it.name).skipAlias(n, c.config)
          # BUGFIX: o.it.name <-> n.ident
        else:
          o.importIdx = 0
          if c.imports.len > 0:
            result = initIdentIter(o.mit, o.marked, c.imports[o.importIdx], o.it.name, c.graph).skipAlias(n, c.config)
            if result == nil:
              result = nextOverloadIterImports(o, c, n)
          break
    elif o.importIdx < c.imports.len:
      result = nextIdentIter(o.mit, o.marked, c.imports[o.importIdx], c.graph).skipAlias(n, c.config)
      if result == nil:
        result = nextOverloadIterImports(o, c, n)
    else:
      result = nil
  of oimSelfModule:
    result = nextIdentIter(o.it, c.topLevelScope.symbols).skipAlias(n, c.config)
  of oimOtherModule:
    result = nextModuleIter(o.mit, c.graph).skipAlias(n, c.config)
  of oimSymChoice:
    if o.symChoiceIndex < n.len:
      result = n[o.symChoiceIndex].sym
      incl(o.marked, result.id)
      inc o.symChoiceIndex
    elif n.kind == nkOpenSymChoice:
      # try 'local' symbols too for Koenig's lookup:
      o.mode = oimSymChoiceLocalLookup
      o.currentScope = c.currentScope
      result = firstIdentExcluding(o.it, o.currentScope.symbols,
                                   n[0].sym.name, o.marked).skipAlias(n, c.config)
      while result == nil:
        o.currentScope = o.currentScope.parent
        if o.currentScope != nil:
          result = firstIdentExcluding(o.it, o.currentScope.symbols,
                                      n[0].sym.name, o.marked).skipAlias(n, c.config)
        else:
          o.importIdx = 0
          result = symChoiceExtension(o, c, n)
          break
      if result != nil:
        incl o.marked, result.id
  of oimSymChoiceLocalLookup:
    if o.currentScope != nil:
      result = nextIdentExcluding(o.it, o.currentScope.symbols, o.marked).skipAlias(n, c.config)
      while result == nil:
        o.currentScope = o.currentScope.parent
        if o.currentScope != nil:
          result = firstIdentExcluding(o.it, o.currentScope.symbols,
                                      n[0].sym.name, o.marked).skipAlias(n, c.config)
        else:
          o.importIdx = 0
          result = symChoiceExtension(o, c, n)
          break
      if result != nil:
        incl o.marked, result.id

    elif o.importIdx < c.imports.len:
      result = nextIdentIter(o.mit, o.marked, c.imports[o.importIdx], c.graph).skipAlias(n, c.config)
      #assert result.id notin o.marked
      #while result != nil and result.id in o.marked:
      #  result = nextIdentIter(o.it, c.imports[o.importIdx]).skipAlias(n, c.config)
      if result == nil:
        inc o.importIdx
        result = symChoiceExtension(o, c, n)

  when false:
    if result != nil and result.kind == skStub: loadStub(result)

proc pickSym*(c: PContext, n: PNode; kinds: set[TSymKind];
              flags: TSymFlags = {}): PSym =
  var o: TOverloadIter
  var a = initOverloadIter(o, c, n)
  while a != nil:
    if a.kind in kinds and flags <= a.flags:
      if result == nil: result = a
      else: return nil # ambiguous
    a = nextOverloadIter(o, c, n)
