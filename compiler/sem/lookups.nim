#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements lookup helpers.

import
  std/[
    algorithm,
    strutils,
    intsets
  ],
  compiler/ast/[
    ast,
    astalgo,
    idents,
    renderer,
    lineinfos,
    errorhandling,
    errorreporting,
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/utils/[
    idioms,
    debugutils
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    semdata
  ]


# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport,
  reportAst,
  reportSem,
  reportStr,
  reportSym,
  reportSymbols
from compiler/ast/report_enums import ReportKind

proc ensureNoMissingOrUnusedSymbols(c: PContext; scope: PScope)

type
  PIdentResult* = tuple
    ident: PIdent      ## found ident, otherwise `IdentCache.notFoundIdent`
    errNode: PNode     ## if ident is notFoundIdent, node with the error, use
                       ## with original PNode for better error reporting

proc considerQuotedIdent*(c: PContext; n: PNode): PIdentResult =
  ## Retrieve a PIdent from a PNode, taking into account accent nodes for
  ## keyword escaping (stropping) and identifier construction. If none found,
  ## returns a `idents.IdentCache.identNotFound` and an error node (`nkError`).
  ##
  ## Idents are derived as follows:
  ## - nkIdent -> ident
  ## - nkSym -> ident
  ## - nkSymChoices -> ident of first child; assert if malformed
  ## - nkAccQuoted:
  ##    - if empty -> an error
  ##    - if singleton with nkIdent|Sym|SymChoices -> handled as above
  ##    - else -> construct identifier, see next
  ## - anything else -> not found (see note )
  ##
  ## Ident Construction, creates an ident by concatenating child string values:
  ## - nkIdent|Sym|SymChoices -> ident.s for string value
  ## - nkAccQuoted:
  ##   - if empty -> empty string
  ##   - else -> recursively evaluate
  ## - string literals -> string
  ## - chararacter literal -> character as string
  ## - integer literal -> int to string
  ## - anything else -> error
  ## To ensure we don't have a run away evaluation, identifier construction has
  ## a hard limit on number of items.
  # xxx: `considerQuotedIdent` raises an interesting question around what is a
  #      syntactically valid identifier vs a core language valid identifier.
  #      That is to say what can a human provide as source that'll pass lexing
  #      and parsing, and is generally known as Nimskull vs what our "lisp-y"
  #      core language allows. Further to that, what do we afford
  #      end-programmers via templates vs macros? Presently, `nkAccQuote` we
  #      don't _really_ enforce identifier well-formed-ness in many places and
  #      `nkAccQuoted` identifier construction let's us get away with a lot
  #      more than keyword escaping.
  let
    ic = c.cache
    notFoundIdent = ic.getNotFoundIdent()

  const
    atomicIdentKinds = {nkIdent, nkSym}
    renderableLiterals = nkLiterals - nkFloatLiterals
    renderableKinds = atomicIdentKinds + renderableLiterals
    allNodeKinds = {low(TNodeKind)..high(TNodeKind)}

  proc accQuotedConstrIdentNode(c: PContext, n: PNode): PNode =
    ## construct an identifier from the `nkAccQuoted` node `n`, returning an
    ## `nkIdent` node, or `nkError` upon failure.
    const limit = 20 ## limit how much evaluation we do here
    assert n.kind == nkAccQuoted
    var
      stack: seq[(int, PNode)] = @[(0, n)]
        ## tracks the depth first node traversal (child index and node) of
        ## `nkAccQuoted` nodes, the index is mutated to track which quoted
        ## child needs to be processed next
      wellFormedAst = true
        ## tracks if `n` is well formed AST even if there are no `nkError`
      hasError = false
        ## `n` already has errors (`nkError` nodes)
      counter = 0
        ## how many iterations we've done, used to avoid lengthy/endless eval
      flattened: seq[PNode]
        ## collection point for a flat `nkAccQuote` tree, for ident string gen

    while stack.len > 0 and counter < limit:
      let
        stackPos = stack.len - 1
        quote = stack[stackPos][1]
      var idx = stack[stackPos][0]

      # process until the next `nkAccQuoted` that requires descent
      while idx < quote.len and
            (quote[idx].kind != nkAccQuoted or quote[idx].len == 0):
        inc counter
        let q = quote[idx]
        case q.kind
        of atomicIdentKinds, nkIntLiterals, nkStrLiterals:
          flattened.add q
        of nkSymChoices:
          flattened.add:
            if q.len > 0 and q[0].kind == nkSym:
              q[0]
            else:
              q   # treat `nkSymChoices` as an error signal below
        of nkAccQuoted:
          assert q.len == 0
          flattened.add q
        of nkError:
          hasError = true
          flattened.add q
        of allNodeKinds - nkIdentKinds - renderableKinds - {nkError}:
          wellFormedAst = false
          flattened.add q
        inc idx

      if idx < quote.len:
        assert quote[idx].kind == nkAccQuoted and quote[idx].len > 0
        stack.add (0, quote[idx])
        inc counter
        inc idx
        stack[stackPos][0] = idx # update to latest index
      else:
        discard stack.pop() # we're done processing pop and move on

    # now we have a very flattened/normalized `nkAccQuoted` ast expression,
    # time to try constructing an identifier.
    result =
      if hasError:
        c.config.wrapError(n)
      elif counter >= limit:
        c.config.newError(n, PAstDiag(kind: adSemExpectedIdentifierQuoteLimit))
      elif wellFormedAst:
        var
          id = ""
          badNode: PNode = nil
        for s in flattened.items:
          case s.kind
          of nkIdent:                     id.add(s.ident.s)
          of nkSym:                       id.add(s.sym.name.s)
          of nkStrLiterals:               id.add(s.strVal)
          of nkCharLit:                   id.add(char(s.intVal))
          of nkIntLiterals - {nkCharLit}: id.addInt(s.intVal)
          of nkAccQuoted:                 discard "empty string"
          of nkSymChoices:
            badNode = s
            break
          of allNodeKinds - nkIdentKinds - renderableKinds:
            unreachable("was eliminated in normalization")
        if badNode != nil:
          c.config.newError(n, PAstDiag(kind: adSemExpectedIdentifierInExpr,
                                        notIdent: badNode))
        elif id == "":
          c.config.newError(n, PAstDiag(kind: adSemExpectedIdentifier))
        else:
          newIdentNode(ic.getIdent(id), n.info)
      else:
        # not well formed, but no errors
        c.config.newError(n,
          PAstDiag(kind: adSemExpectedIdentifier))

  template extractAtomicIdent(atom: PNode): PIdent =
    ## extracts the ident from `atom` which must be an `atomicIdentKinds`
    case atom.kind
    of nkIdent:                         atom.ident
    of nkSym:                           atom.sym.name
    of allNodeKinds - atomicIdentKinds: unreachable()

  var
    currN = n
    ident: PIdent
  case currN.kind
  of nkIdent, nkSym:
    ident = extractAtomicIdent(currN)
  of nkOpenSymChoice, nkClosedSymChoice:
    assert currN.len > 0 and currN[0].kind == nkSym
    currN = currN[0]
  of nkAccQuoted:
    case currN.len
    of 0: ident = notFoundIdent
    of 1:
      let inner = currN[0]
      case inner.kind
      of atomicIdentKinds:
        currN = inner
      of nkSymChoices:
        assert inner.len > 0 and inner[0].kind == nkSym
        currN = inner[0]
      of nkAccQuoted, renderableLiterals:
        currN = accQuotedConstrIdentNode(c, currN)
      of nkError:
        currN = c.config.wrapError(n)
      else:
        ident = notFoundIdent
    else:
      currN = accQuotedConstrIdentNode(c, currN)
  of allNodeKinds - nkIdentKinds:
    # the case condition is awkward, but it ensures exhaustiveness
    ident = notFoundIdent

  if ident.isNil: # we didn't find one, so check the innner part via `currN`
    assert currN != n, "need a different node for different results"
    ident =
      case currN.kind
      of nkIdent, nkSym:
        assert (currN.kind == nkIdent and n.kind == nkAccQuoted) or
                (currN.kind == nkSym and n.kind in nkSymChoices+{nkAccQuoted})
        extractAtomicIdent(currN)
      of allNodeKinds - atomicIdentKinds:
        notFoundIdent

  assert ident != nil, "ident not set to fournd or `notFoundIdent` sentinel"

  result.ident = ident
  result.errNode =
    if currN.kind == nkError:
      currN
    elif ident == notFoundIdent:
      c.config.newError(n):
        if currN == n: PAstDiag(kind: adSemExpectedIdentifier)
        else:          PAstDiag(kind: adSemExpectedIdentifierInExpr,
                                notIdent: currN)
    else:
      nil
  
  assert result.ident != notFoundIdent or result.errNode != nil,
    "we either found and ident or it must be an error"

proc legacyConsiderQuotedIdent*(c: PContext; n, origin: PNode): PIdent =
  ## Legacy/Transition Code: avoid using this
  ##
  ## `considerQuotedIdent` used to allow providing an `origin` for "better"
  ## error reporting, but it was an awkward API. This template creates a block
  ## to replicate the previous behaviour of fetching and doing inline error
  ## with an `origin` parameter for the purposes of transition.
  let (ident, err) = considerQuotedIdent(c, n)
  result = ident
  if err != nil:
    if origin.isNil or n == origin:
      localReport(c.config, err)
    else:
      # xxx: janky error gen, the way to fix is this by starting at the
      #      callsites and reworking how things are consumed/passed in.
      #      also, the nkAccQuote handling likely requires alpha-rewriting
      if origin.isError: c.config.localReport(origin)
      if n.kind == nkError: c.config.localReport(n)
      let
        errTarget =
          if n.kind == nkAccQuoted and n.len == 1:
            n[0]
          else:
            n
        errDiag = PAstDiag(kind: adSemExpectedIdentifierInExpr,
                            notIdent: errTarget)
        finalErr = c.config.newError(origin, errDiag)

      localReport(c.config, finalErr)

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
        c.config.localReport(s.info, reportSym(
          rsemImplementationExpected, s))

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
    c.config.localReport(s.info, reportSym(rsemXDeclaredButNotUsed, s))

proc wrongRedefinition*(
    c: PContext; info: TLineInfo, s: PSym;
                        conflictsWith: PSym) =
  ## Emit a redefinition error if in non-interactive mode
  if c.config.cmd != cmdInteractive:
    c.config.localReport(info, reportSymbols(
      rsemRedefinitionOf, @[s, conflictsWith]))

proc addDecl*(c: PContext, sym: PSym, info = sym.info, scope = c.currentScope) =
  let conflict = scope.addUniqueSym(sym)
  if conflict != nil:
    if sym.kind == skModule and
       conflict.kind == skModule and
       sym.owner == conflict.owner:
      # e.g.: import foo; import foo
      # xxx we could refine this by issuing a different hint for the case
      # where a duplicate import happens inside an include.
      c.config.localReport(info, SemReport(
        kind: rsemDuplicateModuleImport,
        sym: sym,
        previous: conflict))
    else:
      wrongRedefinition(c, info, sym, conflict)

proc addPrelimDecl*(c: PContext, sym: PSym) =
  discard c.currentScope.addUniqueSym(sym)

from compiler/ic/ic import addHidden

proc addInterfaceDeclAux(c: PContext, sym: PSym) =
  ## adds symbol to the module for either private or public access.
  if sfExported in sym.flags:
    # add to interface:
    c.config.internalAssert(c.module != nil, "addInterfaceDeclAux")
    exportSym(c, sym)

  elif sym.kind in ExportableSymKinds and c.module != nil and isTopLevelInsideDeclaration(c, sym):
    strTableAdd(semtabAll(c.graph, c.module), sym)
    if c.config.symbolFiles != disabledSf:
      addHidden(c.encoder, c.packedRepr, sym)

proc addInterfaceDeclAt*(c: PContext, scope: PScope, sym: PSym) =
  ## adds a symbol on the scope and the interface if appropriate
  addDecl(c, sym, scope = scope)
  if not scope.isShadowScope:
    # adding into a non-shadow scope, we need to handle exports, etc
    addInterfaceDeclAux(c, sym)

proc addInterfaceDecl*(c: PContext, sym: PSym) {.inline.} =
  ## adds a decl and the interface if appropriate
  addInterfaceDeclAt(c, c.currentScope, sym)

proc addOverloadableSymAt*(c: PContext; scope: PScope, fn: PSym) =
  ## adds an symbol to the given scope, will check for and raise errors if it's
  ## a redefinition as opposed to an overload.
  c.config.internalAssert(fn.kind in OverloadableSyms, fn.info, "addOverloadableSymAt")
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

template toOrderTup(a: SemSpellCandidate): auto =
  # `dist` is first, to favor nearby matches
  # `depth` is next, to favor nearby enclosing scopes among ties
  # `sym.name.s` is last, to make the list ordered and deterministic among ties
  (a.dist, a.depth, a.sym.name.s)

func `<`(a, b: SemSpellCandidate): bool =
  # QUESTION this is /not/ the same as `a.dist < b.dist and ...`. So how in
  # the world this code even works?
  toOrderTup(a) < toOrderTup(b)

proc mustFixSpelling(c: PContext): bool {.inline.} =
  result = c.config.spellSuggestMax != 0 and c.compilesContextId == 0
    # don't slowdown inside compiles()

proc fixSpelling*(c: PContext, ident: PIdent): seq[SemSpellCandidate] =
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


proc errorUseQualifier*(c: PContext, n: PNode; s: PSym): PSym =
  var
    i = 0
    ignoredModules = 0
    diag = PAstDiag(kind: adSemAmbiguousIdentWithCandidates,
                    candidateSyms: @[s])

  for candidate in importedItems(c, s.name):
    diag.candidateSyms.add candidate
    case candidate.kind
    of skModule:
      inc ignoredModules
    else:
      result = candidate
    inc i

  if ignoredModules == i - 1: # we ignored all but one, not ambiguous
    discard "return result as is"
  else:
    result = newSym(skError, s.name, c.idgen.nextSymId, c.getCurrOwner, n.info)
    result.typ = c.errorType
    result.flags.incl(sfDiscardable)
    result.ast = c.config.newError(n, diag)

proc clearRecursiveDeps(c: PContext) =
  ## clears the recursive dependency stack being tracked, this was done to make
  ## `nim check` less spammy. The storage for this info is much lighter now and
  ## we should clear another way
  
  # what used to happen was some undeclared identifier error would be
  # triggered, as there would be recursive deps detected and populated by the
  # `importer` module in `myImport` we'd pass it along as hints with the error.
  # as part of creating the error the error logic would run the logic in this
  # proc to clear the deps tracking so `nim check` spam was reduced.

  when false:
    if c.recursiveDep.len > 0:
      c.recursiveDep.setLen 0

proc errorUndeclaredIdentifier*(
    c: PContext; info: TLineInfo; name: string,
    candidates: seq[SemSpellCandidate] = @[]
  ) =
  c.config.localReport(info, SemReport(
    kind: rsemUndeclaredIdentifier,
    str: name,
    spellingCandidates: candidates,
    recursiveDeps: c.recursiveDep))

  c.clearRecursiveDeps()

proc newQualifiedLookUpError(c: PContext, ident: PIdent, info: TLineInfo, err: PNode): PSym =
  ## create an error symbol for `qualifiedLookUp` related errors
  result = newSym(skError, ident, nextSymId(c.idgen), getCurrOwner(c), info)
  result.typ = c.errorType
  result.flags.incl(sfDiscardable)
  # result.flags.incl(sfError)
  result.ast = err

proc errorSym*(c: PContext, n, err: PNode): PSym =
  ## creates an error symbol to avoid cascading errors (for IDE support), with
  ## `n` as the node with the error and `err` with the desired `nkError`
  var m = n
  # ensure that 'considerQuotedIdent' can't fail:
  if m.kind == nkDotExpr: m = m[1]
  let ident = if m.kind in {nkIdent, nkSym, nkAccQuoted}:
      let (i, e) = considerQuotedIdent(c, m)
      doAssert e.isNil, "unexpected failure to retrieve ident: " & renderTree(m)
      i
    else:
      getIdent(c.cache, "err:" & renderTree(m))
  result = newQualifiedLookUpError(c, ident, n.info, err)

proc createUndeclaredIdentifierError*(
    c:PContext; n: PNode; name:string;
    candidates: seq[SemSpellCandidate] = @[]
  ): PNode =
  result = c.config.newError(n, PAstDiag(
                      kind: adSemUndeclaredIdentifier,
                      name: name,
                      spellingCandidates: candidates,
                      recursiveDeps: c.recursiveDep))

  c.clearRecursiveDeps()

proc errorUndeclaredIdentifierHint*(
    c: PContext; n: PNode, ident: PIdent): PSym =
  var candidates: seq[SemSpellCandidate]
  if c.mustFixSpelling:
    candidates = fixSpelling(c, ident)

  result = errorSym(c, n,
            createUndeclaredIdentifierError(c, n, ident.s, candidates))

proc lookUp*(c: PContext, n: PNode): PSym =
  # Looks up a symbol. Generates an error in case of nil.
  var amb = false
  case n.kind
  of nkIdent:
    result = searchInScopes(c, n.ident, amb)
    if result == nil:
      result = errorUndeclaredIdentifierHint(c, n, n.ident)
      localReport(c.config, result.ast)
  of nkSym:
    result = n.sym
  of nkAccQuoted:
    var (ident, err) = considerQuotedIdent(c, n)
    if err.isNil:
      result = searchInScopes(c, ident, amb)
      if result == nil:
        result = errorUndeclaredIdentifierHint(c, n, ident)
        localReport(c.config, result.ast)
    else:
      result = newQualifiedLookUpError(c, ident, n.info, err)
  else:
    c.config.internalError("lookUp")
    return
  if amb:
    result = errorUseQualifier(c, n, result)
  when false:
    if result.kind == skStub: loadStub(result)

proc errorExpectedIdentifier(
    c: PContext, ident: PIdent, n: PNode, exp: PNode = nil
  ): PSym {.inline.} =
  ## create an error symbol for non-identifier in identifier position within an
  ## expression (`exp`). non-nil `exp` leads to better error messages.
  let ast =
    if exp.isNil:
      c.config.newError(n, PAstDiag(kind: adSemExpectedIdentifier))
    else:
      c.config.newError(exp):
        PAstDiag(kind: adSemExpectedIdentifierInExpr, notIdent: n)

  result = newQualifiedLookUpError(c, ident, n.info, ast)

proc errorUndeclaredIdentifierWithHint*(
    c: PContext; n: PNode; name: string,
    candidates: seq[SemSpellCandidate] = @[]
  ): PSym =
  ## creates an error symbol with hints as to what it might be eg: recursive
  ## imports
  result = errorSym(c, n, c.config.newError(
    n,
    PAstDiag(
      kind: adSemUndeclaredIdentifier,
      recursiveDeps: c.recursiveDep,
      spellingCandidates: candidates,
      name: name)))

  c.clearRecursiveDeps()

proc errorAmbiguousUseQualifier(
    c: PContext; ident: PIdent, n: PNode, candidates: sink seq[PSym]
  ): PSym =
  ## create an error symbol for an ambiguous unqualified lookup
  let
    diag = PAstDiag(kind: adSemAmbiguousIdentWithCandidates,
                    candidateSyms: candidates)
    err = c.config.newError(n, diag)
  result = newQualifiedLookUpError(c, ident, n.info, err)

type
  TLookupFlag* = enum
    checkAmbiguity, checkUndeclared, checkModule, checkPureEnumFields

proc qualifiedLookUp*(c: PContext, n: PNode, flags: set[TLookupFlag]): PSym =
  ## takes an identifier (ident, accent quoted, dot expression qualified, etc),
  ## finds the associated symbol or reports errors based on the `flags`
  ## configuration (allow ambiguity, etc).
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
  c.config.addInNimDebugUtils("qualifiedLookUp", n, result)

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
      (ident, errNode) = considerQuotedIdent(c, n)

    if isNotFound(c.cache, ident):
      let
        wrongNode =
          if n.kind == nkAccQuoted and n.len == 1:
            n[0]
          else:
            n
        errExprCtx =
          if n != wrongNode:
            n
          else:
            nil
          ## expression within which the error occurred
        
      result = errorExpectedIdentifier(c, ident, wrongNode, errExprCtx)
    elif checkModule in flags:
      result = searchInScopes(c, ident, amb)
      # xxx: search in scopes can return an skError -- this happens because
      # skError is a const referring to skUnknown, which gets used in resolving
      # `result`, which starts off as undeclared/unknown.
      if result.isError and not amb and checkUndeclared in flags:
        let diag = PAstDiag(kind:      adSemOnlyDeclaredIdentifierFoundIsError, 
                            identName: ident.s,
                            err:       result.ast)
        result.ast = c.config.newError(n, diag)
    else:
      let candidates = searchInScopesFilterBy(c, ident, allExceptModule)
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
        candidates = fixSpelling(c, ident)

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
    var m = qualifiedLookUp(c, n[0], (flags * {checkUndeclared}) + {checkModule})
    if m != nil and m.kind == skModule:
      var
        ident: PIdent = nil
        errNode: PNode = nil
      if n[1].kind == nkIdent:
        ident = n[1].ident
      elif n[1].kind == nkAccQuoted:
        (ident, errNode) = considerQuotedIdent(c, n[1])

      if ident != nil and errNode.isNil:
        result =
          if m == c.module:
            strTableGet(c.topLevelScope.symbols, ident)
          else:
            someSym(c.graph, m, ident)

        if result == nil and checkUndeclared in flags:
          result = errorUndeclaredIdentifierWithHint(c, n[1], ident.s, @[])
      elif n[1].kind == nkSym:
        result = n[1].sym
      elif checkUndeclared in flags and
          n[1].kind notin {nkOpenSymChoice, nkClosedSymChoice}:
        result = errorSym(c, n[1]):
          c.config.newError(n[1], PAstDiag(kind: adSemExpectedIdentifier))
    elif m.isError:
      # create a copy of n with the error from `m`'s lookup
      var err = copyTreeWithoutNode(n, n[0])
      err[0] = m.ast
      err = wrapError(c.config, err)
      result = errorSym(c, n, err)
  else:
    result = nil
  when false:
    if result != nil and result.kind == skStub: loadStub(result)

proc initOverloadIter*(o: var TOverloadIter, c: PContext, n: PNode): PSym =
  o.importIdx = -1
  o.marked = initIntSet()
  case n.kind
  of nkIdent, nkAccQuoted:
    let (ident, err) = considerQuotedIdent(c, n)
    if err.isNil:  
      var scope = c.currentScope
      o.mode = oimNoQualifier
      while true:
        result = initIdentIter(o.it, scope.symbols, ident)
        if result != nil:
          o.currentScope = scope
          break
        else:
          scope = scope.parent
          if scope == nil:
            for i in 0..c.imports.high:
              result = initIdentIter(o.mit, o.marked, c.imports[i], ident, c.graph)
              if result != nil:
                o.currentScope = nil
                o.importIdx = i
                return result
            return nil
    else:
      result = newQualifiedLookUpError(c, ident, n.info, err)

  of nkSym:
    result = n.sym
    o.mode = oimDone

  of nkDotExpr:
    o.mode = oimOtherModule
    o.m = qualifiedLookUp(c, n[0], {checkUndeclared, checkModule})
    if o.m.isError:
      let errDotExpr = copyNode n
      errDotExpr.add o.m.ast
      errDotExpr.add n[1]
      result = newQualifiedLookUpError(c, o.m.name, n.info,
                  c.config.wrapError(errDotExpr))
    elif o.m != nil and o.m.kind == skModule:
      let (ident, err) = considerQuotedIdent(c, n[1])

      if err.isNil:
        if ident != nil:
          if o.m == c.module:
            # a module may access its private members:
            result = initIdentIter(o.it, c.topLevelScope.symbols,
                                  ident)
            o.mode = oimSelfModule
          else:
            result = initModuleIter(o.mit, c.graph, o.m, ident)
      else:
        let errDotExpr = copyNode n
        errDotExpr.add o.m.ast
        errDotExpr.add err
        result = newQualifiedLookUpError(c, ident, n.info,
                    c.config.wrapError(errDotExpr))

  of nkClosedSymChoice, nkOpenSymChoice:
    case n[0].kind
    of nkSym:
      o.mode = oimSymChoice
      result = n[0].sym

      o.currentScope = c.currentScope # already remember the start scope
      o.symChoiceIndex = 1
      o.marked = initIntSet()
      incl(o.marked, result.id)
    else:
      o.mode = oimDone
      result = nil

  else:
    discard

  when false:
    if result != nil and result.kind == skStub: loadStub(result)

proc initOverloadIter*(o: var TOverloadIter, c: PContext, scope: PScope,
                       n: PNode): PSym =
  ## Similiar to the normal ``initOverloadIter`` procedure, but allows for
  ## specifying the scope to start in.
  let prev = c.currentScope
  c.currentScope = scope
  result = initOverloadIter(o, c, n)
  c.currentScope = prev # restore the current scope

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
    result = initIdentIter(o.mit, o.marked, c.imports[idx], o.it.name, c.graph)
    if result != nil:
      # oh, we were wrong, some other module had the symbol, so remember that:
      o.importIdx = idx
      break
    inc idx

proc symChoiceExtension(o: var TOverloadIter; c: PContext; n: PNode): PSym =
  assert o.currentScope == nil
  while o.importIdx < c.imports.len:
    result = initIdentIter(o.mit, o.marked, c.imports[o.importIdx], o.it.name, c.graph)
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
      result = nextIdentIter(o.it, o.currentScope.symbols)
      while result == nil:
        o.currentScope = o.currentScope.parent
        if o.currentScope != nil:
          result = initIdentIter(o.it, o.currentScope.symbols, o.it.name)
          # BUGFIX: o.it.name <-> n.ident
        else:
          o.importIdx = 0
          if c.imports.len > 0:
            result = initIdentIter(o.mit, o.marked, c.imports[o.importIdx], o.it.name, c.graph)
            if result == nil:
              result = nextOverloadIterImports(o, c, n)
          break
    elif o.importIdx < c.imports.len:
      result = nextIdentIter(o.mit, o.marked, c.imports[o.importIdx], c.graph)
      if result == nil:
        result = nextOverloadIterImports(o, c, n)
    else:
      result = nil
  of oimSelfModule:
    result = nextIdentIter(o.it, c.topLevelScope.symbols)
  of oimOtherModule:
    result = nextModuleIter(o.mit, c.graph)
  of oimSymChoice:
    if o.symChoiceIndex < n.len:
      result = n[o.symChoiceIndex].sym
      incl(o.marked, result.id)
      inc o.symChoiceIndex
    elif n.kind == nkOpenSymChoice:
      # try 'local' symbols too for Koenig's lookup:
      o.mode = oimSymChoiceLocalLookup
      # the `currentScope` was already initialized by ``initOverloadIter``
      result = firstIdentExcluding(o.it, o.currentScope.symbols,
                                   n[0].sym.name, o.marked)
      while result == nil:
        o.currentScope = o.currentScope.parent
        if o.currentScope != nil:
          result = firstIdentExcluding(o.it, o.currentScope.symbols,
                                      n[0].sym.name, o.marked)
        else:
          o.importIdx = 0
          result = symChoiceExtension(o, c, n)
          break
      if result != nil:
        incl o.marked, result.id
  of oimSymChoiceLocalLookup:
    if o.currentScope != nil:
      result = nextIdentExcluding(o.it, o.currentScope.symbols, o.marked)
      while result == nil:
        o.currentScope = o.currentScope.parent
        if o.currentScope != nil:
          result = firstIdentExcluding(o.it, o.currentScope.symbols,
                                      n[0].sym.name, o.marked)
        else:
          o.importIdx = 0
          result = symChoiceExtension(o, c, n)
          break
      if result != nil:
        incl o.marked, result.id

    elif o.importIdx < c.imports.len:
      result = nextIdentIter(o.mit, o.marked, c.imports[o.importIdx], c.graph)
      #assert result.id notin o.marked
      #while result != nil and result.id in o.marked:
      #  result = nextIdentIter(o.it, c.imports[o.importIdx])
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

proc fieldVisible*(c: PContext, f: PSym): bool {.inline.} =
  let fmoduleId = getModule(f).id
  result = sfExported in f.flags or fmoduleId == c.module.id

  if not result:
    for module in c.friendModules:
      if fmoduleId == module.id: return true
    if f.kind == skField:
      var symObj = f.owner
      let genericOwner =
        if sfFromGeneric in f.owner.flags:
          f.owner.owner
        else:
          nil

      if symObj.typ.kind in {tyRef, tyPtr}:
        symObj = symObj.typ[0].sym
      # go through all scopes and check whether access to the object is
      # allowed. The generic owner test is needed for the "access to all
      # instances of generic object type allowed" feature to work
      for scope in allScopes(c.currentScope):
        for sym in scope.allowPrivateAccess:
          if symObj.id == sym.id or
             (genericOwner != nil and genericOwner.id == sym.id):
            return true