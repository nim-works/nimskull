#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the symbol importing mechanism.

import
  std/[
    intsets,
    sets,
    tables
  ],
  compiler/ast/[
    ast,
    astalgo,
    idents,
    lineinfos,
    wordrecg,
    reports,
    errorhandling,
    errorreporting,
  ],
  compiler/modules/[
    modulepaths,
    modulegraphs
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    lookups,
    semdata,
  ]

proc declarePureEnumField*(c: PContext; s: PSym) =
  # XXX Remove the outer 'if' statement and see what breaks.
  var amb = false
  if someSymFromImportTable(c, s.name, amb) == nil:
    strTableAdd(c.pureEnumFields, s)
    when false:
      let checkB = strTableGet(c.pureEnumFields, s.name)
      if checkB == nil:
        strTableAdd(c.pureEnumFields, s)
    when false:
      # mark as ambiguous:
      incl(c.ambiguousSymbols, checkB.id)
      incl(c.ambiguousSymbols, s.id)

proc importPureEnumField(c: PContext; s: PSym) =
  var amb = false
  if someSymFromImportTable(c, s.name, amb) == nil:
    strTableAdd(c.pureEnumFields, s)
    when false:
      let checkB = strTableGet(c.pureEnumFields, s.name)
      if checkB == nil:
        strTableAdd(c.pureEnumFields, s)
    when false:
      # mark as ambiguous:
      incl(c.ambiguousSymbols, checkB.id)
      incl(c.ambiguousSymbols, s.id)

proc importPureEnumFields(c: PContext; s: PSym; etyp: PType) =
  assert sfPure in s.flags
  for j in 0..<etyp.n.len:
    var e = etyp.n[j].sym
    c.config.internalAssert(e.kind == skEnumField, s.info, "rawImportSymbol")
    # BUGFIX: because of aliases for enums the symbol may already
    # have been put into the symbol table
    # BUGFIX: but only iff they are the same symbols!
    for check in importedItems(c, e.name):
      if check.id == e.id:
        e = nil
        break
    if e != nil:
      importPureEnumField(c, e)

proc rawImportSymbol(c: PContext, s, origin: PSym; importSet: var IntSet) =
  # This does not handle stubs, because otherwise loading on demand would be
  # pointless in practice. So importing stubs is fine here!
  # check if we have already a symbol of the same name:
  when false:
    var check = someSymFromImportTable(c, s.name)
    if check != nil and check.id != s.id:
      if s.kind notin OverloadableSyms or check.kind notin OverloadableSyms:
        # s and check need to be qualified:
        incl(c.ambiguousSymbols, s.id)
        incl(c.ambiguousSymbols, check.id)
  # thanks to 'export' feature, it could be we import the same symbol from
  # multiple sources, so we need to call 'strTableAdd' here:
  when false:
    # now lazy. Speeds up the compiler and is a prerequisite for IC.
    strTableAdd(c.importTable.symbols, s)
  else:
    importSet.incl s.id
  if s.kind == skType:
    var etyp = s.typ
    if etyp.kind in {tyBool, tyEnum}:
      for j in 0..<etyp.n.len:
        var e = etyp.n[j].sym
        c.config.internalAssert(e.kind == skEnumField, s.info, "rawImportSymbol")
        # BUGFIX: because of aliases for enums the symbol may already
        # have been put into the symbol table
        # BUGFIX: but only iff they are the same symbols!
        for check in importedItems(c, e.name):
          if check.id == e.id:
            e = nil
            break
        if e != nil:
          if sfPure notin s.flags:
            rawImportSymbol(c, e, origin, importSet)
          else:
            importPureEnumField(c, e)
  else:
    if s.kind == skConverter: addConverter(c, LazySym(sym: s))
    if hasPattern(s): addPattern(c, LazySym(sym: s))
  if s.owner != origin:
    c.exportIndirections.incl((origin.id, s.id))

proc splitPragmas(c: PContext, n: PNode): (PNode, seq[TSpecialWord]) =
  case n.kind
  of nkPragmaExpr:
    if n.len == 2 and n[1].kind == nkPragma:
      result[0] = n[0]
      for ni in n[1]:
        case ni.kind
        of nkIdent:
          result[1].add whichKeyword(ni.ident)
        else:
          globalReport(c.config, n.info, reportAst(rsemInvalidPragma, n))
    else:
      globalReport(c.config, n.info, reportAst(rsemInvalidPragma, n))
  else:
    result[0] = n
    if result[0].safeLen > 0:
      (result[0][^1], result[1]) = splitPragmas(c, result[0][^1])

proc importSymbol(c: PContext, n: PNode, fromMod: PSym; importSet: var IntSet): PSym =
  let (n, kws) = splitPragmas(c, n)
  if kws.len > 0:
    globalReport(c.config, n.info, reportSem(rsemUnexpectedPragma))

  let (ident, err) = lookups.considerQuotedIdent(c, n)
  if err.isNil:
    let s = someSym(c.graph, fromMod, ident)
    result = s # less typing below
    if s.isNil:
      result = errorUndeclaredIdentifierWithHint(c, n, ident.s)
    else:
      when false:
        if s.kind == skStub: loadStub(s)

      let multiImport = s.kind notin ExportableSymKinds or
                        s.kind in skProcKinds
        ## for an enumeration we have to add all identifiers

      if multiImport:
        # for a overloadable syms add all overloaded routines
        var
          it: ModuleIter
          e = initModuleIter(it, c.graph, fromMod, s.name)
        while e != nil:
          c.config.internalAssert(e.name.id == s.name.id, n.info,
                                  "importSymbol: 3")

          if s.kind in ExportableSymKinds:
            rawImportSymbol(c, e, fromMod, importSet)
          e = nextModuleIter(it, c.graph)
      else:
        rawImportSymbol(c, s, fromMod, importSet)
      if c.graph.onSymImport != nil:
        c.graph.onSymImport(c.graph, n.info, s, c.graph.usageSym, false)
  else:
    result =
      newSym(skError, ident, nextSymId(c.idgen), getCurrOwner(c), n.info)
    result.typ = c.errorType
    result.ast = err

proc addImport(c: PContext; im: sink ImportedModule) =
  for i in 0..high(c.imports):
    if c.imports[i].m == im.m:
      # we have already imported the module: Check which import
      # is more "powerful":
      case c.imports[i].mode
      of importAll: discard "already imported all symbols"
      of importSet:
        case im.mode
        of importAll, importExcept:
          # XXX: slightly wrong semantics for 'importExcept'...
          # But we should probably change the spec and disallow this case.
          c.imports[i] = im
        of importSet:
          # merge the import sets:
          c.imports[i].imported.incl im.imported
      of importExcept:
        case im.mode
        of importAll:
          c.imports[i] = im
        of importSet:
          discard
        of importExcept:
          var cut = initIntSet()
          # only exclude what is consistent between the two sets:
          for j in im.exceptSet:
            if j in c.imports[i].exceptSet:
              cut.incl j
          c.imports[i].exceptSet = cut
      return
  c.imports.add im

template addUnnamedIt(c: PContext, fromMod: PSym; filter: untyped) {.dirty.} =
  for it in mitems c.graph.ifaces[fromMod.position].converters:
    if filter:
      loadPackedSym(c.graph, it)
      if sfExported in it.sym.flags:
        addConverter(c, it)
  for it in mitems c.graph.ifaces[fromMod.position].patterns:
    if filter:
      loadPackedSym(c.graph, it)
      if sfExported in it.sym.flags:
        addPattern(c, it)
  for it in mitems c.graph.ifaces[fromMod.position].pureEnums:
    if filter:
      loadPackedSym(c.graph, it)
      importPureEnumFields(c, it.sym, it.sym.typ)

proc importAllSymbolsExcept(c: PContext, fromMod: PSym, exceptSet: IntSet) =
  c.addImport ImportedModule(m: fromMod, mode: importExcept, exceptSet: exceptSet)
  addUnnamedIt(c, fromMod, it.sym.id notin exceptSet)

proc importAllSymbols*(c: PContext, fromMod: PSym) =
  c.addImport ImportedModule(m: fromMod, mode: importAll)
  addUnnamedIt(c, fromMod, true)
  when false:
    var exceptSet: IntSet
    importAllSymbolsExcept(c, fromMod, exceptSet)

proc importModuleAs(c: PContext; n: PNode, realModule: PSym, importHidden: bool): PSym =
  result = realModule
  
  template createModuleAliasImpl(ident): untyped =
    createModuleAlias(realModule, nextSymId c.idgen, ident, n.info, c.config.options)
  
  if n.kind != nkImportAs:
    discard
  elif n.len != 2 or n[1].kind != nkIdent:
    result = errorSym(c, n,
                      c.config.newError(n, reportAst(
                        rsemExpectedIdentifier,
                        n[1],
                        str = "module alias must be an identifier")))
  elif n[1].ident.id != realModule.name.id:
    # some misguided guy will write 'import abc.foo as foo' ...
    result = createModuleAliasImpl(n[1].ident)
  
  if result == realModule:
    # avoids modifying `realModule`, see D20201209T194412 for `import {.all.}`
    result = createModuleAliasImpl(realModule.name)
  
  if importHidden:
    result.options.incl optImportHidden
  
  c.unusedImports.add((result, n.info))
  c.importModuleMap[result.id] = realModule.id

proc transformImportAs(c: PContext; n: PNode): tuple[node: PNode, importHidden: bool] =
  var ret: typeof(result)
  proc processPragma(n2: PNode): PNode =
    let (result2, kws) = splitPragmas(c, n2)
    result = result2
    for ai in kws:
      case ai
      of wImportHidden:
        ret.importHidden = true
      else:
        globalReport(c.config, n.info, reportAst(
          rsemInvalidPragma, n2,
          str = "invalid pragma, expected: " & ${wImportHidden}))

  if n.kind == nkInfix and
    (let ident = legacyConsiderQuotedIdent(c, n[0], nil); ident.s == "as"):
    ret.node = newNodeI(nkImportAs, n.info)
    ret.node.add n[1].processPragma
    ret.node.add n[2]
  else:
    ret.node = n.processPragma
  return ret

proc myImportModule(c: PContext, n: var PNode, importStmtResult: PNode): PSym =
  let transf = transformImportAs(c, n)
  n = transf.node
  let f = checkModuleName(c.config, n)
  if f != InvalidFileIdx:
    addImportFileDep(c, f)
    let L = c.graph.importStack.len
    let recursion = c.graph.importStack.find(f)
    c.graph.importStack.add f
    #echo "adding ", toFullPath(f), " at ", L+1
    if recursion >= 0:
      for i in recursion ..< L:
        c.recursiveDep.add((
          importer: toFullPath(c.config, c.graph.importStack[i]),
          importee: toFullPath(c.config, c.graph.importStack[i + 1])
        ))

    var realModule: PSym
    discard pushOptionEntry(c)
    realModule = c.graph.importModuleCallback(c.graph, c.module, f)
    result = importModuleAs(c, n, realModule, transf.importHidden)
    popOptionEntry(c)

    #echo "set back to ", L
    c.graph.importStack.setLen(L)
    # we cannot perform this check reliably because of
    # test: modules/import_in_config) # xxx is that still true?
    if not result.isError and realModule == c.module:
      result = errorSym(c, n,
                        c.config.newError(n, reportSym(
                          rsemCannotImportItself,
                          realModule)))

    if sfDeprecated in realModule.flags:
      # xxx: this is a hint, `localReport` doesn't communicate this well
      localReport(c.config, n.info, reportSym(rsemDeprecated, realModule))

    if c.graph.onSymImport != nil:
      c.graph.onSymImport(c.graph, n.info, result, c.graph.usageSym, false)

    importStmtResult.add:
      case result.kind
      of skError:
        n = result.ast # updates the passed in ref to the error node
        result.ast
      else:
        newSymNode(result, n.info)
    #newStrNode(toFullPath(c.config, f), n.info)

proc afterImport(c: PContext, m: PSym) =
  # fixes bug #17510, for re-exported symbols
  case m.kind
  of skModule:
    let realModuleId = c.importModuleMap[m.id]
    for s in allSyms(c.graph, m):
      if s.owner.id != realModuleId:
        c.exportIndirections.incl((m.id, s.id))
  else:
    discard

proc impMod(c: PContext; it: PNode; importStmtResult: PNode): PNode =
  result = it
  let
    m = myImportModule(c, result, importStmtResult)
    hasError = m.isError

  if m != nil:
    # ``addDecl`` needs to be done before ``importAllSymbols``!
    addDecl(c, m, result.info) # add symbol to symbol table of module
    importAllSymbols(c, m)
    afterImport(c, m)

  if hasError:
    result = c.config.wrapError(result)

proc evalImport*(c: PContext, n: PNode): PNode =
  checkMinSonsLen(n, 1, c.config)
  result = newNodeI(nkImportStmt, n.info)
  var hasError = false

  for it in n.sons:
    if it.kind == nkInfix and it.len == 3 and it[2].kind == nkBracket:
      let
        sep = it[0]
        dir = it[1]
        impTmpl =
          block:
            let t = newNodeI(nkInfix, it.info, 3)
            t[0] = sep
            t[1] = dir
            t
          ## `impTmpl` is copied/instanced in the loop below including setting
          ## the third child
      
      for x in it[2]:
        let imp = copyTree(impTmpl)

        # transform `a/b/[c as d]` to `/a/b/c as d`
        imp[2] =
          if x.kind == nkInfix and x[0].ident.s == "as":
            x[1]
          else:
            x

        hasError = impMod(c, imp, result).kind == nkError or hasError
    else:
      hasError = impMod(c, it, result).kind == nkError

  if hasError:
    result = c.config.wrapError(result)

proc evalFrom*(c: PContext, n: PNode): PNode =
  checkMinSonsLen(n, 2, c.config)
  result = newNodeI(nkImportStmt, n.info)
  let m = myImportModule(c, n[0], result)
  var hasError = m.isError
  if m != nil:
    n[0] = newSymNode(m)
    
    addDecl(c, m, n.info)               # add symbol to symbol table of module

    # xxx: a better approach might be to treat each import part in
    #      `from m import a, b` as `m.a` and `m.b` aka a fully qualified lookup
    #      this might clean-up all the error handling bits here
    var im = ImportedModule(m: m, mode: importSet, imported: initIntSet())
    for i in 1..<n.len:
      case n[i].kind
      of nkNilLit:
        discard # no op
      else:
        let imported = importSymbol(c, n[i], m, im.imported)
        case imported.kind
        of skError:
          hasError = true
          result[i] = imported.ast
        else:
          discard
    
    c.addImport im
    afterImport(c, m)

  if hasError:
    result = c.config.wrapError(result)

proc readExceptSet(c: PContext, n: PNode): IntSet =
  assert n.kind in {nkImportExceptStmt, nkExportExceptStmt}
  result = initIntSet()
  for i in 1..<n.len:
    let (ident, err) = lookups.considerQuotedIdent(c, n[i])
    if err.isNil:
      result.incl(ident.id)
    else:
      localReport(c.config, err)

proc evalImportExcept*(c: PContext, n: PNode): PNode =
  checkMinSonsLen(n, 2, c.config)
  result = newNodeI(nkImportStmt, n.info)
  let m = myImportModule(c, n[0], result)
  var hasError = m.isError

  if m != nil:
    n[0] = newSymNode(m)

    addDecl(c, m, n.info)               # add symbol to symbol table of module
    importAllSymbolsExcept(c, m, readExceptSet(c, n))
    afterImport(c, m)

  if hasError:
    result = c.config.wrapError(result)
