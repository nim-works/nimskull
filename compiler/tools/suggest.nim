#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This file implements features required for IDE support.
##
## Due to Nim's nature and the fact that ``system.nim`` is always imported,
## there are lots of potential symbols. Furthermore thanks to templates and
## macros even context based analysis does not help much: In a context like
## ``let x: |`` where a type has to follow, that type might be constructed from
## a template like ``extractField(MyObject, fieldName)``. We deal with this
## problem by smart sorting so that the likely symbols come first. This sorting
## is done this way:
##
## - If there is a prefix (foo|), symbols starting with this prefix come first.
## - If the prefix is part of the name (but the name doesn't start with it),
##   these symbols come second.
## - If we have a prefix, only symbols matching this prefix are returned and
##   nothing else.
## - If we have no prefix, consider the context. We currently distinguish
##   between type and non-type contexts.
## - Finally, sort matches by relevance. The relevance is determined by the
##   number of usages, so ``strutils.replace`` comes before
##   ``strutils.wordWrap``.
## - In any case, sorting also considers scoping information. Local variables
##   get high priority.


import
  std/[
    algorithm,
    intsets,
    parseutils,
    sets,
    strutils,
    tables,
  ],
  compiler/ast/[
    ast,
    astalgo,
    ast_parsed_types,
    ast_types,
    lexer,
    lineinfos,
    linter,
    parser,
    syntaxes,
    types,
    typesrenderer,
    wordrecg,
  ],
  compiler/front/[
    msgs,
    options,
  ],
  compiler/modules/[
    modules,
    modulegraphs,
  ],
  compiler/sem/[
    lookups,
    semdata,
    sigmatch,
  ],
  compiler/utils/[
    prefixmatches,
    astrepr,
    debugutils,
    pathutils
  ]



when defined(nimsuggest):
  import compiler/sem/passes, compiler/utils/pathutils # importer

const
  sep = '\t'


template origModuleName(m: PSym): string = m.name.s

proc findDocComment(n: PNode): PNode =
  if n == nil: return nil
  if n.comment.len > 0: return n
  if n.kind in {nkStmtList, nkStmtListExpr, nkObjectTy, nkRecList} and n.len > 0:
    result = findDocComment(n[0])
    if result != nil: return
    if n.len > 1:
      result = findDocComment(n[1])
  elif n.kind in {nkAsgn, nkFastAsgn} and n.len == 2:
    result = findDocComment(n[1])

proc extractDocComment(g: ModuleGraph; s: PSym): string =
  var n = findDocComment(s.ast)
  if n.isNil and s.kind in routineKinds and s.ast != nil:
    n = findDocComment(getBody(g, s))
  if not n.isNil:
    result = n.comment.replace("\n##", "\n").strip
  else:
    result = ""

proc cmpSuggestions(a, b: Suggest): int =
  template cf(field) {.dirty.} =
    result = b.field.int - a.field.int
    if result != 0: return result

  cf prefix
  cf contextFits
  cf scope
  # when the first type matches, it's better when it's a generic match:
  cf quality
  cf localUsages
  cf globalUsages
  # if all is equal, sort alphabetically for deterministic output,
  # independent of hashing order:
  result = cmp(a.name[], b.name[])

proc getTokenLenFromSource(conf: ConfigRef; ident: string; info: TLineInfo): int =
  let
    line = sourceLine(conf, info)
    column = toColumn(info)

  proc isOpeningBacktick(col: int): bool =
    if col >= 0 and col < line.len:
      if line[col] == '`':
        not isOpeningBacktick(col - 1)
      else:
        isOpeningBacktick(col - 1)
    else:
      false

  if column > line.len:
    result = 0
  elif column > 0 and line[column - 1] == '`' and isOpeningBacktick(column - 1):
    result = skipUntil(line, '`', column)
    if cmpIgnoreStyle(line[column..column + result - 1], ident) != 0:
      result = 0
  elif ident[0] in linter.Letters and ident[^1] != '=':
    result = identLen(line, column)
    if cmpIgnoreStyle(line[column..column + result - 1], ident) != 0:
      result = 0
  else:
    var sourceIdent: string
    result = parseWhile(line, sourceIdent,
                        OpChars + {'[', '(', '{', ']', ')', '}'}, column)
    if ident[^1] == '=' and ident[0] in linter.Letters:
      if sourceIdent != "=":
        result = 0
    elif sourceIdent.len > ident.len and sourceIdent[0..ident.high] == ident:
      result = ident.len
    elif sourceIdent != ident:
      result = 0

proc symToSuggest(g: ModuleGraph; s: PSym, isLocal: bool, section: IdeCmd, info: TLineInfo;
                  quality: range[0..100]; prefix: PrefixMatch;
                  inTypeContext: bool; scope: int;
                  useSuppliedInfo = false): Suggest =
  new(result)
  result.section = section
  result.quality = quality
  result.isGlobal = sfGlobal in s.flags
  result.prefix = prefix
  if section in {ideSug, ideCon}:
    result.contextFits = inTypeContext == (s.kind in {skType, skGenericParam})
  result.scope = scope
  result.name = addr s.name.s
  when defined(nimsuggest):
    if section in {ideSug, ideCon}:
      result.globalUsages = s.allUsages.len
      var c = 0
      for u in s.allUsages:
        if u.fileIndex == info.fileIndex: inc c
      result.localUsages = c
  result.symkind = byte s.kind
  result.qualifiedPath = @[]
  if not isLocal and s.kind != skModule:
    let ow = s.owner
    if ow != nil and ow.kind != skModule and ow.owner != nil:
      let ow2 = ow.owner
      result.qualifiedPath.add(ow2.origModuleName)
    if ow != nil:
      result.qualifiedPath.add(ow.origModuleName)
  if s.name.s[0] in OpChars + {'[', '{', '('} or
      s.name.id in ord(wAddr)..ord(wYield):
    result.qualifiedPath.add('`' & s.name.s & '`')
  else:
    result.qualifiedPath.add(s.name.s)

  if s.typ != nil:
    result.forth = typeToString(s.typ)
  else:
    result.forth = ""
  when defined(nimsuggest) and not defined(noDocgen) and not defined(leanCompiler):
    if section != ideHighlight:
      result.doc = extractDocComment(g, s)
  let infox =
    if useSuppliedInfo or section in {ideUse, ideHighlight, ideOutline}:
      info
    else:
      s.info
  result.filePath = toFullPath(g.config, infox)
  result.line = toLinenumber(infox)
  result.column = toColumn(infox)
  result.tokenLen = if section != ideHighlight:
                      s.name.s.len
                    else:
                      getTokenLenFromSource(g.config, s.name.s, infox)

proc `$`*(suggest: Suggest): string =
  result = $suggest.section
  result.add(sep)
  if suggest.section == ideHighlight:
    if suggest.symkind.TSymKind == skVar and suggest.isGlobal:
      result.add("skGlobalVar")
    elif suggest.symkind.TSymKind == skLet and suggest.isGlobal:
      result.add("skGlobalLet")
    else:
      result.add($suggest.symkind.TSymKind)
    result.add(sep)
    result.add($suggest.line)
    result.add(sep)
    result.add($suggest.column)
    result.add(sep)
    result.add($suggest.tokenLen)
  else:
    result.add($suggest.symkind.TSymKind)
    result.add(sep)
    if suggest.qualifiedPath.len != 0:
      result.add(suggest.qualifiedPath.join("."))
    result.add(sep)
    result.add(suggest.forth)
    result.add(sep)
    result.add(suggest.filePath)
    result.add(sep)
    result.add($suggest.line)
    result.add(sep)
    result.add($suggest.column)
    result.add(sep)
    when defined(nimsuggest) and not defined(noDocgen) and not defined(leanCompiler):
      result.add(suggest.doc.escape)
    result.add(sep)
    result.add($suggest.quality)
    if suggest.section == ideSug:
      result.add(sep)
      result.add($suggest.prefix)

proc suggestResult(conf: ConfigRef; s: Suggest) =
  if not isNil(conf.suggestionResultHook):
    conf.suggestionResultHook(s)

proc produceOutput(a: var Suggestions; conf: ConfigRef) =
  if conf.ideCmd in {ideSug, ideCon}:
    a.sort cmpSuggestions
  when defined(debug):
    # debug code
    writeStackTrace()
  if a.len > conf.suggestMaxResults: a.setLen(conf.suggestMaxResults)
  if not isNil(conf.suggestionResultHook):
    for s in a:
      conf.suggestionResultHook(s)

proc filterSym(s: PSym; prefix: PNode; res: var PrefixMatch): bool {.inline.} =
  proc prefixMatch(s: PSym; n: PNode): PrefixMatch =
    case n.kind
    of nkIdent: result = n.ident.s.prefixMatch(s.name.s)
    of nkSym: result = n.sym.name.s.prefixMatch(s.name.s)
    of nkOpenSymChoice, nkClosedSymChoice, nkAccQuoted:
      if n.len > 0:
        result = prefixMatch(s, n[0])
    else: discard
  if s.kind != skModule:
    if prefix != nil:
      res = prefixMatch(s, prefix)
      result = res != PrefixMatch.None
    else:
      result = true

proc filterSymNoOpr(s: PSym; prefix: PNode; res: var PrefixMatch): bool {.inline.} =
  result = filterSym(s, prefix, res) and s.name.s[0] in lexer.SymChars and
     not isKeyword(s.name)

proc getQuality(s: PSym): range[0..100] =
  result = 100
  if s.typ != nil and s.typ.len > 1:
    var exp = s.typ[1].skipTypes({tyGenericInst, tyVar, tyLent, tyAlias, tySink})
    if exp.kind == tyVarargs: exp = elemType(exp)
    if exp.kind in {tyUntyped, tyTyped, tyGenericParam, tyAnything}: result = 50

  # penalize deprecated symbols
  if sfDeprecated in s.flags:
    result = result - 5

proc suggestField(c: PContext, s: PSym; f: PNode; info: TLineInfo; outputs: var Suggestions) =
  var pm: PrefixMatch
  if filterSym(s, f, pm) and fieldVisible(c, s):
    outputs.add(symToSuggest(c.graph, s, isLocal=true, ideSug, info,
                              s.getQuality, pm, c.inTypeContext > 0, 0))

template wholeSymTab(cond, section: untyped) {.dirty.} =
  for (item, scopeN, isLocal) in allSyms(c):
    let it = item
    var pm: PrefixMatch
    if cond:
      outputs.add(symToSuggest(c.graph, it, isLocal = isLocal, section, info, getQuality(it),
                                pm, c.inTypeContext > 0, scopeN))

proc suggestSymList(c: PContext, list, f: PNode; info: TLineInfo, outputs: var Suggestions) =
  for i in 0..<list.len:
    if list[i].kind == nkSym:
      suggestField(c, list[i].sym, f, info, outputs)

proc suggestObject(c: PContext, n, f: PNode; info: TLineInfo, outputs: var Suggestions) =
  case n.kind
  of nkRecList:
    for i in 0..<n.len: suggestObject(c, n[i], f, info, outputs)
  of nkRecCase:
    if n.len > 0:
      suggestObject(c, n[0], f, info, outputs)
      for i in 1..<n.len: suggestObject(c, lastSon(n[i]), f, info, outputs)
  of nkSym: suggestField(c, n.sym, f, info, outputs)
  else: discard

proc nameFits(c: PContext, s: PSym, n: PNode): bool =
  var op = if n.kind in nkCallKinds: n[0] else: n
  if op.kind in {nkOpenSymChoice, nkClosedSymChoice}: op = op[0]
  if op.kind == nkDotExpr: op = op[1]
  var opr: PIdent
  case op.kind
  of nkSym: opr = op.sym.name
  of nkIdent: opr = op.ident
  else: return false
  result = opr.id == s.name.id

proc argsFit(c: PContext, candidate: PSym, n: PNode): bool =
  case candidate.kind
  of OverloadableSyms:
    var m = newCallCandidate(c, candidate)
    sigmatch.partialMatch(c, n, m)
    result = m.state != csNoMatch
  else:
    result = false

proc suggestCall(c: PContext, n: PNode, outputs: var Suggestions) =
  let info = n.info
  wholeSymTab(filterSym(it, nil, pm) and nameFits(c, it, n) and argsFit(c, it, n),
              ideCon)

proc suggestVar(c: PContext, n: PNode, outputs: var Suggestions) =
  let info = n.info
  wholeSymTab(nameFits(c, it, n), ideCon)

proc typeFits(c: PContext, s: PSym, firstArg: PType): bool {.inline.} =
  if s.typ != nil and s.typ.len > 1 and s.typ[1] != nil:
    # special rule: if system and some weird generic match via 'tyUntyped'
    # or 'tyGenericParam' we won't list it either to reduce the noise (nobody
    # wants 'system.`-|` as suggestion
    let m = s.getModule()
    if m != nil and sfSystemModule in m.flags:
      if s.kind == skType: return
      var exp = s.typ[1].skipTypes({tyGenericInst, tyVar, tyLent, tyAlias, tySink})
      if exp.kind == tyVarargs: exp = elemType(exp)
      if exp.kind in {tyUntyped, tyTyped, tyGenericParam, tyAnything}: return
    result = sigmatch.argtypeMatches(c, s.typ[1], firstArg)

proc suggestOperations(c: PContext, n, f: PNode, typ: PType, outputs: var Suggestions) =
  assert typ != nil
  let info = n.info
  wholeSymTab(filterSymNoOpr(it, f, pm) and typeFits(c, it, typ), ideSug)

proc suggestEverything(c: PContext, n, f: PNode, outputs: var Suggestions) =
  # do not produce too many symbols:
  for (it, scopeN, isLocal) in allSyms(c):
    var pm: PrefixMatch
    if filterSym(it, f, pm):
      outputs.add(symToSuggest(c.graph, it, isLocal = isLocal, ideSug, n.info,
                               it.getQuality, pm, c.inTypeContext > 0, scopeN))

proc suggestFieldAccess(c: PContext, n, field: PNode, outputs: var Suggestions) =
  # special code that deals with ``myObj.``. `n` is NOT the nkDotExpr-node, but
  # ``myObj``.
  var typ = n.typ
  var pm: PrefixMatch
  when defined(nimsuggest):
    if n.kind == nkSym and n.sym.kind == skError:
      # consider 'foo.|' where 'foo' is some not imported module.
      let fullPath = findModule(c.config, n.sym.name.s, toFullPath(c.config, n.info))
      if fullPath.isEmpty:
        # error: no known module name:
        typ = nil
      else:
        let m = c.graph.importModuleCallback(c.graph, c.module, fileInfoIdx(c.config, fullPath))
        if m == nil: typ = nil
        else:
          for it in allSyms(c.graph, n.sym):
            if filterSym(it, field, pm):
              outputs.add(symToSuggest(c.graph, it, isLocal=false, ideSug,
                                        n.info, it.getQuality, pm,
                                        c.inTypeContext > 0, -100))
          outputs.add(symToSuggest(c.graph, m, isLocal=false, ideMod, n.info,
                                    100, PrefixMatch.None, c.inTypeContext > 0,
                                    -99))

  if typ == nil:
    # a module symbol has no type for example:
    if n.kind == nkSym and n.sym.kind == skModule:
      if n.sym == c.module:
        # all symbols accessible, because we are in the current module:
        for it in items(c.topLevelScope.symbols):
          if filterSym(it, field, pm):
            outputs.add(symToSuggest(c.graph, it, isLocal=false, ideSug,
                                      n.info, it.getQuality, pm,
                                      c.inTypeContext > 0, -99))
      else:
        for it in allSyms(c.graph, n.sym):
          if filterSym(it, field, pm):
            outputs.add(symToSuggest(c.graph, it, isLocal=false, ideSug,
                                      n.info, it.getQuality, pm,
                                      c.inTypeContext > 0, -99))
    else:
      # fallback:
      suggestEverything(c, n, field, outputs)
  else:
    let orig = typ
    typ = skipTypes(orig, {tyTypeDesc, tyGenericInst, tyVar, tyLent, tyPtr, tyRef, tyAlias, tySink})

    if typ.kind == tyEnum and n.kind == nkSym and n.sym.kind == skType:
      # look up if the identifier belongs to the enum:
      var t = typ
      while t != nil:
        suggestSymList(c, t.n, field, n.info, outputs)
        t = t[0]
    elif typ.kind == tyObject:
      var t = typ
      while true:
        suggestObject(c, t.n, field, n.info, outputs)
        if t[0] == nil: break
        t = skipTypes(t[0], skipPtrs)
    elif typ.kind == tyTuple and typ.n != nil:
      suggestSymList(c, typ.n, field, n.info, outputs)

    suggestOperations(c, n, field, orig, outputs)
    if typ != orig:
      suggestOperations(c, n, field, typ, outputs)

type
  TCheckPointResult* = enum
    cpNone, cpFuzzy, cpExact

proc inCheckpoint*(current, trackPos: TLineInfo): TCheckPointResult =
  if current.fileIndex == trackPos.fileIndex:
    if current.line == trackPos.line and
        abs(current.col-trackPos.col) < 4:
      return cpExact
    if current.line >= trackPos.line:
      return cpFuzzy

proc isTracked*(current, trackPos: TLineInfo, tokenLen: int): bool =
  if current.fileIndex == trackPos.fileIndex and 
     current.line == trackPos.line:
    let col = trackPos.col
    if col >= current.col and col <= current.col + tokenLen - 1:
      return true

proc findTrackedNode(n: PNode; trackPos: TLineInfo): PSym =
  if n.kind == nkSym:
    if isTracked(n.info, trackPos, n.sym.name.s.len): return n.sym
  else:
    for i in 0 ..< safeLen(n):
      let res = findTrackedNode(n[i], trackPos)
      if res != nil: return res

proc findTrackedSym*(g: ModuleGraph;): PSym =
  let m = g.getModule(g.config.m.trackPos.fileIndex)
  if m != nil and m.ast != nil:
    # xxx: the node finding should be specialized per symbol kind
    result = findTrackedNode(m.ast, g.config.m.trackPos)

proc getSymNode(node: ParsedNode): ParsedNode =
  case node.kind
  of pnkPostfix:    node[^1]
  of pnkPragmaExpr: getSymNode(node[0])
  of pnkIdent:      node
  else:             unreachable(node.kind)

proc pnkToSymKind(kind: ParsedNodeKind): TSymKind =
  result = skUnknown
  case kind
  of pnkConstSection, pnkConstDef: result = skConst
  of pnkLetSection: result = skLet
  of pnkVarSection: result = skVar
  of pnkProcDef: result = skProc
  of pnkFuncDef: result = skFunc
  of pnkMethodDef: result = skMethod
  of pnkConverterDef: result = skConverter
  of pnkIteratorDef: result = skIterator
  of pnkMacroDef: result = skMacro
  of pnkTemplateDef: result = skTemplate
  of pnkTypeDef, pnkTypeSection: result = skType
  else: discard

proc getName(node: ParsedNode): string =
  if node.kind == pnkIdent:
    result = node.startToken.ident.s
  elif node.kind == pnkAccQuoted:
    result = "`"
    for t in node.idents:
      result.add t.ident.s
    result.add "`"

proc processFlags(sug: Suggest; n: ParsedNode) =
  var
    identDeprecated: bool
    colonDeprecated: bool
  for s in n.sons:
    identDeprecated = s.kind == pnkIdent and getName(s) == "deprecated"
    colonDeprecated = s.kind == pnkExprColonExpr and getName(s[0]) == "deprecated" 
    if identDeprecated or colonDeprecated:
      sug.flags.incl SuggestFlag.deprecated

proc parsedNodeToSugget(n: ParsedNode; originKind: ParsedNodeKind; module: string): Suggest =
  if n.kind in {pnkError, pnkEmpty}: return
  if n.kind notin {pnkProcDef..pnkVarTuple}: return
  new(result)
  var
    token = getToken(n)
    name  = ""

  if n.kind in pnkRoutineDefs and n[pragmasPos].kind == pnkPragma:
    processFlags(result, n[pragmasPos])
  elif n[0].kind == pnkPragmaExpr and n[0][^1].kind == pnkPragma:
    processFlags(result, n[0][^1])

  if n.kind != pnkVarTuple:
    var node: ParsedNode = getSymNode(n[0])
    token = getToken(node)
    if node.kind != pnkError:
      name = getName(node)
      when false:
        if n.kind in pnkRoutineDefs and node.kind == pnkAccQuoted:
          let identsLen = n[paramsPos].sons.len
          for i in countup(1, identsLen - 1):
            name.add getName(n[paramsPos][i][1])
            if i != identsLen - 1:
              name.add ","
  else:
    name.add "("
    for c in n.items:
      if c.kind == pnkEmpty: break
      name.add getName(c) & ","
    name.add ")"

  if name != "":
    result.qualifiedPath = @[module, name]
  result.line = token.line.int
  result.column = token.col.int
  result.tokenLen = name.len
  result.symkind = byte pnkToSymKind(originKind)

proc outline*(graph: ModuleGraph; fileIdx: FileIndex) =
  let conf = graph.config
  var parser: Parser
  var sug: Suggest
  var parsedNode: ParsedNode
  let name = splitFile(AbsoluteFile toFilename(conf, fileIdx)).name

  const Sections = {pnkTypeSection, pnkConstSection, pnkLetSection, pnkVarSection}
  template suggestIt(parsedNode: ParsedNode; originKind: ParsedNodeKind) =
    sug = parsedNodeToSugget(parsedNode, originKind, name)
    if sug != nil:
      sug.filePath = toFullPath(conf, fileIdx)
      sug.forth = ""
      sug.section = ideOutline
      sug.quality = 100
      conf.suggestionResultHook(sug)

  if setupParser(parser, fileIdx, graph.cache, conf):
    while true:
      parsedNode = parser.parseTopLevelStmt()
      case parsedNode.kind
      of pnkEmpty:
        break
      of Sections:
        for node in parsedNode.sons:
          suggestIt(node, parsedNode.kind)
      else:
        suggestIt(parsedNode, parsedNode.kind)
    closeParser(parser)

proc executeCmd*(cmd: IdeCmd, file, dirtyfile: AbsoluteFile, line, col: int;
             graph: ModuleGraph) =
  ## executes the given suggest command, `cmd`, for a given `file`, at the
  ## position described by `line` and `col`umn. If `dirtyFile` is non-empty,
  ## then its contents are used as part of the analysis.
  let conf = graph.config
  conf.ideCmd = cmd
  var isKnownFile = true
  let dirtyIdx = fileInfoIdx(conf, file, isKnownFile)

  if dirtyfile.isEmpty: msgs.setDirtyFile(conf, dirtyIdx, AbsoluteFile"")
  else: msgs.setDirtyFile(conf, dirtyIdx, dirtyfile)
  if cmd == ideOutline: return
  conf.m.trackPos = newLineInfo(dirtyIdx, line, col)
  conf.m.trackPosAttached = false
  conf.errorCounter = 0
  if not isKnownFile:
    graph.compileProject(dirtyIdx)
  if conf.ideCmd in {ideUse, ideDus} and
      dirtyfile.isEmpty:
    discard "no need to recompile anything"
  else:
    let modIdx = graph.parentModule(dirtyIdx)
    graph.markDirty dirtyIdx
    graph.markClientsDirty dirtyIdx
    # partially recompiling the project means that that VM and JIT state
    # would become stale, which we prevent by discarding all of it:
    graph.vm = nil
    if conf.ideCmd != ideMod:
      if isKnownFile:
        graph.compileProject(modIdx)

when defined(nimsuggest):

  proc addNoDup(s: PSym; info: TLineInfo) =
    # ensure nothing gets too slow:
    if s.allUsages.len > 500: return
    for infoB in s.allUsages:
      if infoB == info: return
    s.allUsages.add(info)

when defined(nimsuggest):
  proc listUsages*(g: ModuleGraph; s: PSym) =
    for info in s.allUsages:
      let x = if info == s.info: ideDef else: ideUse
      suggestResult(g.config, symToSuggest(g, s, isLocal=false, x, info, 100, PrefixMatch.None, false, 0))

proc findDefinition(g: ModuleGraph; info: TLineInfo; s: PSym; usageSym: var PSym) =
  if s.isNil: return
  if isTracked(info, g.config.m.trackPos, s.name.s.len) or (s == usageSym and sfForward notin s.flags):
    suggestResult(g.config, symToSuggest(g, s, isLocal=false, ideDef, info, 100, PrefixMatch.None, false, 0, useSuppliedInfo = s == usageSym))
    if sfForward notin s.flags:
      suggestQuit()
    else:
      usageSym = s

proc ensureIdx[T](x: var T, y: int) =
  if x.len <= y: x.setLen(y+1)

proc ensureSeq[T](x: var seq[T]) =
  if x == nil: newSeq(x, 0)

proc suggestSym*(g: ModuleGraph; info: TLineInfo; s: PSym; usageSym: var PSym; isDecl=true) {.inline.} =
  ## misnamed: should be 'symDeclared'
  let conf = g.config
  when defined(nimsuggest):
    if s.allUsages.len == 0:
      s.allUsages = @[info]
    else:
      s.addNoDup(info)

    if conf.ideCmd == ideDef:
      findDefinition(g, info, s, usageSym)
    elif conf.ideCmd == ideDus and s != nil:
      if isTracked(info, conf.m.trackPos, s.name.s.len):
        suggestResult(conf, symToSuggest(g, s, isLocal=false, ideDef, info, 100, PrefixMatch.None, false, 0))
    elif conf.ideCmd == ideHighlight and info.fileIndex == conf.m.trackPos.fileIndex:
      suggestResult(conf, symToSuggest(g, s, isLocal=false, ideHighlight, info, 100, PrefixMatch.None, false, 0))

proc safeSemExpr*(c: PContext, n: PNode): PNode =
  # use only for idetools support!
  addInNimDebugUtils(c.config, "safeSemExpr", n, result)
  try:
    result = c.semExpr(c, n)
  except ERecoverableError:
    result = c.graph.emptyNode

proc sugExpr(c: PContext, n: PNode, outputs: var Suggestions) =
  if n.kind == nkDotExpr:
    var obj = safeSemExpr(c, n[0])
    # it can happen that errnously we have collected the fieldname
    # of the next line, so we check the 'field' is actually on the same
    # line as the object to prevent this from happening:
    let prefix = if n.len == 2 and n[1].info.line == n[0].info.line and
       not c.config.m.trackPosAttached: n[1] else: nil
    suggestFieldAccess(c, obj, prefix, outputs)

    #if optIdeDebug in gGlobalOptions:
    #  echo "expression ", renderTree(obj), " has type ", typeToString(obj.typ)
    #writeStackTrace()
  elif n.kind == nkIdent:
    let
      prefix = if c.config.m.trackPosAttached: nil else: n
      info = n.info
    wholeSymTab(filterSym(it, prefix, pm), ideSug)
  else:
    let prefix = if c.config.m.trackPosAttached: nil else: n
    suggestEverything(c, n, prefix, outputs)

proc suggestExprNoCheck*(c: PContext, n: PNode) =
  # This keeps semExpr() from coming here recursively:
  if c.compilesContextId > 0: return
  inc(c.compilesContextId)
  var outputs: Suggestions = @[]
  if c.config.ideCmd == ideSug:
    sugExpr(c, n, outputs)
  elif c.config.ideCmd == ideCon:
    if n.kind in nkCallKinds:
      var a = copyNode(n)
      var x = safeSemExpr(c, n[0])
      if x.kind == nkEmpty or x.typ == nil or x.isErrorLike: x = n[0]
      a.add x
      for i in 1..<n.len:
        # use as many typed arguments as possible:
        var x = safeSemExpr(c, n[i])
        if x.kind == nkEmpty or x.typ == nil or x.isErrorLike: break
        a.add x
      suggestCall(c, a, outputs)
    elif n.kind in nkIdentKinds:
      var x = safeSemExpr(c, n)
      if x.kind == nkEmpty or x.typ == nil or x.isErrorLike: x = n
      suggestVar(c, x, outputs)

  dec(c.compilesContextId)
  if outputs.len > 0 and c.config.ideCmd in {ideSug, ideCon, ideDef}:
    produceOutput(outputs, c.config)
    suggestQuit()

proc suggestExpr*(c: PContext, n: PNode) =
  if c.config.m.trackPos == n.info: suggestExprNoCheck(c, n)

proc suggestDecl*(c: PContext, n: PNode; s: PSym) =
  let attached = c.config.m.trackPosAttached
  if attached: inc(c.inTypeContext)
  defer:
    if attached: dec(c.inTypeContext)
  suggestExpr(c, n)

proc suggestStmt*(c: PContext, n: PNode) =
  suggestExpr(c, n)

proc suggestEnum*(c: PContext; n: PNode; t: PType) =
  var outputs: Suggestions = @[]
  suggestSymList(c, t.n, nil, n.info, outputs)
  produceOutput(outputs, c.config)
  if outputs.len > 0: suggestQuit()

proc suggestSentinel*(c: PContext) =
  if c.config.ideCmd != ideSug or c.module.position != c.config.m.trackPos.fileIndex.int32: return
  if c.compilesContextId > 0: return
  inc(c.compilesContextId)
  var outputs: Suggestions = @[]
  # suggest everything:
  for (it, scopeN, isLocal) in allSyms(c):
    var pm: PrefixMatch
    if filterSymNoOpr(it, nil, pm):
      outputs.add(symToSuggest(c.graph, it, isLocal = isLocal, ideSug,
          newLineInfo(c.config.m.trackPos.fileIndex, 0, -1), it.getQuality,
          PrefixMatch.None, false, scopeN))

  dec(c.compilesContextId)
  produceOutput(outputs, c.config)
