import
  ./docgen_types,
  ./docgen_ast_aux,
  ast/[
    ast,
    renderer,
    lineinfos,
    types,
  ],
  sem/[
    sighashes,
  ],
  modules/[
    modulegraphs
  ],
  utils/[
    astrepr
  ],
  std/[
    strutils,
    options,
    hashes,
    tables,
  ]

import front/options as compiler_options

proc infoLocation*(info: TLineInfo): DocLocation =
  DocLocation(
    file: info.fileIndex,
    line: info.line.int,
    column: info.col.int..info.col.int
  )

proc nodeLocation*(node: PNode): DocLocation =
  result = infoLocation(node.info)
  if node.kind == nkAccQuoted:
    result.column.b += len($node) - 1
  else:
    result.column.b += len($node) - 1

proc nodeLocation*(node: PSym): DocLocation =
  result = infoLocation(node.info)
  result.column.b += len(node.name.s) - 1

const
  nkStringKinds*  = nkStrKinds
  nkFloatKinds*   = { nkFloatLit .. nkFloat128Lit }
  nkLiteralKinds* = nkStrKinds + nkIntKinds + nkFloatKinds + {nkNilLit}
  nkTokenKinds*   = nkLiteralKinds + {nkIdent, nkSym}

  nkProcDeclKinds* = {
    nkProcDef,
    nkFuncDef,
    nkIteratorDef,
    nkTemplateDef,
    nkMacroDef,
    nkMethodDef,
    nkConverterDef
  }

  nkStmtBlockKinds* = {
    nkIfExpr,
    nkIfStmt,
    nkWhenStmt,
    nkWhenExpr,
    nkForStmt,
    nkBlockStmt
  }

  nkIdentDeclKinds* = {
    nkLetSection,
    nkVarSection,
    nkConstSection,
    nkIdentDefs
  }


  nkAllDeclKinds* = nkProcDeclKinds + nkIdentDeclKinds

  skProcDeclKinds* = {
    skProc,
    skTemplate,
    skMethod,
    skMacro,
    skIterator,
    skConverter,
    skFunc
  }


func approxLoc*(s: PSym): ApproximateSymbolLocation =
  (s.name.id, s.info.fileIndex.int, s.info.line.int, s.info.col.int)


func approxLoc*(node: PNode): ApproximateSymbolLocation =
  if node.kind == nkIdent:
    return (
      node.ident.id,
      node.info.fileIndex.int,
      node.info.line.int,
      node.info.col.int
    )

  else:
    return node.sym.approxLoc()

func getHashdata(s: PSym): auto =
  let (a, b, c, d) = approxLoc(s)
  return (s.kind, a, b, c, d)


proc hash*(s: PSym): Hash =
  ## Has symbol using it's location definition data. Note - this is not a
  ## general-purpose hashing for `PSym`, it's behavior is specifically made
  ## to work for documentation generation, and most likely can't be reused
  ## elsewhere.
  hash(getHashdata(s))

proc `==`*(s1, s2: PSym): bool =
  getHashdata(s1) == getHashdata(s2)

proc hashdata*(s: PSym): string =
  let d = getHashdata(s)
  "[$#.$#.$#.$#.$#] = $#" % [$d[0], $d[1], $d[2], $d[3], $d[4], $hash(s)]



proc headSym(s: PSym): PSym = s
proc headSym(t: PType): PSym = t.sym.headSym()

type
  NodeOrSym* = object
    case isSym*: bool
      of true:
        sym*: PSym

      of false:
        node*: PNode

func wrapNode(node: PNode): NodeOrSym = NodeOrSym(isSym: false, node: node)
func wrapSym(sym: PSym): NodeOrSym = NodeOrSym(isSym: true, sym: sym)

proc headIdentOrSym(node: PNode): NodeOrSym =
  ## Return head identifier or symbol for an expression.
  case node.kind:
    of nkProcDeclKinds, nkDistinctTy, nkVarTy, nkAccQuoted,
       nkBracketExpr, nkTypeDef, nkPragmaExpr, nkPar, nkEnumFieldDef,
       nkIdentDefs, nkRecCase, nkCallStrLit:
      result = headIdentOrSym(node[0])

    of nkPostfix:
      result = headIdentOrSym(node[1])

    of nkCommand, nkCall, nkPrefix,
       nkHiddenStdConv, nkHiddenCallConv, nkInfix:
      if node.len == 0:
        result = wrapNode(nil)

      elif node.kind == nkCall:
        if node.len > 1 and node[1].kind == nkSym:
          result = headIdentOrSym(node[1])

        else:
          result = headIdentOrSym(node[0])

      else:
        result = headIdentOrSym(node[0])

    of nkDotExpr:
      result = headIdentOrSym(node[1])

    of nkSym:
      result = wrapNode(node)

    of nkRefTy, nkPtrTy:
      if node.len == 0:
        result = wrapNode(nil)

      else:
        result = headIdentOrSym(node[0])

    of nkEnumTy, nkProcTy, nkObjectTy, nkTupleTy,
       nkTupleClassTy, nkIteratorTy, nkOpenSymChoice,
       nkClosedSymChoice, nkCast, nkLambda, nkCurly,
       nkReturnStmt, nkRaiseStmt, nkBracket, nkEmpty,
       nkIfExpr:
      result = wrapNode(nil)

    of nkIdent:
      result = wrapNode(node)

    of nkCheckedFieldExpr:
      # First encountered during processing of `locks` file. Most likely
      # this is a `object.field` check
      debug node
      assert false
      result = wrapNode(nil)

    of nkType:
      result = wrapSym(node.typ.sym)

    of nkObjConstr:
      debug node
      assert false
      result = wrapNode(node)

    else:
      assert false, "TODO " & $node.kind

func isNil*(nos: NodeOrSym): bool =
  tern(nos.isSym, isNil(nos.sym), isNil(nos.node))

func hasSym*(nos: NodeOrSym): bool =
  nos.isSym or nos.node.kind == nkSym

func getSym*(nos: NodeOrSym): PSym = tern(nos.isSym, nos.sym, nos.node.sym)

proc headSym*(node: PNode): PSym =
  let n = node.headIdentOrSym()
  if isNil(n) or not n.hasSym():
    result = nil

  else:
    result = n.getSym()

proc addSigmap*(db: var DocDb, sym: PSym, entry: DocEntryId) =
  if not isNil(sym):
    db.sigmap[sym] = entry

proc addSigmap*(db: var DocDb, node: PNode, entry: DocEntryId) =
  ## Add mapping between specific symbol and documentable entry. Symbol
  ## node is retrived from the ast.
  let id = headIdentOrSym(node)
  if id.hasSym():
    db.addSigmap(id.getSym(), entry)

  else:
    db.locationSigmap[approxLoc(id.node)] = entry

proc newDocEntry*(
    db: var DocDb, kind: DocEntryKind, name: PNode | PSym,
    context: DocDeclarationContext = DocDeclarationContext()
  ): DocEntryId =
  ## Construct new documentable entry using symbol or identifier node to
  ## get name, location. If node is a symbol also updates sigmap with new
  ## documentable entry.
  when name is PNode:
    assert name.kind in {nkSym, nkIdent, nkAccQuoted}, $name.kind

  result = db.newDocEntry(kind, name.getSName(), context)
  db[result].location = some db.add(name.nodeLocation())
  db.addSigmap(name, result)


proc newDocEntry*(
    db: var DocDb,
    parent: DocEntryId, kind: DocEntryKind, name: PNode | PSym,
    context: DocDeclarationContext = DocDeclarationContext()
  ): DocEntryId =
  result = db.newDocEntry(kind, name, context)
  db[result].parent = some parent
  db[parent].nested.add result

proc contains*(db: DocDb, ntype: PType | PNode | PSym): bool =
  let sym = ntype.headSym()
  return not sym.isNil() and sym in db.sigmap

proc contains*(db: DocDb, nos: NodeOrSym): bool =
  ## Check if symbol or ident node is stored in the documentation database
  nos.hasSym() and tern(nos.isSym, nos.sym in db, nos.node in db)

proc approxLoc*(nos: NodeOrSym): ApproximateSymbolLocation =
  tern(nos.isSym, nos.sym.approxLoc(), nos.node.approxLoc())

proc approxContains*(db: DocDb, it: PSym): bool =
  ## TEMP HACK Check if target has been registered inthe db in any capacity
  ## - either properly or via identifier location.
  return it in db or it.approxLoc() in db.locationSigmap


proc approxContains*(db: DocDb, it: PNode): bool =
  ## TEMP HACK Check if target has been registered inthe db in any capacity
  ## - either properly or via identifier location.
  let it = headIdentOrSym(it)
  return it in db or it.approxLoc() in db.locationSigmap

proc `[]`*(db: var DocDb, sym: PSym): DocEntryId =
  ## Return documentable entry ID associated with given symbol. TEMP HACK:
  ## patch DB if this is a new encounter of the documenable entry that was
  ## previously registered from unchecked identifier.
  assert not isNil(sym)
  if sym in db:
    return db.sigmap[sym]

  elif ((let loc = sym.approxLoc(); loc in db.locationSigmap)):
    # Symbol approximate definition location was registered in the location
    # sigmap - patching DB
    db.addSigmap(sym, db.locationSigmap[loc])
    return db.sigmap[sym]

  else:
    assert false, "no doc entry for node $# symdata was $# (in table $#)" % [
      $treeRepr(nil, sym),
      hashdata(sym),
      $(sym in db.sigmap)
    ]


proc `[]`*(db: var DocDb, ntype: PNode): DocEntryId =
  ## Return documentable entry ID associated with type/node/symbol. TEMP
  ## HACK: patch DB if this is a new encounter of the documenable entry
  ## that was previously registered from unchecked identifier.
  let head = headIdentOrSym(ntype)
  if head in db:
    return db.sigmap[head.getSym()]

  else:
    let loc = head.approxLoc()
    if loc in db.locationSigmap:
      # Location was registered in the documentation sigmap, maybe need to
      # update entries.
      if head.hasSym():
        # Has symbol, updating
        let sym = head.getSym()
        db.addSigmap(sym, db.locationSigmap[loc])
        return db[sym]

      else:
        # No symbol (I assume this code is unreachable, but right now I'm
        # not sure if this is a hard guarantee or I just didn't hit some
        # unlucky path)
        return db.locationSigmap[loc]

    else:
      # No documentable entry found for the symbol at point, and there was
      # no matching approximate locations for a node.
      assert false, "no doc entry for node $# loc was $# (in table $#)" % [
        tern(head.isSym, $treeRepr(nil, head.sym), $treeRepr(nil, head.node)),
        $loc,
        $(loc in db.locationSigmap)
      ]





proc contains(s1, s2: DocLocation): bool =
  s1.line == s2.line and
  s1.column.a <= s2.column.a and s2.column.b <= s1.column.b

func `[]`*[R1, R2](slice: DocLocation, split: HSlice[R1, R2]): DocLocation =
  result = slice
  when R1 is BackwardsIndex:
    result.column.a = result.column.b - split.a.int

  else:
    result.column.a = result.column.a + split.a

  when R2 is BackwardsIndex:
    result.column.b = result.column.b - split.b.int

  else:
    result.column.a = result.column.a + split.b

func `-=`*(slice: var DocLocation, shift: int) =
  slice.column.a -= shift
  slice.column.b -= shift

proc nodeExprSlice(node: PNode): DocLocation =
  ## Return source code slice for `node`.
  result = nodeLocation(node)
  case node.kind:
    of nkDotExpr:
      result -= len($node[0]) - 1

    else:
      discard


proc subslice*(parent, node: PNode): DocLocation =
  let main = parent.nodeExprSlice()
  case parent.kind:
    of nkDotExpr: result = main[^(len($node)) .. ^1]
    of nkExprColonExpr:
      result.line = main.line
      result.column.b = main.column.a
      result.column.a = main.column.a - len($node)

    else:
      result = main

proc startPos*(node: PNode): TLineInfo =
  case node.kind:
    of nkTokenKinds:
      result = node.info

    of nkAccQuoted:
      result = node[0].startPos()
      result.col -= 1

    else:
      result = node[0].startPos()

proc finishPos*(node: PNode): TLineInfo =
  case node.kind:
    of nkTokenKinds:
      result = node.info
      result.col += len($node).int16 - 1

    of nkAccQuoted:
      result = finishPos(node[^1])
      result.col += 1

    else:
      if len(node) > 0:
        var idx = len(node) - 1
        while idx >= 0 and node[idx].kind in {nkEmpty}:
          dec idx

        if idx >= 0:
          result = node[idx].finishPos()

        else:
          result = node.info

      else:
        result = node.info

proc nodeExtent*(node: PNode): DocExtent =
  let (start, finish) = (startPos(node), finishPos(node))
  result.file = start.fileIndex
  result.start = (start.line.int, start.col.int)
  result.finish = (finish.line.int, finish.col.int)

