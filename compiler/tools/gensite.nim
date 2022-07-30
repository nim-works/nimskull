import
  nimja,
  ./docgen_types,
  ./docgen_sqlite,
  utils/[
    pathutils
  ],
  std/[
    os,
    strformat,
    strutils
  ],
  front/[
    options,
  ]

proc toHtml(db: DocDb, doc: DocTextId): string =
  let tree = db[doc].tree
  if not isNil(tree):
    var res = addr result
    template add(a: string): untyped =
      res[].add a


    proc aux(tree: DocTextTree) =
      case tree.kind:
        of dttStmtList:
          for sub in tree:
            aux(sub)

        of dttParagraph:
          add "<p>"
          for sub in tree:
            aux(sub)

          add "</p>"

        of dttItalic:
          add "<i>"
          for sub in tree:
            aux(sub)
          add "</i>"

        of dttAdmonition:
          add "<b>$#</b>" % tree.adType
          for sub in tree:
            aux(sub)

        of dttBold:
          add "<b>"
          for sub in tree:
            aux(sub)
          add "</b>"

        of dttText, dttUrl, dttSignature:
          add tree.str

        of dttIndexLink:
          add "[[index link]]"

        of dttCode:
          add "<code>"
          for it in tree.code:
            aux(it)
          add "</code>"

        of dttUnorderedList:
          add "<ul>"
          for item in tree:
            add "<li>"
            aux(item[1])
            add "</li>"

          add "</ul>"

        of dttDefList:
          add "<ul>"
          for item in tree:
            add "<li><b>"
            aux(item[0])
            add "</b> "
            aux(item[1])
            add "</li>"

          add "</ul>"

        of dttLink:
          add "<a href=\""
          aux(tree[0])
          add "\">"
          aux(tree[1])
          add "</a>"

        else:
          assert false, $tree.kind

    aux(tree)

proc docToHtml(db: DocDb, id: DocEntryId): string =
  for doc in db[id].docs:
    result.add db.toHtml(doc)
    result.add "\n"

iterator nested(db: DocDb, id: DocEntryId, kinds: set[DocEntryKind]): DocEntryId =
  for sub in db[id].nested:
    if db[sub].kind in kinds:
      yield sub

proc toHtml(db: DocDb, id: DocEntryId): string =
  case db[id].kind:
    of ndkBuiltin:
      compileTemplateFile(getScriptDir() / "templates/builtin.nwt")

    of ndkDistinctAlias:
      compileTemplateFile(getScriptDir() / "templates/distinct.nwt")

    of ndkEnum:
      compileTemplateFile(getScriptDir() / "templates/enum.nwt")

    of ndkProc, ndkMacro, ndkIterator, ndkTemplate, ndkFunc:
      compileTemplateFile(getScriptDir() / "templates/proc.nwt")

    else:
      return db$id


proc main(): void =
  let path = paramStr(1)
  var conf = ConfigRef()
  var db = DocDb()
  readSqlite(conf, db, AbsoluteFile(path))
  echo "done reading sqlite"
  for id, en  in db.entries:
    if db[id].kind == ndkModule:
      var result = ""
      echo db$id
      compileTemplateFile(getScriptDir() / "templates/module.nwt")
      writeFile(&"/tmp/{en.name}.html", result)

main()
