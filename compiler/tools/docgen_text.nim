## This module implements conversion from the raw RST AST into structured
## documentation entries with documentation information spliced directly
## in.

import
  ./docgen_types,
  front/[
    options,
    msgs
  ],
  packages/docutils/[
    rstast,
    rst
  ],
  utils/[
    pathutils,
  ],
  ast/[
    reports
  ],
  std/[
    sequtils,
    json,
    os,
    strutils
  ]

import std/options as std_options



const
  rnParagraphBody = {
    rnLeaf,
    rnEmphasis,
    rnStrongEmphasis
  }

iterator items(tree: PRstNode): PRstNode =
  for item in tree.sons:
    yield item

func `[]`*(tree: PRstNode, idx: int): PRstNode = tree.sons[idx]

func toJsonHook*(id: DocEntryId): JsonNode = %id.int
func fromJsonHook*(id: var DocEntryId, json: JsonNode) =
  id = json.getInt().DocEntryId()

func toJsonHook*(kind: DocTextTreeKind): JsonNode = %substr($kind, 3)
func fromJsonHook*(kind: var DocTextTreeKind, json: JsonNode) =
  kind = parseEnum[DocTextTreeKind]("dtt" & json.getStr())

proc conv(
    db: DocDb, id: DocEntryId, rst: PRstNode, conf: ConfigRef): DocTextTree =
  ## Convert RST ast into documentation text tree. This procedure also
  ## performs ast normalization - in RST `text` is a `Leaf` `text text` is
  ## an `Inner` wrapping seveal `Leaf` elements, and `a\n\nb` is ann inner
  ## wrapping wrapping two paragraphs. In normalized form words are never
  ## freestanding structure is more logical.

  proc aux(rst: PRstNode): DocTextTree =
    assert not rst.isNil()
    proc auxAll(rst: PRstNode): seq[DocTextTree] =
      assert not isNil(rst)
      for idx in 0 ..< rst.sons.len():
        let item = rst.sons[idx]
        assert not isNil(item), treeRepr(rst)
        result.add aux(item)

    proc auxTxt(rst: PRstNode): string =
      if rst.kind == rnLeaf:
        return rst.text

      else:
        for sub in rst:
          assert sub.kind in { rnLeaf }
          result.add sub.text

    proc auxParagraph(rst: PRstNode): DocTextTree =
      var textRun: string
      result = newTree(dttParagraph)
      for item in rst:
        if item.kind == rnLeaf:
          textRun.add item.text

        else:
          if textRun.len != 0:
            result.add newTree(dttText, textRun)
            textRun.setLen(0)

          result.add aux(item)

      if textRun.len != 0:
        result.add newTree(dttText, textRun)


    case rst.kind:
      of rnLeaf:
        result = newTree(dttText, rst.text)

      of rnInner:
        # Inner performs several different roles, most mostly used to
        # represent list of statements.
        if allIt(rst, it.kind in rnParagraphBody):
          result = auxParagraph(rst)

        elif allIt(rst, it.kind in {
          rnStrongEmphasis, rnEmphasis, rnInlineCode, rnLeaf
        }):
          # This text is parsed as a `rnInner` item without any paragraphs,
          # even though it clearly should be a paragraph
          #
          #   Built-in pointer type, use the `addr`
          #   operator to get a pointer to a variable.
          result = auxParagraph(rst)

        else:
          result = newTree(dttStmtList, rst.auxAll())

      of rnLineBlock:
        result = newTree(dttStmtList)
        for sub in rst:
          result.add aux(sub)

      of rnLineBlockItem:
        assert rst.len == 1
        result = aux(rst[0])

      of rnParagraph:       result = auxParagraph(rst)
      of rnOverline:        result = newTree(dttOverline, rst.auxAll())
      of rnDefList:         result = newTree(dttDefList, rst.auxAll())
      of rnBulletList:      result = newTree(dttUnorderedList, rst.auxAll())
      of rnDefName:         result = auxParagraph(rst)
      of rnEmphasis:        result = newTree(dttItalic, rst.auxAll())
      of rnStrongEmphasis:  result = newTree(dttBold, rst.auxAll())
      of rnTable:           result = newTree(dttTable, rst.auxAll())
      of rnTableRow:        result = newTree(dttTableRow, rst.auxAll())
      of rnTableDataCell:   result = newTree(dttTableDataCell, rst.auxAll())
      of rnTableHeaderCell: result = newTree(dttTableHeaderCell, rst.auxAll())
      of rnBlockQuote:      result = newTree(dttBlockQuote, rst.auxAll())
      of rnBulletItem:
        result = newTree(dttListItem, newTree(dttEmpty), auxParagraph(rst[0]))

      of rnSubstitutionReferences:
        result = newTree(dttSubstitute, auxTxt(rst))

      of rnDefItem:
        result = newTree(dttListItem, aux(rst[0]), aux(rst[1]))

      of rnEnumItem:        result = newTree(
        dttListItem, newTree(dttEmpty), auxParagraph(rst))

      of rnFieldBody: result = auxParagraph(rst)

      of rnOptionGroup: result = newTree(dttText, auxTxt(rst))

      of rnDescription: result = auxParagraph(rst)

      of rnOptionList:
        result = newTree(dttOptionsList)
        for item in rst:
          result.add newTree(dttListItem, aux(item[0]), aux(item[1]))

      of rnFieldList:
        result = newTree(dttDefinitionList)
        for item in rst:
          result.add newTree(dttListItem, aux(item[0]), aux(item[1]))

      of rnFieldName: result = newTree(dttText, auxTxt(rst))
      of rnSmiley: result = newTree(dttEmoji, rst.text)

      of rnAdmonition:
        result = newTree(dttAdmonition, aux(rst[2]))
        result.adType = rst.adType

      of rnEnumList:
        result = newTree(dttOrderedList, auxAll(rst))

      of rnDefBody:
        result = aux(rst[0])

      of rnStandaloneHyperlink:
        result = newTree(
          dttLink, newTree(dttEmpty),
          newTree(dttUrl, auxTxt(rst[0])))

      of rnRef:
        result = newTree(
          dttLink,
          newTree(dttEmpty),
          newTree(dttSubtreeLink, auxTxt(rst)))

      of rnInternalRef:
        result = newTree(
          dttLink,
          aux(rst[0]),
          newTree(dttSubtreeLink, auxTxt(rst[1])))

      of rnHeadline, rnMarkdownHeadline:
        result = newTree(dttHeadline, auxAll(rst))
        result.level = rst.level


      of rnHyperlink:
        result = newTree(dttLink, aux(rst[0]))
        let target = rst[1]

        # Reassemble link back, because for some reasin RST parser
        # fragments it into smaller piece.
        var txt: string
        if target.kind == rnLeaf:
          # For Markdown syntax compatibility
          txt = target.text

        else:
          for item in target.sons[1..^1]:
            txt.add item.text

        if txt[0] == '#':
          # (ab)use of links to refer to other documentable entries. TODO -
          # resolve signature links into documentable entries later.
          result.add newTree(dttSignature, txt)

        else:
          result.add newTree(dttUrl, txt)

      of rnIdx:
        var txt = auxTxt(rst)
        result = newTree(
          dttLink,
          newTree(dttText, txt),
          newTree(dttIndexLink, txt))

      of rnInlineLiteral:
        result = newTree(dttCode)
        result.code.add newTree(dttText, auxTxt(rst))

      of rnFootnoteGroup: result = newTree(dttStmtList, auxAll(rst))

      of rnFootnote:
        result = newTree(
          dttFootnote,
          newTree(dttText, auxTxt(rst[0])),
          auxParagraph(rst[1]))

        result.str = rst.anchor

      of rnFootnoteRef:
        result = newTree(
          dttLink,
          newTree(dttText, auxTxt(rst[0])),
          newTree(dttFootnoteRef, rst[1].text))

      of rnLiteralBlock:
        result = newTree(dttCode)
        result.isInline = false
        for item in rst:
          result.code.add aux(item)

      of rnDefaultRole:
        echo "default role had been reset"
        result = newTree(dttEmpty)

      of rnDirective, rnTransition:
        {.warning: "[FIXME]".}
        result = newTree(dttEmpty)

      of rnContents:
        result = newTree(dttContents)

      of rnDirArg:
        result = newTree(dttDirArg, auxTxt(rst))

      of rnRawHtml:
        result = newTree(dttPass, aux(rst[0]), aux(rst[2]))

      of rnCodeFragment, rnUnknownRole:
        assert rst.len == 2
        result = newTree(dttCode, auxAll(rst[0]))
        let interp = auxTxt(rst[1])
        # result.interpretation = some interp

        # FIXME TODO add interpretation field (`interpretation: string` in
        # the `dttCode` definition) - JSON `fromJson` cannot handle missing
        # fields, and recompiling db for the whole compiler takes too much
        # time, so I will return to this sometimes later.

      of rnInlineCode, rnCodeBlock:
        let body = rst[2]
        # echo rst.treeRepr()
        if 0 < rst[0].len:
          case rst[0][0].text:
            else:
              result = newTree(dttCode)
              result.lang = rst[0][0].text
              for item in body:
                result.code.add aux(item)

              result.isInline = true

        else:
          result = newTree(dttCode)
          result.lang = "nim"
          result.isInline = false
          for item in body:
            result.code.add aux(item)

        if not rst[1].isNil():
          discard

      else:
        if not id.isNil() and db[id].location.isSome():
          echo "location"
          let loc = db[id].location.get()
          echo db $ loc
          echo conf[db[loc].file].fullPath

        echo "----------------------------------"
        echo rst.treeRepr()
        echo "----------------------------------"
        assert false
        return newTree(dttEmpty)

  result = aux(rst)
  if result.kind == dttParagraph:
    result = newTree(dttStmtList, result)

proc unparseComment*(
    db: var DocDb, id: DocEntryId,
    doc: var DocText, conf: ConfigRef,
    inFileIndex: ast_types.FileIndex = ast_types.FileIndex(0)
  ) =
  if doc.isRunnable:
    return

  let (file, line, col) =
    if not id.isNil() and db[id].location.isSome():
      # FIXME store location of the documentation comments in the `DocText`
      # instead.
      let loc = db[id].location.get()
      (conf.toMsgFilename(db[loc].file), db[loc].line, db[loc].column.a)

    elif inFileIndex.int != 0:
      (conf.toMsgFilename(inFileIndex), 0, 0)

    else:
      ("", 0, 0)

  # echo file, ":", line, ":", col
  var pdb = addr db
  proc compilerMsgHandler(
      filename: string, line, col: int,
      msgKind: rst.MsgKind, arg: string
    ) {.gcsafe.} =
    # translate msg kind:
    {.gcsafe.}:
      return
      localReport(conf, newLineInfo(
        conf, AbsoluteFile filename, line, col), BackendReport(
          msg: arg,
          kind: case msgKind:
            of meCannotOpenFile:          rbackRstCannotOpenFile
            of meExpected:                rbackRstExpected
            of meGridTableNotImplemented: rbackRstGridTableNotImplemented
            of meMarkdownIllformedTable:  rbackRstMarkdownIllformedTable
            of meNewSectionExpected:      rbackRstNewSectionExpected
            of meGeneralParseError:       rbackRstGeneralParseError
            of meInvalidDirective:        rbackRstInvalidDirective
            of meInvalidField:            rbackRstInvalidField
            of meFootnoteMismatch:        rbackRstFootnoteMismatch
            of mwRedefinitionOfLabel:     rbackRstRedefinitionOfLabel
            of mwUnknownSubstitution:     rbackRstUnknownSubstitution
            of mwBrokenLink:              rbackRstBrokenLink
            of mwUnsupportedLanguage:     rbackRstUnsupportedLanguage
            of mwUnsupportedField:        rbackRstUnsupportedField
            of mwRstStyle:                rbackRstRstStyle
      ))

  proc docgenFindFile(s: string): string {.gcsafe.} =
    result = options.findFile(conf, s).string
    let fileIndex = if id.isNil():
                      inFileIndex

                    else:
                      pdb[][pdb[][id].location.get()].file

    if result.len == 0:
      result = conf[fileIndex].fullPath.string.parentDir() / s
      if not fileExists(result): result = ""

  let (ast, filenames, hasToc) = rstParse(doc.text, file, line, col, {
    roNimFile,
    roSupportMarkdown,
    roSupportRawDirective,
    roPreferMarkdown,
    roSupportSmilies
  }, msgHandler = compilerMsgHandler, findFile = docgenFindFile)

  doc.tree = db.conv(id, ast, conf)

proc unparseComments*(db: var DocDb, conf: ConfigRef) =
  ## Unparse all documentation comments in database and update content for
  ## associated entries.
  for id, entry in mpairs(db.entries):
    for doc in entry.docs:
      unparseComment(db, id, db[doc], conf)

proc unparseExtraDocs*(db: var DocDb, conf: ConfigRef, dir: AbsoluteDir) =
  ## Unparse extra documentation, recursively searching for `.rst` files in
  ## the `dir`
  for file in walkDirRec(dir.string):
    if file.endsWith(".rst"):
      var known = false
      let idx = conf.fileInfoIdx(AbsoluteFile(file), known)
      var doc =  DocText(
        text: readFile(file.string),
        location: db.add DocLocation(file: idx)
      )

      echo file
      unparseComment(db, EmptyDocEntryId, doc, conf, inFileIndex = idx)

      discard db.add(doc)
