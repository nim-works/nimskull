import
  ast/[
    ast,
    lexer,
    renderer,
    trees
  ],
  front/[
    options
  ],
  utils/[
    astrepr,
  ],
  std/[
    sequtils,
    strutils,
    tables
  ],
  experimental/text_layouter,
  ./docgen_ast_aux

when not defined(useNodeIds):
  {.error: "Code renderer must be compiled with node ids. Compile with --define=useNodeIds".}

export text_layouter 

initBlockFormatDSL()

proc max(args: varargs[int]): int =
  for arg in args:
    if result < arg:
      result = arg

proc assertKind(node: PNode, kind: set[TNodeKind]) =
  assert node.kind in kind, $treeRepr(nil, node)

func `[]`(n: ast.PNode, sl: HSlice[int, BackwardsIndex]): seq[PNode] =
  var idx = sl.a
  let maxl = n.len - sl.b.int
  while sl.a <= idx and idx <= maxl:
    result.add n[idx]
    inc idx

func high(n: PNode): int = n.len - 1

proc `of`*[A: object or ref object or distinct; K: enum](item: A, kind: K | set[K]): bool =
  ## Check if @arg{item} has @arg{kind}
  when kind is set:
    item.kind in kind

  else:
    item.kind == kind

type
  NimFormatFlag* = enum
    nffAllowMultilineProcHead

    nffHorizontalPackedOf
    nffVerticalPackedOf

    nffAddIInfo

  NimFormatConf* = object
    flags*: set[NimFormatFlag]

  FormatStrStore* = ref object
    strs: seq[string]

func newStrStore*(): FormatStrStore = new(result)

const
  defaultNimFormatConf* = NimFormatConf(
    flags: {
      nffAllowMultilineProcHead,
      nffAddIInfo,
      nffHorizontalPackedOf,
      nffVerticalPackedOf
    }
  )

func contains*(conf: NimFormatConf, flag: NimFormatFlag): bool =
  flag in conf.flags

func contains*(conf: NimFormatConf, flags: set[NimFormatFlag]): bool =
  len(flags * conf.flags) == len(flags)


proc toLytBlock*(
  n: PNode, conf: NimFormatConf, fmtStore: var FormatStrStore): LytBlock

template `~`(node: PNode): untyped = toLytBlock(node, conf, fmtStore)

const
  NodesStr = LytStrIdMask(0)
  TokenStr = LytStrIdMask(1)
  LiterStr = LytStrIdMask(2)


proc toLytStr(str: LytStr): LytStr =
  str

proc toLytStr(count: int): LytStr =
  lytSpaces(count)

proc toLytStr(tok: TokType): LytStr =
  result.id = LytStrId(tok.int)
  result.len = len($tok) # todo optimize
  result.id.setMask(TokenStr)

proc toLytStr(node: PNode): LytStr =
  assert node.kind in {nkSym, nkIdent} + nkLiterals, $node.kind
  result.id = LytStrId(node.id)
  result.id.setMask(NodesStr)

proc toLytStr(fmtStore: var FormatStrStore, str: string): LytStr =
  result.id = toLytStrId(fmtStore.strs.len)
  fmtStore.strs.add str
  result.len = str.len
  result.id.setMask(LiterStr)

type
  NimFormatEventKind* = enum
    nimEvFormatNode
    nimEvFormatToken
    nimEvFormatNewline
    nimEvFormatSpaces
    nimEvFormatStr

  NimFormatEvent* = object
    case kind*: NimFormatEventKind
      of nimEvFormatSpaces:
        spaces*: int

      of nimEvFormatNewline:
        discard

      of nimEvFormatToken:
        token*: TokType

      of nimEvFormatNode:
        node*: int

      of nimEvFormatStr:
        str*: string


iterator nimFormatEvents*(
    lyt: Layout, fmtStore: FormatStrStore): NimFormatEvent =
  for ev in formatEvents(lyt):
    var event: NimFormatEvent
    case ev.kind:
      of layEvSpaces:
        event = NimFormatEvent(kind: nimEvFormatSpaces, spaces: ev.spaces)

      of layEvNewline:
        event = NimFormatEvent(kind: nimEvFormatNewline)

      of layEvStr:
        case getMask(ev.str.id).int:
          of TokenStr.int:
            event = NimFormatEvent(
              kind: nimEvFormatToken,
              token: TokType(ev.str.id.popMask()))

          of NodesStr.int:
            event = NimFormatEvent(
              kind: nimEvFormatNode,
              node: ev.str.id.popMask().int)

          of LiterStr.int:
            event = NimFormatEvent(
              kind: nimEvFormatStr,
              str: fmtStore.strs[ev.str.id.popMask().toIndex])

          else:
            assert false, "Malformed layout string mask: " & $ev.str.id

    yield event

proc lT(args: varargs[LytStr, toLytStr]): LytBlock =
  lT(lytStrSpan(@args))

proc lTX(args: varargs[LytStr, toLytStr]): LytStrSpan =
  lytStrSpan(args)

proc lI(node: LytBlock): LytBlock =
  lI(2, node)

proc lytInfix(
    n: PNode,
    conf: NimFormatConf,
    fmtStore: var FormatStrStore,
    spacing: bool = true
  ): LytBlock =

  if n.kind == nkInfix:
    if spacing:
      result = lH(
        lytInfix(n[1], conf, fmtStore),
        lT(1, n[0], 1),
        lytInfix(n[2], conf, fmtStore)
      )

    else:
      result = lH(
        lytInfix(n[1], conf, fmtStore),
        lT(n[0]),
        lytInfix(n[2], conf, fmtStore)
      )

  else:
    result = toLytBlock(n, conf, fmtStore)

proc lytFormalParams(
    n: PNode,
    vertical: bool,
    conf: NimFormatConf,
    fmtStore: var FormatStrStore
  ): LytBlock =

  assertKind(n, {nkFormalParams})
  result = tern(vertical, lV(), lH())
  let argPad =
    if vertical:
      n[1..^1].mapIt(len($it[0])).max() + 2

    else:
      0


  for idx, arg in n[1..^1]:
    var hor = lH(~arg[0], lT(tkColon, 1), ~arg[1])
    if not(arg[2] of nkEmpty):
      hor.add lT(1, tkEquals, 1)
      hor.add ~arg[2]

    if idx < n.high - 1:
      hor &= tern(vertical, lT(tkComma), lT(tkComma, 1))

    result.add hor

proc lytFormalReturnClose(
    n: PNode, 
    conf: NimFormatConf, 
    fmtStore: var FormatStrStore
  ): LytBlock =

  assertKind(n, {nkFormalParams})
  if n[0] of nkEmpty:
    lT(tkParRi)

  else:
    lH(lT(tkParRi, tkColon, 1), ~n[0])


proc lytDocComment(n: PNode, prefix: string = ""): LytBlock =
  # if n.comment.len > 0:
  #   result = lV()
  #   # for line in n.comment.split('\n'):
  #   #   result.add lT(prefix & "## " & line)

  # else:
  result = lE()

proc lytCsv(
    n: PNode | seq[PNode],
    vertical: bool,
    conf: NimFormatConf,
    fmtStore: var FormatStrStore
  ): LytBlock =

  result = tern(vertical, lV(), lH())
  if vertical:
    for idx, item in n:
      result.add tern(
        idx < len(n) - 1,
        lH(~item, lT(tkComma, 1)),
        ~item
      )

  else:
    for idx, item in n:
      result.add tern(
        idx > 0,
        lH(lT(tkComma, 1), ~item),
        ~item
      )

proc lytTypedefHead(
    n: PNode, conf: NimFormatConf, fmtStore: var FormatStrStore): LytBlock =

  if n[0] of nkPragmaExpr:
    lH(~n[0][0], ~n[1], lT(1), ~n[0][1])

  else:
    lH(~n[0], ~n[1])

const
  nkTypeAliasKinds = {
    nkIdent, nkPtrTy, nkRefTy, nkBracketExpr}


proc lytTypedef(
    n: PNode, conf: NimFormatConf, fmtStore: var FormatStrStore): LytBlock =
  var head = lytTypedefHead(n, conf, fmtStore)
  head.add lT(1, tkEquals, 1)

  case n[2].kind:
    of nkObjectTy:
      head.add lT(tkObject)
      var body: seq[seq[LytBlock]]
      var resBody = lV()

      template flush(): untyped =
        if 0 < len(body):
          resBody.add initAlignedGrid(
            body, [lAlignLeft, lAlignLeft, lAlignLeft])

          body = @[]

      for field in n[2][2]:
        case field.kind:
          of nkEmpty:
            discard

          of nkRecCase:
            flush()
            resBody.add lS()
            resBody.add ~field

          of nkIdentDefs:
            body.add @[
              lH(~field[0], lT(tkColon, 1)),
              ~field[1],
              lytDocComment(field, prefix = " ")]

          of nkCommentStmt:
            flush()
            resBody.add lytDocComment(field, prefix = "")

          else:
            failNode field

      flush()
      result = lV(head, lI(resBody))

    of nkEnumTy:
      head.add lT(tkEnum)
      var body: seq[seq[LytBlock]]
      for field in n[2]:
        case field.kind:
          of nkIdent:
            let f = ~field
            body.add @[f, lS(), lS()]

          of nkEnumFieldDef:
            body.add @[~field[0], lT(1, tkEquals, 1), ~field[1]]

          of nkEmpty:
            discard

          else:
            failNode field

        if field.kind != nkEmpty:
          body[^1].add lytDocComment(field, prefix = " ")

      result = lV(head, lI(initAlignedGrid(
        body, [lAlignLeft, lAlignCenter, lAlignLeft, lAlignLeft])))

    of nkTypeAliasKinds:
      head.add ~n[2]
      result = head

    of nkProcTy:
      result = lV(head, lI(~n[2]))

    of nkDistinctTy:
      result = lH(head, lT(tkDistinct, 1), ~n[2][0])

    else:
      failNode n[2]


proc toLytBlock*(
    n: PNode, conf: NimFormatConf, fmtStore: var FormatStrStore): LytBlock =

  case n.kind:
    of nkProcTy:
      result = lV(lH(lT(tkProc, tkParLe),
          lytFormalParams(n[0], true, conf, fmtStore),
          lytFormalReturnClose(n[0], conf, fmtStore),
          lT(1),
          ~n[1]))

    of nkAccQuoted:
      result = lH(lT(tkAccent))
      var body = false
      for arg in n:
        if body: result.add lT(1)
        body = true

        result.add ~arg

      result.add lT(tkAccent)

    of nkStmtListExpr:
      result = lH(~n[0])
      for sub in n[1..^1]:
        result.add lT(tkSemiColon, 1)
        result.add ~sub

    of nkProcDef, nkLambda, nkConverterDef,
       nkFuncDef, nkTemplateDef:
      result = lC()

      let kindName =
        case n.kind:
          of nkProcDef: toLytStr(tkProc)
          of nkConverterDef: toLytStr(tkConverter)
          of nkTemplateDef: toLytStr(tkTemplate)
          of nkLambda: EmptyLytStr
          of nkFuncDef: toLytStr(tkFunc)
          else: failNode n

      # proc q*(a: B): C {.d.} =
      #   e
      result.add lV(
        lH(lH(lT(kindName, 1), lS(), ~n[0], ~n[1], ~n[2], lT(tkParLe)),
          lytFormalParams(n[3], false, conf, fmtStore),
          lytFormalReturnClose(n[3], conf, fmtStore),
          if n[4] of nkEmpty: lS() else: lH(lT(1), ~n[4]),
          if n[6] of nkEmpty: lS() else: lT(1, tkEquals, 1)),
        lV(lytDocComment(n), lI(~n[6])),
        lS())


      if n[3].len > 1 and nffAllowMultilineProcHead in conf:
        # proc q*(
        #     a: B
        #   ): C {.d.} =
        #     e
        result.add lV(
          lH(lT(kindName, 1), lS(), ~n[0], ~n[1], ~n[2], lT(tkParLe)),
          lI(4, lytFormalParams(n[3], true, conf, fmtStore)),
          lH(lI(lytFormalReturnClose(n[3], conf, fmtStore)),
            if n[4] of nkEmpty: lS() else: lH(lT(1), ~n[4]),
            if n[6] of nkEmpty: lS() else: lT(1, tkEquals, 1)),
          lytDocComment(n),
          lI(~n[6]),
          lS())

    of nkStmtList:
      result = lV()
      var hadReal = false
      for sub in items(n):
        if sub.isEmptyTree() and not hadReal:
          discard

        else:
          hadReal = true
          result.add ~sub

      if not hadReal:
        result.add lS()

    of nkForStmt:
      result = lV(lH(lT(tkFor, 1), ~n[0], # IMPLEMENT multiple identifiers
          lT(1, tkIn, 1), ~n[^2],
          lT(tkColon)),
        lI(~n[^1]))

    of nkWhileStmt:
      result = lV(lH(lT(tkWhile, 1), ~n[0], lT(tkColon)),
        lI(~n[1]))

    of nkCaseStmt, nkRecCase:
      var regular = lV()
      if nffVerticalPackedOf in conf:
        var arms = lV()
        for arm in n[1..^1]:
          arms.add ~arm

        if n of nkRecCase:
          regular = lV(lH(lT(tkCase, 1), ~n[0]), lI(arms))

        else:
          regular = lV(lH(lT(tkCase, 1), ~n[0], lT(tkColon)), lI(arms))

      var grid = lV()

      # FIXME spacing for `of a: lS[A, B]` does not work properly - `B` is
      # indented less than necessary.
      if nffHorizontalPackedOf in conf and false:
        var ofarms: seq[seq[LytBlock]]
        var elsearms: seq[LytBlock]

        for arm in n[1 .. ^1]:
          if arm of nkOfBranch:
            ofarms.add @[
              lH(
                lT(tkOf, 1),
                lytCsv(arm[0 .. ^2], false, conf, fmtStore),
                lT(tkColon, 1)
              ),
              ~arm[^1]
            ]

          else:
            elsearms.add ~arm

        grid = lV(lH(lT(tkCase, 1), ~n[0], lT(tkColon)),
          lI(lV(initAlignedGrid(ofarms, @[lAlignLeft, lAlignLeft]),
             lV(elsearms))))

      if {nffHorizontalPackedOf, nffVerticalPackedOf} in conf:
        result = lC(regular, grid)

      elif nffHorizontalPackedOf in conf:
        result = grid

      else:
        result = regular
      # result = grid

    of nkElse:
      result = lV(lT(tkElse, tkColon), lI(~n[0]))

    of nkOfBranch:
      result = lV(
        lH(
          lT(tkOf, 1),
          lytCsv(n[SliceBranchExpressions], false, conf, fmtStore),
          lT(tkColon)),
        tern(n[PosBody] of nkRecList, ~n[PosBody], lI(~n[PosBody])))

    of nkIfStmt, nkWhenStmt:


      result = lV()
      for idx, branch in pairs(n):
        let isFirst = idx == 0
        if branch.kind == nkElse:
          result.add lV(lT(tkElse, tkColon), lI(~branch[0]), lS())

        else:
          result.add lV(lH(lT(
            tern(isFirst,
              tern(n of nkIfStmt, tkIf, tkWhen),
              tkElse),
            1),
            ~branch[0],
            lT(tkColon)),
            lI(~branch[1]),
            lS())

      if n.len == 2 and
         n.kind == nkIfStmt and
         n[1] of nkElse:
        let bIf = n[0][1]
        let bElse = n[1][0]
        # Either statement list or a literal/token node (in that case it
        # does not have any subnodes and lenght is 0)
        if bIf.safeLen() in [1, 0] and
           bElse.safeLen() in [1, 0]:
          if false:
            result = lC(
              result,
              lH(
                lT(tkIf, 1), ~n[0][0],
                lT(tkColon, 1), ~bIf,
                lT(1, tkElse, tkColon, 1), ~bElse
              )
            )

    of nkTryStmt:
      result = lV(lT(tkTry, tkColon), lI(~n[0]))
      for branch in n[1..^1]:
        result.add ~branch

    of nkContinueStmt:
      result = lT(tkContinue)

    of nkRaiseStmt:
      result = lH(lT(tkRaise, 1), ~n[0])

    of nkExceptBranch:
      result = lV(lH(lT(tkExcept, 1), ~n[0], lT(tkColon)), lI(~n[1]))

    of nkLetSection, nkConstSection, nkVarSection:
      var decls: seq[LytBlock]

      for le in n:
        decls.add ~le


      let name = case n.kind:
        of nkLetSection: tkLet
        of nkVarSection: tkVar
        of nkConstSection: tkConst
        else: failNode n

      if decls.len == 1:
        result = lH(lT(name, 1), decls[0])

      else:
        result = lH(lT(name), lI(lV(decls)))


    of nkIdentDefs, nkConstDef:
      if n[PosInit].kind == nkLambda:
        result = lV(tern(
            n[PosType].kind == nkEmpty,
            lH(lT(n[0]), lT(1, tkEquals, 1)),
            lH(lT(n[0]), lT(1, tkColon, 1), ~n[PosType], lT(1, tkEquals, 1))
          ),
          ~n[PosInit])
      else:
        result = lH(~n[0],
           tern(n[PosType] of nkEmpty,
                lS(), lH(lT(tkColon, 1), ~n[PosType])),
           tern(n[PosInit] of nkEmpty,
                lS(), lH(lT(1, tkEquals, 1), ~n[PosInit])),
           lytDocComment(n, " "))

    of nkEnumFieldDef:
      result = lH(~n[0], lT(1, tkEquals, 1), ~n[1])

    of nkTypeSection:
      if len(n) == 0:
        result = lE()

      else:
        result = lV()
        var buffer: seq[seq[LytBlock]]
        for idx, def in n:
          # echo "def------------"
          # debug def
          if def[2] of nkTypeAliasKinds:
            buffer.add @[lytTypedefHead(def, conf, fmtStore),
                         lT(1, tkEquals, 1), ~def[2]]
            buffer.add @[lS()]

          if not(def[2] of nkTypeAliasKinds) or idx == len(n) - 1:
            if 0 < buffer.len:
              result.add initAlignedGrid(
                buffer, [lAlignLeft, lAlignLeft, lAlignLeft])
              result.add lS()

              buffer = @[]

          if not(def[2] of nkTypeAliasKinds):
            result.add ~def
            result.add lS()

        result = lV(lT(tkType), lI(result))

    of nkTypeDef:
      result = lytTypedef(n, conf, fmtStore)

    of nkPragmaExpr:
      result = lH(~n[0], lT(1), ~n[1])

    of nkRecList:
      result = lV(lytDocComment(n))
      for fld in n:
        result.add ~fld

      result = lI(result)

    of nkPragma:
      result = lH()
      result.add lT(tkCurlyDotLe)
      var idx = 0
      while idx < len(n):
        var res: LytBlock
        let item = n[idx]
        if item of nkIdent and item.ident.s == "push":
          let next = n[idx + 1]
          res = lH(~item, lT(1), tern(
            next of nkExprColonExpr,
            lH(~next[0], lT(tkColon), ~next[1]), ~next))

          inc idx

        else:
          res = ~item

        result.add tern(idx < n.len - 1, lH(res, lT(tkComma, 1)), res)
        inc idx

      result.add lT(tkCurlyDotRi)

    of nkImportStmt:
      var imports = lV()
      for idx, path in n:
        if path of nkInfix:
          imports.add lH(
            lytInfix(path, conf, fmtStore, spacing = false),
            tern(idx < n.len - 1, lT(tkComma), lS())
          )

        else:
          imports.add ~path

      result = lV(lT(tkImport), lI(imports))

    of nkExportStmt:
      result = lH(lT(tkExport, 1))
      for idx, exp in n:
        if idx > 0:
          result.add lH(lT(tkComma, 1), ~exp)

        else:
          result.add ~exp

    of nkCommentStmt:
      result = lV()
      for line in split(n.comment, '\n'):
        discard
        # result.add lH(lT("## " & line))

    of nkExprEqExpr:    result = lH(~n[0], lT(1, tkEquals, 1), ~n[1])
    of nkExprColonExpr: result = lH(~n[0], lT(tkColon, 1), ~n[1])
    of nkPrefix:        result = lH(~n[0], ~n[1])
    of nkPostfix:       result = lH(~n[1], ~n[0])
    of nkInfix:         result = lytInfix(n, conf, fmtStore)
    of nkIdent, nkSym:  result = lT(n)
    of nkDotExpr:       result = lH(~n[0], lT(tkDot), ~n[1])
    of nkEmpty:         result = lS()
    of nkPtrTy:         result = lH(lT(tkPtr, 1), ~n[0])
    of nkRefTy:         result = lH(lT(tkRef, 1), ~n[0])
    of nkVarTy:         result = lH(lT(tkVar, 1), ~n[0])
    of nkNilLit:        result = lT(tkNil)
    of nkReturnStmt:    result = lH(lT(tkReturn, 1), ~n[0])
    of nkBreakStmt:     result = lH(lT(tkBreak, 1), ~n[0])
    of nkDiscardStmt:   result = lH(lT(tkDiscard, 1), ~n[0])
    of nkAsgn:          result = lH(~n[0], lT(1, tkEquals, 1), ~n[1])

    of nkLiterals:
      result = lT(n)

    of nkHiddenStdConv:
      # QUESTION - formatting hidden nodes such as std conversion, up/down
      # cost and so on might 
      result = ~n[1]

    of nkBracket:
      result = lC(
        lH(
          lT(tkBracketLe),
          lytCsv(n, false, conf, fmtStore),
          lT(tkBracketRi)),
        lV(
          lT(tkBracketLe),
          lI(lytCsv(n, true, conf, fmtStore)),
          lT(tkBracketRi)))

    of nkGenericParams:
      result = lH(
        lT(tkBracketLe),
        lytCsv(n, false, conf, fmtStore),
        lT(tkBracketRi))

    of nkCast:
      if n[0] of nkEmpty:
        result = lH(lT(tkCast, tkParLe), ~n[1], lT(tkParRi))

      else:
        result = lH(
          lT(tkCast, tkBracketLe),
          ~n[0],
          lT(tkBracketRi, tkParLe),
          ~n[1],
          lT(tkParRi))


    of nkCurly:
      if len(n) > 6:
        result = lV(
          lT(tkCurlyLe),
          lI(lytCsv(n, true, conf, fmtStore)),
          lT(tkCurlyRi))

      else:
        result = lH(
          lT(tkCurlyLe),
          lytCsv(n, false, conf, fmtStore),
          lT(tkCurlyRi))


    of nkBlockStmt:
      result = lV(
        lH(lT(tkBlock), tern(n[0].isEmptyTree(), lE(), lH(lT(1), ~n[0])), lT(tkColon)),
        lI(~n[1]))

    of nkBracketExpr:
      result = lH(
        ~n[0],
        lT(tkBracketLe),
        lytCsv(n[1..^1], false, conf, fmtStore),
        lT(tkBracketRi)
      )

    of nkPar:
      result = lH(
        lT(tkParLe),
        lytCsv(n[0..^1], false, conf, fmtStore),
        lT(tkParRi))

    of nkCommand:
      result = lH(
        ~n[0],
        lT(1),
        lytCsv(n[1..^1], false, conf, fmtStore))

    of nkCall:
      result = lH(
        ~n[0],
        lT(tkParLe),
        lytCsv(n[1..^1], false, conf, fmtStore),
        lT(tkParRi))

    of nkPragmaBlock:
      result = lV(lH(~n[0], lT(tkColon)), lI(~n[1]))

    of nkTupleConstr:
      result = lH(lT(tkParLe))
      for idx, item in n:
        if 0 < idx: result.add lT(tkComma, 1)
        result.add ~item

      result.add lT(tkParRi)


    else:
      failNode n

  assert not isNil(result), $n.kind
  expectValid(result, $n.kind)

proc formatToStr*(node: PNode): string =
  var known: Table[int, PNode]
  proc aux(n: PNode) =
    known[n.id] = n
    if 0 < safeLen(n):
      for sub in n:
        aux(sub)

  aux(node)
  # Convert node to the layout block tree
  var fmtStore = newStrStore()
  let blc = toLytBlock(node, defaultNimFormatConf, fmtStore)
  let opts = initLytOptions()
  # Select optimal layout of all the ones presented
  let lyt = toLayout(blc, opts)
  # Iterate over layout formatting events
  for ev in nimFormatEvents(lyt, fmtStore):
    case ev.kind:
      # Indentation/separator spaces
      of nimEvFormatSpaces:
        result.add repeat(" ", ev.spaces)

      # Layout newline - tokens themselves can't contain newlines
      of nimEvFormatNewline:
        result.add "\n"

      # Format regular token: `ev.token` is a `lexer.TokType`
      of nimEvFormatToken:
        result.add  $ev.token

      of nimEvFormatStr:
        result.add $ev.str

      # Format token node - ident, symbol, integer or any other literal
      of nimEvFormatNode:
        let node = known[ev.node]
        case node.kind:
          of nkIdent:
            result.add node.ident.s

          of nkSym:
            result.add node.sym.name.s

          of nkFloatLiterals:
            result.add $node.floatVal

          of nkIntKinds:
            result.add $node.intVal

          else:
            result.add $node



when isMainModule:
  import ast/[parser, reports, idents], front/options, std/tables

  proc parse(s: string): PNode =
    proc hook(conf: ConfigRef, report: Report): TErrorHandling =
      if conf.isCodeError(report):
        assert false, $report.kind

    var conf = newConfigRef(hook)
    var cache = newIdentCache()
    return parseString(s, cache, conf)

  proc reformat(s: string): string =
    # Input node - can come from any location, for the purposes of testing
    # using `parse()` here directly
    let node = parse(s)
    if node.kind == nkStmtList and node.len == 1:
      return node[0].formatToStr()

    else:
      return node.formatToStr()

  echo reformat("a = (12, 2)\nb = 2")
  echo reformat("""
block:
  var i: cint = 0
  while i < 5:
    postInc(i)
""")

  echo reformat("""
block:
  while i < 5:
    if portIoMap.count(base):
      postInc(i)
""")

  echo reformat("""
case v:
    of 0xa7:
        ccb.me = 0
        return
""")
  # echo reformat("for i in 0 .. 10: echo i")
