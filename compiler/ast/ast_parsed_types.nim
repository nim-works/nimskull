## Data structure for the parser results

import
  compiler/ast/[
    lineinfos,  # For TLineInfo
    idents,     # For `PIdent`
    numericbase
  ],
  compiler/utils/[
    idioms
  ]

from compiler/ast/lexer import Token, TokType

# NOTE further refactoring considerations for the parser:
#
# - store everything in tokens, do not require identifier interning for any
#   purposes during the parsing stage, it must be done later, during
#   conversion to a PNode. This will simplify some parts of the type
#   definition.
# - remove nim"pretty" - this is an absolute joke of implementation and
#   it should not be placed where it is now.

type
  ParseDiagKind* = enum
    # internal errors begin
    # pdkInternalError
    # internal errors end

    # TODO: likely incorporate lexer errors etc
    
    # errors begin
    pdkInvalidIndentation
    pdkInvalidIndentationWithForgotEqualSignHint
    pdkNestableRequiresIndentation
    pdkIdentExpected
    pdkIdentExpectedEmptyAccQuote
    pdkExprExpected
    pdkMissingToken
    pdkUnexpectedToken
    pdkAsmStmtExpectsStrLit
    pdkFuncNotAllowed   # xxx: bad name
    pdkTupleTypeWithPar # xxx: bad name
    pdkMisplacedParameterVar # xxx: bad name
    pdkConceptNotInType
    pdkMisplacedExport
    pdkPragmaBeforeGenericParameters
    # errors end

    # warnings being
    pdkInconsistentSpacing # xxx: bad name
    pdkPragmaDoesNotFollowTypeName
    pdkEnablePreviewDotOps
    # warnings end

const
  pdkWithExtraData* = {pdkInvalidIndentationWithForgotEqualSignHint,
                        pdkUnexpectedToken,
                        pdkMissingToken,
                        pdkIdentExpected,
                        pdkExprExpected,
                        pdkAsmStmtExpectsStrLit,
                        pdkInconsistentSpacing}
  pdkWithoutExtraData* = {low(ParseDiagKind)..high(ParseDiagKind)} - 
                          pdkWithExtraData

type
  ParseDiag* = object
    instLoc*: InstantiationInfo
    location*: TLineInfo        # xxx: can we get away with line/col only?
    case kind*: ParseDiagKind:
      of pdkInvalidIndentationWithForgotEqualSignHint:
        eqLineInfo*: TLineInfo
      of pdkUnexpectedToken:
        expected*: TokType
        actual*: Token
      of pdkMissingToken:
        missedToks*: seq[TokType]
      of pdkIdentExpected,
          pdkExprExpected,
          pdkAsmStmtExpectsStrLit,
          pdkInconsistentSpacing:
        found*: Token
      of pdkWithoutExtraData:
        discard

  ParsedNodeKind* = enum
    pnkError        ## currently we don't produce error nodes
    pnkEmpty
    pnkIdent
    pnkCharLit
    pnkIntLit
    pnkInt8Lit
    pnkInt16Lit
    pnkInt32Lit
    pnkInt64Lit
    pnkUIntLit
    pnkUInt8Lit
    pnkUInt16Lit
    pnkUInt32Lit
    pnkUInt64Lit
    pnkFloatLit
    pnkFloat32Lit
    pnkFloat64Lit
    pnkFloat128Lit
    pnkStrLit
    pnkRStrLit
    pnkTripleStrLit
    pnkNilLit
    pnkCustomLit
    pnkAccQuoted
    pnkCall
    pnkCommand
    pnkCallStrLit
    pnkInfix
    pnkPrefix
    pnkPostfix
    pnkExprEqExpr
    pnkExprColonExpr
    pnkIdentDefs
    pnkConstDef
    pnkVarTuple
    pnkPar
    pnkSqrBracket
    pnkCurly
    pnkTupleConstr
    pnkObjConstr
    pnkTableConstr
    pnkSqrBracketExpr
    pnkCurlyExpr
    pnkPragmaExpr
    pnkPragma
    pnkPragmaBlock
    pnkDotExpr
    pnkIfExpr
    pnkIfStmt
    pnkElifBranch
    pnkElifExpr
    pnkElse
    pnkElseExpr
    pnkCaseStmt
    pnkOfBranch
    pnkWhenExpr
    pnkWhenStmt
    pnkForStmt
    pnkWhileStmt
    pnkBlockExpr
    pnkBlockStmt
    pnkDiscardStmt
    pnkContinueStmt
    pnkBreakStmt
    pnkReturnStmt
    pnkRaiseStmt
    pnkYieldStmt
    pnkTryStmt
    pnkExceptBranch
    pnkFinally
    pnkDefer
    pnkLambda
    pnkDo
    pnkBind
    pnkBindStmt
    pnkMixinStmt
    pnkCast
    pnkStaticStmt
    pnkAsgn
    pnkGenericParams
    pnkFormalParams
    pnkStmtList
    pnkStmtListExpr
    pnkImportStmt
    pnkImportExceptStmt
    pnkImportAs
    pnkFromStmt
    pnkIncludeStmt
    pnkExportStmt
    pnkExportExceptStmt
    pnkConstSection
    pnkLetSection
    pnkVarSection
    pnkProcDef
    pnkFuncDef
    pnkMethodDef
    pnkConverterDef
    pnkIteratorDef
    pnkMacroDef
    pnkTemplateDef
    pnkTypeSection
    pnkTypeDef
    pnkEnumTy
    pnkEnumFieldDef
    pnkObjectTy
    pnkTupleTy
    pnkProcTy
    pnkIteratorTy
    pnkRecList
    pnkRecCase
    pnkRecWhen
    pnkTypeOfExpr
    pnkRefTy
    pnkVarTy
    pnkPtrTy
    pnkStaticTy
    pnkDistinctTy
    pnkMutableTy
    pnkTupleClassTy
    pnkTypeClassTy
    pnkOfInherit
    pnkArgList
    pnkWith
    pnkWithout
    pnkAsmStmt
    pnkCommentStmt
    pnkUsingStmt
  
  ParsedKindWithSons* = range[pnkCall..pnkUsingStmt]
  ParsedKindLiteral* = range[pnkCharLit..pnkCustomLit]
  ParsedKindBracket* = range[pnkPar..pnkCurly]

  ParsedToken* = object
    ## Used instead of `Token` to save memory
    line*: uint16
    col*: int16
    tokType*: TokType
    ident*: PIdent
    iNumber*: BiggestInt
    fNumber*: BiggestFloat
    base*: NumericalBase
    literal*: string

  ParsedNodeData*{.final, acyclic.} = object
    # TODO: replace token fields with indexing into a token sequence, this
    #       should also address line info tracking.
    comment*: string       # TODO: replace with an index into a token stream
    fileIndex*: FileIndex  # xxx: remove and have the caller handle it?
    case kind*: ParsedNodeKind:
      of pnkError:
        diag*: ParseDiag
      of pnkEmpty:
        line*: uint16
        col*: int16
      of pnkIdent:
        startToken*: ParsedToken
      of pnkCharLit..pnkUInt64Lit,
          pnkFloatLit..pnkFloat128Lit,
          pnkStrLit..pnkTripleStrLit,
          pnkNilLit,
          pnkCustomLit:
        lit*: ParsedToken
      of pnkAccQuoted:
        quote*: ParsedToken
        idents*: seq[tuple[ident: PIdent, line: uint16, col: int16]]
      of pnkCall..pnkUsingStmt:
        token*: ParsedToken
        isBlockArg*: bool       # TODO: rework ast to eliminate this flag,
                                #       maybe with a `pnkStmtListArg` kind.
        sons*: seq[ParsedNode]  # TODO: replace `ref` object graph with
                                #       begin/end ranges for tracking the tree
                                #       hierarchy in a linear data structure.

  ParsedNode* = ref ParsedNodeData


const
  pnkParsedKindsWithSons* = {pnkCall..pnkUsingStmt}
  pnkCallKinds* = {pnkCall, pnkInfix, pnkPrefix, pnkPostfix,
                  pnkCommand, pnkCallStrLit}
  pnkFloatKinds* = {pnkFloatLit..pnkFloat128Lit}
  pnkIntKinds* = {pnkCharLit..pnkUInt64Lit}
  pnkStrKinds* = {pnkStrLit..pnkTripleStrLit}

func len*(node: ParsedNode): int =
  ## Number of sons of the parsed node
  return node.sons.len()

# NOTE added for the sake of API similarity between PNode
func safeLen*(node: ParsedNode): int =
  if node.kind in pnkParsedKindsWithSons:
    node.len()
  else:
    0

proc `[]`*(node: ParsedNode, idx: int | BackwardsIndex): ParsedNode =
  return node.sons[idx]

proc `[]=`*(node: ParsedNode, idx: int | BackwardsIndex, other: ParsedNode) =
  node.sons[idx] = other

iterator items*(node: ParsedNode): ParsedNode =
  for item in node.sons.items():
    yield item

iterator pairs*(node: ParsedNode): (int, ParsedNode) =
  for idx, item in pairs(node.sons):
    yield (idx, item)

proc add*(node: ParsedNode, other: ParsedNode) =
  ## append `other` to `node`'s `sons`
  node.sons.add(other)

proc transitionSonsKind*(n: ParsedNode, kind: ParsedNodeKind) =
  assert n.kind in pnkParsedKindsWithSons and kind in pnkParsedKindsWithSons,
          "kind from: " & $n.kind & " to: " & $kind
  let obj = n[]
  {.cast(uncheckedAssign).}:
    n[] = ParsedNodeData(kind: kind,
                         token: obj.token,
                         isBlockArg: obj.isBlockArg,
                         sons: obj.sons,
                         comment: obj.comment,
                         fileIndex: obj.fileIndex)

func newEmptyParsedNode*(fileIndex: FileIndex,
                         line = unknownLineInfo.line,
                         col = unknownLineInfo.col): ParsedNode =
  ## Create an empty ParsedNode
  ParsedNode(kind: pnkEmpty, fileIndex: fileIndex, line: line, col: col)

func newParsedNode*(
  kind: ParsedKindWithSons,
  fileIndex: FileIndex,
  token: ParsedToken,
  sons: seq[ParsedNode] = @[]): ParsedNode =
  ## Create a new non-leaf parsed node with a specified location
  ## information and sons.
  ParsedNode(kind: kind, fileIndex: fileIndex, token: token, sons: sons)

func newParsedLitNode*(
  kind: ParsedKindLiteral,
  fileIndex: FileIndex,
  token: ParsedToken): ParsedNode =
  ## Create a new leaf literal parsed node.
  ParsedNode(kind: kind, fileIndex: fileIndex, lit: token)

func newParsedNodeIdent*(fileIndex: FileIndex, token: ParsedToken): ParsedNode =
  ## Create a `pnkIdent` node
  ParsedNode(kind: pnkIdent, fileIndex: fileIndex, startToken: token)

proc getToken*(n: ParsedNode): ParsedToken =
  case n.kind
  of pnkError, pnkEmpty:
    unreachable()
  of pnkIdent:
    n.startToken
  of pnkCharLit..pnkUInt64Lit,
      pnkFloatLit..pnkFloat128Lit,
      pnkStrLit..pnkTripleStrLit,
      pnkNilLit,
      pnkCustomLit:
    n.lit
  of pnkAccQuoted:
    n.quote
  of pnkCall..pnkUsingStmt:
    n.token

proc newProcNode*(
    kind: ParsedNodeKind,
    fileIndex: FileIndex,
    token: ParsedToken,
    body, params, name, pattern, genericParams,
    pragmas, exceptions: ParsedNode
  ): ParsedNode =
  newParsedNode(
    kind,
    fileIndex,
    token,
    @[name, pattern, genericParams, params, pragmas, exceptions, body])

func info*(p: ParsedNode): TLineInfo {.inline.} =
  case p.kind
  of pnkError:
    unreachable("IMPLEMENT ME") # xxx: we don't produce pnkError nodes, yet
  of pnkEmpty:
    TLineInfo(fileIndex: p.fileIndex, line: p.line, col: p.col)
  of pnkIdent:
    TLineInfo(fileIndex: p.fileIndex,
              line: p.startToken.line,
              col: p.startToken.col)
  of pnkCharLit..pnkUInt64Lit,
      pnkFloatLit..pnkFloat128Lit,
      pnkStrLit..pnkTripleStrLit,
      pnkNilLit,
      pnkCustomLit:
    TLineInfo(fileIndex: p.fileIndex,
              line: p.lit.line,
              col: p.lit.col)
  of pnkAccQuoted:
    TLineInfo(fileIndex: p.fileIndex,
              line: p.quote.line,
              col: p.quote.col)
  of pnkCall..pnkUsingStmt:
    TLineInfo(fileIndex: p.fileIndex,
              line: p.token.line,
              col: p.token.col)

proc toParsedToken*(t: Token): ParsedToken {.inline.} =
  let (line, col) = clampLineCol(t.line, t.col)
  result = ParsedToken(
    line: line,
    col: col,
    tokType: t.tokType,
    ident: t.ident,
    iNumber: t.iNumber,
    fNumber: t.fNumber,
    base: t.base,
    literal: t.literal)