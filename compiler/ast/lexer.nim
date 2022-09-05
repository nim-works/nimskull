#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This lexer is handwritten for efficiency. I used an elegant buffering
## scheme which I have not seen anywhere else: We guarantee that a whole
## line is in the buffer. Thus only when scanning the `\n` or `\r`
## character we have to check whether we need to read in the next chunk.
## (`\n` or `\r` already need special handling for incrementing the line
## counter; choosing both `\n` and `\r` allows the lexer to properly read
## Unix, DOS or Macintosh text files, even when it is not the native
## format.

import
  compiler/utils/[
    platform,
    pathutils,
  ],
  compiler/ast/[
    wordrecg,
    nimlexbase,
    llstream,
    lineinfos,
    reports
  ],
  std/[
    parseutils,
    strutils,
    strformat,
    hashes,
    tables
  ],
  compiler/front/[
    options,
    msgs
  ],
  experimental/[
    dod_helpers
  ]

import std/options as std_options

const
  MaxLineLength* = 80         # lines longer than this lead to a warning
  numChars*: set[char] = {'0'..'9', 'a'..'z', 'A'..'Z'}
  SymChars*: set[char] = {'a'..'z', 'A'..'Z', '0'..'9', '\x80'..'\xFF'}
  SymStartChars*: set[char] = {'a'..'z', 'A'..'Z', '\x80'..'\xFF'}
  OpChars*: set[char] = {'+', '-', '*', '/', '\\', '<', '>', '!', '?', '^', '.',
    '|', '=', '%', '&', '$', '@', '~', ':'}
  UnaryMinusWhitelist = {' ', '\t', '\n', '\r', ',', ';', '(', '[', '{'}

# don't forget to update the 'highlite' module if these charsets should change

type
  TokenKind* = enum
    tkInvalid   = "tkInvalid"
    tkEof       = "[EOF]" # order is important here!
    tkSymbol    = "tkSymbol" # keywords:
    tkAddr      = "addr"
    tkAnd       = "and"
    tkAs        = "as"
    tkAsm       = "asm"
    tkBind      = "bind"
    tkBlock     = "block"
    tkBreak     = "break"
    tkCase      = "case"
    tkCast      = "cast"
    tkConcept   = "concept"
    tkConst     = "const"
    tkContinue  = "continue"
    tkConverter = "converter"
    tkDefer     = "defer"
    tkDiscard   = "discard"
    tkDistinct  = "distinct"
    tkDiv       = "div"
    tkDo        = "do"
    tkElif      = "elif"
    tkElse      = "else"
    tkEnd       = "end"
    tkEnum      = "enum"
    tkExcept    = "except"
    tkExport    = "export"
    tkFinally   = "finally"
    tkFor       = "for"
    tkFrom      = "from"
    tkFunc      = "func"
    tkIf        = "if"
    tkImport    = "import"
    tkIn        = "in"
    tkInclude   = "include"
    tkInterface = "interface"
    tkIs        = "is"
    tkIsnot     = "isnot"
    tkIterator  = "iterator"
    tkLet       = "let"
    tkMacro     = "macro"
    tkMethod    = "method"
    tkMixin     = "mixin"
    tkMod       = "mod"
    tkNil       = "nil"
    tkNot       = "not"
    tkNotin     = "notin"
    tkObject    = "object"
    tkOf        = "of"
    tkOr        = "or"
    tkOut       = "out"
    tkProc      = "proc"
    tkPtr       = "ptr"
    tkRaise     = "raise"
    tkRef       = "ref"
    tkReturn    = "return"
    tkShl       = "shl"
    tkShr       = "shr"
    tkStatic    = "static"
    tkTemplate  = "template"
    tkTry       = "try"
    tkTuple     = "tuple"
    tkType      = "type"
    tkUsing     = "using"
    tkVar       = "var"
    tkWhen      = "when"
    tkWhile     = "while"
    tkXor       = "xor"
    tkYield     = "yield" # end of keywords

    # Numerica literal tokens - float and integers. There are four groups
    # of nearly identical token kinds, differing only in their literal base
    # - this information was not moved it a separate channel such as
    # 'numerical base' in order to keep the data normalized. Most tokens do
    # not need base information stored, so it makes more sense to
    # make a separate group of kinds instead.

    # Base-10 tokens, default
    tkIntLit        = "tkIntLit"
    tkInt8Lit       = "tkInt8Lit"
    tkInt16Lit      = "tkInt16Lit"
    tkInt32Lit      = "tkInt32Lit"
    tkInt64Lit      = "tkInt64Lit"
    tkUIntLit       = "tkUIntLit"
    tkUInt8Lit      = "tkUInt8Lit"
    tkUInt16Lit     = "tkUInt16Lit"
    tkUInt32Lit     = "tkUInt32Lit"
    tkUInt64Lit     = "tkUInt64Lit"
    tkFloatLit      = "tkFloatLit"
    tkFloat32Lit    = "tkFloat32Lit"
    tkFloat64Lit    = "tkFloat64Lit"
    tkFloat128Lit   = "tkFloat128Lit"

    tkIntLitBase2        = "tkIntLitBase2"
    tkInt8LitBase2       = "tkInt8LitBase2"
    tkInt16LitBase2      = "tkInt16LitBase2"
    tkInt32LitBase2      = "tkInt32LitBase2"
    tkInt64LitBase2      = "tkInt64LitBase2"
    tkUIntLitBase2       = "tkUIntLitBase2"
    tkUInt8LitBase2      = "tkUInt8LitBase2"
    tkUInt16LitBase2     = "tkUInt16LitBase2"
    tkUInt32LitBase2     = "tkUInt32LitBase2"
    tkUInt64LitBase2     = "tkUInt64LitBase2"
    tkFloatLitBase2      = "tkFloatLitBase2"
    tkFloat32LitBase2    = "tkFloat32LitBase2"
    tkFloat64LitBase2    = "tkFloat64LitBase2"
    tkFloat128LitBase2   = "tkFloat128LitBase2"

    tkIntLitBase8        = "tkIntLitBase8"
    tkInt8LitBase8       = "tkInt8LitBase8"
    tkInt16LitBase8      = "tkInt16LitBase8"
    tkInt32LitBase8      = "tkInt32LitBase8"
    tkInt64LitBase8      = "tkInt64LitBase8"
    tkUIntLitBase8       = "tkUIntLitBase8"
    tkUInt8LitBase8      = "tkUInt8LitBase8"
    tkUInt16LitBase8     = "tkUInt16LitBase8"
    tkUInt32LitBase8     = "tkUInt32LitBase8"
    tkUInt64LitBase8     = "tkUInt64LitBase8"
    tkFloatLitBase8      = "tkFloatLitBase8"
    tkFloat32LitBase8    = "tkFloat32LitBase8"
    tkFloat64LitBase8    = "tkFloat64LitBase8"
    tkFloat128LitBase8   = "tkFloat128LitBase8"

    tkIntLitBase16        = "tkIntLitBase16"
    tkInt8LitBase16       = "tkInt8LitBase16"
    tkInt16LitBase16      = "tkInt16LitBase16"
    tkInt32LitBase16      = "tkInt32LitBase16"
    tkInt64LitBase16      = "tkInt64LitBase16"
    tkUIntLitBase16       = "tkUIntLitBase16"
    tkUInt8LitBase16      = "tkUInt8LitBase16"
    tkUInt16LitBase16     = "tkUInt16LitBase16"
    tkUInt32LitBase16     = "tkUInt32LitBase16"
    tkUInt64LitBase16     = "tkUInt64LitBase16"
    tkFloatLitBase16      = "tkFloatLitBase16"
    tkFloat32LitBase16    = "tkFloat32LitBase16"
    tkFloat64LitBase16    = "tkFloat64LitBase16"
    tkFloat128LitBase16   = "tkFloat128LitBase16"

    tkStrLit        = "tkStrLit"
    tkRStrLit       = "tkRStrLit"
    tkTripleStrLit  = "tkTripleStrLit"
    tkGStrLit       = "tkGStrLit"
    tkGTripleStrLit = "tkGTripleStrLit"
    tkCharLit       = "tkCharLit"
    tkCustomLitText = "tkCustomLitText"
    tkCustomLitCall = "tkCustomLitCall"


    tkParLe          = "("
    tkParRi          = ")"
    tkBracketLe      = "["
    tkBracketRi      = "]"
    tkCurlyLe        = "{"
    tkCurlyRi        = "}"
    tkBracketDotLe   = "[."
    tkBracketDotRi   = ".]"
    tkCurlyDotLe     = "{."
    tkCurlyDotRi     = ".}"
    tkParDotLe       = "(."
    tkParDotRi       = ".)"
    tkComma          = ","
    tkSemiColon      = ";"
    tkColon          = ":"
    tkColonColon     = "::"
    tkEquals         = " ="
    tkDot            = "."
    tkDotDot         = ".."
    tkBracketLeColon = "[:"
    tkOpr
    tkOpr1
    tkOpr2
    tkOpr3
    tkOpr4
    tkOpr5
    tkOpr6
    tkOpr7
    tkOpr8
    tkOpr9
    tkOpr10
    tkComment
    tkDocComment
    tkAccent         = "`"
    # these are fake tokens used by renderer.nim
    tkSpaces
    tkInfixOpr
    tkPrefixOpr
    tkPostfixOpr
    tkHideableStart
    tkHideableEnd

  TokenKinds* = set[TokenKind]

const weakTokens = {
  tkComma,
  tkSemiColon,
  tkColon,
  tkParRi,
  tkParDotRi,
  tkBracketRi,
  tkBracketDotRi,
  tkCurlyRi
}
  ## tokens that should not be considered for previousToken

const
  tokKeywordLow* = succ(tkSymbol)
  tokKeywordHigh* = pred(tkIntLit)

declareIdType(Token)

type
  Token* = object
    kind*: TokenKind
    line*: int
    col*: int
    indent*: int
    slice*: Slice[int] ## Range of the text used in the token

declareStoreType(Token)

type
  Tokenized* = ref object
    text*: string ## Full content of the input file
    tokens*: TokenStore

  Lexer* = object
    store*: Tokenized ## Stored list of lexed tokens
    stream*: PLLStream ## Input stream to read more data from
    fileIdx*: FileIndex ## Index of the file begin lexed right now
    bufpos*: int ## The current lexer position in the ingoing string stream
    tokenId*: TokenId ## The current lexer position in the outgoing token stream
    lineNumber*: int ## Current line number
    column*: int ## Current column position
    indentAhead*: int         ## if > 0 an indentation has already been read
                              ## this is needed because scanning comments
                              ## needs so much look-ahead
    currLineIndent*: int ## Indetation of the current line
    config*: ConfigRef
    lineStart*: int

proc getLineInfo*(L: Lexer, tok: Token): TLineInfo {.inline.} =
  result = newLineInfo(L.fileIdx, tok.line, tok.col)

template buf(lex: Lexer): string =
  ## Shorthand for accessing the lexer's text buffer
  lex.store.text

func at*(tokenized: Tokenized, id: TokenId): Token =
  tokenized.tokens[id]

func at*(lex: Lexer, id: TokenId): Token =
  lex.store.tokens[id]

func add(lex: var Lexer, tok: Token) =
  discard lex.store.tokens.add(tok)

func last(lex: var Lexer): var Token =
  lex.store.tokens.last()

func add(lex: var Lexer, tokens: seq[Token]) =
  for tok in tokens:
    lex.add(tok)

func at(lex: Lexer, shift: int = 0): char =
  ## Return current character that the lexer is at
  lex.buf[lex.bufpos + shift]



func next(lex: var Lexer, count: int = 1) =
  inc(lex.bufpos, count)

proc getLineInfo*(lex: Lexer, tok: Token): TLineInfo {.inline.} =
  result = newLineInfo(lex.fileIdx, tok.line, tok.col)

proc getColNumber*(lex: Lexer, pos: int): int =
  assert false, "TODO implement"

func isKeyword*(kind: TokenKind): bool =
  (kind >= tokKeywordLow) and (kind <= tokKeywordHigh)

func isKeyword*(i: PIdent): bool =
  ## is this the `i`dentifier a keyword?
  # xxx: not the best place for this, but putting it in `idents` leads to a
  #      circular dependency; likely need an intermediate module
  (i.id >= ord(tokKeywordLow) - ord(tkSymbol)) and
    (i.id <= ord(tokKeywordHigh) - ord(tkSymbol))

template ones(n): untyped = ((1 shl n)-1) # for utf-8 conversion

proc isNimIdentifier*(s: string): bool =
  let sLen = s.len
  if sLen > 0 and s[0] in SymStartChars:
    var i = 1
    while i < sLen:
      if s[i] == '_': inc(i)
      if i < sLen and s[i] notin SymChars: return
      inc(i)
    result = true

proc `$`*(tok: Token): string =
  &"{tok.kind}:{tok.slice.a}..{tok.slice.b}"

proc `$`*(store: Tokenized, tok: Token): string =
  &"{tok.kind}:{store.text[tok.slice]}"

proc printTok*(conf: ConfigRef; tok: Token) =
  # xxx factor with toLocation
  conf.writeln($tok.line & ":" & $tok.col & "\t" & $tok.kind & " " & $tok)

proc initToken*(lex: var Token) = lex.kind = tkInvalid
proc fillToken(lex: var Token) = lex.kind = tkInvalid

func initToken*(): Token = initToken(result)


proc start(tok: var Token, lex: Lexer) =
  ## Set left end of the token to the current lexer position
  tok.slice.a = lex.bufpos

proc start(tok: var Token, pos: int) =
  ## Set left end of the token to the specified position
  tok.slice.a = pos

proc extend(tok: var Token, right: int) =
  ## Extend the right end of the token
  tok.slice.b = right

func initToken*(
    lex: var Lexer,
    kind: TokenKind,
    start: int = lex.bufpos
  ): Token =
  ## Initialize a new token at the lexer's position
  fillToken(result)
  result.kind = kind
  if 0 <= lex.indentAhead:
    result.indent = lex.indentAhead
    lex.currLineIndent = lex.indentAhead
    lex.indentAhead = -1
  else:
    result.indent = -1

  result.slice = start .. start
  result.line = lex.lineNumber
  result.col = getColNumber(lex, lex.bufpos)


proc openBaseLexer*(lex: var Lexer, stream: PLLStream) =
  lex.stream = stream

proc openLexer*(lex: var Lexer, fileIdx: FileIndex, inputstream: PLLStream;
                 config: ConfigRef) =
  openBaseLexer(lex, inputstream)
  lex.fileIdx = fileIdx
  lex.indentAhead = -1
  lex.currLineIndent = 0
  inc(lex.lineNumber, inputstream.lineOffset)
  lex.config = config

proc openLexer*(
    lex: var Lexer,
    filename: AbsoluteFile,
    inputstream: PLLStream,
    config: ConfigRef
  ) =

  openLexer(lex, fileInfoIdx(config, filename), inputstream, config)

proc closeLexer*(lex: var Lexer) =
  if lex.config != nil:
    inc(lex.config.linesCompiled, lex.lineNumber)
  lex.stream.llStreamClose()

proc getLineInfo(lex: Lexer): TLineInfo =
  result = newLineInfo(
    lex.fileIdx,
    lex.lineNumber, getColNumber(lex, lex.bufpos))

proc matchTwoChars(lex: Lexer, first: char, second: set[char]): bool =
  result = (lex.at() == first) and (lex.at(+1) in second)

proc tokenBegin(lex: Lexer, tok: var Token, pos: int = lex.bufpos) =
  tok.start(pos)

proc eatChar(lex: var Lexer, t: var Token) =
  t.extend(lex.bufpos)
  lex.next()

template localReport*(lex: Lexer, report: ReportTypes): untyped =
  lex.config.handleReport(
    wrap(report, instLoc(), getLineInfo(lex)), instLoc())

template localReportTok*(
    lex: Lexer, report: ReportTypes, tok: Token): untyped =
  lex.config.handleReport(wrap(
    report, instLoc(), newLineInfo(
      lex.fileIdx, tok.line, tok.col)), instLoc())

template localReportPos*(lex: Lexer, report: ReportTypes, pos: int): untyped =
  lex.config.handleReport(wrap(
    report, instLoc(), newLineInfo(
      lex.fileIdx, lex.lineNumber, pos - lex.lineStart)), instLoc())


proc getNumber(lex: var Lexer): seq[Token] =
  proc matchUnderscoreChars(
    lex: var Lexer, tok: var Token, chars: set[char]): Natural =

    var pos = lex.bufpos              # use registers for pos, buf
    result = 0
    while true:
      if lex.buf[pos] in chars:
        tok.extend(pos)
        inc(pos)
        inc(result)

      else:
        break

      if lex.buf[pos] == '_':
        if lex.buf[pos + 1] notin chars:
          lex.localReport(LexerReport(kind: rlexMalformedUnderscores))
          break

        tok.extend(pos)
        inc(pos)

    lex.bufpos = pos

  proc matchChars(lex: var Lexer, tok: var Token, chars: set[char]) =
    var pos = lex.bufpos              # use registers for pos, buf
    while lex.buf[pos] in chars:
      tok.extend(pos)
      inc(pos)
    lex.bufpos = pos

  proc lexMessageLitNum(lex: var Lexer, msg: string, startpos: int, msgKind: LexerReportKind) =
    # Used to get slightly human friendlier err messages.
    const literalishChars = {'A'..'Z', 'a'..'z', '0'..'9', '_', '.', '\''}
    var msgPos = lex.bufpos
    var t: Token
    t.start(lex)
    lex.bufpos = startpos # Use lex.bufpos as pos because of matchChars
    matchChars(lex, t, literalishChars)
    # We must verify +/- specifically so that we're not past the literal
    if  lex.at() in {'+', '-'} and
        lex.buf[lex.bufpos - 1] in {'e', 'E'}:
      t.extend(lex.bufpos)
      lex.next()
      matchChars(lex, t, literalishChars)
    if lex.at() in literalishChars:
      t.extend(lex.bufpos)
      lex.next()
      matchChars(lex, t, {'0'..'9'})
    lex.bufpos = msgPos
    lex.localReport(LexerReport(kind: msgKind, msg: lex.store $ t))

  var
    xi: BiggestInt
    isBase10 = true
    numDigits = 0

  const
    baseCodeChars = {'X', 'x', 'o', 'b', 'B'}
    literalishChars = baseCodeChars + {'A'..'F', 'a'..'f', '0'..'9', '_', '\''}
    floatTypes = {tkFloatLit, tkFloat32Lit, tkFloat64Lit, tkFloat128Lit}

  var number = lex.initToken(tkIntLit) # int literal until we know better
  if lex.at() == '-':
    eatChar(lex, number)

  let startpos = lex.bufpos

  # First stage: find out base, make verifications, build token literal string
  if lex.at() == '0' and lex.at(+1) in baseCodeChars + {'O'}:
    eatChar(lex, number)
    case lex.at():
      of 'O':
        lexMessageLitNum(lex,
          "$1 is an invalid int literal; For octal literals " &
          "use the '0o' prefix.", startpos, rlexInvalidIntegerPrefix)

      of 'x', 'X':
        eatChar(lex, number)
        numDigits = matchUnderscoreChars(lex, number, {'0'..'9', 'a'..'f', 'A'..'F'})
      of 'o':
        eatChar(lex, number)
        numDigits = matchUnderscoreChars(lex, number, {'0'..'7'})
      of 'b', 'B':
        eatChar(lex, number)
        numDigits = matchUnderscoreChars(lex, number, {'0'..'1'})
      else:
        lex.config.internalError(getLineInfo(lex), rintIce, "getNumber")

    if numDigits == 0:
      lexMessageLitNum(lex, "invalid number: '$1'", startpos, rlexInvalidIntegerLiteral)

  else:
    discard matchUnderscoreChars(lex, number, {'0'..'9'})
    if (lex.at() == '.') and (lex.at(+1) in {'0'..'9'}):
      number.kind = tkFloatLit
      eatChar(lex, number)
      discard matchUnderscoreChars(lex, number, {'0'..'9'})
    if lex.at() in {'e', 'E'}:
      number.kind = tkFloatLit
      eatChar(lex, number)
      if lex.at() in {'+', '-'}:
        eatChar(lex, number)
      discard matchUnderscoreChars(lex, number, {'0'..'9'})

  let endpos = lex.bufpos

  # Second stage, find out if there's a datatype suffix and handle it
  var postPos = endpos

  var customSuffix: Option[Token]
  if lex.buf[postPos] in {'\'', 'f', 'F', 'd', 'D', 'i', 'I', 'u', 'U'}:
    let errPos = postPos
    var customLitPossible = false
    if lex.buf[postPos] == '\'':
      inc(postPos)
      customLitPossible = true

    if lex.buf[postPos] in SymChars:
      var suffix = lex.initToken(tkCustomLitCall, postPos)
      while true:
        suffix.extend(postPos)
        inc postPos
        if lex.buf[postPos] notin SymChars + {'_'}:
          break

      let suffixAsLower = lex.buf[suffix.slice].toLowerAscii()

      case suffixAsLower:
        of "f", "f32": number.kind = tkFloat32Lit
        of "d", "f64": number.kind = tkFloat64Lit
        of "f128":     number.kind = tkFloat128Lit
        of "i8":       number.kind = tkInt8Lit
        of "i16":      number.kind = tkInt16Lit
        of "i32":      number.kind = tkInt32Lit
        of "i64":      number.kind = tkInt64Lit
        of "u":        number.kind = tkUIntLit
        of "u8":       number.kind = tkUInt8Lit
        of "u16":      number.kind = tkUInt16Lit
        of "u32":      number.kind = tkUInt32Lit
        of "u64":      number.kind = tkUInt64Lit
        elif customLitPossible:
          customSuffix = some(suffix)
          number.kind = tkCustomLitText

        else:
          lexMessageLitNum(
            lex, "invalid number suffix: '$1'",
            errPos, rlexInvalidIntegerSuffix)

    else:
      lexMessageLitNum(
        lex, "invalid number suffix: '$1'",
        errPos, rlexInvalidIntegerSuffix)

  # Is there still a literalish char awaiting? Then it's an error!
  if  lex.buf[postPos] in literalishChars or
     (lex.buf[postPos] == '.' and lex.buf[postPos + 1] in {'0'..'9'}):
    lexMessageLitNum(lex, "invalid number: '$1'", startpos, rlexInvalidIntegerLiteral)

  lex.bufpos = postPos
  result.add(number)
  if customSuffix.isSome():
    result.add(customSuffix.get())


proc handleHexChar(lex: var Lexer, xi: var int; position: range[0..4]) =
  template invalid() =
    lex.localReport(LexerReport(
      kind: rlexExpectedHex,
      msg: "expected a hex digit, but found: " & lex.at() &
        "; maybe prepend with 0"))

  case lex.at():
    of '0'..'9':
      xi = (xi shl 4) or (ord(lex.at()) - ord('0'))
      lex.next()
    of 'a'..'f':
      xi = (xi shl 4) or (ord(lex.at()) - ord('a') + 10)
      lex.next()
    of 'A'..'F':
      xi = (xi shl 4) or (ord(lex.at()) - ord('A') + 10)
      lex.next()
    of '"', '\'':
      if position <= 1: invalid()
      # do not progress the bufpos here.
      if position == 0: lex.next()
    else:
      invalid()
      # Need to progress for `nim check`
      lex.next()

proc handleDecChars(lex: var Lexer, xi: var int) =
  while lex.at() in {'0'..'9'}:
    xi = (xi * 10) + (ord(lex.at()) - ord('0'))
    lex.next()

proc addUnicodeCodePoint(s: var string, i: int) =
  let i = cast[uint](i)
  # inlined toUTF-8 to avoid unicode and strutils dependencies.
  let pos = s.len
  if i <= 127:
    s.setLen(pos + 1)
    s[pos + 0] = chr(i)
  elif i <= 0x07FF:
    s.setLen(pos + 2)
    s[pos + 0] = chr((i shr 6) or 0b110_00000)
    s[pos + 1] = chr((i and ones(6)) or 0b10_0000_00)
  elif i <= 0xFFFF:
    s.setLen(pos + 3)
    s[pos + 0] = chr(i shr 12 or 0b1110_0000)
    s[pos + 1] = chr(i shr 6 and ones(6) or 0b10_0000_00)
    s[pos + 2] = chr(i and ones(6) or 0b10_0000_00)
  elif i <= 0x001FFFFF:
    s.setLen(pos + 4)
    s[pos + 0] = chr(i shr 18 or 0b1111_0000)
    s[pos + 1] = chr(i shr 12 and ones(6) or 0b10_0000_00)
    s[pos + 2] = chr(i shr 6 and ones(6) or 0b10_0000_00)
    s[pos + 3] = chr(i and ones(6) or 0b10_0000_00)
  elif i <= 0x03FFFFFF:
    s.setLen(pos + 5)
    s[pos + 0] = chr(i shr 24 or 0b111110_00)
    s[pos + 1] = chr(i shr 18 and ones(6) or 0b10_0000_00)
    s[pos + 2] = chr(i shr 12 and ones(6) or 0b10_0000_00)
    s[pos + 3] = chr(i shr 6 and ones(6) or 0b10_0000_00)
    s[pos + 4] = chr(i and ones(6) or 0b10_0000_00)
  elif i <= 0x7FFFFFFF:
    s.setLen(pos + 6)
    s[pos + 0] = chr(i shr 30 or 0b1111110_0)
    s[pos + 1] = chr(i shr 24 and ones(6) or 0b10_0000_00)
    s[pos + 2] = chr(i shr 18 and ones(6) or 0b10_0000_00)
    s[pos + 3] = chr(i shr 12 and ones(6) or 0b10_0000_00)
    s[pos + 4] = chr(i shr 6 and ones(6) or 0b10_0000_00)
    s[pos + 5] = chr(i and ones(6) or 0b10_0000_00)

proc getEscapedChar(lex: var Lexer, tok: var Token) =
  lex.next()               # skip '\'
  case lex.at()
  of 'n', 'N':
    lex.eatChar(tok)

  of 'p', 'P':
    if tok.kind == tkCharLit:
      lex.localReport(LexerReport(
        kind: rlexInvalidCharLiteral,
        msg: "\\p not allowed in character literal"))

    lex.eatChar(tok)
  of 'r', 'R', 'c', 'C', 'l', 'L', 'f', 'F',
     'e', 'E', 'a', 'A', 'b', 'B', 'v', 'V',
     't', 'T', '\'', '\"', '\\':
    lex.eatChar(tok)

  of 'x', 'X':
    lex.next()
    var xi = 0
    handleHexChar(lex, xi, 1)
    handleHexChar(lex, xi, 2)
  of 'u', 'U':
    if tok.kind == tkCharLit:
      lex.localReport(LexerReport(
        kind: rlexInvalidCharLiteral,
        msg: "\\u not allowed in character literal"))
    lex.next()
    var xi = 0
    if lex.at() == '{':
      lex.next()
      var start = lex.bufpos
      while lex.at() != '}':
        handleHexChar(lex, xi, 0)
      if start == lex.bufpos:
        lex.localReport(LexerReport(
          kind: rlexInvalidUnicodeCodepoint,
          msg: "Unicode codepoint cannot be empty"))
      lex.next()
      if xi > 0x10FFFF:
        let hex = lex.buf[start .. lex.bufpos-2]
        lex.localReport(LexerReport(
          kind: rlexInvalidUnicodeCodepoint,
          msg: "Unicode codepoint must be lower than 0x10FFFF, but was: " & hex))
    else:
      handleHexChar(lex, xi, 1)
      handleHexChar(lex, xi, 2)
      handleHexChar(lex, xi, 3)
      handleHexChar(lex, xi, 4)

  of '0'..'9':
    if matchTwoChars(lex, '0', {'0' .. '9'}):
      lex.localReport(LexerReport(kind: rlexDeprecatedOctalPrefix))

    var xi = 0
    handleDecChars(lex, xi)
    if 255 < xi:
      lex.localReport(LexerReport(kind: rlexInvalidCharLiteral))

  else:
    lex.localReport(LexerReport(kind: rlexInvalidCharLiteral))

proc handleCR(lex: var Lexer, pos: int): int =
  ## Call this if you scanned over '\c' in the buffer; it returns the
  ## position to continue the scanning from. `pos` must be the position
  ## of the '\c'.
  assert(lex.buf[pos] == '\c')
  inc(lex.lineNumber)
  lex.lineStart = result

proc handleLF(lex: var Lexer, pos: int): int =
  ## Call this if you scanned over '\L' in the buffer; it returns the
  ## position to continue the scanning from. `pos` must be the position
  ## of the '\L'.
  assert(lex.buf[pos] == '\L')
  inc(lex.lineNumber)
  lex.lineStart = result

proc handleCRLF(lex: var Lexer, pos: int): int =
  template registerLine() =
    if MaxLineLength < lex.getColNumber(pos):
      lex.localReportPos(
        LexerReport(kind: rlexLineTooLong), pos)

  case lex.buf[pos]:
    of CR:
      registerLine()
      result = handleCR(lex, pos)
    of LF:
      registerLine()
      result = handleLF(lex, pos)
    else:
      result = pos

type
  StringMode = enum
    normal
    raw
    generalized

proc getString(lex: var Lexer, mode: StringMode): Token =
  result = lex.initToken(tkStrLit)
  var pos = lex.bufpos
  var line = lex.lineNumber         # save linenumber for better error message
  # lex.tokenBegin(tok, pos - ord(mode == raw))
  inc pos # skip "
  if lex.buf[pos] == '\"' and lex.buf[pos + 1] == '\"':
    result.kind = tkTripleStrLit # long string literal:
    inc(pos, 2)               # skip ""
    # skip leading newline:
    if lex.buf[pos] in {' ', '\t'}:
      var newpos = pos + 1
      while lex.buf[newpos] in {' ', '\t'}:
        inc newpos

      if lex.buf[newpos] in {CR, LF}:
        pos = newpos

    pos = handleCRLF(lex, pos)
    while true:
      case lex.buf[pos]
      of '\"':
        if lex.buf[pos + 1] == '\"' and
           lex.buf[pos + 2] == '\"' and
           lex.buf[pos + 3] != '\"':
          lex.bufpos = pos + 3 # skip the three """
          break

        lex.eatChar(result)
        inc(pos)

      of CR, LF:
        pos = handleCRLF(lex, pos)
        lex.eatChar(result)

      of nimlexbase.EndOfFile:
        var line2 = lex.lineNumber
        lex.lineNumber = line
        lex.localReportPos(LexerReport(
          kind: rlexUnclosedTripleString), lex.lineStart)
        lex.lineNumber = line2
        lex.bufpos = pos
        break

      else:
        result.extend(pos)
        inc(pos)

  else:
    # ordinary string literal
    if mode != normal:
      result.kind = tkRStrLit

    else:
      result.kind = tkStrLit

    while true:
      var c = lex.buf[pos]
      if c == '\"':
        if mode != normal and lex.buf[pos + 1] == '\"':
          inc(pos, 2)
          result.extend(pos)
        else:
          inc(pos) # skip '"'
          break

      elif c in {CR, LF, nimlexbase.EndOfFile}:
        lex.localReport LexerReport(kind: rlexUnclosedSingleString)
        break

      elif (c == '\\') and mode == normal:
        lex.bufpos = pos
        getEscapedChar(lex, result)
        pos = lex.bufpos
      else:
        result.extend(pos)
        inc(pos)

    lex.bufpos = pos

proc getCharacter(lex: var Lexer; tok: var Token) =
  let startPos = lex.bufpos
  lex.next()               # skip '
  case lex.at():
    of '\0'..pred(' '), '\'':
      lex.localReport LexerReport(kind: rlexInvalidCharLiteral)

    of '\\':
      getEscapedChar(lex, tok)

    else:
      lex.next()

  if lex.at() == '\'':
    lex.next() # skip '

  else:
    if startPos > 0 and lex.buf[startPos - 1] == '`':
      tok.extend(startPos)
      lex.bufpos = startPos + 1

    else:
      lex.localReport LexerReport(kind: rlexMissingClosingApostrophe)

const
  UnicodeOperatorStartChars = {'\226', '\194', '\195'}
    # the allowed unicode characters ("∙ ∘ × ★ ⊗ ⊘ ⊙ ⊛ ⊠ ⊡ ∩ ∧ ⊓ ± ⊕ ⊖ ⊞ ⊟ ∪ ∨ ⊔")
    # all start with one of these.

type
  UnicodeOprPred = enum
    Mul
    Add

proc unicodeOprLen(buf: string; pos: int): (int8, UnicodeOprPred) =
  template m(opLen): untyped = (int8(opLen), Mul)
  template a(opLen): untyped = (int8(opLen), Add)
  result = 0.m
  case buf[pos]
  of '\226':
    case buf[pos + 1]:
      of '\136':
        case buf[pos + 2]:
          of '\152': result = 3.m # ∘
          of '\153': result = 3.m # ∙
          of '\167': result = 3.m # ∧
          of '\168': result = 3.a # ∨
          of '\169': result = 3.m # ∩
          of '\170': result = 3.a # ∪
          else: discard

      of '\138':
        case buf[pos + 2]:
          of '\147': result = 3.m # ⊓
          of '\148': result = 3.a # ⊔
          of '\149': result = 3.a # ⊕
          of '\150': result = 3.a # ⊖
          of '\151': result = 3.m # ⊗
          of '\152': result = 3.m # ⊘
          of '\153': result = 3.m # ⊙
          of '\155': result = 3.m # ⊛
          of '\158': result = 3.a # ⊞
          of '\159': result = 3.a # ⊟
          of '\160': result = 3.m # ⊠
          of '\161': result = 3.m # ⊡
          else: discard

      of '\152':
        if buf[pos + 2] == '\133':
          result = 3.m # ★

      else: discard

  of '\194':
    if buf[pos + 1] == '\177':
      result = 2.a # ±

  of '\195':
    if buf[pos + 1] == '\151':
      result = 2.m # ×

  else:
    discard

let tokenMap: Table[Hash, TokenKind] =
  block:
    var tmp: Table[Hash, TokenKind] = initTable[Hash, TokenKind]()
    for it in low(TokenKind) .. high(TokenKind):
      tmp[hashIgnoreCase($it)] = it

    tmp

proc tokenKind(name: string): TokenKind =
  ## Get token kind from it's string representation or `tkInvalid` if there
  ## is no such token.
  let hash = hashIgnoreCase(name)
  return tern(hash in tokenMap, tokenMap[hash], tkInvalid)

proc getSymbol(lex: var Lexer): Token =
  var pos = lex.bufpos
  result = lex.initToken(tkInvalid)
  var suspicious = false
  while true:
    var c = lex.buf[pos]
    case c
    of 'a'..'z', '0'..'9':
      inc(pos)

    of 'A'..'Z':
      c = chr(ord(c) + (ord('a') - ord('A')))
      inc(pos)
      suspicious = true

    of '_':
      if lex.buf[pos + 1] notin SymChars:
        lex.localReport LexerReport(kind: rlexMalformedTrailingUnderscre)
        break

      inc(pos)
      suspicious = true

    of '\x80'..'\xFF':
      if c in UnicodeOperatorStartChars and
         unicodeOprLen(lex.buf, pos)[0] != 0:
        break

      else:
        inc(pos)

    else:
      break

  result.extend(pos - 1)
  let tryToken = tokenKind(lex.buf[result.slice])
  if tryToken == tkInvalid:
    result.kind = tkSymbol
  else:
    result.kind = tryToken

  lex.bufpos = pos


proc endOperator(lex: var Lexer, tok: var Token, pos: int) =
  assert false, "fixme"
  lex.bufpos = pos

proc getOperator(lex: var Lexer): Token =
  result = lex.initToken(tkInvalid)
  var pos = lex.bufpos
  while true:
    var c = lex.buf[pos]
    if c in OpChars:
      inc(pos)

    elif c in UnicodeOperatorStartChars:
      let oprLen = unicodeOprLen(lex.buf, pos)[0]
      if oprLen == 0: break
      for i in 0 ..< oprLen:
        inc pos

    else:
      break

  endOperator(lex, result, pos)
  result.extend(pos - 1)
  # advance pos but don't store it in lex.bufpos so the next token (which might
  # be an operator too) gets the preceding spaces:
  while lex.buf[pos] == ' ':
    inc pos

proc transitionPrecedence(lex: Lexer, tok: var Token) =
  const
    MulPred = tkOpr9
    PlusPred = tkOpr8


  let str = lex.buf[tok.slice]

  # arrow like?
  if str.len > 1 and str[^1] == '>' and str[^2] in {'-', '~', '='}:
    tok.kind = tkOpr1
    return

  proc considerAsgn(value: TokenKind): TokenKind =
    if str[^1] == '=': tkOpr1 else: value

  tok.kind = case str[0]:
    of '*', '%', '/', '\\': considerAsgn(MulPred)
    of '+', '-', '|': considerAsgn(PlusPred)
    of '$', '^': considerAsgn(tkOpr10)
    of '=', '<', '>', '!': tkOpr5
    of '&': considerAsgn(tkOpr7)
    of '.': considerAsgn(tkOpr6)
    of '?': tkOpr2
    of '~': tkOpr
    of UnicodeOperatorStartChars:
      if str[^1] == '=':
        tkOpr1

      else:
        let (len, pred) = unicodeOprLen(str, 0)
        if len != 0:
          if pred == Mul:
            MulPred
          else:
            PlusPred

        else:
          tkOpr2

    else:
      considerAsgn(tkOpr2)


proc getPrecedence*(tok: Token): int =
  ## Calculates the precedence of the given token.
  case tok.kind:
    of tkIn, tkNotin, tkIs, tkIsnot, tkOf, tkAs, tkFrom: result = 5
    of tkDiv, tkMod, tkShl, tkShr: result = 9
    of tkOr, tkXor, tkPtr, tkRef: result = 3
    of tkOpr1 .. tkOpr10: result = ord(tkOpr10) - ord(tok.kind) + 1
    of tkDotDot: result = 6
    of tkAnd: result = 4
    else: return -10

proc newlineFollows*(lex: Lexer): bool =
  var pos = lex.bufpos
  while true:
    case lex.buf[pos]
    of ' ', '\t':
      inc(pos)
    of CR, LF:
      result = true
      break
    of '#':
      inc(pos)
      if lex.buf[pos] == '#': inc(pos)
      if lex.buf[pos] != '[': return true
    else:
      break

proc skipMultiLineComment(
    lex: var Lexer,
    result: var Token,
    start: int,
    isDoc: bool
  ) =

  var pos = start
  var toStrip = 0
  # detect the amount of indentation:
  if isDoc:
    toStrip = getColNumber(lex, pos)
    while lex.buf[pos] == ' ':
      inc pos
      inc toStrip
    while lex.buf[pos] in {CR, LF}:  # skip blank lines
      pos = handleCRLF(lex, pos)
      toStrip = 0
      while lex.buf[pos] == ' ':
        inc pos
        inc toStrip
  var nesting = 0
  while true:
    case lex.buf[pos]
    of '#':
      if isDoc:
        if lex.buf[pos + 1] == '#' and lex.buf[pos + 2] == '[':
          inc nesting

      elif lex.buf[pos + 1] == '[':
        inc nesting

      inc pos
    of ']':
      if isDoc:
        if lex.buf[pos + 1] == '#' and lex.buf[pos + 2] == '#':
          if nesting == 0:
            inc(pos, 3)
            break
          dec nesting

      elif lex.buf[pos + 1] == '#':
        if nesting == 0:
          inc(pos, 2)
          break

        dec nesting
      inc pos
    of CR, LF:
      pos = handleCRLF(lex, pos)
      # strip leading whitespace:
      if isDoc:
        var c = toStrip
        while lex.buf[pos] == ' ' and c > 0:
          inc pos
          dec c

    of nimlexbase.EndOfFile:
      lex.localReportPos(
        LexerReport(kind: rlexUnclosedComment), pos)

      break

    else:
      inc(pos)

    result.extend(pos)

  lex.bufpos = pos

proc scanComment(lex: var Lexer): Token =
  result = lex.initToken(tkComment)
  var pos = lex.bufpos
  assert lex.buf[pos + 1] == '#'

  if lex.buf[pos + 2] == '[':
    skipMultiLineComment(lex, result, pos + 3, true)
    return

  result.start(pos)
  inc(pos, 2)

  var toStrip = 0
  var stripInit = false

  while true:
    if not stripInit:  # find baseline indentation inside comment
      while lex.buf[pos] == ' ':
        inc pos
        inc toStrip
      if lex.buf[pos] in {CR, LF}:  # don't set toStrip in blank comment lines
        toStrip = 0
      else:  # found first non-whitespace character
        stripInit = true
    var lastBackslash = -1
    while lex.buf[pos] notin {CR, LF, nimlexbase.EndOfFile}:
      if lex.buf[pos] == '\\': lastBackslash = pos + 1
      result.extend(pos)
      inc(pos)

    pos = handleCRLF(lex, pos)
    var indent = 0
    while lex.buf[pos] == ' ':
      inc(pos)
      inc(indent)

    if lex.buf[pos] == '#' and lex.buf[pos + 1] == '#':
      result.extend(pos)
      inc(pos, 2)
      if stripInit:
        var c = toStrip
        while lex.buf[pos] == ' ' and c > 0:
          inc pos
          dec c
    else:
      if lex.buf[pos] > ' ':
        lex.indentAhead = indent

      break

  lex.bufpos = pos

proc skip(lex: var Lexer, tok: var Token) =
  lex.tokenBegin(tok)
  var pos = lex.bufpos
  tok.line = -1
  while true:
    case lex.buf[pos]
    of ' ':
      inc(pos)

    of '\t':
      lex.localReportPos(
        LexerReport(kind: rlexNoTabs), pos)

      inc(pos)
    of CR, LF:
      pos = handleCRLF(lex, pos)
      var indent = 0
      while true:
        if lex.buf[pos] == ' ':
          inc(pos)
          inc(indent)
        elif lex.buf[pos] == '#' and lex.buf[pos + 1] == '[':
          skipMultiLineComment(lex, tok, pos + 2, false)
          pos = lex.bufpos
        else:
          break

      if lex.buf[pos] > ' ' and (lex.buf[pos] != '#' or lex.buf[pos + 1] == '#'):
        tok.indent = indent
        lex.currLineIndent = indent
        break
    of '#':
      # do not skip documentation comment:
      if lex.buf[pos + 1] == '#': break
      if lex.buf[pos + 1] == '[':
        skipMultiLineComment(lex, tok, pos + 2, false)
        pos = lex.bufpos

      else:
        lex.tokenBegin(tok, pos)
        while lex.buf[pos] notin {CR, LF, nimlexbase.EndOfFile}:
          inc(pos)

    else:
      break # EndOfFile also leaves the loop

  lex.bufpos = pos

proc rawGetTok*(lex: var Lexer): seq[Token] =
  block:
    var tmp: Token
    skip(lex, tmp)

  var c = lex.at()
  if c in SymStartChars - {'r', 'R'} - UnicodeOperatorStartChars:
    lex.add getSymbol(lex)
  else:
    case c
    of UnicodeOperatorStartChars:
      if unicodeOprLen(lex.buf, lex.bufpos)[0] != 0:
        lex.add getOperator(lex)
      else:
        lex.add getSymbol(lex)

    of '#':
      lex.add scanComment(lex)

    of '*':
      # '*:' is unfortunately a special case, because it is two tokens in
      # 'var v*: int'.
      if lex.at(+1) == ':' and lex.at(+2) notin OpChars:
        assert false, "fixme"
        # endOperator(lex, tok, lex.bufpos + 1)
      else:
        lex.add getOperator(lex)
    of ',':
      lex.add lex.initToken(tkComma)
      lex.next()
    of 'r', 'R':
      if lex.at(+1) == '\"':
        lex.next()
        lex.add getString(lex, raw)
      else:
        lex.add getSymbol(lex)

    of '(':
      var tok = lex.initToken(tkParLe)
      lex.next()
      if lex.at() == '.' and lex.at(+1) != '.':
        tok.extend(lex.bufpos)
        tok.kind = tkParDotLe
        lex.next()

      lex.add tok

    of ')':
      lex.add lex.initToken(tkParRi)
      lex.next()

    of '[':
      var tok = lex.initToken(tkBracketLe)
      lex.next()
      if lex.at() == '.' and lex.at(+1) != '.':
        tok.extend(lex.bufpos)
        tok.kind = tkBracketDotLe
        lex.next()

      elif lex.at() == ':':
        tok.extend(lex.bufpos)
        tok.kind = tkBracketLeColon
        lex.next()

      lex.add tok

    of ']':
      lex.add lex.initToken(tkBracketRi)
      lex.next()

    of '.':
      if lex.at(+1) == ']':
        lex.add lex.initToken(tkBracketDotRi)
        lex.next(2)

      elif lex.at(+1) == '}':
        lex.add lex.initToken(tkCurlyDotRi)
        lex.next(2)

      elif lex.at(+1) == ')':
        lex.add lex.initToken(tkParDotRi)
        lex.next(2)

      else:
        lex.add getOperator(lex)

    of '{':
      var tok = lex.initToken(tkCurlyLe)
      lex.next()
      if lex.at() == '.' and lex.at(+1) != '.':
        tok.extend(lex.bufpos)
        tok.kind = tkCurlyDotLe
        lex.next()

      lex.add(tok)

    of '}':
      lex.add lex.initToken(tkCurlyRi)
      lex.next()

    of ';':
      lex.add lex.initToken(tkSemiColon)
      lex.next()

    of '`':
      lex.add lex.initToken(tkAccent)
      lex.next()

    of '_':
      var tok = lex.initToken(tkInvalid)
      lex.next()
      if lex.at() notin SymChars + {'_'}:
        tok.kind = tkSymbol

      else:
        lex.localReport LexerReport(
          kind: rlexInvalidToken,
          msg: "invalid token: " & c & " (\\" & $(ord(c)) & ')')

    of '\"':
      # check for generalized raw string literalex:
      let mode = if lex.bufpos > 0 and
                    lex.buf[lex.bufpos-1] in SymChars:
                   generalized
                 else:
                   normal

      var tok = getString(lex, mode)
      if mode == generalized:
        case tok.kind:
          of tkRStrLit: tok.kind = tkGStrLit
          of tkTripleStrLit: tok.kind = tkGTripleStrLit
          else: discard

      lex.add tok

    of '\'':
      var tok = lex.initToken(tkCharLit)
      getCharacter(lex, tok)
      tok.kind = tkCharLit
      lex.add tok

    of '0'..'9':
      lex.add getNumber(lex)
      let c = lex.at()
      if c in SymChars + {'_'}:
        if c in UnicodeOperatorStartChars and
           unicodeOprLen(lex.buf, lex.bufpos)[0] != 0:
          discard

        else:
          lex.localReport LexerReport(
            kind: rlexInvalidToken,
            msg: "invalid token: no whitespace between number and identifier")

    of '-':
      if lex.at(+1) in {'0'..'9'} and
          (lex.bufpos-1 == 0 or lex.buf[lex.bufpos-1] in UnaryMinusWhitelist):
        # x)-23 # binary minus
        # ,-23  # unary minus
        # \n-78 # unary minus? Yes.
        # =-3   # parsed as `=-` anyway
        lex.add getNumber(lex)
        let c = lex.at()
        if c in SymChars+{'_'}:
          if c in UnicodeOperatorStartChars and
             unicodeOprLen(lex.buf, lex.bufpos)[0] != 0:
            discard
          else:
            lex.localReport LexerReport(
              kind: rlexInvalidToken,
              msg: "invalid token: no whitespace between number and identifier")

      else:
        lex.add getOperator(lex)

    else:
      if c in OpChars:
        lex.add getOperator(lex)

      elif c == nimlexbase.EndOfFile:
        lex.add lex.initToken(tkEof)
        lex.last().indent = 0

      else:
        lex.localReport LexerReport(
          kind: rlexInvalidToken,
          msg: "invalid token: " & c & " (\\" & $(ord(c)) & ')')
        lex.next()

proc rawGetTok*(lex: var Lexer, tok: var Token) =
  tok = lex.at(lex.tokenId)
  lex.tokenId = succ(lex.tokenId)

proc getIndentWidth*(
    fileIdx: FileIndex,
    inputstream: PLLStream,
    config: ConfigRef
  ): int =

  var lex: Lexer
  var tok: Token
  initToken(tok)
  openLexer(lex, fileIdx, inputstream, config)
  var prevToken = tkEof
  while tok.kind != tkEof:
    rawGetTok(lex, tok)
    if tok.indent > 0 and
       prevToken in {tkColon, tkEquals, tkType, tkConst, tkLet, tkVar, tkUsing}:
      result = tok.indent
      if result > 0: break
    prevToken = tok.kind
  closeLexer(lex)

