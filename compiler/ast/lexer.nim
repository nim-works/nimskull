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
    reports,
    idents
  ],
  std/[
    parseutils,
    hashes,
    strutils
  ],
  compiler/front/[
    options,
    msgs
  ]

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
  TokType* = enum
    tkInvalid = "tkInvalid", tkEof = "[EOF]", # order is important here!
    tkSymbol = "tkSymbol", # keywords:
    tkAddr = "addr", tkAnd = "and", tkAs = "as", tkAsm = "asm",
    tkBind = "bind", tkBlock = "block", tkBreak = "break", tkCase = "case", tkCast = "cast",
    tkConcept = "concept", tkConst = "const", tkContinue = "continue", tkConverter = "converter",
    tkDefer = "defer", tkDiscard = "discard", tkDistinct = "distinct", tkDiv = "div", tkDo = "do",
    tkElif = "elif", tkElse = "else", tkEnd = "end", tkEnum = "enum", tkExcept = "except", tkExport = "export",
    tkFinally = "finally", tkFor = "for", tkFrom = "from", tkFunc = "func",
    tkIf = "if", tkImport = "import", tkIn = "in", tkInclude = "include", tkInterface = "interface",
    tkIs = "is", tkIsnot = "isnot", tkIterator = "iterator",
    tkLet = "let",
    tkMacro = "macro", tkMethod = "method", tkMixin = "mixin", tkMod = "mod", tkNil = "nil", tkNot = "not", tkNotin = "notin",
    tkObject = "object", tkOf = "of", tkOr = "or", tkOut = "out",
    tkProc = "proc", tkPtr = "ptr", tkRaise = "raise", tkRef = "ref", tkReturn = "return",
    tkShl = "shl", tkShr = "shr", tkStatic = "static",
    tkTemplate = "template",
    tkTry = "try", tkTuple = "tuple", tkType = "type", tkUsing = "using",
    tkVar = "var", tkWhen = "when", tkWhile = "while", tkXor = "xor",
    tkYield = "yield", # end of keywords

    tkIntLit = "tkIntLit", tkInt8Lit = "tkInt8Lit", tkInt16Lit = "tkInt16Lit",
    tkInt32Lit = "tkInt32Lit", tkInt64Lit = "tkInt64Lit",
    tkUIntLit = "tkUIntLit", tkUInt8Lit = "tkUInt8Lit", tkUInt16Lit = "tkUInt16Lit",
    tkUInt32Lit = "tkUInt32Lit", tkUInt64Lit = "tkUInt64Lit",
    tkFloatLit = "tkFloatLit", tkFloat32Lit = "tkFloat32Lit",
    tkFloat64Lit = "tkFloat64Lit", tkFloat128Lit = "tkFloat128Lit",
    tkStrLit = "tkStrLit", tkRStrLit = "tkRStrLit", tkTripleStrLit = "tkTripleStrLit",
    tkGStrLit = "tkGStrLit", tkGTripleStrLit = "tkGTripleStrLit", tkCharLit = "tkCharLit",
    tkCustomLit = "tkCustomLit",

    tkParLe = "(", tkParRi = ")", tkBracketLe = "[",
    tkBracketRi = "]", tkCurlyLe = "{", tkCurlyRi = "}",
    tkBracketDotLe = "[.", tkBracketDotRi = ".]",
    tkCurlyDotLe = "{.", tkCurlyDotRi = ".}",
    tkParDotLe = "(.", tkParDotRi = ".)",
    tkComma = ",", tkSemiColon = ";",
    tkColon = ":", tkColonColon = "::", tkEquals = "=",
    tkDot = ".", tkDotDot = "..", tkBracketLeColon = "[:",
    tkOpr, tkComment, tkAccent = "`",
    # these are fake tokens used by renderer.nim
    tkSpaces, tkInfixOpr, tkPrefixOpr, tkPostfixOpr, tkHideableStart, tkHideableEnd

  TokTypes* = set[TokType]

const
  weakTokens = {tkComma, tkSemiColon, tkColon,
                tkParRi, tkParDotRi, tkBracketRi, tkBracketDotRi,
                tkCurlyRi} # \
    # tokens that should not be considered for previousToken
  tokKeywordLow* = succ(tkSymbol)
  tokKeywordHigh* = pred(tkIntLit)

type
  NumericalBase* = enum
    base10,                   ## base10 is listed as the first element,
                              ## so that it is the correct default value
    base2, base8, base16

  Token* = object             ## a Nim token
    tokType*: TokType         ## the type of the token
    indent*: int              ## the indentation; != -1 if the token has been
                              ## preceded with indentation
    ident*: PIdent            ## the parsed identifier
    iNumber*: BiggestInt      ## the parsed integer literal
    fNumber*: BiggestFloat    ## the parsed floating point literal
    base*: NumericalBase      ## the numerical base; only valid for int
                              ## or float literals
    strongSpaceA*: int8       ## leading spaces of an operator
    strongSpaceB*: int8       ## trailing spaces of an operator
    literal*: string          ## the parsed (string) literal; and
                              ## documentation comments are here too
    line*, col*: int
    when defined(nimpretty):
      offsetA*, offsetB*: int ## used for pretty printing so that literals
                              ## like 0b01 or  r"\L" are unaffected
      commentOffsetA*, commentOffsetB*: int

  Lexer* = object of TBaseLexer
    fileIdx*: FileIndex
    indentAhead*: int         ## if > 0 an indentation has already been read
                              ## this is needed because scanning comments
                              ## needs so much look-ahead
    currLineIndent*: int
    strongSpaces*, allowTabs*: bool
    cache*: IdentCache
    when defined(nimsuggest):
      previousToken: TLineInfo
    config*: ConfigRef

proc getLineInfo*(L: Lexer, tok: Token): TLineInfo {.inline.} =
  result = newLineInfo(L.fileIdx, tok.line, tok.col)
  when defined(nimpretty):
    result.offsetA = tok.offsetA
    result.offsetB = tok.offsetB
    result.commentOffsetA = tok.commentOffsetA
    result.commentOffsetB = tok.commentOffsetB

func isKeyword*(kind: TokType): bool =
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
  case tok.tokType
  of tkIntLit..tkInt64Lit: $tok.iNumber
  of tkFloatLit..tkFloat64Lit: $tok.fNumber
  of tkInvalid, tkStrLit..tkCharLit, tkComment: tok.literal
  of tkParLe..tkColon, tkEof, tkAccent: $tok.tokType
  else:
    if tok.ident != nil:
      tok.ident.s
    else:
      ""

proc prettyTok*(tok: Token): string =
  if isKeyword(tok.tokType): "keyword " & tok.ident.s
  else: $tok

proc printTok*(conf: ConfigRef; tok: Token) =
  # xxx factor with toLocation
  conf.writeln($tok.line & ":" & $tok.col & "\t" & $tok.tokType & " " & $tok)

proc initToken*(L: var Token) =
  L.tokType = tkInvalid
  L.iNumber = 0
  L.indent = 0
  L.strongSpaceA = 0
  L.literal = ""
  L.fNumber = 0.0
  L.base = base10
  L.ident = nil
  when defined(nimpretty):
    L.commentOffsetA = 0
    L.commentOffsetB = 0

proc fillToken(L: var Token) =
  L.tokType = tkInvalid
  L.iNumber = 0
  L.indent = 0
  L.strongSpaceA = 0
  setLen(L.literal, 0)
  L.fNumber = 0.0
  L.base = base10
  L.ident = nil
  when defined(nimpretty):
    L.commentOffsetA = 0
    L.commentOffsetB = 0

proc openLexer*(lex: var Lexer, fileIdx: FileIndex, inputstream: PLLStream;
                 cache: IdentCache; config: ConfigRef) =
  openBaseLexer(lex, inputstream)
  lex.fileIdx = fileIdx
  lex.indentAhead = -1
  lex.currLineIndent = 0
  inc(lex.lineNumber, inputstream.lineOffset)
  lex.cache = cache
  when defined(nimsuggest):
    lex.previousToken.fileIndex = fileIdx
  lex.config = config

proc openLexer*(lex: var Lexer, filename: AbsoluteFile, inputstream: PLLStream;
                cache: IdentCache; config: ConfigRef) =
  openLexer(lex, fileInfoIdx(config, filename), inputstream, cache, config)

proc closeLexer*(lex: var Lexer) =
  if lex.config != nil:
    inc(lex.config.linesCompiled, lex.lineNumber)
  closeBaseLexer(lex)

proc getLineInfo(L: Lexer): TLineInfo =
  result = newLineInfo(L.fileIdx, L.lineNumber, getColNumber(L, L.bufpos))

proc matchTwoChars(L: Lexer, first: char, second: set[char]): bool =
  result = (L.buf[L.bufpos] == first) and (L.buf[L.bufpos + 1] in second)

template tokenBegin(tok, pos) {.dirty.} =
  when defined(nimsuggest):
    var colA = getColNumber(L, pos)
  when defined(nimpretty):
    tok.offsetA = L.offsetBase + pos

template tokenEnd(tok, pos) {.dirty.} =
  when defined(nimsuggest):
    let colB = getColNumber(L, pos)+1
    if L.fileIdx == L.config.m.trackPos.fileIndex and L.config.m.trackPos.col in colA..colB and
        L.lineNumber == L.config.m.trackPos.line.int and L.config.ideCmd in {ideSug, ideCon}:
      L.config.m.trackPos.col = colA.int16
    colA = 0
  when defined(nimpretty):
    tok.offsetB = L.offsetBase + pos

template tokenEndIgnore(tok, pos) =
  when defined(nimsuggest):
    let colB = getColNumber(L, pos)
    if L.fileIdx == L.config.m.trackPos.fileIndex and L.config.m.trackPos.col in colA..colB and
        L.lineNumber == L.config.m.trackPos.line.int and L.config.ideCmd in {ideSug, ideCon}:
      L.config.m.trackPos.fileIndex = trackPosInvalidFileIdx
      L.config.m.trackPos.line = 0'u16
    colA = 0
  when defined(nimpretty):
    tok.offsetB = L.offsetBase + pos

template tokenEndPrevious(tok, pos) =
  when defined(nimsuggest):
    # when we detect the cursor in whitespace, we attach the track position
    # to the token that came before that, but only if we haven't detected
    # the cursor in a string literal or comment:
    let colB = getColNumber(L, pos)
    if L.fileIdx == L.config.m.trackPos.fileIndex and L.config.m.trackPos.col in colA..colB and
        L.lineNumber == L.config.m.trackPos.line.int and L.config.ideCmd in {ideSug, ideCon}:
      L.config.m.trackPos = L.previousToken
      L.config.m.trackPosAttached = true
    colA = 0
  when defined(nimpretty):
    tok.offsetB = L.offsetBase + pos

template eatChar(L: var Lexer, t: var Token, replacementChar: char) =
  t.literal.add(replacementChar)
  inc(L.bufpos)

template eatChar(L: var Lexer, t: var Token) =
  t.literal.add(L.buf[L.bufpos])
  inc(L.bufpos)

template localReport*(L: Lexer, report: ReportTypes): untyped =
  L.config.handleReport(wrap(report, instLoc(), getLineInfo(L)), instLoc())

template localReportTok*(L: Lexer, report: ReportTypes, tok: Token): untyped =
  L.config.handleReport(wrap(
    report, instLoc(), newLineInfo(L.fileIdx, tok.line, tok.col)), instLoc())

template localReportPos*(L: Lexer, report: ReportTypes, pos: int): untyped =
  L.config.handleReport(wrap(
    report, instLoc(), newLineInfo(
      L.fileIdx, L.lineNumber, pos - L.lineStart)), instLoc())


proc getNumber(L: var Lexer, result: var Token) =
  proc matchUnderscoreChars(L: var Lexer, tok: var Token, chars: set[char]): Natural =
    var pos = L.bufpos              # use registers for pos, buf
    result = 0
    while true:
      if L.buf[pos] in chars:
        tok.literal.add(L.buf[pos])
        inc(pos)
        inc(result)
      else:
        break
      if L.buf[pos] == '_':
        if L.buf[pos+1] notin chars:
          L.localReport(LexerReport(kind: rlexMalformedUnderscores))
          break
        tok.literal.add('_')
        inc(pos)
    L.bufpos = pos

  proc matchChars(L: var Lexer, tok: var Token, chars: set[char]) =
    var pos = L.bufpos              # use registers for pos, buf
    while L.buf[pos] in chars:
      tok.literal.add(L.buf[pos])
      inc(pos)
    L.bufpos = pos

  proc lexMessageLitNum(L: var Lexer, msg: string, startpos: int, msgKind: LexerReportKind) =
    # Used to get slightly human friendlier err messages.
    const literalishChars = {'A'..'Z', 'a'..'z', '0'..'9', '_', '.', '\''}
    var msgPos = L.bufpos
    var t: Token
    t.literal = ""
    L.bufpos = startpos # Use L.bufpos as pos because of matchChars
    matchChars(L, t, literalishChars)
    # We must verify +/- specifically so that we're not past the literal
    if  L.buf[L.bufpos] in {'+', '-'} and
        L.buf[L.bufpos - 1] in {'e', 'E'}:
      t.literal.add(L.buf[L.bufpos])
      inc(L.bufpos)
      matchChars(L, t, literalishChars)
    if L.buf[L.bufpos] in literalishChars:
      t.literal.add(L.buf[L.bufpos])
      inc(L.bufpos)
      matchChars(L, t, {'0'..'9'})
    L.bufpos = msgPos
    L.localReport(LexerReport(kind: msgKind, msg: msg % t.literal))

  var
    xi: BiggestInt
    isBase10 = true
    numDigits = 0
  const
    baseCodeChars = {'X', 'x', 'o', 'b', 'B'}
    literalishChars = baseCodeChars + {'A'..'F', 'a'..'f', '0'..'9', '_', '\''}
    floatTypes = {tkFloatLit, tkFloat32Lit, tkFloat64Lit, tkFloat128Lit}
  result.tokType = tkIntLit   # int literal until we know better
  result.literal = ""
  result.base = base10
  tokenBegin(result, L.bufpos)

  var isPositive = true
  if L.buf[L.bufpos] == '-':
    eatChar(L, result)
    isPositive = false

  let startpos = L.bufpos

  template setNumber(field, value) =
    field = (if isPositive: value else: -value)

  # First stage: find out base, make verifications, build token literal string
  if L.buf[L.bufpos] == '0' and L.buf[L.bufpos + 1] in baseCodeChars + {'O'}:
    isBase10 = false
    eatChar(L, result, '0')
    case L.buf[L.bufpos]
    of 'O':
      lexMessageLitNum(L, "$1 is an invalid int literal; For octal literals " &
                          "use the '0o' prefix.", startpos, rlexInvalidIntegerPrefix)
    of 'x', 'X':
      eatChar(L, result, 'x')
      numDigits = matchUnderscoreChars(L, result, {'0'..'9', 'a'..'f', 'A'..'F'})
    of 'o':
      eatChar(L, result, 'o')
      numDigits = matchUnderscoreChars(L, result, {'0'..'7'})
    of 'b', 'B':
      eatChar(L, result, 'b')
      numDigits = matchUnderscoreChars(L, result, {'0'..'1'})
    else:
      L.config.internalError(getLineInfo(L), rintIce, "getNumber")
    if numDigits == 0:
      lexMessageLitNum(L, "invalid number: '$1'", startpos, rlexInvalidIntegerLiteral)
  else:
    discard matchUnderscoreChars(L, result, {'0'..'9'})
    if (L.buf[L.bufpos] == '.') and (L.buf[L.bufpos + 1] in {'0'..'9'}):
      result.tokType = tkFloatLit
      eatChar(L, result, '.')
      discard matchUnderscoreChars(L, result, {'0'..'9'})
    if L.buf[L.bufpos] in {'e', 'E'}:
      result.tokType = tkFloatLit
      eatChar(L, result)
      if L.buf[L.bufpos] in {'+', '-'}:
        eatChar(L, result)
      discard matchUnderscoreChars(L, result, {'0'..'9'})
  let endpos = L.bufpos

  # Second stage, find out if there's a datatype suffix and handle it
  var postPos = endpos

  if L.buf[postPos] in {'\'', 'f', 'F', 'd', 'D', 'i', 'I', 'u', 'U'}:
    let errPos = postPos
    var customLitPossible = false
    if L.buf[postPos] == '\'':
      inc(postPos)
      customLitPossible = true

    if L.buf[postPos] in SymChars:
      var suffix = newStringOfCap(10)
      while true:
        suffix.add L.buf[postPos]
        inc postPos
        if L.buf[postPos] notin SymChars+{'_'}: break
      let suffixAsLower = suffix.toLowerAscii
      case suffixAsLower
      of "f", "f32": result.tokType = tkFloat32Lit
      of "d", "f64": result.tokType = tkFloat64Lit
      of "f128": result.tokType = tkFloat128Lit
      of "i8": result.tokType = tkInt8Lit
      of "i16": result.tokType = tkInt16Lit
      of "i32": result.tokType = tkInt32Lit
      of "i64": result.tokType = tkInt64Lit
      of "u": result.tokType = tkUIntLit
      of "u8": result.tokType = tkUInt8Lit
      of "u16": result.tokType = tkUInt16Lit
      of "u32": result.tokType = tkUInt32Lit
      of "u64": result.tokType = tkUInt64Lit
      elif customLitPossible:
        # remember the position of the `'` so that the parser doesn't
        # have to reparse the custom literal:
        result.iNumber = len(result.literal)
        result.literal.add '\''
        result.literal.add suffix
        result.tokType = tkCustomLit
      else:
        lexMessageLitNum(L, "invalid number suffix: '$1'", errPos, rlexInvalidIntegerSuffix)
    else:
      lexMessageLitNum(L, "invalid number suffix: '$1'", errPos, rlexInvalidIntegerSuffix)

  # Is there still a literalish char awaiting? Then it's an error!
  if  L.buf[postPos] in literalishChars or
     (L.buf[postPos] == '.' and L.buf[postPos + 1] in {'0'..'9'}):
    lexMessageLitNum(L, "invalid number: '$1'", startpos, rlexInvalidIntegerLiteral)

  if result.tokType != tkCustomLit:
    # Third stage, extract actual number
    L.bufpos = startpos            # restore position
    var pos = startpos
    try:
      if (L.buf[pos] == '0') and (L.buf[pos + 1] in baseCodeChars):
        inc(pos, 2)
        xi = 0                  # it is a base prefix

        case L.buf[pos - 1]
        of 'b', 'B':
          result.base = base2
          while pos < endpos:
            if L.buf[pos] != '_':
              xi = `shl`(xi, 1) or (ord(L.buf[pos]) - ord('0'))
            inc(pos)
        of 'o':
          result.base = base8
          while pos < endpos:
            if L.buf[pos] != '_':
              xi = `shl`(xi, 3) or (ord(L.buf[pos]) - ord('0'))
            inc(pos)
        of 'x', 'X':
          result.base = base16
          while pos < endpos:
            case L.buf[pos]
            of '_':
              inc(pos)
            of '0'..'9':
              xi = `shl`(xi, 4) or (ord(L.buf[pos]) - ord('0'))
              inc(pos)
            of 'a'..'f':
              xi = `shl`(xi, 4) or (ord(L.buf[pos]) - ord('a') + 10)
              inc(pos)
            of 'A'..'F':
              xi = `shl`(xi, 4) or (ord(L.buf[pos]) - ord('A') + 10)
              inc(pos)
            else:
              break
        else:
          L.config.internalError(getLineInfo(L), rintIce, "getNumber")

        case result.tokType
        of tkIntLit, tkInt64Lit: setNumber result.iNumber, xi
        of tkInt8Lit: setNumber result.iNumber, ashr(xi shl 56, 56)
        of tkInt16Lit: setNumber result.iNumber, ashr(xi shl 48, 48)
        of tkInt32Lit: setNumber result.iNumber, ashr(xi shl 32, 32)
        of tkUIntLit, tkUInt64Lit: setNumber result.iNumber, xi
        of tkUInt8Lit: setNumber result.iNumber, xi and 0xff
        of tkUInt16Lit: setNumber result.iNumber, xi and 0xffff
        of tkUInt32Lit: setNumber result.iNumber, xi and 0xffffffff
        of tkFloat32Lit:
          setNumber result.fNumber, (cast[PFloat32](addr(xi)))[]
          # note: this code is endian neutral!
          # XXX: Test this on big endian machine!
        of tkFloat64Lit, tkFloatLit:
          setNumber result.fNumber, (cast[PFloat64](addr(xi)))[]
        else:
          L.config.internalError(getLineInfo(L), rintIce, "getNumber")

        # Bounds checks. Non decimal literals are allowed to overflow the range of
        # the datatype as long as their pattern don't overflow _bitwise_, hence
        # below checks of signed sizes against uint*.high is deliberate:
        # (0x80'u8 = 128, 0x80'i8 = -128, etc == OK)
        if result.tokType notin floatTypes:
          let outOfRange =
            case result.tokType
            of tkUInt8Lit, tkUInt16Lit, tkUInt32Lit: result.iNumber != xi
            of tkInt8Lit:  (xi > BiggestInt(uint8.high))
            of tkInt16Lit: (xi > BiggestInt(uint16.high))
            of tkInt32Lit: (xi > BiggestInt(uint32.high))
            else: false

          if outOfRange:
            #echo "out of range num: ", result.iNumber, " vs ", xi
            lexMessageLitNum(
              L, "number out of range: '$1'", startpos, rlexNumberNotInRange)

      else:
        case result.tokType
        of floatTypes:
          result.fNumber = parseFloat(result.literal)
        of tkUInt64Lit, tkUIntLit:
          var iNumber: uint64
          var len: int
          try:
            len = parseBiggestUInt(result.literal, iNumber)
          except ValueError:
            raise newException(OverflowDefect, "number out of range: " & result.literal)
          if len != result.literal.len:
            raise newException(ValueError, "invalid integer: " & result.literal)
          result.iNumber = cast[int64](iNumber)
        else:
          var iNumber: int64
          var len: int
          try:
            len = parseBiggestInt(result.literal, iNumber)
          except ValueError:
            raise newException(OverflowDefect, "number out of range: " & result.literal)
          if len != result.literal.len:
            raise newException(ValueError, "invalid integer: " & result.literal)
          result.iNumber = iNumber

        # Explicit bounds checks.
        let outOfRange =
          case result.tokType
          of tkInt8Lit: result.iNumber > int8.high or result.iNumber < int8.low
          of tkUInt8Lit: result.iNumber > BiggestInt(uint8.high) or result.iNumber < 0
          of tkInt16Lit: result.iNumber > int16.high or result.iNumber < int16.low
          of tkUInt16Lit: result.iNumber > BiggestInt(uint16.high) or result.iNumber < 0
          of tkInt32Lit: result.iNumber > int32.high or result.iNumber < int32.low
          of tkUInt32Lit: result.iNumber > BiggestInt(uint32.high) or result.iNumber < 0
          else: false

        if outOfRange:
          lexMessageLitNum(L, "number out of range: '$1'", startpos, rlexNumberNotInRange)

      # Promote int literal to int64? Not always necessary, but more consistent
      if result.tokType == tkIntLit:
        if result.iNumber > high(int32) or result.iNumber < low(int32):
          result.tokType = tkInt64Lit

    except ValueError:
      lexMessageLitNum(L, "invalid number: '$1'", startpos, rlexInvalidIntegerLiteral)
    except OverflowDefect, RangeDefect:
      lexMessageLitNum(L, "number out of range: '$1'", startpos, rlexNumberNotInRange)
  tokenEnd(result, postPos-1)
  L.bufpos = postPos

proc handleHexChar(L: var Lexer, xi: var int; position: range[0..4]) =
  template invalid() =
    L.localReport(LexerReport(
      kind: rlexExpectedHex,
      msg: "expected a hex digit, but found: " & L.buf[L.bufpos] &
        "; maybe prepend with 0"))

  case L.buf[L.bufpos]
  of '0'..'9':
    xi = (xi shl 4) or (ord(L.buf[L.bufpos]) - ord('0'))
    inc(L.bufpos)
  of 'a'..'f':
    xi = (xi shl 4) or (ord(L.buf[L.bufpos]) - ord('a') + 10)
    inc(L.bufpos)
  of 'A'..'F':
    xi = (xi shl 4) or (ord(L.buf[L.bufpos]) - ord('A') + 10)
    inc(L.bufpos)
  of '"', '\'':
    if position <= 1: invalid()
    # do not progress the bufpos here.
    if position == 0: inc(L.bufpos)
  else:
    invalid()
    # Need to progress for `nim check`
    inc(L.bufpos)

proc handleDecChars(L: var Lexer, xi: var int) =
  while L.buf[L.bufpos] in {'0'..'9'}:
    xi = (xi * 10) + (ord(L.buf[L.bufpos]) - ord('0'))
    inc(L.bufpos)

proc addUnicodeCodePoint(s: var string, i: int) =
  let i = cast[uint](i)
  # inlined toUTF-8 to avoid unicode and strutils dependencies.
  let pos = s.len
  if i <= 127:
    s.setLen(pos+1)
    s[pos+0] = chr(i)
  elif i <= 0x07FF:
    s.setLen(pos+2)
    s[pos+0] = chr((i shr 6) or 0b110_00000)
    s[pos+1] = chr((i and ones(6)) or 0b10_0000_00)
  elif i <= 0xFFFF:
    s.setLen(pos+3)
    s[pos+0] = chr(i shr 12 or 0b1110_0000)
    s[pos+1] = chr(i shr 6 and ones(6) or 0b10_0000_00)
    s[pos+2] = chr(i and ones(6) or 0b10_0000_00)
  elif i <= 0x001FFFFF:
    s.setLen(pos+4)
    s[pos+0] = chr(i shr 18 or 0b1111_0000)
    s[pos+1] = chr(i shr 12 and ones(6) or 0b10_0000_00)
    s[pos+2] = chr(i shr 6 and ones(6) or 0b10_0000_00)
    s[pos+3] = chr(i and ones(6) or 0b10_0000_00)
  elif i <= 0x03FFFFFF:
    s.setLen(pos+5)
    s[pos+0] = chr(i shr 24 or 0b111110_00)
    s[pos+1] = chr(i shr 18 and ones(6) or 0b10_0000_00)
    s[pos+2] = chr(i shr 12 and ones(6) or 0b10_0000_00)
    s[pos+3] = chr(i shr 6 and ones(6) or 0b10_0000_00)
    s[pos+4] = chr(i and ones(6) or 0b10_0000_00)
  elif i <= 0x7FFFFFFF:
    s.setLen(pos+6)
    s[pos+0] = chr(i shr 30 or 0b1111110_0)
    s[pos+1] = chr(i shr 24 and ones(6) or 0b10_0000_00)
    s[pos+2] = chr(i shr 18 and ones(6) or 0b10_0000_00)
    s[pos+3] = chr(i shr 12 and ones(6) or 0b10_0000_00)
    s[pos+4] = chr(i shr 6 and ones(6) or 0b10_0000_00)
    s[pos+5] = chr(i and ones(6) or 0b10_0000_00)

proc getEscapedChar(L: var Lexer, tok: var Token) =
  inc(L.bufpos)               # skip '\'
  case L.buf[L.bufpos]
  of 'n', 'N':
    tok.literal.add('\L')
    inc(L.bufpos)
  of 'p', 'P':
    if tok.tokType == tkCharLit:
      L.localReport(LexerReport(
        kind: rlexInvalidCharLiteral,
        msg: "\\p not allowed in character literal"))

    tok.literal.add(L.config.target.tnl)
    inc(L.bufpos)
  of 'r', 'R', 'c', 'C':
    tok.literal.add(CR)
    inc(L.bufpos)
  of 'l', 'L':
    tok.literal.add(LF)
    inc(L.bufpos)
  of 'f', 'F':
    tok.literal.add(FF)
    inc(L.bufpos)
  of 'e', 'E':
    tok.literal.add(ESC)
    inc(L.bufpos)
  of 'a', 'A':
    tok.literal.add(BEL)
    inc(L.bufpos)
  of 'b', 'B':
    tok.literal.add(BACKSPACE)
    inc(L.bufpos)
  of 'v', 'V':
    tok.literal.add(VT)
    inc(L.bufpos)
  of 't', 'T':
    tok.literal.add('\t')
    inc(L.bufpos)
  of '\'', '\"':
    tok.literal.add(L.buf[L.bufpos])
    inc(L.bufpos)
  of '\\':
    tok.literal.add('\\')
    inc(L.bufpos)
  of 'x', 'X':
    inc(L.bufpos)
    var xi = 0
    handleHexChar(L, xi, 1)
    handleHexChar(L, xi, 2)
    tok.literal.add(chr(xi))
  of 'u', 'U':
    if tok.tokType == tkCharLit:
      L.localReport(LexerReport(
        kind: rlexInvalidCharLiteral,
        msg: "\\u not allowed in character literal"))
    inc(L.bufpos)
    var xi = 0
    if L.buf[L.bufpos] == '{':
      inc(L.bufpos)
      var start = L.bufpos
      while L.buf[L.bufpos] != '}':
        handleHexChar(L, xi, 0)
      if start == L.bufpos:
        L.localReport(LexerReport(
          kind: rlexInvalidUnicodeCodepoint,
          msg: "Unicode codepoint cannot be empty"))
      inc(L.bufpos)
      if xi > 0x10FFFF:
        let hex = ($L.buf)[start..L.bufpos-2]
        L.localReport(LexerReport(
          kind: rlexInvalidUnicodeCodepoint,
          msg: "Unicode codepoint must be lower than 0x10FFFF, but was: " & hex))
    else:
      handleHexChar(L, xi, 1)
      handleHexChar(L, xi, 2)
      handleHexChar(L, xi, 3)
      handleHexChar(L, xi, 4)
    addUnicodeCodePoint(tok.literal, xi)
  of '0'..'9':
    if matchTwoChars(L, '0', {'0'..'9'}):
      L.localReport(LexerReport(kind: rlexDeprecatedOctalPrefix))
    var xi = 0
    handleDecChars(L, xi)
    if (xi <= 255):
      tok.literal.add(chr(xi))
    else:
      L.localReport(LexerReport(kind: rlexInvalidCharLiteral))
  else:
    L.localReport(LexerReport(kind: rlexInvalidCharLiteral))

proc handleCRLF(L: var Lexer, pos: int): int =
  template registerLine =
    let col = L.getColNumber(pos)

    when not defined(nimpretty):
      if col > MaxLineLength:
        L.localReportPos(
          LexerReport(kind: rlexLineTooLong), pos)

  case L.buf[pos]
  of CR:
    registerLine()
    result = nimlexbase.handleCR(L, pos)
  of LF:
    registerLine()
    result = nimlexbase.handleLF(L, pos)
  else: result = pos

type
  StringMode = enum
    normal,
    raw,
    generalized

proc getString(L: var Lexer, tok: var Token, mode: StringMode) =
  var pos = L.bufpos
  var line = L.lineNumber         # save linenumber for better error message
  tokenBegin(tok, pos - ord(mode == raw))
  inc pos # skip "
  if L.buf[pos] == '\"' and L.buf[pos+1] == '\"':
    tok.tokType = tkTripleStrLit # long string literal:
    inc(pos, 2)               # skip ""
    # skip leading newline:
    if L.buf[pos] in {' ', '\t'}:
      var newpos = pos+1
      while L.buf[newpos] in {' ', '\t'}: inc newpos
      if L.buf[newpos] in {CR, LF}: pos = newpos
    pos = handleCRLF(L, pos)
    while true:
      case L.buf[pos]
      of '\"':
        if L.buf[pos+1] == '\"' and L.buf[pos+2] == '\"' and
            L.buf[pos+3] != '\"':
          tokenEndIgnore(tok, pos+2)
          L.bufpos = pos + 3 # skip the three """
          break
        tok.literal.add('\"')
        inc(pos)
      of CR, LF:
        tokenEndIgnore(tok, pos)
        pos = handleCRLF(L, pos)
        tok.literal.add("\n")
      of nimlexbase.EndOfFile:
        tokenEndIgnore(tok, pos)
        var line2 = L.lineNumber
        L.lineNumber = line
        L.localReportPos(LexerReport(
          kind: rlexUnclosedTripleString), L.lineStart)
        L.lineNumber = line2
        L.bufpos = pos
        break
      else:
        tok.literal.add(L.buf[pos])
        inc(pos)
  else:
    # ordinary string literal
    if mode != normal: tok.tokType = tkRStrLit
    else: tok.tokType = tkStrLit
    while true:
      var c = L.buf[pos]
      if c == '\"':
        if mode != normal and L.buf[pos+1] == '\"':
          inc(pos, 2)
          tok.literal.add('"')
        else:
          tokenEndIgnore(tok, pos)
          inc(pos) # skip '"'
          break
      elif c in {CR, LF, nimlexbase.EndOfFile}:
        tokenEndIgnore(tok, pos)
        L.localReport LexerReport(kind: rlexUnclosedSingleString)
        break
      elif (c == '\\') and mode == normal:
        L.bufpos = pos
        getEscapedChar(L, tok)
        pos = L.bufpos
      else:
        tok.literal.add(c)
        inc(pos)
    L.bufpos = pos

proc getCharacter(L: var Lexer; tok: var Token) =
  tokenBegin(tok, L.bufpos)
  let startPos = L.bufpos
  inc(L.bufpos)               # skip '
  var c = L.buf[L.bufpos]
  case c
  of '\0'..pred(' '), '\'':
    L.localReport LexerReport(kind: rlexInvalidCharLiteral)
    tok.literal = $c
  of '\\': getEscapedChar(L, tok)
  else:
    tok.literal = $c
    inc(L.bufpos)
  if L.buf[L.bufpos] == '\'':
    tokenEndIgnore(tok, L.bufpos)
    inc(L.bufpos)               # skip '
  else:
    if startPos > 0 and L.buf[startPos-1] == '`':
      tok.literal = "'"
      L.bufpos = startPos+1
    else:
      L.localReport LexerReport(kind: rlexMissingClosingApostrophe)
    tokenEndIgnore(tok, L.bufpos)

const
  UnicodeOperatorStartChars = {'\226', '\194', '\195'}
    # the allowed unicode characters ("∙ ∘ × ★ ⊗ ⊘ ⊙ ⊛ ⊠ ⊡ ∩ ∧ ⊓ ± ⊕ ⊖ ⊞ ⊟ ∪ ∨ ⊔")
    # all start with one of these.

type
  UnicodeOprPred = enum
    Mul, Add

proc unicodeOprLen(buf: cstring; pos: int): (int8, UnicodeOprPred) =
  template m(len): untyped = (int8(len), Mul)
  template a(len): untyped = (int8(len), Add)
  result = 0.m
  case buf[pos]
  of '\226':
    if buf[pos+1] == '\136':
      if buf[pos+2] == '\152': result = 3.m # ∘
      elif buf[pos+2] == '\153': result = 3.m # ∙
      elif buf[pos+2] == '\167': result = 3.m # ∧
      elif buf[pos+2] == '\168': result = 3.a # ∨
      elif buf[pos+2] == '\169': result = 3.m # ∩
      elif buf[pos+2] == '\170': result = 3.a # ∪
    elif buf[pos+1] == '\138':
      if buf[pos+2] == '\147': result = 3.m # ⊓
      elif buf[pos+2] == '\148': result = 3.a # ⊔
      elif buf[pos+2] == '\149': result = 3.a # ⊕
      elif buf[pos+2] == '\150': result = 3.a # ⊖
      elif buf[pos+2] == '\151': result = 3.m # ⊗
      elif buf[pos+2] == '\152': result = 3.m # ⊘
      elif buf[pos+2] == '\153': result = 3.m # ⊙
      elif buf[pos+2] == '\155': result = 3.m # ⊛
      elif buf[pos+2] == '\158': result = 3.a # ⊞
      elif buf[pos+2] == '\159': result = 3.a # ⊟
      elif buf[pos+2] == '\160': result = 3.m # ⊠
      elif buf[pos+2] == '\161': result = 3.m # ⊡
    elif buf[pos+1] == '\152' and buf[pos+2] == '\133': result = 3.m # ★
  of '\194':
    if buf[pos+1] == '\177': result = 2.a # ±
  of '\195':
    if buf[pos+1] == '\151': result = 2.m # ×
  else:
    discard

proc getSymbol(L: var Lexer, tok: var Token) =
  var h: Hash = 0
  var pos = L.bufpos
  tokenBegin(tok, pos)
  var suspicious = false
  while true:
    var c = L.buf[pos]
    case c
    of 'a'..'z', '0'..'9':
      h = h !& ord(c)
      inc(pos)
    of 'A'..'Z':
      c = chr(ord(c) + (ord('a') - ord('A'))) # toLower()
      h = h !& ord(c)
      inc(pos)
      suspicious = true
    of '_':
      if L.buf[pos+1] notin SymChars:
        L.localReport LexerReport(kind: rlexMalformedTrailingUnderscre)
        break
      inc(pos)
      suspicious = true
    of '\x80'..'\xFF':
      if c in UnicodeOperatorStartChars and
         unicodeOperators in L.config.features and
         unicodeOprLen(L.buf, pos)[0] != 0:
        break
      else:
        h = h !& ord(c)
        inc(pos)
    else: break
  tokenEnd(tok, pos-1)
  h = !$h
  tok.ident = L.cache.getIdent(addr(L.buf[L.bufpos]), pos - L.bufpos, h)
  if (tok.ident.id < ord(tokKeywordLow) - ord(tkSymbol)) or
      (tok.ident.id > ord(tokKeywordHigh) - ord(tkSymbol)):
    tok.tokType = tkSymbol
  else:
    tok.tokType = TokType(tok.ident.id + ord(tkSymbol))
    if suspicious and {optStyleHint, optStyleError} * L.config.globalOptions != {}:
      L.localReport LexerReport(
        kind: rlexLinterReport,
        wanted: tok.ident.s.normalize,
        got: tok.ident.s)
  L.bufpos = pos


proc endOperator(L: var Lexer, tok: var Token, pos: int,
                 hash: Hash) {.inline.} =
  var h = !$hash
  tok.ident = L.cache.getIdent(addr(L.buf[L.bufpos]), pos - L.bufpos, h)
  if (tok.ident.id < oprLow) or (tok.ident.id > oprHigh): tok.tokType = tkOpr
  else: tok.tokType = TokType(tok.ident.id - oprLow + ord(tkColon))
  L.bufpos = pos

proc getOperator(L: var Lexer, tok: var Token) =
  var pos = L.bufpos
  tokenBegin(tok, pos)
  var h: Hash = 0
  while true:
    var c = L.buf[pos]
    if c in OpChars:
      h = h !& ord(c)
      inc(pos)
    elif c in UnicodeOperatorStartChars and unicodeOperators in L.config.features:
      let oprLen = unicodeOprLen(L.buf, pos)[0]
      if oprLen == 0: break
      for i in 0..<oprLen:
        h = h !& ord(L.buf[pos])
        inc pos
    else:
      break
  endOperator(L, tok, pos, h)
  tokenEnd(tok, pos-1)
  # advance pos but don't store it in L.bufpos so the next token (which might
  # be an operator too) gets the preceding spaces:
  tok.strongSpaceB = 0
  while L.buf[pos] == ' ':
    inc pos
    inc tok.strongSpaceB
  if L.buf[pos] in {CR, LF, nimlexbase.EndOfFile}:
    tok.strongSpaceB = -1

proc getPrecedence*(tok: Token): int =
  ## Calculates the precedence of the given token.
  const
    MulPred = 9
    PlusPred = 8
  case tok.tokType
  of tkOpr:
    let relevantChar = tok.ident.s[0]

    # arrow like?
    if tok.ident.s.len > 1 and tok.ident.s[^1] == '>' and
      tok.ident.s[^2] in {'-', '~', '='}: return 1

    template considerAsgn(value: untyped) =
      result = if tok.ident.s[^1] == '=': 1 else: value

    case relevantChar
    of '$', '^': considerAsgn(10)
    of '*', '%', '/', '\\': considerAsgn(MulPred)
    of '~': result = 8
    of '+', '-', '|': considerAsgn(PlusPred)
    of '&': considerAsgn(7)
    of '=', '<', '>', '!': result = 5
    of '.': considerAsgn(6)
    of '?': result = 2
    of UnicodeOperatorStartChars:
      if tok.ident.s[^1] == '=':
        result = 1
      else:
        let (len, pred) = unicodeOprLen(cstring(tok.ident.s), 0)
        if len != 0:
          result = if pred == Mul: MulPred else: PlusPred
        else:
          result = 2
    else: considerAsgn(2)
  of tkDiv, tkMod, tkShl, tkShr: result = 9
  of tkDotDot: result = 6
  of tkIn, tkNotin, tkIs, tkIsnot, tkOf, tkAs, tkFrom: result = 5
  of tkAnd: result = 4
  of tkOr, tkXor, tkPtr, tkRef: result = 3
  else: return -10

proc newlineFollows*(L: Lexer): bool =
  var pos = L.bufpos
  while true:
    case L.buf[pos]
    of ' ', '\t':
      inc(pos)
    of CR, LF:
      result = true
      break
    of '#':
      inc(pos)
      if L.buf[pos] == '#': inc(pos)
      if L.buf[pos] != '[': return true
    else:
      break

proc skipMultiLineComment(L: var Lexer; tok: var Token; start: int;
                          isDoc: bool) =
  var pos = start
  var toStrip = 0
  tokenBegin(tok, pos)
  # detect the amount of indentation:
  if isDoc:
    toStrip = getColNumber(L, pos)
    while L.buf[pos] == ' ':
      inc pos
      inc toStrip
    while L.buf[pos] in {CR, LF}:  # skip blank lines
      pos = handleCRLF(L, pos)
      toStrip = 0
      while L.buf[pos] == ' ':
        inc pos
        inc toStrip
  var nesting = 0
  while true:
    case L.buf[pos]
    of '#':
      if isDoc:
        if L.buf[pos+1] == '#' and L.buf[pos+2] == '[':
          inc nesting
        tok.literal.add '#'
      elif L.buf[pos+1] == '[':
        inc nesting
      inc pos
    of ']':
      if isDoc:
        if L.buf[pos+1] == '#' and L.buf[pos+2] == '#':
          if nesting == 0:
            tokenEndIgnore(tok, pos+2)
            inc(pos, 3)
            break
          dec nesting
        tok.literal.add ']'
      elif L.buf[pos+1] == '#':
        if nesting == 0:
          tokenEndIgnore(tok, pos+1)
          inc(pos, 2)
          break
        dec nesting
      inc pos
    of CR, LF:
      tokenEndIgnore(tok, pos)
      pos = handleCRLF(L, pos)
      # strip leading whitespace:
      when defined(nimpretty): tok.literal.add "\L"
      if isDoc:
        when not defined(nimpretty): tok.literal.add "\n"
        inc tok.iNumber
        var c = toStrip
        while L.buf[pos] == ' ' and c > 0:
          inc pos
          dec c
    of nimlexbase.EndOfFile:
      tokenEndIgnore(tok, pos)
      L.localReportPos(
        LexerReport(kind: rlexUnclosedComment), pos)

      break
    else:
      if isDoc or defined(nimpretty): tok.literal.add L.buf[pos]
      inc(pos)
  L.bufpos = pos
  when defined(nimpretty):
    tok.commentOffsetB = L.offsetBase + pos - 1

proc scanComment(L: var Lexer, tok: var Token) =
  var pos = L.bufpos
  tok.tokType = tkComment
  # iNumber contains the number of '\n' in the token
  tok.iNumber = 0
  assert L.buf[pos+1] == '#'
  when defined(nimpretty):
    tok.commentOffsetA = L.offsetBase + pos

  if L.buf[pos+2] == '[':
    skipMultiLineComment(L, tok, pos+3, true)
    return
  tokenBegin(tok, pos)
  inc(pos, 2)

  var toStrip = 0
  var stripInit = false

  while true:
    if not stripInit:  # find baseline indentation inside comment
      while L.buf[pos] == ' ':
        inc pos
        inc toStrip
      if L.buf[pos] in {CR, LF}:  # don't set toStrip in blank comment lines
        toStrip = 0
      else:  # found first non-whitespace character
        stripInit = true
    var lastBackslash = -1
    while L.buf[pos] notin {CR, LF, nimlexbase.EndOfFile}:
      if L.buf[pos] == '\\': lastBackslash = pos+1
      tok.literal.add(L.buf[pos])
      inc(pos)
    tokenEndIgnore(tok, pos)
    pos = handleCRLF(L, pos)
    var indent = 0
    while L.buf[pos] == ' ':
      inc(pos)
      inc(indent)

    if L.buf[pos] == '#' and L.buf[pos+1] == '#':
      tok.literal.add "\n"
      inc(pos, 2)
      if stripInit:
        var c = toStrip
        while L.buf[pos] == ' ' and c > 0:
          inc pos
          dec c
        inc tok.iNumber
    else:
      if L.buf[pos] > ' ':
        L.indentAhead = indent
      tokenEndIgnore(tok, pos)
      break
  L.bufpos = pos
  when defined(nimpretty):
    tok.commentOffsetB = L.offsetBase + pos - 1

proc skip(L: var Lexer, tok: var Token) =
  var pos = L.bufpos
  tokenBegin(tok, pos)
  tok.strongSpaceA = 0
  when defined(nimpretty):
    var hasComment = false
    var commentIndent = L.currLineIndent
    tok.commentOffsetA = L.offsetBase + pos
    tok.commentOffsetB = tok.commentOffsetA
    tok.line = -1
  while true:
    case L.buf[pos]
    of ' ':
      inc(pos)
      inc(tok.strongSpaceA)
    of '\t':
      if not L.allowTabs:
        L.localReportPos(
          LexerReport(kind: rlexNoTabs), pos)

      inc(pos)
    of CR, LF:
      tokenEndPrevious(tok, pos)
      pos = handleCRLF(L, pos)
      var indent = 0
      while true:
        if L.buf[pos] == ' ':
          inc(pos)
          inc(indent)
        elif L.buf[pos] == '#' and L.buf[pos+1] == '[':
          when defined(nimpretty):
            hasComment = true
            if tok.line < 0:
              tok.line = L.lineNumber
              commentIndent = indent
          skipMultiLineComment(L, tok, pos+2, false)
          pos = L.bufpos
        else:
          break
      tok.strongSpaceA = 0
      when defined(nimpretty):
        if L.buf[pos] == '#' and tok.line < 0:
          commentIndent = indent

      if L.buf[pos] > ' ' and (L.buf[pos] != '#' or L.buf[pos+1] == '#'):
        tok.indent = indent
        L.currLineIndent = indent
        break
    of '#':
      # do not skip documentation comment:
      if L.buf[pos+1] == '#': break
      when defined(nimpretty):
        hasComment = true
        if tok.line < 0:
          tok.line = L.lineNumber

      if L.buf[pos+1] == '[':
        skipMultiLineComment(L, tok, pos+2, false)
        pos = L.bufpos
      else:
        tokenBegin(tok, pos)
        while L.buf[pos] notin {CR, LF, nimlexbase.EndOfFile}:
          when defined(nimpretty): tok.literal.add L.buf[pos]
          inc(pos)
        tokenEndIgnore(tok, pos+1)
        when defined(nimpretty):
          tok.commentOffsetB = L.offsetBase + pos + 1
    else:
      break                   # EndOfFile also leaves the loop
  tokenEndPrevious(tok, pos-1)
  L.bufpos = pos
  when defined(nimpretty):
    if hasComment:
      tok.commentOffsetB = L.offsetBase + pos - 1
      tok.tokType = tkComment
      tok.indent = commentIndent

proc rawGetTok*(L: var Lexer, tok: var Token) =
  template atTokenEnd() {.dirty.} =
    when defined(nimsuggest):
      # we attach the cursor to the last *strong* token
      if tok.tokType notin weakTokens:
        L.previousToken.line = tok.line.uint16
        L.previousToken.col = tok.col.int16

  fillToken(tok)
  if L.indentAhead >= 0:
    tok.indent = L.indentAhead
    L.currLineIndent = L.indentAhead
    L.indentAhead = -1
  else:
    tok.indent = -1
  skip(L, tok)
  when defined(nimpretty):
    if tok.tokType == tkComment:
      L.indentAhead = L.currLineIndent
      return
  var c = L.buf[L.bufpos]
  tok.line = L.lineNumber
  tok.col = getColNumber(L, L.bufpos)
  if c in SymStartChars - {'r', 'R'} - UnicodeOperatorStartChars:
    getSymbol(L, tok)
  else:
    case c
    of UnicodeOperatorStartChars:
      if unicodeOperators in L.config.features and unicodeOprLen(L.buf, L.bufpos)[0] != 0:
        getOperator(L, tok)
      else:
        getSymbol(L, tok)
    of '#':
      scanComment(L, tok)
    of '*':
      # '*:' is unfortunately a special case, because it is two tokens in
      # 'var v*: int'.
      if L.buf[L.bufpos+1] == ':' and L.buf[L.bufpos+2] notin OpChars:
        var h = 0 !& ord('*')
        endOperator(L, tok, L.bufpos+1, h)
      else:
        getOperator(L, tok)
    of ',':
      tok.tokType = tkComma
      inc(L.bufpos)
    of 'r', 'R':
      if L.buf[L.bufpos + 1] == '\"':
        inc(L.bufpos)
        getString(L, tok, raw)
      else:
        getSymbol(L, tok)
    of '(':
      inc(L.bufpos)
      if L.buf[L.bufpos] == '.' and L.buf[L.bufpos+1] != '.':
        tok.tokType = tkParDotLe
        inc(L.bufpos)
      else:
        tok.tokType = tkParLe
        when defined(nimsuggest):
          if L.fileIdx == L.config.m.trackPos.fileIndex and tok.col < L.config.m.trackPos.col and
                    tok.line == L.config.m.trackPos.line.int and L.config.ideCmd == ideCon:
            L.config.m.trackPos.col = tok.col.int16
    of ')':
      tok.tokType = tkParRi
      inc(L.bufpos)
    of '[':
      inc(L.bufpos)
      if L.buf[L.bufpos] == '.' and L.buf[L.bufpos+1] != '.':
        tok.tokType = tkBracketDotLe
        inc(L.bufpos)
      elif L.buf[L.bufpos] == ':':
        tok.tokType = tkBracketLeColon
        inc(L.bufpos)
      else:
        tok.tokType = tkBracketLe
    of ']':
      tok.tokType = tkBracketRi
      inc(L.bufpos)
    of '.':
      when defined(nimsuggest):
        if L.fileIdx == L.config.m.trackPos.fileIndex and tok.col+1 == L.config.m.trackPos.col and
            tok.line == L.config.m.trackPos.line.int and L.config.ideCmd == ideSug:
          tok.tokType = tkDot
          L.config.m.trackPos.col = tok.col.int16
          inc(L.bufpos)
          atTokenEnd()
          return
      if L.buf[L.bufpos+1] == ']':
        tok.tokType = tkBracketDotRi
        inc(L.bufpos, 2)
      elif L.buf[L.bufpos+1] == '}':
        tok.tokType = tkCurlyDotRi
        inc(L.bufpos, 2)
      elif L.buf[L.bufpos+1] == ')':
        tok.tokType = tkParDotRi
        inc(L.bufpos, 2)
      else:
        getOperator(L, tok)
    of '{':
      inc(L.bufpos)
      if L.buf[L.bufpos] == '.' and L.buf[L.bufpos+1] != '.':
        tok.tokType = tkCurlyDotLe
        inc(L.bufpos)
      else:
        tok.tokType = tkCurlyLe
    of '}':
      tok.tokType = tkCurlyRi
      inc(L.bufpos)
    of ';':
      tok.tokType = tkSemiColon
      inc(L.bufpos)
    of '`':
      tok.tokType = tkAccent
      inc(L.bufpos)
    of '_':
      inc(L.bufpos)
      if L.buf[L.bufpos] notin SymChars+{'_'}:
        tok.tokType = tkSymbol
        tok.ident = L.cache.getIdent("_")
      else:
        tok.literal = $c
        tok.tokType = tkInvalid
        L.localReport LexerReport(
          kind: rlexInvalidToken,
          msg: "invalid token: " & c & " (\\" & $(ord(c)) & ')')
    of '\"':
      # check for generalized raw string literal:
      let mode = if L.bufpos > 0 and L.buf[L.bufpos-1] in SymChars: generalized else: normal
      getString(L, tok, mode)
      if mode == generalized:
        # tkRStrLit -> tkGStrLit
        # tkTripleStrLit -> tkGTripleStrLit
        inc(tok.tokType, 2)
    of '\'':
      tok.tokType = tkCharLit
      getCharacter(L, tok)
      tok.tokType = tkCharLit
    of '0'..'9':
      getNumber(L, tok)
      let c = L.buf[L.bufpos]
      if c in SymChars+{'_'}:
        if c in UnicodeOperatorStartChars and unicodeOperators in L.config.features and
            unicodeOprLen(L.buf, L.bufpos)[0] != 0:
          discard
        else:
          L.localReport LexerReport(
            kind: rlexInvalidToken,
            msg: "invalid token: no whitespace between number and identifier")
    of '-':
      if L.buf[L.bufpos+1] in {'0'..'9'} and
          (L.bufpos-1 == 0 or L.buf[L.bufpos-1] in UnaryMinusWhitelist):
        # x)-23 # binary minus
        # ,-23  # unary minus
        # \n-78 # unary minus? Yes.
        # =-3   # parsed as `=-` anyway
        getNumber(L, tok)
        let c = L.buf[L.bufpos]
        if c in SymChars+{'_'}:
          if c in UnicodeOperatorStartChars and unicodeOperators in L.config.features and
              unicodeOprLen(L.buf, L.bufpos)[0] != 0:
            discard
          else:
            L.localReport LexerReport(
              kind: rlexInvalidToken,
              msg: "invalid token: no whitespace between number and identifier")
      else:
        getOperator(L, tok)
    else:
      if c in OpChars:
        getOperator(L, tok)
      elif c == nimlexbase.EndOfFile:
        tok.tokType = tkEof
        tok.indent = 0
      else:
        tok.literal = $c
        tok.tokType = tkInvalid
        L.localReport LexerReport(
          kind: rlexInvalidToken,
          msg: "invalid token: " & c & " (\\" & $(ord(c)) & ')')
        inc(L.bufpos)
  atTokenEnd()

proc getIndentWidth*(fileIdx: FileIndex, inputstream: PLLStream;
                     cache: IdentCache; config: ConfigRef): int =
  var lex: Lexer
  var tok: Token
  initToken(tok)
  openLexer(lex, fileIdx, inputstream, cache, config)
  var prevToken = tkEof
  while tok.tokType != tkEof:
    rawGetTok(lex, tok)
    if tok.indent > 0 and prevToken in {tkColon, tkEquals, tkType, tkConst, tkLet, tkVar, tkUsing}:
      result = tok.indent
      if result > 0: break
    prevToken = tok.tokType
  closeLexer(lex)

proc getPrecedence*(ident: PIdent): int =
  ## assumes ident is binary operator already
  var tok: Token
  initToken(tok)
  tok.ident = ident
  tok.tokType =
    if tok.ident.id in ord(tokKeywordLow) - ord(tkSymbol)..ord(tokKeywordHigh) - ord(tkSymbol):
      TokType(tok.ident.id + ord(tkSymbol))
    else: tkOpr
  getPrecedence(tok)
