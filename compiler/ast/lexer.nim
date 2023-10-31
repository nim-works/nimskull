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
    idioms
  ],
  compiler/ast/[
    numericbase,
    wordrecg,
    nimlexbase,
    llstream,
    lineinfos,
    idents
  ],
  std/[
    parseutils,
    hashes,
    strutils
  ],
  compiler/front/[
    options
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
    tkInvalid = "tkInvalid", tkError = "tkError", tkEof = "[EOF]", # order is important here!
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
    tkFloat64Lit = "tkFloat64Lit",
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

  LexerDiagKind* = enum
    # users errors begin

    # spacing
    lexDiagMalformedNumUnderscores
    lexDiagMalformedIdentUnderscores
    lexDiagMalformedTrailingUnderscre
    lexDiagInvalidToken
    lexDiagInvalidTokenSpaceBetweenNumAndIdent
    lexDiagNoTabs

    # numbers
    lexDiagInvalidIntegerLiteralOctalPrefix
    lexDiagInvalidIntegerSuffix
    lexDiagNumberNotInRange
    lexDiagExpectedHex
    lexDiagInvalidIntegerLiteral
    lexDiagInvalidNumericLiteral

    # char
    lexDiagInvalidCharLiteral
    lexDiagInvalidCharLiteralConstant
    lexDiagInvalidCharLiteralPlatformNewline
    lexDiagInvalidCharLiteralUnicodeCodepoint
    lexDiagMissingClosingApostrophe
    lexDiagInvalidUnicodeCodepointEmpty
    lexDiagInvalidUnicodeCodepointGreaterThan0x10FFFF
    # xxx: differentiate between invalid codepoint empty vs out of range, but
    #      still be treated within the invalidUnicodeCodepoint "family"

    # string
    lexDiagUnclosedTripleString
    lexDiagUnclosedSingleString

    # comments
    lexDiagUnclosedComment

    # user errors end

    # warnings begin
    lexDiagDeprecatedOctalPrefix = "OctalEscape"
    # warnings end

    # linting hints begin
    lexDiagLineTooLong = "LineTooLong"
    lexDiagNameXShouldBeY = "Name"
    # linting hints end

  LexerDiag* = object
    ## `Diag`nostic data from the Lexer, mostly errors
    msg*: string
    location*: TLineInfo        ## diagnostic location
    instLoc*: InstantiationInfo ## instantiation in lexer's source
    case kind*: LexerDiagKind:
      of lexDiagNameXShouldBeY:
        # use `msg` for `wanted`
        got*: string
      else:
        discard

const
  LexDiagsError*   = {lexDiagMalformedNumUnderscores..lexDiagUnclosedComment}
  LexDiagsWarning* = {lexDiagDeprecatedOctalPrefix}
  LexDiagsHint*    = {lexDiagLineTooLong..lexDiagNameXShouldBeY}

when defined(nimsuggest):
  const weakTokens = {tkComma, tkSemiColon, tkColon,
                      tkParRi, tkParDotRi, tkBracketRi, tkBracketDotRi,
                      tkCurlyRi}
    ## tokens that should not be considered for previousToken

const
  tokKeywordLow* = succ(tkSymbol)
  tokKeywordHigh* = pred(tkIntLit)
  tkKeywords* = {tokKeywordLow..tokKeywordHigh}

func diagToHumanStr*(d: LexerDiag): string =
  ## creates a human readable string message for a diagnostic, does not include
  ## any extra information such as line info, severity, and so on.
  case d.kind
  of lexDiagMalformedTrailingUnderscre:
    "invalid token: trailing underscore"
  of lexDiagMalformedNumUnderscores:
    "invalid number: only non-consecutive and non-trailing underscores " &
      "allowed: e.g. '1__1' and '1_' are invalid"
  of lexDiagMalformedIdentUnderscores:
    "invalid identifier: only non-consecutive and non-trailing underscores " &
      "allowed: eg. 'a__b' and 'a_' are invalid"
  of lexDiagInvalidToken:
    "invalid token: $1 (\\$2)" % [d.msg, $ord(d.msg[0])]
  of lexDiagInvalidTokenSpaceBetweenNumAndIdent:
    "invalid token: no blankspace between number and identifier"
  of lexDiagNoTabs:
    "tabs are not allowed, use spaces instead"
  of lexDiagInvalidIntegerLiteralOctalPrefix:
    "$1 is an invalid int literal; For octal literals use the '0o' prefix" %
      d.msg
  of lexDiagInvalidIntegerSuffix:
    "invalid number suffix: '$1'" % d.msg
  of lexDiagNumberNotInRange:
    "number out of range: '$1'" % d.msg
  of lexDiagExpectedHex:
    "expected a hex digit, but found: '$1'; maybe prefix with 0" % d.msg
  of lexDiagInvalidIntegerLiteral, lexDiagInvalidNumericLiteral:
    "invalid number: '$1'" % d.msg
  of lexDiagInvalidCharLiteral:
    "invalid character literal"
  of lexDiagInvalidCharLiteralConstant:
    "invalid character constant"
  of lexDiagInvalidCharLiteralPlatformNewline:
    "\\p not allowed in character literal"
  of lexDiagInvalidCharLiteralUnicodeCodepoint:
    "\\u not allowed in character literal"
  of lexDiagMissingClosingApostrophe:
    "missing closing ' for character literal"
  of lexDiagInvalidUnicodeCodepointEmpty:
    "Unicode codepoint cannot be empty"
  of lexDiagInvalidUnicodeCodepointGreaterThan0x10FFFF:
    "Unicode codepoint must be 0x10FFFF or lower, but was: $1" & d.msg
  of lexDiagUnclosedTripleString:
    "closing \"\"\" expected, but end of file reached"
  of lexDiagUnclosedSingleString:
    "closing \" expected"
  of lexDiagUnclosedComment:
    "end of multiline comment expected"
  of lexDiagDeprecatedOctalPrefix:
    "octal escape sequences do not exist; leading zero is ignored"
  of lexDiagLineTooLong:
    "line too long"
  of lexDiagNameXShouldBeY:
    "'$1' should be: '$2'" % [d.got, d.msg]

type
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
    error*: LexerDiag         ## error diagnostic if `tokType` is `tkError`

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
    diags: seq[LexerDiag]

func diagOffset*(L: Lexer): int {.inline.} =
  ## return value represents a point in time where all existing diagnostics are
  ## considered in the past, used in conjunction with `errorsHintsAndWarnings`
  L.diags.len

iterator errorsHintsAndWarnings*(L: Lexer, diagOffset = 0): LexerDiag =
  ## iterate over all diagnostics from the beginning, or from the point in time
  ## specificed via `diagOffset`
  for d in L.diags.toOpenArray(diagOffset, L.diags.high):
    yield d

proc getLineInfo*(L: Lexer, tok: Token): TLineInfo {.inline.} =
  result = newLineInfo(L.fileIdx, tok.line, tok.col)

func isKeyword*(kind: TokType): bool =
  kind in tkKeywords

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
  of tkError: tok.error.msg
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

proc fillToken(L: var Token) =
  L.tokType = tkInvalid
  L.iNumber = 0
  L.indent = 0
  L.strongSpaceA = 0
  setLen(L.literal, 0)
  L.fNumber = 0.0
  L.base = base10
  L.ident = nil

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
  lex.diags.setLen(0)
  closeBaseLexer(lex)

template getColNumber(L: Lexer, pos: int): int =
  # supresses the hint on the CLI
  getColNumber(TBaseLexer L, pos)

proc getLineInfo*(L: Lexer): TLineInfo =
  result = newLineInfo(L.fileIdx, L.lineNumber, getColNumber(L, L.bufpos))

proc matchTwoChars(L: Lexer, first: char, second: set[char]): bool =
  result = (L.buf[L.bufpos] == first) and (L.buf[L.bufpos + 1] in second)

template tokenBegin(tok, pos) {.dirty.} =
  when defined(nimsuggest):
    var colA = getColNumber(L, pos)

template tokenEnd(tok, pos) {.dirty.} =
  when defined(nimsuggest):
    let colB = getColNumber(L, pos)+1
    if L.fileIdx == L.config.m.trackPos.fileIndex and L.config.m.trackPos.col in colA..colB and
        L.lineNumber == L.config.m.trackPos.line.int and L.config.ideCmd in {ideSug, ideCon}:
      L.config.m.trackPos.col = colA.int16
    colA = 0

template tokenEndIgnore(tok, pos) =
  when defined(nimsuggest):
    let colB = getColNumber(L, pos)
    if L.fileIdx == L.config.m.trackPos.fileIndex and L.config.m.trackPos.col in colA..colB and
        L.lineNumber == L.config.m.trackPos.line.int and L.config.ideCmd in {ideSug, ideCon}:
      L.config.m.trackPos.fileIndex = trackPosInvalidFileIdx
      L.config.m.trackPos.line = 0'u16
    colA = 0

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

template eatChar(L: var Lexer, t: var Token, replacementChar: char) =
  t.literal.add(replacementChar)
  inc(L.bufpos)

template eatChar(L: var Lexer, t: var Token) =
  t.literal.add(L.buf[L.bufpos])
  inc(L.bufpos)


func handleDiag(L: var Lexer, diag: LexerDiagKind, instLoc = instLoc(-1)) =
  assert diag notin {lexDiagNameXShouldBeY}
  L.diags.add LexerDiag(
                location: L.getLineInfo,
                instLoc: instLoc,
                kind: diag
              )

func handleDiag(L: var Lexer,
                diag: LexerDiagKind, 
                message: string, 
                instLoc = instLoc(-1)) =
  assert diag notin {lexDiagNameXShouldBeY}
  L.diags.add LexerDiag(
                msg: message,
                location: L.getLineInfo,
                instLoc: instLoc,
                kind: diag
              )

func handleDiagPos(L: var Lexer,
                   diag: LexerDiagKind,
                   pos: int,
                   instLoc = instLoc(-1)) =
  assert diag notin {lexDiagNameXShouldBeY}
  L.diags.add LexerDiag(
                location: newLineInfo(L.fileIdx,
                                      L.lineNumber,
                                      pos - L.lineStart),
                instLoc: instLoc,
                kind: diag
              )

func diagLineTooLong(L: var Lexer, pos: int, instLoc = instLoc(-1)) =
  L.diags.add LexerDiag(
                location: newLineInfo(L.fileIdx,
                                      L.lineNumber,
                                      pos - L.lineStart),
                instLoc: instLoc,
                kind: lexDiagLineTooLong
              )

func diagLintName(L: var Lexer,
                  wantedName, gotName: string,
                  instLoc = instLoc(-1)) =
  L.diags.add LexerDiag(
                location: L.getLineInfo,
                instLoc: instLoc,
                kind: lexDiagNameXShouldBeY,
                msg: wantedName,
                got: gotName
              )

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
          L.handleDiag(lexDiagMalformedNumUnderscores)
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

  proc lexMessageLitNum(L: var Lexer, startpos: int, msgKind: LexerDiagKind) =
    # Used to get slightly human friendlier err messages.
    const literalishChars = {'A'..'Z', 'a'..'z', '0'..'9', '_', '.', '\''}
    # preserve the old pos so we can restore it later
    let msgPos = L.bufpos 
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
    # restore the old pos
    L.bufpos = msgPos
    L.handleDiag(msgKind, t.literal)

  var xi: BiggestInt

  const
    baseCodeChars = {'X', 'x', 'o', 'b', 'B'}
    literalishChars = baseCodeChars + {'A'..'F', 'a'..'f', '0'..'9', '_', '\''}
    floatTypes = {tkFloatLit, tkFloat32Lit, tkFloat64Lit}
  result.tokType = tkIntLit   # int literal until we know better
  result.literal = ""
  result.base = base10
  tokenBegin(result, L.bufpos)

  var isPositive = true
  if L.buf[L.bufpos] == '-':
    eatChar(L, result)
    isPositive = false

  let startpos = L.bufpos

  # First stage: find out base, make verifications, build token literal string
  if L.buf[L.bufpos] == '0' and L.buf[L.bufpos + 1] in baseCodeChars + {'O'}:
    eatChar(L, result, '0')
    var numDigits = 0
    case L.buf[L.bufpos]
    of 'O':
      lexMessageLitNum(L, startpos, lexDiagInvalidIntegerLiteralOctalPrefix)
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
      unreachable("if guard precludes this from happening")

    if numDigits == 0:
      lexMessageLitNum(L, startpos, lexDiagInvalidIntegerLiteral)
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
    let
      errPos = postPos
      customLitPossible =
        if L.buf[postPos] == '\'':
          inc(postPos) # move past the apostrophe
          true
        else:
          false

    if L.buf[postPos] in SymChars:
      var suffix = newStringOfCap(10)
      while L.buf[postPos] in SymChars+{'_'}:
        suffix.add L.buf[postPos]
        inc postPos
      let suffixAsLower = suffix.toLowerAscii
      case suffixAsLower
      of "f", "f32": result.tokType = tkFloat32Lit
      of "d", "f64": result.tokType = tkFloat64Lit
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
        result.ident = L.cache.getIdent("'" & suffix)
        result.iNumber = len(result.literal)
        result.literal.add '\''
        result.literal.add suffix
        result.tokType = tkCustomLit
      else:
        lexMessageLitNum(L, errPos, lexDiagInvalidIntegerSuffix)
    else:
      lexMessageLitNum(L, errPos, lexDiagInvalidIntegerSuffix)

  # Is there still a literalish char awaiting? Then it's an error!
  if  L.buf[postPos] in literalishChars or
     (L.buf[postPos] == '.' and L.buf[postPos + 1] in {'0'..'9'}):
    lexMessageLitNum(L, startpos, lexDiagInvalidNumericLiteral)

  template setNumber(field, value) =
    field = (if isPositive: value else: -value)

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
          unreachable("if guard precludes this from happening, again")

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
          unreachable("tokType is at least tkIntLit and if guards tkCustomLit")

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
            lexMessageLitNum(L, startpos, lexDiagNumberNotInRange)

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
            raise newException(OverflowDefect, result.literal)
          if len != result.literal.len:
            raise newException(ValueError, result.literal)
          result.iNumber = cast[int64](iNumber)
        else:
          var iNumber: int64
          var len: int
          try:
            len = parseBiggestInt(result.literal, iNumber)
          except ValueError:
            raise newException(OverflowDefect, result.literal)
          if len != result.literal.len:
            raise newException(ValueError, result.literal)
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
          lexMessageLitNum(L, startpos, lexDiagNumberNotInRange)

      # Promote int literal to int64? Not always necessary, but more consistent
      if result.tokType == tkIntLit:
        if result.iNumber > high(int32) or result.iNumber < low(int32):
          result.tokType = tkInt64Lit

    except ValueError:
      lexMessageLitNum(L, startpos, lexDiagInvalidNumericLiteral)
    except OverflowDefect, RangeDefect:
      lexMessageLitNum(L, startpos, lexDiagNumberNotInRange)
  tokenEnd(result, postPos-1)
  L.bufpos = postPos

proc handleHexChar(L: var Lexer, xi: var int; position: range[0..4]) =
  template invalid() =
    L.handleDiag(lexDiagExpectedHex, $L.buf[L.bufpos])

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
    s[pos+2] = chr(i shr 6  and ones(6) or 0b10_0000_00)
    s[pos+3] = chr(i and ones(6) or 0b10_0000_00)
  elif i <= 0x03FFFFFF:
    s.setLen(pos+5)
    s[pos+0] = chr(i shr 24 or 0b111110_00)
    s[pos+1] = chr(i shr 18 and ones(6) or 0b10_0000_00)
    s[pos+2] = chr(i shr 12 and ones(6) or 0b10_0000_00)
    s[pos+3] = chr(i shr 6  and ones(6) or 0b10_0000_00)
    s[pos+4] = chr(i and ones(6) or 0b10_0000_00)
  elif i <= 0x7FFFFFFF:
    s.setLen(pos+6)
    s[pos+0] = chr(i shr 30 or 0b1111110_0)
    s[pos+1] = chr(i shr 24 and ones(6) or 0b10_0000_00)
    s[pos+2] = chr(i shr 18 and ones(6) or 0b10_0000_00)
    s[pos+3] = chr(i shr 12 and ones(6) or 0b10_0000_00)
    s[pos+4] = chr(i shr 6  and ones(6) or 0b10_0000_00)
    s[pos+5] = chr(i and ones(6) or 0b10_0000_00)

proc getEscapedChar(L: var Lexer, tok: var Token) =
  template addAndProgress(c: char | string) =
    tok.literal.add(c)
    inc(L.bufpos)

  inc(L.bufpos)               # skip '\'
  case L.buf[L.bufpos]
  of 'n', 'N':
    addAndProgress('\L')
  of 'p', 'P':
    if tok.tokType == tkCharLit:
      L.handleDiag(lexDiagInvalidCharLiteralPlatformNewline)
    addAndProgress(L.config.target.tnl)
  of 'r', 'R', 'c', 'C':
    addAndProgress(CR)
  of 'l', 'L':
    addAndProgress(LF)
  of 'f', 'F':
    addAndProgress(FF)
  of 'e', 'E':
    addAndProgress(ESC)
  of 'a', 'A':
    addAndProgress(BEL)
  of 'b', 'B':
    addAndProgress(BACKSPACE)
  of 'v', 'V':
    addAndProgress(VT)
  of 't', 'T':
    addAndProgress('\t')
  of '\'', '\"':
    addAndProgress(L.buf[L.bufpos])
  of '\\':
    addAndProgress('\\')
  of 'x', 'X':
    inc(L.bufpos)
    var xi = 0
    handleHexChar(L, xi, 1)
    handleHexChar(L, xi, 2)
    tok.literal.add(chr(xi))
  of 'u', 'U':
    if tok.tokType == tkCharLit:
      L.handleDiag(lexDiagInvalidCharLiteralUnicodeCodepoint)
    inc(L.bufpos)
    var xi = 0
    if L.buf[L.bufpos] == '{':
      inc(L.bufpos)
      let start = L.bufpos
      while L.buf[L.bufpos] != '}':
        handleHexChar(L, xi, 0)
      if start == L.bufpos:
        L.handleDiag(lexDiagInvalidUnicodeCodepointEmpty)
      inc(L.bufpos)
      if xi > 0x10FFFF:
        let hex = ($L.buf)[start..L.bufpos-1]
        L.handleDiag(lexDiagInvalidUnicodeCodepointGreaterThan0x10FFFF, hex)
    else:
      handleHexChar(L, xi, 1)
      handleHexChar(L, xi, 2)
      handleHexChar(L, xi, 3)
      handleHexChar(L, xi, 4)
    addUnicodeCodePoint(tok.literal, xi)
  of '0'..'9':
    if matchTwoChars(L, '0', {'0'..'9'}):
      L.handleDiag(lexDiagDeprecatedOctalPrefix)
    var xi = 0
    handleDecChars(L, xi)
    if (xi <= 255):
      tok.literal.add(chr(xi))
    else:
      L.handleDiag(lexDiagInvalidCharLiteralConstant)
  else:
      L.handleDiag(lexDiagInvalidCharLiteralConstant)

proc handleCRLF(L: var Lexer, pos: int): int =
  template registerLine =
    if L.getColNumber(pos) > MaxLineLength:
      L.diagLineTooLong(pos)

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
        L.handleDiagPos(lexDiagUnclosedTripleString, L.lineStart)
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
        L.handleDiag(lexDiagUnclosedSingleString)
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
    L.handleDiag(lexDiagInvalidCharLiteral)
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
      L.handleDiag(lexDiagMissingClosingApostrophe)
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
    case buf[pos+1]
    of '\136':
      case buf[pos+2]
      of '\152': result = 3.m # ∘
      of '\153': result = 3.m # ∙
      of '\167': result = 3.m # ∧
      of '\168': result = 3.a # ∨
      of '\169': result = 3.m # ∩
      of '\170': result = 3.a # ∪
      else:      discard
    of '\138':
      case buf[pos+2]
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
      else:      discard
    of '\152':
      if buf[pos+2] == '\133': result = 3.m # ★
    else:
      discard
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
      case L.buf[pos+1]
      of SymChars:
        inc(pos)
        suspicious = true
      of '_':
        L.handleDiag(lexDiagMalformedIdentUnderscores)
        break
      else:
        L.handleDiag(lexDiagMalformedTrailingUnderscre)
        break
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
      L.diagLintName(tok.ident.s.normalize, tok.ident.s)
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
      if isDoc:
        tok.literal.add "\n"
        inc tok.iNumber
        var c = toStrip
        while L.buf[pos] == ' ' and c > 0:
          inc pos
          dec c
    of nimlexbase.EndOfFile:
      tokenEndIgnore(tok, pos)
      L.handleDiagPos(lexDiagUnclosedComment, pos)
      break
    else:
      if isDoc: tok.literal.add L.buf[pos]
      inc(pos)
  L.bufpos = pos

proc scanComment(L: var Lexer, tok: var Token) =
  var pos = L.bufpos
  tok.tokType = tkComment
  # iNumber contains the number of '\n' in the token
  tok.iNumber = 0
  assert L.buf[pos+1] == '#'

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

proc skip(L: var Lexer, tok: var Token) =
  var pos = L.bufpos
  tokenBegin(tok, pos)
  tok.strongSpaceA = 0

  while true:
    case L.buf[pos]
    of ' ':
      inc(pos)
      inc(tok.strongSpaceA)
    of '\t':
      if not L.allowTabs:
        L.handleDiagPos(lexDiagNoTabs, pos)
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
          skipMultiLineComment(L, tok, pos+2, false)
          pos = L.bufpos
        else:
          break
      tok.strongSpaceA = 0

      if L.buf[pos] > ' ' and (L.buf[pos] != '#' or L.buf[pos+1] == '#'):
        tok.indent = indent
        L.currLineIndent = indent
        break
    of '#':
      # do not skip documentation comment:
      if L.buf[pos+1] == '#': break

      if L.buf[pos+1] == '[':
        skipMultiLineComment(L, tok, pos+2, false)
        pos = L.bufpos
      else:
        tokenBegin(tok, pos)
        while L.buf[pos] notin {CR, LF, nimlexbase.EndOfFile}:
          inc(pos)
        tokenEndIgnore(tok, pos+1)
    else:
      break                   # EndOfFile also leaves the loop
  tokenEndPrevious(tok, pos-1)
  L.bufpos = pos

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
      case L.buf[L.bufpos+1]
      of ']':
        tok.tokType = tkBracketDotRi
        inc(L.bufpos, 2)
      of '}':
        tok.tokType = tkCurlyDotRi
        inc(L.bufpos, 2)
      of ')':
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
        L.handleDiag(lexDiagInvalidToken, $c)
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
          L.handleDiag(lexDiagInvalidTokenSpaceBetweenNumAndIdent)
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
            L.handleDiag(lexDiagInvalidTokenSpaceBetweenNumAndIdent)
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
        L.handleDiag(lexDiagInvalidToken, $c)
        inc(L.bufpos)
  atTokenEnd()

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
