## **Note:** Import ``experimental/sexp_parse`` to use this module

import
  std/[lexbase, streams]

from std/strutils import
  parseBiggestInt, parseFloat,
  `%`,
  Digits, Whitespace,
  contains
from std/unicode import toUTF8, Rune
from std/private/decode_helpers import handleHexChar

type
  SexpEventKind* = enum  ## enumeration of all events that may occur when
                         ## parsing
    sexpError,           ## an error occurred during parsing
    sexpEof,             ## end of file reached
    sexpString,          ## a string literal
    sexpSymbol,          ## a symbol
    sexpKeyword,         ## `:keyword` event
    sexpInt,             ## an integer literal
    sexpFloat,           ## a float literal
    sexpNil,             ## the value ``nil``
    sexpDot,             ## the dot to separate car/cdr
    sexpListStart,       ## start of a list: the ``(`` token
    sexpListEnd,         ## end of a list: the ``)`` token

  TTokKind* = enum        # must be synchronized with SexpEventKind!
    tkError,
    tkEof,
    tkString,
    tkSymbol,
    tkKeyword,
    tkInt,
    tkFloat,
    tkNil,
    tkDot,
    tkParensLe,
    tkParensRi
    tkSpace

  SexpError* = enum        ## enumeration that lists all errors that can occur
    errNone,               ## no error
    errInvalidToken,       ## invalid token
    errParensRiExpected,   ## ``)`` expected
    errQuoteExpected,      ## ``"`` expected
    errEofExpected,        ## EOF expected

  SexpParser* = object of BaseLexer ## the parser object.
    str: string
    tok: TTokKind ## Current token that lexer has processed
    kind: SexpEventKind
    err: SexpError

proc close*(parser: var SexpParser) {.inline.} =
  ## closes the parser `parser` and its associated input stream.
  lexbase.close(parser)

proc str*(parser: SexpParser): string {.inline.} =
  ## returns the character data for the events: ``sexpInt``, ``sexpFloat``,
  ## ``sexpString``
  assert(parser.kind in {sexpInt, sexpFloat, sexpString})
  result = parser.str

proc getInt*(parser: SexpParser): BiggestInt {.inline.} =
  ## returns the number for the event: ``sexpInt``
  assert(parser.kind == sexpInt)
  result = parseBiggestInt(parser.str)

proc getFloat*(parser: SexpParser): float {.inline.} =
  ## returns the number for the event: ``sexpFloat``
  assert(parser.kind == sexpFloat)
  result = parseFloat(parser.str)

proc kind*(parser: SexpParser): SexpEventKind {.inline.} =
  ## returns the current event type for the SEXP parser
  result = parser.kind

proc getColumn*(parser: SexpParser): int {.inline.} =
  ## get the current column the parser has arrived at.
  result = getColNumber(parser, parser.bufpos)

proc getLine*(parser: SexpParser): int {.inline.} =
  ## get the current line the parser has arrived at.
  result = parser.lineNumber

proc parseString(parser: var SexpParser): TTokKind =
  result = tkString
  var pos = parser.bufpos + 1
  while true:
    case parser.buf[pos]
    of '\0':
      parser.err = errQuoteExpected
      result = tkError
      break
    of '"':
      inc(pos)
      break
    of '\\':
      case parser.buf[pos+1]
      of '\\', '"', '\'', '/':
        add(parser.str, parser.buf[pos+1])
        inc(pos, 2)
      of 'b':
        add(parser.str, '\b')
        inc(pos, 2)
      of 'f':
        add(parser.str, '\f')
        inc(pos, 2)
      of 'n':
        add(parser.str, '\L')
        inc(pos, 2)
      of 'r':
        add(parser.str, '\C')
        inc(pos, 2)
      of 't':
        add(parser.str, '\t')
        inc(pos, 2)
      of 'u':
        inc(pos, 2)
        var r: int
        if handleHexChar(parser.buf[pos], r): inc(pos)
        if handleHexChar(parser.buf[pos], r): inc(pos)
        if handleHexChar(parser.buf[pos], r): inc(pos)
        if handleHexChar(parser.buf[pos], r): inc(pos)
        add(parser.str, toUTF8(Rune(r)))
      else:
        # don't bother with the error
        add(parser.str, parser.buf[pos])
        inc(pos)
    of '\c':
      pos = lexbase.handleCR(parser, pos)
      add(parser.str, '\c')
    of '\L':
      pos = lexbase.handleLF(parser, pos)
      add(parser.str, '\L')
    else:
      add(parser.str, parser.buf[pos])
      inc(pos)
  parser.bufpos = pos # store back

proc parseNumber(parser: var SexpParser) =
  template pos(): var int = parser.bufpos

  # sign, positive, negative, or implicitly positive
  if parser.buf[pos] in ['-', '+']:
    add(parser.str, parser.buf[pos])
    inc(pos)

  # the decimal point or integer digits prior to
  if parser.buf[pos] == '.':
    add(parser.str, "0.")
    inc(pos)
  else:
    while parser.buf[pos] in Digits:
      add(parser.str, parser.buf[pos])
      inc(pos)
    if parser.buf[pos] == '.':
      add(parser.str, '.')
      inc(pos)

  # digits after the dot:
  while parser.buf[pos] in Digits:
    add(parser.str, parser.buf[pos])
    inc(pos)

  # exponentiation
  if parser.buf[pos] in {'E', 'e'}:
    add(parser.str, parser.buf[pos])
    inc(pos)
    if parser.buf[pos] in {'+', '-'}:
      add(parser.str, parser.buf[pos])
      inc(pos)
    while parser.buf[pos] in Digits:
      add(parser.str, parser.buf[pos])
      inc(pos)

proc parseSymbol(p: var SexpParser) =
  ## Using symbol definition from
  ## http://www.lispworks.com/documentation/HyperSpec/Body/02_cd.htm
  ## 
  ## Note: presently escaping such as '\.' for the dot character or blank
  ##       spaces is not yet supported. Unclear whether this will be accepted
  ##       or not in the future.
  var pos = p.bufpos
  while p.buf[pos] notin Whitespace + {')', '(', '\0'}:
    add(p.str, p.buf[pos])
    inc(pos)
  p.bufpos = pos

proc open*(parser: var SexpParser, input: Stream) =
  ## initializes the parser with an input stream.
  lexbase.open(parser, input)
  parser.kind = sexpError
  parser.str = ""

proc getTok*(parser: var SexpParser): TTokKind =
  # echo ">> ", parser.tok, " ", parser.bufpos, " @ ", parser.buf[parser.bufpos]
  setLen(parser.str, 0)
  case parser.buf[parser.bufpos]
  of '-', '+', '0'..'9': # numbers that start with a . are handled below.
    parseNumber(parser)
    if {'.', 'e', 'E'} in parser.str:
      result = tkFloat
    else:
      result = tkInt
  of '"': #" # gotta fix nim-mode
    result = parseString(parser)
  of '(':
    inc(parser.bufpos)
    result = tkParensLe
  of ')':
    inc(parser.bufpos)
    result = tkParensRi
  of '\0':
    result = tkEof
  of ':':
    parseSymbol(parser)
    result = tkKeyword
  of {'\x01' .. '\x1F'} - Whitespace:
    inc(parser.bufpos)
    result = tkError
  of Whitespace:
    result = tkSpace
    if parser.buf[parser.bufpos] in lexbase.NewLines:
      parser.bufpos = parser.handleRefillChar(parser.bufpos)
    else:
      inc(parser.bufpos)

    while parser.bufpos < parser.buf.len and
          parser.buf[parser.bufpos] in Whitespace:
      if parser.buf[parser.bufpos] in lexbase.NewLines:
        parser.bufpos = parser.handleRefillChar(parser.bufpos)
      else:
        inc parser.bufpos
  of '.':
    inc(parser.bufpos)
    if parser.bufpos < parser.buf.len and
        parser.buf[parser.bufpos] in '0' .. '9':
      dec(parser.bufpos)
      result = tkFloat
      parseNumber(parser)
    else:
      result = tkDot
  else:
    parseSymbol(parser)
    if parser.str == "nil":
      result = tkNil
    else:
      result = tkSymbol
  parser.tok = result
  # echo " << ", parser.tok, " ", parser.bufpos

proc space*(p: var SexpParser) =
  ## Skip all space tokens from the current point onwards
  while p.tok == tkSpace:
    discard getTok(p)

func error*(p: SexpParser): SexpError {.inline.} =
  ## most recent error kind
  p.err
func currString*(p: SexpParser): string {.inline.} =
  ## the last string that was parsed
  p.str
func currToken*(p: SexpParser): TTokKind {.inline.} =
  ## the last token that was parsed
  p.tok

proc captureCurrString*(p: var SexpParser): string =
  result = p.str
  p.str = ""

func isTok*(p: SexpParser, tok: TTokKind): bool {.inline.} =
  p.tok == tok