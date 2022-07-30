import std/[terminal, unicode, sequtils, strformat, strutils]

## This module provides implementation of the basic colored text
## abstractions. Text is represented using sequence of colored unicode
## runes.
##
## The module is designed to be easily reusable for quick formatting - most
## basic usage is `"text" + fgRed` - merge text with given formatting
## style.
##
## In addition to the string construction basic modification options are
## added - indent, align left/right/center, split by lines, add
## indentation. This module is designed to /generate/ formatted text, so no
## `find/replace` and other search-based procedures are added. It is
## assumed that all necessary formatting is done beforehand.
##
## ..note:: If you implement string formatting functions that return
##   `ColText` (which is a recommended approach, since regular string
##   embedded ansi escapes does not compose properly) you need to `export`
##   stringification operator as well - ``export $``, otherwise
##   `echo treeRepr()` and similar operations become a little more
##   annoying to use.

export
  # Export most commonly used styles so people won't have to import
  # `std/terminal` all over the place.
  terminal.ForegroundColor,
  terminal.BackgroundColor,
  terminal.Style

type
  TermColorBg* = distinct uint8
    ## Terminal background color. 16-colors are represented as the first 16
    ## values (with 0 being the default), everything higher maps to 256
    ## colors

  TermColorFg* = distinct uint8
    ## Terminal foreground color. Mapping is identical to the background
    ## version

func `$`(fg: TermColorFg): string =
  ## Convert foreground color to human-readble representation
  result = $fg.uint8
  if fg.uint8 < 15:
    result.add "($1)" % $ForegroundColor(low(ForegroundColor).uint8 + fg.uint8)

func `$`(bg: TermColorBg): string =
  ## Convert background color to human-readable representation
  result = $bg.uint8
  if bg.uint8 < 15:
    result.add "($1)" % $BackgroundColor(low(BackgroundColor).uint8 + bg.uint8)

func `==`*(a, b: TermColorBg | TermColorFg): bool =
  ## Color equality comparison
  a.uint8 == b.uint8

type
  ColStyle* = object
    ## Styling options
    fg*: TermColorFg ## Foreground color
    bg*: TermColorBg ## Background
    style*: set[Style] ## Other styling options (italic, underline, dim,
                       ## bright etc.)

  ColRune* = object
    ## Single unicode rune with applied coloring
    rune*: Rune
    style*: ColStyle

  ColText* = object
    ## Sequence of colored unicode runes
    runes*: seq[ColRune]

  ColRuneLine* = seq[ColRune]
  ColRuneGrid* = seq[ColRuneLine]
  ColModifier* =
    ForegroundColor | BackgroundColor | TermColorFg | TermColorBg |
    set[Style] | Style | ColStyle ## Typeclass, contains all types that can
                                  ## be used for text styling




func len*(text: ColText): int =
  ## Number of *runes* in text
  text.runes.len

func add*(text: var ColText, rune: ColRune | seq[ColRune]) =
  ## Append one or more colored runes to the text
  text.runes.add rune

iterator items*(text: ColText): ColRune =
  ## Iterate over all runes left to right
  for item in text.runes:
    yield item

iterator ritems*(text: ColText): ColRune =
  ## Iterate over all runes right to left
  var idx = text.runes.high
  while 0 <= idx:
    yield text.runes[idx]
    dec idx

func `==`*(s1, s2: ColStyle): bool =
  ## Compare styles for equality - foreground, background and style
  s1.fg == s2.fg and s1.bg == s2.bg and s1.style == s2.style

func contains*(ps: ColStyle, s: Style): bool =
  ## Check if coloring contains specific style option
  ps.style.contains(s)

func termColor*(bg: BackgroundColor): TermColorBg =
  ## Convert `std/terminal.BackgroundColor` to terminal background color
  if bg != bgDefault:
    result = TermColorBg(bg.uint8 - low(BackgroundColor).uint8)

func termColor*(bg: ForegroundColor): TermColorFg =
  ## Convert `std/terminal.ForegroundColor` to terminal foreground color
  if bg != fgDefault:
    result = TermColorFg(bg.uint8 - low(ForegroundColor).uint8)

func `==`*(f1: TermColorFg, f2: ForegroundColor): bool =
  ## Compare for equality with `std/terminal` color
  termColor(f2) == f1

func `==`*(f1: TermColorBg, f2: BackgroundColor): bool =
  ## Compare for equality with `std/terminal` color
  termColor(f2) == f1


func initColStyle*(
    fg: ForegroundColor = fgDefault,
    bg: BackgroundColor = bgDefault,
    style: set[Style] = {}
  ): ColStyle =
  ## Initialize color style with given options
  ColStyle(fg: fg.termColor, bg: bg.termColor, style: style)

func colStyle*(fg: ForegroundColor): ColStyle =
  ## Convert to style with only foreground option set
  ColStyle(fg: fg.termColor)

func colStyle*(bg: BackgroundColor): ColStyle =
  ## Convert to style with only background option set
  ColStyle(bg: bg.termColor)

func colStyle*(fg: TermColorFg): ColStyle =
  ## Convert to style with only foreground color set
  ColStyle(fg: fg)

func colStyle*(bg: TermColorBg): ColStyle =
  ## Convert to style with only background color set
  ColStyle(bg: bg)

func colStyle*(style: set[Style]): ColStyle =
  ## Convert to style with given set of styling options
  ColStyle(style: style)

func colStyle*(style: Style): ColStyle =
  ## Convert to style with single styling option set
  ColStyle(style: {style})

func colStyle*(style: ColStyle): ColStyle =
  ## Passthrough proc, used to simplify mapping of the `ColModifier`
  ## typeclass to the `ColStyle` object.
  style

func default*(rune: typedesc[ColRune]): ColRune =
  ## Default value of the colored rune
  ColRune(style: initColStyle())

func default*(style: typedesc[ColStyle]): ColStyle =
  ## Default value of the color style
  ColStyle(fg: fgDefault.termColor(), bg: bgDefault.termColor())

func uc*(s: static[string]): Rune =
  ## Create single unicode rune from string literal - `uc"⮰"`
  runeAt(s, 0)

func isDefault*(col: TermColorFg | TermColorBg): bool =
  ## Check if foreground or background color have default value
  col.uint8 == 0

func `+=`*(s1: var ColStyle, s2: ColStyle) =
  ## Merge two styling optons, overriding target if the source has
  ## non-default value for background or foreground colors
  s1.style = s1.style + s2.style
  s1.fg = if s2.fg.isDefault(): s1.fg else: s2.fg
  s1.bg = if s2.bg.isDefault(): s1.bg else: s2.bg

func `+`*(s1, s2: ColStyle): ColStyle =
  ## Merge two styles. Second one overrides colors if they are non-default.
  result = s1
  result += s2

func `+`*(a: ColModifier, b: distinct ColModifier): ColStyle =
  ## Merge two color modifiers and generate color style
  ## (`fgRed+{styleUnderline}`)
  colStyle(a) + colStyle(b)

func toColRune*(rune: Rune, style: ColStyle): ColRune =
  ## Convert colered rune to styled one with `style`
  ColRune(rune: rune, style: style)

func toColText*(text: string, style: ColStyle = default(ColStyle)): ColText =
  ## Convert text colored one using given style
  for rune in runes(text):
    result.add toColRune(rune, style)

func `+`*(text: string, style: ColModifier): ColText =
  ## Convert text colored one using given style
  toColText(text, colStyle(style))

func `+`*(ch: char, style: ColModifier): ColRune =
  ## Convert character to colored rune with given style
  toColRune(Rune(ch), colStyle(style))

func `+`*(r: Rune, style: ColModifier): ColRune =
  ## Convert rune to colored one with given style modifier
  toColRune(r, colStyle(style))

func setStyle*(
    text: var ColText,
    new: ColStyle,
    override: bool = true,
  ) =
  ## Set style for all runes in text, optionally overriding non-defaulted
  ## values (or resetting them completely)

  for ch in mitems(text.runes):
    if override:
      ch.style.fg = new.fg
      ch.style.bg = new.bg
      ch.style.style = new.style

    else:
      if isDefault(ch.style.fg):
        ch.style.fg = new.fg

      if isDefault(ch.style.bg):
        ch.style.bg = new.bg

      for s in new.style:
        ch.style.style.incl s

func `+`*(text: sink ColText, style: ColModifier): ColText =
  ## Apply styling options to colored text
  result = text
  result.setStyle(colStyle(style), false)

func toColText*(text: ColText): ColText =
  ## Passthrough implementation of converter to colored text
  text

func toColText*(rune: ColRune): ColText =
  ## Convert single rune to colored text
  ColText(runes: @[rune])


func clt*(str: string): ColText =
  ## Shorthand to construct colored text string with default value
  ## (`clt"default"`)
  str + default(ColStyle)

func clt*(ch: char): ColText =
  ## Shorthand to construct colored text with single character
  toColText(ch + default(ColStyle))

func clr*(ch: char): ColRune =
  ## Construct colored rune with default styling
  ch + default(ColStyle)


func isNewline*(rune: ColRune): bool =
  ## Check if rune is a newline rune
  rune.rune == Rune(int('\n'))

func toLower*(text: sink ColText): ColText =
  ## Convert colored text to lowercase
  result = text
  for rune in mitems(result.runes):
    rune.rune = toLower(rune.rune)

func toUpper*(text: sink ColText): ColText =
  ## Convert colored text to uppercase
  result = text
  for rune in mitems(result.runes):
    rune.rune = toUpper(rune.rune)


func alignLeft*(
    text: sink ColText, length: int, padding: ColRune = clr(' ')
  ): ColText =
  ## Align colored text left, using padding rune to fill in missing
  ## elements

  result = text
  if result.len < length:
    result.runes.add padding.repeat(length - result.len)

func alignCenter*(
    text: sink ColText, length: int, padding: ColRune = clr(' ')
  ): ColText =
  ## Center colored text, using padding rune to fill in missing elements

  if result.len < length:
    let
      diff = length - text.len
      left = diff div 2
      right = diff - left

    result.runes.add padding.repeat(left)
    result.add text.runes
    result.runes.add padding.repeat(right)

func alignRight*(
    text: ColText, length: int, padding: ColRune = clr(' ')
  ): ColText =
  ## Align colored text right, using padding rune to fill in missing
  ## elements

  if text.len < length:
    result.runes.add padding.repeat(length - text.len)

  result.runes.add text.runes

func `|<<`*(s: sink ColText, l: int): ColText =
  ## Shorthand for left align of the colored text. Since library is mostly
  ## used for final message formatting it is a very common operation and it
  ## was implemented as an operator as well.
  alignLeft(s, l)

func `|>>`*(s: sink ColText, l: int): ColText =
  ## Shorthand of the right align of the colored text.
  alignRight(s, l)

func `==`*(rune: ColRune, ch: char): bool =
  ## Compare rune for equality with regular character
  rune.rune == Rune(int(ch))

func hasNewline*(text: ColText): bool =
  ## Check if colored text has any newlines
  for rune in text.runes:
    if isNewline(rune):
      return true

func hasOnlyTailNewline*(text: ColText): bool =
  ## Check if colored text has only trailing newlines and no in-body ones
  var onTail = true
  result = true
  for rune in ritems(text):
    if isNewline(rune):
      if not onTail:
         return false

    else:
      onTail = false

func newline*(text: var ColText) =
  ## Append newline to the colored text
  text.runes.add uc("\n") + default(ColStyle)

iterator lines*(text: ColText): ColRuneLine =
  ## Iterate over lines of the colored text
  var buf: ColRuneLine
  for rune in text.runes:
    if rune.isNewline:
      yield buf
      buf.setLen(0)

    else:
      buf.add rune

  if buf.len > 0:
    yield buf

func width*(text: ColText): int =
  ## Get maximum width (line length) of the colored text
  var buf = 0
  for rune in text.runes:
    if rune.isNewline:
      result = max(buf, result)
      buf = 0

    else:
      inc buf

  if buf > 0:
    return buf

func add*(colored: var ColText, other: ColText) =
  ## Add one colored text block to another
  colored.runes.add other.runes

func add*(colored: var ColText, other: ColRuneLine) =
  ## Add colored rune line (sequence of runes) to colored text
  colored.runes.add other

func add*(colored: var ColText, rune: ColRune) {.inline.} =
  ## Add colored rune to the text
  colored.runes.add rune

func add*(colored: var ColText, ch: string | char) {.inline.} =
  ## Add string or character with default style to colored text,
  colored.add ch + default(ColStyle)

proc indent*(
    str: ColText,
    count: int,
    indentAfter: int = -1,
    indent: ColText = clt(" "),
    prefix: ColText = clt("")
  ): ColText =
  ## Indent colored text lines by `count` repetitions of the `indent` text.
  ## Add `prefix` before each line.
  ##
  ## - `indentAfter` - first line to start applying indentation after
  ##   (defaults to `-1`, so indentation is applied to the whole text). Can
  ##   be used to allow `[header, line, line]` text to be easily processed


  var idx = 0
  for line in lines(str):
    if idx <= indentAfter:
      result.add line

    else:
      result.newline()
      for _ in 0 ..< count - prefix.len:
        result.add indent

      result.add prefix
      result.add line

    inc idx

func join*(text: seq[ColText], sep: ColText): ColText =
  var first = true
  for item in text:
    if not first:
      result.add sep
    first = false

    result.add item

func stripLines*(
    text: ColText,
    leading: bool = false,
    trails: bool = true,
    chars: set[char] = {' '}
  ): ColText =
  ## Strip character from each line in text

  var idx = 0
  for line in lines(text):
    var start = 0
    if leading:
      while int(line[start].rune) <= ord(high(char)) and
            char(line[start].rune) in chars and
            start < high(line):
        inc start

    var final = high(line)
    if trails:
      while int(line[final].rune) <= ord(high(char)) and
            char(line[final].rune) in chars and
            0 < final:
        dec final


    if start == final and
       int(line[final].rune) <= ord(high(char)) and
       char(line[final].rune) in chars:

      if idx > 0:
        result.add clt("\n")

    else:
      if idx > 0:
        result.add clt("\n")

      result.add line[start .. final]

    inc idx

func `&`*(t1: sink ColText, t2: ColText): ColText =
  ## Concatenate two colored text blocks
  result = t1
  result.runes.add t2.runes

func `&`*(t1: sink ColText, t2: string): ColText =
  ## Concatenate colored text and regular string (with default style)
  result = t1
  result.add clt(t2)

func `&`*(t1: string, t2: ColText): ColText =
  ## Concatenate colored text and regular string (with default style)
  result = toColText(t1)
  result.add t2

func code(col: TermColorFg): uint8 =
  ## Get ansi code for the 16-color foreground terminal color
  col.uint8 + low(ForegroundColor).uint8

func code(col: TermColorBg): uint8 =
  ## Get ansi code for the 16-color background terminal color
  col.uint8 + low(BackgroundColor).uint8

func ansiEsc(code: SomeInteger): string =
  ## Create ansi escape sequence with given code
  "\e[" & $code & "m"

func ansiEsc(col: TermColorFg): string =
  ## Create ansi escape sequence with given terminal color
  if col.uint8 <= 7:
    ansiEsc(code(col))

  elif col.uint8 <= 15:
    ansiEsc(code(col) + 60)

  else:
    "\e[38;5;" & $(col.uint8) & "m"


func ansiEsc(col: TermColorBg): string =
  ## Create ansi escape sequence with given terminal color
  if col.uint8 <= 7:
    ansiEsc(code(col))

  elif col.uint8 <= 15:
    ansiEsc(code(col) + 60)

  else:
    "\e[48;5;" & $(col.uint8) & "m"

func ansiEsc(style: Style, open: bool): string =
  ## Create ansi escape sequence with given style. `open` controls whether
  ## styling sequence is used for open or for close
  let diff = if open: 0 else: 20
  case style:
    of styleBright:        ansiEsc(1 + diff)
    of styleDim:           ansiEsc(2 + diff)
    of styleItalic:        ansiEsc(3 + diff)
    of styleUnderscore:    ansiEsc(4 + diff)
    of styleBlink:         ansiEsc(5 + diff)
    of styleBlinkRapid:    ansiEsc(6 + diff)
    of styleReverse:       ansiEsc(7 + diff)
    of styleHidden:        ansiEsc(8 + diff)
    of styleStrikethrough: ansiEsc(9 + diff)



func ansiDiff(s1, s2: ColStyle): string =
  ## Generate ansi escape sequences to transition from style `s1` to style
  ## `s2`
  if s2.fg != s1.fg:
    if s2.fg.isDefault():
      result &= ansiEsc(39)

    else:
      result &= ansiEsc(s2.fg)

  if s2.bg != s1.bg:
    if s2.bg.isDefault():
      result &= ansiEsc(49)

    else:
      result &= ansiEsc(s2.bg)

  for style in (s1.style - s2.style):
    result &= ansiEsc(style, false)

  for style in (s2.style - s1.style):
    result &= ansiEsc(style, true)


func lispRepr*(rune: ColRune): string =
  &"({rune.rune} :fg {rune.style.fg} :bg {rune.style.bg} :style {rune.style.style})"

func lispRepr*(rune: ColText): string =
  result = "("
  for idx, rune in rune.runes:
    if 0 < idx: result.add "\n "
    result.add lispRepr(rune)

  result &= ")"

func toString*(rune: ColRune, color: bool = true): string =
  ## Convert colored rune to regular string, with ansi escape sequences.
  ## `color` controls whether styling is going to be applied or not.
  if color:
    result &= ansiDiff(default ColStyle, rune.style)
    result &= $rune.rune
    result &= ansiEsc(0)

  else:
    result = $rune.rune

func toString*(runes: seq[ColRune], color: bool = true): string =
  ## Convert sequence of colored runes to the string, with ansi escape
  ## sequences in. `color` controls whether styling is going to be applied
  ## or not.
  if color:
    var prev = initColStyle()
    for rune in runes:
      result &= ansiDiff(prev, rune.style)
      result &= $rune.rune
      prev = rune.style

    if not prev.fg.isDefault() or
       not prev.bg.isDefault() or
       0 < prev.style.len():
      result &= ansiEsc(0)

  else:
    for rune in runes:
      result &= $rune.rune


func toString*(text: ColText, color: bool = true): string =
  ## Convert colored text to string with ansi escape sequences
  toString(text.runes, color)

const
  offsetGray: uint8 = 232
  offset256:  uint8 = 16
  max256:     uint8 = 255
  scaleRed:   uint8 = 6 * 6
  scaleGreen: uint8 = 6

func `$`*(colored: ColRune): string =
  ## Convert to string with ansi escape sequences. To disable coloring use
  ## `toString` procedure instead.
  toString(colored)

func `$`*(colr: ColRuneLine): string =
  ## Convert to string with ansi escape sequences. To disable coloring use
  ## `toString` procedure instead.
  toString(colr)

func `$`*(text: ColText): string =
  ## Convert to string with ansi escape sequences. To disable coloring use
  ## `toString` procedure instead.
  toString(text.runes)

func `$`*(colr: ColRuneGrid): string =
  ## Convert to multiline string with ansi escape sequences. To disable
  ## coloring use `toString` procedure instead.
  for idx, line in colr:
    if idx > 0:
      result &= "\n"

    result.add toString(line)



func termFg*(r, g, b: range[0 .. 5]): TermColorFg =
  ## Create 256-terminal color with given red, green and blue coloring.
  ## Colors are mapped to 216-element color cube.
  TermColorFg(offset256 + b.uint8 + g.uint8 * scaleGreen + scaleRed * r.uint8)

func termBg*(r, g, b: range[0 .. 5]): TermColorBg =
  ## 256-color for background. Maps to 216-element color cube.
  TermColorBg(offset256 + b.uint8 + g.uint8 * scaleGreen + scaleRed * r.uint8)


func termBg*(gray: range[0 .. 23]): TermColorBg =
  ## Create 256-color with given grey background value (from 0 to 23, for
  ## total 24 shades).
  TermColorBg(offsetGray + gray.uint8)

func termFg*(gray: range[0 .. 23]): TermColorFg =
  ## Grey foreground value
  TermColorFg(offsetGray + gray.uint8)

func addIndent*(
    res: var ColText,
    level: int, sep: int = 2,
    prefix: ColRune = clr(' ')
  ) =
  ## Add indentation prefix to the colored text.
  for _ in 0 ..< level * sep:
    res.add prefix

template coloredResult*(indentationStep: int = 2): untyped =
  ## Inject helper templates for construction of the colored text in
  ## recursive procedure. Adds one-argument procedures and templates that
  ## append text to the main procedure `result`. Injected indentation procs
  ## use `indentationStep` of whitespaces per level.
  ##
  ## .. code-block:: nim
  ##
  ##     proc treeRepr(node: SomeNode): ColText =
  ##       coloredResult() # Inject `add`, `addIndent` procs
  ##       proc aux(node: SomeNode, level: int) =
  ##         addIndent(level)
  ##         add $node.kind # Add text to the `result` of the main proc
  ##         for sub in node:
  ##           aux(sub, level + 1)
  ##
  ##       aux(node)
  ##
  ## Injected procs
  ##
  ## - `add(string)`, `add(string, string)`
  ## - `addIndent(int)`, `addi(int, string)`
  ## - `endResult()` - return colored result. Required for proper work
  ##   of the code in the compile-time context, otherwise modification
  ##   of the  `addr result` does not work properly.
  static:
    when not declared(result):
      {.error: "'coloredResult' template can only be called inside of the procedure returning 'ColText' as a result, or other environment that has `var result: ColText` defined."}

  var outPtr {.used.}: ptr ColText = addr result

  template endResult(): untyped {.used.} =
    when nimvm:
      return outPtr[]

    else:
      return

  proc addf(format: string, args: varargs[ColText, toColText]) =
    outPtr[].addf(format, args)

  template add(arg: untyped): untyped {.used.} = outPtr[].add arg
  template add(arg1, arg2: untyped): untyped {.used.} =
    outPtr[].add(arg1)
    outPtr[].add(arg2)

  template addIndent(level: int, sep: int = indentationStep): untyped {.used.} =
    outPtr[].addIndent(level, sep)

  template addi(ind: int, arg: untyped): untyped {.used.} =
    outPtr[].addIndent(ind, indentationStep)
    outPtr[].add(arg)

func `[]=`*[R1, R2: openArray[int] | Slice[int] | int](
    buf: var ColRuneGrid,
    rowIdx: R1, colIdx: R2,
    ch: ColRune,
    fill: ColRune = clr(' ')
  ) =
  ## Assign rune to specific part of the colored rune grid, expanding it as
  ## needed to fill out missing parts. This procs provide a simplified
  ## resizeable 'canvas' that allows to draw colored text on it, without
  ## having to constantly check for out-of-bounds errors.
  ##
  ## - `fill` - colored rune used to fill in missing parts

  proc aux(row, col: int, buf: var ColRuneGrid, ch: ColRune) =
    for _ in buf.len .. row:
      buf.add @[fill]

    buf[row] &= repeat(fill, max(col - buf[row].len + 1, 0))
    buf[row][col] = ch


  var rows: seq[int]
  var cols: seq[int]

  when rowIdx is int:
    rows = @[rowIdx]

  elif rowIdx is Slice[int] | openArray[int]:
    for row in rowIdx:
      rows.add row


  when colIdx is int:
    cols = @[colIdx]

  elif colIdx is Slice[int] | openArray[int]:
    for col in colIdx:
      cols.add col

  for row in rows:
    for col in cols:
      aux(row, col, buf, ch)

func `[]=`*(buf: var ColRuneGrid, row, col: int, str: ColText) =
  ## Assign colored text at specific point on the colored grid. Multiline
  ## text is assigned propery
  var rowIdx = 0
  for line in lines(str):
    for colIdx, ch in line:
      buf[row + rowIdx, col + colIdx] = ch

    inc rowIdx

func `[]=`*(buf: var ColRuneGrid, row, col: int, ch: char) =
  ## Assign single character to the colored rune grid, using default style
  buf[row, col] = toColRune(Rune(ch), initColStyle())

func grid*(text: ColText): ColRuneGrid =
  ## Convert colored text to grid
  for line in lines(text):
     result.add line

func addf*(
    text: var ColText,
    formatstr: string,
    colored: varargs[ColText, toColText]
  ) =
  ## Interpolate `formatstr` using values from `colored` and add results to
  ## the `text`.
  ##
  ## Iterpolation syntax is identical to the `std/strutils.addf` except
  ## `$name` is currently not supported, so only positional interpolation
  ## is available.
  for fr in addfFragments(formatstr):
    case fr.kind:
      of addfDollar:
        text.add "$"

      of addfText:
        text.add fr.text

      of addfVar, addfExpr:
        assert false, "var/expr formatting is not supported for colored text yet"

      of addfPositional, addfIndexed, addfBackIndexed:
        let idx = if fr.kind == addfBackIndexed:
                    len(colored) - fr.idx
                  else:
                    fr.idx

        assert (0 <= idx and idx < colored.len)
        text.add colored[idx]

func clformat*(
    formatstr: string, colored: varargs[ColText, toColText]): ColText =
  result.addf(formatstr, colored)

func `%`*(format: string, interpolate: openArray[ColText]): ColText =
  ## Shorthand for colored text interpolation
  result.addf(format, interpolate)
