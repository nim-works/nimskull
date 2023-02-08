#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf, Dominik Picheta
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## **Note:** Import ``experimental/sexp`` to use this module
##
## Symbolic Expressions, S-Expressions, s-expresssions, s-exp, sexp, whatever
## someone might call them are woefully underspecified. There aren't too many
## places too screw-up, but edge cases matter.
##
## This modules allows for s-exp handling and is "flavoured" by remaining
## largely compatible with emacs' EPC specification. As such it's meant for
## serdes (serializing and deserializing) data and will likely fall short for
## forgiving/precise parsing that a high quality lisp like language interpreter
## may require.
##
## Future Direction:
## - Keyword support (:someKeyWord) might be removed or reworked to not require
##     a key/value pair approach
## - cons vs list separation and nil handling

import experimental/sexp_parse

import
  std/[hashes, strutils, lexbase, streams, unicode, macros, algorithm]

import std/private/decode_helpers

type
  SexpNodeKind* = enum ## possible s-exp node types
    SNil,
    SInt,
    SFloat,
    SString,
    SSymbol,
    SKeyword,
    SList,
    SCons

  SexpNode* = ref SexpNodeObj ## s-exp node
  SexpNodeObj* {.acyclic.} = object
    case kind*: SexpNodeKind
    of SString:
      str*: string
    of SSymbol:
      symbol*: string
    of SInt:
      num*: BiggestInt
    of SFloat:
      fnum*: float
    of SList:
      elems*: seq[SexpNode]
    of SKeyword:             # xxx: might remove keywords, at least the forced
                             #      key/value aspect.
      key*: string
      value*: SexpNode
    of SCons:
      car*: SexpNode
      cdr*: SexpNode
    of SNil:
      discard

  Cons = tuple[car: SexpNode, cdr: SexpNode]

  SexpParsingError* = object of ValueError ## is raised for a s-exp error

const
  errorMessages: array[SexpError, string] = [
    "no error",
    "invalid token",
    "')' expected",
    "'\"' or \"'\" expected",
    "EOF expected",
  ]
  tokToStr: array[TTokKind, string] = [
    "invalid token",
    "EOF",
    "string literal",
    "symbol",
    "keyword",
    "int literal",
    "float literal",
    "nil",
    ".",
    "(", ")", "space"
  ]

proc errorMsg*(parser: SexpParser): string =
  ## returns a helpful error message for the event ``sexpError``
  assert(parser.kind == sexpError)
  result = "($1, $2) Error: $3" % [$getLine(parser),
                                   $getColumn(parser),
                                   errorMessages[parser.error]]

proc errorMsgExpected*(parser: SexpParser, e: string): string =
  ## returns an error message "`e` expected" in the same format as the
  ## other error messages
  result = "($1, $2) Error: $3, but found '$4' ($5)" % [
    $getLine(parser),
    $getColumn(parser),
    e & " expected",
    $parser.currString,
    $parser.currToken
  ]

proc raiseParseErr*(p: SexpParser, msg: string) {.noinline, noreturn.} =
  ## raises an `SexpParsingError` exception.
  raise newException(SexpParsingError, errorMsgExpected(p, msg))

proc newSString*(s: string): SexpNode =
  ## Creates a new `SString SexpNode`.
  result = SexpNode(kind: SString, str: s)

proc newSStringMove(s: sink string): SexpNode =
  result = SexpNode(kind: SString)
  shallowCopy(result.str, s)

proc newSInt*(n: BiggestInt): SexpNode =
  ## Creates a new `SInt SexpNode`.
  result = SexpNode(kind: SInt, num: n)

proc newSFloat*(n: float): SexpNode =
  ## Creates a new `SFloat SexpNode`.
  result = SexpNode(kind: SFloat, fnum: n)

proc newSNil*(): SexpNode =
  ## Creates a new `SNil SexpNode`.
  result = SexpNode(kind: SNil)

proc newSCons*(car, cdr: SexpNode): SexpNode =
  ## Creates a new `SCons SexpNode`
  ## NB: this will not evaluate the arguments, eg: 'nil . nil' -> 'nil'
  result = SexpNode(kind: SCons, car: car, cdr: cdr)

proc newSKeyword*(key: string, value: SexpNode): SexpNode =
  ## Create new `SKeyword` node with `key` and `value` specified
  result = SexpNode(kind: SKeyword, key: key, value: value)

proc newSList*(items: varargs[SexpNode]): SexpNode =
  ## Creates a new `SList SexpNode`
  result = SexpNode(kind: SList, elems: @items)

proc newSSymbol*(s: string): SexpNode =
  result = SexpNode(kind: SSymbol, symbol: s)

proc newSSymbolMove(s: string): SexpNode =
  result = SexpNode(kind: SSymbol)
  shallowCopy(result.symbol, s)

proc getStr*(n: SexpNode, default: string = ""): string =
  ## Retrieves the string value of a `SString SexpNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``SString``.
  if n.kind != SString: return default
  else: return n.str

proc getNum*(n: SexpNode, default: BiggestInt = 0): BiggestInt =
  ## Retrieves the int value of a `SInt SexpNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``SInt``.
  if n.kind != SInt: return default
  else: return n.num

proc getFNum*(n: SexpNode, default: float = 0.0): float =
  ## Retrieves the float value of a `SFloat SexpNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``SFloat``.
  if n.kind != SFloat: return default
  else: return n.fnum

proc getSymbol*(n: SexpNode, default: string = ""): string =
  ## Retrieves the int value of a `SList SexpNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``SList``.
  if n.kind != SSymbol: return default
  else: return n.symbol

proc getKey*(n: SexpNode, default: string = ""): string =
  ## Get key value from the `SKeyword` node
  ##
  ## Return `default` is `n` is not a `SKeyword`
  if n.kind != SKeyword: default else: n.key

proc getElems*(n: SexpNode, default: seq[SexpNode] = @[]): seq[SexpNode] =
  ## Retrieves the int value of a `SList SexpNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``SList``.
  if n.kind == SNil: return @[]
  elif n.kind != SList: return default
  else: return n.elems

proc getCons*(n: SexpNode, defaults: Cons = (newSNil(), newSNil())): Cons =
  ## Retrieves the cons value of a `SList SexpNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``SList``.
  if n.kind == SCons: return (n.car, n.cdr)
  elif n.kind == SList: return (n.elems[0], n.elems[1])
  else: return defaults

proc sexp*(s: string): SexpNode =
  ## Generic constructor for SEXP data. Creates a new `SString SexpNode`.
  result = SexpNode(kind: SString, str: s)

proc sexp*(keyword: (string, SexpNode)): SexpNode =
  ## Generic constructor for SEXP data. Creates a new `SKeyword SexpNode`.
  result = SexpNode(kind: SKeyword, key: keyword[0], value: keyword[1])

proc sexp*(n: BiggestInt): SexpNode =
  ## Generic constructor for SEXP data. Creates a new `SInt SexpNode`.
  result = SexpNode(kind: SInt, num: n)

proc sexp*(n: float): SexpNode =
  ## Generic constructor for SEXP data. Creates a new `SFloat SexpNode`.
  result = SexpNode(kind: SFloat, fnum: n)

proc sexp*(b: bool): SexpNode =
  ## Generic constructor for SEXP data. Creates a new `SSymbol
  ## SexpNode` with value t or `SNil SexpNode`.
  ##
  ## Future Direction: the t/nil behaviour may change unless required by EPC
  if b:
    result = SexpNode(kind: SSymbol, symbol: "t")
  else:
    result = SexpNode(kind: SNil)

proc sexp*(elements: openArray[SexpNode]): SexpNode =
  ## Generic constructor for SEXP data. Creates a new `SList SexpNode`
  result = SexpNode(kind: SList)
  newSeq(result.elems, elements.len)
  for i, p in pairs(elements): result.elems[i] = p

proc sexp*(s: SexpNode): SexpNode =
  result = s

proc toSexp(x: NimNode): NimNode {.compileTime.} =
  case x.kind
  of nnkBracket:
    result = newNimNode(nnkBracket)
    for i in 0 ..< x.len:
      result.add(toSexp(x[i]))
    result = prefix(result, "sexp")
  of nnkExprEqExpr:
    result = prefix(nnkTupleConstr.newTree(
      newLit(x[0].strVal()), toSexp(x[1])), "sexp")
  else:
    result = prefix(x, "sexp")

macro convertSexp*(x: untyped): untyped =
  ## Convert an expression to a SexpNode directly, without having to specify
  ## `%` for every element.
  result = toSexp(x)

proc `==`* (a, b: SexpNode): bool {.noSideEffect.} =
  ## Check two nodes for equality
  if a.isNil:
    b.isNil
  elif b.isNil or a.kind != b.kind:
    false
  else:
    case a.kind
    of SString:
      a.str == b.str
    of SInt:
      a.num == b.num
    of SFloat:
      a.fnum == b.fnum
    of SNil:
      true
    of SList:
      a.elems == b.elems
    of SSymbol:
      a.symbol == b.symbol
    of SKeyword:
      a.key == b.key and a.value == b.value
    of SCons:
      a.car == b.car and a.cdr == b.cdr

proc hash* (n:SexpNode): Hash =
  ## Compute the hash for a SEXP node
  case n.kind
  of SList:
    result = hash(n.elems)
  of SInt:
    result = hash(n.num)
  of SFloat:
    result = hash(n.fnum)
  of SString:
    result = hash(n.str)
  of SNil:
    result = hash(0)
  of SSymbol:
    result = hash(n.symbol)
  of SKeyword:
    result = hash(n.key) !& hash(n.value)
  of SCons:
    result = hash(n.car) !& hash(n.cdr)

proc len*(n: SexpNode): int =
  ## If `n` is a `SList`, it returns the number of elements.
  ## If `n` is a `JObject`, it returns the number of pairs.
  ## Else it returns 0.
  case n.kind
  of SList: result = n.elems.len
  else: discard

proc `[]`*(node: SexpNode, index: int): SexpNode =
  ## Gets the node at `index` in a List. Result is undefined if `index`
  ## is out of bounds
  assert(not isNil(node))
  assert(node.kind == SList)
  return node.elems[index]

proc add*(father, child: SexpNode) =
  ## Adds `child` to a SList node `father`.
  assert father.kind == SList
  father.elems.add(child)

proc addField*(node: SexpNode, name: string, value: SexpNode) =
  ## Add `:name value` keyword pair to the `node`
  node.add(newSKeyword(name, value))

proc getField*(
    node: SexpNode, name: string, default: SexpNode = nil
  ): SexpNode =
  ## Iterate over direct subnodes of `node`, searching for the SKeyword
  ## with name set to `name`. If found return it's `.value`, otherwise
  ## return `default`
  for field in node.elems:
    if field.kind == SKeyword and field.getKey() == name:
      return field.value

# ------------- pretty printing ----------------------------------------------

proc indent(s: var string, i: int) =
  s.add(spaces(i))

proc newIndent(curr, indent: int, ml: bool): int =
  if ml: return curr + indent
  else: return indent

proc nl(s: var string, ml: bool) =
  if ml: s.add("\n")

proc escapeJson*(s: string): string =
  ## Converts a string `s` to its JSON representation.
  result = newStringOfCap(s.len + s.len shr 3)
  result.add("\"")
  for x in runes(s):
    var r = int(x)
    if r >= 32 and r <= 127:
      var c = chr(r)
      case c
      of '"': result.add("\\\"") #" # gotta fix nim-mode
      of '\\': result.add("\\\\")
      else: result.add(c)
    else:
      result.add("\\u")
      result.add(toHex(r, 4))
  result.add("\"")

proc copy*(p: SexpNode): SexpNode =
  ## Performs a deep copy of `a`.
  case p.kind
  of SString:
    result = newSString(p.str)
  of SInt:
    result = newSInt(p.num)
  of SFloat:
    result = newSFloat(p.fnum)
  of SNil:
    result = newSNil()
  of SSymbol:
    result = newSSymbol(p.symbol)
  of SList:
    result = newSList()
    for i in items(p.elems):
      result.elems.add(copy(i))
  of SKeyword:
    result = newSKeyword(p.key, copy(p.value))
  of SCons:
    result = newSCons(copy(p.car), copy(p.cdr))

proc toPretty(result: var string, node: SexpNode, indent = 2, ml = true,
              lstArr = false, currIndent = 0) =
  case node.kind
  of SString:
    if lstArr: result.indent(currIndent)
    result.add(escapeJson(node.str))
  of SInt:
    if lstArr: result.indent(currIndent)
    result.addInt(node.num)
  of SFloat:
    if lstArr: result.indent(currIndent)
    result.addFloat(node.fnum)
  of SNil:
    if lstArr: result.indent(currIndent)
    result.add("nil")
  of SSymbol:
    if lstArr: result.indent(currIndent)
    result.add(node.symbol)
  of SList:
    if lstArr: result.indent(currIndent)
    if len(node.elems) != 0:
      result.add("(")
      result.nl(ml)
      for i in 0..len(node.elems)-1:
        if i > 0:
          result.add(" ")
          result.nl(ml) # New Line
        toPretty(result, node.elems[i], indent, ml,
            true, newIndent(currIndent, indent, ml))
      result.nl(ml)
      result.indent(currIndent)
      result.add(")")
    else: result.add("nil")
  of SKeyword:
    if lstArr: result.indent(currIndent)
    result.add ":"
    result.add node.key
    result.add " "
    toPretty(result, node.value, indent, ml,
        true, newIndent(currIndent, indent, ml))
  of SCons:
    if lstArr: result.indent(currIndent)
    result.add("(")
    toPretty(result, node.car, indent, ml,
        true, newIndent(currIndent, indent, ml))
    result.add(" . ")
    toPretty(result, node.cdr, indent, ml,
        true, newIndent(currIndent, indent, ml))
    result.add(")")

proc pretty*(node: SexpNode, indent = 2): string =
  ## Converts `node` to its S-Expression representation, with indentation and
  ## on multiple lines.
  result = ""
  toPretty(result, node, indent)

proc `$`*(node: SexpNode): string =
  ## Converts `node` to its s-expression representation on one line.
  result = ""
  toPretty(result, node, 0, false)

iterator items*(node: SexpNode): SexpNode =
  ## Iterator for the items of `node`. `node` has to be a SList.
  assert node.kind == SList
  for i in items(node.elems):
    yield i

iterator pairs*(node: SexpNode): (int, SexpNode) =
  ## Iterator for the pairs of `node`. `node` has to be a SList.
  assert node.kind == SList
  for i in pairs(node.elems):
    yield i

iterator mitems*(node: var SexpNode): var SexpNode =
  ## Iterator for the items of `node`. `node` has to be a SList. Items can be
  ## modified.
  assert node.kind == SList
  for i in mitems(node.elems):
    yield i

iterator mpairs*(node: var SexpNode): (int, var SexpNode) =
  ## Iterator for the pairs of `node`. `node` has to be a SList. Items can be
  ## modified.
  assert node.kind == SList
  for i, node in mpairs(node.elems):
    yield (i, node)

proc treeRepr*(node: SexpNode): string =
  ## Generate uncolored tree repr string for the S-Expression AST.
  proc aux(node: SexpNode, level: int, res: var string) =
    res.add repeat("  ", level)
    res.add $node.kind
    case node.kind
    of SInt:
      res.add " "
      res.add $node.getNum()
    of SFloat:
      res.add " "
      res.add $node.getFNum()
    of SString:
      res.add " \""
      res.add node.getStr()
      res.add "\""
    of SList:
      for item in node:
        res.add "\n"
        aux(item, level + 1, res)
    of SKeyword:
      res.add " :"
      res.add node.key
      res.add "\n"
      aux(node.value, level + 1, res)
    of SNil:
      res.add " null"
    of SSymbol:
      res.add " " & node.symbol
    of SCons:
      res.add "\n" & repeat("  ", level + 1) & "car"
      aux(node.car, level + 2, res)
      res.add "\n" & repeat("  ", level + 1) & "cdr"
      aux(node.cdr, level + 2, res)

  aux(node, 0, result)

proc eat(p: var SexpParser, tok: TTokKind) =
  if p.isTok(tok): discard getTok(p)
  else: raiseParseErr(p, tokToStr[tok])

proc parseSexp(p: var SexpParser): SexpNode =
  ## Parses S-Expression from the given S-Expression Parser `p`.
  p.space()
  case p.currToken
  of tkString:
    result = newSStringMove(captureCurrString(p))
    discard getTok(p)
  of tkInt:
    result = newSInt(parseBiggestInt(p.currString))
    discard getTok(p)
  of tkFloat:
    result = newSFloat(parseFloat(p.currString))
    discard getTok(p)
  of tkNil:
    result = newSNil()
    discard getTok(p)
  of tkSymbol:
    result = newSSymbolMove(captureCurrString(p))
    discard getTok(p)
  of tkParensLe:
    result = newSList()
    discard getTok(p)
    p.space()         # remove spaces in case of empty list such as: '(   )',
                      # or the `parseSexp` loop below errors on `tkParensRi`.

    while p.currToken notin {tkParensRi, tkDot}:
      result.add(parseSexp(p))
      # Account for possible space in the list elements.
      p.space()

    if p.currToken == tkDot:
      eat(p, tkDot)
      eat(p, tkSpace)
      result.add(parseSexp(p))
      result = newSCons(result[0], result[1])
    eat(p, tkParensRi)
  of tkKeyword:
    # `:key (value)`
    let key = p.currString[1 .. ^1]
    discard getTok(p)

    eat(p, tkSpace)
    result = newSKeyword(key, parseSexp(p))
  of tkSpace, tkDot, tkError, tkParensRi, tkEof:
    raiseParseErr(p,
      "':key', '(', 'symbol', '<float>', '<string>' or '<int>'")

proc parseSexp*(s: Stream): SexpNode =
  ## Parses from a buffer `s` into a `SexpNode`.
  var p: SexpParser
  p.open(s)
  discard getTok(p) # read first token
  result = p.parseSexp()
  p.close()

proc parseSexp*(buffer: string): SexpNode =
  ## Parses an s-expression from `buffer`.
  result = parseSexp(newStringStream(buffer))

when isMainModule:
  # These tests have been replicated and largely superceded in `tsexp` unit
  # tests. After some confirmation I think it's reasonable to remove these.

  discard parseSexp("(\"foo\")")
  let testSexp = parseSexp("""(1 (98 2) nil (2) foobar "foo" 9.234)""")
  doAssert testSexp[0].getNum == 1
  doAssert testSexp[1][0].getNum == 98
  doAssert testSexp[2].getElems == @[]
  doAssert testSexp[4].getSymbol == "foobar"
  doAssert testSexp[5].getStr == "foo"

  let alist = parseSexp("""((1 . 2) (2 . "foo"))""")
  doAssert alist[0].getCons.car.getNum == 1
  doAssert alist[0].getCons.cdr.getNum == 2
  doAssert alist[1].getCons.cdr.getStr == "foo"

  # Generator:
  var j = convertSexp([true, false, "foobar", [1, 2, "baz"]])
  doAssert $j == """(t nil "foobar" (1 2 "baz"))"""

  doAssert $convertSexp([key = "value"]) == "(:key \"value\")"
  doAssert $convertSexp([k1 = 1, k2 = 3, "k3"]) == "(:k1 1 :k2 3 \"k3\")"
  doAssert $parseSexp("(:key val)") == "(:key val)"
  doAssert $parseSexp("(:key\n\n\nval)") == "(:key val)"
  doAssert $parseSexp("(:key\n\n\nval  )") == "(:key val)"
  doAssert $parseSexp("(Sem:ExpandMacro :expression (___) :original (___))") ==
    "(Sem:ExpandMacro :expression (___) :original (___))"

  doAssert $parseSexp("(> 12 2)") == "(> 12 2)"
  doAssert $parseSexp("((12) 2 (3))") == "((12) 2 (3))"

  block:
    # Spaces between list elements can be optional
    let node = parseSexp("((tt)(tt))")
    doAssert $node == "((tt) (tt))", $treeRepr(node)

  block:
    # Apparently S-expression parser is having some mild troubles with
    # expressions that are longer than a lexer buffer length, so its
    # ability to parse things need to be checked as well.
    for withNl in [true, false]:
      let count = 128
      let size = 64
      let text = "(" & repeat("(" & (
        if withNl: "\n" else: ""
      )  & repeat("t", size) & (
        if withNl: "\n" else: ""
      ) & ")", count) & ")"

      let node = parseSexp(text)
      doAssert node.len == count, $node
