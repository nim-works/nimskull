discard """
description: "Tests for the sexp module"
joinable: false
"""

import experimental/sexp
import std/unittest
from std/strutils import repeat

# Parsing

suite "parse s-expressions - empty/invalid strings":
  test "parse empty raises an error":
    expect(SexpParsingError):
      discard parseSexp("")

  test "parse empty, blank space only strings raises an error":
    expect(SexpParsingError):
      discard parseSexp(" ")

    expect(SexpParsingError):
      discard parseSexp("   ")

    expect(SexpParsingError):
      discard parseSexp("\n")

  test "parse dot '.' only raises an error":
    expect(SexpParsingError):
      discard parseSexp(".")

    expect(SexpParsingError):
      discard parseSexp("..")

  test "parse close/right paren ')', without an open/left paran is an error":
    expect(SexpParsingError):
      discard parseSexp(")")

    expect(SexpParsingError):
      discard parseSexp(")(")

suite "parse s-expressions - string atoms":
  test "an empty string atom, is two double quotes":
    let parsed = parseSexp("\"\"")
    check parsed == newSString("")

  test "a string atom is any characters surrounded by double quotes '\"'":
    let parsed = parseSexp("\"string\"")
    check parsed == newSString("string")

  test "a string atom can have spaces within":
    let parsed = parseSexp("\"this is a string\"")
    check parsed == newSString("this is a string")

  test "you can even have double quote characters through escaping '\\\"'":
    let parsed = parseSexp("\"look at me \\\"escaping\\\"\"")
    check parsed == newSString("look at me \"escaping\"")

suite "parse s-expressions - numeric atoms":
  test "integers are parsed into an integer cell":
    # xxx: not sure if BiggestInt should be part of the spec
    let parsed = parseSexp("0")
    check parsed == newSInt(0)
  
  test "integers can be negative":
    let parsed = parseSexp("-1")
    check parsed == newSInt(-1)

  test "integers or explicitly positive":
    let parsed = parseSexp("+1")
    check parsed == newSInt(1)

  test "floats are parsed into float cell":
    let parsed = parseSexp("0.0")
    check parsed == newSFloat(0.0)

  test "floats can start with a '.' dropping the need for a leading '0'":
    let parsed = parseSexp(".0")
    check parsed == newSFloat(0.0)

  test "floats can be negative numbers":
    let parsed = parseSexp("-.1")
    check parsed == newSFloat(-0.1)

suite "parse s-expressions - symbol atoms":
  test "symbols unquoted character strings, uninterrupted by space or parens":
    let parsed = parseSexp("test")
    check parsed == newSSymbol("test")

  test "symbols can have hyphens, underscores, and other punctuation":
    let parsed = parseSexp("with-hyphens_and|other!things,too?")
    check parsed == newSSymbol("with-hyphens_and|other!things,too?")

  test "symbols can be single punctuation marks":
      let parsed = parseSexp("(> 12 2)")
      check $parsed == "(> 12 2)"

  test "with escaping a dot can be a symbol":
    let parsed = parseSexp("\\.")
    skip()
    when false: # not supported at present, unsure if we should
      check parsed.kind == SSymbol
      check parsed == newSSymbol(".")

suite "parse s-expressions - keyword atoms":
  # Note: might remove keywords or at least drop the key/value requirement
  test "keywords are symbols prefixed with a ':', followed by a sexp":
    let expected = newSList(newSKeyword("key", newSSymbol("val")))
    check parseSexp("(:key val)") == expected
    check parseSexp("(:key\n\n\nval)") == expected
    check parseSexp("(:key\n\n\nval  )") == expected
    check parseSexp("(Sem:ExpandMacro :expression (___) :original (___))") ==
            newSList(newSSymbol("Sem:ExpandMacro"),
                     newSKeyword("expression", newSList(newSSymbol("___"))),
                     newSKeyword("original", newSList(newSSymbol("___"))))

suite "parse s-expression - nil atom":
  test "nil s-expression is end of list, or a sort of empty list":
    let parsed = parseSexp("nil")
    check parsed == newSNil()

suite "parse s-expressions - cons":
  test "cons are created with a '.' and s-exprs on either side":
    let parsed = parseSexp("(left . right)")
    check parsed == newSCons(newSSymbol("left"), newSSymbol("right"))

suite "parse s-expressions - lists":
  test "an empty list is two parens":
    let parsed = parseSexp("()")
    check parsed == newSList()

  test "spaces in an empty list don't matter":
    let parsed = parseSexp("(     )")
    check parsed == newSList()
  
  test "spaces between lists don't matter":
    let parsed = parseSexp("((1)(2))")
    check parsed == newSList(newSList(newSInt(1)), newSList(newSInt(2)))

  test "lists don't have to start with symbols, this isn't lisp":
    let parsed = parseSexp("((12) 2 (3))")
    check parsed == newSList(newSList(newSInt(12)),
                             newSInt(2),
                             newSList(newSInt(3)))

# SexpNode API - Traversing, Querying, Generating, Transforming, and Printing

suite "working with parsed SexpNode output":
  test "traversing and querying a SexpNode tree":
    let parsed = parseSexp("""(1 (98 2) nil (2) foobar "foo" 9.234)""")
    check parsed[0].getNum == 1
    check parsed[1][0].getNum == 98
    check parsed[2].getElems == newSeq[SexpNode]()
    check parsed[4].getSymbol == "foobar"
    check parsed[5].getStr == "foo"

  test "working with cons":
    let parsed = parseSexp("""((1 . 2) (2 . "foo"))""")
    check parsed[0].getCons.car.getNum == 1
    check parsed[0].getCons.cdr.getNum == 2
    check parsed[1].getCons.cdr.getStr == "foo"

suite "basic to string conversion":
  test "`$` converts to string":
    let parsed = parseSexp("(1 . 2)")
    check $parsed == "(1 . 2)"

  test "an empty list is printed as nil":
    let parsed = parseSexp("()")
    check $parsed == "nil"
  
  test "extra spaces are not preserved":
    let parsed = parseSexp("(:key \n   \n val)")
    check $parsed == "(:key val)"

suite "generating SexpNode from data":
  test "convertSexp takes syntax to sexp representation":
    let generated = convertSexp([true, false, "foobar", [1, 2, "baz"]])
    check $generated == """(t nil "foobar" (1 2 "baz"))"""

  test "convertSexp takes literal bool syntax to SexpNode":
    for b in [true, false]:
      let
        generated = convertSexp(b)
        expected = if b: "t" else: "nil"
      check $generated == expected

  test "convertSexp takes literal integer syntax to SexpNode":
    let generated = convertSexp(-11)
    check $generated == "-11"

  test "convertSexp takes literal float syntax to SexpNode":
    let generated = convertSexp(-1.1)
    check $generated == "-1.1"

  test "convertSexp takes string syntax to SexpNode":
    let generated = convertSexp("string")
    check $generated == "\"string\""

  test "convertSexp can also handle key value maps":
    check $convertSexp([key = "value"]) == "(:key \"value\")"
    check $convertSexp([k1 = 1, k2 = 3, "k3"]) == "(:k1 1 :k2 3 \"k3\")"

# Miscellaneous

suite "Regression Test suite":
  test "ensure parsing works when input exceeds the lexer buffer":
    # Apparently S-expression parser is having some mild troubles with
    # expressions that are longer than a lexer buffer length, so its
    # ability to parse things need to be checked as well.
    for withNl in [true, false]:
      let
        count = 128
        size = 64
        nl = if withNl: "\n" else: ""
        item = "(" & nl & repeat("t", size) & nl & ")"
        items = "(" & repeat(item, count) & ")"
        node = parseSexp(items)
        nodeCount = node.len
      check nodeCount == count
      # checkpoint $node