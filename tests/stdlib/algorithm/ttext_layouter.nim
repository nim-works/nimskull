import experimental/text_layouter
import std/[strutils, sequtils, strformat]

initBlockFormatDSL()

type
  StrStore = ref object
    strings: seq[string]

proc str(store: var StrStore, str: string): LytStr =
  result = lytStrIdx(store.strings.len, str.len)
  store.strings.add str

proc str(s: StrStore, str: LytStr): string =
  if str.isSpaces():
    repeat(" ", str.len)

  else:
    s.strings[str.id.toIndex()]

proc toString(s: StrStore, opts: LytOptions, blc: LytBlock): string =
  let lyt = blc.toLayout(opts)
  for event in formatEvents(lyt):
    case event.kind:
      of layEvNewline:
        result.add "\n"

      of layEvSpaces:
        result.add repeat(" ", event.spaces)

      of layEvStr:
        result.add s.str(event.str)

  var res: seq[string]
  for line in splitLines(result):
    res.add strip(line, leading = false)

  return res.join("\n")

var s = StrStore()


proc toString(b: LytBlock, width: int = 80): string =
  var opts = initLytOptions()
  opts.rightMargin = width
  toString(s, opts, b)

proc `$`(o: LytOptions, b: LytBlock): string = toString(s, o, b)
proc `$`(b: LytBlock, width: int = 80): string = toString(b, width)

proc toLyt(b: LytBlock): Layout = b.toLayout(initLytOptions())

proc lS(str: string): LytStr = s.str(str)
proc lTX(str: string): LytBlock = lT(s.str(str))

proc lytProc(args: openArray[LytBlock], body: LytBlock): LytBlock =
  let
    h = "proc ("
    t = ") = "
    hsep = initHSeparated(@args, lTX(", "))
    vsep = initVSeparated(@args, lTX(", "))


  result = lC(
    lH(lTX(h), hsep, lTX(t), body),
    lV(lH(lTX(h), hsep, lTX(t), lI(2, body))),
    lV(lTX(h), lI(4, vsep), lTX(t), lI(2, body))
  )



block:
  doAssert lTX("S").toString() == "S"
  doAssert $lI(4, lTX("S")) == "    S"
  doAssert $lH(lTX("A"), lTX("B")) == "AB"
  doAssert $lV(lTX("A"), lTX("B")) == "A\nB"
  doAssert $lI(2, lH(lTX("A"), lTX("B"))) == "  AB"
  doAssert $lI(2, lV(lTX("A"), lTX("B"))) == "  A\n  B"
  doAssert $lH(
    lV(lTX("A"), lTX("B")),
    lV(lTX("C"), lTX("D"))
  ) == """
A
BC
 D"""

  doAssert $lC(lTX("123456"), lTX("123")) == "123"
  doAssert $lC(lV(lTX("123"), lTX("456")), lTX("123456")) == "123456"
  var o = initLytOptions()
  o.lineBreakCost = 1
  o.rightMargin = 2
  doAssert o$lC(
    lV(lTX("12"), lTX("34"), lTX("56")),
    lTX("123456")
  ) == "12\n34\n56"

  o.lineBreakCost = 5
  doAssert o$lC(
    lV(lTX("12"), lTX("34"), lTX("56")),
    lTX("123456")
  ) == "123456"

block:
  var o = initLytOptions()
  o.rightMargin = 2
  doAssert o$lW(lS("@+"), lTX("[###]"), lTX("[###]")) == "[###]@+\n[###]"

block:
  var res: seq[string]
  for args in [1, 2]:
    for body in [20, 60]:
      let bl = lytProc(
        args = mapIt(0 ..< args, lTX(&"arg{it}: arg{it}_type")),
        body = lTX(repeat("?", body))
      )

      res.add toString(bl)

  doAssert res[0] == "proc (arg0: arg0_type) = ????????????????????"
  doAssert res[1] == """
proc (
    arg0: arg0_type
) =
  ????????????????????????????????????????????????????????????"""

  doAssert res[2] == """
proc (arg0: arg0_type, arg1: arg1_type) = ????????????????????"""

  doAssert res[3] == """
proc (
    arg0: arg0_type,
    arg1: arg1_type
) =
  ????????????????????????????????????????????????????????????"""

block:
  var res: seq[string]
  for i in [1, 5, 10]:
    var blocks = mapIt(0 .. i, lTX(&"arg{it}: int{it}"))
    let bl = lH(lTX("proc ("),
      lC(join(lH(blocks), lTX(", ")), join(lV(blocks), lTX(","))),
      lTX(")"))

    res.add toString(bl)

  doAssert res[0] == "proc (arg0: int0, arg1: int1)"
  doAssert res[1] == """
proc (arg0: int0, arg1: int1, arg2: int2, arg3: int3, arg4: int4, arg5: int5)"""
  doAssert res[2] == """
proc (arg0: int0,
      arg1: int1,
      arg2: int2,
      arg3: int3,
      arg4: int4,
      arg5: int5,
      arg6: int6,
      arg7: int7,
      arg8: int8,
      arg9: int9,
      arg10: int10)"""

let comm = s.str(", ")

block:
  let res =  toString(
  lH(lH(lTX("FnName"), lTX("(")),
  lW(comm, mapIt(1 .. 10, lTX(&"argument{it}"))),
  lTX(")")), 60)

  doAssert res == """
FnName(argument1, argument2, argument3, argument4,
       argument5, argument6, argument7, argument8,
       argument9, argument10)"""

block:
  let res = toString(
    lH(lH(lTX("FnName"), lTX("(")),
    lW(comm, mapIt(1 .. 10, lTX(&"argument{it}"))),
    lTX(")")), 30)

  doAssert res == """
FnName(argument1, argument2,
       argument3, argument4,
       argument5, argument6,
       argument7, argument8,
       argument9, argument10)"""

doAssert toString(lH(lH(lTX("AVeryLongAndDescriptiveFunctionName"), lTX("(")),
  lW(comm, mapIt(1 .. 10, lTX(&"argument{it}"))),
  lTX(")")), 50) == """
AVeryLongAndDescriptiveFunctionName(argument1,
                                    argument2,
                                    argument3,
                                    argument4,
                                    argument5,
                                    argument6,
                                    argument7,
                                    argument8,
                                    argument9,
                                    argument10)"""

block:
  let res = toString(lC(lH(lH(lTX("AVeryLongAndDescriptiveFunctionName"), lTX("(")),
    lW(comm, mapIt(1 .. 10, lTX(&"argument{it}"))),
    lTX(")")),
  lV(lH(lTX("AVeryLongAndDescriptiveFunctionName"), lTX("(")),
    lI(4, lW(comm, mapIt(1 .. 10, lTX(&"argument{it}")))),
    lTX(")"))), 50)

  doAssert res == """
AVeryLongAndDescriptiveFunctionName(
    argument1, argument2, argument3, argument4,
    argument5, argument6, argument7, argument8,
    argument9, argument10
)"""

proc getStr(str: LytStr): string = s.str(str)

block:
  let bl = lH(
    lTX("stmtPragmas* = "),
    lH(
      lTX("{ "),
      lV(
        lTX("wChecks"),
        lTX("wOverflowChecks"),
        lTX("wNilChecks")),
      lTX(" }")))

  # echo treeRepr(bl.toLyt(), getStr)
  doAssert toString(bl) == """
stmtPragmas* = { wChecks
                 wOverflowChecks
                 wNilChecks }"""

block:
  let bl = lV(lTX("stmtPragmas* = {"),
    lI(2, lV(
      lTX("wChecks,      wObjChecks,"),
      lTX("wBoundChecks, wOverflowChecks, wNilChecks")
    )),
    lTX("}"))

  # echo treeRepr(bl.toLyt(), getStr)
  doAssert toString(bl) == """
stmtPragmas* = {
  wChecks,      wObjChecks,
  wBoundChecks, wOverflowChecks, wNilChecks
}"""

block:
  let bl = lV(
    lTX("block:"),
    lI(2, lV(
      lH(lTX("var"), lS(1), lTX("i: int = 0")),
      lI(2, lTX("postInc(i)")))))

  doAssert toString(bl) == """
block:
  var i: int = 0
    postInc(i)"""
