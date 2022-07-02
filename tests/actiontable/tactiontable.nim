discard """
labels: "table var const"
"""

import tables

proc action1(arg: string) : string =
  return "action 1 " & arg

proc action2(arg: string) : string =
  return "action 2 " & arg

proc action3(arg: string) : string =
  return "action 3 " & arg

proc action4(arg: string) : string =
  return "action 4 " & arg

var
  actionTable1 = {
    "A": action1,
    "B": action2,
    "C": action3,
    "D": action4}.toTable

const
  actionTable2 = {
    "A": action1,
    "B": action2,
    "C": action3,
    "D": action4}.toTable

doAssert actionTable1["C"]("arg") == "action 3 arg"
doAssert actionTable2["C"]("arg") == "action 3 arg"
