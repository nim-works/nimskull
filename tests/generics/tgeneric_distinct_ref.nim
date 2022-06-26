discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/6060
    Error with distinct generic TableRef
  . Curiously, it does seem to work when Table is used instead of TableRef;
  . It has been fixed since Nim 1.0
  . Mentions https://github.com/nim-lang/Nim/issues/7165
'''
"""
import tables

type MyTab[A,B] = distinct TableRef[A,B]

proc `$`[A,B](t: MyTab[A,B]): string =
  "My special table " & $TableRef[A,B](t)

proc create[A,B](): MyTab[A,B] = MyTab(newTable[A,B]())

var a = create[int,int]()
doAssert $a == "My special table {:}"

