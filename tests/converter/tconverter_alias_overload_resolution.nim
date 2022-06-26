discard """
description:  '''
  . From https://github.com/nim-lang/Nim/issues/9165
    Converter is applied to the first parameter of operator instead of last
  . Your []= definition doesn't overload the []= from lib/system.nim.
    During the overload resolution the alias Slice[int] is not considered
    equivalent to HSlice[int, int]. This is not intended behavior.
  . Your s[1..2] = "3" matches the signature of []= in lib/system.nim because
    you have the converter toString and because Slice[int] is an alias for and
    therefore implicitly convertible to HSlice[int, int], But your
    toString returns an immutable value.
  '''
"""
type ustring = distinct string

converter toUString(s: string): ustring = ustring(s)
converter toString(s: ustring): string = string(s)

proc `[]=`*(s: var ustring, slice: Slice[int], replacement: ustring) {.inline.} =
  s = replacement

var s = ustring("123")
s[1..2] = "3"
doAssert s == "3"