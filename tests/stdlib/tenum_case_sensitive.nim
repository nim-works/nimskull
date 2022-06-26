discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/7686
    yet another cmpIgnoreStyle bug
  . strutils.parseEnum uses strutils.cmpIgnoreStyle to check
    for identifier equality.
  . Fixed in https://github.com/nim-lang/Nim/pull/14046
    Works in Nim 1.4
'''
"""

import strutils

type
  MyEnum = enum
    A,
    a

doAssert parseEnum[MyEnum]("A") == A
doAssert parseEnum[MyEnum]("a") == a

