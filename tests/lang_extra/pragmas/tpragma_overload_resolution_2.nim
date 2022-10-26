discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/4384
    Macros not subjected to overload resolution when used as pragmas
  . Current Output
    test.nim(3, 1) Error: expression 'proc foo() {..}' has no type (or is ambiguous)
  . Change testMacro definition order, and it works.
'''
"""

macro testMacro(body: untyped): untyped = discard
macro testMacro(s: string, body: untyped): untyped = discard
proc foo() {.testMacro: "foo".} = discard

