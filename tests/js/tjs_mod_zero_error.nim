discard """
description : '''
  . From https://github.com/nim-lang/Nim/issues/7127
    1 mod 0 gives NaN with JS backend
  . Since mod is mapped to the % JavaScript operator, 1 mod 0 gives NaN
    instead of raising an DivByZeroError exception.
  . It works since Nim v1.4
'''
"""

doAssertRaises(DivByZeroDefect): discard 1 mod 0
doAssertRaises(DivByZeroDefect): discard 1 div 0
