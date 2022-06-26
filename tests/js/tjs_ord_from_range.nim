discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/14570
    Can't get ord of a value of a Range type in the JS backend
  . This code compiles fine with the C backend but results in a compiler
    crash in the JS backend.
    '''
"""

type A = range[15 .. 30]

let a: A = 18

doAssert ord(a) == 18

