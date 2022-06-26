discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/3784
    Static[T] generic type cause invalid type
'''
"""

import typetraits

type
  S[N: static[int]] = object
  T[A,B: static[int]] = object
  
  C = S[1]

var
  x: T[1,1]
  y: T[C.N, C.N]

doAssert y.type.name == "T[1, 1]"


