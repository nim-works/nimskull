discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/12303
    small nimCopy jsgen bug: field is not copied to
  . Current Output: undefined
    Expected output: {b: 2}
  . I think `cast` object may not be allowed in JS backend.
'''
"""

import jsconsole, jsffi

type
  A = ref object
   b: B

  B = object
    b: int


let obj = js{}
var a = cast[A](obj)
a.b = B(b: 2)

doAssert a.b == B( b: 2)

