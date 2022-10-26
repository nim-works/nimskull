discard """
  targets: "js"
description: '''
  . From https://github.com/nim-lang/Nim/issues/7249
    Javascript Generation
  . When obj is global, it's a "fat pointer":
    so it's an array with one element which is the actual object.
  . However the for loop is for (.. in a_) so it iterates
    through the array instead through it's [0].
  . If it's in a function, it works correctly, but because for some reason a
    is just the object instead of a fat pointer (which I am not sure why)
  . Fixed by Automatic deref for everything but pointers in asm/emit
     https://github.com/nim-lang/Nim/pull/8891
'''
"""

import jsffi
proc testInsideProc =
  var obj = JsAssoc[cstring, int]{a: 2}
  for key, value in obj:
     doAssert key == "a"
     doAssert value == 2
#


testInsideProc()
var obj = JsAssoc[cstring, int]{a: 2}
for key, value in obj:
    doAssert key == "a"
    doAssert value == 2




