discard """
joinable: false
description: '''
 . From https://github.com/nim-lang/Nim/issues/7345
   Internal errors with newSeq when custom = operator is defined

 . From https://github.com/nim-lang/Nim/issues/7346
   Internal error when assigning seq for type that have custom = operator
 . Code snippet (compile with --newruntime)
'''
"""

# This bug could only be reproduced with --newruntime

type
  Obj = object
    a: int

proc `=`(a: var Obj, b: Obj) = discard

let a: seq[Obj] = @[] # bug #7346
let b = newSeq[Obj]() # bug #7345