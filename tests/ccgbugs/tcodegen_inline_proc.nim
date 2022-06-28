discard """
labels: "codegen generic inline pragma proc"
description: '''
  . From https://github.com/nim-lang/Nim/issues/5345
    proc with inline pragma passed to generic proc leads to bad codegen
'''
"""

proc compareZero(d: int): bool {.inline.} = d > 0

proc apply[C](fn: C, arg: int) =
  doAssert fn(arg) == true
  
apply(compareZero, 10)