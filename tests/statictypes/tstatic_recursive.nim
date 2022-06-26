discard """
  errormsg: '''
type mismatch: got <proc (a0: int): string{.noSideEffect, gcsafe, locks: 0.}>
'''
  line: 18
  description: '''
  . From https://github.com/nim-lang/Nim/issues/9255
    Compiler hangs forever instead of giving CT error
  . https://github.com/LemonBoy/Nim/commit/90a706798461e66464fa74beeafb687ec829727c
    Prevent the construction of recursive tyStatic Types.
  '''
"""
macro fun(a: static float): untyped =
  discard

when true:
  proc bar(a0: int): string = discard
  fun(bar)

