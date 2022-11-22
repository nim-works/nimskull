discard """
  errormsg: "cannot instantiate: \'T\'"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/8270
      SIGSEGV on pragma with array of generic proc
    . Fixed by https://github.com/nim-lang/Nim/pull/8279
      Do not crash while instantiating a generic outside a call.
  '''
"""

proc m[T](x: T): int = discard
echo [m]

