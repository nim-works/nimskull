discard """
  description: '''
    Moving an argument type constraint into a separate generic parameter causes
    and "ambiguous overload" error.
  '''
  knownIssue: '''
    generic parameter with alternate constraint is ambiguous, see:
    https://github.com/nim-lang/Nim/issues/6840
  '''
"""


block:
  proc impl[T](x: T): int = 2
  proc impl[T: int16](x: T): int = 3

  doAssert impl(1'i8) == 2
  doAssert impl(1'i16) == 3

block:
  proc impl[T](x: T): int = 2
  proc impl(x: int16 | int16): int = 3

  doAssert impl(1'i8) == 2
  doAssert impl(1'i16) == 3

block:
  proc impl[T](x: T): int = 2
  ## Should be identical to writing `T: A | A`
  proc impl[T: int16 | int16](x: T): int = 3

  doAssert impl(1'i8) == 2
  doAssert impl(1'i16) == 3
