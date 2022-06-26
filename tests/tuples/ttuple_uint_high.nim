discard """
  disabled: i386
  description: '''
    . From https://github.com/nim-lang/Nim/issues/12892
      uint.high -1 broken in tuple
    . This works as of 1.2.0
  '''

"""

template works[T](): auto = T.high - 1
template breaks[T](): auto = (T.high - 1, true)
doAssert $works[uint]() == "18446744073709551614"
doAssert $breaks[uint]() == "(18446744073709551614, true)"