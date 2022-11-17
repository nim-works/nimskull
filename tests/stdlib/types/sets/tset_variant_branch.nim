discard """
  matrix: "--gc:refc; --gc:arc"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/5792
      error when a discriminator branch is constructed with certain sets
  '''
"""

type
  T = enum
    a
    b
    c
  U = object
    case k: T
    of a:
      x: int
    of {b, c} - {a}:
      y: int

doAssert U(k: b, y: 1).y == 1

