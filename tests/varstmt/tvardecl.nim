discard """
description: "Test the new variable declaration syntax"
"""

var
  x = 0
  s = "Hallo"
  a, b: int = 4

doAssert a == 4
doAssert b == 4
