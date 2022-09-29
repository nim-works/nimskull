discard """
  target: c
"""

block:
  var a = [1, 2]
  doAssert toOpenArray(a, 0, 0).len == 1