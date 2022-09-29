discard """
  action: run
  target: c
"""

block setLen:
  var s: seq[int]
  s.setLen(1)
  doAssert s.len == 1