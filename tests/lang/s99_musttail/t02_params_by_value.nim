discard """
  description: '''
    Pass-by-value parameters with no overriden copy behaviour can receive
    arbitrary expressions.
  '''
"""

proc p(a, b: int): int {.musttail.} =
  a + b

# arguments may be locals:
proc test(): int {.musttail.} =
  var x = 1
  var y = 2
  p(x, y)

doAssert test() == 3

# arguments may be temporaries:
proc test(a, b: int): int {.musttail.} =
  p(a * 3, b * 4)

doAssert test(1, 2) == 11

# arguments may be constants:
proc testConstants(): int {.musttail.} =
  p(1, 2)

doAssert testConstants() == 3
