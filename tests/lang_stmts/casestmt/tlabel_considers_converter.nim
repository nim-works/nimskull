discard """
  description: '''
    Ensure that converters are considered with each 'of'-branch label syntax.
  '''
"""

converter f2i(x: float): int8 =
  int8(x)

proc test(x: int8): int =
  case x
  of 1.0:
    1
  of 2.2 .. 3.3: # with range syntax: 2 .. 3
    2
  of [4.1, 6.1]: # with array constructor syntax: 4, 6
    3
  of @[10.1] & @[12.1]: # with compile-time-evaluated sequence: 10, 12
    4
  else:
    5

doAssert test(1) == 1
doAssert test(2) == 2
doAssert test(3) == 2
doAssert test(4) == 3
doAssert test(6) == 3
doAssert test(10) == 4
doAssert test(12) == 4
doAssert test(11) == 5 # covered by the else branch
