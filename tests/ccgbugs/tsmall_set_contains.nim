discard """
  targets: "c"
  description: '''
    Ensure that no '||' chain is emitted for `contains` where the set operand
    is a small set literal
  '''
  action: compile
  ccodecheck: "\\i !@('||')"
"""

type T = range[0..7]

var elem = T(1)
doAssert contains({T(1), T(3)}, elem)