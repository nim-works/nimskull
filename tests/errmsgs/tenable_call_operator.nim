discard """
  errormsg: '''
    the overloaded () operator has to be enabled with {.experimental: "callOperator".}
  '''
  line: 8
"""

proc `()`(a: int) = discard
