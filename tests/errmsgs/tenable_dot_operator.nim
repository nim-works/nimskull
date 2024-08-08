discard """
  errormsg: '''
    the overloaded . operator has to be enabled with {.experimental: "dotOperators".}
  '''
  line: 8
"""

proc `.`(a: int) = discard
