discard """
  errormsg: "60 can't be converted to TRange"
  line: 20
"""

type
  TRange = range[0..40]

proc p(r: TRange) =
  discard

var
  r: TRange
  y = 50
r = y

p y

const
  myConst: TRange = 60
