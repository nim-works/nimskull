discard """
  targets: "c cpp"
  outputsub: "value out of range: 50 notin 0 .. 40 [RangeDefect]"
  exitcode: "1"
"""

# xxx: this should work in JS but the exception output differs too much

type
  TRange = range[0..40]

proc p(r: TRange) =
  discard

var
  r: TRange
  y = 50
r = y

#p y
