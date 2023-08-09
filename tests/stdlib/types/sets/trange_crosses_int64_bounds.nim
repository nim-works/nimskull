discard """
  targets: "c vm js"
  description: '''
    Test the `incl`, `contains`, and set construction operation for sets where
    the element range crosses the signed 64-bit integer upper bound
  '''
  knownIssue.js: '''
    sets with sets with elements beyond 2^53-1 don't work the JavaScript
    backend
  '''
"""

const
  Low  = uint64(high(int64))
  High = Low + 3

type
  Range   = range[Low .. High]
  SetType = set[Range]

const
  a = Range(Low + 0)
  b = Range(Low + 1)
  c = Range(Low + 2)
  d = Range(Low + 3)

var
  s: SetType
  val: Range

# test `incl` and `contains` and set construction with a constant value that
# is still in the int64 range
s.incl a
doAssert s.contains(a)
doAssert s == SetType({ a })
doAssert card(s) == 1

# now test with a constant value that's outside of the int64 range
s.incl d
doAssert d in s
doAssert s == SetType({ a, d })
doAssert card(s) == 2

s = {}

# test with a run-time value that's in the int64 range
val = a
s.incl val
doAssert s.contains(a)
doAssert s.contains(val)
doAssert s == SetType({ a }) # check that the constant set agrees
doAssert s == SetType({ val })
doAssert card(s) == 1

# now test with a run-time value that's outside int64 range
val = d
s.incl val
doAssert s.contains(d)
doAssert s.contains(val)
doAssert s == SetType({ a, d })
doAssert s == SetType({ a, val })