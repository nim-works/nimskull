discard """
  targets: "c js vm"
  output: '''10
10
1
1
true'''
  knownIssue.vm: "`RangeDefect`s aren't catchable"
"""

# bug #1344

var expected: int
var x: range[1..10] = 10

try:
  x += 1
  echo x
except OverflowDefect, RangeDefect:
  expected += 1
  echo x

try:
  inc x
  echo x
except OverflowDefect, RangeDefect:
  expected += 1
  echo x

x = 1
try:
  x -= 1
  echo x
except OverflowDefect, RangeDefect:
  expected += 1
  echo x

try:
  dec x
  echo x
except OverflowDefect, RangeDefect:
  expected += 1
  echo x

echo expected == 4

# bug #13698
var
  x45 = "hello".cstring
  p = x45.len.int32

static:
  # test that out-of-range errors with arguments to `range` parameters can
  # be detected with ``compiles``
  proc f(x: range[1..4]) =
    discard

  doAssert not compiles(f(0))

block unsigned_64_bit_to_range_conversion:
  # regression test to make sure a uint/uint64 to integer range conversion
  # checks the target range's lower bound
  var x: uint64 = 0
  doAssertRaises RangeDefect:
    discard range[1..high(int)](x)
