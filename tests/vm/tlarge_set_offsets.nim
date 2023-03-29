discard """
  targets: vm
  labels: "set"
  description: '''
    Regression test for sets where the first possible element has
    a large offset from zero
  '''
"""

template rangeSet(a, b: static int): typedesc =
  set[range[a..b]]

template testCase(name: untyped, r: static Slice[int], val: static int) =
  block name:
    var s: rangeSet(r.a, r.b) = {}
    s.incl val
    {.line.}: doAssert val in s

    s.excl val
    # test with a run-time value. In order to prevent the optimizer from
    # replacing it with a constant, it's returned by a procedure
    proc get(): int {.noinline.} = val

    var tmp = get()
    s.incl tmp
    {.line.}: doAssert tmp in s

# test for the case where the offset is representable with 7 bits
testCase small_offset, 20..30, 25

block abi_edge:
  # make sure the right encoding is chosen if the offset value is either -128,
  # -127, 127, or 128 (at the edge of what the ABI encoding supports)
  testCase _, -128..0, -10
  testCase _, -127..0, -10
  testCase _, 127..256, 150
  testCase _, 128..256, 150

testCase less_than_16bit, 10_000..11_000, 10_500
testCase greater_than_16bit, 100_000..100_003, 100_002
testCase greater_than_32bit, int(high(int64)-4)..int(high(int64)), int(high(int64)-2)