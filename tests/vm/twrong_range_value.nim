discard """
  description: '''
    Regression test for the wrong value being loaded from locations of range
    type
  '''
  targets: "c js vm"
"""

# VM specific bug; tested with all targets for the sake of coverage

type Object = object
  x: range[0'u8..255'u8]

var x = Object(x: 255'u8) # all bits of `x` are set
# the value wasn't properly read from the location, resulting in the
# temporary register storing 18446744073709551615 (all bits set)
doAssert x.x == 255
