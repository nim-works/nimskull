discard """
  description: '''
    Regression test for a compiler bug where incrementing an unsigned range
    resulted in an overflow defect.
  '''
  targets: "c js vm"
"""

var a: range[0'u32..high(uint32)] = high(uint32)
inc a
doAssert a == typeof(a)(0)
