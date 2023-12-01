discard """
  description: '''
    Regression test for where assigning `default(T)` (where `T` maps to a fat
    pointer) resulted in an internal compiler error
  '''
"""

proc test() =
  # test that the intial assignment works
  var x = default(pointer)
  doAssert x == nil

  # test that a normal assignment works
  x = default(pointer)
  doAssert x == nil

test()