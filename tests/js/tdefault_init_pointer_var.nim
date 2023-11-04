discard """
  description: '''
    Regression test for wrong code being generated for locals of `pointer`
    type
  '''
"""

proc test(): pointer =
  # the result variable was affected too
  var x: pointer
  doAssert x == nil # this assertion failed

doAssert test() == nil # this one too