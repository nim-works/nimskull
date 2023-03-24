discard """
  description: '''
    Test array type inference where the first element is a ``sink`` type
  '''
"""

proc p(x: sink int) =
  var a = [x] # the array must not be of ``array[0..0, sink int]`` type
  doAssert a is array[0..0, int]
  doAssert a[0] == 1

p(1)