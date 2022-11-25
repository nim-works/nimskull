discard """
  description: '''
    . From https://github.com/nim-lang/Nim/issues/9177
      tuple destructuring and reassignment
    . For performance reasons, there are no creation of a temporary tuple
    . Checking if the RHS of an assignment depends on the value of the LHS
      may also be useful in order to perform this transformation only
      when/if needed.
'''
"""

block:
  var x = (a: 5, b: 1)
  x = (3 * x.a + 2 * x.b, x.a + x.b)
  doAssert x.a == 17
  doAssert x.b == 6
block:
  # Transformation of a tuple constructor with named arguments
  var x = (a: 5, b: 1)
  x = (a: 3 * x.a + 2 * x.b, b: x.a + x.b)
  doAssert x.a == 17
  doAssert x.b == 6