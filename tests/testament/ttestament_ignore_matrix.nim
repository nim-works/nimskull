discard """
  matrix:"-d:nimTest_t16576"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/16576
      Testament can silently ignore matrix spec
  '''
"""

# bug #16576
doAssert defined(nimTest_t16576)
doAssert not defined(nimMegatest)

