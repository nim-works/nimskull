discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/8914
    [Regression] JS: seq delete not working
    '''
"""

var x = @[42,4242]
x.delete(1)
doAssert x == @[42]
x.insert(24)
doAssert x == @[24, 42]

