discard """
errormsg: "parameter 'a' requires a type"
nimout: '''
tproc_multi_optional_param.nim(25, 14) Error: parameter 'a' requires a type'''
description: '''
  . From https://github.com/nim-lang/Nim/issues/15949
    proc main(a,b=1)=discard not documented
    (optional value applying to multiple params)
  . https://github.com/PMunch/Nim/commit/3ce926b0680ecbabf443fb5af72784f3234a9385
    Document that comma propagates the default values of parameters
'''
"""


# line 15
proc procGood(a, b = 1): (int, int) = (a, b)

doAssert procGood() == (1, 1)
doAssert procGood(b = 3) == (1, 3)
doAssert procGood(a = 2) == (2, 1)
doAssert procGood(a = 5, b = 6) == (5, 6)

# The type (and default value propagation breaks in the below example
# as semicolon is used instead of comma.
proc procBad(a; b = 1): (int, int) = (a, b)