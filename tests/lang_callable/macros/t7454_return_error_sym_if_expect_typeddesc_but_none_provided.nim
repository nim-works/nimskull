discard """
errormsg: "expression has no type:"
description: '''
Return an error symbol if the macro output has not type and a typedes was
expected.
'''
line: 12
"""

macro p(t: typedesc): typedesc =
  discard
var a: p(int)
