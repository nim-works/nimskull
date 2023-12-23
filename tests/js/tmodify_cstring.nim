discard """
  errormsg: "cstring doesn't support `[]=` operator"
  knownIssue: '''
    The JS code generator relies on tree shapes that aren't guaranteed
  '''
"""

var x = cstring"abcd"
x[0] = 'x'
