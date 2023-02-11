discard """
  errormsg: '''type mismatch: got <(typedesc[NimEdAppWindow], int)>'''
  line: 21
  description: '''error message and hint if there is a space between the
  routine name and the arguments at a call site.'''
"""

type
  NimEdAppWindow = ptr NimEdAppWindowObj
  NimEdAppWindowObj = object
    i: int

template gDefineTypeExtended*(tn: typeDesc) =
  discard

gDefineTypeExtended (NimEdAppWindow)

template xxx*(tn: typeDesc, i: int) =
  discard

xxx (NimEdAppWindow, 0)