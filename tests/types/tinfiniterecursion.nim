discard """
  targets: native
  errormsg: "illegal recursion in type 'XIM'"
  line: 9
"""

type
  XIM* = ptr XIM
  XIMProc* = proc (a2: XIM)
