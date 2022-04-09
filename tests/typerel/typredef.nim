discard """
  targets: native
  errormsg: "illegal recursion in type \'Uint8\'"
  file: "typredef.nim"
  line: 8
"""
type
  Uint8 = Uint8 #ERROR_MSG illegal recursion in type 'Uint8'
