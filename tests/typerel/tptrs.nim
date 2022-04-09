discard """
  targets: native
  errormsg: "type mismatch: got <ptr int16> but expected 'ptr int'"
  line: 9
"""

var
  n: int16
  p: ptr int = addr n
