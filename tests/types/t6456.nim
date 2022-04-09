discard """
  targets: native
  errormsg: "type \'ptr void\' is not allowed"
  line: 7
"""

proc foo(x: ptr void) =
  discard
