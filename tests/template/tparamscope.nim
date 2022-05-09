discard """
  targets: native
  errormsg: "undeclared identifier: 'a'"
  line: 11
"""


template secondArg(a, b: typed): untyped =
  b

echo secondArg((var a = 1; 1), a)
