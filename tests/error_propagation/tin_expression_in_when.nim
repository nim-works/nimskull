discard """
  errormsg: "undeclared identifier: 'missing'"
"""

var x = missing
when x is int:
  discard
