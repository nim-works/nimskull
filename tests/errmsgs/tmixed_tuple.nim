discard """
  errormsg: "named expression not allowed here"
  line: 7
"""

var x = 0
discard (x, a: x)
