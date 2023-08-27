discard """
  errormsg: "accessed location 'x' doesn't exist in the current compile-time context"
  line: 8
"""

proc f() =
  var x = 1
  const y = x + 1
