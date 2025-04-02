discard """
  errormsg: "cannot tail call; local 'x' requires cleanup"
  line: 8
"""

proc test(i: int) {.tailcall.} =
  var x = $i
  test(i)

test(1)
