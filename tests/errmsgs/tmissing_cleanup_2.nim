discard """
  errormsg: "cannot tail call; local 'x' requires cleanup"
  line: 7
"""

proc test(x: sink string) {.tailcall.} =
  test("")

test("")
