discard """
  errormsg: "cannot tail call; temporary requires cleanup [comes from: tmissing_cleanup_3.nim(7, 8)]"
  line: 8
"""

proc test(x: int) {.tailcall.} =
  echo x
  test(x)

test(1)
