discard """
  description: "Both procs and funcs can use the .tailcall calling convention"
"""

proc a(): int {.tailcall.} = 1
func b(): int {.tailcall.} = 2

doAssert a() == 1
doAssert b() == 2
