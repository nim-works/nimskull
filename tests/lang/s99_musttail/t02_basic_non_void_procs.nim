discard """
  description: "Both procs and funcs can use the .musttail calling convention"
"""

proc a(): int {.musttail.} = 1
func b(): int {.musttail.} = 2

doAssert a() == 1
doAssert b() == 2
