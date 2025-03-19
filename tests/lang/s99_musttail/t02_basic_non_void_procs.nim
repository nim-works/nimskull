discard """
  description: "Both procs and funcs can use the .musttail calling convention"
"""

proc a(): int {.musttail.} = 1
proc test_a(): int = a()

func b(): int {.musttail.} = 2
proc test_b(): int = b()

doAssert test_a() == 1
doAssert test_b() == 2
