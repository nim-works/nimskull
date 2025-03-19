discard """
  description: "Both procs and funcs can use the .musttail calling convention"
  output: "a\nb\n"
"""

proc a() {.musttail.} = echo "a"
proc test_a() =
  a()

func b() {.musttail.} = debugEcho "b"
proc test_b() =
  b()

test_a()
test_b()
