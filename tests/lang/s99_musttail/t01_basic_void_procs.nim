discard """
  description: "Both procs and funcs can use the .musttail calling convention"
  output: "a\nb\n"
"""

proc a() {.musttail.} = echo "a"
func b() {.musttail.} = debugEcho "b"

a()
b()
