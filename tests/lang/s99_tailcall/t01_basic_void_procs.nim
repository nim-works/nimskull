discard """
  description: "Both procs and funcs can use the .tailcall calling convention"
  output: "a\nb\n"
"""

proc a() {.tailcall.} = echo "a"
func b() {.tailcall.} = debugEcho "b"

a()
b()
