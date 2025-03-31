discard """
  description: ".tailcall is a valid calling convention for procedural values"
  output: "a\n"
"""

# with void return type and no parameters:
proc a() {.tailcall.} = echo "a"
proc test_a(x: proc() {.tailcall.}) =
  x()

test_a(a)

# with non-void return type and no parameters:
proc b(): int {.tailcall.} = 1
proc test_b(x: proc(): int {.tailcall.}): int =
  x()

doAssert test_b(b) == 1

# with non-void return type and parameters:
proc c(a, b: int): int {.tailcall.} = a + b
proc test_c(x: proc(a, b: int): int {.tailcall.}): int =
  x(1, 2)

doAssert test_c(c) == 3
