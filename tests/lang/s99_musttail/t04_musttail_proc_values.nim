discard """
  description: ".musttail is a valid calling convention for procedural values"
  output: "a\n"
"""

# with void return type and no parameters:
proc a() {.musttail.} = echo "a"
proc test_a(x: proc() {.musttail.}) =
  x()

test_a(a)

# with non-void return type and no parameters:
proc b(): int {.musttail.} = 1
proc test_b(x: proc(): int {.musttail.}): int =
  x()

doAssert test_b(b) == 1

# with non-void return type and parameters:
proc c(a, b: int): int {.musttail.} = a + b
proc test_c(x: proc(a, b: int): int {.musttail.}): int =
  x(1, 2)

doAssert test_c(c) == 3
