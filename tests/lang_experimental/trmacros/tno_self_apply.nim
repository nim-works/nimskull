discard """
  description: "Term-rewriting macros don't apply to their own bodies"
"""

proc add(x, y: int): int = x + y
proc sub(x, y: int): int = x - y

# term-rewriting macros don't apply to their own body
macro m{add(x, 1)}(x: untyped): untyped =
  doAssert add(1, 1) == 2 # must not be rewritten
  result = x

doAssert add(1, 1) == 1

macro m2{sub(x, 1)}(x: untyped): untyped =
  # but term-rewriting macro bodies can still be rewritten by others
  let v = add(3, 1)
  doAssert v == 3
  result = x

doAssert sub(2, 1) == 2