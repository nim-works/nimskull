discard """
"""

import std/macros

macro foo*(n: untyped): untyped =
  echo "foo 1 ", n.astGenRepr
  doAssert true, "called the right one"

macro foo*(n, n2: untyped): untyped =
  echo "foo 2 ", n.astGenRepr, " ", n2.astGenRepr
  doAssert false, "called the wrong one"

proc test[T](v: T, str: string) =
  foo >"test" 

test("", "Test")