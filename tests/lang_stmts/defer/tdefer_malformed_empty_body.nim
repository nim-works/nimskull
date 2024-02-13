discard """
  description: "Empty `defer` body is not allowed (macro input)."
  errormsg: "illformed AST"
  line: 10
"""

import std/macros

macro bar(): untyped =
  result = newNimNode(nnkDefer)

proc foo() =
  bar()

foo()