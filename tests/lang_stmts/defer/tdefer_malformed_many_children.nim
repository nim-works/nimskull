discard """
  description: "Empty `defer` body is not allowed (macro input)."
  errormsg: "illformed AST"
  file: "macros.nim"
  line: 618
"""

import std/macros

macro bar(): untyped =
  result = newTree(nnkDefer, newNimNode(nnkEmpty), newNimNode(nnkEmpty))

proc foo() =
  bar()

foo()