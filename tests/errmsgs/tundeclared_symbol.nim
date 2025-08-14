discard """
  errormsg: "symbol used before declaration: 'c' [generated in tundeclared_symbol.nim(9, 17)]"
  line: 9
"""

import std/macros

macro useBeforeDecl(): untyped =
  let c = genSym("c")
  result = c

useBeforeDecl()
