discard """
  description: '''
    Ensures that pragma macros with typed input provide a useful error mesage
    when the input AST has errors.

    See issue: https://github.com/nim-works/nimskull/issues/1108
  '''
  errormsg: "undeclared identifier: 'totallyInvalidSym'"
  file: "tpragmamacros_typed_errmsg.nim"
  line: 22
  column: 3
"""

# Regression test: previously the compiler reported `bar` as a bad pragma

import std/macros

macro bar(x: typed): untyped =
  x

proc foobar() {.bar.} =
  totallyInvalidSym()