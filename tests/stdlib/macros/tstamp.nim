discard """
  description: Tests for macros.stamp
"""

import std/macros

block binder:
  ## Test automatic binding behavior
  macro simple() =
    let ast = stamp(echo "hello")
    doAssert ast[0] == bindSym"echo"

  simple()

  macro noResult() =
    let ast = stamp(result)
    doAssert ast.kind == nnkIdent
    doAssert eqIdent(ast, "result")

  noResult()
