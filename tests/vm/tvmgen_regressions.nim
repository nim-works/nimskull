discard """
  description: "Regression tests for the VM code-generator"
  targets: vm
"""

import std/[parseutils, macros]

block:
  # the ``parseBiggestFloat`` magic had an evaluation-order issue where the
  # second parameter was evaluated first
  var
    x = 0
    o: BiggestFloat
  let r = parseBiggestFloat((x = 1; "1.0"),
                            (doAssert x == 1; x = 2; o),
                            (doAssert x == 2; 0))

  doAssert r == 3
  doAssert o == 1.0

block wrong_getast:
  macro m() = # <- no return type specified
    result = quote: 1

  macro m2() =
    # the macro's public signature was used to detect whether it returns
    # something, which led to incorrect bytecode being generated in situations
    # where the result of a ``getAst`` call wasn't directly assigned to a
    # local, as the macro was treated as returning nothing
    let x = [getAst(m())]
    doAssert x[0].intVal == 1

  m2()