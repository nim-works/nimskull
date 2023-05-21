discard """
  description: "Specifies basic assertion operation used in other tests."
"""

block bool_literals:
  ## again, discard is required for forwards compatibility
  discard true
  discard false

block builtin_assert:
  ## `doAssert` command is used throughout the specification to demonstrate
  ## the correctness of an expression; ergo, `doAssert` evaluates the
  ## expression which must return `true`.

  ## If the expression is evaluated as true then the assertion is successful
  doAssert true

block tuatologies:
  ## `true == true` is evaluated as true, the assertion is succesful
  doAssert true == true

  ## `false == false` is evaluated as true, the assertion is succesful
  doAssert false == false