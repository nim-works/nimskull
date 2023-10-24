discard """
description: '''
This test specifies basic operations used in other tests.
'''
"""

block builtin_assert:
  ## `doAssert` command is used throughout the specification to demonstrate
  ## the correctness of an expression; ergo, it `doAssert` evaluates the
  ## expression which must return `true`.

  ## If the expression is evaluated as true then the assertion is successful
  doAssert true

  ## `0 == 0` is evaluated as true, the assertion is succesful
  doAssert 0 == 0

block built_integer_operations:
  ## Built-in operations on integer values

  ## Declare integer variable with initial value of `0`
  var value: int = 0

  ## Assert equality of the variable using `==` operator
  doAssert value == 0

  ## Increment value of the variale using `inc` command
  inc value

  doAssert value == 1

  ## Decrement value using `dec` command
  dec value

  ## Value of the variable returned to 0
  doAssert value == 0

block assert_type_of_the_expression:
  ## Every expression has a type. This can be checked for using `is` operator
  ## (or it's reverse `isnot`).

  ## Declare variable of type `int` and check if expression `value` has this
  ## type.
  var value: int = 0

  doAssert value is int
  doAssert value == 0

block get_type_of_the_expression_explicitly:
  ## Using `typeof()` procedure, you can get the type of the expression explicitly.
  ## Name of the type can be converted to string using `$` operator.
  ## The `$` operator is an inbuilt operation that converts built-in types to
  ## string. This will be discussed further in subsequent documents.

  var value: int = 0

  doAssert value is int
  doAssert $int == "int"
  doAssert $typeof(value) == "int"