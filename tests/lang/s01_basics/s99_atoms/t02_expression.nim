discard """
description: '''
Test expression evaluation - via blocks, noreturn statements, regular expressions.
'''
"""

## Basic expressions. This section explains and tests basic expressions -
## without using `if/when/case` expressions, procedure calls and so on.



block basic_expression:
  ## Any literal is an expression. Concrete type of the expression depends on
  ## the type of literal used.

  ## String literal is an expression of type `string`
  doAssert "literal" is string

  ## Standalone integer literal is an expression of type `int`
  doAssert 12 is int

  ## Floating point value literal is an expression of type `float`
  doAssert 3.14 is float

  ## It is posssible to explicitly specify type of the numeric literal (integer
  ## or float) using suffix -

  ## Literal of type "unsigned 8-bit integer"
  doAssert 1'u8 is uint8

  ## "signed 8-bit integer"
  doAssert 1'i8 is int8

  ## Other types of integer literals include `16`, `32` and `64`-bit integers.

  doAssert 1'u16 is uint16
  doAssert 1'i16 is int16

  doAssert 1'u32 is uint32
  doAssert 1'i32 is int32

  doAssert 1'u64 is uint64
  doassert 1'i64 is int64

block block_expressions:
  doAssert(
    block:
      ## Block expression might contain multiple statements, including comments
      discard
      ## Value of the last expression in block will be used as a resulting value
      ## in the whole expression.
      true
  )

  doAssert(
    block:
      block:
        block:
          ## It is possible to nest block expressions when needed
          true
  )


  ## It is also possible to put multiple preceding statements on a single line
  ## using semicolon (`;`) as a separator.

  var value = 0

  ## This expression allows to implement pre-increment action
  doAssert value == 0
  doAssert ((inc value; value)) == 1 # QUESTION why double parens are necessary?
  doAssert value == 1

  ## This one is similar, but does a post-increment
  doAssert value == 1
  doAssert ((let tmp = value; inc value; tmp)) == 1
  doAssert value == 2


block statements_as_expressions:
  ## It is possible to use `if`, `case`, `try` statements as expressions.

  ## `if` can be used as inline expressions. For more examples on this see
  ## specification for if statement.
  doAssert(if 12 == 12: true else: false)
  doAssert(if 12 == 13: true elif 12 == 12: true else: false)

  ## Case statement cannot be used as a single-line expression, but it is still
  ## possible to use it in any other form. For more examples and specific rules
  ## on the case expression see specification for case statement.
  let vcase = case range[0..2](0):
                of 0: 13
                of 1: 14
                of 2: 15

  doAssert vcase == 13

  ## `try` can be used as a single-line expression as well. For more examples on
  ## and specific rules see specification for exception handling.
  let vtry = try:
    raise (ref OSError)()
    222

  except:
    12

  doAssert vtry == 12
