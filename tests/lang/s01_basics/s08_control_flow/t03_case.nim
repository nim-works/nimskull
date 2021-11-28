discard """
description: '''
Case statements, case expressions
'''
"""

#[
  - nested
  - mutliple statements
  - nested as expression
  - together with named block and break (simplest `{.noreturn.}`)
  - min and max allowed ranges
  - unhandled values
]#

## `case` statement is used to match value of the expression against multiple
## other values and execute first branch that matched.A

## Case is very commonly used with enums - they are covered in "user-defined
## data types" section of the specification, but to simplify provided examples
## most test here specify `case` behavior using `enum` unless test is explicitly
## meant to illustrate other part of behavior.

type SmallEnum = enum small1, small2, small3
type BigEnum = enum big1, big2, big3, big4, big5, big6, big7

block case_syntax:
  ## Case statement can be written in different forms - colon after expression
  ## is optional, as well as indentation for the `of` branches inside the body.
  ## Spec tests use indented version without omitting colon after expressions.

  case small3:
    of small1: discard
    of small2, small3: discard

  case small3:
    of small1: discard
    of small2, small3: discard

  case small3:
  of small1: discard
  of small2, small3: discard

  case small3
  of small1: discard
  of small2, small3: discard


  ## Body of the case expression might also contain `else`, `elif` branches that would
  ## be sequentially executed if expression value does not match any of the `of` branches.

  case small3:
    of small1: discard
    elif false: discard
    else: discard


block case_statement:
  var value = 0

  ## Expression in case statement is evaluated once and then matched against.
  ## Each `of` branch in the `case` body consists of a **constant** list of
  ## values to match against - it means the values must be known at a compile
  ## time, by either being specified as a literal value, or provided via `const`.
  case small3:
    of small1: value = 12
    of small2: value = 13
    of small3: value = 14

  doAssert value == 14

block case_expression:
  ## It is possible to use case as an expression, but only when all of the
  ## values are explicitly handled and each branch body is either and expression
  ## or noreturn statement. Value of the each branch must be

  block case_break:
    block name_1:
      let value = case small1:
                    of small1: 12
                    of small2: 13
                    else: break name_1

      doAssert value == 12

    block name_2:
      let value = case small3:
                    of small1: 12
                    of small2: 13
                    else:
                      doAssert true
                      break name_2

      ## This assert will never be exectude because previous statement executed
      ## `break name_2` statement.
      doAssert false

  block case_return:
    proc casewrap() =
      let value = case small1:
                    of small1: 12
                    of small2: 13
                    else: return

      doAssert value == 12

    casewrap()

block case_for_strings:
  ## In addition to handling ordinal values, `case` also supports matching
  ## against strings.

  var value = 0
  case "string":
    of "str":
      doAssert false

    of "string":
      value = 1

  doAssert value == 1

  ## When used with strings, `case` is not checked for exhaustiveness. It is
  ## still necessary to include an `else` branch in order to be able to use
  ## `case` over strings in expressions.

  let scase = case "string":
                of "str": 12
                of "string": 13
                else: 0

  doAssert scase == 13



block case_branches:
  ## Each branch of a case statement might contain one or more literal, set
  ## expression, range of values or an array of values.

  block of_array:
    ## Using array of string values in case statement branch.
    const stringArray: array[2, string] = ["value1", "value2"]

    var value = 0
    case "value1":
      of "other", stringArray:
        value = 1

      else:
        value = 12

    doAssert value == 1

  block of_set:
    var value = 1
    case small3:
      of small1:
        value = 2

      of {small2, small3}:
        value = 3

    doAssert value == 3
