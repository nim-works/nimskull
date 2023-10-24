discard """
description: '''
If statements, if expressions
'''
"""

#[
  - nested
  - mutliple statements
  - nested as expression
  - together with named block and break (simplest `{.noreturn.}`)
]#

block single_branch_if:
  var value = 0
  if true:
    value = 12

  doAssert value == 12

  if false:
    value = 24

  doAssert value == 12

block if_else_statement:
  var value = 0
  if true:
    value = 12

  else:
    value = 24

  doAssert value == 12

  if false:
    value = 36

  else:
    value = 48

  doAssert value == 48

block if_elif_statement:
  var value = 0
  if true:
    value = 20

  elif true:
    value = 30

  doAssert value == 20

  if false:
    value = 40

  elif true:
    value = 90

  doAssert value == 90

  if false:
    value = 100

  elif false:
    value = 120

  doAssert value == 90


block if_elif_else_statement:
  var value = 0
  if true:
    value = 1

  elif true:
    value = 2

  else:
    value = 3

  doAssert value == 1


block if_can_have_unlimited_number_of_elifs:
  var value = 0

  if false: value = 1
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif false: value = 2
  elif true: value = 4
  elif false: value = 2
  elif false: value = 2
  else: value = 3

  doAssert value == 4

block if_expression:
  let value = if true: 12 else: 3

block if_expression_noreturn:
  proc impl() =
    let value =
      if true:
        12

      else:
        doAssert false, "This statement should never be reached"
        return

    doAssert value == 12

  while true:
    let value = if true: 12 else: doAssert(false); break
    doAssert value == 12
    break