discard """
description: '''
Cannot mutate borrow of the first argument
'''
errormsg: "cannot borrow view; what it borrows from is potentially mutated"
"""

{.experimental: "views".}

proc borrowArg(e: var int): lent int = e

proc block_argument_borrow() =
  ## source itself is a path expression.

  block borrow_arg_call:
    ## example-explanation

    var source = 12

    var view: var int = borrowArg(source)

    source = 24

    discard view

block_argument_borrow()