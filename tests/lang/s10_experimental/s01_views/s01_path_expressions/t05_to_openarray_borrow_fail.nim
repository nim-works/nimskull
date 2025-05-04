discard """
description: '''
Cannot mutate location of an openarray
'''
errormsg: "cannot borrow view; what it borrows from is potentially mutated"
"""

{.experimental: "views".}

proc block_to_openarray_borrow() =
  ## `system.toOpenArray(e, ...)` is a path expression.

  var source = @[1, 2, 3]

  var view: openarray[int] = toOpenArray(source, 0, 1)

  source[0] = 12

  discard view

block_to_openarray_borrow()