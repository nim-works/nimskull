discard """
description: '''
Cannot mutate location of a container access
'''
errormsg: "cannot borrow view; what it borrows from is potentially mutated"
"""

{.experimental: "views".}

proc block_tuple_borrow() =
  ## Tuple access e[0] is a path expression.

  var source: (int, int) = (1, 2)

  var view: var int = source[0]

  ## Location access only tracks single element of a tuple, so it is
  ## possible to mutate other elements.
  source[1] = 200
  source[0] = 24

  discard view

block_tuple_borrow()