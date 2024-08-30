discard """
description: '''
Cannot mutate location of a container access
'''
errormsg: "cannot borrow view; what it borrows from is potentially mutated"
"""

{.experimental: "views".}

proc block_container_borrow() =
  ## Container access like `e[i]` is a path expression.

  var source: seq[int] = @[0, 1, 2]

  let idx0 = 1
  var view: var int = source[idx0]

  let idx1 = 0
  source[idx1] = 24

  discard view

block_container_borrow()