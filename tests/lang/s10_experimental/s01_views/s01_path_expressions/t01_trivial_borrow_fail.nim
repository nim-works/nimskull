discard """
description: '''
Cannot mutate location of a trivial borrow
'''
errormsg: "'view' borrows from location 'source' which does not live long enough"
"""

{.experimental: "views".}

proc block_trivial_borrow() =
  ## source itself is a path expression.

  var source = 12

  var view: var int = source

  source = 24

  discard view

block_trivial_borrow()