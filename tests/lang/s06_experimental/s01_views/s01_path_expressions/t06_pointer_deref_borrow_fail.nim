discard """
description: '''
Cannot mutate location created from pointer dereference
'''
errormsg: "'view' borrows from location 'source' which does not live long enough"
"""

{.experimental: "views".}

proc block_pointer_deref_borrow() =
  ## Pointer dereference `e[]` is a path expression.

  var data = 12
  var source = addr data

  var view: lent int = source[]

  source = addr data

  discard view

block_pointer_deref_borrow()