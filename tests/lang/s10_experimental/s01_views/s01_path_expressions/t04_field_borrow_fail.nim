discard """
description: '''
Cannot mutate location of a container access
'''
errormsg: "cannot borrow view; what it borrows from is potentially mutated"
"""

{.experimental: "views".}

type
  Object = object
    field1: int
    field2: int

proc block_trivial_borrow() =
  ## Object field access e.field is a path expression.

  var source = Object()

  var view: var int = source.field1

  ## Path expression tracks only specific fields, so all other ones can
  ## be modified as needed.
  source.field2 = 12
  source.field1 = 10

  discard view

block_trivial_borrow()