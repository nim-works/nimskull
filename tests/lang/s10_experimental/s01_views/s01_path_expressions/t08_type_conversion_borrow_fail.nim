discard """
description: '''
Cannot mutate location of a container access
'''
errormsg: "'view' borrows from location 'source' which does not live long enough"
"""

{.experimental: "views".}

proc block_type_conversion_borrow() =
  ## Object field access e.field is a path expression.

  var source = int16(12)

  var view: var int = int32(source)

  ## Path expression tracks only specific fields, so all other ones can
  ## be modified as needed.
  source = 10

  discard view

block_type_conversion_borrow()