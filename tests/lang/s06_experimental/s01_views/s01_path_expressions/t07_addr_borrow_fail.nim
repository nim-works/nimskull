discard """
description: '''
addr is not detected as a path expresesion
'''
knownIssue: "addr is not a path expression"

"""


{.experimental: "views".}

proc block_pointer_deref_borrow() =
  ## An address `addr e`, is a path expression.

  var source = 12

  var view: ptr lent int = addr source

  source = 12

  discard view

block_pointer_deref_borrow()