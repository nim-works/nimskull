discard """
description: '''
addr is not detected as a path expresesion
'''
action: reject
knownIssue: "The illegal ``ptr lent int`` type is not rejected"
"""

{.experimental: "views".}

proc block_pointer_deref_borrow() =
  var source = 12

  var view: ptr lent int = addr source

  source = 12

  discard view

block_pointer_deref_borrow()