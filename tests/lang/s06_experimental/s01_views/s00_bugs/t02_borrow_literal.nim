discard """
description: '''
C codegen error when trying to borrow from the literal. Should be a
nim compilation error.
'''
knownIssue: "codegen-fail on literal value borrow"

"""
{.experimental: "views".}

proc block_trivial_borrow() =
  var view: lent int = 12

block_trivial_borrow()