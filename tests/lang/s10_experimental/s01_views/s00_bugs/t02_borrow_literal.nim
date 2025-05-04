discard """
description: '''
Borrowing from a literal value is disallowed and results in a semantic error.
'''
action: reject
knownIssue: "codegen-fail on literal value borrow"

"""
{.experimental: "views".}

proc block_trivial_borrow() =
  var view: lent int = 12

block_trivial_borrow()