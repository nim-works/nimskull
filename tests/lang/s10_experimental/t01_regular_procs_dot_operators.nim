discard """
description: '''
Specification of the experimental dot operators feature
'''
matrix: "-d:nimPreviewDotLikeOps"
"""

{.experimental: "dotOperators".}

block int_dot_operator:
  template `.`(lhs: int, field: untyped): int = 1

  doAssert 1.test == 1