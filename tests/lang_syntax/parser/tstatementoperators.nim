discard """
  nimout: '''
Infix
  Ident "from"
  Ident "a"
  Ident "b"
'''
"""

from std/macros import dumpTree

dumpTree(a from b)
