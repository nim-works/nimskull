discard """
  matrix: "--errorMax:2"
  action: reject
  nimout: '''
tnnodeindexkind.nim(11, 4) Error: cannot set child of node kind: nnkStrLit
tnnodeindexkind.nim(15, 7) Error: cannot set child of node kind: nnkCommentStmt
'''
"""
import macros
macro t(x: untyped): untyped =
  x[0] = newEmptyNode()
t("abc")

macro t2(x: untyped): untyped =
  x[0][0] = newEmptyNode()
t2:
  ## comment
