discard """
  cmd: "nim check $options $file"
  action: reject
  nimout: '''
tif_ill_formed.nim(13, 15) Error: illformed AST:  else: <<0th child missing for nkElseExpr >>
'''
"""
import std/macros

macro t1(): untyped =
  result =
    nnkIfStmt.newTree(
      newNimNode(nnkElseExpr))

t1()
